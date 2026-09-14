# Tests for R/service_llm.R — LLM API key persistence (Mistral /
# Anthropic / OpenAI), mirror of the Theia API key pattern.
#
# Each test isolates HOME and the LLM env vars so the host's real
# config never leaks in or out.


.llm_isolated_envvars <- c(
  MISTRAL_API_KEY   = NA,
  ANTHROPIC_API_KEY = NA,
  OPENAI_API_KEY    = NA
)


test_that("llm_providers exposes mistral / anthropic / openai with their env names", {
  p <- nemetonshiny:::llm_providers()
  expect_equal(sort(names(p)), c("anthropic", "mistral", "openai"))
  expect_equal(p$mistral$env,   "MISTRAL_API_KEY")
  expect_equal(p$anthropic$env, "ANTHROPIC_API_KEY")
  expect_equal(p$openai$env,    "OPENAI_API_KEY")
})


test_that("llm_status_all returns configured=FALSE for all providers when nothing is set", {
  tmp <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp, .llm_isolated_envvars))
  st <- nemetonshiny:::llm_status_all()
  expect_equal(sort(names(st)), c("anthropic", "mistral", "openai"))
  expect_true(all(vapply(st, function(s) isFALSE(s$configured),
                         logical(1))))
})


test_that("llm_status_all detects env-only configuration", {
  tmp <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp,
                        MISTRAL_API_KEY = "env-mistral",
                        ANTHROPIC_API_KEY = NA,
                        OPENAI_API_KEY = NA))
  st <- nemetonshiny:::llm_status_all()
  expect_true(st$mistral$env_ok)
  expect_false(st$mistral$file_ok)
  expect_true(st$mistral$configured)
  expect_false(st$anthropic$configured)
})


test_that("llm_save_api_key persists, sets env, chmod 0600, and isolates providers", {
  tmp <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp, .llm_isolated_envvars))

  ok <- nemetonshiny:::llm_save_api_key("mistral", "sk-mistral-test")
  expect_true(ok)
  expect_equal(Sys.getenv("MISTRAL_API_KEY"), "sk-mistral-test")

  path <- nemetonshiny:::.llm_apikey_path()
  expect_true(file.exists(path))
  if (.Platform$OS.type != "windows") {
    expect_equal(as.integer(file.info(path)$mode),
                 as.integer(as.octmode("0600")))
  }

  # Saving anthropic should not touch the mistral entry.
  nemetonshiny:::llm_save_api_key("anthropic", "sk-anthropic-test")
  st <- nemetonshiny:::llm_status_all()
  expect_true(st$mistral$configured)
  expect_true(st$anthropic$configured)
  expect_false(st$openai$configured)
  expect_equal(Sys.getenv("ANTHROPIC_API_KEY"), "sk-anthropic-test")
})


test_that("llm_save_api_key refuses empty / unknown providers", {
  tmp <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp, .llm_isolated_envvars))
  expect_false(nemetonshiny:::llm_save_api_key("mistral", ""))
  expect_false(nemetonshiny:::llm_save_api_key("mistral", "   "))
  expect_false(nemetonshiny:::llm_save_api_key("not-a-provider", "x"))
})


test_that("llm_clear_api_key removes the entry, unsets env, unlinks empty file", {
  tmp <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp, .llm_isolated_envvars))
  nemetonshiny:::llm_save_api_key("mistral", "x")
  nemetonshiny:::llm_save_api_key("openai", "y")
  path <- nemetonshiny:::.llm_apikey_path()
  expect_true(file.exists(path))

  expect_true(nemetonshiny:::llm_clear_api_key("mistral"))
  expect_equal(Sys.getenv("MISTRAL_API_KEY"), "")
  st <- nemetonshiny:::llm_status_all()
  expect_false(st$mistral$configured)
  expect_true(st$openai$configured)            # untouched
  expect_true(file.exists(path))                # still has openai

  expect_true(nemetonshiny:::llm_clear_api_key("openai"))
  expect_false(file.exists(path))               # file unlinked when empty

  # Idempotent : clearing nothing returns FALSE.
  expect_false(nemetonshiny:::llm_clear_api_key("openai"))
})


# ---------------------------------------------------------------------------
# Repli de modele (403 palier / 429 quota)
# ---------------------------------------------------------------------------

test_that(".llm_is_tier_error distingue un refus de palier d'une vraie panne", {
  tier <- function(msg) simpleError(msg)

  # Refus de palier / quota : un autre modele peut passer.
  expect_true(nemetonshiny:::.llm_is_tier_error(
    tier("HTTP 403 Forbidden. This model is not available in your subscription tier")))
  expect_true(nemetonshiny:::.llm_is_tier_error(tier("HTTP 429 Too Many Requests")))
  expect_true(nemetonshiny:::.llm_is_tier_error(tier("Rate limit exceeded")))
  expect_true(nemetonshiny:::.llm_is_tier_error(tier("HTTP  429")))

  # Tout le reste doit remonter tel quel : reessayer masquerait le diagnostic.
  expect_false(nemetonshiny:::.llm_is_tier_error(tier("HTTP 401 Unauthorized")))
  expect_false(nemetonshiny:::.llm_is_tier_error(tier("Could not resolve host")))
  expect_false(nemetonshiny:::.llm_is_tier_error(tier("HTTP 500 Internal Server Error")))
  expect_false(nemetonshiny:::.llm_is_tier_error(tier("")))
})


test_that(".llm_fallback_models ne repond que pour les providers qui en ont", {
  expect_equal(
    nemetonshiny:::.llm_fallback_models("mistral"),
    c("ministral-14b-latest", "ministral-8b-latest", "ministral-3b-latest")
  )
  expect_length(nemetonshiny:::.llm_fallback_models("anthropic"), 0L)
  expect_length(nemetonshiny:::.llm_fallback_models("inconnu"), 0L)
  expect_length(nemetonshiny:::.llm_fallback_models(NULL), 0L)
})


test_that("llm_chat_with_fallback n'appelle PAS de repli quand le principal repond", {
  vus <- character(0)
  build <- function(m) {
    vus <<- c(vus, m)
    list(chat = function(...) paste("reponse de", m))
  }
  obj <- nemetonshiny:::llm_chat_with_fallback(build, "mistral", "mistral-medium-latest")

  expect_equal(obj$chat("prompt"), "reponse de mistral-medium-latest")
  expect_equal(vus, "mistral-medium-latest")
})


test_that("llm_chat_with_fallback bascule sur le premier repli qui repond", {
  vus <- character(0)
  build <- function(m) {
    vus <<- c(vus, m)
    list(chat = function(...) {
      if (m == "mistral-medium-latest") stop("HTTP 403 Forbidden. subscription tier")
      if (m == "ministral-14b-latest") stop("HTTP 429 Rate limit exceeded")
      paste("reponse de", m)
    })
  }
  obj <- nemetonshiny:::llm_chat_with_fallback(build, "mistral", "mistral-medium-latest")

  expect_equal(obj$chat("prompt"), "reponse de ministral-8b-latest")
  # L'ordre compte : du plus capable au moins capable, sans sauter d'etape.
  expect_equal(vus, c("mistral-medium-latest", "ministral-14b-latest",
                      "ministral-8b-latest"))
})


test_that("llm_chat_with_fallback laisse passer une erreur qui n'est pas un palier", {
  vus <- character(0)
  build <- function(m) {
    vus <<- c(vus, m)
    list(chat = function(...) stop("Could not resolve host: api.mistral.ai"))
  }
  obj <- nemetonshiny:::llm_chat_with_fallback(build, "mistral", "mistral-medium-latest")

  expect_error(obj$chat("prompt"), "Could not resolve host")
  # Aucun repli tente : une panne reseau ne se soigne pas en changeant de modele.
  expect_equal(vus, "mistral-medium-latest")
})


test_that("llm_chat_with_fallback remonte l'erreur D'ORIGINE quand tout echoue", {
  build <- function(m) list(chat = function(...) {
    if (m == "mistral-medium-latest") stop("HTTP 403 palier principal")
    stop("HTTP 429 repli sature")
  })
  obj <- nemetonshiny:::llm_chat_with_fallback(build, "mistral", "mistral-medium-latest")

  # C'est le refus du modele CONFIGURE qui informe l'utilisateur, pas celui
  # d'un remplacant qu'il n'a jamais choisi.
  expect_error(obj$chat("prompt"), "palier principal")
})


test_that("llm_chat_with_fallback ne rejoue pas le modele qui vient d'echouer", {
  vus <- character(0)
  build <- function(m) {
    vus <<- c(vus, m)
    list(chat = function(...) {
      if (m == "ministral-14b-latest") stop("HTTP 403 Forbidden")
      paste("reponse de", m)
    })
  }
  # Le modele principal appartient lui-meme a la chaine de repli.
  obj <- nemetonshiny:::llm_chat_with_fallback(build, "mistral", "ministral-14b-latest")

  expect_equal(obj$chat("prompt"), "reponse de ministral-8b-latest")
  expect_equal(sum(vus == "ministral-14b-latest"), 1L)
})

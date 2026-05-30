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

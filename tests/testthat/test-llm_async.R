# Appels LLM hors de la boucle Shiny (audit 1.0, phase 4)

.fake_chat_factory <- function(reponses) {
  i <- 0L
  function(system_prompt) {
    list(chat = function(prompt, echo = FALSE) {
      i <<- i + 1L
      r <- reponses[[min(i, length(reponses))]]
      if (inherits(r, "error")) stop(r)
      r
    })
  }
}

test_that(".llm_batch_run stops after a failed first prompt", {
  local_mocked_bindings(create_llm_chat = .fake_chat_factory(list(simpleError("429"))))
  r <- .llm_batch_run("sys", list(.synthese = "a", C = "b"))
  expect_length(r$results, 0L)
  expect_identical(names(r$errors), ".synthese")
  expect_match(r$errors[[".synthese"]], "429")
})

test_that(".llm_batch_run retries a family once on an empty response", {
  local_mocked_bindings(create_llm_chat = .fake_chat_factory(list("synthese", "  ", "famille C")))
  local_mocked_bindings(Sys.sleep = function(...) NULL, .package = "base")
  r <- .llm_batch_run("sys", list(.synthese = "a", C = "b"), retries = 1L)
  expect_identical(r$results$.synthese, "synthese")
  expect_identical(r$results$C, "famille C")
  expect_length(r$errors, 0L)
})

test_that("llm_batch_async resolves off the call (inline mode)", {
  withr::local_options(nemetonshiny.llm_inline = TRUE)
  local_mocked_bindings(create_llm_chat = .fake_chat_factory(list("ok")))
  vu <- NULL
  p <- promises::then(llm_batch_async("sys", list(.synthese = "x")), function(r) vu <<- r)
  expect_null(vu)   # pas encore : la promesse n'est pas resolue dans l'appel
  for (i in 1:20) later::run_now(0.05)
  expect_identical(vu$results$.synthese, "ok")
})

test_that("the worker receives the API keys set during the session", {
  withr::local_envvar(MISTRAL_API_KEY = "cle-session", ANTHROPIC_API_KEY = "")
  snap <- .llm_env_snapshot()
  expect_identical(snap[["MISTRAL_API_KEY"]], "cle-session")
  expect_false("ANTHROPIC_API_KEY" %in% names(snap))
})

test_that("family AI generation runs as a task and fills the comment", {
  skip_if_not_installed("sf")
  withr::local_options(nemetonshiny.llm_inline = TRUE)
  geom <- sf::st_sfc(sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol = 2, byrow = TRUE))),
                     crs = 2154)
  ind_sf <- sf::st_sf(ug_id = "u1", indicateur_c1_biomasse_norm = 0.5, geometry = geom)
  st <- shiny::reactiveValues(
    current_project = list(indicators = sf::st_drop_geometry(ind_sf), indicators_sf = ind_sf),
    language = "fr", family_comments = list(), project_id = "p1")
  maj <- NULL
  local_mocked_bindings(create_llm_chat = .fake_chat_factory(list("Analyse famille C")))
  local_mocked_bindings(get_llm_api_key_var = function(provider) NULL)
  local_mocked_bindings(updateTextAreaInput = function(session, inputId, value = NULL, ...) {
    if (identical(inputId, "analysis_comments")) maj <<- value
  }, .package = "shiny")
  shiny::testServer(mod_family_server, args = list(family_code = "C", app_state = st), {
    session$setInputs(ai_generate = 1)
    expect_null(maj)   # le clic rend la main avant la reponse
    for (i in 1:30) { later::run_now(0.05); session$flushReact() }
    expect_identical(maj, "Analyse famille C")
  })
})

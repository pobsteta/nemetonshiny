# Chaine « Tout calculer » : chien de garde et reponses tardives (audit 1.0)

test_that("pipeline_step_timeout_s distinguishes long engines from short steps", {
  expect_identical(pipeline_step_timeout_s("indicateurs"), 24 * 3600)
  expect_identical(pipeline_step_timeout_s("ia_plan"), 2 * 3600)
  withr::local_options(nemetonshiny.pipeline_timeout_h = 0.5)
  expect_identical(pipeline_step_timeout_s("indicateurs"), 1800)
})

test_that("a silent step is failed by the watchdog and the chain moves on", {
  withr::local_options(nemetonshiny.pipeline_timeout_h = 1 / 3600)  # 1 s
  local_mocked_bindings(pipeline_state_save = function(...) invisible(NULL))
  app_state <- shiny::reactiveValues(language = "fr", current_project = list(id = "p1"),
                                     project_id = "p1",
                                     pipeline_request = NULL, pipeline_answer = NULL)
  shiny::testServer(mod_pipeline_server, args = list(app_state = app_state), {
    session$setInputs(scope = c("regen_annees", "regen_gel"), profil = "generalist")
    session$setInputs(start = 1)
    expect_identical(app_state$pipeline_request$step_id, "regen_annees")
    # La requete date d'il y a 5 s : le module n'a jamais repondu
    req <- app_state$pipeline_request
    req$ts <- Sys.time() - 5
    app_state$pipeline_request <- req
    suppressWarnings(session$elapse(61000))
    expect_identical(rv$state$results$regen_annees$status, "error")
    expect_identical(app_state$pipeline_request$step_id, "regen_gel")
  })
})

test_that("a late answer for an already decided step does not re-post the request", {
  local_mocked_bindings(pipeline_state_save = function(...) invisible(NULL))
  app_state <- shiny::reactiveValues(language = "fr", current_project = list(id = "p1"),
                                     project_id = "p1",
                                     pipeline_request = NULL, pipeline_answer = NULL)
  shiny::testServer(mod_pipeline_server, args = list(app_state = app_state), {
    session$setInputs(scope = c("regen_annees", "regen_gel"), profil = "generalist")
    session$setInputs(start = 1)
    run <- rv$state$run_id
    pipeline_answer(app_state, list(run_id = run, step_id = "regen_annees"), "ok")
    session$flushReact()
    ts_gel <- app_state$pipeline_request$ts
    expect_identical(app_state$pipeline_request$step_id, "regen_gel")
    # Reponse tardive pour l'etape deja tranchee
    suppressWarnings({
      pipeline_answer(app_state, list(run_id = run, step_id = "regen_annees"), "error")
      session$flushReact()
    })
    expect_identical(app_state$pipeline_request$ts, ts_gel)   # pas reemise
    expect_identical(rv$state$results$regen_annees$status, "ok")
  })
})

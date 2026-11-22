# Mission 043 - l'arret du lancement enchaine arrete le moteur reGeneration.

test_that("pipeline_arret_moteur_requis : seulement a l'etape regeneration d'un run ouvert", {
  etat <- nemetonshiny:::pipeline_new_run(c("regen_gel", "regeneration"))
  expect_false(nemetonshiny:::pipeline_arret_moteur_requis(etat))      # etape regen_gel
  etat <- nemetonshiny:::pipeline_record(etat, "regen_gel", "ok", NULL)
  expect_true(nemetonshiny:::pipeline_arret_moteur_requis(etat))       # etape regeneration
  # Une fois annule, le curseur est passe au-dela : plus d'arret a demander.
  expect_false(nemetonshiny:::pipeline_arret_moteur_requis(
    nemetonshiny:::pipeline_cancel(etat)))
  expect_false(nemetonshiny:::pipeline_arret_moteur_requis(NULL))
})

.etat_app_043 <- function(proj) {
  shiny::reactiveValues(
    language = "fr", project_id = "p1",
    current_project = list(id = "p1", path = proj),
    pipeline_request = NULL, pipeline_answer = NULL, regen_stop_request = NULL)
}

test_that("arret manuel pendant l'etape regeneration : demande d'arret du moteur", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  proj <- withr::local_tempdir()
  app_state <- .etat_app_043(proj)
  suppressWarnings(shiny::testServer(
    nemetonshiny:::mod_pipeline_server,
    args = list(app_state = app_state),
    {
      session$setInputs(scope = "regeneration", profil = "generalist")
      session$setInputs(start = 1)
      expect_identical(app_state$pipeline_request$step_id, "regeneration")
      session$setInputs(cancel = 1)
      expect_false(is.null(app_state$regen_stop_request))
      expect_identical(app_state$regen_stop_request$project_path, proj)
    }
  ))
})

test_that("arret manuel pendant une autre etape : pas de demande d'arret du moteur", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  app_state <- .etat_app_043(withr::local_tempdir())
  suppressWarnings(shiny::testServer(
    nemetonshiny:::mod_pipeline_server,
    args = list(app_state = app_state),
    {
      session$setInputs(scope = "indicateurs", profil = "generalist")
      session$setInputs(start = 1)
      session$setInputs(cancel = 1)
      expect_null(app_state$regen_stop_request)
    }
  ))
})

test_that("projet change avant l'etape regeneration : demande d'arret du moteur", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  proj <- withr::local_tempdir()
  app_state <- .etat_app_043(proj)
  suppressWarnings(shiny::testServer(
    nemetonshiny:::mod_pipeline_server,
    args = list(app_state = app_state),
    {
      session$setInputs(scope = c("regen_gel", "regeneration"), profil = "generalist")
      session$setInputs(start = 1)
      run_id <- rv$state$run_id
      app_state$project_id <- "p2"
      app_state$current_project <- list(id = "p2", path = withr::local_tempdir())
      app_state$pipeline_answer <- list(run_id = run_id, step_id = "regen_gel",
                                        status = "ok", message = NULL,
                                        ts = Sys.time())
      session$flushReact()
      expect_true(nemetonshiny:::pipeline_is_done(rv$state))
      expect_identical(app_state$regen_stop_request$project_path, proj)
    }
  ))
})

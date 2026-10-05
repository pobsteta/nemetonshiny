# Suivi sanitaire : annulation, relance et zones pendant un calcul (audit 1.0)

.fake_task <- function(result = NULL, status = "initial") {
  st <- new.env(parent = emptyenv())
  st$calls <- list()
  list(invoke = function(...) { st$calls[[length(st$calls) + 1L]] <- list(...); invisible(NULL) },
       result = function() result,
       status = function() status,
       .calls = function() st$calls)
}

.run_monitoring <- function(fast = .fake_task(), fordead = .fake_task(),
                            reconfort = .fake_task(), expr, project = NULL) {
  notes <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) { notes <<- c(notes, paste(as.character(ui), collapse = " ")); invisible("id") },
    .package = "shiny")
  app_state <- shiny::reactiveValues(language = "fr", current_project = project)
  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) NULL,
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() fast,
    run_fordead_async              = function() fordead,
    run_reconfort_async            = function() reconfort,
    .resolve_progress_path         = function(...) NULL,
    shiny::testServer(mod_monitoring_server, args = list(app_state = app_state), {
      session$flushReact()
      expr(session, input)
    })
  )
  notes
}

test_that("a cancelled FORDEAD run is not announced as a success", {
  skip_if_not_installed("shiny")
  i18n <- get_i18n("fr")
  notes <- .run_monitoring(
    fordead = .fake_task(result = list(status = "cancelled"), status = "success"),
    expr = function(session, input) NULL)
  expect_true(any(grepl(i18n$t("monitoring_fordead_annule"), notes, fixed = TRUE)))
  succes <- sub("%s", "", i18n$t("monitoring_health_success_done"), fixed = TRUE)
  expect_false(any(grepl(succes, notes, fixed = TRUE)))
})

test_that("a cancelled FAST run is not announced as a success", {
  skip_if_not_installed("shiny")
  i18n <- get_i18n("fr")
  notes <- .run_monitoring(
    fast = .fake_task(result = list(status = "cancelled", summary = list(n_scenes = 3L)),
                      status = "success"),
    expr = function(session, input) NULL)
  expect_true(any(grepl(i18n$t("monitoring_fast_annule"), notes, fixed = TRUE)))
})

test_that("RECONFORT cannot be relaunched while the previous run is alive", {
  skip_if_not_installed("shiny")
  i18n <- get_i18n("fr")
  rec <- .fake_task(status = "running")
  testthat::local_mocked_bindings(
    reconfort_year_bounds = function(v_model = "v3", ...) list(min = 2016L, max = 2025L, default = 2025L),
    .package = "nemeton")
  notes <- .run_monitoring(
    reconfort = rec, project = list(id = "p", path = withr::local_tempdir()),
    expr = function(session, input) {
      session$setInputs(zone_id = "1", reconfort_s2_year = 2025L, run_reconfort = 1L)
    })
  expect_length(rec$.calls(), 0L)
  expect_true(any(grepl(i18n$t("monitoring_run_precedent_actif"), notes, fixed = TRUE)))
})

test_that("zones cannot be re-registered while a health engine runs", {
  skip_if_not_installed("shiny")
  i18n <- get_i18n("fr")
  appel <- FALSE
  testthat::local_mocked_bindings(
    build_project_monitoring_zones = function(...) { appel <<- TRUE; list() },
    .package = "nemeton")
  notes <- .run_monitoring(
    fordead = .fake_task(status = "running"),
    project = list(id = "p", path = withr::local_tempdir(), metadata = list(name = "P")),
    expr = function(session, input) session$setInputs(register = 1L))
  expect_false(appel)
  expect_true(any(grepl(i18n$t("zones_bloquees_calcul"), notes, fixed = TRUE)))
})

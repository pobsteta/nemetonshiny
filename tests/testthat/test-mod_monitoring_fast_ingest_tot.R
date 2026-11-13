# Mission 017 : l'ingestion FAST manuelle part sur la zone `_tot`, pas sur la
# strate choisie dans le menu (parite FORDEAD / pipeline).

.i017_task <- function() {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  list(
    invoke = function(...) {
      state$calls[[length(state$calls) + 1L]] <- list(...)
      invisible(NULL)
    },
    result = function() NULL,
    status = function() "initial",
    .calls = function() state$calls
  )
}

.i017_run <- function(zones_df) {
  task <- .i017_task()
  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) "fake-con",
    list_monitoring_zones          = function(con) zones_df,
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() task,
    get_monitoring_zone_aoi        = function(con, zone_id) NULL,
    {
      testthat::with_mocked_bindings(
        find_zones_by_project = function(con, project_uuid) zones_df,
        .package = "nemeton",
        {
          shiny::testServer(
            nemetonshiny:::mod_monitoring_server,
            args = list(app_state = shiny::reactiveValues(
              language = "fr", current_project = list(id = "fake-project-id"))),
            {
              session$setInputs(
                zone_id    = "1",   # strate choisie dans le menu
                date_range = c(as.Date("2025-06-01"), as.Date("2025-06-30")),
                run        = 1L
              )
            }
          )
        }
      )
    }
  )
  task$.calls()
}

test_that("manual FAST ingestion targets the _tot zone, not the picked stratum", {
  skip_if_not_installed("shiny")
  calls <- .i017_run(data.frame(id = c(1L, 2L, 9L),
                                name = c("Foret", "Massif", "proj_tot"),
                                stringsAsFactors = FALSE))
  expect_length(calls, 1L)
  expect_identical(calls[[1]]$zone_id, 9L)   # _tot, jamais la strate 1
})

test_that("manual FAST ingestion falls back on the picked zone without _tot", {
  skip_if_not_installed("shiny")
  calls <- .i017_run(data.frame(id = c(1L, 2L), name = c("Foret", "Massif"),
                                stringsAsFactors = FALSE))
  expect_length(calls, 1L)
  expect_identical(calls[[1]]$zone_id, 1L)   # repli explicite, pas de NA
})

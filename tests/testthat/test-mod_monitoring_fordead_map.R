# Tests for mod_monitoring_fordead_map.R — Carte FORDEAD sub-tab.
#
# The module reads the categorical 0-4 dieback mask persisted by
# nemeton@v0.41.0 to <project>/cache/layers/fordead/zone_<id>/ and
# renders it on a leaflet map; empty-state otherwise.

test_that("mod_monitoring_fordead_map_ui returns a uiOutput tag", {
  skip_if_not_installed("shiny")
  ui <- nemetonshiny:::mod_monitoring_fordead_map_ui("fm")
  expect_s3_class(ui, "shiny.tag")
  expect_true(grepl("fm-panel", as.character(ui)))
})

test_that("mask_r returns NULL when the fordead cache dir is absent", {
  skip_if_not_installed("shiny")

  withr::with_tempdir({
    proj_path <- getwd()  # no cache/layers/fordead subdir
    testthat::with_mocked_bindings(
      get_monitoring_db_connection   = function(...) "fake-con",
      close_monitoring_db_connection = function(con) invisible(TRUE),
      {
        shiny::testServer(
          nemetonshiny:::mod_monitoring_fordead_map_server,
          args = list(
            app_state = shiny::reactiveValues(
              language = "fr",
              current_project = list(id = "x", path = proj_path)
            ),
            zone_id_r = shiny::reactive("1")
          ),
          {
            session$flushReact()
            expect_null(mask_r())
            # The panel falls back to the empty-state card.
            html <- paste(as.character(output$panel), collapse = "")
            expect_true(grepl("hourglass-split", html))
          }
        )
      }
    )
  })
})

test_that("mask_r re-reads the cache when refresh_r is bumped", {
  skip_if_not_installed("shiny")

  # v0.38.6 — a completed FORDEAD run bumps the parent's
  # alerts_refresh counter (passed in as refresh_r). Without that
  # dependency mask_r evaluated once (pre-run, cache dir absent →
  # NULL) and stayed frozen, so the Carte FORDEAD sub-tab kept
  # showing the empty-state even after the mask was persisted.
  withr::with_tempdir({
    proj_path <- getwd()
    cd <- file.path(proj_path, "cache", "layers", "fordead")
    refresh <- shiny::reactiveVal(0L)
    read_calls <- 0L

    testthat::with_mocked_bindings(
      get_monitoring_db_connection   = function(...) "fake-con",
      close_monitoring_db_connection = function(con) invisible(TRUE),
      {
        testthat::local_mocked_bindings(
          read_fordead_dieback_mask = function(con, zone_id,
                                               run_id = NULL,
                                               cache_dir = NULL) {
            read_calls <<- read_calls + 1L
            NULL
          },
          .package = "nemeton"
        )
        shiny::testServer(
          nemetonshiny:::mod_monitoring_fordead_map_server,
          args = list(
            app_state = shiny::reactiveValues(
              language = "fr",
              current_project = list(id = "x", path = proj_path)
            ),
            zone_id_r = shiny::reactive("1"),
            refresh_r = refresh
          ),
          {
            # Cache dir now exists (FORDEAD run created it).
            dir.create(cd, recursive = TRUE)
            shiny::observe({ mask_r() })
            session$flushReact()
            base <- read_calls
            expect_gt(base, 0L)  # read attempted once cache dir exists

            # Simulate a FORDEAD run completing → refresh_r bumps.
            refresh(refresh() + 1L)
            session$flushReact()
            # mask_r re-evaluated → read_fordead_dieback_mask called again.
            expect_gt(read_calls, base)
          }
        )
      }
    )
  })
})

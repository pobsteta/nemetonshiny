# testServer coverage for the RECONFORT-fixed validation sampling sub-tab
# (spec 021 G4). The cœur planner + DB are mocked — no real run/raster.

`%||%` <- function(a, b) if (is.null(a)) b else a

.rc_fake_plan <- function() {
  sf::st_sf(
    plot_id     = c("V01", "V02", "T01"),
    type        = c("Validation", "Validation", "Temoin"),
    alert_class = c(2L, 3L, 1L),
    source      = "RECONFORT",
    zone_id     = 7L,
    geometry    = sf::st_sfc(
      lapply(1:3, function(i) sf::st_point(c(i * 10, i * 10))), crs = 2154
    )
  )
}

test_that("RECONFORT validation sub-tab generates a plan via the RECONFORT source", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  seen_source <- NULL
  testthat::local_mocked_bindings(
    get_monitoring_db_connection   = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    generate_validation_plan = function(con, project, source, ...) {
      seen_source <<- source
      .rc_fake_plan()
    }
  )

  app_state <- shiny::reactiveValues(
    language        = "fr",
    active_main_tab = "monitoring",
    current_project = list(id = "p", path = withr::local_tempdir(),
                           metadata = list(monitoring_zone_id = 7L))
  )

  shiny::testServer(
    nemetonshiny:::mod_validation_sampling_server,
    args = list(id = "rc", app_state = app_state,
                zone_id_r    = shiny::reactive("7"),
                mode_r       = shiny::reactive("reconfort"),
                thresholds_r = shiny::reactive(list(index = "NDVI")),
                date_range_r = shiny::reactive(NULL),
                source_fixed = "RECONFORT"),
    {
      session$setInputs(
        classes = c("2", "3"), control_classes = "1",
        n_validation = 2L, n_control = 1L, buffer_m = 0, seed = 42L
      )
      session$flushReact()
      expect_null(session$returned$plan())   # nothing yet
      session$setInputs(generate = 1L)
      session$flushReact()
      # Plan produced, routed through the RECONFORT source.
      expect_identical(seen_source, "RECONFORT")
      expect_s3_class(session$returned$plan(), "sf")
      expect_equal(nrow(session$returned$plan()), 3L)
    }
  )
})

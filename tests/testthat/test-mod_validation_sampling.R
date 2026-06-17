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

# ---------------------------------------------------------------------------
# FAST = plan sanitaire branché sur le trend (spec 025)
# ---------------------------------------------------------------------------

.ft_fake_plan <- function() {
  sf::st_sf(
    plot_id     = c("S01", "S02", "T01"),
    type        = c("Sanitaire", "Sanitaire", "Temoin"),
    alert_value = c(0.03, 0.02, 0),
    index       = "NDRE",
    source      = "FAST_TREND",
    seed        = 42L,
    zone_id     = 7L,
    geometry    = sf::st_sfc(
      lapply(1:3, function(i) sf::st_point(c(i * 10, i * 10))), crs = 2154
    )
  )
}

test_that("FAST validation sub-tab calls the trend plan with right params", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  seen <- new.env()
  testthat::local_mocked_bindings(
    get_monitoring_db_connection   = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    generate_trend_sanitary_plan = function(con, project, zone_id = NULL,
                                            index, n_plots, n_control,
                                            date_from, date_to, months,
                                            min_years, min_obs_per_year,
                                            alpha, seed) {
      seen$zone_id <- zone_id
      seen$index <- index; seen$n_plots <- n_plots
      seen$n_control <- n_control; seen$seed <- seed
      seen$date_from <- date_from; seen$date_to <- date_to
      seen$months <- months
      .ft_fake_plan()
    }
  )

  app_state <- shiny::reactiveValues(
    language = "fr", active_main_tab = "monitoring",
    current_project = list(id = "p", path = withr::local_tempdir(),
                           metadata = list(monitoring_zone_id = 7L))
  )

  shiny::testServer(
    nemetonshiny:::mod_validation_sampling_server,
    args = list(id = "ft", app_state = app_state,
                zone_id_r    = shiny::reactive("7"),
                mode_r       = shiny::reactive("quick"),
                thresholds_r = shiny::reactive(list(index = "NDRE")),
                date_range_r = shiny::reactive(NULL),
                source_fixed = "FAST"),
    {
      session$setInputs(
        trend_index = "NDRE",
        trend_win = c(as.Date("2017-01-01"), as.Date("2025-12-31")),
        n_validation = 20L, n_control = 5L, seed = 42L
      )
      session$flushReact()
      expect_null(session$returned$plan())
      session$setInputs(generate = 1L)
      session$flushReact()
      # Cœur appelé avec la zone SÉLECTIONNÉE (zone_id_r = "7"), pas la
      # metadata — et les bons paramètres trend (pas de classes/buffer).
      expect_equal(seen$zone_id, 7L)
      expect_equal(seen$index, "NDRE")
      expect_equal(seen$n_plots, 20L)
      expect_equal(seen$n_control, 5L)
      expect_equal(seen$seed, 42L)
      expect_equal(seen$date_from, as.Date("2017-01-01"))
      expect_equal(seen$months, 6:9)
      # Plan trend routé (3 placettes : 2 sanitaires + 1 témoin).
      plan <- session$returned$plan()
      expect_s3_class(plan, "sf")
      expect_equal(nrow(plan), 3L)
      expect_setequal(unique(as.character(plan$type)), c("Sanitaire", "Temoin"))
    }
  )
})

test_that("FAST validation : no significant decline → empty, no plan", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  testthat::local_mocked_bindings(
    get_monitoring_db_connection   = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    generate_trend_sanitary_plan = function(...) {
      rlang::abort("No significant decline.", class = "validation_empty_trend")
    }
  )

  app_state <- shiny::reactiveValues(
    language = "fr", active_main_tab = "monitoring",
    current_project = list(id = "p", path = withr::local_tempdir(),
                           metadata = list(monitoring_zone_id = 7L))
  )

  shiny::testServer(
    nemetonshiny:::mod_validation_sampling_server,
    args = list(id = "ft2", app_state = app_state,
                zone_id_r    = shiny::reactive("7"),
                mode_r       = shiny::reactive("quick"),
                thresholds_r = shiny::reactive(list(index = "NDRE")),
                date_range_r = shiny::reactive(NULL),
                source_fixed = "FAST"),
    {
      session$setInputs(
        trend_index = "NDRE",
        trend_win = c(as.Date("2017-01-01"), as.Date("2025-12-31")),
        n_validation = 20L, n_control = 5L, seed = 42L
      )
      session$setInputs(generate = 1L)
      session$flushReact()
      # Pas de plan ; erreur typée capturée (panneau « aucun déclin »).
      expect_null(session$returned$plan())
    }
  )
})

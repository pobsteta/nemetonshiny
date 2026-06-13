# Tests for mod_monitoring_reconfort_map.R (spec 021, L6).
#
# The module wires nemeton::list_alerts / check_reconfort_validity /
# read_reconfort_pixel_series + the app's get_monitoring_db_connection /
# get_monitoring_zone_aoi. All are mocked — no DB, no real run.

`%||%` <- function(a, b) if (is.null(a)) b else a

.reconfort_fake_alerts <- function(n = 2L) {
  sf::st_sf(
    id               = seq_len(n),
    confidence_class = rep(c("2-deperissant", "3-tres-deperissant"),
                           length.out = n),
    stress_index     = seq_len(n) / 10,
    geometry = sf::st_sfc(
      lapply(seq_len(n), function(i)
        sf::st_point(c(5 + i / 100, 47 + i / 100))),
      crs = 4326
    )
  )
}

.reconfort_fake_aoi <- function() {
  sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(5, 47), c(5.1, 47), c(5.1, 47.1), c(5, 47.1), c(5, 47)
      ))), crs = 4326
    )
  )
}

test_that("reconfort map shows the empty state when there are no alerts", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not(exists("list_alerts", where = asNamespace("nemeton"),
                     inherits = FALSE),
              "nemeton RECONFORT API not installed")

  empty <- .reconfort_fake_alerts(0L)

  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    get_monitoring_zone_aoi = function(con, zone_id) .reconfort_fake_aoi()
  )
  testthat::local_mocked_bindings(
    list_alerts = function(con, zone_id, classes = NULL, ...) empty,
    check_reconfort_validity = function(aoi, ...) list(geo_valid = TRUE,
                                                       species_valid = TRUE,
                                                       advisory = TRUE),
    .package = "nemeton"
  )

  app_state <- shiny::reactiveValues(
    language = "fr",
    current_project = list(id = "p", path = withr::local_tempdir())
  )

  shiny::testServer(
    nemetonshiny:::mod_monitoring_reconfort_map_server,
    args = list(id = "rc", app_state = app_state,
                zone_id_r = shiny::reactive("1")),
    {
      session$flushReact()
      expect_equal(nrow(session$returned$alerts()), 0L)
      html <- as.character(output$panel$html %||% output$panel)
      i18n <- get_i18n("fr")
      expect_true(grepl(i18n$t("monitoring_reconfort_map_empty_title"),
                        html, fixed = TRUE))
      # No validity banner when geo_valid is TRUE.
      expect_false(grepl(i18n$t("monitoring_reconfort_outside_validity"),
                         html, fixed = TRUE))
    }
  )
})

test_that("reconfort map renders the G3 advisory banner when geo invalid", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not(exists("check_reconfort_validity", where = asNamespace("nemeton"),
                     inherits = FALSE),
              "nemeton RECONFORT API not installed")

  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    get_monitoring_zone_aoi = function(con, zone_id) .reconfort_fake_aoi()
  )
  testthat::local_mocked_bindings(
    list_alerts = function(con, zone_id, classes = NULL, ...) .reconfort_fake_alerts(2L),
    check_reconfort_validity = function(aoi, ...) list(geo_valid = FALSE,
                                                       species_valid = TRUE,
                                                       advisory = TRUE),
    .package = "nemeton"
  )

  app_state <- shiny::reactiveValues(
    language = "fr",
    current_project = list(id = "p", path = withr::local_tempdir())
  )

  shiny::testServer(
    nemetonshiny:::mod_monitoring_reconfort_map_server,
    args = list(id = "rc", app_state = app_state,
                zone_id_r = shiny::reactive("1")),
    {
      session$flushReact()
      html <- as.character(output$panel$html %||% output$panel)
      i18n <- get_i18n("fr")
      # Advisory banner present, and the map (not the empty state) shown.
      expect_true(grepl(i18n$t("monitoring_reconfort_outside_validity"),
                        html, fixed = TRUE))
      expect_false(grepl(i18n$t("monitoring_reconfort_map_empty_title"),
                         html, fixed = TRUE))
    }
  )
})

test_that("reconfort map click produces a pixel time-series plot", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not_installed("plotly")
  skip_if_not(exists("read_reconfort_pixel_series",
                     where = asNamespace("nemeton"), inherits = FALSE),
              "nemeton >= 0.80.0 (read_reconfort_pixel_series) not installed")

  proj_dir <- withr::local_tempdir()
  dir.create(file.path(proj_dir, "cache", "layers", "reconfort"),
             recursive = TRUE, showWarnings = FALSE)

  series <- data.frame(
    obs_date   = as.Date(c("2023-05-01", "2023-06-01", "2023-07-01")),
    crswir_obs = c(0.20, 0.25, 0.31),
    crre_obs   = c(0.55, 0.50, 0.42),
    stringsAsFactors = FALSE
  )
  attr(series, "species") <- "CHE"
  attr(series, "v_model") <- "v3"

  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    get_monitoring_zone_aoi = function(con, zone_id) .reconfort_fake_aoi()
  )
  testthat::local_mocked_bindings(
    list_alerts = function(con, zone_id, classes = NULL, ...) .reconfort_fake_alerts(1L),
    check_reconfort_validity = function(aoi, ...) list(geo_valid = TRUE, advisory = TRUE),
    read_reconfort_pixel_series = function(con, zone_id, xy, crs = 4326,
                                           run_id = NULL, cache_dir) series,
    .package = "nemeton"
  )

  app_state <- shiny::reactiveValues(
    language = "fr",
    current_project = list(id = "p", path = proj_dir)
  )

  shiny::testServer(
    nemetonshiny:::mod_monitoring_reconfort_map_server,
    args = list(id = "rc", app_state = app_state,
                zone_id_r = shiny::reactive("1")),
    {
      session$flushReact()
      session$setInputs(map_click = list(lat = 47.05, lng = 5.05))
      # The click handler showModal()s a plotly bound to pixel_ts_plot.
      expect_no_error(session$flushReact())
      expect_false(is.null(output$pixel_ts_plot))
    }
  )
})

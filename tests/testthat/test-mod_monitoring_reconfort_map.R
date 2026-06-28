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

# ---- Manifest-driven layered display (toggles + opacity) -----------------

# Builds three small EPSG:2154 rasters on disk + a synthetic run `result`
# matching the contract of nemeton::run_reconfort_dieback() ($rasters).
.reconfort_fake_result <- function(dir) {
  base <- terra::rast(nrows = 8, ncols = 8,
                      xmin = 900000, xmax = 900080,
                      ymin = 6500000, ymax = 6500080,
                      crs = "EPSG:2154")
  score <- base; terra::values(score) <- seq(1, 100, length.out = 64)
  classif <- base; terra::values(classif) <- rep(1:3, length.out = 64)
  prob <- base; terra::values(prob) <- seq(0, 1000, length.out = 64)
  sp <- file.path(dir, "score.tif")
  cp <- file.path(dir, "classif.tif")
  pp <- file.path(dir, "prob.tif")
  terra::writeRaster(score,   sp, overwrite = TRUE)
  terra::writeRaster(classif, cp, overwrite = TRUE)
  terra::writeRaster(prob,    pp, overwrite = TRUE)
  list(
    status   = "ok",
    zone_id  = 1L,
    rasters  = list(continuous_score = sp, classif = cp, probability = pp),
    n_alerts = 0L
  )
}

test_that("manifest drives layer toggles + opacity slider", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")
  skip_if_not(exists("reconfort_layer_manifest",
                     where = asNamespace("nemeton"), inherits = FALSE),
              "nemeton reconfort_layer_manifest() not installed")

  rdir <- withr::local_tempdir()
  fake_result <- .reconfort_fake_result(rdir)

  # No DB needed : alerts_r / validity_r short-circuit on a NULL connection.
  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) NULL,
    close_monitoring_db_connection = function(con) invisible(TRUE)
  )

  app_state <- shiny::reactiveValues(
    language = "fr",
    current_project = list(id = "p", path = withr::local_tempdir())
  )

  shiny::testServer(
    nemetonshiny:::mod_monitoring_reconfort_map_server,
    args = list(id = "rc", app_state = app_state,
                zone_id_r = shiny::reactive("1"),
                result_r  = shiny::reactive(fake_result)),
    {
      session$flushReact()
      i18n <- get_i18n("fr")
      html <- as.character(output$panel$html %||% output$panel)

      # Controls present : layer checkbox group + opacity slider.
      expect_true(grepl(i18n$t("reconfort_couches"), html, fixed = TRUE))
      expect_true(grepl(i18n$t("reconfort_opacite"), html, fixed = TRUE))
      # The three manifest raster labels appear as checkbox choices.
      expect_true(grepl(i18n$t("reconfort_couche_score"),   html, fixed = TRUE))
      expect_true(grepl(i18n$t("reconfort_couche_classes"), html, fixed = TRUE))
      expect_true(grepl(i18n$t("reconfort_couche_proba"),   html, fixed = TRUE))
      # Empty-state must NOT show when layers are available.
      expect_false(grepl(i18n$t("monitoring_reconfort_map_empty_title"),
                         html, fixed = TRUE))

      # Base leaflet map rendered (rasters read from disk).
      expect_false(is.null(output$map))

      # Toggling layers + moving the opacity slider re-renders without error.
      session$setInputs(layers = c("score", "probability"), opacity = 0.4)
      expect_no_error(session$flushReact())
      session$setInputs(layers = "classification", opacity = 0.9)
      expect_no_error(session$flushReact())
    }
  )
})

test_that("no in-memory result falls back to the DB-only alerts view", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not(exists("reconfort_layer_manifest",
                     where = asNamespace("nemeton"), inherits = FALSE),
              "nemeton reconfort_layer_manifest() not installed")

  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    get_monitoring_zone_aoi = function(con, zone_id) .reconfort_fake_aoi()
  )
  testthat::local_mocked_bindings(
    list_alerts = function(con, zone_id, classes = NULL, ...) .reconfort_fake_alerts(2L),
    check_reconfort_validity = function(aoi, ...) list(geo_valid = TRUE, advisory = TRUE),
    .package = "nemeton"
  )

  app_state <- shiny::reactiveValues(
    language = "fr",
    current_project = list(id = "p", path = withr::local_tempdir())
  )

  shiny::testServer(
    nemetonshiny:::mod_monitoring_reconfort_map_server,
    args = list(id = "rc", app_state = app_state,
                zone_id_r = shiny::reactive("1"),
                result_r  = shiny::reactive(NULL)),
    {
      session$flushReact()
      i18n <- get_i18n("fr")
      html <- as.character(output$panel$html %||% output$panel)
      # Legacy mode : NO layer toggles, map shown from DB alerts.
      expect_false(grepl(i18n$t("reconfort_couches"), html, fixed = TRUE))
      expect_false(grepl(i18n$t("monitoring_reconfort_map_empty_title"),
                         html, fixed = TRUE))
      expect_equal(nrow(session$returned$alerts()), 2L)
    }
  )
})

# ---- RECONFORT progress dispatcher (.reconfort_handle_progress_event) ----

test_that(".reconfort_handle_progress_event toasts on reconfort:phase", {
  skip_if_not_installed("shiny")
  captured <- list()
  i18n <- nemetonshiny:::get_i18n("fr")
  fake_session <- list(ns = function(id) paste0("monitoring-", id))

  testthat::with_mocked_bindings(
    showNotification = function(ui, id = NULL, ...) {
      captured$ui <<- ui; captured$id <<- id; captured$args <<- list(...)
      invisible(NULL)
    },
    .package = "shiny",
    {
      nemetonshiny:::.reconfort_handle_progress_event(
        ev = list(current = "reconfort:phase", phase_name = "ingest",
                  completed = 4L, total = 10L),
        session = fake_session, i18n = i18n
      )
    }
  )
  expect_false(is.null(captured$ui))
  rendered <- htmltools::renderTags(captured$ui)$html
  expect_match(rendered, "Phase 5/10", fixed = TRUE)
  expect_match(rendered, "Sentinel-2", fixed = TRUE)  # phase label
  expect_identical(captured$id, "monitoring-reconfort_progress")
  expect_null(captured$args$duration)
})

test_that(".reconfort_handle_progress_event is silent on reconfort:start", {
  skip_if_not_installed("shiny")
  n <- 0L
  fake_session <- list(ns = function(id) paste0("monitoring-", id))
  testthat::with_mocked_bindings(
    showNotification = function(...) { n <<- n + 1L; invisible(NULL) },
    .package = "shiny",
    {
      nemetonshiny:::.reconfort_handle_progress_event(
        ev = list(current = "reconfort:start", zone_id = 1L),
        session = fake_session, i18n = nemetonshiny:::get_i18n("fr")
      )
    }
  )
  expect_equal(n, 0L)
})

test_that(".reconfort_handle_progress_event surfaces reconfort:error", {
  skip_if_not_installed("shiny")
  captured <- list()
  fake_session <- list(ns = function(id) paste0("monitoring-", id))
  testthat::with_mocked_bindings(
    showNotification = function(ui, id = NULL, ...) {
      captured$ui <<- ui; captured$args <<- list(...); invisible(NULL)
    },
    removeNotification = function(...) invisible(NULL),
    .package = "shiny",
    {
      nemetonshiny:::.reconfort_handle_progress_event(
        ev = list(current = "reconfort:error", error_message = "conda missing"),
        session = fake_session, i18n = nemetonshiny:::get_i18n("fr")
      )
    }
  )
  expect_match(as.character(captured$ui), "conda missing", fixed = TRUE)
  expect_identical(captured$args$type, "error")
})

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

# ---------------------------------------------------------------------------
# Phase A (spec 008 §15, ADR-013 A5, décision D2) — affichage piloté par le
# raster + masquage par strate. FORDEAD est calculé une fois sur la zone
# `_tot` ; l'affichage par strate masque le raster `_tot` ; « zone saine »
# se décide sur le raster (pas sur un compte d'alertes DB).
# ---------------------------------------------------------------------------

# Petit raster catégoriel 0-4 en EPSG:2154 pour les tests (4x4 cellules).
.fm_test_raster <- function(maxval) {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 40,
                   ymin = 0, ymax = 40, crs = "EPSG:2154")
  v <- rep(0, terra::ncell(r))
  v[length(v)] <- maxval
  terra::values(r) <- v
  r
}

# AOI couvrant tout l'extent du raster (le masquage conserve les valeurs).
.fm_test_aoi <- function() {
  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0))))
  sf::st_sf(zone_id = 1L, geometry = sf::st_sfc(poly, crs = 2154))
}

# data.frame de zones avec une `_tot` (id 10) et une strate `_res` (id 11).
.fm_test_zones <- function() {
  data.frame(id = c(10L, 11L),
             name = c("proj_tot", "proj_res"),
             stringsAsFactors = FALSE)
}

test_that("AC.15.2 — masque avec pixels classe >=1 → carte raster (zone sans placette)", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("terra")

  withr::with_tempdir({
    proj_path <- getwd()
    cd <- file.path(proj_path, "cache", "layers", "fordead")
    dir.create(cd, recursive = TRUE)

    testthat::with_mocked_bindings(
      get_monitoring_db_connection   = function(...) "fake-con",
      close_monitoring_db_connection = function(con) invisible(TRUE),
      get_monitoring_zone_aoi        = function(con, zone_id) .fm_test_aoi(),
      {
        testthat::local_mocked_bindings(
          find_zones_by_project = function(con, project_uuid) .fm_test_zones(),
          read_fordead_dieback_mask = function(con, zone_id, run_id = NULL,
                                               cache_dir = NULL) {
            .fm_test_raster(3)  # classe 3-forte présente
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
            zone_id_r = shiny::reactive("10")  # zone _tot sélectionnée
          ),
          {
            session$flushReact()
            r <- mask_r()
            expect_s4_class(r, "SpatRaster")
            expect_gte(nemetonshiny:::.fordead_raster_max(r), 1)
            html <- paste(as.character(output$panel), collapse = "")
            # État (a) : carte raster, PAS la carte « zone saine ».
            expect_true(grepl("-map\"", html) || grepl("leaflet", html))
            expect_false(grepl("check-circle-fill", html))
          }
        )
      }
    )
  })
})

test_that("AC.15.3 — masque tout classe 0 → carte « zone saine »", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("terra")

  withr::with_tempdir({
    proj_path <- getwd()
    cd <- file.path(proj_path, "cache", "layers", "fordead")
    dir.create(cd, recursive = TRUE)

    testthat::with_mocked_bindings(
      get_monitoring_db_connection   = function(...) "fake-con",
      close_monitoring_db_connection = function(con) invisible(TRUE),
      get_monitoring_zone_aoi        = function(con, zone_id) .fm_test_aoi(),
      {
        testthat::local_mocked_bindings(
          find_zones_by_project = function(con, project_uuid) .fm_test_zones(),
          read_fordead_dieback_mask = function(con, zone_id, run_id = NULL,
                                               cache_dir = NULL) {
            .fm_test_raster(0)  # tout sain (classe 0)
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
            zone_id_r = shiny::reactive("10")
          ),
          {
            session$flushReact()
            r <- mask_r()
            expect_s4_class(r, "SpatRaster")
            expect_lt(nemetonshiny:::.fordead_raster_max(r), 1)
            html <- paste(as.character(output$panel), collapse = "")
            # État (b) : carte verte « zone saine ».
            expect_true(grepl("check-circle-fill", html))
          }
        )
      }
    )
  })
})

test_that("AC.15.8 — changer de strate re-masque sans relire une autre zone", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("terra")

  withr::with_tempdir({
    proj_path <- getwd()
    cd <- file.path(proj_path, "cache", "layers", "fordead")
    dir.create(cd, recursive = TRUE)

    read_zone_ids <- integer(0)
    aoi_zone_ids  <- integer(0)
    sel <- shiny::reactiveVal("10")  # démarre sur _tot

    testthat::with_mocked_bindings(
      get_monitoring_db_connection   = function(...) "fake-con",
      close_monitoring_db_connection = function(con) invisible(TRUE),
      get_monitoring_zone_aoi        = function(con, zone_id) {
        aoi_zone_ids <<- c(aoi_zone_ids, as.integer(zone_id))
        .fm_test_aoi()
      },
      {
        testthat::local_mocked_bindings(
          find_zones_by_project = function(con, project_uuid) .fm_test_zones(),
          read_fordead_dieback_mask = function(con, zone_id, run_id = NULL,
                                               cache_dir = NULL) {
            read_zone_ids <<- c(read_zone_ids, as.integer(zone_id))
            .fm_test_raster(3)
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
            zone_id_r = sel
          ),
          {
            shiny::observe({ mask_r() })
            session$flushReact()
            # Sur _tot : pas de masquage par strate (sel == id_tot).
            expect_true(all(read_zone_ids == 10L))
            n_aoi_tot <- length(aoi_zone_ids)
            expect_equal(n_aoi_tot, 0L)

            # Bascule sur la strate _res (11) → re-masquage.
            sel("11")
            session$flushReact()
            # Le masque relu reste celui de _tot (10), JAMAIS 11.
            expect_true(all(read_zone_ids == 10L))
            # Le masquage par strate a appelé get_monitoring_zone_aoi(11).
            expect_true(11L %in% aoi_zone_ids)
          }
        )
      }
    )
  })
})

test_that("AC.15.5 — aucune clé i18n monitoring_fordead_* ne mentionne « placette »/« plot »", {
  tr <- nemetonshiny:::TRANSLATIONS
  fordead_keys <- grep("^monitoring_fordead", names(tr), value = TRUE)
  expect_gt(length(fordead_keys), 0L)
  for (k in fordead_keys) {
    for (lng in c("fr", "en")) {
      txt <- tr[[k]][[lng]] %||% ""
      expect_false(grepl("placette", txt, ignore.case = TRUE),
                   info = sprintf("%s[%s] contient « placette »", k, lng))
      expect_false(grepl("\\bplots?\\b", txt, ignore.case = TRUE),
                   info = sprintf("%s[%s] contient « plot »", k, lng))
    }
  }
})

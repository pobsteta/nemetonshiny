# Tests for the Carte pixel sub-tab module (mod_monitoring_pixel_map).
#
# All tests skip cleanly when nemeton@v0.22.0 is not installed (the 4
# new exports — read_s2_band_stack, build_index_stack,
# extract_pixel_timeseries, read_s2_band_raster — are required) so the
# CI runs that pin an older nemeton don't fail.

.skip_if_no_pixel_map <- function() {
  testthat::skip_if_not_installed("nemeton")
  needed <- c("build_index_stack", "extract_pixel_timeseries",
              "read_s2_band_stack", "read_s2_band_raster")
  exports <- getNamespaceExports("nemeton")
  if (!all(needed %in% exports)) {
    testthat::skip(sprintf(
      "nemeton missing pixel-map exports (need >= v0.22.0; missing: %s)",
      paste(setdiff(needed, exports), collapse = ", ")))
  }
}

# ---- UI sanity -------------------------------------------------------

test_that("mod_monitoring_pixel_map_ui returns valid Shiny UI", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  testthat::with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonshiny:::mod_monitoring_pixel_map_ui("pmap")
      expect_true(inherits(ui, "shiny.tag") ||
                  inherits(ui, "shiny.tag.list") ||
                  inherits(ui, "bslib_fragment"))
      html <- as.character(ui)
      expect_true(grepl("pmap-map",   html))
      expect_true(grepl("pmap-index", html))
      # NDMI wired into the index selector, listed FIRST, NDVI default.
      expect_true(grepl("NDMI", html))
      expect_lt(regexpr('value="NDMI"', html), regexpr('value="NDVI"', html))
      expect_match(html, 'value="NDVI"[^>]*checked', perl = TRUE)
    }
  )
})


# ---- Server: pixel_stack reactive does NOT fire in health mode -------

test_that("pixel_stack does not call build_index_stack in health mode", {
  skip_if_not_installed("shiny")
  .skip_if_no_pixel_map()

  # v0.52.16 — param `obs_pixel_data` retiré du module (FAST pure raster).
  call_count <- 0L
  testthat::with_mocked_bindings(
    build_index_stack = function(...) {
      call_count <<- call_count + 1L
      stop("should not be called in health mode")
    },
    .package = "nemeton",
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_pixel_map_server,
        args = list(
          app_state  = shiny::reactiveValues(language = "fr",
                                             current_project = NULL),
          mode_input = shiny::reactive("health")  # not quick
        ),
        {
          session$flushReact()
          expect_equal(call_count, 0L)
        }
      )
    }
  )
})


# ---- Server: scenes_df derivation from the on-disk cache -------------

test_that("scenes_df enumerates populated cache dirs, date parsed from scene id", {
  skip_if_not_installed("shiny")
  .skip_if_no_pixel_map()

  # v0.52.16 — `scenes_df_r` est désormais purement disk-driven : la
  # date provient toujours du parsing du scene_id (plus de jointure avec
  # `obs_pixel`). Test simplifié : 2 tuiles MGRS sur la même date, les
  # 2 doivent apparaître dans scenes_df avec leur date parsée.
  captured <- list()
  testthat::with_mocked_bindings(
    build_index_stack = function(cache_dir, scenes_df, index) {
      captured$scenes_df <<- scenes_df
      captured$index     <<- index
      NULL  # don't try to actually build a SpatRaster
    },
    .package = "nemeton",
    {
      proj <- list(path = withr::local_tempdir())
      cd   <- file.path(proj$path, "cache", "layers", "sentinel2")
      dir.create(cd, recursive = TRUE)
      sids <- c("S2A_MSIL2A_20250610T103021_T31TGM",
                "S2A_MSIL2A_20250610T103021_T31TFM")
      for (s in sids) {
        dir.create(file.path(cd, s))
        file.create(file.path(cd, s, "B08.tif"))
      }
      shiny::testServer(
        nemetonshiny:::mod_monitoring_pixel_map_server,
        args = list(
          app_state  = shiny::reactiveValues(language = "fr",
                                             current_project = proj),
          mode_input = shiny::reactive("quick")
        ),
        {
          session$setInputs(index = "NDVI")
          session$flushReact()
          expect_false(is.null(captured$scenes_df))
          expect_equal(nrow(captured$scenes_df), 2L)
          expect_setequal(captured$scenes_df$scene_id, sids)
          expect_true(all(captured$scenes_df$obs_date ==
                          as.Date("2025-06-10")))
          expect_equal(captured$index, "NDVI")
        }
      )
    }
  )
})


# ---- Server: cache_dir resolution ------------------------------------

test_that("cache_dir resolves NULL when no project / no folder", {
  skip_if_not_installed("shiny")
  .skip_if_no_pixel_map()

  # v0.52.16 — param `obs_pixel_data` retiré.
  call_count <- 0L
  testthat::with_mocked_bindings(
    build_index_stack = function(...) {
      call_count <<- call_count + 1L
      NULL
    },
    .package = "nemeton",
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_pixel_map_server,
        args = list(
          app_state  = shiny::reactiveValues(language = "fr",
                                             current_project = NULL),
          mode_input = shiny::reactive("quick")
        ),
        {
          session$setInputs(index = "NDVI")
          session$flushReact()
          # No project → cache_dir NULL → build_index_stack NOT called.
          expect_equal(call_count, 0L)
        }
      )
    }
  )
})


# ---- Server: map_click triggers extract_pixel_timeseries -------------

test_that("map_click invokes extract_pixel_timeseries with lat/lng", {
  skip_if_not_installed("shiny")
  .skip_if_no_pixel_map()

  # v0.52.16 — param `obs_pixel_data` retiré. Le scene_id sur disque
  # doit être un vrai nom S2 (sinon `.pixel_scene_date_from_id()`
  # renvoie NA et scenes_df est vide).
  captured <- list()
  fake_ts <- data.frame(
    obs_date = as.Date(c("2025-06-10", "2025-06-25",
                         "2025-06-10", "2025-06-25")),
    index    = c("NDVI", "NDVI", "NBR", "NBR"),
    value    = c(0.7, 0.65, 0.5, 0.45),
    stringsAsFactors = FALSE
  )
  testthat::with_mocked_bindings(
    build_index_stack       = function(...) NULL,
    extract_pixel_timeseries = function(cache_dir, scenes_df, xy,
                                         crs = 4326,
                                         indices = c("NDVI", "NBR")) {
      captured$xy      <<- xy
      captured$crs     <<- crs
      captured$indices <<- indices
      fake_ts
    },
    .package = "nemeton",
    {
      proj <- list(path = withr::local_tempdir())
      cd   <- file.path(proj$path, "cache", "layers", "sentinel2")
      dir.create(cd, recursive = TRUE)
      sid <- "S2A_MSIL2A_20250610T103021_T31TGM"
      dir.create(file.path(cd, sid))
      file.create(file.path(cd, sid, "B08.tif"))
      shiny::testServer(
        nemetonshiny:::mod_monitoring_pixel_map_server,
        args = list(
          app_state  = shiny::reactiveValues(language = "fr",
                                             current_project = proj),
          mode_input = shiny::reactive("quick")
        ),
        {
          session$setInputs(
            index     = "NDVI",
            map_click = list(lat = 47.5, lng = 4.5)
          )
          session$flushReact()
          expect_equal(captured$xy, c(4.5, 47.5))   # (lng, lat)
          expect_equal(captured$crs, 4326)
          expect_setequal(captured$indices, c("NDVI", "NBR"))
        }
      )
    }
  )
})

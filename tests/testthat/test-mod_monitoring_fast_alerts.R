# Tests for the FAST alerts module helpers — v0.45.0.

test_that("fast alerts UI exposes NDMI first with NDVI as default", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  ui <- nemetonshiny:::mod_monitoring_fast_alerts_ui("fa")
  html <- as.character(ui)
  expect_true(grepl("fa-index", html))
  expect_true(grepl("NDMI", html))
  # NDMI listed first; NDVI is the checked default.
  expect_lt(regexpr('value="NDMI"', html), regexpr('value="NDVI"', html))
  expect_match(html, 'value="NDVI"[^>]*checked', perl = TRUE)
  # v0.70.5 — Le placeholder `fa-ndmi_note` a été retiré avec le
  # helper `.fast_ndmi_note()` (avertissement B11 obsolète depuis
  # plancher cœur nemeton >= 0.65.1).
  expect_false(grepl("fa-ndmi_note", html))
})

# v0.70.5 — Test `.fast_ndmi_note` retiré : le helper a été
# supprimé avec ses 2 clés i18n (`monitoring_fast_ndmi_hint` +
# `monitoring_fast_ndmi_b11_note`). L'avertissement B11 était
# obsolète depuis le plancher cœur `nemeton (>= 0.65.1)` qui
# garantit la mise en cache best-effort de B11 (spec 019 D3).

test_that("fast alerts UI exposes the three modes and trend params", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  ui <- nemetonshiny:::mod_monitoring_fast_alerts_ui("fa")
  html <- as.character(ui)
  # Three radio mode values, including the new trend mode.
  expect_true(grepl('value="count"', html))
  expect_true(grepl('value="rolling"', html))
  expect_true(grepl('value="trend"', html))
  # Trend-only params live in a conditionalPanel (rendered in the DOM).
  expect_true(grepl("fa-trend_months", html))
  expect_true(grepl("fa-trend_min_years", html))
  expect_true(grepl("fa-trend_alpha", html))
})

test_that(".fast_index_choices is mode-dependent (trend -> NDMI/NDRE)", {
  # count / rolling : indices de choc court terme (NDRE réservé à trend).
  cr <- nemetonshiny:::.fast_index_choices("count")
  expect_setequal(unname(cr), c("NDMI", "NDVI", "NBR"))
  expect_false("NDRE" %in% cr)
  rr <- nemetonshiny:::.fast_index_choices("rolling")
  expect_setequal(unname(rr), c("NDMI", "NDVI", "NBR"))
  expect_false("NDRE" %in% rr)
  tr <- nemetonshiny:::.fast_index_choices("trend")
  expect_setequal(unname(tr), c("NDMI", "NDRE"))
  # Defaults : NDVI for short-term modes, NDMI for trend.
  expect_equal(nemetonshiny:::.fast_index_default("count"), "NDVI")
  expect_equal(nemetonshiny:::.fast_index_default("rolling"), "NDVI")
  expect_equal(nemetonshiny:::.fast_index_default("trend"), "NDMI")
})

test_that(".fast_class_labels accepts trend mode with its own unit", {
  i18n <- nemetonshiny:::get_i18n("fr")
  r <- terra::rast(nrows = 10, ncols = 10,
                   vals = c(rep(0, 50), seq.int(1, 50) / 100),
                   crs = "EPSG:2154", extent = terra::ext(0, 100, 0, 100))
  out <- nemetonshiny:::.fast_class_labels(r = r, source = "FAST",
                                            mode = "trend", i18n = i18n)
  expect_equal(names(out), c("1", "2", "3", "4"))
  # Trend unit (pente/an) appended to the labels.
  expect_match(out[["1"]], "pente/an$")
})

test_that(".classify_alert_count returns 4 prefixed bins for a wide distribution", {
  # 100-cell raster (10x10) : 50 zero pixels + values 1..50.
  r <- terra::rast(nrows = 10, ncols = 10,
                   vals = c(rep(0, 50), seq.int(1, 50)),
                   crs = "EPSG:2154", extent = terra::ext(0, 100, 0, 100))
  cls <- nemetonshiny:::.classify_alert_count(r)
  # v0.45.0 — 4 quartile-based bins (aligned with FAST mask classes 1-4),
  # 5 break points.
  expect_length(cls$breaks, 5L)
  expect_length(cls$labels, 4L)
  expect_equal(min(cls$breaks), 0.5)
  # Labels are prefixed by the class number.
  expect_match(cls$labels[1], "^1 — ")
  expect_match(cls$labels[2], "^2 — ")
  expect_match(cls$labels[3], "^3 — ")
  expect_match(cls$labels[4], "^4 — >")  # top bin is "Q4+"
})

test_that(".classify_alert_count appends the unit when provided", {
  r <- terra::rast(nrows = 10, ncols = 10,
                   vals = c(rep(0, 50), seq.int(1, 50)),
                   crs = "EPSG:2154", extent = terra::ext(0, 100, 0, 100))
  cls <- nemetonshiny:::.classify_alert_count(r, unit = " j")
  expect_match(cls$labels[1], " j$")
  expect_match(cls$labels[length(cls$labels)], " j$")
})

test_that(".classify_alert_count collapses to one bucket for a constant raster", {
  r <- terra::rast(nrows = 4, ncols = 4, vals = c(rep(0, 8), rep(3, 8)),
                   crs = "EPSG:2154", extent = terra::ext(0, 40, 0, 40))
  cls <- nemetonshiny:::.classify_alert_count(r)
  expect_length(cls$breaks, 2L)
  expect_length(cls$labels, 1L)
  expect_match(cls$labels[1], "^1 — 3")
})

test_that(".classify_alert_count handles a zone with no alert", {
  r <- terra::rast(nrows = 4, ncols = 4, vals = 0,
                   crs = "EPSG:2154", extent = terra::ext(0, 40, 0, 40))
  cls <- nemetonshiny:::.classify_alert_count(r)
  expect_length(cls$labels, 0L)
})

test_that(".classify_alert_count ignores NA pixels", {
  # 40-cell raster (4x10) : 33 NA + 7 alert values.
  vals <- c(rep(NA_real_, 33), 1, 2, 3, 5, 8, 13, 21)
  r <- terra::rast(nrows = 4, ncols = 10, vals = vals,
                   crs = "EPSG:2154", extent = terra::ext(0, 100, 0, 40))
  cls <- nemetonshiny:::.classify_alert_count(r)
  expect_true(length(cls$labels) >= 1L)
  expect_true(all(cls$breaks >= 0))
})

test_that(".fast_class_labels returns biological labels for FORDEAD", {
  i18n <- nemetonshiny:::get_i18n("fr")
  out <- nemetonshiny:::.fast_class_labels(r = NULL, source = "FORDEAD",
                                            mode = "count", i18n = i18n)
  expect_equal(names(out), c("1", "2", "3", "4"))
  expect_match(out[["4"]], "sol nu")
})

test_that(".fast_class_labels falls back to static labels for FAST when r is NULL", {
  i18n <- nemetonshiny:::get_i18n("fr")
  out <- nemetonshiny:::.fast_class_labels(r = NULL, source = "FAST",
                                            mode = "count", i18n = i18n)
  expect_equal(names(out), c("1", "2", "3", "4"))
  # Static fallback : labels carry the class number prefix.
  expect_match(out[["1"]], "^1 — ")
})

test_that(".fast_class_labels uses quartiles + unit when raster is provided (count mode)", {
  i18n <- nemetonshiny:::get_i18n("fr")
  r <- terra::rast(nrows = 10, ncols = 10,
                   vals = c(rep(0, 50), seq.int(1, 50)),
                   crs = "EPSG:2154", extent = terra::ext(0, 100, 0, 100))
  out <- nemetonshiny:::.fast_class_labels(r = r, source = "FAST",
                                            mode = "count", i18n = i18n)
  expect_match(out[["1"]], " j$")           # unit "j" (days) for count mode
  expect_match(out[["4"]], "^4 — >")        # top is open bucket
})

test_that(".alert_count_palette returns a length-matched palette", {
  expect_length(nemetonshiny:::.alert_count_palette(1L), 1L)
  expect_length(nemetonshiny:::.alert_count_palette(4L), 4L)
  expect_length(nemetonshiny:::.alert_count_palette(10L), 5L)  # clamps to 5
})

test_that(".ugf_for_overlay reprojects to WGS84", {
  poly <- sf::st_polygon(list(rbind(
    c(800000, 6500000), c(800100, 6500000),
    c(800100, 6500100), c(800000, 6500100),
    c(800000, 6500000)
  )))
  ug <- sf::st_sf(ug_id = 1L,
                  geometry = sf::st_sfc(poly, crs = 2154))
  project <- list(indicators_sf = ug)
  out <- nemetonshiny:::.ugf_for_overlay(project)
  expect_s3_class(out, "sf")
  expect_equal(sf::st_crs(out)$epsg, 4326L)
})

test_that(".ugf_for_overlay returns NULL when indicators_sf is missing", {
  expect_null(nemetonshiny:::.ugf_for_overlay(NULL))
  expect_null(nemetonshiny:::.ugf_for_overlay(list()))
  expect_null(nemetonshiny:::.ugf_for_overlay(list(indicators_sf = NULL)))
})

test_that(".compute_fast_mask forwards index/threshold/mode to the core fn", {
  captured <- NULL
  testthat::local_mocked_bindings(
    compute_fast_alert_mask = function(con, zone_id, index, threshold,
                                       date_from, date_to, mode, window_days,
                                       months, min_years, alpha, cache_dir,
                                       mask_cache_dir, cache_result,
                                       result_cache_dir, parallel) {
      captured <<- list(
        zone_id = zone_id, index = index, threshold = threshold,
        mode = mode, months = months, min_years = min_years, alpha = alpha,
        cache_result = cache_result, parallel = parallel
      )
      "/tmp/mask_NDRE.tif"
    },
    .package = "nemeton"
  )

  out <- nemetonshiny:::.compute_fast_mask(
    con = NULL, zone = "42", index = "NDRE", threshold = 0.2,
    dr = c("2020-06-01", "2024-09-30"), mode = "trend",
    window_days = 30L, months = 6:9, min_years = 4L, alpha = 0.05,
    cache_dir = "/cd", mask_cache = "/mc", result_cache = "/rc"
  )

  expect_equal(out, "/tmp/mask_NDRE.tif")
  expect_equal(captured$zone_id, 42L)        # coerced to integer
  expect_equal(captured$index, "NDRE")
  expect_equal(captured$threshold, 0.2)
  expect_equal(captured$mode, "trend")
  expect_true(captured$cache_result)         # always disk-caches
  expect_true(captured$parallel)
})

# ---------------------------------------------------------------------------
# Clic carte (mode trend) → extract_pixel_trend() (graphe trend par pixel)
# ---------------------------------------------------------------------------

.fast_trend_test_project <- function() {
  proj <- list(path = withr::local_tempdir(.local_envir = parent.frame()))
  cd <- file.path(proj$path, "cache", "layers", "sentinel2")
  dir.create(cd, recursive = TRUE)
  sid <- "S2A_MSIL2A_20250610T103021_T31TGM"
  dir.create(file.path(cd, sid))
  file.create(file.path(cd, sid, "B05.tif"))
  proj
}

test_that("trend-mode map click invokes extract_pixel_trend with xy/index", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")

  captured <- new.env()
  fake_res <- list(
    index = "NDRE",
    composites = data.frame(year = 2019:2024,
                            value = c(0.55, 0.54, 0.50, 0.45, 0.40, 0.36)),
    n_years = 6L, theil_sen_slope = -0.03, theil_sen_intercept = 60,
    mann_kendall_p = 0.02, mann_kendall_tau = -0.7,
    significant_decline = TRUE, alert_value = 0.03, enough_years = TRUE
  )

  testthat::with_mocked_bindings(
    extract_pixel_trend = function(cache_dir, scenes_df, xy, crs = 4326,
                                   index = "NDRE", ...) {
      captured$xy    <- xy
      captured$index <- index
      captured$crs   <- crs
      fake_res
    },
    .package = "nemeton",
    {
      proj <- .fast_trend_test_project()
      shiny::testServer(
        nemetonshiny:::mod_monitoring_fast_alerts_server,
        args = list(
          app_state    = shiny::reactiveValues(language = "fr",
                                               current_project = proj),
          zone_id_r    = shiny::reactive(""),  # court-circuite raster_r (pas de DB)
          date_range_r = shiny::reactive(as.Date(c("2017-01-01", "2025-12-31"))),
          thresholds_r = shiny::reactive(list(ndvi = 0.6, nbr = 0.5, ndmi = 0.2))
        ),
        {
          session$setInputs(mode = "trend", index = "NDRE",
                            trend_months = c(6L, 9L), trend_min_years = 4L,
                            trend_alpha = 0.05,
                            map_click = list(lat = 46.65, lng = 6.11))
          session$flushReact()  # onFlushed exécute show_pixel_trend()
          expect_equal(captured$xy, c(6.11, 46.65))  # (lng, lat)
          expect_equal(captured$index, "NDRE")
          expect_equal(captured$crs, 4326)
        }
      )
    }
  )
})

test_that("non-trend map click does NOT invoke extract_pixel_trend", {
  skip_if_not_installed("shiny")

  called <- new.env(); called$n <- 0L
  testthat::with_mocked_bindings(
    extract_pixel_trend = function(...) { called$n <- called$n + 1L; NULL },
    .package = "nemeton",
    {
      proj <- .fast_trend_test_project()
      shiny::testServer(
        nemetonshiny:::mod_monitoring_fast_alerts_server,
        args = list(
          app_state    = shiny::reactiveValues(language = "fr",
                                               current_project = proj),
          zone_id_r    = shiny::reactive(""),
          date_range_r = shiny::reactive(as.Date(c("2017-01-01", "2025-12-31"))),
          thresholds_r = shiny::reactive(list(ndvi = 0.6, nbr = 0.5, ndmi = 0.2))
        ),
        {
          session$setInputs(mode = "count", index = "NDVI",
                            map_click = list(lat = 46.65, lng = 6.11))
          session$flushReact()
          expect_equal(called$n, 0L)  # ignoré hors mode trend
        }
      )
    }
  )
})

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
  cr <- nemetonshiny:::.fast_index_choices("count")
  expect_setequal(unname(cr), c("NDMI", "NDVI", "NBR", "NDRE"))
  rr <- nemetonshiny:::.fast_index_choices("rolling")
  expect_setequal(unname(rr), c("NDMI", "NDVI", "NBR", "NDRE"))
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

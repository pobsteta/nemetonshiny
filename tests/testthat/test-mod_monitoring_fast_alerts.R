# Tests for the FAST alerts module helpers — v0.45.0.

test_that(".classify_alert_count returns 5 bins for a wide distribution", {
  # 100-cell raster (10x10) : 50 zero pixels + values 1..50.
  r <- terra::rast(nrows = 10, ncols = 10,
                   vals = c(rep(0, 50), seq.int(1, 50)),
                   crs = "EPSG:2154", extent = terra::ext(0, 100, 0, 100))
  cls <- nemetonshiny:::.classify_alert_count(r)
  # 5 distinct quartile values (Q0=1, Q1=13, Q2=25, Q3=38, Q4=50) →
  # 5 bins, 6 break points.
  expect_length(cls$breaks, 6L)
  expect_length(cls$labels, 5L)
  expect_equal(min(cls$breaks), 0.5)
  # Top label carries a "+" sign for the open upper bucket.
  expect_match(cls$labels[length(cls$labels)], "\\+$")
})

test_that(".classify_alert_count collapses to one bucket for a constant raster", {
  r <- terra::rast(nrows = 4, ncols = 4, vals = c(rep(0, 8), rep(3, 8)),
                   crs = "EPSG:2154", extent = terra::ext(0, 40, 0, 40))
  cls <- nemetonshiny:::.classify_alert_count(r)
  expect_length(cls$breaks, 2L)
  expect_length(cls$labels, 1L)
  expect_equal(cls$labels[1], "3")
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

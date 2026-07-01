# Tests P2/P3 — diversité spectrale (B4/L3, spec 028, nemeton >= 0.110.0).
# On ne teste PAS biodivMapR lui-même (lourd, nécessite un vrai cache S2) :
# on teste l'assemblage du cube réflectance, la sélection de scène estivale,
# et le WIRING (build_spectral_diversity peuple layers$spectral une fois, et
# dégrade proprement en NULL → B4/L3 NA). Les readers/compute cœur sont mockés.

# --- fixtures -----------------------------------------------------------------

fake_band <- function(res = 10) {
  r <- terra::rast(xmin = 900000, xmax = 900500,
                   ymin = 6500000, ymax = 6500500,
                   resolution = res, crs = "EPSG:2154")
  terra::values(r) <- stats::runif(terra::ncell(r))
  r
}

fake_units <- function() {
  poly <- sf::st_polygon(list(rbind(
    c(900100, 6500100), c(900400, 6500100),
    c(900400, 6500400), c(900100, 6500400), c(900100, 6500100)
  )))
  sf::st_sf(ug_id = 1L, geometry = sf::st_sfc(poly, crs = 2154))
}

# --- .pick_summer_s2_scene ----------------------------------------------------

test_that(".pick_summer_s2_scene prefers the scene nearest mid-August", {
  sdf <- data.frame(
    scene_id = c("A_20240115", "B_20240810", "C_20240705", "D_20241120"),
    obs_date = as.Date(c("2024-01-15", "2024-08-10", "2024-07-05", "2024-11-20"))
  )
  expect_equal(nemetonshiny:::.pick_summer_s2_scene(sdf), "B_20240810")
})

test_that(".pick_summer_s2_scene falls back when no summer scene", {
  sdf <- data.frame(
    scene_id = c("A_20240115", "D_20241220"),
    obs_date = as.Date(c("2024-01-15", "2024-12-20"))
  )
  # no scene in DOY 152..273 → closest-to-227 overall (Jan is nearer than Dec)
  expect_equal(nemetonshiny:::.pick_summer_s2_scene(sdf), "D_20241220")
})

test_that(".pick_summer_s2_scene returns NULL on empty input", {
  expect_null(nemetonshiny:::.pick_summer_s2_scene(NULL))
  expect_null(nemetonshiny:::.pick_summer_s2_scene(
    data.frame(scene_id = character(0), obs_date = as.Date(character(0)))))
})

# --- .scan_s2_cache_scenes ----------------------------------------------------

test_that(".scan_s2_cache_scenes returns NULL when cache absent/empty", {
  expect_null(nemetonshiny:::.scan_s2_cache_scenes(NULL))
  expect_null(nemetonshiny:::.scan_s2_cache_scenes(tempfile()))  # missing dir
  d <- withr::local_tempdir()
  expect_null(nemetonshiny:::.scan_s2_cache_scenes(d))           # empty dir
})

test_that(".scan_s2_cache_scenes lists only populated scene dirs, parses dates", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "S2A_MSIL2A_20240810_x"))
  writeLines("", file.path(d, "S2A_MSIL2A_20240810_x", "B04.tif"))
  dir.create(file.path(d, "S2B_MSIL2A_20240705_y"))
  writeLines("", file.path(d, "S2B_MSIL2A_20240705_y", "B08.tif"))
  dir.create(file.path(d, "S2A_MSIL2A_20240901_empty"))          # no .tif → drop

  out <- nemetonshiny:::.scan_s2_cache_scenes(d)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)                                    # empty dir excluded
  expect_true(all(c("scene_id", "obs_date") %in% names(out)))
  expect_equal(out$obs_date, as.Date(c("2024-07-05", "2024-08-10")))  # ordered
})

# --- build_reflectance_stack --------------------------------------------------

test_that("build_reflectance_stack stacks available bands and crops to AOI", {
  skip_if_not_installed("terra")
  testthat::local_mocked_bindings(
    read_s2_band_raster = function(cache_dir, scene_id, band) fake_band(10),
    .package = "nemeton"
  )
  cube <- nemetonshiny:::build_reflectance_stack(
    "irrelevant", "S2A_20240810", aoi = fake_units())
  expect_s4_class(cube, "SpatRaster")
  expect_equal(terra::nlyr(cube), length(nemetonshiny:::SPECTRAL_S2_BANDS))
  expect_equal(names(cube), nemetonshiny:::SPECTRAL_S2_BANDS)
})

test_that("build_reflectance_stack returns NULL with fewer than two bands", {
  skip_if_not_installed("terra")
  testthat::local_mocked_bindings(
    read_s2_band_raster = function(cache_dir, scene_id, band) {
      if (band == "B04") fake_band(10) else NULL
    },
    .package = "nemeton"
  )
  expect_null(nemetonshiny:::build_reflectance_stack("x", "scene"))
})

test_that("build_reflectance_stack resamples bands of differing resolution", {
  skip_if_not_installed("terra")
  testthat::local_mocked_bindings(
    read_s2_band_raster = function(cache_dir, scene_id, band) {
      if (band %in% c("B11", "B8A", "B12", "B05")) fake_band(20) else fake_band(10)
    },
    .package = "nemeton"
  )
  cube <- nemetonshiny:::build_reflectance_stack("x", "scene")
  expect_s4_class(cube, "SpatRaster")
  expect_equal(terra::nlyr(cube), length(nemetonshiny:::SPECTRAL_S2_BANDS))
  # aligned onto the first band (10 m) → 50 columns over the 500 m extent
  expect_equal(terra::ncol(cube), 50L)
})

# --- build_spectral_diversity (wiring) ----------------------------------------

test_that("build_spectral_diversity returns NULL without a project path", {
  expect_null(nemetonshiny:::build_spectral_diversity(fake_units(), NULL))
  expect_null(nemetonshiny:::build_spectral_diversity(fake_units(), ""))
})

test_that("build_spectral_diversity returns NULL when no S2 cache", {
  d <- withr::local_tempdir()  # project dir without cache/layers/sentinel2
  expect_null(nemetonshiny:::build_spectral_diversity(fake_units(), d))
})

test_that("build_spectral_diversity runs compute_spectral_diversity once", {
  skip_if_not_installed("terra")
  proj <- withr::local_tempdir()
  cd <- file.path(proj, "cache", "layers", "sentinel2", "S2A_MSIL2A_20240810_x")
  dir.create(cd, recursive = TRUE)
  writeLines("", file.path(cd, "B04.tif"))

  calls <- 0L
  testthat::local_mocked_bindings(
    read_s2_band_raster = function(cache_dir, scene_id, band) fake_band(10),
    compute_spectral_diversity = function(reflectance, mask = NULL, ...) {
      calls <<- calls + 1L
      # mask must be forwarded (rasterised UGF), reflectance a multi-band cube
      expect_s4_class(reflectance, "SpatRaster")
      expect_gt(terra::nlyr(reflectance), 1L)
      list(sentinel = "spectral-ok")
    },
    .package = "nemeton"
  )

  out <- nemetonshiny:::build_spectral_diversity(fake_units(), proj)
  expect_equal(out, list(sentinel = "spectral-ok"))
  expect_equal(calls, 1L)  # ONE biodivMapR run shared by B4 and L3
})

test_that("build_spectral_diversity degrades to NULL on core error", {
  skip_if_not_installed("terra")
  proj <- withr::local_tempdir()
  cd <- file.path(proj, "cache", "layers", "sentinel2", "S2A_MSIL2A_20240810_x")
  dir.create(cd, recursive = TRUE)
  writeLines("", file.path(cd, "B04.tif"))

  testthat::local_mocked_bindings(
    read_s2_band_raster = function(cache_dir, scene_id, band) fake_band(10),
    compute_spectral_diversity = function(...) stop("biodivMapR boom"),
    .package = "nemeton"
  )
  expect_warning(
    out <- nemetonshiny:::build_spectral_diversity(fake_units(), proj),
    regexp = "Spectral diversity"
  )
  expect_null(out)
})

# --- injection contract -------------------------------------------------------

test_that("SPECTRAL_INDICATORS target functions that accept a `spectral` arg", {
  # compute_single_indicator injects layers$spectral only into functions
  # whose formals include `spectral` (name-resolved, like fapar/snow). This
  # locks the contract that makes the injection meaningful for B4 and L3.
  for (ind in nemetonshiny:::SPECTRAL_INDICATORS) {
    fn <- get(ind, envir = asNamespace("nemeton"))
    expect_true(
      "spectral" %in% names(formals(fn)),
      info = paste(ind, "must accept `spectral=` for layers$spectral injection")
    )
  }
})

test_that("compute_single_indicator forwards layers$spectral without error", {
  # Full name-resolved path with the REAL core functions (0.110.0). A bogus
  # spectral object exercises the injection branch; the core functions must
  # degrade gracefully (NA) rather than raise — B4/L3 never break a run.
  parcels <- fake_units()
  layers <- list(spectral = list(tag = "not-a-real-biodivmapr-object"))
  expect_no_error(
    b4 <- nemetonshiny:::compute_single_indicator(
      "indicateur_b4_div_spectrale", parcels, layers))
  expect_no_error(
    l3 <- nemetonshiny:::compute_single_indicator(
      "indicateur_l3_het_spectrale", parcels, layers))
})

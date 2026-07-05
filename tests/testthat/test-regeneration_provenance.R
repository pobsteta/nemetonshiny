# Tests — provenance canopée reGénération (spec 033 D5)
#
# Lecture (jamais recalcul) de la provenance via nemeton::detect_ndp(), et repli
# NDP 0 sur le LAI Sentinel-2/PROSAIL quand le LiDAR HD est absent. Les vraies
# scènes S2 / le modèle PROSAIL ne sont PAS exécutés ici (mocks cœur).

`%||%` <- function(a, b) if (is.null(a)) b else a

.prov_units <- function(n = 2) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- i * 10
    sf::st_polygon(list(matrix(c(
      x0, 0, x0 + 5, 0, x0 + 5, 5, x0, 5, x0, 0), ncol = 2, byrow = TRUE)))
  })
  sf::st_sf(ug_id = seq_len(n), geometry = sf::st_sfc(polys, crs = 2154))
}

.prov_grid <- function(project_path) {
  base <- file.path(project_path, "cache", "layers")
  for (d in c("lidar_mnt", "lidar_mnh")) {
    dd <- file.path(base, d); dir.create(dd, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(dd, "t.tif"))
  }
}

test_that("regen_canopy_provenance reads detect_ndp augmentation flags", {
  skip_if_not_installed("sf")
  u <- .prov_units(1)

  testthat::local_mocked_bindings(
    detect_ndp = function(data) list(augmented = c("height_lidar")), .package = "nemeton")
  expect_equal(nemetonshiny:::regen_canopy_provenance(u), "lidar")

  testthat::local_mocked_bindings(
    detect_ndp = function(data) list(augmented = c("lai_ml", "foo")), .package = "nemeton")
  expect_equal(nemetonshiny:::regen_canopy_provenance(u), "satellite")

  testthat::local_mocked_bindings(
    detect_ndp = function(data) list(augmented = character(0)), .package = "nemeton")
  expect_true(is.na(nemetonshiny:::regen_canopy_provenance(u)))

  # Erreur cœur → NA (dégradation propre, pas de crash).
  testthat::local_mocked_bindings(
    detect_ndp = function(data) stop("boom"), .package = "nemeton")
  expect_true(is.na(nemetonshiny:::regen_canopy_provenance(u)))
})

test_that("run_regeneration_engine reports canopy = lidar when the grid is present", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd(); .prov_grid(p)
    testthat::local_mocked_bindings(regen_cds_credentials_ready = function() TRUE)
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ...) { units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(),
      build_biljou_soil = function(units = NULL, ...) list(),
      regen_bilan_hydrique = function(units, ...) { units$njstress <- 1; units },
      .package = "nemeton")
    out <- nemetonshiny:::run_regeneration_engine(.prov_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))
    expect_equal(out$canopy, "lidar")
  })
})

test_that("run_regeneration_engine falls back to satellite LAI without a grid", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  withr::with_tempdir({
    p <- getwd()  # pas de grille LiDAR
    seen <- new.env()
    lai_r <- terra::rast(xmin = 0, xmax = 100, ymin = 0, ymax = 100,
                         resolution = 10, crs = "EPSG:2154")
    terra::values(lai_r) <- 3.5
    testthat::local_mocked_bindings(regen_cds_credentials_ready = function() TRUE)
    testthat::local_mocked_bindings(
      lai_sentinel2 = function(aoi = NULL, start = NULL, end = NULL, ...) { seen$lai <- TRUE; lai_r },
      load_biljou_forcing = function(aoi, years, ...) data.frame(),
      build_biljou_soil = function(units = NULL, ...) list(),
      regen_bilan_hydrique = function(units, lai_max = NULL, ...) {
        seen$lai_max <- lai_max; units$njstress <- 1; units },
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(.prov_units(2), p,
      cfg = list(year_moyenne = 2018, forcing = "safran"))  # lai_max UI absent

    expect_true(seen$lai)                                  # LAI S2 appelé
    expect_equal(out$canopy, "satellite")
    expect_false(is.null(seen$lai_max))                    # lai_max injecté (agrégé)
    expect_equal(length(seen$lai_max), 2L)                 # un par UGF
    expect_true(file.exists(file.path(p, "cache", "regeneration", "lai_prosail.tif")))
  })
})

test_that("run_regeneration_engine keeps UI lai_max and no satellite when provided", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd()
    seen <- new.env(); seen$lai <- FALSE
    testthat::local_mocked_bindings(regen_cds_credentials_ready = function() TRUE)
    testthat::local_mocked_bindings(
      lai_sentinel2 = function(...) { seen$lai <- TRUE; NULL },
      load_biljou_forcing = function(aoi, years, ...) data.frame(),
      build_biljou_soil = function(units = NULL, ...) list(),
      regen_bilan_hydrique = function(units, lai_max = NULL, ...) { seen$lai_max <- lai_max; units },
      .package = "nemeton")
    out <- nemetonshiny:::run_regeneration_engine(.prov_units(2), p,
      cfg = list(forcing = "safran", lai_max = 4.2))
    expect_false(seen$lai)                                 # pas de repli si lai_max fourni
    expect_equal(seen$lai_max, 4.2)
    expect_true(is.na(out$canopy))                         # ni LiDAR ni satellite
  })
})

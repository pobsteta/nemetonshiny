# Tests du service RVT (fond relief du comparateur de desserte).

test_that(".rvt_cache_path derives a stable name next to the DEM", {
  p <- nemetonshiny:::.rvt_cache_path("/a/b/mnt_highres_1m.tif")
  expect_identical(p, "/a/b/rvt_mnt_highres_1m.tif")
})

test_that(".rvt_py_available returns a single logical, never errors", {
  v <- nemetonshiny:::.rvt_py_available()
  expect_true(is.logical(v) && length(v) == 1L && !is.na(v))
})

test_that("rvt_engine reports cvat, vat or hillshade", {
  expect_true(nemetonshiny:::rvt_engine() %in% c("cvat", "vat", "hillshade"))
})

test_that("generate_rvt returns NULL on missing / bad input", {
  expect_null(nemetonshiny:::generate_rvt(NULL))
  expect_null(nemetonshiny:::generate_rvt("/does/not/exist.tif"))
})

test_that("generate_rvt (terra fallback) produces a normalized relief raster", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    # MNT synthétique : une colline gaussienne -> vrai relief, pas du plat.
    r <- terra::rast(nrows = 60, ncols = 60, xmin = 0, xmax = 60,
                     ymin = 0, ymax = 60, crs = "EPSG:2154")
    xy <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
    z <- 100 + 30 * exp(-((xy[, 1] - 30)^2 + (xy[, 2] - 30)^2) / 200)
    terra::values(r) <- z
    terra::writeRaster(r, "mnt.tif", overwrite = TRUE)

    # Force le repli terra (pas de rvt-py en CI).
    testthat::local_mocked_bindings(.rvt_py_available = function() FALSE,
                                    .rvt_cvat_available = function() FALSE)
    out <- nemetonshiny:::generate_rvt("mnt.tif")
    expect_true(!is.null(out) && file.exists(out))
    expect_match(out, "rvt_mnt\\.tif$")

    rr <- terra::rast(out)
    rng <- c(terra::global(rr, "min", na.rm = TRUE)[[1]],
             terra::global(rr, "max", na.rm = TRUE)[[1]])
    expect_gte(rng[1], 0)
    expect_lte(rng[2], 1)
    expect_gt(rng[2] - rng[1], 0.1)          # du contraste, pas une image plate
    expect_equal(terra::nrow(rr), 60)        # même géométrie que le MNT
  })
})

test_that("generate_rvt reuses the cache on a second call", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    r <- terra::rast(nrows = 30, ncols = 30, crs = "EPSG:2154")
    terra::values(r) <- as.numeric(seq_len(terra::ncell(r)))
    terra::writeRaster(r, "mnt.tif", overwrite = TRUE)
    testthat::local_mocked_bindings(.rvt_py_available = function() FALSE,
                                    .rvt_cvat_available = function() FALSE)
    p1 <- nemetonshiny:::generate_rvt("mnt.tif")
    m1 <- file.mtime(p1)
    Sys.sleep(0.05)
    p2 <- nemetonshiny:::generate_rvt("mnt.tif")     # cache -> pas de réécriture
    expect_identical(p1, p2)
    expect_identical(file.mtime(p2), m1)
    # overwrite = TRUE force la régénération.
    p3 <- nemetonshiny:::generate_rvt("mnt.tif", overwrite = TRUE)
    expect_true(file.mtime(p3) >= m1)
  })
})

# --- Moteur CVAT (foretaccess >= 1.24.0) -------------------------------------

test_that("rvt_engine reports cvat when foretaccess::vat_combined exists", {
  testthat::local_mocked_bindings(.rvt_cvat_available = function() TRUE)
  expect_identical(nemetonshiny:::rvt_engine(), "cvat")
})

test_that("rvt_engine falls back to vat then hillshade without CVAT", {
  testthat::local_mocked_bindings(
    .rvt_cvat_available = function() FALSE,
    .rvt_py_available = function() TRUE)
  expect_identical(nemetonshiny:::rvt_engine(), "vat")
  testthat::local_mocked_bindings(
    .rvt_cvat_available = function() FALSE,
    .rvt_py_available = function() FALSE)
  expect_identical(nemetonshiny:::rvt_engine(), "hillshade")
})

test_that("generate_rvt prefers the CVAT engine over rvt-py and terra", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    r <- terra::rast(nrows = 20, ncols = 20, crs = "EPSG:2154")
    terra::values(r) <- as.numeric(seq_len(terra::ncell(r)))
    terra::writeRaster(r, "mnt.tif", overwrite = TRUE)
    cvat_called <- FALSE
    testthat::local_mocked_bindings(
      .rvt_cvat_ft = function(mnt) {
        cvat_called <<- TRUE
        out <- mnt; terra::values(out) <- runif(terra::ncell(out)); out
      },
      # terra ne doit PAS être appelé si le CVAT rend un raster.
      .rvt_terra = function(mnt) stop("terra fallback ne doit pas etre appele"),
      .rvt_py_available = function() FALSE)
    out <- nemetonshiny:::generate_rvt("mnt.tif")
    expect_true(cvat_called)
    expect_true(!is.null(out) && file.exists(out))
  })
})

test_that("generate_rvt falls back to terra when CVAT is unavailable", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    r <- terra::rast(nrows = 20, ncols = 20, crs = "EPSG:2154")
    terra::values(r) <- as.numeric(seq_len(terra::ncell(r)))
    terra::writeRaster(r, "mnt.tif", overwrite = TRUE)
    testthat::local_mocked_bindings(
      .rvt_cvat_ft = function(mnt) NULL,     # CVAT indisponible
      .rvt_py_available = function() FALSE)
    out <- nemetonshiny:::generate_rvt("mnt.tif")   # -> terra
    expect_true(!is.null(out) && file.exists(out))
  })
})

# --- Source MNT et réutilisation du CVAT pré-calculé -------------------------

test_that(".rvt_precomputed reuses an 8-bit CVAT next to the DEM, rescaled to [0,1]", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    mnt <- terra::rast(nrows = 10, ncols = 10, crs = "EPSG:2154")
    terra::values(mnt) <- 1
    terra::writeRaster(mnt, "lidar_mnt_mosaic.tif", overwrite = TRUE)
    # CVAT 8bit pré-calculé à côté (valeurs 0-255)
    cvat8 <- terra::rast(nrows = 10, ncols = 10, crs = "EPSG:2154")
    terra::values(cvat8) <- rep(c(0, 128, 255), length.out = 100)
    terra::writeRaster(cvat8, "lidar_mnt_mosaic_CVAT_8bit_foretaccess.tif",
                       overwrite = TRUE)
    out <- nemetonshiny:::.rvt_precomputed("lidar_mnt_mosaic.tif")
    expect_false(is.null(out))
    rng <- c(terra::global(out, "min", na.rm = TRUE)[[1]],
             terra::global(out, "max", na.rm = TRUE)[[1]])
    expect_gte(rng[1], 0); expect_lte(rng[2], 1)     # rescalé
    expect_gt(rng[2], 0.9)                            # 255/255 ~ 1
  })
})

test_that(".rvt_precomputed returns NULL when no precomputed CVAT exists", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    m <- terra::rast(nrows = 5, ncols = 5); terra::values(m) <- 1
    terra::writeRaster(m, "mnt.tif", overwrite = TRUE)
    expect_null(nemetonshiny:::.rvt_precomputed("mnt.tif"))
    expect_null(nemetonshiny:::.rvt_precomputed(NULL))
  })
})

test_that("generate_rvt adopts the precomputed CVAT before any live compute", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    mnt <- terra::rast(nrows = 8, ncols = 8, crs = "EPSG:2154")
    terra::values(mnt) <- as.numeric(seq_len(64))
    terra::writeRaster(mnt, "lidar_mnt_mosaic.tif", overwrite = TRUE)
    cvat8 <- mnt; terra::values(cvat8) <- rep(200, 64)
    terra::writeRaster(cvat8, "lidar_mnt_mosaic_CVAT_8bit.tif", overwrite = TRUE)
    testthat::local_mocked_bindings(
      .rvt_cvat_ft = function(mnt) stop("live CVAT ne doit pas etre appele"),
      .rvt_terra = function(mnt) stop("terra ne doit pas etre appele"),
      .rvt_py_available = function() FALSE)
    out <- nemetonshiny:::generate_rvt("lidar_mnt_mosaic.tif", overwrite = TRUE)
    expect_true(!is.null(out) && file.exists(out))
  })
})

test_that(".acc_rvt_mnt_path prefers the native 0.5 m LiDAR DTM over the WMS DEM", {
  withr::with_tempdir({
    proj <- getwd()
    lyr <- file.path(proj, "cache", "layers")
    emp <- file.path(proj, "cache", "accessibility", "emprise_1000m")
    dir.create(lyr, recursive = TRUE); dir.create(emp, recursive = TRUE)
    writeLines("x", file.path(lyr, "lidar_mnt_mosaic.tif"))
    writeLines("x", file.path(emp, "mnt_highres_1m.tif"))
    got <- nemetonshiny:::.acc_rvt_mnt_path(proj)
    expect_match(got, "lidar_mnt_mosaic\\.tif$")
    # Sans le LiDAR : repli sur le WMS.
    file.remove(file.path(lyr, "lidar_mnt_mosaic.tif"))
    expect_match(nemetonshiny:::.acc_rvt_mnt_path(proj), "mnt_highres_1m\\.tif$")
  })
})

test_that(".rvt_is_cheap is TRUE with a precomputed CVAT, FALSE otherwise", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    m <- terra::rast(nrows = 5, ncols = 5); terra::values(m) <- 1
    terra::writeRaster(m, "lidar_mnt_mosaic.tif", overwrite = TRUE)
    expect_false(nemetonshiny:::.rvt_is_cheap("lidar_mnt_mosaic.tif"))
    # CVAT pré-calculé à côté -> cheap
    terra::writeRaster(m, "lidar_mnt_mosaic_CVAT_8bit.tif", overwrite = TRUE)
    expect_true(nemetonshiny:::.rvt_is_cheap("lidar_mnt_mosaic.tif"))
    expect_false(nemetonshiny:::.rvt_is_cheap(NULL))
  })
})

test_that(".rvt_is_cheap is TRUE once the RVT cache exists", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    m <- terra::rast(nrows = 5, ncols = 5); terra::values(m) <- 1
    terra::writeRaster(m, "mnt.tif", overwrite = TRUE)
    expect_false(nemetonshiny:::.rvt_is_cheap("mnt.tif"))
    terra::writeRaster(m, nemetonshiny:::.rvt_cache_path("mnt.tif"), overwrite = TRUE)
    expect_true(nemetonshiny:::.rvt_is_cheap("mnt.tif"))
  })
})

# --- Producteur du CVAT pré-calculé (build_cvat_precomputed) -----------------

test_that("build_cvat_precomputed returns NULL without foretaccess >= 1.24.0", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    m <- terra::rast(nrows = 5, ncols = 5); terra::values(m) <- 1
    terra::writeRaster(m, "lidar_mnt_mosaic.tif", overwrite = TRUE)
    testthat::local_mocked_bindings(.rvt_cvat_available = function() FALSE)
    expect_null(nemetonshiny:::build_cvat_precomputed("lidar_mnt_mosaic.tif"))
  })
  expect_null(nemetonshiny:::build_cvat_precomputed(NULL))
  expect_null(nemetonshiny:::build_cvat_precomputed("/nope.tif"))
})

test_that("build_cvat_precomputed writes <base>_CVAT_8bit_foretaccess.tif", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    m <- terra::rast(nrows = 8, ncols = 8, crs = "EPSG:2154")
    terra::values(m) <- as.numeric(seq_len(64))
    terra::writeRaster(m, "lidar_mnt_mosaic.tif", overwrite = TRUE)
    # Mock vat_combined pour ne pas dépendre de foretaccess dans ce test.
    testthat::local_mocked_bindings(.rvt_cvat_available = function() TRUE)
    testthat::local_mocked_bindings(
      vat_combined = function(mnt, as_byte = TRUE) {
        r <- mnt; terra::values(r) <- rep(c(0L, 128L, 255L), length.out = 64L); r
      }, .package = "foretaccess")
    out <- nemetonshiny:::build_cvat_precomputed("lidar_mnt_mosaic.tif")
    expect_true(!is.null(out) && file.exists(out))
    expect_match(out, "lidar_mnt_mosaic_CVAT_8bit_foretaccess\\.tif$")
    # idempotent : 2e appel sans overwrite ne réécrit pas
    m1 <- file.mtime(out); Sys.sleep(0.05)
    out2 <- nemetonshiny:::build_cvat_precomputed("lidar_mnt_mosaic.tif")
    expect_identical(out2, out)
    expect_identical(file.mtime(out2), m1)
    # ce fichier rend .rvt_is_cheap TRUE
    expect_true(nemetonshiny:::.rvt_is_cheap("lidar_mnt_mosaic.tif"))
  })
})

test_that("build_cvat_precomputed delegates to foretaccess when an AOI is given", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    m <- terra::rast(nrows = 5, ncols = 5, crs = "EPSG:2154"); terra::values(m) <- 1
    terra::writeRaster(m, "lidar_mnt_mosaic.tif", overwrite = TRUE)
    testthat::local_mocked_bindings(.rvt_cvat_available = function() TRUE)
    delegated <- FALSE
    testthat::local_mocked_bindings(
      build_cvat_precomputed = function(aoi, cache_dir, buffer_m, mnt_existant,
                                        out, overwrite) {
        delegated <<- TRUE
        writeLines("x", out); out            # simule l'écriture foretaccess
      }, .package = "foretaccess")
    aoi <- sf::st_sf(geometry = sf::st_sfc(
      sf::st_point(c(8e5, 63e5)), crs = 2154))
    out <- nemetonshiny:::build_cvat_precomputed("lidar_mnt_mosaic.tif",
                                                 aoi = aoi, buffer_m = 100)
    expect_true(delegated)                   # AOI -> délégation foretaccess
    expect_match(out, "_CVAT_8bit_foretaccess\\.tif$")
  })
})

test_that("build_cvat_precomputed without AOI keeps the local vat_combined path", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    m <- terra::rast(nrows = 6, ncols = 6, crs = "EPSG:2154")
    terra::values(m) <- as.numeric(seq_len(36))
    terra::writeRaster(m, "lidar_mnt_mosaic.tif", overwrite = TRUE)
    testthat::local_mocked_bindings(.rvt_cvat_available = function() TRUE)
    ft_called <- FALSE
    testthat::local_mocked_bindings(
      vat_combined = function(mnt, as_byte = TRUE) {
        ft_called <<- TRUE; r <- mnt; terra::values(r) <- 100L; r
      }, .package = "foretaccess")
    out <- nemetonshiny:::build_cvat_precomputed("lidar_mnt_mosaic.tif")  # pas d'AOI
    expect_true(ft_called)                   # chemin local vat_combined
    expect_true(!is.null(out) && file.exists(out))
  })
})

test_that(".cvat_covers is TRUE when the raster extent contains AOI + buffer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  withr::with_tempdir({
    # CVAT couvrant [8e5, 8.01e5] x [63e5, 63.01e5] (1 km de côté)
    r <- terra::rast(xmin = 8e5, xmax = 8.01e5, ymin = 63e5, ymax = 63.01e5,
                     resolution = 10, crs = "EPSG:2154")
    terra::values(r) <- 1
    terra::writeRaster(r, "cvat.tif", overwrite = TRUE)
    aoi <- sf::st_sf(geometry = sf::st_sfc(
      sf::st_point(c(8.005e5, 63.005e5)), crs = 2154))       # centre
    expect_true(nemetonshiny:::.cvat_covers("cvat.tif", aoi, buffer_m = 100))
    # un buffer qui déborde l'emprise -> non couvert
    expect_false(nemetonshiny:::.cvat_covers("cvat.tif", aoi, buffer_m = 5000))
  })
})

test_that(".cvat_covers is FALSE for a missing file or empty AOI", {
  skip_if_not_installed("terra")
  expect_false(nemetonshiny:::.cvat_covers(NULL, NULL))
  expect_false(nemetonshiny:::.cvat_covers("does_not_exist.tif", NULL))
})

test_that("build_cvat_precomputed skips foretaccess when the CVAT covers AOI+buffer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  withr::with_tempdir({
    m <- terra::rast(nrows = 5, ncols = 5, crs = "EPSG:2154"); terra::values(m) <- 1
    terra::writeRaster(m, "lidar_mnt_mosaic.tif", overwrite = TRUE)
    # CVAT pré-existant couvrant largement l'AOI+buffer
    out <- "lidar_mnt_mosaic_CVAT_8bit_foretaccess.tif"
    cov <- terra::rast(xmin = 8e5, xmax = 8.01e5, ymin = 63e5, ymax = 63.01e5,
                       resolution = 10, crs = "EPSG:2154")
    terra::values(cov) <- 1; terra::writeRaster(cov, out, overwrite = TRUE)
    testthat::local_mocked_bindings(.rvt_cvat_available = function() TRUE)
    delegated <- FALSE
    testthat::local_mocked_bindings(
      build_cvat_precomputed = function(...) { delegated <<- TRUE; out },
      .package = "foretaccess")
    aoi <- sf::st_sf(geometry = sf::st_sfc(
      sf::st_point(c(8.005e5, 63.005e5)), crs = 2154))
    res <- nemetonshiny:::build_cvat_precomputed("lidar_mnt_mosaic.tif",
                                                 aoi = aoi, buffer_m = 100)
    expect_false(delegated)                  # couverture OK -> pas de recalcul
    expect_match(res, "lidar_mnt_mosaic_CVAT_8bit_foretaccess\\.tif$")
  })
})

test_that("build_cvat_precomputed forces recalc when the CVAT is too short for the buffer", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  withr::with_tempdir({
    m <- terra::rast(nrows = 5, ncols = 5, crs = "EPSG:2154"); terra::values(m) <- 1
    terra::writeRaster(m, "lidar_mnt_mosaic.tif", overwrite = TRUE)
    out <- "lidar_mnt_mosaic_CVAT_8bit_foretaccess.tif"
    cov <- terra::rast(xmin = 8e5, xmax = 8.01e5, ymin = 63e5, ymax = 63.01e5,
                       resolution = 10, crs = "EPSG:2154")
    terra::values(cov) <- 1; terra::writeRaster(cov, out, overwrite = TRUE)
    testthat::local_mocked_bindings(.rvt_cvat_available = function() TRUE)
    forced <- NULL
    testthat::local_mocked_bindings(
      build_cvat_precomputed = function(aoi, cache_dir, buffer_m, mnt_existant,
                                        out, overwrite) {
        forced <<- overwrite; out
      }, .package = "foretaccess")
    aoi <- sf::st_sf(geometry = sf::st_sfc(
      sf::st_point(c(8.005e5, 63.005e5)), crs = 2154))
    nemetonshiny:::build_cvat_precomputed("lidar_mnt_mosaic.tif",
                                          aoi = aoi, buffer_m = 5000)  # déborde
    expect_true(isTRUE(forced))              # recalcul forcé (overwrite=TRUE)
  })
})

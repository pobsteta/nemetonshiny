# Tests du service RVT (fond relief du comparateur de desserte).

test_that(".rvt_cache_path derives a stable name next to the DEM", {
  p <- nemetonshiny:::.rvt_cache_path("/a/b/mnt_highres_1m.tif")
  expect_identical(p, "/a/b/rvt_mnt_highres_1m.tif")
})

test_that(".rvt_py_available returns a single logical, never errors", {
  v <- nemetonshiny:::.rvt_py_available()
  expect_true(is.logical(v) && length(v) == 1L && !is.na(v))
})

test_that("rvt_engine reports vat or hillshade", {
  expect_true(nemetonshiny:::rvt_engine() %in% c("vat", "hillshade"))
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
    testthat::local_mocked_bindings(.rvt_py_available = function() FALSE)
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
    testthat::local_mocked_bindings(.rvt_py_available = function() FALSE)
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

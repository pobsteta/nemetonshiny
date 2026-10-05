# Relief RVT : ordre des cellules et cache perime (audit 1.0)

test_that("terra cell order round-trips through a by-row matrix", {
  r <- terra::rast(nrows = 2, ncols = 3, vals = 1:6)
  m <- matrix(terra::values(r, mat = FALSE), nrow = 2, ncol = 3, byrow = TRUE)
  expect_identical(m[1, ], c(1L, 2L, 3L))   # premiere ligne de l'image
  out <- r
  terra::values(out) <- as.numeric(t(m))
  expect_equal(as.numeric(terra::values(out)), as.numeric(1:6))
})

test_that("an RVT cache older than its DEM is regenerated", {
  d <- withr::local_tempdir()
  mnt <- file.path(d, "mnt.tif")
  terra::writeRaster(terra::rast(nrows = 3, ncols = 3, vals = 1:9, crs = "EPSG:2154"), mnt)
  out <- .rvt_cache_path(mnt)
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  file.create(out)
  Sys.setFileTime(out, Sys.time() - 3600)
  expect_true(.rvt_cache_perime(out, mnt))
  Sys.setFileTime(out, Sys.time() + 3600)
  expect_false(.rvt_cache_perime(out, mnt))
})

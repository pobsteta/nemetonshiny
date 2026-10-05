# Caches de couches cles sur l'emprise (audit 1.0, constat majeur)

.mk_raster <- function(path, ext = c(5, 6, 47, 48)) {
  r <- terra::rast(xmin = ext[1], xmax = ext[2], ymin = ext[3], ymax = ext[4],
                   resolution = 0.01, crs = "EPSG:4326", vals = 1)
  terra::writeRaster(r, path, overwrite = TRUE)
  path
}

test_that(".bbox_covers compares WGS84 extents", {
  expect_true(.bbox_covers(c(5, 47, 6, 48), c(5.2, 47.2, 5.8, 47.8)))
  expect_false(.bbox_covers(c(5, 47, 6, 48), c(5.2, 47.2, 6.1, 47.8)))
})

test_that("a raster cache is reused only while it covers the extent", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  n <- 0L
  local_mocked_bindings(download_ign_dem = function(bbox, cache_file) {
    n <<- n + 1L
    .mk_raster(cache_file, ext = bbox[c(1, 3, 2, 4)])
    terra::rast(cache_file)
  })
  cfg <- list(source = "ign_bd_alti")
  b1 <- c(5, 47, 6, 48)
  download_raster_source("dem", cfg, b1, d)
  expect_identical(n, 1L)
  expect_true(file.exists(file.path(d, "dem.tif.bbox.json")))
  # Emprise incluse : cache reutilise
  download_raster_source("dem", cfg, c(5.2, 47.2, 5.8, 47.8), d)
  expect_identical(n, 1L)
  # Emprise elargie : nouveau telechargement
  b2 <- c(5, 47, 6.5, 48)
  suppressMessages(download_raster_source("dem", cfg, b2, d))
  expect_identical(n, 2L)
  side <- jsonlite::read_json(file.path(d, "dem.tif.bbox.json"))
  expect_equal(as.numeric(unlist(side$bbox)), b2)
  expect_false(file.exists(file.path(d, "dem.tif.perime")))
})

test_that("a legacy raster cache without sidecar is adopted when it covers", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  .mk_raster(file.path(d, "dem.tif"), ext = c(4, 7, 46, 49))
  local_mocked_bindings(download_ign_dem = function(...) stop("ne doit pas telecharger"))
  r <- download_raster_source("dem", list(source = "ign_bd_alti"), c(5, 47, 6, 48), d)
  expect_s4_class(r, "SpatRaster")
  expect_true(file.exists(file.path(d, "dem.tif.bbox.json")))
})

test_that("a failed re-download restores the previous layer", {
  skip_if_not_installed("terra")
  d <- withr::local_tempdir()
  .mk_raster(file.path(d, "dem.tif"), ext = c(5, 6, 47, 48))
  local_mocked_bindings(download_ign_dem = function(bbox, cache_file) NULL)
  expect_warning(
    r <- suppressMessages(download_raster_source("dem", list(source = "ign_bd_alti"),
                                                 c(5, 47, 7, 48), d)),
    "conserv")
  expect_s4_class(r, "SpatRaster")
  expect_true(file.exists(file.path(d, "dem.tif")))
})

test_that("a legacy vector cache is downloaded again, then keyed", {
  d <- withr::local_tempdir()
  pt <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_point(c(5.5, 47.5)), crs = 4326))
  sf::st_write(pt, file.path(d, "roads.gpkg"), quiet = TRUE)
  n <- 0L
  local_mocked_bindings(download_ign_bdtopo = function(source_name, bbox, cache_file) {
    n <<- n + 1L
    sf::st_write(pt, cache_file, quiet = TRUE, delete_dsn = TRUE)
    pt
  })
  cfg <- list(source = "ign_bd_topo")
  suppressMessages(download_vector_source("roads", cfg, c(5, 47, 6, 48), d))
  expect_identical(n, 1L)
  download_vector_source("roads", cfg, c(5, 47, 6, 48), d)
  expect_identical(n, 1L)
})

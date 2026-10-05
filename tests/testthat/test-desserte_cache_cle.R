# Cache desserte cle sur le tampon et l'emprise ; moteurs non melanges (audit 1.0)

.dc_parcelles <- function(n = 2) {
  sq <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0), c(x + 100, 100),
                                              c(x, 100), c(x, 0))))
  sf::st_sf(id = seq_len(n), geometry = sf::st_sfc(lapply(seq_len(n) * 200 + 800000, sq),
                                                   crs = 2154))
}

.dc_cache <- function(root, engine, meta, age = 0) {
  cd <- .desserte_cache_dir(root)
  dir.create(cd, recursive = TRUE, showWarnings = FALSE)
  r <- terra::rast(nrows = 2, ncols = 2, vals = 1)
  terra::writeRaster(r, file.path(cd, paste0("reseau_", engine, ".tif")), overwrite = TRUE)
  f <- file.path(cd, paste0("reseau_", engine, ".rds"))
  saveRDS(c(meta, list(pondere_cout = TRUE)), f)
  Sys.setFileTime(f, Sys.time() - age)
  invisible(cd)
}

test_that(".desserte_empreinte changes with the parcels, not with order of calls", {
  a <- .desserte_empreinte(.dc_parcelles(2))
  expect_identical(a, .desserte_empreinte(.dc_parcelles(2)))
  expect_false(identical(a, .desserte_empreinte(.dc_parcelles(3))))
  expect_true(is.na(.desserte_empreinte(NULL)))
})

test_that("a cached network computed on another buffer or AOI is not served", {
  skip_if_not_installed("terra")
  root <- withr::local_tempdir()
  emp <- .desserte_empreinte(.dc_parcelles(2))
  base <- list(skidding_m = 300, methode_pente = "bareme", largeur_m = 4, pente_max_pct = 12)
  .dc_cache(root, "glouton", c(base, list(buffer_m = 1000, aoi_empreinte = emp)))
  ok <- c(base, list(buffer_m = 1000, aoi_empreinte = emp))
  expect_identical(.load_cached_desserte(root, ok)$engine, "glouton")
  expect_null(.load_cached_desserte(root, modifyList(ok, list(buffer_m = 500))))
  expect_null(.load_cached_desserte(root, modifyList(ok, list(
    aoi_empreinte = .desserte_empreinte(.dc_parcelles(3))))))
  # Cache anterieur sans ces champs : diverge (on ne peut pas l'affirmer a jour)
  .dc_cache(root, "glouton", base)
  expect_null(.load_cached_desserte(root, ok))
})

test_that("the engine just run, else the most recent one, is reloaded", {
  skip_if_not_installed("terra")
  root <- withr::local_tempdir()
  .dc_cache(root, "glouton", list(), age = 100)
  .dc_cache(root, "steiner", list(), age = 0)
  expect_identical(.load_cached_desserte(root)$engine, "steiner")
  expect_identical(.load_cached_desserte(root, engine = "glouton")$engine, "glouton")
  r <- .load_cached_desserte(root, engine = "glouton")
  expect_null(r$gpkg_path)   # pas de GeoPackage glouton : pas celui de Steiner
  cd <- .desserte_cache_dir(root)
  sf::st_write(.dc_parcelles(1), .desserte_gpkg_path(cd, "steiner"), quiet = TRUE)
  expect_identical(basename(.load_cached_desserte(root)$gpkg_path), "desserte_steiner.gpkg")
  expect_identical(basename(.desserte_gpkg_courant(cd)), "desserte_steiner.gpkg")
})

test_that("the detection memory guard counts cells at the detection resolution", {
  aoi <- .dc_parcelles(2)
  local_mocked_bindings(.available_memory_bytes = function() 64 * 1024^3)
  a <- .desserte_detection_memory_check(aoi, res_m = 1)
  b <- .desserte_detection_memory_check(aoi, res_m = 0.5)
  expect_gt(b$cells, 3 * a$cells)
  expect_equal(a$bytes, a$cells * DESSERTE_DETECTION_BYTES_PER_CELL)
  local_mocked_bindings(.available_memory_bytes = function() 1)
  expect_false(.desserte_detection_memory_check(aoi, res_m = 1)$ok)
  expect_identical(.desserte_detection_res(NULL), 1)
  d <- withr::local_tempdir()
  f <- file.path(d, "mnt.tif")
  terra::writeRaster(terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 2, ymin = 0, ymax = 2,
                                 crs = "EPSG:2154", vals = 1), f)
  expect_equal(.desserte_detection_res(f), 0.5)
})

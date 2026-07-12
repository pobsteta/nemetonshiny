# Tests — raster de contexte régional E-OBS (eobs_downscale, brief 027 eobs-context-dem).
#
# run_regeneration_context_raster orchestre load_eobs_source -> eobs_downscale
# (dem = NULL, auto-source cœur), cache .tif + meta.json ; regeneration_context_
# cached relit sans re-kriger. Cœur mocké — aucune logique métier testée ici.

`%||%` <- function(a, b) if (is.null(a)) b else a

.ctx_units <- function(n = 2) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- i * 10
    sf::st_polygon(list(matrix(c(
      x0, 0, x0 + 5, 0, x0 + 5, 5, x0, 5, x0, 0), ncol = 2, byrow = TRUE)))
  })
  sf::st_sf(ug_id = seq_len(n), geometry = sf::st_sfc(polys, crs = 2154))
}

test_that("tx absent -> status need_tx, aucun appel au cœur", {
  skip_if_not_installed("sf")
  u <- .ctx_units(2); pp <- withr::local_tempdir()
  called <- new.env(); called$ds <- FALSE
  testthat::local_mocked_bindings(
    regen_eobs_cached_nc = function(project_path, var = "tx") NULL,
    .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    eobs_downscale = function(...) { called$ds <- TRUE; NULL }, .package = "nemeton")

  res <- nemetonshiny:::run_regeneration_context_raster(u, pp)
  expect_identical(res$meta$status, "need_tx")
  expect_true(is.na(res$cache_path))
  expect_false(called$ds)                    # eobs_downscale jamais appelé sans tx
})

test_that("eobs_downscale dégradé -> status propagé, meta.json écrit, cache NA", {
  skip_if_not_installed("sf")
  u <- .ctx_units(2); pp <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    regen_eobs_cached_nc = function(project_path, var = "tx") "FAKE.nc",
    .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    load_eobs_source = function(aoi, var, nc, ...) "TX_STACK",
    eobs_downscale = function(...) list(raster = NULL,
      meta = list(status = "insufficient_data", reason = "eobs_downscale_too_few_cells")),
    .package = "nemeton")

  res <- nemetonshiny:::run_regeneration_context_raster(u, pp)
  expect_identical(res$meta$status, "insufficient_data")
  expect_identical(res$meta$reason, "eobs_downscale_too_few_cells")
  expect_true(is.na(res$cache_path))         # pas de raster -> pas de chemin
  # meta persisté même en dégradé (pour ré-afficher le motif sans re-kriger).
  paths <- nemetonshiny:::.regen_context_raster_paths(pp, "trend")
  expect_true(file.exists(paths$meta))
})

test_that("eobs_downscale qui lève -> repli no_dem, jamais d'erreur", {
  skip_if_not_installed("sf")
  u <- .ctx_units(1); pp <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    regen_eobs_cached_nc = function(project_path, var = "tx") "FAKE.nc",
    .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    load_eobs_source = function(...) "TX_STACK",
    eobs_downscale = function(...) stop("WMS down"), .package = "nemeton")

  res <- nemetonshiny:::run_regeneration_context_raster(u, pp)
  expect_identical(res$meta$status, "insufficient_data")
  expect_identical(res$meta$reason, "eobs_downscale_no_dem")
})

test_that("regeneration_context_cached relit .tif + meta.json (fast-path)", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  pp <- withr::local_tempdir()
  paths <- nemetonshiny:::.regen_context_raster_paths(pp, "trend")
  dir.create(paths$dir, recursive = TRUE)
  # Petit raster réel + meta.
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 4, crs = "EPSG:2154")
  terra::values(r) <- runif(16)
  terra::writeRaster(r, paths$tif, overwrite = TRUE)
  jsonlite::write_json(list(status = "ok", value_label = "Tendance T°max estivale",
    palette = list(low = 1.0, high = 1.5, sense = "hot_unfavorable")),
    paths$meta, auto_unbox = TRUE)

  cached <- nemetonshiny:::regeneration_context_cached(pp, "trend")
  expect_true(inherits(cached$raster, "SpatRaster"))
  expect_identical(cached$meta$status, "ok")
  expect_identical(cached$meta$palette$sense, "hot_unfavorable")

  # Cache absent -> NULL.
  expect_null(nemetonshiny:::regeneration_context_cached(withr::local_tempdir(), "trend"))
})

# Caches reutilises sur la seule existence du fichier (brief LiDAR HD du
# 2026-10-07, point 5) : irc.tif, ndvi_s2_v2.tif, spectral/<scene>/,
# opencanopy/chm_*.tif. Et le nombre de workers lasR borne par la memoire.

skip_if_not_installed("sf")
skip_if_not_installed("terra")

.carre <- function(x0, cote = 0.01) {
  sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(x0, 47), c(x0 + cote, 47), c(x0 + cote, 47 + cote), c(x0, 47 + cote),
    c(x0, 47)))), crs = 4326))
}

test_that("la cle de cache suit la geometrie, pas le CRS de travail", {
  a <- .carre(4.9)
  expect_identical(nemetonshiny:::.cache_cle_geometrie(a),
                   nemetonshiny:::.cache_cle_geometrie(a))
  expect_false(identical(nemetonshiny:::.cache_cle_geometrie(a),
                         nemetonshiny:::.cache_cle_geometrie(.carre(4.95))))
  withr::with_tempdir({
    writeLines("x", "produit.tif")
    cle <- nemetonshiny:::.cache_cle_geometrie(a)
    # Sans fichier .cle : produit anterieur, non fiable.
    expect_false(nemetonshiny:::.cache_cle_valide("produit.tif", cle))
    nemetonshiny:::.cache_cle_ecrire("produit.tif", cle)
    expect_true(nemetonshiny:::.cache_cle_valide("produit.tif", cle))
    expect_false(nemetonshiny:::.cache_cle_valide(
      "produit.tif", nemetonshiny:::.cache_cle_geometrie(.carre(4.95))))
  })
})

test_that("le composite NDVI S2 d'une autre emprise n'est pas relu", {
  withr::with_tempdir({
    r <- terra::rast(nrows = 5, ncols = 5, xmin = 4.9, xmax = 4.92,
                     ymin = 47, ymax = 47.02, crs = "EPSG:4326", vals = 0.8)
    terra::writeRaster(r, "ndvi_s2_v2.tif")
    nemetonshiny:::.cache_cle_ecrire("ndvi_s2_v2.tif",
                                     nemetonshiny:::.cache_cle_geometrie(.carre(4.9)))
    appels <- 0L
    local_mocked_bindings(
      .scan_s2_cache_scenes = function(...) data.frame(scene = "s1"),
      .package = "nemetonshiny")
    local_mocked_bindings(
      build_ndvi_season_composite = function(...) { appels <<- appels + 1L; r },
      .package = "nemeton")
    # Meme emprise : lu depuis le cache.
    suppressMessages(nemetonshiny:::build_s2_ndvi_layer(getwd(), aoi = .carre(4.9)))
    expect_equal(appels, 0L)
    # Autre emprise : recalcule, et la nouvelle cle est ecrite.
    suppressMessages(nemetonshiny:::build_s2_ndvi_layer(getwd(), aoi = .carre(4.95)))
    expect_equal(appels, 1L)
    expect_true(nemetonshiny:::.cache_cle_valide(
      "ndvi_s2_v2.tif", nemetonshiny:::.cache_cle_geometrie(.carre(4.95))))
  })
})

test_that("une ortho IRC d'une autre emprise n'est pas reutilisee", {
  withr::with_tempdir({
    r <- terra::rast(nrows = 50, ncols = 50, xmin = 4.9, xmax = 4.91,
                     ymin = 47, ymax = 47.01, crs = "EPSG:4326", vals = 100)
    terra::writeRaster(r, "irc.tif")
    # irc.tif ne couvre pas l'emprise demandee, plus a l'est.
    expect_false(nemetonshiny:::.layer_cache_reusable(
      "irc.tif", c(4.95, 47, 4.96, 47.01), "raster"))
    expect_true(nemetonshiny:::.layer_cache_reusable(
      "irc.tif", c(4.901, 47.001, 4.909, 47.009), "raster"))
  })
})

test_that("le nombre de workers lasR est borne par la memoire", {
  local_mocked_bindings(.available_memory_bytes = function() 8e9,
                        .package = "nemetonshiny")
  withr::local_options(nemetonshiny.lasr_ncores = NULL)
  withr::local_envvar(NEMETON_LASR_NCORES = "")
  expect_lte(nemetonshiny:::.lasr_ncores(), 1L)   # 4 Go / 3 Go -> 1
  local_mocked_bindings(.available_memory_bytes = function() 64e9,
                        .package = "nemetonshiny")
  expect_lte(nemetonshiny:::.lasr_ncores(), 4L)
  expect_gte(nemetonshiny:::.lasr_ncores(), 1L)
  withr::local_options(nemetonshiny.lasr_ncores = 6)
  expect_equal(nemetonshiny:::.lasr_ncores(), 6L)
})

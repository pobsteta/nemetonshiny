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

test_that("le nombre de workers lasR est borne par le budget du calcul", {
  withr::local_options(nemetonshiny.lasr_ncores = NULL)
  withr::local_envvar(NEMETON_LASR_NCORES = "")
  f <- nemetonshiny:::.lasr_ncores
  # Cas du brief : plafond de 12 Go, 28 dalles COPC d'environ 260 Mo -> 1
  dalles <- rep(260e6, 28)
  expect_equal(f(dalles, budget = 12 * 1024^3), 1L)
  # Dalle de 342 Mo mesuree a 7,3 Go : deux workers demandent 15 Go
  expect_equal(nemetonshiny:::.lasr_octets_par_worker(342e6), 22 * 342e6)
  expect_equal(f(342e6, budget = 15e9), 1L)
  expect_lte(f(342e6, budget = 64e9), 4L)
  # Sans taille connue : 3 Go par worker au minimum
  expect_equal(nemetonshiny:::.lasr_octets_par_worker(), 3e9)
  expect_equal(f(budget = 4e9), 1L)
  # Budget inconnu : borne par les coeurs et 4 seulement
  expect_lte(f(dalles, budget = NA_real_), 4L)
  expect_gte(f(dalles, budget = 1), 1L)
  withr::local_options(nemetonshiny.lasr_ncores = 6)
  expect_equal(f(dalles, budget = 1), 6L)
})

test_that("le budget lasR prend le plus petit de MemAvailable, cgroup et plafond", {
  local_mocked_bindings(.available_memory_bytes = function() 24e9,
                        .cgroup_marge_octets = function(...) 9e9,
                        .package = "nemetonshiny")
  local_mocked_bindings(.memory_ceiling_bytes = function(...) 12 * 1024^3,
                        .package = "nemeton")
  expect_equal(nemetonshiny:::.lasr_budget_octets(), 9e9)
  local_mocked_bindings(.cgroup_marge_octets = function(...) NA_real_,
                        .package = "nemetonshiny")
  expect_equal(nemetonshiny:::.lasr_budget_octets(), 12 * 1024^3)
})

test_that(".cgroup_marge_octets lit la plus petite marge de la hierarchie", {
  withr::with_tempdir({
    dir.create("cg/user.slice/job.scope", recursive = TRUE)
    writeLines("max", "cg/user.slice/memory.max")
    writeLines("20000000000", "cg/user.slice/memory.current")
    writeLines("12884901888", "cg/user.slice/job.scope/memory.max")
    writeLines("4000000000", "cg/user.slice/job.scope/memory.current")
    writeLines("0::/user.slice/job.scope", "cgroup")
    expect_equal(nemetonshiny:::.cgroup_marge_octets("cg", "cgroup"),
                 12884901888 - 4e9)
    # Aucun plafond dans la hierarchie : NA
    writeLines("max", "cg/user.slice/job.scope/memory.max")
    expect_true(is.na(nemetonshiny:::.cgroup_marge_octets("cg", "cgroup")))
    expect_true(is.na(nemetonshiny:::.cgroup_marge_octets("cg", "absent")))
  })
})

test_that("un lasR tue par la memoire est relance une fois a 1 worker", {
  withr::local_options(nemetonshiny.lasr_ncores = 3)
  appels <- integer(0)
  local_mocked_bindings(
    run_memory_capped = function(fun, args, ...) {
      appels <<- c(appels, args$ncores)
      if (args$ncores > 1L) {
        stop("\"compute_dtm_chm_from_laz\" ran out of memory and was killed (ceiling: 12G).")
      }
      list(chm = "chm.tif")
    },
    .package = "nemeton")
  out <- suppressWarnings(suppressMessages(
    nemetonshiny:::.lasr_executer(list(laz_dir = "x"), 3e8, budget = 12e9)))
  expect_equal(appels, c(3L, 1L))
  expect_equal(out$chm, "chm.tif")

  # Deuxieme echec : NULL, la chaine passe a la source suivante
  appels <- integer(0)
  local_mocked_bindings(
    run_memory_capped = function(fun, args, ...) {
      appels <<- c(appels, args$ncores)
      stop("\"compute_dtm_chm_from_laz\" ran out of memory and was killed (ceiling: 12G).")
    },
    .package = "nemeton")
  expect_null(suppressWarnings(suppressMessages(
    nemetonshiny:::.lasr_executer(list(laz_dir = "x"), 3e8, budget = 12e9))))
  expect_equal(appels, c(3L, 1L))

  # Une autre erreur n'est pas relancee
  appels <- integer(0)
  local_mocked_bindings(
    run_memory_capped = function(fun, args, ...) {
      appels <<- c(appels, args$ncores)
      stop("lasR: fichier illisible")
    },
    .package = "nemeton")
  expect_null(suppressWarnings(suppressMessages(
    nemetonshiny:::.lasr_executer(list(laz_dir = "x"), 3e8, budget = 12e9))))
  expect_equal(appels, 3L)
})

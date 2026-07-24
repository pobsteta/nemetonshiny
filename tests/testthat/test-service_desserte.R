# Tests du service Desserte (création de réseau, adaptateur foretaccess).

test_that("DESSERTE_ENGINES : glouton exposé, steiner/optimiseurs NON exposés", {
  # v1 : seul le glouton est exposable (Steiner N² -> > 5 h à 30 parcelles, cf.
  # brief cœur). Garde-fou de régression contre une réexposition accidentelle.
  expect_true("glouton" %in% DESSERTE_ENGINES)
  expect_false("steiner" %in% DESSERTE_ENGINES)
  expect_false(any(c("multistart", "recuit", "riprute") %in% DESSERTE_ENGINES))
})

test_that("export_desserte_geopackage : copie si présent, FALSE sinon", {
  src <- withr::local_tempfile(fileext = ".gpkg"); writeLines("x", src)
  dst <- withr::local_tempfile(fileext = ".gpkg")
  expect_true(nemetonshiny:::export_desserte_geopackage(list(gpkg_path = src), dst))
  expect_true(file.exists(dst))
  expect_false(nemetonshiny:::export_desserte_geopackage(
    list(gpkg_path = "/no/such/file.gpkg"), dst))
  expect_false(nemetonshiny:::export_desserte_geopackage(list(), dst))
})

test_that(".load_cached_desserte : NULL si pas de cache", {
  expect_null(nemetonshiny:::.load_cached_desserte(NULL))
  expect_null(nemetonshiny:::.load_cached_desserte(""))
  expect_null(nemetonshiny:::.load_cached_desserte(withr::local_tempdir()))
})

test_that("run_desserte : chemins de garde structurés (pas d'exception)", {
  skip_if_not_installed("sf")
  skip_if_not_installed("foretaccess")
  cache <- withr::local_tempdir()
  # AOI manquante -> erreur structurée avant toute acquisition.
  expect_equal(nemetonshiny:::run_desserte(NULL, "glouton", cache)$reason,
               "desserte_need_project")
  expect_equal(
    nemetonshiny:::run_desserte("/no/such/aoi.gpkg", "glouton", cache)$reason,
    "desserte_need_project")
  # AOI valide mais moteur inconnu / vide : garde AVANT acquisition.
  poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))), crs = 2154))
  aoi_gpkg <- file.path(cache, "aoi.gpkg")
  sf::st_write(poly, aoi_gpkg, quiet = TRUE, delete_dsn = TRUE)
  expect_equal(nemetonshiny:::run_desserte(aoi_gpkg, character(0), cache)$reason,
               "desserte_need_engine")
  expect_equal(nemetonshiny:::run_desserte(aoi_gpkg, "steiner", cache)$reason,
               "desserte_need_engine")
})

test_that("run_desserte : échec d'acquisition MNT -> erreur structurée", {
  skip_if_not_installed("sf")
  skip_if_not_installed("foretaccess")
  cache <- withr::local_tempdir()
  poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))), crs = 2154))
  aoi_gpkg <- file.path(cache, "aoi.gpkg")
  sf::st_write(poly, aoi_gpkg, quiet = TRUE, delete_dsn = TRUE)
  testthat::with_mocked_bindings(
    .acquire_mnt_highres = function(...) NULL,
    .package = "nemetonshiny",
    testthat::with_mocked_bindings(
      acquire_mnt = function(...) stop("réseau IGN indisponible"),
      .package = "foretaccess",
      {
        res <- nemetonshiny:::run_desserte(aoi_gpkg, "glouton", cache)
        expect_equal(res$reason, "desserte_mnt_failed")
      }))
})

test_that("run_desserte : pipeline complet sur données toy (IGN mocké)", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("sf")
  toy <- system.file("extdata", "toy", package = "foretaccess")
  skip_if(!nzchar(toy) || !file.exists(file.path(toy, "mnt.tif")))
  loadNamespace("foretaccess")

  aoi_path <- file.path(toy, "foret.gpkg")   # parcelles à desservir (1 polygone)
  foret_toy <- sf::st_transform(sf::st_read(aoi_path, quiet = TRUE), 2154)
  desserte <- sf::st_read(file.path(toy, "desserte.gpkg"), quiet = TRUE)
  mnt_toy <- file.path(toy, "mnt.tif")
  # Structure de cache identique au module : run_desserte écrit dans
  # `<projet>/cache/desserte`, `.load_cached_desserte()` relit depuis `<projet>`.
  project_path <- withr::local_tempdir()
  cache <- nemetonshiny:::.desserte_cache_dir(project_path)

  testthat::with_mocked_bindings(
    .acquire_mnt_highres = function(aoi, res_m = 5, crs = 2154,
                                    cache_dir = tempdir(), overwrite = FALSE) mnt_toy,
    .package = "nemetonshiny",
    testthat::with_mocked_bindings(
      acquire_desserte = function(aoi, crs = 2154, cache_dir = tempdir(),
                                  overwrite = FALSE, country = "FR") desserte,
      acquire_foret = function(aoi, crs = 2154, cache_dir = tempdir(),
                               overwrite = FALSE, country = "FR") foret_toy,
      .package = "foretaccess",
      {
        res <- nemetonshiny:::run_desserte(aoi_path, "glouton", cache)
        expect_equal(res$status, "success")
        expect_equal(res$engine, "glouton")
        # Raster réseau + GeoPackage écrits sur disque.
        expect_true(file.exists(res$reseau_path))
        expect_true(file.exists(res$gpkg_path))
        # Sidecar de scalaires écrit pour le rechargement depuis le cache.
        expect_true(file.exists(file.path(cache, "reseau_glouton.rds")))
        # (cache = <projet>/cache/desserte, cf. structure module)
        # Scalaires de badge cohérents (1 parcelle toy -> desservie).
        expect_equal(res$n_parcelles, 1L)
        expect_true(res$n_desservies >= 0L)
        expect_type(res$connexe, "logical")
        # `raccorde` (foretaccess >= 1.11) : vrai indicateur qualité remonté.
        expect_true(is.logical(res$raccorde))
        expect_true(is.finite(res$cout))
        # Le GeoPackage porte les 3 couches attendues.
        layers <- sf::st_layers(res$gpkg_path)$name
        expect_true(all(c("parcelles", "desserte_existante") %in% layers))

        # Rechargement depuis le cache : mêmes chemins + scalaires restaurés.
        cached <- nemetonshiny:::.load_cached_desserte(project_path)
        expect_equal(cached$engine, "glouton")
        expect_equal(cached$n_parcelles, res$n_parcelles)
        expect_equal(cached$cout, res$cout)
        expect_true(isTRUE(cached$from_cache))
      }))
})

# --- Garde-fou memoire du glouton -------------------------------------------
# Le pic de foretaccess::reseau_desserte() est proportionnel a la grille et
# quadratique en d_neighborhood/resolution (table de voisinage materialisee
# cellule par cellule). Sans garde-fou, l'echec arrive en OOM apres ~15 min.

test_that(".desserte_n_offsets reproduces the solver's disc enumeration", {
  # Meme formule que build_offsets() cote Rust : carre [-nb, nb]^2 borne au
  # disque, centre exclu. nb = as.integer(d / csize + 0.5).
  expect_identical(nemetonshiny:::.desserte_n_offsets(42, 5), 220L)
  expect_identical(nemetonshiny:::.desserte_n_offsets(21, 5), 56L)
  expect_identical(nemetonshiny:::.desserte_n_offsets(42, 10), 56L)
  # Entrees degenerees -> 0, jamais une erreur.
  expect_identical(nemetonshiny:::.desserte_n_offsets(0, 5), 0L)
  expect_identical(nemetonshiny:::.desserte_n_offsets(NA, 5), 0L)
})

test_that(".desserte_memory_estimate tracks the measured 4.4 KB/cell at defaults", {
  # Mesure de reference (foretaccess 1.21.0, grille 600x600 @ 5 m, d = 42 m) :
  # pic 1 537 Mo, soit 4,37 Ko/cellule. On tolere 20 %.
  per_cell <- nemetonshiny:::.desserte_memory_estimate(1, 42, 5)
  expect_gt(per_cell, 4.37 * 1024 * 0.8)
  expect_lt(per_cell, 4.37 * 1024 * 1.2)

  # Lineaire en nombre de cellules.
  expect_equal(nemetonshiny:::.desserte_memory_estimate(2e6, 42, 5),
               2 * nemetonshiny:::.desserte_memory_estimate(1e6, 42, 5))

  # Decroit avec d_neighborhood (le levier mesure : 42 -> 21 m divise par ~3).
  ratio <- nemetonshiny:::.desserte_memory_estimate(1e6, 42, 5) /
           nemetonshiny:::.desserte_memory_estimate(1e6, 21, 5)
  expect_gt(ratio, 2.5)
  expect_lt(ratio, 4)

  expect_true(is.na(nemetonshiny:::.desserte_memory_estimate(0)))
  expect_true(is.na(nemetonshiny:::.desserte_memory_estimate(NA)))
})

test_that(".desserte_grid_cells derives the cell count from the bbox", {
  skip_if_not_installed("sf")
  sq <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(cbind(c(0, 1000, 1000, 0, 0), c(0, 0, 1000, 1000, 0)))),
    crs = 2154))
  expect_equal(nemetonshiny:::.desserte_grid_cells(sq, res_m = 5), 200 * 200)
  expect_equal(nemetonshiny:::.desserte_grid_cells(sq, res_m = 10), 100 * 100)
  expect_true(is.na(nemetonshiny:::.desserte_grid_cells(sq, res_m = 0)))
})

test_that(".desserte_memory_check refuses an extent that would exhaust RAM", {
  skip_if_not_installed("sf")
  skip_if_not(file.exists("/proc/meminfo"), "garde-fou base sur /proc/meminfo")
  # 60 km x 60 km a 5 m = 144 M cellules -> ~600 Go : refus certain.
  huge <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(cbind(c(0, 6e4, 6e4, 0, 0), c(0, 0, 6e4, 6e4, 0)))),
    crs = 2154))
  withr::with_envvar(c(NEMETON_DESSERTE_SKIP_GUARD = ""), {
    chk <- nemetonshiny:::.desserte_memory_check(huge, res_m = 5)
    expect_false(chk$ok)
    expect_gt(chk$bytes, chk$available)
  })
  # Echappatoire documentee.
  withr::with_envvar(c(NEMETON_DESSERTE_SKIP_GUARD = "1"), {
    expect_true(nemetonshiny:::.desserte_memory_check(huge, res_m = 5)$ok)
  })
})

test_that(".desserte_memory_check accepts a small extent", {
  skip_if_not_installed("sf")
  small <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(cbind(c(0, 500, 500, 0, 0), c(0, 0, 500, 500, 0)))),
    crs = 2154))
  withr::with_envvar(c(NEMETON_DESSERTE_SKIP_GUARD = ""), {
    expect_true(nemetonshiny:::.desserte_memory_check(small, res_m = 5)$ok)
  })
})

test_that("run_desserte refuses an oversized extent BEFORE any acquisition", {
  skip_if_not_installed("sf")
  skip_if_not(file.exists("/proc/meminfo"), "garde-fou base sur /proc/meminfo")
  skip_if_not_installed("foretaccess")
  withr::with_tempdir({
    # AOI 40 km x 40 km : le garde-fou doit trancher avant tout appel reseau.
    aoi <- sf::st_sf(geometry = sf::st_sfc(
      sf::st_polygon(list(cbind(c(0, 4e4, 4e4, 0, 0) + 8e5,
                                c(0, 0, 4e4, 4e4, 0) + 63e5))), crs = 2154))
    sf::st_write(aoi, "aoi.gpkg", quiet = TRUE)
    called <- FALSE
    testthat::local_mocked_bindings(
      acquire_mnt = function(...) { called <<- TRUE; stop("ne doit pas etre appele") },
      .package = "foretaccess")
    withr::with_envvar(c(NEMETON_DESSERTE_SKIP_GUARD = ""), {
      res <- nemetonshiny:::run_desserte("aoi.gpkg", "glouton", "cache", buffer_m = 0)
    })
    expect_identical(res$status, "error")
    expect_identical(res$reason, "desserte_memory_guard")
    expect_false(called)   # aucune acquisition declenchee
  })
})

test_that("the memory-guard reason has FR/EN translations", {
  for (lg in c("fr", "en")) {
    i18n <- get_i18n(lg)
    for (k in c("desserte_memory_guard", "dess_mem_estimate_fmt",
                "dess_mem_ok", "dess_mem_risk")) {
      expect_true(nzchar(i18n$t(k)))
      expect_false(identical(i18n$t(k), k))   # clé non traduite -> renvoie la clé
    }
  }
})

test_that(".desserte_grid_cells buffers the BBOX, not the geometries", {
  skip_if_not_installed("sf")
  sq <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(cbind(c(0, 1000, 1000, 0, 0), c(0, 0, 1000, 1000, 0)))),
    crs = 2154))
  # 1 km de cote + 500 m de chaque cote = 2 km -> 400x400 a 5 m.
  expect_equal(nemetonshiny:::.desserte_grid_cells(sq, 5, buffer_m = 500), 400 * 400)
  # Equivalence avec un st_buffer reel : meme bbox, donc meme compte de cellules.
  buffered <- sf::st_buffer(sq, 500)
  expect_equal(nemetonshiny:::.desserte_grid_cells(sq, 5, buffer_m = 500),
               nemetonshiny:::.desserte_grid_cells(buffered, 5))
  # Deux parcelles disjointes : bufferiser chacune ne change pas la bbox non plus
  # (le point corrige dans le diagnostic).
  two <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(cbind(c(0, 100, 100, 0, 0), c(0, 0, 100, 100, 0)))),
    sf::st_polygon(list(cbind(c(900, 1000, 1000, 900, 900), c(900, 900, 1000, 1000, 900)))),
    crs = 2154))
  expect_equal(nemetonshiny:::.desserte_grid_cells(two, 5, buffer_m = 500),
               nemetonshiny:::.desserte_grid_cells(sf::st_buffer(two, 500), 5))
  expect_identical(nemetonshiny:::.desserte_grid_cells(sq, 5, buffer_m = -10),
                   nemetonshiny:::.desserte_grid_cells(sq, 5, buffer_m = 0))
})

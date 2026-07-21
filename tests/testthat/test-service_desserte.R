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

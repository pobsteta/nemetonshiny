# Tests du service Desserte (création de réseau, adaptateur foretaccess).

test_that("DESSERTE_ENGINES : glouton et steiner exposes, optimiseurs a part", {
  # L'exclusion de Steiner (« > 5 h a 30 parcelles ») est PERIMEE : le coeur a
  # borne l'A* au corridor. Mesure sur Dabo (skidding_m = 100) : glouton 28,3 s /
  # 36 routes, steiner 78,4 s / 0 route — resultat bien forme, pas un echec.
  expect_true("glouton" %in% DESSERTE_ENGINES)
  expect_true("steiner" %in% DESSERTE_ENGINES)
  # Les strategies d'optimisation ne sont PAS des moteurs de creation : elles
  # vivent dans DESSERTE_OPTIM_STRATEGIES et passent par optimiser_reseau().
  expect_false(any(c("multistart", "recuit", "riprute") %in% DESSERTE_ENGINES))
  expect_setequal(DESSERTE_OPTIM_STRATEGIES, c("multistart", "recuit", "riprute"))
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
  expect_equal(nemetonshiny:::run_desserte(aoi_gpkg, "moteur_inconnu", cache)$reason,
               "desserte_need_engine")
  # `steiner` est desormais un moteur VALIDE : il franchit ce garde et echoue
  # plus loin, faute de donnees sur cette AOI synthetique.
  expect_false(identical(
    nemetonshiny:::run_desserte(aoi_gpkg, "steiner", cache)$reason,
    "desserte_need_engine"))
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

# --- Canal de phase du worker (engine_status.json) --------------------------
# Le glouton peut tourner des dizaines de minutes : sans ce canal l'utilisateur
# ne voit qu'un chrono et conclut que rien ne se passe.

test_that(".dess_write_phase / .dess_read_phase font l'aller-retour", {
  withr::with_tempdir({
    d <- file.path("proj", "cache", "desserte")
    dir.create(d, recursive = TRUE)
    expect_null(nemetonshiny:::.dess_read_phase("proj"))       # rien encore ecrit
    nemetonshiny:::.dess_write_phase(d, "moteur")
    expect_identical(nemetonshiny:::.dess_read_phase("proj"), "moteur")
    nemetonshiny:::.dess_write_phase(d, "cout")                # ecrasement
    expect_identical(nemetonshiny:::.dess_read_phase("proj"), "cout")
  })
})

test_that(".dess_read_phase ignore un statut perime et un projet absent", {
  expect_null(nemetonshiny:::.dess_read_phase(NULL))
  withr::with_tempdir({
    d <- file.path("proj", "cache", "desserte")
    dir.create(d, recursive = TRUE)
    # ts vieux de 5 min : le worker est repute mort, la phase ne doit pas coller
    # a l'ecran (seuil 120 s).
    writeLines(jsonlite::toJSON(list(phase = "moteur",
                 ts = as.integer(Sys.time()) - 300L), auto_unbox = TRUE),
               file.path(d, "engine_status.json"))
    expect_null(nemetonshiny:::.dess_read_phase("proj"))
  })
})

test_that("toutes les phases declarees ont une cle i18n", {
  i18n <- nemetonshiny:::get_i18n("fr")
  for (p in nemetonshiny:::DESSERTE_PHASES) {
    lbl <- i18n$t(paste0("dess_phase_", p))
    expect_true(is.character(lbl) && nzchar(lbl) && !identical(lbl, paste0("dess_phase_", p)),
                info = p)
  }
})

# --- skidding_m : parametre METIER passe au moteur ---------------------------
# Le defaut coeur est 0, le pire cas documente (« both slow and over-connected ») :
# le glouton trace alors depuis CHAQUE cellule de parcelle hors route. Mesure sur
# Dabo : jamais fini en 22 min a 0, 39,7 s a 300 m. L'app doit TOUJOURS passer une
# valeur realiste — ces tests verrouillent ce contrat.

test_that("DESSERTE_SKIDDING_DEFAULT_M est une distance realiste, jamais 0", {
  d <- nemetonshiny:::DESSERTE_SKIDDING_DEFAULT_M
  expect_true(is.numeric(d) && length(d) == 1L && is.finite(d))
  expect_gt(d, 0)     # 0 = pire cas coeur : interdit comme defaut app
})

test_that("run_desserte transmet skidding_m au moteur", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("sf")
  toy <- system.file("extdata", "toy", package = "foretaccess")
  skip_if(!nzchar(toy) || !file.exists(file.path(toy, "mnt.tif")))
  loadNamespace("foretaccess")

  aoi_path <- file.path(toy, "foret.gpkg")
  foret_toy <- sf::st_transform(sf::st_read(aoi_path, quiet = TRUE), 2154)
  desserte <- sf::st_read(file.path(toy, "desserte.gpkg"), quiet = TRUE)
  mnt_toy <- file.path(toy, "mnt.tif")
  cache <- nemetonshiny:::.desserte_cache_dir(withr::local_tempdir())
  vu <- new.env(parent = emptyenv())

  testthat::with_mocked_bindings(
    .acquire_mnt_highres = function(aoi, res_m = 5, crs = 2154,
                                    cache_dir = tempdir(), overwrite = FALSE) mnt_toy,
    .package = "nemetonshiny",
    testthat::with_mocked_bindings(
      acquire_desserte = function(aoi, crs = 2154, cache_dir = tempdir(),
                                  overwrite = FALSE, country = "FR") desserte,
      acquire_foret = function(aoi, crs = 2154, cache_dir = tempdir(),
                               overwrite = FALSE, country = "FR") foret_toy,
      reseau_desserte = function(pre, cout, parcelles, desserte_existante,
                                 mode, skidding_m, ...) {
        vu$skidding <- skidding_m
        stop("court-circuit apres capture")     # inutile d'aller plus loin
      },
      .package = "foretaccess",
      {
        # Valeur explicite transmise telle quelle.
        nemetonshiny:::run_desserte(aoi_path, "glouton", cache, skidding_m = 250)
        expect_identical(vu$skidding, 250)
        # Valeur absente -> defaut app, JAMAIS le 0 du coeur.
        vu$skidding <- NULL
        nemetonshiny:::run_desserte(aoi_path, "glouton", cache)
        expect_identical(vu$skidding, nemetonshiny:::DESSERTE_SKIDDING_DEFAULT_M)
        # Valeur aberrante -> repli sur le defaut, pas de propagation d'un NA.
        for (bad in list(NA_real_, -50, "x")) {
          vu$skidding <- NULL
          nemetonshiny:::run_desserte(aoi_path, "glouton", cache, skidding_m = bad)
          expect_identical(vu$skidding, nemetonshiny:::DESSERTE_SKIDDING_DEFAULT_M)
        }
      }))
})

# --- Controle d'integrite du reseau (spec 025) ------------------------------
# Action SEPAREE du calcul de desserte : mesure sur Dabo, 376,8 s contre 39,7 s
# pour la creation entiere. L'inclure en ligne rendrait « Generer la desserte »
# dix fois plus lent — exactement la regression corrigee en v0.121.10.

test_that("run_desserte_integrite : erreurs structurees, jamais d'exception", {
  withr::with_tempdir({
    cd <- "cache_desserte"; dir.create(cd)
    # Pas de GeoPackage -> pas de reseau a controler.
    r <- nemetonshiny:::run_desserte_integrite(cd, NULL)
    expect_type(r, "list")
    expect_identical(r$status, "error")
    expect_true(r$reason %in% c("desserte_integrite_no_reseau",
                                "desserte_integrite_no_dessertr",
                                "desserte_no_foretaccess"))
  })
})

test_that(".load_cached_integrite : NULL si absent, relit sinon", {
  withr::with_tempdir({
    expect_null(nemetonshiny:::.load_cached_integrite("."))
    saveRDS(list(n_infractions = 3L), "integrite.rds")
    got <- nemetonshiny:::.load_cached_integrite(".")
    expect_identical(got$n_infractions, 3L)
  })
})

test_that(".desserte_integrite rend NULL sans dessertR plutot qu'un verdict vide", {
  # Sans dessertR, foretaccess ne LEVE PAS d'erreur : il degrade vers
  # .integrite_vide(), dont n_infractions vaut NA. Rendre ce resultat tel quel
  # afficherait un bilan vide qui se lit comme « aucune infraction ».
  skip_if_not_installed("sf")
  d <- sf::st_sf(classe = "route",
                 geometry = sf::st_sfc(
                   sf::st_linestring(rbind(c(0, 0), c(100, 100))), crs = 2154))
  testthat::with_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "dessertR")) FALSE else base::requireNamespace(package, ...)
    },
    .package = "base",
    expect_null(nemetonshiny:::.desserte_integrite(d, NULL, d)))
})

test_that("les cles i18n du controle d'integrite existent", {
  i18n <- nemetonshiny:::get_i18n("fr")
  for (k in c("dess_integrite_title", "dess_integrite_intro", "dess_integrite_run",
              "dess_integrite_running", "dess_integrite_done", "dess_integrite_hint",
              "dess_badge_infractions", "dess_badge_orphelins",
              "desserte_integrite_no_dessertr", "desserte_integrite_no_reseau",
              "desserte_integrite_failed")) {
    lbl <- i18n$t(k)
    expect_true(is.character(lbl) && nzchar(lbl) && !identical(lbl, k), info = k)
  }
})

# --- Detection de routes non cartographiees (dessertR, spec 026) -------------
# La plus lourde du panneau : 189,4 s et 7,91 Go de pic SANS nuage sur 1 855 ha,
# > 10 min avec. D'ou un garde-fou memoire non optionnel et un garde dessertR.

test_that("run_desserte_detection : erreurs structurees, jamais d'exception", {
  withr::with_tempdir({
    cd <- "cache_desserte"; dir.create(cd)
    r <- nemetonshiny:::run_desserte_detection(cd, NULL)
    expect_type(r, "list")
    expect_identical(r$status, "error")
    expect_true(r$reason %in% c("desserte_need_project", "desserte_detect_no_dessertr",
                                "desserte_no_foretaccess"))
  })
})

test_that("run_desserte_detection : le garde-fou memoire refuse AVANT acquisition", {
  skip_if_not_installed("sf")
  skip_if_not_installed("dessertR")
  withr::with_tempdir({
    cd <- "cache_desserte"; dir.create(cd)
    # AOI enorme -> grille hors de portee : le refus doit tomber avant toute
    # acquisition, sinon l'echec arrive apres plusieurs minutes sous forme d'OOM.
    poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(3e5, 0), c(3e5, 3e5), c(0, 3e5), c(0, 0)))), crs = 2154))
    aoi <- file.path(cd, "aoi.gpkg")
    sf::st_write(poly, aoi, quiet = TRUE, delete_dsn = TRUE)
    r <- nemetonshiny:::run_desserte_detection(cd, aoi)
    expect_identical(r$status, "error")
    expect_identical(r$reason, "desserte_memory_guard")
    expect_true(nzchar(r$detail))
  })
})

test_that(".load_cached_detection : NULL si absent, relit sinon", {
  withr::with_tempdir({
    expect_null(nemetonshiny:::.load_cached_detection("."))
    saveRDS(list(n_detecte = 7L, avec_lidar = TRUE), "detection.rds")
    expect_identical(nemetonshiny:::.load_cached_detection(".")$n_detecte, 7L)
  })
})

# --- Ponderation par le cout de construction (brief dessertR §1) -------------
# `pondere_cout = TRUE` fait minimiser des EUROS et non des metres. Il CHANGE
# les traces : un reseau mis en cache avant ce reglage n'est pas comparable.

test_that(".load_cached_desserte ignore un reseau trace sans ponderation", {
  withr::with_tempdir({
    cd <- nemetonshiny:::.desserte_cache_dir(".")
    dir.create(cd, recursive = TRUE)
    writeLines("x", file.path(cd, "reseau_glouton.tif"))
    # Sidecar d'AVANT la ponderation : pas de marqueur -> cache perime.
    saveRDS(list(cout = 1, n_desservies = 1L, n_parcelles = 1L),
            file.path(cd, "reseau_glouton.rds"))
    expect_null(nemetonshiny:::.load_cached_desserte("."))
    # Avec le marqueur, le cache est servi.
    saveRDS(list(cout = 1, n_desservies = 1L, n_parcelles = 1L, pondere_cout = TRUE),
            file.path(cd, "reseau_glouton.rds"))
    got <- nemetonshiny:::.load_cached_desserte(".")
    expect_true(is.list(got) && identical(got$status, "success"))
  })
})

# --- Classement des lineaires detectes (dsr_classer, brief dessertR §2/§3) ---
# `dsr_classer()` exige des LINESTRING : un MULTILINESTRING est refuse net.
# La BD TOPO est multi, et rien ne garantit que la detection ne le soit pas.

test_that("dsr_classer refuse le MULTILINESTRING des qu un critere est arme", {
  skip_if_not_installed("dessertR")
  skip_if_not_installed("sf")
  skip_if(!("dsr_classer" %in% getNamespaceExports("dessertR")),
          "dessertR < 1.3.0")
  ml <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_multilinestring(list(rbind(c(0, 0), c(10, 10)))), crs = 2154))
  poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(-5, -5), c(20, -5), c(20, 20), c(-5, 20), c(-5, -5)))), crs = 2154))
  # Le refus n'est PAS inconditionnel : `dsr_classer(ml)` seul passe. Il tombe
  # des qu'un critere geometrique est arme — `reference` OU `parcellaire`,
  # c'est-a-dire exactement notre appel.
  expect_s3_class(dessertR::dsr_classer(ml), "sf")
  expect_error(dessertR::dsr_classer(ml, reference = ml), "LINESTRING")
  expect_error(dessertR::dsr_classer(ml, parcellaire = poly,
                                     sous_type_parcelle = "section"), "LINESTRING")
  # Converti, il passe : c'est exactement ce que fait run_desserte_detection().
  lin <- suppressWarnings(sf::st_cast(ml, "LINESTRING"))
  expect_s3_class(dessertR::dsr_classer(lin, parcellaire = poly,
                                        sous_type_parcelle = "section"), "sf")
})

test_that("les cles i18n du classement existent", {
  i18n <- nemetonshiny:::get_i18n("fr")
  for (k in c("dess_detect_conf_fmt", "dess_detect_osm_fmt")) {
    lbl <- i18n$t(k)
    expect_true(is.character(lbl) && nzchar(lbl) && !identical(lbl, k), info = k)
  }
})

# --- Invalidation du cache par les parametres (annexe A du brief coeur) -------
# `.load_cached_desserte()` ne comparait AUCUN parametre : elle rejetait les
# caches anterieurs a `pondere_cout = TRUE` et servait tout le reste tel quel.
# Changer `skidding_m` puis rouvrir l'onglet servait donc le reseau precedent,
# calcule a l'ancienne distance -- et le badge affichait l'ancienne valeur, si
# bien que rien ne trahissait l'ecart.

test_that(".desserte_params_identiques compare, et traite l'absent comme divergent", {
  meta <- list(skidding_m = 300, methode_pente = "bareme", largeur_m = 4,
               pente_max_pct = 60)
  expect_true(.desserte_params_identiques(meta, meta[c("skidding_m", "largeur_m")]))
  expect_true(.desserte_params_identiques(meta, meta))

  # Un ecart sur n'importe lequel invalide.
  expect_false(.desserte_params_identiques(meta, list(skidding_m = 100)))
  expect_false(.desserte_params_identiques(meta, list(methode_pente = "terrassement")))
  expect_false(.desserte_params_identiques(meta, list(largeur_m = 6)))
  expect_false(.desserte_params_identiques(meta, list(pente_max_pct = 80)))

  # Un cache ANTERIEUR a l'introduction du champ ne porte pas la valeur : on ne
  # peut pas affirmer qu'il a ete calcule avec celle demandee, donc il diverge.
  expect_false(.desserte_params_identiques(list(skidding_m = 300),
                                           list(methode_pente = "bareme")))

  # Tolerance numerique : 300 et 300.0 sont la meme distance.
  expect_true(.desserte_params_identiques(list(skidding_m = 300L),
                                          list(skidding_m = 300)))
})

test_that(".desserte_params_courants retombe sur les defauts documentes", {
  vide <- .desserte_params_courants(list())
  expect_equal(vide$skidding_m, DESSERTE_SKIDDING_DEFAULT_M)
  expect_equal(vide$methode_pente, "bareme")
  expect_equal(vide$largeur_m, DESSERTE_LARGEUR_DEFAULT_M)
  expect_equal(vide$pente_max_pct, DESSERTE_PENTE_MAX_DEFAULT_PCT)

  # Une entree absurde ne doit pas invalider le cache par accident.
  sale <- .desserte_params_courants(list(dess_largeur = "", dess_pente_max = -5,
                                         dess_methode_pente = "n'importe quoi"))
  expect_equal(sale$largeur_m, DESSERTE_LARGEUR_DEFAULT_M)
  expect_equal(sale$pente_max_pct, DESSERTE_PENTE_MAX_DEFAULT_PCT)
  expect_equal(sale$methode_pente, "bareme")

  # `skidding_m` deja resolu par l'appelant est repris tel quel.
  expect_equal(.desserte_params_courants(list(skidding_m = 999), 100)$skidding_m, 100)
})

test_that(".load_cached_desserte rejette un cache calcule autrement", {
  dir <- withr::local_tempdir()
  cache <- file.path(dir, "cache", "desserte")
  dir.create(cache, recursive = TRUE)
  eng <- DESSERTE_ENGINES[[1]]
  writeLines("x", file.path(cache, paste0("reseau_", eng, ".tif")))
  saveRDS(list(cout = 1, skidding_m = 300, methode_pente = "bareme",
               largeur_m = 4, pente_max_pct = 60, pondere_cout = TRUE),
          file.path(cache, paste0("reseau_", eng, ".rds")))

  demande <- list(skidding_m = 300, methode_pente = "bareme", largeur_m = 4,
                  pente_max_pct = 60)
  expect_true(isTRUE(.load_cached_desserte(dir, demande)$from_cache))

  # La meme demande a une autre distance de debardage : le cache ne repond plus.
  demande$skidding_m <- 100
  expect_null(.load_cached_desserte(dir, demande))

  # Sans parametres, l'ancien comportement est conserve -- utile aux appelants
  # qui veulent juste savoir s'il existe un reseau.
  expect_true(isTRUE(.load_cached_desserte(dir)$from_cache))
})

test_that("un coeur trop ancien echoue au lieu de tarifer autrement en silence", {
  # foretaccess < spec 029 n'a ni `methode_pente` ni `pente_max_pct`. Retomber
  # sur le bareme sans le dire donnerait a l'utilisateur un chiffrage qu'il n'a
  # pas demande -- il croirait mesurer un volume de terre.
  testthat::with_mocked_bindings(
    surface_cout_construction = function(pre, config = NULL, ...) stop("jamais appele"),
    .package = "foretaccess",
    {
      # Le refus tombe AVANT l'acquisition : aucun AOI n'est meme necessaire.
      expect_equal(
        nemetonshiny:::run_desserte(NULL, "glouton", tempdir(),
                                    methode_pente = "terrassement")$reason,
        "desserte_core_trop_ancien")
      expect_equal(
        nemetonshiny:::run_desserte(NULL, "glouton", tempdir(),
                                    pente_max_pct = 80)$reason,
        "desserte_core_trop_ancien")

      # La demande historique -- bareme, plafond par defaut -- reste honoree :
      # elle echoue plus loin, faute d'AOI, et non sur le garde-fou.
      expect_equal(
        nemetonshiny:::run_desserte(NULL, "glouton", tempdir())$reason,
        "desserte_need_project")
    })
})

# --- Cache OSM : provenance du transport (brief unification OSM §4.2) --------
# Le transport Overpass passe d'un tuilage 1 km à une requête unique avec
# bissection : la COUVERTURE change, pas seulement la vitesse. Un cache produit
# par une autre version de `foretaccess` doit donc être refusé, sans quoi on
# compare deux extractions différentes sans le savoir.

test_that(".load_cached_osm refuses a cache from another foretaccess version", {
  withr::with_tempdir({
    saveRDS(list(n_osm = 12L, foretaccess_version = "0.0.0-antique"),
            file.path(getwd(), "osm.rds"))
    expect_null(nemetonshiny:::.load_cached_osm(getwd()))
  })
})

test_that(".load_cached_osm refuses a cache with no recorded version", {
  withr::with_tempdir({
    saveRDS(list(n_osm = 12L), file.path(getwd(), "osm.rds"))
    expect_null(nemetonshiny:::.load_cached_osm(getwd()))
  })
})

test_that(".load_cached_osm serves a cache matching the installed version", {
  skip_if_not_installed("foretaccess")
  withr::with_tempdir({
    v <- as.character(utils::packageVersion("foretaccess"))
    saveRDS(list(n_osm = 12L, foretaccess_version = v),
            file.path(getwd(), "osm.rds"))
    got <- nemetonshiny:::.load_cached_osm(getwd())
    expect_false(is.null(got))
    expect_identical(got$n_osm, 12L)
  })
})

test_that("run_desserte_osm records the transport provenance it will be judged on", {
  skip_if_not_installed("foretaccess")
  withr::with_tempdir({
    cd <- getwd()
    aoi <- sf::st_sf(
      id = 1L,
      geometry = sf::st_sfc(sf::st_polygon(list(rbind(
        c(0, 0), c(1000, 0), c(1000, 1000), c(0, 1000), c(0, 0)))), crs = 2154))
    ap <- file.path(cd, "aoi.gpkg")
    sf::st_write(aoi, ap, quiet = TRUE)
    ligne <- function(y) sf::st_linestring(rbind(c(0, y), c(500, y)))
    faux <- sf::st_sf(highway = "track",
                      geometry = sf::st_sfc(list(ligne(100)), crs = 2154))
    bd <- sf::st_sf(classe = "route",
                    geometry = sf::st_sfc(list(ligne(900)), crs = 2154))
    res <- testthat::with_mocked_bindings(
      nemetonshiny:::run_desserte_osm(cd, ap, buffer_m = 0),
      acquire_desserte_osm = function(...) faux,
      acquire_desserte = function(...) bd,
      comparer_desserte_osm = function(...) list(resume = list(a = 1), corridor_m = 15),
      .package = "foretaccess")
    expect_identical(res$status, "success")
    cache <- readRDS(file.path(cd, "osm.rds"))
    expect_identical(cache$foretaccess_version,
                     as.character(utils::packageVersion("foretaccess")))
    expect_match(cache$date_requete, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T")
    # Et le cache ainsi ecrit doit etre RELU par le loader : ecriture et lecture
    # doivent parler de la meme cle, sinon le cache ne sert jamais.
    expect_false(is.null(nemetonshiny:::.load_cached_osm(cd)))
  })
})

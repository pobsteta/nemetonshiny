# Tests du service Accessibilité (adaptateur foretaccess).

test_that(".accessibility_recap_table : union des classes + une colonne par moteur", {
  i18n <- get_i18n("fr")
  recaps <- list(
    skidder = data.frame(classe = c("parcourable", "hors_foret"),
                         surface_ha = c(4.08, 1.68), stringsAsFactors = FALSE),
    porteur = data.frame(classe = c("parcourable", "indetermine"),
                         surface_ha = c(4.08, 0.49), stringsAsFactors = FALSE))
  tab <- nemetonshiny:::.accessibility_recap_table(recaps, i18n)
  expect_s3_class(tab, "data.frame")
  # Colonnes : classe + un libellé traduit par moteur.
  expect_equal(names(tab)[1], "classe")
  expect_true(i18n$t("acc_engine_skidder") %in% names(tab))
  expect_true(i18n$t("acc_engine_porteur") %in% names(tab))
  # Union des classes (3 distinctes), NA -> 0 pour la classe absente d'un moteur.
  expect_setequal(tab$classe, c("parcourable", "hors_foret", "indetermine"))
  row_hf <- tab[tab$classe == "hors_foret", ]
  expect_equal(row_hf[[i18n$t("acc_engine_porteur")]], 0)  # absente chez porteur
})

test_that(".accessibility_recap_table : NULL si aucun récap exploitable", {
  i18n <- get_i18n("fr")
  expect_null(nemetonshiny:::.accessibility_recap_table(list(), i18n))
  expect_null(nemetonshiny:::.accessibility_recap_table(
    list(skidder = data.frame()), i18n))
})

test_that(".resolve_accessibility_aoi : NULL projet -> NULL ; indicators_sf -> 2154", {
  skip_if_not_installed("sf")
  expect_null(nemetonshiny:::.resolve_accessibility_aoi(NULL))

  poly <- sf::st_sf(
    ug_id = "U1",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(6.0, 46.0), c(6.001, 46.0), c(6.001, 46.001), c(6.0, 46.001), c(6.0, 46.0)))),
      crs = 4326))
  aoi <- nemetonshiny:::.resolve_accessibility_aoi(list(indicators_sf = poly))
  expect_s3_class(aoi, "sf")
  expect_equal(sf::st_crs(aoi)$epsg, 2154L)
})

test_that("export_accessibility_geopackage : copie si présent, FALSE sinon", {
  src <- withr::local_tempfile(fileext = ".gpkg"); writeLines("x", src)
  dst <- withr::local_tempfile(fileext = ".gpkg")
  expect_true(nemetonshiny:::export_accessibility_geopackage(
    list(gpkg_path = src), dst))
  expect_true(file.exists(dst))
  # Pas de gpkg_path -> best-effort FALSE.
  expect_false(nemetonshiny:::export_accessibility_geopackage(list(), dst))
  expect_false(nemetonshiny:::export_accessibility_geopackage(
    list(gpkg_path = "/no/such/file.gpkg"), dst))
})

test_that("run_accessibility : chemins de garde structurés (pas d'exception)", {
  skip_if_not_installed("sf")
  skip_if_not_installed("foretaccess")
  cache <- withr::local_tempdir()
  # AOI manquante (chemin NULL / inexistant) — avant toute acquisition réseau.
  expect_equal(
    nemetonshiny:::run_accessibility(NULL, "skidder", cache)$reason,
    "accessibility_need_project")
  expect_equal(
    nemetonshiny:::run_accessibility("/no/such/aoi.gpkg", "skidder", cache)$reason,
    "accessibility_need_project")
  # AOI valide (par fichier) mais aucun moteur : garde AVANT acquisition MNT/OSM.
  poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))), crs = 2154))
  aoi_gpkg <- file.path(cache, "aoi.gpkg")
  sf::st_write(poly, aoi_gpkg, quiet = TRUE, delete_dsn = TRUE)
  expect_equal(
    nemetonshiny:::run_accessibility(aoi_gpkg, character(0), cache)$reason,
    "accessibility_need_engine")
})

test_that("run_accessibility : échec d'acquisition MNT -> erreur structurée", {
  skip_if_not_installed("sf")
  skip_if_not_installed("foretaccess")
  cache <- withr::local_tempdir()
  poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))), crs = 2154))
  aoi_gpkg <- file.path(cache, "aoi.gpkg")
  sf::st_write(poly, aoi_gpkg, quiet = TRUE, delete_dsn = TRUE)
  # HIGHRES indisponible (NULL) -> repli sur acquire_mnt, qui échoue -> erreur
  # structurée. Mocker les deux évite tout appel réseau.
  testthat::with_mocked_bindings(
    .acquire_mnt_highres = function(...) NULL,
    .package = "nemetonshiny",
    testthat::with_mocked_bindings(
      acquire_mnt = function(...) stop("réseau IGN indisponible"),
      .package = "foretaccess",
      {
        res <- nemetonshiny:::run_accessibility(aoi_gpkg, "skidder", cache)
        expect_equal(res$reason, "accessibility_mnt_failed")
      }))
})

test_that("run_accessibility : pipeline complet sur données toy (IGN mocké)", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("sf")
  toy <- system.file("extdata", "toy", package = "foretaccess")
  skip_if(!nzchar(toy) || !file.exists(file.path(toy, "mnt.tif")))
  loadNamespace("foretaccess")

  # L'AOI est passée par FICHIER (le worker la relit) : le GeoPackage toy fait
  # directement office de chemin d'entrée. MNT (acquire_mnt), desserte
  # (acquire_desserte) et masque forêt BD Forêt V2 (acquire_foret) sont mockés
  # vers les données toy (pas de réseau IGN).
  aoi_path <- file.path(toy, "foret.gpkg")
  foret_toy <- sf::st_transform(sf::st_read(aoi_path, quiet = TRUE), 2154)
  desserte <- sf::st_read(file.path(toy, "desserte.gpkg"), quiet = TRUE)
  mnt_toy <- file.path(toy, "mnt.tif")
  cache <- withr::local_tempdir()

  # Le MNT primaire passe par le helper interne `.acquire_mnt_highres` (couche
  # HIGHRES) : on le mocke côté nemetonshiny ; desserte/forêt côté foretaccess.
  testthat::with_mocked_bindings(
    .acquire_mnt_highres = function(aoi, res_m = 5, crs = 2154,
                                    cache_dir = tempdir(), overwrite = FALSE) mnt_toy,
    .package = "nemetonshiny",
    testthat::with_mocked_bindings(
    acquire_desserte = function(aoi, crs = 2154, cache_dir = tempdir(),
                                overwrite = FALSE, country = "FR") desserte,
    acquire_foret = function(aoi, crs = 2154, cache_dir = tempdir(),
                             overwrite = FALSE, country = "FR") foret_toy,
    # DFCI mocké (pas de réseau OSM) : toutes les dessertes = sources DFCI, pour
    # que camion_dfci ait des points de départ sur les données toy.
    acquire_dfci = function(aoi, crs = 2154, cache_dir = tempdir(),
                            overwrite = FALSE) NULL,
    flag_dfci = function(desserte, dfci_lignes = NULL, ...) {
      desserte$dfci <- 1L; desserte
    },
    .package = "foretaccess",
    {
      res <- nemetonshiny:::run_accessibility(
        aoi_path, c("skidder", "porteur", "camion_dfci"), cache)
      expect_equal(res$status, "success")
      expect_setequal(res$engines, c("skidder", "porteur", "camion_dfci"))
      expect_true(all(file.exists(unlist(res$raster_paths))))
      expect_true(file.exists(res$gpkg_path))
      # Chaque moteur a produit un récap non vide.
      expect_true(all(vapply(res$recaps, nrow, integer(1)) > 0L))
      # Le skidder ajoute le raster « classes de débardage » — UNIQUEMENT si la
      # version de foretaccess installée exporte `classes_debardage` (ajoutée
      # après la release v1.2.0). Sinon dégradation gracieuse (pas de couche).
      dbg_available <- isTRUE(tryCatch(
        is.function(foretaccess::classes_debardage), error = function(e) FALSE))
      if (dbg_available) {
        expect_true("classes_debardage" %in% names(res$raster_paths))
        dbg <- terra::rast(res$raster_paths[["classes_debardage"]])
        expect_true(terra::is.factor(dbg))
      } else {
        expect_false("classes_debardage" %in% names(res$raster_paths))
      }
    }))
})

test_that("run_accessibility : la zone tampon élargit l'emprise d'acquisition", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("sf")
  toy <- system.file("extdata", "toy", package = "foretaccess")
  skip_if(!nzchar(toy) || !file.exists(file.path(toy, "mnt.tif")))
  loadNamespace("foretaccess")

  aoi_path <- file.path(toy, "foret.gpkg")
  foret_toy <- sf::st_transform(sf::st_read(aoi_path, quiet = TRUE), 2154)
  desserte <- sf::st_read(file.path(toy, "desserte.gpkg"), quiet = TRUE)
  mnt_toy <- file.path(toy, "mnt.tif")
  cache <- withr::local_tempdir()

  base_area <- as.numeric(sum(sf::st_area(foret_toy)))
  seen_area <- NULL       # emprise reçue par le MNT (helper HIGHRES)
  foret_area <- NULL      # emprise reçue par acquire_foret (masque BD Forêt)

  # On capture l'emprise (aoi) reçue par le helper MNT `.acquire_mnt_highres`
  # (primaire, côté nemetonshiny) ET par acquire_foret (côté foretaccess) : avec
  # un tampon de 1 km, les deux doivent être plus grandes que la forêt d'origine
  # (le masque = BD Forêt ∩ emprise tamponnée).
  testthat::with_mocked_bindings(
    .acquire_mnt_highres = function(aoi, res_m = 5, crs = 2154,
                                    cache_dir = tempdir(), overwrite = FALSE) {
      seen_area <<- as.numeric(sum(sf::st_area(aoi)))
      mnt_toy
    },
    .package = "nemetonshiny",
    testthat::with_mocked_bindings(
    acquire_desserte = function(aoi, crs = 2154, cache_dir = tempdir(),
                                overwrite = FALSE, country = "FR") desserte,
    acquire_foret = function(aoi, crs = 2154, cache_dir = tempdir(),
                             overwrite = FALSE, country = "FR") {
      foret_area <<- as.numeric(sum(sf::st_area(aoi)))
      foret_toy   # masque toy (dans l'emprise du MNT) pour que preprocess passe
    },
    .package = "foretaccess",
    {
      res <- nemetonshiny:::run_accessibility(
        aoi_path, "skidder", cache, buffer_m = 1000)
      expect_equal(res$status, "success")
    }))

  expect_true(!is.null(seen_area) && seen_area > base_area)
  # Le masque forêt est bien acquis sur l'emprise tamponnée (BD Forêt V2).
  expect_true(!is.null(foret_area) && foret_area > base_area)
  # Le sous-cache d'acquisition est bien nommé selon le tampon (en mètres).
  expect_true(dir.exists(file.path(cache, "emprise_1000m")))
})

test_that(".resolve_desserte_dfci : provenance osm / heuristique du flag DFCI", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("sf")
  ln <- function(a, b) sf::st_linestring(rbind(a, b))
  desserte <- sf::st_sf(
    classe = c("route", "chemin"),
    geometry = sf::st_sfc(ln(c(0, 0), c(10, 10)), ln(c(0, 10), c(10, 0)), crs = 2154))
  aoi <- sf::st_as_sfc(sf::st_bbox(desserte))

  # 1. OSM : acquire_dfci renvoie un réseau, flag_dfci le tague -> source "osm".
  testthat::with_mocked_bindings(
    acquire_dfci = function(aoi, crs = 2154, cache_dir = tempdir(),
                            overwrite = FALSE) {
      sf::st_sf(geometry = sf::st_sfc(ln(c(0, 0), c(10, 10)), crs = 2154))
    },
    flag_dfci = function(desserte, dfci_lignes = NULL, ...) {
      desserte$dfci <- c(1L, 0L); desserte
    },
    .package = "foretaccess",
    {
      r <- nemetonshiny:::.resolve_desserte_dfci(desserte, aoi, 2154, tempdir())
      expect_equal(r$source, "osm")
      expect_equal(r$desserte$dfci, c(1L, 0L))
    })

  # 2. Heuristique : pas d'OSM, flag_dfci ne tague rien -> repli routes/pistes.
  testthat::with_mocked_bindings(
    acquire_dfci = function(aoi, crs = 2154, cache_dir = tempdir(),
                            overwrite = FALSE) NULL,
    flag_dfci = function(desserte, dfci_lignes = NULL, ...) {
      desserte$dfci <- 0L; desserte
    },
    .package = "foretaccess",
    {
      r <- nemetonshiny:::.resolve_desserte_dfci(desserte, aoi, 2154, tempdir())
      expect_equal(r$source, "heuristique")
      expect_equal(r$desserte$dfci, c(1L, 0L))   # route -> 1, chemin -> 0
    })
})

test_that(".acquire_mnt_highres : réutilise le cache sans appel réseau", {
  skip_if_not_installed("sf")
  dir <- withr::local_tempdir()
  # Un fichier de cache déjà présent -> le helper le renvoie tel quel (pas de
  # fetch happign). On écrit un fichier bidon : seul le chemin est vérifié.
  writeLines("x", file.path(dir, "mnt_highres.tif"))
  aoi <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(737500, 6384500)), crs = 2154))
  got <- nemetonshiny:::.acquire_mnt_highres(aoi, cache_dir = dir)
  expect_equal(got, file.path(dir, "mnt_highres.tif"))
})

test_that(".load_cached_accessibility : restaure les rasters cachés d'un projet", {
  skip_if_not_installed("terra")
  proj <- withr::local_tempdir()
  # Pas de cache -> NULL.
  expect_null(nemetonshiny:::.load_cached_accessibility(proj))
  expect_null(nemetonshiny:::.load_cached_accessibility(NULL))

  cache_dir <- nemetonshiny:::.accessibility_cache_dir(proj)
  dir.create(cache_dir, recursive = TRUE)
  r <- terra::rast(nrows = 2, ncols = 2, vals = 1:4)
  terra::writeRaster(r, file.path(cache_dir, "acc_skidder.tif"))
  terra::writeRaster(r, file.path(cache_dir, "acc_classes_debardage.tif"))

  res <- nemetonshiny:::.load_cached_accessibility(proj)
  expect_equal(res$status, "success")
  expect_true(isTRUE(res$from_cache))
  expect_setequal(names(res$raster_paths), c("skidder", "classes_debardage"))
  # L'engine (skidder) précède les classes de débardage -> couche par défaut.
  expect_equal(names(res$raster_paths)[[1]], "skidder")
  expect_equal(res$engines, "skidder")
})

test_that(".acc_level_colors : coltab du raster prioritaire, sinon repli", {
  skip_if_not_installed("terra")
  # Raster catégoriel SANS coltab -> palette de repli (#RRGGBB, 7 caractères).
  r <- terra::rast(nrows = 2, ncols = 2, vals = c(1, 2, 1, 2))
  levels(r) <- data.frame(value = 1:2, classe = c("a", "b"))
  cols <- nemetonshiny:::.acc_level_colors(r, c(1, 2))
  expect_length(cols, 2L)
  expect_true(all(nchar(cols) == 7L))
  # Avec une coltab explicite -> ces couleurs (avec alpha, 9 caractères).
  terra::coltab(r) <- data.frame(value = 1:2, col = c("#123456", "#00000000"))
  cols2 <- nemetonshiny:::.acc_level_colors(r, c(1, 2))
  expect_equal(toupper(substr(cols2[1], 1L, 7L)), "#123456")
  expect_equal(substr(cols2[2], 8L, 9L), "00")   # transparent
})

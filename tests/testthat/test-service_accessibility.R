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

test_that(".resolve_accessibility_mnt : NULL chemin projet -> NULL", {
  expect_null(nemetonshiny:::.resolve_accessibility_mnt(NULL))
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
  cache <- withr::local_tempdir()
  # AOI manquant.
  r1 <- nemetonshiny:::run_accessibility(NULL, "x.tif", "skidder", cache)
  expect_equal(r1$status, "error")
  expect_equal(r1$reason, "accessibility_need_project")
  # MNT manquant.
  poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))), crs = 2154))
  r2 <- nemetonshiny:::run_accessibility(poly, "/no/such/mnt.tif", "skidder", cache)
  expect_equal(r2$reason, "accessibility_no_mnt")
})

test_that("run_accessibility : pipeline complet sur données toy (OSM mockée)", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("sf")
  toy <- system.file("extdata", "toy", package = "foretaccess")
  skip_if(!nzchar(toy) || !file.exists(file.path(toy, "mnt.tif")))
  loadNamespace("foretaccess")

  aoi <- sf::st_read(file.path(toy, "foret.gpkg"), quiet = TRUE)
  desserte <- sf::st_read(file.path(toy, "desserte.gpkg"), quiet = TRUE)
  mnt_path <- file.path(toy, "mnt.tif")
  cache <- withr::local_tempdir()

  testthat::with_mocked_bindings(
    acquire_desserte = function(aoi, crs = 2154, cache_dir = tempdir(),
                                overwrite = FALSE, country = "FR") desserte,
    .package = "foretaccess",
    {
      res <- nemetonshiny:::run_accessibility(
        aoi, mnt_path, c("skidder", "porteur", "camion_dfci"), cache)
      expect_equal(res$status, "success")
      expect_setequal(res$engines, c("skidder", "porteur", "camion_dfci"))
      expect_true(all(file.exists(unlist(res$raster_paths))))
      expect_true(file.exists(res$gpkg_path))
      # Chaque moteur a produit un récap non vide.
      expect_true(all(vapply(res$recaps, nrow, integer(1)) > 0L))
    })
})

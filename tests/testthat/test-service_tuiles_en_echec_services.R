# Mission 038 : un test par service qui porte `avertissements`. Le reste du
# service est mocke au plus court ; seuls acquire_desserte / acquire_foret
# emettent l'avertissement de tuile, et l'alerte doit etre dans le RESULTAT.

.avert_tuile <- function(n = 1L, total = 4L,
                         couche = "BDTOPO_V3:troncon_de_route") {
  cli::cli_warn(c(
    "{n} tuile{?s} WFS sur {total} en echec pour {.val {couche}}.",
    "!" = "La couche rendue est incomplete sur une partie de l'emprise.",
    "x" = "Derniere erreur : Unexpected content type \"text/plain\"."))
}

.desserte_toy <- function() {
  sf::st_sf(classe = "route", largeur = 4, geometry = sf::st_sfc(
    sf::st_linestring(rbind(c(0, 10), c(50, 10))), crs = 2154))
}

.carre <- function(cote = 100) {
  sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(0, 0), c(cote, 0), c(cote, cote), c(0, cote), c(0, 0)))), crs = 2154))
}

.aoi_gpkg <- function(dir) {
  ap <- file.path(dir, "aoi.gpkg")
  sf::st_write(.carre(), ap, quiet = TRUE)
  ap
}

.mnt_tif <- function(dir, cote = 100) {
  f <- file.path(dir, "mnt.tif")
  terra::writeRaster(terra::rast(nrows = cote, ncols = cote, xmin = 0,
    xmax = cote, ymin = 0, ymax = cote, vals = 1, crs = "EPSG:2154"), f)
  f
}

test_that("run_accessibility rend les alertes desserte + foret dans le resultat", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("terra")
  withr::with_tempdir({
    cd <- getwd()
    ap <- .aoi_gpkg(cd); mnt <- .mnt_tif(cd)
    testthat::local_mocked_bindings(
      .acquire_mnt_highres = function(...) mnt, .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      acquire_desserte = function(...) { .avert_tuile(1L, 4L); .desserte_toy() },
      acquire_foret = function(...) {
        .avert_tuile(2L, 3L, "BDFORET_V2:formation_vegetale"); .carre() },
      preprocess = function(...) list(),
      porteur = function(pre) list(accessibilite = terra::rast(mnt),
                                   recap = data.frame()),
      .package = "foretaccess")
    res <- nemetonshiny:::run_accessibility(ap, "porteur", file.path(cd, "cache"))
    expect_identical(res$status, "success")
    expect_length(res$avertissements, 2L)
    expect_true(any(grepl("troncon_de_route", res$avertissements)))
    expect_true(any(grepl("formation_vegetale", res$avertissements)))
  })
})

test_that("run_desserte_lidar_correction rend l'alerte de tuile dans le resultat", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("terra")
  withr::with_tempdir({
    cd <- getwd()
    ap <- .aoi_gpkg(cd); mnt <- .mnt_tif(cd)
    projet <- file.path(cd, "projet")
    laz <- file.path(projet, "cache", "layers", "lidar_nuage")
    dir.create(laz, recursive = TRUE)
    file.create(file.path(laz, "dalle.laz"))
    reseau <- .desserte_toy(); reseau$source <- "bdtopo"
    testthat::local_mocked_bindings(
      .acquire_mnt_highres = function(...) mnt,
      .desserte_complement_osm = function(...) list(
        reseau = reseau, n_bdtopo = 1L, n_ajoutes = 0L, statut = "ok"),
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      acquire_desserte = function(...) { .avert_tuile(); .desserte_toy() },
      qualifier_desserte = function(reseau, ...) reseau,
      .package = "foretaccess")
    res <- nemetonshiny:::run_desserte_lidar_correction(
      ap, file.path(cd, "cache", "accessibilite"), project_path = projet)
    expect_identical(res$status, "success")
    expect_length(res$avertissements, 1L)
    expect_match(res$avertissements, "1 tuile WFS sur 4", fixed = TRUE)
  })
})

test_that("run_desserte_optimiser rend les alertes desserte + foret", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("terra")
  withr::with_tempdir({
    cd <- getwd()
    ap <- .aoi_gpkg(cd); mnt <- .mnt_tif(cd)
    testthat::local_mocked_bindings(
      .acquire_mnt_highres = function(...) mnt, .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      acquire_desserte = function(...) { .avert_tuile(1L, 4L); .desserte_toy() },
      acquire_foret = function(...) {
        .avert_tuile(2L, 3L, "BDFORET_V2:formation_vegetale"); .carre() },
      preprocess = function(...) list(),
      surface_cout_construction = function(...) list(),
      optimiser_reseau = function(...) list(cout = 12, lignes = NULL),
      .package = "foretaccess")
    dir.create(file.path(cd, "cache"))
    res <- nemetonshiny:::run_desserte_optimiser(
      file.path(cd, "cache"), ap, "multistart")
    expect_identical(res$status, "success")
    expect_length(res$avertissements, 2L)
    expect_true(any(grepl("troncon_de_route", res$avertissements)))
    expect_true(any(grepl("formation_vegetale", res$avertissements)))
  })
})

test_that("run_desserte_detection rend l'alerte de tuile de la reference", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("terra")
  withr::with_tempdir({
    cd <- getwd()
    ap <- .aoi_gpkg(cd); mnt <- .mnt_tif(cd)
    testthat::local_mocked_bindings(
      .acquire_mnt_highres = function(...) mnt,
      .dessertR_dispo = function() TRUE,
      .desserte_detection_memory_check = function(...) list(ok = TRUE),
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      acquire_desserte = function(...) { .avert_tuile(); .desserte_toy() },
      detecter_desserte = function(...) .desserte_toy()[0, ],
      .package = "foretaccess")
    dir.create(file.path(cd, "cache"))
    res <- nemetonshiny:::run_desserte_detection(file.path(cd, "cache"), ap)
    expect_identical(res$status, "success")
    expect_length(res$avertissements, 1L)
    expect_match(res$avertissements, "1 tuile WFS sur 4", fixed = TRUE)
  })
})

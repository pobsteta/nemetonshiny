# Tests du service de typage de desserte (volume_mobilisable -> typer_desserte).

# Objet reseau persisté factice : une liste de classe foretaccess_reseau avec un
# SpatRaster wrappé (comme le persiste run_desserte).
.typage_write_reseau_obj <- function(cache_dir, engine = "glouton") {
  skip_if_not_installed("terra")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  r <- terra::rast(nrows = 4, ncols = 4, vals = 1,
                   xmin = 900000, xmax = 900040, ymin = 6500000, ymax = 6500040,
                   crs = "EPSG:2154")
  obj <- structure(list(reseau = terra::wrap(r)), class = "foretaccess_reseau")
  saveRDS(obj, file.path(cache_dir, paste0("reseau_obj_", engine, ".rds")))
}

.typage_parcelles <- function(with_p1 = TRUE) {
  p <- sf::st_sf(
    P1 = if (with_p1) 200 else NA_real_,
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(900000, 6500000), c(900040, 6500000), c(900040, 6500040),
      c(900000, 6500040), c(900000, 6500000)))), crs = 2154))
  p
}

test_that("run_desserte_typage : gardes structurées", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("nemeton")
  skip_if_not_installed("sf")
  cache <- withr::local_tempdir()

  # Pas d'objet reseau persisté -> no_reseau (avant toute autre garde).
  expect_equal(
    nemetonshiny:::run_desserte_typage(cache, .typage_parcelles(), 0.5, 30)$reason,
    "desserte_typage_no_reseau")

  .typage_write_reseau_obj(cache)
  # Parcelles absentes.
  expect_equal(
    nemetonshiny:::run_desserte_typage(cache, NULL, 0.5, 30)$reason,
    "desserte_typage_no_parcelles")
  # Volume P1 absent.
  expect_equal(
    nemetonshiny:::run_desserte_typage(cache, .typage_parcelles(with_p1 = FALSE), 0.5, 30)$reason,
    "desserte_typage_no_volume")
  # Paramètres invalides.
  expect_equal(
    nemetonshiny:::run_desserte_typage(cache, .typage_parcelles(), 0, 30)$reason,
    "desserte_typage_bad_params")
})

test_that("run_desserte_typage : chaîne complète (piège d'unité vérifié)", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("nemeton")
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  cache <- withr::local_tempdir()
  .typage_write_reseau_obj(cache)
  parc <- .typage_parcelles()

  troncons <- sf::st_sf(
    type = c("primaire", "tertiaire"),
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(900000, 6500000), c(900040, 6500040))),
      sf::st_linestring(rbind(c(900000, 6500040), c(900040, 6500000))),
      crs = 2154))
  recap <- data.frame(type = c("primaire", "tertiaire"),
                      longueur = c(1000, 2000), stringsAsFactors = FALSE)

  # Capture l'`unite` passée à volume_mobilisable : DOIT être "m3_total" (§3).
  seen_unite <- NULL

  testthat::with_mocked_bindings(
    volume_mobilisable = function(units, volume_col = "P1", unite = "m3_total", ...) {
      seen_unite <<- unite
      units$volume_mobilisable <- 3000
      units
    },
    .package = "nemeton",
    testthat::with_mocked_bindings(
      vectoriser_reseau = function(reseau) list(troncons = troncons, noeuds = NULL),
      calculer_flux = function(graphe, parcelles, volume_champ = "volume", ...) graphe,
      typer_desserte = function(graphe, seuils_flux, ...) {
        structure(list(troncons = troncons, recap = recap, seuils_flux = seuils_flux),
                  class = "foretaccess_desserte_typee")
      },
      .package = "foretaccess",
      {
        res <- nemetonshiny:::run_desserte_typage(cache, parc, 0.5, 30)
        expect_equal(res$status, "success")
        # Le piège d'unité : le typage DOIT demander m3_total, jamais m3_ha.
        expect_equal(seen_unite, "m3_total")
        expect_s3_class(res$recap, "data.frame")
        expect_setequal(res$recap$type, c("primaire", "tertiaire"))
        expect_true(!is.null(res$gpkg_path) && file.exists(res$gpkg_path))
        expect_true("reseau_type" %in% sf::st_layers(res$gpkg_path)$name)
      }))
})

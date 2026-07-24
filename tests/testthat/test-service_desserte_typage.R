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

# --- Resolution du nom de la colonne volume (P1) -----------------------------
# Le coeur ecrit `P1` (indicateur_p1_volume(column_name = "P1")), le projet
# persiste `indicateur_p1_volume` : chercher « P1 » en dur produisait un faux
# « volume P1 absent » sur un projet entierement calcule.

test_that(".resolve_volume_col accepts the core's P1 name", {
  df <- data.frame(P1 = c(10, 20), autre = c(1, 2))
  expect_identical(nemetonshiny:::.resolve_volume_col(df), "P1")
})

test_that(".resolve_volume_col accepts the PERSISTED indicateur_p1_volume name", {
  df <- data.frame(indicateur_p1_volume = c(682.5, 2315.5), ug_id = 1:2)
  expect_identical(nemetonshiny:::.resolve_volume_col(df), "indicateur_p1_volume")
})

test_that(".resolve_volume_col prefers P1 when both are present", {
  df <- data.frame(indicateur_p1_volume = c(1, 2), P1 = c(3, 4))
  expect_identical(nemetonshiny:::.resolve_volume_col(df), "P1")
})

test_that(".resolve_volume_col skips an all-NA column and falls through", {
  df <- data.frame(P1 = c(NA_real_, NA_real_),
                   indicateur_p1_volume = c(100, 200))
  expect_identical(nemetonshiny:::.resolve_volume_col(df), "indicateur_p1_volume")
})

test_that(".resolve_volume_col is case-insensitive as a last resort", {
  df <- data.frame(Indicateur_P1_Volume = c(5, 6))
  expect_identical(nemetonshiny:::.resolve_volume_col(df), "Indicateur_P1_Volume")
})

test_that(".resolve_volume_col returns NULL when there is genuinely no volume", {
  expect_null(nemetonshiny:::.resolve_volume_col(data.frame(a = 1, b = 2)))
  expect_null(nemetonshiny:::.resolve_volume_col(data.frame()))
  expect_null(nemetonshiny:::.resolve_volume_col(NULL))
  # Colonne presente mais non numerique exploitable.
  expect_null(nemetonshiny:::.resolve_volume_col(
    data.frame(P1 = c("beaucoup", "peu"))))
})

test_that("run_desserte_typage no longer reports no_volume for a persisted P1", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    parcelles <- sf::st_sf(
      indicateur_p1_volume = c(682.5, 1556, 2315.5),
      geometry = sf::st_sfc(
        sf::st_polygon(list(cbind(c(0, 10, 10, 0, 0), c(0, 0, 10, 10, 0)))),
        sf::st_polygon(list(cbind(c(20, 30, 30, 20, 20), c(0, 0, 10, 10, 0)))),
        sf::st_polygon(list(cbind(c(40, 50, 50, 40, 40), c(0, 0, 10, 10, 0)))),
        crs = 2154))
    dir.create("cache")
    # Pas de reseau persiste -> on doit sortir sur no_reseau, PAS sur no_volume :
    # la colonne volume est desormais reconnue.
    res <- nemetonshiny:::run_desserte_typage("cache", parcelles,
                                             taux_prelevement = 0.5,
                                             horizon_ans = 30)
    expect_identical(res$reason, "desserte_typage_no_reseau")
    expect_false(identical(res$reason, "desserte_typage_no_volume"))
  })
})

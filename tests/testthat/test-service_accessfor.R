# Tests du service de validation ACCESSFOR (crosswalk cœur + WFS IGN mockés).

# Crosswalk minimal (forme de foretaccess::accessfor_correspondance()).
.accessfor_corr <- function() {
  data.frame(
    fa_value = 1:9,
    fa_classe = c("0-250", "250-500", "500-1000", "1000-1500", "1500-2000",
                  "> 2000", "inaccessible", "inexploitable", "hors_foret"),
    accessfor_class = c(3L, 4L, 5L, 6L, 7L, 8L, 1L, 2L, NA),
    accessfor_cat = c("A1", "A2", "A3", "A4", "A5", "A6", "Inaccessible",
                      "Inexploitable", NA),
    stringsAsFactors = FALSE)
}

# Raster classes_debardage synthétique (catégoriel, EPSG:2154, valeurs 1..9).
.accessfor_raster <- function(dir) {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 6, ncols = 6,
                   xmin = 900000, xmax = 900060, ymin = 6500000, ymax = 6500060,
                   crs = "EPSG:2154", vals = rep(1:9, length.out = 36))
  levels(r) <- data.frame(value = 1:9, classe = .accessfor_corr()$fa_classe)
  p <- file.path(dir, "acc_classes_debardage.tif")
  terra::writeRaster(r, p, overwrite = TRUE)
  p
}

test_that("run_accessfor_validation : gardes structurées", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("happign")
  expect_equal(nemetonshiny:::run_accessfor_validation(NULL)$reason,
               "accessfor_no_layer")
  expect_equal(nemetonshiny:::run_accessfor_validation("/no/such.tif")$reason,
               "accessfor_no_layer")
  p <- .accessfor_raster(withr::local_tempdir())
  expect_equal(nemetonshiny:::run_accessfor_validation(p, "avion")$reason,
               "accessfor_bad_engine")
})

test_that("run_accessfor_validation : pipeline complet (WFS + crosswalk mockés)", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("happign")
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  p <- .accessfor_raster(withr::local_tempdir())

  # Couche ACCESSFOR mockée : un polygone couvrant tout le raster, class = 3
  # (correspond à fa_value 1 dans le crosswalk).
  af <- sf::st_sf(
    class = 3L, cat = "A1",
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(900000, 6500000), c(900060, 6500000), c(900060, 6500060),
      c(900000, 6500060), c(900000, 6500000)))), crs = 2154))

  testthat::with_mocked_bindings(
    accessfor_correspondance = function(...) .accessfor_corr(),
    .package = "foretaccess",
    testthat::with_mocked_bindings(
      get_wfs = function(x, layer, ...) af,
      .package = "happign",
      {
        res <- nemetonshiny:::run_accessfor_validation(p, "skidder")
        expect_equal(res$status, "success")
        expect_true(is.finite(res$overall_pct))
        expect_gt(res$n_cells, 0L)
        expect_s3_class(res$table, "data.frame")
        expect_true(all(c("accessfor_class", "accord_pct", "cellules") %in%
                          names(res$table)))
        # ACCESSFOR = class 3 partout ; nos cellules valant fa_value 1 (-> code 3)
        # sont en accord. La table contient la classe 3.
        expect_true(3L %in% res$table$accessfor_class)
        # Raster ACCESSFOR affichable écrit sur disque, catégoriel (reclassé vers
        # nos bandes) — devient une couche sélectionnable dans la carte.
        expect_true(!is.null(res$accessfor_raster_path) &&
                      file.exists(res$accessfor_raster_path))
        af_disp <- terra::rast(res$accessfor_raster_path)
        expect_true(terra::is.factor(af_disp))
        # Emprise identique aux classes de débardage : ACCESSFOR est masqué sur les
        # cellules « hors_foret » (fa_value 9 dans le raster app) -> NA.
        base_v <- terra::values(terra::rast(p), mat = FALSE)
        disp_v <- terra::values(af_disp, mat = FALSE)
        expect_true(all(is.na(disp_v[base_v == 9L])))
        expect_true(any(!is.na(disp_v[base_v != 9L])))
      }))
})

test_that("run_accessfor_validation : WFS vide -> erreur structurée", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("happign")
  p <- .accessfor_raster(withr::local_tempdir())
  testthat::with_mocked_bindings(
    accessfor_correspondance = function(...) .accessfor_corr(),
    .package = "foretaccess",
    testthat::with_mocked_bindings(
      get_wfs = function(x, layer, ...) sf::st_sf(
        geometry = sf::st_sfc(crs = 2154)),
      .package = "happign",
      {
        res <- nemetonshiny:::run_accessfor_validation(p, "skidder")
        expect_equal(res$reason, "accessfor_wfs_empty")
      }))
})

# Tests testServer du module Accessibilité (ForêtAccess).
# Le run réel tourne dans un worker `future` (non exécuté sous testServer) : on
# valide ici la résolution d'AOI, les gardes, et le rendu à partir d'un résultat
# posé directement dans `rv`.

.acc_units <- function(n = 2) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- 6 + i / 1000
    sf::st_polygon(list(rbind(
      c(x0, 46), c(x0 + 0.001, 46), c(x0 + 0.001, 46.001),
      c(x0, 46.001), c(x0, 46))))
  })
  sf::st_sf(ug_id = paste0("U", seq_len(n)),
            geometry = sf::st_sfc(polys, crs = 4326))
}

test_that("AOI résolu depuis indicators_sf (EPSG:2154)", {
  skip_if_not_installed("sf")
  proj <- list(id = "p1", path = withr::local_tempdir(),
               indicators_sf = .acc_units(2))
  as <- shiny::reactiveValues(current_project = proj)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"), .package = "nemetonshiny")

  shiny::testServer(
    nemetonshiny:::mod_accessibility_server,
    args = list(app_state = as),
    {
      aoi <- units_sf()
      expect_s3_class(aoi, "sf")
      expect_equal(sf::st_crs(aoi)$epsg, 2154L)
      expect_equal(nrow(aoi), 2L)
    })
})

test_that("run sans projet chargé = no-op gardé (aucun worker lancé)", {
  skip_if_not_installed("sf")
  as <- shiny::reactiveValues(current_project = NULL)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"), .package = "nemetonshiny")

  shiny::testServer(
    nemetonshiny:::mod_accessibility_server,
    args = list(app_state = as),
    {
      session$setInputs(engines = c("skidder", "porteur"), run = 1)
      expect_false(isTRUE(rv$running))
      expect_null(rv$result)
    })
})

test_that("résultat posé -> sélecteur de couche rendu", {
  skip_if_not_installed("sf")
  proj <- list(id = "p1", path = withr::local_tempdir(),
               indicators_sf = .acc_units(1))
  as <- shiny::reactiveValues(current_project = proj)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"), .package = "nemetonshiny")

  shiny::testServer(
    nemetonshiny:::mod_accessibility_server,
    args = list(app_state = as),
    {
      # Laisser l'observer de purge (déféré sur current_project) s'exécuter à
      # l'init AVANT de poser un résultat, sinon il l'écraserait au 1er flush.
      session$flushReact()
      rv$result <- list(
        status = "success", engines = c("skidder", "porteur"),
        recaps = list(
          skidder = data.frame(classe = "parcourable", surface_ha = 4.0),
          porteur = data.frame(classe = "parcourable", surface_ha = 4.0)),
        raster_paths = list(skidder = "/x/acc_skidder.tif",
                            classes_debardage = "/x/acc_classes_debardage.tif",
                            porteur = "/x/acc_porteur.tif"),
        gpkg_path = NULL)
      session$flushReact()
      # Le sélecteur de couche apparaît une fois un résultat présent (une entrée
      # par raster disponible, dont « classes_debardage »).
      expect_false(is.null(output$layer_ui))
    })
})

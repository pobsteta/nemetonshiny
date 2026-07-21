# Tests testServer du module Desserte (ForêtAccess — création de réseau).
# Le run réel tourne dans un worker `future` (non exécuté sous testServer) : on
# valide ici la résolution des parcelles, les gardes, et le rendu à partir d'un
# résultat posé directement dans `rv`.

.dess_units <- function(n = 2) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- 6 + i / 1000
    sf::st_polygon(list(rbind(
      c(x0, 46), c(x0 + 0.001, 46), c(x0 + 0.001, 46.001),
      c(x0, 46.001), c(x0, 46))))
  })
  sf::st_sf(ug_id = paste0("U", seq_len(n)),
            geometry = sf::st_sfc(polys, crs = 4326))
}

test_that("parcelles résolues depuis indicators_sf (EPSG:2154)", {
  skip_if_not_installed("sf")
  proj <- list(id = "p1", path = withr::local_tempdir(),
               indicators_sf = .dess_units(2))
  as <- shiny::reactiveValues(current_project = proj)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"), .package = "nemetonshiny")

  shiny::testServer(
    nemetonshiny:::mod_desserte_server,
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
    nemetonshiny:::mod_desserte_server,
    args = list(app_state = as),
    {
      session$setInputs(engine = "glouton", run = 1)
      expect_false(isTRUE(rv$running))
      expect_null(rv$result)
    })
})

test_that("résultat posé -> badges de bilan rendus", {
  skip_if_not_installed("sf")
  proj <- list(id = "p1", path = withr::local_tempdir(),
               indicators_sf = .dess_units(1))
  as <- shiny::reactiveValues(current_project = proj)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"), .package = "nemetonshiny")

  shiny::testServer(
    nemetonshiny:::mod_desserte_server,
    args = list(app_state = as),
    {
      # Laisser l'observer de purge (déféré) s'exécuter à l'init AVANT de poser
      # un résultat, sinon il l'écraserait au 1er flush.
      session$flushReact()
      rv$result <- list(
        status = "success", engine = "glouton",
        reseau_path = "/x/reseau_glouton.tif", gpkg_path = NULL,
        cout = 125972, connexe = FALSE,
        n_desservies = 30L, n_parcelles = 30L)
      session$flushReact()
      # Le panneau de bilan s'affiche une fois un résultat présent.
      expect_false(is.null(output$summary))
    })
})

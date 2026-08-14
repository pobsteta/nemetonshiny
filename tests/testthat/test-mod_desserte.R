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
        cout = 125972, connexe = FALSE, raccorde = TRUE,
        n_desservies = 30L, n_parcelles = 30L)
      session$flushReact()
      # Le panneau de bilan s'affiche une fois un résultat présent.
      expect_false(is.null(output$summary))
    })
})

test_that("les lignes vectorisees ont leur couche ET leur case", {
  # Le resultat du moteur existe sous DEUX formes : le raster (support du
  # calcul) et les lignes (`reseau_cree` du GeoPackage, deja ecrites par
  # `run_desserte()`). Seul le raster etait peint.
  f <- testthat::test_path("..", "..", "R", "mod_desserte.R")
  testthat::skip_if_not(file.exists(f), "sources R absentes (package installe)")
  src <- readLines(f, warn = FALSE)
  code <- src[!grepl("^\\s*#", src)]

  # Declaree dans le controle de couches : un groupe peint sans case est
  # ineteignable — c'est le defaut corrige en 0.122.6 sur la carte voisine.
  ov <- grep("^\\s*overlays <-", code)
  expect_length(ov, 1L)
  bloc <- paste(code[ov:(ov + 5L)], collapse = " ")
  expect_match(bloc, "DESS_GROUPE_LIGNES", fixed = TRUE)

  # Peinte depuis la couche vectorielle, pas re-derivee du raster.
  expect_true(any(grepl('layer = "reseau_cree"', code, fixed = TRUE)))
  expect_true(any(grepl("group = DESS_GROUPE_LIGNES", code, fixed = TRUE)))

  # Au-dessus du raster : sinon la grille en escalier masque le trace.
  expect_true(any(grepl("nemetonDessLignes", code, fixed = TRUE)))
  pane_lignes <- grep('addMapPane\\("nemetonDessLignes", zIndex = ([0-9]+)\\)', code,
                      value = TRUE)
  pane_raster <- grep('addMapPane\\("nemetonDessRaster", zIndex = ([0-9]+)\\)', code,
                      value = TRUE)
  z <- function(x) as.integer(sub('.*zIndex = ([0-9]+).*', "\\1", x))
  expect_gt(z(pane_lignes), z(pane_raster))

  # Etat de la case respecte au re-dessin, lu sous isolate (garde v0.122.4).
  expect_true(any(grepl("hideGroup(proxy, DESS_GROUPE_LIGNES)", code, fixed = TRUE)))
})

test_that("les libelles du controle de couches sont uniformement accentues", {
  # Les deux cartes de l'onglet Terrain affichaient des menus qui se
  # contredisaient : accents sur l'une, pas sur l'autre. Ces noms SONT les
  # libelles leaflet (pas de chaine d'affichage separee), donc ils s'ecrivent
  # accentues, et via des CONSTANTES - un nom present a cinq endroits finit par
  # diverger, c'est ainsi que le relief s'etait retrouve sans case en 0.122.6.
  groupes <- c(nemetonshiny:::DESS_GROUPE_RESEAU,
               nemetonshiny:::DESS_GROUPE_LIGNES,
               nemetonshiny:::DESS_GROUPE_TYPE,
               nemetonshiny:::ACC_ACCESSIBILITE_GROUP,
               nemetonshiny:::ACC_DESSERTE_CORR_GROUP,
               nemetonshiny:::PLACES_DEPOT_GROUP)
  for (g in groupes) {
    expect_true(is.character(g) && nzchar(g))
  }
  expect_identical(nemetonshiny:::DESS_GROUPE_RESEAU, "R\u00e9seau cr\u00e9\u00e9")
  expect_identical(nemetonshiny:::DESS_GROUPE_LIGNES, "Lignes cr\u00e9\u00e9es")
  expect_identical(nemetonshiny:::DESS_GROUPE_TYPE, "R\u00e9seau typ\u00e9")

  # Aucun litteral de ces groupes ne doit subsister dans le module.
  f <- testthat::test_path("..", "..", "R", "mod_desserte.R")
  testthat::skip_if_not(file.exists(f), "sources R absentes (package installe)")
  code <- readLines(f, warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  for (litt in c('"Reseau cree"', '"Reseau type"', '"Lignes creees"')) {
    expect_false(any(grepl(litt, code, fixed = TRUE)), info = litt)
  }
})

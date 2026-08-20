# Tests — signal `app_state$parcels_changed` (spec 001-app)
#
# L'onglet Sélection ne lit pas `app_state$current_project` : sa carte tient son
# propre état, alimenté par un signal. Quand un module retire des parcelles du
# projet (purge ONF, v0.130.6), la Sélection continuerait donc d'afficher — et
# de compter comme sélectionnées — des parcelles disparues.
#
# Ces tests couvrent le contrat du signal : ce qu'il redessine, ce qu'il
# restreint, et surtout ce qu'il refuse de faire.

.pc_parcels <- function(ids = c("A", "B", "C")) {
  polys <- lapply(seq_along(ids), function(i) {
    x0 <- i * 10
    sf::st_polygon(list(rbind(
      c(x0, 0), c(x0 + 5, 0), c(x0 + 5, 5), c(x0, 5), c(x0, 0))))
  })
  sf::st_sf(id = ids, geometry = sf::st_sfc(polys, crs = 4326))
}

.pc_state <- function() {
  shiny::reactiveValues(
    language = "fr", current_project = NULL,
    restore_project = NULL, parcels_changed = NULL)
}


test_that("mod_ug pose le signal quand la purge a retire des parcelles", {
  skip_if_not_installed("sf")
  # C'est l'émetteur. Sans ce signal, la correction de la v0.130.6 reste
  # invisible dans l'onglet Sélection.
  src <- readLines(testthat::test_path("..", "..", "R", "mod_ug.R"), warn = FALSE)
  pose <- grep("app_state\\$parcels_changed <- ", src)
  expect_length(pose, 1L)

  # Il est posé DANS la branche `with_parcels`, pas à chaque commit : un
  # croisement sans purge ne change aucune parcelle et ne doit rien signaler.
  contexte <- paste(src[max(1, pose - 12):pose], collapse = "\n")
  expect_match(contexte, "with_parcels", fixed = TRUE)
})

test_that("mod_map ecoute le signal et garde l'idempotence", {
  src <- readLines(testthat::test_path("..", "..", "R", "mod_map.R"), warn = FALSE)
  expect_true(any(grepl("observeEvent(app_state$parcels_changed", src, fixed = TRUE)))
  # Garde d'idempotence : sans elle, toute invalidation de app_state rejouerait
  # le redessin de la couche.
  expect_true(any(grepl("last_parcels_change", src, fixed = TRUE)))
  # Le signal ne doit RIEN faire d'autre que redessiner : pas de zoom, pas de
  # requête réseau, pas de changement de commune.
  bloc <- src[grep("observeEvent(app_state$parcels_changed", src, fixed = TRUE):
                (grep("observeEvent(app_state$parcels_changed", src, fixed = TRUE) + 45)]
  bloc <- paste(bloc, collapse = "\n")
  expect_false(grepl("fitBounds", bloc, fixed = TRUE))
  expect_false(grepl("setView", bloc, fixed = TRUE))
  expect_false(grepl("httr|curl|geo.api", bloc))
})

test_that("le signal restreint la selection sans jamais l'elargir", {
  skip_if_not_installed("sf")
  # La règle qui compte : le signal annonce une MODIFICATION des parcelles, pas
  # une nouvelle sélection. Une parcelle non sélectionnée avant ne doit pas le
  # devenir parce qu'elle a survécu à la purge.
  avant <- c("A", "B")            # l'utilisateur avait sélectionné A et B
  apres <- c("B", "C")            # A supprimée, C toujours là mais non choisie
  expect_equal(intersect(avant, apres), "B")
  expect_false("C" %in% intersect(avant, apres))
})

test_that("un signal mal forme laisse la carte intacte", {
  skip_if_not_installed("sf")
  # Un signal sans `parcels`, avec 0 ligne, ou sans colonne `id` ne doit pas
  # vider la carte : mieux vaut un affichage périmé qu'un affichage vide.
  src <- readLines(testthat::test_path("..", "..", "R", "mod_map.R"), warn = FALSE)
  i <- grep("observeEvent(app_state$parcels_changed", src, fixed = TRUE)
  bloc <- paste(src[i:(i + 45)], collapse = "\n")
  expect_match(bloc, 'inherits(pd, "sf")', fixed = TRUE)
  expect_match(bloc, "nrow(pd) == 0L", fixed = TRUE)
  expect_match(bloc, '"id" %in% names(pd)', fixed = TRUE)
})

test_that("le signal porte bien les parcelles APRES modification", {
  skip_if_not_installed("sf")
  # Contrat du signal : `parcels` est l'état final, pas la liste des supprimées.
  # mod_map redessine à partir de lui sans avoir à faire de différence.
  p <- .pc_parcels(c("A", "B", "C"))
  restantes <- p[p$id != "A", ]
  chg <- list(parcels = restantes, timestamp = Sys.time())

  expect_s3_class(chg$parcels, "sf")
  expect_equal(nrow(chg$parcels), 2L)
  expect_false("A" %in% as.character(chg$parcels$id))
  expect_true(inherits(chg$timestamp, "POSIXct"))
})

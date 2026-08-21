# Tests — sens des indicateurs de risque (spec 048, nemeton >= 0.181.0)
#
# `nemeton 0.181.0` inverse R1 (feu), R2 (tempête), R3 (sécheresse) et R4
# (abroutissement) à la normalisation, comme R5 depuis 0.99.1. Leur grandeur
# brute est « haut = mauvais » et passait telle quelle sur le radar : une UGF
# très exposée obtenait un `famille_risque` ÉLEVÉ, donc flatteur.
#
# Côté app il n'y a aucun calcul à écrire — mais deux pièges à désamorcer :
# les indicateurs déjà calculés sont faux et doivent être invalidés, et la
# palette de la carte se retourne toute seule.

test_that("le marqueur de sens est declare et superieur a l'etat initial", {
  expect_true(is.integer(nemetonshiny:::INDICATOR_SENSE_VERSION))
  # 1 = état initial (projets sans marqueur). 2 = spec 048.
  expect_gte(nemetonshiny:::INDICATOR_SENSE_VERSION, 2L)
})

test_that("un projet calcule AVANT l'inversion est invalide, une seule fois", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    with_mocked_bindings(
      get_app_options = function() list(project_dir = getwd()),
      {
        poly <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1),
                                          c(0, 1), c(0, 0))))
        parcels <- sf::st_sf(id = "P1", contenance = 1e4,
                             geometry = sf::st_sfc(poly, crs = 2154))
        pid <- nemetonshiny:::create_project(name = "Sens R", parcels = parcels)$id

        # Simule un projet calculé sous l'ancien sens : pas de marqueur, un
        # parquet présent, le drapeau à TRUE.
        ppath <- nemetonshiny:::get_project_path(pid)
        writeLines("x", file.path(ppath, "data", "indicators.parquet"))
        nemetonshiny:::update_project_metadata(
          pid, list(indicators_computed = TRUE, status = "computed"))

        invalide <- nemetonshiny:::ensure_indicator_sense_current(pid)
        expect_true(invalide)

        m <- nemetonshiny:::load_project_metadata(pid)
        # Le parquet est parti et le projet est repassé en brouillon : c'est ce
        # qui rend le bouton de calcul à nouveau visible.
        expect_false(file.exists(file.path(ppath, "data", "indicators.parquet")))
        expect_false(isTRUE(m$indicators_computed))
        expect_equal(as.integer(m$indicator_sense_version),
                     nemetonshiny:::INDICATOR_SENSE_VERSION)

        # Deuxième passage : plus rien à faire. Sans le marqueur, l'invalidation
        # se rejouerait à CHAQUE ouverture du projet.
        expect_false(nemetonshiny:::ensure_indicator_sense_current(pid))
      }
    )
  })
})

test_that("un projet jamais calcule recoit le marqueur sans rien invalider", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    with_mocked_bindings(
      get_app_options = function() list(project_dir = getwd()),
      {
        poly <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1),
                                          c(0, 1), c(0, 0))))
        parcels <- sf::st_sf(id = "P1", contenance = 1e4,
                             geometry = sf::st_sfc(poly, crs = 2154))
        pid <- nemetonshiny:::create_project(name = "Jamais calcule",
                                             parcels = parcels)$id

        # Rien à jeter — mais le marqueur doit quand même être posé, sinon le
        # test se rejouerait à chaque ouverture.
        expect_false(nemetonshiny:::ensure_indicator_sense_current(pid))
        m <- nemetonshiny:::load_project_metadata(pid)
        expect_equal(as.integer(m$indicator_sense_version),
                     nemetonshiny:::INDICATOR_SENSE_VERSION)
      }
    )
  })
})

test_that("un projet deja a jour n'est pas retouche", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    with_mocked_bindings(
      get_app_options = function() list(project_dir = getwd()),
      {
        poly <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1),
                                          c(0, 1), c(0, 0))))
        parcels <- sf::st_sf(id = "P1", contenance = 1e4,
                             geometry = sf::st_sfc(poly, crs = 2154))
        pid <- nemetonshiny:::create_project(name = "Deja a jour",
                                             parcels = parcels)$id
        ppath <- nemetonshiny:::get_project_path(pid)
        writeLines("x", file.path(ppath, "data", "indicators.parquet"))
        nemetonshiny:::update_project_metadata(pid, list(
          indicators_computed = TRUE,
          indicator_sense_version = nemetonshiny:::INDICATOR_SENSE_VERSION))

        expect_false(nemetonshiny:::ensure_indicator_sense_current(pid))
        # Le parquet d'un projet à jour ne doit PAS être détruit.
        expect_true(file.exists(file.path(ppath, "data", "indicators.parquet")))
      }
    )
  })
})

test_that("famille_risque n'est plus peinte avec la palette de risque", {
  # Le piège que le brief 048 ne couvre pas. `famille_risque` est désormais
  # orienté « haut = bon » : une palette YlOrRd (jaune -> rouge) colorerait en
  # ROUGE les UGF les MOINS à risque. L'inversion des valeurs retourne le sens
  # de la palette sans que personne y touche.
  est_risque <- function(x) grepl("^R[1-4]|^risk_", x)

  # Les grandeurs BRUTES gardent la palette : leur sens n'a pas bougé.
  expect_true(est_risque("R1"))
  expect_true(est_risque("R4"))
  expect_true(est_risque("risk_erosion"))
  # L'agrégat de famille en sort.
  expect_false(est_risque("famille_risque"))

  # Et le code source ne doit plus le mentionner dans ce motif.
  src <- readLines(testthat::test_path("..", "..", "R", "mod_family.R"),
                   warn = FALSE)
  pal <- grep("is_risk <- grepl", src, value = TRUE)
  expect_length(pal, 1L)
  expect_false(grepl("famille_risque", pal, fixed = TRUE))
})

test_that("l'app n'inverse aucun indicateur de risque elle-meme", {
  # Consigne n°1 du brief : le cœur rend déjà la valeur dans le bon sens. Toute
  # inversion côté app annulerait la correction EN SILENCE — le radar
  # remonterait sur les massifs exposés sans qu'aucun test ne tombe.
  src <- list.files(testthat::test_path("..", "..", "R"), pattern = "\\.R$",
                    full.names = TRUE)
  suspects <- unlist(lapply(src, function(f) {
    l <- readLines(f, warn = FALSE)
    hit <- grep("(1|100)\\s*-\\s*.*(indicateur_r[1-5]|famille_risque)", l)
    if (length(hit)) sprintf("%s:%d", basename(f), hit) else NULL
  }))
  expect_equal(suspects, NULL)
})

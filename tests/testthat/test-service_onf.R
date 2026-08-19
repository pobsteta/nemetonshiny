# Tests — service parcellaire forestier ONF (spec 046)
#
# Le cœur porte l'acquisition (`load_onf_parcelles_source`) et l'arithmétique
# du croisement (`croiser_parcelles_onf`) ; ces tests couvrent ce que l'app
# ajoute : le tri des issues du service, la construction du projet, et surtout
# l'invariant qui se casserait en silence — le pavage exact des parcelles
# cadastrales après un croisement.
#
# Les appels réseau sont mockés : le WFS ONF n'est pas joignable en CI.

.onf_test_parcelles <- function() {
  sf::st_sf(
    id = c("F001-1", "F001-2"),
    nom_ugf = c("FD X - parcelle 1", "FD X - parcelle 2"),
    foret_id = "F001", foret_nom = "FD X", parcelle = c("1", "2"),
    domaniale = TRUE, contenance = c(1e4, 1e4), surface_ha = c(1, 1),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(100, 0), c(100, 100), c(0, 100), c(0, 0)))),
      sf::st_polygon(list(rbind(c(100, 0), c(200, 0), c(200, 100), c(100, 100), c(100, 0)))),
      crs = 2154))
}

# Cadastre volontairement DÉCALÉ du parcellaire forestier : c'est la situation
# réelle (les deux découpages ne coïncident pas), et la seule qui teste quelque
# chose. C1 porte les deux UGF, C2 déborde en zone sans forêt publique.
.onf_test_cadastre <- function() {
  sf::st_sf(
    id = c("C1", "C2"), contenance = c(1.5e4, 1.5e4),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(150, 0), c(150, 100), c(0, 100), c(0, 0)))),
      sf::st_polygon(list(rbind(c(150, 0), c(300, 0), c(300, 100), c(150, 100), c(150, 0)))),
      crs = 2154))
}

.onf_test_projet <- function() {
  nemetonshiny:::ug_init_default(list(parcels = .onf_test_cadastre()))
}


# ---- onf_load_parcelles : trier les issues du service ----------------------

test_that("onf_load_parcelles distingue indisponible, vide et ok", {
  skip_if_not_installed("sf")
  aoi <- .onf_test_cadastre()

  # NULL du cœur = service muet (réseau / pare-feu / territoire inconnu).
  testthat::with_mocked_bindings(
    load_onf_parcelles_source = function(...) NULL,
    .package = "nemeton",
    {
      r <- nemetonshiny:::onf_load_parcelles(aoi)
      expect_equal(r$status, "unavailable")
      expect_null(r$parcelles)
    })

  # sf à 0 ligne = le service a répondu « pas de forêt publique ici ». C'est
  # une réponse, pas une panne : les deux ne doivent PAS produire le même
  # message côté UI, d'où deux statuts distincts.
  testthat::with_mocked_bindings(
    load_onf_parcelles_source = function(...) .onf_test_parcelles()[0, ],
    .package = "nemeton",
    {
      r <- nemetonshiny:::onf_load_parcelles(aoi)
      expect_equal(r$status, "empty")
      expect_equal(nrow(r$parcelles), 0L)
    })

  testthat::with_mocked_bindings(
    load_onf_parcelles_source = function(...) .onf_test_parcelles(),
    .package = "nemeton",
    {
      r <- nemetonshiny:::onf_load_parcelles(aoi)
      expect_equal(r$status, "ok")
      expect_equal(nrow(r$parcelles), 2L)
    })
})

test_that("onf_load_parcelles ne propage pas une erreur du coeur", {
  skip_if_not_installed("sf")
  # Un plantage du cœur (timeout, parse) doit devenir « indisponible » et
  # laisser le chemin cadastral utilisable, pas remonter en erreur Shiny.
  testthat::with_mocked_bindings(
    load_onf_parcelles_source = function(...) stop("boom réseau"),
    .package = "nemeton",
    {
      r <- nemetonshiny:::onf_load_parcelles(.onf_test_cadastre())
      expect_equal(r$status, "unavailable")
    })
})

test_that("onf_load_parcelles refuse une emprise absente et borne la domanialite", {
  skip_if_not_installed("sf")
  expect_equal(nemetonshiny:::onf_load_parcelles(NULL)$status, "no_aoi")
  expect_equal(nemetonshiny:::onf_load_parcelles("pas un sf")$status, "no_aoi")

  # Une valeur inattendue retombe sur "toutes" plutôt que d'être transmise au
  # cœur, qui la rejetterait.
  vu <- new.env()
  testthat::with_mocked_bindings(
    load_onf_parcelles_source = function(aoi, domanialite, ...) {
      vu$dom <- domanialite; .onf_test_parcelles()
    },
    .package = "nemeton",
    {
      nemetonshiny:::onf_load_parcelles(.onf_test_cadastre(), domanialite = "magie")
      expect_equal(vu$dom, "toutes")
      nemetonshiny:::onf_load_parcelles(.onf_test_cadastre(), domanialite = "domaniale")
      expect_equal(vu$dom, "domaniale")
    })
})


# ---- onf_projet_from_parcelles : une parcelle forestiere = une UGF ---------

test_that("onf_projet_from_parcelles nomme les UGF avec nom_ugf", {
  skip_if_not_installed("sf")
  projet <- .onf_test_projet()
  p <- nemetonshiny:::onf_projet_from_parcelles(projet, .onf_test_parcelles())

  expect_equal(nrow(p$tenements), 2L)
  expect_equal(nrow(p$ugs), 2L)
  # Sans le passage par geo_parcelle, les UGF s'appelleraient « F001-1 ».
  expect_setequal(p$ugs$label, c("FD X - parcelle 1", "FD X - parcelle 2"))
  # Les parcelles du projet sont REMPLACÉES par le parcellaire forestier.
  expect_equal(nrow(p$parcels), 2L)
  expect_true(all(p$parcels$id %in% c("F001-1", "F001-2")))
  expect_silent(nemetonshiny:::projet_validate(p))
})

test_that("onf_projet_from_parcelles remplace un parcellaire de taille differente", {
  skip_if_not_installed("sf")
  # Régression trouvée par la recette sur données réelles (forêt domaniale de
  # Chaux) : l'esquisse du brief passait par `utils::modifyList()`, qui RECURSE
  # dans les listes — et un data.frame en est une. Au lieu de remplacer l'objet
  # `parcels`, il fusionnait les colonnes de l'ancien parcellaire avec celles du
  # nouveau : erreur immédiate dès que les deux n'ont pas le même nombre de
  # lignes (« replacement has 427 rows, data has 1 »).
  #
  # Les tests précédents ne le voyaient PAS parce que cadastre et parcellaire y
  # avaient tous deux 2 lignes : la fusion « marchait » par accident. D'où ce
  # test avec des tailles VOLONTAIREMENT différentes — c'est le cas normal en
  # vrai, où une emprise de quelques parcelles cadastrales rencontre des
  # centaines de parcelles forestières.
  aoi <- .onf_test_cadastre()[1, ]                 # 1 parcelle
  projet <- nemetonshiny:::ug_init_default(list(parcels = aoi))
  expect_equal(nrow(projet$parcels), 1L)

  p <- nemetonshiny:::onf_projet_from_parcelles(projet, .onf_test_parcelles())
  expect_equal(nrow(p$parcels), 2L)                # 2 parcelles forestières
  expect_setequal(as.character(p$parcels$id), c("F001-1", "F001-2"))
  expect_equal(nrow(p$tenements), 2L)
  expect_equal(nrow(p$ugs), 2L)
  expect_silent(nemetonshiny:::projet_validate(p))
})

test_that("onf_projet_from_parcelles refuse un parcellaire vide", {
  skip_if_not_installed("sf")
  projet <- .onf_test_projet()
  expect_error(
    nemetonshiny:::onf_projet_from_parcelles(projet, .onf_test_parcelles()[0, ]),
    "non-empty")
})


# ---- onf_projet_croise : l'invariant qui compte ----------------------------

test_that("le croisement preserve le pavage exact de chaque parcelle cadastrale", {
  skip_if_not_installed("sf")
  # C'EST le test du lot. `tenement_import_replace()` remplace toute la couche
  # de tènements sans recréer de reliquat : sans `inclure_reste = TRUE`, les
  # parts de parcelle hors forêt publique perdraient leur tènement et la
  # parcelle cesserait d'être exactement pavée — en silence, puisque
  # `projet_validate()` ne vérifie pas le pavage.
  projet <- .onf_test_projet()
  cad <- .onf_test_cadastre()
  out <- nemetonshiny:::onf_projet_croise(projet, .onf_test_parcelles(),
                                          label_hors = "Hors foret publique")
  expect_equal(out$status, "ok")
  p <- out$projet

  for (pid in unique(p$tenements$parent_parcelle_id)) {
    aire_ten <- sum(as.numeric(sf::st_area(
      p$tenements[p$tenements$parent_parcelle_id == pid, ])))
    aire_par <- as.numeric(sf::st_area(cad[cad$id == pid, ]))
    expect_equal(aire_ten, aire_par, tolerance = 1e-6)
  }
})

test_that("le croisement produit des tenement_id uniques et des invariants valides", {
  skip_if_not_installed("sf")
  out <- nemetonshiny:::onf_projet_croise(.onf_test_projet(), .onf_test_parcelles(),
                                          label_hors = "Hors foret publique")
  p <- out$projet
  expect_equal(length(unique(p$tenements$tenement_id)), nrow(p$tenements))
  expect_silent(nemetonshiny:::projet_validate(p))
})

test_that("le reste hors foret publique recoit une UGF plutot que NA", {
  skip_if_not_installed("sf")
  # `croiser_parcelles_onf(inclure_reste = TRUE)` rend ces lignes avec
  # `nom_ugf = NA`. Sans étiquetage elles deviendraient des tènements sans UGF,
  # ce que l'invariant 2 interdit — l'import échouerait au lieu de dégrader.
  out <- nemetonshiny:::onf_projet_croise(.onf_test_projet(), .onf_test_parcelles(),
                                          label_hors = "Hors foret publique")
  p <- out$projet
  expect_true("Hors foret publique" %in% p$ugs$label)
  expect_false(any(is.na(p$tenements$ug_id)))
})

test_that("une UGF a cheval sur deux parcelles cadastrales donne UNE seule UGF", {
  skip_if_not_installed("sf")
  # La parcelle forestière 2 déborde de C1 sur C2 : elle doit rassembler ses
  # deux tènements sous une UGF unique, pas en créer une par cadastre.
  out <- nemetonshiny:::onf_projet_croise(.onf_test_projet(), .onf_test_parcelles(),
                                          label_hors = "Hors foret publique")
  p <- out$projet
  ug2 <- p$ugs$ug_id[p$ugs$label == "FD X - parcelle 2"]
  expect_length(ug2, 1L)
  tn <- p$tenements[p$tenements$ug_id == ug2, ]
  expect_equal(nrow(tn), 2L)
  expect_setequal(tn$parent_parcelle_id, c("C1", "C2"))
})

test_that("onf_projet_croise exige des donnees UGF et des parcelles", {
  skip_if_not_installed("sf")
  expect_error(
    nemetonshiny:::onf_projet_croise(list(parcels = .onf_test_cadastre()),
                                     .onf_test_parcelles()),
    "UG data")

  projet <- .onf_test_projet()
  projet$parcels <- NULL
  expect_error(
    nemetonshiny:::onf_projet_croise(projet, .onf_test_parcelles()),
    "parcels")
})

test_that("aucun recoupement rend no_overlap sans toucher au projet", {
  skip_if_not_installed("sf")
  # Régression : avec `inclure_reste = TRUE`, un parcellaire hors sujet rend
  # quand même une ligne par parcelle cadastrale — le reste. Se fier à
  # `nrow(ten) > 0` faisait donc passer ce cas pour un succès, et TOUS les
  # tènements étaient réétiquetés « hors forêt publique » : le découpage de
  # l'utilisateur détruit pour rien. Le signal juste est « aucune ligne
  # rattachée à une UGF ».
  projet <- .onf_test_projet()
  loin <- .onf_test_parcelles()
  sf::st_geometry(loin) <- sf::st_geometry(loin) + c(10000, 10000)
  sf::st_crs(loin) <- 2154

  out <- nemetonshiny:::onf_projet_croise(projet, loin,
                                          label_hors = "Hors foret publique")
  expect_equal(out$status, "no_overlap")
  # Le projet ressort INTACT : mêmes tènements, mêmes UGF, mêmes libellés.
  expect_equal(nrow(out$projet$tenements), nrow(projet$tenements))
  expect_equal(out$projet$ugs$ug_id, projet$ugs$ug_id)
  expect_equal(out$projet$ugs$label, projet$ugs$label)
  expect_false("Hors foret publique" %in% out$projet$ugs$label)
})


# ---- onf_croise_resume : lire le retour, ne rien recalculer ----------------

test_that("onf_croise_resume compte UGF, parcelles, cheval et hors-foret", {
  skip_if_not_installed("sf")
  out <- nemetonshiny:::onf_projet_croise(.onf_test_projet(), .onf_test_parcelles(),
                                          label_hors = "Hors foret publique")
  r <- nemetonshiny:::onf_croise_resume(out$tenements)

  expect_equal(r$n_ugf, 2L)
  expect_equal(r$n_parcelles, 2L)
  expect_equal(r$n_multi, 1L)          # la parcelle forestière 2 est à cheval
  expect_gt(r$surface_hors_ha, 0)      # C2 déborde hors forêt publique
})

test_that("onf_croise_resume tient une table vide et un tout-hors-foret", {
  expect_equal(nemetonshiny:::onf_croise_resume(NULL)$n_ugf, 0L)
  expect_equal(nemetonshiny:::onf_croise_resume(NULL)$surface_hors_ha, 0)

  # Table ne portant QUE du hors-forêt : pas d'UGF, mais la surface doit
  # quand même remonter (sinon l'utilisateur ne saurait pas ce qu'il perd).
  ten <- data.frame(ugf_id = NA_character_, nom_ugf = NA_character_,
                    parcelle_cadastrale = "C1", hors_ugf = TRUE,
                    surface_ha = 2.5, part_ugf = NA_real_)
  r <- nemetonshiny:::onf_croise_resume(ten)
  expect_equal(r$n_ugf, 0L)
  expect_equal(r$surface_hors_ha, 2.5)
})

test_that("onf_croise_resume signale les parcelles forestieres detenues en partie", {
  # part_ugf somme à 0,4 : l'utilisateur ne détient que 40 % de cette parcelle
  # forestière — c'est la première question d'un propriétaire.
  ten <- data.frame(
    ugf_id = c("F1", "F2"), nom_ugf = c("A", "B"),
    parcelle_cadastrale = c("C1", "C2"), hors_ugf = FALSE,
    surface_ha = c(1, 1), part_ugf = c(0.4, 1))
  r <- nemetonshiny:::onf_croise_resume(ten)
  expect_named(r$partielles, "F1")
  expect_equal(unname(r$partielles), 0.4)
})

test_that(".isTRUE_vec traite NA comme FALSE", {
  # Un `hors_ugf` à NA ne doit pas propager : il compterait une surface
  # « hors forêt » imaginaire.
  expect_equal(nemetonshiny:::.isTRUE_vec(c(TRUE, FALSE, NA)),
               c(TRUE, FALSE, FALSE))
})


# ---- Etiquette du reste ----------------------------------------------------

test_that(".onf_label_hors_ugf passe par i18n quand il est fourni", {
  i18n <- nemetonshiny:::get_i18n("fr")
  expect_equal(nemetonshiny:::.onf_label_hors_ugf(i18n),
               i18n$t("onf_hors_ugf_label"))
  # Sans i18n, un repli lisible plutôt qu'une clé brute.
  expect_true(nzchar(nemetonshiny:::.onf_label_hors_ugf(NULL)))
  expect_false(identical(nemetonshiny:::.onf_label_hors_ugf(NULL),
                         "onf_hors_ugf_label"))
})

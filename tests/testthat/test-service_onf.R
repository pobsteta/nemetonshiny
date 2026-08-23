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

  # v0.130.2.9001 — une valeur inattendue ne retombe PLUS sur « toutes » : elle
  # rend `no_domanialite`. Retomber sur « toutes » revenait à rapatrier un
  # parcellaire que personne n'a demandé, en silence ; mieux vaut dire que la
  # question n'a pas d'objet.
  vu <- new.env()
  testthat::with_mocked_bindings(
    load_onf_parcelles_source = function(aoi, domanialite, ...) {
      vu$dom <- domanialite; .onf_test_parcelles()
    },
    .package = "nemeton",
    {
      vu$dom <- "non appele"
      r <- nemetonshiny:::onf_load_parcelles(.onf_test_cadastre(),
                                             domanialite = "magie")
      expect_equal(r$status, "no_domanialite")
      expect_equal(vu$dom, "non appele")
      nemetonshiny:::onf_load_parcelles(.onf_test_cadastre(), domanialite = "domaniale")
      expect_equal(vu$dom, "domaniale")
    })
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

# ---- Domanialité : deux coches, plus de « Toutes » (v0.130.2.9001) ---------

test_that(".onf_domanialite traduit les coches vers l'argument du coeur", {
  # « Toutes » a disparu de l'UI parce qu'elle n'était que la conjonction des
  # deux autres. Le cœur, lui, attend toujours une chaîne unique.
  expect_equal(nemetonshiny:::.onf_domanialite(c("domaniale", "autre")), "toutes")
  expect_equal(nemetonshiny:::.onf_domanialite("domaniale"), "domaniale")
  expect_equal(nemetonshiny:::.onf_domanialite("autre"), "autre")
  # Ordre indifférent : ce sont des coches, pas une séquence.
  expect_equal(nemetonshiny:::.onf_domanialite(c("autre", "domaniale")), "toutes")

  # Aucune cochée n'est PAS « tout » : c'est une question sans objet.
  expect_null(nemetonshiny:::.onf_domanialite(character(0)))
  expect_null(nemetonshiny:::.onf_domanialite(NULL))
  expect_null(nemetonshiny:::.onf_domanialite(c("", NA)))

  # Une valeur déjà résolue passe telle quelle (appel direct au service, tests).
  expect_equal(nemetonshiny:::.onf_domanialite("toutes"), "toutes")
  # Une valeur inconnue ne doit pas être transmise au cœur.
  expect_null(nemetonshiny:::.onf_domanialite("magie"))
})

test_that("onf_load_parcelles rend no_domanialite sans appeler le coeur", {
  skip_if_not_installed("sf")
  appele <- FALSE
  testthat::with_mocked_bindings(
    load_onf_parcelles_source = function(...) { appele <<- TRUE; NULL },
    .package = "nemeton",
    {
      r <- nemetonshiny:::onf_load_parcelles(.onf_test_cadastre(),
                                             domanialite = character(0))
      expect_equal(r$status, "no_domanialite")
      expect_null(r$parcelles)
    })
  # Le garde est EN AMONT : pas de requête réseau pour une question sans objet.
  expect_false(appele)
})

test_that("les deux coches se traduisent en 'toutes' pour le coeur", {
  skip_if_not_installed("sf")
  vu <- new.env()
  testthat::with_mocked_bindings(
    load_onf_parcelles_source = function(aoi, domanialite, ...) {
      vu$dom <- domanialite; .onf_test_parcelles()
    },
    .package = "nemeton",
    {
      nemetonshiny:::onf_load_parcelles(.onf_test_cadastre(),
                                        domanialite = c("domaniale", "autre"))
      expect_equal(vu$dom, "toutes")
      nemetonshiny:::onf_load_parcelles(.onf_test_cadastre(),
                                        domanialite = "domaniale")
      expect_equal(vu$dom, "domaniale")
    })
})


# ---- Auto-sélection des parcelles concernées (v0.130.2.9001) ---------------

test_that("les parcelles sans foret sont couvertes et comptees par le coeur", {
  skip_if_not_installed("sf")
  # Depuis `nemeton 0.180.0` c'est le CŒUR qui écarte les parcelles qu'aucune
  # parcelle forestière ne rencontre, et qui expose le compteur via l'attribut
  # `parcelles_concernees`. L'app ne fait plus ce tri (elle le faisait en
  # v0.130.3, avec une réinjection maison désormais supprimée).
  #
  # Ce que ce test verrouille du côté app : une parcelle écartée n'est PAS
  # perdue — elle porte un tènement, rattaché à « hors forêt » — et le compteur
  # affiché à l'utilisateur vient bien du cœur, sans recalcul.
  cad <- .onf_test_cadastre()
  loin <- sf::st_sf(
    id = "C3", contenance = 1e4,
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(10000, 10000), c(10100, 10000), c(10100, 10100),
      c(10000, 10100), c(10000, 10000)))), crs = 2154))
  cad3 <- rbind(cad, loin)
  projet <- nemetonshiny:::ug_init_default(list(parcels = cad3))

  out <- nemetonshiny:::onf_projet_croise(projet, .onf_test_parcelles(),
                                          label_hors = "Hors foret publique")
  expect_equal(out$status, "ok")
  expect_equal(out$n_retenues, 2L)      # C1 et C2, pas C3
  expect_equal(out$n_total, 3L)

  p <- out$projet
  # C3 n'a pas disparu : elle porte un tènement, rattaché à « hors forêt ».
  expect_true("C3" %in% p$tenements$parent_parcelle_id)
  ug_hors <- p$ugs$ug_id[p$ugs$label == "Hors foret publique"]
  expect_length(ug_hors, 1L)
  expect_true(all(p$tenements$ug_id[p$tenements$parent_parcelle_id == "C3"]
                  %in% ug_hors))
  expect_silent(nemetonshiny:::projet_validate(p))

  # Pavage de C3 : la parcelle écartée est couverte en entier.
  a <- sum(as.numeric(sf::st_area(
    p$tenements[p$tenements$parent_parcelle_id == "C3", ])))
  b <- as.numeric(sf::st_area(cad3[cad3$id == "C3", ]))
  expect_equal(a, b, tolerance = 1e-4)
})

test_that("aucune parcelle concernee rend no_overlap et n'altere pas le projet", {
  skip_if_not_installed("sf")
  projet <- .onf_test_projet()
  loin <- .onf_test_parcelles()
  sf::st_geometry(loin) <- sf::st_geometry(loin) + c(10000, 10000)
  sf::st_crs(loin) <- 2154

  out <- nemetonshiny:::onf_projet_croise(projet, loin,
                                          label_hors = "Hors foret publique")
  expect_equal(out$status, "no_overlap")
  expect_equal(out$n_retenues, 0L)
  # Le projet ressort intact — aucune UGF « hors forêt » n'apparaît.
  expect_equal(out$projet$ugs$label, projet$ugs$label)
})

# ---- Purge des parcelles hors foret publique (v0.130.5.9001) ---------------

test_that("onf_purger_hors_foret raisonne par PARCELLE, jamais par tenement", {
  skip_if_not_installed("sf")
  # C'est la subtilité du lot. Une parcelle seulement EN PARTIE forestière porte
  # aussi un fragment « hors forêt » — la part que la forêt ne couvre pas. Ce
  # fragment est ce qui rend la parcelle exactement pavée : le supprimer
  # trouerait une parcelle que l'utilisateur possède. Le test porte donc sur la
  # PARCELLE, pas sur le tènement.
  cad <- .onf_test_cadastre()
  loin <- sf::st_sf(
    id = "C3", contenance = 1e4,
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(10000, 10000), c(10100, 10000), c(10100, 10100),
      c(10000, 10100), c(10000, 10000)))), crs = 2154))
  cad3 <- rbind(cad, loin)
  projet <- nemetonshiny:::ug_init_default(list(parcels = cad3))

  out <- nemetonshiny:::onf_projet_croise(projet, .onf_test_parcelles(),
                                          label_hors = "Hors foret publique")
  avant <- out$projet
  # C2 est mi-forestière : elle porte un tènement forestier ET un hors-forêt.
  expect_true("C2" %in% avant$tenements$parent_parcelle_id)
  expect_true("C3" %in% avant$tenements$parent_parcelle_id)

  purge <- nemetonshiny:::onf_purger_hors_foret(avant, "Hors foret publique")
  p <- purge$projet

  # Seule C3, entièrement hors forêt, disparaît.
  expect_equal(purge$n_supprimees, 1L)
  expect_false("C3" %in% p$tenements$parent_parcelle_id)
  expect_false("C3" %in% as.character(p$parcels$id))
  # C2 reste, AVEC son fragment hors forêt : sa surface est intacte.
  expect_true("C2" %in% p$tenements$parent_parcelle_id)
  a <- sum(as.numeric(sf::st_area(
    p$tenements[p$tenements$parent_parcelle_id == "C2", ])))
  b <- as.numeric(sf::st_area(cad3[cad3$id == "C2", ]))
  expect_equal(a, b, tolerance = 1e-4)
  expect_silent(nemetonshiny:::projet_validate(p))
})

test_that("la purge retire les parcelles de $parcels, pas seulement des tenements", {
  skip_if_not_installed("sf")
  # Les laisser dans $parcels produirait des parcelles SANS tènement : visibles
  # dans l'onglet Sélection, absentes de la carte UGF, rattachées à aucune unité
  # de gestion. Un état que le reste de l'app n'attend pas.
  cad <- .onf_test_cadastre()
  loin <- sf::st_sf(
    id = "C3", contenance = 1e4,
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(10000, 10000), c(10100, 10000), c(10100, 10100),
      c(10000, 10100), c(10000, 10000)))), crs = 2154))
  projet <- nemetonshiny:::ug_init_default(list(parcels = rbind(cad, loin)))
  out <- nemetonshiny:::onf_projet_croise(projet, .onf_test_parcelles(),
                                          label_hors = "Hors foret publique")
  p <- nemetonshiny:::onf_purger_hors_foret(out$projet, "Hors foret publique")$projet

  expect_equal(nrow(p$parcels), 2L)
  # Aucune parcelle orpheline : toute parcelle restante porte au moins un tènement.
  expect_setequal(as.character(p$parcels$id),
                  unique(as.character(p$tenements$parent_parcelle_id)))
})

test_that("la purge supprime l'UGF hors foret devenue vide", {
  skip_if_not_installed("sf")
  # Une UGF que plus aucun tènement ne porte violerait l'invariant 3.
  cad <- .onf_test_cadastre()
  # Ici les deux parcelles touchent la forêt, mais C2 déborde : son fragment
  # hors forêt subsiste, donc l'UGF « hors » doit RESTER.
  projet <- nemetonshiny:::ug_init_default(list(parcels = cad))
  out <- nemetonshiny:::onf_projet_croise(projet, .onf_test_parcelles(),
                                          label_hors = "Hors foret publique")
  purge <- nemetonshiny:::onf_purger_hors_foret(out$projet, "Hors foret publique")
  expect_equal(purge$n_supprimees, 0L)
  expect_true("Hors foret publique" %in% purge$projet$ugs$label)
  expect_silent(nemetonshiny:::projet_validate(purge$projet))
})

test_that("la purge est un no-op quand il n'y a pas d'UGF hors foret", {
  skip_if_not_installed("sf")
  projet <- .onf_test_projet()
  out <- nemetonshiny:::onf_purger_hors_foret(projet, "Hors foret publique")
  expect_equal(out$n_supprimees, 0L)
  expect_equal(nrow(out$projet$tenements), nrow(projet$tenements))
  expect_equal(out$projet$ugs$label, projet$ugs$label)
})

test_that("la purge retire aussi les parcelles forestieres a moins de 10 %", {
  skip_if_not_installed("sf")
  # Une parcelle que la forêt ne fait qu'effleurer est un effet de bord de
  # numérisation, pas un peuplement à gérer — et la porter dans le plan dilue
  # tous les indicateurs calculés par unité. Le seuil de 10 % englobe le cas
  # « aucune forêt » (part 0) et y ajoute ces parcelles-là.
  #
  # Projet construit à la main pour maîtriser les parts exactement : la
  # parcelle P90 est forestière à 90 %, P05 à 5 %.
  carre <- function(x0, x1) sf::st_polygon(list(rbind(
    c(x0, 0), c(x1, 0), c(x1, 100), c(x0, 100), c(x0, 0))))
  parcels <- sf::st_sf(
    id = c("P90", "P05"), contenance = c(1e4, 1e4),
    geometry = sf::st_sfc(carre(0, 100), carre(100, 200), crs = 2154))

  tenements <- sf::st_sf(
    tenement_id = c("t1", "t2", "t3", "t4"),
    parent_parcelle_id = c("P90", "P90", "P05", "P05"),
    ug_id = c("ug_f", "ug_h", "ug_f", "ug_h"),
    surface_m2 = c(9000, 1000, 500, 9500),
    surface_sig_m2 = c(9000, 1000, 500, 9500),
    geometry = sf::st_sfc(carre(0, 90), carre(90, 100),
                          carre(100, 105), carre(105, 200), crs = 2154))
  ugs <- data.frame(
    ug_id = c("ug_f", "ug_h"),
    label = c("Foret domaniale X", "Hors foret publique"),
    groupe = NA_character_, stringsAsFactors = FALSE)
  projet <- list(parcels = parcels, tenements = tenements, ugs = ugs)

  out <- nemetonshiny:::onf_purger_hors_foret(projet, "Hors foret publique",
                                              seuil_foret = 0.10)
  expect_equal(out$n_supprimees, 1L)
  # P05 (5 % de forêt) part ENTIÈRE, y compris son tènement forestier.
  expect_false("P05" %in% out$projet$tenements$parent_parcelle_id)
  expect_false("P05" %in% as.character(out$projet$parcels$id))
  # P90 reste entière, part hors forêt comprise.
  expect_setequal(out$projet$tenements$parent_parcelle_id, c("P90", "P90"))
  expect_silent(nemetonshiny:::projet_validate(out$projet))
})

test_that("le seuil de purge est parametrable et exclusif au bord", {
  skip_if_not_installed("sf")
  carre <- function(x0, x1) sf::st_polygon(list(rbind(
    c(x0, 0), c(x1, 0), c(x1, 100), c(x0, 100), c(x0, 0))))
  # Parcelle forestière à EXACTEMENT 10 %.
  projet <- list(
    parcels = sf::st_sf(id = "P10", contenance = 1e4,
                        geometry = sf::st_sfc(carre(0, 100), crs = 2154)),
    tenements = sf::st_sf(
      tenement_id = c("t1", "t2"), parent_parcelle_id = c("P10", "P10"),
      ug_id = c("ug_f", "ug_h"), surface_m2 = c(1000, 9000),
      surface_sig_m2 = c(1000, 9000),
      geometry = sf::st_sfc(carre(0, 10), carre(10, 100), crs = 2154)),
    ugs = data.frame(ug_id = c("ug_f", "ug_h"),
                     label = c("Foret domaniale X", "Hors foret publique"),
                     groupe = NA_character_, stringsAsFactors = FALSE))

  # Au seuil exact, la parcelle PART (`<=`, plus `<`). Cette inversion est le
  # prix d'un défaut à 0 % qui fasse quelque chose : avec `<`, un seuil de 0 %
  # ne supprimait RIEN, pas même une parcelle sans un mètre carré de forêt — un
  # réglage inerte à sa propre valeur par défaut.
  expect_equal(nemetonshiny:::onf_purger_hors_foret(
    projet, "Hors foret publique", seuil_foret = 0.10)$n_supprimees, 1L)
  # Sous le seuil, elle reste.
  expect_equal(nemetonshiny:::onf_purger_hors_foret(
    projet, "Hors foret publique", seuil_foret = 0.05)$n_supprimees, 0L)
  # Seuil 0 : la parcelle est forestière à 10 %, elle reste.
  expect_equal(nemetonshiny:::onf_purger_hors_foret(
    projet, "Hors foret publique", seuil_foret = 0)$n_supprimees, 0L)
  # Seuil aberrant -> défaut 0 %, qui ne retire pas une parcelle forestière.
  expect_equal(nemetonshiny:::onf_purger_hors_foret(
    projet, "Hors foret publique", seuil_foret = NA)$n_supprimees, 0L)
})

test_that("a 0 %, une parcelle SANS la moindre foret part", {
  skip_if_not_installed("sf")
  # Le cas que le défaut doit couvrir : la purge est cochée par défaut, à 0 %,
  # et ce qu'elle retire alors est exactement ce qui n'a rien de forestier.
  carre <- function(x0, x1) sf::st_polygon(list(rbind(
    c(x0, 0), c(x1, 0), c(x1, 100), c(x0, 100), c(x0, 0))))
  projet <- list(
    parcels = sf::st_sf(id = c("PF", "P0"), contenance = c(1e4, 1e4),
                        geometry = sf::st_sfc(carre(0, 100), carre(100, 200),
                                              crs = 2154)),
    tenements = sf::st_sf(
      tenement_id = c("t1", "t2"), parent_parcelle_id = c("PF", "P0"),
      ug_id = c("ug_f", "ug_h"), surface_m2 = c(1e4, 1e4),
      surface_sig_m2 = c(1e4, 1e4),
      geometry = sf::st_sfc(carre(0, 100), carre(100, 200), crs = 2154)),
    ugs = data.frame(ug_id = c("ug_f", "ug_h"),
                     label = c("Foret domaniale X", "Hors foret publique"),
                     groupe = NA_character_, stringsAsFactors = FALSE))

  out <- nemetonshiny:::onf_purger_hors_foret(projet, "Hors foret publique",
                                              seuil_foret = 0)
  expect_equal(out$n_supprimees, 1L)
  expect_false("P0" %in% as.character(out$projet$parcels$id))
  expect_true("PF" %in% as.character(out$projet$parcels$id))
})

test_that("la purge compte les parcelles restees partiellement forestieres", {
  skip_if_not_installed("sf")
  # Ce chiffre existe pour une raison d'interface : une ligne « Hors forêt
  # publique » qui subsiste après une purge se lit comme un échec, tant qu'on
  # ignore qu'elle porte les fragments des parcelles mi-forestières.
  carre <- function(x0, x1) sf::st_polygon(list(rbind(
    c(x0, 0), c(x1, 0), c(x1, 100), c(x0, 100), c(x0, 0))))
  projet <- list(
    parcels = sf::st_sf(
      id = c("P60", "P05"), contenance = c(1e4, 1e4),
      geometry = sf::st_sfc(carre(0, 100), carre(100, 200), crs = 2154)),
    tenements = sf::st_sf(
      tenement_id = c("t1", "t2", "t3", "t4"),
      parent_parcelle_id = c("P60", "P60", "P05", "P05"),
      ug_id = c("ug_f", "ug_h", "ug_f", "ug_h"),
      surface_m2 = c(6000, 4000, 500, 9500),
      surface_sig_m2 = c(6000, 4000, 500, 9500),
      geometry = sf::st_sfc(carre(0, 60), carre(60, 100),
                            carre(100, 105), carre(105, 200), crs = 2154)),
    ugs = data.frame(ug_id = c("ug_f", "ug_h"),
                     label = c("Foret domaniale X", "Hors foret publique"),
                     groupe = NA_character_, stringsAsFactors = FALSE))

  out <- nemetonshiny:::onf_purger_hors_foret(projet, "Hors foret publique",
                                              seuil_foret = 0.10)
  expect_equal(out$n_supprimees, 1L)     # P05, forestière à 5 %
  expect_equal(out$n_partielles, 1L)     # P60 garde sa part hors forêt
  # Et c'est bien elle qui maintient l'UGF « hors forêt » en vie.
  expect_true("Hors foret publique" %in% out$projet$ugs$label)
})

test_that("n_partielles vaut 0 quand la purge ne laisse que du forestier", {
  skip_if_not_installed("sf")
  carre <- function(x0, x1) sf::st_polygon(list(rbind(
    c(x0, 0), c(x1, 0), c(x1, 100), c(x0, 100), c(x0, 0))))
  projet <- list(
    parcels = sf::st_sf(
      id = c("PF", "P00"), contenance = c(1e4, 1e4),
      geometry = sf::st_sfc(carre(0, 100), carre(100, 200), crs = 2154)),
    tenements = sf::st_sf(
      tenement_id = c("t1", "t2"),
      parent_parcelle_id = c("PF", "P00"),
      ug_id = c("ug_f", "ug_h"),
      surface_m2 = c(1e4, 1e4), surface_sig_m2 = c(1e4, 1e4),
      geometry = sf::st_sfc(carre(0, 100), carre(100, 200), crs = 2154)),
    ugs = data.frame(ug_id = c("ug_f", "ug_h"),
                     label = c("Foret domaniale X", "Hors foret publique"),
                     groupe = NA_character_, stringsAsFactors = FALSE))

  out <- nemetonshiny:::onf_purger_hors_foret(projet, "Hors foret publique")
  expect_equal(out$n_supprimees, 1L)
  expect_equal(out$n_partielles, 0L)
  # Plus aucun fragment hors forêt : l'UGF disparaît (invariant 3).
  expect_false("Hors foret publique" %in% out$projet$ugs$label)
})

test_that("n_partielles est present meme quand la purge ne fait rien", {
  skip_if_not_installed("sf")
  projet <- .onf_test_projet()
  out <- nemetonshiny:::onf_purger_hors_foret(projet, "Hors foret publique")
  expect_equal(out$n_supprimees, 0L)
  # Le champ doit exister quoi qu'il arrive : le module lit `%||% 0L`, mais un
  # champ manquant signalerait un chemin de retour oublié.
  expect_false(is.null(out$n_partielles))
})

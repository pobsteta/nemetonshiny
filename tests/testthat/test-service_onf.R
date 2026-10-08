# Tests — service parcellaire forestier ONF (spec 046)
#
# Le cœur porte l'acquisition (`load_onf_parcelles_source`) et toute
# l'arithmétique du croisement (`construire_ugf_onf`, spec 058) ; ces tests
# couvrent ce que l'app ajoute : le tri des issues du service, la construction
# du projet, et surtout l'invariant qui se casserait en silence — le pavage
# exact des parcelles cadastrales après un croisement.
#
# Les appels réseau sont mockés : le WFS ONF n'est pas joignable en CI. En
# sélection « toutes », le cœur ne lit pas la DGFiP : il tourne pour de vrai
# sur les géométries de test. La sélection « foret », qui la lit, est simulée.

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


test_that(".isTRUE_vec traite NA comme FALSE", {
  # Un `hors_ugf` à NA ne doit pas propager : il compterait une surface
  # « hors forêt » imaginaire.
  expect_equal(nemetonshiny:::.isTRUE_vec(c(TRUE, FALSE, NA)),
               c(TRUE, FALSE, FALSE))
})


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


# ---- onf_projet_croise : l'invariant qui compte ----------------------------

# Géométries de test en Lambert 93 plausible : le calage élastique du cœur
# raisonne en mètres, à l'emprise réelle d'une commune.
.onf_l93 <- function(x) {
  sf::st_geometry(x) <- sf::st_geometry(x) + c(800000, 6700000)
  sf::st_crs(x) <- 2154
  x
}
.onf_cad_l93 <- function() {
  cad <- .onf_test_cadastre()
  cad$id <- c("21001000AA0001", "21001000AA0002")
  cad$code_insee <- "21001"
  .onf_l93(cad)
}
.onf_projet_l93 <- function() {
  nemetonshiny:::ug_init_default(list(parcels = .onf_cad_l93()))
}
# Seuils de rattachement abaissés : les parcelles de test font 1,5 ha.
.onf_params_test <- function(...) {
  utils::modifyList(
    utils::modifyList(nemetonshiny:::ONF_PARAMS_DEFAULT,
                      list(purger = FALSE, seuil = 0.1, seuil_hors = 0.1)),
    list(...))
}
.onf_croise_test <- function(projet = .onf_projet_l93(), onf = .onf_l93(.onf_test_parcelles()),
                             ...) {
  nemetonshiny:::onf_projet_croise(projet, onf, params = .onf_params_test(...))
}

test_that("le croisement preserve le pavage exact de chaque parcelle cadastrale", {
  skip_if_not_installed("sf")
  out <- .onf_croise_test()
  expect_equal(out$status, "ok")
  p <- out$projet
  cad <- .onf_cad_l93()
  for (pid in cad$id) {
    aire_ten <- sum(as.numeric(sf::st_area(
      p$tenements[p$tenements$parent_parcelle_id == pid, ])))
    aire_par <- as.numeric(sf::st_area(cad[cad$id == pid, ]))
    expect_equal(aire_ten, aire_par, tolerance = 1e-6)
  }
  expect_equal(length(unique(p$tenements$tenement_id)), nrow(p$tenements))
  expect_silent(nemetonshiny:::projet_validate(p))
})

test_that("les colonnes ONF arrivent sur les UGF, NA pour un bloc cad~", {
  skip_if_not_installed("sf")
  p <- .onf_croise_test()$projet
  onf <- p$ugs[!is.na(p$ugs$onf_parcelle), ]
  expect_setequal(onf$onf_parcelle, c("1", "2"))
  expect_true(all(onf$onf_foret_id == "F001"))
  expect_true(all(onf$onf_domaniale))
  expect_true(all(onf$onf_part > 0.9 & onf$onf_part <= 1))
  # AA0002 deborde du parcellaire ONF : son reste devient une UGF cadastrale,
  # habillee, sans colonne ONF.
  cad <- p$ugs[is.na(p$ugs$onf_parcelle), ]
  expect_equal(nrow(cad), 1L)
  expect_match(cad$label, "AA0002", fixed = TRUE)
  expect_true(is.na(cad$onf_foret_id))
})

test_that("une UGF a cheval sur deux parcelles cadastrales donne UNE seule UGF", {
  skip_if_not_installed("sf")
  p <- .onf_croise_test()$projet
  ug2 <- p$ugs$ug_id[p$ugs$label == "FD X - parcelle 2"]
  expect_length(ug2, 1L)
  expect_setequal(p$tenements$parent_parcelle_id[p$tenements$ug_id == ug2],
                  c("21001000AA0001", "21001000AA0002"))
})

test_that("le parcellaire ONF passe BRUT au coeur, avec les reglages du projet", {
  skip_if_not_installed("sf")
  # Le calage élastique a besoin du contour ONF qui déborde : un parcellaire
  # découpé par `clip_cadastre` lui retirerait ce qu'il recale.
  vu <- NULL
  local_mocked_bindings(
    construire_ugf_onf = function(...) { vu <<- list(...); NULL },
    .package = "nemeton")
  onf <- .onf_l93(.onf_test_parcelles())
  sf::st_geometry(onf)[[2]] <- sf::st_polygon(list(rbind(
    c(800100, 6700000), c(800400, 6700000), c(800400, 6700100),
    c(800100, 6700100), c(800100, 6700000))))
  testthat::local_mocked_bindings(
    onf_load_parcelles = function(parcels, domanialite, clip_cadastre) {
      expect_false(clip_cadastre)
      list(status = "ok", parcelles = onf)
    }, .package = "nemetonshiny")
  res <- nemetonshiny:::onf_croise_tache(
    .onf_projet_l93(),
    nemetonshiny:::project_onf_params(list(onf_params = list(tol = 7, purger = TRUE))))
  expect_equal(res$status, "unavailable")
  expect_identical(vu$parcelles_onf, onf)
  expect_identical(vu$selection, "foret")
  expect_identical(vu$tol, 7)
  expect_identical(vu$seuil_couverture, 0.5)
  # Un seul appel pour tout le projet : pas d'insee, le coeur le deduit.
  expect_null(vu$insee)
  expect_setequal(vu$cadastre$idu, c("21001000AA0001", "21001000AA0002"))
})

test_that("une source injoignable laisse le projet intact", {
  skip_if_not_installed("sf")
  local_mocked_bindings(construire_ugf_onf = function(...) NULL, .package = "nemeton")
  projet <- .onf_projet_l93()
  out <- .onf_croise_test(projet)
  expect_equal(out$status, "unavailable")
  expect_identical(out$projet, projet)
})

test_that("aucun recoupement rend no_overlap sans toucher au projet", {
  skip_if_not_installed("sf")
  # En sélection « toutes », un parcellaire hors sujet rend quand même une UGF
  # `cad~` par parcelle. L'appliquer détruirait le découpage de l'utilisateur
  # pour rien : le signal juste est « aucune UGF forestière ».
  projet <- .onf_projet_l93()
  loin <- .onf_l93(.onf_test_parcelles())
  sf::st_geometry(loin) <- sf::st_geometry(loin) + c(10000, 10000)
  sf::st_crs(loin) <- 2154
  out <- .onf_croise_test(projet, loin)
  expect_equal(out$status, "no_overlap")
  expect_identical(out$projet, projet)
})

test_that("selection foret : les parcelles ecartees quittent le projet, avec leur raison", {
  skip_if_not_installed("sf")
  # Le coeur lit la DGFiP en mode « foret » : simulé. AA0001 est retenue,
  # AA0002 ne touche pas l'ONF.
  local_mocked_bindings(
    construire_ugf_onf = function(parcelles_onf, cadastre, selection, ...) {
      expect_identical(selection, "foret")
      x <- cadastre[cadastre$idu == "21001000AA0001", "idu"]
      x$ugf_id <- "F001-1"; x$nom_ugf <- "FD X - parcelle 1"
      x$foret_id <- "F001"; x$foret_nom <- "FD X"; x$parcelle <- "1"
      x$domaniale <- TRUE; x$part_onf <- 1
      attr(x, "parcelles") <- data.frame(
        idu = cadastre$idu, retenue = c(TRUE, FALSE), raison = c(NA, "hors ONF"),
        proprietaire = c("ETAT", "PRIVE"), couverture_onf = c(1, 0))
      x
    }, .package = "nemeton")
  out <- .onf_croise_test(purger = TRUE)
  expect_equal(out$status, "ok")
  expect_equal(out$n_retenues, 1L)
  expect_equal(out$n_total, 2L)
  expect_identical(out$ecartees$idu, "21001000AA0002")
  expect_identical(out$ecartees$raison, "hors_onf")
  expect_identical(out$ecartees$proprietaire, "PRIVE")
  # Pas de parcelle sans tènement : elle part des DEUX couches.
  expect_identical(as.character(out$projet$parcels$id), "21001000AA0001")
  expect_false("21001000AA0002" %in% out$projet$tenements$parent_parcelle_id)
  expect_silent(nemetonshiny:::projet_validate(out$projet))
})

test_that(".onf_ecartees traduit les raisons du coeur", {
  cand <- data.frame(idu = c("A", "B", "C"),
                     raison = c("privee", "couverture < 50 %", "hors ONF"),
                     proprietaire = c("X", "COMMUNE", NA),
                     couverture_onf = c(0.8, 0.2, 0))
  e <- nemetonshiny:::.onf_ecartees(c("A", "B", "C", "D"), cand)
  expect_identical(e$raison, c("privee", "couverture", "hors_onf", "hors_onf"))
  expect_equal(e$couverture_onf, c(0.8, 0.2, 0, NA))
  i18n <- nemetonshiny:::get_i18n("fr")
  txt <- nemetonshiny:::onf_ecartees_texte(e, i18n)
  expect_match(txt, "B (", fixed = TRUE)
  expect_match(txt, "20", fixed = TRUE)
  expect_match(txt, "COMMUNE", fixed = TRUE)
  expect_identical(nrow(nemetonshiny:::.onf_ecartees(character(0), cand)), 0L)
})


# ---- onf_croise_resume : lire le retour, ne rien recalculer ----------------

test_that("onf_croise_resume compte UGF, parcelles, cheval et cad~", {
  skip_if_not_installed("sf")
  out <- .onf_croise_test()
  r <- nemetonshiny:::onf_croise_resume(out$tenements)
  expect_equal(r$n_parcelles, 2L)
  expect_equal(r$n_ugf, 3L)
  expect_equal(r$n_multi, 1L)   # la parcelle forestiere 2 est a cheval
  expect_equal(r$n_cad, 1L)
  expect_equal(nemetonshiny:::onf_croise_resume(NULL)$n_ugf, 0L)
})

test_that(".onf_labels_ugf habille une UGF purement cadastrale", {
  # Le coeur nomme une parcelle sans voisin forestier par sa reference brute
  # (`nom_ugf = "212000000A0036"`, `ugf_id = "cad~..."`). C'est la bonne
  # IDENTITE ; ce n'est pas un libelle qu'on lit dans un tableau a cote de
  # « Foret communale de Couchey - parcelle 12 ».
  ten <- data.frame(
    ugf_id  = c("F1", "cad~212000000A0036", "cad~212000000A0036", NA),
    nom_ugf = c("FD X - parcelle 12", "212000000A0036", "212000000A0037", NA),
    idu     = c("C1", "212000000A0036", "212000000A0037", "C9"),
    stringsAsFactors = FALSE)
  lab <- nemetonshiny:::.onf_labels_ugf(ten)
  expect_identical(lab[1], "FD X - parcelle 12")
  expect_match(lab[2], "212000000A0036", fixed = TRUE)
  expect_false(identical(lab[2], "212000000A0036"))
  # Une parcelle rattachee au bloc cad~ de sa voisine porte le libelle du
  # BLOC, pas le sien : sinon elle ferait une UGF a part.
  expect_identical(lab[3], lab[2])
  expect_match(lab[4], "C9", fixed = TRUE)
  # La detection est sur `ugf_id`, PAS sur la forme du nom.
  ten2 <- data.frame(ugf_id = "F9", nom_ugf = "12", idu = "C1",
                     stringsAsFactors = FALSE)
  expect_identical(nemetonshiny:::.onf_labels_ugf(ten2), "12")
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



# ---- Chemin A : projet depuis la foret publique d'une commune (spec 058) ---

# Commune de test : C1 et C2 touchent l'ONF, C3 (loin) non.
.onf_commune_test <- function() {
  cad <- .onf_cad_l93()
  loin <- sf::st_sf(id = "21001000AA0003", contenance = 1e4, code_insee = "21001",
                    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
                      c(810000, 6710000), c(810100, 6710000), c(810100, 6710100),
                      c(810000, 6710100), c(810000, 6710000)))), crs = 2154))
  rbind(cad[, c("id", "contenance", "code_insee")], loin)
}
.onf_mock_commune <- function(env = parent.frame(), cadastre = .onf_commune_test()) {
  local_mocked_bindings(
    get_commune_geometry = function(code_insee) NULL,
    get_cadastral_parcels = function(code_insee, commune_geometry = NULL) cadastre,
    get_communes_in_department = function(d) data.frame(code_insee = "21001",
                                                        nom = "Testville"),
    onf_load_parcelles = function(aoi, domanialite, clip_cadastre) {
      list(status = "ok", parcelles = .onf_l93(.onf_test_parcelles()))
    },
    .package = "nemetonshiny", .env = env)
}

test_that("le chemin A selectionne la foret parmi les parcelles qui touchent l'ONF", {
  skip_if_not_installed("sf")
  .onf_mock_commune()
  vu <- NULL
  local_mocked_bindings(
    construire_ugf_onf = function(parcelles_onf, cadastre, selection, ...) {
      vu <<- list(cadastre = cadastre$idu, selection = selection)
      x <- cadastre[cadastre$idu == "21001000AA0001", "idu"]
      x$ugf_id <- "F001-1"; x$nom_ugf <- "FD X - parcelle 1"
      x$foret_id <- "F001"; x$foret_nom <- "FD X"; x$parcelle <- "1"
      x$domaniale <- TRUE; x$part_onf <- 1
      attr(x, "parcelles") <- data.frame(
        idu = c("21001000AA0001", "21001000AA0002"), retenue = c(TRUE, FALSE),
        raison = c(NA, "privee"), proprietaire = c("ETAT", NA),
        couverture_onf = c(1, 0.3))
      attr(x, "calage") <- c(ecart_median_m = 2)
      x
    }, .package = "nemeton")

  # Le reglage `purger` du projet n'a pas cours : ce chemin SELECTIONNE.
  res <- nemetonshiny:::onf_projet_depuis_commune(
    "21001", params = .onf_params_test(purger = FALSE))
  expect_identical(res$status, "ok")
  expect_identical(vu$selection, "foret")
  # AA0003 ne touche pas l'ONF : ni candidate ni ecartee.
  expect_setequal(vu$cadastre, c("21001000AA0001", "21001000AA0002"))
  expect_identical(res$n_candidates, 2L)
  expect_identical(res$commune, "Testville")
  expect_identical(res$nom, "FD X")
  expect_identical(as.character(res$projet$parcels$id), "21001000AA0001")
  expect_identical(res$ecartees$idu, "21001000AA0002")
  expect_silent(nemetonshiny:::projet_validate(res$projet))
})

test_that("le chemin A trie les ecartees de la plus couverte a la moins couverte", {
  skip_if_not_installed("sf")
  .onf_mock_commune()
  local_mocked_bindings(
    construire_ugf_onf = function(parcelles_onf, cadastre, selection, ...) {
      x <- cadastre[cadastre$idu == "21001000AA0001", "idu"]
      x$ugf_id <- "F001-1"; x$nom_ugf <- "FD X - parcelle 1"; x$foret_nom <- "FD X"
      attr(x, "parcelles") <- data.frame(
        idu = c("21001000AA0001", "21001000AA0002"), retenue = c(TRUE, FALSE),
        raison = c(NA, "privee"), proprietaire = NA, couverture_onf = c(1, 0.9))
      x
    }, .package = "nemeton")
  cad <- .onf_commune_test()
  sf::st_geometry(cad)[[3]] <- sf::st_geometry(cad)[[2]] + c(0, 100)
  sf::st_crs(cad) <- 2154
  .onf_mock_commune(cadastre = cad)
  res <- nemetonshiny:::onf_projet_depuis_commune("21001")
  expect_identical(res$ecartees$raison[1], "privee")
  expect_true(all(diff(res$ecartees$couverture_onf[!is.na(res$ecartees$couverture_onf)]) <= 0))
})

test_that("le chemin A rend un statut sans rien ecrire quand une source manque", {
  skip_if_not_installed("sf")
  .onf_mock_commune()
  local_mocked_bindings(construire_ugf_onf = function(...) stop("ne doit pas etre appele"),
                        .package = "nemeton")
  local_mocked_bindings(get_cadastral_parcels = function(...) NULL,
                        .package = "nemetonshiny")
  expect_identical(nemetonshiny:::onf_projet_depuis_commune("21001")$status, "cadastre")

  .onf_mock_commune()
  local_mocked_bindings(
    onf_load_parcelles = function(...) list(status = "empty", parcelles = NULL),
    .package = "nemetonshiny")
  expect_identical(nemetonshiny:::onf_projet_depuis_commune("21001")$status, "empty")

  # Une commune ou l'ONF ne touche aucune parcelle.
  .onf_mock_commune(cadastre = .onf_commune_test()[3, ])
  expect_identical(nemetonshiny:::onf_projet_depuis_commune("21001")$status,
                   "no_overlap")
})

test_that("onf_creer_projet_commune ecrit un projet neuf avec ses UGF ONF", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")
  .onf_mock_commune()
  withr::with_tempdir({
    local_mocked_bindings(get_app_options = function() list(project_dir = getwd()),
                          .package = "nemetonshiny")
    # Vrai coeur, mode « toutes » simule par la selection : la foret est
    # choisie par un mock qui rend le vrai decoupage de AA0001 et AA0002.
    vrai <- nemeton::construire_ugf_onf
    local_mocked_bindings(
      construire_ugf_onf = function(..., selection) {
        x <- vrai(..., selection = "toutes")
        p <- attr(x, "parcelles"); p$raison <- NA_character_
        attr(x, "parcelles") <- p
        x
      }, .package = "nemeton")
    res <- nemetonshiny:::onf_projet_depuis_commune("21001", params = .onf_params_test())
    expect_identical(res$status, "ok")
    pid <- suppressMessages(nemetonshiny:::onf_creer_projet_commune(res, "Foret test"))
    p <- suppressMessages(nemetonshiny:::load_project(pid))
    expect_identical(p$metadata$name, "Foret test")
    expect_setequal(p$parcels$id, c("21001000AA0001", "21001000AA0002"))
    expect_equal(nrow(p$ugs), 3L)
    expect_setequal(stats::na.omit(p$ugs$onf_parcelle), c("1", "2"))
    expect_silent(nemetonshiny:::projet_validate(p))
  })
  expect_error(nemetonshiny:::onf_creer_projet_commune(list(status = "empty")),
               "Nothing to create")
})

test_that("une parcelle privee ecartee dit aussi sa couverture ONF", {
  i18n <- nemetonshiny:::get_i18n("fr")
  e <- data.frame(idu = c("A", "B"), raison = c("privee", "privee"),
                  proprietaire = NA_character_, couverture_onf = c(0.987, 0.001))
  txt <- nemetonshiny:::onf_ecartees_texte(e, i18n)
  expect_match(txt, "A (privée, couverture ONF 99 %)", fixed = TRUE)
  # Un effet de bord de numerisation (0,1 %) ne merite pas d'etre chiffre.
  expect_match(txt, "B (privée)", fixed = TRUE)
})

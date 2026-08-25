# Tests — import d'un projet depuis une liste CSV de parcelles cadastrales
#
# Le fichier est une ligne de références courtes (`A1;A2;AO212`) et **ne porte
# aucune trace de la commune** : celle-ci vient du NOM du fichier, par
# convention `commune-code_insee.csv`. C'est le point sensible du lot — `A1`
# existe dans presque toutes les communes de France, donc un INSEE erroné
# apparierait quelques parcelles par pure coïncidence.

.csv_ecrire <- function(dir, nom, contenu) {
  p <- file.path(dir, nom)
  writeLines(contenu, p)
  p
}

.csv_cadastre <- function() {
  # Deux sections, dont une à deux lettres, et des numéros ZÉRO-REMPLIS comme
  # le cadastre les stocke réellement (`0001` pour la parcelle 1).
  poly <- function(i) sf::st_polygon(list(rbind(
    c(i, 0), c(i + 1, 0), c(i + 1, 1), c(i, 1), c(i, 0))))
  sf::st_sf(
    id = c("212000000A0001", "212000000A0002", "2120000AO0212"),
    section = c("A", "A", "AO"),
    numero = c("0001", "0002", "0212"),
    contenance = c(10000, 20000, 30000),
    geometry = sf::st_sfc(poly(1), poly(2), poly(3), crs = 4326))
}


# ---- Lecture du nom et du contenu ------------------------------------------

test_that("parse_parcelles_csv lit la commune dans le NOM, les refs dans le contenu", {
  withr::with_tempdir({
    p <- .csv_ecrire(getwd(), "couchey-21200.csv", "A1;A2;AO212")
    r <- nemetonshiny:::parse_parcelles_csv(p)
    expect_equal(r$commune, "Couchey")
    expect_equal(r$code_insee, "21200")
    expect_equal(r$refs, c("A1", "A2", "AO212"))
  })
})

test_that("un nom hors convention est REFUSE, jamais devine", {
  withr::with_tempdir({
    # Deviner l'INSEE serait pire que refuser : on irait chercher le cadastre
    # d'une autre commune, où quelques références s'apparieraient par hasard.
    for (nom in c("parcelles.csv", "couchey.csv", "couchey-212.csv",
                  "couchey-2120A.csv")) {
      p <- .csv_ecrire(getwd(), nom, "A1")
      expect_null(suppressWarnings(nemetonshiny:::parse_parcelles_csv(p)),
                  info = nom)
    }
  })
})

test_that("le nom accepte la Corse et les communes composees", {
  withr::with_tempdir({
    p <- .csv_ecrire(getwd(), "ajaccio-2A004.csv", "A1")
    expect_equal(nemetonshiny:::parse_parcelles_csv(p)$code_insee, "2A004")

    p2 <- .csv_ecrire(getwd(), "la-vieille-loye-39560.csv", "A1")
    r2 <- nemetonshiny:::parse_parcelles_csv(p2)
    expect_equal(r2$commune, "La Vieille Loye")
    expect_equal(r2$code_insee, "39560")
  })
})

test_that("le contenu tolere plusieurs lignes, des espaces et des vides", {
  withr::with_tempdir({
    # Rien ne doit dépendre du fichier tenant sur une ligne : c'est ainsi que
    # celui-ci est écrit, pas une propriété du format.
    p <- .csv_ecrire(getwd(), "x-21200.csv", c(" A1 ; a2 ;", ";AO212;"))
    r <- nemetonshiny:::parse_parcelles_csv(p)
    expect_equal(r$refs, c("A1", "A2", "AO212"))
  })
})


# ---- Appariement des references --------------------------------------------

test_that("A1 s'apparie a la parcelle A0001 du cadastre", {
  skip_if_not_installed("sf")
  # LE test du lot. Le cadastre stocke `numero = "0001"` ; comparer les chaînes
  # brutes ferait échouer toute la liste. La comparaison porte sur le couple
  # (section, numéro ENTIER).
  r <- nemetonshiny:::resolve_parcelles_refs(c("A1", "A2", "AO212"),
                                             .csv_cadastre())
  expect_equal(nrow(r$parcelles), 3L)
  expect_length(r$absentes, 0L)
})

test_that("les references absentes sont rapportees, pas silencieusement perdues", {
  skip_if_not_installed("sf")
  r <- nemetonshiny:::resolve_parcelles_refs(c("A1", "ZZ999"), .csv_cadastre())
  expect_equal(nrow(r$parcelles), 1L)
  expect_equal(r$absentes, "ZZ999")
})

test_that("une reference sans numero ne s'apparie a rien", {
  skip_if_not_installed("sf")
  # Sans ce garde, `sub()` rendrait la référence telle quelle et « A »
  # s'apparierait à toute la section A.
  r <- nemetonshiny:::resolve_parcelles_refs(c("A", ""), .csv_cadastre())
  expect_equal(nrow(r$parcelles), 0L)
})

test_that("resolve_parcelles_refs tient un cadastre vide ou sans colonnes", {
  skip_if_not_installed("sf")
  vide <- .csv_cadastre()[0, ]
  expect_equal(nrow(nemetonshiny:::resolve_parcelles_refs("A1", vide)$parcelles), 0L)
  expect_equal(nrow(nemetonshiny:::resolve_parcelles_refs("A1", NULL)$parcelles), 0L)

  sans <- sf::st_sf(id = "x", geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326))
  r <- suppressWarnings(nemetonshiny:::resolve_parcelles_refs("A1", sans))
  expect_equal(nrow(r$parcelles), 0L)
})


# ---- Chaine complete, reseau mocke -----------------------------------------

test_that("importer_parcelles_csv distingue ses quatre echecs", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    # Les confondre laisserait l'utilisateur sans piste : un nom mal formé et
    # un INSEE qui ne correspond pas à la liste n'appellent pas le même geste.
    expect_equal(
      suppressWarnings(nemetonshiny:::importer_parcelles_csv(
        .csv_ecrire(getwd(), "sansinsee.csv", "A1")))$status,
      "bad_name")

    expect_equal(
      nemetonshiny:::importer_parcelles_csv(
        .csv_ecrire(getwd(), "x-21200.csv", ""))$status,
      "no_refs")

    p <- .csv_ecrire(getwd(), "x-21200.csv", "A1")
    testthat::with_mocked_bindings(
      get_cadastral_parcels = function(...) NULL,
      .package = "nemetonshiny",
      expect_equal(nemetonshiny:::importer_parcelles_csv(p)$status, "cadastre"))

    testthat::with_mocked_bindings(
      get_cadastral_parcels = function(...) .csv_cadastre(),
      .package = "nemetonshiny",
      {
        p2 <- .csv_ecrire(getwd(), "x-21200.csv", "ZZ999")
        expect_equal(nemetonshiny:::importer_parcelles_csv(p2)$status, "no_match")
      })
  })
})

test_that("une liste partiellement resolue est un SUCCES, avec son rapport", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    # Une parcelle a pu être fusionnée ou renumérotée depuis l'établissement de
    # la liste. Refuser l'import serait excessif ; se taire serait pire — la
    # surface obtenue passerait pour la surface demandée.
    p <- .csv_ecrire(getwd(), "couchey-21200.csv", "A1;ZZ999")
    testthat::with_mocked_bindings(
      get_cadastral_parcels = function(...) .csv_cadastre(),
      .package = "nemetonshiny",
      {
        r <- nemetonshiny:::importer_parcelles_csv(p)
        expect_equal(r$status, "ok")
        expect_equal(nrow(r$parcelles), 1L)
        expect_equal(r$absentes, "ZZ999")
        expect_equal(r$n_refs, 2L)
      })
  })
})

test_that("le bouton d'import est dans le bloc Tableau UGF du sidebar", {
  # Deux surfaces portent le nom " Tableau UGF " : le bloc repliable du sidebar
  # gauche et l'onglet du panneau principal. Le bouton est dans le SIDEBAR,
  # c'est la qu'on le cherche. Il y a ete deplace apres qu'on l'a cherche en
  # vain dans le sidebar alors qu'il vivait en en-tete du tableau.
  skip_if_not_installed("bslib")
  h <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_ug_table_actions_bar("ug"))
  )
  expect_true(grepl("ug-btn_import_csv", h, fixed = TRUE))
  # La portee est dite : le geste ne suit pas la selection du tableau, contrairement
  # aux trois actions qui le suivent dans ce meme bloc.
  expect_true(grepl(get_i18n("fr")$t("csv_import_scope_hint"), h, fixed = TRUE))
})

test_that("le bouton d'import n'est PAS duplique dans l'en-tete du tableau", {
  # Un second point d'entree serait deux fois l'occasion de declencher par
  # erreur un geste qui remplace le projet courant.
  skip_if_not_installed("bslib")
  h <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_ug_table_panel("ug"))
  )
  expect_false(grepl("btn_import_csv", h, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Remplacement du projet courant
#
# Un import REMPLACE : l'ancien projet est supprime, toutes composantes
# comprises. Ce qui se teste ici est la frontiere entre « remplacer » et
# « detruire sans rien mettre a la place ».
# ---------------------------------------------------------------------------

test_that("l'import supprime l'ancien projet et prend sa place", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    with_mocked_bindings(
      get_app_options = function() list(project_dir = getwd()),
      {
        poly <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1),
                                          c(0, 1), c(0, 0))))
        parcels <- sf::st_sf(id = "P1", contenance = 1e4,
                             geometry = sf::st_sfc(poly, crs = 2154))

        ancien <- nemetonshiny:::create_project(name = "Ancien",
                                                parcels = parcels)$id
        nouveau <- nemetonshiny:::create_project(name = "Nouveau",
                                                 parcels = parcels)$id
        chemin_ancien <- nemetonshiny:::get_project_path(ancien)
        expect_true(dir.exists(chemin_ancien))

        app_state <- shiny::reactiveValues(
          project_id = ancien,
          current_project = nemetonshiny:::load_project(ancien),
          family_comments = list(B = "un commentaire de l'ancien projet"))

        charge <- nemetonshiny:::load_project(nouveau)
        out <- nemetonshiny:::.remplacer_projet_courant(app_state, charge)

        expect_identical(out, nouveau)
        # L'ancien est parti, repertoire compris.
        expect_false(dir.exists(chemin_ancien))
        # `project_id` suit `current_project` : c'est lui que lit
        # `save_comments()` et lui qui porte le verrou.
        expect_identical(shiny::isolate(app_state$project_id), nouveau)
        expect_identical(shiny::isolate(app_state$current_project$id), nouveau)
        # Les commentaires du projet detruit ne suivent pas dans le nouveau.
        expect_length(shiny::isolate(app_state$family_comments), 0L)
        # Le calcul en cours parlait du projet detruit.
        expect_false(is.null(shiny::isolate(app_state$project_replaced)))
      }
    )
  })
})

test_that("sans projet de remplacement valide, RIEN n'est detruit", {
  # L'invariant qui compte : un import a mi-chemin ne doit jamais laisser
  # l'utilisateur sans rien. Le garde porte sur l'id du remplacant.
  skip_if_not_installed("sf")
  withr::with_tempdir({
    with_mocked_bindings(
      get_app_options = function() list(project_dir = getwd()),
      {
        poly <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1),
                                          c(0, 1), c(0, 0))))
        parcels <- sf::st_sf(id = "P1", contenance = 1e4,
                             geometry = sf::st_sfc(poly, crs = 2154))
        ancien <- nemetonshiny:::create_project(name = "Ancien",
                                                parcels = parcels)$id
        chemin <- nemetonshiny:::get_project_path(ancien)

        app_state <- shiny::reactiveValues(
          project_id = ancien,
          current_project = nemetonshiny:::load_project(ancien))

        for (charge in list(NULL, list(), list(id = ""), list(metadata = list()))) {
          expect_null(nemetonshiny:::.remplacer_projet_courant(app_state, charge))
        }

        expect_true(dir.exists(chemin))
        expect_identical(shiny::isolate(app_state$project_id), ancien)
      }
    )
  })
})

test_that("la modale previent AVANT de detruire, et son bouton passe au rouge", {
  skip_if_not_installed("bslib")
  i18n <- get_i18n("fr")
  # Le texte porte le nom du projet detruit et le mot qui compte.
  msg <- sprintf(i18n$t("csv_import_replace_warn"), "Chaux")
  expect_true(grepl("Chaux", msg, fixed = TRUE))
  expect_true(grepl("supprim", msg, fixed = TRUE))
  # Et la mention sous le bouton dit la portee du geste.
  expect_true(grepl("[Rr]emplace", i18n$t("csv_import_scope_hint")))
})


test_that("l'import CSV purge comme le bouton ONF, et persiste les parcelles", {
  # Constat de Pascal le 2026-08-25 : apres import de couchey-21200.csv, une UGF
  # « Hors foret publique » subsistait - 74 tenements, 50,15 ha sur 535,59 ha.
  # Le RESTE est produit a dessein (sans lui la parcelle cadastrale cesse d'etre
  # entierement pavee) ; ce qui manquait, c'est l'etape d'apres.
  #
  # Le correctif naif serait faux : la purge retire des PARCELLES, pas seulement
  # des tenements. `save_ug_data()` seul les laisserait revenir au prochain
  # chargement - le defaut paye en v0.130.7.
  f <- testthat::test_path("..", "..", "R", "mod_ug.R")
  testthat::skip_if_not(file.exists(f), "sources R absentes")
  code <- readLines(f, warn = FALSE)

  # Le bloc CSV : depuis `csv_file` jusqu'a la fin de son croisement.
  i_csv <- grep("confirm_import_csv", code)[1]
  testthat::skip_if(is.na(i_csv), "bloc CSV introuvable")
  bloc <- code[i_csv:min(length(code), i_csv + 220)]

  expect_true(any(grepl("onf_purger_hors_foret", bloc, fixed = TRUE)))
  expect_true(any(grepl("cfg_csv$seuil_foret", bloc, fixed = TRUE)))
  # ET la persistance des parcelles, sans quoi la purge serait cosmetique.
  expect_true(any(grepl("with_parcels", bloc, fixed = TRUE)))
  # L'utilisateur vient de fournir ces parcelles : on lui dit ce qu'on retire.
  # Le compte rendu passe par le MEME helper que le bouton ONF - il disait deux
  # choses differentes selon le chemin, alors que la purge y est identique.
  expect_true(any(grepl(".onf_notify_purge(", bloc, fixed = TRUE)))
  # Et il ne doit PAS etre conditionne a une suppression effective : une purge
  # qui ne trouve rien a prendre laisse quand meme l'UGF « Hors foret
  # publique » a l'ecran, et c'est justement ce silence qui la faisait passer
  # pour cassee (Couchey : 21 parcelles, TOUTES touchant la foret publique,
  # la plus faible a 5,05 % - donc rien a purger au seuil 0).
  expect_false(any(grepl("if (n_purgees > 0L) {", bloc, fixed = TRUE)))
  expect_true(any(grepl("n_partielles", bloc, fixed = TRUE)))
})

test_that("onf_purger_hors_foret compte les partielles meme sans suppression", {
  # Le retour anticipe « rien a supprimer » rendait n_partielles = 0, donc les
  # deux chemins se taisaient au moment ou l'explication etait la plus utile.
  ten <- data.frame(
    tenement_id = c("t1", "t2", "t3"),
    parent_parcelle_id = c("p1", "p1", "p2"),
    ug_id = c("u_for", "u_hors", "u_for"),
    surface_m2 = c(8000, 2000, 5000),
    stringsAsFactors = FALSE)
  ugs <- data.frame(
    ug_id = c("u_for", "u_hors"),
    label = c("For\u00eat", "Hors for\u00eat publique"),
    stringsAsFactors = FALSE)
  projet <- list(parcels = NULL, tenements = ten, ugs = ugs)

  # Seuil 0 : p1 est forestiere a 80 %, p2 a 100 % - aucune n'est a retirer.
  r <- onf_purger_hors_foret(projet, "Hors for\u00eat publique", seuil_foret = 0)
  expect_identical(r$n_supprimees, 0L)
  expect_identical(r$n_partielles, 1L)   # p1 garde sa part hors foret
})

test_that("onf_purger_hors_foret reste appelee sur les DEUX chemins d'entree", {
  # Elle n'etait appelee qu'a UNE ligne de toute l'application - celle du
  # bouton ONF. C'est ce qui rendait l'ecart invisible.
  f <- testthat::test_path("..", "..", "R", "mod_ug.R")
  testthat::skip_if_not(file.exists(f), "sources R absentes")
  code <- readLines(f, warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  appels <- sum(grepl("onf_purger_hors_foret\\(", code))
  expect_gte(appels, 2L)
})

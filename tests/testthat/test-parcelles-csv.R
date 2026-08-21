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

test_that("le bouton d'import figure dans le bloc Tableau UGF", {
  skip_if_not_installed("bslib")
  h <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_ug_table_panel("ug"))
  )
  expect_true(grepl("ug-btn_import_csv", h, fixed = TRUE))
})

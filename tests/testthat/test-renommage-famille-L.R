# Renommage des deux colonnes L (coeur v0.176.0, spec 045).
#
#   indicateur_l2_fragmentation -> indicateur_l1_effet_lisiere  (effet lisiere)
#   indicateur_l1_sylvosphere   -> indicateur_l2_morcellement   (fragmentation)
#
# Aucune valeur ne change : chaque fonction portait le nom de la metrique de
# l'AUTRE, et ce sont les slugs qui cessent de mentir. Ce qui se teste ici est
# la frontiere : un projet calcule AVANT le renommage doit rester lisible.
#
# Brief : `specs/brief-nemetonshiny-renommage-famille-L.md`.

test_that("la famille L de l'app porte les nouveaux slugs", {
  # `INDICATOR_FAMILIES` vient du coeur : c'est par lui que `mod_family`
  # decide quelles colonnes afficher dans l'onglet Paysage.
  fam <- nemetonshiny:::get_family_config("L")

  expect_true("indicateur_l1_effet_lisiere" %in% fam$column_names)
  expect_true("indicateur_l2_morcellement" %in% fam$column_names)
  expect_false("indicateur_l2_fragmentation" %in% fam$column_names)
  expect_false("indicateur_l1_sylvosphere" %in% fam$column_names)

  # Le croisement code <-> colonne de la famille L a disparu : L1 pointe
  # desormais sur le slug l1. Celui de la famille F, lui, subsiste.
  expect_identical(fam$column_names[match("L1", fam$indicators)],
                   "indicateur_l1_effet_lisiere")
  expect_identical(
    nemetonshiny:::get_family_config("F")$column_names[
      match("F1", nemetonshiny:::get_family_config("F")$indicators)],
    "indicateur_f2_erosion")
})

test_that("les nouvelles fonctions existent dans le coeur", {
  expect_true(exists("indicateur_l1_effet_lisiere", mode = "function",
                     envir = asNamespace("nemeton")))
  expect_true(exists("indicateur_l2_morcellement", mode = "function",
                     envir = asNamespace("nemeton")))
})

test_that("un projet calcule avant le renommage reste lisible", {
  # C'est le point qui compte du brief : sans migration a la lecture, les deux
  # cartes Paysage d'un ancien projet disparaissent de l'onglet - la table des
  # familles ne connait plus ces noms.
  skip_if_not_installed("arrow")

  withr::with_tempdir({
    dir.create(file.path("proj", "data"), recursive = TRUE)

    ancien <- data.frame(
      id_parcelle                      = c("A", "B"),
      indicateur_l1_sylvosphere        = c(75.2, 69.8),
      indicateur_l2_fragmentation      = c(60.4, 55.1),
      indicateur_l2_fragmentation_norm = c(0.60, 0.55),
      famille_paysage                  = c(67.8, 62.5)
    )
    arrow::write_parquet(ancien, file.path("proj", "data", "indicators.parquet"))

    testthat::local_mocked_bindings(
      get_project_path = function(project_id) "proj",
      load_project_metadata = function(project_id) list()
    )

    out <- nemetonshiny:::load_indicators("ancien_projet")

    expect_true("indicateur_l1_effet_lisiere" %in% names(out))
    expect_true("indicateur_l2_morcellement" %in% names(out))
    expect_false("indicateur_l1_sylvosphere" %in% names(out))
    expect_false("indicateur_l2_fragmentation" %in% names(out))

    # Les valeurs suivent leur colonne : l'effet lisiere etait stocke sous
    # `l2_fragmentation`. Une migration qui se tromperait de sens rendrait
    # 75.2 ici, et le test passerait a cote.
    expect_equal(out$indicateur_l1_effet_lisiere, c(60.4, 55.1))
    expect_equal(out$indicateur_l2_morcellement, c(75.2, 69.8))
    expect_equal(out$indicateur_l1_effet_lisiere_norm, c(0.60, 0.55))

    # Le reste du jeu est intact.
    expect_equal(out$famille_paysage, c(67.8, 62.5))
    expect_equal(out$id_parcelle, c("A", "B"))
  })
})

test_that("un projet deja migre traverse la lecture inchange", {
  skip_if_not_installed("arrow")

  withr::with_tempdir({
    dir.create(file.path("proj", "data"), recursive = TRUE)
    neuf <- data.frame(
      indicateur_l1_effet_lisiere = 60.4,
      indicateur_l2_morcellement  = 75.2
    )
    arrow::write_parquet(neuf, file.path("proj", "data", "indicators.parquet"))

    testthat::local_mocked_bindings(
      get_project_path = function(project_id) "proj",
      load_project_metadata = function(project_id) list()
    )

    out <- nemetonshiny:::load_indicators("projet_neuf")
    expect_equal(out$indicateur_l1_effet_lisiere, 60.4)
    expect_equal(out$indicateur_l2_morcellement, 75.2)
  })
})

test_that(".slugs_l_vers_schema rend les noms du schema PostGIS", {
  # Le schema porte encore les anciens noms ; l'appariement se lit PAR VALEUR.
  df <- data.frame(
    indicateur_l1_effet_lisiere      = 60.4,
    indicateur_l2_morcellement       = 75.2,
    indicateur_l1_effet_lisiere_norm = 0.60,
    famille_paysage                  = 67.8
  )
  out <- nemetonshiny:::.slugs_l_vers_schema(df)

  expect_equal(out$indicateur_l2_fragmentation, 60.4)
  expect_equal(out$indicateur_l1_sylvosphere, 75.2)
  expect_equal(out$indicateur_l2_fragmentation_norm, 0.60)
  expect_equal(out$famille_paysage, 67.8)
  expect_false("indicateur_l1_effet_lisiere" %in% names(out))
})

test_that("ecriture puis lecture rendent les valeurs d'origine", {
  # L'aller-retour app -> schema -> app est l'invariant qui protege les
  # donnees existantes. Il doit tenir sans qu'aucune valeur ne change de
  # colonne en chemin.
  origine <- data.frame(
    indicateur_l1_effet_lisiere = c(60.4, 55.1),
    indicateur_l2_morcellement  = c(75.2, 69.8)
  )

  vers_db  <- nemetonshiny:::.slugs_l_vers_schema(origine)
  retour   <- nemeton::migrer_colonnes_l(vers_db, quiet = TRUE)

  expect_equal(retour[, names(origine)], origine)
})

test_that(".slugs_l_vers_schema laisse un jeu etranger intact", {
  df <- data.frame(x = 1, indicateur_c1_biomasse = 80.1)
  expect_identical(nemetonshiny:::.slugs_l_vers_schema(df), df)
})

# Les libelles d'indicateur suivent la COLONNE, pas le slug de la colonne.
#
# Une colonne porte le nom de la fonction qui la remplit. Le coeur a renomme
# les deux fonctions L en v0.176.0 (spec 045) puis decroise la famille F en
# v0.182.0 (spec 049) : plus AUCUNE famille n'est croisee aujourd'hui, code,
# colonne et slug concordent partout.
#
# Ces tests gardent leur sens sans ce croisement, mais il faut voir lequel : ils
# ne peuvent plus distinguer une lecture par colonne d'une lecture par slug -
# les deux donnent la meme reponse. Ce qu'ils verrouillent desormais, c'est la
# concordance elle-meme : si le coeur recroise ou renomme, ils tombent, et le
# helper devra rester indexe sur la COLONNE.
#
# Suite de `specs/brief-nemetonshiny-libelles-famille-L.md` (livre v0.127.1)
# puis de `specs/brief-nemetonshiny-renommage-famille-L.md`.

test_that("les deux colonnes L portent desormais le bon slug", {
  # Depuis v0.176.0 le slug et le contenu concordent.
  fr <- nemetonshiny:::indicator_label_by_column("indicateur_l1_effet_lisiere", "fr")
  expect_true(grepl("[Ss]ylvosph", fr))
  expect_false(grepl("[Ff]ragmentation", fr))

  # ... et reciproquement.
  fr2 <- nemetonshiny:::indicator_label_by_column("indicateur_l2_morcellement", "fr")
  expect_true(grepl("[Ff]ragmentation", fr2))
  expect_false(grepl("[Ss]ylvosph", fr2))
})

test_that("la concordance L tient aussi en anglais", {
  en <- nemetonshiny:::indicator_label_by_column("indicateur_l1_effet_lisiere", "en")
  expect_true(grepl("[Ss]ylvosphere", en))
  expect_false(grepl("[Ff]ragmentation", en))

  en2 <- nemetonshiny:::indicator_label_by_column("indicateur_l2_morcellement", "en")
  expect_true(grepl("[Ff]ragmentation", en2))
  expect_false(grepl("[Ss]ylvosphere", en2))
})

test_that("la famille F, decroisee en v0.182.0, concorde", {
  ero <- nemetonshiny:::indicator_label_by_column("indicateur_f2_erosion", "fr")
  expect_true(grepl("rosion", ero))
  expect_false(grepl("[Ff]ertilit", ero))

  fer <- nemetonshiny:::indicator_label_by_column("indicateur_f1_fertilite", "fr")
  expect_true(grepl("[Ff]ertilit", fer))
  expect_false(grepl("rosion", fer))
})

test_that("with_family prefixe le nom de famille du coeur", {
  fr <- nemetonshiny:::indicator_label_by_column(
    "indicateur_l1_effet_lisiere", "fr", with_family = TRUE)
  expect_true(grepl(" - ", fr, fixed = TRUE))
  expect_true(grepl("[Ss]ylvosph", sub("^.* - ", "", fr)))

  en <- nemetonshiny:::indicator_label_by_column(
    "indicateur_l1_effet_lisiere", "en", with_family = TRUE)
  expect_false(identical(sub(" - .*$", "", fr), sub(" - .*$", "", en)))
})

test_that("les 41 colonnes du coeur se resolvent toutes", {
  # L'ancienne table locale ignorait W4, A3-A5, T3, R5-R7 : ils sortaient en
  # nom de colonne brut dans la barre de progression. Balayage complet.
  cols <- nemeton::indicator_labels()$column_name
  expect_gte(length(cols), 41L)

  for (lang in c("fr", "en")) {
    manquants <- Filter(
      function(cn) is.null(nemetonshiny:::indicator_label_by_column(cn, lang)),
      cols
    )
    expect_identical(manquants, character(0),
                     info = paste("colonnes non resolues en", lang))
  }
})

test_that("le suffixe _norm est ignore, l'inconnu rend NULL", {
  expect_identical(
    nemetonshiny:::indicator_label_by_column("indicateur_l1_effet_lisiere_norm", "fr"),
    nemetonshiny:::indicator_label_by_column("indicateur_l1_effet_lisiere", "fr")
  )
  expect_null(nemetonshiny:::indicator_label_by_column("indicateur_zz_inexistant", "fr"))
  expect_null(nemetonshiny:::indicator_label_by_column(NULL, "fr"))
  expect_null(nemetonshiny:::indicator_label_by_column("", "fr"))
})

test_that("la ligne de progression annonce la grandeur calculee", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  msg <- nemetonshiny:::translate_task_message(
    "compute:indicateur_l1_effet_lisiere", i18n_fr)
  expect_true(grepl("[Ss]ylvosph", msg))
  expect_false(grepl("[Ff]ragmentation", msg))

  msg_en <- nemetonshiny:::translate_task_message(
    "compute:indicateur_l2_morcellement", i18n_en)
  expect_true(grepl("[Ff]ragmentation", msg_en))
  expect_false(grepl("[Ss]ylvosphere", msg_en))

  # Les indicateurs recents ne sortent plus en nom de colonne brut.
  r5 <- nemetonshiny:::translate_task_message("compute:indicateur_r5_deperissement", i18n_fr)
  expect_false(grepl("indicateur_r5", r5, fixed = TRUE))
})

test_that("aucune table i18n indexee par nom de colonne ne revient", {
  # Ces cles etaient une troisieme copie des libelles, ecrite selon la
  # semantique du CODE alors qu'elle indexait des COLONNES.
  i18n <- nemetonshiny:::get_i18n("fr")
  for (k in c("indicateur_l2_morcellement", "indicateur_l1_effet_lisiere",
              "indicateur_l1_sylvosphere", "indicateur_l2_fragmentation",
              "indicateur_f1_fertilite", "indicateur_f2_erosion",
              "indicateur_c1_biomasse")) {
    expect_false(i18n$has(k), info = k)
  }
})

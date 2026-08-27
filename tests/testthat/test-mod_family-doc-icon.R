# Spec 052 - icone "fiche indicateur", a cote du "i"
#
# Deux niveaux de test, deliberement separes :
#
#  * le MECANISME (`doc_icon()`) se teste sur des lignes synthetiques. Il n'a
#    besoin d'aucun coeur particulier et couvre les cas qui ne se produiront
#    jamais en recette mais se produiront un jour en production : colonne
#    absente, `NA`, langue servie differente de la langue demandee.
#  * le BRANCHEMENT sur le coeur (`get_indicator_doc()`) se teste sur C1
#    (documente) et C2 (non documente), et se saute quand le coeur installe est
#    anterieur a 0.192.0.
#
# Aucun des deux ne fige la liste des indicateurs documentes ni leur nombre :
# la premiere fiche ajoutee cote coeur est une bonne nouvelle, elle ne doit pas
# faire rougir cette suite.

test_that("doc_icon renders a new-tab link for a documented indicator", {
  skip_if_not_installed("bsicons")
  i18n <- nemetonshiny:::get_i18n("fr")

  row <- list(
    doc_url = "https://pobsteta.github.io/nemeton/articles/fiche-c1-biomasse_fr.html",
    doc_lang = "fr"
  )
  icon <- nemetonshiny:::doc_icon(row, "fr", i18n)

  expect_false(is.null(icon))
  html <- as.character(icon)
  expect_match(html, "fiche-c1-biomasse", fixed = TRUE)
  expect_match(html, 'target="_blank"', fixed = TRUE)
  expect_match(html, 'rel="noopener noreferrer"', fixed = TRUE)
  # Le libelle est lu par un lecteur d'ecran : il doit etre traduit, pas
  # litteral, et present sur les deux attributs.
  expect_match(html, i18n$t("indicateur_fiche_ouvrir"), fixed = TRUE)
  expect_match(html, "aria-label", fixed = TRUE)
})

test_that("doc_icon returns NULL when the indicator has no fact sheet", {
  i18n <- nemetonshiny:::get_i18n("fr")

  expect_null(nemetonshiny:::doc_icon(NULL, "fr", i18n))
  expect_null(nemetonshiny:::doc_icon(list(doc_url = NA_character_), "fr", i18n))
  expect_null(nemetonshiny:::doc_icon(list(doc_url = ""), "fr", i18n))
  # Coeur < 0.192.0 : la colonne n'existe pas. `is.na(NULL)` rend `logical(0)`,
  # qu'un `if` refuse - c'est le piege que garde le test de longueur.
  expect_null(nemetonshiny:::doc_icon(list(label = "C1"), "fr", i18n))
  expect_null(nemetonshiny:::doc_icon(list(doc_url = character(0)), "fr", i18n))
})

test_that("doc_icon announces a fact sheet served in another language", {
  skip_if_not_installed("bsicons")

  row <- list(
    doc_url = "https://pobsteta.github.io/nemeton/articles/fiche-c1-biomasse_fr.html",
    doc_lang = "fr"
  )

  en <- as.character(nemetonshiny:::doc_icon(row, "en", nemetonshiny:::get_i18n("en")))
  expect_match(en, "in French", fixed = TRUE)

  # Meme langue demandee et servie : aucune mention ajoutee. On compare a la
  # cle i18n, pas a un litteral accentue - le test suit la traduction.
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  fr <- as.character(nemetonshiny:::doc_icon(row, "fr", i18n_fr))
  expect_false(grepl(i18n_fr$t("langue_fr"), fr, fixed = TRUE))
})

test_that("doc_icon tolerates a row of nemeton::indicator_labels()", {
  skip_if_not_installed("bsicons")
  ind <- nemeton::indicator_labels(lang = "fr")
  skip_if_not("doc_url" %in% names(ind), "nemeton < 0.192.0: pas de fiches")

  c1 <- ind[ind$code == "C1", , drop = FALSE]
  icon <- nemetonshiny:::doc_icon(c1, "fr", nemetonshiny:::get_i18n("fr"))
  expect_false(is.null(icon))
  expect_match(as.character(icon), "fiche-c1-biomasse", fixed = TRUE)
})

test_that("get_indicator_doc reads the fact sheets from the core", {
  ind <- nemeton::indicator_labels(lang = "fr")
  skip_if_not("doc_url" %in% names(ind), "nemeton < 0.192.0: pas de fiches")

  # C1 est documente aujourd'hui ; C2 ne l'est pas. On teste le mecanisme sur
  # les deux, pas un decompte de fiches.
  doc <- nemetonshiny:::get_indicator_doc("C1", "fr")
  expect_false(is.null(doc))
  expect_match(doc$doc_url, "^https://", fixed = FALSE)
  expect_true(doc$doc_lang %in% c("fr", "en"))

  # Colonne longue et suffixe `_norm` resolus comme pour l'infobulle.
  expect_equal(nemetonshiny:::get_indicator_doc("indicateur_c1_biomasse", "fr"), doc)
  expect_equal(nemetonshiny:::get_indicator_doc("indicateur_c1_biomasse_norm", "fr"), doc)

  expect_null(nemetonshiny:::get_indicator_doc("C2", "fr"))
  expect_null(nemetonshiny:::get_indicator_doc("unknown_indicator", "fr"))
})

test_that("get_indicator_doc serves the other language rather than nothing", {
  ind <- nemeton::indicator_labels(lang = "en")
  skip_if_not("doc_url" %in% names(ind), "nemeton < 0.192.0: pas de fiches")

  doc_en <- nemetonshiny:::get_indicator_doc("C1", "en")
  expect_false(is.null(doc_en))
  # Une fiche existe, quelle que soit la langue reellement servie. Le test ne
  # fige PAS `doc_lang == "fr"` : le jour ou `fiche-c1-biomasse_en.Rmd` est
  # ecrite cote coeur, `doc_lang` passe a "en" sans qu'on touche a l'app - ce
  # test doit rester vert ce jour-la.
  expect_true(doc_en$doc_lang %in% c("fr", "en"))
})

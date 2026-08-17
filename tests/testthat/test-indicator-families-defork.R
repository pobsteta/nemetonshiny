# Identity test: the app's family table must BE the core's, not a copy of it
# R/app_config.R - .build_indicator_families()
#
# `app_config.R` used to restate the 12 families. That copy had drifted twice in
# ways the user saw: A5 was missing from family A (computed, then filtered out at
# display time), and the erosion column came out labelled "Fertilité des sols".
# This file is what stops a copy from creeping back.

test_that("the app declares no family table of its own", {
  src <- readLines("../../R/app_config.R", warn = FALSE)
  # Une re-declaration littérale est exactement ce qui a dérivé la première fois.
  expect_false(any(grepl("^INDICATOR_FAMILIES <- list\\(", src)))
  expect_true(any(grepl("delayedAssign\\(\"INDICATOR_FAMILIES\"", src)))
})


test_that("every family matches the core, family by family", {
  skip_if_not_installed("nemeton")

  fam <- nemeton::indicator_families()
  app <- nemetonshiny:::INDICATOR_FAMILIES

  expect_equal(length(app), nrow(fam))
  # L'ordre porte du sens : il pilote l'ordre des onglets et des axes du radar.
  expect_identical(names(app), fam$code)

  for (i in seq_len(nrow(fam))) {
    code <- fam$code[i]
    expect_identical(app[[code]]$code, code, info = code)
    expect_identical(app[[code]]$name_fr, fam$name_fr[i], info = code)
    expect_identical(app[[code]]$name_en, fam$name_en[i], info = code)
    expect_identical(app[[code]]$icon, fam$icon[i], info = code)
    expect_identical(app[[code]]$color, fam$color[i], info = code)
    expect_identical(app[[code]]$indicators,
                     unlist(fam$indicators[[i]], use.names = FALSE), info = code)
    expect_identical(app[[code]]$column_names,
                     unlist(fam$column_names[[i]], use.names = FALSE), info = code)
  }
})


test_that("A5 is part of family A", {
  a <- nemetonshiny:::INDICATOR_FAMILIES$A

  # Le symptôme d'origine : A5 était calculé puis filtré à l'affichage.
  expect_true("A5" %in% a$indicators)
  expect_true("indicateur_a5_rafraichissement" %in% a$column_names)
  # Et `indicators_data()` filtre sur ces deux vecteurs : sans l'un des deux,
  # la colonne n'atteint pas la vue famille.
  cfg <- get_family_config("A")
  expect_true("indicateur_a5_rafraichissement" %in%
                c(cfg$indicators, cfg$column_names))
})


test_that("every indicator carries a bilingual label", {
  app <- nemetonshiny:::INDICATOR_FAMILIES

  for (fam in app) {
    for (code in fam$indicators) {
      lbl <- fam$indicator_labels[[code]]
      expect_false(is.null(lbl), info = paste(fam$code, code))
      for (lang in c("fr", "en")) {
        expect_true(!is.null(lbl[[lang]]) && !is.na(lbl[[lang]]) &&
                      nzchar(lbl[[lang]]),
                    info = paste(fam$code, code, lang))
      }
    }
  }
})


test_that("the label describes the column, not its rank", {
  i18n <- get_i18n("fr")

  # LE bug corrigé. L'appariement code <-> colonne est positionnel et croisé
  # pour F : `F1` pointe sur `indicateur_f2_erosion`. Les clés i18n
  # `indicator_<code>` étant écrites selon la sémantique du CODE, la carte
  # d'érosion sortait « F1 - Fertilité des sols ».
  erosion <- nemetonshiny:::clean_indicator_label("indicateur_f2_erosion", i18n)
  expect_true(grepl("rosion", erosion, fixed = TRUE), info = erosion)
  expect_false(grepl("Fertilit", erosion, fixed = TRUE), info = erosion)

  fertilite <- nemetonshiny:::clean_indicator_label("indicateur_f1_fertilite", i18n)
  expect_true(grepl("Fertilit", fertilite, fixed = TRUE), info = fertilite)
  expect_false(grepl("rosion", fertilite, fixed = TRUE), info = fertilite)

  # A5 n'avait aucune clé i18n `indicator_A5` : sans lecture du cœur, il
  # retombait sur le nom de colonne humanisé.
  a5 <- nemetonshiny:::clean_indicator_label("indicateur_a5_rafraichissement", i18n)
  expect_true(grepl("^A5 - ", a5), info = a5)
})


test_that("labels follow the requested language", {
  fr <- nemetonshiny:::clean_indicator_label("indicateur_f2_erosion",
                                             get_i18n("fr"))
  en <- nemetonshiny:::clean_indicator_label("indicateur_f2_erosion",
                                             get_i18n("en"))
  expect_false(identical(fr, en))
  expect_true(grepl("Erosion", en, fixed = TRUE), info = en)
})


test_that("an unknown column degrades instead of erroring", {
  i18n <- get_i18n("fr")
  out <- nemetonshiny:::clean_indicator_label("indicateur_zz_inexistant", i18n)
  expect_true(nzchar(out))
  # Repli humanisé, jamais une erreur ni une chaîne vide.
  expect_false(grepl("_", out, fixed = TRUE))
})


test_that("the R5 status keys are translated in both languages", {
  keys <- c("r5_skipped_no_fordead", "r5_skipped_no_reconfort",
            "r5_skipped_no_method")
  for (lang in c("fr", "en")) {
    i18n <- get_i18n(lang)
    for (k in keys) {
      expect_true(i18n$has(k), info = paste(lang, k))
      expect_false(identical(i18n$t(k), k), info = paste(lang, k))
    }
  }
})


test_that("a skipped R5 says why, via the shared banner", {
  # Le mécanisme est celui d'A5 : `.r5_status` + clé `<code>_<status>`.
  d <- data.frame(x = 1:2, y = 1:2,
                  indicateur_r5_deperissement = c(NA_real_, NA_real_),
                  .r5_status = rep("skipped_no_fordead", 2),
                  check.names = FALSE)
  sf_d <- sf::st_as_sf(d, coords = c("x", "y"), crs = 4326)
  i18n <- get_i18n("fr")

  b <- nemetonshiny:::indicator_na_banner(
    sf_d, "indicateur_r5_deperissement", i18n)

  expect_false(is.null(b))
  expect_true(grepl(i18n$t("r5_skipped_no_fordead"), as.character(b),
                    fixed = TRUE))
})

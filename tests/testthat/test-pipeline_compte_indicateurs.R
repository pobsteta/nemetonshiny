# Mission 048 : le compte d'indicateurs de l'etape « Indicateurs » est lu sur
# le projet, jamais code en dur (il varie : indicateurs conditionnels du coeur).

.m048_df <- function(n) {
  cols <- unlist(nemeton::indicator_families()$column_names, use.names = FALSE)
  cols <- if (n <= length(cols)) cols[seq_len(n)] else
    c(cols, paste0("indicateur_extra_", seq_len(n - length(cols))))
  as.data.frame(stats::setNames(rep(list(c(0.1, 0.2)), n), cols))
}

.m048_label <- function(lang, data) {
  nemetonshiny:::pipeline_step_label(
    nemetonshiny:::get_i18n(lang), "pipeline_step_indicateurs",
    nemetonshiny:::.pipeline_compte_indicateurs(data))
}

test_that("36 indicateurs : le libelle dit 36 (fr et en)", {
  expect_equal(nemetonshiny:::.pipeline_compte_indicateurs(.m048_df(36))$n, 36L)
  expect_match(.m048_label("fr", .m048_df(36)), "36 indicateurs", fixed = TRUE)
  expect_match(.m048_label("en", .m048_df(36)), "36 indicators", fixed = TRUE)
})

test_that("le nombre suit les donnees : 31 et 44, jamais 31 en dur", {
  for (n in c(31L, 44L)) {
    expect_match(.m048_label("fr", .m048_df(n)),
                 sprintf("%d indicateurs", n), fixed = TRUE)
  }
  expect_false(grepl("31", .m048_label("fr", .m048_df(44)), fixed = TRUE))
})

test_that("les jumeaux _norm ne comptent pas ; un seul est au singulier", {
  df <- data.frame(indicateur_b1_protection = 1,
                   indicateur_b1_protection_norm = 1)
  compte <- nemetonshiny:::.pipeline_compte_indicateurs(df)
  expect_equal(compte$n, 1L)
  expect_equal(compte$familles, 1L)
  expect_match(.m048_label("fr", df), "1 indicateur, 1 famille)", fixed = TRUE)
  expect_match(.m048_label("en", df), "1 indicator, 1 family)", fixed = TRUE)
})

test_that("avant calcul : pas de nombre d'indicateurs invente, pas d'erreur", {
  for (d in list(NULL, data.frame(), data.frame(ug_id = 1, label = "a"))) {
    expect_true(is.na(nemetonshiny:::.pipeline_compte_indicateurs(d)$n))
    expect_match(.m048_label("fr", d), "^Indicateurs \\(\\d+ familles\\)$")
    expect_match(.m048_label("en", d), "^Indicators \\(\\d+ families\\)$")
  }
  expect_equal(nemetonshiny:::.pipeline_compte_projet(list())$familles,
               length(nemetonshiny:::INDICATOR_FAMILIES))
})

test_that("seule l'etape indicateurs porte un compte ; registre inchange", {
  i18n <- nemetonshiny:::get_i18n("fr")
  compte <- nemetonshiny:::.pipeline_compte_indicateurs(.m048_df(36))
  expect_equal(
    nemetonshiny:::pipeline_step_label(i18n, "pipeline_step_desserte", compte),
    i18n$t("pipeline_step_desserte"))
  expect_equal(nemetonshiny:::PIPELINE_STEPS[[1]]$id, "indicateurs")
  expect_equal(nemetonshiny:::PIPELINE_STEPS[[1]]$label,
               "pipeline_step_indicateurs")
})

test_that("l'aide du NDP ne fixe plus de nombre d'indicateurs ni de familles", {
  for (l in c("fr", "en")) {
    expect_false(grepl("31|12", nemetonshiny:::get_i18n(l)$t("ndp_tip_intro")))
  }
})

test_that("panneau et modale passent par le meme libelle", {
  f <- testthat::test_path("..", "..", "R", "mod_pipeline.R")
  skip_if_not(file.exists(f))
  src <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_false(grepl("i18n\\$t\\(s\\$label\\)|i18n\\$t\\(rep\\$label", src))
  expect_gte(lengths(regmatches(src, gregexpr("pipeline_step_label\\(", src))), 3L)
})

test_that("le compte suit la liste du calcul : une colonne d'un ancien calcul n'est pas comptee", {
  # 37 colonnes dans le projet (dont A5, calcule avant que sa source soit
  # desactivee), 36 indicateurs dans la liste du calcul : on affiche 36,
  # comme le panneau de progression (signale par Pascal le 2026-11-25).
  liste <- nemetonshiny:::list_available_indicators(NULL)
  sans_a5 <- setdiff(liste, "indicateur_a5_rafraichissement")
  cols <- unique(c(sans_a5, "indicateur_a5_rafraichissement"))
  df <- as.data.frame(stats::setNames(rep(list(c(0.1, 0.2)), length(cols)), cols))
  compte <- nemetonshiny:::.pipeline_compte_indicateurs(df, attendus = sans_a5)
  expect_equal(compte$n, length(sans_a5))
  # codes courts acceptes aussi
  df2 <- data.frame(A1 = 1, A5 = 2)
  expect_equal(nemetonshiny:::.pipeline_compte_indicateurs(
    df2, attendus = "indicateur_a1_couverture")$n, 1L)
  # sans filtre : comportement de la mission 048 inchange
  expect_equal(nemetonshiny:::.pipeline_compte_indicateurs(df)$n, length(cols))
})

test_that(".pipeline_compte_projet filtre par la liste du calcul du projet", {
  liste <- nemetonshiny:::list_available_indicators(NULL)
  cols <- unique(c(liste, "indicateur_extra_ancien"))
  df <- as.data.frame(stats::setNames(rep(list(c(0.1, 0.2)), length(cols)), cols))
  testthat::local_mocked_bindings(list_available_indicators = function(metadata = NULL) liste)
  expect_equal(nemetonshiny:::.pipeline_compte_projet(list(indicators = df))$n,
               length(liste))
})

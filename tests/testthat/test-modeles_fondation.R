# Modeles de fondation (nemeton 064 ; ecarts 20, 21, 22 du PLAN.md du coeur) :
# statuts N5, methode B4/L3, drapeau NDP `embedding_ml`.

.mk_sf_mf <- function(vals, status = NULL, col = "indicateur_n5_distance_reference") {
  d <- data.frame(x = seq_along(vals), y = seq_along(vals))
  d[[col]] <- vals
  if (!is.null(status)) d[[".n5_status"]] <- status
  sf::st_as_sf(d, coords = c("x", "y"), crs = 4326)
}

# ---- Ecart 20 : N5 --------------------------------------------------------

test_that("chaque statut N5 a une traduction FR et EN, et ressort dans le bandeau", {
  statuts <- c("skipped_no_embedding", "skipped_no_reference",
               "skipped_reference_too_small", "skipped_no_coverage")
  for (lang in c("fr", "en")) {
    i18n <- get_i18n(lang)
    for (st in statuts) {
      cle <- paste0("n5_", st)
      expect_true(isTRUE(i18n$has(cle)), info = cle)
      expect_lte(nchar(cle), 30L)
      b <- nemetonshiny:::indicator_na_banner(
        .mk_sf_mf(c(NA_real_, NA_real_), st),
        "indicateur_n5_distance_reference", i18n)
      expect_match(as.character(b), i18n$t(cle), fixed = TRUE)
    }
  }
})

test_that("un statut N5 inconnu retombe sur le message generique, jamais sur une cle brute", {
  i18n <- get_i18n("fr")
  b <- as.character(nemetonshiny:::indicator_na_banner(
    .mk_sf_mf(c(NA_real_, NA_real_), "skipped_inconnu"),
    "indicateur_n5_distance_reference", i18n))
  expect_match(b, i18n$t("indicator_all_na"), fixed = TRUE)
  expect_false(grepl("n5_skipped_inconnu", b, fixed = TRUE))
})

test_that("N5 calcule dit que la famille N gagne un indicateur", {
  i18n <- get_i18n("en")
  b <- nemetonshiny:::indicator_na_banner(
    .mk_sf_mf(c(0.2, 0.4), "calculated"), "indicateur_n5_distance_reference", i18n)
  expect_match(as.character(b), i18n$t("n5_calculated"), fixed = TRUE)
})

test_that("N5 est declare par le coeur dans la famille N : aucune table locale", {
  # La table de familles de l'app vient du coeur (`.build_indicator_families()`).
  cols <- unlist(lapply(INDICATOR_FAMILIES, function(f) f$column_names))
  expect_true("indicateur_n5_distance_reference" %in% cols)
  expect_true("N5" %in% INDICATOR_FAMILIES[["N"]]$indicators)
})

test_that("le statut N5 du coeur voyage jusqu'a la colonne `.n5_status`", {
  res <- data.frame(N5 = c(NA_real_, 0.3),
                    n5_status = c("skipped_no_coverage", "calculated"))
  vals <- nemetonshiny:::.capture_status_attr(c(NA_real_, 0.3), res)
  expect_identical(attr(vals, "nemeton_status_name"), "n5_status")
  expect_identical(attr(vals, "nemeton_status"),
                   c("skipped_no_coverage", "calculated"))
})

# ---- Ecart 21 : B4 / L3 ---------------------------------------------------

test_that("B4 en embedding est normalise avec le statut calculated_embedding", {
  df <- data.frame(ug_id = c("emb", "bio"),
                   indicateur_b4_div_spectrale = c(1.2, 1.2),
                   .b4_status = c("calculated_embedding", "calculated"),
                   check.names = FALSE)
  n <- nemetonshiny:::.add_normalized_indicators(df)$indicateur_b4_div_spectrale_norm
  expect_equal(n[1], nemeton::normalize_indicator(
    "indicateur_b4_div_spectrale", 1.2, statut = "calculated_embedding"))
  expect_equal(n[2], nemeton::normalize_indicator(
    "indicateur_b4_div_spectrale", 1.2, statut = "calculated"))
  # Meme valeur brute, deux bornes : sinon le statut ne servirait a rien.
  expect_false(isTRUE(all.equal(n[1], n[2])))
})

test_that("L3 en embedding est normalise avec le statut calculated_embedding", {
  df <- data.frame(ug_id = "emb", indicateur_l3_het_spectrale = 0.1,
                   .l3_status = "calculated_embedding", check.names = FALSE)
  n <- nemetonshiny:::.add_normalized_indicators(df)$indicateur_l3_het_spectrale_norm
  expect_equal(n, nemeton::normalize_indicator(
    "indicateur_l3_het_spectrale", 0.1, statut = "calculated_embedding"))
})

test_that("la methode B4 / L3 du coeur est transportee en colonne annexe", {
  res <- data.frame(B4 = c(1, 2), b4_status = "calculated_embedding",
                    b4_methode = "embedding")
  vals <- nemetonshiny:::.capture_annex_attr(c(1, 2), res,
                                             "indicateur_b4_div_spectrale")
  out <- nemetonshiny:::.write_production_annex(
    data.frame(ug_id = 1:2), "indicateur_b4_div_spectrale", vals, NULL)
  expect_identical(out$.b4_methode, c("embedding", "embedding"))

  res3 <- data.frame(L3 = c(1, 2), l3_methode = "biodivmapr")
  v3 <- nemetonshiny:::.capture_annex_attr(c(1, 2), res3,
                                           "indicateur_l3_het_spectrale")
  out3 <- nemetonshiny:::.write_production_annex(
    data.frame(ug_id = 1:2), "indicateur_l3_het_spectrale", v3, NULL)
  expect_identical(out3$.l3_methode, c("biodivmapr", "biodivmapr"))

  # Un recalcul sans methode ne garde pas celle du calcul precedent.
  out4 <- nemetonshiny:::.write_production_annex(
    out, "indicateur_b4_div_spectrale", NULL, NULL)
  expect_false(".b4_methode" %in% names(out4))
})

test_that("le bandeau nomme la methode ; NA ou absente : rien d'invente", {
  i18n <- get_i18n("fr")
  d <- data.frame(indicateur_b4_div_spectrale = c(1, 2),
                  .b4_methode = c("embedding", "embedding"), check.names = FALSE)
  b <- as.character(nemetonshiny:::methode_banner(
    d, "indicateur_b4_div_spectrale", i18n))
  expect_match(b, i18n$t("methode_embedding"), fixed = TRUE)
  expect_false(grepl(i18n$t("methode_non_comparable"), b, fixed = TRUE))

  # Deux methodes dans un projet : l'avertissement de non-comparaison.
  d$.b4_methode <- c("embedding", "biodivmapr")
  b2 <- as.character(nemetonshiny:::methode_banner(
    d, "indicateur_b4_div_spectrale_norm", i18n))
  expect_match(b2, i18n$t("methode_biodivmapr"), fixed = TRUE)
  expect_match(b2, i18n$t("methode_non_comparable"), fixed = TRUE)

  d$.b4_methode <- NA_character_
  expect_null(nemetonshiny:::methode_banner(d, "indicateur_b4_div_spectrale", i18n))
  expect_null(nemetonshiny:::methode_banner(
    d[, "indicateur_b4_div_spectrale", drop = FALSE],
    "indicateur_b4_div_spectrale", i18n))
  # Autre indicateur : jamais de bandeau de methode.
  expect_null(nemetonshiny:::methode_banner(d, "indicateur_c1_biomasse", i18n))
})

test_that("la fiche de l'UGF affiche la methode a cote de la valeur", {
  i18n <- get_i18n("fr")
  full <- data.frame(ug_id = c("u1", "u2"),
                     .b4_methode = c("embedding", NA), check.names = FALSE)
  expect_identical(
    nemetonshiny:::.methode_pour_unite(full, data.frame(ug_id = "u1"),
                                       "indicateur_b4_div_spectrale", i18n),
    i18n$t("methode_embedding"))
  # Sans methode : rien.
  expect_identical(
    nemetonshiny:::.methode_pour_unite(full, data.frame(ug_id = "u2"),
                                       "indicateur_b4_div_spectrale", i18n), "")
  # Un autre indicateur n'a pas de methode.
  expect_identical(
    nemetonshiny:::.methode_pour_unite(full, data.frame(ug_id = "u1"),
                                       "indicateur_c1_biomasse", i18n), "")
  # Un code inconnu ressort tel quel.
  full$.b4_methode <- "autre"
  expect_identical(
    nemetonshiny:::.methode_pour_unite(full, data.frame(ug_id = "u1"),
                                       "indicateur_b4_div_spectrale", i18n), "autre")
})

test_that("la comparaison entre projets ne tombe que pour embedding des deux cotes", {
  expect_true(nemetonshiny:::methodes_comparables("embedding", "embedding"))
  expect_false(nemetonshiny:::methodes_comparables("biodivmapr", "biodivmapr"))
  expect_false(nemetonshiny:::methodes_comparables("embedding", "biodivmapr"))
  expect_false(nemetonshiny:::methodes_comparables("embedding", NA_character_))
  expect_false(nemetonshiny:::methodes_comparables(NA_character_, NA_character_))
})

# ---- Ecart 22 : NDP -------------------------------------------------------

test_that("embedding_ml leve le badge ; sans source, rien ne change", {
  proj <- list(metadata = list(embedding_source = "aef"))
  expect_true("embedding_ml" %in% nemetonshiny:::project_augmented_flags(proj))
  expect_true("embedding_ml" %in% nemetonshiny:::project_augmented_flags(
    list(metadata = list(embedding_source = "maestro"))))
  for (m in list(list(), list(embedding_source = "none"),
                 list(embedding_source = ""), list(embedding_source = NA_character_))) {
    expect_identical(nemetonshiny:::project_augmented_flags(list(metadata = m)),
                     character(0))
  }
  expect_identical(nemetonshiny:::project_augmented_flags(NULL), character(0))

  for (lang in c("fr", "en")) {
    i18n <- get_i18n(lang)
    html <- as.character(nemetonshiny:::ndp_embedding_badge(i18n))
    expect_match(html, i18n$t("augmented_embedding_ml_badge"), fixed = TRUE)
    expect_match(html, i18n$t("augmented_embedding_ml_tooltip"), fixed = TRUE)
  }
})

test_that("les nouvelles cles i18n tiennent en 30 caracteres", {
  cles <- c("methode_calcul_fmt", "methode_biodivmapr", "methode_embedding",
            "methode_non_comparable", "augmented_embedding_ml_badge",
            "augmented_embedding_ml_tooltip", "n5_calculated")
  expect_true(all(nchar(cles) <= 30L))
  expect_true(all(cles %in% names(TRANSLATIONS)))
})

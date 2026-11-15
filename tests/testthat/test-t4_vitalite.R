# T4 vitalite dynamique (spec 068, nemeton >= 2026.11.28) : essence en code
# IFN ESPAR, SER, coupes lues dans T3, statuts traduits, colonnes annexes
# conservees, affichage observe / attendu. Aucun chiffre n'est calcule ici.

.t4_unites <- function(species = c("Chêne sessile", "Hêtre", "Sapin, épicéa")) {
  n <- length(species)
  g <- lapply(seq_len(n), function(i) {
    x0 <- 900000 + (i - 1) * 500
    sf::st_polygon(list(rbind(c(x0, 6800000), c(x0 + 200, 6800000),
                              c(x0 + 200, 6800200), c(x0, 6800200),
                              c(x0, 6800000))))
  })
  sf::st_sf(ug_id = as.character(seq_len(n)), species = species,
            geometry = sf::st_sfc(g, crs = 2154))
}

# ---- Essence en code ESPAR ---------------------------------------------------

test_that("T4 reçoit l'essence dominante en code IFN ESPAR", {
  u <- nemetonshiny:::.units_for_indicator(
    "indicateur_t4_vitalite", .t4_unites(), results = data.frame(),
    cfg = NULL)
  # 03 chêne sessile, 09 hêtre (codes du référentiel IFN, via le cœur).
  expect_equal(u$essence[1:2], c("03", "09"))
})

test_that("une essence sans correspondance donne NA, jamais un code deviné", {
  u <- nemetonshiny:::.units_for_indicator(
    "indicateur_t4_vitalite", .t4_unites(), results = data.frame(),
    cfg = NULL)
  # « Sapin, épicéa » nomme deux espèces (61 et 62) : T4 dira skipped_no_essence.
  expect_true(is.na(u$essence[3]))
})

test_that("sans colonne species, essence vaut NA partout (skipped_no_essence)", {
  p <- .t4_unites()
  p$species <- NULL
  u <- nemetonshiny:::.t4_ajouter_essence(p)
  expect_true(all(is.na(u$essence)))
  expect_type(u$essence, "character")
})

test_that("une résolution en échec donne NA et ne casse pas le calcul", {
  testthat::local_mocked_bindings(
    resoudre_espar = function(x) stop("referentiel indisponible"),
    .package = "nemeton")
  u <- nemetonshiny:::.t4_ajouter_essence(.t4_unites())
  expect_true(all(is.na(u$essence)))
})

test_that("une colonne essence fournie par l'utilisateur est conservée", {
  p <- .t4_unites()
  p$essence <- c("62", "62", "62")
  u <- nemetonshiny:::.t4_ajouter_essence(p)
  expect_equal(u$essence, c("62", "62", "62"))
})

test_that("les unités d'un autre indicateur ne reçoivent pas d'essence", {
  u <- nemetonshiny:::.units_for_indicator(
    "indicateur_c1_biomasse", .t4_unites(), results = data.frame(), cfg = NULL)
  expect_false("essence" %in% names(u))
})

# ---- Coupes : T3 brut, lu dans les résultats déjà calculés --------------------

test_that("T4 lit la colonne T3 brute des résultats pour écarter les coupes", {
  res <- data.frame(indicateur_t3_coupes_rases = c(0, 12.5, NA))
  u <- nemetonshiny:::.units_for_indicator(
    "indicateur_t4_vitalite", .t4_unites(), results = res, cfg = NULL)
  expect_equal(u$T3, c(0, 12.5, NA))
  # Le cœur lit `units$T3 > 0` : seule la 2e unité est une coupe.
  expect_equal(u$T3 > 0, c(FALSE, TRUE, NA))
})

test_that("sans T3 calculé, aucune colonne T3 n'est passée", {
  u <- nemetonshiny:::.units_for_indicator(
    "indicateur_t4_vitalite", .t4_unites(), results = data.frame(), cfg = NULL)
  expect_false("T3" %in% names(u))
})

test_that("T4 est calculé après T3", {
  inds <- nemetonshiny:::list_available_indicators()
  expect_true("indicateur_t4_vitalite" %in% inds)
  expect_gt(match("indicateur_t4_vitalite", inds),
            match("indicateur_t3_coupes_rases", inds))
})

# ---- SER : la colonne `ser` des unités est celle que le cœur lit -------------

test_that("la colonne ser des unités traverse sans changement jusqu'au cœur", {
  p <- .t4_unites()
  p$ser <- c("C11", "C11", "D12")
  u <- nemetonshiny:::.units_for_indicator(
    "indicateur_t4_vitalite", p, results = data.frame(), cfg = NULL)
  expect_equal(u$ser, c("C11", "C11", "D12"))
})

test_that("compute_single_indicator passe forms_t, essence et ser au cœur", {
  captured <- new.env()
  assign("indicateur_test_t4",
         function(units, forms_t = NULL, essence = NULL, ser = NULL) {
           captured$forms_t <- forms_t
           captured$colonnes <- names(units)
           rep(50, nrow(units))
         },
         envir = globalenv())
  withr::defer(rm("indicateur_test_t4", envir = globalenv()))

  p <- .t4_unites()
  p$ser <- "C11"
  u <- nemetonshiny:::.units_for_indicator(
    "indicateur_t4_vitalite", p, results = data.frame(), cfg = NULL)
  serie <- terra::rast(nrows = 2, ncols = 2, nlyrs = 3, vals = 1)
  layers <- structure(
    list(rasters = list(forms_t = serie), vectors = list()),
    class = "nemeton_layers")
  out <- nemetonshiny:::compute_single_indicator("indicateur_test_t4", u, layers)
  expect_equal(out, rep(50, 3))
  expect_s4_class(captured$forms_t, "SpatRaster")
  expect_true(all(c("essence", "ser") %in% captured$colonnes))
})

# ---- Statuts traduits ---------------------------------------------------------

test_that("chaque statut skipped_* de T4 a une traduction FR et EN", {
  for (st in c("no_forms_t", "no_coverage", "cut", "drop", "no_essence",
               "no_analogues")) {
    key <- paste0("t4_skipped_", st)
    for (lang in c("fr", "en")) {
      i18n <- nemetonshiny:::get_i18n(lang)
      expect_true(isTRUE(i18n$has(key)), info = paste(key, lang))
      expect_false(identical(i18n$t(key), key), info = paste(key, lang))
    }
    expect_lte(nchar(key), 30)
  }
})

test_that("indicator_na_banner traduit le statut T4 de la colonne .t4_status", {
  i18n <- nemetonshiny:::get_i18n("fr")
  d <- data.frame(indicateur_t4_vitalite = c(NA_real_, NA_real_),
                  .t4_status = c("skipped_cut", "skipped_cut"))
  html <- as.character(nemetonshiny:::indicator_na_banner(
    d, "indicateur_t4_vitalite", i18n))
  expect_match(html, i18n$t("t4_skipped_cut"), fixed = TRUE)

  for (st in c("no_forms_t", "no_coverage", "drop", "no_essence",
               "no_analogues")) {
    d$.t4_status <- paste0("skipped_", st)
    html <- as.character(nemetonshiny:::indicator_na_banner(
      d, "indicateur_t4_vitalite", i18n))
    expect_match(html, i18n$t(paste0("t4_skipped_", st)), fixed = TRUE)
  }

  # Statut nominal sur des valeurs : rien à nuancer.
  d <- data.frame(indicateur_t4_vitalite = c(55, 60),
                  .t4_status = "calculated")
  expect_null(nemetonshiny:::indicator_na_banner(
    d, "indicateur_t4_vitalite", i18n))
})

test_that("le statut T4 est porté par la colonne .t4_status", {
  expect_equal(nemetonshiny:::.indicator_status_col("indicateur_t4_vitalite"),
               ".t4_status")
  result <- data.frame(T4 = c(50, NA),
                       t4_status = c("calculated", "skipped_cut"))
  v <- nemetonshiny:::.capture_status_attr(c(50, NA), result)
  expect_equal(attr(v, "nemeton_status_name"), "t4_status")
  expect_equal(attr(v, "nemeton_status"), c("calculated", "skipped_cut"))
})

# ---- Colonnes annexes : conservées jusqu'au parquet --------------------------

.t4_resultat_coeur <- function() {
  data.frame(T4 = c(57, NA),
             t4_status = c("calculated", "skipped_no_analogues"),
             t4_accroissement_obs = c(1.4, 3.1),
             t4_accroissement_att = c(1.3, NA),
             t4_h_dom = c(24.5, 36.2),
             t4_n_analogues = c(120L, NA))
}

test_that("les colonnes annexes de T4 survivent à l'écriture et à la lecture", {
  skip_if_not_installed("arrow")
  res <- .t4_resultat_coeur()
  vals <- .t4_resultat_coeur()$T4
  vals <- nemetonshiny:::.capture_annex_attr(vals, res, "indicateur_t4_vitalite")
  annexe <- attr(vals, "nemeton_annex")
  expect_setequal(names(annexe),
                  c(".t4_accroissement_obs", ".t4_accroissement_att",
                    ".t4_h_dom", ".t4_n_analogues"))

  results <- data.frame(ug_id = c("1", "2"))
  results[["indicateur_t4_vitalite"]] <- as.numeric(vals)
  results <- nemetonshiny:::.write_production_annex(
    results, "indicateur_t4_vitalite", vals, NULL)
  expect_equal(results$.t4_accroissement_obs, c(1.4, 3.1))

  f <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(results, f)
  relu <- as.data.frame(arrow::read_parquet(f))
  expect_equal(relu$.t4_accroissement_att, c(1.3, NA))
  expect_equal(relu$.t4_h_dom, c(24.5, 36.2))
  expect_equal(relu$.t4_n_analogues, c(120L, NA))

  # Préfixées d'un point : jamais prises pour des indicateurs.
  expect_equal(nemetonshiny:::get_indicator_cols(relu),
               "indicateur_t4_vitalite")
})

test_that("un recalcul de T4 remplace les annexes de l'ancien calcul", {
  results <- data.frame(ug_id = "1", indicateur_t4_vitalite = 50,
                        .t4_accroissement_obs = 9, .t4_h_dom = 9)
  # Échec : les annexes de l'ancien calcul disparaissent avec la valeur.
  out <- nemetonshiny:::.write_production_annex(
    results, "indicateur_t4_vitalite", NULL, NULL)
  expect_false(any(c(".t4_accroissement_obs", ".t4_h_dom") %in% names(out)))
})

# ---- Affichage : observé / attendu + mention de la référence ------------------

test_that("t4_vitalite_banner montre observé, attendu et la référence fixe", {
  i18n <- nemetonshiny:::get_i18n("fr")
  d <- data.frame(indicateur_t4_vitalite = c(57, 40),
                  .t4_accroissement_obs = c(1.4, 0.9),
                  .t4_accroissement_att = c(1.3, 1.3),
                  .t4_h_dom = c(24.5, 24.5),
                  .t4_n_analogues = c(120L, 120L))
  html <- as.character(nemetonshiny:::t4_vitalite_banner(
    d, "indicateur_t4_vitalite_norm", i18n))
  expect_match(html, "0,9 à 1,4", fixed = TRUE)   # observé : plage des UGF
  expect_match(html, "1,3", fixed = TRUE)         # attendu : valeur commune
  expect_match(html, "24,5", fixed = TRUE)
  expect_match(html, "120", fixed = TRUE)
  expect_match(html, "Bourgogne-Franche-Comt", fixed = TRUE)
  expect_match(html, "2019-2023", fixed = TRUE)

  en <- as.character(nemetonshiny:::t4_vitalite_banner(
    d, "indicateur_t4_vitalite", nemetonshiny:::get_i18n("en")))
  expect_match(en, "0.9 to 1.4", fixed = TRUE)
  expect_match(en, "2019-2023", fixed = TRUE)
})

test_that("t4_vitalite_banner se tait hors T4 ou sans valeur calculée", {
  i18n <- nemetonshiny:::get_i18n("fr")
  d <- data.frame(indicateur_t4_vitalite = c(NA_real_, NA_real_),
                  .t4_accroissement_obs = c(NA_real_, 1),
                  .t4_accroissement_att = NA_real_)
  expect_null(nemetonshiny:::t4_vitalite_banner(
    d, "indicateur_t4_vitalite", i18n))
  expect_null(nemetonshiny:::t4_vitalite_banner(
    d, "indicateur_t3_coupes_rases", i18n))
  expect_null(nemetonshiny:::t4_vitalite_banner(NULL, "indicateur_t4_vitalite", i18n))
})

# ---- Cœur réel (>= 2026.11.28) ------------------------------------------------

test_that("le cœur réel rend T4 = NA et skipped_no_forms_t sans série", {
  skip_if(utils::packageVersion("nemeton") < "2026.11.28",
          "nemeton >= 2026.11.28 requis pour T4")
  p <- .t4_unites()
  layers <- structure(list(rasters = list(), vectors = list()),
                      class = "nemeton_layers")
  out <- suppressMessages(nemetonshiny:::compute_single_indicator(
    "indicateur_t4_vitalite", p, layers))
  expect_true(all(is.na(out)))
  expect_equal(attr(out, "nemeton_status"), rep("skipped_no_forms_t", 3))
  expect_equal(attr(out, "nemeton_status_name"), "t4_status")
  expect_true(".t4_h_dom" %in% names(attr(out, "nemeton_annex")))
})

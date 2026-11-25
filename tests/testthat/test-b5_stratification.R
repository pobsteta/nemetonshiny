# B5 stratification verticale (spec 067, nemeton >= 2026.11.5) : nuage passé au
# calcul, statuts traduits, couche sur la carte, profil vertical dans la fiche
# de l'UGF. Aucun chiffre n'est calculé ici : seul le cœur mesure.

.b5_unites <- function(n = 2) {
  g <- lapply(seq_len(n), function(i) {
    x0 <- 900000 + (i - 1) * 500
    sf::st_polygon(list(rbind(c(x0, 6800000), c(x0 + 200, 6800000),
                              c(x0 + 200, 6800200), c(x0, 6800200),
                              c(x0, 6800000))))
  })
  sf::st_sf(ug_id = as.character(seq_len(n)),
            geometry = sf::st_sfc(g, crs = 2154))
}

.b5_projet_avec_nuage <- function(n_dalles = 1, taille = 4096) {
  proj <- withr::local_tempdir(.local_envir = parent.frame())
  d <- file.path(proj, "cache", "layers", "lidar_nuage")
  dir.create(d, recursive = TRUE)
  for (i in seq_len(n_dalles)) {
    writeBin(raw(taille), file.path(d, sprintf("dalle%d.copc.laz", i)))
  }
  proj
}

# ---- Cas 1 : sans nuage --------------------------------------------------------

test_that("sans dossier lidar_nuage, aucun dossier n'est retenu", {
  proj <- withr::local_tempdir()
  expect_null(nemetonshiny:::.b5_dossier_nuage(proj))
  expect_null(nemetonshiny:::.b5_dossier_nuage(NULL))
  expect_null(nemetonshiny:::.b5_dossier_nuage(""))
})

test_that("un dossier vide ou des dalles de quelques octets ne sont pas un nuage", {
  proj <- withr::local_tempdir()
  d <- file.path(proj, "cache", "layers", "lidar_nuage")
  dir.create(d, recursive = TRUE)
  expect_null(nemetonshiny:::.b5_dossier_nuage(proj))
  writeBin(raw(10), file.path(d, "echec.copc.laz"))
  file.create(file.path(d, "notes.txt"))
  expect_null(nemetonshiny:::.b5_dossier_nuage(proj))
})

test_that("sans nuage, B5 est NA avec skipped_no_lidar et aucune erreur", {
  skip_if_not("indicateur_b5_stratification" %in% getNamespaceExports("nemeton"),
              "nemeton >= 2026.11.5 requis")
  layers <- structure(list(rasters = list(), vectors = list()),
                      class = "nemeton_layers")
  vals <- nemetonshiny:::compute_single_indicator(
    "indicateur_b5_stratification", .b5_unites(), layers)
  expect_true(all(is.na(vals)))
  expect_equal(attr(vals, "nemeton_status"), rep("skipped_no_lidar", 2))
  expect_equal(attr(vals, "nemeton_status_name"), "b5_status")
})

test_that("B5 est dans la liste des indicateurs calculables", {
  expect_true("indicateur_b5_stratification" %in%
                nemetonshiny:::list_available_indicators())
})

# ---- Cas 2 : avec nuage ---------------------------------------------------------

test_that("le dossier du nuage et normaliser = TRUE arrivent au cœur", {
  proj <- .b5_projet_avec_nuage(2)
  dossier <- nemetonshiny:::.b5_dossier_nuage(proj)
  expect_equal(basename(dossier), "lidar_nuage")

  vu <- new.env()
  assign("indicateur_test_b5",
         function(units, nuage = NULL, normaliser = FALSE) {
           vu$nuage <- nuage
           vu$normaliser <- normaliser
           rep(50, nrow(units))
         }, envir = globalenv())
  withr::defer(rm("indicateur_test_b5", envir = globalenv()))

  layers <- structure(list(rasters = list(), vectors = list(),
                           lidar_nuage_dir = dossier),
                      class = "nemeton_layers")
  out <- nemetonshiny:::compute_single_indicator(
    "indicateur_test_b5", .b5_unites(), layers)
  expect_equal(out, rep(50, 2))
  expect_equal(vu$nuage, dossier)
  expect_true(isTRUE(vu$normaliser))
})

test_that("sans dossier dans layers, nuage et normaliser ne sont pas passés", {
  vu <- new.env()
  assign("indicateur_test_b5b",
         function(units, nuage = NULL, normaliser = FALSE) {
           vu$nuage <- nuage
           vu$normaliser <- normaliser
           rep(1, nrow(units))
         }, envir = globalenv())
  withr::defer(rm("indicateur_test_b5b", envir = globalenv()))
  layers <- structure(list(rasters = list(), vectors = list()),
                      class = "nemeton_layers")
  nemetonshiny:::compute_single_indicator(
    "indicateur_test_b5b", .b5_unites(), layers)
  expect_null(vu$nuage)
  expect_false(vu$normaliser)
})

test_that("le cœur lit un vrai nuage et le profil traverse jusqu'aux colonnes .b5_*", {
  skip_if_not("indicateur_b5_stratification" %in% getNamespaceExports("nemeton"),
              "nemeton >= 2026.11.5 requis")
  u <- .b5_unites()
  set.seed(1)
  n <- 8000
  nuage <- data.frame(X = runif(n, 900000, 900200), Y = runif(n, 6800000, 6800200),
                      Z = runif(n, 0.6, 25), Classification = 1L)
  layers <- structure(list(rasters = list(), vectors = list()),
                      class = "nemeton_layers")
  # Appel via le cœur avec un data.frame (la lecture de dalles .laz demande
  # lidR et de vraies dalles : non prouvée ici, voir le rendu).
  r <- nemeton::indicateur_b5_stratification(u, nuage)
  vals <- nemeton::extract_indicator_value(r, "indicateur_b5_stratification",
                                           exclude = names(u))
  vals <- nemetonshiny:::.capture_status_attr(vals, r)
  vals <- nemetonshiny:::.capture_annex_attr(vals, r,
                                             "indicateur_b5_stratification")
  expect_equal(attr(vals, "nemeton_status"), c("calculated", "skipped_no_coverage"))
  annexe <- attr(vals, "nemeton_annex")
  expect_setequal(names(annexe),
                  c(".b5_densite", ".b5_points", ".b5_profil",
                    paste0(".b5_s", 1:5)))
  parts <- vapply(paste0(".b5_s", 1:5), function(k) annexe[[k]][1], numeric(1))
  expect_equal(sum(parts), 1, tolerance = 1e-8)
})

# ---- Cas 3 : statuts traduits ---------------------------------------------------

test_that("les quatre statuts de B5 ont une traduction FR et EN, clés <= 30 car.", {
  cles <- c("b5_skipped_no_lidar", "b5_skipped_no_coverage",
            "b5_skipped_low_density", "b5_statut_calculated")
  for (key in cles) {
    expect_lte(nchar(key), 30)
    for (lang in c("fr", "en")) {
      i18n <- nemetonshiny:::get_i18n(lang)
      expect_true(isTRUE(i18n$has(key)), info = paste(key, lang))
      expect_false(identical(i18n$t(key), key), info = paste(key, lang))
    }
  }
})

test_that("le bandeau d'un B5 vide dit la cause traduite ; calculé : rien", {
  i18n <- nemetonshiny:::get_i18n("fr")
  for (st in c("no_lidar", "no_coverage", "low_density")) {
    d <- data.frame(indicateur_b5_stratification = c(NA_real_, NA_real_),
                    .b5_status = paste0("skipped_", st))
    html <- as.character(nemetonshiny:::indicator_na_banner(
      d, "indicateur_b5_stratification", i18n))
    expect_match(html, i18n$t(paste0("b5_skipped_", st)), fixed = TRUE)
  }
  d <- data.frame(indicateur_b5_stratification = c(60, 70),
                  .b5_status = "calculated")
  expect_null(nemetonshiny:::indicator_na_banner(
    d, "indicateur_b5_stratification", i18n))
})

# ---- Cas 4 : fiche de l'UGF -----------------------------------------------------

.b5_ligne <- function(densite = 22.2, statut = "calculated",
                      parts = c(0.1, 0.2, 0.3, 0.25, 0.15)) {
  d <- data.frame(.b5_status = statut, .b5_densite = densite,
                  check.names = FALSE)
  for (k in 1:5) d[[paste0(".b5_s", k)]] <- parts[k]
  d
}

test_that("la fiche montre cinq barres (somme 1) et la densité", {
  i18n <- nemetonshiny:::get_i18n("fr")
  html <- as.character(nemetonshiny:::b5_profil_tag(.b5_ligne(), i18n))
  expect_equal(lengths(regmatches(html, gregexpr("width:\\d+%; height:10px",
                                                 html))), 5L)
  pct <- as.integer(regmatches(html, gregexpr("(?<=width:)\\d+(?=%; height:10px)",
                                              html, perl = TRUE))[[1]])
  expect_equal(sum(pct), 100L)
  expect_match(html, "22,2 retours/m", fixed = TRUE)
  expect_match(html, i18n$t("b5_statut_calculated"), fixed = TRUE)
  # Densité suffisante : pas d'avertissement.
  expect_false(grepl(i18n$t("b5_densite_faible"), html, fixed = TRUE))
})

test_that("sous 10 retours/m2, l'avertissement s'affiche (FR et EN)", {
  for (lang in c("fr", "en")) {
    i18n <- nemetonshiny:::get_i18n(lang)
    html <- as.character(nemetonshiny:::b5_profil_tag(
      .b5_ligne(densite = 6.5), i18n))
    expect_match(html, i18n$t("b5_densite_faible"), fixed = TRUE)
  }
  # Pile à 10 : pas d'avertissement.
  i18n <- nemetonshiny:::get_i18n("fr")
  html <- as.character(nemetonshiny:::b5_profil_tag(.b5_ligne(densite = 10), i18n))
  expect_false(grepl(i18n$t("b5_densite_faible"), html, fixed = TRUE))
})

test_that("un statut autre que calculated remplace l'histogramme", {
  i18n <- nemetonshiny:::get_i18n("fr")
  for (st in c("skipped_no_lidar", "skipped_no_coverage", "skipped_low_density")) {
    html <- as.character(nemetonshiny:::b5_profil_tag(
      .b5_ligne(statut = st, densite = NA, parts = rep(NA_real_, 5)), i18n))
    expect_match(html, i18n$t(paste0("b5_", st)), fixed = TRUE)
    expect_false(grepl("height:10px", html, fixed = TRUE))
  }
})

test_that("sans colonne .b5_status, la fiche n'ajoute rien", {
  i18n <- nemetonshiny:::get_i18n("fr")
  expect_null(nemetonshiny:::b5_profil_tag(data.frame(x = 1), i18n))
  expect_null(nemetonshiny:::b5_profil_tag(NULL, i18n))
})

# ---- Cas 5 : carte --------------------------------------------------------------

.b5_famille_b <- function(avec_b5) {
  u <- .b5_unites(3)
  u$indicateur_b1_protection_norm <- c(0.2, 0.5, 0.8)
  if (avec_b5) {
    u$indicateur_b5_stratification <- c(60, 72, 55)
    u$indicateur_b5_stratification_norm <- c(0.6, 0.72, 0.55)
    u$.b5_status <- "calculated"
  } else {
    u$indicateur_b5_stratification <- NA_real_
    u$.b5_status <- "skipped_no_lidar"
  }
  u
}

.b5_cartes_famille <- function(sf_projet) {
  etat <- shiny::reactiveValues(
    current_project = list(indicators = sf::st_drop_geometry(sf_projet),
                           indicators_sf = sf_projet),
    language = "fr", family_comments = list())
  cols <- NULL
  shiny::testServer(
    nemetonshiny:::mod_family_server,
    args = list(family_code = "B", app_state = etat),
    { cols <<- nemetonshiny:::get_indicator_cols(indicators_sf()) })
  cols
}

test_that("la couche B5 existe quand la colonne existe, et seulement alors", {
  avec <- .b5_cartes_famille(.b5_famille_b(TRUE))
  expect_true(any(grepl("b5_stratification", avec)))
  sans <- .b5_cartes_famille(.b5_famille_b(FALSE))
  expect_false(any(grepl("b5_stratification", sans)))
  expect_true(any(grepl("b1_protection", sans)))
})

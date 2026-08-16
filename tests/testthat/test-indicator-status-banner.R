# Tests for the "why is this indicator empty" banner (spec 032, brief A5)
# R/mod_family.R - .indicator_status_col() / indicator_na_banner()
# R/service_r5.R - the status column must survive to the UI

.mk_sf <- function(vals, status = NULL, col = "indicateur_a5_rafraichissement") {
  d <- data.frame(x = seq_along(vals), y = seq_along(vals))
  d[[col]] <- vals
  if (!is.null(status)) d[[".a5_status"]] <- status
  sf::st_as_sf(d, coords = c("x", "y"), crs = 4326)
}


# ==============================================================================
# Mapping indicator -> status column
# ==============================================================================

test_that(".indicator_status_col follows the core naming convention", {
  expect_equal(
    nemetonshiny:::.indicator_status_col("indicateur_a5_rafraichissement"),
    ".a5_status")
  expect_equal(
    nemetonshiny:::.indicator_status_col("indicateur_r5_deperissement"),
    ".r5_status")
  # Les colonnes normalisees portent le meme code.
  expect_equal(
    nemetonshiny:::.indicator_status_col("indicateur_c1_biomasse_norm"),
    ".c1_status")
  # Hors convention : pas de colonne de statut, pas de devinette.
  expect_null(nemetonshiny:::.indicator_status_col("famille_carbone"))
  expect_null(nemetonshiny:::.indicator_status_col("surface_m2"))
})


# ==============================================================================
# The banner itself
# ==============================================================================

test_that("no banner when the indicator has at least one value", {
  i18n <- get_i18n("fr")
  # Un indicateur partiellement rempli se lit sur la carte : rien a expliquer.
  expect_null(nemetonshiny:::indicator_na_banner(
    .mk_sf(c(10, NA, 30)), "indicateur_a5_rafraichissement", i18n))
})


test_that("a fully NA indicator without status gets the generic message", {
  i18n <- get_i18n("fr")
  b <- nemetonshiny:::indicator_na_banner(
    .mk_sf(c(NA_real_, NA_real_)), "indicateur_a5_rafraichissement", i18n)

  expect_false(is.null(b))
  expect_true(grepl(i18n$t("indicator_all_na"), as.character(b), fixed = TRUE))
})


test_that("a named cause is preferred over the generic message", {
  i18n <- get_i18n("fr")
  b <- nemetonshiny:::indicator_na_banner(
    .mk_sf(c(NA_real_, NA_real_), status = rep("skipped_no_lst", 2)),
    "indicateur_a5_rafraichissement", i18n)

  html <- as.character(b)
  expect_true(grepl(i18n$t("a5_skipped_no_lst"), html, fixed = TRUE))
  # Le message generique ne doit PAS apparaitre : sinon la cause nommee n'a
  # servi a rien.
  expect_false(grepl(i18n$t("indicator_all_na"), html, fixed = TRUE))
})


test_that("an untranslated cause falls back rather than showing a raw key", {
  i18n <- get_i18n("fr")
  b <- nemetonshiny:::indicator_na_banner(
    .mk_sf(c(NA_real_, NA_real_), status = rep("skipped_no_such_thing", 2)),
    "indicateur_a5_rafraichissement", i18n)

  html <- as.character(b)
  expect_true(grepl(i18n$t("indicator_all_na"), html, fixed = TRUE))
  expect_false(grepl("skipped_no_such_thing", html, fixed = TRUE))
})


test_that("an all-NA status column does not break the banner", {
  i18n <- get_i18n("fr")
  b <- nemetonshiny:::indicator_na_banner(
    .mk_sf(c(NA_real_, NA_real_), status = c(NA_character_, NA_character_)),
    "indicateur_a5_rafraichissement", i18n)

  expect_true(grepl(i18n$t("indicator_all_na"), as.character(b), fixed = TRUE))
})


test_that("the banner is language-aware", {
  fr <- get_i18n("fr")
  en <- get_i18n("en")
  d <- .mk_sf(c(NA_real_, NA_real_), status = rep("skipped_no_lst", 2))

  html_fr <- as.character(nemetonshiny:::indicator_na_banner(
    d, "indicateur_a5_rafraichissement", fr))
  html_en <- as.character(nemetonshiny:::indicator_na_banner(
    d, "indicateur_a5_rafraichissement", en))

  expect_false(identical(html_fr, html_en))
  expect_true(grepl("Thermocity", html_en, fixed = TRUE))
})


# ==============================================================================
# The status column must not be discarded (§4.1)
# ==============================================================================

test_that(".r5_finalize keeps r5_status instead of discarding it", {
  # Ce que rend le coeur : R5 + r5_status + les colonnes de routage de l'app.
  out <- sf::st_as_sf(
    data.frame(ug_id = c("a", "b"), x = 1:2, y = 1:2,
               R5 = c(40, 60),
               r5_status = c("calculated", "skipped_no_fordead"),
               .r5_resineux = c(1, 0), .r5_feuillus = c(0, 1),
               check.names = FALSE),
    coords = c("x", "y"), crs = 4326)

  res <- nemetonshiny:::.r5_finalize(out)

  expect_equal(res$indicateur_r5_deperissement, c(40, 60))
  expect_null(res$R5)
  # Le geste corrige : la cause survit, prefixee.
  expect_equal(res$.r5_status, c("calculated", "skipped_no_fordead"))
  expect_false("r5_status" %in% names(res))
  # Les colonnes de routage, elles, disparaissent bien.
  expect_false(".r5_resineux" %in% names(res))
  expect_false(".r5_feuillus" %in% names(res))
  # Et le statut ne doit jamais etre pris pour un indicateur.
  expect_false(".r5_status" %in% get_indicator_cols(res))
  expect_true("indicateur_r5_deperissement" %in% get_indicator_cols(res))
})


test_that(".r5_finalize tolerates a core that returns no status", {
  out <- sf::st_as_sf(
    data.frame(ug_id = "a", x = 1, y = 1, R5 = 50),
    coords = c("x", "y"), crs = 4326)

  res <- nemetonshiny:::.r5_finalize(out)
  expect_equal(res$indicateur_r5_deperissement, 50)
  expect_false(".r5_status" %in% names(res))
})


test_that("a status column never reaches the indicator column list", {
  d <- .mk_sf(c(1, 2), status = c("skipped_no_lst", "skipped_no_lst"))
  cols <- get_indicator_cols(d)

  expect_true("indicateur_a5_rafraichissement" %in% cols)
  expect_false(".a5_status" %in% cols)
})


# ==============================================================================
# The cause must survive extract_indicator_value() (§4.1)
# ==============================================================================

test_that(".capture_status_attr carries the core status column", {
  result <- data.frame(
    A5 = c(NA_real_, NA_real_),
    a5_status = c("skipped_no_lst", "skipped_no_lst"),
    stringsAsFactors = FALSE)
  vals <- c(NA_real_, NA_real_)

  out <- nemetonshiny:::.capture_status_attr(vals, result)

  expect_equal(attr(out, "nemeton_status_name"), "a5_status")
  expect_equal(attr(out, "nemeton_status"), c("skipped_no_lst", "skipped_no_lst"))
  # La valeur elle-meme n'est pas touchee.
  expect_equal(as.numeric(out), vals)
})


test_that(".capture_status_attr ignores a result without status", {
  result <- data.frame(A5 = c(1, 2))
  out <- nemetonshiny:::.capture_status_attr(c(1, 2), result)

  expect_null(attr(out, "nemeton_status_name"))
  expect_null(attr(out, "nemeton_status"))
})


test_that(".capture_status_attr refuses an ambiguous or mismatched status", {
  # Deux colonnes de statut : deviner laquelle serait pire que de ne rien dire.
  two <- data.frame(A5 = 1:2, a5_status = c("a", "b"), r5_status = c("c", "d"),
                    stringsAsFactors = FALSE)
  expect_null(attr(nemetonshiny:::.capture_status_attr(c(1, 2), two),
                   "nemeton_status_name"))

  # Longueur incoherente : la colonne ne decrit pas ces unites.
  bad <- data.frame(A5 = 1:2, a5_status = c("a", "b"), stringsAsFactors = FALSE)
  expect_null(attr(nemetonshiny:::.capture_status_attr(c(1, 2, 3), bad),
                   "nemeton_status_name"))
})

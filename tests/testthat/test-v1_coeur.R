# Adoption du coeur nemeton 1.0.0

test_that("P2 without real age is named, not left as a bare gap", {
  p <- data.frame(id = 1:2, age = NA_real_)
  v <- .cause_sans_age(c(NA_real_, NA_real_), "indicateur_p2_station", p, list())
  expect_identical(attr(v, "nemeton_status"), c("sans_age", "sans_age"))
  expect_identical(attr(v, "nemeton_status_name"), "p2_status")
  # En mode IFN, l'age n'intervient pas
  v2 <- .cause_sans_age(c(NA_real_, NA_real_), "indicateur_p2_station", p, list(),
                        ifn_mode = TRUE)
  expect_null(attr(v2, "nemeton_status"))
  # Un age connu : rien a dire
  v3 <- .cause_sans_age(c(NA_real_, NA_real_), "indicateur_p2_station",
                        data.frame(id = 1:2, age = c(40, NA)), list())
  expect_null(attr(v3, "nemeton_status"))
  # Un statut du coeur (hors_courbe) n'est pas ecrase
  v4 <- structure(c(NA_real_, NA_real_), nemeton_status = c("hors_courbe", NA),
                  nemeton_status_name = "p2_status")
  expect_identical(attr(.cause_sans_age(v4, "indicateur_p2_station", p, list()),
                        "nemeton_status"), c("hors_courbe", NA))
})

test_that("C1 from the NDVI proxy is flagged only without a LiDAR canopy model", {
  p <- data.frame(id = 1:2)
  local_mocked_bindings(resolve_raster_layer = function(layers, name) NULL)
  v <- .cause_sans_age(c(80, 95), "indicateur_c1_biomasse", p, list())
  expect_identical(attr(v, "nemeton_status_name"), "c1_status")
  expect_identical(unique(attr(v, "nemeton_status")), "ndvi_sans_age")
  local_mocked_bindings(resolve_raster_layer = function(layers, name) "mnh")
  expect_null(attr(.cause_sans_age(c(80, 95), "indicateur_c1_biomasse", p, list()),
                   "nemeton_status"))
})

test_that("the family view explains P2 without age and the core 1.0 statuses", {
  i18n <- get_i18n("fr")
  sf_p2 <- data.frame(indicateur_p2_station = c(NA_real_, NA_real_),
                      .p2_status = c("sans_age", "sans_age"))
  html <- as.character(indicator_na_banner(sf_p2, "indicateur_p2_station", i18n))
  expect_match(html, i18n$t("p2_sans_age"), fixed = TRUE)
  sf_p3 <- data.frame(indicateur_p3_qualite_bois = c(40, 55),
                      .p3_status = c("diametre_seul", "diametre_seul"))
  html3 <- as.character(indicator_na_banner(sf_p3, "indicateur_p3_qualite_bois", i18n))
  expect_match(html3, i18n$t("p3_diametre_seul"), fixed = TRUE)
  # Statut nominal : pas de bandeau
  sf_ok <- data.frame(indicateur_p3_qualite_bois = c(40, 55), .p3_status = "complet")
  expect_null(indicator_na_banner(sf_ok, "indicateur_p3_qualite_bois", i18n))
})

test_that("every status the core 1.0 emits for its conditional indicators is translated", {
  i18n <- get_i18n("en")
  for (k in c(paste0(c("a3", "a4", "w4", "r6"), "_skipped_no_micro"), "t3_skipped_no_sufosat",
              paste0(c("b4", "l3"), "_skipped_no_spectral"),
              paste0(c("a3", "a4", "w4", "r6", "t3", "b4", "l3"), "_skipped_no_coverage"),
              "p2_hors_courbe", "p3_diametre_seul", "p3_diametre_forme", "p3_diametre_defauts")) {
    expect_true(isTRUE(i18n$has(k)), info = k)
  }
})

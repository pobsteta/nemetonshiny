# Widgets NDP (audit 1.0, #59)

test_that("ndp_badge uses dark text on the light backgrounds and translated names", {
  b3 <- as.character(ndp_badge(3, "en"))
  expect_match(b3, "color: #212529", fixed = TRUE)
  expect_match(b3, get_i18n("en")$t("ndp_niveau_3"), fixed = TRUE)
  b0 <- as.character(ndp_badge(0, "fr"))
  expect_match(b0, "color: #ffffff", fixed = TRUE)
  expect_match(b0, get_i18n("fr")$t("ndp_niveau_0"), fixed = TRUE)
})

test_that("NDP widgets survive NA and out-of-range levels", {
  expect_match(as.character(ndp_badge(NA)), "NDP 0", fixed = TRUE)
  expect_match(as.character(ndp_badge(9)), "NDP 4", fixed = TRUE)
  expect_match(as.character(ndp_progress_bar(NA, "en")), "Confidence", fixed = TRUE)
  expect_identical(.ndp_borne(-1), 0L)
})

# Avertissements de telechargement traduits a l'affichage (audit 1.0, #63)
test_that(".message_calcul translates keyed entries and keeps plain messages", {
  en <- get_i18n("en")
  expect_match(.message_calcul(list(key = "dl_lasr_echec", args = list("boom"),
                                    message = "Dérivation"), en),
               "CHM derivation with lasR failed: boom", fixed = TRUE)
  expect_identical(.message_calcul(list(message = "brut"), en), "brut")
  expect_identical(.message_calcul(list(key = "cle_absente", message = "repli"), en), "repli")
})

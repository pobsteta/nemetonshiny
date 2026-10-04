# Motifs du rapport d'import des validations sanitaires (brief coeur 0.209.0)

test_that(".hv_translate_reasons translates known codes, keeps unknown ones", {
  i18n <- get_i18n("fr")
  d <- data.frame(alert_id = 1:5,
                  reason = c("ok", "missing_stade", "unknown_stade",
                             "no_alert_within_snap", "code_futur"))
  out <- nemetonshiny:::.hv_translate_reasons(d, i18n)
  expect_identical(out$reason[1:4], c(i18n$t("hv_motif_ok"),
                                      i18n$t("hv_motif_missing_stade"),
                                      i18n$t("hv_motif_unknown_stade"),
                                      i18n$t("hv_motif_no_alert_within_snap")))
  expect_identical(out$reason[5], "code_futur")
  expect_identical(out$alert_id, 1:5)
})

test_that("reason keys exist in both languages and fit the NMT limit", {
  for (lang in c("fr", "en")) {
    i18n <- get_i18n(lang)
    for (k in paste0("hv_motif_", c("ok", "missing_stade", "unknown_stade",
                                    "no_alert_within_snap"))) {
      expect_true(i18n$has(k), label = paste(lang, k))
      expect_lte(nchar(k), 30L)
    }
  }
})

test_that(".hv_translate_reasons tolerates missing input", {
  i18n <- get_i18n("en")
  expect_null(nemetonshiny:::.hv_translate_reasons(NULL, i18n))
  d <- data.frame(x = 1)
  expect_identical(nemetonshiny:::.hv_translate_reasons(d, i18n), d)
  d2 <- data.frame(reason = NA_character_)
  expect_true(is.na(nemetonshiny:::.hv_translate_reasons(d2, i18n)$reason))
})

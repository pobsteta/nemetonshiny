# Tests — constructeurs de graphiques au clic sur la maille E-OBS (spec 036).
# Pure viz : on vérifie les stats ombrothermiques (numériques) et que chaque
# constructeur renvoie un objet plotly, y compris en repli NA-safe.

i18n <- nemetonshiny:::get_i18n("fr")

test_that(".regen_ctx_ombro_stats calcule mois secs (P<2T) + De Martonne", {
  clim_rr <- data.frame(month = 1:12, value = c(80,70,60,50,40,30,20,25,45,70,85,90))
  clim_t  <- data.frame(month = 1:12, value = c(2,3,6,9,13,17,19,19,15,11,6,3))
  st <- nemetonshiny:::.regen_ctx_ombro_stats(clim_rr, clim_t)
  # Mois secs = ceux où P < 2T : juin (30<34), juillet (20<38), août (25<38).
  expect_equal(st$dry_idx, c(6L, 7L, 8L))
  expect_equal(st$dry_months, 3L)
  # De Martonne = sum(P) / (mean(T) + 10).
  expect_equal(st$demartonne, sum(clim_rr$value) / (mean(clim_t$value) + 10))
})

test_that(".regen_ctx_ombro_stats NA-safe (data.frame vide / manquant)", {
  st <- nemetonshiny:::.regen_ctx_ombro_stats(NULL, NULL)
  expect_equal(st$dry_months, NA_integer_)
  expect_true(is.na(st$demartonne))
  expect_length(st$dry_idx, 0L)
})

test_that(".regen_ctx_series_plot rend un plotly, repli si série NA", {
  skip_if_not_installed("plotly")
  ser <- data.frame(year = 2011:2020, value = c(24,25,24.5,26,25.5,27,26,28,27.5,29))
  fit <- nemeton::eobs_trend_fit(ser)
  p <- nemetonshiny:::.regen_ctx_series_plot(ser, fit, "T", "°C/déc", i18n)
  expect_s3_class(p, "plotly")
  # Série tout NA -> panneau « hors couverture », toujours un plotly (pas d'erreur).
  pe <- nemetonshiny:::.regen_ctx_series_plot(
    data.frame(year = 2011:2013, value = c(NA, NA, NA)), NULL, "T", "x", i18n)
  expect_s3_class(pe, "plotly")
})

test_that(".regen_ctx_anomaly_plot et distrib rendent un plotly", {
  skip_if_not_installed("plotly")
  ser <- data.frame(year = 2011:2020, value = seq(24, 29, length.out = 10))
  expect_s3_class(nemetonshiny:::.regen_ctx_anomaly_plot(ser, "T", "hot_red", i18n), "plotly")
  expect_s3_class(nemetonshiny:::.regen_ctx_anomaly_plot(ser, "P", "dry_red", i18n), "plotly")
  expect_s3_class(
    nemetonshiny:::.regen_ctx_distrib_plot(rnorm(200, 0.5, 0.3), 1.1, "T", "°C/déc", i18n),
    "plotly")
  # < 5 valeurs finies -> repli distribution indisponible.
  expect_s3_class(nemetonshiny:::.regen_ctx_distrib_plot(c(1, NA, 2), 1, "T", "x", i18n), "plotly")
  # Percentile régional (spec 036 §5.3) : point au-dessus de 8 valeurs sur 10 -> P80.
  p <- nemetonshiny:::.regen_ctx_distrib_plot(1:10, 8, "T", "°C/déc", i18n)
  ann <- vapply(p$x$layoutAttrs, function(a)
    paste(unlist(lapply(a$annotations, `[[`, "text")), collapse = " "), character(1))
  expect_true(any(grepl("80", ann)))
})

test_that(".regen_ctx_ombro_plot rend un plotly, titre honnête tg vs tx", {
  skip_if_not_installed("plotly")
  clim_rr <- data.frame(month = 1:12, value = c(80,70,60,50,40,30,20,25,45,70,85,90))
  clim_t  <- data.frame(month = 1:12, value = c(2,3,6,9,13,17,19,19,15,11,6,3))
  p_tg <- nemetonshiny:::.regen_ctx_ombro_plot(clim_rr, clim_t, TRUE, i18n)
  p_tx <- nemetonshiny:::.regen_ctx_ombro_plot(clim_rr, clim_t, FALSE, i18n)
  expect_s3_class(p_tg, "plotly")
  expect_s3_class(p_tx, "plotly")
  # Le titre honnête distingue tg (T° moyenne réelle) du repli tx (majorant).
  expect_match(i18n$t("regen_ctx_ombro_proxy_tx"), "major", ignore.case = TRUE)
  expect_no_match(i18n$t("regen_ctx_ombro_tg"), "major", ignore.case = TRUE)
  # Climatologie manquante -> repli hors couverture.
  expect_s3_class(nemetonshiny:::.regen_ctx_ombro_plot(NULL, clim_t, TRUE, i18n), "plotly")
})

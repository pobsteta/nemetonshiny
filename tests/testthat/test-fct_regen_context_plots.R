# Tests — constructeurs de graphiques au clic sur la maille E-OBS (spec 036).
# Pure viz : on vérifie les stats ombrothermiques (numériques) et que chaque
# constructeur renvoie un objet plotly, y compris en repli NA-safe.

`%||%` <- function(a, b) if (is.null(a)) b else a

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

test_that("chaque panneau porte son titre en annotation (pas un titre global écrasé)", {
  skip_if_not_installed("plotly")
  ser1 <- data.frame(year = 2011:2020, value = seq(24, 29, length.out = 10))
  ser2 <- data.frame(year = 2011:2020, value = seq(200, 120, length.out = 10))
  p1 <- nemetonshiny:::.regen_ctx_series_plot(ser1, nemeton::eobs_trend_fit(ser1),
    "T°max estivale", "°C/déc", nemetonshiny:::get_i18n("fr"))
  p2 <- nemetonshiny:::.regen_ctx_series_plot(ser2, nemeton::eobs_trend_fit(ser2),
    "Précipitations estivales", "mm/déc", nemetonshiny:::get_i18n("fr"))
  # Titre porté par une annotation (pas layout$title) -> survit au subplot.
  b1 <- plotly::plotly_build(p1)
  has_title_ann <- any(vapply(b1$x$layout$annotations,
    function(a) grepl("T°max estivale", a$text %||% ""), logical(1)))
  expect_true(has_title_ann)
  # Bivariée : subplot conserve LES DEUX titres, remappés sur chaque panneau.
  sp <- plotly::subplot(list(p1, p2), nrows = 1, margin = 0.06)
  b <- plotly::plotly_build(sp)
  titles <- Filter(function(a) grepl("estivale", a$text %||% ""), b$x$layout$annotations)
  expect_length(titles, 2L)
  xs <- sort(vapply(titles, function(a) a$x, numeric(1)))
  expect_lt(xs[1], 0.4); expect_gt(xs[2], 0.6)   # un titre par panneau (gauche/droite)
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

  # Barres précip + courbe température ; l'axe précip contient max(P) SANS
  # débordement (l'axe = max(T, P/2)*2) et respecte le couplage de Gaussen P=2T.
  # yaxis = axe de BASE = précipitations ; yaxis2 = axe SUPERPOSÉ = températures
  # (la courbe T doit passer DEVANT les barres, d'où le portage sur l'axe superposé).
  b <- plotly::plotly_build(p_tg)
  types <- vapply(b$x$data, function(tr) tr$type, character(1))
  expect_true("bar" %in% types && "scatter" %in% types)
  r_precip <- unlist(b$x$layout$yaxis$range); r_temp <- unlist(b$x$layout$yaxis2$range)
  expect_equal(r_precip, 2 * r_temp)                     # couplage P = 2T
  expect_gte(max(r_precip), max(clim_rr$value))          # aucune barre tronquée
  expect_gte(max(r_temp), max(clim_t$value))             # courbe T non tronquée
  # Les 12 mois doivent être présents : x NUMÉRIQUE 1-12 (un axe catégoriel à
  # lettres fusionnerait les mois homonymes -> saison estivale repliée).
  for (tr in b$x$data) expect_length(tr$x, 12L)
  expect_equal(unlist(b$x$layout$xaxis$tickvals), 1:12)
})

# Tests de la fonction de tracé pure de la planche pixel dépérissement.
# `plot_pixel_dieback()` ne calcule RIEN : elle consomme un `prepared`
# (sortie de nemeton::prepare_pixel_dieback_series). On teste ici avec un
# `prepared` factice reproduisant exactement la structure de la Partie A.

fake_prepared <- function() {
  d <- as.Date("2023-01-01") + seq(0, 700, 10)
  yr <- as.integer(format(d, "%Y"))
  dy <- as.integer(format(d, "%j"))
  set.seed(42)
  list(
    grid_swir = data.frame(date = d, val = runif(length(d)), year = yr, doy = dy),
    grid_re   = data.frame(date = d, val = runif(length(d)), year = yr, doy = dy),
    obs_swir  = data.frame(date = d, val = runif(length(d)), year = yr, doy = dy),
    obs_re    = data.frame(date = d, val = runif(length(d)), year = yr, doy = dy),
    trough_swir = data.frame(year = c(2023L, 2024L), date = d[c(20, 55)], val = c(.6, .55)),
    peak_re     = data.frame(year = c(2023L, 2024L), date = d[c(20, 55)], val = c(.65, .62)),
    state     = data.frame(date = d[20:30], year = 2023L,
                           val_sw = runif(11), val_re = runif(11)),
    centroids = data.frame(year = c(2023L, 2024L),
                           val_sw = c(.6, .55), val_re = c(.62, .6)),
    gaps      = data.frame(from = as.Date("2023-11-15"), to = as.Date("2024-02-10"))
  )
}

test_that("plot_pixel_dieback returns a plotly object that builds without error", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("viridisLite")

  i18n <- get_i18n("fr")
  g <- nemetonshiny:::plot_pixel_dieback(
    fake_prepared(), opts = list(show_points = TRUE), i18n = i18n
  )
  expect_s3_class(g, "plotly")

  # Le build plotly ne doit lever aucun warning (ni clé i18n manquante, ni
  # attribut plotly invalide type `colorbar` au mauvais niveau).
  built <- expect_no_warning(plotly::plotly_build(g))
  # 4 panneaux → un double axe Y sur le panneau A (yaxis + yaxis2 remappés
  # par subplot en yaxis/yaxis3…). On vérifie qu'un second axe Y overlay
  # existe et que des traces ont bien été produites.
  expect_gt(length(built$x$data), 4L)
  y_over <- vapply(built$x$layout, function(a)
    is.list(a) && identical(a$overlaying, "y"), logical(1))
  expect_true(any(y_over))
})

test_that("plot_pixel_dieback honours show_points = FALSE (fewer traces)", {
  skip_if_not_installed("plotly")

  fp <- fake_prepared()
  with_pts <- plotly::plotly_build(
    nemetonshiny:::plot_pixel_dieback(fp, opts = list(show_points = TRUE))
  )
  no_pts <- plotly::plotly_build(
    nemetonshiny:::plot_pixel_dieback(fp, opts = list(show_points = FALSE))
  )
  # Retirer les points bruts (2 traces markers) réduit le nombre de traces.
  expect_lt(length(no_pts$x$data), length(with_pts$x$data))
})

test_that("plot_pixel_dieback tolerates 0-row gaps (realistic empty case)", {
  skip_if_not_installed("plotly")

  # `gaps` est légitimement souvent vide (0 ligne) sur une série sans longue
  # lacune d'interpolation — cf. sortie réelle de prepare_pixel_dieback_series.
  fp <- fake_prepared()
  fp$gaps <- fp$gaps[0, , drop = FALSE]
  g <- nemetonshiny:::plot_pixel_dieback(fp, i18n = get_i18n("en"))
  expect_s3_class(g, "plotly")
  expect_no_error(plotly::plotly_build(g))
})

test_that("plot_pixel_dieback works without an i18n translator (raw keys)", {
  skip_if_not_installed("plotly")
  g <- nemetonshiny:::plot_pixel_dieback(fake_prepared(), i18n = NULL)
  expect_s3_class(g, "plotly")
})

test_that(".pixel_export_engine returns kaleido/webshot2/NA without error", {
  eng <- nemetonshiny:::.pixel_export_engine()
  expect_length(eng, 1L)
  expect_true(is.na(eng) || eng %in% c("kaleido", "webshot2"))
})

test_that("save_plotly_png degrades gracefully when no engine is available", {
  skip_if_not_installed("plotly")
  # Force l'absence de moteur pour un test déterministe (l'env CI n'a ni
  # kaleido ni webshot2, mais on ne veut pas dépendre de ça).
  testthat::local_mocked_bindings(.pixel_export_engine = function() NA_character_)
  g <- nemetonshiny:::plot_pixel_dieback(fake_prepared())
  out <- withr::local_tempfile(fileext = ".png")
  res <- NULL
  expect_warning(res <- nemetonshiny:::save_plotly_png(g, out))
  expect_false(res)
  expect_false(file.exists(out))
})

# Tests - contexte climatique estival sur un cercle (spec 063, brief 2026-10-18
# cablage). Coeur moque (nemeton::climat_*), petits rasters synthetiques, aucun
# reseau.

`%||%` <- function(a, b) if (is.null(a)) b else a

.climat_units <- function() {
  sf::st_sf(ug_id = 1:2, indicateur_c1_biomasse = 50,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(900000, 6700000), c(901000, 6700000),
        c(901000, 6701000), c(900000, 6701000), c(900000, 6700000)))),
      sf::st_polygon(list(rbind(c(902000, 6700000), c(903000, 6700000),
        c(903000, 6701000), c(902000, 6701000), c(902000, 6700000)))),
      crs = 2154))
}

# Meta d'une carte au schema courant (10 x 10, variation totale).
.climat_meta <- function(ncol = 10L, statistique = "variation_totale", ...) {
  utils::modifyList(list(
    status = "ok", periode = c(1950L, 2024L), n_annees = 75L,
    statistique = statistique,
    bornes = list(tmax = c(-0.7, 4), precip = c(-270, 120), mode = "france",
                  reference = "eobs_france_metropole"),
    palette = list(classes = seq_len(ncol * ncol),
                   colors = rep("#E52521", ncol * ncol), ncol = ncol,
                   zero = list(tmax = 0.149, precip = 0.692)),
    source = "eobs", attribution = "E-OBS"), list(...))
}

# Ecrit dans `pp` une carte en cache (raster de classes, tendances, meta, series
# du cercle, cercle) pour (source, rayon, periode).
.climat_ecrire_cache <- function(pp, source = "eobs", rayon = 25, periode = "complete",
                                 meta = .climat_meta()) {
  p <- nemetonshiny:::.regen_climat_paths(pp, source, rayon, periode)
  dir.create(p$dir, recursive = TRUE, showWarnings = FALSE)
  g <- terra::rast(nrows = 4, ncols = 4, xmin = 6, xmax = 6.4, ymin = 47, ymax = 47.4,
                   crs = "EPSG:4326")
  terra::writeRaster(terra::setValues(g, rep(c(1, 50, 91, 100), 4)), p$tif, overwrite = TRUE)
  tend <- c(terra::setValues(g, seq(0, 3, length.out = 16)),
            terra::setValues(g, seq(-200, 100, length.out = 16)))
  names(tend) <- c("tendance_tmax", "tendance_precip")
  terra::writeRaster(tend, p$tendances, overwrite = TRUE)
  jsonlite::write_json(meta, p$meta, auto_unbox = TRUE, digits = NA)
  s <- terra::rast(g, nlyrs = 10)
  terra::values(s) <- matrix(rep(seq(24, 29, length.out = 10), each = 16), 16)
  names(s) <- as.character(2011:2020)
  ftx <- file.path(p$dir, "s_tx.tif"); frr <- file.path(p$dir, "s_rr.tif")
  terra::writeRaster(s, ftx, overwrite = TRUE)
  terra::writeRaster(s * 10, frr, overwrite = TRUE)
  fc <- file.path(p$dir, "s_cercle.gpkg")
  cercle <- sf::st_sf(rayon_km = rayon, geometry = sf::st_buffer(
    sf::st_sfc(sf::st_point(c(6.2, 47.2)), crs = 4326), 20000))
  sf::st_write(cercle, fc, quiet = TRUE, delete_dsn = TRUE)
  jsonlite::write_json(list(tx = ftx, rr = frr, cercle = fc), p$series, auto_unbox = TRUE)
  invisible(p)
}

.climat_attendre <- function(session, n = 40L) {
  for (i in seq_len(n)) {
    later::run_now(0.05)
    session$flushReact()
  }
}

.climat_mocks_app <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    .package = "nemetonshiny", .env = env)
}

# --- Service ------------------------------------------------------------------

test_that(".regen_climat_periode maps the period to the core argument and a tag", {
  f <- nemetonshiny:::.regen_climat_periode
  expect_identical(f("complete"), list(arg = "complete", tag = "complete"))
  expect_identical(f(c(2020, 1991)), list(arg = c(1991L, 2020L), tag = "1991-2020"))
  expect_identical(f(NULL)$arg, "complete")
})

test_that("map cache paths are keyed on source, radius and period", {
  pp <- withr::local_tempdir()
  f <- function(...) nemetonshiny:::.regen_climat_paths(pp, ...)$tif
  chemins <- c(f("eobs", 25, "complete"), f("chelsa", 25, "complete"),
               f("eobs", 50, "complete"), f("eobs", 25, c(2006, 2020)),
               f("eobs", 25, c(1991, 2020)))
  expect_length(unique(chemins), 5L)
  expect_match(f("safran", 25, c(2006, 2020)), "climat/carte_safran_r25_2006-2020\\.tif$")
})

test_that("regeneration_climat_cached reads the map back, and rejects a stale scheme", {
  skip_if_not_installed("terra"); skip_if_not_installed("sf")
  pp <- withr::local_tempdir()
  expect_null(nemetonshiny:::regeneration_climat_cached(pp, "eobs", 25, "complete"))

  .climat_ecrire_cache(pp)
  x <- nemetonshiny:::regeneration_climat_cached(pp, "eobs", 25, "complete")
  expect_s4_class(x$raster, "SpatRaster")
  expect_identical(names(x$tendances), c("tendance_tmax", "tendance_precip"))
  expect_s3_class(x$cercle, "sf")
  expect_true(file.exists(x$series$tx))
  expect_equal(x$meta$palette$ncol, 10)

  # Ancien schema 5 x 5, ou pente par decennie : recalcul, pas de service.
  .climat_ecrire_cache(pp, meta = .climat_meta(ncol = 5L))
  expect_null(nemetonshiny:::regeneration_climat_cached(pp, "eobs", 25, "complete"))
  .climat_ecrire_cache(pp, meta = .climat_meta(statistique = "pente_decennie"))
  expect_null(nemetonshiny:::regeneration_climat_cached(pp, "eobs", 25, "complete"))
})

test_that("regeneration_climat_purge removes the map but keeps the circle series", {
  skip_if_not_installed("terra"); skip_if_not_installed("sf")
  pp <- withr::local_tempdir()
  p <- .climat_ecrire_cache(pp)
  serie <- jsonlite::read_json(p$series)$tx
  nemetonshiny:::regeneration_climat_purge(pp, "eobs", 25, "complete")
  expect_false(any(file.exists(c(p$tif, p$tendances, p$meta, p$series))))
  expect_true(file.exists(serie))
})

test_that("run_regeneration_climat passes cache dir, period and cache path to the core", {
  pp <- withr::local_tempdir()
  vu <- new.env()
  testthat::local_mocked_bindings(
    climat_ete_series = function(aoi, source, rayon_km, cache_dir, progress_callback, ...) {
      vu$source <- source; vu$rayon <- rayon_km; vu$cache_dir <- cache_dir
      progress_callback(list(current = "climat:annee", source = source, annee = 2001, i = 3, n = 9))
      list(tx = NULL, rr = NULL, meta = list(status = "ok",
           fichiers = list(tx = "a_tx.tif", rr = "a_rr.tif", cercle = "c.gpkg")))
    },
    climat_tendance_bivariee = function(series, periode, cache_path, ...) {
      vu$periode <- periode; vu$cache_path <- cache_path
      list(meta = list(status = "ok", palette = list(ncol = 10)))
    },
    .package = "nemeton")
  res <- nemetonshiny:::run_regeneration_climat(.climat_units(), pp, "chelsa", 40, c(1991, 2020))
  expect_identical(vu$source, "chelsa")
  expect_equal(vu$rayon, 40)
  expect_identical(vu$cache_dir, file.path(pp, "cache", "regeneration"))
  expect_identical(vu$periode, c(1991L, 2020L))
  p <- nemetonshiny:::.regen_climat_paths(pp, "chelsa", 40, c(1991, 2020))
  expect_identical(vu$cache_path, p$tif)
  expect_identical(jsonlite::read_json(p$series)$tx, "a_tx.tif")
  expect_identical(res$carte$status, "ok")
  # L'etape du coeur a traverse le fichier d'etat.
  st <- nemetonshiny:::.regen_read_climat_step(pp)
  expect_identical(st$phase, "climat:annee")
  expect_equal(st$annee, 2001)
})

test_that("run_regeneration_nuages memoises an ok result on disk", {
  pp <- withr::local_tempdir()
  n_appels <- 0L
  testthat::local_mocked_bindings(
    climat_ete_series = function(...) list(meta = list(status = "ok")),
    climat_nuages_annuels = function(series, mode, ...) {
      n_appels <<- n_appels + 1L
      list(points = data.frame(annee = 2001L, x = 1, y = 2),
           moyennes = data.frame(annee = 2001L, x = 1, y = 2),
           limites = list(x = c(-1, 2), y = c(-1, 3)),
           meta = list(status = "ok", mode = mode))
    },
    .package = "nemeton")
  a <- nemetonshiny:::run_regeneration_nuages(.climat_units(), pp, "safran", 25, "anomalie")
  b <- nemetonshiny:::run_regeneration_nuages(.climat_units(), pp, "safran", 25, "anomalie")
  expect_identical(n_appels, 1L)
  expect_identical(a, b)
  expect_identical(nemetonshiny:::regeneration_nuages_cached(pp, "safran", 25, "anomalie"), a)
  expect_null(nemetonshiny:::regeneration_nuages_cached(pp, "safran", 25, "tendance_cumulee"))
})

test_that("climate progress steps are translated", {
  i18n <- nemetonshiny:::get_i18n("fr")
  f <- function(...) nemetonshiny:::.regen_climat_step_label(i18n, list(...))
  expect_match(f(phase = "climat:telechargement", var = "rr"), "rr")
  expect_match(f(phase = "climat:mensualisation", annee = 1990, i = 41, n = 76), "1990.*41/76")
  expect_match(f(phase = "climat:annee", source = "safran", annee = 2003, i = 2, n = 9),
               "SAFRAN.*2003.*2/9")
  expect_null(f(phase = "climat:complete"))
  expect_null(nemetonshiny:::.regen_climat_step_label(i18n, NULL))
})

# --- Legende 10 x 10 -----------------------------------------------------------

test_that("bivariate_legend_html draws a 10 x 10 grid with the 0/0 dashed lines", {
  pal <- stats::setNames(rep("#E52521", 100), as.character(1:100))
  html <- as.character(nemetonshiny:::bivariate_legend_html(
    palette = pal, ncol = 10, zero = list(tmax = 0.149, precip = 0.692),
    x_range = c(-270, 120), y_range = c(-0.7, 4)))
  expect_identical(lengths(regmatches(html, gregexpr("title=\"classe ", html))), 100L)
  # Cellules de 14 px a partir de 10 x 10 : grille de 140 px.
  expect_match(html, "width:14px;height:14px")
  # Bordure comprise dans la cellule : sinon 10 x 16 px = 160 px debordent du
  # cadre de 140 px et recouvrent l'axe des abscisses (vu en Chrome).
  expect_match(html, "box-sizing:border-box;width:14px")
  # Bornes de l'axe X sur leur propre ligne, libelle dessous.
  expect_match(html, "<span>-270</span>\\s*<span>120</span>")
  expect_match(html, sprintf("left:%.1fpx", 0.692 * 140), fixed = TRUE)
  expect_match(html, sprintf("bottom:%.1fpx", 0.149 * 140), fixed = TRUE)
})

test_that(".regen_climat_legende flags relative colours when the France bounds are missing", {
  i18n <- nemetonshiny:::get_i18n("fr")
  ok <- nemetonshiny:::.regen_climat_legende(.climat_meta(), i18n)
  expect_match(ok, "1950-2024")
  expect_false(grepl(i18n$t("regen_climat_bornes_relatives"), ok, fixed = TRUE))
  rel <- nemetonshiny:::.regen_climat_legende(.climat_meta(bornes = list(
    tmax = c(0, 2), precip = c(-50, 10), mode = "emprise",
    reason = "france_eobs_cache_missing")), i18n)
  expect_true(grepl(i18n$t("regen_climat_bornes_relatives"), rel, fixed = TRUE))
})

test_that("the yearly clouds build an animated plotly and a small-multiples grid", {
  skip_if_not_installed("plotly"); skip_if_not_installed("ggplot2")
  i18n <- nemetonshiny:::get_i18n("fr")
  res <- list(points = data.frame(annee = rep(2001:2012, each = 5), x = rnorm(60), y = rnorm(60)),
              moyennes = data.frame(annee = 2001:2012, x = 0, y = 0),
              limites = list(x = c(-3, 3), y = c(-3, 3)),
              meta = list(status = "ok", mode = "tendance_cumulee", annees = 2001:2012,
                          recul_min = 10L, n_cellules = 5L, attribution = "SAFRAN"))
  p <- nemetonshiny:::.regen_nuages_plot(res, i18n)
  expect_s3_class(p, "plotly")
  b <- plotly::plotly_build(p)
  expect_length(b$x$frames, 12L)
  # Tendance cumulee : la 1re annee de la serie = 2001 - (10 - 1) = 1992.
  titre <- b$x$layout$xaxis$title
  expect_match(if (is.list(titre)) titre$text else titre, "1992")
  expect_s3_class(nemetonshiny:::.regen_nuages_grille(res, i18n), "ggplot")
})

# --- Module (testServer) ---------------------------------------------------------

test_that("no cache: the tab shows 'to compute' and launches nothing", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")
  pp <- withr::local_tempdir()
  as <- shiny::reactiveValues(current_project = list(id = "p1", path = pp,
                                                     indicators_sf = .climat_units()))
  appels <- 0L
  testthat::local_mocked_bindings(
    climat_ete_series = function(...) { appels <<- appels + 1L; NULL },
    climat_tendance_bivariee = function(...) { appels <<- appels + 1L; NULL },
    .package = "nemeton")
  .climat_mocks_app()
  testthat::local_mocked_bindings(regen_climat_eobs_pret = function(...) FALSE,
                                  .package = "nemetonshiny")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(climat_onglet = "eobs", climat_rayon_km = 25, climat_periode = "complete")
    expect_identical(climat$eobs$status, "a_calculer")
    expect_identical(climat_task$status(), "initial")
    expect_identical(appels, 0L)
    html <- as.character(output$climat_statut$html)
    # E-OBS sans cache machine : avertissement du premier usage (8 Go).
    expect_match(html, "8 Go")
  })
})

test_that("the period defaults to the chosen years 2006-2020, without an IF49 option", {
  skip_if_not_installed("bslib")
  ui <- with_mocked_bindings(get_app_options = function() list(language = "fr"),
    nemetonshiny:::mod_regeneration_ui("regen"), .package = "nemetonshiny")
  h <- as.character(ui)
  expect_false(grepl("if49", h, fixed = TRUE))
  # Radio « Choix des annees » coche, curseur 2006-2020.
  expect_match(h, 'value="libre"\\s+checked', perl = TRUE)
  expect_match(h, 'data-from="2006"', fixed = TRUE)
  expect_match(h, 'data-to="2020"', fixed = TRUE)
})

test_that("without any period input, the server reads the 2006-2020 map", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf"); skip_if_not_installed("terra")
  pp <- withr::local_tempdir()
  .climat_ecrire_cache(pp, source = "eobs", rayon = 25, periode = c(2006, 2020))
  as <- shiny::reactiveValues(current_project = list(id = "p1", path = pp,
                                                     indicators_sf = .climat_units()))
  .climat_mocks_app()
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(climat_onglet = "eobs", climat_rayon_km = 25)
    expect_identical(climat$eobs$status, "ok")
    expect_match(climat$eobs$cle, "2006-2020$")
  })
})

test_that("a cached map is shown without any computation", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf"); skip_if_not_installed("terra")
  pp <- withr::local_tempdir()
  .climat_ecrire_cache(pp, source = "chelsa", rayon = 30, periode = c(2006, 2020))
  as <- shiny::reactiveValues(current_project = list(id = "p1", path = pp,
                                                     indicators_sf = .climat_units()))
  .climat_mocks_app()
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(climat_onglet = "chelsa", climat_rayon_km = 30,
                      climat_periode = "libre", climat_periode_libre = c(2006, 2020))
    expect_identical(climat$chelsa$status, "ok")
    expect_s4_class(climat$chelsa$raster, "SpatRaster")
    expect_identical(climat_task$status(), "initial")
    expect_match(as.character(output$climat_statut$html), "1950-2024")
    # Autre rayon : pas de cache -> a calculer.
    session$setInputs(climat_rayon_km = 35)
    expect_identical(climat$chelsa$status, "a_calculer")
  })
})

test_that("Compute runs the task, then the map is read back from the cache", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf"); skip_if_not_installed("terra")
  withr::local_options(nemetonshiny.analyse_inline = TRUE)
  pp <- withr::local_tempdir()
  as <- shiny::reactiveValues(current_project = list(id = "p1", path = pp,
                                                     indicators_sf = .climat_units()))
  vu <- new.env()
  .climat_mocks_app()
  testthat::local_mocked_bindings(
    run_regeneration_climat = function(units, project_path, source, rayon_km, periode) {
      vu$args <- list(source = source, rayon_km = rayon_km, periode = periode)
      .climat_ecrire_cache(project_path, source, rayon_km, periode)
      list(series = list(status = "ok"), carte = list(status = "ok"))
    },
    .package = "nemetonshiny")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(climat_onglet = "eobs", climat_rayon_km = 25,
                      climat_periode = "libre", climat_periode_libre = c(1991, 2020))
    expect_identical(climat$eobs$status, "a_calculer")
    session$setInputs(climat_calculer = 1)
    expect_true(isTRUE(busy()))
    .climat_attendre(session)
    expect_identical(climat_task$status(), "success")
    expect_identical(vu$args$source, "eobs")
    expect_equal(vu$args$periode, c(1991, 2020))
    expect_identical(climat$eobs$status, "ok")
    expect_s4_class(climat$eobs$raster, "SpatRaster")
    expect_false(isTRUE(busy()))
  })
})

test_that("an out-of-coverage source shows its banner", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")
  withr::local_options(nemetonshiny.analyse_inline = TRUE)
  pp <- withr::local_tempdir()
  as <- shiny::reactiveValues(current_project = list(id = "p1", path = pp,
                                                     indicators_sf = .climat_units()))
  .climat_mocks_app()
  testthat::local_mocked_bindings(
    run_regeneration_climat = function(...) list(
      series = list(status = "hors_couverture", reason = "safran_france_only"),
      carte = list(status = "hors_couverture", reason = "safran_france_only")),
    .package = "nemetonshiny")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(climat_onglet = "safran", climat_rayon_km = 25, climat_periode = "complete")
    session$setInputs(climat_calculer = 1)
    .climat_attendre(session)
    expect_identical(climat$safran$status, "hors_couverture")
    expect_match(as.character(output$climat_statut$html), "France m")
  })
})

test_that("yearly clouds: Compute fills the clouds from the task result", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf"); skip_if_not_installed("plotly")
  withr::local_options(nemetonshiny.analyse_inline = TRUE)
  pp <- withr::local_tempdir()
  as <- shiny::reactiveValues(current_project = list(id = "p1", path = pp,
                                                     indicators_sf = .climat_units()))
  .climat_mocks_app()
  vu <- new.env()
  testthat::local_mocked_bindings(
    run_regeneration_nuages = function(units, project_path, source, rayon_km, mode) {
      vu$source <- source; vu$mode <- mode
      list(points = data.frame(annee = rep(1990:1999, each = 3), x = 1, y = 1),
           moyennes = data.frame(annee = 1990:1999, x = 1, y = 1),
           limites = list(x = c(-1, 2), y = c(-1, 2)),
           meta = list(status = "ok", mode = mode, annees = 1990:1999, n_cellules = 3L))
    },
    .package = "nemetonshiny")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(climat_onglet = "nuages", climat_rayon_km = 25,
                      climat_nuages_source = "safran", climat_nuages_mode = "tendance_cumulee")
    expect_identical(climat$nuages$status, "a_calculer")
    session$setInputs(climat_calculer = 1)
    .climat_attendre(session)
    expect_identical(vu$source, "safran"); expect_identical(vu$mode, "tendance_cumulee")
    expect_identical(climat$nuages$status, "ok")
    expect_false(is.null(output$climat_nuages))
  })
})

test_that("Recompute purges only the current map, then computes again", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf"); skip_if_not_installed("terra")
  withr::local_options(nemetonshiny.analyse_inline = TRUE)
  pp <- withr::local_tempdir()
  courant <- .climat_ecrire_cache(pp, "eobs", 25, "complete")
  autre <- .climat_ecrire_cache(pp, "eobs", 25, c(2006, 2020))
  as <- shiny::reactiveValues(current_project = list(id = "p1", path = pp,
                                                     indicators_sf = .climat_units()))
  .climat_mocks_app()
  purge_vue <- NULL
  testthat::local_mocked_bindings(
    run_regeneration_climat = function(units, project_path, source, rayon_km, periode) {
      purge_vue <<- file.exists(courant$tif)
      .climat_ecrire_cache(project_path, source, rayon_km, periode)
      list(series = list(status = "ok"), carte = list(status = "ok"))
    },
    .package = "nemetonshiny")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(climat_onglet = "eobs", climat_rayon_km = 25, climat_periode = "complete")
    session$setInputs(climat_recalculer = 1)
    .climat_attendre(session)
    expect_false(purge_vue)                    # le cache courant etait purge au lancement
    expect_true(file.exists(autre$tif))        # l'autre periode est intacte
    expect_identical(climat$eobs$status, "ok")
  })
})

test_that("a click on a map cell fills the 4 graphs from the sub-tab series", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf"); skip_if_not_installed("terra")
  pp <- withr::local_tempdir()
  .climat_ecrire_cache(pp, "eobs", 25, "complete")
  as <- shiny::reactiveValues(current_project = list(id = "p1", path = pp,
                                                     indicators_sf = .climat_units()))
  vu <- new.env(); vu$n <- 0L
  testthat::local_mocked_bindings(
    eobs_summer_series = function(stack, point) {
      vu$n <- vu$n + 1L; vu$nlyr <- terra::nlyr(stack)
      data.frame(year = 2011:2020, value = seq(24, 29, length.out = 10))
    },
    eobs_trend_fit = function(series) list(slope_decade = 1.2, intercept = -400,
      r2 = 0.8, p_value = 0.01, n = 10),
    eobs_monthly_climatology = function(daily, point, var, years = NULL)
      data.frame(month = 1:12, value = rep(50, 12)),
    .package = "nemeton")
  .climat_mocks_app()
  testthat::local_mocked_bindings(regen_eobs_cached_nc = function(project_path, var) NULL,
                                  .package = "nemetonshiny")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(climat_onglet = "eobs", climat_rayon_km = 25, climat_periode = "complete")
    session$setInputs(climat_carte_eobs_click = list(lng = 6.1, lat = 47.1))
    cs <- rv$click_series
    expect_identical(cs$view, "eobs")
    expect_length(cs$entries, 2L)               # tx ET rr
    expect_identical(vu$nlyr, 10)               # series estivales du cercle
    expect_length(cs$distrib, 2L)
    expect_true(all(is.finite(vapply(cs$distrib, function(e) e$point, numeric(1)))))
    expect_false(is.null(output$ctx_g1)); expect_false(is.null(output$ctx_g3))
    expect_false(is.null(output$ctx_g4))
  })
})

test_that("a click without a project, or on a map not computed, is a guarded no-op", {
  skip_if_not_installed("shiny")
  as <- shiny::reactiveValues(current_project = NULL)
  .climat_mocks_app()
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(climat_carte_eobs_click = list(lng = 6.1, lat = 47.2))
    expect_null(rv$click_series)
  })
  pp <- withr::local_tempdir()
  as2 <- shiny::reactiveValues(current_project = list(id = "p1", path = pp,
                                                      indicators_sf = .climat_units()))
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as2), {
    session$setInputs(climat_onglet = "eobs", climat_rayon_km = 25, climat_periode = "complete")
    session$setInputs(climat_carte_eobs_click = list(lng = 6.1, lat = 47.2))
    expect_null(rv$click_series)
  })
})

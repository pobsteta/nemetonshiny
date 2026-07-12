# Tests — moteur « risque de gel tardif » (R7, meteoland) — spec 027 P4.
#
# run_regeneration_frost orchestre meteoland_daily_grid -> indicateur_r7_gel,
# avec cache + repli gracieux. Aucune logique métier testée (cœur mocké) —
# uniquement l'orchestration app.

`%||%` <- function(a, b) if (is.null(a)) b else a

.frost_units <- function(n = 2) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- i * 10
    sf::st_polygon(list(matrix(c(
      x0, 0, x0 + 5, 0, x0 + 5, 5, x0, 5, x0, 0), ncol = 2, byrow = TRUE)))
  })
  sf::st_sf(ug_id = seq_len(n), geometry = sf::st_sfc(polys, crs = 2154))
}

test_that("meteoland absent -> R7 skip, tmin NULL, aucune erreur", {
  skip_if_not_installed("sf")
  u <- .frost_units(2)
  seen <- new.env(); seen$tmin <- "unset"
  testthat::local_mocked_bindings(
    regen_meteoland_available = function() FALSE, .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    indicateur_r7_gel = function(units, tmin = NULL, ...) {
      seen$tmin <- tmin
      units$R7 <- NA_real_; units$r7_status <- "skipped_no_tmin"; units
    }, .package = "nemeton")

  res <- nemetonshiny:::run_regeneration_frost(
    u, project_path = NULL, cfg = list(year_moyenne = 2018))
  expect_null(seen$tmin)                         # tmin NULL transmis au cœur
  expect_identical(res$r7_status, "skipped_no_tmin")
  expect_false(res$tmin_available)
})

test_that("meteoland présent -> grille interpolée, cachée et transmise à R7", {
  skip_if_not_installed("sf")
  u <- .frost_units(2)
  pp <- withr::local_tempdir()
  seen <- new.env()
  testthat::local_mocked_bindings(
    regen_meteoland_available = function() TRUE,
    .resolve_regen_dem = function(project_path) "FAKE_DEM",
    .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    meteoland_daily_grid = function(aoi, dem, years, ...) {
      seen$dem <- dem; seen$years <- years; "TMIN_STACK"
    },
    indicateur_r7_gel = function(units, tmin = NULL, ...) {
      seen$tmin <- tmin
      units$R7 <- 80; units$r7_gel_days <- 2; units$r7_status <- "calculated"; units
    }, .package = "nemeton")
  # Éviter l'IO terra réelle sur la grille factice.
  testthat::local_mocked_bindings(
    writeRaster = function(x, filename, ...) invisible(NULL), .package = "terra")

  res <- nemetonshiny:::run_regeneration_frost(
    u, project_path = pp, cfg = list(year_moyenne = 2018, year_canicule = 2022))

  expect_identical(seen$dem, "FAKE_DEM")         # MNT résolu et passé
  expect_setequal(seen$years, c(2018, 2022))
  expect_identical(seen$tmin, "TMIN_STACK")      # grille passée à R7
  expect_identical(res$r7_status, "calculated")
  expect_true(res$tmin_available)
  # dossier de cache meteoland créé (le .tif y serait écrit).
  expect_true(dir.exists(file.path(pp, "cache", "regeneration", "meteoland")))
})

test_that("run_regeneration_frost n'échoue jamais si le cœur lève", {
  skip_if_not_installed("sf")
  u <- .frost_units(1)
  testthat::local_mocked_bindings(
    regen_meteoland_available = function() FALSE, .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    indicateur_r7_gel = function(...) stop("boom"), .package = "nemeton")

  res <- nemetonshiny:::run_regeneration_frost(u, project_path = NULL, cfg = list())
  expect_identical(res$r7_status, "skipped_no_tmin")   # repli interne, pas d'erreur
  expect_false(res$tmin_available)
})

test_that("le radar résout R7 (sens normal, via i18n) sans routage spécial", {
  # R7 est dans INDICATOR_FAMILIES$R (cœur >= 0.151.0) ; l'app doit produire un
  # libellé lisible via la clé i18n `indicator_R7`, comme R1-R6.
  has_r7 <- "R7" %in% tryCatch(nemetonshiny:::INDICATOR_FAMILIES$R$indicators,
                               error = function(e) character())
  skip_if_not(has_r7, "cœur installé sans R7")
  i18n <- get_i18n("fr")
  lbl <- nemetonshiny:::clean_indicator_label("indicateur_r7_gel", i18n)
  expect_match(lbl, "R7")
  expect_match(lbl, "gel", ignore.case = TRUE)
})

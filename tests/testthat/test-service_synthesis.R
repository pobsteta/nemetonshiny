# Tests du service synthese (scores de famille + score global, hors Shiny)

.synth_project <- function(ndp_level = 0L) {
  ind <- sf::st_sf(
    ug_id = 1:3,
    indicateur_b1_protection = c(40, 60, 80),
    indicateur_c1_biomasse = c(70, 90, NA),
    indicateur_w1_reseau = c(10, 20, 30),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2)),
      crs = 2154
    )
  )
  list(
    id = "p-test",
    metadata = list(name = "Foret test", status = "completed",
                    ndp_level = ndp_level, updated_at = "2026-10-04T10:00:00"),
    parcels = data.frame(id = 1:5),
    indicators_sf = ind
  )
}

.identity_enrichers <- function(env = parent.frame()) {
  local_mocked_bindings(
    add_r5_to_indicators = function(base_sf, project) base_sf,
    add_regen_r_indicators = function(base_sf, project) base_sf,
    .env = env
  )
}

test_that("project_family_scores returns NULL without indicators", {
  expect_null(project_family_scores(NULL))
  expect_null(project_family_scores(list(indicators_sf = NULL)))
  empty <- sf::st_sf(geometry = sf::st_sfc(crs = 2154))
  expect_null(project_family_scores(list(indicators_sf = empty)))
})

test_that("project_family_scores delegates to the core aggregation", {
  .identity_enrichers()
  p <- .synth_project()
  got <- suppressMessages(project_family_scores(p))
  want <- suppressMessages(
    create_family_index(p$indicators_sf, method = "mean", na.rm = TRUE)
  )
  expect_s3_class(got, "sf")
  expect_identical(sf::st_drop_geometry(got), sf::st_drop_geometry(want))
})

test_that("project_family_scores returns NULL when the core aggregation fails", {
  .identity_enrichers()
  local_mocked_bindings(create_family_index = function(...) stop("boom"))
  expect_warning(res <- project_family_scores(.synth_project()), "boom")
  expect_null(res)
})

test_that("project_family_means averages every famille_* column", {
  expect_identical(project_family_means(NULL), numeric(0))
  sf_x <- sf::st_sf(
    famille_carbone = c(10, 30, NA), famille_eau = c(1, 2, 3), autre = 1:3,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)),
                          sf::st_point(c(2, 2)))
  )
  # Points sans surface : le coeur retombe sur la moyenne simple et le dit.
  m <- project_family_means(sf_x)
  expect_equal(c(m), c(famille_carbone = 20, famille_eau = 2))
  expect_identical(attr(m, "weighting"), "none")
  no_fam <- sf::st_sf(autre = 1, geometry = sf::st_sfc(sf::st_point(c(0, 0))))
  expect_identical(project_family_means(no_fam), numeric(0))
})

test_that("project_family_means weights the families by UGF area (n. 66)", {
  sq <- function(x, cote) sf::st_polygon(list(rbind(c(x, 0), c(x + cote, 0),
    c(x + cote, cote), c(x, cote), c(x, 0))))
  ugf <- sf::st_sf(famille_carbone = c(20, 80), surface_m2 = c(5000, 495000),
                   geometry = sf::st_sfc(sq(0, 70), sq(1000, 700), crs = 2154))
  m <- project_family_means(ugf)
  # 0,5 ha a 20 et 49,5 ha a 80 : la grande UGF pese 99 %
  expect_equal(unname(m[["famille_carbone"]]), 20 * 0.01 + 80 * 0.99)
  expect_identical(attr(m, "weighting"), "surface")
  expect_equal(unname(project_family_means(ugf, weights = "none")[["famille_carbone"]]), 50)
})

test_that("project_global_index is the core general index, NULL without scores", {
  expect_null(project_global_index(numeric(0), 0L))
  m <- c(famille_biodiversite = 50, famille_carbone = 70)
  expect_identical(project_global_index(m, 1L),
                   nemeton::compute_general_index(m, ndp = 1L))
})

test_that("project_synthesis_summary matches the Synthesis tab figures", {
  .identity_enrichers()
  p <- .synth_project(ndp_level = 1L)
  s <- suppressMessages(project_synthesis_summary(p, "fr"))

  # Reference : le chemin exact de l'onglet (create_family_index -> moyennes ->
  # compute_general_index), recalcule independamment.
  fam_sf <- suppressMessages(
    create_family_index(p$indicators_sf, method = "mean", na.rm = TRUE)
  )
  df <- sf::st_drop_geometry(fam_sf)
  cols <- grep("^famille_[a-z]", names(df), value = TRUE)
  means <- vapply(cols, function(col) mean(df[[col]], na.rm = TRUE), numeric(1))
  ref <- nemeton::compute_general_index(means, ndp = 1L)

  expect_identical(s$global_score, ref$score)
  expect_identical(s$ndp_level, 1L)
  expect_equal(s$confidence, round(ref$confidence, 3))
  expect_identical(s$project_id, "p-test")
  expect_identical(s$name, "Foret test")
  expect_identical(s$status, "completed")
  expect_identical(s$n_ugf, 3L)
  expect_identical(s$n_parcels, 5L)

  expect_identical(nrow(s$families), 12L)
  expect_identical(s$families$code, names(INDICATOR_FAMILIES))
  for (code in s$families$code) {
    col <- get_famille_col(code)
    want <- if (col %in% names(means)) round(unname(means[[col]]), 1) else NA_real_
    expect_identical(s$families$score[s$families$code == code], want,
                     label = paste("score famille", code))
  }
  # Familles sans indicateur : NA, pas NaN
  expect_false(any(is.nan(s$families$score)))
  expect_true(any(is.na(s$families$score)))
})

test_that("project_synthesis_summary labels follow the language", {
  .identity_enrichers()
  p <- .synth_project()
  fr <- suppressMessages(project_synthesis_summary(p, "fr"))
  en <- suppressMessages(project_synthesis_summary(p, "en"))
  b <- INDICATOR_FAMILIES[["B"]]
  expect_identical(fr$families$famille[fr$families$code == "B"], b$name_fr)
  expect_identical(en$families$famille[en$families$code == "B"], b$name_en)
  # Langue inconnue : repli francais
  xx <- suppressMessages(project_synthesis_summary(p, "de"))
  expect_identical(xx$families$famille, fr$families$famille)
})

test_that("project_synthesis_summary handles a project without indicators", {
  p <- list(id = "vide", metadata = list(name = "Vide", status = "draft"))
  s <- project_synthesis_summary(p)
  expect_true(is.na(s$global_score))
  expect_identical(s$n_ugf, 0L)
  expect_identical(s$n_parcels, 0L)
  expect_true(all(is.na(s$families$score)))
  expect_error(project_synthesis_summary(NULL), "NULL")
})

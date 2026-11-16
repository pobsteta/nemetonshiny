# Tests — mission 034 : le froid d'hiver (minimum decennal E-OBS TN) ecarte les
# essences gelives du classement reGeneration (coeur nemeton >= 2026.11.32).
#
# `nemeton::regen_tmin_decennale()` coute ~15 min et touche le reseau : il est
# TOUJOURS mocke ici. Seul `regen_rank_species()` est appele pour de vrai (pur,
# sans reseau), quand le coeur installe le fournit.

`%||%` <- function(a, b) if (is.null(a)) b else a

# UGF en Lambert-93 (EPSG:2154, x/y en metres), polygones valides.
.froid_units <- function(n = 2, dx = 0) {
  polys <- lapply(seq_len(n), function(i) {
    sf::st_buffer(sf::st_point(c(900000 + dx + i * 500, 6600000)), 50)
  })
  sf::st_sf(ug_id = as.character(seq_len(n)),
            geometry = sf::st_sfc(polys, crs = 2154))
}

.froid_core <- function(units, ...) {
  units$tmin_decennale <- rep(-13.2, nrow(units))
  units$tmin_decennale_n_annees <- rep(30L, nrow(units))
  units
}

test_that("Point de terrain : les UGF de test tombent bien en Lambert-93", {
  u <- .froid_units(2)
  expect_identical(sf::st_crs(u)$epsg, 2154L)
  # Couchey (Cote-d'Or) ~ x 850 000, y 6 750 000 ; ici un point du plateau
  # franc-comtois : en WGS84 (lon, lat) il doit tomber en France metropolitaine.
  ll <- sf::st_coordinates(sf::st_transform(sf::st_centroid(u[1, ]), 4326))
  expect_true(ll[1, "X"] > -5 && ll[1, "X"] < 10)   # longitude
  expect_true(ll[1, "Y"] > 41 && ll[1, "Y"] < 51.5) # latitude
})

test_that("run_regeneration_tmin : coeur appele UNE fois, puis lu en cache", {
  skip_if_not_installed("sf")
  pp <- withr::local_tempdir()
  u <- .froid_units(2)
  n_appels <- 0L
  testthat::local_mocked_bindings(
    regen_tmin_decennale = function(units, ...) { n_appels <<- n_appels + 1L; .froid_core(units) },
    .package = "nemeton")

  r1 <- nemetonshiny:::run_regeneration_tmin(u, pp)
  expect_identical(r1$status, "calculated")
  expect_equal(r1$units$tmin_decennale, c(-13.2, -13.2))
  expect_true(file.exists(file.path(pp, "cache", "regeneration", "tmin_decennale.csv")))

  r2 <- nemetonshiny:::run_regeneration_tmin(u, pp)
  expect_identical(r2$status, "cached")
  expect_equal(r2$units$tmin_decennale, c(-13.2, -13.2))
  expect_identical(n_appels, 1L)   # le 2e passage n'appelle PAS le coeur

  # UGF redessinees (autre position, meme ug_id) : le cache ne vaut plus.
  r3 <- nemetonshiny:::run_regeneration_tmin(.froid_units(2, dx = 5000), pp)
  expect_identical(r3$status, "calculated")
  expect_identical(n_appels, 2L)
})

test_that("run_regeneration_tmin : echec ou E-OBS absent -> colonne absente, jamais d'erreur", {
  skip_if_not_installed("sf")
  pp <- withr::local_tempdir()
  u <- .froid_units(2)

  testthat::local_mocked_bindings(
    regen_tmin_decennale = function(units, ...) stop("pas de reseau"),
    .package = "nemeton")
  r <- NULL
  expect_warning(r <- nemetonshiny:::run_regeneration_tmin(u, pp), "tmin_decennale")
  expect_identical(r$status, "skipped")
  expect_false("tmin_decennale" %in% names(r$units))
  expect_false(file.exists(file.path(pp, "cache", "regeneration", "tmin_decennale.csv")))

  # Le coeur degrade (E-OBS indisponible) rend NA : meme repli, rien de cache.
  testthat::local_mocked_bindings(
    regen_tmin_decennale = function(units, ...) {
      units$tmin_decennale <- NA_real_; units$tmin_decennale_n_annees <- 0L; units
    }, .package = "nemeton")
  expect_warning(r <- nemetonshiny:::run_regeneration_tmin(u, pp), "unavailable")
  expect_identical(r$status, "skipped")
  expect_false("tmin_decennale" %in% names(r$units))
})

test_that("resultat partiel : UGF finies gardees et cachees, UGF NA essayees sans relance", {
  skip_if_not_installed("sf")
  pp <- withr::local_tempdir()
  u <- .froid_units(3)
  n_appels <- 0L
  testthat::local_mocked_bindings(
    regen_tmin_decennale = function(units, ...) {
      n_appels <<- n_appels + 1L
      units$tmin_decennale <- c(-13.2, NA, -9)
      units$tmin_decennale_n_annees <- c(30L, 0L, 29L)
      units
    }, .package = "nemeton")
  r <- NULL
  expect_warning(r <- nemetonshiny:::run_regeneration_tmin(u, pp), "without value")
  expect_identical(r$status, "calculated")
  expect_equal(r$units$tmin_decennale, c(-13.2, NA, -9))
  r2 <- nemetonshiny:::run_regeneration_tmin(u, pp)
  expect_identical(r2$status, "cached")           # l'UGF NA n'est pas relancee
  expect_equal(r2$units$tmin_decennale, c(-13.2, NA, -9))
  expect_identical(n_appels, 1L)
})

test_that("cle de cache : version du coeur, parametres et signature NA invalident", {
  skip_if_not_installed("sf")
  pp <- withr::local_tempdir()
  u <- .froid_units(2)
  testthat::local_mocked_bindings(
    regen_tmin_decennale = function(units, ...) .froid_core(units),
    .package = "nemeton")
  nemetonshiny:::run_regeneration_tmin(u, pp)
  expect_true("tmin_decennale" %in% names(nemetonshiny:::.regen_tmin_from_cache(u, pp)))
  # Cle differente (autre coeur / autre parametre) : cache ignore.
  testthat::local_mocked_bindings(.regen_tmin_params_key = function() "autre",
                                  .package = "nemetonshiny")
  expect_false("tmin_decennale" %in% names(nemetonshiny:::.regen_tmin_from_cache(u, pp)))
})

test_that("cle de cache : embarque version du coeur et parametres par defaut du coeur", {
  skip_if_not_installed("sf")
  testthat::skip_if_not(exists("regen_tmin_decennale", envir = asNamespace("nemeton")))
  cle <- nemetonshiny:::.regen_tmin_params_key()
  expect_match(cle, paste0("nemeton=", as.character(utils::packageVersion("nemeton"))), fixed = TRUE)
  expect_match(cle, "prob=0.1", fixed = TRUE)
  expect_match(cle, "years=1991:2020", fixed = TRUE)
})

test_that("signature de position NA : l'entree de cache n'est pas valable", {
  skip_if_not_installed("sf")
  pp <- withr::local_tempdir()
  u <- .froid_units(2)
  testthat::local_mocked_bindings(
    regen_tmin_decennale = function(units, ...) .froid_core(units),
    .package = "nemeton")
  nemetonshiny:::run_regeneration_tmin(u, pp)
  testthat::local_mocked_bindings(
    .regen_tmin_signature = function(units) rep(NA_character_, nrow(units)),
    .package = "nemetonshiny")
  expect_false("tmin_decennale" %in% names(nemetonshiny:::.regen_tmin_from_cache(u, pp)))
})

test_that(".regen_attach_tmin et .regen_tmin_from_cache : jointure par ug_id", {
  skip_if_not_installed("sf")
  u <- .froid_units(3)
  src <- data.frame(ug_id = c("3", "1"), tmin_decennale = c(-9, -13.2),
                    tmin_decennale_n_annees = c(30L, 30L))
  out <- nemetonshiny:::.regen_attach_tmin(u, src)
  expect_equal(out$tmin_decennale, c(-13.2, NA, -9))
  # Sans froid a reporter : inchange.
  expect_identical(names(nemetonshiny:::.regen_attach_tmin(u, data.frame(ug_id = "1"))),
                   names(u))
  expect_false(nemetonshiny:::.regen_tmin_essaye(u))
  expect_true(nemetonshiny:::.regen_tmin_essaye(out))   # meme avec des NA "essayes"
  # Pas de cache (ou pas de projet) : inchange.
  expect_identical(nemetonshiny:::.regen_tmin_from_cache(u, NULL), u)
  expect_identical(nemetonshiny:::.regen_tmin_from_cache(u, withr::local_tempdir()), u)
})

test_that("regeneration_species_ranking : tmin_decennale lu par le coeur, exclus_froid conserve", {
  skip_if_not_installed("sf")
  u <- .froid_core(.froid_units(2))
  vu <- new.env()
  testthat::local_mocked_bindings(
    regen_rank_species = function(units, ...) {
      vu$cols <- names(units)
      r <- data.frame(ug_id = c("1", "2"), rank = 1L, species_code = "a", label = "A",
                      suitability = 90, limiting_factor = "gel", confidence = "eleve")
      attr(r, "exclus_froid") <- data.frame(ug_id = c("1", "1", "2"),
        species_code = c("pinus_halepensis", "pinus_brutia", "pinus_brutia"))
      r
    }, .package = "nemeton")
  out <- nemetonshiny:::regeneration_species_ranking(u)
  expect_true("tmin_decennale" %in% vu$cols)       # la colonne arrive au coeur
  expect_identical(nemetonshiny:::.regen_n_exclus_froid(out, "1"), 2L)
  expect_identical(nemetonshiny:::.regen_n_exclus_froid(out, "2"), 1L)
  expect_identical(nemetonshiny:::.regen_n_exclus_froid(out, "9"), 0L)
  expect_identical(nemetonshiny:::.regen_n_exclus_froid(NULL, "1"), 0L)
})

test_that("regen_rank_species reel : tmin_decennale = -13,2 ecarte le Pin d'Alep", {
  skip_if_not_installed("sf")
  testthat::skip_if_not(exists("regen_tmin_decennale", envir = asNamespace("nemeton")),
                         "coeur sans regen_tmin_decennale (< 2026.11.32)")
  u <- .froid_units(2)
  u$tmax_moyenne <- 30; u$d_tmax <- 0; u$vpd_canicule <- 2; u$rew_min <- 0.3
  u$r7_gel_days <- 0
  u$tmin_decennale <- c(-13.2, -3)
  r <- nemeton::regen_rank_species(u, top_n = 3)
  # UGF 1 (hiver decennal -13,2) : plus de Pin d'Alep, il est dans exclus_froid.
  expect_false("pinus_halepensis" %in% r$species_code[r$ug_id == "1"])
  ex <- attr(r, "exclus_froid")
  expect_true("pinus_halepensis" %in% ex$species_code[ex$ug_id == "1"])
  # UGF 2 (hiver doux, -3) : le Pin d'Alep reste classe.
  expect_true("pinus_halepensis" %in% r$species_code[r$ug_id == "2"])
  expect_identical(nemetonshiny:::.regen_n_exclus_froid(r, "2"), 0L)

  # Toutes les essences ecartees : ligne rank = NA, facteur froid_hiver.
  r1 <- nemeton::regen_rank_species(u[1, ], species_pool = "pinus_halepensis")
  expect_true(is.na(r1$rank))
  expect_identical(r1$limiting_factor, "froid_hiver")
})

test_that(".regen_species_ranking_ui : froid_hiver traduit fr/en, ligne rank = NA sans erreur", {
  rk <- data.frame(ug_id = c("U1", "U2"), rank = c(NA_integer_, 1L),
    species_code = c(NA, "a"), label = c(NA, "Chêne"),
    suitability = c(NA, 90), limiting_factor = c("froid_hiver", "gel"),
    confidence = c(NA, "eleve"), stringsAsFactors = FALSE)
  attr(rk, "exclus_froid") <- data.frame(ug_id = c("U1", "U1", "U2"),
    species_code = c("x", "y", "z"))

  fr <- nemetonshiny:::get_i18n("fr"); en <- nemetonshiny:::get_i18n("en")
  html <- as.character(nemetonshiny:::.regen_species_ranking_ui(rk, "U1", fr, tmin = -13.2))
  expect_false(grepl("NA", html))
  expect_true(grepl("Aucune essence ne tient le froid", html))
  expect_true(grepl("hiver décennal : −13,2 °C", html, fixed = TRUE))
  expect_true(grepl("essences écartées pour le froid : 2", html, fixed = TRUE))

  html_en <- as.character(nemetonshiny:::.regen_species_ranking_ui(rk, "U1", en, tmin = -13.2))
  expect_true(grepl("ten-year winter: −13.2 °C", html_en, fixed = TRUE))
  expect_true(grepl("species excluded for cold: 2", html_en, fixed = TRUE))

  # Traduction du facteur dans les deux langues.
  expect_identical(nemetonshiny:::.regen_lf_label("froid_hiver", fr), "froid d'hiver")
  expect_identical(nemetonshiny:::.regen_lf_label("froid_hiver", en), "winter cold")
  expect_identical(nemetonshiny:::.regen_lf_label("gel", fr), "gel")

  # Une UGF classee normalement affiche son facteur, sans message "tout exclu".
  h2 <- as.character(nemetonshiny:::.regen_species_ranking_ui(rk, "U2", fr))
  expect_false(grepl("Aucune essence ne tient", h2))
  expect_true(grepl("Chêne", h2))
  expect_true(grepl("essences écartées pour le froid : 1", h2, fixed = TRUE))
  # Sans froid ni exclusion : pas de ligne de froid.
  rk0 <- rk; attr(rk0, "exclus_froid") <- NULL
  expect_false(grepl("hiver décennal", as.character(
    nemetonshiny:::.regen_species_ranking_ui(rk0, "U2", fr)), fixed = TRUE))
})

test_that("export PDF : ligne rank = NA sans erreur ni NA/100, froid dans les conditions", {
  skip_if_not_installed("sf")
  u <- .froid_core(.froid_units(2))
  rk <- data.frame(ug_id = c("1", "2"), rank = c(NA_integer_, 1L),
    species_code = c(NA, "a"), label = c(NA, "Chêne"),
    suitability = c(NA, 90), limiting_factor = c("froid_hiver", "gel"),
    confidence = c(NA, "eleve"), stringsAsFactors = FALSE)
  d <- nemetonshiny:::.build_regeneration_pdf_data(
    list(id = "p"), u, ranking = rk, language = "fr")
  sp1 <- d$ugfs[[1]]$species[[1]]
  expect_identical(sp1$suitability, "--")
  expect_identical(sp1$limiting, "froid d'hiver")
  expect_match(sp1$label, "Aucune essence ne tient le froid")
  expect_false(any(grepl("NA", unlist(sp1))))
  expect_identical(d$ugfs[[2]]$species[[1]]$suitability, "90/100")
  lab <- vapply(d$ugfs[[1]]$conditions, function(x) x$label, "")
  expect_true("Hiver décennal (°C)" %in% lab)
  d_en <- nemetonshiny:::.build_regeneration_pdf_data(
    list(id = "p"), u, ranking = rk, language = "en")
  expect_identical(d_en$ugfs[[1]]$species[[1]]$limiting, "winter cold")
})

test_that("prompt IA : ligne rank = NA sans erreur ni NA, hiver decennal en condition", {
  rk <- data.frame(ug_id = c("1", "2"), rank = c(NA_integer_, 1L),
    species_code = c(NA, "a"), label = c(NA, "Chêne"),
    suitability = c(NA, 90), limiting_factor = c("froid_hiver", "gel"),
    confidence = c(NA, "eleve"), stringsAsFactors = FALSE)
  st <- data.frame(ug_id = c("1", "2"), tmin_decennale = c(-13.2, -3))
  p <- nemetonshiny:::build_regen_advice_prompt(rk, st, "français")
  expect_false(grepl("NA", p))
  expect_true(grepl("aucune essence ne tient le froid", p, fixed = TRUE))
  expect_true(grepl("hiver décennal=-13.2", p, fixed = TRUE))
  expect_true(grepl("1. Chêne (90/100, gel)", p, fixed = TRUE))
  pe <- nemetonshiny:::build_regen_advice_prompt(rk, st, "English")
  expect_true(grepl("no species withstands", pe, fixed = TRUE))
})

test_that("les cles i18n du froid existent en fr et en, <= 30 caracteres", {
  cles <- c("regen_limit_froid_hiver", "regen_col_tmin", "regen_tmin_decennale",
            "regen_froid_ecartees", "regen_froid_tout_exclu", "regen_tmin_running",
            "regen_tmin_done", "regen_tmin_skipped")
  for (k in cles) {
    expect_lte(nchar(k), 30L)
    expect_true(!is.null(nemetonshiny:::TRANSLATIONS[[k]]$fr), info = k)
    expect_true(!is.null(nemetonshiny:::TRANSLATIONS[[k]]$en), info = k)
  }
})

# ---------------------------------------------------------------------------
# Module : la tache de fond part apres l'analyse, une seule fois, et le froid
# arrive dans rv$result sans bloquer.
# ---------------------------------------------------------------------------

.froid_attendre <- function(session, n = 40L) {
  for (i in seq_len(n)) { later::run_now(0.05); session$flushReact() }
}

test_that("module : apres l'analyse, tmin_task calcule le froid (1 appel) et le pose sur rv$result", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  withr::local_options(nemetonshiny.analyse_inline = TRUE,
                       nemetonshiny.tmin_inline = TRUE)
  units <- .froid_units(2)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj)
  n_appels <- 0L
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    run_regeneration = function(u, cfg = list(), precomputed = NULL, progress = NULL) {
      u$indice_priorite_regen <- 70
      list(units = u, years = list(year_moyenne = 2018, year_canicule = 2022),
           warnings = character(0))
    }, .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    regen_tmin_decennale = function(units, ...) { n_appels <<- n_appels + 1L; .froid_core(units) },
    .package = "nemeton")

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(map_layer = "indice_priorite_regen", forest_type = "feuillu",
        year_moyenne = NA, year_canicule = NA, lai_max = NA, species = "")
      session$setInputs(run = 1)
      .froid_attendre(session)
      expect_false(is.null(rv$result))
      expect_equal(rv$result$tmin_decennale, c(-13.2, -13.2))
      expect_identical(n_appels, 1L)

      # Relance de l'analyse : le cache projet sert, le coeur n'est PAS rappele.
      session$setInputs(run = 2)
      .froid_attendre(session)
      expect_equal(rv$result$tmin_decennale, c(-13.2, -13.2))
      expect_identical(n_appels, 1L)
    })
})

test_that("module : E-OBS en echec -> analyse intacte, pas d'erreur, pas de colonne", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  withr::local_options(nemetonshiny.analyse_inline = TRUE,
                       nemetonshiny.tmin_inline = TRUE)
  units <- .froid_units(2)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    run_regeneration = function(u, cfg = list(), precomputed = NULL, progress = NULL) {
      u$indice_priorite_regen <- 70
      list(units = u, years = list(), warnings = character(0))
    }, .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    regen_tmin_decennale = function(units, ...) stop("KNMI injoignable"),
    .package = "nemeton")

  suppressWarnings(shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(map_layer = "indice_priorite_regen", forest_type = "feuillu",
        year_moyenne = NA, year_canicule = NA, lai_max = NA, species = "")
      session$setInputs(run = 1)
      .froid_attendre(session)
      expect_false(is.null(rv$result))
      expect_false("tmin_decennale" %in% names(rv$result))
      expect_equal(unique(rv$result$indice_priorite_regen), 70)
    }))
})

test_that("module : en test d'analyse en ligne, le froid n'est jamais lance sans l'option dediee", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  withr::local_options(nemetonshiny.analyse_inline = TRUE, nemetonshiny.tmin_inline = NULL)
  units <- .froid_units(2)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj)
  n_appels <- 0L
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    run_regeneration = function(u, cfg = list(), precomputed = NULL, progress = NULL) {
      u$indice_priorite_regen <- 70
      list(units = u, years = list(), warnings = character(0))
    }, .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    regen_tmin_decennale = function(units, ...) { n_appels <<- n_appels + 1L; .froid_core(units) },
    .package = "nemeton")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(map_layer = "indice_priorite_regen", forest_type = "feuillu",
      year_moyenne = NA, year_canicule = NA, lai_max = NA, species = "")
    session$setInputs(run = 1)
    .froid_attendre(session)
    expect_identical(n_appels, 0L)
  })
})

test_that("module : projet change pendant la tache -> resultat NON joint au nouveau projet", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  withr::local_options(nemetonshiny.analyse_inline = TRUE,
                       nemetonshiny.tmin_inline = TRUE)
  units <- .froid_units(2)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj, project_id = "p1")
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    run_regeneration = function(u, cfg = list(), precomputed = NULL, progress = NULL) {
      u$indice_priorite_regen <- 70
      list(units = u, years = list(), warnings = character(0))
    }, .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    regen_tmin_decennale = function(units, ...) .froid_core(units),
    .package = "nemeton")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(map_layer = "indice_priorite_regen", forest_type = "feuillu",
      year_moyenne = NA, year_canicule = NA, lai_max = NA, species = "")
    session$setInputs(run = 1)
    # L'analyse (inline) se termine et lance la tache froid ; on change de projet
    # AVANT que la tache ne se resolve.
    for (i in 1:40) {
      later::run_now(0.02); session$flushReact()
      if (identical(tmin_task$status(), "running")) break
    }
    expect_identical(tmin_task$status(), "running")
    expect_false(is.null(rv$result))
    as$project_id <- "p2"
    .froid_attendre(session)
    expect_identical(tmin_task$status(), "success")   # la tache s'est bien terminee
    expect_false(is.null(rv$result))                  # rv$result existe encore
    expect_false("tmin_decennale" %in% names(rv$result))   # mais sans le froid de p1
    expect_true(isTRUE(rv$tmin_retry))                # a relancer pour le projet courant
  })
})

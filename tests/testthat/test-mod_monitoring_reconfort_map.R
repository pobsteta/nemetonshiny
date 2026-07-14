# Tests for mod_monitoring_reconfort_map.R (spec 021, L6).
#
# The module wires nemeton::check_reconfort_validity / read_reconfort_layer /
# read_reconfort_pixel_series + the app's get_monitoring_db_connection /
# get_monitoring_zone_aoi. All are mocked — no DB, no real run.
#
# v0.106.4 — the vector "Alertes" layer (nemeton::list_alerts markers) is
# GONE : RECONFORT is 100 % raster, like FAST and FORDEAD. The last test of
# this file guards that regression.

`%||%` <- function(a, b) if (is.null(a)) b else a

# renderUI outputs are list(html=, deps=) ; a NULL output (e.g. an absent
# overlay/banner) must coerce to "" so grepl() returns a length-1 FALSE
# rather than logical(0) (which would break expect_false()).
.html_of <- function(x) as.character(x$html %||% x %||% "")

.reconfort_fake_aoi <- function() {
  sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(5, 47), c(5.1, 47), c(5.1, 47.1), c(5, 47.1), c(5, 47)
      ))), crs = 4326
    )
  )
}

test_that("reconfort map shows the empty state when there is no run", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not(exists("check_reconfort_validity", where = asNamespace("nemeton"),
                     inherits = FALSE),
              "nemeton RECONFORT API not installed")

  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    get_monitoring_zone_aoi = function(con, zone_id) .reconfort_fake_aoi()
  )
  testthat::local_mocked_bindings(
    check_reconfort_validity = function(aoi, ...) list(geo_valid = TRUE,
                                                       species_valid = TRUE,
                                                       advisory = TRUE),
    .package = "nemeton"
  )

  app_state <- shiny::reactiveValues(
    language = "fr",
    current_project = list(id = "p", path = withr::local_tempdir())
  )

  shiny::testServer(
    nemetonshiny:::mod_monitoring_reconfort_map_server,
    args = list(id = "rc", app_state = app_state,
                zone_id_r = shiny::reactive("1")),
    {
      session$flushReact()
      i18n <- get_i18n("fr")
      overlay <- .html_of(output$overlay)
      # Empty-state overlay shown when no run (in memory or cached) exists.
      expect_true(grepl(i18n$t("monitoring_reconfort_map_empty_title"),
                        overlay, fixed = TRUE))
      # v0.94.x — le bandeau « hors domaine » est rendu au PARENT
      # (mod_monitoring::output$reconfort_validity_banner) ; le module se
      # contente d'exposer `validity`. geo_valid TRUE => pas de bandeau.
      v <- session$returned$validity()
      expect_true(isTRUE(v$geo_valid))
    }
  )
})

test_that("reconfort map exposes geo-invalid validity (parent renders the banner)", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not(exists("check_reconfort_validity", where = asNamespace("nemeton"),
                     inherits = FALSE),
              "nemeton RECONFORT API not installed")

  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    get_monitoring_zone_aoi = function(con, zone_id) .reconfort_fake_aoi()
  )
  testthat::local_mocked_bindings(
    check_reconfort_validity = function(aoi, ...) list(geo_valid = FALSE,
                                                       species_valid = TRUE,
                                                       advisory = TRUE),
    .package = "nemeton"
  )

  app_state <- shiny::reactiveValues(
    language = "fr",
    current_project = list(id = "p", path = withr::local_tempdir())
  )

  shiny::testServer(
    nemetonshiny:::mod_monitoring_reconfort_map_server,
    args = list(id = "rc", app_state = app_state,
                zone_id_r = shiny::reactive("1")),
    {
      session$flushReact()
      # v0.94.x — bandeau « hors domaine » rendu au PARENT ; on vérifie que le
      # module expose bien geo_valid == FALSE (le parent en tire le bandeau).
      # La validité est exposée même sans run : elle ne dépend que de l'AOI.
      v <- session$returned$validity()
      expect_true(isFALSE(v$geo_valid))
    }
  )
})

# `prepared` factice minimal (structure Partie A) pour mocker le cœur dans le
# test du module : plot_pixel_dieback est testée à part (test-fct_plot_...).
.reconfort_fake_prepared <- function() {
  d  <- as.Date("2023-01-01") + seq(0, 500, 20)
  yr <- as.integer(format(d, "%Y")); dy <- as.integer(format(d, "%j"))
  list(
    grid_swir = data.frame(date = d, val = 0.4, year = yr, doy = dy),
    grid_re   = data.frame(date = d, val = 0.6, year = yr, doy = dy),
    obs_swir  = data.frame(date = d, val = 0.4, year = yr, doy = dy),
    obs_re    = data.frame(date = d, val = 0.6, year = yr, doy = dy),
    trough_swir = data.frame(year = 2023L, date = d[8], val = 0.35),
    peak_re     = data.frame(year = 2023L, date = d[8], val = 0.65),
    state     = data.frame(date = d[6:10], year = 2023L,
                           val_sw = 0.4, val_re = 0.6),
    centroids = data.frame(year = 2023L, val_sw = 0.4, val_re = 0.6),
    gaps      = data.frame(from = as.Date(character(0)),
                           to   = as.Date(character(0)))
  )
}

test_that("reconfort map click produces the 4-panel pixel dieback plate + DT table", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not_installed("plotly")
  skip_if_not_installed("DT")
  skip_if_not(exists("read_reconfort_pixel_series",
                     where = asNamespace("nemeton"), inherits = FALSE),
              "nemeton >= 0.80.0 (read_reconfort_pixel_series) not installed")
  skip_if_not(exists("prepare_pixel_dieback_series",
                     where = asNamespace("nemeton"), inherits = FALSE),
              "nemeton >= 0.106.0 (prepare_pixel_dieback_series) not installed")

  proj_dir <- withr::local_tempdir()
  dir.create(file.path(proj_dir, "cache", "layers", "reconfort"),
             recursive = TRUE, showWarnings = FALSE)

  series <- data.frame(
    obs_date   = as.Date(c("2023-05-01", "2023-06-01", "2023-07-01")),
    crswir_obs = c(0.20, 0.25, 0.31),
    crre_obs   = c(0.55, 0.50, 0.42),
    stringsAsFactors = FALSE
  )
  attr(series, "species") <- "CHE"
  attr(series, "v_model") <- "v3"

  smooth_seen <- new.env(parent = emptyenv()); smooth_seen$vals <- character(0)

  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    get_monitoring_zone_aoi = function(con, zone_id) .reconfort_fake_aoi()
  )
  testthat::local_mocked_bindings(
    check_reconfort_validity = function(aoi, ...) list(geo_valid = TRUE, advisory = TRUE),
    read_reconfort_pixel_series = function(con, zone_id, xy, crs = 4326,
                                           run_id = NULL, cache_dir) series,
    # Le module délègue TOUT le dérivé au cœur : on mocke prepare_* pour
    # rester déterministe et vérifier que le `smooth` du toggle est propagé.
    prepare_pixel_dieback_series = function(df, smooth = "light", ...) {
      smooth_seen$vals <- c(smooth_seen$vals, smooth)
      .reconfort_fake_prepared()
    },
    .package = "nemeton"
  )

  app_state <- shiny::reactiveValues(
    language = "fr",
    current_project = list(id = "p", path = proj_dir)
  )

  shiny::testServer(
    nemetonshiny:::mod_monitoring_reconfort_map_server,
    args = list(id = "rc", app_state = app_state,
                zone_id_r = shiny::reactive("1")),
    {
      session$flushReact()
      session$setInputs(map_click = list(lat = 47.05, lng = 5.05))
      expect_no_error(session$flushReact())
      # La planche (plotly) et la table DT équivalente doivent rendre.
      expect_false(is.null(output$pixel_ts_plot))
      expect_false(is.null(output$pixel_ts_table))
      # Défaut : lissage "light" propagé au cœur.
      expect_true("light" %in% smooth_seen$vals)
      # Toggle lissage/points → re-render sans erreur, `smooth` propagé.
      session$setInputs(pixel_smooth = "none", pixel_points = FALSE)
      expect_no_error(session$flushReact())
      expect_false(is.null(output$pixel_ts_plot))
      expect_true("none" %in% smooth_seen$vals)
    }
  )
})

# ---- Manifest-driven layered display (toggles + opacity) -----------------

# Builds three small EPSG:2154 rasters on disk + a synthetic run `result`
# matching the contract of nemeton::run_reconfort_dieback() ($rasters).
.reconfort_fake_result <- function(dir) {
  base <- terra::rast(nrows = 8, ncols = 8,
                      xmin = 900000, xmax = 900080,
                      ymin = 6500000, ymax = 6500080,
                      crs = "EPSG:2154")
  score <- base; terra::values(score) <- seq(1, 100, length.out = 64)
  classif <- base; terra::values(classif) <- rep(1:3, length.out = 64)
  prob <- base; terra::values(prob) <- seq(0, 1000, length.out = 64)
  sp <- file.path(dir, "score.tif")
  cp <- file.path(dir, "classif.tif")
  pp <- file.path(dir, "prob.tif")
  terra::writeRaster(score,   sp, overwrite = TRUE)
  terra::writeRaster(classif, cp, overwrite = TRUE)
  terra::writeRaster(prob,    pp, overwrite = TRUE)
  list(
    status   = "ok",
    zone_id  = 1L,
    rasters  = list(continuous_score = sp, classif = cp, probability = pp),
    n_alerts = 0L
  )
}

test_that("manifest drives layer toggles + opacity slider", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("terra")
  skip_if_not_installed("leaflet")
  skip_if_not(exists("reconfort_layer_manifest",
                     where = asNamespace("nemeton"), inherits = FALSE),
              "nemeton reconfort_layer_manifest() not installed")
  skip_if_not(exists("read_reconfort_layer",
                     where = asNamespace("nemeton"), inherits = FALSE),
              "nemeton >= 0.98.0 (read_reconfort_layer) not installed")

  rdir <- withr::local_tempdir()
  fake_result <- .reconfort_fake_result(rdir)

  # EPSG:2154 AOI covering the fake rasters → passed as `mask_polygon` to
  # the core reader. Also used as the project UGF (indicators_sf) so the
  # UGF overlay + framing run.
  aoi_2154 <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(900000, 6500000), c(900080, 6500000), c(900080, 6500080),
      c(900000, 6500080), c(900000, 6500000)))), crs = 2154)
  )

  # Recorder : the module must delegate read + UGF mask to the core reader
  # (no terra::mask of its own). Capture each call's layer id + whether a
  # mask_polygon was supplied.
  rl_calls <- new.env()
  rl_calls$ids  <- character()
  rl_calls$mask <- logical()

  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    get_monitoring_zone_aoi = function(con, zone_id) aoi_2154
  )
  testthat::local_mocked_bindings(
    check_reconfort_validity = function(aoi, ...) list(geo_valid = TRUE, advisory = TRUE),
    read_reconfort_layer = function(layer, con = NULL, zone_id = NULL,
                                    apply_zone_mask = TRUE, mask_polygon = NULL) {
      rl_calls$ids  <- c(rl_calls$ids,  as.character(layer$id))
      rl_calls$mask <- c(rl_calls$mask, !is.null(mask_polygon))
      terra::rast(layer$path)
    },
    .package = "nemeton"
  )

  app_state <- shiny::reactiveValues(
    language = "fr",
    current_project = list(id = "p", path = withr::local_tempdir(),
                           indicators_sf = aoi_2154)
  )

  shiny::testServer(
    nemetonshiny:::mod_monitoring_reconfort_map_server,
    args = list(id = "rc", app_state = app_state,
                zone_id_r = shiny::reactive("1"),
                result_r  = shiny::reactive(fake_result)),
    {
      session$flushReact()
      i18n <- get_i18n("fr")
      controls <- .html_of(output$controls)
      overlay  <- .html_of(output$overlay)

      # Controls (right sidebar) : EXCLUSIVE layers (radio) + opacity slider.
      expect_true(grepl(i18n$t("reconfort_couches"), controls, fixed = TRUE))
      expect_true(grepl(i18n$t("reconfort_opacite"), controls, fixed = TRUE))
      expect_true(grepl("type=\"radio\"", controls, fixed = TRUE))   # exclusive
      expect_false(grepl("type=\"checkbox\"", controls, fixed = TRUE))
      # The three manifest raster labels appear as checkbox choices.
      expect_true(grepl(i18n$t("reconfort_couche_score"),   controls, fixed = TRUE))
      expect_true(grepl(i18n$t("reconfort_couche_classes"), controls, fixed = TRUE))
      expect_true(grepl(i18n$t("reconfort_couche_proba"),   controls, fixed = TRUE))
      # Each layer carries an info "i" tooltip (parity FORDEAD).
      expect_true(grepl("info-circle", controls, fixed = TRUE))
      expect_true(grepl(i18n$t("reconfort_couche_score_info"), controls, fixed = TRUE))
      # Empty-state overlay must NOT show when layers are available.
      expect_false(grepl(i18n$t("monitoring_reconfort_map_empty_title"),
                         overlay, fixed = TRUE))

      # Base leaflet map rendered.
      expect_false(is.null(output$map))

      # Read + UGF mask delegated to the core reader for every raster layer,
      # each with a mask_polygon (the selected-zone AOI). No terra::mask in
      # the module.
      expect_true(all(c("score", "classification", "probability") %in%
                        rl_calls$ids))
      expect_true(length(rl_calls$mask) > 0L && all(rl_calls$mask))

      # Toggling layers + moving the opacity slider re-renders without error.
      session$setInputs(layers = c("score", "probability"), opacity = 0.4)
      expect_no_error(session$flushReact())
      session$setInputs(layers = "classification", opacity = 0.9)
      expect_no_error(session$flushReact())
    }
  )
})

test_that("no run at all : empty state, no layer toggles, NO alerts layer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not(exists("check_reconfort_validity",
                     where = asNamespace("nemeton"), inherits = FALSE),
              "nemeton RECONFORT API not installed")

  # Garde-fou de non-régression (v0.106.4) : sans run (ni en mémoire, ni en
  # cache) la carte n'a RIEN à montrer. Avant ce bump, elle retombait sur une
  # couche vectorielle « Alertes » lue en base (marqueurs « placettes ») —
  # supprimée pour aligner RECONFORT sur FAST / FORDEAD (100 % raster).
  #
  # `list_alerts` est volontairement mocké pour LEVER : si le module la
  # rappelait, le test échouerait au lieu de passer silencieusement.
  called <- new.env(parent = emptyenv()); called$list_alerts <- FALSE

  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    get_monitoring_zone_aoi = function(con, zone_id) .reconfort_fake_aoi()
  )
  testthat::local_mocked_bindings(
    list_alerts = function(...) {
      # NE PAS asserter ici : l'appel serait enveloppé dans un tryCatch(error=)
      # côté module, qui avalerait l'échec d'expectation (test vacant).
      # Enregistrer, asserter après le testServer.
      called$list_alerts <- TRUE
      NULL
    },
    check_reconfort_validity = function(aoi, ...) list(geo_valid = TRUE, advisory = TRUE),
    .package = "nemeton"
  )

  app_state <- shiny::reactiveValues(
    language = "fr",
    current_project = list(id = "p", path = withr::local_tempdir())
  )

  shiny::testServer(
    nemetonshiny:::mod_monitoring_reconfort_map_server,
    args = list(id = "rc", app_state = app_state,
                zone_id_r = shiny::reactive("1"),
                result_r  = shiny::reactive(NULL)),
    {
      session$flushReact()
      i18n <- get_i18n("fr")
      controls <- .html_of(output$controls)
      overlay  <- .html_of(output$overlay)

      # Empty-state : aucun run → aucune couche.
      expect_true(grepl(i18n$t("monitoring_reconfort_map_empty_title"),
                        overlay, fixed = TRUE))
      expect_true(grepl(i18n$t("monitoring_reconfort_map_empty_body"),
                        controls, fixed = TRUE))
      expect_false(grepl(i18n$t("reconfort_couches"), controls, fixed = TRUE))
      expect_false(grepl(i18n$t("reconfort_opacite"), controls, fixed = TRUE))

      # Le module n'expose plus de réactive `alerts`.
      expect_null(session$returned$alerts)
      expect_false(is.null(session$returned$validity))
    }
  )

  # La couche vectorielle est bien morte : le module n'interroge plus la table
  # `alerts` de la base de suivi (celle-ci reste lue par service_r5.R).
  expect_false(called$list_alerts)
})

test_that("cache fallback shows persisted rasters after a project reload", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("terra")
  skip_if_not(exists("reconfort_cache_manifest",
                     where = asNamespace("nemeton"), inherits = FALSE),
              "nemeton >= 0.100.0 (reconfort_cache_manifest) not installed")

  # Project with a REAL reconfort cache dir (the module guards on
  # dir.exists) + a fake classification tif referenced by the manifest.
  pp <- withr::local_tempdir()
  cd <- file.path(pp, "cache", "layers", "reconfort", "zone_1")
  dir.create(cd, recursive = TRUE, showWarnings = FALSE)
  base <- terra::rast(nrows = 6, ncols = 6, xmin = 900000, xmax = 900060,
                      ymin = 6500000, ymax = 6500060, crs = "EPSG:2154")
  terra::values(base) <- rep(1:3, length.out = 36)
  tif <- file.path(cd, "reconfort_mask_20260629T055540.tif")
  terra::writeRaster(base, tif, overwrite = TRUE)

  cache_manifest <- data.frame(
    id = "classification", label_key = "reconfort_couche_classes",
    type = "raster", role = "classification", path = tif, categorical = TRUE,
    palette = NA_character_, reverse = FALSE, vmin = NA_real_, vmax = NA_real_,
    default_visible = TRUE, default_opacity = 0.8, n_features = NA_integer_,
    stringsAsFactors = FALSE
  )

  caz <- new.env(); caz$called <- FALSE

  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) NULL,
    close_monitoring_db_connection = function(con) invisible(TRUE)
  )
  testthat::local_mocked_bindings(
    reconfort_cache_manifest = function(cache_dir, zone_id, run_id = NULL,
                                        include_range = FALSE) {
      caz$called <- TRUE
      cache_manifest
    },
    read_reconfort_layer = function(layer, con = NULL, zone_id = NULL,
                                    apply_zone_mask = TRUE, mask_polygon = NULL) {
      terra::rast(layer$path)
    },
    .package = "nemeton"
  )

  app_state <- shiny::reactiveValues(
    language = "fr", current_project = list(id = "p", path = pp))

  shiny::testServer(
    nemetonshiny:::mod_monitoring_reconfort_map_server,
    args = list(id = "rc", app_state = app_state,
                zone_id_r = shiny::reactive("1"),
                result_r  = shiny::reactive(NULL)),  # no in-memory run
    {
      session$flushReact()
      i18n <- get_i18n("fr")
      controls <- .html_of(output$controls)
      # The persisted run was discovered from the cache (no result in memory)
      # → raster toggle + opacity slider present, just like FORDEAD.
      expect_true(caz$called)
      expect_true(grepl(i18n$t("reconfort_couche_classes"), controls, fixed = TRUE))
      expect_true(grepl(i18n$t("reconfort_opacite"), controls, fixed = TRUE))
      expect_false(is.null(output$map))
    }
  )
})

# ---- RECONFORT progress dispatcher (.reconfort_handle_progress_event) ----

test_that(".reconfort_handle_progress_event toasts on reconfort:phase", {
  skip_if_not_installed("shiny")
  captured <- list()
  i18n <- nemetonshiny:::get_i18n("fr")
  fake_session <- list(ns = function(id) paste0("monitoring-", id))

  testthat::with_mocked_bindings(
    showNotification = function(ui, id = NULL, ...) {
      captured$ui <<- ui; captured$id <<- id; captured$args <<- list(...)
      invisible(NULL)
    },
    .package = "shiny",
    {
      nemetonshiny:::.reconfort_handle_progress_event(
        ev = list(current = "reconfort:phase", phase_name = "ingest",
                  completed = 4L, total = 10L),
        session = fake_session, i18n = i18n
      )
    }
  )
  expect_false(is.null(captured$ui))
  rendered <- htmltools::renderTags(captured$ui)$html
  expect_match(rendered, "Phase 5/10", fixed = TRUE)
  expect_match(rendered, "Sentinel-2", fixed = TRUE)  # phase label
  expect_identical(captured$id, "monitoring-reconfort_progress")
  expect_null(captured$args$duration)
})

test_that(".reconfort_handle_progress_event is silent on reconfort:start", {
  skip_if_not_installed("shiny")
  n <- 0L
  fake_session <- list(ns = function(id) paste0("monitoring-", id))
  testthat::with_mocked_bindings(
    showNotification = function(...) { n <<- n + 1L; invisible(NULL) },
    .package = "shiny",
    {
      nemetonshiny:::.reconfort_handle_progress_event(
        ev = list(current = "reconfort:start", zone_id = 1L),
        session = fake_session, i18n = nemetonshiny:::get_i18n("fr")
      )
    }
  )
  expect_equal(n, 0L)
})

test_that(".reconfort_handle_progress_event surfaces reconfort:error", {
  skip_if_not_installed("shiny")
  captured <- list()
  fake_session <- list(ns = function(id) paste0("monitoring-", id))
  testthat::with_mocked_bindings(
    showNotification = function(ui, id = NULL, ...) {
      captured$ui <<- ui; captured$args <<- list(...); invisible(NULL)
    },
    removeNotification = function(...) invisible(NULL),
    .package = "shiny",
    {
      nemetonshiny:::.reconfort_handle_progress_event(
        ev = list(current = "reconfort:error", error_message = "conda missing"),
        session = fake_session, i18n = nemetonshiny:::get_i18n("fr")
      )
    }
  )
  expect_match(as.character(captured$ui), "conda missing", fixed = TRUE)
  expect_identical(captured$args$type, "error")
})

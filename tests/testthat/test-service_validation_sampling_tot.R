# Mission 017 : Plan de validation FAST calcule sur `_tot` (id 5), la strate
# (id 6) ne fait que borner le tirage et l'affichage. 100 % hors ligne : le
# coeur et la base sont moques.

.v017_aoi <- function(con, zone_id) {
  # Polygone propre a chaque zone, pour distinguer strate (6) et `_tot` (5).
  x0 <- if (identical(as.integer(zone_id), 6L)) 950011 else 950000
  x1 <- if (identical(as.integer(zone_id), 6L)) 950019 else 950020
  sf::st_sf(zone_id = as.integer(zone_id), geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(x0, 6620011), c(x1, 6620011), c(x1, 6620019),
                              c(x0, 6620019), c(x0, 6620011)))), crs = 2154))
}

# 2x2, EPSG:2154 : TL=0, TR=1, BL=2, BR=4. La strate (6) ne couvre que TR.
.v017_mask <- function() {
  terra::rast(nrows = 2, ncols = 2, crs = "EPSG:2154",
              xmin = 950000, xmax = 950020, ymin = 6620000, ymax = 6620020,
              vals = c(0, 1, 2, 4))
}

.v017_project <- function(env = parent.frame()) {
  path <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(path, "cache", "layers", "sentinel2"), recursive = TRUE)
  list(id = "p", path = path, metadata = list(monitoring_zone_id = 6L),
       indicators_sf = NULL)
}

.v017_trend_plan <- function() {
  sf::st_sf(plot_id = "S01", type = "Sanitaire", alert_value = 0.03,
            index = "NDRE", source = "FAST_TREND", seed = 42L,
            geometry = sf::st_sfc(sf::st_point(c(950015, 6620015)), crs = 2154))
}

# ---- Chemin trend (celui de l'interface) -----------------------------------

test_that("trend plan: core gets zone_id = _tot and mask_polygon = stratum AOI", {
  skip_if_not_installed("sf")
  seen <- new.env()
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .v017_aoi)
  testthat::local_mocked_bindings(
    create_trend_sanitary_plan = function(con, zone_id, ..., mask_polygon = NULL) {
      seen$zone_id <- zone_id; seen$mask <- mask_polygon
      .v017_trend_plan()
    },
    .package = "nemeton"
  )
  plan <- nemetonshiny:::generate_trend_sanitary_plan(
    "con", .v017_project(), zone_id = 6L, run_zone_id = 5L)
  expect_identical(seen$zone_id, 5L)                       # _tot, pas la strate
  expect_equal(sf::st_bbox(seen$mask), sf::st_bbox(.v017_aoi(NULL, 6L)))
  expect_identical(plan$zone_id, 6L)                       # le plan reste a la strate
})

test_that("trend plan: stratum = calculation zone passes no mask_polygon", {
  seen <- new.env(); seen$mask <- "sentinelle"
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .v017_aoi)
  testthat::local_mocked_bindings(
    create_trend_sanitary_plan = function(con, zone_id, ..., mask_polygon = NULL) {
      seen$zone_id <- zone_id; seen$mask <- mask_polygon
      .v017_trend_plan()
    },
    .package = "nemeton"
  )
  nemetonshiny:::generate_trend_sanitary_plan(
    "con", .v017_project(), zone_id = 5L, run_zone_id = 5L)
  expect_identical(seen$zone_id, 5L)
  expect_null(seen$mask)
})

test_that("trend plan: unresolved stratum polygon aborts, core not called", {
  called <- FALSE
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = function(con, zone_id) NULL)
  testthat::local_mocked_bindings(
    create_trend_sanitary_plan = function(...) { called <<- TRUE; .v017_trend_plan() },
    .package = "nemeton"
  )
  expect_error(
    nemetonshiny:::generate_trend_sanitary_plan(
      "con", .v017_project(), zone_id = 6L, run_zone_id = 5L),
    class = "validation_no_zone_geom")
  expect_false(called)
})

# ---- Chemin classes (generate_validation_plan, source = "FAST") ------------

test_that("classes plan: mask read on _tot, draw polygon stays the stratum's", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  proj <- .v017_project()
  dir.create(file.path(proj$path, "cache", "layers", "fast_sampling", "zone_5"),
             recursive = TRUE)
  file.create(file.path(proj$path, "cache", "layers", "fast_sampling", "zone_5",
                        "fast_alert_20260524T080000.tif"))
  seen <- new.env(); seen$read <- integer(0); seen$compute <- 0L
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .v017_aoi)
  testthat::local_mocked_bindings(
    read_fast_alert_mask = function(con, zone_id, ...) {
      seen$read <- c(seen$read, zone_id); .v017_mask()
    },
    compute_fast_alert_mask = function(...) { seen$compute <- seen$compute + 1L; "x" },
    create_validation_sampling_plan = function(zone, alert_raster, ...) {
      seen$zone <- zone
      p <- .v017_trend_plan(); p$alert_class <- 4L; p
    },
    .package = "nemeton"
  )
  plan <- nemetonshiny:::generate_validation_plan(
    "con", proj, source = "FAST", zone_id = 6L, run_zone_id = 5L,
    n_validation = 1L, n_control = 0L)
  expect_identical(seen$read, 5L)                           # _tot
  expect_equal(seen$compute, 0L)
  expect_equal(sf::st_bbox(seen$zone), sf::st_bbox(.v017_aoi(NULL, 6L)))
  expect_identical(plan$zone_id, 6L)
  expect_identical(plan$source_run_id, "20260524T080000")   # dossier zone_5
})

test_that("classes plan: cache miss computes on _tot as well", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  proj <- .v017_project()
  seen <- new.env(); seen$compute <- integer(0); seen$n_read <- 0L
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .v017_aoi)
  testthat::local_mocked_bindings(
    read_fast_alert_mask = function(con, zone_id, ...) {
      seen$n_read <- seen$n_read + 1L
      seen$read <- c(seen$read, zone_id)
      if (seen$n_read == 1L) NULL else .v017_mask()   # 1er appel : cache vide
    },
    compute_fast_alert_mask = function(con, zone_id, ...) {
      seen$compute <- c(seen$compute, zone_id); "x"
    },
    create_validation_sampling_plan = function(zone, alert_raster, ...) {
      p <- .v017_trend_plan(); p$alert_class <- 4L; p
    },
    .package = "nemeton"
  )
  nemetonshiny:::generate_validation_plan(
    "con", proj, source = "FAST", zone_id = 6L, run_zone_id = 5L,
    n_validation = 1L, n_control = 0L,
    ndvi_threshold = 0.4, nbr_threshold = 0.3,
    date_from = as.Date("2024-04-01"), date_to = as.Date("2024-09-30"))
  expect_identical(seen$compute, 5L)
  expect_identical(seen$read, c(5L, 5L))
})

# ---- Lectures du masque FAST cote module (histogramme, surcouche) ----------

test_that("FAST mask read for the stratum counts only the stratum's pixels", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .v017_aoi)
  testthat::local_mocked_bindings(
    read_fast_alert_mask = function(con, zone_id, ...) {
      seen$read <- c(seen$read, zone_id); .v017_mask()
    },
    .package = "nemeton"
  )
  r <- nemetonshiny:::.read_fast_mask_stratum("con", 6L, 5L, tempdir())
  expect_identical(seen$read, 5L)                           # lu sur _tot
  dist <- nemetonshiny:::.validation_class_distribution(r)
  # Seul TR (classe 1) est dans la strate ; TL=0, BL=2, BR=4 sont exclus.
  expect_identical(unname(dist), c(0L, 1L, 0L, 0L, 0L))

  # Strate = zone du calcul : histogramme complet, aucune decoupe.
  full <- nemetonshiny:::.read_fast_mask_stratum("con", 5L, 5L, tempdir())
  expect_identical(
    unname(nemetonshiny:::.validation_class_distribution(full)),
    c(1L, 1L, 1L, 0L, 1L))
})

test_that("FAST mask read returns NULL when no mask is cached", {
  testthat::local_mocked_bindings(
    read_fast_alert_mask = function(...) NULL, .package = "nemeton")
  expect_null(nemetonshiny:::.read_fast_mask_stratum("con", 6L, 5L, tempdir()))
})

# ---- Cablage du module : le Plan FAST recoit la zone du calcul -------------

test_that("FAST validation sub-tab forwards the _tot zone as run_zone_id", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")
  seen <- new.env()
  testthat::local_mocked_bindings(
    get_monitoring_db_connection   = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    generate_trend_sanitary_plan = function(con, project, zone_id = NULL, ...,
                                            run_zone_id = NULL) {
      seen$zone_id <- zone_id; seen$run_zone_id <- run_zone_id
      p <- .v017_trend_plan(); p$zone_id <- zone_id; p
    }
  )
  app_state <- shiny::reactiveValues(
    language = "fr", active_main_tab = "monitoring",
    current_project = list(id = "p", path = withr::local_tempdir(),
                           metadata = list(monitoring_zone_id = 6L)))
  shiny::testServer(
    nemetonshiny:::mod_validation_sampling_server,
    args = list(id = "ft", app_state = app_state,
                zone_id_r     = shiny::reactive("6"),
                run_zone_id_r = shiny::reactive("5"),
                mode_r        = shiny::reactive("quick"),
                thresholds_r  = shiny::reactive(list(index = "NDRE")),
                date_range_r  = shiny::reactive(NULL),
                source_fixed  = "FAST"),
    {
      session$setInputs(trend_index = "NDRE",
                        trend_win = c(as.Date("2017-01-01"), as.Date("2025-12-31")),
                        n_validation = 20L, n_control = 5L, seed = 42L)
      session$flushReact()
      session$setInputs(generate = 1L)
      session$flushReact()
      expect_identical(seen$zone_id, 6L)         # la strate borne le tirage
      expect_identical(seen$run_zone_id, "5")    # la zone du calcul
      expect_s3_class(session$returned$plan(), "sf")
    }
  )
})

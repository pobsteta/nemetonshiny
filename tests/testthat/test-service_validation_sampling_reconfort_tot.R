# Mission 021 : Plan de validation RECONFORT lu sur `_tot` (id 5), la strate
# (id 6) ne fait que borner le tirage et l'affichage. Modele : mission 019
# (test-service_validation_sampling_fordead_tot.R). 100 % hors ligne : le
# coeur et la base sont moques. NB : aucun expect_* dans un mock (les lecteurs
# sont enveloppes dans tryCatch) ; on capture dans un environnement.

.m21_aoi <- function(con, zone_id) {
  # La strate (6) ne couvre que la maille TR du raster ci-dessous.
  x0 <- if (identical(as.integer(zone_id), 6L)) 950011 else 950000
  x1 <- if (identical(as.integer(zone_id), 6L)) 950019 else 950020
  sf::st_sf(zone_id = as.integer(zone_id), geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(x0, 6620011), c(x1, 6620011), c(x1, 6620019),
                              c(x0, 6620019), c(x0, 6620011)))), crs = 2154))
}

# 2x2, EPSG:2154 : TL=3, TR=1 (saine), BL=2, BR=3.
.m21_mask <- function() {
  terra::rast(nrows = 2, ncols = 2, crs = "EPSG:2154",
              xmin = 950000, xmax = 950020, ymin = 6620000, ymax = 6620020,
              vals = c(3, 1, 2, 3))
}

# Projet avec caches RECONFORT sous les zones demandees (fichier de provenance).
.m21_project <- function(zones_with_cache, env = parent.frame()) {
  path <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(path, "cache", "layers", "reconfort"), recursive = TRUE)
  for (z in zones_with_cache) {
    d <- file.path(path, "cache", "layers", "reconfort", sprintf("zone_%d", z))
    dir.create(d, recursive = TRUE)
    file.create(file.path(d, sprintf("reconfort_mask_RUN%d.tif", z)))
  }
  list(id = "p", path = path, metadata = list(monitoring_zone_id = 6L),
       indicators_sf = NULL)
}

.m21_zones <- function(with_tot = TRUE) {
  function(con, project_uuid) {
    if (with_tot) data.frame(id = c(5L, 6L), name = c("p_tot", "p_res"))
    else data.frame(id = 6L, name = "p_res")
  }
}

# Mock du lecteur coeur : `available` = zones ayant un masque.
.m21_reader <- function(seen, available) {
  function(con, zone_id, run_id = NULL, cache_dir) {
    seen$read <- c(seen$read, zone_id)
    if (zone_id %in% available) .m21_mask() else NULL
  }
}

.m21_gen <- function(proj, ...) {
  nemetonshiny:::generate_validation_plan(
    "con", proj, source = "RECONFORT", zone_id = 6L,
    n_validation = 1L, n_control = 0L, classes = c(2L, 3L),
    control_classes = 1L, ...)
}

.m21_mock_plan <- function(zone, alert_raster, ...) {
  sf::st_sf(plot_id = "V01", type = "Validation", alert_class = 3L,
            source = "RECONFORT",
            geometry = sf::st_sfc(sf::st_point(c(950015, 6620015)),
                                  crs = 2154))
}

# ---- Generation ------------------------------------------------------------

test_that("RECONFORT plan on a stratum reads the mask on _tot, provenance from _tot", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m21_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m21_zones(TRUE),
    read_reconfort_alert_mask = .m21_reader(seen, available = c(5L, 6L)),
    create_validation_sampling_plan = .m21_mock_plan,
    .package = "nemeton"
  )
  plan <- .m21_gen(.m21_project(c(5L, 6L)))
  expect_identical(seen$read, 5L)                       # lu sur _tot seul
  expect_identical(plan$zone_id, 6L)                    # le plan reste a la strate
  expect_identical(plan$source_run_id, "RUN5")          # provenance = _tot
})

test_that("RECONFORT fallback (a): no _tot zone in DB reads the stratum", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m21_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m21_zones(FALSE),
    read_reconfort_alert_mask = .m21_reader(seen, available = 6L),
    create_validation_sampling_plan = .m21_mock_plan,
    .package = "nemeton"
  )
  plan <- .m21_gen(.m21_project(6L))
  expect_identical(seen$read, 6L)
  expect_identical(plan$source_run_id, "RUN6")
})

test_that("RECONFORT fallback (b): _tot without mask falls back to the stratum", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m21_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m21_zones(TRUE),
    read_reconfort_alert_mask = .m21_reader(seen, available = 6L),
    create_validation_sampling_plan = .m21_mock_plan,
    .package = "nemeton"
  )
  plan <- .m21_gen(.m21_project(6L))
  expect_identical(seen$read, c(5L, 6L))               # _tot d'abord, puis strate
  expect_identical(plan$source_run_id, "RUN6")         # provenance = zone lue
})

test_that("RECONFORT plan with no mask anywhere aborts validation_no_mask", {
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m21_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m21_zones(TRUE),
    read_reconfort_alert_mask = function(...) NULL,
    .package = "nemeton"
  )
  expect_error(.m21_gen(.m21_project(integer(0))), class = "validation_no_mask")
})

test_that("RECONFORT generation hands the full _tot mask to the core, not a clipped one", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env()
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m21_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m21_zones(TRUE),
    read_reconfort_alert_mask = .m21_reader(new.env(), available = 5L),
    create_validation_sampling_plan = function(zone, alert_raster, ...) {
      seen$vals <- as.numeric(terra::values(alert_raster))
      seen$zone <- zone
      .m21_mock_plan(zone, alert_raster)
    },
    .package = "nemeton"
  )
  .m21_gen(.m21_project(5L))
  expect_equal(seen$vals, c(3, 1, 2, 3))                # pas de NA : le coeur borne
  expect_equal(sf::st_bbox(seen$zone)[["xmin"]], 950011)  # polygone = strate
})

# ---- Ponderation continue : score lu sur la zone du masque -----------------

.m21_weight_run <- function(proj, available, seen) {
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m21_aoi,
                                  .env = parent.frame())
  testthat::local_mocked_bindings(
    find_zones_by_project = .m21_zones(TRUE),
    read_reconfort_alert_mask = .m21_reader(new.env(), available = available),
    reconfort_cache_manifest = function(cache_dir, zone_id, run_id = NULL,
                                        include_range = FALSE) {
      seen$manifest_zone <- c(seen$manifest_zone, zone_id)
      data.frame(id = c("score", "classification"),
                 type = c("raster", "raster"), stringsAsFactors = FALSE)
    },
    read_reconfort_layer = function(layer, con = NULL, zone_id = NULL,
                                    apply_zone_mask = TRUE, mask_polygon = NULL) {
      seen$mask_polygon <- mask_polygon
      .m21_mask()
    },
    create_validation_sampling_plan = function(zone, alert_raster, ...,
                                               weighting = "uniform",
                                               weight_raster = NULL) {
      seen$weighting <- weighting
      p <- .m21_mock_plan(zone, alert_raster)
      p$alert_weight <- 1
      p
    },
    .package = "nemeton",
    .env = parent.frame()
  )
  .m21_gen(proj, weighting = "continuous")
}

test_that("RECONFORT continuous score is read on _tot, cut to the stratum polygon", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$manifest_zone <- integer(0)
  .m21_weight_run(.m21_project(c(5L, 6L)), available = c(5L, 6L), seen)
  expect_identical(seen$manifest_zone, 5L)
  expect_identical(seen$weighting, "continuous")
  # aoi = strate : le coeur decoupe le score a ce polygone.
  expect_equal(sf::st_bbox(seen$mask_polygon)[["xmin"]], 950011)
})

test_that("RECONFORT score follows the zone the mask was read on (fallback b)", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$manifest_zone <- integer(0)
  .m21_weight_run(.m21_project(6L), available = 6L, seen)
  expect_identical(seen$manifest_zone, 6L)   # masque lu sur la strate -> poids aussi
})

# ---- Lecture pour l'affichage (histogramme, surcouche) ---------------------

test_that("RECONFORT mask for the stratum counts only the stratum's pixels", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m21_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m21_zones(TRUE),
    read_reconfort_alert_mask = .m21_reader(seen, available = c(5L, 6L)),
    .package = "nemeton"
  )
  r <- nemetonshiny:::.read_reconfort_mask_stratum(
    "con", .m21_project(c(5L, 6L)), 6L)
  expect_identical(seen$read, 5L)
  # Seul TR (classe 1, saine) est dans la strate ; les autres sont NA.
  expect_identical(unname(nemetonshiny:::.validation_class_distribution(r)),
                   c(0L, 1L, 0L, 0L, 0L))
})

test_that("RECONFORT mask: stratum = _tot is not clipped", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m21_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m21_zones(TRUE),
    read_reconfort_alert_mask = .m21_reader(seen, available = 5L),
    .package = "nemeton"
  )
  r <- nemetonshiny:::.read_reconfort_mask_stratum(
    "con", .m21_project(5L), 5L)
  expect_identical(unname(nemetonshiny:::.validation_class_distribution(r)),
                   c(0L, 1L, 1L, 2L, 0L))
})

test_that("RECONFORT mask: fallback to the stratum is not clipped, NULL when absent", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m21_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m21_zones(TRUE),
    read_reconfort_alert_mask = .m21_reader(seen, available = 6L),
    .package = "nemeton"
  )
  r <- nemetonshiny:::.read_reconfort_mask_stratum(
    "con", .m21_project(6L), 6L)
  expect_identical(unname(nemetonshiny:::.validation_class_distribution(r)),
                   c(0L, 1L, 1L, 2L, 0L))
  testthat::local_mocked_bindings(
    read_reconfort_alert_mask = function(...) NULL, .package = "nemeton")
  expect_null(nemetonshiny:::.read_reconfort_mask_stratum(
    "con", .m21_project(6L), 6L))
})

# ---- Garde-fou commun FAST / FORDEAD / RECONFORT ---------------------------
# Pour une strate (6) != _tot (5), le lecteur de masque de chaque source est
# appele avec l'id `_tot` et le raster rendu a l'histogramme est decoupe a la
# strate (NA hors strate). Cablage reel de mod_monitoring.R : `run_zone_id_r`
# n'est passe qu'a FAST ; FORDEAD et RECONFORT resolvent `_tot` seuls.
# Le chemin FAST de generation (trend) reste couvert par
# test-service_validation_sampling_tot.R (cas "trend plan").

.m21_guard_cases <- list(
  FAST      = list(sub = "fast_sampling", reader = "read_fast_alert_mask"),
  FORDEAD   = list(sub = "fordead",       reader = "read_fordead_dieback_mask"),
  RECONFORT = list(sub = "reconfort",     reader = "read_reconfort_alert_mask")
)

for (.src in names(.m21_guard_cases)) {
  local({
    src <- .src
    case <- .m21_guard_cases[[src]]
    test_that(sprintf("%s validation histogram is read on _tot and clipped to the stratum", src), {
      skip_if_not_installed("shiny"); skip_if_not_installed("sf")
      skip_if_not_installed("terra")
      seen <- new.env(); seen$read <- integer(0)
      path <- withr::local_tempdir()
      # Le dossier de cache doit exister, sinon alert_mask_r rend NULL (vacant).
      dir.create(file.path(path, "cache", "layers", case$sub), recursive = TRUE)
      proj <- list(id = "p", path = path, metadata = list(), indicators_sf = NULL)
      testthat::local_mocked_bindings(
        get_monitoring_db_connection   = function(...) structure(list(), class = "fakecon"),
        close_monitoring_db_connection = function(con) invisible(TRUE),
        get_monitoring_zone_aoi = .m21_aoi
      )
      mocks <- list(find_zones_by_project = .m21_zones(TRUE))
      mocks[[case$reader]] <- .m21_reader(seen, available = c(5L, 6L))
      do.call(testthat::local_mocked_bindings,
              c(mocks, list(.package = "nemeton")))
      app_state <- shiny::reactiveValues(
        language = "fr", active_main_tab = "monitoring", current_project = proj)
      args <- list(id = "g", app_state = app_state,
                   zone_id_r    = shiny::reactive("6"),
                   mode_r       = shiny::reactive("health"),
                   thresholds_r = shiny::reactive(list(index = "NDVI")),
                   date_range_r = shiny::reactive(NULL),
                   source_fixed = src)
      # run_zone_id_r : FAST seulement, comme mod_monitoring.R.
      if (identical(src, "FAST")) args$run_zone_id_r <- shiny::reactive("5")
      res <- new.env()
      shiny::testServer(
        nemetonshiny:::mod_validation_sampling_server, args = args,
        {
          session$setInputs(classes = c("3", "4"), control_classes = "0")
          session$flushReact()
          res$dist <- class_distribution_r()
          # Surcouche carte : meme lecture que l'histogramme (_tot + decoupe).
          plan_rv(data.frame(zone_id = 6L, source = src))
          res$overlay <- alert_raster_r()
        }
      )
      expect_true(5L %in% seen$read)
      expect_false(6L %in% seen$read)
      expect_identical(unname(res$dist), c(0L, 1L, 0L, 0L, 0L))
      expect_identical(
        unname(nemetonshiny:::.validation_class_distribution(res$overlay)),
        c(0L, 1L, 0L, 0L, 0L))
    })
  })
}

# ---- FORDEAD : anomaly_index suit la zone du masque -------------------------

.m21_fordead_weight_run <- function(proj, available, seen) {
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m21_aoi,
                                  .env = parent.frame())
  testthat::local_mocked_bindings(
    find_zones_by_project = .m21_zones(TRUE),
    read_fordead_dieback_mask = .m21_reader(new.env(), available = available),
    read_fordead_layer = function(con, zone_id, layer, run_id = NULL,
                                  cache_dir = NULL, ...) {
      seen$weight_zone <- c(seen$weight_zone, zone_id)
      .m21_mask()
    },
    create_validation_sampling_plan = function(zone, alert_raster, ...,
                                               weighting = "uniform",
                                               weight_raster = NULL) {
      p <- .m21_mock_plan(zone, alert_raster)
      p$alert_weight <- 1
      p
    },
    .package = "nemeton",
    .env = parent.frame()
  )
  nemetonshiny:::generate_validation_plan(
    "con", proj, source = "FORDEAD", zone_id = 6L, n_validation = 1L,
    n_control = 0L, weighting = "continuous")
}

.m21_fordead_project <- function(zones, env = parent.frame()) {
  p <- .m21_project(integer(0), env = env)
  for (z in zones) {
    d <- file.path(p$path, "cache", "layers", "fordead", sprintf("zone_%d", z))
    dir.create(d, recursive = TRUE)
    file.create(file.path(d, sprintf("dieback_mask_RUN%d.tif", z)))
  }
  p
}

test_that("FORDEAD anomaly_index follows the chain zone (fallback b)", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$weight_zone <- integer(0)
  .m21_fordead_weight_run(.m21_fordead_project(c(5L, 6L)), c(5L, 6L), seen)
  expect_identical(seen$weight_zone, 5L)           # masque _tot -> poids _tot
  seen <- new.env(); seen$weight_zone <- integer(0)
  .m21_fordead_weight_run(.m21_fordead_project(6L), 6L, seen)
  expect_identical(seen$weight_zone, 6L)           # masque strate -> poids strate
})

# ---- Strate = _tot : aucun changement pour RECONFORT -----------------------

test_that("RECONFORT with stratum = _tot reads _tot once, no clipping at generation", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m21_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m21_zones(TRUE),
    read_reconfort_alert_mask = .m21_reader(seen, available = 5L),
    create_validation_sampling_plan = .m21_mock_plan,
    .package = "nemeton"
  )
  plan <- nemetonshiny:::generate_validation_plan(
    "con", .m21_project(5L), source = "RECONFORT", zone_id = 5L,
    n_validation = 1L, n_control = 0L, classes = c(2L, 3L), control_classes = 1L)
  expect_identical(seen$read, 5L)
  expect_identical(plan$zone_id, 5L)
  expect_identical(plan$source_run_id, "RUN5")
})

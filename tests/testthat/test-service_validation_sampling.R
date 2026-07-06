# Tests for generate_validation_plan() — spec 014 phase B.
#
# 100 % offline : the nemeton entry points are mocked via
# local_mocked_bindings(.package = "nemeton"). DBI connections are
# sentinel strings (never touched, since all DB-touching paths go
# through nemeton::*).

.make_project_with_zone <- function(path = tempfile("proj-"),
                                    zone_id = 7L,
                                    has_indicators = TRUE) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  ug <- if (has_indicators) {
    poly <- sf::st_polygon(list(rbind(
      c(0, 0), c(1000, 0), c(1000, 1000), c(0, 1000), c(0, 0)
    )))
    sf::st_sf(ug_id = 1L, geometry = sf::st_sfc(poly, crs = 2154))
  } else NULL
  list(
    id            = "test-proj",
    path          = path,
    metadata      = list(name = "test", monitoring_zone_id = zone_id),
    indicators_sf = ug
  )
}

.fake_plan <- function(n_val = 2L, n_ctrl = 1L) {
  n <- n_val + n_ctrl
  pts <- sf::st_sf(
    plot_id     = c(sprintf("V%02d", seq_len(n_val)),
                    sprintf("T%02d", seq_len(n_ctrl))),
    type        = c(rep("Validation", n_val), rep("Temoin", n_ctrl)),
    alert_class = c(rep(3L, n_val), rep(0L, n_ctrl)),
    visit_order = seq_len(n),
    source      = "FORDEAD",
    classes     = "3,4",
    seed        = 42L,
    geometry    = sf::st_sfc(
      lapply(seq_len(n), function(i) sf::st_point(c(i * 10, i * 10))),
      crs = 2154
    )
  )
  pts
}


test_that("generate_validation_plan aborts when no project is loaded", {
  expect_error(
    nemetonshiny:::generate_validation_plan(con = "fake", project = NULL),
    class = "validation_no_project"
  )
})

test_that("generate_validation_plan aborts when monitoring_zone_id is missing", {
  proj <- .make_project_with_zone(zone_id = NULL)
  proj$metadata$monitoring_zone_id <- NULL
  expect_error(
    nemetonshiny:::generate_validation_plan(con = "fake", project = proj),
    class = "validation_no_zone"
  )
})

test_that("generate_validation_plan FORDEAD happy path enriches provenance", {
  proj <- .make_project_with_zone(zone_id = 7L)
  fordead_dir <- file.path(proj$path, "cache", "layers", "fordead",
                           "zone_7")
  dir.create(fordead_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fordead_dir, "dieback_mask_20260520T100000.tif"))

  fake_raster <- terra::rast(nrows = 4, ncols = 4, vals = 0L,
                             crs = "EPSG:2154",
                             extent = terra::ext(0, 40, 0, 40))

  testthat::local_mocked_bindings(
    get_monitoring_zone_aoi = function(con, zone_id) {
      sf::st_sf(zone_id = zone_id,
                geometry = sf::st_sfc(
                  sf::st_polygon(list(rbind(
                    c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)
                  ))), crs = 2154
                ))
    }
  )
  testthat::local_mocked_bindings(
    read_fordead_dieback_mask = function(con, zone_id, run_id = NULL,
                                         cache_dir) fake_raster,
    create_validation_sampling_plan = function(zone, alert_raster,
                                               n_validation, n_control,
                                               classes, control_classes,
                                               buffer_m, source, weighting = "uniform", weight_raster = NULL, seed) {
      .fake_plan(n_val = n_validation, n_ctrl = n_control)
    },
    .package = "nemeton"
  )

  plan <- nemetonshiny:::generate_validation_plan(
    con = "fake-con", project = proj, source = "FORDEAD",
    n_validation = 2L, n_control = 1L
  )

  expect_s3_class(plan, "sf")
  expect_equal(nrow(plan), 3L)
  expect_true(all(c("zone_id", "source_run_id", "generated_at") %in%
                  names(plan)))
  expect_equal(unique(plan$zone_id), 7L)
  expect_equal(unique(plan$source_run_id), "20260520T100000")
  expect_s3_class(plan$generated_at, "POSIXct")
})

test_that("generate_validation_plan FAST path reuses cached mask when present", {
  proj <- .make_project_with_zone(zone_id = 9L)
  fast_dir <- file.path(proj$path, "cache", "layers", "fast_sampling",
                        "zone_9")
  dir.create(fast_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fast_dir, "fast_alert_20260524T080000.tif"))

  fake_raster <- terra::rast(nrows = 4, ncols = 4, vals = 0L,
                             crs = "EPSG:2154",
                             extent = terra::ext(0, 40, 0, 40))

  compute_calls <- 0L
  testthat::local_mocked_bindings(
    get_monitoring_zone_aoi = function(con, zone_id) {
      sf::st_sf(zone_id = zone_id,
                geometry = sf::st_sfc(
                  sf::st_polygon(list(rbind(
                    c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)
                  ))), crs = 2154
                ))
    }
  )
  testthat::local_mocked_bindings(
    read_fast_alert_mask = function(con, zone_id, run_id = NULL,
                                    cache_dir) fake_raster,
    compute_fast_alert_mask = function(...) {
      compute_calls <<- compute_calls + 1L
      "irrelevant"
    },
    create_validation_sampling_plan = function(zone, alert_raster,
                                               n_validation, n_control,
                                               classes, control_classes,
                                               buffer_m, source, weighting = "uniform", weight_raster = NULL, seed) {
      .fake_plan(n_val = n_validation, n_ctrl = n_control)
    },
    .package = "nemeton"
  )

  plan <- nemetonshiny:::generate_validation_plan(
    con = "fake-con", project = proj, source = "FAST",
    n_validation = 2L, n_control = 1L,
    ndvi_threshold = 0.4, nbr_threshold = 0.3,
    date_from = as.Date("2024-04-01"),
    date_to   = as.Date("2024-09-30")
  )

  expect_equal(compute_calls, 0L)  # cache hit — no recompute
  expect_equal(unique(plan$source_run_id), "20260524T080000")
})

test_that("generate_validation_plan errors as validation_no_mask when source has no mask", {
  proj <- .make_project_with_zone(zone_id = 7L)

  testthat::local_mocked_bindings(
    get_monitoring_zone_aoi = function(con, zone_id) {
      sf::st_sf(zone_id = zone_id,
                geometry = sf::st_sfc(
                  sf::st_polygon(list(rbind(
                    c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)
                  ))), crs = 2154
                ))
    }
  )
  testthat::local_mocked_bindings(
    read_fordead_dieback_mask = function(...) NULL,
    .package = "nemeton"
  )

  # No cache dir created → fordead path returns NULL → typed error.
  expect_error(
    nemetonshiny:::generate_validation_plan(
      con = "fake-con", project = proj, source = "FORDEAD"
    ),
    class = "validation_no_mask"
  )
})

test_that("generate_validation_plan translates nemeton_empty_alert_mask to validation_empty_mask", {
  proj <- .make_project_with_zone(zone_id = 7L)
  fordead_dir <- file.path(proj$path, "cache", "layers", "fordead",
                           "zone_7")
  dir.create(fordead_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fordead_dir, "dieback_mask_20260520T100000.tif"))

  fake_raster <- terra::rast(nrows = 4, ncols = 4, vals = 0L,
                             crs = "EPSG:2154",
                             extent = terra::ext(0, 40, 0, 40))

  testthat::local_mocked_bindings(
    get_monitoring_zone_aoi = function(con, zone_id) {
      sf::st_sf(zone_id = zone_id,
                geometry = sf::st_sfc(
                  sf::st_polygon(list(rbind(
                    c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)
                  ))), crs = 2154
                ))
    }
  )
  testthat::local_mocked_bindings(
    read_fordead_dieback_mask = function(...) fake_raster,
    create_validation_sampling_plan = function(...) {
      rlang::abort("empty mask", class = "nemeton_empty_alert_mask")
    },
    .package = "nemeton"
  )

  expect_error(
    nemetonshiny:::generate_validation_plan(
      con = "fake-con", project = proj, source = "FORDEAD"
    ),
    class = "validation_empty_mask"
  )
})

test_that("generate_validation_plan propagates control_classes to the cœur planner", {
  proj <- .make_project_with_zone(zone_id = 7L)
  fordead_dir <- file.path(proj$path, "cache", "layers", "fordead",
                           "zone_7")
  dir.create(fordead_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fordead_dir, "dieback_mask_20260520T100000.tif"))

  fake_raster <- terra::rast(nrows = 4, ncols = 4, vals = 0L,
                             crs = "EPSG:2154",
                             extent = terra::ext(0, 40, 0, 40))
  seen_control <- NULL

  testthat::local_mocked_bindings(
    get_monitoring_zone_aoi = function(con, zone_id) {
      sf::st_sf(zone_id = zone_id,
                geometry = sf::st_sfc(
                  sf::st_polygon(list(rbind(
                    c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)
                  ))), crs = 2154))
    }
  )
  testthat::local_mocked_bindings(
    read_fordead_dieback_mask = function(...) fake_raster,
    create_validation_sampling_plan = function(zone, alert_raster,
                                               n_validation, n_control,
                                               classes, control_classes,
                                               buffer_m, source, weighting = "uniform", weight_raster = NULL, seed) {
      seen_control <<- control_classes
      .fake_plan(n_val = n_validation, n_ctrl = n_control)
    },
    .package = "nemeton"
  )

  nemetonshiny:::generate_validation_plan(
    con = "fake-con", project = proj, source = "FORDEAD",
    n_validation = 2L, n_control = 1L, control_classes = c(3L)
  )
  expect_equal(seen_control, 3L)
})

test_that(".validation_class_distribution flags a raster with no class-0 cell", {
  # All cells class 4 (villards case) → "0" bucket present but zero.
  r <- terra::rast(nrows = 4, ncols = 4, vals = 4L, crs = "EPSG:2154",
                   extent = terra::ext(0, 40, 0, 40))
  dist <- nemetonshiny:::.validation_class_distribution(r)
  expect_equal(names(dist), c("0", "1", "2", "3", "4"))
  expect_equal(unname(dist[["0"]]), 0L)
  expect_equal(unname(dist[["4"]]), 16L)
  expect_null(nemetonshiny:::.validation_class_distribution(NULL))
})

test_that("generate_validation_plan FAST requires compute params when no cache", {
  proj <- .make_project_with_zone(zone_id = 9L)
  # No fast cache directory — and we won't supply ndvi/nbr/dates.

  testthat::local_mocked_bindings(
    get_monitoring_zone_aoi = function(con, zone_id) {
      sf::st_sf(zone_id = zone_id,
                geometry = sf::st_sfc(
                  sf::st_polygon(list(rbind(
                    c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)
                  ))), crs = 2154
                ))
    }
  )
  testthat::local_mocked_bindings(
    read_fast_alert_mask = function(...) NULL,
    .package = "nemeton"
  )

  expect_error(
    nemetonshiny:::generate_validation_plan(
      con = "fake-con", project = proj, source = "FAST"
      # ndvi/nbr/dates intentionally omitted
    ),
    class = "validation_no_mask"
  )
})

test_that("generate_validation_plan RECONFORT reads the reconfort mask + classes 2/3, control 1", {
  proj <- .make_project_with_zone(zone_id = 7L)
  rc_dir <- file.path(proj$path, "cache", "layers", "reconfort", "zone_7")
  dir.create(rc_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(rc_dir, "reconfort_mask_20260601T120000.tif"))

  # Categorical 1/2/3 mask (1=sain, 2=deperissant, 3=tres-deperissant).
  fake_raster <- terra::rast(nrows = 4, ncols = 4, vals = 2L,
                             crs = "EPSG:2154",
                             extent = terra::ext(0, 40, 0, 40))
  seen <- list()

  testthat::local_mocked_bindings(
    get_monitoring_zone_aoi = function(con, zone_id) {
      sf::st_sf(zone_id = zone_id,
                geometry = sf::st_sfc(sf::st_polygon(list(rbind(
                  c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)
                ))), crs = 2154))
    }
  )
  read_calls <- 0L
  testthat::local_mocked_bindings(
    read_reconfort_alert_mask = function(con, zone_id, run_id = NULL,
                                         cache_dir = NULL, ...) {
      read_calls <<- read_calls + 1L
      fake_raster
    },
    create_validation_sampling_plan = function(zone, alert_raster,
                                               n_validation, n_control,
                                               classes, control_classes,
                                               buffer_m, source, weighting = "uniform", weight_raster = NULL, seed) {
      seen$classes <<- classes
      seen$control <<- control_classes
      seen$source  <<- source
      .fake_plan(n_val = n_validation, n_ctrl = n_control)
    },
    .package = "nemeton"
  )

  plan <- nemetonshiny:::generate_validation_plan(
    con = "fake-con", project = proj, source = "RECONFORT",
    n_validation = 2L, n_control = 1L,
    classes = c(2L, 3L), control_classes = c(1L)
  )

  expect_equal(read_calls, 1L)              # read the persisted mask
  expect_equal(seen$source, "RECONFORT")    # routed to the RECONFORT planner
  expect_equal(seen$classes, c(2L, 3L))     # alert classes
  expect_equal(seen$control, 1L)            # control (témoin) class
  expect_s3_class(plan, "sf")
  expect_equal(unique(plan$zone_id), 7L)
  expect_equal(unique(plan$source_run_id), "20260601T120000")  # reconfort_mask_<TS>
})

test_that("generate_validation_plan RECONFORT errors as validation_no_mask without a persisted run", {
  proj <- .make_project_with_zone(zone_id = 7L)
  # No reconfort cache dir → no on-the-fly compute (unlike FAST) → typed error.
  testthat::local_mocked_bindings(
    get_monitoring_zone_aoi = function(con, zone_id) {
      sf::st_sf(zone_id = zone_id,
                geometry = sf::st_sfc(sf::st_polygon(list(rbind(
                  c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)
                ))), crs = 2154))
    }
  )
  expect_error(
    nemetonshiny:::generate_validation_plan(
      con = "fake-con", project = proj, source = "RECONFORT"
    ),
    class = "validation_no_mask"
  )
})


# --- spec 014 : pondération continue (FORDEAD / RECONFORT) ----------------

# Helper : zone AOI mock partagé par les tests continus.
.mock_zone_aoi <- function() {
  testthat::local_mocked_bindings(
    get_monitoring_zone_aoi = function(con, zone_id) {
      sf::st_sf(zone_id = zone_id,
                geometry = sf::st_sfc(sf::st_polygon(list(rbind(
                  c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)
                ))), crs = 2154))
    },
    .env = parent.frame()
  )
}

test_that("FORDEAD continuous weighting resolves anomaly_index and forwards it", {
  proj <- .make_project_with_zone(zone_id = 7L)
  fordead_dir <- file.path(proj$path, "cache", "layers", "fordead", "zone_7")
  dir.create(fordead_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fordead_dir, "dieback_mask_20260520T100000.tif"))

  alert_r <- terra::rast(nrows = 4, ncols = 4, vals = c(rep(3L, 8), rep(0L, 8)),
                         crs = "EPSG:2154", extent = terra::ext(0, 40, 0, 40))
  sev_r   <- terra::rast(nrows = 4, ncols = 4, vals = seq_len(16),
                         crs = "EPSG:2154", extent = terra::ext(0, 40, 0, 40))
  seen <- new.env()
  .mock_zone_aoi()
  testthat::local_mocked_bindings(
    read_fordead_dieback_mask = function(con, zone_id, run_id = NULL,
                                         cache_dir) alert_r,
    read_fordead_layer = function(con, zone_id, layer, run_id = NULL,
                                  cache_dir = NULL, ...) {
      seen$layer <- layer; seen$zone_id <- zone_id; sev_r
    },
    find_zones_by_project = function(con, project_uuid) {
      data.frame(id = c(41L, 42L), name = c("proj_res", "proj_tot"))
    },
    create_validation_sampling_plan = function(zone, alert_raster, n_validation,
                                               n_control, classes, control_classes,
                                               buffer_m, source,
                                               weighting = "uniform",
                                               weight_raster = NULL, seed) {
      seen$weighting <- weighting
      seen$has_weight <- inherits(weight_raster, "SpatRaster")
      p <- .fake_plan(n_val = n_validation, n_ctrl = n_control)
      p$alert_weight <- as.numeric(seq_len(nrow(p)))   # colonne mode continu
      p
    },
    .package = "nemeton"
  )

  plan <- nemetonshiny:::generate_validation_plan(
    con = "fake-con", project = proj, source = "FORDEAD",
    n_validation = 2L, n_control = 1L, weighting = "continuous"
  )

  expect_equal(seen$layer, "anomaly_index")     # couche continue lue
  expect_equal(seen$zone_id, 42L)               # id _tot résolu
  expect_equal(seen$weighting, "continuous")    # transmis au cœur
  expect_true(seen$has_weight)                  # weight_raster non-NULL
  expect_true("alert_weight" %in% names(plan))  # colonne exposée
})

test_that("FORDEAD continuous falls back to uniform when anomaly_index is absent", {
  proj <- .make_project_with_zone(zone_id = 7L)
  fordead_dir <- file.path(proj$path, "cache", "layers", "fordead", "zone_7")
  dir.create(fordead_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fordead_dir, "dieback_mask_20260520T100000.tif"))

  alert_r <- terra::rast(nrows = 4, ncols = 4, vals = c(rep(3L, 8), rep(0L, 8)),
                         crs = "EPSG:2154", extent = terra::ext(0, 40, 0, 40))
  seen <- new.env()
  .mock_zone_aoi()
  testthat::local_mocked_bindings(
    read_fordead_dieback_mask = function(con, zone_id, run_id = NULL,
                                         cache_dir) alert_r,
    read_fordead_layer = function(con, zone_id, layer, run_id = NULL,
                                  cache_dir = NULL, ...) NULL,  # ancien run : pas de couche
    find_zones_by_project = function(con, project_uuid) {
      data.frame(id = 42L, name = "proj_tot")
    },
    create_validation_sampling_plan = function(zone, alert_raster, n_validation,
                                               n_control, classes, control_classes,
                                               buffer_m, source,
                                               weighting = "uniform",
                                               weight_raster = NULL, seed) {
      seen$weighting <- weighting
      seen$weight_null <- is.null(weight_raster)
      .fake_plan(n_val = n_validation, n_ctrl = n_control)  # pas d'alert_weight
    },
    .package = "nemeton"
  )

  plan <- nemetonshiny:::generate_validation_plan(
    con = "fake-con", project = proj, source = "FORDEAD",
    n_validation = 2L, n_control = 1L, weighting = "continuous"
  )

  expect_equal(seen$weighting, "uniform")        # repli app
  expect_true(seen$weight_null)                  # aucun raster transmis
  expect_false("alert_weight" %in% names(plan))  # colonne absente en uniforme
})

test_that("continuous weighting maps the core mismatch to validation_weight_mismatch", {
  proj <- .make_project_with_zone(zone_id = 7L)
  fordead_dir <- file.path(proj$path, "cache", "layers", "fordead", "zone_7")
  dir.create(fordead_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(fordead_dir, "dieback_mask_20260520T100000.tif"))

  alert_r <- terra::rast(nrows = 4, ncols = 4, vals = 3L,
                         crs = "EPSG:2154", extent = terra::ext(0, 40, 0, 40))
  sev_r   <- terra::rast(nrows = 4, ncols = 4, vals = seq_len(16),
                         crs = "EPSG:2154", extent = terra::ext(0, 40, 0, 40))
  .mock_zone_aoi()
  testthat::local_mocked_bindings(
    read_fordead_dieback_mask = function(con, zone_id, run_id = NULL,
                                         cache_dir) alert_r,
    read_fordead_layer = function(con, zone_id, layer, run_id = NULL,
                                  cache_dir = NULL, ...) sev_r,
    find_zones_by_project = function(con, project_uuid) {
      data.frame(id = 42L, name = "proj_tot")
    },
    create_validation_sampling_plan = function(zone, alert_raster, n_validation,
                                               n_control, classes, control_classes,
                                               buffer_m, source,
                                               weighting = "uniform",
                                               weight_raster = NULL, seed) {
      rlang::abort("no CRS", class = "validation_weight_raster_mismatch")
    },
    .package = "nemeton"
  )

  expect_error(
    nemetonshiny:::generate_validation_plan(
      con = "fake-con", project = proj, source = "FORDEAD",
      n_validation = 2L, n_control = 1L, weighting = "continuous"
    ),
    class = "validation_weight_mismatch"
  )
})

test_that("RECONFORT continuous weighting resolves the score layer from the cache manifest", {
  proj <- .make_project_with_zone(zone_id = 7L)
  reconfort_dir <- file.path(proj$path, "cache", "layers", "reconfort", "zone_7")
  dir.create(reconfort_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(reconfort_dir, "reconfort_mask_20260601T120000.tif"))

  alert_r <- terra::rast(nrows = 4, ncols = 4, vals = c(rep(2L, 8), rep(1L, 8)),
                         crs = "EPSG:2154", extent = terra::ext(0, 40, 0, 40))
  sev_r   <- terra::rast(nrows = 4, ncols = 4, vals = seq_len(16),
                         crs = "EPSG:2154", extent = terra::ext(0, 40, 0, 40))
  seen <- new.env()
  .mock_zone_aoi()
  testthat::local_mocked_bindings(
    read_reconfort_alert_mask = function(con, zone_id, run_id = NULL,
                                         cache_dir) alert_r,
    reconfort_cache_manifest = function(cache_dir, zone_id, run_id = NULL,
                                        include_range = FALSE) {
      data.frame(id = c("score", "classification"),
                 type = c("raster", "raster"),
                 stringsAsFactors = FALSE)
    },
    read_reconfort_layer = function(layer, con = NULL, zone_id = NULL,
                                    apply_zone_mask = TRUE, mask_polygon = NULL) {
      seen$layer_id <- layer$id; sev_r
    },
    create_validation_sampling_plan = function(zone, alert_raster, n_validation,
                                               n_control, classes, control_classes,
                                               buffer_m, source,
                                               weighting = "uniform",
                                               weight_raster = NULL, seed) {
      seen$weighting <- weighting
      seen$has_weight <- inherits(weight_raster, "SpatRaster")
      p <- .fake_plan(n_val = n_validation, n_ctrl = n_control)
      p$alert_weight <- as.numeric(seq_len(nrow(p)))
      p
    },
    .package = "nemeton"
  )

  plan <- nemetonshiny:::generate_validation_plan(
    con = "fake-con", project = proj, source = "RECONFORT",
    zone_id = 7L, n_validation = 2L, n_control = 1L,
    classes = c(2L, 3L), control_classes = c(1L), weighting = "continuous"
  )

  expect_equal(seen$layer_id, "score")          # couche continue (pas classification)
  expect_equal(seen$weighting, "continuous")
  expect_true(seen$has_weight)
  expect_true("alert_weight" %in% names(plan))
})

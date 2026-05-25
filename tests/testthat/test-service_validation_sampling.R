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
                                               classes, buffer_m,
                                               source, seed) {
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
  fast_dir <- file.path(proj$path, "cache", "layers", "fast",
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
                                               classes, buffer_m,
                                               source, seed) {
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

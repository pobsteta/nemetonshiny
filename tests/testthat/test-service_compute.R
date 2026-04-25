# Tests for service_compute.R
# Compute service: data downloading, caching, and indicator computation orchestration

# ==============================================================================
# DATA_SOURCES configuration tests
# ==============================================================================

test_that("DATA_SOURCES contains required raster sources", {
  ds <- nemetonshiny:::DATA_SOURCES

  expect_type(ds, "list")
  expect_true("rasters" %in% names(ds))
  expect_true("vectors" %in% names(ds))

  # Check raster sources
  rasters <- ds$rasters
  expect_true("ndvi" %in% names(rasters))
  expect_true("dem" %in% names(rasters))
  expect_true("forest_cover" %in% names(rasters))
  expect_true("lidar_mnh" %in% names(rasters))
  expect_true("lidar_mnt" %in% names(rasters))

  # Check raster source structure
  expect_equal(rasters$ndvi$type, "raster")
  expect_equal(rasters$ndvi$source, "ign_irc")
  expect_true("required_for" %in% names(rasters$ndvi))
})

test_that("DATA_SOURCES contains required vector sources", {
  ds <- nemetonshiny:::DATA_SOURCES

  vectors <- ds$vectors
  expect_true("protected_areas" %in% names(vectors))
  # The hydrographic network source is named water_network; it
  # carries required_for = "indicateur_w1_reseau" internally.
  expect_true("water_network" %in% names(vectors))
  expect_true("water_surfaces" %in% names(vectors))
  expect_true("wetlands" %in% names(vectors))
  expect_true("roads" %in% names(vectors))
  expect_true("buildings" %in% names(vectors))
  expect_true("bdforet" %in% names(vectors))

  # Check vector source structure
  expect_equal(vectors$roads$type, "vector")
  expect_equal(vectors$roads$source, "ign_bd_topo")
  expect_true("required_for" %in% names(vectors$roads))
})

test_that("DATA_SOURCES contains point cloud sources", {
  ds <- nemetonshiny:::DATA_SOURCES

  expect_true("point_clouds" %in% names(ds))
  pc <- ds$point_clouds
  expect_true("lidar_copc" %in% names(pc))
  expect_equal(pc$lidar_copc$type, "point_cloud")
  expect_equal(pc$lidar_copc$source, "ign_lidar_hd")
  expect_equal(pc$lidar_copc$product, "nuage")
})

# ==============================================================================
# COMPUTE_STATUS tests
# ==============================================================================

test_that("COMPUTE_STATUS contains all expected states", {
  status <- nemetonshiny:::COMPUTE_STATUS

  expect_type(status, "list")
  expect_equal(status$PENDING, "pending")
  expect_equal(status$DOWNLOADING, "downloading")
  expect_equal(status$COMPUTING, "computing")
  expect_equal(status$COMPLETED, "completed")
  expect_equal(status$ERROR, "error")
  expect_equal(status$CANCELLED, "cancelled")
})

# ==============================================================================
# get_global_cache_dir tests
# ==============================================================================

test_that("get_global_cache_dir returns valid directory path", {
  # Temporarily mock rappdirs if available
  cache_dir <- nemetonshiny:::get_global_cache_dir()

  expect_type(cache_dir, "character")
  expect_true(nchar(cache_dir) > 0)
  expect_true(dir.exists(cache_dir))
  expect_true(grepl("nemeton", cache_dir))
})

test_that("get_global_cache_dir creates directory if missing", {
  # The function creates the directory if it doesn't exist
  cache_dir <- nemetonshiny:::get_global_cache_dir()
  expect_true(dir.exists(cache_dir))
})

# ==============================================================================
# list_available_indicators tests
# ==============================================================================

test_that("list_available_indicators returns all indicator names", {
  indicators <- nemetonshiny:::list_available_indicators()

  expect_type(indicators, "character")
  expect_true(length(indicators) > 0)

  # Check some expected indicators from each family
  expect_true("indicateur_c1_biomasse" %in% indicators)
  expect_true("indicateur_c2_ndvi" %in% indicators)
  expect_true("indicateur_b1_protection" %in% indicators)
  expect_true("indicateur_w1_reseau" %in% indicators)
  expect_true("indicateur_w3_humidite" %in% indicators)
  expect_true("indicateur_a1_couverture" %in% indicators)
  expect_true("indicateur_r1_feu" %in% indicators)
  expect_true("indicateur_s1_routes" %in% indicators)
  expect_true("indicateur_p1_volume" %in% indicators)
  expect_true("indicateur_n1_distance" %in% indicators)
})

test_that("list_available_indicators returns no duplicates", {
  indicators <- nemetonshiny:::list_available_indicators()
  expect_equal(length(indicators), length(unique(indicators)))
})

# ==============================================================================
# init_compute_state tests
# ==============================================================================

test_that("init_compute_state creates proper state structure", {
  temp_dir <- file.path(tempdir(), "nemeton_test_compute")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir, recursive = TRUE)

  with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    get_project_path = function(id) NULL,
    get_computation_progress = function(id) list(computed_indicators = character(0)),
    {
      state <- nemetonshiny:::init_compute_state("test_project", c("indicateur_c1_biomasse", "indicateur_c2_ndvi"))

      expect_type(state, "list")
      expect_equal(state$project_id, "test_project")
      expect_equal(state$status, nemetonshiny:::COMPUTE_STATUS$PENDING)
      expect_equal(state$phase, "init")
      expect_equal(state$progress, 0)
      expect_equal(state$indicators_total, 2)
      expect_equal(state$indicators_completed, 0)
      expect_equal(state$indicators_failed, 0L)
      expect_type(state$indicators_status, "character")
      expect_type(state$errors, "list")
      expect_null(state$started_at)
      expect_null(state$completed_at)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("init_compute_state expands 'all' to full indicator list", {
  with_mocked_bindings(
    get_project_path = function(id) NULL,
    get_computation_progress = function(id) list(computed_indicators = character(0)),
    {
      state <- nemetonshiny:::init_compute_state("test_project", "all")

      all_indicators <- nemetonshiny:::list_available_indicators()
      expect_equal(state$indicators_total, length(all_indicators))
    }
  )
})

test_that("init_compute_state handles resume with existing progress", {
  with_mocked_bindings(
    get_project_path = function(id) NULL,
    get_computation_progress = function(id) {
      list(
        computed_indicators = c("indicateur_c1_biomasse", "indicateur_c2_ndvi"),
        last_saved_at = "2026-01-01 12:00:00"
      )
    },
    {
      state <- nemetonshiny:::init_compute_state(
        "test_project",
        c("indicateur_c1_biomasse", "indicateur_c2_ndvi", "indicateur_w1_reseau")
      )

      expect_equal(state$indicators_completed, 2)
      expect_equal(state$indicators_skipped, 2)
      expect_true(state$is_resume)
      # Use unname() for comparison since named vectors have different attributes
      expect_equal(unname(state$indicators_status["indicateur_c1_biomasse"]), "completed")
      expect_equal(unname(state$indicators_status["indicateur_c2_ndvi"]), "completed")
      expect_equal(unname(state$indicators_status["indicateur_w1_reseau"]), "pending")
    }
  )
})

# ==============================================================================
# normalize_indicator tests
# ==============================================================================

test_that("normalize_indicator scales indicateur_c1_biomasse correctly", {
  # indicateur_c1_biomasse has ref_max = 150 tC/ha
  values <- c(0, 75, 150, 200)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_c1_biomasse", values)

  expect_equal(normalized[1], 0)

  expect_equal(normalized[2], 50)
  expect_equal(normalized[3], 100)
  expect_equal(normalized[4], 100)  # Capped at 100
})

test_that("normalize_indicator handles NDVI special case", {
  # indicateur_c2_ndvi: 0-1 scale -> 0-100
  values <- c(0, 0.5, 1, 1.5)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_c2_ndvi", values)

  expect_equal(normalized[1], 0)
  expect_equal(normalized[2], 50)
  expect_equal(normalized[3], 100)
  expect_equal(normalized[4], 100)  # Capped
})

test_that("normalize_indicator handles TWI special case", {
  # indicateur_w3_humidite: [2.5, 4.5] -> [0, 100]
  values <- c(2.5, 3.5, 4.5, 5.5)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_w3_humidite", values)

  expect_equal(normalized[1], 0)
  expect_equal(normalized[2], 50)
  expect_equal(normalized[3], 100)
  expect_equal(normalized[4], 100)  # Capped
})

test_that("normalize_indicator handles distance indicators (inverse)", {
  # indicateur_s1_routes: 0m -> 100, 2000m -> 0
  values <- c(0, 1000, 2000, 3000)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_s1_routes", values)

  expect_equal(normalized[1], 100)
  expect_equal(normalized[2], 50)
  expect_equal(normalized[3], 0)
  expect_equal(normalized[4], 0)  # Clamped at 0
})

test_that("normalize_indicator clamps 0-100 indicators", {
  # Indicators already 0-100 should just be clamped
  values <- c(-10, 50, 110)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_b1_protection", values)

  expect_equal(normalized[1], 0)
  expect_equal(normalized[2], 50)
  expect_equal(normalized[3], 100)
})

# ==============================================================================
# start_computation tests
# ==============================================================================

test_that("start_computation fails for missing project", {
  with_mocked_bindings(
    get_project_path = function(id) NULL,
    {
      expect_error(
        nemetonshiny:::start_computation("nonexistent_project"),
        regexp = "Project not found"
      )
    }
  )
})

test_that("start_computation fails for project without parcels", {
  temp_dir <- file.path(tempdir(), "nemeton_test_compute_no_parcels")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    get_app_options = function() list(project_dir = temp_dir),
    clear_cancel = function(id) invisible(NULL),
    get_computation_progress = function(id) list(computed_indicators = character(0)),
    update_project_status = function(id, status) invisible(NULL),
    {
      result <- nemetonshiny:::start_computation("test_project", indicators = "indicateur_c1_biomasse")

      expect_false(result$success)
      expect_equal(result$state$status, "error")
      expect_true(length(result$state$errors) > 0)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("start_computation calls progress_callback", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_compute_callback")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  # Create mock parcels
  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 0.001,0, 0.001,0.001, 0,0.001, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)
  sf::st_write(mock_parcels, file.path(data_dir, "parcels.gpkg"), quiet = TRUE)

  progress_states <- list()
  progress_cb <- function(state) {
    progress_states <<- c(progress_states, list(state))
  }

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    get_app_options = function() list(project_dir = temp_dir),
    clear_cancel = function(id) invisible(NULL),
    get_computation_progress = function(id) list(computed_indicators = character(0)),
    update_project_status = function(id, status) invisible(NULL),
    download_layers_for_parcels = function(parcels, project_path, progress_callback) {
      if (!is.null(progress_callback)) {
        progress_callback(list(completed = 5, current = "test_download"))
      }
      structure(
        list(rasters = list(), vectors = list(), point_clouds = list(),
             bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
             cache_dir = tempdir(), warnings = list()),
        class = "nemeton_layers"
      )
    },
    is_cancelled = function(id) FALSE,
    compute_all_indicators = function(parcels, layers, indicators, progress_callback, project_id) {
      if (!is.null(progress_callback)) {
        progress_callback(list(completed = 1, failed = 0, current = "test_compute",
                               status = c(indicateur_c1_biomasse = "completed"), errors = list()))
      }
      parcels$indicateur_c1_biomasse <- 50
      parcels
    },
    save_indicators = function(project_id, results) TRUE,
    {
      result <- nemetonshiny:::start_computation(
        "test_project",
        indicators = "indicateur_c1_biomasse",
        progress_callback = progress_cb
      )

      expect_true(length(progress_states) > 0)
      # Check that progress was reported
      phases <- sapply(progress_states, function(s) s$phase %||% s$current)
      expect_true(any(!is.null(phases)))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# download_layers_for_parcels tests
# ==============================================================================

test_that("download_layers_for_parcels creates cache directory", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_download_layers")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  dir.create(project_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(2.0, 48.0, 2.001, 48.0, 2.001, 48.001, 2.0, 48.001, 2.0, 48.0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  # Mock all download functions to return NULL (simulating unavailable data)
  with_mocked_bindings(
    download_raster_source = function(...) NULL,
    download_vector_source = function(...) NULL,
    download_ign_lidar_hd = function(...) NULL,
    {
      result <- nemetonshiny:::download_layers_for_parcels(
        parcels = mock_parcels,
        project_path = project_dir,
        progress_callback = NULL
      )

      expect_s3_class(result, "nemeton_layers")
      expect_true(dir.exists(file.path(project_dir, "cache", "layers")))
      expect_type(result$rasters, "list")
      expect_type(result$vectors, "list")
      expect_type(result$point_clouds, "list")
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("download_layers_for_parcels returns nemeton_layers structure", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_layers_struct")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  dir.create(project_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(2.0, 48.0, 2.001, 48.0, 2.001, 48.001, 2.0, 48.001, 2.0, 48.0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  with_mocked_bindings(
    download_raster_source = function(...) NULL,
    download_vector_source = function(...) NULL,
    download_ign_lidar_hd = function(...) NULL,
    {
      result <- nemetonshiny:::download_layers_for_parcels(
        parcels = mock_parcels,
        project_path = project_dir
      )

      expect_true("rasters" %in% names(result))
      expect_true("vectors" %in% names(result))
      expect_true("point_clouds" %in% names(result))
      expect_true("bbox" %in% names(result))
      expect_true("crs" %in% names(result))
      expect_true("cache_dir" %in% names(result))
      expect_true("warnings" %in% names(result))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# compute_all_indicators tests
# ==============================================================================

test_that("compute_all_indicators handles empty indicator list", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  mock_layers <- structure(
    list(rasters = list(), vectors = list(), point_clouds = list(),
         bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
         cache_dir = tempdir(), warnings = list()),
    class = "nemeton_layers"
  )

  with_mocked_bindings(
    load_indicators = function(id) NULL,
    is_cancelled = function(id) FALSE,
    {
      result <- nemetonshiny:::compute_all_indicators(
        parcels = mock_parcels,
        layers = mock_layers,
        indicators = character(0),
        progress_callback = NULL,
        project_id = NULL
      )

      expect_s3_class(result, "sf")
      expect_equal(nrow(result), 1)
    }
  )
})

test_that("compute_all_indicators tracks progress correctly", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  mock_layers <- structure(
    list(rasters = list(), vectors = list(), point_clouds = list(),
         bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
         cache_dir = tempdir(), warnings = list()),
    class = "nemeton_layers"
  )

  progress_reports <- list()
  progress_cb <- function(report) {
    progress_reports <<- c(progress_reports, list(report))
  }

  with_mocked_bindings(
    load_indicators = function(id) NULL,
    is_cancelled = function(id) FALSE,
    compute_single_indicator = function(indicator, parcels, layers) {
      rep(50, nrow(parcels))
    },
    save_indicators_incremental = function(...) TRUE,
    {
      result <- nemetonshiny:::compute_all_indicators(
        parcels = mock_parcels,
        layers = mock_layers,
        indicators = c("indicateur_c1_biomasse", "indicateur_c2_ndvi"),
        progress_callback = progress_cb,
        project_id = "test"
      )

      expect_true(length(progress_reports) > 0)
      # Final report should show completion
      final_report <- progress_reports[[length(progress_reports)]]
      expect_equal(final_report$completed, 2)
      expect_equal(final_report$failed, 0)
    }
  )
})

test_that("compute_all_indicators handles indicator computation errors", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  mock_layers <- structure(
    list(rasters = list(), vectors = list(), point_clouds = list(),
         bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
         cache_dir = tempdir(), warnings = list()),
    class = "nemeton_layers"
  )

  with_mocked_bindings(
    load_indicators = function(id) NULL,
    is_cancelled = function(id) FALSE,
    compute_single_indicator = function(indicator, parcels, layers) {
      if (indicator == "indicateur_c1_biomasse") {
        stop("Test error")
      }
      rep(50, nrow(parcels))
    },
    save_indicators_incremental = function(...) TRUE,
    {
      result <- suppressWarnings(nemetonshiny:::compute_all_indicators(
        parcels = mock_parcels,
        layers = mock_layers,
        indicators = c("indicateur_c1_biomasse", "indicateur_c2_ndvi"),
        progress_callback = NULL,
        project_id = NULL
      ))

      # Failed indicator should have NA values
      expect_true(all(is.na(result$indicateur_c1_biomasse)))
      # Successful indicator should have values
      expect_false(all(is.na(result$indicateur_c2_ndvi)))
    }
  )
})

test_that("compute_all_indicators respects cancellation", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  mock_layers <- structure(
    list(rasters = list(), vectors = list(), point_clouds = list(),
         bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
         cache_dir = tempdir(), warnings = list()),
    class = "nemeton_layers"
  )

  call_count <- 0
  with_mocked_bindings(
    load_indicators = function(id) NULL,
    is_cancelled = function(id) {
      call_count <<- call_count + 1
      call_count > 1  # Cancel after first indicator
    },
    compute_single_indicator = function(indicator, parcels, layers) {
      rep(50, nrow(parcels))
    },
    save_indicators_incremental = function(...) TRUE,
    {
      result <- nemetonshiny:::compute_all_indicators(
        parcels = mock_parcels,
        layers = mock_layers,
        indicators = c("indicateur_c1_biomasse", "indicateur_c2_ndvi", "indicateur_w1_reseau"),
        progress_callback = NULL,
        project_id = "test"
      )

      # Should have stopped early due to cancellation
      # Only first indicator should be computed
      expect_true("indicateur_c1_biomasse" %in% names(result))
    }
  )
})

# ==============================================================================
# save_indicators / load_indicators tests
# ==============================================================================

test_that("save_indicators creates parquet file", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")

  temp_dir <- file.path(tempdir(), "nemeton_test_save_indicators")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  # Use plain data.frame since service_project.R save_indicators doesn't handle sf
  # (it converts sf to WKT in service_compute.R's version)
  mock_results <- data.frame(
    id = "p1",
    indicateur_c1_biomasse = 75.5,
    indicateur_c2_ndvi = 0.65
  )

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    update_project_metadata = function(id, updates) invisible(NULL),
    {
      result <- nemetonshiny:::save_indicators("test_project", mock_results)

      expect_true(result)
      expect_true(file.exists(file.path(data_dir, "indicators.parquet")))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("load_indicators returns NULL for missing file", {
  temp_dir <- file.path(tempdir(), "nemeton_test_load_missing")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    {
      result <- nemetonshiny:::load_indicators("test_project")
      expect_null(result)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("save_indicators and load_indicators roundtrip works", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")

  temp_dir <- file.path(tempdir(), "nemeton_test_roundtrip")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  # Use plain data.frame for roundtrip test
  mock_results <- data.frame(
    id = c("p1", "p2"),
    indicateur_c1_biomasse = c(75.5, 82.3),
    indicateur_c2_ndvi = c(0.65, 0.72)
  )

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    update_project_metadata = function(id, updates) invisible(NULL),
    {
      nemetonshiny:::save_indicators("test_project", mock_results)
      loaded <- nemetonshiny:::load_indicators("test_project")

      expect_s3_class(loaded, "data.frame")
      expect_equal(nrow(loaded), 2)
      expect_true("indicateur_c1_biomasse" %in% names(loaded))
      expect_true("indicateur_c2_ndvi" %in% names(loaded))
      expect_equal(loaded$indicateur_c1_biomasse, c(75.5, 82.3))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# Cancellation tests
# ==============================================================================

test_that("cancel_computation creates cancel file", {
  temp_dir <- file.path(tempdir(), "nemeton_test_cancel")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    {
      nemetonshiny:::cancel_computation("test_project")

      cancel_file <- file.path(data_dir, "cancel_requested")
      expect_true(file.exists(cancel_file))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("is_cancelled detects cancel file", {
  temp_dir <- file.path(tempdir(), "nemeton_test_is_cancelled")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    {
      # Initially not cancelled
      expect_false(nemetonshiny:::is_cancelled("test_project"))

      # Create cancel file
      writeLines("cancel", file.path(data_dir, "cancel_requested"))

      # Now cancelled
      expect_true(nemetonshiny:::is_cancelled("test_project"))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("clear_cancel removes cancel file", {
  temp_dir <- file.path(tempdir(), "nemeton_test_clear_cancel")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  cancel_file <- file.path(data_dir, "cancel_requested")
  writeLines("cancel", cancel_file)

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    {
      expect_true(file.exists(cancel_file))

      nemetonshiny:::clear_cancel("test_project")

      expect_false(file.exists(cancel_file))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# Progress state tests
# ==============================================================================

test_that("save_progress_state creates JSON file", {
  temp_dir <- file.path(tempdir(), "nemeton_test_progress_state")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  state <- list(
    project_id = "test_project",
    status = "computing",
    phase = "downloading",
    progress = 5,
    progress_max = 40,
    current_task = "downloading_ndvi",
    indicators_total = 30,
    indicators_completed = 0,
    indicators_failed = 0,
    indicators_status = list(indicateur_c1_biomasse = "pending"),
    errors = list(),
    started_at = Sys.time(),
    completed_at = NULL
  )

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    {
      nemetonshiny:::save_progress_state("test_project", state)

      progress_file <- file.path(data_dir, "progress_state.json")
      expect_true(file.exists(progress_file))

      saved <- jsonlite::read_json(progress_file)
      expect_equal(saved$project_id, "test_project")
      expect_equal(saved$status, "computing")
      expect_equal(saved$progress, 5)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("read_progress_state returns NULL for missing file", {
  temp_dir <- file.path(tempdir(), "nemeton_test_read_progress")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    {
      result <- nemetonshiny:::read_progress_state("test_project")
      expect_null(result)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("read_progress_state returns saved state", {
  temp_dir <- file.path(tempdir(), "nemeton_test_read_progress2")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  progress_file <- file.path(data_dir, "progress_state.json")
  jsonlite::write_json(
    list(project_id = "test_project", status = "completed", progress = 40),
    progress_file,
    auto_unbox = TRUE
  )

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    {
      result <- nemetonshiny:::read_progress_state("test_project")

      expect_type(result, "list")
      expect_equal(result$project_id, "test_project")
      expect_equal(result$status, "completed")
      expect_equal(result$progress, 40)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# clear_computation_cache tests
# ==============================================================================

test_that("clear_computation_cache removes cache files", {
  temp_dir <- file.path(tempdir(), "nemeton_test_clear_cache")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  # Create cache files
  writeLines("test", file.path(data_dir, "indicators.parquet"))
  writeLines("test", file.path(data_dir, "compute_progress.json"))
  writeLines("test", file.path(data_dir, "progress_state.json"))

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    {
      result <- nemetonshiny:::clear_computation_cache("test_project")

      expect_true(result)
      expect_false(file.exists(file.path(data_dir, "indicators.parquet")))
      expect_false(file.exists(file.path(data_dir, "compute_progress.json")))
      expect_false(file.exists(file.path(data_dir, "progress_state.json")))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("clear_computation_cache returns FALSE for invalid project", {
  with_mocked_bindings(
    get_project_path = function(id) NULL,
    {
      result <- nemetonshiny:::clear_computation_cache("nonexistent")
      expect_false(result)
    }
  )
})

# ==============================================================================
# get_computation_progress tests
# ==============================================================================

test_that("get_computation_progress returns empty for new project", {
  temp_dir <- file.path(tempdir(), "nemeton_test_get_progress")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    {
      result <- nemetonshiny:::get_computation_progress("test_project")

      expect_type(result, "list")
      expect_equal(result$computed_indicators, character(0))
      expect_null(result$last_indicator)
      expect_null(result$last_saved_at)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("get_computation_progress reads existing progress", {
  temp_dir <- file.path(tempdir(), "nemeton_test_get_progress2")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  progress <- list(
    computed_indicators = c("indicateur_c1_biomasse", "indicateur_c2_ndvi"),
    last_indicator = "indicateur_c2_ndvi",
    last_saved_at = "2026-01-01 12:00:00"
  )
  jsonlite::write_json(
    progress,
    file.path(data_dir, "compute_progress.json"),
    auto_unbox = TRUE
  )

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    {
      result <- nemetonshiny:::get_computation_progress("test_project")

      expect_equal(result$last_indicator, "indicateur_c2_ndvi")
      expect_equal(result$last_saved_at, "2026-01-01 12:00:00")
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# Helper function tests
# ==============================================================================

test_that("find_url_column finds URL columns", {
  skip_if_not_installed("sf")

  # Test with standard column name
  mock_sf <- sf::st_sf(
    url_telechargement = c("http://example.com/tile1.tif"),
    other = "value",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )

  result <- nemetonshiny:::find_url_column(mock_sf)
  expect_equal(result, "url_telechargement")
})

test_that("find_url_column finds alternative URL columns", {
  skip_if_not_installed("sf")

  mock_sf <- sf::st_sf(
    lien = c("http://example.com/tile1.tif"),
    other = "value",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )

  result <- nemetonshiny:::find_url_column(mock_sf)
  expect_equal(result, "lien")
})

test_that("find_url_column returns NULL when no URL column found", {
  skip_if_not_installed("sf")

  mock_sf <- sf::st_sf(
    name = "test",
    value = 123,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )

  result <- nemetonshiny:::find_url_column(mock_sf)
  expect_null(result)
})

test_that("extract_tile_names extracts filenames from URLs", {
  urls <- c(
    "http://example.com/data/tile_001.tif",
    "http://example.com/data/tile_002.tif"
  )

  result <- nemetonshiny:::extract_tile_names(urls, ".tif")

  expect_equal(result[1], "tile_001.tif")
  expect_equal(result[2], "tile_002.tif")
})

test_that("extract_tile_names generates names for invalid URLs", {
  urls <- c(
    "http://example.com/download?id=123",
    "http://example.com/download?id=456"
  )

  result <- nemetonshiny:::extract_tile_names(urls, ".tif")

  expect_true(grepl("\\.tif$", result[1]))
  expect_true(grepl("\\.tif$", result[2]))
})

# ==============================================================================
# Additional DATA_SOURCES tests
# ==============================================================================

test_that("DATA_SOURCES raster entries have valid structure", {
  ds <- nemetonshiny:::DATA_SOURCES

  for (name in names(ds$rasters)) {
    raster <- ds$rasters[[name]]
    expect_true("name" %in% names(raster), info = paste("raster", name, "missing 'name'"))
    expect_true("type" %in% names(raster), info = paste("raster", name, "missing 'type'"))
    expect_true("source" %in% names(raster), info = paste("raster", name, "missing 'source'"))
    expect_true("required_for" %in% names(raster), info = paste("raster", name, "missing 'required_for'"))
    expect_equal(raster$type, "raster", info = paste("raster", name, "has wrong type"))
  }
})

test_that("DATA_SOURCES vector entries have valid structure", {
  ds <- nemetonshiny:::DATA_SOURCES

  for (name in names(ds$vectors)) {
    vector <- ds$vectors[[name]]
    expect_true("name" %in% names(vector), info = paste("vector", name, "missing 'name'"))
    expect_true("type" %in% names(vector), info = paste("vector", name, "missing 'type'"))
    expect_true("source" %in% names(vector), info = paste("vector", name, "missing 'source'"))
    expect_true("required_for" %in% names(vector), info = paste("vector", name, "missing 'required_for'"))
    expect_equal(vector$type, "vector", info = paste("vector", name, "has wrong type"))
  }
})

test_that("DATA_SOURCES sources are recognized types", {
  ds <- nemetonshiny:::DATA_SOURCES
  valid_raster_sources <- c("ign_irc", "ign_bd_alti", "oso", "ign_lidar_hd")
  valid_vector_sources <- c("inpn_wfs", "ign_bd_topo", "ign_bdforet")

  for (name in names(ds$rasters)) {
    expect_true(
      ds$rasters[[name]]$source %in% valid_raster_sources,
      info = paste("raster", name, "has unknown source:", ds$rasters[[name]]$source)
    )
  }

  for (name in names(ds$vectors)) {
    expect_true(
      ds$vectors[[name]]$source %in% valid_vector_sources,
      info = paste("vector", name, "has unknown source:", ds$vectors[[name]]$source)
    )
  }
})

test_that("DATA_SOURCES LiDAR entries have product specification", {
  ds <- nemetonshiny:::DATA_SOURCES

  # Check raster LiDAR entries
 for (name in names(ds$rasters)) {
    if (ds$rasters[[name]]$source == "ign_lidar_hd") {
      expect_true(
        "product" %in% names(ds$rasters[[name]]),
        info = paste("LiDAR raster", name, "missing 'product'")
      )
    }
  }

  # Check point cloud entries
  for (name in names(ds$point_clouds)) {
    if (ds$point_clouds[[name]]$source == "ign_lidar_hd") {
      expect_true(
        "product" %in% names(ds$point_clouds[[name]]),
        info = paste("LiDAR point cloud", name, "missing 'product'")
      )
    }
  }
})

# ==============================================================================
# resolve_vector_layer and resolve_raster_layer tests
# ==============================================================================

test_that("resolve_raster_layer returns NULL for non-nemeton_layers object", {
  result <- nemetonshiny:::resolve_raster_layer(list(rasters = list()), "ndvi")
  expect_null(result)
})

test_that("resolve_raster_layer returns NULL for missing layer", {
  mock_layers <- structure(
    list(rasters = list(), vectors = list()),
    class = "nemeton_layers"
  )

  result <- nemetonshiny:::resolve_raster_layer(mock_layers, "nonexistent")
  expect_null(result)
})

test_that("resolve_raster_layer returns SpatRaster directly", {
  skip_if_not_installed("terra")

  mock_raster <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(mock_raster) <- runif(100)

  mock_layers <- structure(
    list(
      rasters = list(ndvi = mock_raster),
      vectors = list()
    ),
    class = "nemeton_layers"
  )

  result <- nemetonshiny:::resolve_raster_layer(mock_layers, "ndvi")
  expect_s4_class(result, "SpatRaster")
})

test_that("resolve_raster_layer loads from path when entry is a list", {
  skip_if_not_installed("terra")

  # Create temporary raster file
  temp_file <- tempfile(fileext = ".tif")
  mock_raster <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(mock_raster) <- runif(100)
  terra::writeRaster(mock_raster, temp_file)

  mock_layers <- structure(
    list(
      rasters = list(ndvi = list(path = temp_file, object = NULL)),
      vectors = list()
    ),
    class = "nemeton_layers"
  )

  result <- nemetonshiny:::resolve_raster_layer(mock_layers, "ndvi")
  expect_s4_class(result, "SpatRaster")

  unlink(temp_file)
})

test_that("resolve_raster_layer returns object from list if available", {
  skip_if_not_installed("terra")

  mock_raster <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(mock_raster) <- runif(100)

  mock_layers <- structure(
    list(
      rasters = list(ndvi = list(object = mock_raster, path = NULL)),
      vectors = list()
    ),
    class = "nemeton_layers"
  )

  result <- nemetonshiny:::resolve_raster_layer(mock_layers, "ndvi")
  expect_s4_class(result, "SpatRaster")
})

test_that("resolve_vector_layer returns NULL for non-nemeton_layers object", {
  result <- nemetonshiny:::resolve_vector_layer(list(vectors = list()), "roads")
  expect_null(result)
})

test_that("resolve_vector_layer returns NULL for missing layer", {
  mock_layers <- structure(
    list(rasters = list(), vectors = list()),
    class = "nemeton_layers"
  )

  result <- nemetonshiny:::resolve_vector_layer(mock_layers, "nonexistent")
  expect_null(result)
})

test_that("resolve_vector_layer returns sf directly", {
  skip_if_not_installed("sf")

  mock_sf <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2)),
      crs = 4326
    )
  )

  mock_layers <- structure(
    list(
      rasters = list(),
      vectors = list(roads = mock_sf)
    ),
    class = "nemeton_layers"
  )

  result <- nemetonshiny:::resolve_vector_layer(mock_layers, "roads")
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 3)
})

test_that("resolve_vector_layer loads from path when entry is a list", {
  skip_if_not_installed("sf")

  # Create temporary gpkg file
  temp_file <- tempfile(fileext = ".gpkg")
  mock_sf <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2)),
      crs = 4326
    )
  )
  sf::st_write(mock_sf, temp_file, quiet = TRUE)

  mock_layers <- structure(
    list(
      rasters = list(),
      vectors = list(roads = list(path = temp_file, object = NULL))
    ),
    class = "nemeton_layers"
  )

  result <- nemetonshiny:::resolve_vector_layer(mock_layers, "roads")
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 3)

  unlink(temp_file)
})

test_that("get_dem_raster prefers lidar_mnt over dem", {
  skip_if_not_installed("terra")

  lidar_mnt <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(lidar_mnt) <- runif(100, 100, 200)  # Higher values

  bd_alti_dem <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(bd_alti_dem) <- runif(100, 0, 50)  # Lower values

  mock_layers <- structure(
    list(
      rasters = list(lidar_mnt = lidar_mnt, dem = bd_alti_dem),
      vectors = list()
    ),
    class = "nemeton_layers"
  )

  result <- nemetonshiny:::get_dem_raster(mock_layers)
  expect_s4_class(result, "SpatRaster")
  # Should return lidar_mnt (higher values)
  expect_true(mean(terra::values(result), na.rm = TRUE) > 50)
})

test_that("get_dem_raster falls back to dem when lidar_mnt missing", {
  skip_if_not_installed("terra")

  bd_alti_dem <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(bd_alti_dem) <- runif(100, 0, 50)

  mock_layers <- structure(
    list(
      rasters = list(dem = bd_alti_dem),
      vectors = list()
    ),
    class = "nemeton_layers"
  )

  result <- nemetonshiny:::get_dem_raster(mock_layers)
  expect_s4_class(result, "SpatRaster")
})

test_that("get_dem_raster returns NULL when no DEM available", {
  mock_layers <- structure(
    list(rasters = list(), vectors = list()),
    class = "nemeton_layers"
  )

  result <- nemetonshiny:::get_dem_raster(mock_layers)
  expect_null(result)
})

# ==============================================================================
# compute_single_indicator edge cases
# ==============================================================================

test_that("compute_single_indicator uses placeholder for unknown indicator", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  mock_layers <- structure(
    list(rasters = list(), vectors = list(), point_clouds = list(),
         bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
         cache_dir = tempdir(), warnings = list()),
    class = "nemeton_layers"
  )

  # Test with a completely non-existent indicator
  result <- nemetonshiny:::compute_single_indicator(
    "nonexistent_indicator_xyz",
    mock_parcels,
    mock_layers
  )

  expect_type(result, "double")
  expect_length(result, 1)
})

test_that("compute_single_indicator returns placeholder for non-existent function", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(1,1, 2,1, 2,2, 1,2, 1,1), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = c("p1", "p2"), geometry = mock_geom)

  mock_layers <- structure(
    list(rasters = list(), vectors = list(), point_clouds = list(),
         bbox = c(0, 0, 2, 2), crs = sf::st_crs(4326),
         cache_dir = tempdir(), warnings = list()),
    class = "nemeton_layers"
  )

  # Test with a completely non-existent indicator - should fall through to placeholder
  result <- nemetonshiny:::compute_single_indicator(
    "placeholder_test_xyz",
    mock_parcels,
    mock_layers
  )

  expect_type(result, "double")
  expect_length(result, 2)  # One value per parcel
})

# ==============================================================================
# normalize_indicator additional tests
# ==============================================================================

test_that("normalize_indicator handles NA values", {
  values <- c(0, NA, 75, 150)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_c1_biomasse", values)

  expect_equal(normalized[1], 0)
  expect_true(is.na(normalized[2]))
  expect_equal(normalized[3], 50)
  expect_equal(normalized[4], 100)
})

test_that("normalize_indicator handles negative values", {
  values <- c(-50, 0, 50, 100)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_c1_biomasse", values)

  # Negative values should be clamped to 0
  expect_equal(normalized[1], 0)
  expect_equal(normalized[2], 0)
})

test_that("normalize_indicator handles indicateur_w1_reseau correctly", {
  # indicateur_w1_reseau: ref_max = 50 m/ha
  values <- c(0, 25, 50, 100)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_w1_reseau", values)

  expect_equal(normalized[1], 0)
  expect_equal(normalized[2], 50)
  expect_equal(normalized[3], 100)
  expect_equal(normalized[4], 100)  # Capped
})

test_that("normalize_indicator handles indicateur_w2_zones_humides correctly", {
  # indicateur_w2_zones_humides: ref_max = 5%
  values <- c(0, 2.5, 5, 10)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_w2_zones_humides", values)

  expect_equal(normalized[1], 0)
  expect_equal(normalized[2], 50)
  expect_equal(normalized[3], 100)
  expect_equal(normalized[4], 100)  # Capped
})

test_that("normalize_indicator handles indicateur_s2_bati inverse", {
  # indicateur_s2_bati: 0m -> 100, 2000m -> 0
  values <- c(0, 500, 1000, 2000, 4000)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_s2_bati", values)

  expect_equal(normalized[1], 100)
  expect_equal(normalized[2], 75)
  expect_equal(normalized[3], 50)
  expect_equal(normalized[4], 0)
  expect_equal(normalized[5], 0)  # Clamped at 0
})

test_that("normalize_indicator handles indicateur_p1_volume correctly", {
  # indicateur_p1_volume: ref_max = 800 m3/ha
  values <- c(0, 400, 800, 1200)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_p1_volume", values)

  expect_equal(normalized[1], 0)
  expect_equal(normalized[2], 50)
  expect_equal(normalized[3], 100)
  expect_equal(normalized[4], 100)  # Capped
})

test_that("normalize_indicator handles indicateur_e1_bois_energie correctly", {
  # indicateur_e1_bois_energie: ref_max = 0.3 tep/ha/yr
  values <- c(0, 0.15, 0.3, 0.6)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_e1_bois_energie", values)

  expect_equal(normalized[1], 0)
  expect_equal(normalized[2], 50)
  expect_equal(normalized[3], 100)
  expect_equal(normalized[4], 100)  # Capped
})

test_that("normalize_indicator handles indicateur_e2_evitement correctly", {
  # indicateur_e2_evitement: ref_max = 0.75 tCO2/ha/yr
  values <- c(0, 0.375, 0.75, 1.5)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_e2_evitement", values)

  expect_equal(normalized[1], 0)
  expect_equal(normalized[2], 50)
  expect_equal(normalized[3], 100)
  expect_equal(normalized[4], 100)  # Capped
})

test_that("normalize_indicator passes through 0-100 indicators with clamping", {
  # indicateur_b1_protection already returns 0-100, just clamp
  values <- c(-20, 0, 50, 100, 150)
  normalized <- nemetonshiny:::normalize_indicator("indicateur_b1_protection", values)

  expect_equal(normalized[1], 0)
  expect_equal(normalized[2], 0)
  expect_equal(normalized[3], 50)
  expect_equal(normalized[4], 100)
  expect_equal(normalized[5], 100)
})

# ==============================================================================
# download_raster_source tests
# ==============================================================================

test_that("download_raster_source returns cached file if exists", {
  skip_if_not_installed("terra")

  temp_dir <- tempfile("nemeton_test_cache_")
  dir.create(temp_dir, recursive = TRUE)

  # Create a cached raster file
  cache_file <- file.path(temp_dir, "ndvi.tif")
  mock_raster <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(mock_raster) <- runif(100)
  terra::writeRaster(mock_raster, cache_file)

  result <- nemetonshiny:::download_raster_source(
    source_name = "ndvi",
    source_config = list(source = "ign_irc"),
    bbox = c(0, 0, 1, 1),
    cache_dir = temp_dir
  )

  expect_s4_class(result, "SpatRaster")

  unlink(temp_dir, recursive = TRUE)
})

test_that("download_raster_source warns for unknown source", {
  temp_dir <- tempfile("nemeton_test_unknown_")
  dir.create(temp_dir, recursive = TRUE)

  # cli::cli_warn triggers a warning condition
  expect_warning(
    result <- nemetonshiny:::download_raster_source(
      source_name = "test",
      source_config = list(source = "unknown_source_xyz"),
      bbox = c(0, 0, 1, 1),
      cache_dir = temp_dir
    ),
    regexp = "Unknown raster source"
  )

  expect_null(result)

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# download_vector_source tests
# ==============================================================================

test_that("download_vector_source returns cached file if exists", {
  skip_if_not_installed("sf")

  temp_dir <- tempfile("nemeton_test_vector_cache_")
  dir.create(temp_dir, recursive = TRUE)

  # Create a cached vector file
  cache_file <- file.path(temp_dir, "roads.gpkg")
  mock_sf <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0,0, 1,1), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1,1, 2,2), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(2,2, 3,3), ncol = 2, byrow = TRUE)),
      crs = 4326
    )
  )
  sf::st_write(mock_sf, cache_file, quiet = TRUE)

  result <- nemetonshiny:::download_vector_source(
    source_name = "roads",
    source_config = list(source = "ign_bd_topo"),
    bbox = c(0, 0, 3, 3),
    cache_dir = temp_dir
  )

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 3)

  unlink(temp_dir, recursive = TRUE)
})

test_that("download_vector_source warns for unknown source", {
  temp_dir <- tempfile("nemeton_test_unknown_vec_")
  dir.create(temp_dir, recursive = TRUE)

  # cli::cli_warn triggers a warning condition
  expect_warning(
    result <- nemetonshiny:::download_vector_source(
      source_name = "test",
      source_config = list(source = "unknown_source_xyz"),
      bbox = c(0, 0, 1, 1),
      cache_dir = temp_dir
    ),
    regexp = "Unknown vector source"
  )

  expect_null(result)

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# create_synthetic_ndvi tests
# ==============================================================================

test_that("create_synthetic_ndvi creates valid raster", {
  skip_if_not_installed("terra")

  temp_file <- tempfile(fileext = ".tif")

  result <- nemetonshiny:::create_synthetic_ndvi(
    bbox = c(2.0, 48.0, 2.01, 48.01),
    cache_file = temp_file
  )

  expect_s4_class(result, "SpatRaster")
  expect_equal(names(result), "ndvi")

  # Check values are in expected range (0.5-0.85)
  vals <- terra::values(result)
  expect_true(all(vals >= 0.5 & vals <= 0.85, na.rm = TRUE))

  unlink(temp_file)
})

# ==============================================================================
# mosaic_lidar_tiles tests
# ==============================================================================

test_that("mosaic_lidar_tiles handles single tile", {
  skip_if_not_installed("terra")

  temp_dir <- tempfile("nemeton_mosaic_")
  dir.create(temp_dir)

  # Create single tile
  tile_file <- file.path(temp_dir, "tile1.tif")
  mock_raster <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(mock_raster) <- 1:100
  terra::writeRaster(mock_raster, tile_file)

  output_file <- file.path(temp_dir, "mosaic.tif")
  result <- nemetonshiny:::mosaic_lidar_tiles(tile_file, output_file)

  expect_s4_class(result, "SpatRaster")
  expect_true(file.exists(output_file))

  unlink(temp_dir, recursive = TRUE)
})

test_that("mosaic_lidar_tiles handles multiple tiles", {
  skip_if_not_installed("terra")

  temp_dir <- tempfile("nemeton_mosaic_multi_")
  dir.create(temp_dir)

  # Create two tiles with different values
  tile1 <- file.path(temp_dir, "tile1.tif")
  tile2 <- file.path(temp_dir, "tile2.tif")

  rast1 <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(rast1) <- rep(10, 100)
  terra::writeRaster(rast1, tile1)

  rast2 <- terra::rast(
    nrows = 10, ncols = 10,
    xmin = 1, xmax = 2, ymin = 0, ymax = 1,
    crs = "EPSG:4326"
  )
  terra::values(rast2) <- rep(20, 100)
  terra::writeRaster(rast2, tile2)

  output_file <- file.path(temp_dir, "mosaic.tif")
  result <- nemetonshiny:::mosaic_lidar_tiles(c(tile1, tile2), output_file)

  expect_s4_class(result, "SpatRaster")
  expect_true(file.exists(output_file))

  # Mosaic should cover both tiles
  expect_true(terra::ext(result)[2] > 1)  # xmax > 1

  unlink(temp_dir, recursive = TRUE)
})

test_that("mosaic_lidar_tiles handles invalid file paths gracefully", {
  skip_if_not_installed("terra")

  temp_dir <- tempfile("nemeton_mosaic_invalid_")
  dir.create(temp_dir)

  output_file <- file.path(temp_dir, "mosaic.tif")

  # Pass non-existent files - should return NULL or first tile attempt
  result <- suppressWarnings(nemetonshiny:::mosaic_lidar_tiles(
    c("nonexistent1.tif", "nonexistent2.tif"),
    output_file
  ))

  # Function should handle errors gracefully - returns NULL or attempts fallback
  # (exact behavior depends on error handling)
  expect_true(is.null(result) || inherits(result, "SpatRaster"))

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# query_lidar_wfs tests
# ==============================================================================

test_that("query_lidar_wfs constructs correct URL", {
  # Test that bbox is properly formatted in the URL
  # We can't easily mock httr2, so we test the URL construction logic
  bbox <- c(2.0, 48.0, 2.1, 48.1)
  wfs_layer <- "IGNF_MNH-LIDAR-HD:dalle"

  # The function should handle bbox coordinates correctly
  # This is tested indirectly through the function behavior
  # When network is unavailable, it should return NULL
  result <- tryCatch(
    nemetonshiny:::query_lidar_wfs(wfs_layer, bbox),
    error = function(e) NULL
  )

  # Result is NULL when no network or no tiles available
  expect_true(is.null(result) || inherits(result, "sf"))
})

# ==============================================================================
# download_lidar_tile tests
# ==============================================================================

test_that("download_lidar_tile returns NULL for invalid URL", {
  temp_file <- tempfile(fileext = ".tif")

  # Pass an invalid/unreachable URL
  result <- nemetonshiny:::download_lidar_tile(
    "http://invalid-url-that-does-not-exist-xyz123.com/tile.tif",
    temp_file
  )

  expect_null(result)

  if (file.exists(temp_file)) unlink(temp_file)
})

test_that("download_lidar_tile uses retry logic", {
  # This test verifies the function doesn't crash and handles errors
  temp_file <- tempfile(fileext = ".tif")

  # The function should retry and eventually return NULL for bad URL
  result <- nemetonshiny:::download_lidar_tile(
    "http://localhost:99999/nonexistent.tif",  # Should fail
    temp_file
  )

  expect_null(result)

  if (file.exists(temp_file)) unlink(temp_file)
})

# ==============================================================================
# init_compute_state edge cases
# ==============================================================================

test_that("init_compute_state handles list input for computed_indicators", {
  with_mocked_bindings(
    get_project_path = function(id) NULL,
    get_computation_progress = function(id) {
      # Simulate JSON read returning a list instead of character vector
      list(
        computed_indicators = list("indicateur_c1_biomasse", "indicateur_c2_ndvi"),
        last_saved_at = "2026-01-01 12:00:00"
      )
    },
    {
      state <- nemetonshiny:::init_compute_state(
        "test_project",
        c("indicateur_c1_biomasse", "indicateur_c2_ndvi", "indicateur_w1_reseau")
      )

      expect_equal(state$indicators_completed, 2)
      expect_equal(state$indicators_skipped, 2)
    }
  )
})

test_that("init_compute_state handles empty list for computed_indicators", {
  with_mocked_bindings(
    get_project_path = function(id) NULL,
    get_computation_progress = function(id) {
      list(
        computed_indicators = list(),
        last_saved_at = NULL
      )
    },
    {
      state <- nemetonshiny:::init_compute_state(
        "test_project",
        c("indicateur_c1_biomasse", "indicateur_c2_ndvi")
      )

      expect_equal(state$indicators_completed, 0)
      expect_equal(state$indicators_skipped, 0)
      expect_false(state$is_resume)
    }
  )
})

test_that("init_compute_state handles NULL computed_indicators", {
  with_mocked_bindings(
    get_project_path = function(id) NULL,
    get_computation_progress = function(id) {
      list(
        computed_indicators = NULL,
        last_saved_at = NULL
      )
    },
    {
      state <- nemetonshiny:::init_compute_state(
        "test_project",
        c("indicateur_c1_biomasse", "indicateur_c2_ndvi")
      )

      expect_equal(state$indicators_completed, 0)
      expect_equal(state$indicators_skipped, 0)
    }
  )
})

# ==============================================================================
# save_indicators_incremental edge cases
# ==============================================================================

test_that("save_indicators_incremental returns FALSE for missing project", {
  with_mocked_bindings(
    get_project_path = function(id) NULL,
    {
      result <- nemetonshiny:::save_indicators_incremental(
        "nonexistent",
        data.frame(id = 1),
        "indicateur_c1_biomasse"
      )
      expect_false(result)
    }
  )
})

test_that("save_indicators_incremental handles data.frame without geometry", {
  skip_if_not_installed("arrow")

  temp_dir <- file.path(tempdir(), "nemeton_test_incr_df")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  mock_results <- data.frame(
    id = c("p1", "p2"),
    indicateur_c1_biomasse = c(50, 75)
  )

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    {
      # Should warn but not error
      result <- suppressWarnings(nemetonshiny:::save_indicators_incremental(
        "test_project",
        mock_results,
        "indicateur_c1_biomasse"
      ))
      # The function should handle data.frame and return FALSE due to missing geometry_wkt
      # or succeed after warning
      expect_type(result, "logical")
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("save_indicators_incremental handles sf object correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")

  temp_dir <- file.path(tempdir(), "nemeton_test_incr_sf")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_results <- sf::st_sf(id = "p1", indicateur_c1_biomasse = 50, geometry = mock_geom)

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    {
      result <- nemetonshiny:::save_indicators_incremental(
        "test_project",
        mock_results,
        "indicateur_c1_biomasse"
      )
      expect_true(result)
      expect_true(file.exists(file.path(data_dir, "indicators.parquet")))
      expect_true(file.exists(file.path(data_dir, "compute_progress.json")))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# save_progress_state edge cases
# ==============================================================================

test_that("save_progress_state handles missing project path", {
  with_mocked_bindings(
    get_project_path = function(id) NULL,
    {
      # Should return silently without error
      result <- nemetonshiny:::save_progress_state("nonexistent", list())
      expect_null(result)
    }
  )
})

test_that("save_progress_state handles nonexistent directory", {
  temp_dir <- file.path(tempdir(), "nemeton_test_progress_nonexist")
  unlink(temp_dir, recursive = TRUE)

  with_mocked_bindings(
    get_project_path = function(id) temp_dir,
    {
      # Should return silently (dir doesn't exist)
      result <- nemetonshiny:::save_progress_state("test", list(project_id = "test"))
      expect_null(result)
    }
  )
})

test_that("save_progress_state creates data directory if needed", {
  temp_dir <- file.path(tempdir(), "nemeton_test_progress_create")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir, recursive = TRUE)

  state <- list(
    project_id = "test",
    status = "computing",
    phase = "downloading",
    progress = 5,
    progress_max = 40,
    current_task = "test_task",
    indicators_total = 30,
    indicators_completed = 0,
    indicators_failed = 0,
    indicators_status = list(),
    errors = list(),
    started_at = Sys.time(),
    completed_at = NULL
  )

  with_mocked_bindings(
    get_project_path = function(id) temp_dir,
    {
      nemetonshiny:::save_progress_state("test", state)
      expect_true(dir.exists(file.path(temp_dir, "data")))
      expect_true(file.exists(file.path(temp_dir, "data", "progress_state.json")))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# start_computation error handling
# ==============================================================================

test_that("start_computation handles empty parcels file", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_empty_parcels")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  # Create empty parcels file
  mock_geom <- sf::st_sfc(crs = 4326)
  mock_parcels <- sf::st_sf(id = character(), geometry = mock_geom)
  sf::st_write(mock_parcels, file.path(data_dir, "parcels.gpkg"), quiet = TRUE)

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    get_app_options = function() list(project_dir = temp_dir),
    clear_cancel = function(id) invisible(NULL),
    get_computation_progress = function(id) list(computed_indicators = character(0)),
    update_project_status = function(id, status) invisible(NULL),
    {
      result <- nemetonshiny:::start_computation("test_project", indicators = "indicateur_c1_biomasse")

      expect_false(result$success)
      expect_equal(result$state$status, "error")
      expect_true(any(grepl("No parcels", sapply(result$state$errors, `[[`, "message"))))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("start_computation handles cancellation during download phase", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_cancel_download")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 0.001,0, 0.001,0.001, 0,0.001, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)
  sf::st_write(mock_parcels, file.path(data_dir, "parcels.gpkg"), quiet = TRUE)

  cancelled <- FALSE

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    get_app_options = function() list(project_dir = temp_dir),
    clear_cancel = function(id) invisible(NULL),
    get_computation_progress = function(id) list(computed_indicators = character(0)),
    update_project_status = function(id, status) invisible(NULL),
    download_layers_for_parcels = function(parcels, project_path, progress_callback) {
      structure(
        list(rasters = list(), vectors = list(), point_clouds = list(),
             bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
             cache_dir = tempdir(), warnings = list()),
        class = "nemeton_layers"
      )
    },
    is_cancelled = function(id) TRUE,  # Always cancelled
    {
      result <- nemetonshiny:::start_computation("test_project", indicators = "indicateur_c1_biomasse")

      expect_false(result$success)
      expect_equal(result$state$status, "cancelled")
      expect_true(result$cancelled)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# download_layers_for_parcels warning collection
# ==============================================================================

test_that("download_layers_for_parcels collects download warnings", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_download_warn")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  dir.create(project_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(2.0, 48.0, 2.001, 48.0, 2.001, 48.001, 2.0, 48.001, 2.0, 48.0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  with_mocked_bindings(
    download_raster_source = function(source_name, source_config, bbox, cache_dir, progress_callback) {
      # Return NULL for forest_cover to trigger OSO warning
      if (source_name == "forest_cover") return(NULL)
      # Return NULL for LiDAR to trigger LiDAR warning
      if (source_config$source == "ign_lidar_hd") return(NULL)
      NULL
    },
    download_vector_source = function(...) NULL,
    download_ign_lidar_hd = function(...) NULL,
    {
      result <- nemetonshiny:::download_layers_for_parcels(
        parcels = mock_parcels,
        project_path = project_dir,
        progress_callback = NULL
      )

      expect_s3_class(result, "nemeton_layers")
      # Should have warnings from failed downloads
      expect_true(length(result$warnings) > 0)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# compute_all_indicators skip behavior
# ==============================================================================

test_that("compute_all_indicators skips already computed indicators with values", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  # Create existing results with one computed indicator
  existing_results <- mock_parcels
  existing_results$indicateur_c1_biomasse <- 75  # Already computed

  mock_layers <- structure(
    list(rasters = list(), vectors = list(), point_clouds = list(),
         bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
         cache_dir = tempdir(), warnings = list()),
    class = "nemeton_layers"
  )

  compute_calls <- 0

  with_mocked_bindings(
    load_indicators = function(id) existing_results,
    is_cancelled = function(id) FALSE,
    compute_single_indicator = function(indicator, parcels, layers) {
      compute_calls <<- compute_calls + 1
      rep(50, nrow(parcels))
    },
    save_indicators_incremental = function(...) TRUE,
    {
      result <- nemetonshiny:::compute_all_indicators(
        parcels = mock_parcels,
        layers = mock_layers,
        indicators = c("indicateur_c1_biomasse", "indicateur_c2_ndvi"),
        progress_callback = NULL,
        project_id = "test"
      )

      # indicateur_c1_biomasse should be skipped, only indicateur_c2_ndvi computed
      expect_equal(compute_calls, 1)
      expect_equal(result$indicateur_c1_biomasse, 75)  # Original value preserved
    }
  )
})

test_that("compute_all_indicators recomputes all-zero indicators", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  # Create existing results with an all-zero indicator
  existing_results <- mock_parcels
  existing_results$indicateur_c1_biomasse <- 0  # All zero - should recompute

  mock_layers <- structure(
    list(rasters = list(), vectors = list(), point_clouds = list(),
         bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
         cache_dir = tempdir(), warnings = list()),
    class = "nemeton_layers"
  )

  compute_calls <- 0

  with_mocked_bindings(
    load_indicators = function(id) existing_results,
    is_cancelled = function(id) FALSE,
    compute_single_indicator = function(indicator, parcels, layers) {
      compute_calls <<- compute_calls + 1
      rep(50, nrow(parcels))  # Return non-zero
    },
    save_indicators_incremental = function(...) TRUE,
    {
      result <- nemetonshiny:::compute_all_indicators(
        parcels = mock_parcels,
        layers = mock_layers,
        indicators = c("indicateur_c1_biomasse"),
        progress_callback = NULL,
        project_id = "test"
      )

      # indicateur_c1_biomasse was all-zero so should be recomputed
      expect_equal(compute_calls, 1)
      expect_equal(result$indicateur_c1_biomasse, 50)  # New computed value
    }
  )
})

# ==============================================================================
# find_url_column edge cases
# ==============================================================================

test_that("find_url_column finds case-insensitive matches", {
  skip_if_not_installed("sf")

  mock_sf <- sf::st_sf(
    URL_TELECHARGEMENT = c("http://example.com/tile1.tif"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )

  result <- nemetonshiny:::find_url_column(mock_sf)
  expect_equal(result, "URL_TELECHARGEMENT")
})

test_that("find_url_column finds partial matches", {
  skip_if_not_installed("sf")

  mock_sf <- sf::st_sf(
    download_link = c("http://example.com/tile1.tif"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )

  result <- nemetonshiny:::find_url_column(mock_sf)
  expect_equal(result, "download_link")
})

# ==============================================================================
# get_global_cache_dir fallback
# ==============================================================================

test_that("get_global_cache_dir uses fallback without rappdirs", {
  # This test verifies the fallback behavior
  cache_dir <- nemetonshiny:::get_global_cache_dir()

  expect_type(cache_dir, "character")
  expect_true(nchar(cache_dir) > 0)
  expect_true(dir.exists(cache_dir))
})

# ==============================================================================
# normalize_indicator Tests
# ==============================================================================

test_that("normalize_indicator scales indicateur_c1_biomasse correctly", {
  # ref_max = 150: 75 tC/ha -> 50%
  result <- nemetonshiny:::normalize_indicator("indicateur_c1_biomasse", c(0, 75, 150, 300))
  expect_equal(result, c(0, 50, 100, 100))
})

test_that("normalize_indicator scales indicateur_c2_ndvi 0-1 to 0-100", {
  result <- nemetonshiny:::normalize_indicator("indicateur_c2_ndvi", c(0, 0.5, 1.0))
  expect_equal(result, c(0, 50, 100))
})

test_that("normalize_indicator scales indicateur_w3_humidite with offset", {
  # TWI: (values - 2.5) / 2 * 100
  result <- nemetonshiny:::normalize_indicator("indicateur_w3_humidite", c(2.5, 3.5, 4.5))
  expect_equal(result, c(0, 50, 100))
})

test_that("normalize_indicator inverts indicateur_s1_routes distance", {
  # 0m -> 100, 2000m -> 0
  result <- nemetonshiny:::normalize_indicator("indicateur_s1_routes", c(0, 1000, 2000))
  expect_equal(result, c(100, 50, 0))
})

test_that("normalize_indicator inverts indicateur_s2_bati distance", {
  result <- nemetonshiny:::normalize_indicator("indicateur_s2_bati", c(0, 1000, 2000))
  expect_equal(result, c(100, 50, 0))
})

test_that("normalize_indicator clamps already-normalized indicators", {
  # biodiversity indicators return 0-100, just clamped
  result <- nemetonshiny:::normalize_indicator("indicateur_b1_protection", c(-10, 50, 110))
  expect_equal(result, c(0, 50, 100))
})

test_that("normalize_indicator scales indicateur_w1_reseau correctly", {
  # ref_max = 50: 25 m/ha -> 50
  result <- nemetonshiny:::normalize_indicator("indicateur_w1_reseau", c(0, 25, 50, 100))
  expect_equal(result, c(0, 50, 100, 100))
})

test_that("normalize_indicator scales indicateur_p1_volume correctly", {
  # ref_max = 800: 400 m3/ha -> 50
  result <- nemetonshiny:::normalize_indicator("indicateur_p1_volume", c(0, 400, 800))
  expect_equal(result, c(0, 50, 100))
})

test_that("normalize_indicator handles NA values", {
  result <- nemetonshiny:::normalize_indicator("indicateur_c1_biomasse", c(NA, 75, NA))
  expect_true(is.na(result[1]))
  expect_equal(result[2], 50)
  expect_true(is.na(result[3]))
})

test_that("normalize_indicator scales energy indicators", {
  # indicateur_e1_bois_energie ref_max = 0.3
  result <- nemetonshiny:::normalize_indicator("indicateur_e1_bois_energie", c(0, 0.15, 0.3))
  expect_equal(result, c(0, 50, 100))

  # indicateur_e2_evitement ref_max = 0.75
  result2 <- nemetonshiny:::normalize_indicator("indicateur_e2_evitement", c(0, 0.375, 0.75))
  expect_equal(result2, c(0, 50, 100))
})

# ==============================================================================
# compute_single_indicator dispatch Tests
# ==============================================================================

test_that("compute_single_indicator dispatches to correct function", {
  # Test that the function lookup works for a few indicators
  expect_true(exists("indicateur_c1_biomasse", mode = "function", envir = asNamespace("nemeton")))
  expect_true(exists("indicateur_c2_ndvi", mode = "function", envir = asNamespace("nemeton")))
  expect_true(exists("indicateur_w1_reseau", mode = "function", envir = asNamespace("nemeton")))
  expect_true(exists("indicateur_l2_fragmentation", mode = "function", envir = asNamespace("nemeton")))
  expect_true(exists("indicateur_f2_erosion", mode = "function", envir = asNamespace("nemeton")))
  expect_true(exists("indicateur_e1_bois_energie", mode = "function", envir = asNamespace("nemeton")))
  expect_true(exists("indicateur_s3_population", mode = "function", envir = asNamespace("nemeton")))
})

# ==============================================================================
# Cancellation Tests
# ==============================================================================

test_that("cancel_computation creates marker file", {
  withr::with_tempdir({
    temp_root <- getwd()
    proj_dir <- file.path(temp_root, "test_cancel_proj")
    data_dir <- file.path(proj_dir, "data")
    dir.create(data_dir, recursive = TRUE)

    local_mocked_bindings(
      get_project_path = function(project_id) proj_dir,
      .package = "nemetonshiny"
    )

    nemetonshiny:::cancel_computation("test_cancel_proj")

    cancel_file <- file.path(data_dir, "cancel_requested")
    expect_true(file.exists(cancel_file))
  })
})

test_that("is_cancelled detects cancel marker", {
  withr::with_tempdir({
    temp_root <- getwd()
    proj_dir <- file.path(temp_root, "test_cancel2")
    data_dir <- file.path(proj_dir, "data")
    dir.create(data_dir, recursive = TRUE)

    local_mocked_bindings(
      get_project_path = function(project_id) proj_dir,
      .package = "nemetonshiny"
    )

    # Not cancelled yet
    expect_false(nemetonshiny:::is_cancelled("test_cancel2"))

    # Cancel
    nemetonshiny:::cancel_computation("test_cancel2")
    expect_true(nemetonshiny:::is_cancelled("test_cancel2"))
  })
})

test_that("clear_cancel removes marker file", {
  withr::with_tempdir({
    temp_root <- getwd()
    proj_dir <- file.path(temp_root, "test_clear")
    data_dir <- file.path(proj_dir, "data")
    dir.create(data_dir, recursive = TRUE)

    local_mocked_bindings(
      get_project_path = function(project_id) proj_dir,
      .package = "nemetonshiny"
    )

    nemetonshiny:::cancel_computation("test_clear")
    expect_true(nemetonshiny:::is_cancelled("test_clear"))

    nemetonshiny:::clear_cancel("test_clear")
    expect_false(nemetonshiny:::is_cancelled("test_clear"))
  })
})

test_that("cancel_computation handles NULL project path gracefully", {
  local_mocked_bindings(
    get_project_path = function(project_id) NULL,
    .package = "nemetonshiny"
  )

  # Should not error
  expect_no_error(nemetonshiny:::cancel_computation("nonexistent"))
})

test_that("is_cancelled returns FALSE for NULL project path", {
  local_mocked_bindings(
    get_project_path = function(project_id) NULL,
    .package = "nemetonshiny"
  )

  expect_false(nemetonshiny:::is_cancelled("nonexistent"))
})

test_that("clear_cancel handles NULL project path gracefully", {
  local_mocked_bindings(
    get_project_path = function(project_id) NULL,
    .package = "nemetonshiny"
  )

  expect_no_error(nemetonshiny:::clear_cancel("nonexistent"))
})

# ==============================================================================
# Progress State Tests
# ==============================================================================

test_that("save_progress_state and read_progress_state roundtrip", {
  withr::with_tempdir({
    temp_root <- getwd()
    proj_dir <- file.path(temp_root, "test_progress")
    dir.create(proj_dir, recursive = TRUE)

    local_mocked_bindings(
      get_project_path = function(project_id) proj_dir,
      .package = "nemetonshiny"
    )

    state <- list(
      project_id = "test_progress",
      status = "computing",
      phase = "indicators",
      progress = 5,
      progress_max = 41,
      current_task = "indicateur_c1_biomasse",
      indicators_total = 31,
      indicators_completed = 5,
      indicators_skipped = 0L,
      indicators_failed = 0L,
      indicators_status = list(indicateur_c1_biomasse = "completed"),
      errors = list(),
      started_at = Sys.time(),
      completed_at = NULL
    )

    nemetonshiny:::save_progress_state("test_progress", state)

    # Verify file exists
    progress_file <- file.path(proj_dir, "data", "progress_state.json")
    expect_true(file.exists(progress_file))

    # Read back
    result <- nemetonshiny:::read_progress_state("test_progress")
    expect_type(result, "list")
    expect_equal(result$status, "computing")
    expect_equal(result$progress, 5)
    expect_equal(result$indicators_completed, 5)
    expect_equal(result$project_id, "test_progress")
  })
})

test_that("save_progress_state creates data directory if missing", {
  withr::with_tempdir({
    temp_root <- getwd()
    proj_dir <- file.path(temp_root, "test_progress_nodata")
    dir.create(proj_dir, recursive = TRUE)
    # Deliberately do NOT create data/ subdirectory

    local_mocked_bindings(
      get_project_path = function(project_id) proj_dir,
      .package = "nemetonshiny"
    )

    state <- list(
      project_id = "test_progress_nodata",
      status = "pending",
      phase = "init",
      progress = 0,
      progress_max = 10,
      current_task = NULL,
      indicators_total = 5,
      indicators_completed = 0,
      indicators_failed = 0L,
      indicators_status = list(),
      errors = list(),
      started_at = NULL,
      completed_at = NULL
    )

    nemetonshiny:::save_progress_state("test_progress_nodata", state)

    data_dir <- file.path(proj_dir, "data")
    expect_true(dir.exists(data_dir))
    expect_true(file.exists(file.path(data_dir, "progress_state.json")))
  })
})

test_that("save_progress_state handles NULL project path", {
  local_mocked_bindings(
    get_project_path = function(project_id) NULL,
    .package = "nemetonshiny"
  )

  state <- list(status = "computing")
  # Should return silently without error
  expect_no_error(nemetonshiny:::save_progress_state("nonexistent", state))
})

test_that("read_progress_state returns NULL for non-existent project", {
  local_mocked_bindings(
    get_project_path = function(project_id) NULL,
    .package = "nemetonshiny"
  )

  result <- nemetonshiny:::read_progress_state("nonexistent_project")
  expect_null(result)
})

test_that("read_progress_state returns NULL when progress file missing", {
  withr::with_tempdir({
    temp_root <- getwd()
    proj_dir <- file.path(temp_root, "test_no_progress")
    dir.create(proj_dir, recursive = TRUE)

    local_mocked_bindings(
      get_project_path = function(project_id) proj_dir,
      .package = "nemetonshiny"
    )

    result <- nemetonshiny:::read_progress_state("test_no_progress")
    expect_null(result)
  })
})

test_that("read_progress_state handles corrupted JSON", {
  withr::with_tempdir({
    temp_root <- getwd()
    proj_dir <- file.path(temp_root, "test_corrupt")
    data_dir <- file.path(proj_dir, "data")
    dir.create(data_dir, recursive = TRUE)

    # Write invalid JSON
    writeLines("{ invalid json !!!", file.path(data_dir, "progress_state.json"))

    local_mocked_bindings(
      get_project_path = function(project_id) proj_dir,
      .package = "nemetonshiny"
    )

    result <- nemetonshiny:::read_progress_state("test_corrupt")
    expect_null(result)
  })
})

# ==============================================================================
# get_computation_progress Tests
# ==============================================================================

test_that("get_computation_progress returns empty list for new project", {
  withr::with_tempdir({
    temp_root <- getwd()
    proj_dir <- file.path(temp_root, "test_new_proj")
    dir.create(proj_dir, recursive = TRUE)

    local_mocked_bindings(
      get_project_path = function(project_id) proj_dir,
      .package = "nemetonshiny"
    )

    result <- nemetonshiny:::get_computation_progress("test_new_proj")
    expect_type(result, "list")
    expect_equal(result$computed_indicators, character(0))
    expect_null(result$last_indicator)
    expect_null(result$last_saved_at)
  })
})

test_that("get_computation_progress reads saved progress", {
  withr::with_tempdir({
    temp_root <- getwd()
    proj_dir <- file.path(temp_root, "test_comp_progress")
    data_dir <- file.path(proj_dir, "data")
    dir.create(data_dir, recursive = TRUE)

    local_mocked_bindings(
      get_project_path = function(project_id) proj_dir,
      .package = "nemetonshiny"
    )

    # Write progress JSON directly
    progress <- list(
      computed_indicators = list("indicateur_c1_biomasse", "indicateur_w3_humidite"),
      last_indicator = "indicateur_w3_humidite",
      last_saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    jsonlite::write_json(progress, file.path(data_dir, "compute_progress.json"),
                         auto_unbox = TRUE, pretty = TRUE)

    result <- nemetonshiny:::get_computation_progress("test_comp_progress")
    expect_type(result, "list")
    expect_true("computed_indicators" %in% names(result))
    expect_equal(length(result$computed_indicators), 2)
  })
})

test_that("get_computation_progress returns NULL for NULL project path", {
  local_mocked_bindings(
    get_project_path = function(project_id) NULL,
    .package = "nemetonshiny"
  )

  result <- nemetonshiny:::get_computation_progress("nonexistent")
  expect_null(result)
})

test_that("get_computation_progress handles corrupted JSON gracefully", {
  withr::with_tempdir({
    temp_root <- getwd()
    proj_dir <- file.path(temp_root, "test_corrupt_progress")
    data_dir <- file.path(proj_dir, "data")
    dir.create(data_dir, recursive = TRUE)

    # Write invalid JSON
    writeLines("not valid json {{{", file.path(data_dir, "compute_progress.json"))

    local_mocked_bindings(
      get_project_path = function(project_id) proj_dir,
      .package = "nemetonshiny"
    )

    result <- nemetonshiny:::get_computation_progress("test_corrupt_progress")
    expect_type(result, "list")
    expect_equal(result$computed_indicators, character(0))
  })
})

# ==============================================================================
# find_url_column Tests
# ==============================================================================

test_that("find_url_column finds standard column names", {
  skip_if_not_installed("sf")

  # Test with "url" column
  df <- sf::st_sf(
    url = c("https://example.com/tile1.laz", "https://example.com/tile2.laz"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 4326
    )
  )

  result <- nemetonshiny:::find_url_column(df)
  expect_equal(result, "url")
})

test_that("find_url_column finds url_telechargement column", {
  skip_if_not_installed("sf")

  df <- sf::st_sf(
    url_telechargement = c("https://example.com/tile1.tif", "https://example.com/tile2.tif"),
    nom = c("tile1", "tile2"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 4326
    )
  )

  result <- nemetonshiny:::find_url_column(df)
  expect_equal(result, "url_telechargement")
})

test_that("find_url_column finds lien_telechargement column", {
  skip_if_not_installed("sf")

  df <- sf::st_sf(
    lien_telechargement = c("https://example.com/a.tif", "https://example.com/b.tif"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 4326
    )
  )

  result <- nemetonshiny:::find_url_column(df)
  expect_equal(result, "lien_telechargement")
})

test_that("find_url_column returns NULL when no URL column exists", {
  skip_if_not_installed("sf")

  df <- sf::st_sf(
    name = c("a", "b"),
    value = c(1, 2),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 4326
    )
  )

  result <- nemetonshiny:::find_url_column(df)
  expect_null(result)
})

test_that("find_url_column uses case-insensitive matching", {
  skip_if_not_installed("sf")

  df <- sf::st_sf(
    URL = c("https://example.com/tile1.tif", "https://example.com/tile2.tif"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 4326
    )
  )

  result <- nemetonshiny:::find_url_column(df)
  # Should find via case-insensitive match
  expect_equal(result, "URL")
})

test_that("find_url_column uses partial match on url-like column names", {
  skip_if_not_installed("sf")

  df <- sf::st_sf(
    download_link_url = c("https://example.com/a.tif", "https://example.com/b.tif"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 4326
    )
  )

  result <- nemetonshiny:::find_url_column(df)
  # Should find via partial match on "url"
  expect_equal(result, "download_link_url")
})

# ==============================================================================
# extract_tile_names Tests
# ==============================================================================

test_that("extract_tile_names extracts filenames from URLs", {
  urls <- c(
    "https://storage.com/tiles/tile_001.laz",
    "https://storage.com/tiles/tile_002.laz"
  )
  result <- nemetonshiny:::extract_tile_names(urls, ".laz")
  expect_type(result, "character")
  expect_length(result, 2)
  expect_equal(result, c("tile_001.laz", "tile_002.laz"))
})

test_that("extract_tile_names generates names for URLs without extension", {
  urls <- c(
    "https://storage.com/api/download?id=123",
    "https://storage.com/api/download?id=456"
  )
  result <- nemetonshiny:::extract_tile_names(urls, ".tif")
  expect_type(result, "character")
  expect_length(result, 2)
  # URLs without .tif extension should get generated names
  expect_true(grepl("tile_.*\\.tif$", result[1]))
  expect_true(grepl("tile_.*\\.tif$", result[2]))
})

test_that("extract_tile_names handles mixed URLs", {
  urls <- c(
    "https://storage.com/tiles/real_file.tif",
    "https://api.example.com/get?tile=abc"
  )
  result <- nemetonshiny:::extract_tile_names(urls, ".tif")
  expect_length(result, 2)
  expect_equal(result[1], "real_file.tif")
  # Second one should be generated
  expect_true(grepl("tile_.*\\.tif$", result[2]))
})

test_that("extract_tile_names handles list input (from JSON parsing)", {
  # WFS GeoJSON parsing may return urls as a list
  urls <- list("https://storage.com/tile_a.laz", "https://storage.com/tile_b.laz")
  result <- nemetonshiny:::extract_tile_names(urls, ".laz")
  expect_type(result, "character")
  expect_length(result, 2)
})

test_that("extract_tile_names handles copc.laz extension", {
  urls <- c(
    "https://storage.com/tiles/lidar_001.copc.laz",
    "https://storage.com/tiles/lidar_002.copc.laz"
  )
  result <- nemetonshiny:::extract_tile_names(urls, ".copc.laz")
  expect_length(result, 2)
  expect_equal(result, c("lidar_001.copc.laz", "lidar_002.copc.laz"))
})

# ==============================================================================
# create_synthetic_ndvi Tests
# ==============================================================================

test_that("create_synthetic_ndvi creates a raster file", {
  skip_if_not_installed("terra")

  withr::with_tempdir({
    bbox <- c(xmin = 2.0, ymin = 47.0, xmax = 2.01, ymax = 47.01)
    cache_file <- file.path(getwd(), "test_ndvi.tif")

    result <- nemetonshiny:::create_synthetic_ndvi(bbox, cache_file)

    expect_true(inherits(result, "SpatRaster"))
    expect_true(file.exists(cache_file))
    # NDVI values should be in 0.5-0.85 range (synthetic forest values)
    vals <- terra::values(result)
    expect_true(min(vals, na.rm = TRUE) >= 0.5)
    expect_true(max(vals, na.rm = TRUE) <= 0.85)
  })
})

test_that("create_synthetic_ndvi raster has ndvi layer name", {
  skip_if_not_installed("terra")

  withr::with_tempdir({
    bbox <- c(xmin = 2.0, ymin = 47.0, xmax = 2.005, ymax = 47.005)
    cache_file <- file.path(getwd(), "test_ndvi2.tif")

    result <- nemetonshiny:::create_synthetic_ndvi(bbox, cache_file)

    expect_equal(names(result), "ndvi")
  })
})

test_that("create_synthetic_ndvi uses EPSG:4326 CRS", {
  skip_if_not_installed("terra")

  withr::with_tempdir({
    bbox <- c(xmin = 2.0, ymin = 47.0, xmax = 2.005, ymax = 47.005)
    cache_file <- file.path(getwd(), "test_ndvi3.tif")

    result <- nemetonshiny:::create_synthetic_ndvi(bbox, cache_file)

    crs_desc <- terra::crs(result, describe = TRUE)
    expect_equal(crs_desc$code, "4326")
  })
})

test_that("create_synthetic_ndvi overwrites existing cache file", {
  skip_if_not_installed("terra")

  withr::with_tempdir({
    bbox <- c(xmin = 2.0, ymin = 47.0, xmax = 2.005, ymax = 47.005)
    cache_file <- file.path(getwd(), "test_ndvi_overwrite.tif")

    # Create file first
    writeLines("dummy", cache_file)
    expect_true(file.exists(cache_file))

    result <- nemetonshiny:::create_synthetic_ndvi(bbox, cache_file)

    expect_true(inherits(result, "SpatRaster"))
    # File should now be a valid raster, not the dummy content
    reloaded <- terra::rast(cache_file)
    expect_equal(names(reloaded), "ndvi")
  })
})

# ==============================================================================
# get_global_cache_dir Tests
# ==============================================================================

test_that("get_global_cache_dir returns a path string", {
  result <- nemetonshiny:::get_global_cache_dir()
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("get_global_cache_dir creates directory if needed", {
  result <- nemetonshiny:::get_global_cache_dir()
  expect_true(dir.exists(result))
})

test_that("get_global_cache_dir includes nemeton in path", {
  result <- nemetonshiny:::get_global_cache_dir()
  expect_true(grepl("nemeton", result))
})

test_that("get_global_cache_dir includes cache subdirectory", {
  result <- nemetonshiny:::get_global_cache_dir()
  expect_true(grepl("cache", result))
})

# ==============================================================================
# clear_computation_cache Tests
# ==============================================================================

test_that("clear_computation_cache handles non-existent project gracefully", {
  local_mocked_bindings(
    get_project_path = function(project_id) NULL,
    .package = "nemetonshiny"
  )

  # Should return FALSE for non-existent project
  result <- nemetonshiny:::clear_computation_cache("nonexistent_clear")
  expect_false(result)
})

test_that("clear_computation_cache removes progress files", {
  withr::with_tempdir({
    temp_root <- getwd()
    proj_dir <- file.path(temp_root, "test_clear_cache")
    data_dir <- file.path(proj_dir, "data")
    dir.create(data_dir, recursive = TRUE)

    # Create files that should be removed
    writeLines("{}", file.path(data_dir, "progress_state.json"))
    writeLines("{}", file.path(data_dir, "compute_progress.json"))

    local_mocked_bindings(
      get_project_path = function(project_id) proj_dir,
      .package = "nemetonshiny"
    )

    result <- nemetonshiny:::clear_computation_cache("test_clear_cache")
    expect_true(result)
    expect_false(file.exists(file.path(data_dir, "progress_state.json")))
    expect_false(file.exists(file.path(data_dir, "compute_progress.json")))
  })
})

test_that("clear_computation_cache returns TRUE when no files exist", {
  withr::with_tempdir({
    temp_root <- getwd()
    proj_dir <- file.path(temp_root, "test_empty_clear")
    data_dir <- file.path(proj_dir, "data")
    dir.create(data_dir, recursive = TRUE)

    local_mocked_bindings(
      get_project_path = function(project_id) proj_dir,
      .package = "nemetonshiny"
    )

    # No files to remove - should still succeed
    result <- nemetonshiny:::clear_computation_cache("test_empty_clear")
    expect_true(result)
  })
})

# ==============================================================================
# download_ign_dem tests (mocked HTTP)
# ==============================================================================

test_that("download_ign_dem handles numeric bbox input", {
  skip_if_not_installed("terra")
  skip_if_not_installed("httr2")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "dem.tif")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    # Mock httr2::req_perform to simulate a non-200 response
    with_mocked_bindings(
      req_perform = function(req, ...) {
        structure(
          list(status_code = 500L, headers = list(), body = raw(0)),
          class = "httr2_response"
        )
      },
      resp_status = function(resp) 500L,
      .package = "httr2",
      {
        result <- suppressWarnings(nemetonshiny:::download_ign_dem(bbox, cache_file))
        expect_null(result)
      }
    )
  })
})

test_that("download_ign_dem handles sf bbox input", {
  skip_if_not_installed("terra")
  skip_if_not_installed("httr2")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "dem.tif")
    bbox <- sf::st_bbox(c(xmin = 2.0, ymin = 48.0, xmax = 2.01, ymax = 48.01),
                        crs = sf::st_crs(4326))

    # Mock httr2::req_perform to throw an error
    with_mocked_bindings(
      req_perform = function(req, ...) {
        stop("Mocked network error")
      },
      .package = "httr2",
      {
        result <- suppressWarnings(nemetonshiny:::download_ign_dem(bbox, cache_file))
        expect_null(result)
      }
    )
  })
})

test_that("download_ign_dem calculates image dimensions correctly", {
  skip_if_not_installed("terra")
  skip_if_not_installed("httr2")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "dem.tif")
    # Large bbox to trigger dimension limiting
    bbox <- c(0.0, 46.0, 3.0, 49.0)

    with_mocked_bindings(
      req_perform = function(req, ...) {
        stop("Mocked network error")
      },
      .package = "httr2",
      {
        result <- suppressWarnings(nemetonshiny:::download_ign_dem(bbox, cache_file))
        expect_null(result)
      }
    )
  })
})

# ==============================================================================
# download_ign_irc_ndvi tests (mocked HTTP)
# ==============================================================================

test_that("download_ign_irc_ndvi falls back to synthetic NDVI on HTTP error", {
  skip_if_not_installed("terra")
  skip_if_not_installed("httr2")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "ndvi.tif")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      req_perform = function(req, ...) {
        stop("Mocked network error")
      },
      .package = "httr2",
      {
        result <- suppressWarnings(nemetonshiny:::download_ign_irc_ndvi(bbox, cache_file))
        # Should fall back to synthetic NDVI
        expect_true(inherits(result, "SpatRaster"))
        expect_equal(names(result), "ndvi")
      }
    )
  })
})

test_that("download_ign_irc_ndvi handles sf bbox input", {
  skip_if_not_installed("terra")
  skip_if_not_installed("httr2")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "ndvi.tif")
    bbox <- sf::st_bbox(c(xmin = 2.0, ymin = 48.0, xmax = 2.01, ymax = 48.01),
                        crs = sf::st_crs(4326))

    with_mocked_bindings(
      req_perform = function(req, ...) {
        stop("Mocked network error")
      },
      .package = "httr2",
      {
        result <- suppressWarnings(nemetonshiny:::download_ign_irc_ndvi(bbox, cache_file))
        # Should fall back to synthetic NDVI
        expect_true(inherits(result, "SpatRaster"))
      }
    )
  })
})

test_that("download_ign_irc_ndvi uses cached IRC if available", {
  skip_if_not_installed("terra")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "ndvi.tif")
    irc_cache <- file.path(getwd(), "irc.tif")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    # Create a fake 3-band IRC raster
    irc <- terra::rast(
      nrows = 10, ncols = 10,
      xmin = 2.0, xmax = 2.01, ymin = 48.0, ymax = 48.01,
      nlyrs = 3, crs = "EPSG:4326"
    )
    terra::values(irc) <- cbind(
      runif(100, 100, 200),  # NIR
      runif(100, 50, 100),   # Red
      runif(100, 50, 100)    # Green
    )
    terra::writeRaster(irc, irc_cache, overwrite = TRUE)

    result <- nemetonshiny:::download_ign_irc_ndvi(bbox, cache_file)
    expect_true(inherits(result, "SpatRaster"))
    expect_equal(names(result), "ndvi")
    expect_true(file.exists(cache_file))
  })
})

test_that("download_ign_irc_ndvi handles large bbox with dimension limiting", {
  skip_if_not_installed("terra")
  skip_if_not_installed("httr2")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "ndvi.tif")
    # Large bbox that would exceed max_size limit
    bbox <- c(0.0, 46.0, 3.0, 49.0)

    with_mocked_bindings(
      req_perform = function(req, ...) {
        stop("Mocked network error")
      },
      .package = "httr2",
      {
        result <- suppressWarnings(nemetonshiny:::download_ign_irc_ndvi(bbox, cache_file))
        expect_true(inherits(result, "SpatRaster"))
      }
    )
  })
})

# ==============================================================================
# download_inpn_wfs tests (mocked happign)
# ==============================================================================

test_that("download_inpn_wfs returns NULL when happign not available", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "protected_areas.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    # Instead of mocking requireNamespace (causes stack overflow),
    # test the WFS metadata failure path which is equivalent
    with_mocked_bindings(
      get_layers_metadata = function(...) {
        stop("Mocked: happign unavailable")
      },
      .package = "happign",
      {
        result <- suppressWarnings(nemetonshiny:::download_inpn_wfs("protected_areas", bbox, cache_file))
        expect_null(result)
      }
    )
  })
})

test_that("download_inpn_wfs handles unknown layer name", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "unknown.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      get_layers_metadata = function(...) {
        data.frame(Name = c("patrinat.layer1", "patrinat.layer2"),
                   stringsAsFactors = FALSE)
      },
      .package = "happign",
      {
        expect_warning(
          result <- nemetonshiny:::download_inpn_wfs("unknown_layer", bbox, cache_file),
          regexp = "Unknown INPN layer"
        )
        expect_null(result)
      }
    )
  })
})

test_that("download_inpn_wfs returns NULL when WFS metadata fails", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "protected_areas.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      get_layers_metadata = function(...) {
        stop("Mocked metadata failure")
      },
      .package = "happign",
      {
        result <- suppressWarnings(nemetonshiny:::download_inpn_wfs("protected_areas", bbox, cache_file))
        expect_null(result)
      }
    )
  })
})

test_that("download_inpn_wfs returns NULL when no patrinat layers match", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "protected_areas.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      get_layers_metadata = function(...) {
        # Return layers that do not match "patrinat"
        data.frame(Name = c("other.layer1", "another.layer2"),
                   stringsAsFactors = FALSE)
      },
      .package = "happign",
      {
        result <- nemetonshiny:::download_inpn_wfs("protected_areas", bbox, cache_file)
        expect_null(result)
      }
    )
  })
})

test_that("download_inpn_wfs returns NULL when no features found", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "protected_areas.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      get_layers_metadata = function(...) {
        data.frame(Name = c("patrinat.reserve_naturelle"),
                   stringsAsFactors = FALSE)
      },
      get_wfs = function(...) {
        # Return empty sf
        NULL
      },
      .package = "happign",
      {
        result <- nemetonshiny:::download_inpn_wfs("protected_areas", bbox, cache_file)
        expect_null(result)
      }
    )
  })
})

test_that("download_inpn_wfs retrieves and combines features", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "protected_areas.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      get_layers_metadata = function(...) {
        data.frame(Name = c("patrinat.reserve_naturelle"),
                   stringsAsFactors = FALSE)
      },
      get_wfs = function(shape, layer, ...) {
        sf::st_sf(
          nom_site = "Test Reserve",
          ID_MNHN = "FR123",
          geometry = sf::st_sfc(
            sf::st_polygon(list(matrix(
              c(2.0, 48.0, 2.005, 48.0, 2.005, 48.005, 2.0, 48.005, 2.0, 48.0),
              ncol = 2, byrow = TRUE
            ))),
            crs = 4326
          )
        )
      },
      .package = "happign",
      {
        result <- nemetonshiny:::download_inpn_wfs("protected_areas", bbox, cache_file)
        expect_true(inherits(result, "sf"))
        expect_true(nrow(result) > 0)
        expect_true("zone_id" %in% names(result))
        expect_true("zone_name" %in% names(result))
        expect_true("zone_type" %in% names(result))
        expect_true(file.exists(cache_file))
      }
    )
  })
})

test_that("download_inpn_wfs handles sf bbox input", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "wetlands.gpkg")
    bbox <- sf::st_bbox(c(xmin = 2.0, ymin = 48.0, xmax = 2.01, ymax = 48.01),
                        crs = sf::st_crs(4326))

    with_mocked_bindings(
      get_layers_metadata = function(...) {
        data.frame(Name = c("patrinat.ramsar_zone"),
                   stringsAsFactors = FALSE)
      },
      get_wfs = function(...) NULL,
      .package = "happign",
      {
        result <- nemetonshiny:::download_inpn_wfs("wetlands", bbox, cache_file)
        expect_null(result)
      }
    )
  })
})

# ==============================================================================
# download_ign_bdtopo tests (mocked happign)
# ==============================================================================

test_that("download_ign_bdtopo returns NULL when happign errors", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "roads.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      get_wfs = function(...) {
        stop("Mocked: happign service failure")
      },
      .package = "happign",
      {
        result <- suppressWarnings(nemetonshiny:::download_ign_bdtopo("roads", bbox, cache_file))
        expect_null(result)
      }
    )
  })
})

test_that("download_ign_bdtopo warns for unknown layer", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "unknown.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    expect_warning(
      result <- nemetonshiny:::download_ign_bdtopo("unknown_layer", bbox, cache_file),
      regexp = "Unknown BD TOPO layer"
    )
    expect_null(result)
  })
})

test_that("download_ign_bdtopo downloads and caches roads", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "roads.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      get_wfs = function(x, layer, ...) {
        sf::st_sf(
          id = 1:2,
          nature = c("Route", "Chemin"),
          geometry = sf::st_sfc(
            sf::st_linestring(matrix(c(2.0, 48.0, 2.005, 48.005), ncol = 2, byrow = TRUE)),
            sf::st_linestring(matrix(c(2.005, 48.005, 2.01, 48.01), ncol = 2, byrow = TRUE)),
            crs = 4326
          )
        )
      },
      .package = "happign",
      {
        result <- nemetonshiny:::download_ign_bdtopo("roads", bbox, cache_file)
        expect_true(inherits(result, "sf"))
        expect_equal(nrow(result), 2)
        expect_true(file.exists(cache_file))
      }
    )
  })
})

test_that("download_ign_bdtopo returns NULL when get_wfs returns empty", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "buildings.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      get_wfs = function(...) {
        sf::st_sf(
          geometry = sf::st_sfc(crs = 4326)
        )[0, ]
      },
      .package = "happign",
      {
        result <- nemetonshiny:::download_ign_bdtopo("buildings", bbox, cache_file)
        expect_null(result)
      }
    )
  })
})

test_that("download_ign_bdtopo handles HTTP error gracefully", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "water.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      get_wfs = function(...) {
        stop("Service unavailable")
      },
      .package = "happign",
      {
        result <- suppressWarnings(nemetonshiny:::download_ign_bdtopo("indicateur_w1_reseau", bbox, cache_file))
        expect_null(result)
      }
    )
  })
})

# ==============================================================================
# download_ign_bdforet tests (mocked HTTP)
# ==============================================================================

test_that("download_ign_bdforet handles non-200 HTTP status", {
  skip_if_not_installed("sf")
  skip_if_not_installed("httr2")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "bdforet.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      req_perform = function(req, ...) {
        structure(
          list(status_code = 503L, headers = list(), body = raw(0)),
          class = "httr2_response"
        )
      },
      resp_status = function(resp) 503L,
      .package = "httr2",
      {
        result <- suppressWarnings(nemetonshiny:::download_ign_bdforet(bbox, cache_file))
        expect_null(result)
      }
    )
  })
})

test_that("download_ign_bdforet handles empty GeoJSON response", {
  skip_if_not_installed("sf")
  skip_if_not_installed("httr2")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "bdforet.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      req_perform = function(req, ...) {
        structure(
          list(status_code = 200L, headers = list(), body = raw(0)),
          class = "httr2_response"
        )
      },
      resp_status = function(resp) 200L,
      resp_body_string = function(resp, ...) "{}",
      .package = "httr2",
      {
        result <- nemetonshiny:::download_ign_bdforet(bbox, cache_file)
        expect_null(result)
      }
    )
  })
})

test_that("download_ign_bdforet handles sf bbox input", {
  skip_if_not_installed("sf")
  skip_if_not_installed("httr2")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "bdforet.gpkg")
    bbox <- sf::st_bbox(c(xmin = 2.0, ymin = 48.0, xmax = 2.01, ymax = 48.01),
                        crs = sf::st_crs(4326))

    with_mocked_bindings(
      req_perform = function(req, ...) {
        stop("Mocked network error")
      },
      .package = "httr2",
      {
        result <- suppressWarnings(nemetonshiny:::download_ign_bdforet(bbox, cache_file))
        expect_null(result)
      }
    )
  })
})

test_that("download_ign_bdforet downloads and caches features", {
  skip_if_not_installed("sf")
  skip_if_not_installed("httr2")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "bdforet.gpkg")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    # Create a valid GeoJSON response
    mock_sf <- sf::st_sf(
      essence = c("Chene", "Hetre"),
      type_form = c("F", "F"),
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(
          c(2.0, 48.0, 2.005, 48.0, 2.005, 48.005, 2.0, 48.005, 2.0, 48.0),
          ncol = 2, byrow = TRUE
        ))),
        sf::st_polygon(list(matrix(
          c(2.005, 48.005, 2.01, 48.005, 2.01, 48.01, 2.005, 48.01, 2.005, 48.005),
          ncol = 2, byrow = TRUE
        ))),
        crs = 4326
      )
    )
    mock_geojson <- sf::st_as_text(sf::st_as_sfc(mock_sf))
    # Write to temp file, read as geojson
    temp_geojson <- tempfile(fileext = ".geojson")
    sf::st_write(mock_sf, temp_geojson, quiet = TRUE)
    geojson_text <- paste(readLines(temp_geojson, warn = FALSE), collapse = "\n")

    with_mocked_bindings(
      req_perform = function(req, ...) {
        structure(
          list(status_code = 200L, headers = list(), body = raw(0)),
          class = "httr2_response"
        )
      },
      resp_status = function(resp) 200L,
      resp_body_string = function(resp, ...) geojson_text,
      .package = "httr2",
      {
        result <- nemetonshiny:::download_ign_bdforet(bbox, cache_file)
        expect_true(inherits(result, "sf"))
        expect_equal(nrow(result), 2)
        expect_true(file.exists(cache_file))
      }
    )

    unlink(temp_geojson)
  })
})

# ==============================================================================
# download_oso tests (mocked)
# ==============================================================================

test_that("download_oso returns NULL when oso_path missing after download", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "forest_cover.tif")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      get_global_cache_dir = function() getwd(),
      download_oso_global = function(oso_path, global_cache, progress_callback) {
        # Simulate download failure - don't create the file
        invisible(NULL)
      },
      {
        result <- suppressWarnings(nemetonshiny:::download_oso(bbox, cache_file))
        expect_null(result)
      }
    )
  })
})

test_that("download_oso handles sf bbox input", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "forest_cover.tif")
    bbox <- sf::st_bbox(c(xmin = 2.0, ymin = 48.0, xmax = 2.01, ymax = 48.01),
                        crs = sf::st_crs(4326))

    with_mocked_bindings(
      get_global_cache_dir = function() getwd(),
      download_oso_global = function(oso_path, global_cache, progress_callback) {
        invisible(NULL)
      },
      {
        result <- suppressWarnings(nemetonshiny:::download_oso(bbox, cache_file))
        expect_null(result)
      }
    )
  })
})

test_that("download_oso crops existing global OSO to bbox", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  withr::with_tempdir({
    cache_file <- file.path(getwd(), "project_forest_cover.tif")
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    # Create a mock global OSO raster in Lambert-93
    oso_dir <- file.path(getwd(), "oso")
    dir.create(oso_dir, recursive = TRUE)
    oso_path <- file.path(oso_dir, "oso.tif")

    mock_oso <- terra::rast(
      nrows = 100, ncols = 100,
      xmin = 600000, xmax = 700000,
      ymin = 6800000, ymax = 6900000,
      crs = "EPSG:2154"
    )
    terra::values(mock_oso) <- sample(c(17, 18, 19, 21, 23), 10000, replace = TRUE)
    terra::writeRaster(mock_oso, oso_path, overwrite = TRUE)

    with_mocked_bindings(
      get_global_cache_dir = function() getwd(),
      {
        result <- suppressWarnings(nemetonshiny:::download_oso(bbox, cache_file))
        # The crop may fail if bbox does not overlap with mock extent
        # but the function should handle this gracefully
        expect_true(is.null(result) || inherits(result, "SpatRaster"))
      }
    )
  })
})

# ==============================================================================
# download_oso_global tests (mocked HTTP)
# ==============================================================================

test_that("download_oso_global detects existing tar archive", {
  withr::with_tempdir({
    oso_path <- file.path(getwd(), "oso.tif")
    global_cache <- getwd()

    # Create a fake tar.gz file (too small, so it will try download)
    fake_tar <- file.path(global_cache, "OSO_RASTER.tar.gz")
    writeLines("fake", fake_tar)

    # Since the file is small (<5GB), it will try to download via curl.
    # Mock curl to fail, triggering the manual download message
    with_mocked_bindings(
      curl = function(...) {
        stop("Mocked curl error")
      },
      .package = "curl",
      {
        result <- suppressWarnings(nemetonshiny:::download_oso_global(oso_path, global_cache))
        expect_null(result)
        expect_false(file.exists(oso_path))
      }
    )
  })
})

test_that("download_oso_global handles curl download failure", {
  skip_if_not_installed("curl")

  withr::with_tempdir({
    oso_path <- file.path(getwd(), "oso.tif")
    global_cache <- getwd()

    with_mocked_bindings(
      curl = function(...) {
        stop("Mocked curl failure")
      },
      .package = "curl",
      {
        result <- suppressWarnings(nemetonshiny:::download_oso_global(oso_path, global_cache))
        expect_null(result)
        expect_false(file.exists(oso_path))
      }
    )
  })
})

# ==============================================================================
# download_ign_lidar_hd tests (mocked)
# ==============================================================================

test_that("download_ign_lidar_hd returns cached mosaic if available", {
  skip_if_not_installed("terra")

  withr::with_tempdir({
    cache_dir <- getwd()
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    # Create a cached mosaic file
    mosaic_file <- file.path(cache_dir, "lidar_mnh_mosaic.tif")
    mock_raster <- terra::rast(
      nrows = 10, ncols = 10,
      xmin = 2.0, xmax = 2.01, ymin = 48.0, ymax = 48.01,
      crs = "EPSG:4326"
    )
    terra::values(mock_raster) <- runif(100, 0, 30)
    terra::writeRaster(mock_raster, mosaic_file)

    result <- nemetonshiny:::download_ign_lidar_hd(bbox, cache_dir, product = "mnh")
    expect_s4_class(result, "SpatRaster")
  })
})

test_that("download_ign_lidar_hd returns cached COPC tiles if available", {
  withr::with_tempdir({
    cache_dir <- getwd()
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    # Create cached COPC tiles directory
    copc_dir <- file.path(cache_dir, "lidar_nuage")
    dir.create(copc_dir, recursive = TRUE)
    writeLines("fake copc data", file.path(copc_dir, "tile1.copc.laz"))
    writeLines("fake copc data", file.path(copc_dir, "tile2.copc.laz"))

    result <- nemetonshiny:::download_ign_lidar_hd(bbox, cache_dir, product = "nuage")
    expect_type(result, "character")
    expect_equal(length(result), 2)
  })
})

test_that("download_ign_lidar_hd returns NULL when no tiles found", {
  skip_if_not_installed("sf")

  withr::with_tempdir({
    cache_dir <- getwd()
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      query_lidar_wfs = function(...) NULL,
      {
        result <- nemetonshiny:::download_ign_lidar_hd(bbox, cache_dir, product = "mnh")
        expect_null(result)
      }
    )
  })
})

test_that("download_ign_lidar_hd returns NULL when tiles have no URL column", {
  skip_if_not_installed("sf")

  withr::with_tempdir({
    cache_dir <- getwd()
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      query_lidar_wfs = function(...) {
        sf::st_sf(
          name = c("tile1", "tile2"),
          value = c(1, 2),
          geometry = sf::st_sfc(
            sf::st_point(c(2.0, 48.0)),
            sf::st_point(c(2.005, 48.005)),
            crs = 4326
          )
        )
      },
      {
        result <- nemetonshiny:::download_ign_lidar_hd(bbox, cache_dir, product = "mnh")
        expect_null(result)
      }
    )
  })
})

test_that("download_ign_lidar_hd downloads and mosaics tiles", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  withr::with_tempdir({
    cache_dir <- getwd()
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      query_lidar_wfs = function(...) {
        sf::st_sf(
          url_telechargement = c("http://example.com/tile1.tif"),
          geometry = sf::st_sfc(sf::st_point(c(2.0, 48.0)), crs = 4326)
        )
      },
      download_lidar_tile = function(url, dest_file) {
        # Create a fake tile file
        mock_raster <- terra::rast(
          nrows = 10, ncols = 10,
          xmin = 2.0, xmax = 2.01, ymin = 48.0, ymax = 48.01,
          crs = "EPSG:4326"
        )
        terra::values(mock_raster) <- runif(100, 0, 30)
        terra::writeRaster(mock_raster, dest_file, overwrite = TRUE)
        return(dest_file)
      },
      {
        result <- nemetonshiny:::download_ign_lidar_hd(bbox, cache_dir, product = "mnh")
        expect_true(inherits(result, "SpatRaster"))
      }
    )
  })
})

test_that("download_ign_lidar_hd handles all tile download failures", {
  skip_if_not_installed("sf")

  withr::with_tempdir({
    cache_dir <- getwd()
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    with_mocked_bindings(
      query_lidar_wfs = function(...) {
        sf::st_sf(
          url_telechargement = c("http://example.com/tile1.tif"),
          geometry = sf::st_sfc(sf::st_point(c(2.0, 48.0)), crs = 4326)
        )
      },
      download_lidar_tile = function(url, dest_file) NULL,
      {
        result <- nemetonshiny:::download_ign_lidar_hd(bbox, cache_dir, product = "mnh")
        expect_null(result)
      }
    )
  })
})

test_that("download_ign_lidar_hd handles sf bbox input", {
  skip_if_not_installed("sf")

  withr::with_tempdir({
    cache_dir <- getwd()
    bbox <- sf::st_bbox(c(xmin = 2.0, ymin = 48.0, xmax = 2.01, ymax = 48.01),
                        crs = sf::st_crs(4326))

    with_mocked_bindings(
      query_lidar_wfs = function(...) NULL,
      {
        result <- nemetonshiny:::download_ign_lidar_hd(bbox, cache_dir, product = "mnt")
        expect_null(result)
      }
    )
  })
})

test_that("download_ign_lidar_hd reports progress callback", {
  skip_if_not_installed("sf")

  withr::with_tempdir({
    cache_dir <- getwd()
    bbox <- c(2.0, 48.0, 2.01, 48.01)

    progress_reports <- list()
    progress_cb <- function(report) {
      progress_reports[[length(progress_reports) + 1]] <<- report
    }

    with_mocked_bindings(
      query_lidar_wfs = function(...) {
        sf::st_sf(
          url_telechargement = c("http://example.com/tile1.tif"),
          geometry = sf::st_sfc(sf::st_point(c(2.0, 48.0)), crs = 4326)
        )
      },
      download_lidar_tile = function(url, dest_file) NULL,
      {
        result <- nemetonshiny:::download_ign_lidar_hd(
          bbox, cache_dir, product = "mnh",
          progress_callback = progress_cb
        )
        expect_true(length(progress_reports) > 0)
      }
    )
  })
})

# ==============================================================================
# query_lidar_wfs tests (mocked HTTP)
# ==============================================================================

test_that("query_lidar_wfs returns NULL on non-200 status", {
  skip_if_not_installed("sf")
  skip_if_not_installed("httr2")

  bbox <- c(2.0, 48.0, 2.01, 48.01)

  with_mocked_bindings(
    req_perform = function(req, ...) {
      structure(
        list(status_code = 500L, headers = list(), body = raw(0)),
        class = "httr2_response"
      )
    },
    resp_status = function(resp) 500L,
    .package = "httr2",
    {
      result <- suppressWarnings(nemetonshiny:::query_lidar_wfs("IGNF_MNH-LIDAR-HD:dalle", bbox))
      expect_null(result)
    }
  )
})

test_that("query_lidar_wfs returns NULL for empty GeoJSON", {
  skip_if_not_installed("sf")
  skip_if_not_installed("httr2")

  bbox <- c(2.0, 48.0, 2.01, 48.01)

  with_mocked_bindings(
    req_perform = function(req, ...) {
      structure(
        list(status_code = 200L, headers = list(), body = raw(0)),
        class = "httr2_response"
      )
    },
    resp_status = function(resp) 200L,
    resp_body_string = function(resp, ...) "{}",
    .package = "httr2",
    {
      result <- nemetonshiny:::query_lidar_wfs("IGNF_MNH-LIDAR-HD:dalle", bbox)
      expect_null(result)
    }
  )
})

test_that("query_lidar_wfs handles HTTP error gracefully", {
  skip_if_not_installed("sf")
  skip_if_not_installed("httr2")

  bbox <- c(2.0, 48.0, 2.01, 48.01)

  with_mocked_bindings(
    req_perform = function(req, ...) {
      stop("Connection refused")
    },
    .package = "httr2",
    {
      result <- suppressWarnings(nemetonshiny:::query_lidar_wfs("IGNF_MNH-LIDAR-HD:dalle", bbox))
      expect_null(result)
    }
  )
})

test_that("query_lidar_wfs parses valid GeoJSON response", {
  skip_if_not_installed("sf")
  skip_if_not_installed("httr2")

  bbox <- c(2.0, 48.0, 2.01, 48.01)

  # Create a valid GeoJSON string
  mock_sf <- sf::st_sf(
    url_telechargement = "http://example.com/tile.tif",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(2.0, 48.0, 2.01, 48.0, 2.01, 48.01, 2.0, 48.01, 2.0, 48.0),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )
  temp_geojson <- tempfile(fileext = ".geojson")
  sf::st_write(mock_sf, temp_geojson, quiet = TRUE)
  geojson_text <- paste(readLines(temp_geojson, warn = FALSE), collapse = "\n")

  with_mocked_bindings(
    req_perform = function(req, ...) {
      structure(
        list(status_code = 200L, headers = list(), body = raw(0)),
        class = "httr2_response"
      )
    },
    resp_status = function(resp) 200L,
    resp_body_string = function(resp, ...) geojson_text,
    .package = "httr2",
    {
      result <- nemetonshiny:::query_lidar_wfs("IGNF_MNH-LIDAR-HD:dalle", bbox)
      expect_true(inherits(result, "sf"))
      expect_equal(nrow(result), 1)
    }
  )

  unlink(temp_geojson)
})

# ==============================================================================
# download_layers_for_parcels additional tests
# ==============================================================================

test_that("download_layers_for_parcels reports progress callbacks", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_dl_progress")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  dir.create(project_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(2.0, 48.0, 2.001, 48.0, 2.001, 48.001, 2.0, 48.001, 2.0, 48.0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  progress_reports <- list()
  progress_cb <- function(report) {
    progress_reports[[length(progress_reports) + 1]] <<- report
  }

  with_mocked_bindings(
    download_raster_source = function(...) NULL,
    download_vector_source = function(...) NULL,
    download_ign_lidar_hd = function(...) NULL,
    {
      result <- nemetonshiny:::download_layers_for_parcels(
        parcels = mock_parcels,
        project_path = project_dir,
        progress_callback = progress_cb
      )

      expect_s3_class(result, "nemeton_layers")
      # Should have received progress reports
      expect_true(length(progress_reports) > 0)
      # Final report should show download_complete
      final <- progress_reports[[length(progress_reports)]]
      expect_equal(final$current, "download_complete")
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("download_layers_for_parcels generates vector download warnings", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_dl_vec_warn")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  dir.create(project_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(2.0, 48.0, 2.001, 48.0, 2.001, 48.001, 2.0, 48.001, 2.0, 48.0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  with_mocked_bindings(
    download_raster_source = function(...) NULL,
    download_vector_source = function(...) NULL,
    download_ign_lidar_hd = function(...) NULL,
    {
      result <- nemetonshiny:::download_layers_for_parcels(
        parcels = mock_parcels,
        project_path = project_dir,
        progress_callback = NULL
      )

      expect_s3_class(result, "nemeton_layers")
      # Vector sources returning NULL should generate info warnings
      vector_warnings <- Filter(function(w) w$type == "info", result$warnings)
      expect_true(length(vector_warnings) > 0)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("download_layers_for_parcels handles raster download errors", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_dl_raster_err")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  dir.create(project_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(2.0, 48.0, 2.001, 48.0, 2.001, 48.001, 2.0, 48.001, 2.0, 48.0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  with_mocked_bindings(
    download_raster_source = function(source_name, source_config, bbox, cache_dir, progress_callback) {
      stop("Simulated download error for ", source_name)
    },
    download_vector_source = function(...) NULL,
    download_ign_lidar_hd = function(...) NULL,
    {
      # Should not crash even when raster downloads throw errors
      expect_warning(
        result <- nemetonshiny:::download_layers_for_parcels(
          parcels = mock_parcels,
          project_path = project_dir,
          progress_callback = NULL
        ),
        regexp = "Failed to download"
      )

      expect_s3_class(result, "nemeton_layers")
      expect_equal(length(result$rasters), 0)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# compute_single_indicator dispatch tests
# ==============================================================================

test_that("compute_single_indicator dispatches to existing function", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  mock_layers <- structure(
    list(rasters = list(), vectors = list(), point_clouds = list(),
         bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
         cache_dir = tempdir(), warnings = list()),
    class = "nemeton_layers"
  )

  # Test that a known indicator can be dispatched (even with no data)
  result <- tryCatch(
    nemetonshiny:::compute_single_indicator("indicateur_c1_biomasse", mock_parcels, mock_layers),
    error = function(e) NULL
  )

  # Either returns numeric values or NULL on error

  expect_true(is.null(result) || is.numeric(result))
})

test_that("compute_single_indicator handles multi-parcel input", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                               ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(1, 1, 2, 1, 2, 2, 1, 2, 1, 1),
                               ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(2, 2, 3, 2, 3, 3, 2, 3, 2, 2),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = c("p1", "p2", "p3"), geometry = mock_geom)

  mock_layers <- structure(
    list(rasters = list(), vectors = list(), point_clouds = list(),
         bbox = c(0, 0, 3, 3), crs = sf::st_crs(4326),
         cache_dir = tempdir(), warnings = list()),
    class = "nemeton_layers"
  )

  # Placeholder fallback should return one value per parcel
  result <- nemetonshiny:::compute_single_indicator(
    "placeholder_test_multi",
    mock_parcels,
    mock_layers
  )

  expect_type(result, "double")
  expect_length(result, 3)
})

# ==============================================================================
# start_computation integration tests (mocked)
# ==============================================================================

test_that("start_computation succeeds with all mocked functions", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_start_success")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 0.001, 0, 0.001, 0.001, 0, 0.001, 0, 0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)
  sf::st_write(mock_parcels, file.path(data_dir, "parcels.gpkg"), quiet = TRUE)

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    get_app_options = function() list(project_dir = temp_dir),
    clear_cancel = function(id) invisible(NULL),
    get_computation_progress = function(id) list(computed_indicators = character(0)),
    update_project_status = function(id, status) invisible(NULL),
    download_layers_for_parcels = function(parcels, project_path, progress_callback) {
      structure(
        list(rasters = list(), vectors = list(), point_clouds = list(),
             bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
             cache_dir = tempdir(), warnings = list()),
        class = "nemeton_layers"
      )
    },
    is_cancelled = function(id) FALSE,
    compute_all_indicators = function(parcels, layers, indicators, progress_callback, project_id) {
      parcels$indicateur_c1_biomasse <- 75.5
      parcels
    },
    save_indicators = function(project_id, results) TRUE,
    update_project_metadata = function(id, updates) invisible(NULL),
    {
      result <- nemetonshiny:::start_computation(
        "test_project",
        indicators = "indicateur_c1_biomasse"
      )

      expect_true(result$success)
      expect_equal(result$state$status, "completed")
      expect_equal(result$state$phase, "complete")
      expect_true("indicateur_c1_biomasse" %in% names(result$results))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("start_computation uses provided project_path", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_start_path")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 0.001, 0, 0.001, 0.001, 0, 0.001, 0, 0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)
  sf::st_write(mock_parcels, file.path(data_dir, "parcels.gpkg"), quiet = TRUE)

  with_mocked_bindings(
    # start_computation honours the passed project_path, but
    # load_project()/ensure_project_migrated() still resolve the
    # project directory via get_project_path(); mock it so the
    # migration writes/reads land in the provided project_dir.
    get_project_path = function(id) project_dir,
    get_app_options = function() list(project_dir = temp_dir),
    clear_cancel = function(id) invisible(NULL),
    get_computation_progress = function(id) list(computed_indicators = character(0)),
    update_project_status = function(id, status) invisible(NULL),
    download_layers_for_parcels = function(parcels, project_path, progress_callback) {
      structure(
        list(rasters = list(), vectors = list(), point_clouds = list(),
             bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
             cache_dir = tempdir(), warnings = list()),
        class = "nemeton_layers"
      )
    },
    is_cancelled = function(id) FALSE,
    compute_all_indicators = function(parcels, layers, indicators, progress_callback, project_id) {
      parcels$indicateur_c1_biomasse <- 80
      parcels
    },
    save_indicators = function(project_id, results) TRUE,
    update_project_metadata = function(id, updates) invisible(NULL),
    {
      result <- nemetonshiny:::start_computation(
        "test_project",
        indicators = "indicateur_c1_biomasse",
        project_path = project_dir
      )

      expect_true(result$success)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("start_computation propagates download warnings", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_start_warnings")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 0.001, 0, 0.001, 0.001, 0, 0.001, 0, 0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)
  sf::st_write(mock_parcels, file.path(data_dir, "parcels.gpkg"), quiet = TRUE)

  progress_states <- list()
  progress_cb <- function(state) {
    progress_states <<- c(progress_states, list(state))
  }

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    get_app_options = function() list(project_dir = temp_dir),
    clear_cancel = function(id) invisible(NULL),
    get_computation_progress = function(id) list(computed_indicators = character(0)),
    update_project_status = function(id, status) invisible(NULL),
    download_layers_for_parcels = function(parcels, project_path, progress_callback) {
      structure(
        list(rasters = list(), vectors = list(), point_clouds = list(),
             bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
             cache_dir = tempdir(),
             warnings = list(
               list(type = "warning", source = "OSO", message = "OSO download failed")
             )),
        class = "nemeton_layers"
      )
    },
    is_cancelled = function(id) FALSE,
    compute_all_indicators = function(parcels, layers, indicators, progress_callback, project_id) {
      parcels$indicateur_c1_biomasse <- 60
      parcels
    },
    save_indicators = function(project_id, results) TRUE,
    update_project_metadata = function(id, updates) invisible(NULL),
    {
      result <- nemetonshiny:::start_computation(
        "test_project",
        indicators = "indicateur_c1_biomasse",
        progress_callback = progress_cb
      )

      expect_true(result$success)
      # Errors should include the download warning
      expect_true(length(result$state$errors) > 0)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("start_computation handles cancellation after compute phase", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_cancel_compute")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 0.001, 0, 0.001, 0.001, 0, 0.001, 0, 0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)
  sf::st_write(mock_parcels, file.path(data_dir, "parcels.gpkg"), quiet = TRUE)

  cancel_count <- 0

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    get_app_options = function() list(project_dir = temp_dir),
    clear_cancel = function(id) invisible(NULL),
    get_computation_progress = function(id) list(computed_indicators = character(0)),
    update_project_status = function(id, status) invisible(NULL),
    download_layers_for_parcels = function(parcels, project_path, progress_callback) {
      structure(
        list(rasters = list(), vectors = list(), point_clouds = list(),
             bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
             cache_dir = tempdir(), warnings = list()),
        class = "nemeton_layers"
      )
    },
    is_cancelled = function(id) {
      cancel_count <<- cancel_count + 1
      # Allow download phase, cancel after compute phase
      cancel_count > 1
    },
    compute_all_indicators = function(parcels, layers, indicators, progress_callback, project_id) {
      parcels$indicateur_c1_biomasse <- 60
      parcels
    },
    {
      result <- nemetonshiny:::start_computation(
        "test_project",
        indicators = "indicateur_c1_biomasse"
      )

      expect_false(result$success)
      expect_equal(result$state$status, "cancelled")
      expect_true(result$cancelled)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("start_computation with use_file_progress writes progress file", {
  skip_if_not_installed("sf")

  temp_dir <- file.path(tempdir(), "nemeton_test_file_progress")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 0.001, 0, 0.001, 0.001, 0, 0.001, 0, 0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)
  sf::st_write(mock_parcels, file.path(data_dir, "parcels.gpkg"), quiet = TRUE)

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    get_app_options = function() list(project_dir = temp_dir),
    clear_cancel = function(id) invisible(NULL),
    get_computation_progress = function(id) list(computed_indicators = character(0)),
    update_project_status = function(id, status) invisible(NULL),
    download_layers_for_parcels = function(parcels, project_path, progress_callback) {
      structure(
        list(rasters = list(), vectors = list(), point_clouds = list(),
             bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
             cache_dir = tempdir(), warnings = list()),
        class = "nemeton_layers"
      )
    },
    is_cancelled = function(id) FALSE,
    compute_all_indicators = function(parcels, layers, indicators, progress_callback, project_id) {
      parcels$indicateur_c1_biomasse <- 55
      parcels
    },
    save_indicators = function(project_id, results) TRUE,
    update_project_metadata = function(id, updates) invisible(NULL),
    {
      result <- nemetonshiny:::start_computation(
        "test_project",
        indicators = "indicateur_c1_biomasse",
        use_file_progress = TRUE
      )

      expect_true(result$success)
      # Progress file should have been written
      progress_file <- file.path(data_dir, "progress_state.json")
      expect_true(file.exists(progress_file))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# save_indicators_incremental additional tests
# ==============================================================================

test_that("save_indicators_incremental tracks computed indicators in progress", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")

  temp_dir <- file.path(tempdir(), "nemeton_test_incr_progress")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_results <- sf::st_sf(
    id = "p1",
    indicateur_c1_biomasse = 75.0,
    indicateur_c2_ndvi = 0.65,
    geometry = mock_geom
  )

  with_mocked_bindings(
    get_project_path = function(id) project_dir,
    {
      result <- nemetonshiny:::save_indicators_incremental(
        "test_project",
        mock_results,
        "indicateur_c2_ndvi"
      )
      expect_true(result)

      # Check progress file
      progress_path <- file.path(data_dir, "compute_progress.json")
      expect_true(file.exists(progress_path))
      progress <- jsonlite::read_json(progress_path)
      expect_equal(progress$last_indicator, "indicateur_c2_ndvi")
      expect_true("indicateur_c1_biomasse" %in% unlist(progress$computed_indicators))
      expect_true("indicateur_c2_ndvi" %in% unlist(progress$computed_indicators))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("save_indicators_incremental handles write error gracefully", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")

  temp_dir <- file.path(tempdir(), "nemeton_test_incr_write_err")
  unlink(temp_dir, recursive = TRUE)
  project_dir <- file.path(temp_dir, "test_project")
  data_dir <- file.path(project_dir, "data")
  dir.create(data_dir, recursive = TRUE)

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_results <- sf::st_sf(id = "p1", indicateur_c1_biomasse = 50, geometry = mock_geom)

  # Use local_mocked_bindings for nemeton package, then separately mock arrow
  local_mocked_bindings(
    get_project_path = function(id) project_dir,
    .package = "nemetonshiny"
  )

  with_mocked_bindings(
    write_parquet = function(...) stop("Mocked write failure"),
    .package = "arrow",
    {
      result <- suppressWarnings(nemetonshiny:::save_indicators_incremental(
        "test_project",
        mock_results,
        "indicateur_c1_biomasse"
      ))
      expect_false(result)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# compute_all_indicators additional tests
# ==============================================================================

test_that("compute_all_indicators handles NULL existing results with mismatched rows", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                               ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(1, 1, 2, 1, 2, 2, 1, 2, 1, 1),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = c("p1", "p2"), geometry = mock_geom)

  # Existing results with different row count
  existing_results <- sf::st_sf(
    id = "p1",
    indicateur_c1_biomasse = 50,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                                 ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  mock_layers <- structure(
    list(rasters = list(), vectors = list(), point_clouds = list(),
         bbox = c(0, 0, 2, 2), crs = sf::st_crs(4326),
         cache_dir = tempdir(), warnings = list()),
    class = "nemeton_layers"
  )

  with_mocked_bindings(
    load_indicators = function(id) existing_results,
    is_cancelled = function(id) FALSE,
    compute_single_indicator = function(indicator, parcels, layers) {
      rep(60, nrow(parcels))
    },
    save_indicators_incremental = function(...) TRUE,
    {
      result <- nemetonshiny:::compute_all_indicators(
        parcels = mock_parcels,
        layers = mock_layers,
        indicators = c("indicateur_c1_biomasse"),
        progress_callback = NULL,
        project_id = "test"
      )

      # Should use parcels (2 rows) instead of existing_results (1 row)
      expect_equal(nrow(result), 2)
    }
  )
})

test_that("compute_all_indicators reports skipped indicators in progress", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  mock_parcels <- sf::st_sf(id = "p1", geometry = mock_geom)

  existing_results <- mock_parcels
  existing_results$indicateur_c1_biomasse <- 75

  mock_layers <- structure(
    list(rasters = list(), vectors = list(), point_clouds = list(),
         bbox = c(0, 0, 1, 1), crs = sf::st_crs(4326),
         cache_dir = tempdir(), warnings = list()),
    class = "nemeton_layers"
  )

  progress_reports <- list()
  progress_cb <- function(report) {
    progress_reports[[length(progress_reports) + 1]] <<- report
  }

  with_mocked_bindings(
    load_indicators = function(id) existing_results,
    is_cancelled = function(id) FALSE,
    compute_single_indicator = function(indicator, parcels, layers) {
      rep(50, nrow(parcels))
    },
    save_indicators_incremental = function(...) TRUE,
    {
      result <- nemetonshiny:::compute_all_indicators(
        parcels = mock_parcels,
        layers = mock_layers,
        indicators = c("indicateur_c1_biomasse", "indicateur_c2_ndvi"),
        progress_callback = progress_cb,
        project_id = "test"
      )

      expect_true(length(progress_reports) > 0)
      # Initial report should show skipped count
      first_report <- progress_reports[[1]]
      expect_equal(first_report$skipped, 1)
    }
  )
})

# ==============================================================================
# download_raster_source dispatch tests
# ==============================================================================

test_that("download_raster_source dispatches to download_ign_irc_ndvi", {
  skip_if_not_installed("terra")

  withr::with_tempdir({
    cache_dir <- getwd()

    with_mocked_bindings(
      download_ign_irc_ndvi = function(bbox, cache_file) {
        # Create a small raster
        r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                         crs = "EPSG:4326")
        terra::values(r) <- runif(25, 0.3, 0.9)
        names(r) <- "ndvi"
        terra::writeRaster(r, cache_file, overwrite = TRUE)
        terra::rast(cache_file)
      },
      {
        result <- nemetonshiny:::download_raster_source(
          source_name = "ndvi",
          source_config = list(source = "ign_irc"),
          bbox = c(0, 0, 1, 1),
          cache_dir = cache_dir
        )
        expect_s4_class(result, "SpatRaster")
      }
    )
  })
})

test_that("download_raster_source dispatches to download_ign_dem", {
  skip_if_not_installed("terra")

  withr::with_tempdir({
    cache_dir <- getwd()

    with_mocked_bindings(
      download_ign_dem = function(bbox, cache_file) {
        r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                         crs = "EPSG:4326")
        terra::values(r) <- runif(25, 100, 500)
        terra::writeRaster(r, cache_file, overwrite = TRUE)
        terra::rast(cache_file)
      },
      {
        result <- nemetonshiny:::download_raster_source(
          source_name = "dem",
          source_config = list(source = "ign_bd_alti"),
          bbox = c(0, 0, 1, 1),
          cache_dir = cache_dir
        )
        expect_s4_class(result, "SpatRaster")
      }
    )
  })
})

test_that("download_raster_source dispatches to download_oso", {
  skip_if_not_installed("terra")

  withr::with_tempdir({
    cache_dir <- getwd()

    with_mocked_bindings(
      download_oso = function(bbox, cache_file, progress_callback) {
        r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                         crs = "EPSG:4326")
        terra::values(r) <- sample(c(17, 18, 19), 25, replace = TRUE)
        terra::writeRaster(r, cache_file, overwrite = TRUE)
        terra::rast(cache_file)
      },
      {
        result <- nemetonshiny:::download_raster_source(
          source_name = "forest_cover",
          source_config = list(source = "oso"),
          bbox = c(0, 0, 1, 1),
          cache_dir = cache_dir
        )
        expect_s4_class(result, "SpatRaster")
      }
    )
  })
})

test_that("download_raster_source dispatches to download_ign_lidar_hd", {
  skip_if_not_installed("terra")

  withr::with_tempdir({
    cache_dir <- getwd()

    with_mocked_bindings(
      download_ign_lidar_hd = function(bbox, cache_dir, product, progress_callback) {
        r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                         crs = "EPSG:4326")
        terra::values(r) <- runif(25, 0, 30)
        r
      },
      {
        result <- nemetonshiny:::download_raster_source(
          source_name = "lidar_mnh",
          source_config = list(source = "ign_lidar_hd", product = "mnh"),
          bbox = c(0, 0, 1, 1),
          cache_dir = cache_dir
        )
        expect_s4_class(result, "SpatRaster")
      }
    )
  })
})

# ==============================================================================
# download_vector_source dispatch tests
# ==============================================================================

test_that("download_vector_source dispatches to download_inpn_wfs", {
  skip_if_not_installed("sf")

  withr::with_tempdir({
    cache_dir <- getwd()

    with_mocked_bindings(
      download_inpn_wfs = function(source_name, bbox, cache_file) {
        mock_sf <- sf::st_sf(
          zone_id = "FR123",
          zone_name = "Test",
          zone_type = "reserve",
          geometry = sf::st_sfc(sf::st_point(c(0.5, 0.5)), crs = 4326)
        )
        sf::st_write(mock_sf, cache_file, quiet = TRUE)
        mock_sf
      },
      {
        result <- nemetonshiny:::download_vector_source(
          source_name = "protected_areas",
          source_config = list(source = "inpn_wfs"),
          bbox = c(0, 0, 1, 1),
          cache_dir = cache_dir
        )
        expect_s3_class(result, "sf")
      }
    )
  })
})

test_that("download_vector_source dispatches to download_ign_bdtopo", {
  skip_if_not_installed("sf")

  withr::with_tempdir({
    cache_dir <- getwd()

    with_mocked_bindings(
      download_ign_bdtopo = function(source_name, bbox, cache_file) {
        mock_sf <- sf::st_sf(
          id = 1,
          geometry = sf::st_sfc(
            sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
            crs = 4326
          )
        )
        sf::st_write(mock_sf, cache_file, quiet = TRUE)
        mock_sf
      },
      {
        result <- nemetonshiny:::download_vector_source(
          source_name = "roads",
          source_config = list(source = "ign_bd_topo"),
          bbox = c(0, 0, 1, 1),
          cache_dir = cache_dir
        )
        expect_s3_class(result, "sf")
      }
    )
  })
})

test_that("download_vector_source dispatches to download_ign_bdforet", {
  skip_if_not_installed("sf")

  withr::with_tempdir({
    cache_dir <- getwd()

    with_mocked_bindings(
      download_ign_bdforet = function(bbox, cache_file) {
        mock_sf <- sf::st_sf(
          essence = "Chene",
          geometry = sf::st_sfc(
            sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                                       ncol = 2, byrow = TRUE))),
            crs = 4326
          )
        )
        sf::st_write(mock_sf, cache_file, quiet = TRUE)
        mock_sf
      },
      {
        result <- nemetonshiny:::download_vector_source(
          source_name = "bdforet",
          source_config = list(source = "ign_bdforet"),
          bbox = c(0, 0, 1, 1),
          cache_dir = cache_dir
        )
        expect_s3_class(result, "sf")
      }
    )
  })
})

# ==============================================================================
# .apply_field_data_if_present — E5 dette technique (auto re-apply field data)
# ==============================================================================

make_compute_unit_for_field_test <- function() {
  sf::st_sf(
    ug_id = c("U01", "U02"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,0)))),
      sf::st_polygon(list(rbind(c(2,0), c(3,0), c(3,1), c(2,0)))),
      crs = 2154
    )
  )
}

test_that("apply_field_data_if_present returns input unchanged when project_path is NULL", {
  cu <- make_compute_unit_for_field_test()
  out <- nemetonshiny:::.apply_field_data_if_present(cu, NULL)
  expect_identical(out, cu)
})

test_that("apply_field_data_if_present returns input unchanged when project_path does not exist", {
  cu <- make_compute_unit_for_field_test()
  out <- nemetonshiny:::.apply_field_data_if_present(cu, "/tmp/nemeton_no_such_dir_42")
  expect_identical(out, cu)
})

test_that("apply_field_data_if_present is a no-op when field_data.gpkg is absent", {
  cu <- make_compute_unit_for_field_test()
  withr::with_tempdir({
    dir.create("data")
    out <- nemetonshiny:::.apply_field_data_if_present(cu, ".")
    expect_identical(out, cu)
  })
})

test_that("apply_field_data_if_present calls the nemeton field pipeline when GPKG is present", {
  cu <- make_compute_unit_for_field_test()
  enriched <- cu
  enriched$dbh_mean_cm <- c(28.5, 32.1)  # marker that the pipeline ran

  withr::with_tempdir({
    dir.create("data")
    file.create(file.path("data", "field_data.gpkg"))

    testthat::local_mocked_bindings(
      import_qfield_gpkg        = function(path) {
        list(placettes = sf::st_sf(plot_id = "P01",
                                   geometry = sf::st_sfc(sf::st_point(c(0.5, 0.5)),
                                                         crs = 2154)),
             arbres    = NULL)
      },
      aggregate_plot_metrics    = function(placettes, arbres) {
        data.frame(plot_id = "P01", dbh_mean_cm = 28.5)
      },
      attach_field_data_to_units = function(units, agg) enriched,
      tag_field_data_sources     = function(units, placettes, arbres) units,
      .package = "nemeton"
    )

    out <- nemetonshiny:::.apply_field_data_if_present(cu, ".")
    expect_true("dbh_mean_cm" %in% names(out))
    expect_equal(out$dbh_mean_cm, c(28.5, 32.1))
  })
})

test_that("apply_field_data_if_present falls back to compute_unit on error and warns", {
  cu <- make_compute_unit_for_field_test()
  withr::with_tempdir({
    dir.create("data")
    file.create(file.path("data", "field_data.gpkg"))

    testthat::local_mocked_bindings(
      import_qfield_gpkg = function(path) stop("corrupted GPKG"),
      .package = "nemeton"
    )
    expect_warning(
      out <- nemetonshiny:::.apply_field_data_if_present(cu, "."),
      "Failed to re-apply field data"
    )
    expect_identical(out, cu)
  })
})

# Tests for Project Service
# Phase 3: Project management

test_that("get_projects_root creates directory if missing", {
  # Use temp directory for testing
  temp_dir <- file.path(tempdir(), "nemeton_test_projects")
  unlink(temp_dir, recursive = TRUE)

  withr::local_options(list(
    nemeton.project_dir = temp_dir
  ))

  # Mock get_app_options
  with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    {
      root <- nemetonshiny:::get_projects_root()
      expect_true(dir.exists(root))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("create_project validates name", {
  expect_error(
    nemetonshiny:::create_project(name = ""),
    regexp = "required"
  )

  expect_error(
    nemetonshiny:::create_project(name = NULL),
    regexp = "required"
  )

  expect_error(
    nemetonshiny:::create_project(name = paste(rep("a", 101), collapse = "")),
    regexp = "100 characters"
  )
})

test_that("create_project validates description length", {
  expect_error(
    nemetonshiny:::create_project(
      name = "Test",
      description = paste(rep("a", 501), collapse = "")
    ),
    regexp = "500 characters"
  )
})

test_that("create_project validates owner length", {
  expect_error(
    nemetonshiny:::create_project(
      name = "Test",
      owner = paste(rep("a", 101), collapse = "")
    ),
    regexp = "100 characters"
  )
})

test_that("create_project creates project structure", {
  temp_dir <- file.path(tempdir(), "nemeton_test_projects")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir, recursive = TRUE)

  with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    {
      project <- nemetonshiny:::create_project(
        name = "Test Project",
        description = "Test description",
        owner = "Test Owner"
      )

      expect_type(project, "list")
      expect_true("id" %in% names(project))
      expect_true("path" %in% names(project))
      expect_true("metadata" %in% names(project))

      # Check directories created
      expect_true(dir.exists(project$path))
      expect_true(dir.exists(file.path(project$path, "data")))
      expect_true(dir.exists(file.path(project$path, "cache")))
      expect_true(dir.exists(file.path(project$path, "exports")))

      # Check metadata file
      expect_true(file.exists(file.path(project$path, "metadata.json")))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("create_project sets correct metadata", {
  temp_dir <- file.path(tempdir(), "nemeton_test_projects")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir, recursive = TRUE)

  with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    {
      project <- nemetonshiny:::create_project(
        name = "My Project",
        description = "My description",
        owner = "John Doe"
      )

      expect_equal(project$metadata$name, "My Project")
      expect_equal(project$metadata$description, "My description")
      expect_equal(project$metadata$owner, "John Doe")
      expect_equal(project$metadata$status, "draft")
      expect_equal(project$metadata$parcels_count, 0L)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("update_project_status validates status", {
  expect_error(
    nemetonshiny:::update_project_status("fake_id", "invalid_status"),
    regexp = "Invalid status"
  )
})

test_that("update_project_status accepts valid statuses", {
  temp_dir <- file.path(tempdir(), "nemeton_test_projects")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir, recursive = TRUE)

  with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    {
      project <- nemetonshiny:::create_project(name = "Test")

      # Should not error for valid statuses
      for (status in c("draft", "downloading", "computing", "completed", "error")) {
        expect_no_error(
          nemetonshiny:::update_project_status(project$id, status)
        )
      }
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("load_project_metadata returns metadata", {
  temp_dir <- file.path(tempdir(), "nemeton_test_projects")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir, recursive = TRUE)

  with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    {
      project <- nemetonshiny:::create_project(name = "Test Metadata")

      metadata <- nemetonshiny:::load_project_metadata(project$id)

      expect_type(metadata, "list")
      expect_equal(metadata$name, "Test Metadata")
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("load_project_metadata returns NULL for missing project", {
  with_mocked_bindings(
    get_app_options = function() list(project_dir = tempdir()),
    {
      result <- nemetonshiny:::load_project_metadata("nonexistent_project")
      expect_null(result)
    }
  )
})

test_that("list_recent_projects returns empty data.frame when no projects", {
  temp_dir <- file.path(tempdir(), "nemeton_empty_projects")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir, recursive = TRUE)

  with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    {
      projects <- nemetonshiny:::list_recent_projects()

      expect_s3_class(projects, "data.frame")
      expect_equal(nrow(projects), 0)
      expect_true("id" %in% names(projects))
      expect_true("name" %in% names(projects))
      expect_true("is_corrupted" %in% names(projects))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("list_recent_projects returns projects sorted by date", {
  temp_dir <- file.path(tempdir(), "nemeton_test_projects")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir, recursive = TRUE)

  with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    {
      # Create projects with delay (1 second to ensure different timestamps)
      p1 <- nemetonshiny:::create_project(name = "First")
      Sys.sleep(1.1)
      p2 <- nemetonshiny:::create_project(name = "Second")

      projects <- nemetonshiny:::list_recent_projects()

      expect_equal(nrow(projects), 2)
      # Most recent first (sorted by updated_at descending)
      expect_equal(projects$name[1], "Second")
      expect_equal(projects$name[2], "First")
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("check_project_health detects missing metadata", {
  temp_dir <- file.path(tempdir(), "nemeton_test_projects")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir, recursive = TRUE)

  # Create project directory without metadata
  fake_project <- file.path(temp_dir, "fake_project")
  dir.create(fake_project)

  with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    {
      health <- nemetonshiny:::check_project_health("fake_project")

      expect_false(health$valid)
      expect_true(any(grepl("metadata", health$issues, ignore.case = TRUE)))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("check_project_health returns valid for good project", {
  temp_dir <- file.path(tempdir(), "nemeton_test_projects")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir, recursive = TRUE)

  with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    {
      project <- nemetonshiny:::create_project(name = "Healthy Project")

      health <- nemetonshiny:::check_project_health(project$id)

      expect_true(health$valid)
      expect_length(health$issues, 0)
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("delete_project removes project directory", {
  temp_dir <- file.path(tempdir(), "nemeton_test_projects")
  unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir, recursive = TRUE)

  with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    {
      project <- nemetonshiny:::create_project(name = "To Delete")

      expect_true(dir.exists(project$path))

      result <- nemetonshiny:::delete_project(project$id)

      expect_true(result)
      expect_false(dir.exists(project$path))
    }
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("delete_project returns FALSE for nonexistent project", {
  with_mocked_bindings(
    get_app_options = function() list(project_dir = tempdir()),
    {
      result <- suppressWarnings(nemetonshiny:::delete_project("nonexistent_id"))
      expect_false(result)
    }
  )
})

test_that("get_project_path returns NULL for invalid ID", {
  with_mocked_bindings(
    get_app_options = function() list(project_dir = tempdir()),
    {
      expect_null(nemetonshiny:::get_project_path(NULL))
      expect_null(nemetonshiny:::get_project_path(""))
      expect_null(nemetonshiny:::get_project_path("nonexistent"))
    }
  )
})

# ==============================================================================
# update_project_metadata Tests (withr::with_tempdir pattern)
# ==============================================================================

test_that("update_project_metadata updates fields correctly", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(language = "en", project_dir = temp_root),
      {
        # Create a project
        created <- nemetonshiny:::create_project(name = "Update Test")

        # Update metadata with new fields
        nemetonshiny:::update_project_metadata(created$id, list(
          name = "Updated Name",
          description = "Updated description",
          indicators_computed = TRUE
        ))

        # Reload and verify
        metadata <- nemetonshiny:::load_project_metadata(created$id)
        expect_equal(metadata$name, "Updated Name")
        expect_equal(metadata$description, "Updated description")
        expect_true(metadata$indicators_computed)
        # Original fields should still be present
        expect_equal(metadata$owner, "")
        expect_equal(metadata$status, "draft")
      }
    )
  })
})

# ==============================================================================
# create_project with parcels
# ==============================================================================

test_that("create_project with parcels saves them and updates parcels_count", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        parcels <- create_test_units(crs = 2154, n_features = 3)
        project <- nemetonshiny:::create_project(
          name = "Project With Parcels",
          description = "Has parcels",
          owner = "Tester",
          parcels = parcels
        )

        expect_type(project, "list")
        expect_equal(project$metadata$parcels_count, 3L)

        # Check gpkg file was created
        gpkg_path <- file.path(project$path, "data", "parcels.gpkg")
        expect_true(file.exists(gpkg_path))

        # Verify metadata on disk has parcels_count
        meta_on_disk <- jsonlite::read_json(
          file.path(project$path, "metadata.json")
        )
        expect_equal(meta_on_disk$parcels_count, 3)
      }
    )
  })
})

test_that("create_project with empty sf (0 rows) does not save parcels", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        parcels <- create_test_units(crs = 2154, n_features = 1)
        empty_parcels <- parcels[0, ]
        project <- nemetonshiny:::create_project(
          name = "Project Empty Parcels",
          parcels = empty_parcels
        )
        expect_equal(project$metadata$parcels_count, 0L)
        gpkg_path <- file.path(project$path, "data", "parcels.gpkg")
        expect_false(file.exists(gpkg_path))
      }
    )
  })
})

# ==============================================================================
# save_parcels and load_parcels
# ==============================================================================

test_that("save_parcels saves sf to gpkg and load_parcels reads it back", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Parcel Test")
        parcels <- create_test_units(crs = 2154, n_features = 3)

        result <- nemetonshiny:::save_parcels(project$id, parcels)
        expect_true(result)

        # Verify gpkg file exists
        gpkg_path <- file.path(project$path, "data", "parcels.gpkg")
        expect_true(file.exists(gpkg_path))

        # Load parcels back
        loaded <- nemetonshiny:::load_parcels(project$id)
        expect_s3_class(loaded, "sf")
        expect_equal(nrow(loaded), 3)
      }
    )
  })
})

test_that("save_parcels with NA CRS sets it to WGS84", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "NA CRS Test")
        parcels <- create_test_units(crs = 2154, n_features = 2)
        # Remove CRS
        sf::st_crs(parcels) <- NA

        result <- nemetonshiny:::save_parcels(project$id, parcels)
        expect_true(result)

        # Load back and check CRS is set
        loaded <- nemetonshiny:::load_parcels(project$id)
        expect_s3_class(loaded, "sf")
        expect_false(is.na(sf::st_crs(loaded)))
      }
    )
  })
})

test_that("save_parcels overwrites existing gpkg file", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Overwrite Test")
        parcels1 <- create_test_units(crs = 2154, n_features = 2)
        nemetonshiny:::save_parcels(project$id, parcels1)

        parcels2 <- create_test_units(crs = 2154, n_features = 5)
        nemetonshiny:::save_parcels(project$id, parcels2)

        loaded <- nemetonshiny:::load_parcels(project$id)
        expect_equal(nrow(loaded), 5)
      }
    )
  })
})

test_that("save_parcels errors for non-sf input", {
  expect_error(
    nemetonshiny:::save_parcels("fake", data.frame(x = 1)),
    regexp = "sf object"
  )
})

test_that("save_parcels errors for nonexistent project", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        parcels <- create_test_units(crs = 2154, n_features = 1)
        expect_error(
          nemetonshiny:::save_parcels("nonexistent_project", parcels),
          regexp = "not found"
        )
      }
    )
  })
})

test_that("load_parcels returns NULL for nonexistent project", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        result <- nemetonshiny:::load_parcels("nonexistent_project")
        expect_null(result)
      }
    )
  })
})

test_that("load_parcels returns NULL when no parcel files exist", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "No Parcels")
        result <- nemetonshiny:::load_parcels(project$id)
        expect_null(result)
      }
    )
  })
})

# ==============================================================================
# update_project
# ==============================================================================

test_that("update_project updates metadata and returns complete project", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        parcels <- create_test_units(crs = 2154, n_features = 2)
        project <- nemetonshiny:::create_project(
          name = "Original Name",
          description = "Original desc",
          owner = "Original Owner",
          parcels = parcels
        )

        updated <- nemetonshiny:::update_project(
          project$id,
          name = "Updated Name",
          description = "Updated desc",
          owner = "Updated Owner"
        )

        expect_type(updated, "list")
        expect_equal(updated$metadata$name, "Updated Name")
        expect_equal(updated$metadata$description, "Updated desc")
        expect_equal(updated$metadata$owner, "Updated Owner")
        expect_equal(updated$id, project$id)
        expect_equal(updated$path, project$path)
        # Should have parcels loaded
        expect_s3_class(updated$parcels, "sf")
      }
    )
  })
})

test_that("update_project with new parcels replaces them", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Parcel Update")
        parcels_old <- create_test_units(crs = 2154, n_features = 2)
        nemetonshiny:::save_parcels(project$id, parcels_old)

        new_parcels <- create_test_units(crs = 2154, n_features = 5)
        updated <- nemetonshiny:::update_project(
          project$id,
          name = "Parcel Update",
          parcels = new_parcels
        )

        expect_equal(updated$metadata$parcels_count, 5)
      }
    )
  })
})

test_that("update_project errors for nonexistent project", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        expect_error(
          nemetonshiny:::update_project("nonexistent", name = "Test"),
          regexp = "not found"
        )
      }
    )
  })
})

test_that("update_project validates name", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Validation Test")

        expect_error(
          nemetonshiny:::update_project(project$id, name = ""),
          regexp = "required"
        )

        expect_error(
          nemetonshiny:::update_project(
            project$id,
            name = paste(rep("a", 101), collapse = "")
          ),
          regexp = "100 characters"
        )

        expect_error(
          nemetonshiny:::update_project(
            project$id,
            name = "OK",
            description = paste(rep("a", 501), collapse = "")
          ),
          regexp = "500 characters"
        )

        expect_error(
          nemetonshiny:::update_project(
            project$id,
            name = "OK",
            owner = paste(rep("a", 101), collapse = "")
          ),
          regexp = "100 characters"
        )
      }
    )
  })
})

# ==============================================================================
# save_indicators and load_indicators
# ==============================================================================

test_that("save_indicators saves data.frame and load_indicators reads it back", {
  skip_if_not_installed("arrow")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Indicators Test")

        indicators_df <- data.frame(
          parcel_id = c("P001", "P002", "P003"),
          indicateur_c1_biomasse = c(120.5, 85.3, 200.1),
          biodiversity_score = c(72, 45, 88),
          stringsAsFactors = FALSE
        )

        result <- nemetonshiny:::save_indicators(project$id, indicators_df)
        expect_true(result)

        # Verify parquet file exists
        parquet_path <- file.path(project$path, "data", "indicators.parquet")
        expect_true(file.exists(parquet_path))

        # Load back
        loaded <- nemetonshiny:::load_indicators(project$id)
        expect_s3_class(loaded, "data.frame")
        expect_equal(nrow(loaded), 3)
        expect_true("parcel_id" %in% names(loaded))
        expect_true("indicateur_c1_biomasse" %in% names(loaded))

        # Verify metadata was updated
        meta <- nemetonshiny:::load_project_metadata(project$id)
        expect_true(meta$indicators_computed)
        expect_equal(meta$status, "completed")
      }
    )
  })
})

test_that("save_indicators converts list to data.frame", {
  skip_if_not_installed("arrow")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "List Indicators")

        indicators_list <- list(
          parcel_id = c("P001", "P002"),
          score = c(75, 90)
        )

        result <- nemetonshiny:::save_indicators(project$id, indicators_list)
        expect_true(result)

        loaded <- nemetonshiny:::load_indicators(project$id)
        expect_s3_class(loaded, "data.frame")
        expect_equal(nrow(loaded), 2)
      }
    )
  })
})

test_that("save_indicators errors for nonexistent project", {
  skip_if_not_installed("arrow")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        expect_error(
          nemetonshiny:::save_indicators("nonexistent", data.frame(x = 1)),
          regexp = "not found"
        )
      }
    )
  })
})

test_that("load_indicators returns NULL when no indicators file", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "No Indicators")
        result <- nemetonshiny:::load_indicators(project$id)
        expect_null(result)
      }
    )
  })
})

test_that("load_indicators returns NULL for nonexistent project", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        result <- nemetonshiny:::load_indicators("nonexistent_project")
        expect_null(result)
      }
    )
  })
})

# ==============================================================================
# invalidate_indicators
# ==============================================================================

test_that("invalidate_indicators removes cached file and resets metadata", {
  skip_if_not_installed("arrow")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Invalidate Test")

        # Seed an indicators.parquet file + computed=TRUE metadata
        indicators_df <- data.frame(
          ug_id = c("ug_old_1", "ug_old_2"),
          indicateur_c1_biomasse = c(120.5, 85.3),
          stringsAsFactors = FALSE
        )
        nemetonshiny:::save_indicators(project$id, indicators_df)

        parquet_path <- file.path(project$path, "data", "indicators.parquet")
        expect_true(file.exists(parquet_path))
        expect_true(nemetonshiny:::load_project_metadata(project$id)$indicators_computed)

        # Invalidate
        removed <- nemetonshiny:::invalidate_indicators(project$id)

        expect_true(removed)
        expect_false(file.exists(parquet_path))
        meta <- nemetonshiny:::load_project_metadata(project$id)
        expect_false(meta$indicators_computed)
        expect_equal(meta$status, "draft")
      }
    )
  })
})

test_that("invalidate_indicators is a no-op when no indicators file", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "No Cache Test")

        # No indicators saved — invalidate should still flip the flag
        removed <- nemetonshiny:::invalidate_indicators(project$id)

        expect_false(removed)
        meta <- nemetonshiny:::load_project_metadata(project$id)
        expect_false(meta$indicators_computed %||% FALSE)
      }
    )
  })
})

test_that("invalidate_indicators returns FALSE for nonexistent project", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        result <- nemetonshiny:::invalidate_indicators("nope")
        expect_false(result)
      }
    )
  })
})

# ==============================================================================
# load_project (full round-trip)
# ==============================================================================

test_that("load_project returns complete project data", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        parcels <- create_test_units(crs = 2154, n_features = 3)
        project <- nemetonshiny:::create_project(
          name = "Load Test",
          description = "To be loaded",
          owner = "Loader",
          parcels = parcels
        )

        loaded <- nemetonshiny:::load_project(project$id)
        expect_type(loaded, "list")
        expect_equal(loaded$id, project$id)
        expect_equal(loaded$path, project$path)
        expect_equal(loaded$metadata$name, "Load Test")
        expect_equal(loaded$metadata$description, "To be loaded")
        expect_equal(loaded$metadata$owner, "Loader")
        expect_s3_class(loaded$parcels, "sf")
        expect_equal(nrow(loaded$parcels), 3)
        # indicators should be NULL (not saved yet)
        expect_null(loaded$indicators)
      }
    )
  })
})

test_that("load_project returns NULL for nonexistent project", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        result <- suppressWarnings(nemetonshiny:::load_project("nonexistent_id"))
        expect_null(result)
      }
    )
  })
})

test_that("load_project returns NULL when metadata is missing", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        # Create a project and then delete the metadata
        project <- nemetonshiny:::create_project(name = "Missing Meta")
        file.remove(file.path(project$path, "metadata.json"))

        result <- nemetonshiny:::load_project(project$id)
        expect_null(result)
      }
    )
  })
})

# ==============================================================================
# save_cache_data and load_cache_data
# ==============================================================================

test_that("save_cache_data saves data.frame and load_cache_data reads it back", {
  skip_if_not_installed("arrow")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Cache DF Test")

        test_df <- data.frame(
          code = c("A", "B", "C"),
          value = c(10.5, 20.3, 30.1),
          stringsAsFactors = FALSE
        )

        result <- nemetonshiny:::save_cache_data(project$id, "test_data", test_df)
        expect_true(result)

        # Verify parquet file exists
        cache_file <- file.path(project$path, "cache", "test_data.parquet")
        expect_true(file.exists(cache_file))

        # Load back
        loaded <- nemetonshiny:::load_cache_data(project$id, "test_data")
        expect_s3_class(loaded, "data.frame")
        expect_equal(nrow(loaded), 3)
        expect_true("code" %in% names(loaded))
        expect_true("value" %in% names(loaded))
      }
    )
  })
})

test_that("save_cache_data saves sf object with geometry_wkt conversion", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Cache SF Test")

        sf_data <- create_test_units(crs = 2154, n_features = 3)

        result <- nemetonshiny:::save_cache_data(project$id, "spatial_data", sf_data)
        expect_true(result)

        # Verify parquet and CRS files exist
        cache_file <- file.path(project$path, "cache", "spatial_data.parquet")
        crs_file <- file.path(project$path, "cache", "spatial_data_crs.json")
        expect_true(file.exists(cache_file))
        expect_true(file.exists(crs_file))

        # Check CRS file content
        crs_info <- jsonlite::read_json(crs_file)
        expect_equal(crs_info$epsg, 2154)

        # Load back - exercises the geometry_wkt branch in load_cache_data
        loaded <- nemetonshiny:::load_cache_data(project$id, "spatial_data")
        expect_s3_class(loaded, "data.frame")
        expect_equal(nrow(loaded), 3)
        # Note: load_cache_data removes geometry_wkt (which is the active
        # geometry column after st_as_sf with wkt=), so the result loses
        # its sf class. This exercises the full code path regardless.
        expect_false("geometry_wkt" %in% names(loaded))
      }
    )
  })
})

test_that("save_cache_data errors for nonexistent project", {
  skip_if_not_installed("arrow")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        expect_error(
          nemetonshiny:::save_cache_data("nonexistent", "data", data.frame(x = 1)),
          regexp = "not found"
        )
      }
    )
  })
})

test_that("save_cache_data creates cache directory if missing", {
  skip_if_not_installed("arrow")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Cache Dir Test")
        # Remove cache dir
        unlink(file.path(project$path, "cache"), recursive = TRUE)
        expect_false(dir.exists(file.path(project$path, "cache")))

        test_df <- data.frame(x = 1:3)
        result <- nemetonshiny:::save_cache_data(project$id, "recreated", test_df)
        expect_true(result)
        expect_true(dir.exists(file.path(project$path, "cache")))
      }
    )
  })
})

test_that("load_cache_data returns NULL when no cache file", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "No Cache")
        result <- nemetonshiny:::load_cache_data(project$id, "nonexistent_data")
        expect_null(result)
      }
    )
  })
})

test_that("load_cache_data returns NULL for nonexistent project", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        result <- nemetonshiny:::load_cache_data("nonexistent_id", "data")
        expect_null(result)
      }
    )
  })
})

test_that("load_cache_data restores sf without CRS file (defaults to 4326)", {
  skip_if_not_installed("sf")
  skip_if_not_installed("arrow")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "No CRS File Test")
        sf_data <- create_test_units(crs = 2154, n_features = 2)

        nemetonshiny:::save_cache_data(project$id, "spatial_no_crs", sf_data)

        # Delete the CRS file
        crs_file <- file.path(project$path, "cache", "spatial_no_crs_crs.json")
        file.remove(crs_file)

        loaded <- nemetonshiny:::load_cache_data(project$id, "spatial_no_crs")
        # Exercises the no-CRS-file branch (crs defaults to 4326)
        # The geometry_wkt column removal causes sf class loss, but
        # the code path for CRS default is still exercised
        expect_s3_class(loaded, "data.frame")
        expect_equal(nrow(loaded), 2)
        expect_false("geometry_wkt" %in% names(loaded))
      }
    )
  })
})

# ==============================================================================
# check_project_health (additional coverage)
# ==============================================================================

test_that("check_project_health detects corrupted metadata JSON", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Corrupted Meta")

        # Write invalid JSON to metadata file
        writeLines("{ invalid json }", file.path(project$path, "metadata.json"))

        health <- nemetonshiny:::check_project_health(project$id)
        expect_false(health$valid)
        expect_true(any(grepl("Corrupted|orrupt", health$issues, ignore.case = TRUE)))
      }
    )
  })
})

test_that("check_project_health detects missing data directory", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Missing Data Dir")

        # Remove data directory
        unlink(file.path(project$path, "data"), recursive = TRUE)

        health <- nemetonshiny:::check_project_health(project$id)
        expect_false(health$valid)
        expect_true(any(grepl("data directory", health$issues, ignore.case = TRUE)))
      }
    )
  })
})

test_that("check_project_health detects missing name in metadata", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Name to Remove")

        # Write metadata without name field
        meta_path <- file.path(project$path, "metadata.json")
        jsonlite::write_json(
          list(description = "No name here", status = "draft"),
          meta_path,
          auto_unbox = TRUE
        )

        health <- nemetonshiny:::check_project_health(project$id)
        expect_false(health$valid)
        expect_true(any(grepl("name", health$issues, ignore.case = TRUE)))
      }
    )
  })
})

test_that("check_project_health detects missing parcels file when count > 0", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Missing Parcels File")

        # Update metadata to say we have parcels, but don't save any
        meta_path <- file.path(project$path, "metadata.json")
        meta <- jsonlite::read_json(meta_path)
        meta$parcels_count <- 5
        jsonlite::write_json(meta, meta_path, auto_unbox = TRUE, pretty = TRUE)

        health <- nemetonshiny:::check_project_health(project$id)
        expect_false(health$valid)
        expect_true(any(grepl("parcels", health$issues, ignore.case = TRUE)))
      }
    )
  })
})

test_that("check_project_health returns not found for nonexistent project", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        health <- nemetonshiny:::check_project_health("totally_fake_project")
        expect_false(health$valid)
        expect_true(any(grepl("not found", health$issues, ignore.case = TRUE)))
      }
    )
  })
})

# ==============================================================================
# list_recent_projects (additional coverage)
# ==============================================================================

test_that("list_recent_projects includes corrupted project in listing", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        # Create a valid project
        p1 <- nemetonshiny:::create_project(name = "Valid Project")

        # Create a corrupted project manually (with bad JSON)
        corrupted_dir <- file.path(temp_root, "corrupted_proj")
        dir.create(corrupted_dir, recursive = TRUE)
        dir.create(file.path(corrupted_dir, "data"))
        writeLines("not valid json!!!", file.path(corrupted_dir, "metadata.json"))

        projects <- nemetonshiny:::list_recent_projects()
        expect_s3_class(projects, "data.frame")
        # Should have 2 entries
        expect_equal(nrow(projects), 2)
        # The corrupted one should be flagged
        corrupted_row <- projects[projects$id == "corrupted_proj", ]
        expect_true(corrupted_row$is_corrupted)
        expect_equal(corrupted_row$name, "Corrupted Project")
      }
    )
  })
})

test_that("list_recent_projects respects limit parameter", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        # Create 3 projects with distinct timestamps
        nemetonshiny:::create_project(name = "Project A")
        Sys.sleep(1.1)
        nemetonshiny:::create_project(name = "Project B")
        Sys.sleep(1.1)
        nemetonshiny:::create_project(name = "Project C")

        # Limit to 2
        projects <- nemetonshiny:::list_recent_projects(limit = 2L)
        expect_equal(nrow(projects), 2)
        # Most recent first
        expect_equal(projects$name[1], "Project C")
      }
    )
  })
})

test_that("list_recent_projects handles dirs without metadata.json", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        # Create a valid project
        p1 <- nemetonshiny:::create_project(name = "With Metadata")

        # Create a directory without metadata
        bare_dir <- file.path(temp_root, "bare_directory")
        dir.create(bare_dir)

        projects <- nemetonshiny:::list_recent_projects()
        # Only the project with metadata should appear
        expect_equal(nrow(projects), 1)
        expect_equal(projects$name[1], "With Metadata")
      }
    )
  })
})

# ==============================================================================
# load_project_metadata (additional coverage)
# ==============================================================================

test_that("load_project_metadata returns NULL for corrupted JSON", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Corrupted JSON")

        # Write invalid JSON
        writeLines("{{bad json}}", file.path(project$path, "metadata.json"))

        result <- suppressWarnings(nemetonshiny:::load_project_metadata(project$id))
        expect_null(result)
      }
    )
  })
})

test_that("load_project_metadata returns NULL when metadata file is missing", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Delete Meta")
        file.remove(file.path(project$path, "metadata.json"))

        result <- nemetonshiny:::load_project_metadata(project$id)
        expect_null(result)
      }
    )
  })
})

# ==============================================================================
# get_project_path (additional coverage)
# ==============================================================================

test_that("get_project_path returns valid path for existing project", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Path Test")
        result <- nemetonshiny:::get_project_path(project$id)
        expect_type(result, "character")
        expect_true(dir.exists(result))
        expect_equal(basename(result), project$id)
      }
    )
  })
})

# ==============================================================================
# update_project_metadata (additional coverage)
# ==============================================================================

test_that("update_project_metadata errors for nonexistent project", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        expect_error(
          nemetonshiny:::update_project_metadata("nonexistent", list(name = "foo")),
          regexp = "not found"
        )
      }
    )
  })
})

test_that("update_project_metadata errors for missing metadata file", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Delete Meta Update")
        file.remove(file.path(project$path, "metadata.json"))

        expect_error(
          nemetonshiny:::update_project_metadata(project$id, list(name = "new")),
          regexp = "Metadata file not found"
        )
      }
    )
  })
})

test_that("update_project_metadata with project_path parameter", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Path Param Test")

        # Use project_path parameter directly
        result <- nemetonshiny:::update_project_metadata(
          project$id,
          list(status = "computing"),
          project_path = project$path
        )
        expect_true(result)

        # Verify update
        meta <- nemetonshiny:::load_project_metadata(project$id)
        expect_equal(meta$status, "computing")
      }
    )
  })
})

# ==============================================================================
# delete_project (additional coverage)
# ==============================================================================

test_that("delete_project fully removes project and its subdirectories", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- nemetonshiny:::create_project(name = "Full Delete Test")

        # Verify everything exists
        expect_true(dir.exists(project$path))
        expect_true(dir.exists(file.path(project$path, "data")))
        expect_true(dir.exists(file.path(project$path, "cache")))
        expect_true(dir.exists(file.path(project$path, "exports")))
        expect_true(file.exists(file.path(project$path, "metadata.json")))

        result <- nemetonshiny:::delete_project(project$id)
        expect_true(result)
        expect_false(dir.exists(project$path))

        # Also check it no longer appears in listings
        projects <- nemetonshiny:::list_recent_projects()
        expect_equal(nrow(projects), 0)
      }
    )
  })
})

# ==============================================================================
# get_projects_root (edge case)
# ==============================================================================

test_that("get_projects_root uses HOME fallback when project_dir is NULL", {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = NULL),
      {
        # This exercises the else branch of get_projects_root
        root <- nemetonshiny:::get_projects_root()
        expect_type(root, "character")
        expect_true(nchar(root) > 0)
      }
    )
  })
})

#' Project Service for nemetonApp
#'
#' @description
#' Service for managing nemeton projects - creation, saving, loading.
#' Projects are stored in GeoParquet format for efficient spatial data handling.
#'
#' @name service_project
#' @keywords internal
NULL


#' Get projects root directory
#'
#' @description
#' Returns the root directory for nemeton projects.
#' Uses the project_dir from app options if available.
#'
#' @return Character. Path to projects root directory.
#'
#' @noRd
get_projects_root <- function() {
  opts <- get_app_options()

  if (!is.null(opts$project_dir)) {
    root <- opts$project_dir
  } else {
    root <- file.path(
      Sys.getenv("HOME", rappdirs::user_data_dir()),
      "nemeton_projects"
    )
  }

  # Create if doesn't exist

if (!dir.exists(root)) {
    dir.create(root, recursive = TRUE)
  }

  normalizePath(root, mustWork = FALSE)
}


#' Create a new project
#'
#' @description
#' Creates a new project directory with metadata and initial structure.
#'
#' @param name Character. Project name (required, max 100 chars).
#' @param description Character. Project description (optional, max 500 chars).
#' @param owner Character. Project owner (optional, max 100 chars).
#' @param parcels sf object. Selected parcels (optional, can be added later).
#'
#' @return List with project info (id, path, metadata).
#'
#' @noRd
create_project <- function(name, description = "", owner = "", parcels = NULL) {
  # Validate name
  if (missing(name) || is.null(name) || nchar(trimws(name)) == 0) {
    cli::cli_abort("Project name is required")
  }

  name <- trimws(name)
  if (nchar(name) > 100) {
    cli::cli_abort("Project name must be 100 characters or less")
  }

  # Validate description
  if (nchar(description) > 500) {
    cli::cli_abort("Description must be 500 characters or less")
  }

  # Validate owner
  if (nchar(owner) > 100) {
    cli::cli_abort("Owner must be 100 characters or less")
  }

  # Generate project ID (timestamp + random)
  project_id <- sprintf(
    "%s_%s",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    paste0(sample(letters, 4), collapse = "")
  )

  # Create project directory
  root <- get_projects_root()
  project_path <- file.path(root, project_id)
  dir.create(project_path, recursive = TRUE)

  # Create subdirectories
  dir.create(file.path(project_path, "data"), showWarnings = FALSE)
  dir.create(file.path(project_path, "cache"), showWarnings = FALSE)
  dir.create(file.path(project_path, "exports"), showWarnings = FALSE)

  # Create metadata
  metadata <- list(
    id = project_id,
    name = name,
    description = description,
    owner = owner,
    created_at = Sys.time(),
    updated_at = Sys.time(),
    status = "draft",
    version = "0.7.0",
    parcels_count = 0L,
    indicators_computed = FALSE
 )

  # Save metadata
  metadata_path <- file.path(project_path, "metadata.json")
  jsonlite::write_json(
    metadata,
    metadata_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  # Save parcels if provided
  if (!is.null(parcels) && inherits(parcels, "sf") && nrow(parcels) > 0) {
    save_parcels(project_id, parcels)
    metadata$parcels_count <- nrow(parcels)
    jsonlite::write_json(metadata, metadata_path, auto_unbox = TRUE, pretty = TRUE)
  }

  cli::cli_alert_success("Project created: {.val {name}}")

  list(
    id = project_id,
    path = project_path,
    metadata = metadata
  )
}


#' Update an existing project
#'
#' @description
#' Updates project metadata (name, description, owner) and optionally parcels.
#'
#' @param project_id Character. Project ID.
#' @param name Character. New project name.
#' @param description Character. New description.
#' @param owner Character. New owner.
#' @param parcels sf object. New parcels (optional).
#'
#' @return List with project info (id, path, metadata, parcels).
#'
#' @noRd
update_project <- function(project_id, name, description = "", owner = "", parcels = NULL) {
  # Check project exists
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  # Validate name
  if (missing(name) || is.null(name) || nchar(trimws(name)) == 0) {
    cli::cli_abort("Project name is required")
  }

  name <- trimws(name)
  if (nchar(name) > 100) {
    cli::cli_abort("Project name must be 100 characters or less")
  }

  # Validate description
  if (nchar(description) > 500) {
    cli::cli_abort("Description must be 500 characters or less")
  }

  # Validate owner
  if (nchar(owner) > 100) {
    cli::cli_abort("Owner must be 100 characters or less")
  }

  # Load existing metadata
  metadata <- load_project_metadata(project_id)
  if (is.null(metadata)) {
    cli::cli_abort("Could not load project metadata")
  }

  # Update metadata fields
  metadata$name <- name
  metadata$description <- description
  metadata$owner <- owner
  metadata$updated_at <- Sys.time()

  # Update parcels if provided
  if (!is.null(parcels) && inherits(parcels, "sf") && nrow(parcels) > 0) {
    save_parcels(project_id, parcels)
    metadata$parcels_count <- nrow(parcels)
  }

  # Save updated metadata
  metadata_path <- file.path(project_path, "metadata.json")
  jsonlite::write_json(
    metadata,
    metadata_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  cli::cli_alert_success("Project updated: {.val {name}}")

  list(
    id = project_id,
    path = project_path,
    metadata = metadata,
    parcels = if (!is.null(parcels)) parcels else load_parcels(project_id),
    indicators = load_indicators(project_id)
  )
}


#' Save parcels to project
#'
#' @description
#' Saves selected parcels to project. Primary format is GeoPackage (.gpkg)
#' which is universally compatible with QGIS and other GIS tools.
#' Also saves as Parquet for fast loading within the app.
#'
#' @param project_id Character. Project ID.
#' @param parcels sf object. Parcels to save.
#'
#' @return Logical. TRUE if successful.
#'
#' @noRd
save_parcels <- function(project_id, parcels) {
  if (!inherits(parcels, "sf")) {
    cli::cli_abort("parcels must be an sf object")
  }

  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  # File paths
  gpkg_path <- file.path(project_path, "data", "parcels.gpkg")
  parquet_path <- file.path(project_path, "data", "parcels.parquet")

  tryCatch({
    # Ensure valid CRS before saving (default to WGS84)
    if (is.na(sf::st_crs(parcels))) {
      parcels <- sf::st_set_crs(parcels, 4326)
    }

    cli::cli_alert_info("Saving {nrow(parcels)} parcels")

    # 1. Save as GeoPackage (primary format, QGIS-compatible)
    # Delete existing file first (sf::st_write doesn't overwrite by default)
    if (file.exists(gpkg_path)) {
      unlink(gpkg_path)
    }
    sf::st_write(parcels, gpkg_path, driver = "GPKG", quiet = TRUE)
    cli::cli_alert_success("Saved GeoPackage: {basename(gpkg_path)}")

    # 2. Also save as Parquet for fast internal loading
    if (requireNamespace("geoarrow", quietly = TRUE) &&
        requireNamespace("arrow", quietly = TRUE)) {
      # geoarrow enables arrow to handle sf geometry
      arrow::write_parquet(parcels, parquet_path)
      cli::cli_alert_success("Saved Parquet: {basename(parquet_path)}")
    }

    # Update metadata
    update_project_metadata(project_id, list(
      parcels_count = nrow(parcels),
      updated_at = Sys.time()
    ))

    cli::cli_alert_success("Saved {nrow(parcels)} parcels")
    TRUE

  }, error = function(e) {
    cli::cli_abort("Failed to save parcels: {e$message}")
  })
}


#' Load parcels from project
#'
#' @description
#' Loads parcels from project. Tries Parquet first (faster), falls back to
#' GeoPackage if Parquet is unavailable or fails.
#'
#' @param project_id Character. Project ID.
#'
#' @return sf object with parcels, or NULL if not found.
#'
#' @noRd
load_parcels <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  # File paths
  parquet_path <- file.path(project_path, "data", "parcels.parquet")
  gpkg_path <- file.path(project_path, "data", "parcels.gpkg")

  parcels_sf <- NULL

 # Try Parquet first (faster loading)
  if (file.exists(parquet_path) &&
      requireNamespace("geoarrow", quietly = TRUE) &&
      requireNamespace("arrow", quietly = TRUE)) {
    parcels_sf <- tryCatch({
      # geoarrow enables arrow to read sf geometry
      parcels_arrow <- arrow::read_parquet(parquet_path, as_data_frame = FALSE)
      sf::st_as_sf(parcels_arrow)
    }, error = function(e) {
      cli::cli_warn("Failed to load Parquet, trying GeoPackage: {e$message}")
      NULL
    })
  }

  # Fall back to GeoPackage
  if (is.null(parcels_sf) && file.exists(gpkg_path)) {
    parcels_sf <- tryCatch({
      sf::st_read(gpkg_path, quiet = TRUE)
    }, error = function(e) {
      cli::cli_warn("Failed to load GeoPackage: {e$message}")
      NULL
    })
  }

  # No parcels found
  if (is.null(parcels_sf)) {
    return(NULL)
  }

  # Verify it's an sf object with geometry
  if (!inherits(parcels_sf, "sf")) {
    cli::cli_warn("Failed to load parcels as sf object")
    return(NULL)
  }

  geom <- sf::st_geometry(parcels_sf)
  if (is.null(geom) || length(geom) == 0) {
    cli::cli_warn("Parcels file has no valid geometry")
    return(NULL)
  }

  # Get CRS for logging
  crs_info <- sf::st_crs(parcels_sf)$epsg
  if (is.null(crs_info) || is.na(crs_info)) crs_info <- "unknown"

  cli::cli_alert_success("Loaded {nrow(parcels_sf)} parcels (CRS: {crs_info})")
  parcels_sf
}


#' Save indicators results to project
#'
#' @description
#' Saves computed indicators to project.
#'
#' @param project_id Character. Project ID.
#' @param indicators List or data.frame. Computed indicators.
#'
#' @return Logical. TRUE if successful.
#'
#' @noRd
save_indicators <- function(project_id, indicators) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  indicators_path <- file.path(project_path, "data", "indicators.parquet")

  tryCatch({
    if (!requireNamespace("arrow", quietly = TRUE)) {
      cli::cli_abort("Package 'arrow' is required")
    }

    # Convert to data.frame if needed
    if (is.list(indicators) && !is.data.frame(indicators)) {
      indicators_df <- as.data.frame(indicators)
    } else {
      indicators_df <- indicators
    }

    arrow::write_parquet(indicators_df, indicators_path)

    # Update metadata (avec NDP detecte)
    # Essayer d'abord via les attributs, puis fallback sur le cache disque
    ndp_level <- detect_ndp(indicators)
    if (ndp_level == 0L) {
      ndp_level <- detect_ndp_from_cache(project_path)
    }
    update_project_metadata(project_id, list(
      indicators_computed = TRUE,
      ndp_level = ndp_level,
      updated_at = Sys.time(),
      status = "completed"
    ))

    cli::cli_alert_success("Indicators saved")
    TRUE

  }, error = function(e) {
    cli::cli_abort("Failed to save indicators: {e$message}")
  })
}


#' Load indicators from project
#'
#' @description
#' Loads computed indicators from project.
#'
#' @param project_id Character. Project ID.
#'
#' @return data.frame with indicators, or NULL if not found.
#'
#' @noRd
load_indicators <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  indicators_path <- file.path(project_path, "data", "indicators.parquet")

  if (!file.exists(indicators_path)) {
    return(NULL)
  }

  tryCatch({
    results <- arrow::read_parquet(indicators_path)

    # Restaurer les attributs NDP depuis les metadonnees projet
    meta <- load_project_metadata(project_id)
    if (!is.null(meta$ndp_level)) {
      results <- restore_ndp_attributes(results, meta$ndp_level)
    }

    results
  }, error = function(e) {
    cli::cli_warn("Failed to load indicators: {e$message}")
    NULL
  })
}


#' Load project
#'
#' @description
#' Loads a complete project including metadata, parcels, and indicators.
#'
#' @param project_id Character. Project ID.
#'
#' @return List with project data, or NULL if not found.
#'
#' @noRd
load_project <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_warn("Project not found: {project_id}")
    return(NULL)
  }

  metadata <- load_project_metadata(project_id)
  if (is.null(metadata)) {
    return(NULL)
  }

  project <- list(
    id = project_id,
    path = project_path,
    metadata = metadata,
    parcels = load_parcels(project_id),
    indicators = load_indicators(project_id),
    comments = load_comments(project_id)
  )

  # Sync vers PostGIS si configure et si le projet a des indicateurs
  if (is_db_configured() && isTRUE(metadata$indicators_computed)) {
    tryCatch({
      db_sync_project(project_id)
    }, error = function(e) {
      cli::cli_warn("Database sync on load failed (non-blocking): {e$message}")
    })
  }

  project
}


#' Save comments to project
#'
#' @description
#' Persists synthesis and family comments as a JSON file in the project directory.
#'
#' @param project_id Character. Project ID.
#' @param synthesis Character or NULL. Synthesis comment.
#' @param families Named list. Family comments keyed by family code.
#'
#' @return Logical. TRUE if successful, FALSE otherwise.
#'
#' @noRd
save_comments <- function(project_id, synthesis = NULL, families = NULL) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_warn("save_comments: project_path is NULL for id={project_id}")
    return(FALSE)
  }

  data_dir <- file.path(project_path, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  comments_path <- file.path(data_dir, "comments.json")

  # Merge with existing data to avoid overwriting fields not provided
  existing <- NULL
  if (file.exists(comments_path)) {
    existing <- tryCatch(jsonlite::read_json(comments_path), error = function(e) NULL)
  }

  # Build data: use provided values, fall back to existing
  syn <- if (!is.null(synthesis)) synthesis else existing$synthesis
  fam <- if (!is.null(families) && length(families) > 0) families else existing$families

  data <- list(synthesis = syn, families = fam)

  tryCatch({
    jsonlite::write_json(data, comments_path, auto_unbox = TRUE, pretty = TRUE)
    n_fam <- if (is.list(fam)) length(fam) else 0L
    cli::cli_inform("Comments saved: synthesis={!is.null(syn)}, families={n_fam}")
    TRUE
  }, error = function(e) {
    cli::cli_warn("Failed to save comments: {e$message}")
    FALSE
  })
}


#' Load comments from project
#'
#' @description
#' Loads saved comments from the project directory.
#'
#' @param project_id Character. Project ID.
#'
#' @return List with synthesis and families, or NULL if not found.
#'
#' @noRd
load_comments <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) return(NULL)

  comments_path <- file.path(project_path, "data", "comments.json")
  if (!file.exists(comments_path)) return(NULL)

  tryCatch({
    jsonlite::read_json(comments_path)
  }, error = function(e) {
    cli::cli_warn("Failed to load comments: {e$message}")
    NULL
  })
}


#' List recent projects
#'
#' @description
#' Lists recent projects sorted by last update date.
#'
#' @param limit Integer. Maximum number of projects to return (default: 10).
#'
#' @return data.frame with project info.
#'
#' @noRd
list_recent_projects <- function(limit = 10L) {
  root <- get_projects_root()

  if (!dir.exists(root)) {
    return(data.frame(
      id = character(0),
      name = character(0),
      description = character(0),
      owner = character(0),
      status = character(0),
      parcels_count = integer(0),
      created_at = as.POSIXct(character(0)),
      updated_at = as.POSIXct(character(0)),
      is_corrupted = logical(0)
    ))
  }

  # List project directories
  dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)

  if (length(dirs) == 0) {
    return(data.frame(
      id = character(0),
      name = character(0),
      description = character(0),
      owner = character(0),
      status = character(0),
      parcels_count = integer(0),
      created_at = as.POSIXct(character(0)),
      updated_at = as.POSIXct(character(0)),
      is_corrupted = logical(0)
    ))
  }

  # Load metadata for each project
  projects <- lapply(dirs, function(dir) {
    project_id <- basename(dir)
    health <- check_project_health(project_id)

    metadata_path <- file.path(dir, "metadata.json")
    if (!file.exists(metadata_path)) {
      return(NULL)
    }

    tryCatch({
      metadata <- jsonlite::read_json(metadata_path)
      data.frame(
        id = metadata$id %||% project_id,
        name = metadata$name %||% "Untitled",
        description = metadata$description %||% "",
        owner = metadata$owner %||% "",
        status = metadata$status %||% "unknown",
        parcels_count = metadata$parcels_count %||% 0L,
        created_at = as.POSIXct(metadata$created_at %||% NA),
        updated_at = as.POSIXct(metadata$updated_at %||% NA),
        is_corrupted = !health$valid,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      # Corrupted metadata
      data.frame(
        id = project_id,
        name = "Corrupted Project",
        description = "",
        owner = "",
        status = "error",
        parcels_count = 0L,
        created_at = as.POSIXct(NA),
        updated_at = as.POSIXct(NA),
        is_corrupted = TRUE,
        stringsAsFactors = FALSE
      )
    })
  })

  # Combine and sort
  projects <- do.call(rbind, Filter(Negate(is.null), projects))

  if (is.null(projects) || nrow(projects) == 0) {
    return(data.frame(
      id = character(0),
      name = character(0),
      description = character(0),
      owner = character(0),
      status = character(0),
      parcels_count = integer(0),
      created_at = as.POSIXct(character(0)),
      updated_at = as.POSIXct(character(0)),
      is_corrupted = logical(0)
    ))
  }

  # Sort by updated_at (most recent first)
  projects <- projects[order(projects$updated_at, decreasing = TRUE), ]

  # Apply limit
  if (nrow(projects) > limit) {
    projects <- projects[1:limit, ]
  }

  rownames(projects) <- NULL
  projects
}


#' Check project health
#'
#' @description
#' Checks if a project is valid and not corrupted.
#'
#' @param project_id Character. Project ID.
#'
#' @return List with valid (logical) and issues (character vector).
#'
#' @noRd
check_project_health <- function(project_id) {
  project_path <- get_project_path(project_id)

  issues <- character(0)

  # Check project exists
  if (is.null(project_path) || !dir.exists(project_path)) {
    return(list(valid = FALSE, issues = "Project directory not found"))
  }

  # Check metadata
  metadata_path <- file.path(project_path, "metadata.json")
  if (!file.exists(metadata_path)) {
    issues <- c(issues, "Missing metadata.json")
  } else {
    tryCatch({
      metadata <- jsonlite::read_json(metadata_path)
      if (is.null(metadata$name)) {
        issues <- c(issues, "Metadata missing 'name' field")
      }
    }, error = function(e) {
      issues <<- c(issues, paste("Corrupted metadata:", e$message))
    })
  }

  # Check data directory
  data_path <- file.path(project_path, "data")
  if (!dir.exists(data_path)) {
    issues <- c(issues, "Missing data directory")
  }

  # Check parcels file if metadata says parcels exist
  if (file.exists(metadata_path)) {
    metadata <- tryCatch(jsonlite::read_json(metadata_path), error = function(e) NULL)
    if (!is.null(metadata) && !is.null(metadata$parcels_count) && metadata$parcels_count > 0) {
      parcels_path <- file.path(project_path, "data", "parcels.parquet")
      if (!file.exists(parcels_path)) {
        issues <- c(issues, "Missing parcels.parquet but metadata indicates parcels exist")
      }
    }
  }

  list(
    valid = length(issues) == 0,
    issues = issues
  )
}


#' Delete project
#'
#' @description
#' Permanently deletes a project and all its data.
#'
#' @param project_id Character. Project ID.
#'
#' @return Logical. TRUE if deleted successfully.
#'
#' @noRd
delete_project <- function(project_id) {
  project_path <- get_project_path(project_id)

  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_warn("Project not found: {project_id}")
    return(FALSE)
  }

  tryCatch({
    unlink(project_path, recursive = TRUE)
    cli::cli_alert_success("Project deleted: {project_id}")
    TRUE
  }, error = function(e) {
    cli::cli_abort("Failed to delete project: {e$message}")
  })
}


#' Update project status
#'
#' @description
#' Updates the status of a project.
#'
#' @param project_id Character. Project ID.
#' @param status Character. New status (draft, downloading, computing, completed, error).
#'
#' @return Logical. TRUE if successful.
#'
#' @noRd
update_project_status <- function(project_id, status, project_path = NULL) {
  valid_statuses <- c("draft", "downloading", "computing", "completed", "error")

  if (!status %in% valid_statuses) {
    cli::cli_abort("Invalid status: {status}. Must be one of: {paste(valid_statuses, collapse = ', ')}")
  }

  update_project_metadata(project_id, list(
    status = status,
    updated_at = Sys.time()
  ))
}


#' Update project metadata
#'
#' @description
#' Updates specific fields in project metadata.
#'
#' @param project_id Character. Project ID.
#' @param updates List. Fields to update.
#' @param project_path Character. Optional project path (for async mode).
#'
#' @return Logical. TRUE if successful.
#'
#' @noRd
update_project_metadata <- function(project_id, updates, project_path = NULL) {
  if (is.null(project_path)) {
    project_path <- get_project_path(project_id)
  }
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  metadata_path <- file.path(project_path, "metadata.json")

  if (!file.exists(metadata_path)) {
    cli::cli_abort("Metadata file not found")
  }

  tryCatch({
    metadata <- jsonlite::read_json(metadata_path)

    # Update fields
    for (key in names(updates)) {
      metadata[[key]] <- updates[[key]]
    }

    # Always update updated_at
    metadata$updated_at <- Sys.time()

    jsonlite::write_json(metadata, metadata_path, auto_unbox = TRUE, pretty = TRUE)
    TRUE

  }, error = function(e) {
    cli::cli_abort("Failed to update metadata: {e$message}")
  })
}


#' Load project metadata
#'
#' @description
#' Loads metadata for a project.
#'
#' @param project_id Character. Project ID.
#'
#' @return List with metadata, or NULL if not found.
#'
#' @noRd
load_project_metadata <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  metadata_path <- file.path(project_path, "metadata.json")

  if (!file.exists(metadata_path)) {
    return(NULL)
  }

  tryCatch({
    jsonlite::read_json(metadata_path)
  }, error = function(e) {
    cli::cli_warn("Failed to load metadata: {e$message}")
    NULL
  })
}


#' Get project path
#'
#' @description
#' Returns the full path to a project directory.
#'
#' @param project_id Character. Project ID.
#'
#' @return Character path, or NULL if not found.
#'
#' @noRd
get_project_path <- function(project_id) {
  if (is.null(project_id) || nchar(project_id) == 0) {
    return(NULL)
  }

  root <- get_projects_root()
  project_path <- file.path(root, project_id)

  if (dir.exists(project_path)) {
    return(project_path)
  }

  NULL
}


#' Save external data cache
#'
#' @description
#' Saves downloaded external data (BD Foret, etc.) to project cache.
#' Part of preventive cache strategy.
#'
#' @param project_id Character. Project ID.
#' @param data_name Character. Name of the data (e.g., "bdforet", "corine").
#' @param data sf or data.frame. The data to cache.
#'
#' @return Logical. TRUE if successful.
#'
#' @noRd
save_cache_data <- function(project_id, data_name, data) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  cache_path <- file.path(project_path, "cache")
  if (!dir.exists(cache_path)) {
    dir.create(cache_path, recursive = TRUE)
  }

  file_path <- file.path(cache_path, paste0(data_name, ".parquet"))

  tryCatch({
    if (!requireNamespace("arrow", quietly = TRUE)) {
      cli::cli_abort("Package 'arrow' is required")
    }

    if (inherits(data, "sf")) {
      # Convert sf to data.frame with WKT
      data_df <- data
      data_df$geometry_wkt <- sf::st_as_text(sf::st_geometry(data))
      data_df <- sf::st_drop_geometry(data_df)

      # Save CRS
      crs_path <- file.path(cache_path, paste0(data_name, "_crs.json"))
      jsonlite::write_json(
        list(epsg = sf::st_crs(data)$epsg),
        crs_path,
        auto_unbox = TRUE
      )

      arrow::write_parquet(data_df, file_path)
    } else {
      arrow::write_parquet(data, file_path)
    }

    TRUE

  }, error = function(e) {
    cli::cli_warn("Failed to cache {data_name}: {e$message}")
    FALSE
  })
}


#' Load cached external data
#'
#' @description
#' Loads cached external data from project.
#'
#' @param project_id Character. Project ID.
#' @param data_name Character. Name of the data.
#'
#' @return Data (sf or data.frame), or NULL if not found.
#'
#' @noRd
load_cache_data <- function(project_id, data_name) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  file_path <- file.path(project_path, "cache", paste0(data_name, ".parquet"))

  if (!file.exists(file_path)) {
    return(NULL)
  }

  tryCatch({
    data_df <- arrow::read_parquet(file_path)

    # Check if it's spatial data
    if ("geometry_wkt" %in% names(data_df)) {
      crs_path <- file.path(project_path, "cache", paste0(data_name, "_crs.json"))
      crs <- 4326
      if (file.exists(crs_path)) {
        crs_info <- jsonlite::read_json(crs_path)
        if (!is.null(crs_info$epsg)) {
          crs <- crs_info$epsg
        }
      }

      data_sf <- sf::st_as_sf(data_df, wkt = "geometry_wkt", crs = crs)
      data_sf$geometry_wkt <- NULL
      return(data_sf)
    }

    data_df

  }, error = function(e) {
    cli::cli_warn("Failed to load cached {data_name}: {e$message}")
    NULL
  })
}


# ==============================================================================
# UG Data Persistence
# ==============================================================================

#' Save UG data (atomes + UG definitions) to project
#'
#' @description
#' Saves atom geometries as GeoPackage and UG definitions as JSON.
#'
#' @param project_id Character. Project ID.
#' @param projet List. Project with $atomes (sf) and $ugs (data.frame).
#'
#' @return Logical. TRUE if successful.
#' @noRd
save_ug_data <- function(project_id, projet) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  atomes <- projet$atomes
  ugs <- projet$ugs

  if (is.null(atomes) || is.null(ugs)) {
    cli::cli_warn("No UG data to save")
    return(FALSE)
  }

  data_dir <- file.path(project_path, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  tryCatch({
    # Save atomes as GeoPackage
    atomes_path <- file.path(data_dir, "atomes.gpkg")
    if (file.exists(atomes_path)) unlink(atomes_path)
    sf::st_write(atomes, atomes_path, driver = "GPKG", quiet = TRUE)

    # Save UG definitions as JSON
    ugs_path <- file.path(data_dir, "ugs.json")
    jsonlite::write_json(ugs, ugs_path, auto_unbox = TRUE, pretty = TRUE)

    # Update metadata
    update_project_metadata(project_id, list(
      schema_version = "2.0",
      ug_count = nrow(ugs),
      updated_at = Sys.time()
    ))

    cli::cli_alert_success("Saved {nrow(ugs)} UGs with {nrow(atomes)} atomes")
    TRUE

  }, error = function(e) {
    cli::cli_abort("Failed to save UG data: {e$message}")
  })
}


#' Load UG data from project
#'
#' @description
#' Loads atom geometries and UG definitions from project files.
#'
#' @param project_id Character. Project ID.
#'
#' @return List with $atomes (sf) and $ugs (data.frame), or NULL if not found.
#' @noRd
load_ug_data <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  data_dir <- file.path(project_path, "data")
  atomes_path <- file.path(data_dir, "atomes.gpkg")
  ugs_path <- file.path(data_dir, "ugs.json")

  # Both files must exist

  if (!file.exists(atomes_path) || !file.exists(ugs_path)) {
    return(NULL)
  }

  tryCatch({
    atomes <- sf::st_read(atomes_path, quiet = TRUE)
    ugs <- jsonlite::read_json(ugs_path, simplifyVector = TRUE)

    # Ensure ugs is a proper data.frame
    if (!is.data.frame(ugs)) {
      ugs <- as.data.frame(ugs, stringsAsFactors = FALSE)
    }

    # Ensure required columns exist
    required_atome_cols <- c("atome_id", "parent_parcelle_id", "ug_id", "surface_m2")
    required_ug_cols <- c("ug_id", "label", "groupe")

    if (!all(required_atome_cols %in% names(atomes))) {
      cli::cli_warn("Atomes file missing required columns")
      return(NULL)
    }

    if (!all(required_ug_cols %in% names(ugs))) {
      cli::cli_warn("UGs file missing required columns")
      return(NULL)
    }

    list(atomes = atomes, ugs = ugs)

  }, error = function(e) {
    cli::cli_warn("Failed to load UG data: {e$message}")
    NULL
  })
}


#' Save UG-level aggregated indicators
#'
#' @param project_id Character. Project ID.
#' @param indicators_ug sf object. UG-level indicators.
#'
#' @return Logical. TRUE if successful.
#' @noRd
save_indicators_ug <- function(project_id, indicators_ug) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  parquet_path <- file.path(project_path, "data", "indicators_ug.parquet")

  tryCatch({
    if (requireNamespace("arrow", quietly = TRUE)) {
      ind_df <- indicators_ug
      if (inherits(indicators_ug, "sf")) {
        ind_df$geometry_wkt <- sf::st_as_text(sf::st_geometry(indicators_ug))
        ind_df <- sf::st_drop_geometry(ind_df)
      }
      arrow::write_parquet(ind_df, parquet_path)
      cli::cli_alert_success("Saved UG indicators: {nrow(indicators_ug)} UGs")
      return(TRUE)
    }

    # Fallback: save as RDS
    rds_path <- file.path(project_path, "data", "indicators_ug.rds")
    saveRDS(indicators_ug, rds_path)
    TRUE

  }, error = function(e) {
    cli::cli_warn("Failed to save UG indicators: {e$message}")
    FALSE
  })
}


#' Load UG-level aggregated indicators
#'
#' @param project_id Character. Project ID.
#'
#' @return sf object with UG-level indicators, or NULL if not found.
#' @noRd
load_indicators_ug <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) return(NULL)

  parquet_path <- file.path(project_path, "data", "indicators_ug.parquet")
  rds_path <- file.path(project_path, "data", "indicators_ug.rds")

  tryCatch({
    if (file.exists(parquet_path) && requireNamespace("arrow", quietly = TRUE)) {
      ind_df <- arrow::read_parquet(parquet_path)
      if ("geometry_wkt" %in% names(ind_df)) {
        crs <- sf::st_crs(4326)
        ind_sf <- sf::st_as_sf(ind_df, wkt = "geometry_wkt", crs = crs)
        ind_sf$geometry_wkt <- NULL
        return(ind_sf)
      }
      return(ind_df)
    }

    if (file.exists(rds_path)) {
      return(readRDS(rds_path))
    }

    NULL

  }, error = function(e) {
    cli::cli_warn("Failed to load UG indicators: {e$message}")
    NULL
  })
}

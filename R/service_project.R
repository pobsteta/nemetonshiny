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
create_project <- function(name, description = "", owner = "", parcels = NULL,
                           groupes_profile = NULL) {
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

  # Resolve UGF groupes profile (default from YAML config)
  if (is.null(groupes_profile) || !nzchar(groupes_profile)) {
    groupes_profile <- tryCatch(get_default_groupes_profile(),
                                error = function(e) "onf")
  }

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
    indicators_computed = FALSE,
    groupes_profile = groupes_profile
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
update_project <- function(project_id, name, description = "", owner = "",
                           parcels = NULL, groupes_profile = NULL) {
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
  if (!is.null(groupes_profile) && nzchar(groupes_profile)) {
    metadata$groupes_profile <- groupes_profile
  }

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

    # Convert to data.frame (drop geometry if sf — UGF geometry is
    # rebuilt from tenements.gpkg + ugs.json at load time via
    # ug_build_sf(), keeping indicators.parquet geometry-free).
    if (inherits(indicators, "sf")) {
      indicators_df <- sf::st_drop_geometry(indicators)
    } else if (is.list(indicators) && !is.data.frame(indicators)) {
      indicators_df <- as.data.frame(indicators)
    } else {
      indicators_df <- indicators
    }

    # Write via temp file to avoid Windows memory-mapped file lock
    tmp_path <- paste0(indicators_path, ".tmp")
    arrow::write_parquet(indicators_df, tmp_path)
    if (file.exists(indicators_path)) file.remove(indicators_path)
    file.rename(tmp_path, indicators_path)

    # Update metadata (avec NDP detecte)
    # Essayer d'abord via les attributs, puis fallback sur le cache disque
    ndp_level <- nemeton:::detect_ndp(indicators)
    if (ndp_level == 0L) {
      ndp_level <- nemeton:::detect_ndp_from_cache(project_path)
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
      results <- nemeton:::restore_ndp_attributes(results, meta$ndp_level)
    }

    results
  }, error = function(e) {
    cli::cli_warn("Failed to load indicators: {e$message}")
    NULL
  })
}


#' Invalidate cached indicator results
#'
#' @description
#' Removes \code{indicators.parquet} and flips the \code{indicators_computed}
#' metadata flag back to \code{FALSE}. Call this whenever the UGF layout
#' changes in a way that invalidates previously computed indicator values
#' (e.g. after a découpage import that renumbers \code{ug_id}s — the
#' cached values are keyed on ug_id and silently tell
#' \code{compute_all_indicators()} that "everything is already done",
#' leaving the new UGFs unpopulated).
#'
#' The parcels cache and the UGF layout files (\code{tenements.gpkg},
#' \code{ugs.json}) are untouched — only the indicator results.
#'
#' @param project_id Character. Project ID.
#'
#' @return Invisible TRUE if the indicators file was present and removed,
#'   FALSE if there was nothing to invalidate.
#' @noRd
invalidate_indicators <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(invisible(FALSE))
  }

  indicators_path <- file.path(project_path, "data", "indicators.parquet")
  removed <- FALSE
  if (file.exists(indicators_path)) {
    removed <- file.remove(indicators_path)
  }

  # Reset the computed flag + status so the UI knows the project is
  # back to a "needs compute" state.
  tryCatch({
    update_project_metadata(project_id, list(
      indicators_computed = FALSE,
      status = "draft",
      updated_at = Sys.time()
    ))
  }, error = function(e) {
    cli::cli_warn("Failed to reset indicators flag: {e$message}")
  })

  if (removed) {
    cli::cli_alert_info("Invalidated cached indicators for project {project_id}")
  }
  invisible(removed)
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

  # Auto-migrate to v2 (UG support) if needed
  tryCatch({
    project <- ensure_project_migrated(project_id, project)
    # Build indicators_sf: one row per UGF with geometry + indicator
    # columns joined via ug_id. Used by family/synthesis/export so
    # they can map, score and aggregate at the UGF level without
    # rejoining parcels. Requires both UGF data and indicator data.
    #
    # IMPORTANT: indicators.parquet captures UGF metadata (label, groupe,
    # surfaces, cadastral_refs) AT COMPUTE TIME. If the user later renames
    # / re-groups / splits UGFs, ugs.json gets updated but the parquet
    # still carries stale metadata. We therefore rebuild BOTH
    # project$indicators and project$indicators_sf from the fresh ug_sf,
    # keeping only the indicator VALUES from the parquet. This way every
    # consumer (mod_family table, mod_synthesis, export, ...) sees the
    # current UGF labels/groupes without having to re-join manually.
    if (!is.null(project$indicators) && has_ug_data(project) &&
        "ug_id" %in% names(project$indicators)) {
      ug_sf <- ug_build_sf(project)
      if (!is.null(ug_sf) && nrow(ug_sf) > 0) {
        # Drop stale UGF metadata columns from indicators before merge
        dup_cols <- intersect(
          c("label", "groupe", "surface_m2", "surface_sig_m2",
            "n_tenements", "cadastral_refs"),
          names(project$indicators)
        )
        ind <- project$indicators[, setdiff(names(project$indicators), dup_cols),
                                  drop = FALSE]
        project$indicators_sf <- merge(ug_sf, ind, by = "ug_id", all.x = TRUE)
        # Refresh the geometry-free indicators with fresh UGF metadata.
        # Single source of truth so mod_family, mod_synthesis and
        # service_export stay in sync with the UGF tab.
        project$indicators <- sf::st_drop_geometry(project$indicators_sf)
      }
    }
  }, error = function(e) {
    cli::cli_warn("UG load / indicators_sf build failed (non-blocking): {e$message}")
  })

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

#' Save UG data (tenements + UG definitions) to project
#'
#' @description
#' Saves tenement geometries as GeoPackage and UG definitions as JSON.
#'
#' @param project_id Character. Project ID.
#' @param projet List. Project with $tenements (sf) and $ugs (data.frame).
#'
#' @return Logical. TRUE if successful.
#' @noRd
save_ug_data <- function(project_id, projet) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  tenements <- projet$tenements
  ugs <- projet$ugs

  if (is.null(tenements) || is.null(ugs)) {
    cli::cli_warn("No UG data to save")
    return(FALSE)
  }

  data_dir <- file.path(project_path, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  tryCatch({
    # Save tenements as GeoPackage
    tenements_path <- file.path(data_dir, "tenements.gpkg")
    if (file.exists(tenements_path)) unlink(tenements_path)
    sf::st_write(tenements, tenements_path, driver = "GPKG", quiet = TRUE)

    # Save UG definitions as JSON.
    # NB: We use `dataframe = "columns"` (column-oriented object) instead of
    # the default row-oriented array. When a column is entirely NA (e.g.
    # `groupe` for fresh UGFs), the row-oriented serialisation emits
    # `"groupe": null` on every row, which `read_json(simplifyVector = TRUE)`
    # elides when reconstructing the data.frame — dropping the column
    # entirely. `load_ug_data()` would then see "UGs file missing required
    # columns" and `ensure_project_migrated()` would silently wipe the
    # imported UGF layout with the default 1-UGF-per-parcel migration.
    # Column-oriented output keeps every key present regardless of NA
    # density.
    ugs_path <- file.path(data_dir, "ugs.json")
    jsonlite::write_json(ugs, ugs_path, auto_unbox = TRUE, pretty = TRUE,
                         dataframe = "columns")

    # Update metadata
    update_project_metadata(project_id, list(
      schema_version = "2.1",
      ug_count = nrow(ugs),
      updated_at = Sys.time()
    ))

    cli::cli_alert_success("Saved {nrow(ugs)} UGs with {nrow(tenements)} tenements")
    TRUE

  }, error = function(e) {
    cli::cli_abort("Failed to save UG data: {e$message}")
  })
}


#' Load UG data from project
#'
#' @description
#' Loads tenement geometries and UG definitions from project files.
#'
#' @param project_id Character. Project ID.
#'
#' @return List with $tenements (sf) and $ugs (data.frame), or NULL if not found.
#' @noRd
load_ug_data <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  data_dir <- file.path(project_path, "data")
  tenements_path <- file.path(data_dir, "tenements.gpkg")
  legacy_atomes_path <- file.path(data_dir, "atomes.gpkg")  # pre-v2.1 name
  ugs_path <- file.path(data_dir, "ugs.json")

  # Prefer new filename, fall back to legacy "atomes.gpkg" (schema v2.0 projects)
  gpkg_path <- if (file.exists(tenements_path)) {
    tenements_path
  } else if (file.exists(legacy_atomes_path)) {
    legacy_atomes_path
  } else {
    return(NULL)
  }

  if (!file.exists(ugs_path)) {
    return(NULL)
  }

  tryCatch({
    tenements <- sf::st_read(gpkg_path, quiet = TRUE)
    ugs <- jsonlite::read_json(ugs_path, simplifyVector = TRUE)

    # Backward compat: legacy schema used atome_id, rename to tenement_id
    if ("atome_id" %in% names(tenements) && !"tenement_id" %in% names(tenements)) {
      names(tenements)[names(tenements) == "atome_id"] <- "tenement_id"
    }

    # Ensure ugs is a proper data.frame
    if (!is.data.frame(ugs)) {
      ugs <- as.data.frame(ugs, stringsAsFactors = FALSE)
    }

    # Ensure required columns exist
    required_tenement_cols <- c("tenement_id", "parent_parcelle_id", "ug_id", "surface_m2")
    required_ug_cols <- c("ug_id", "label", "groupe")

    if (!all(required_tenement_cols %in% names(tenements))) {
      cli::cli_warn("Tenements file missing required columns")
      return(NULL)
    }

    # Backward compat: pre-v2.2 projects may lack surface_sig_m2 →
    # compute it on-the-fly from the geometry so downstream code works.
    if (!"surface_sig_m2" %in% names(tenements)) {
      tenements$surface_sig_m2 <- as.numeric(sf::st_area(tenements))
    }

    # Normalise list-columns produced by jsonlite when a field was all
    # `null` in a row-oriented JSON (legacy save format): convert them
    # back to proper character vectors with NA_character_ for empty
    # entries so downstream column checks work.
    for (col in intersect(required_ug_cols, names(ugs))) {
      if (is.list(ugs[[col]])) {
        ugs[[col]] <- vapply(ugs[[col]], function(x) {
          if (is.null(x) || length(x) == 0L) NA_character_ else as.character(x[[1L]])
        }, character(1))
      }
    }

    # Without ug_id there is no way to link tenements to UGs → genuinely
    # broken file, skip.
    if (!"ug_id" %in% names(ugs)) {
      cli::cli_warn("UGs file missing ug_id column")
      return(NULL)
    }

    # `label` and `groupe` can legitimately be all-NA (fresh UGFs have
    # no management group). Older jsonlite round-trips (row-oriented +
    # `null`) dropped these columns entirely — rather than re-triggering
    # the default migration (which silently wipes the imported layout),
    # backfill them with NA so the UGF layout on disk is preserved.
    for (col in setdiff(required_ug_cols, c("ug_id", names(ugs)))) {
      cli::cli_alert_info("Backfilling missing {.val {col}} column in ugs.json")
      ugs[[col]] <- NA_character_
    }

    list(tenements = tenements, ugs = ugs)

  }, error = function(e) {
    cli::cli_warn("Failed to load UG data: {e$message}")
    NULL
  })
}


# NB: save_indicators_ug() / load_indicators_ug() were removed along
# with the obsolete "Recalculer les indicateurs" button. Indicators
# are now computed directly on the UGF geometries in one pass and
# stored in indicators.parquet — there is no separate UGF-level file
# to persist or reload.

#' Project Migration: v1 to v2 (UG support)
#'
#' @description
#' Migrates projects from schema v1 (parcel-only) to v2 (with UG support).
#' Creates default atoms and UGs where 1 parcel = 1 atom = 1 UG.
#'
#' Migration is transparent: existing indicator values are preserved exactly.
#' The UG layer is additive — it sits on top of the unchanged parcel data.
#'
#' @name migrate
#' @keywords internal
NULL


#' Migrate a project from v1 to v2
#'
#' @description
#' Detects projects without UG data and initializes the default mapping:
#' 1 cadastral parcel = 1 atom = 1 UG.
#'
#' This is called automatically when loading a project if schema_version < 2.0.
#'
#' @param project_id Character. Project ID.
#' @param projet List. Optional pre-loaded project (avoids double load).
#'
#' @return Updated projet list with $atomes and $ugs populated.
#' @noRd
migrate_project_v1_to_v2 <- function(project_id, projet = NULL) {
  # Load project if not provided
  if (is.null(projet)) {
    parcels <- load_parcels(project_id)
    if (is.null(parcels)) {
      cli::cli_warn("Cannot migrate project {project_id}: no parcels found")
      return(projet)
    }
    metadata <- load_project_metadata(project_id)
    projet <- list(
      parcels = parcels,
      metadata = metadata
    )
  }

  # Check if already migrated
  if (has_ug_data(projet)) {
    cli::cli_alert_info("Project {project_id} already has UG data, skipping migration")
    return(projet)
  }

  # Check parcels exist
  if (is.null(projet$parcels) || !inherits(projet$parcels, "sf") || nrow(projet$parcels) == 0) {
    cli::cli_warn("Cannot migrate project {project_id}: no valid parcels")
    return(projet)
  }

  cli::cli_alert_info("Migrating project {project_id} from v1 to v2 (adding UG support)")

  # Initialize default UGs
  projet <- ug_init_default(projet)

  # Save UG data
  tryCatch({
    save_ug_data(project_id, projet)
    cli::cli_alert_success(
      "Migration complete: {nrow(projet$ugs)} UGs created for {nrow(projet$parcels)} parcels"
    )
  }, error = function(e) {
    cli::cli_warn("Failed to persist migration for {project_id}: {e$message}")
  })

  projet
}


#' Check if a project needs migration
#'
#' @param project_id Character. Project ID.
#'
#' @return Logical. TRUE if migration is needed.
#' @noRd
needs_migration <- function(project_id) {
  metadata <- load_project_metadata(project_id)
  if (is.null(metadata)) return(FALSE)

  # Projects without schema_version or with version < 2.0 need migration
  version <- metadata$schema_version
  is.null(version) || as.numeric(version) < 2.0
}


#' Ensure a project is migrated before use
#'
#' @description
#' Loads a project and ensures it has UG data. If not, runs migration.
#' This is the safe entry point for loading projects.
#'
#' @param project_id Character. Project ID.
#' @param projet List. Optional pre-loaded project.
#'
#' @return Updated projet with UG data guaranteed.
#' @noRd
ensure_project_migrated <- function(project_id, projet = NULL) {
  # Try to load existing UG data first
  if (!is.null(projet) && has_ug_data(projet)) {
    return(projet)
  }

  ug_data <- load_ug_data(project_id)
  if (!is.null(ug_data)) {
    if (is.null(projet)) {
      projet <- list(
        parcels = load_parcels(project_id),
        metadata = load_project_metadata(project_id),
        indicators = load_indicators(project_id)
      )
    }
    projet$atomes <- ug_data$atomes
    projet$ugs <- ug_data$ugs
    return(projet)
  }

  # No UG data found — run migration
  migrate_project_v1_to_v2(project_id, projet)
}

# Unites de gestion (UGF) d'un projet : chargement, et decoupage par defaut a la
# premiere ouverture (une UGF par parcelle). Ce n'est pas une migration : un
# projet neuf n'a pas encore de decoupage. Les projets anterieurs a la 1.0.0 ne
# sont pas repris (cf. `.projet_format_ok()`).

#' Create the default UGF of a project (one per parcel) and save them
#'
#' @param project_id Character. Project ID.
#' @param projet List. Optional pre-loaded project (avoids double load).
#' @return Updated projet list with `$tenements` and `$ugs` populated.
#' @noRd
.init_project_ug <- function(project_id, projet = NULL) {
  # Load project if not provided
  if (is.null(projet)) {
    parcels <- load_parcels(project_id)
    if (is.null(parcels)) {
      cli::cli_warn("Cannot initialise the UGF of project {project_id}: no parcels found")
      return(projet)
    }
    metadata <- load_project_metadata(project_id)
    projet <- list(
      parcels = parcels,
      metadata = metadata
    )
  }

  # UGF deja presentes : rien a creer
  if (has_ug_data(projet)) {
    return(projet)
  }

  # Check parcels exist
  if (is.null(projet$parcels) || !inherits(projet$parcels, "sf") || nrow(projet$parcels) == 0) {
    cli::cli_warn("Cannot initialise the UGF of project {project_id}: no valid parcels")
    return(projet)
  }

  cli::cli_alert_info("Project {project_id}: default UGF (one per parcel)")

  # Initialize default UGs
  projet <- ug_init_default(projet)

  # Save UG data
  tryCatch({
    save_ug_data(project_id, projet)
    cli::cli_alert_success(
      "{nrow(projet$ugs)} UGF created for {nrow(projet$parcels)} parcels"
    )
  }, error = function(e) {
    cli::cli_warn("Failed to save the UGF of {project_id}: {e$message}")
  })

  projet
}


#' Load a project's UGF, creating the default layout when there is none
#'
#' @param project_id Character. Project ID.
#' @param projet List. Optional pre-loaded project.
#' @return The project list with `$tenements` and `$ugs`.
#' @noRd
ensure_project_ug <- function(project_id, projet = NULL) {
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
    projet$tenements <- ug_data$tenements
    projet$ugs <- ug_data$ugs
    return(projet)
  }

  # Pas de donnees UGF LISIBLES. Projet neuf : on cree le decoupage par defaut.
  # Si des fichiers UGF existent quand meme (lecture en erreur, `ugs.json`
  # absent apres une ecriture interrompue), on les met d'abord de cote : le
  # decoupage de l'utilisateur n'est jamais ecrase sur une erreur de lecture.
  .mettre_de_cote_ug(project_id)
  .init_project_ug(project_id, projet)
}


#' Set aside unreadable UGF files before the default layout overwrites them
#'
#' @description
#' Moves `tenements.gpkg` and `ugs.json` - when any exists - to
#' `data/ug_sauvegarde_<timestamp>/`, records the folder in
#' `metadata$ug_sauvegarde` and warns. Nothing is deleted: the user's layout
#' can be restored by hand.
#'
#' @param project_id Character.
#' @return The backup directory, or `NULL` when there was nothing to set aside.
#' @noRd
.mettre_de_cote_ug <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) return(invisible(NULL))
  data_dir <- file.path(project_path, "data")
  fichiers <- file.path(data_dir, c("tenements.gpkg", "ugs.json"))
  fichiers <- fichiers[file.exists(fichiers)]
  if (!length(fichiers)) return(invisible(NULL))

  dest <- file.path(data_dir,
                    paste0("ug_sauvegarde_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  ok <- file.rename(fichiers, file.path(dest, basename(fichiers)))
  if (!all(ok)) {
    # Repli : copier (le fichier reste en place et sera ecrase, mais la copie
    # est sauve).
    file.copy(fichiers[!ok], dest, overwrite = TRUE)
  }
  cli::cli_warn(c(
    "Projet {project_id} : donnees UGF illisibles, mises de cote.",
    i = "Sauvegarde : {dest}"))
  tryCatch(update_project_metadata(project_id, list(ug_sauvegarde = basename(dest))),
           error = function(e) NULL)
  # Le decoupage recree par defaut a d'autres `ug_id` : les indicateurs
  # calcules sur l'ancien ne correspondent plus.
  tryCatch(invalidate_indicators(project_id, motif = "ugf"), error = function(e) NULL)
  invisible(dest)
}

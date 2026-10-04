# Service synthese : scores de famille et score global d'un projet, hors Shiny.
#
# Extrait de `mod_synthesis.R` pour que l'onglet Synthese et les consommateurs
# sans interface (serveur MCP, cf. specs/BRIEF-pilotage-victor-aigora.md)
# calculent les MEMES chiffres a partir du meme code. Aucune logique metier :
# l'agregation par famille et le score global restent ceux du coeur
# (`create_family_index()`, `nemeton::compute_general_index()`).

#' Family scores of a project (one row per management unit)
#'
#' Enriches `project$indicators_sf` with the conditional R indicators (R5 from
#' the linked monitoring zone, R6/R7 from reGeneration) then aggregates the
#' indicators into the 12 family columns (`famille_*`) via the core.
#'
#' @param project A project list, as returned by [load_project()].
#' @return An sf object with one row per UGF and the `famille_*` columns, or
#'   `NULL` when the project has no indicators or the aggregation fails.
#' @noRd
project_family_scores <- function(project) {
  if (is.null(project)) return(NULL)

  # project$indicators_sf is always built by load_project(): one
  # row per UGF with geometry + indicator columns + label/groupe.
  base_sf <- project$indicators_sf
  if (is.null(base_sf) || !inherits(base_sf, "sf") || nrow(base_sf) == 0) {
    return(NULL)
  }

  # R5 deperissement (32e indicateur, conditionnel) : injecte en direct
  # depuis les alertes de la zone de suivi liee. Best-effort - sans zone
  # / sans alerte, base_sf est inchange et la famille R reste R1-R4.
  base_sf <- add_r5_to_indicators(base_sf, project)

  # R6 (sensibilite 0-100) + R7 (gel) issus de reGeneration. Normalises 0-100
  # par le coeur (>= 0.161.0), ils entrent desormais dans le score de famille R
  # via create_family_index (la famille R passe de R1-R5 a R1-R7). Best-effort :
  # sans cache reGeneration, base_sf est inchange.
  base_sf <- add_regen_r_indicators(base_sf, project)

  tryCatch(
    create_family_index(base_sf, method = "mean", na.rm = TRUE),
    error = function(e) {
      cli::cli_warn("Failed to compute family index: {conditionMessage(e)}")
      NULL
    }
  )
}

#' Mean score of each family over the project's management units
#'
#' @param family_sf Output of [project_family_scores()].
#' @return A named numeric vector (names = `famille_*` columns), empty when
#'   `family_sf` is `NULL` or carries no family column.
#' @noRd
project_family_means <- function(family_sf) {
  if (is.null(family_sf)) return(numeric(0))
  family_cols <- grep("^famille_[a-z]", names(family_sf), value = TRUE)
  if (length(family_cols) == 0) return(numeric(0))
  df <- sf::st_drop_geometry(family_sf)
  vapply(family_cols, function(col) mean(df[[col]], na.rm = TRUE), numeric(1))
}

#' NDP level recorded in a project's metadata
#'
#' @param project A project list.
#' @return A single integer (0 when absent).
#' @noRd
project_ndp_level <- function(project) {
  as.integer(project$metadata$ndp_level %||% 0L)
}

#' Global score of a project (Fibonacci-weighted via the NDP system)
#'
#' @param family_means Output of [project_family_means()].
#' @param ndp_level Integer NDP level.
#' @return The core's `compute_general_index()` result (list with `score`,
#'   `ndp`, `confidence`, ...), or `NULL` when there is no family score.
#' @noRd
project_global_index <- function(family_means, ndp_level = 0L) {
  if (length(family_means) == 0) return(NULL)
  nemeton::compute_general_index(family_means, ndp = ndp_level)
}

#' Serialisable synthesis of a project
#'
#' Same figures as the Synthesis tab (global score, 12 family scores, NDP),
#' as plain R values with no geometry, so they can be returned as JSON by a
#' headless consumer.
#'
#' @param project A project list, as returned by [load_project()].
#' @param language `"fr"` or `"en"`, for the family labels.
#' @param family_sf Optional output of [project_family_scores()] already
#'   computed for this project (avoids aggregating twice).
#' @return A list: `project_id`, `name`, `status`, `ndp_level`, `ndp_name`,
#'   `confidence`, `global_score` (`NA` when not computed), `n_ugf`,
#'   `n_parcels`, `updated_at`, and `families`, a data.frame with `code`,
#'   `famille` and `score` (`NA` for a family without score), in the
#'   canonical family order.
#' @noRd
project_synthesis_summary <- function(project, language = "fr", family_sf = NULL) {
  if (is.null(project)) cli::cli_abort("{.arg project} is NULL.")
  language <- if (identical(language, "en")) "en" else "fr"

  family_sf <- family_sf %||% project_family_scores(project)
  means <- project_family_means(family_sf)
  ndp_level <- project_ndp_level(project)
  index <- project_global_index(means, ndp_level)
  ndp_info <- tryCatch(nemeton::get_ndp_level(ndp_level), error = function(e) NULL)

  codes <- names(INDICATOR_FAMILIES)
  families <- data.frame(
    code = codes,
    famille = vapply(codes, function(code) {
      fam <- INDICATOR_FAMILIES[[code]]
      if (language == "fr") fam$name_fr else fam$name_en
    }, character(1), USE.NAMES = FALSE),
    score = vapply(codes, function(code) {
      col <- get_famille_col(code)
      if (col %in% names(means)) round(unname(means[[col]]), 1) else NA_real_
    }, numeric(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  families$score[is.nan(families$score)] <- NA_real_

  meta <- project$metadata %||% list()
  list(
    project_id = project$id %||% meta$id %||% NA_character_,
    name = meta$name %||% NA_character_,
    status = meta$status %||% NA_character_,
    ndp_level = ndp_level,
    ndp_name = ndp_info$name %||% NA_character_,
    confidence = if (is.null(index)) NA_real_ else round(index$confidence, 3),
    global_score = if (is.null(index)) NA_real_ else index$score,
    n_ugf = if (is.null(family_sf)) 0L else nrow(family_sf),
    n_parcels = if (is.null(project$parcels)) 0L else nrow(project$parcels),
    updated_at = meta$updated_at %||% NA_character_,
    families = families
  )
}

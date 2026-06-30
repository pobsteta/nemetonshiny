# service_r5.R — live R5 dieback column for the synthesis radar
# ------------------------------------------------------------------
# R5 is NOT a standard per-UGF raster indicator: it is derived from the
# dieback alerts persisted for the project's monitoring zone
# (`fordead_dieback` / `reconfort_dieback`) and routed by species. Since
# UGFs carry no essence column (see ug_build_sf / domain_ug.R), each UGF
# is routed by the alert type that intersects it:
#   * a RECONFORT alert inside the UGF  -> feuillus -> RECONFORT scoring,
#   * a FORDEAD   alert inside the UGF  -> resineux -> FORDEAD scoring.
# The actual score and sens-inversion are done in the core
# (nemeton::indicateur_r5_deperissement + normalize_indicator, v0.99.1):
# the app stays free of dieback business logic.

#' Enrich the radar indicator sf with a live R5 dépérissement column
#'
#' Best-effort: returns `base_sf` unchanged when there is no bound
#' monitoring zone, no reachable DB, or no dieback alert. The added
#' column is named `indicateur_r5_deperissement` (raw, "high = dieback")
#' so `nemeton::create_family_index()` detects it via the core
#' `INDICATOR_FAMILIES$R` config and inverts it for the radar.
#'
#' @param base_sf The per-UGF indicator `sf` (`project$indicators_sf`).
#' @param project The current project (reads `metadata$monitoring_zone_id`).
#' @return `base_sf`, possibly with an added `indicateur_r5_deperissement`
#'   column (NA on UGFs with no intersecting alert / no routed method).
#' @keywords internal
add_r5_to_indicators <- function(base_sf, project) {
  if (!inherits(base_sf, "sf") || nrow(base_sf) == 0L) return(base_sf)

  zone_id <- suppressWarnings(as.integer(project$metadata$monitoring_zone_id))
  if (length(zone_id) != 1L || is.na(zone_id)) return(base_sf)

  con <- tryCatch(
    get_monitoring_db_connection(project = project, read_only = TRUE),
    error = function(e) NULL)
  if (is.null(con)) return(base_sf)
  on.exit(close_monitoring_db_connection(con), add = TRUE)

  alerts <- tryCatch(nemeton::list_alerts(con, zone_id, classes = NULL),
                     error = function(e) NULL)
  if (is.null(alerts) || !inherits(alerts, "sf") || nrow(alerts) == 0L) {
    return(base_sf)
  }

  fordead   <- alerts[alerts$alert_type == "fordead_dieback", , drop = FALSE]
  reconfort <- alerts[alerts$alert_type == "reconfort_dieback", , drop = FALSE]
  if (nrow(fordead) == 0L && nrow(reconfort) == 0L) return(base_sf)

  # Route each UGF by the alert type intersecting it (no essence column on
  # UGFs). Spatial test in EPSG:2154.
  ug_m <- tryCatch(sf::st_transform(base_sf, 2154), error = function(e) NULL)
  if (is.null(ug_m)) return(base_sf)
  intersects_any <- function(a) {
    if (nrow(a) == 0L) return(rep(FALSE, nrow(ug_m)))
    a_m <- tryCatch(sf::st_transform(a, 2154), error = function(e) NULL)
    if (is.null(a_m)) return(rep(FALSE, nrow(ug_m)))
    lengths(sf::st_intersects(ug_m, a_m)) > 0L
  }
  base_sf$.r5_resineux <- as.numeric(intersects_any(fordead))
  base_sf$.r5_feuillus <- as.numeric(intersects_any(reconfort))

  out <- tryCatch(
    nemeton::indicateur_r5_deperissement(
      units             = base_sf,
      fordead_results   = list(alerts_sf = fordead),
      reconfort_results = list(alerts_sf = reconfort),
      resineux_col      = ".r5_resineux",
      feuillus_col      = ".r5_feuillus"
    ),
    error = function(e) {
      cli::cli_warn("R5 dieback skipped: {conditionMessage(e)}")
      NULL
    })

  if (is.null(out) || is.null(out$R5)) {
    base_sf$.r5_resineux <- NULL
    base_sf$.r5_feuillus <- NULL
    return(base_sf)
  }

  # indicateur_r5_deperissement() adds column `R5`; rename to the canonical
  # long name (so create_family_index detects it like R1-R4) and drop the
  # temporary routing / diagnostic columns.
  out$indicateur_r5_deperissement <- out$R5
  out$R5 <- NULL
  out$r5_status <- NULL
  out$.r5_resineux <- NULL
  out$.r5_feuillus <- NULL
  out
}

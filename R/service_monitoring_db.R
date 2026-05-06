#' Monitoring DB connection adapter (E6.b)
#'
#' @description
#' Thin adapter that bridges the existing nemetonshiny env-var
#' convention (`NEMETON_DB_HOST` / `_PORT` / `_NAME` / `_USER` /
#' `_PASSWORD`, plus the Clever Cloud `POSTGRESQL_ADDON_*` fallbacks
#' used by `service_db.R`) to the URL-based API exposed by
#' `nemeton::db_connect()`. Calling code does not need to know which
#' style is configured.
#'
#' Resolution order:
#'
#' 1. `NEMETON_DB_URL` — passed straight through to nemeton.
#' 2. `POSTGRESQL_ADDON_*` then `NEMETON_DB_*` parts — assembled into
#'    a postgresql URL (credentials URL-encoded so passwords with `@`,
#'    `:`, `/` … don't break the parse).
#' 3. None of the above set — return NULL so the Monitoring tab can
#'    show a configuration hint instead of crashing.
#'
#' @name service_monitoring_db
#' @keywords internal
NULL


#' Open a connection to the monitoring (TimescaleDB) database
#'
#' @return A DBIConnection or NULL when no DB is configured / the
#'   connection failed.
#' @noRd
get_monitoring_db_connection <- function() {
  if (!requireNamespace("nemeton", quietly = TRUE)) return(NULL)

  url <- Sys.getenv("NEMETON_DB_URL", "")
  if (!nzchar(url)) {
    url <- .build_monitoring_db_url()
  }
  if (!nzchar(url)) return(NULL)

  con <- tryCatch(
    nemeton::db_connect(url),
    error = function(e) {
      cli::cli_warn("Failed to connect to monitoring DB: {conditionMessage(e)}")
      NULL
    }
  )
  .ensure_monitoring_schema(con)
  con
}


#' Run monitoring-schema migrations once per R session
#'
#' Calls `nemeton::db_migrate()`, which is itself idempotent
#' (`schema_migration` table tracks applied versions). Memoized in
#' `.nemeton_env` so subsequent connections skip the round-trip.
#'
#' @noRd
.ensure_monitoring_schema <- function(con) {
  if (is.null(con)) return(invisible(FALSE))
  if (!requireNamespace("nemeton", quietly = TRUE)) return(invisible(FALSE))
  if (isTRUE(.nemeton_env$.monitoring_schema_initialized)) return(invisible(TRUE))
  ok <- tryCatch({
    nemeton::db_migrate(con)
    TRUE
  }, error = function(e) {
    cli::cli_warn("Monitoring schema migration failed: {conditionMessage(e)}")
    FALSE
  })
  if (isTRUE(ok)) .nemeton_env$.monitoring_schema_initialized <- TRUE
  invisible(ok)
}


#' Close a monitoring DB connection
#'
#' @param con A DBIConnection or NULL.
#' @noRd
close_monitoring_db_connection <- function(con) {
  if (is.null(con)) return(invisible(TRUE))
  if (requireNamespace("nemeton", quietly = TRUE)) {
    nemeton::db_disconnect(con)
  }
  invisible(TRUE)
}


#' Build a postgresql URL from individual env vars
#'
#' Returns "" (empty string) when not enough information is available
#' to build a usable URL — caller must treat that as "not configured".
#'
#' @noRd
.build_monitoring_db_url <- function() {
  host <- Sys.getenv("POSTGRESQL_ADDON_HOST",
                     Sys.getenv("NEMETON_DB_HOST", ""))
  if (!nzchar(host)) return("")

  port     <- Sys.getenv("POSTGRESQL_ADDON_PORT",
                         Sys.getenv("NEMETON_DB_PORT", "5432"))
  dbname   <- Sys.getenv("POSTGRESQL_ADDON_DB",
                         Sys.getenv("NEMETON_DB_NAME", ""))
  user     <- Sys.getenv("POSTGRESQL_ADDON_USER",
                         Sys.getenv("NEMETON_DB_USER", ""))
  password <- Sys.getenv("POSTGRESQL_ADDON_PASSWORD",
                         Sys.getenv("NEMETON_DB_PASSWORD", ""))

  # Refuse to emit a URL with empty credentials — nemeton::db_connect()
  # would crash on parse or on dbConnect itself, hiding the real cause.
  if (!all(nzchar(c(host, dbname, user, password)))) return("")

  sprintf("postgresql://%s:%s@%s:%s/%s",
          utils::URLencode(user,     reserved = TRUE),
          utils::URLencode(password, reserved = TRUE),
          host, port, dbname)
}


#' List the monitoring zones registered in the DB
#'
#' Returns a data.frame with columns `id` (integer) and `name`
#' (character). On any error (DB down, schema not migrated, …) returns
#' an empty data.frame with the same columns and emits a warning. The
#' UI must always have a value to bind to.
#'
#' @param con A DBIConnection or NULL.
#' @return data.frame.
#' @noRd
list_monitoring_zones <- function(con) {
  empty <- data.frame(id = integer(0), name = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(con)) return(empty)
  tryCatch(
    DBI::dbGetQuery(con,
      "SELECT id, name FROM monitoring_zone ORDER BY name"),
    error = function(e) {
      cli::cli_warn("Failed to list monitoring zones: {conditionMessage(e)}")
      empty
    }
  )
}


#' Register the loaded project as a monitoring zone (idempotent)
#'
#' Reads the project's persisted samples ([load_samples()]), unions
#' its UGF geometry into a zone polygon, and either re-uses the
#' existing `monitoring_zone_id` from the project metadata (if that
#' zone still exists in the DB) or calls
#' [nemeton::register_monitoring_zone()] to insert a new row. On
#' success, persists the resulting `zone_id` back to the project's
#' `metadata.json`.
#'
#' @param con A DBIConnection (must be non-NULL).
#' @param project A project list with `id`, `metadata`, and
#'   `indicators_sf` (the UGF polygons). Typically
#'   `app_state$current_project`.
#'
#' @return list with:
#'   \itemize{
#'     \item `zone_id`: integer DB id (existing or freshly inserted),
#'     \item `zone_name`: character display name (from project metadata),
#'     \item `n_plots`: integer number of placettes registered,
#'     \item `was_existing`: TRUE if we re-used an already-registered zone.
#'   }
#'   Aborts with a helpful message on any precondition failure
#'   (no DB, no project, no plan, no UGF geometry).
#' @noRd
register_project_as_zone <- function(con, project) {
  if (is.null(con)) {
    cli::cli_abort("Monitoring DB is not connected.")
  }
  if (is.null(project) || is.null(project$id)) {
    cli::cli_abort("No project loaded.")
  }

  plots <- load_samples(project$id)
  if (is.null(plots) || !inherits(plots, "sf") || nrow(plots) == 0L) {
    cli::cli_abort("Project has no sampling plan on disk \\
                    (run the sampling tab first).")
  }
  if (!"plot_id" %in% names(plots)) {
    cli::cli_abort("samples.gpkg is missing the {.val plot_id} column.")
  }

  # Idempotency: the project metadata may already point at a registered
  # zone. Check that the row still exists before reusing — the DB could
  # have been wiped or the zone deleted out-of-band.
  existing_id <- project$metadata$monitoring_zone_id
  if (!is.null(existing_id) && length(existing_id) == 1L &&
      !is.na(suppressWarnings(as.integer(existing_id)))) {
    rs <- tryCatch(
      DBI::dbGetQuery(
        con,
        "SELECT id, name FROM monitoring_zone WHERE id = $1",
        params = list(as.integer(existing_id))
      ),
      error = function(e) NULL
    )
    if (!is.null(rs) && nrow(rs) > 0L) {
      return(list(
        zone_id      = as.integer(rs$id[1]),
        zone_name    = as.character(rs$name[1]),
        n_plots      = nrow(plots),
        was_existing = TRUE
      ))
    }
  }

  if (is.null(project$indicators_sf) ||
      !inherits(project$indicators_sf, "sf") ||
      nrow(project$indicators_sf) == 0L) {
    cli::cli_abort("Project has no UGF geometry to derive a zone polygon.")
  }
  zone_polygon <- sf::st_union(sf::st_transform(project$indicators_sf, 2154))
  zone_polygon <- sf::st_sf(geometry = sf::st_sfc(zone_polygon, crs = 2154))

  zone_name <- project$metadata$name %||% project$id
  zone_id   <- nemeton::register_monitoring_zone(
    con,
    zone_name    = zone_name,
    zone_polygon = zone_polygon,
    placettes    = plots
  )

  update_project_metadata(project$id, list(
    monitoring_zone_id = as.integer(zone_id)
  ))

  list(
    zone_id      = as.integer(zone_id),
    zone_name    = zone_name,
    n_plots      = nrow(plots),
    was_existing = FALSE
  )
}


#' Fetch the AOI polygon for a monitoring zone
#'
#' Reads `monitoring_zone.zone_wkt` (EPSG:4326) and returns the polygon
#' reprojected to Lambert-93 (EPSG:2154), the CRS expected by
#' `nemeton::run_fordead_dieback()` and `nemeton::check_fordead_validity()`.
#'
#' @param con A DBIConnection or NULL.
#' @param zone_id Integer.
#' @return An sf POLYGON in EPSG:2154, or NULL on any failure (zone
#'   absent, geometry malformed, DB down).
#' @noRd
get_monitoring_zone_aoi <- function(con, zone_id) {
  if (is.null(con) || is.null(zone_id)) return(NULL)
  tryCatch({
    rs <- DBI::dbGetQuery(con,
      "SELECT name, zone_wkt FROM monitoring_zone WHERE id = $1",
      params = list(as.integer(zone_id)))
    if (!nrow(rs)) return(NULL)
    geom <- sf::st_as_sfc(rs$zone_wkt[1], crs = 4326)
    aoi <- sf::st_sf(zone_id = as.integer(zone_id),
                     name    = rs$name[1],
                     geometry = geom)
    sf::st_transform(aoi, 2154)
  }, error = function(e) {
    cli::cli_warn("Failed to fetch zone AOI {.val {zone_id}}: {conditionMessage(e)}")
    NULL
  })
}


#' G3 — validity check for a monitoring zone
#'
#' Thin wrapper around `nemeton::check_fordead_validity()` that pulls the
#' zone AOI from the DB. `units` is optional — when missing, only the
#' geographic check is informative and `species_valid` will be NA.
#'
#' @param con A DBIConnection or NULL.
#' @param zone_id Integer.
#' @param units Optional sf of forest units carrying a species column
#'   (`essence_dominante` / `essence` / `species_label` / `species` /
#'   `essence_principale`). When NULL, species check is skipped.
#' @return The list returned by `nemeton::check_fordead_validity()`, or
#'   NULL if the zone or AOI cannot be resolved.
#' @noRd
validity_check_for_zone <- function(con, zone_id, units = NULL) {
  aoi <- get_monitoring_zone_aoi(con, zone_id)
  if (is.null(aoi)) return(NULL)
  if (!requireNamespace("nemeton", quietly = TRUE)) return(NULL)
  tryCatch(
    nemeton::check_fordead_validity(aoi, units = units),
    error = function(e) {
      cli::cli_warn("check_fordead_validity failed: {conditionMessage(e)}")
      NULL
    }
  )
}


#' List alerts for a monitoring zone with disturbance classification
#'
#' Wraps `nemeton::list_alerts()` and applies `classify_disturbance()` so
#' the returned sf carries a `disturbance_type` column. G1 default
#' (classes 3-forte + 4-sol-nu only) is enforced upstream by
#' `nemeton::list_alerts()`.
#'
#' @param con A DBIConnection or NULL.
#' @param zone_id Integer.
#' @param classes Character vector of `confidence_class` values to keep.
#'   Default G1: `c("3-forte", "4-sol-nu")`.
#' @param validation_status Optional filter, e.g. `"pending"`.
#' @param period Optional length-2 Date for trigger_date filtering.
#' @return An sf POINT EPSG:4326, ready for leaflet, or an empty sf on
#'   error.
#' @noRd
list_alerts_for_zone <- function(con, zone_id,
                                 classes = c("3-forte", "4-sol-nu"),
                                 validation_status = NULL,
                                 period = NULL) {
  empty <- sf::st_sf(data.frame(),
                     geometry = sf::st_sfc(crs = 4326))
  if (is.null(con) || is.null(zone_id)) return(empty)
  if (!requireNamespace("nemeton", quietly = TRUE)) return(empty)
  tryCatch({
    a <- nemeton::list_alerts(con, zone_id,
                              classes           = classes,
                              validation_status = validation_status,
                              period            = period)
    if (!nrow(a)) return(a)
    nemeton::classify_disturbance(a, window_days = 30L)
  }, error = function(e) {
    cli::cli_warn("Failed to list alerts for zone {.val {zone_id}}: {conditionMessage(e)}")
    empty
  })
}

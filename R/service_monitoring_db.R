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
#' 3. No PG env vars set AND a `project` is passed → fall back to a
#'    **local DuckDB file** at `<project$path>/data/monitoring.duckdb`.
#'    This is the single-user mode introduced in nemeton v0.21.0 ; it
#'    avoids requiring a TimescaleDB instance for individual users.
#'    Requires the `duckdb` R package.
#' 4. None of the above — return NULL so the Monitoring tab can show
#'    a configuration hint instead of crashing.
#'
#' @name service_monitoring_db
#' @keywords internal
NULL


#' Open a connection to the monitoring database
#'
#' @param project Optional. The current project list (typically
#'   `app_state$current_project`). When provided and no PG env var is
#'   set, a local DuckDB file is used at
#'   `<project$path>/data/monitoring.duckdb`. When NULL and no PG env
#'   var is set, returns NULL — preserves the pre-0.24.0 behaviour for
#'   callers that don't have a project in scope.
#' @param db_url Optional pre-resolved URL. Takes precedence over
#'   `project`. Used by async workers (`run_ingestion_async`,
#'   `run_fordead_async`) that cannot access `app_state` and instead
#'   receive the URL from the observer that fires them.
#'
#' @return A DBIConnection or NULL when no DB is configured / the
#'   connection failed. On failure, `last_monitoring_db_error()` returns
#'   the human-readable cause so the UI can surface it.
#' @noRd
get_monitoring_db_connection <- function(project = NULL, db_url = NULL) {
  # Reset the package-level error slot on every attempt so a previous
  # transient failure doesn't get re-displayed once the user has fixed
  # the cause.
  .nemeton_env$.last_monitoring_db_error <- NULL

  if (!requireNamespace("nemeton", quietly = TRUE)) {
    .nemeton_env$.last_monitoring_db_error <-
      "Package 'nemeton' not installed."
    return(NULL)
  }

  url <- if (!is.null(db_url) && nzchar(db_url)) {
    db_url
  } else {
    .resolve_monitoring_db_url(project)
  }
  if (!nzchar(url)) {
    # Empty URL is not an "error" per se — the resolver already
    # decided we are not configured. The bandeau will show the
    # diagnostic state (no project / duckdb missing / …).
    return(NULL)
  }

  con <- tryCatch(
    nemeton::db_connect(url),
    error = function(e) {
      msg <- conditionMessage(e)
      cli::cli_warn("Failed to connect to monitoring DB: {msg}")
      # Surface the real error to the UI via the bandeau body. Strip
      # nemeton's CRLF-padded multiline cli output to a single line
      # so it fits the status card.
      flat <- gsub("\\s+", " ", msg, perl = TRUE)
      .nemeton_env$.last_monitoring_db_error <- substr(flat, 1L, 240L)
      NULL
    }
  )
  if (is.null(con)) return(NULL)

  # Migration can also fail (e.g. on a corrupted DuckDB file). Capture
  # its error too so the user sees something actionable instead of an
  # empty schema + downstream crashes.
  schema_ok <- tryCatch({
    .ensure_monitoring_schema(con)
    TRUE
  }, error = function(e) {
    msg <- conditionMessage(e)
    cli::cli_warn("Monitoring schema migration failed: {msg}")
    .nemeton_env$.last_monitoring_db_error <-
      paste0("Migration failed: ", substr(gsub("\\s+", " ", msg), 1L, 220L))
    FALSE
  })
  if (!isTRUE(schema_ok)) {
    tryCatch(close_monitoring_db_connection(con), error = function(e) NULL)
    return(NULL)
  }
  con
}


#' Last monitoring DB error message captured by [get_monitoring_db_connection()]
#'
#' Returns the most recent connection failure message (or NULL if the
#' last attempt succeeded). Used by `mod_monitoring`'s status card to
#' surface the real cause when the bandeau falls back to the generic
#' "Base non configurée" path.
#'
#' @noRd
last_monitoring_db_error <- function() {
  .nemeton_env$.last_monitoring_db_error
}


# Feature-flag the DuckDB backend: nemeton >= 0.21.0 exposes
# `.detect_driver` as part of the URL-dispatch rework. Older versions
# (which only handle Postgres URLs) reject a duckdb:// or bare
# .duckdb URL with "Invalid DB URL" — which manifested as a silent
# "Base non configurée" bandeau even though `duckdb` itself was
# installed correctly.
.nemeton_supports_duckdb <- function() {
  if (!requireNamespace("nemeton", quietly = TRUE)) return(FALSE)
  tryCatch(
    {
      # `:::` would also work but is technically a CRAN policy
      # violation; getFromNamespace is the sanctioned form.
      utils::getFromNamespace(".detect_driver", "nemeton")
      TRUE
    },
    error = function(e) FALSE
  )
}


#' Inspect the configured monitoring DB backend without opening it
#'
#' Used by the UI to decide which banner to show ("Postgres ready" vs
#' "Local DuckDB" vs "Not configured"). Returns one of:
#'
#' * `"postgres"` — a PG URL is resolvable (env vars or NEMETON_DB_URL).
#' * `"duckdb"`   — fallback to local DuckDB file (project provided,
#'                  no PG env vars).
#' * `"none"`     — nothing configured.
#'
#' @param project Optional project list (same semantic as
#'   [get_monitoring_db_connection()]).
#' @noRd
monitoring_db_backend <- function(project = NULL) {
  url <- .resolve_monitoring_db_url(project)
  if (!nzchar(url)) return("none")
  # Match the same logic as nemeton:::.detect_driver: a leading
  # `duckdb:` scheme OR a bare path ending in `.duckdb`.
  if (grepl("^duckdb:", url, ignore.case = TRUE) ||
      grepl("\\.duckdb$", url, ignore.case = TRUE)) {
    return("duckdb")
  }
  "postgres"
}


#' Run monitoring-schema migrations on every connection
#'
#' Delegates to `nemeton::db_migrate()`, which is itself idempotent
#' (the `schema_migration` table tracks applied versions, so already-
#' applied migrations are skipped after a single SELECT). The cost on
#' an already-migrated database is sub-millisecond.
#'
#' Historical note: a previous version of this helper memoized success
#' in `.nemeton_env$.monitoring_schema_initialized`. That flag was
#' process-level (shared across ALL connections in the R session),
#' so opening a *fresh* DuckDB file in the same session — different
#' project, deleted file, or just a different `db_url` — caused the
#' migration to be silently SKIPPED, leaving the new file without any
#' table. The bug surfaced as "Catalog Error: Table monitoring_zone
#' does not exist!" at registration time. Always call `db_migrate()`
#' now; the per-version check inside it is the right granularity.
#'
#' @noRd
.ensure_monitoring_schema <- function(con) {
  if (is.null(con)) return(invisible(FALSE))
  if (!requireNamespace("nemeton", quietly = TRUE)) return(invisible(FALSE))
  ok <- tryCatch({
    nemeton::db_migrate(con)
    TRUE
  }, error = function(e) {
    cli::cli_warn("Monitoring schema migration failed: {conditionMessage(e)}")
    FALSE
  })
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


#' Resolve a monitoring DB URL from env vars or project fallback
#'
#' Returns an empty string when nothing is usable — callers must treat
#' "" as "not configured" and either skip the DB call or surface the
#' configuration hint to the user.
#'
#' Resolution order:
#' 1. If `NEMETON_DB_LOCAL` is truthy (1 / true / yes / on, case
#'    insensitive), JUMP STRAIGHT to the DuckDB fallback below. Useful
#'    when developing locally with `POSTGRESQL_ADDON_*` credentials
#'    sitting in `.Renviron` (Clever Cloud creds dumped by the build
#'    pipeline) that would otherwise win the cascade and try to dial
#'    an unreachable Postgres host.
#' 2. `NEMETON_DB_URL` — passed straight through.
#' 3. `POSTGRESQL_ADDON_*` / `NEMETON_DB_*` env parts — assembled into
#'    a postgresql URL.
#' 4. `project$path` + `duckdb` pkg present → local DuckDB file.
#' 5. None of the above — return "".
#'
#' @noRd
.resolve_monitoring_db_url <- function(project = NULL) {
  force_local <- isTRUE(.nemeton_truthy(Sys.getenv("NEMETON_DB_LOCAL", "")))

  if (!force_local) {
    url <- Sys.getenv("NEMETON_DB_URL", "")
    if (nzchar(url)) return(url)

    url <- .build_monitoring_db_url()
    if (nzchar(url)) return(url)
  }

  # Single-user fallback (nemeton v0.21.0+): if a project is loaded
  # and the `duckdb` package is available, drop a monitoring.duckdb
  # next to samples.gpkg under the project's data directory.
  # Requires duckdb (Suggests) — we surface a friendly cli_inform
  # the first time it kicks in so the user understands the mode.
  if (is.null(project) || is.null(project$path)) return("")
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    if (!isTRUE(.nemeton_env$.duckdb_missing_warned)) {
      cli::cli_inform(c(
        "Local DuckDB fallback for monitoring is unavailable.",
        "i" = "Install with {.code install.packages('duckdb')} or set {.envvar NEMETON_DB_URL}."
      ))
      .nemeton_env$.duckdb_missing_warned <- TRUE
    }
    return("")
  }
  # Sanity-check that the installed nemeton actually knows about
  # DuckDB. Pak / install_github sometimes serves a stale v0.21.0
  # tag (or the user upgraded nemetonshiny but not nemeton itself).
  # The `.detect_driver` helper was introduced in v0.21.0 alongside
  # the DuckDB backend; its presence is a reliable feature flag.
  if (!.nemeton_supports_duckdb()) {
    if (!isTRUE(.nemeton_env$.nemeton_too_old_warned)) {
      cli::cli_warn(c(
        "Installed `nemeton` does not support the DuckDB backend.",
        "i" = "Re-install with {.code pak::pak('pobsteta/nemeton@v0.21.0')} (clear cache if needed)."
      ))
      .nemeton_env$.nemeton_too_old_warned <- TRUE
    }
    .nemeton_env$.last_monitoring_db_error <-
      "Installed nemeton is too old (no DuckDB support). Re-install pobsteta/nemeton@v0.21.0."
    return("")
  }
  data_dir <- file.path(project$path, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  duckdb_path <- file.path(data_dir, "monitoring.duckdb")
  if (!isTRUE(.nemeton_env$.duckdb_announced)) {
    if (force_local) {
      cli::cli_alert_info(
        "Monitoring uses local DuckDB at {.path {duckdb_path}} (NEMETON_DB_LOCAL override)."
      )
    } else {
      cli::cli_alert_info(
        "Monitoring uses local DuckDB at {.path {duckdb_path}} (no Postgres configured)."
      )
    }
    .nemeton_env$.duckdb_announced <- TRUE
  }
  # nemeton::db_connect() accepts both `duckdb:///abs/path` and bare
  # paths ending in `.duckdb`. We emit the BARE PATH form so Windows
  # absolute paths (`C:/Users/...`) round-trip correctly. The
  # 3-slash form (`duckdb:///C:/Users/...`) confuses nemeton's
  # `.parse_duckdb_url` on Windows: `sub("^duckdb:(?://)?", "", url)`
  # leaves `/C:/Users/...` (with a leading slash) which DuckDB then
  # rejects as an invalid path.
  normalizePath(duckdb_path, winslash = "/", mustWork = FALSE)
}


# Recognize a wide set of truthy env-var spellings: "1", "TRUE",
# "true", "yes", "on" (case insensitive). Everything else (incl.
# empty string) is FALSE. Used by NEMETON_DB_LOCAL to bypass the PG
# resolution branch.
.nemeton_truthy <- function(x) {
  if (is.null(x) || !length(x) || !nzchar(x)) return(FALSE)
  tolower(x) %in% c("1", "true", "yes", "on", "y", "t")
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

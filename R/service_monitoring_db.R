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

  tryCatch(
    nemeton::db_connect(url),
    error = function(e) {
      cli::cli_warn("Failed to connect to monitoring DB: {conditionMessage(e)}")
      NULL
    }
  )
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

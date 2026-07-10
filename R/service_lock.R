# Project lock — thin app-side wrapper over the core lock API (spec: serveur
# multi-utilisateurs). All locking logic (atomic acquire, re-entrance, TTL steal)
# lives in `nemeton::project_lock_*` (>= 0.148.0). This file only opens/closes a
# short-lived connection per call and forwards — no SQL, no business logic (rule #1).
#
# Why open/close per call: the core deliberately chose a TABLE-based lock, not a
# `pg_advisory_lock`, precisely so the lock survives connection churn. Holding a
# connection open to "keep" the lock would defeat that — the lock is held by the
# HEARTBEAT, not the connection.

# Run `f(con)` on a fresh DB connection, always closed afterwards. Returns the
# `no_db` sentinel when no database is configured (local dev, DB-less deploy):
# there is then no lock to speak of, and the caller treats the project as editable.
.with_lock_con <- function(f) {
  con <- get_db_connection(check_postgis = FALSE)
  if (is.null(con)) return(structure(list(), class = "regen_lock_no_db"))
  on.exit(close_db_connection(con))
  f(con)
}

#' @return `TRUE` when no database is configured — locking is a no-op then.
#' @noRd
lock_no_db <- function(x) inherits(x, "regen_lock_no_db")

#' Acquire the edit lock for a project (opt-in, best-effort)
#'
#' @param pid Project id.
#' @param hid Stable holder id — the OAuth email. Never a session id.
#' @param label Optional display name.
#' @return The core result list (`ok`, `holder_id`, `holder_label`, `stolen`, …),
#'   or the `no_db` sentinel when no database is configured.
#' @noRd
lock_acquire <- function(pid, hid, label = NULL) {
  .with_lock_con(function(con) nemeton::project_lock_acquire(con, pid, hid, label))
}

#' @noRd
lock_heartbeat <- function(pid, hid) {
  res <- .with_lock_con(function(con) nemeton::project_lock_heartbeat(con, pid, hid))
  if (lock_no_db(res)) TRUE else isTRUE(res)   # no DB → we "hold" trivially
}

#' @noRd
lock_release <- function(pid, hid) {
  res <- .with_lock_con(function(con) nemeton::project_lock_release(con, pid, hid))
  if (lock_no_db(res)) invisible(FALSE) else res
}

#' @noRd
lock_status <- function(pid) {
  res <- .with_lock_con(function(con) nemeton::project_lock_status(con, pid))
  if (lock_no_db(res)) NULL else res
}

#' Is the current project open read-only for this user?
#'
#' Read-only when the lock is held by someone else, or when the user is anonymous
#' (no stable identity → never a lock holder). Consumed by modules to gate every
#' mutating action, and by the app-level banner. `app_state$readonly` is set by
#' the lock lifecycle in `app_server`; this is the single point modules read.
#' @noRd
project_is_readonly <- function(app_state) {
  isTRUE(tryCatch(app_state$readonly, error = function(e) FALSE))
}

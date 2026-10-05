# Project lock - thin app-side wrapper over the core lock API (spec: serveur
# multi-utilisateurs). All locking logic (atomic acquire, re-entrance, TTL steal)
# lives in `nemeton::project_lock_*` (>= 0.148.0). This file only opens/closes a
# short-lived connection per call and forwards - no SQL, no business logic (rule #1).
#
# Why open/close per call: the core deliberately chose a TABLE-based lock, not a
# `pg_advisory_lock`, precisely so the lock survives connection churn. Holding a
# connection open to "keep" the lock would defeat that - the lock is held by the
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

#' @return `TRUE` when no database is configured - locking is a no-op then.
#' @noRd
lock_no_db <- function(x) inherits(x, "regen_lock_no_db")

#' Acquire the edit lock for a project (opt-in, best-effort)
#'
#' @param pid Project id.
#' @param hid Stable holder id - the OAuth email. Never a session id.
#' @param label Optional display name.
#' @return The core result list (`ok`, `holder_id`, `holder_label`, `stolen`, ...),
#'   or the `no_db` sentinel when no database is configured.
#' @noRd
lock_acquire <- function(pid, hid, label = NULL) {
  .with_lock_con(function(con) nemeton::project_lock_acquire(con, pid, hid, label))
}

#' @noRd
lock_heartbeat <- function(pid, hid) {
  res <- .with_lock_con(function(con) nemeton::project_lock_heartbeat(con, pid, hid))
  if (lock_no_db(res)) TRUE else isTRUE(res)   # no DB -> we "hold" trivially
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
#' (no stable identity -> never a lock holder). Consumed by modules to gate every
#' mutating action, and by the app-level banner. `app_state$readonly` is set by
#' the lock lifecycle in `app_server`; this is the single point modules read.
#' @noRd
project_is_readonly <- function(app_state) {
  isTRUE(tryCatch(app_state$readonly, error = function(e) FALSE))
}

#' Gate a mutating action when the project is read-only
#'
#' The single guard every module puts at the top of a mutating `observeEvent`:
#' `if (deny_if_readonly(app_state, i18n)) return()`. When read-only it warns the
#' user via a toast and returns `TRUE` (caller must bail); otherwise `FALSE`.
#' `i18n` is optional - when omitted it is resolved from `app_state$language`, so
#' modules without an i18n object in scope can still call it.
#' @noRd
deny_if_readonly <- function(app_state, i18n = NULL) {
  if (!project_is_readonly(app_state)) return(FALSE)
  if (is.null(i18n)) {
    lang <- tryCatch(app_state$language, error = function(e) "fr") %||% "fr"
    i18n <- get_i18n(lang)
  }
  shiny::showNotification(i18n$t("lock_readonly_action"), type = "warning", duration = 5)
  TRUE
}


#' Acquire the edit lock, `NULL` meaning "no lock to hold"
#'
#' @description
#' What `app_server` needs: the core result, or `NULL` when there is no
#' database (or the call failed), in which case the project stays editable
#' without a lock. Without a database `lock_acquire()` returns the `no_db`
#' sentinel - an EMPTY LIST, not `NULL` - and testing `is.null()` on it put
#' every signed-in user of a database-less deployment in read-only mode, as if
#' someone else held the lock.
#'
#' @inheritParams lock_acquire
#' @return The core result list, or `NULL`.
#' @noRd
lock_acquire_or_null <- function(pid, hid, label = NULL) {
  res <- tryCatch(lock_acquire(pid, hid, label), error = function(e) NULL)
  if (lock_no_db(res)) NULL else res
}


#' Should a comment edit be dropped because the project is read-only?
#'
#' Comment text areas save on change. In read-only mode nothing must be
#' written (the lock holder's `comments.json` would be overwritten); the user
#' is told once per session instead of at every keystroke.
#'
#' @param app_state Shared `reactiveValues`.
#' @param session Shiny session (for the once-per-session flag).
#' @return `TRUE` when the edit must not be saved.
#' @noRd
.comments_readonly <- function(app_state, session = shiny::getDefaultReactiveDomain()) {
  if (!project_is_readonly(app_state)) return(FALSE)
  ud <- if (!is.null(session)) session$userData else NULL
  if (is.null(ud) || !isTRUE(ud$.comments_readonly_warned)) {
    if (!is.null(ud)) ud$.comments_readonly_warned <- TRUE
    lang <- tryCatch(shiny::isolate(app_state$language), error = function(e) "fr") %||% "fr"
    shiny::showNotification(get_i18n(lang)$t("lock_readonly_action"),
                            type = "warning", duration = 5)
  }
  TRUE
}


#' Who is acting, for audit trails
#'
#' The connected user (e-mail, else name) when authentication is on. Only in
#' anonymous mode (single-user station, no OAuth) does it fall back to the
#' system account - it used to be the system account always, which made a
#' multi-user history useless (every entry signed by the server account).
#'
#' @param app_state Shared `reactiveValues` (with `$auth` from mod_auth).
#' @return A single character string.
#' @noRd
.acting_user <- function(app_state) {
  auth <- tryCatch(shiny::isolate(app_state$auth), error = function(e) NULL)
  get <- function(k) tryCatch(shiny::isolate(auth[[k]]), error = function(e) NULL)
  if (!is.null(auth) && isTRUE(get("authenticated")) && !isTRUE(get("anonymous"))) {
    who <- get("user_email") %||% get("user_name")
    if (!is.null(who) && length(who) && nzchar(who[[1]])) return(as.character(who[[1]]))
  }
  Sys.info()[["user"]] %||% "user"
}


#' One lock heartbeat, then re-acquire if lost (worker-side body)
#'
#' @param pid,hid Project id and holder id.
#' @param label Holder label for a re-acquire.
#' @return `list(pid, ok, info)`: `ok` is `TRUE` when the lock is (still)
#'   held after the call.
#' @noRd
.lock_heartbeat_run <- function(pid, hid, label = NULL) {
  ok <- tryCatch(lock_heartbeat(pid, hid), error = function(e) FALSE)
  if (isTRUE(ok)) return(list(pid = pid, ok = TRUE, info = NULL))
  res <- tryCatch(lock_acquire(pid, hid, label), error = function(e) list(ok = FALSE))
  list(pid = pid, ok = isTRUE(res$ok), info = res)
}

#' Lock heartbeat off the Shiny loop
#'
#' @inheritParams .lock_heartbeat_run
#' @return A promise resolving to the value of [.lock_heartbeat_run()].
#' @noRd
.lock_heartbeat_async <- function(pid, hid, label = NULL) {
  if (isTRUE(getOption("nemetonshiny.lock_inline")) ||
      !requireNamespace("future", quietly = TRUE)) {
    return(promises::promise(function(resolve, reject) {
      .later_sur(function() resolve(.lock_heartbeat_run(pid, hid, label)))
    }))
  }
  plan_classes <- class(future::plan())
  if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
    .ensure_async_plan()
  }
  dev_path <- .dev_pkg_path_courant()
  app_opts <- getOption("nemeton.app_options")
  promises::future_promise({
    if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
      pkgload::load_all(dev_path, quiet = TRUE)
    } else {
      loadNamespace("nemetonshiny")
    }
    options(nemeton.app_options = app_opts)
    utils::getFromNamespace(".lock_heartbeat_run", "nemetonshiny")(pid, hid, label)
  }, seed = TRUE)
}

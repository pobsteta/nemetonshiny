# Liens profonds : ouvrir l'application sur un projet et un onglet
# (`?project=<id>&tab=<onglet>`), pour un assistant qui pilote l'app
# (specs/BRIEF-pilotage-victor-aigora.md, A.4).

#' Main-navigation tabs a deep link may select
#'
#' Logical tabs (`service_navigation.R`): the `main_nav` panels, the Atlas
#' sub-tabs and the family tabs (`app_ui.R`). A value outside this list is
#' ignored.
#' @noRd
DEEP_LINK_TABS <- c(
  "atlas", "selection", "synthesis", "action_plan", "terrain", "monitoring",
  "regeneration",
  "famille_carbone", "famille_biodiversite", "famille_eau", "famille_air",
  "famille_sol", "famille_paysage", "famille_temporel", "famille_risque",
  "famille_social", "famille_production", "famille_energie",
  "famille_naturalite"
)

#' Parse a deep link query string
#'
#' @param url_search Character. `session$clientData$url_search`
#'   (e.g. `"?project=20261004_101500_abcd&tab=synthesis"`).
#' @return A list with `project` (an existing project id, or `NULL`), `tab`
#'   (an allowed tab, or `NULL`) and `invalid` (names of the parameters given
#'   but rejected: unknown project, unknown tab).
#' @noRd
.parse_deep_link <- function(url_search) {
  out <- list(project = NULL, tab = NULL, invalid = character(0))
  if (is.null(url_search) || !nzchar(url_search)) return(out)
  q <- tryCatch(shiny::parseQueryString(url_search), error = function(e) list())

  p <- q$project
  if (!is.null(p) && nzchar(p)) {
    # get_project_path() refuse deja un identifiant qui n'est pas un simple
    # segment de chemin : la valeur vient du navigateur, elle n'est pas sure.
    if (!is.null(get_project_path(p))) out$project <- p
    else out$invalid <- c(out$invalid, "project")
  }

  t <- q$tab
  if (!is.null(t) && nzchar(t)) {
    if (t %in% DEEP_LINK_TABS) out$tab <- t
    else out$invalid <- c(out$invalid, "tab")
  }
  out
}

#' Script that opens a project through the home module's existing handler
#'
#' Same input the recent-project cards set on click, so a deep link follows the
#' exact interactive load path (lock, monitoring zone, deferred UGF build...).
#'
#' @param project_id A validated project id.
#' @param input_id Namespaced id of the home module's `load_project` input.
#' @return A `<script>` tag.
#' @noRd
.deep_link_load_script <- function(project_id, input_id = "home-load_project") {
  htmltools::tags$script(htmltools::HTML(sprintf(
    "Shiny.setInputValue(%s, %s, {priority: 'event'});",
    jsonlite::toJSON(input_id, auto_unbox = TRUE),
    jsonlite::toJSON(project_id, auto_unbox = TRUE)
  )))
}

#' Origin of the assistant page allowed to receive the "page ready" signal
#'
#' VICTOR opens the app with `window.open()` from its own page and cannot read
#' the app's state (other origin): the app tells it when a deep link has been
#' applied, through `window.opener.postMessage()`. The target origin is never
#' `"*"`: `NEMETON_VICTOR_ORIGIN`, default `http://127.0.0.1:8788`; set but
#' empty, nothing is sent. A value that is not a bare `http(s)://host[:port]`
#' origin is ignored too.
#'
#' @return A single origin string, or `NULL` (no signal).
#' @noRd
.victor_origin <- function() {
  v <- Sys.getenv("NEMETON_VICTOR_ORIGIN", unset = NA_character_)
  if (is.na(v)) v <- "http://127.0.0.1:8788"
  v <- sub("/+$", "", trimws(v))
  if (!nzchar(v) || !grepl("^https?://[A-Za-z0-9.-]+(:[0-9]{1,5})?$", v)) return(NULL)
  v
}

#' Tell the opener page that the deep link has been applied (or refused)
#'
#' Sends the `nemeton_pret` custom message; `custom.js` waits for Shiny to be
#' idle (stable ~1 s) before posting `{source, type, project, tab}` to the
#' opener, and posts nothing when there is no opener.
#'
#' @param session Shiny session.
#' @param type `"ready"` or `"invalid"`.
#' @param project,tab Project id and tab, or `NULL`.
#' @return Invisible `TRUE` when a message was sent.
#' @noRd
.signal_pret <- function(session, type, project = NULL, tab = NULL) {
  origine <- .victor_origin()
  if (is.null(origine)) return(invisible(FALSE))
  session$sendCustomMessage("nemeton_pret", list(
    type = type, project = project %||% "", tab = tab %||% "", origin = origine))
  invisible(TRUE)
}


#' Wire the deep link into a session
#'
#' The project opens through the same input as the recent-project cards; the
#' tab is selected only once THAT project is loaded, otherwise the
#' "completed project required" guard of `app_server()` would send the user
#' straight back to the Selection tab.
#'
#' @param session Shiny session (root).
#' @param app_state Shared `reactiveValues`.
#' @param url_search Function returning the query string; injectable for
#'   tests (`MockShinySession` hard-codes `url_search`).
#' @return Invisible `NULL`; registers two observers. Once the link is applied
#'   (or refused), the opener page is told through [.signal_pret()].
#' @noRd
.setup_deep_link <- function(session, app_state,
                             url_search = function() session$clientData$url_search) {
  pending <- shiny::reactiveValues(project = NULL, tab = NULL, signal = NULL)

  shiny::observeEvent(url_search(), {
    lien <- .parse_deep_link(url_search())
    if (length(lien$invalid)) {
      shiny::showNotification(
        get_i18n(shiny::isolate(app_state$language) %||% "fr")$t("lien_profond_invalide"),
        type = "warning", duration = 6, session = session)
      # Un seul signal par session : le refus prime, meme si une partie du lien
      # (l'onglet) reste appliquee.
      .signal_pret(session, "invalid", lien$project, lien$tab)
    }
    if (!is.null(lien$project)) {
      pending$project <- lien$project
      pending$tab <- lien$tab
      pending$signal <- !length(lien$invalid)
      shiny::insertUI("head", "beforeEnd", .deep_link_load_script(lien$project),
                      immediate = TRUE, session = session)
    } else if (!is.null(lien$tab)) {
      .aller_onglet(session, lien$tab)
      if (!length(lien$invalid)) .signal_pret(session, "ready", NULL, lien$tab)
    } else if (!length(lien$invalid)) {
      # `/` nu : pret au premier repos de la session.
      .signal_pret(session, "ready")
    }
  }, once = TRUE)

  shiny::observeEvent(app_state$current_project, {
    pid <- pending$project
    if (is.null(pid)) return()
    cur <- app_state$current_project$id %||% app_state$current_project$metadata$id
    if (!identical(cur, pid)) return()
    tab <- pending$tab
    signal <- isTRUE(pending$signal)
    pending$project <- NULL
    pending$tab <- NULL
    pending$signal <- NULL
    if (!is.null(tab)) .aller_onglet(session, tab)
    if (signal) .signal_pret(session, "ready", pid, tab)
  })
  invisible(NULL)
}

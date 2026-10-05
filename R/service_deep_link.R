# Liens profonds : ouvrir l'application sur un projet et un onglet
# (`?project=<id>&tab=<onglet>`), pour un assistant qui pilote l'app
# (specs/BRIEF-pilotage-victor-aigora.md, A.4).

#' Main-navigation tabs a deep link may select
#'
#' Values of the `main_nav` panels and of the family tabs (`app_ui.R`). A value
#' outside this list is ignored.
#' @noRd
DEEP_LINK_TABS <- c(
  "selection", "synthesis", "action_plan", "terrain", "monitoring",
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
#' @return Invisible `NULL`; registers two observers.
#' @noRd
.setup_deep_link <- function(session, app_state,
                             url_search = function() session$clientData$url_search) {
  pending <- shiny::reactiveValues(project = NULL, tab = NULL)

  shiny::observeEvent(url_search(), {
    lien <- .parse_deep_link(url_search())
    if (length(lien$invalid)) {
      shiny::showNotification(
        get_i18n(shiny::isolate(app_state$language) %||% "fr")$t("lien_profond_invalide"),
        type = "warning", duration = 6, session = session)
    }
    if (!is.null(lien$project)) {
      pending$project <- lien$project
      pending$tab <- lien$tab
      shiny::insertUI("head", "beforeEnd", .deep_link_load_script(lien$project),
                      immediate = TRUE, session = session)
    } else if (!is.null(lien$tab)) {
      shiny::updateNavbarPage(session, "main_nav", selected = lien$tab)
    }
  }, once = TRUE)

  shiny::observeEvent(app_state$current_project, {
    pid <- pending$project
    if (is.null(pid)) return()
    cur <- app_state$current_project$id %||% app_state$current_project$metadata$id
    if (!identical(cur, pid)) return()
    tab <- pending$tab
    pending$project <- NULL
    pending$tab <- NULL
    if (!is.null(tab)) shiny::updateNavbarPage(session, "main_nav", selected = tab)
  })
  invisible(NULL)
}

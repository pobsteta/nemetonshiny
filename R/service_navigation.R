# Navigation entre onglets : l'onglet principal " Atlas " (`main_nav = "atlas"`)
# porte deux sous-onglets, " Selection " et " Synthese " (`atlas_nav`). Le reste
# de l'application raisonne en onglets LOGIQUES - "selection", "synthesis",
# "action_plan", "famille_*"... - comme avant le regroupement : la garde de
# navigation, le tour guide, les liens profonds et les modules qui ne dessinent
# que s'ils sont visibles. Ces helpers font la traduction dans les deux sens.

#' Sub-tabs of the main "Atlas" tab
#'
#' Values of the `atlas_nav` panels (`app_ui.R`), in display order.
#' @noRd
ATLAS_SUBTABS <- c("selection", "synthesis")

#' Main-navigation tab that hosts a logical tab
#'
#' @param tab A logical tab value (`"selection"`, `"synthesis"`,
#'   `"monitoring"`, ...).
#' @return `"atlas"` for an Atlas sub-tab, `tab` unchanged otherwise.
#' @noRd
.onglet_parent <- function(tab) {
  if (length(tab) == 1L && !is.na(tab) && tab %in% ATLAS_SUBTABS) "atlas" else tab
}

#' Logical tab currently displayed
#'
#' @param main_nav Value of `input$main_nav`.
#' @param atlas_nav Value of `input$atlas_nav` (may be `NULL` before the
#'   client has reported it).
#' @return The Atlas sub-tab when the Atlas tab is active (`"selection"` by
#'   default), `main_nav` otherwise.
#' @noRd
.onglet_effectif <- function(main_nav, atlas_nav) {
  if (!identical(main_nav, "atlas")) return(main_nav)
  if (length(atlas_nav) == 1L && atlas_nav %in% ATLAS_SUBTABS) atlas_nav
  else ATLAS_SUBTABS[[1L]]
}

#' Display a logical tab
#'
#' Selects the hosting main tab and, for an Atlas sub-tab, the sub-tab itself.
#'
#' @param session The ROOT Shiny session (a module session is namespaced and
#'   cannot reach `main_nav`).
#' @param tab A logical tab value.
#' @return `NULL`, invisibly.
#' @noRd
.aller_onglet <- function(session, tab) {
  parent <- .onglet_parent(tab)
  shiny::updateNavbarPage(session, "main_nav", selected = parent)
  if (identical(parent, "atlas") && !identical(tab, "atlas")) {
    bslib::nav_select("atlas_nav", selected = tab, session = session)
  }
  invisible(NULL)
}

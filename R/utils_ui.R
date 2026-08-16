#' Shared UI helpers
#'
#' @description
#' Small layout pieces reused across modules, so that "the same block as the
#' Plan d'actions tab" is true by construction rather than by copy.
#'
#' @name utils_ui
#' @keywords internal
NULL


#' Collapsible "Tableau des actions" card
#'
#' @description
#' The right-hand action panel of the Plan d'actions tab: a green clickable
#' header (icon + title + chevron) folding a body that groups the actions of
#' the view. `mod_action_plan`, `mod_desserte` and `mod_regeneration` share it
#' so a user finds the actions of a view in the same place, under the same
#' header, whatever the tab.
#'
#' The header is the collapse toggle, so `collapse_id` must be namespaced by
#' the calling module - two panels sharing an id would fold each other.
#'
#' @param collapse_id Character. Namespaced id of the collapsible body.
#' @param title Character. Header label, already translated.
#' @param ... UI elements placed in the card body.
#' @param icon Character. bsicons name shown before the title.
#' @param open Logical. Whether the body starts unfolded.
#' @param card_class Character. Classes of the outer card.
#' @param body_class Character. Classes of the card body.
#'
#' @return A [htmltools::tags] div.
#'
#' @noRd
action_table_card <- function(collapse_id,
                              title,
                              ...,
                              icon = "clipboard-check",
                              open = TRUE,
                              card_class = "card mb-3",
                              body_class = "card-body p-3") {
  htmltools::tags$div(
    class = card_class,
    # En-tete cliquable : c'est LUI qui replie le corps, d'ou le curseur main.
    htmltools::tags$div(
      class = "card-header bg-success text-white py-2",
      style = "cursor: pointer;",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = paste0("#", collapse_id),
      `aria-expanded` = if (isTRUE(open)) "true" else "false",
      `aria-controls` = collapse_id,
      htmltools::div(
        class = "d-flex align-items-center justify-content-between",
        htmltools::div(
          class = "d-flex align-items-center",
          bsicons::bs_icon(icon, class = "me-2"),
          title
        ),
        bsicons::bs_icon("chevron-down", class = "collapse-icon")
      )
    ),
    htmltools::tags$div(
      id = collapse_id,
      class = if (isTRUE(open)) "collapse show" else "collapse",
      htmltools::tags$div(class = body_class, ...)
    )
  )
}

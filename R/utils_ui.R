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
#' @param card_id Character or NULL. Id of the OUTER card (header + body).
#'   Only needed when something must target the whole card rather than its
#'   collapsible body - the guided tour does, so that the header stays above
#'   the overlay instead of being left in the dark.
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
                              card_id = NULL,
                              card_class = "card mb-3",
                              body_class = "card-body p-3") {
  htmltools::tags$div(
    id = card_id,
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


#' Render untrusted Markdown to HTML without letting raw HTML through
#'
#' `shiny::markdown()` keeps raw HTML: its `tagfilter` extension only disarms a
#' few tags (`script`, `iframe`, `style`...), so `<img src=x onerror=...>`
#' passes. The texts rendered here are not trusted - documentary sources from
#' `nemeton::format_citations(format = "markdown")`, which the core escapes
#' only in its HTML format (nemeton >= 0.210.0), and LLM answers, which a
#' poisoned corpus document can steer. Every `<` is therefore escaped, except
#' the one opening a Markdown autolink (`<https://...>`, `<mailto:...>`) used
#' by the citations. With no `<` left, no tag can open; `>` (blockquotes) and
#' the rest of Markdown are untouched.
#'
#' @param md Character. Markdown text.
#' @return HTML, as returned by [shiny::markdown()].
#' @noRd
markdown_safe <- function(md) {
  md <- paste(as.character(md %||% ""), collapse = "\n")
  md <- gsub("<(?!(?:https?://|mailto:)[^\\s<>]*>)", "&lt;", md, perl = TRUE)
  shiny::markdown(md)
}


#' Project AOI that only changes when the parcels do
#'
#' `app_state$current_project` is reassigned for many reasons that do not
#' touch the geometry (settings saved, deferred `indicators_sf`, comments):
#' a map rendered from it was rebuilt each time, and its overlays (raster,
#' network, depot places) drawn by proxy disappeared. This reactive recomputes
#' the AOI on every project change but only invalidates its readers when the
#' AOI itself (geometry and attributes) changed.
#'
#' Must be called inside a module server (it registers an observer).
#'
#' @param app_state Shared `reactiveValues`.
#' @return A function returning the AOI (`sf` EPSG:2154) or `NULL`.
#' @noRd
.projet_aoi_stable <- function(app_state) {
  # Valeur initiale calculee tout de suite : la premiere lecture ne doit pas
  # dependre du passage de l'observateur.
  aoi_rv <- shiny::reactiveVal(shiny::isolate(
    tryCatch(.resolve_project_aoi_2154(app_state$current_project),
             error = function(e) NULL)))
  signature <- function(a) {
    if (is.null(a)) return("")
    tryCatch(rlang::hash(list(sf::st_as_binary(sf::st_geometry(a)),
                              sf::st_drop_geometry(a))),
             error = function(e) rlang::hash(a))
  }
  shiny::observeEvent(app_state$current_project, {
    a <- .resolve_project_aoi_2154(app_state$current_project)
    if (!identical(signature(a), signature(shiny::isolate(aoi_rv())))) aoi_rv(a)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  function() aoi_rv()
}

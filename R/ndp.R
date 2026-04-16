#' NDP Shiny Widgets
#'
#' @description
#' Shiny-specific NDP widgets (badge, progress bar). These are UI components
#' that belong in nemeton_shiny, not in the nemeton core package.
#'
#' All other NDP functions (NDP_LEVELS, detect_ndp, compute_general_index,
#' get_ndp_level, set/restore_ndp_attributes, etc.) live in the nemeton
#' core package and are called via nemeton:::.
#'
#' @name ndp_widgets
#' @keywords internal
NULL


#' NDP badge HTML widget
#'
#' Creates an HTML badge displaying the NDP level with color coding.
#'
#' @param ndp Integer. NDP level (0-4).
#' @param lang Character. Language ("fr" or "en"). Default "fr".
#'
#' @return An htmltools tag object.
#' @noRd
ndp_badge <- function(ndp, lang = "fr") {
  ndp <- as.integer(ndp)
  level <- nemeton::get_ndp_level(ndp)

  colors <- c(
    "#6c757d",  # NDP 0 - gray
    "#17a2b8",  # NDP 1 - cyan
    "#28a745",  # NDP 2 - green
    "#ffc107",  # NDP 3 - yellow
    "#fd7e14"   # NDP 4 - orange
  )
  color <- colors[ndp + 1L]

  names_en <- c("Discovery", "Observation", "Exploration", "Diagnostic", "Digital Twin")
  name <- if (lang == "en") names_en[ndp + 1L] else level$name

  htmltools::tags$span(
    class = "badge",
    style = paste0(
      "background-color: ", color, "; ",
      "color: white; ",
      "font-size: 0.85rem; ",
      "padding: 4px 10px;"
    ),
    paste0("NDP ", ndp, " \u2013 ", name)
  )
}


#' NDP confidence progress bar widget
#'
#' Creates an HTML progress bar showing the confidence phi percentage.
#'
#' @param ndp Integer. NDP level (0-4).
#' @param lang Character. Language ("fr" or "en"). Default "fr".
#'
#' @return An htmltools tag object.
#' @noRd
ndp_progress_bar <- function(ndp, lang = "fr") {
  ndp <- as.integer(ndp)
  level <- nemeton::get_ndp_level(ndp)
  pct <- round(level$confidence * 100, 1)

  label <- if (lang == "en") {
    paste0("Confidence \u03c6: ", pct, "%")
  } else {
    paste0("Confiance \u03c6 : ", pct, "%")
  }

  htmltools::div(
    class = "mt-1",
    htmltools::tags$small(class = "text-muted", label),
    htmltools::div(
      class = "progress",
      style = "height: 8px;",
      htmltools::div(
        class = "progress-bar bg-success",
        role = "progressbar",
        style = paste0("width: ", pct, "%;"),
        `aria-valuenow` = pct,
        `aria-valuemin` = "0",
        `aria-valuemax` = "100"
      )
    )
  )
}

#' nemetonApp Theme Configuration
#'
#' @description
#' Theme and accessibility configuration for the nemetonApp Shiny application.
#' Implements WCAG 2.1 AA accessibility guidelines.
#'
#' @name utils_theme
#' @keywords internal
NULL


#' Create the nemeton bslib theme
#'
#' @description
#' Creates a forest-themed bslib theme with WCAG 2.1 AA compliant colors.
#' All colors have been tested for contrast ratio >= 4.5:1 for text
#' and >= 3:1 for UI components.
#'
#' @return A bslib::bs_theme object
#'
#' @details
#' Color palette:
#' \itemize{
#'   \item Primary: Forest Green (#1B6B1B)
#'   \item Secondary: Saddle Brown (#6B3710)
#'   \item Success: Forest Green (#1B6B1B) - merged with Primary (single green)
#'   \item Info: Steel Blue (#2B5B8B)
#'   \item Warning: Goldenrod (#9B7510)
#'   \item Danger: Crimson (#B01030)
#' }
#'
#' @noRd
nemeton_theme <- function() {
 if (!requireNamespace("bslib", quietly = TRUE)) {
    cli::cli_abort("Package 'bslib' is required for nemeton_theme()")
  }

  theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",

    # Forest color palette - WCAG AA compliant
    # All colors tested with WebAIM contrast checker
    primary = "#1B6B1B",
    secondary = "#6B3710",
    success = "#1B6B1B",  # fusionne avec primary : un seul vert (UX boutons)
    info = "#2B5B8B",
    warning = "#9B7510",
    danger = "#B01030",

    # Background and foreground
    # Page bg is light gray so browser freeze doesn't show as white screen
    bg = "#f0f0f0",
    fg = "#2C3E50",

    # Typography
    base_font = bslib::font_google("Open Sans", wght = "400;600;700"),
    heading_font = bslib::font_google("Montserrat", wght = "500;700"),
    code_font = bslib::font_google("Fira Code"),

    # Font sizes
    "font-size-base" = "1rem",
    "h1-font-size" = "2rem",
    "h2-font-size" = "1.75rem",
    "h3-font-size" = "1.5rem",

    # Spacing
    "spacer" = "1rem",

    # Border radius
    "border-radius" = "0.375rem",

    # Enable responsive font sizes
    "enable-responsive-font-sizes" = TRUE
  )

  # Add custom CSS rules
  theme <- bslib::bs_add_rules(theme, "
    /* Focus visible for accessibility */
    :focus-visible {
      outline: 3px solid #005FCC !important;
      outline-offset: 2px !important;
    }

    /* Skip link for keyboard navigation */
    .skip-link {
      position: absolute;
      top: -40px;
      left: 0;
      background: #1B6B1B;
      color: white;
      padding: 8px 16px;
      z-index: 10000;
      transition: top 0.3s;
    }

    .skip-link:focus {
      top: 0;
    }

    /* Minimum touch target size (44x44px) */
    .btn, .form-control, .form-select, .nav-link {
      min-height: 44px;
    }

    /* Card styling */
    .card {
      border: 1px solid rgba(0,0,0,0.125);
      box-shadow: 0 2px 4px rgba(0,0,0,0.05);
    }

    .card-header {
      background-color: #f8f9fa;
      font-weight: 600;
    }

    /* Navbar styling */
    .navbar {
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }

    /* Sidebar styling */
    .sidebar {
      border-right: 1px solid rgba(0,0,0,0.1);
    }

    /* Table styling */
    .table th {
      background-color: #f8f9fa;
      font-weight: 600;
    }

    /* Alert styling */
    .alert {
      border-left-width: 4px;
    }

    /* Progress bar */
    .progress {
      height: 24px;
      border-radius: 12px;
    }

    .progress-bar {
      font-weight: 600;
    }

    /* Cards stay white for clean content appearance */
    .card {
      background-color: #ffffff;
    }

    .card-body {
      background-color: #ffffff;
    }
  ")

  theme
}


#' Information "i" - the one and only pattern
#'
#' @description
#' The blue `circle-info` icon opening a popover, as used next to the titles of
#' the Synthesis tab (`Score global`, `Radar`, `Recapitulatif par famille`).
#' This is the app-wide pattern for an information affordance: every new "i"
#' must go through this helper rather than re-inventing an icon.
#'
#' Deliberately a **popover** (click) and not a tooltip (hover): the content is
#' explanatory prose, it must stay on screen while being read, and it may be
#' scrollable - a hover tooltip vanishes as soon as the pointer moves toward it.
#'
#' @param ... Popover content, passed on to [bslib::popover()].
#' @param placement Character. Popover side, see [bslib::popover()].
#'
#' @return A [bslib::popover()] tag.
#'
#' @noRd
info_popover <- function(..., placement = "auto") {
  bslib::popover(
    htmltools::tags$span(
      class = "text-info",
      style = "cursor: help;",
      shiny::icon("circle-info", class = "fa-sm")
    ),
    ...,
    options = list(customClass = "popover-lg"),
    placement = placement,
    title = NULL
  )
}


#' Information "i" inside the `<label>` of a radio / checkbox choice
#'
#' @description
#' Same "i" as [info_popover()], but safe to put inside the label of a choice
#' (`choiceNames` of [shiny::radioButtons()], a checkbox label, ...).
#'
#' A click anywhere in a `<label>` activates its control - so an "i" placed
#' there would SELECT the choice on its way to opening the popover. Informing
#' oneself is not choosing, and the cost is real: on the map-layer radios, the
#' selected layer triggers a raster read (and, in the regeneration context view,
#' an ~800 MB E-OBS download).
#'
#' The popover's own handler runs first (capture-phase), then the label's
#' default action is cancelled. Note it does NOT stop propagation: the document
#' must keep seeing the click, otherwise already-open popovers would never
#' close.
#'
#' @param ... Popover content, passed on to [info_popover()].
#' @param placement Character. Popover side.
#'
#' @return A `<span>` wrapping the popover trigger.
#'
#' @noRd
info_popover_in_label <- function(..., placement = "auto") {
  htmltools::tags$span(
    onclick = "event.preventDefault();",
    info_popover(..., placement = placement)
  )
}


#' Why there is no `info_popover_in_header()`
#'
#' @description
#' An "i" placed inside a collapse toggle - an `accordion_panel()` title, a
#' `data-bs-toggle="collapse"` card header - CANNOT be made safe. Measured in
#' Chrome, three attempts, all of which still folded the panel:
#'
#' - inline `onclick="event.stopPropagation()"` on a wrapping `<span>`;
#' - the same plus `preventDefault()`;
#' - a document-level listener in the CAPTURE phase, instrumented to confirm it
#'   fired and matched the target before calling `stopPropagation()`.
#'
#' Bootstrap registers its own handler first, so nothing added afterwards can
#' get in front of it. Asking for help would collapse the panel being read.
#'
#' The pattern to use instead is a row holding the action and its "i" side by
#' side inside the panel BODY - see `.dess_action_info()` in `mod_desserte.R`.
#' Keep this note: the header placement looks obvious and has now cost three
#' rounds of debugging.
#'
#' @name info_popover_header_note
#' @keywords internal
#' @noRd
NULL

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
#'   \item Success: Lime Green (#1E7B1E)
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
    success = "#1E7B1E",
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


#' Accessibility configuration
#'
#' @description
#' Configuration constants for WCAG 2.1 AA accessibility compliance.
#'
#' @details
#' Guidelines implemented:
#' \itemize{
#'   \item 1.4.3 Contrast (Minimum): Text contrast >= 4.5:1
#'   \item 1.4.11 Non-text Contrast: UI contrast >= 3:1
#'   \item 2.1.1 Keyboard: All functionality keyboard accessible
#'   \item 2.4.7 Focus Visible: Focus indicator clearly visible
#'   \item 2.5.5 Target Size: Touch targets >= 44x44 CSS pixels
#' }
#'
#' @noRd
ACCESSIBILITY_CONFIG <- list(
  # Color palettes - colorblind-friendly only
  color_palettes = c("viridis", "plasma", "inferno", "magma", "cividis"),
  default_palette = "viridis",

  # Always use symbols in addition to colors
  use_symbols = TRUE,
  # Circle, triangle up, square, diamond, triangle down, plus, cross
  symbol_shapes = c(16L, 17L, 15L, 18L, 25L, 3L, 4L),

  # Minimum touch target size (WCAG 2.5.5)
  min_touch_target = 44L,  # pixels

  # Focus indicator (WCAG 2.4.7)
  focus_ring_width = 3L,   # pixels
  focus_ring_color = "#005FCC",
  focus_ring_offset = 2L,  # pixels

  # Contrast ratios (WCAG 1.4.3, 1.4.11)
  min_text_contrast = 4.5,
  min_ui_contrast = 3.0,

  # Font sizes
  min_font_size = 16L,  # pixels

  # Line height for readability
  line_height = 1.5
)


#' Get accessibility configuration
#'
#' @param key Configuration key
#' @param default Default value if not found
#' @return Configuration value
#' @noRd
get_accessibility_config <- function(key, default = NULL) {
  if (key %in% names(ACCESSIBILITY_CONFIG)) {
    return(ACCESSIBILITY_CONFIG[[key]])
  }
  return(default)
}


#' Get colorblind-friendly palette
#'
#' @description
#' Returns a viridis-family color palette that is safe for colorblind users.
#'
#' @param n Integer. Number of colors needed.
#' @param palette Character. Palette name (default: "viridis").
#' @param alpha Numeric. Alpha transparency (0-1).
#'
#' @return Character vector of hex colors.
#'
#' @noRd
get_accessible_palette <- function(n, palette = "viridis", alpha = 1) {
  valid_palettes <- get_accessibility_config("color_palettes")

  if (!palette %in% valid_palettes) {
    cli::cli_warn("Palette '{palette}' is not colorblind-friendly. Using 'viridis'.")
    palette <- "viridis"
  }

  if (requireNamespace("viridisLite", quietly = TRUE)) {
    viridisLite::viridis(n, alpha = alpha, option = palette)
  } else {
    # Fallback to basic viridis approximation
    grDevices::hcl.colors(n, palette = "viridis")
  }
}


#' Get symbol shapes for accessibility
#'
#' @description
#' Returns symbol shapes to use in addition to colors for accessibility.
#'
#' @param n Integer. Number of symbols needed.
#'
#' @return Integer vector of pch values.
#'
#' @noRd
get_accessible_symbols <- function(n) {
  symbols <- get_accessibility_config("symbol_shapes")
  rep(symbols, length.out = n)
}


#' Family colors for consistent visualization
#'
#' @description
#' Returns a named vector of colors for the 12 indicator families.
#' Colors are from the viridis palette for colorblind accessibility.
#'
#' @return Named character vector of hex colors.
#'
#' @noRd
get_family_colors <- function() {
  # Use viridis palette for 12 families
  colors <- get_accessible_palette(12)
  names(colors) <- c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")
  colors
}


#' Apply accessible styling to ggplot
#'
#' @description
#' Returns a ggplot2 theme with accessibility improvements.
#'
#' @return A ggplot2 theme object.
#'
#' @noRd
theme_nemeton_accessible <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package 'ggplot2' is required for theme_nemeton_accessible()")
  }

  ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      # Text
      text = ggplot2::element_text(
        family = "Open Sans",
        color = "#2C3E50"
      ),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(1.2),
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        color = "#666666",
        margin = ggplot2::margin(b = 10)
      ),

      # Axes
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(size = ggplot2::rel(0.9)),

      # Legend
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),

      # Grid
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#EEEEEE"),

      # Background
      panel.background = ggplot2::element_rect(fill = "#f0f0f0", color = NA),
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", color = NA),

      # Strip (facets)
      strip.background = ggplot2::element_rect(fill = "#f8f9fa"),
      strip.text = ggplot2::element_text(face = "bold")
    )
}


#' Build radar chart data in canonical family order
#'
#' Aggregates family columns to mean values and returns a data.frame
#' with factor-ordered labels matching the INDICATOR_FAMILIES sequence.
#'
#' @param df data.frame (no geometry). Must contain family columns.
#' @param family_cols Character vector of famille_* column names present in df.
#' @param language Character. "fr" or "en".
#' @return data.frame with columns: code, label (ordered factor), value.
#' @noRd
build_radar_data <- function(df, family_cols, language = "fr") {
  canonical_codes <- names(INDICATOR_FAMILIES)
  rows <- list()
  for (code in canonical_codes) {
    col <- get_famille_col(code)
    if (col %in% family_cols && col %in% names(df)) {
      fam <- INDICATOR_FAMILIES[[code]]
      label <- if (language == "fr") fam$name_fr else fam$name_en
      val <- mean(df[[col]], na.rm = TRUE)
      if (is.nan(val)) val <- 0
      rows[[length(rows) + 1]] <- data.frame(
        code = code, label = label, value = val,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  out$label <- factor(out$label, levels = out$label)
  out
}

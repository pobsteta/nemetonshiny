# Import internal functions from nemeton core package
# These functions are not exported by nemeton but are needed by the Shiny app

# Family system
get_famille_col <- nemeton:::get_famille_col
get_famille_code <- nemeton:::get_famille_code
FAMILLE_NMT_MAP <- nemeton:::FAMILLE_NMT_MAP

# i18n (messages CLI)
msg_info <- nemeton:::msg_info
msg_warn <- nemeton:::msg_warn
msg_error <- nemeton:::msg_error
msg_success <- nemeton:::msg_success
msg <- nemeton:::msg
get_language <- nemeton:::get_language

# Normalization
normalize_indicator <- nemeton:::normalize_indicator

# Utilities
resolve_raster_layer <- nemeton:::resolve_raster_layer
resolve_vector_layer <- nemeton:::resolve_vector_layer
safe_extract <- nemeton:::safe_extract
as_pure_sf <- nemeton:::as_pure_sf
enrich_parcels_bdforet <- nemeton:::enrich_parcels_bdforet
map_essence_to_species <- nemeton:::map_essence_to_species
get_allometric_coefficients <- nemeton:::get_allometric_coefficients
clean_indicator_name <- nemeton:::clean_indicator_name
get_dem_raster <- nemeton:::get_dem_raster

# NDP system (Niveau de Donnees Probant)
set_ndp_attributes <- nemeton:::set_ndp_attributes
detect_ndp_from_cache <- nemeton:::detect_ndp_from_cache
restore_ndp_attributes <- nemeton:::restore_ndp_attributes
# ndp_badge and ndp_progress_bar exist in nemeton:::
# but we keep local versions here for Shiny-specific styling


#' Generate an HTML badge for the NDP level
#'
#' @param ndp_level Integer NDP level (0-4).
#' @param lang Character. Language code ("fr" or "en").
#' @return An htmltools tag (span with Bootstrap badge styling).
#' @noRd
ndp_badge <- function(ndp_level, lang = "fr") {
  ndp_level <- as.integer(ndp_level %||% 0L)
  ndp_info <- get_ndp_level(ndp_level)

  # Color scale from red (0) to green (4)
  colors <- c(
    `0` = "#dc3545",
    `1` = "#fd7e14",
    `2` = "#ffc107",
    `3` = "#20c997",
    `4` = "#198754"
  )
  bg_color <- colors[[as.character(ndp_level)]] %||% colors[["0"]]

  # Use white text for dark backgrounds, dark for light
  text_color <- if (ndp_level %in% c(0L, 4L)) "#ffffff" else "#212529"

  label <- sprintf("NDP %d \u2013 %s", ndp_level, ndp_info$name)

  htmltools::tags$span(
    class = "badge",
    style = sprintf(
      "background-color: %s; color: %s; font-size: 0.8rem; padding: 4px 10px; border-radius: 12px;",
      bg_color, text_color
    ),
    label
  )
}


#' Generate an HTML progress bar for the NDP confidence level
#'
#' @param ndp_level Integer NDP level (0-4).
#' @param lang Character. Language code ("fr" or "en").
#' @return An htmltools tag (div with Bootstrap progress bar).
#' @noRd
ndp_progress_bar <- function(ndp_level, lang = "fr") {
  ndp_level <- as.integer(ndp_level %||% 0L)
  ndp_info <- get_ndp_level(ndp_level)
  pct <- round(ndp_info$confidence * 100)

  colors <- c(
    `0` = "#dc3545",
    `1` = "#fd7e14",
    `2` = "#ffc107",
    `3` = "#20c997",
    `4` = "#198754"
  )
  bar_color <- colors[[as.character(ndp_level)]] %||% colors[["0"]]

  label <- if (lang == "fr") {
    sprintf("Confiance : %d%%", pct)
  } else {
    sprintf("Confidence: %d%%", pct)
  }

  htmltools::div(
    class = "mt-2",
    style = "max-width: 300px; margin: 0 auto;",
    htmltools::tags$small(class = "text-muted", label),
    htmltools::div(
      class = "progress",
      style = "height: 8px; border-radius: 4px;",
      htmltools::div(
        class = "progress-bar",
        role = "progressbar",
        style = sprintf(
          "width: %d%%; background-color: %s; border-radius: 4px;",
          pct, bar_color
        ),
        `aria-valuenow` = pct,
        `aria-valuemin` = "0",
        `aria-valuemax` = "100"
      )
    )
  )
}

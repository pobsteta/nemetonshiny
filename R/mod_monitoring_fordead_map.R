# mod_monitoring_fordead_map.R — Sub-tab "Carte FORDEAD" of Suivi sanitaire.
#
# Wires `nemeton::read_fordead_dieback_mask()` (shipped in nemeton@v0.25.0)
# into the monitoring navset. Renders the categorical 0..4 dieback mask :
#
#   0 = sain     → green   (#2CA02C)
#   1 = faible   → pale-orange (#FFD27F)
#   2 = moyenne  → orange  (#FF9933)
#   3 = forte    → red     (#D62728)
#   4 = sol nu   → near-black (#222222)
#   NA = hors mask forestier → transparent
#
# Raster pinned to the custom `nemetonRaster` Leaflet pane (z-index 250),
# same pattern as Carte FAST so the basemap toggle (OSM / Satellite) keeps
# the mask visible without re-stacking the overlay on every interaction.
#
# Caveat (nemeton@v0.25.0) : `run_fordead_dieback()` does NOT yet persist
# the classified mask to disk — the persist hook ships in a later cœur
# release. Until then this reader returns NULL and the panel shows an
# empty state. The wiring is in place so the panel activates automatically
# the day the cœur writer ships.

#' Carte FORDEAD sub-tab UI
#'
#' @param id Module namespace id.
#' @return A `shiny.tag`.
#' @noRd
mod_monitoring_fordead_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("panel"))
}

#' Carte FORDEAD sub-tab server
#'
#' @param id Module namespace id.
#' @param app_state Parent `reactiveValues` carrying `language`,
#'   `current_project`.
#' @param zone_id_r Reactive returning the active monitoring zone id.
#' @return invisible list with `mask` reactive.
#' @noRd
mod_monitoring_fordead_map_server <- function(id, app_state, zone_id_r) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # ----- Core call ------------------------------------------------------
    # cache_dir is resolved from the active project, same convention as the
    # FORDEAD worker writes its outputs there. NULL until the cœur writer
    # ships — caller-side handled by the empty-state branch of output$panel.
    mask_r <- shiny::reactive({
      zone <- zone_id_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return(NULL)
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return(NULL)
      cd <- file.path(proj$path, "cache", "layers", "fordead")
      if (!dir.exists(cd)) return(NULL)
      con <- get_monitoring_db_connection(project = proj)
      if (is.null(con)) return(NULL)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      tryCatch(
        nemeton::read_fordead_dieback_mask(
          con,
          zone_id   = as.integer(zone),
          run_id    = NULL,  # latest
          cache_dir = cd
        ),
        error = function(e) {
          cli::cli_alert_warning(
            "read_fordead_dieback_mask failed: {e$message}"
          )
          NULL
        }
      )
    })

    # ----- Panel : either map or empty state -----------------------------
    output$panel <- shiny::renderUI({
      i18n <- i18n_r()
      r <- mask_r()
      if (is.null(r)) {
        return(htmltools::div(
          class = "p-4 text-center text-muted",
          bsicons::bs_icon("hourglass-split",
                           class = "fs-1 d-block mx-auto mb-3"),
          htmltools::h5(class = "mt-3",
                        i18n$t("monitoring_fordead_map_empty_title")),
          htmltools::p(class = "mb-0",
                       i18n$t("monitoring_fordead_map_empty_body"))
        ))
      }
      leaflet::leafletOutput(session$ns("map"), height = "55vh")
    })

    output$map <- leaflet::renderLeaflet({
      r <- mask_r()
      if (is.null(r)) return(NULL)
      i18n <- i18n_r()

      cats   <- c("0", "1", "2", "3", "4")
      colors <- c("#2CA02C", "#FFD27F", "#FF9933", "#D62728", "#222222")
      pal <- leaflet::colorFactor(colors, levels = cats,
                                  na.color = "transparent")

      legend_labels <- c(
        sprintf("0 - %s", i18n$t("monitoring_fordead_class_0")),
        sprintf("1 - %s", i18n$t("monitoring_fordead_class_1")),
        sprintf("2 - %s", i18n$t("monitoring_fordead_class_2")),
        sprintf("3 - %s", i18n$t("monitoring_fordead_class_3")),
        sprintf("4 - %s", i18n$t("monitoring_fordead_class_4"))
      )

      leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",   group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          options    = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::addRasterImage(
          x       = r,
          colors  = pal,
          opacity = 1.0,
          options = leaflet::gridOptions(pane = "nemetonRaster")
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors   = colors,
          labels   = legend_labels,
          title    = i18n$t("monitoring_fordead_class_title"),
          opacity  = 0.85
        )
    })

    # v0.37.1 — force the panel renderUI to evaluate even while the
    # sub-tab is hidden. The Suivi sanitaire navset toggles its
    # nav_panels with bslib::nav_show() / nav_hide() (mode-driven
    # visibility). That mechanism leaves Shiny's per-output
    # visibility detection unreliable : the `uiOutput(ns("panel"))`
    # stayed suspended (suspendWhenHidden defaults to TRUE) and the
    # empty-state never rendered even after the user clicked the
    # Carte FORDEAD tab — the panel showed up blank. Disabling the
    # suspend makes the empty-state / leaflet wrapper render
    # unconditionally.
    shiny::outputOptions(output, "panel", suspendWhenHidden = FALSE)

    invisible(list(mask = mask_r))
  })
}

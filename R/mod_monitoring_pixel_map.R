# mod_monitoring_pixel_map.R — Sub-tab "Carte pixel" of Suivi sanitaire
#
# Renders the cached Sentinel-2 NDVI / NBR rasters at native 10 m
# resolution as a leaflet layer, with a date slider, an index toggle,
# and a per-pixel time series plotly that opens on map click.
#
# Spec 010 (carte pixel + time series interactive). Consumes 4 read-
# only public functions from `nemeton@>=v0.22.0`:
#   * read_s2_band_stack()
#   * build_index_stack()
#   * extract_pixel_timeseries()
#   * read_obs_pixel()  (re-used for the placettes overlay only)
#
# All four go through `cache_dir = <project>/cache/layers/sentinel2`,
# zero HTTP and zero DB write — the map is fully offline once the
# ingestion has populated the cache.

#' Pixel map sub-tab UI
#'
#' @param id Module namespace.
#' @return A `bslib::card` tag list to drop into a `bslib::nav_panel`.
#' @noRd
mod_monitoring_pixel_map_ui <- function(id) {
  ns <- shiny::NS(id)
  i18n <- get_i18n(get_app_options()$language %||% "fr")

  bslib::card(
    bslib::card_header(
      htmltools::div(
        class = "d-flex align-items-center",
        bsicons::bs_icon("map", class = "me-2"),
        i18n$t("monitoring_pixel_map_title")
      )
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 250L, position = "right", open = "always",
        shiny::radioButtons(
          ns("index"),
          i18n$t("monitoring_pixel_map_index"),
          choices  = c(NDVI = "NDVI", NBR = "NBR"),
          selected = "NDVI",
          inline   = TRUE
        ),
        shiny::uiOutput(ns("date_slider_ui")),
        shiny::helpText(
          class = "small text-muted fst-italic",
          i18n$t("monitoring_pixel_map_click_hint")
        ),
        shiny::uiOutput(ns("scene_count_hint"))
      ),
      htmltools::div(
        style = "position: relative;",
        leaflet::leafletOutput(ns("map"), height = "55vh"),
        shiny::uiOutput(ns("loading_overlay"))
      )
    )
  )
}

#' Pixel map sub-tab server
#'
#' @param id Module namespace.
#' @param app_state Shared `reactiveValues` (carries `language`,
#'   `current_project`).
#' @param obs_pixel_data Reactive returning the per-plot obs from
#'   `nemeton::read_obs_pixel()` (already wired in the parent
#'   `mod_monitoring_server`). Used to derive `scenes_df`
#'   (DISTINCT scene_id × obs_date) without a second SQL query.
#' @param mode_input Reactive returning the current mode
#'   (`"quick"` / `"health"`). The pixel map only makes sense in
#'   quick mode (FORDEAD doesn't expose per-pixel raster output).
#'
#' @noRd
mod_monitoring_pixel_map_server <- function(id, app_state,
                                            obs_pixel_data,
                                            mode_input) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # Loading flag — toggles the spinning gear overlay on the map.
    loading <- shiny::reactiveVal(FALSE)
    output$loading_overlay <- shiny::renderUI({
      if (!isTRUE(loading())) return(NULL)
      i18n <- i18n_r()
      htmltools::div(
        class = "position-absolute top-50 start-50 translate-middle",
        style = paste("z-index: 1000; padding: 12px 18px;",
                      "background: rgba(255,255,255,0.85);",
                      "border-radius: 6px;",
                      "box-shadow: 0 2px 6px rgba(0,0,0,0.15);"),
        shiny::icon("gear", class = "fa-spin me-2"),
        i18n$t("monitoring_pixel_map_loading")
      )
    })

    # Resolve <project>/cache/layers/sentinel2 from the active
    # project. NULL when no project is loaded.
    cache_dir_r <- shiny::reactive({
      proj <- app_state$current_project
      if (is.null(proj)) return(NULL)
      ppath <- proj$path %||% proj$project_path
      if (is.null(ppath) || !nzchar(ppath)) return(NULL)
      cd <- file.path(ppath, "cache", "layers", "sentinel2")
      if (!dir.exists(cd)) return(NULL)
      cd
    })

    # scenes_df = DISTINCT (scene_id, obs_date) from the per-plot
    # obs already fetched by the parent module's obs_pixel_data
    # reactive. Avoids a second roundtrip to the DB.
    scenes_df_r <- shiny::reactive({
      df <- obs_pixel_data()
      if (is.null(df) || !nrow(df)) return(NULL)
      out <- unique(df[, c("scene_id", "obs_date"), drop = FALSE])
      out <- out[order(out$obs_date), , drop = FALSE]
      rownames(out) <- NULL
      out
    })

    # Build the multi-temporal index stack. Heavy compute (reads N
    # GeoTIFFs from disk + arithmetic), debounced on (cache_dir,
    # scenes_df, index) — re-runs only when one of those changes.
    pixel_stack_r <- shiny::reactive({
      shiny::req(identical(mode_input(), "quick"))
      cd <- cache_dir_r()
      sdf <- scenes_df_r()
      if (is.null(cd) || is.null(sdf) || !nrow(sdf)) return(NULL)
      loading(TRUE)
      on.exit(loading(FALSE), add = TRUE)
      tryCatch(
        nemeton::build_index_stack(cd, sdf, index = input$index),
        error = function(e) {
          cli::cli_alert_warning(sprintf(
            "build_index_stack failed: %s", conditionMessage(e)))
          NULL
        }
      )
    })

    # Date slider built dynamically from the layer names of the
    # stack (one layer per obs_date).
    output$date_slider_ui <- shiny::renderUI({
      ns <- session$ns
      i18n <- i18n_r()
      st <- pixel_stack_r()
      if (is.null(st)) {
        return(shiny::helpText(
          class = "small text-muted",
          i18n$t("monitoring_pixel_map_no_cache")
        ))
      }
      dates <- as.Date(names(st))
      dates <- dates[!is.na(dates)]
      if (!length(dates)) return(NULL)
      shiny::sliderInput(
        ns("date"),
        i18n$t("monitoring_pixel_map_date"),
        min   = min(dates),
        max   = max(dates),
        value = max(dates),
        timeFormat = "%Y-%m-%d",
        step  = 1L,
        animate = shiny::animationOptions(interval = 800L,
                                          loop     = FALSE)
      )
    })

    # Snap the requested date to the closest scene actually present
    # in the stack (the slider lets the user drag day-by-day, but
    # the data is sparse — typically every 5 days max).
    current_layer_r <- shiny::reactive({
      st <- pixel_stack_r()
      if (is.null(st)) return(NULL)
      target <- input$date
      if (is.null(target)) return(NULL)
      dates <- as.Date(names(st))
      idx <- which.min(abs(as.numeric(dates - as.Date(target))))
      if (!length(idx)) return(NULL)
      st[[idx]]
    })

    output$scene_count_hint <- shiny::renderUI({
      i18n <- i18n_r()
      sdf <- scenes_df_r()
      if (is.null(sdf)) return(NULL)
      htmltools::tags$p(
        class = "small text-muted mb-0",
        sprintf(i18n$t("monitoring_pixel_map_scene_count_fmt"),
                nrow(sdf))
      )
    })

    # Stable, language-independent overlay group labels. Used by BOTH
    # addLayersControl(overlayGroups=) and the leafletProxy() add* calls
    # — they MUST match exactly so the control knows these are overlays
    # to keep visible across base-layer toggles. Kept out of i18n so
    # renderLeaflet stays free of reactive deps (preserves base-layer
    # choice across language toggle).
    .pixel_overlay_group     <- "NDVI / NBR"
    .placettes_overlay_group <- "Placettes"

    # Static map skeleton: rendered ONCE. Re-rendering the whole map on
    # every date tick would reset the user's base-layer choice
    # (Satellite → OSM) because `baseGroups` re-applies the first entry
    # as default on each remount. Raster + markers + legend updates go
    # through leafletProxy() below.
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",   group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(
          baseGroups    = c("OSM", "Satellite"),
          overlayGroups = c(.pixel_overlay_group, .placettes_overlay_group),
          options       = leaflet::layersControlOptions(collapsed = TRUE)
        )
    })

    # Index value range is roughly [-1, 1] for both NDVI and NBR.
    # Use a divergent palette anchored on 0; NA stays transparent.
    .pixel_palette <- leaflet::colorNumeric(
      palette  = c("#D62728", "#FFD27F", "#FFFFCC",
                   "#A8DDB5", "#2CA02C"),
      domain   = c(-1, 1),
      na.color = "transparent"
    )

    # Reactive raster + legend swap. Fires on date / index / project
    # changes; preserves the base-layer selection because the map
    # widget itself is not re-rendered.
    shiny::observe({
      r <- current_layer_r()
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearImages() |>
        leaflet::removeControl("pixel_legend")
      if (is.null(r)) return()
      proxy |>
        leaflet::addRasterImage(
          x       = r,
          colors  = .pixel_palette,
          opacity = 0.75,
          group   = .pixel_overlay_group
        ) |>
        leaflet::addLegend(
          layerId  = "pixel_legend",
          position = "bottomright",
          pal      = .pixel_palette,
          values   = c(-1, 1),
          title    = input$index,
          opacity  = 0.85
        )
    })

    # Placettes overlay: load_samples() returns sf POINT (EPSG:2154
    # by convention). Filter to plot_ids that actually have observations
    # in the current window so the markers reflect the same scope as the
    # rest of the sub-tab. NULL when no plan, no project, no obs.
    placettes_sf_r <- shiny::reactive({
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$id)) return(NULL)
      df <- obs_pixel_data()
      if (is.null(df) || !nrow(df)) return(NULL)
      plots <- tryCatch(load_samples(proj$id, layer = "plots"),
                        error = function(e) NULL)
      if (is.null(plots) || !inherits(plots, "sf") || !nrow(plots)) {
        return(NULL)
      }
      if (!"plot_id" %in% names(plots)) return(NULL)
      plots <- plots[as.character(plots$plot_id) %in%
                     unique(as.character(df$plot_id)), , drop = FALSE]
      if (!nrow(plots)) return(NULL)
      # Leaflet expects WGS84.
      if ((sf::st_crs(plots)$epsg %||% 4326L) != 4326L) {
        plots <- sf::st_transform(plots, 4326)
      }
      plots
    })

    # Markers swap. Same proxy pattern as the raster observe — clears
    # the overlay group then re-adds. layerId = plot_id so the click
    # handler downstream can identify which placette was tapped.
    shiny::observe({
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup(.placettes_overlay_group)
      ps <- placettes_sf_r()
      if (is.null(ps) || !nrow(ps)) return()
      proxy |>
        leaflet::addCircleMarkers(
          data        = ps,
          layerId     = ~as.character(plot_id),
          group       = .placettes_overlay_group,
          radius      = 5,
          weight      = 2,
          color       = "#000000",
          fillColor   = "#1F77B4",
          fillOpacity = 0.9,
          label       = ~as.character(plot_id)
        )
    })

    # Marker click handler: open a modal with the per-placette NDVI/NBR
    # time series. Reuses obs_pixel_data() — the values are already the
    # placette mean computed core-side, no re-aggregation needed.
    shiny::observeEvent(input$map_marker_click, {
      i18n <- i18n_r()
      ev <- input$map_marker_click
      pid <- as.character(ev$id %||% "")
      if (!nzchar(pid)) return()
      df <- obs_pixel_data()
      if (is.null(df) || !nrow(df)) return()
      sub <- df[as.character(df$plot_id) == pid, , drop = FALSE]
      # Restrict to NDVI / NBR even if the user picked more bands —
      # consistent with the pixel-click modal.
      sub <- sub[as.character(sub$band) %in% c("NDVI", "NBR"), ,
                 drop = FALSE]
      if (!nrow(sub)) {
        shiny::showNotification(
          i18n$t("monitoring_pixel_map_no_placette_data"),
          type = "warning", duration = 4
        )
        return()
      }
      band_colors <- c(NDVI = "#2CA02C", NBR = "#D62728")
      p <- plotly::plot_ly(type = "scatter", mode = "lines+markers")
      for (b in unique(sub$band)) {
        bsub <- sub[sub$band == b, , drop = FALSE]
        bsub <- bsub[order(bsub$obs_date), , drop = FALSE]
        col  <- band_colors[b] %||% "#7F7F7F"
        p <- plotly::add_trace(
          p,
          x      = as.Date(bsub$obs_date),
          y      = as.numeric(bsub$value),
          name   = b,
          mode   = "lines+markers",
          line   = list(color = unname(col), width = 1.5),
          marker = list(color = unname(col), size = 5),
          hovertemplate = paste0(
            "<b>", b, "</b><br>",
            "%{x|%Y-%m-%d}<br>",
            b, " = %{y:.3f}<extra></extra>"
          )
        )
      }
      p <- plotly::layout(
        p,
        margin = list(t = 20, b = 40, l = 50, r = 10),
        xaxis  = list(title = i18n$t("monitoring_timeseries_xaxis"),
                      type = "date"),
        yaxis  = list(title = i18n$t("monitoring_timeseries_yaxis"),
                      range = c(-0.2, 1)),
        legend = list(orientation = "h", y = -0.25)
      )
      shiny::showModal(shiny::modalDialog(
        title = sprintf(
          i18n$t("monitoring_pixel_map_placette_modal_title_fmt"),
          pid
        ),
        size      = "l",
        easyClose = TRUE,
        plotly::plotlyOutput(session$ns("placette_ts_plot"),
                             height = "320px"),
        footer = shiny::modalButton(i18n$t("close"))
      ))
      output$placette_ts_plot <- plotly::renderPlotly(p)
    })

    # Click handler: extract the per-pixel time series at the clicked
    # coordinate and pop a modal with NDVI + NBR superimposed.
    shiny::observeEvent(input$map_click, {
      i18n <- i18n_r()
      cd <- cache_dir_r()
      sdf <- scenes_df_r()
      if (is.null(cd) || is.null(sdf) || !nrow(sdf)) return()
      lat <- input$map_click$lat
      lng <- input$map_click$lng
      if (is.null(lat) || is.null(lng)) return()

      ts <- tryCatch(
        nemeton::extract_pixel_timeseries(
          cache_dir = cd, scenes_df = sdf,
          xy        = c(lng, lat),
          crs       = 4326,
          indices   = c("NDVI", "NBR")
        ),
        error = function(e) {
          cli::cli_alert_warning(sprintf(
            "extract_pixel_timeseries failed: %s",
            conditionMessage(e)))
          NULL
        }
      )
      if (is.null(ts) || !nrow(ts)) {
        shiny::showNotification(
          i18n$t("monitoring_pixel_map_no_pixel"),
          type = "warning", duration = 4
        )
        return()
      }

      # Stable per-band colors (same palette as the per-plot view
      # in v0.27.0 — keeps the user's mental model consistent).
      band_colors <- c(NDVI = "#2CA02C", NBR = "#D62728")
      p <- plotly::plot_ly(type = "scatter", mode = "lines+markers")
      for (b in unique(ts$index)) {
        sub <- ts[ts$index == b, , drop = FALSE]
        col <- band_colors[b] %||% "#7F7F7F"
        p <- plotly::add_trace(
          p,
          x      = as.Date(sub$obs_date),
          y      = as.numeric(sub$value),
          name   = b,
          mode   = "lines+markers",
          line   = list(color = unname(col), width = 1.5),
          marker = list(color = unname(col), size = 5),
          hovertemplate = paste0(
            "<b>", b, "</b><br>",
            "%{x|%Y-%m-%d}<br>",
            b, " = %{y:.3f}<extra></extra>"
          )
        )
      }
      p <- plotly::layout(
        p,
        margin = list(t = 20, b = 40, l = 50, r = 10),
        xaxis  = list(title = i18n$t("monitoring_timeseries_xaxis"),
                      type = "date"),
        yaxis  = list(title = i18n$t("monitoring_timeseries_yaxis"),
                      range = c(-0.2, 1)),
        legend = list(orientation = "h", y = -0.25)
      )

      shiny::showModal(shiny::modalDialog(
        title = sprintf(
          i18n$t("monitoring_pixel_map_modal_title_fmt"),
          round(lat, 5), round(lng, 5)
        ),
        size  = "l",
        easyClose = TRUE,
        plotly::plotlyOutput(session$ns("pixel_ts_plot"),
                             height = "320px"),
        footer = shiny::modalButton(i18n$t("close"))
      ))
      output$pixel_ts_plot <- plotly::renderPlotly(p)
    })

    invisible(list(
      pixel_stack    = pixel_stack_r,
      scenes_df      = scenes_df_r,
      cache_dir      = cache_dir_r
    ))
  })
}

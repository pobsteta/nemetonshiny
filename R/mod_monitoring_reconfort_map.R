# mod_monitoring_reconfort_map.R — Sous-onglet "Carte RECONFORT" du Suivi
# sanitaire (spec 021, L6). Miroir de mod_monitoring_fordead_map.R, adapté
# au pipeline RECONFORT (dépérissement feuillus, méthode RECONFORT/DSF) :
#
#   * Couche Leaflet = ALERTES vectorielles (pas un raster) lues via
#     `nemeton::list_alerts(con, zone_id, classes = RECONFORT_ALERT_CLASSES)`
#     — filtre G1 sur les classes 2-deperissant / 3-tres-deperissant ;
#     popup = confidence_class + stress_index (score continu).
#   * Bannière de validité G3 (advisory, non bloquante) en haut :
#     `nemeton::check_reconfort_validity(zone_aoi)` — avertit quand la zone
#     sort du domaine géographique de calibration, SANS bloquer.
#   * Clic carte → diagnostic pixel : `read_reconfort_pixel_series()` →
#     modal plotly à 2 traces (CRSWIR observé + CRre observé). Contrairement
#     à FORDEAD, RECONFORT n'a PAS de modèle harmonique : pas de courbe de
#     prédiction ni de seuil.
#
# cache_dir RECONFORT : <project>/cache/layers/reconfort (la phase persist
# du run y écrit zone_<id>/run_<run_id>/ ; même répertoire passé à
# read_reconfort_pixel_series()).

#' Couleurs des classes d'alerte RECONFORT (parité avec la légende FORDEAD).
#' @noRd
.RECONFORT_CLASS_COLORS <- c(
  "2-deperissant"      = "#FF9933",  # orange  (dépérissant)
  "3-tres-deperissant" = "#D62728"   # rouge   (très dépérissant)
)

#' Carte RECONFORT sub-tab UI
#'
#' @param id Module namespace id.
#' @return A `shiny.tag`.
#' @noRd
mod_monitoring_reconfort_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("panel"))
}

#' Carte RECONFORT sub-tab server
#'
#' @param id Module namespace id.
#' @param app_state Parent `reactiveValues` carrying `language`,
#'   `current_project`.
#' @param zone_id_r Reactive returning the active monitoring zone id.
#' @param refresh_r Reactive bumped whenever a RECONFORT run completes (the
#'   parent's refresh counter). Read by `alerts_r` so the sub-tab re-reads
#'   the freshly-persisted alerts without a project reload. Optional —
#'   defaults to a constant reactive for back-compat / tests.
#' @return invisible list with `alerts` reactive.
#' @noRd
mod_monitoring_reconfort_map_server <- function(id, app_state, zone_id_r,
                                                refresh_r = shiny::reactive(0L)) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # ----- cache_dir resolver -------------------------------------------
    .reconfort_cache_dir <- function(proj) {
      if (is.null(proj) || is.null(proj$path)) return(NA_character_)
      file.path(proj$path, "cache", "layers", "reconfort")
    }

    # ----- Alerts layer (sf) --------------------------------------------
    # `nemeton::list_alerts()` returns an sf data.frame (CRS 4326) of the
    # plots carrying a RECONFORT alert, restricted to the G1 classes. The
    # reactive reads refresh_r() so a completed run re-invalidates it.
    alerts_r <- shiny::reactive({
      refresh_r()
      zone <- zone_id_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return(NULL)
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return(NULL)
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
      if (is.null(con)) return(NULL)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      tryCatch(
        nemeton::list_alerts(
          con,
          zone_id = as.integer(zone),
          classes = nemeton::RECONFORT_ALERT_CLASSES
        ),
        error = function(e) {
          cli::cli_alert_warning("list_alerts (reconfort) failed: {e$message}")
          NULL
        }
      )
    })

    # ----- Validity (G3, advisory) --------------------------------------
    # `check_reconfort_validity()` is advisory (`advisory = TRUE`) : it
    # warns when the zone sits outside the calibration domain but never
    # blocks. Returns NULL when the AOI cannot be resolved.
    validity_r <- shiny::reactive({
      zone <- zone_id_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return(NULL)
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return(NULL)
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
      if (is.null(con)) return(NULL)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      aoi <- tryCatch(get_monitoring_zone_aoi(con, as.integer(zone)),
                      error = function(e) NULL)
      if (is.null(aoi)) return(NULL)
      tryCatch(
        nemeton::check_reconfort_validity(aoi),
        error = function(e) {
          cli::cli_alert_warning("check_reconfort_validity failed: {e$message}")
          NULL
        }
      )
    })

    # ----- Panel : validity banner + (map | empty state) ----------------
    output$panel <- shiny::renderUI({
      i18n <- i18n_r()

      # G3 advisory banner — shown above the map when the zone is outside
      # the geographic calibration domain. Non-blocking (alert-warning).
      v <- validity_r()
      banner <- NULL
      if (!is.null(v) && isFALSE(v$geo_valid)) {
        banner <- htmltools::div(
          class = "alert alert-warning d-flex align-items-center gap-2 mb-2",
          role = "alert",
          bsicons::bs_icon("exclamation-triangle-fill"),
          htmltools::span(i18n$t("monitoring_reconfort_outside_validity"))
        )
      }

      a <- alerts_r()
      body <- if (is.null(a) || nrow(a) == 0L) {
        htmltools::div(
          class = "p-4 text-center text-muted",
          bsicons::bs_icon("hourglass-split",
                           class = "fs-1 d-block mx-auto mb-3"),
          htmltools::h5(class = "mt-3",
                        i18n$t("monitoring_reconfort_map_empty_title")),
          htmltools::p(class = "mb-0",
                       i18n$t("monitoring_reconfort_map_empty_body"))
        )
      } else {
        leaflet::leafletOutput(session$ns("map"), height = "55vh")
      }

      htmltools::tagList(banner, body)
    })

    output$map <- leaflet::renderLeaflet({
      a <- alerts_r()
      if (is.null(a) || nrow(a) == 0L) return(NULL)
      i18n <- i18n_r()

      # Alert plots may be polygons or points — use centroids for stable
      # markers + popups (the pixel diagnostic itself runs on the raw
      # map click coordinate, not on a marker).
      pts <- tryCatch(
        suppressWarnings(sf::st_centroid(sf::st_geometry(a))),
        error = function(e) NULL
      )
      if (is.null(pts)) return(NULL)
      coords <- sf::st_coordinates(pts)

      cls    <- as.character(a$confidence_class)
      stress <- suppressWarnings(as.numeric(a$stress_index))
      colors <- unname(.RECONFORT_CLASS_COLORS)
      cats   <- names(.RECONFORT_CLASS_COLORS)
      pal <- leaflet::colorFactor(colors, levels = cats,
                                  na.color = "#888888")

      class_label <- function(code) {
        switch(code,
               "2-deperissant"      = i18n$t("monitoring_reconfort_class_2"),
               "3-tres-deperissant" = i18n$t("monitoring_reconfort_class_3"),
               code)
      }
      popups <- vapply(seq_len(nrow(coords)), function(i) {
        sprintf(
          "<b>%s</b>: %s<br/><b>%s</b>: %s",
          i18n$t("monitoring_reconfort_popup_class"),
          htmltools::htmlEscape(class_label(cls[i])),
          i18n$t("monitoring_reconfort_popup_stress"),
          if (is.na(stress[i])) "—" else format(round(stress[i], 3))
        )
      }, character(1))

      legend_labels <- c(
        sprintf("2 - %s", i18n$t("monitoring_reconfort_class_2")),
        sprintf("3 - %s", i18n$t("monitoring_reconfort_class_3"))
      )

      leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",     group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          options    = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::addCircleMarkers(
          lng = coords[, 1], lat = coords[, 2],
          radius      = 6,
          color       = "#333333",
          weight      = 1,
          fillColor   = pal(cls),
          fillOpacity = 0.85,
          popup       = popups
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors   = colors,
          labels   = legend_labels,
          title    = i18n$t("monitoring_reconfort_class_title"),
          opacity  = 0.85
        )
    })

    # Force the panel to render even while the sub-tab is hidden (the
    # Suivi navset toggles nav_panels via nav_show/nav_hide, which leaves
    # Shiny's per-output visibility detection unreliable). Same rationale
    # as the FORDEAD / FAST map modules.
    shiny::outputOptions(output, "panel", suspendWhenHidden = FALSE)

    # ----- Pixel diagnostic on map click --------------------------------
    # Mirror of the FORDEAD pixel click, adapted to RECONFORT : the series
    # carries CRSWIR + CRre observed only (no harmonic prediction / seuil).
    shiny::observeEvent(input$map_click, {
      i18n <- i18n_r()
      zone <- zone_id_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return()
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return()
      cd <- .reconfort_cache_dir(proj)
      if (is.na(cd) || !dir.exists(cd)) {
        shiny::showNotification(
          i18n$t("monitoring_reconfort_pixel_no_data"),
          type = "warning", duration = 8
        )
        return()
      }
      lat <- input$map_click$lat
      lng <- input$map_click$lng
      if (is.null(lat) || is.null(lng)) return()

      s <- tryCatch(
        nemeton::read_reconfort_pixel_series(
          con       = NULL,   # spec : con réservé, NULL accepté
          zone_id   = as.integer(zone),
          xy        = c(lng, lat),
          crs       = 4326,
          run_id    = NULL,   # dernier run
          cache_dir = cd
        ),
        error = function(e) {
          cli::cli_alert_warning(sprintf(
            "read_reconfort_pixel_series failed: %s", conditionMessage(e)))
          NULL
        }
      )
      if (is.null(s) || !nrow(s)) {
        shiny::showNotification(
          i18n$t("monitoring_reconfort_pixel_no_data"),
          type = "warning", duration = 8
        )
        return()
      }

      species <- attr(s, "species")
      v_model <- attr(s, "v_model")
      subtitle_bits <- c(
        if (!is.null(species) && length(species) == 1L && !is.na(species))
          sprintf("%s : %s", i18n$t("monitoring_reconfort_species"), species),
        if (!is.null(v_model) && length(v_model) == 1L && !is.na(v_model))
          sprintf("%s : %s", i18n$t("monitoring_reconfort_model"), v_model)
      )
      subtitle <- if (length(subtitle_bits))
        paste(subtitle_bits, collapse = " — ") else NULL

      col_crswir <- "#2C7FB8"  # bleu
      col_crre   <- "#2CA02C"  # vert

      p <- plotly::plot_ly(type = "scatter")
      p <- plotly::add_trace(
        p,
        x      = as.Date(s$obs_date),
        y      = as.numeric(s$crswir_obs),
        name   = i18n$t("monitoring_reconfort_crswir"),
        mode   = "lines+markers",
        line   = list(color = col_crswir, width = 1.5),
        marker = list(color = col_crswir, size = 4),
        hovertemplate = paste0(
          "<b>", i18n$t("monitoring_reconfort_crswir"), "</b><br>",
          "%{x|%Y-%m-%d}<br>%{y:.3f}<extra></extra>"
        )
      )
      p <- plotly::add_trace(
        p,
        x      = as.Date(s$obs_date),
        y      = as.numeric(s$crre_obs),
        name   = i18n$t("monitoring_reconfort_crre"),
        mode   = "lines+markers",
        line   = list(color = col_crre, width = 1.5),
        marker = list(color = col_crre, size = 4),
        hovertemplate = paste0(
          "<b>", i18n$t("monitoring_reconfort_crre"), "</b><br>",
          "%{x|%Y-%m-%d}<br>%{y:.3f}<extra></extra>"
        )
      )
      p <- plotly::layout(
        p,
        margin = list(t = 30, b = 40, l = 50, r = 10),
        title  = if (!is.null(subtitle))
          list(text = subtitle, font = list(size = 12), x = 0) else NULL,
        xaxis  = list(title = i18n$t("monitoring_timeseries_xaxis"),
                      type = "date"),
        yaxis  = list(title = i18n$t("monitoring_reconfort_pixel_yaxis")),
        legend = list(orientation = "h", y = -0.25)
      )

      shiny::showModal(shiny::modalDialog(
        title = sprintf(
          i18n$t("monitoring_reconfort_pixel_modal_title_fmt"),
          round(lat, 5), round(lng, 5)
        ),
        size  = "l",
        easyClose = TRUE,
        plotly::plotlyOutput(session$ns("pixel_ts_plot"), height = "320px"),
        footer = shiny::modalButton(i18n$t("close"))
      ))
      output$pixel_ts_plot <- plotly::renderPlotly(p)
    })

    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)

    invisible(list(alerts = alerts_r))
  })
}

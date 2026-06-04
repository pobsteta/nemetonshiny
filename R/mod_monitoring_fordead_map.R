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
#' @param refresh_r Reactive bumped whenever a FORDEAD run completes
#'   (the parent's `alerts_refresh` counter). `mask_r` reads it so the
#'   sub-tab re-reads the freshly-persisted mask without the user
#'   having to reload the project or re-pick the zone. Optional —
#'   defaults to a constant reactive for back-compat / tests.
#' @return invisible list with `mask` reactive.
#' @noRd
mod_monitoring_fordead_map_server <- function(id, app_state, zone_id_r,
                                              refresh_r = shiny::reactive(0L)) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # ----- Core call ------------------------------------------------------
    # cache_dir is resolved from the active project — nemeton@v0.41.0
    # persists the categorical 0-4 mask to
    # <project>/cache/layers/fordead/zone_<id>/dieback_mask_<ts>.tif
    # after the postprocess phase.
    #
    # v0.38.6 — `refresh_r()` is read here so a completed FORDEAD run
    # invalidates mask_r. Without it the reactive only depended on
    # zone_id + current_project: it evaluated once (before the run,
    # when cache/layers/fordead/ did not exist yet → NULL) and stayed
    # frozen, so the sub-tab kept showing the empty-state even after
    # the mask had been written to disk.
    mask_r <- shiny::reactive({
      refresh_r()  # re-read after each FORDEAD run completion
      zone <- zone_id_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return(NULL)
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return(NULL)
      cd <- file.path(proj$path, "cache", "layers", "fordead")
      if (!dir.exists(cd)) return(NULL)
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
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

      # v0.38.7 — `levels` numériques 0:4, alignés sur les valeurs
      # entières du raster catégoriel (le masque FORDEAD stocke
      # 0=sain, 1=faible, 2=moyenne, 3=forte, 4=sol-nu).
      cats   <- 0:4
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
          # v0.38.7 — nearest-neighbour resampling. addRasterImage()
          # reprojects to web-mercator and defaults to method =
          # "bilinear", which interpolates BETWEEN the discrete
          # classes of a categorical raster (0.7, 2.3…). Those
          # fractional values match no colorFactor level → leaflet
          # logs "Some values were outside the color scale and will
          # be treated as NA" once per raster block. "ngb" keeps the
          # integer classes intact.
          method  = "ngb",
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

    # v0.59.0 (TODO #3) — diagnostic pixel CRSWIR au clic sur la
    # carte FORDEAD. Parité fonctionnelle avec la « Carte pixel
    # FAST » (mod_monitoring_pixel_map.R::observeEvent(input$map_click)).
    # Le clic extrait via `nemeton::read_fordead_pixel_series()` la
    # série CRSWIR observée (points) et la prédiction harmonique
    # (ligne) au pixel cliqué, puis affiche un plotly dans un modal
    # avec un marqueur vertical sur la date de 1re anomalie
    # (`attr(., "premiere_detection")`) si présente.
    shiny::observeEvent(input$map_click, {
      i18n <- i18n_r()
      zone <- zone_id_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return()
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return()
      cd <- file.path(proj$path, "cache", "layers", "fordead")
      if (!dir.exists(cd)) return()
      lat <- input$map_click$lat
      lng <- input$map_click$lng
      if (is.null(lat) || is.null(lng)) return()

      ts <- tryCatch(
        nemeton::read_fordead_pixel_series(
          con       = NULL,  # spec : con reserved, NULL accepté
          zone_id   = as.integer(zone),
          xy        = c(lng, lat),
          crs       = 4326,
          run_id    = NULL,  # dernier run
          cache_dir = cd
        ),
        error = function(e) {
          cli::cli_alert_warning(sprintf(
            "read_fordead_pixel_series failed: %s",
            conditionMessage(e)))
          NULL
        }
      )
      if (is.null(ts) || !nrow(ts)) {
        # v0.72.0 — duration ↑ 4 → 8 s + wording plus explicite.
        # Cas typique : clic hors zone modélisée (extent du bundle
        # FORDEAD plus petit que l'AOI rendue sur la carte).
        shiny::showNotification(
          i18n$t("monitoring_fordead_pixel_no_data"),
          type = "warning", duration = 8
        )
        return()
      }

      # v0.72.0 — Enrichissement du modal CRSWIR (consume les 5
      # colonnes de `nemeton::read_fordead_pixel_series()` au lieu
      # de juste obs/pred) :
      #   * Bande `seuil_haut` (`crswir_pred + threshold_anomaly`)
      #     → ligne pointillée orange : enveloppe de détection.
      #   * Points en anomalie (`anomalie == TRUE`) surlignés rouge
      #     vif marker size 8 par-dessus la trace observée.
      #   * `attr(ts, "premiere_detection")` → marqueur vertical
      #     (existant depuis v0.59.0).
      #   * `attr(ts, "dans_zone_validite")` → annotation discrète
      #     en haut à gauche si FALSE (pixel hors zone de validité
      #     pour l'essence dominante).
      premiere      <- attr(ts, "premiere_detection")
      in_validity   <- attr(ts, "dans_zone_validite")
      veg_index     <- attr(ts, "vegetation_index") %||% "CRSWIR"

      col_obs     <- "#2C7FB8"  # bleu observé (existant)
      col_pred    <- "#D62728"  # rouge prédiction harmonique (existant)
      col_seuil   <- "#FF7F0E"  # orange seuil_haut (nouveau v0.72.0)
      col_anomaly <- "#9C0E0E"  # rouge foncé points anomalie (nouveau)

      p <- plotly::plot_ly(type = "scatter")
      # 1. Observé (points bleus, mode = markers).
      p <- plotly::add_trace(
        p,
        x      = as.Date(ts$obs_date),
        y      = as.numeric(ts$crswir_obs),
        name   = i18n$t("monitoring_fordead_pixel_observed"),
        mode   = "markers",
        marker = list(color = col_obs, size = 5),
        hovertemplate = paste0(
          "<b>", i18n$t("monitoring_fordead_pixel_observed"), "</b><br>",
          "%{x|%Y-%m-%d}<br>",
          veg_index, " = %{y:.3f}<extra></extra>"
        )
      )
      # 2. Prédiction harmonique (ligne rouge).
      p <- plotly::add_trace(
        p,
        x      = as.Date(ts$obs_date),
        y      = as.numeric(ts$crswir_pred),
        name   = i18n$t("monitoring_fordead_pixel_predicted"),
        mode   = "lines",
        line   = list(color = col_pred, width = 1.5),
        hovertemplate = paste0(
          "<b>", i18n$t("monitoring_fordead_pixel_predicted"), "</b><br>",
          "%{x|%Y-%m-%d}<br>",
          veg_index, " = %{y:.3f}<extra></extra>"
        )
      )
      # 3. v0.72.0 — Seuil haut (= crswir_pred + threshold_anomaly).
      # Ligne orange pointillée. Quand un point observé dépasse ce
      # seuil, c'est une anomalie candidate.
      if ("seuil_haut" %in% names(ts)) {
        p <- plotly::add_trace(
          p,
          x      = as.Date(ts$obs_date),
          y      = as.numeric(ts$seuil_haut),
          name   = i18n$t("monitoring_fordead_pixel_threshold"),
          mode   = "lines",
          line   = list(color = col_seuil, width = 1.2,
                        dash = "dash"),
          hovertemplate = paste0(
            "<b>", i18n$t("monitoring_fordead_pixel_threshold"), "</b><br>",
            "%{x|%Y-%m-%d}<br>",
            veg_index, " = %{y:.3f}<extra></extra>"
          )
        )
      }
      # 4. v0.72.0 — Points en anomalie (rouge foncé, taille 8),
      # surlignés par-dessus la trace observée. Filtre :
      # ts$anomalie == TRUE && !is.na(ts$crswir_obs).
      if ("anomalie" %in% names(ts)) {
        anom_rows <- which(isTRUE(any(ts$anomalie)) |
                           (!is.na(ts$anomalie) & ts$anomalie))
        if (length(anom_rows)) {
          p <- plotly::add_trace(
            p,
            x      = as.Date(ts$obs_date[anom_rows]),
            y      = as.numeric(ts$crswir_obs[anom_rows]),
            name   = i18n$t("monitoring_fordead_pixel_anomaly"),
            mode   = "markers",
            marker = list(color = col_anomaly, size = 8,
                          line = list(color = "#FFFFFF", width = 1)),
            hovertemplate = paste0(
              "<b>", i18n$t("monitoring_fordead_pixel_anomaly"), "</b><br>",
              "%{x|%Y-%m-%d}<br>",
              veg_index, " = %{y:.3f}<extra></extra>"
            )
          )
        }
      }

      shapes <- list()
      annotations <- list()
      if (!is.null(premiere) && length(premiere) == 1L &&
          inherits(premiere, "Date") && !is.na(premiere)) {
        # Marqueur vertical sur la date de 1re anomalie détectée.
        shapes <- list(list(
          type = "line",
          xref = "x",   x0 = premiere, x1 = premiere,
          yref = "paper", y0 = 0, y1 = 1,
          line = list(color = "#000000", dash = "dash", width = 1.5)
        ))
        annotations <- list(list(
          xref = "x",  x = premiere, xanchor = "left",
          yref = "paper", y = 1, yanchor = "top",
          text = sprintf("%s : %s",
                         i18n$t("monitoring_fordead_pixel_first_anomaly"),
                         format(premiere, "%Y-%m-%d")),
          showarrow = FALSE,
          font = list(color = "#000000", size = 11)
        ))
      }
      # v0.72.0 — Annotation discrète en haut à gauche si le pixel
      # est hors zone de validité de l'essence dominante (calibration
      # FORDEAD non-applicable strictement). Visible quand
      # `attr(ts, "dans_zone_validite") == FALSE`.
      if (!is.null(in_validity) && length(in_validity) == 1L &&
          !is.na(in_validity) && isFALSE(in_validity)) {
        annotations <- c(annotations, list(list(
          xref = "paper", x = 0, xanchor = "left",
          yref = "paper", y = 1, yanchor = "top",
          text = i18n$t("monitoring_fordead_pixel_outside_validity"),
          showarrow = FALSE,
          font = list(color = "#FF7F0E", size = 11),
          bgcolor = "rgba(255, 255, 220, 0.85)",
          borderpad = 4
        )))
      }

      p <- plotly::layout(
        p,
        margin = list(t = 20, b = 40, l = 50, r = 10),
        xaxis  = list(title = i18n$t("monitoring_timeseries_xaxis"),
                      type = "date"),
        yaxis  = list(title = sprintf("%s (%s)",
                                       i18n$t("monitoring_fordead_pixel_yaxis"),
                                       veg_index)),
        legend = list(orientation = "h", y = -0.25),
        shapes = if (length(shapes)) shapes else NULL,
        annotations = if (length(annotations)) annotations else NULL
      )

      shiny::showModal(shiny::modalDialog(
        title = sprintf(
          i18n$t("monitoring_fordead_pixel_modal_title_fmt"),
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

    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)

    invisible(list(mask = mask_r))
  })
}

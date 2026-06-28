# mod_monitoring_reconfort_map.R — Sous-onglet "Alertes RECONFORT" du Suivi
# sanitaire (spec 021, L6). Miroir de mod_monitoring_fordead_map.R, adapté
# au pipeline RECONFORT (dépérissement feuillus, méthode RECONFORT/DSF).
#
# Deux sources, deux modes d'affichage :
#
#   * MODE MANIFESTE (post-run, en session) — quand le parent fournit le
#     `result` de `nemeton::run_reconfort_dieback()` via `result_r`, le
#     module appelle `nemeton::reconfort_layer_manifest(result,
#     include_range = TRUE)` et expose les COUCHES du run : rasters (score,
#     classes de santé, probabilité) + vecteur (alertes), pilotés par des
#     cases à cocher + un curseur d'opacité agissant sur les rasters. AUCUNE
#     sémantique métier ici : ids, palettes, domaines, sens (reverse) et
#     visibilité par défaut viennent tous du manifeste (CLAUDE.md §2-4).
#   * MODE DB (legacy / projet rechargé) — sans `result` en mémoire, le
#     module retombe sur l'affichage vectoriel des alertes lues en base via
#     `nemeton::list_alerts(con, zone_id, classes = RECONFORT_ALERT_CLASSES)`.
#
# Dans les deux modes : bannière de validité G3 (advisory, non bloquante)
# `nemeton::check_reconfort_validity(zone_aoi)`, et clic carte → diagnostic
# pixel `read_reconfort_pixel_series()` → modal plotly (CRSWIR + CRre).
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

#' Couleurs des classes du raster de classification RECONFORT (codes 1-2-3).
#' 1-sain (vert), 2-deperissant (orange), 3-tres-deperissant (rouge). Mappé
#' sur les codes entiers du raster `classification` du manifeste.
#' @noRd
.RECONFORT_CLASSIF_COLORS <- c(
  "1" = "#1A9850",  # vert    (sain)
  "2" = "#FF9933",  # orange  (dépérissant)
  "3" = "#D62728"   # rouge   (très dépérissant)
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
#' @param result_r Reactive returning the in-memory `result` list of the last
#'   `nemeton::run_reconfort_dieback()` of this session (carries `$rasters`),
#'   or `NULL`. When non-NULL, the module switches to the manifest-driven
#'   layered display (raster toggles + opacity). Optional — defaults to a
#'   constant `NULL` reactive (legacy DB-only behaviour, back-compat / tests).
#' @return invisible list with `alerts` reactive.
#' @noRd
mod_monitoring_reconfort_map_server <- function(id, app_state, zone_id_r,
                                                refresh_r = shiny::reactive(0L),
                                                result_r = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # ----- cache_dir resolver -------------------------------------------
    .reconfort_cache_dir <- function(proj) {
      if (is.null(proj) || is.null(proj$path)) return(NA_character_)
      file.path(proj$path, "cache", "layers", "reconfort")
    }

    # ----- Layer manifest (cœur) ----------------------------------------
    # Le contrat stable : le module ne lit JAMAIS result$rasters en dur, il
    # délègue au cœur la liste des couches + palettes/domaines/sens. NULL
    # quand aucun run n'est disponible en mémoire (mode DB legacy).
    manifest_r <- shiny::reactive({
      res <- result_r()
      if (is.null(res)) return(NULL)
      m <- tryCatch(
        nemeton::reconfort_layer_manifest(res, include_range = TRUE),
        error = function(e) {
          cli::cli_alert_warning("reconfort_layer_manifest failed: {e$message}")
          NULL
        }
      )
      if (is.null(m) || !nrow(m)) return(NULL)
      m
    })

    raster_manifest_r <- shiny::reactive({
      m <- manifest_r()
      if (is.null(m)) return(NULL)
      rm <- m[m$type == "raster", , drop = FALSE]
      if (!nrow(rm)) return(NULL)
      rm
    })

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

    # Default-visible layer ids from the manifest (checkbox initial state).
    .default_selected <- function(m) {
      if (is.null(m)) return(character())
      as.character(m$id[isTRUE(m$default_visible) | m$default_visible %in% TRUE])
    }

    # ----- Panel : validity banner + controls + (map | empty state) -----
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

      m <- manifest_r()
      a <- alerts_r()
      has_layers <- !is.null(m)
      has_alerts <- !is.null(a) && nrow(a) > 0L

      body <- if (has_layers) {
        # MODE MANIFESTE — toggles (libellés via i18n du label_key cœur) +
        # curseur d'opacité (rasters uniquement) + carte.
        choices  <- stats::setNames(
          as.character(m$id),
          vapply(m$label_key, function(k) i18n$t(k), character(1))
        )
        selected <- .default_selected(m)
        has_raster <- any(m$type == "raster")
        controls <- htmltools::div(
          class = "d-flex flex-wrap align-items-end gap-3 mb-2",
          htmltools::div(
            shiny::checkboxGroupInput(
              session$ns("layers"), i18n$t("reconfort_couches"),
              choices = choices, selected = selected, inline = TRUE
            )
          ),
          if (has_raster) htmltools::div(
            style = "min-width: 220px;",
            shiny::sliderInput(
              session$ns("opacity"), i18n$t("reconfort_opacite"),
              min = 0, max = 1, value = 0.8, step = 0.05
            )
          )
        )
        htmltools::tagList(
          controls,
          leaflet::leafletOutput(session$ns("map"), height = "55vh")
        )
      } else if (has_alerts) {
        # MODE DB legacy — alertes vectorielles seules.
        leaflet::leafletOutput(session$ns("map"), height = "55vh")
      } else {
        htmltools::div(
          class = "p-4 text-center text-muted",
          bsicons::bs_icon("hourglass-split",
                           class = "fs-1 d-block mx-auto mb-3"),
          htmltools::h5(class = "mt-3",
                        i18n$t("monitoring_reconfort_map_empty_title")),
          htmltools::p(class = "mb-0",
                       i18n$t("monitoring_reconfort_map_empty_body"))
        )
      }

      htmltools::tagList(banner, body)
    })

    # ----- Alert markers helper (shared by base render + proxy) ----------
    # Draws the RECONFORT alert plots as circle markers + class legend on a
    # leaflet map or proxy. Centroids for stable markers (the pixel
    # diagnostic itself runs on the raw map-click coordinate).
    .add_alerts <- function(map, a, i18n, opacity = 0.85) {
      if (is.null(a) || nrow(a) == 0L) return(map)
      pts <- tryCatch(
        suppressWarnings(sf::st_centroid(sf::st_geometry(a))),
        error = function(e) NULL
      )
      if (is.null(pts)) return(map)
      coords <- sf::st_coordinates(pts)

      cls    <- as.character(a$confidence_class)
      stress <- suppressWarnings(as.numeric(a$stress_index))
      colors <- unname(.RECONFORT_CLASS_COLORS)
      cats   <- names(.RECONFORT_CLASS_COLORS)
      pal <- leaflet::colorFactor(colors, levels = cats, na.color = "#888888")

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

      map |>
        leaflet::addCircleMarkers(
          lng = coords[, 1], lat = coords[, 2],
          radius      = 6,
          color       = "#333333",
          weight      = 1,
          fillColor   = pal(cls),
          fillOpacity = opacity,
          group       = "alerts",
          popup       = popups
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors   = colors,
          labels   = legend_labels,
          title    = i18n$t("monitoring_reconfort_class_title"),
          opacity  = 0.85,
          layerId  = "legend_alerts"
        )
    }

    # ----- Raster layer helper (shared by base render + proxy) -----------
    # Draws one manifest raster row (continuous or categorical) + its legend
    # at the requested opacity. Palettes / domain / reverse come from the
    # manifest — no business semantics here.
    .add_raster <- function(map, row, opacity, i18n) {
      r <- tryCatch(terra::rast(row$path), error = function(e) NULL)
      if (is.null(r)) return(map)
      if (terra::nlyr(r) > 1L) r <- r[[1L]]

      if (isTRUE(row$categorical)) {
        cols <- .RECONFORT_CLASSIF_COLORS
        lv   <- as.integer(names(cols))
        pal  <- leaflet::colorFactor(unname(cols), levels = lv,
                                     na.color = "transparent")
        r_show <- terra::ifel(is.na(r) | r < 1, NA, r)
        map <- leaflet::addRasterImage(
          map, x = r_show, colors = pal, opacity = opacity,
          method = "ngb", project = TRUE, group = row$id,
          options = leaflet::gridOptions(pane = "nemetonRaster")
        )
        labels <- c(i18n$t("reconfort_class_label_1"),
                    i18n$t("reconfort_class_label_2"),
                    i18n$t("reconfort_class_label_3"))
        map <- leaflet::addLegend(
          map, position = "bottomright", colors = unname(cols),
          labels = labels, title = i18n$t("reconfort_couche_classes"),
          opacity = opacity, layerId = paste0("legend_", row$id)
        )
      } else {
        dom <- c(row$vmin, row$vmax)
        if (!all(is.finite(dom)) || dom[1L] == dom[2L]) {
          mm <- tryCatch(as.numeric(terra::minmax(r)),
                         error = function(e) NULL)
          if (!is.null(mm) && all(is.finite(mm)) && mm[1L] != mm[2L]) {
            dom <- c(mm[1L], mm[2L])
          } else {
            dom <- c(0, 1)
          }
        }
        palname <- if (!is.na(row$palette)) row$palette else "viridis"
        pal <- leaflet::colorNumeric(palname, domain = dom,
                                     reverse = isTRUE(row$reverse),
                                     na.color = "transparent")
        map <- leaflet::addRasterImage(
          map, x = r, colors = pal, opacity = opacity,
          method = "bilinear", project = TRUE, group = row$id,
          options = leaflet::gridOptions(pane = "nemetonRaster")
        )
        map <- leaflet::addLegend(
          map, position = "bottomright", pal = pal, values = dom,
          title = i18n$t(row$label_key), opacity = opacity,
          layerId = paste0("legend_", row$id)
        )
      }
      map
    }

    # Currently selected layer ids (checkbox in manifest mode, else "alerts"
    # when DB alerts exist). Reactive so both the base render and the proxy
    # observer agree.
    selected_ids_r <- shiny::reactive({
      m <- manifest_r()
      if (!is.null(m)) {
        sel <- input$layers
        if (is.null(sel)) sel <- .default_selected(m)
        return(as.character(sel))
      }
      a <- alerts_r()
      if (!is.null(a) && nrow(a) > 0L) "alerts" else character()
    })

    opacity_r <- shiny::reactive({
      op <- suppressWarnings(as.numeric(input$opacity))
      if (length(op) != 1L || !is.finite(op)) op <- 0.8
      max(0, min(1, op))
    })

    # ----- Base map (rendered once per zone / result, not per toggle) ----
    output$map <- leaflet::renderLeaflet({
      m <- manifest_r()
      a <- alerts_r()
      if (is.null(m) && (is.null(a) || nrow(a) == 0L)) return(NULL)
      i18n <- i18n_r()
      op   <- shiny::isolate(opacity_r())
      sel  <- shiny::isolate(selected_ids_r())
      rm   <- shiny::isolate(raster_manifest_r())

      map <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",     group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          options    = leaflet::layersControlOptions(collapsed = TRUE)
        )

      # Initial layers (isolate) so the map is correct on first paint; the
      # observer below keeps them in sync with the checkboxes / slider.
      if (!is.null(rm)) {
        for (k in seq_len(nrow(rm))) {
          if (rm$id[k] %in% sel) map <- .add_raster(map, rm[k, ], op, i18n)
        }
      }
      if ("alerts" %in% sel) map <- .add_alerts(map, a, i18n)

      # Frame on the alerts (or AOI fallback handled by leaflet auto).
      if (!is.null(a) && nrow(a) > 0L) {
        bb <- tryCatch(sf::st_bbox(sf::st_transform(a, 4326)),
                       error = function(e) NULL)
        if (!is.null(bb)) {
          map <- leaflet::fitBounds(
            map, lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
            lng2 = bb[["xmax"]], lat2 = bb[["ymax"]]
          )
        }
      }
      map
    })

    # ----- Toggle / opacity → leafletProxy (re-render léger) -------------
    # addRasterImage has no dynamic opacity setter, so a slider move
    # re-draws the checked rasters at the new opacity via leafletProxy
    # (base map, zoom, base layer preserved). Mirror of the FORDEAD map.
    shiny::observe({
      m    <- manifest_r()
      a    <- alerts_r()
      if (is.null(m) && (is.null(a) || nrow(a) == 0L)) return()
      sel  <- selected_ids_r()
      op   <- opacity_r()
      i18n <- i18n_r()
      rm   <- raster_manifest_r()

      proxy <- leaflet::leafletProxy("map")
      # Clear every known group + its legend, then re-add the selected ones.
      known <- character()
      if (!is.null(rm)) known <- as.character(rm$id)
      known <- c(known, "alerts")
      for (g in known) {
        proxy <- proxy |>
          leaflet::clearGroup(g) |>
          leaflet::removeControl(paste0("legend_", g))
      }
      if (!is.null(rm)) {
        for (k in seq_len(nrow(rm))) {
          if (rm$id[k] %in% sel) proxy <- .add_raster(proxy, rm[k, ], op, i18n)
        }
      }
      if ("alerts" %in% sel) proxy <- .add_alerts(proxy, a, i18n)
      proxy
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

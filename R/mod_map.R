#' Map Module for nemetonApp
#'
#' @description
#' Shiny module for displaying an interactive Leaflet map with cadastral parcels.
#' Supports parcel selection/deselection by click, basemap switching, and
#' enforces maximum selection limit.
#'
#' @name mod_map
#' @keywords internal
NULL


#' Map Module UI
#'
#' @param id Module namespace ID.
#'
#' @return Shiny UI elements.
#'
#' @noRd
mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language)

  bslib::card(
    full_screen = TRUE,
    id = ns("map_card"),

    bslib::card_header(
      class = "d-flex justify-content-between align-items-center py-2",

      # Title
      htmltools::span(
        bsicons::bs_icon("map"),
        i18n$t("map_title"),
        class = "fw-semibold"
      ),

      # Controls
      htmltools::div(
        class = "d-flex gap-2 align-items-center",

        # Basemap toggle (use tags$button to avoid Shiny's btn-default class)
        htmltools::div(
          class = "btn-group btn-group-sm",
          role = "group",
          `aria-label` = "Basemap selection",
          htmltools::tags$button(
            id = ns("basemap_osm"),
            type = "button",
            class = "btn action-button basemap-btn basemap-btn-active",
            `data-val` = 0,
            "OSM"
          ),
          htmltools::tags$button(
            id = ns("basemap_satellite"),
            type = "button",
            class = "btn action-button basemap-btn",
            `data-val` = 0,
            "Satellite"
          )
        ),

        # Clear selection button
        shiny::actionButton(
          ns("clear_selection"),
          label = NULL,
          icon = bsicons::bs_icon("x-circle"),
          class = "btn btn-outline-danger btn-sm",
          title = i18n$t("clear_selection")
        )
      )
    ),

    bslib::card_body(
      padding = 0,
      class = "p-0",

      # Map container with loading overlay
      htmltools::div(
        id = ns("map_container"),
        style = "height: 100%; min-height: 500px; position: relative;",

        # Loading overlay (semi-transparent over map, hidden by default)
        htmltools::div(
          id = ns("map_loading_overlay"),
          class = "position-absolute top-0 start-0 w-100 h-100 d-none",
          style = "background: rgba(255,255,255,0.7); z-index: 99999; display: none; align-items: center; justify-content: center;",
          htmltools::div(
            class = "text-center",
            htmltools::div(
              class = "spinner-border text-success mb-2",
              role = "status"
            ),
            htmltools::div(
              class = "text-muted",
              i18n$t("rendering_parcels")
            )
          )
        ),

        # Leaflet output
        leaflet::leafletOutput(ns("map"), height = "100%")
      )
    ),

    bslib::card_footer(
      class = "py-2",

      # Selection summary — static HTML updated via JS (no renderUI)
      # to avoid Shiny busy state on each parcel click
      htmltools::div(
        id = ns("selection_summary"),
        class = "text-muted",
        htmltools::span(id = ns("selection_text"), i18n$t("click_to_select")),
        htmltools::span(id = ns("selection_area"), class = "text-muted ms-2")
      )
    )
  )
}


#' Map Module Server
#'
#' @param id Module namespace ID.
#' @param app_state Reactive values for app state.
#' @param commune_geometry Reactive sf object with commune boundary.
#' @param parcels Reactive sf object with cadastral parcels.
#'
#' @return List of reactive values:
#'   - selected_ids: Reactive character vector of selected parcel IDs
#'   - selected_parcels: Reactive sf object of selected parcels
#'   - selection_count: Reactive integer
#'
#' @noRd
mod_map_server <- function(id, app_state, commune_geometry, parcels) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================
    # Constants
    # ========================================

    MAX_PARCELS <- get_app_config("max_parcels", 30L)

    # Map styling
    STYLE <- list(
      # Commune boundary
      commune = list(
        color = "#1B6B1B",
        weight = 3,
        fill = FALSE,
        dashArray = "5, 5"
      ),
      # Unselected parcels
      parcel_default = list(
        color = "#666666",
        weight = 1,
        fillColor = "#CCCCCC",
        fillOpacity = 0.3
      ),
      # Selected parcels
      parcel_selected = list(
        color = "#1B6B1B",
        weight = 2,
        fillColor = "#1B6B1B",
        fillOpacity = 0.5
      ),
      # Hover state
      parcel_hover = list(
        weight = 2,
        fillOpacity = 0.4
      )
    )


    # ========================================
    # Reactive Values
    # ========================================

    rv <- shiny::reactiveValues(
      selected_ids = character(0),
      basemap = "osm",
      parcels_zoomed = FALSE,  # Flag to zoom only once per commune
      last_restore_timestamp = NULL  # Track last processed restore request
    )


    # ========================================
    # Map Loading Overlay
    # ========================================

    # Helper to show/hide map loading overlay via direct JS message
    # (avoids renderUI round-trip delay that caused green flash)
    #
    show_map_loading <- function(show, ...) {
      session$sendCustomMessage("showMapLoading", list(
        loadingId = ns("map_loading_overlay"),
        show = show
      ))
    }


    # ========================================
    # Base Map Rendering
    # ========================================

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        # Set default view (France)
        leaflet::setView(lng = 2.5, lat = 46.5, zoom = 6) |>
        # Add OSM tiles (default basemap)
        leaflet::addProviderTiles(
          leaflet::providers$OpenStreetMap,
          group = "basemap",
          layerId = "basemap_tiles",
          options = leaflet::providerTileOptions(
            updateWhenZooming = FALSE,
            updateWhenIdle = TRUE
          )
        ) |>
        # Scale bar
        leaflet::addScaleBar(
          position = "bottomleft",
          options = leaflet::scaleBarOptions(imperial = FALSE)
        ) |>
        # Attribution
        htmlwidgets::onRender("
          function(el, x) {
            // Add keyboard navigation for parcels
            el.addEventListener('keydown', function(e) {
              if (e.key === 'Enter' || e.key === ' ') {
                var target = e.target;
                if (target.classList.contains('leaflet-interactive')) {
                  target.click();
                }
              }
            });
          }
        ")
    })


    # ========================================
    # Basemap Toggle
    # ========================================

    shiny::observeEvent(input$basemap_osm, {
      rv$basemap <- "osm"
      cli::cli_alert_info("Switching to OSM basemap")

      leaflet::leafletProxy(ns("map")) |>
        leaflet::clearGroup("basemap") |>
        leaflet::addProviderTiles(
          leaflet::providers$OpenStreetMap,
          group = "basemap",
          layerId = "basemap_tiles"
        )

      session$sendCustomMessage("toggleBasemapButtons", list(
        osmId = ns("basemap_osm"),
        satId = ns("basemap_satellite"),
        active = "osm"
      ))
    })

    shiny::observeEvent(input$basemap_satellite, {
      rv$basemap <- "satellite"
      cli::cli_alert_info("Switching to Satellite basemap")

      leaflet::leafletProxy(ns("map")) |>
        leaflet::clearGroup("basemap") |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "basemap",
          layerId = "basemap_tiles"
        )

      session$sendCustomMessage("toggleBasemapButtons", list(
        osmId = ns("basemap_osm"),
        satId = ns("basemap_satellite"),
        active = "satellite"
      ))
    })


    # ========================================
    # Show loading when commune changes (before parcels arrive)
    # ========================================

    shiny::observe({
      geom <- commune_geometry()
      parcel_data <- parcels()
      if (!is.null(geom)) {
        rv$parcels_zoomed <- FALSE
        # Only show loading if parcels aren't available yet.
        # During project restore, parcels are set directly BEFORE
        # commune_geometry arrives, so loading is already visible
        # (shown by mod_home) and the combined observer will render
        # immediately — no need to re-show here.
        if (is.null(parcel_data) || nrow(parcel_data) == 0) {
          show_map_loading(TRUE)
        }
      }
    })


    # ========================================
    # Single combined observer: render map when both
    # commune_geometry AND parcels are ready
    # ========================================

    shiny::observe({
      parcel_data <- parcels()
      geom <- commune_geometry()

      # Both cleared (commune deselected): clear map and hide overlay
      if ((is.null(parcel_data) || nrow(parcel_data) == 0) && is.null(geom)) {
        leaflet::leafletProxy(ns("map")) |>
          leaflet::clearGroup("commune") |>
          leaflet::clearGroup("parcels") |>
          leaflet::clearGroup("selection")
        show_map_loading(FALSE)
        return()
      }

      # Wait for both commune geometry AND parcels before rendering.
      # While parcels are loading (geom set, parcels NULL), the loading
      # overlay stays visible — no map clearing or flashing.
      if (is.null(geom) || is.null(parcel_data) || nrow(parcel_data) == 0) {
        return()
      }

      # During commune transitions, the new geometry may arrive before stale
      # parcels are cleared (observer execution order is non-deterministic).
      # Skip rendering to prevent a flash of old parcels with new boundary.
      if (isTRUE(shiny::isolate(app_state$commune_transitioning))) {
        return()
      }

      # --- Both commune geometry and parcels are ready: render everything ---

      # 1. Validate and add commune boundary
      geom_types <- sf::st_geometry_type(geom)
      polygon_idx <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
      if (sum(polygon_idx) == 0) {
        cli::cli_warn("Commune geometry is not a polygon, skipping display")
        show_map_loading(FALSE)
        return()
      }
      if (sum(!polygon_idx) > 0) {
        geom <- geom[polygon_idx, ]
      }

      bbox_commune <- sf::st_bbox(geom)

      leaflet::leafletProxy(ns("map")) |>
        leaflet::clearGroup("commune") |>
        leaflet::clearGroup("parcels") |>
        leaflet::clearGroup("selection") |>
        leaflet::addPolygons(
          data = geom,
          group = "commune",
          color = STYLE$commune$color,
          weight = STYLE$commune$weight,
          fill = STYLE$commune$fill,
          dashArray = STYLE$commune$dashArray
        )

      # 2. Render parcels
      render_parcels_to_map(parcel_data)

      # 3. Handle project restore (apply saved selection)
      restore <- shiny::isolate(app_state$restore_project)
      if (!is.null(restore) && !is.null(restore$selected_ids) &&
          (is.null(rv$last_restore_timestamp) ||
           !identical(rv$last_restore_timestamp, restore$timestamp))) {

        matching_ids <- intersect(restore$selected_ids, parcel_data$id)
        if (length(matching_ids) > 0) {
          rv$last_restore_timestamp <- restore$timestamp
          rv$selected_ids <- matching_ids
          cli::cli_alert_info("Restoring {length(matching_ids)} parcels")

          # Apply selection styles
          for (pid in matching_ids) {
            update_parcel_style(pid, selected = TRUE)
          }

          # Zoom to selected parcels
          selected_sf <- parcel_data[parcel_data$id %in% matching_ids, ]
          bbox_sel <- sf::st_bbox(selected_sf)
          leaflet::leafletProxy(ns("map")) |>
            leaflet::fitBounds(
              lng1 = as.numeric(bbox_sel[["xmin"]]),
              lat1 = as.numeric(bbox_sel[["ymin"]]),
              lng2 = as.numeric(bbox_sel[["xmax"]]),
              lat2 = as.numeric(bbox_sel[["ymax"]])
            )
          rv$parcels_zoomed <- TRUE

          # NOTE: Do NOT clear restore_in_progress here. This observer
          # can fire BEFORE restore_task completes (if parcels_data was set
          # while commune_geometry still holds the PREVIOUS commune's geom).
          # Clearing here would cause the selected_commune observer in
          # mod_home to call reset_project_state(), wiping the selection.
          # restore_in_progress is cleared by mod_search.R's restore_task
          # result handler, after the commune geometry is actually updated.
          cli::cli_alert_success("Parcels selection restored on map")
        }

      } else {
        # Normal navigation: zoom to parcels on first load
        if (!rv$parcels_zoomed) {
          cli::cli_alert_info("Zooming to all parcels")
          bbox_p <- sf::st_bbox(parcel_data)
          leaflet::leafletProxy(ns("map")) |>
            leaflet::fitBounds(
              lng1 = as.numeric(bbox_p[["xmin"]]),
              lat1 = as.numeric(bbox_p[["ymin"]]),
              lng2 = as.numeric(bbox_p[["xmax"]]),
              lat2 = as.numeric(bbox_p[["ymax"]])
            )
          rv$parcels_zoomed <- TRUE
        }

        # Re-apply selection styling if needed
        if (length(shiny::isolate(rv$selected_ids)) > 0) {
          update_parcel_styles(shiny::isolate(rv$selected_ids))
        }
      }

      show_map_loading(FALSE)
    })


    # ========================================
    # Handle Parcel Click (Toggle Selection)
    # ========================================

    shiny::observeEvent(input$map_shape_click, {
      click <- input$map_shape_click

      if (is.null(click) || is.null(click$id)) return()

      i18n <- get_i18n(app_state$language)

      # Block parcel modification during computation (T089)
      if (isTRUE(app_state$computation_running)) {
        shiny::showNotification(
          i18n$t("selection_blocked_during_computation"),
          type = "warning"
        )
        return()
      }

      parcel_id <- click$id

      # Check if already selected
      if (parcel_id %in% rv$selected_ids) {
        # Deselect
        rv$selected_ids <- setdiff(rv$selected_ids, parcel_id)

        # Update styling
        update_parcel_style(parcel_id, selected = FALSE)

        # Announce to screen reader
        session$sendCustomMessage("announceSelection", list(
          action = "deselected",
          id = parcel_id,
          count = length(rv$selected_ids)
        ))

      } else {
        # Check limit
        if (length(rv$selected_ids) >= MAX_PARCELS) {
          shiny::showNotification(
            i18n$t("max_parcels_warning", max = MAX_PARCELS),
            type = "warning"
          )
          return()
        }

        # Select
        rv$selected_ids <- c(rv$selected_ids, parcel_id)

        # Update styling
        update_parcel_style(parcel_id, selected = TRUE)

        # Announce
        session$sendCustomMessage("announceSelection", list(
          action = "selected",
          id = parcel_id,
          count = length(rv$selected_ids)
        ))
      }
    })


    # ========================================
    # Clear Selection
    # ========================================

    shiny::observeEvent(input$clear_selection, {
      if (length(rv$selected_ids) == 0) return()

      # Block during computation (T089)
      if (isTRUE(app_state$computation_running)) {
        shiny::showNotification(
          get_i18n(app_state$language)$t("selection_blocked_during_computation"),
          type = "warning"
        )
        return()
      }

      # Clear the selection overlay group
      leaflet::leafletProxy(ns("map")) |>
        leaflet::clearGroup("selection")

      rv$selected_ids <- character(0)

      shiny::showNotification(
        get_i18n(app_state$language)$t("selection_cleared"),
        type = "message"
      )
    })

    # Clear selection when triggered externally (e.g., project deletion)
    shiny::observeEvent(app_state$clear_map_selection, {
      if (length(rv$selected_ids) == 0) return()

      # Clear the selection overlay group
      leaflet::leafletProxy(ns("map")) |>
        leaflet::clearGroup("selection")

      rv$selected_ids <- character(0)
      rv$parcels_zoomed <- FALSE
    }, ignoreInit = TRUE)


    # ========================================
    # Restore Project: show loading overlay immediately
    # ========================================

    shiny::observeEvent(app_state$restore_project, {
      restore <- app_state$restore_project
      if (!is.null(restore) && !is.null(restore$selected_ids)) {
        show_map_loading(TRUE)

        # Reset restore tracking so the combined observer re-enters
        # the restore block when it fires with the correct geometry.
        # Without this, if the combined observer fires first with stale
        # geometry (before restore_task completes), it sets
        # last_restore_timestamp. The second firing (with correct geometry)
        # skips the restore block because the timestamp already matches.
        rv$last_restore_timestamp <- NULL
        rv$parcels_zoomed <- FALSE
      }
    }, ignoreInit = TRUE)

    # Safety net: when restore_in_progress goes FALSE (from success in the
    # combined observer, or from failure in mod_search), ensure the spinner
    # is hidden. On success, the combined observer already called
    # show_map_loading(FALSE), so this is a no-op. On failure (e.g.
    # commune_geometry never arrived), this ensures the spinner stops.
    shiny::observeEvent(app_state$restore_in_progress, {
      if (identical(app_state$restore_in_progress, FALSE)) {
        show_map_loading(FALSE)
      }
    }, ignoreInit = TRUE)


    # ========================================
    # Render Parcels Helper
    # ========================================

    # Renders base parcel polygons to the map.
    render_parcels_to_map <- function(parcel_data) {
      # Filter to keep only polygon geometries (defensive check)
      geom_types <- sf::st_geometry_type(parcel_data)
      polygon_idx <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
      if (sum(polygon_idx) == 0) {
        cli::cli_warn("No polygon geometries in parcel data")
        leaflet::leafletProxy(ns("map")) |>
          leaflet::clearGroup("parcels")
        return(invisible(FALSE))
      }
      if (sum(!polygon_idx) > 0) {
        parcel_data <- parcel_data[polygon_idx, ]
      }

      # Ensure WGS84
      crs_epsg <- tryCatch(sf::st_crs(parcel_data)$epsg, error = function(e) NULL)
      if (!is.null(crs_epsg) && !is.na(crs_epsg) && crs_epsg != 4326) {
        parcel_data <- sf::st_transform(parcel_data, 4326)
      }

      # Create labels for hover display
      labels <- sapply(seq_len(nrow(parcel_data)), function(i) {
        create_parcel_label(parcel_data[i, ])
      })

      leaflet::leafletProxy(ns("map")) |>
        leaflet::clearGroup("parcels") |>
        leaflet::addPolygons(
          data = parcel_data,
          layerId = parcel_data$id,
          group = "parcels",
          color = STYLE$parcel_default$color,
          weight = STYLE$parcel_default$weight,
          fillColor = STYLE$parcel_default$fillColor,
          fillOpacity = STYLE$parcel_default$fillOpacity,
          label = lapply(labels, htmltools::HTML),
          labelOptions = leaflet::labelOptions(
            style = list(
              "font-size" = "12px",
              "font-weight" = "normal",
              "padding" = "6px 10px",
              "background-color" = "white",
              "border" = "1px solid #ccc",
              "border-radius" = "4px",
              "box-shadow" = "0 2px 4px rgba(0,0,0,0.2)"
            ),
            direction = "auto",
            offset = c(0, -5)
          ),
          highlightOptions = leaflet::highlightOptions(
            weight = STYLE$parcel_hover$weight,
            fillOpacity = STYLE$parcel_hover$fillOpacity,
            bringToFront = TRUE
          ),
          options = leaflet::pathOptions(
            className = "parcel-polygon"
          )
        )

      invisible(TRUE)
    }


    # ========================================
    # Restore Project Selection
    # ========================================

    # Capture restore request
    shiny::observeEvent(app_state$restore_project, {
      restore <- app_state$restore_project
      if (!is.null(restore) && !is.null(restore$selected_ids)) {
        rv$pending_restore <- restore
        cli::cli_alert_info("Restoration pending for {length(restore$selected_ids)} parcels")
      }
    }, ignoreInit = TRUE)

    # Apply restoration when parcels are loaded
    shiny::observe({
      restore <- rv$pending_restore
      if (is.null(restore)) return()

      parcel_data <- parcels()
      if (is.null(parcel_data) || nrow(parcel_data) == 0) return()

      # Find matching parcel IDs
      matching_ids <- intersect(restore$selected_ids, parcel_data$id)

      if (length(matching_ids) > 0) {
        # Set selected IDs
        rv$selected_ids <- matching_ids

        # Update styles for selected parcels
        for (pid in matching_ids) {
          update_parcel_style(pid, selected = TRUE)
        }

        # Zoom to selected parcels extent
        selected_parcels <- parcel_data[parcel_data$id %in% matching_ids, ]
        if (nrow(selected_parcels) > 0) {
          bbox <- sf::st_bbox(selected_parcels)
          leaflet::leafletProxy(ns("map")) |>
            leaflet::fitBounds(
              lng1 = as.numeric(bbox[["xmin"]]),
              lat1 = as.numeric(bbox[["ymin"]]),
              lng2 = as.numeric(bbox[["xmax"]]),
              lat2 = as.numeric(bbox[["ymax"]])
            )
        }

        cli::cli_alert_success("Restored {length(matching_ids)} selected parcels")
      }

      # Clear pending restore
      rv$pending_restore <- NULL
    })


    # ========================================
    # Update Parcel Style Helper
    # ========================================

    update_parcel_style <- function(parcel_id, selected) {
      if (selected) {
        # Add parcel to selection overlay
        parcel_data <- shiny::isolate(parcels())
        if (is.null(parcel_data)) return()

        parcel <- parcel_data[parcel_data$id == parcel_id, ]
        if (nrow(parcel) == 0) return()

        style <- STYLE$parcel_selected

        leaflet::leafletProxy(ns("map")) |>
          leaflet::addPolygons(
            data = parcel,
            layerId = paste0("sel_", parcel_id),
            group = "selection",
            color = style$color,
            weight = style$weight,
            fillColor = style$fillColor,
            fillOpacity = style$fillOpacity,
            options = leaflet::pathOptions(
              interactive = FALSE
            )
          )
      } else {
        # Remove from selection overlay
        leaflet::leafletProxy(ns("map")) |>
          leaflet::removeShape(paste0("sel_", parcel_id))
      }
    }

    update_parcel_styles <- function(selected_ids) {
      for (pid in selected_ids) {
        update_parcel_style(pid, selected = TRUE)
      }
    }


    # ========================================
    # Selection Summary (JS-driven, no renderUI)
    #
    # Using sendCustomMessage instead of renderUI avoids triggering
    # Shiny's busy state on every parcel click, which caused the
    # white overlay flash between selections.
    # ========================================

    shiny::observe({
      i18n <- get_i18n(app_state$language)
      n <- length(rv$selected_ids)
      parcel_data <- parcels()

      area_text <- ""
      if (n > 0 && !is.null(parcel_data)) {
        selected <- parcel_data[parcel_data$id %in% rv$selected_ids, ]
        stats <- calculate_parcel_stats(selected)
        area_text <- sprintf("| %.2f ha", stats$total_area_ha)
      }

      at_limit <- n >= MAX_PARCELS

      session$sendCustomMessage("updateSelectionSummary", list(
        containerId = ns("selection_summary"),
        textId = ns("selection_text"),
        areaId = ns("selection_area"),
        count = n,
        max = MAX_PARCELS,
        atLimit = at_limit,
        areaText = area_text,
        emptyLabel = i18n$t("click_to_select"),
        selectedLabel = i18n$t("parcels_selected")
      ))
    })


    # ========================================
    # Return Values
    # ========================================

    list(
      selected_ids = shiny::reactive(rv$selected_ids),
      selected_parcels = shiny::reactive({
        parcel_data <- parcels()
        if (is.null(parcel_data) || length(rv$selected_ids) == 0) {
          return(NULL)
        }
        filter_selected_parcels(parcel_data, rv$selected_ids)
      }),
      selection_count = shiny::reactive(length(rv$selected_ids))
    )
  })
}

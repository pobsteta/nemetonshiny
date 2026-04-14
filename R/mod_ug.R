#' UG (Management Units) Module
#'
#' @description
#' Shiny module for managing Unités de Gestion (UG).
#' Boucle 1: table + leaflet map with tenement selection, merge/split/rename,
#' groupe d'aménagement assignment, and color-coded map display.
#'
#' @name mod_ug
#' @keywords internal
NULL


#' UG Module UI
#'
#' @param id Character. Module namespace ID.
#'
#' @return Shiny UI tag list.
#' @noRd
mod_ug_ui <- function(id) {
  # Composite UI (layout_sidebar) — used when the UG module has its own tab.
  # When embedded in the Sélection tab, use the helpers below instead:
  #   mod_ug_actions_bar(id), mod_ug_map_panel(id), mod_ug_table_panel(id)
  ns <- shiny::NS(id)

  opts <- get_app_options()
  lang <- opts$language
  i18n <- get_i18n(lang)

  bslib::layout_sidebar(
    fillable = TRUE,
    sidebar = bslib::sidebar(
      title = i18n$t("ug_title"),
      width = 350,
      mod_ug_actions_bar(id)
    ),
    bslib::navset_card_tab(
      bslib::nav_panel(
        title = i18n$t("ug_map_tab"),
        icon = bsicons::bs_icon("map"),
        mod_ug_map_panel(id)
      ),
      bslib::nav_panel(
        title = i18n$t("ug_table_tab"),
        icon = bsicons::bs_icon("table"),
        mod_ug_table_panel(id)
      )
    )
  )
}


#' UG map panel (leaflet output)
#'
#' @description
#' Lightweight UI helper producing just the tenement leaflet output.
#' Uses the given \code{id} as the module namespace.
#'
#' @param id Character. Module namespace ID (typically "ug").
#' @return Shiny tag with the leaflet output.
#' @noRd
mod_ug_map_panel <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language %||% "fr")

  htmltools::tagList(
    # Header bar with title + basemap toggles (same pattern as mod_map)
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center py-2",

      # Title
      htmltools::span(
        bsicons::bs_icon("diagram-3"),
        i18n$t("ug_map_card_title"),
        class = "fw-semibold"
      ),

      # Controls
      htmltools::div(
        class = "d-flex gap-2 align-items-center",
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
        )
      )
    ),

    # Body: leaflet map
    bslib::card_body(
      padding = 0,
      class = "p-0",
      htmltools::div(
        id = ns("ug_map_container"),
        style = "height: 100%; min-height: 500px; position: relative;",
        leaflet::leafletOutput(ns("ug_map"), height = "100%")
      )
    ),

    # Footer: tenement count + surface ratio
    bslib::card_footer(
      class = "py-2",
      htmltools::div(
        class = "d-flex justify-content-between align-items-center text-muted small",
        shiny::textOutput(ns("ug_map_count"), inline = TRUE),
        shiny::textOutput(ns("ug_map_surface"), inline = TRUE)
      )
    )
  )
}


#' UG table panel (DT output)
#'
#' @param id Character. Module namespace ID.
#' @return Shiny tag with the UG data table.
#' @noRd
mod_ug_table_panel <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language %||% "fr")

  bslib::card_body(
    htmltools::div(
      class = "d-flex justify-content-between align-items-center mb-2",
      shiny::h5(i18n$t("ug_title"), class = "mb-0"),
      shiny::textOutput(ns("ug_summary"), inline = TRUE)
    ),
    DT::dataTableOutput(ns("ug_table"))
  )
}


#' UG actions bar (buttons, groupe selector, detail panel)
#'
#' @description
#' The sidebar-style action bar for UG management. Used both as
#' the sidebar of mod_ug_ui and as a vertical panel embedded next
#' to the map/table in other layouts.
#'
#' @param id Character. Module namespace ID.
#' @return Shiny tag list with all UG actions.
#' @noRd
mod_ug_actions_bar <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language %||% "fr")

  htmltools::tagList(
    # Action buttons
    htmltools::div(
      class = "d-grid gap-2 mb-3",

      shiny::actionButton(
        ns("btn_merge"),
        label = i18n$t("ug_merge"),
        icon = shiny::icon("object-group"),
        class = "btn-success",
        width = "100%"
      ),

      shiny::actionButton(
        ns("btn_create_from_map"),
        label = i18n$t("ug_create_from_map"),
        icon = shiny::icon("plus-circle"),
        class = "btn-success btn-sm",
        width = "100%"
      ),

      shiny::actionButton(
        ns("btn_move_to_ug"),
        label = i18n$t("ug_move_to"),
        icon = shiny::icon("arrow-right-arrow-left"),
        class = "btn-outline-success btn-sm",
        width = "100%"
      ),

      shiny::actionButton(
        ns("btn_split"),
        label = i18n$t("ug_split"),
        icon = shiny::icon("scissors"),
        class = "btn-warning",
        width = "100%"
      ),

      shiny::actionButton(
        ns("btn_rename"),
        label = i18n$t("ug_rename"),
        icon = shiny::icon("pen"),
        class = "btn-outline-secondary",
        width = "100%"
      ),

      shiny::actionButton(
        ns("btn_recompute"),
        label = i18n$t("ug_recompute"),
        icon = shiny::icon("calculator"),
        class = "btn-primary",
        width = "100%"
      ),

      shiny::actionButton(
        ns("btn_import_split"),
        label = i18n$t("ug_import_split"),
        icon = shiny::icon("file-import"),
        class = "btn-outline-info",
        width = "100%"
      ),

      shiny::actionButton(
        ns("btn_undo_split"),
        label = i18n$t("ug_undo_split"),
        icon = shiny::icon("rotate-left"),
        class = "btn-outline-secondary btn-sm",
        width = "100%"
      )
    ),

    shiny::hr(),

    # Groupe d'aménagement selector
    htmltools::div(
      class = "mb-3",
      shiny::selectInput(
        ns("sel_groupe"),
        label = i18n$t("ug_group"),
        choices = c(
          "---" = "",
          "AMETS" = "AMETS", "AMER" = "AMER", "IRR" = "IRR",
          "TSF" = "TSF", "REGT" = "REGT", "REGF" = "REGF",
          "HSN" = "HSN", "HSY" = "HSY", "PROT" = "PROT", "ACC" = "ACC"
        ),
        selected = ""
      ),
      shiny::actionButton(
        ns("btn_set_groupe"),
        label = i18n$t("ug_apply_group"),
        icon = shiny::icon("tag"),
        class = "btn-outline-primary btn-sm",
        width = "100%"
      )
    ),

    shiny::hr(),

    # Map selection info
    shiny::uiOutput(ns("map_selection_info")),

    shiny::hr(),

    # UG detail panel
    htmltools::div(
      id = ns("detail_panel"),
      shiny::h6(i18n$t("ug_composition")),
      shiny::uiOutput(ns("ug_detail"))
    )
  )
}


#' UG Module Server
#'
#' @param id Character. Module namespace ID.
#' @param app_state reactiveValues. Application state.
#'
#' @return NULL (called for side effects).
#' @noRd
mod_ug_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    lang <- shiny::reactive(app_state$language %||% "fr")
    i18n <- shiny::reactive(get_i18n(lang()))

    # ================================================================
    # REACTIVE: UG state (projet with tenements/ugs)
    # ================================================================
    rv <- shiny::reactiveValues(
      projet_ug = NULL,          # projet list with $tenements, $ugs, $parcels
      needs_recompute = FALSE,   # flag: UGs changed, indicators need refresh
      selected_tenement_ids = character(0),  # tenements selected on the map
      map_needs_zoom = FALSE,    # flag: zoom to bounds on next map update
      pending_bbox = NULL,       # stored bbox for deferred zoom
      redraw_counter = 0L        # incremented to force map polygon redraw
    )

    # Initialize UG data when project loads
    shiny::observe({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$parcels)) {
        rv$projet_ug <- NULL
        rv$selected_tenement_ids <- character(0)
        rv$map_needs_zoom <- FALSE
        return()
      }

      # Reset map zoom flag for new project
      rv$map_needs_zoom <- TRUE
      rv$selected_tenement_ids <- character(0)

      # Try to load existing UG data
      if (has_ug_data(project)) {
        rv$projet_ug <- project
      } else if (!is.null(project$metadata$id)) {
        projet <- ensure_project_migrated(project$metadata$id, project)
        rv$projet_ug <- projet
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs
      }
    }) |> shiny::bindEvent(app_state$current_project)

    # ================================================================
    # REACTIVE: UG listing for the table
    # ================================================================
    ug_listing <- shiny::reactive({
      projet <- rv$projet_ug
      rv$redraw_counter  # also invalidate when redraw is forced
      if (is.null(projet) || !has_ug_data(projet)) return(NULL)
      ug_list(projet)
    })

    # ================================================================
    # MAP: Render leaflet
    # ================================================================
    # Basemap state
    rv$basemap <- "osm"

    output$ug_map <- leaflet::renderLeaflet({
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$OpenStreetMap,
          group = "basemap",
          layerId = "basemap_tiles"
        ) |>
        leaflet::addLayersControl(
          overlayGroups = c("UGF", "Tenements", "Selection"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::setView(lng = 2.5, lat = 46.5, zoom = 6)

      # Add draw toolbar if leaflet.extras is available
      if (requireNamespace("leaflet.extras", quietly = TRUE)) {
        m <- m |>
          leaflet.extras::addDrawToolbar(
            targetGroup = "draw",
            polylineOptions = leaflet.extras::drawPolylineOptions(
              shapeOptions = leaflet.extras::drawShapeOptions(
                color = "#FF0000",
                weight = 3
              )
            ),
            circleOptions = FALSE,
            circleMarkerOptions = FALSE,
            markerOptions = FALSE,
            rectangleOptions = FALSE,
            polygonOptions = leaflet.extras::drawPolygonOptions(
              shapeOptions = leaflet.extras::drawShapeOptions(
                color = "#FF4500",
                fillColor = "#FF6347",
                fillOpacity = 0.3,
                weight = 2
              )
            ),
            editOptions = leaflet.extras::editToolbarOptions(
              edit = TRUE,
              remove = TRUE
            )
          )
      }

      m
    })

    # ================================================================
    # MAP: Basemap toggle (OSM / Satellite)
    # ================================================================
    shiny::observeEvent(input$basemap_osm, {
      rv$basemap <- "osm"
      leaflet::leafletProxy(ns("ug_map")) |>
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
      leaflet::leafletProxy(ns("ug_map")) |>
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

    # ================================================================
    # MAP FOOTER: tenement count and surface summary
    # ================================================================
    output$ug_map_count <- shiny::renderText({
      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) return("")
      n <- nrow(projet$tenements)
      sprintf(i18n()$t("ug_map_summary_count"), n)
    })

    output$ug_map_surface <- shiny::renderText({
      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) return("")

      # Total surface of tenements (in hectares)
      surf_tenements_ha <- sum(projet$tenements$surface_m2, na.rm = TRUE) / 10000

      # Total surface of cadastral parcels (the parent parcels that have tenements)
      parcels <- projet$parcels
      parent_ids <- unique(projet$tenements$parent_parcelle_id)
      id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(parcels))
      surf_parcels_ha <- if (length(id_col) > 0) {
        selected_parcels <- parcels[as.character(parcels[[id_col[1]]]) %in% parent_ids, ]
        if ("contenance" %in% names(selected_parcels)) {
          sum(as.numeric(selected_parcels$contenance), na.rm = TRUE) / 10000
        } else {
          sum(as.numeric(sf::st_area(selected_parcels)), na.rm = TRUE) / 10000
        }
      } else {
        surf_tenements_ha
      }

      sprintf(
        i18n()$t("ug_map_summary_surface"),
        format(round(surf_tenements_ha, 2), nsmall = 2),
        format(round(surf_parcels_ha, 2), nsmall = 2)
      )
    })

    # ================================================================
    # MAP: Update polygons when UG data changes OR tab becomes visible
    # ================================================================
    shiny::observe({
      # Dependencies: data change AND explicit redraw requests
      projet <- rv$projet_ug
      rv$redraw_counter  # invalidate on redraw trigger
      if (is.null(projet) || !has_ug_data(projet)) return()

      tenements <- projet$tenements
      ugs <- projet$ugs

      # Ensure WGS84 for leaflet
      if (!is.na(sf::st_crs(tenements)) && sf::st_crs(tenements)$epsg != 4326L) {
        tenements <- sf::st_transform(tenements, 4326)
      }

      # Compute fill colors per tenement (based on UG groupe or index)
      ug_index_map <- stats::setNames(seq_len(nrow(ugs)), ugs$ug_id)
      fill_colors <- vapply(seq_len(nrow(tenements)), function(i) {
        uid <- tenements$ug_id[i]
        ug_row <- ugs[ugs$ug_id == uid, ]
        if (nrow(ug_row) == 0) return("#CCCCCC")
        idx <- ug_index_map[[uid]]
        ug_color(ug_row$groupe[1], idx)
      }, character(1))

      # Labels for hover
      atom_labels <- vapply(seq_len(nrow(tenements)), function(i) {
        uid <- tenements$ug_id[i]
        ug_row <- ugs[ugs$ug_id == uid, ]
        ug_label <- if (nrow(ug_row) > 0) ug_row$label[1] else "?"
        groupe_str <- if (nrow(ug_row) > 0 && !is.na(ug_row$groupe[1])) {
          paste0(" [", ug_row$groupe[1], "]")
        } else {
          ""
        }
        sprintf(
          "<b>%s</b>%s<br>Tenement: %s<br>Surface: %s m\u00b2",
          ug_label, groupe_str,
          tenements$tenement_id[i],
          format(round(tenements$surface_m2[i]), big.mark = " ")
        )
      }, character(1))

      # Clear and redraw tenements (selection is handled by separate overlay)
      proxy <- leaflet::leafletProxy(ns("ug_map"))
      proxy |>
        leaflet::clearGroup("Tenements") |>
        leaflet::clearGroup("UGF") |>
        leaflet::clearGroup("Selection") |>
        leaflet::addPolygons(
          data = tenements,
          group = "Tenements",
          layerId = tenements$tenement_id,
          fillColor = fill_colors,
          fillOpacity = 0.5,
          color = "#333333",
          weight = 1,
          label = lapply(atom_labels, htmltools::HTML),
          labelOptions = leaflet::labelOptions(
            style = list("font-size" = "12px", "background" = "white"),
            textsize = "12px"
          ),
          highlightOptions = leaflet::highlightOptions(
            weight = 3,
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        )

      # Clear selection state when tenements are redrawn (new project)
      rv$selected_tenement_ids <- character(0)

      # Add UG dissolved boundaries as an overlay
      tryCatch({
        ug_sf <- ug_build_sf(projet)
        if (!is.null(ug_sf) && nrow(ug_sf) > 0) {
          if (!is.na(sf::st_crs(ug_sf)) && sf::st_crs(ug_sf)$epsg != 4326L) {
            ug_sf <- sf::st_transform(ug_sf, 4326)
          }

          ug_colors <- vapply(seq_len(nrow(ug_sf)), function(i) {
            ug_color(ug_sf$groupe[i], i)
          }, character(1))

          ug_labels <- vapply(seq_len(nrow(ug_sf)), function(i) {
            groupe_str <- if (!is.na(ug_sf$groupe[i])) {
              paste0(" [", ug_sf$groupe[i], "]")
            } else {
              ""
            }
            sprintf(
              "<b>%s</b>%s<br>%d tenement(s) | %s ha",
              ug_sf$label[i], groupe_str,
              ug_sf$n_tenements[i],
              format(round(ug_sf$surface_m2[i] / 10000, 2), nsmall = 2)
            )
          }, character(1))

          proxy |>
            leaflet::addPolygons(
              data = ug_sf,
              group = "UGF",
              fillColor = ug_colors,
              fillOpacity = 0.15,
              color = ug_colors,
              weight = 3,
              dashArray = "5,5",
              label = lapply(ug_labels, htmltools::HTML),
              labelOptions = leaflet::labelOptions(
                style = list("font-size" = "13px", "font-weight" = "bold"),
                textsize = "13px"
              )
            )
        }
      }, error = function(e) {
        cli::cli_warn("Failed to render UG boundaries: {e$message}")
      })

      # Store bbox and attempt zoom (may fail if tab not visible)
      if (isTRUE(rv$map_needs_zoom)) {
        bbox <- sf::st_bbox(tenements)
        rv$pending_bbox <- bbox
        proxy |> leaflet::fitBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
        rv$map_needs_zoom <- FALSE
      }

      # Add legend + re-add layers control (clearControls also removes it)
      proxy |>
        leaflet::clearControls() |>
        leaflet::addLayersControl(
          overlayGroups = c("UGF", "Tenements", "Selection"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors = GROUPE_COLORS[names(GROUPE_COLORS) %in% ugs$groupe],
          labels = names(GROUPE_COLORS)[names(GROUPE_COLORS) %in% ugs$groupe],
          title = i18n()$t("ug_group"),
          opacity = 0.8
        )
    })

    # ================================================================
    # MAP: Re-zoom when the tenement map becomes visible
    # ================================================================
    # Leaflet fitBounds fails silently when the map container has 0 size
    # (hidden tab). The tenement map lives in a sub-tab of "Sélection"
    # (home-main_tabs = "tenements"). Every time the user navigates to
    # that sub-tab, we:
    #  1. Recompute the bbox from the current rv$projet_ug (always fresh)
    #  2. Ask leaflet to invalidate its size (redetect container dims)
    #  3. Apply fitBounds with a small delay to let the DOM settle
    shiny::observe({
      root_session <- session$userData$root_session
      if (is.null(root_session)) return()

      top_nav <- root_session$input$main_nav
      sub_nav <- root_session$input[["home-main_tabs"]]

      if (is.null(top_nav) || top_nav != "selection") return()
      if (is.null(sub_nav) || sub_nav != "tenements") return()

      projet <- shiny::isolate(rv$projet_ug)
      if (is.null(projet) || !has_ug_data(projet)) return()

      # Always recompute bbox from current data (not cached)
      tenements <- projet$tenements
      if (!is.na(sf::st_crs(tenements)) && sf::st_crs(tenements)$epsg != 4326L) {
        tenements <- sf::st_transform(tenements, 4326)
      }
      bbox <- tryCatch(sf::st_bbox(tenements), error = function(e) NULL)
      if (is.null(bbox)) return()

      # Force re-drawing the polygons — they may have been issued while the
      # tab was hidden (map not in DOM) and silently dropped by leaflet.
      rv$redraw_counter <- shiny::isolate(rv$redraw_counter) + 1L

      # Force leaflet to re-detect its container size (critical for hidden tabs)
      later::later(function() {
        proxy <- leaflet::leafletProxy(ns("ug_map"), session = session)
        # Trigger invalidateSize via custom message to the map
        session$sendCustomMessage("leafletInvalidateSize", list(
          id = ns("ug_map")
        ))
        proxy |> leaflet::fitBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
      }, delay = 0.3)

      # Consume pending_bbox if any (kept for compat)
      rv$pending_bbox <- NULL
    })

    # ================================================================
    # TABLE: force redraw when the "Tableau" sub-tab becomes visible
    # ================================================================
    shiny::observe({
      root_session <- session$userData$root_session
      if (is.null(root_session)) return()

      top_nav <- root_session$input$main_nav
      sub_nav <- root_session$input[["home-main_tabs"]]

      if (is.null(top_nav) || top_nav != "selection") return()
      if (is.null(sub_nav) || sub_nav != "table_ug") return()

      projet <- shiny::isolate(rv$projet_ug)
      if (is.null(projet) || !has_ug_data(projet)) return()

      # Bump counter to invalidate ug_listing reactive
      rv$redraw_counter <- shiny::isolate(rv$redraw_counter) + 1L
    })

    # ================================================================
    # MAP: Click handler for tenement selection (same pattern as mod_map.R)
    # ================================================================
    shiny::observeEvent(input$ug_map_shape_click, {
      click <- input$ug_map_shape_click
      if (is.null(click) || is.null(click$id)) return()

      tenement_id <- click$id
      projet <- rv$projet_ug
      if (is.null(projet)) return()

      # Only handle clicks on tenements (not UG overlay)
      if (!tenement_id %in% projet$tenements$tenement_id) return()

      if (tenement_id %in% rv$selected_tenement_ids) {
        # Deselect: remove from selection overlay
        rv$selected_tenement_ids <- setdiff(rv$selected_tenement_ids, tenement_id)
        leaflet::leafletProxy(ns("ug_map")) |>
          leaflet::removeShape(paste0("sel_", tenement_id))
      } else {
        # Select: add to selection overlay
        rv$selected_tenement_ids <- c(rv$selected_tenement_ids, tenement_id)
        update_tenement_selection_style(tenement_id, selected = TRUE)
      }
    })

    # Helper: add/remove selection overlay for a single tenement
    update_tenement_selection_style <- function(tenement_id, selected) {
      projet <- rv$projet_ug
      if (is.null(projet)) return()

      if (selected) {
        tenements <- projet$tenements
        tenement <- tenements[tenements$tenement_id == tenement_id, ]
        if (nrow(tenement) == 0) return()

        # Ensure WGS84
        if (!is.na(sf::st_crs(tenement)) && sf::st_crs(tenement)$epsg != 4326L) {
          tenement <- sf::st_transform(tenement, 4326)
        }

        leaflet::leafletProxy(ns("ug_map")) |>
          leaflet::addPolygons(
            data = tenement,
            layerId = paste0("sel_", tenement_id),
            group = "Selection",
            color = "#FF4500",
            weight = 3,
            fillColor = "#FF6347",
            fillOpacity = 0.4,
            options = leaflet::pathOptions(interactive = FALSE)
          )
      } else {
        leaflet::leafletProxy(ns("ug_map")) |>
          leaflet::removeShape(paste0("sel_", tenement_id))
      }
    }

    # Clear all selection overlays (used by clear button and project load)
    clear_tenement_selection <- function() {
      leaflet::leafletProxy(ns("ug_map")) |>
        leaflet::clearGroup("Selection")
      rv$selected_tenement_ids <- character(0)
    }

    # ================================================================
    # ================================================================
    # MAP: Handle drawn shapes (interactive split)
    # ================================================================
    # When the user finishes drawing on the map:
    #   - LINESTRING → cut the SELECTED tenement along that line
    #   - POLYGON / RECTANGLE → split the chosen PARCEL using those polygons
    shiny::observeEvent(input$ug_map_draw_all_features, {
      drawn <- input$ug_map_draw_all_features
      if (is.null(drawn) || length(drawn$features) == 0) return()

      projet <- rv$projet_ug
      if (is.null(projet)) return()

      # Detect geometry types of drawn features
      geom_types <- vapply(drawn$features, function(f) {
        f$geometry$type %||% ""
      }, character(1))
      n_lines <- sum(geom_types %in% c("LineString", "MultiLineString"))
      n_polys <- sum(geom_types %in% c("Polygon", "MultiPolygon"))

      # Store the drawn GeoJSON for later use
      rv$drawn_geojson <- jsonlite::toJSON(drawn, auto_unbox = TRUE)

      # ----- Case 1: LINE drawn → cut a SELECTED tenement -----
      if (n_lines > 0 && n_polys == 0) {
        sel_ids <- rv$selected_tenement_ids
        if (length(sel_ids) != 1) {
          shiny::showNotification(
            i18n()$t("ug_line_split_select_one"),
            type = "warning",
            duration = 8
          )
          return()
        }

        target_id <- sel_ids[1]
        target_label <- projet$tenements$tenement_id[
          projet$tenements$tenement_id == target_id
        ][1]

        shiny::showModal(shiny::modalDialog(
          title = i18n()$t("ug_line_split_title"),
          shiny::p(sprintf(i18n()$t("ug_line_split_desc"), target_label)),
          footer = htmltools::tagList(
            shiny::modalButton(i18n()$t("cancel")),
            shiny::actionButton(
              ns("confirm_line_split"),
              i18n()$t("ug_split_apply"),
              class = "btn-info",
              icon = shiny::icon("scissors")
            )
          )
        ))
        return()
      }

      # ----- Case 2: POLYGON drawn → auto-split all crossed tenements -----
      tenements <- projet$tenements
      # Quick preview: how many tenements does the polygon cross?
      n_affected <- tryCatch({
        polys_sf <- sf::st_read(rv$drawn_geojson, quiet = TRUE)
        if (!is.na(sf::st_crs(polys_sf)) && !is.na(sf::st_crs(tenements))) {
          if (sf::st_crs(polys_sf) != sf::st_crs(tenements)) {
            polys_sf <- sf::st_transform(polys_sf, sf::st_crs(tenements))
          }
        }
        cutter <- sf::st_union(sf::st_geometry(polys_sf))
        sum(sf::st_intersects(sf::st_geometry(tenements), cutter, sparse = FALSE)[, 1])
      }, error = function(e) 0L)

      if (n_affected == 0) {
        shiny::showNotification(
          i18n()$t("ug_poly_split_no_hit"),
          type = "warning",
          duration = 8
        )
        return()
      }

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_poly_split_title"),
        shiny::p(sprintf(i18n()$t("ug_poly_split_desc"), n_affected)),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_draw_split"),
            i18n()$t("ug_split_apply"),
            class = "btn-info",
            icon = shiny::icon("scissors")
          )
        )
      ))
    })

    # Confirm POLYGON split (auto-split all tenements crossed by polygon)
    shiny::observeEvent(input$confirm_draw_split, {
      shiny::removeModal()

      geojson <- rv$drawn_geojson
      if (is.null(geojson)) return()

      tryCatch({
        projet <- rv$projet_ug
        projet <- tenement_split_by_drawn_polygon(projet, geojson)

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        rv$redraw_counter <- shiny::isolate(rv$redraw_counter) + 1L
        rv$needs_recompute <- TRUE
        rv$drawn_geojson <- NULL
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        # Clear drawn shapes from the map (both leaflet group + JS fallback)
        leaflet::leafletProxy(ns("ug_map")) |>
          leaflet::clearGroup("draw")
        session$sendCustomMessage("leafletClearDrawn", list(id = ns("ug_map")))

        shiny::showNotification(
          i18n()$t("ug_poly_split_success"),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("ug_split_error"), e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Confirm LINE split (cut the selected tenement along the drawn line)
    shiny::observeEvent(input$confirm_line_split, {
      shiny::removeModal()

      sel_ids <- rv$selected_tenement_ids
      geojson <- rv$drawn_geojson
      if (length(sel_ids) != 1 || is.null(geojson)) return()

      target_id <- sel_ids[1]

      tryCatch({
        projet <- rv$projet_ug
        projet <- tenement_split_by_line(projet, target_id, geojson)

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        rv$redraw_counter <- shiny::isolate(rv$redraw_counter) + 1L
        rv$needs_recompute <- TRUE
        rv$drawn_geojson <- NULL
        clear_tenement_selection()
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        # Clear drawn shapes from the map (both leaflet group + JS fallback)
        leaflet::leafletProxy(ns("ug_map")) |>
          leaflet::clearGroup("draw")
        session$sendCustomMessage("leafletClearDrawn", list(id = ns("ug_map")))

        shiny::showNotification(
          i18n()$t("ug_line_split_success"),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("ug_split_error"), e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # ================================================================
    # OUTPUT: Map selection info
    # ================================================================
    output$map_selection_info <- shiny::renderUI({
      n_sel <- length(rv$selected_tenement_ids)
      if (n_sel == 0) {
        return(shiny::p(
          class = "text-muted small",
          i18n()$t("ug_map_click_hint")
        ))
      }

      projet <- rv$projet_ug
      if (is.null(projet)) return(NULL)

      # Find which UGs are involved
      sel_tenements <- projet$tenements[projet$tenements$tenement_id %in% rv$selected_tenement_ids, ]
      ug_ids_involved <- unique(sel_tenements$ug_id)
      ug_labels <- projet$ugs$label[projet$ugs$ug_id %in% ug_ids_involved]

      total_surface <- sum(sel_tenements$surface_m2, na.rm = TRUE)

      htmltools::tagList(
        shiny::tags$span(
          class = "badge bg-warning",
          sprintf("%d tenement(s)", n_sel)
        ),
        shiny::br(),
        shiny::tags$small(
          class = "text-muted",
          sprintf("%s ha | UG: %s",
                  format(round(total_surface / 10000, 2), nsmall = 2),
                  paste(ug_labels, collapse = ", "))
        ),
        shiny::br(),
        shiny::actionButton(
          ns("btn_clear_map_sel"),
          label = i18n()$t("ug_clear_selection"),
          icon = shiny::icon("xmark"),
          class = "btn-outline-secondary btn-sm mt-1",
          width = "100%"
        )
      )
    })

    # Clear map selection
    # Clear map selection
    shiny::observeEvent(input$btn_clear_map_sel, {
      clear_tenement_selection()
    })

    # ================================================================
    # ACTION: Create UG from map-selected tenements
    # ================================================================
    shiny::observeEvent(input$btn_create_from_map, {
      sel_ids <- rv$selected_tenement_ids
      if (length(sel_ids) == 0) {
        shiny::showNotification(
          i18n()$t("ug_map_select_tenements_first"),
          type = "warning"
        )
        return()
      }

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_create_from_map"),
        shiny::textInput(
          ns("create_map_label"),
          label = i18n()$t("ug_label_prompt"),
          value = sprintf("UG-%03d", nrow(rv$projet_ug$ugs) + 1)
        ),
        shiny::selectInput(
          ns("create_map_groupe"),
          label = i18n()$t("ug_group"),
          choices = c("---" = "", stats::setNames(GROUPES_AMENAGEMENT, GROUPES_AMENAGEMENT)),
          selected = ""
        ),
        shiny::p(
          class = "text-muted",
          sprintf(i18n()$t("ug_create_confirm"), length(sel_ids))
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_create_from_map"),
            i18n()$t("ug_create_btn"),
            class = "btn-success"
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_create_from_map, {
      shiny::removeModal()

      sel_ids <- rv$selected_tenement_ids
      if (length(sel_ids) == 0) return()

      label <- trimws(input$create_map_label)
      groupe <- input$create_map_groupe
      if (nchar(label) == 0) {
        shiny::showNotification(i18n()$t("ug_label_required"), type = "warning")
        return()
      }
      groupe_val <- if (nchar(groupe) == 0) NA_character_ else groupe

      tryCatch({
        projet <- rv$projet_ug
        projet <- ug_create(projet, sel_ids, label, groupe_val)

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        rv$needs_recompute <- TRUE
        rv$selected_tenement_ids <- character(0)
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        shiny::showNotification(
          sprintf("UG \u00ab %s \u00bb cr\u00e9\u00e9e avec %d tenement(s)", label, length(sel_ids)),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(e$message, type = "error")
      })
    })

    # ================================================================
    # ACTION: Move selected tenements to an existing UG
    # ================================================================
    shiny::observeEvent(input$btn_move_to_ug, {
      sel_ids <- rv$selected_tenement_ids
      if (length(sel_ids) == 0) {
        shiny::showNotification(
          i18n()$t("ug_map_select_tenements_first"),
          type = "warning"
        )
        return()
      }

      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) return()

      # Build UG choices (exclude UGs that contain ALL the selected tenements)
      ugs <- projet$ugs
      ug_choices <- stats::setNames(ugs$ug_id, ugs$label)

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_move_to"),
        shiny::p(sprintf(i18n()$t("ug_move_desc"), length(sel_ids))),
        shiny::selectInput(
          ns("move_target_ug"),
          label = i18n()$t("ug_move_target"),
          choices = ug_choices
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_move_to_ug"),
            i18n()$t("ug_move_confirm"),
            class = "btn-success",
            icon = shiny::icon("arrow-right-arrow-left")
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_move_to_ug, {
      shiny::removeModal()

      sel_ids <- rv$selected_tenement_ids
      target_ug_id <- input$move_target_ug
      if (length(sel_ids) == 0 || is.null(target_ug_id)) return()

      tryCatch({
        projet <- rv$projet_ug

        # Move each tenement to the target UG
        for (tenement_id in sel_ids) {
          projet <- ug_assign_tenement(projet, tenement_id, target_ug_id)
        }

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        rv$needs_recompute <- TRUE
        clear_tenement_selection()
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        target_label <- projet$ugs$label[projet$ugs$ug_id == target_ug_id]
        shiny::showNotification(
          sprintf(i18n()$t("ug_move_success"), length(sel_ids), target_label),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(e$message, type = "error")
      })
    })

    # ================================================================
    # OUTPUT: UG table
    # ================================================================
    output$ug_table <- DT::renderDataTable({
      listing <- ug_listing()
      if (is.null(listing) || nrow(listing) == 0) {
        return(DT::datatable(
          data.frame(Message = i18n()$t("ug_no_data")),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }

      # Build display table
      display_df <- data.frame(
        Label = listing$label,
        Groupe = ifelse(is.na(listing$groupe), "---", listing$groupe),
        Tenements = listing$n_tenements,
        `Surface (ha)` = round(listing$surface_m2 / 10000, 2),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      # Get cadastral refs for each UG
      projet <- rv$projet_ug
      refs <- vapply(listing$ug_id, function(uid) {
        r <- ug_cadastral_refs(projet, uid)
        if (nrow(r) == 0) return("")
        paste(r$geo_parcelle, collapse = ", ")
      }, character(1))
      display_df[["Refs cadastrales"]] <- refs

      DT::datatable(
        display_df,
        selection = "multiple",
        rownames = FALSE,
        options = list(
          pageLength = 25,
          dom = "frtip",
          language = if (lang() == "fr") {
            list(
              search = "Rechercher :",
              info = "_TOTAL_ UG",
              lengthMenu = "Afficher _MENU_ UG"
            )
          } else {
            list()
          }
        )
      ) |>
        DT::formatStyle("Groupe",
          backgroundColor = DT::styleEqual(
            c("TSF", "REGT", "REGF", "IRR", "HSN", "HSY", "PROT", "ACC", "AMETS", "AMER"),
            c("#e8f5e9", "#fff3e0", "#fff3e0", "#e3f2fd", "#f3e5f5", "#fce4ec",
              "#e0f7fa", "#fff9c4", "#e8f5e9", "#fff3e0")
          )
        )
    })

    # ================================================================
    # OUTPUT: Summary text
    # ================================================================
    output$ug_summary <- shiny::renderText({
      listing <- ug_listing()
      if (is.null(listing)) return("")
      total_ha <- round(sum(listing$surface_m2, na.rm = TRUE) / 10000, 2)
      sprintf("%d UG | %s ha", nrow(listing), total_ha)
    })

    # ================================================================
    # OUTPUT: UG detail panel
    # ================================================================
    output$ug_detail <- shiny::renderUI({
      sel <- input$ug_table_rows_selected
      listing <- ug_listing()
      projet <- rv$projet_ug

      if (is.null(sel) || length(sel) == 0 || is.null(listing)) {
        return(shiny::p(
          class = "text-muted",
          i18n()$t("ug_select_hint")
        ))
      }

      if (length(sel) == 1) {
        uid <- listing$ug_id[sel]
        refs <- ug_cadastral_refs(projet, uid)
        surface_ha <- round(ug_surface(projet, uid) / 10000, 2)
        color <- ug_color(listing$groupe[sel], sel)

        htmltools::tagList(
          htmltools::div(
            style = sprintf("border-left: 4px solid %s; padding-left: 8px;", color),
            shiny::tags$strong(listing$label[sel]),
            shiny::br(),
            shiny::tags$span(
              class = "text-muted",
              sprintf("%s ha | %d tenement(s)", surface_ha, listing$n_tenements[sel])
            ),
            if (!is.na(listing$groupe[sel])) {
              shiny::tags$span(
                class = "badge ms-1",
                style = sprintf("background-color: %s;", color),
                listing$groupe[sel]
              )
            }
          ),
          shiny::hr(),
          if (nrow(refs) > 0) {
            shiny::tags$ul(
              class = "list-unstyled small",
              lapply(seq_len(nrow(refs)), function(i) {
                shiny::tags$li(
                  shiny::icon("map-pin", class = "text-muted me-1"),
                  sprintf(
                    "%s (sect. %s, n\u00b0%s) \u2014 %s m\u00b2",
                    refs$geo_parcelle[i],
                    refs$section[i],
                    refs$numero[i],
                    format(refs$surface_m2[i], big.mark = " ")
                  )
                )
              })
            )
          } else {
            shiny::p(class = "text-muted", "Aucune r\u00e9f\u00e9rence")
          }
        )
      } else {
        htmltools::tagList(
          shiny::p(sprintf("%d UG s\u00e9lectionn\u00e9es", length(sel))),
          shiny::p(
            class = "text-muted",
            sprintf(
              "Surface totale : %s ha",
              round(sum(listing$surface_m2[sel], na.rm = TRUE) / 10000, 2)
            )
          )
        )
      }
    })

    # ================================================================
    # ACTION: Merge UGs (from table selection)
    # ================================================================
    shiny::observeEvent(input$btn_merge, {
      sel <- input$ug_table_rows_selected
      listing <- ug_listing()

      if (is.null(sel) || length(sel) < 2 || is.null(listing)) {
        shiny::showNotification(
          i18n()$t("ug_select_at_least_2"),
          type = "warning"
        )
        return()
      }

      ug_ids_to_merge <- listing$ug_id[sel]

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_merge"),
        shiny::textInput(
          ns("merge_label"),
          label = i18n()$t("ug_label_prompt"),
          value = paste(listing$label[sel[1]], "+", length(sel) - 1)
        ),
        shiny::p(
          class = "text-muted",
          sprintf(i18n()$t("ug_confirm_merge"), length(sel))
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_merge"),
            i18n()$t("ug_merge"),
            class = "btn-success"
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_merge, {
      shiny::removeModal()

      sel <- input$ug_table_rows_selected
      listing <- ug_listing()
      if (is.null(sel) || length(sel) < 2 || is.null(listing)) return()

      ug_ids <- listing$ug_id[sel]
      label <- input$merge_label

      tryCatch({
        projet <- rv$projet_ug
        projet <- ug_merge(projet, ug_ids, label)

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        rv$needs_recompute <- TRUE
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        shiny::showNotification(
          sprintf("%s : %d UG \u2192 1", i18n()$t("ug_merge"), length(ug_ids)),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(e$message, type = "error")
      })
    })

    # ================================================================
    # ACTION: Split UG (restore 1 UG per tenement)
    # ================================================================
    shiny::observeEvent(input$btn_split, {
      sel <- input$ug_table_rows_selected
      listing <- ug_listing()

      if (is.null(sel) || length(sel) != 1 || is.null(listing)) {
        shiny::showNotification(i18n()$t("ug_select_one_to_split"), type = "warning")
        return()
      }

      uid <- listing$ug_id[sel]
      n_tenements <- listing$n_tenements[sel]

      if (n_tenements < 2) {
        shiny::showNotification(i18n()$t("ug_cannot_split_single"), type = "warning")
        return()
      }

      tryCatch({
        projet <- rv$projet_ug
        projet <- ug_split(projet, uid)

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        rv$needs_recompute <- TRUE
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        shiny::showNotification(
          sprintf("UG dissoci\u00e9e en %d UG", n_tenements),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(e$message, type = "error")
      })
    })

    # ================================================================
    # ACTION: Rename UG
    # ================================================================
    shiny::observeEvent(input$btn_rename, {
      sel <- input$ug_table_rows_selected
      listing <- ug_listing()

      if (is.null(sel) || length(sel) != 1 || is.null(listing)) {
        shiny::showNotification(i18n()$t("ug_select_one"), type = "warning")
        return()
      }

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_rename"),
        shiny::textInput(
          ns("rename_label"),
          label = i18n()$t("ug_label_prompt"),
          value = listing$label[sel]
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_rename"),
            i18n()$t("ug_rename"),
            class = "btn-primary"
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_rename, {
      shiny::removeModal()

      sel <- input$ug_table_rows_selected
      listing <- ug_listing()
      if (is.null(sel) || length(sel) != 1 || is.null(listing)) return()

      uid <- listing$ug_id[sel]
      new_label <- trimws(input$rename_label)

      if (nchar(new_label) == 0) {
        shiny::showNotification(i18n()$t("ug_label_required"), type = "warning")
        return()
      }

      projet <- rv$projet_ug
      projet$ugs$label[projet$ugs$ug_id == uid] <- new_label

      if (!is.null(projet$metadata$id)) {
        save_ug_data(projet$metadata$id, projet)
      }
      rv$projet_ug <- projet
      app_state$current_project$ugs <- projet$ugs

      shiny::showNotification(
        sprintf("UG renomm\u00e9e : %s", new_label),
        type = "message"
      )
    })

    # ================================================================
    # ACTION: Set groupe d'aménagement
    # ================================================================
    shiny::observeEvent(input$btn_set_groupe, {
      sel <- input$ug_table_rows_selected
      listing <- ug_listing()
      groupe <- input$sel_groupe

      if (is.null(sel) || length(sel) == 0 || is.null(listing)) {
        shiny::showNotification(i18n()$t("ug_select_one"), type = "warning")
        return()
      }

      projet <- rv$projet_ug
      for (idx in sel) {
        uid <- listing$ug_id[idx]
        groupe_val <- if (nchar(groupe) == 0) NA_character_ else groupe
        projet <- ug_set_groupe(projet, uid, groupe_val)
      }

      if (!is.null(projet$metadata$id)) {
        save_ug_data(projet$metadata$id, projet)
      }
      rv$projet_ug <- projet
      app_state$current_project$ugs <- projet$ugs

      shiny::showNotification(
        sprintf("Groupe mis \u00e0 jour pour %d UG", length(sel)),
        type = "message"
      )
    })

    # ================================================================
    # ACTION: Recompute indicators for UGs
    # ================================================================
    shiny::observeEvent(input$btn_recompute, {
      projet <- rv$projet_ug
      project <- app_state$current_project

      if (is.null(projet) || is.null(project$indicators)) {
        shiny::showNotification(i18n()$t("ug_no_indicators"), type = "warning")
        return()
      }

      tryCatch({
        shiny::showNotification(
          i18n()$t("ug_recomputing"),
          type = "message",
          id = "ug_recompute_notif"
        )

        indicators_ug <- aggregate_indicators_to_ug(project$indicators, projet)

        if (!is.null(indicators_ug)) {
          if (!is.null(project$metadata$id)) {
            save_indicators_ug(project$metadata$id, indicators_ug)
          }

          app_state$current_project$indicators_ug <- indicators_ug
          rv$needs_recompute <- FALSE

          shiny::showNotification(
            sprintf(i18n()$t("ug_recompute_done"), nrow(indicators_ug)),
            type = "message",
            id = "ug_recompute_notif"
          )
        } else {
          shiny::showNotification(
            i18n()$t("ug_recompute_failed"),
            type = "error"
          )
        }
      }, error = function(e) {
        shiny::showNotification(paste("Erreur :", e$message), type = "error")
      })
    })

    # ================================================================
    # ACTION: Import split (GeoJSON/Shapefile)
    # ================================================================
    shiny::observeEvent(input$btn_import_split, {
      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) {
        shiny::showNotification(i18n()$t("ug_no_data"), type = "warning")
        return()
      }

      # Build parcel choices
      parcels <- projet$parcels
      id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(parcels))
      if (length(id_col) == 0) return()
      parcel_ids <- as.character(parcels[[id_col[1]]])
      parcel_labels <- if ("geo_parcelle" %in% names(parcels)) {
        as.character(parcels$geo_parcelle)
      } else {
        parcel_ids
      }
      choices <- stats::setNames(parcel_ids, parcel_labels)

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_import_split"),
        size = "l",
        shiny::selectInput(
          ns("split_parcelle"),
          label = i18n()$t("ug_split_select_parcel"),
          choices = choices
        ),
        shiny::fileInput(
          ns("split_file"),
          label = i18n()$t("ug_split_file"),
          accept = c(".geojson", ".json", ".shp", ".gpkg"),
          placeholder = "GeoJSON / Shapefile / GeoPackage"
        ),
        shiny::p(
          class = "text-muted small",
          i18n()$t("ug_split_hint")
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_import_split"),
            i18n()$t("ug_split_apply"),
            class = "btn-info",
            icon = shiny::icon("scissors")
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_import_split, {
      file_info <- input$split_file
      parcelle_id <- input$split_parcelle

      if (is.null(file_info) || is.null(parcelle_id)) {
        shiny::showNotification(i18n()$t("ug_split_no_file"), type = "warning")
        return()
      }

      shiny::removeModal()

      tryCatch({
        # Read the imported file
        sf_polygones <- sf::st_read(file_info$datapath, quiet = TRUE)

        if (nrow(sf_polygones) == 0) {
          shiny::showNotification(i18n()$t("ug_split_empty_file"), type = "error")
          return()
        }

        projet <- rv$projet_ug
        projet <- tenement_split_by_import(projet, parcelle_id, sf_polygones)

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        rv$needs_recompute <- TRUE
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        n_tenements <- sum(projet$tenements$parent_parcelle_id == parcelle_id)
        shiny::showNotification(
          sprintf(i18n()$t("ug_split_success"), parcelle_id, n_tenements),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("ug_split_error"), e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # ================================================================
    # ACTION: Undo split (restore single tenement per parcel)
    # ================================================================
    shiny::observeEvent(input$btn_undo_split, {
      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) {
        shiny::showNotification(i18n()$t("ug_no_data"), type = "warning")
        return()
      }

      # Find parcels that have multiple tenements (i.e., have been split)
      atom_counts <- table(projet$tenements$parent_parcelle_id)
      split_parcels <- names(atom_counts[atom_counts > 1])

      if (length(split_parcels) == 0) {
        shiny::showNotification(i18n()$t("ug_no_split_to_undo"), type = "info")
        return()
      }

      # Build choices
      parcels <- projet$parcels
      id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(parcels))
      parcel_labels <- if ("geo_parcelle" %in% names(parcels)) {
        geo_refs <- as.character(parcels$geo_parcelle)
        ids <- as.character(parcels[[id_col[1]]])
        stats::setNames(ids, geo_refs)[split_parcels]
      } else {
        stats::setNames(split_parcels, split_parcels)
      }

      shiny::showModal(shiny::modalDialog(
        title = i18n()$t("ug_undo_split"),
        shiny::selectInput(
          ns("undo_split_parcelle"),
          label = i18n()$t("ug_split_select_parcel"),
          choices = parcel_labels
        ),
        shiny::p(
          class = "text-muted",
          i18n()$t("ug_undo_split_hint")
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n()$t("cancel")),
          shiny::actionButton(
            ns("confirm_undo_split"),
            i18n()$t("ug_undo_split"),
            class = "btn-warning"
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_undo_split, {
      shiny::removeModal()
      parcelle_id <- input$undo_split_parcelle
      if (is.null(parcelle_id)) return()

      tryCatch({
        projet <- rv$projet_ug
        projet <- tenement_undo_split(projet, parcelle_id)

        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        rv$needs_recompute <- TRUE
        app_state$current_project$tenements <- projet$tenements
        app_state$current_project$ugs <- projet$ugs

        shiny::showNotification(
          sprintf(i18n()$t("ug_undo_split_success"), parcelle_id),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(e$message, type = "error")
      })
    })

  })
}

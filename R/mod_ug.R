#' UG (Management Units) Module
#'
#' @description
#' Shiny module for managing Unités de Gestion (UG).
#' Boucle 1: table + leaflet map with atom selection, merge/split/rename,
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
  ns <- shiny::NS(id)

  opts <- get_app_options()
  lang <- opts$language
  i18n <- get_i18n(lang)

  bslib::layout_sidebar(
    fillable = TRUE,

    # === Sidebar: actions and UG details ===
    sidebar = bslib::sidebar(
      title = i18n$t("ug_title"),
      width = 350,

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
    ),

    # === Main content: map + table ===
    bslib::navset_card_tab(
      # Tab 1: Map
      bslib::nav_panel(
        title = i18n$t("ug_map_tab"),
        icon = bsicons::bs_icon("map"),
        bslib::card_body(
          class = "p-0",
          leaflet::leafletOutput(ns("ug_map"), height = "500px")
        )
      ),
      # Tab 2: Table
      bslib::nav_panel(
        title = i18n$t("ug_table_tab"),
        icon = bsicons::bs_icon("table"),
        bslib::card_body(
          htmltools::div(
            class = "d-flex justify-content-between align-items-center mb-2",
            shiny::h5(i18n$t("ug_title"), class = "mb-0"),
            shiny::textOutput(ns("ug_summary"), inline = TRUE)
          ),
          DT::dataTableOutput(ns("ug_table"))
        )
      )
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
    # REACTIVE: UG state (projet with atomes/ugs)
    # ================================================================
    rv <- shiny::reactiveValues(
      projet_ug = NULL,          # projet list with $atomes, $ugs, $parcels
      needs_recompute = FALSE,   # flag: UGs changed, indicators need refresh
      selected_atome_ids = character(0),  # atoms selected on the map
      map_needs_zoom = FALSE,    # flag: zoom to bounds on next map update
      pending_bbox = NULL        # stored bbox for deferred zoom
    )

    # Initialize UG data when project loads
    shiny::observe({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$parcels)) {
        rv$projet_ug <- NULL
        rv$selected_atome_ids <- character(0)
        rv$map_needs_zoom <- FALSE
        return()
      }

      # Reset map zoom flag for new project
      rv$map_needs_zoom <- TRUE
      rv$selected_atome_ids <- character(0)

      # Try to load existing UG data
      if (has_ug_data(project)) {
        rv$projet_ug <- project
      } else if (!is.null(project$metadata$id)) {
        projet <- ensure_project_migrated(project$metadata$id, project)
        rv$projet_ug <- projet
        app_state$current_project$atomes <- projet$atomes
        app_state$current_project$ugs <- projet$ugs
      }
    }) |> shiny::bindEvent(app_state$current_project)

    # ================================================================
    # REACTIVE: UG listing for the table
    # ================================================================
    ug_listing <- shiny::reactive({
      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) return(NULL)
      ug_list(projet)
    })

    # ================================================================
    # MAP: Render leaflet
    # ================================================================
    output$ug_map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$OpenStreetMap,
          group = "OSM"
        ) |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "Satellite"
        ) |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          overlayGroups = c("UG", "Atomes"),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::setView(lng = 2.5, lat = 46.5, zoom = 6)
    })

    # ================================================================
    # MAP: Update polygons when UG data changes
    # ================================================================
    shiny::observe({
      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) return()

      atomes <- projet$atomes
      ugs <- projet$ugs

      # Ensure WGS84 for leaflet
      if (!is.na(sf::st_crs(atomes)) && sf::st_crs(atomes)$epsg != 4326L) {
        atomes <- sf::st_transform(atomes, 4326)
      }

      # Compute fill colors per atom (based on UG groupe or index)
      ug_index_map <- stats::setNames(seq_len(nrow(ugs)), ugs$ug_id)
      fill_colors <- vapply(seq_len(nrow(atomes)), function(i) {
        uid <- atomes$ug_id[i]
        ug_row <- ugs[ugs$ug_id == uid, ]
        if (nrow(ug_row) == 0) return("#CCCCCC")
        idx <- ug_index_map[[uid]]
        ug_color(ug_row$groupe[1], idx)
      }, character(1))

      # Labels for hover
      atom_labels <- vapply(seq_len(nrow(atomes)), function(i) {
        uid <- atomes$ug_id[i]
        ug_row <- ugs[ugs$ug_id == uid, ]
        ug_label <- if (nrow(ug_row) > 0) ug_row$label[1] else "?"
        groupe_str <- if (nrow(ug_row) > 0 && !is.na(ug_row$groupe[1])) {
          paste0(" [", ug_row$groupe[1], "]")
        } else {
          ""
        }
        sprintf(
          "<b>%s</b>%s<br>Atome: %s<br>Surface: %s m\u00b2",
          ug_label, groupe_str,
          atomes$atome_id[i],
          format(round(atomes$surface_m2[i]), big.mark = " ")
        )
      }, character(1))

      # Highlight selected atoms
      border_colors <- ifelse(
        atomes$atome_id %in% rv$selected_atome_ids,
        "#FF4500",  # OrangeRed for selected
        "#333333"
      )
      border_weights <- ifelse(
        atomes$atome_id %in% rv$selected_atome_ids,
        3, 1
      )

      # Clear and redraw atoms
      proxy <- leaflet::leafletProxy(ns("ug_map"))
      proxy |>
        leaflet::clearGroup("Atomes") |>
        leaflet::clearGroup("UG") |>
        leaflet::addPolygons(
          data = atomes,
          group = "Atomes",
          layerId = atomes$atome_id,
          fillColor = fill_colors,
          fillOpacity = 0.5,
          color = border_colors,
          weight = border_weights,
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
              "<b>%s</b>%s<br>%d atome(s) | %s ha",
              ug_sf$label[i], groupe_str,
              ug_sf$n_atomes[i],
              format(round(ug_sf$surface_m2[i] / 10000, 2), nsmall = 2)
            )
          }, character(1))

          proxy |>
            leaflet::addPolygons(
              data = ug_sf,
              group = "UG",
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
        bbox <- sf::st_bbox(atomes)
        rv$pending_bbox <- bbox
        proxy |> leaflet::fitBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
        rv$map_needs_zoom <- FALSE
      }

      # Add legend
      proxy |>
        leaflet::clearControls() |>
        leaflet::addLegend(
          position = "bottomright",
          colors = GROUPE_COLORS[names(GROUPE_COLORS) %in% ugs$groupe],
          labels = names(GROUPE_COLORS)[names(GROUPE_COLORS) %in% ugs$groupe],
          title = i18n()$t("ug_group"),
          opacity = 0.8
        )
    })

    # ================================================================
    # MAP: Re-zoom when UG tab becomes visible
    # ================================================================
    # Leaflet fitBounds fails silently when the map container has 0 size
    # (hidden tab). This observer fires when the user navigates to the
    # UG tab and re-applies the pending zoom after a short delay.
    shiny::observe({
      root_session <- session$userData$root_session
      if (is.null(root_session)) return()

      nav <- root_session$input$main_nav
      if (is.null(nav) || nav != "ug") return()

      bbox <- shiny::isolate(rv$pending_bbox)
      if (is.null(bbox)) return()

      # Delay to let the tab fully render before zooming
      later::later(function() {
        leaflet::leafletProxy(ns("ug_map"), session = session) |>
          leaflet::fitBounds(
            lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
            lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
          )
      }, delay = 0.5)

      rv$pending_bbox <- NULL
    })

    # ================================================================
    # MAP: Click handler for atom selection
    # ================================================================
    shiny::observeEvent(input$ug_map_shape_click, {
      click <- input$ug_map_shape_click
      if (is.null(click) || is.null(click$id)) return()

      atome_id <- click$id
      projet <- rv$projet_ug
      if (is.null(projet)) return()

      # Only handle clicks on atoms
      if (!atome_id %in% projet$atomes$atome_id) return()

      # Toggle selection
      current <- rv$selected_atome_ids
      if (atome_id %in% current) {
        rv$selected_atome_ids <- setdiff(current, atome_id)
      } else {
        rv$selected_atome_ids <- c(current, atome_id)
      }
    })

    # ================================================================
    # OUTPUT: Map selection info
    # ================================================================
    output$map_selection_info <- shiny::renderUI({
      n_sel <- length(rv$selected_atome_ids)
      if (n_sel == 0) {
        return(shiny::p(
          class = "text-muted small",
          i18n()$t("ug_map_click_hint")
        ))
      }

      projet <- rv$projet_ug
      if (is.null(projet)) return(NULL)

      # Find which UGs are involved
      sel_atomes <- projet$atomes[projet$atomes$atome_id %in% rv$selected_atome_ids, ]
      ug_ids_involved <- unique(sel_atomes$ug_id)
      ug_labels <- projet$ugs$label[projet$ugs$ug_id %in% ug_ids_involved]

      total_surface <- sum(sel_atomes$surface_m2, na.rm = TRUE)

      htmltools::tagList(
        shiny::tags$span(
          class = "badge bg-warning",
          sprintf("%d atome(s)", n_sel)
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
    shiny::observeEvent(input$btn_clear_map_sel, {
      rv$selected_atome_ids <- character(0)
    })

    # ================================================================
    # ACTION: Create UG from map-selected atoms
    # ================================================================
    shiny::observeEvent(input$btn_create_from_map, {
      sel_ids <- rv$selected_atome_ids
      if (length(sel_ids) == 0) {
        shiny::showNotification(
          i18n()$t("ug_map_select_atoms_first"),
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

      sel_ids <- rv$selected_atome_ids
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
        rv$selected_atome_ids <- character(0)
        app_state$current_project$atomes <- projet$atomes
        app_state$current_project$ugs <- projet$ugs

        shiny::showNotification(
          sprintf("UG \u00ab %s \u00bb cr\u00e9\u00e9e avec %d atome(s)", label, length(sel_ids)),
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
        Atomes = listing$n_atomes,
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
              sprintf("%s ha | %d atome(s)", surface_ha, listing$n_atomes[sel])
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
        app_state$current_project$atomes <- projet$atomes
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
    # ACTION: Split UG (restore 1 UG per atom)
    # ================================================================
    shiny::observeEvent(input$btn_split, {
      sel <- input$ug_table_rows_selected
      listing <- ug_listing()

      if (is.null(sel) || length(sel) != 1 || is.null(listing)) {
        shiny::showNotification(i18n()$t("ug_select_one_to_split"), type = "warning")
        return()
      }

      uid <- listing$ug_id[sel]
      n_atomes <- listing$n_atomes[sel]

      if (n_atomes < 2) {
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
        app_state$current_project$atomes <- projet$atomes
        app_state$current_project$ugs <- projet$ugs

        shiny::showNotification(
          sprintf("UG dissoci\u00e9e en %d UG", n_atomes),
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

  })
}

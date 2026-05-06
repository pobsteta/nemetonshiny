#' Action Plan Module for nemetonApp (Lots 1-2 - skeleton + interactive map/table)
#'
#' @description
#' Renders the "Plan d'actions" tab. The current implementation covers:
#'
#' * S1: persistence + audit + validation (`R/service_action_plan.R`)
#' * S2: LLM extraction + parser (`R/llm_prompts.R`)
#' * S3: navbar entry + module skeleton + i18n
#' * S4: Leaflet map of UGFs colored by year / type / priority + popups
#' * S5: DT table with multi-select + inline edition for the editable
#'       columns (annee, type, intensite, priorite, statut, commentaire,
#'       quantites)
#' * S6: bidirectional synchronisation: clicking a UGF on the map filters
#'       the table to that UGF, selecting rows in the table highlights /
#'       zooms the corresponding UGFs on the map. Filters apply to both.
#'
#' Subsequent stories (S7 IA, S8 Kanban, S9 Gantt, S10/11 ponts terrain,
#' S12/13 exports, S15 permissions) will land in following lots.
#'
#' @name mod_action_plan
#' @keywords internal
NULL


# ---- UI --------------------------------------------------------------

#' Action Plan Module UI
#'
#' @param id Character. Module namespace ID.
#' @return Shiny UI elements.
#' @noRd
mod_action_plan_ui <- function(id) {
  ns <- shiny::NS(id)

  opts <- get_app_options()
  lang <- opts$language %||% "fr"
  i18n <- get_i18n(lang)

  bslib::layout_sidebar(
    fillable = TRUE,
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      width = 320,
      open = TRUE,
      shiny::tags$h6(i18n$t("action_plan_filters_title")),
      shiny::sliderInput(
        ns("filter_horizon"),
        label = i18n$t("action_plan_horizon"),
        min = 1L, max = 20L, value = c(1L, 20L), step = 1L
      ),
      shiny::selectizeInput(
        ns("filter_type"),
        label = i18n$t("action_plan_type"),
        choices = NULL, multiple = TRUE
      ),
      shiny::selectizeInput(
        ns("filter_statut"),
        label = i18n$t("action_plan_statut"),
        choices = NULL, multiple = TRUE
      ),
      shiny::selectizeInput(
        ns("filter_famille"),
        label = i18n$t("action_plan_famille"),
        choices = NULL, multiple = TRUE
      ),
      shiny::selectizeInput(
        ns("filter_ug"),
        label = i18n$t("action_plan_ug"),
        choices = NULL, multiple = TRUE
      ),
      shiny::actionButton(
        ns("reset_filters"),
        label = i18n$t("action_plan_reset_filters"),
        icon = shiny::icon("rotate-left"),
        class = "btn-sm btn-outline-secondary"
      )
    ),

    bslib::navset_card_underline(
      id = ns("inner_nav"),

      bslib::nav_panel(
        title = i18n$t("action_plan_view_map_table"),
        value = "map_table",
        icon = bsicons::bs_icon("map"),

        bslib::card(
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            i18n$t("action_plan_map_title"),
            shiny::radioButtons(
              ns("map_color_by"),
              label = NULL,
              choices = stats::setNames(
                c("annee", "type", "priorite"),
                c(i18n$t("action_plan_color_year"),
                  i18n$t("action_plan_color_type"),
                  i18n$t("action_plan_color_priority"))
              ),
              selected = "annee",
              inline = TRUE
            )
          ),
          bslib::card_body(
            min_height = "320px",
            leaflet::leafletOutput(ns("map"), height = "420px")
          )
        ),

        bslib::card(
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            i18n$t("action_plan_table_title"),
            htmltools::div(
              class = "d-flex gap-2 align-items-center",
              shiny::textOutput(ns("table_count_inline"), inline = TRUE),
              shiny::actionButton(
                ns("clear_map_selection"),
                label = i18n$t("action_plan_clear_selection"),
                icon = shiny::icon("eraser"),
                class = "btn-sm btn-outline-secondary"
              ),
              shiny::actionButton(
                ns("generate_all"),
                label = i18n$t("action_plan_generate_all"),
                icon = shiny::icon("wand-magic-sparkles"),
                class = "btn-sm btn-primary"
              ),
              shiny::actionButton(
                ns("add_action"),
                label = i18n$t("action_plan_add"),
                icon = shiny::icon("plus"),
                class = "btn-sm btn-outline-primary"
              )
            )
          ),
          bslib::card_body(
            DT::dataTableOutput(ns("action_table"))
          )
        )
      ),

      bslib::nav_panel(
        title = i18n$t("action_plan_view_gantt"),
        value = "gantt",
        icon = bsicons::bs_icon("calendar-range"),
        bslib::card(
          bslib::card_body(
            htmltools::div(class = "text-muted",
                           i18n$t("action_plan_gantt_placeholder"))
          )
        )
      )
    )
  )
}


# ---- Server ----------------------------------------------------------

#' Action Plan Module Server
#'
#' @param id Character. Module namespace ID.
#' @param app_state reactiveValues. Application state, must expose
#'   `current_project` and `language`.
#' @return Invisible NULL.
#' @noRd
mod_action_plan_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ============================================================
    # REACTIVES: plan, UGs, geometry
    # ============================================================

    plan_rv <- shiny::reactiveVal(NULL)

    shiny::observe({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$id)) {
        plan_rv(NULL)
        return()
      }
      plan <- tryCatch(
        load_action_plan(project$id),
        error = function(e) {
          cli::cli_warn("Failed to load action plan: {e$message}")
          init_empty_action_plan(project$id)
        }
      )
      plan_rv(plan)
    })

    ug_sf_4326 <- shiny::reactive({
      project <- app_state$current_project
      shiny::req(project)
      sf <- tryCatch(ug_build_sf(project), error = function(e) NULL)
      if (is.null(sf) || nrow(sf) == 0) return(NULL)
      if (!is.na(sf::st_crs(sf)) && sf::st_crs(sf)$epsg != 4326L) {
        sf <- tryCatch(sf::st_transform(sf, 4326), error = function(e) sf)
      }
      sf
    })

    ug_ids <- shiny::reactive({
      sf <- ug_sf_4326()
      if (is.null(sf)) return(character())
      as.character(sf$ug_id)
    })

    # Selected UGFs (clicks on map; sync'd with row selection downstream)
    selected_ug_rv <- shiny::reactiveVal(character())

    # ============================================================
    # SIDEBAR: choices + filter slider follow plan + project
    # ============================================================

    shiny::observe({
      shiny::updateSelectizeInput(session, "filter_type",
                                  choices = ACTION_PLAN_TYPES,
                                  server = FALSE)
      shiny::updateSelectizeInput(session, "filter_statut",
                                  choices = ACTION_PLAN_STATUTS,
                                  server = FALSE)
      shiny::updateSelectizeInput(session, "filter_famille",
                                  choices = ACTION_PLAN_FAMILY_CODES,
                                  server = FALSE)
      shiny::updateSelectizeInput(session, "filter_ug",
                                  choices = ug_ids(),
                                  server = FALSE)
      plan <- plan_rv()
      horizon <- plan$horizon_annees %||% 20L
      shiny::updateSliderInput(session, "filter_horizon",
                               min = 1L, max = as.integer(horizon),
                               value = c(1L, as.integer(horizon)))
    })

    shiny::observeEvent(input$reset_filters, {
      shiny::updateSelectizeInput(session, "filter_type",   selected = character())
      shiny::updateSelectizeInput(session, "filter_statut", selected = character())
      shiny::updateSelectizeInput(session, "filter_famille", selected = character())
      shiny::updateSelectizeInput(session, "filter_ug",     selected = character())
      plan <- plan_rv()
      horizon <- plan$horizon_annees %||% 20L
      shiny::updateSliderInput(session, "filter_horizon",
                               value = c(1L, as.integer(horizon)))
      selected_ug_rv(character())
    })

    shiny::observeEvent(input$clear_map_selection, {
      selected_ug_rv(character())
      DT::selectRows(DT::dataTableProxy("action_table"), NULL)
    })

    # ============================================================
    # FILTERED ACTIONS (used by map + table + Gantt)
    # ============================================================

    filtered_actions <- shiny::reactive({
      plan <- plan_rv()
      if (is.null(plan)) return(list())
      h <- input$filter_horizon
      ug_filter <- if (length(input$filter_ug) > 0) input$filter_ug else NULL
      # Map click acts as a soft additional filter when the user has
      # not explicitly set the UGF filter.
      if (is.null(ug_filter) && length(selected_ug_rv()) > 0) {
        ug_filter <- selected_ug_rv()
      }
      filter_actions(
        plan,
        ug_id     = ug_filter,
        type      = if (length(input$filter_type)    > 0) input$filter_type    else NULL,
        statut    = if (length(input$filter_statut)  > 0) input$filter_statut  else NULL,
        famille   = if (length(input$filter_famille) > 0) input$filter_famille else NULL,
        annee_min = if (!is.null(h)) as.integer(h[1]) else NULL,
        annee_max = if (!is.null(h)) as.integer(h[2]) else NULL
      )
    })

    actions_df <- shiny::reactive({
      plan <- plan_rv()
      if (is.null(plan)) return(actions_to_dataframe(NULL))
      tmp <- plan
      tmp$actions <- filtered_actions()
      actions_to_dataframe(tmp)
    })

    # Per-UGF aggregation for map coloring (always over filtered actions)
    ugf_summary <- shiny::reactive({
      df <- actions_df()
      ugs <- ug_ids()
      lapply(stats::setNames(ugs, ugs), function(uid) {
        sub <- df[df$ug_id == uid & !is.na(df$ug_id), , drop = FALSE]
        list(
          n      = nrow(sub),
          annees = sort(unique(stats::na.omit(sub$annee_cible))),
          types  = sort(unique(stats::na.omit(sub$type))),
          prios  = sort(unique(stats::na.omit(sub$priorite)))
        )
      })
    })

    # ============================================================
    # MAP: base render + reactive redraw
    # ============================================================

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$OpenStreetMap,
          group = "basemap"
        ) |>
        leaflet::addLayersControl(
          overlayGroups = c("UGF", "Selection"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::setView(lng = 2.5, lat = 46.5, zoom = 6)
    })

    # Color helpers --------------------------------------------------

    PALETTE_PRIO <- c(haute = "#d62728", moyenne = "#ff7f0e", basse = "#2ca02c")

    type_palette <- function(types) {
      if (length(types) == 0L) return(stats::setNames(character(0), character(0)))
      pal <- grDevices::hcl.colors(length(types), palette = "Set 2")
      stats::setNames(pal, types)
    }

    year_color <- function(year, ymin, ymax) {
      if (is.na(year) || is.na(ymin) || is.na(ymax) || ymax == ymin) {
        return("#bbbbbb")
      }
      pal <- leaflet::colorNumeric("YlOrRd", domain = c(ymin, ymax),
                                   reverse = FALSE)
      pal(year)
    }

    # Compute per-UGF color according to current toggle
    map_colors <- shiny::reactive({
      sf <- ug_sf_4326()
      shiny::req(sf)
      summary_list <- ugf_summary()
      mode <- input$map_color_by %||% "annee"

      if (mode == "annee") {
        firsts <- vapply(sf$ug_id, function(uid) {
          ann <- summary_list[[uid]]$annees
          if (length(ann) == 0L) return(NA_real_) else min(ann, na.rm = TRUE)
        }, numeric(1))
        ymin <- suppressWarnings(min(firsts, na.rm = TRUE))
        ymax <- suppressWarnings(max(firsts, na.rm = TRUE))
        list(
          colors = vapply(firsts, year_color, ymin = ymin, ymax = ymax,
                          FUN.VALUE = character(1)),
          legend_title = "year",
          legend_pal   = leaflet::colorNumeric("YlOrRd",
                                               domain = c(ymin, ymax)),
          legend_vals  = if (is.finite(ymin) && is.finite(ymax)) c(ymin, ymax) else numeric()
        )
      } else if (mode == "type") {
        firsts <- vapply(sf$ug_id, function(uid) {
          ts <- summary_list[[uid]]$types
          if (length(ts) == 0L) return(NA_character_) else ts[1]
        }, character(1))
        all_types <- sort(unique(stats::na.omit(firsts)))
        pal <- type_palette(all_types)
        col <- ifelse(is.na(firsts), "#bbbbbb", pal[firsts])
        list(
          colors = unname(col),
          legend_title = "type",
          legend_pal   = leaflet::colorFactor(palette = unname(pal),
                                              domain = all_types),
          legend_vals  = all_types
        )
      } else { # priorite
        firsts <- vapply(sf$ug_id, function(uid) {
          ps <- summary_list[[uid]]$prios
          # Highest priority among the ones present
          hierarchy <- c("haute", "moyenne", "basse")
          best <- intersect(hierarchy, ps)
          if (length(best) == 0L) return(NA_character_) else best[1]
        }, character(1))
        col <- ifelse(is.na(firsts), "#bbbbbb", PALETTE_PRIO[firsts])
        list(
          colors = unname(col),
          legend_title = "priority",
          legend_pal   = leaflet::colorFactor(
            palette = unname(PALETTE_PRIO[c("haute", "moyenne", "basse")]),
            domain = c("haute", "moyenne", "basse")),
          legend_vals  = c("haute", "moyenne", "basse")
        )
      }
    })

    # Redraw UGFs whenever colors / data change
    shiny::observe({
      sf <- ug_sf_4326()
      shiny::req(sf)
      cols <- map_colors()
      summary_list <- ugf_summary()

      proxy <- leaflet::leafletProxy(ns("map"))
      proxy |>
        leaflet::clearGroup("UGF") |>
        leaflet::clearControls()

      labels <- vapply(seq_len(nrow(sf)), function(i) {
        uid <- sf$ug_id[i]
        s <- summary_list[[uid]] %||% list(n = 0L, annees = integer(),
                                            types = character(), prios = character())
        sprintf(
          "<b>%s</b><br>%d action(s)<br>Annees: %s<br>Types: %s<br>Priorites: %s",
          uid, s$n,
          if (length(s$annees) == 0L) "-" else paste(s$annees, collapse = ", "),
          if (length(s$types)  == 0L) "-" else paste(s$types,  collapse = ", "),
          if (length(s$prios)  == 0L) "-" else paste(s$prios,  collapse = ", ")
        )
      }, character(1))

      proxy |>
        leaflet::addPolygons(
          data = sf,
          group = "UGF",
          layerId = sf$ug_id,
          fillColor = cols$colors,
          fillOpacity = 0.55,
          color = "#333333",
          weight = 1,
          label = lapply(labels, htmltools::HTML),
          highlightOptions = leaflet::highlightOptions(
            weight = 3, fillOpacity = 0.75, bringToFront = TRUE
          )
        )

      # Legend
      if (length(cols$legend_vals) > 0L) {
        proxy |>
          leaflet::addLegend(
            position = "bottomright",
            pal      = cols$legend_pal,
            values   = cols$legend_vals,
            title    = cols$legend_title,
            opacity  = 0.7
          )
      }

      # Re-bbox if any UG has actions, otherwise leave the view alone
      if (any(vapply(summary_list, function(s) s$n > 0L, logical(1)))) {
        bbox <- tryCatch(sf::st_bbox(sf), error = function(e) NULL)
        if (!is.null(bbox)) {
          proxy |> leaflet::fitBounds(
            lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
            lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
          )
        }
      }
    })

    # Map click on UGF -> select that UGF (toggle)
    shiny::observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      if (is.null(click$id)) return()
      uid <- as.character(click$id)
      cur <- selected_ug_rv()
      cur <- if (uid %in% cur) setdiff(cur, uid) else c(cur, uid)
      selected_ug_rv(cur)
    })

    # Selection overlay (highlight selected UGFs)
    shiny::observe({
      sf <- ug_sf_4326()
      shiny::req(sf)
      sel <- selected_ug_rv()
      proxy <- leaflet::leafletProxy(ns("map"))
      proxy |> leaflet::clearGroup("Selection")
      if (length(sel) == 0L) return()
      sub <- sf[sf$ug_id %in% sel, , drop = FALSE]
      if (nrow(sub) == 0L) return()
      proxy |>
        leaflet::addPolygons(
          data = sub,
          group = "Selection",
          layerId = paste0("sel_", sub$ug_id),
          color = "#FF8C00",
          weight = 4,
          fill = FALSE,
          opacity = 0.95
        )
    })

    # ============================================================
    # TABLE (DT) with multi-select + inline edition
    # ============================================================

    EDITABLE_COLS <- c(
      "annee_cible", "type", "type_libre", "intensite", "priorite",
      "statut", "objectifs_lies", "volume_m3", "surface_ha",
      "nb_tiges", "rdi", "cout_eur", "commentaire"
    )

    output$action_table <- DT::renderDataTable({
      df <- actions_df()
      i18n <- get_i18n(app_state$language)
      if (nrow(df) == 0L) {
        return(DT::datatable(
          data.frame(message = i18n$t("action_plan_table_empty")),
          options = list(dom = "t"), rownames = FALSE,
          selection = "none"
        ))
      }
      # Reorder for display, keep the id hidden
      display <- df[, c("id", "ug_id", "annee_cible", "type", "type_libre",
                        "intensite", "priorite", "statut", "objectifs_lies",
                        "volume_m3", "surface_ha", "nb_tiges", "rdi",
                        "cout_eur", "commentaire", "source_origine")]
      editable_idx <- which(names(display) %in% EDITABLE_COLS) - 1L  # 0-based
      DT::datatable(
        display,
        rownames = FALSE,
        selection = list(mode = "multiple", target = "row"),
        editable  = list(target = "cell",
                         disable = list(columns = setdiff(seq_len(ncol(display)) - 1L,
                                                          editable_idx))),
        options = list(
          pageLength = 15,
          dom = "frtip",
          columnDefs = list(list(visible = FALSE, targets = 0L)),
          language = if (identical(app_state$language, "en")) list() else list(
            search = "Rechercher :",
            info   = "_TOTAL_ action(s)",
            lengthMenu = "Afficher _MENU_"
          )
        )
      )
    }, server = FALSE)

    shiny::outputOptions(output, "action_table", suspendWhenHidden = FALSE)

    # Inline edits -> persist via update_action_in_plan
    shiny::observeEvent(input$action_table_cell_edit, {
      info <- input$action_table_cell_edit
      df <- actions_df()
      if (nrow(df) == 0L) return()
      row_idx <- info$row
      col_idx <- info$col + 1L  # DT is 0-based
      # Display ordering uses the same order as in renderDataTable above:
      display_cols <- c("id", "ug_id", "annee_cible", "type", "type_libre",
                        "intensite", "priorite", "statut", "objectifs_lies",
                        "volume_m3", "surface_ha", "nb_tiges", "rdi",
                        "cout_eur", "commentaire", "source_origine")
      if (col_idx < 1L || col_idx > length(display_cols)) return()
      field <- display_cols[col_idx]
      action_id <- df$id[row_idx]
      if (is.na(action_id)) return()
      idx <- find_action_index(plan_rv(), action_id)
      if (is.na(idx)) return()
      raw_value <- info$value
      coerced <- coerce_table_value(field, raw_value)
      updates <- if (field %in% c("volume_m3", "surface_ha", "nb_tiges",
                                  "rdi", "cout_eur")) {
        existing_q <- plan_rv()$actions[[idx]]$quantite %||% list()
        list(quantite = utils::modifyList(
          existing_q,
          stats::setNames(list(coerced), field)
        ))
      } else if (field == "objectifs_lies") {
        list(objectifs_lies = strsplit(as.character(raw_value),
                                       "\\s*,\\s*")[[1]])
      } else {
        stats::setNames(list(coerced), field)
      }
      tryCatch({
        new_plan <- update_action_in_plan(
          plan_rv(), action_id, updates,
          ug_ids = ug_ids(),
          user = Sys.info()[["user"]] %||% "user"
        )
        save_action_plan(app_state$current_project$id, new_plan)
        plan_rv(new_plan)
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
      })
    })

    # Sync table row selection -> selected UGFs (for the map highlight).
    # Using `selectRows` style updates instead of fully resetting filters,
    # so the user can mix row selection + filters freely.
    shiny::observe({
      sel <- input$action_table_rows_selected
      df <- actions_df()
      if (length(sel) == 0L || nrow(df) == 0L) return()
      ugs <- unique(df$ug_id[sel])
      ugs <- ugs[!is.na(ugs)]
      # Merge with existing map clicks: row selection IS the source of
      # truth when present.
      selected_ug_rv(ugs)
    })

    # ============================================================
    # OUTPUTS: small text helpers
    # ============================================================

    output$table_count_inline <- shiny::renderText({
      i18n <- get_i18n(app_state$language)
      n <- length(filtered_actions())
      sprintf(i18n$t("action_plan_table_count_fmt"), n)
    })

    # Skeleton handlers for IA / add buttons (real impl in S5/S7)
    shiny::observeEvent(input$generate_all, {
      i18n <- get_i18n(app_state$language)
      shiny::showNotification(i18n$t("action_plan_generate_pending"),
                              type = "message", duration = 4)
    })
    shiny::observeEvent(input$add_action, {
      i18n <- get_i18n(app_state$language)
      shiny::showNotification(i18n$t("action_plan_add_pending"),
                              type = "message", duration = 4)
    })

    invisible(NULL)
  })
}


# ---- Helpers ----------------------------------------------------------

#' Coerce a raw cell value coming back from DT::edit to the right R type
#' @noRd
coerce_table_value <- function(field, raw) {
  if (is.null(raw) || (is.character(raw) && !nzchar(raw))) {
    return(NA)
  }
  if (field %in% c("annee_cible", "duree", "nb_tiges")) {
    return(suppressWarnings(as.integer(raw)))
  }
  if (field %in% c("volume_m3", "surface_ha", "rdi", "cout_eur")) {
    return(suppressWarnings(as.numeric(raw)))
  }
  as.character(raw)
}

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

  # Toolbar shared by the Map+Table view (placed in the table card header).
  table_toolbar <- htmltools::div(
    class = "d-flex gap-2 align-items-center flex-wrap",
    shiny::textOutput(ns("table_count_inline"), inline = TRUE),
    shiny::actionButton(
      ns("clear_map_selection"),
      label = i18n$t("action_plan_clear_selection"),
      icon = shiny::icon("eraser"),
      class = "btn-sm btn-outline-secondary"
    ),
    shiny::actionButton(
      ns("show_history"),
      label = i18n$t("action_plan_show_history"),
      icon = shiny::icon("clock-rotate-left"),
      class = "btn-sm btn-outline-secondary"
    ),
    shiny::selectInput(
      ns("bulk_status"), label = NULL,
      choices = stats::setNames(
        c("",
          "validee", "planifiee", "realisee",
          "abandonnee", "proposee"),
        c(i18n$t("action_plan_bulk_status_placeholder"),
          i18n$t("action_plan_status_validee"),
          i18n$t("action_plan_status_planifiee"),
          i18n$t("action_plan_status_realisee"),
          i18n$t("action_plan_status_abandonnee"),
          i18n$t("action_plan_status_proposee"))
      ),
      width = "180px",
      selected = ""
    ),
    shiny::actionButton(
      ns("apply_bulk_status"),
      label = i18n$t("action_plan_bulk_status_apply"),
      icon = shiny::icon("forward"),
      class = "btn-sm btn-outline-primary"
    ),
    shiny::actionButton(
      ns("open_chat"),
      label = i18n$t("action_plan_open_chat"),
      icon = shiny::icon("comments"),
      class = "btn-sm btn-outline-primary"
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

  bslib::navset_card_underline(
    id = ns("inner_nav"),
    full_screen = TRUE,

    bslib::nav_panel(
      title = i18n$t("action_plan_view_map_table"),
      value = "map_table",
      icon = bsicons::bs_icon("map"),

      # Carte (gauche) + tableau (droite), 50/50 horizontal sur toute la
      # hauteur disponible.
      bslib::layout_columns(
        col_widths = c(6, 6),
        fillable = TRUE,

        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
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
            class = "p-0",
            leaflet::leafletOutput(ns("map"), height = "100%")
          )
        ),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
            i18n$t("action_plan_table_title"),
            table_toolbar
          ),
          bslib::card_body(
            DT::dataTableOutput(ns("action_table"))
          )
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

    # Module-local state (e.g. last-fitted bbox signature, project id).
    rv_state <- shiny::reactiveValues(last_bbox_sig = NULL,
                                      last_project_id = NULL,
                                      pending_chat_actions = NULL)

    shiny::observe({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$id)) {
        plan_rv(NULL)
        rv_state$last_bbox_sig <- NULL
        rv_state$last_project_id <- NULL
        return()
      }
      if (!identical(rv_state$last_project_id, project$id)) {
        rv_state$last_bbox_sig <- NULL
        rv_state$last_project_id <- project$id
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

    shiny::observeEvent(input$clear_map_selection, {
      selected_ug_rv(character())
      DT::selectRows(DT::dataTableProxy("action_table"), NULL)
    })

    # ============================================================
    # ACTIONS (full plan + visible-rows view)
    # ============================================================

    # Full plan as a data.frame, augmented with derived display columns:
    # - `ug_label`        : human-readable UGF name (joined from project$ugs)
    # - `annee_realisation`: calendar year (current_year + annee_cible)
    actions_df_all <- shiny::reactive({
      plan <- plan_rv()
      df <- actions_to_dataframe(plan)
      sf <- ug_sf_4326()
      if (!is.null(sf) && nrow(df) > 0L) {
        label_map <- stats::setNames(as.character(sf$label),
                                     as.character(sf$ug_id))
        df$ug_label <- ifelse(is.na(df$ug_id), NA_character_,
                              label_map[df$ug_id])
      } else {
        df$ug_label <- NA_character_
      }
      base_year <- as.integer(format(Sys.Date(), "%Y"))
      df$annee_realisation <- ifelse(is.na(df$annee_cible),
                                     NA_integer_,
                                     base_year + as.integer(df$annee_cible))
      df
    })

    # Indices of rows currently visible in the DT (after column filters /
    # global search). Defaults to all rows when DT hasn't reported yet.
    visible_indices <- shiny::reactive({
      idx <- input$action_table_rows_all
      n <- nrow(actions_df_all())
      if (is.null(idx)) seq_len(n) else as.integer(idx)
    })

    # Visible actions as a data.frame, used by map colorization + Gantt.
    actions_df <- shiny::reactive({
      df <- actions_df_all()
      if (nrow(df) == 0L) return(df)
      df[visible_indices(), , drop = FALSE]
    })

    # Per-UGF aggregation for map coloring (over the *visible* actions)
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

    # Compute per-UGF color according to current toggle. When the plan is
    # empty for the current filters, every UGF is grey and no legend is
    # rendered (leaflet::colorNumeric / colorFactor refuse empty domains).
    map_colors <- shiny::reactive({
      sf <- ug_sf_4326()
      shiny::req(sf)
      summary_list <- ugf_summary()
      mode <- input$map_color_by %||% "annee"
      n_ug <- nrow(sf)

      empty_result <- list(
        colors = rep("#bbbbbb", n_ug),
        legend_title = NULL,
        legend_pal   = NULL,
        legend_vals  = NULL
      )

      if (mode == "annee") {
        firsts <- vapply(sf$ug_id, function(uid) {
          ann <- summary_list[[uid]]$annees
          if (length(ann) == 0L) return(NA_real_) else min(ann, na.rm = TRUE)
        }, numeric(1))
        ymin <- suppressWarnings(min(firsts, na.rm = TRUE))
        ymax <- suppressWarnings(max(firsts, na.rm = TRUE))
        if (!is.finite(ymin) || !is.finite(ymax)) return(empty_result)
        if (ymin == ymax) {
          # Single distinct year: avoid degenerate colorNumeric domain.
          colors <- ifelse(is.na(firsts), "#bbbbbb", "#fc8d59")
          return(list(
            colors = colors,
            legend_title = "year",
            legend_pal   = leaflet::colorFactor(
              palette = "#fc8d59", domain = as.character(ymin)),
            legend_vals  = as.character(ymin)
          ))
        }
        list(
          colors = vapply(firsts, year_color, ymin = ymin, ymax = ymax,
                          FUN.VALUE = character(1)),
          legend_title = "year",
          legend_pal   = leaflet::colorNumeric("YlOrRd",
                                               domain = c(ymin, ymax)),
          legend_vals  = c(ymin, ymax)
        )
      } else if (mode == "type") {
        firsts <- vapply(sf$ug_id, function(uid) {
          ts <- summary_list[[uid]]$types
          if (length(ts) == 0L) return(NA_character_) else ts[1]
        }, character(1))
        all_types <- sort(unique(stats::na.omit(firsts)))
        if (length(all_types) == 0L) return(empty_result)
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
        if (all(is.na(firsts))) return(empty_result)
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

      # Fit the view to the project bbox whenever the *bbox value* changes
      # (different project, or first render after leaflet remount).
      # We track a signature of the last-fitted bbox so the user's manual
      # pan/zoom on the same project is not overridden on every redraw.
      bbox <- tryCatch(sf::st_bbox(sf), error = function(e) NULL)
      if (!is.null(bbox)) {
        sig <- paste(round(bbox, 4), collapse = "_")
        if (!identical(rv_state$last_bbox_sig, sig)) {
          proxy |> leaflet::fitBounds(
            lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
            lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
          )
          rv_state$last_bbox_sig <- sig
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
    # TABLE (DT) with multi-select + inline edition + column filters
    # ============================================================

    # Display columns (in order). `id` and `ug_id` stay hidden but are
    # required to identify rows / drive map sync. `ug_label` is what the
    # user sees instead of the raw ug_id. `annee_realisation` is the
    # calendar year derived from the offset (`annee_cible`); the offset
    # column is hidden but kept for backward compatibility.
    DISPLAY_COLS <- c(
      "id", "ug_id", "annee_cible",
      "ug_label", "annee_realisation",
      "type", "type_libre",
      "intensite", "priorite", "statut", "objectifs_lies",
      "volume_m3", "surface_ha", "nb_tiges", "rdi",
      "cout_eur", "revenu_eur", "bilan_eur",
      "commentaire", "source_origine"
    )
    # Cells the user can edit. `annee_realisation` accepts a calendar
    # year and is converted back to an offset before persistence.
    EDITABLE_COLS <- c(
      "annee_realisation", "type", "type_libre", "intensite", "priorite",
      "statut", "objectifs_lies", "volume_m3", "surface_ha",
      "nb_tiges", "rdi", "cout_eur", "revenu_eur", "commentaire"
    )

    PLAN_BASE_YEAR <- function() as.integer(format(Sys.Date(), "%Y"))

    output$action_table <- DT::renderDataTable({
      df <- actions_df_all()
      i18n <- get_i18n(app_state$language)
      if (nrow(df) == 0L) {
        return(DT::datatable(
          data.frame(message = i18n$t("action_plan_table_empty")),
          options = list(dom = "t"), rownames = FALSE,
          selection = "none"
        ))
      }

      display <- df[, DISPLAY_COLS, drop = FALSE]

      # Promote categorical columns to factors so DT renders dropdown
      # filters above each column (filter = "top" auto-detects type).
      for (cc in c("ug_label", "type", "priorite", "statut")) {
        if (cc %in% names(display)) {
          v <- display[[cc]]
          v[is.na(v)] <- ""
          lev <- sort(unique(c(
            switch(cc,
                   type     = ACTION_PLAN_TYPES,
                   priorite = ACTION_PLAN_PRIORITES,
                   statut   = ACTION_PLAN_STATUTS,
                   character()),
            v
          )))
          display[[cc]] <- factor(v, levels = lev)
        }
      }

      # Localised column headers, in the same order as DISPLAY_COLS.
      colname_map <- c(
        i18n$t("action_plan_col_id"),
        i18n$t("action_plan_col_ug_id"),
        i18n$t("action_plan_col_annee_offset"),
        i18n$t("action_plan_col_ug_label"),
        i18n$t("action_plan_col_annee"),
        i18n$t("action_plan_col_type"),
        i18n$t("action_plan_col_type_libre"),
        i18n$t("action_plan_col_intensite"),
        i18n$t("action_plan_col_priorite"),
        i18n$t("action_plan_col_statut"),
        i18n$t("action_plan_col_objectifs"),
        i18n$t("action_plan_col_volume"),
        i18n$t("action_plan_col_surface"),
        i18n$t("action_plan_col_tiges"),
        i18n$t("action_plan_col_rdi"),
        i18n$t("action_plan_col_cout"),
        i18n$t("action_plan_col_revenu"),
        i18n$t("action_plan_col_bilan"),
        i18n$t("action_plan_col_commentaire"),
        i18n$t("action_plan_col_source")
      )

      # Hidden columns: id (0), ug_id (1), annee_cible offset (2).
      hidden_targets <- as.integer(c(0L, 1L, 2L))
      editable_idx <- which(DISPLAY_COLS %in% EDITABLE_COLS) - 1L

      DT::datatable(
        display,
        colnames = colname_map,
        rownames = FALSE,
        filter = "top",
        selection = list(mode = "multiple", target = "row"),
        editable  = list(target = "cell",
                         disable = list(columns = setdiff(seq_len(ncol(display)) - 1L,
                                                          editable_idx))),
        extensions = c("FixedHeader"),
        options = list(
          pageLength = 25,
          dom = "frtip",
          fixedHeader = TRUE,
          scrollX = TRUE,
          columnDefs = list(
            list(visible = FALSE, targets = hidden_targets)
          ),
          language = if (identical(app_state$language, "en")) list() else list(
            search = "Rechercher :",
            info   = "_TOTAL_ action(s)",
            lengthMenu = "Afficher _MENU_",
            paginate = list(previous = "Pr\u00e9c.", `next` = "Suiv.")
          )
        )
      ) |>
        # Color the Bilan column: green when >= 0, red when negative.
        DT::formatStyle(
          "bilan_eur",
          color = DT::styleInterval(c(-0.001, 0), c("#b91c1c", "#374151", "#15803d")),
          fontWeight = "bold"
        )
    }, server = FALSE)

    shiny::outputOptions(output, "action_table", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)

    # Inline edits -> persist via update_action_in_plan. The DT row index
    # corresponds to the *full* data.frame (actions_df_all) since DT is
    # told about all rows; column filtering is purely client-side.
    shiny::observeEvent(input$action_table_cell_edit, {
      info <- input$action_table_cell_edit
      df <- actions_df_all()
      if (nrow(df) == 0L) return()
      row_idx <- info$row
      col_idx <- info$col + 1L  # DT is 0-based
      if (col_idx < 1L || col_idx > length(DISPLAY_COLS)) return()
      field_disp <- DISPLAY_COLS[col_idx]
      # ug_label is shown but not editable (it follows ug_id), drop it
      # defensively here too.
      if (!(field_disp %in% EDITABLE_COLS)) return()
      action_id <- df$id[row_idx]
      if (is.na(action_id)) return()
      idx <- find_action_index(plan_rv(), action_id)
      if (is.na(idx)) return()
      raw_value <- info$value
      coerced <- coerce_table_value(field_disp, raw_value)
      updates <- if (field_disp %in% c("volume_m3", "surface_ha", "nb_tiges",
                                       "rdi", "cout_eur", "revenu_eur")) {
        existing_q <- plan_rv()$actions[[idx]]$quantite %||% list()
        list(quantite = utils::modifyList(
          existing_q,
          stats::setNames(list(coerced), field_disp)
        ))
      } else if (field_disp == "objectifs_lies") {
        list(objectifs_lies = strsplit(as.character(raw_value),
                                       "\\s*,\\s*")[[1]])
      } else if (field_disp == "annee_realisation") {
        # User typed a calendar year; convert it back to an offset.
        year <- suppressWarnings(as.integer(raw_value))
        if (is.na(year)) {
          i18n2 <- get_i18n(app_state$language)
          shiny::showNotification(i18n2$t("action_plan_invalid_year"),
                                  type = "error", duration = 4)
          return()
        }
        offset <- year - PLAN_BASE_YEAR()
        list(annee_cible = as.integer(offset))
      } else {
        stats::setNames(list(coerced), field_disp)
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
    shiny::observe({
      sel <- input$action_table_rows_selected
      df <- actions_df_all()
      if (length(sel) == 0L || nrow(df) == 0L) return()
      ugs <- unique(df$ug_id[sel])
      ugs <- ugs[!is.na(ugs)]
      selected_ug_rv(ugs)
    })

    # ============================================================
    # OUTPUTS: small text helpers
    # ============================================================

    output$table_count_inline <- shiny::renderText({
      i18n <- get_i18n(app_state$language)
      n <- nrow(actions_df())
      sprintf(i18n$t("action_plan_table_count_fmt"), n)
    })

    # ============================================================
    # S7 - IA generation (all UGFs / current selection)
    # ============================================================

    # Build LLM context from project comments + current ug_ids.
    plan_llm_context <- shiny::reactive({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$id)) return(NULL)
      cmt <- tryCatch(load_comments(project$id), error = function(e) NULL)
      list(
        comments = list(
          synthesis = as.character(cmt$synthesis %||% ""),
          families  = as.list(cmt$families %||% list())
        ),
        ug_ids = ug_ids(),
        horizon = (plan_rv()$horizon_annees %||% 20L),
        language = if (identical(app_state$language, "fr")) "fran\u00e7ais" else "English"
      )
    })

    # The IA generation flow opens a modal so the user picks the scope and
    # confirms before any write happens.
    shiny::observeEvent(input$generate_all, {
      i18n <- get_i18n(app_state$language)
      project <- app_state$current_project
      if (is.null(project)) {
        shiny::showNotification(i18n$t("no_project"), type = "warning")
        return()
      }
      ctx <- plan_llm_context()
      if (is.null(ctx) || length(ctx$ug_ids) == 0L) {
        shiny::showNotification(i18n$t("action_plan_no_ug"),
                                type = "warning")
        return()
      }
      sel_ugs <- selected_ug_rv()
      shiny::showModal(shiny::modalDialog(
        title = i18n$t("action_plan_generate_title"),
        easyClose = TRUE,
        size = "m",
        shiny::p(i18n$t("action_plan_generate_help")),
        shiny::radioButtons(
          ns("gen_scope"), label = i18n$t("action_plan_generate_scope"),
          choices = stats::setNames(
            c("all", "selected"),
            c(i18n$t("action_plan_generate_scope_all"),
              if (length(sel_ugs) > 0L)
                sprintf(i18n$t("action_plan_generate_scope_sel_fmt"),
                        length(sel_ugs))
              else i18n$t("action_plan_generate_scope_sel_none"))
          ),
          selected = if (length(sel_ugs) > 0L) "selected" else "all"
        ),
        shiny::checkboxInput(
          ns("gen_overwrite"), label = i18n$t("action_plan_generate_overwrite"),
          value = FALSE
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n$t("cancel")),
          shiny::actionButton(ns("gen_run"),
                              label = i18n$t("action_plan_generate_run"),
                              icon = shiny::icon("play"),
                              class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$gen_run, {
      shiny::removeModal()
      i18n <- get_i18n(app_state$language)
      project <- app_state$current_project
      ctx <- plan_llm_context()
      if (is.null(ctx)) return()
      sel_ugs <- selected_ug_rv()
      scope <- input$gen_scope %||% "all"
      target_ugs <- if (scope == "selected" && length(sel_ugs) > 0L) {
        sel_ugs
      } else {
        ctx$ug_ids
      }

      # Pre-flight: warn (but don't block) if no comments are available.
      no_text <- !nzchar(trimws(ctx$comments$synthesis %||% "")) &&
                 (length(ctx$comments$families) == 0L ||
                  all(!vapply(ctx$comments$families,
                              function(x) nzchar(trimws(x %||% "")),
                              logical(1))))
      if (no_text) {
        shiny::showNotification(i18n$t("action_plan_generate_no_comments"),
                                type = "warning", duration = 8)
        return()
      }

      provider <- get_app_config("llm_provider", "anthropic")
      key_var <- get_llm_api_key_var(provider)
      if (!is.null(key_var) && nchar(Sys.getenv(key_var)) == 0L) {
        msg <- gsub("\\{key_var\\}", key_var, i18n$t("ai_no_api_key"))
        shiny::showNotification(msg, type = "warning", duration = 8)
        return()
      }

      notif <- shiny::showNotification(
        htmltools::div(shiny::icon("spinner", class = "fa-spin me-2"),
                       i18n$t("action_plan_generating")),
        type = "message", duration = NULL
      )

      # Build prompt: generate per UGF when targeting many, in a single
      # call when scope = selected (one UGF) or to keep it simple here we
      # stay one-shot "all UGFs from this list".
      lang_param <- if (identical(app_state$language, "fr")) "fran\u00e7ais" else "English"
      prompt <- build_action_plan_prompt(
        comments       = ctx$comments,
        ug_ids         = target_ugs,
        horizon_annees = ctx$horizon,
        language       = lang_param,
        scope          = if (length(target_ugs) == 1L) "by_ug" else "all",
        ug_id          = if (length(target_ugs) == 1L) target_ugs else NULL
      )
      system_prompt <- build_system_prompt(lang_param, expert = "planificateur")

      response <- tryCatch({
        chat <- create_llm_chat(system_prompt)
        as.character(chat$chat(prompt, echo = FALSE))
      }, error = function(e) {
        shiny::removeNotification(notif)
        shiny::showNotification(
          paste(i18n$t("ai_error"), ":", strip_ansi(conditionMessage(e))),
          type = "error", duration = 10
        )
        NULL
      })
      shiny::removeNotification(notif)
      if (is.null(response)) return()

      parsed <- parse_action_plan_response(
        response,
        ug_ids = target_ugs,
        horizon_annees = ctx$horizon
      )
      if (length(parsed$actions) == 0L) {
        shiny::showNotification(
          paste(i18n$t("action_plan_generate_no_actions"),
                if (length(parsed$errors) > 0L) {
                  paste0(" (", parsed$errors[1], ")")
                } else ""),
          type = "warning", duration = 8
        )
        return()
      }

      cur_plan <- plan_rv()
      if (isTRUE(input$gen_overwrite)) {
        # Drop existing actions for the targeted UGFs.
        cur_plan$actions <- Filter(
          function(a) !(a$ug_id %in% target_ugs), cur_plan$actions
        )
      }
      new_plan <- tryCatch({
        bulk_upsert_actions(cur_plan, parsed$actions,
                            ug_ids = ctx$ug_ids,
                            user = "llm:planificateur")
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error",
                                duration = 8)
        NULL
      })
      if (is.null(new_plan)) return()

      save_action_plan(project$id, new_plan)
      plan_rv(new_plan)

      shiny::showNotification(
        sprintf(i18n$t("action_plan_generate_ok_fmt"),
                length(parsed$actions),
                length(parsed$errors)),
        type = "message", duration = 6
      )
    })

    # ============================================================
    # S7 - Q/R chat for plan refinement
    # ============================================================

    chat_history_rv <- shiny::reactiveVal(list())

    shiny::observeEvent(input$open_chat, {
      i18n <- get_i18n(app_state$language)
      shiny::showModal(shiny::modalDialog(
        title = i18n$t("action_plan_chat_title"),
        size = "l",
        easyClose = TRUE,
        shiny::uiOutput(ns("chat_history_ui")),
        shiny::textAreaInput(
          ns("chat_input"),
          label = i18n$t("action_plan_chat_input_label"),
          placeholder = i18n$t("action_plan_chat_placeholder"),
          rows = 3, width = "100%"
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n$t("close")),
          shiny::actionButton(
            ns("chat_clear"),
            label = i18n$t("action_plan_chat_clear"),
            icon = shiny::icon("broom"),
            class = "btn-outline-secondary"
          ),
          shiny::actionButton(
            ns("chat_send"),
            label = i18n$t("action_plan_chat_send"),
            icon = shiny::icon("paper-plane"),
            class = "btn-primary"
          )
        )
      ))
    })

    output$chat_history_ui <- shiny::renderUI({
      hist <- chat_history_rv()
      if (length(hist) == 0L) {
        i18n <- get_i18n(app_state$language)
        return(htmltools::div(class = "text-muted small",
                              i18n$t("action_plan_chat_empty")))
      }
      htmltools::tagList(lapply(hist, function(msg) {
        htmltools::div(
          class = paste0("p-2 mb-2 rounded ",
                         if (msg$role == "user") "bg-light" else "bg-success-subtle"),
          htmltools::tags$strong(msg$role, ": "),
          htmltools::tags$pre(
            class = "mb-0",
            style = "white-space: pre-wrap; font-size: 0.85rem;",
            msg$text
          )
        )
      }))
    })

    shiny::observeEvent(input$chat_clear, {
      chat_history_rv(list())
    })

    shiny::observeEvent(input$chat_send, {
      i18n <- get_i18n(app_state$language)
      q <- trimws(input$chat_input %||% "")
      if (!nzchar(q)) return()
      ctx <- plan_llm_context()
      if (is.null(ctx)) return()

      # Append user message immediately
      hist <- chat_history_rv()
      hist <- c(hist, list(list(role = "user", text = q)))
      chat_history_rv(hist)
      shiny::updateTextAreaInput(session, "chat_input", value = "")

      provider <- get_app_config("llm_provider", "anthropic")
      key_var <- get_llm_api_key_var(provider)
      if (!is.null(key_var) && nchar(Sys.getenv(key_var)) == 0L) {
        msg <- gsub("\\{key_var\\}", key_var, i18n$t("ai_no_api_key"))
        shiny::showNotification(msg, type = "warning")
        return()
      }

      cur_actions_json <- jsonlite::toJSON(
        lapply(plan_rv()$actions, function(a) {
          list(id = a$id, ug_id = a$ug_id, type = a$type,
               annee_cible = a$annee_cible, statut = a$statut,
               priorite = a$priorite)
        }),
        auto_unbox = TRUE, null = "null"
      )

      lang_param <- if (identical(app_state$language, "fr")) "fran\u00e7ais" else "English"
      prompt <- build_action_plan_chat_prompt(
        question = q,
        ctx = ctx,
        plan_summary_json = as.character(cur_actions_json),
        language = lang_param
      )
      system_prompt <- build_system_prompt(lang_param, expert = "planificateur")

      response <- tryCatch({
        chat <- create_llm_chat(system_prompt)
        as.character(chat$chat(prompt, echo = FALSE))
      }, error = function(e) {
        msg <- paste(i18n$t("ai_error"), ":",
                     strip_ansi(conditionMessage(e)))
        shiny::showNotification(msg, type = "error", duration = 8)
        NULL
      })
      if (is.null(response)) return()

      hist <- chat_history_rv()
      hist <- c(hist, list(list(role = "assistant", text = response)))
      chat_history_rv(hist)

      # Optional apply: if the response embeds a JSON `actions` block,
      # try to parse + offer apply.
      parsed <- parse_action_plan_response(
        response,
        ug_ids = ctx$ug_ids,
        horizon_annees = ctx$horizon
      )
      if (length(parsed$actions) > 0L) {
        shiny::showModal(shiny::modalDialog(
          title = i18n$t("action_plan_chat_apply_title"),
          size = "m", easyClose = TRUE,
          shiny::p(sprintf(i18n$t("action_plan_chat_apply_fmt"),
                           length(parsed$actions))),
          footer = htmltools::tagList(
            shiny::modalButton(i18n$t("cancel")),
            shiny::actionButton(ns("chat_apply"),
                                label = i18n$t("action_plan_chat_apply_btn"),
                                icon = shiny::icon("check"),
                                class = "btn-primary")
          )
        ))
        # Stash for the apply observer
        rv_state$pending_chat_actions <- parsed$actions
      }
    })

    shiny::observeEvent(input$chat_apply, {
      shiny::removeModal()
      acts <- rv_state$pending_chat_actions
      if (is.null(acts) || length(acts) == 0L) return()
      i18n <- get_i18n(app_state$language)
      project <- app_state$current_project
      cur_plan <- plan_rv()
      new_plan <- tryCatch(
        bulk_upsert_actions(cur_plan, acts, ug_ids = ug_ids(),
                            user = "llm:chat"),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (is.null(new_plan)) return()
      save_action_plan(project$id, new_plan)
      plan_rv(new_plan)
      rv_state$pending_chat_actions <- NULL
      shiny::showNotification(
        sprintf(i18n$t("action_plan_chat_applied_fmt"), length(acts)),
        type = "message", duration = 4
      )
    })

    # ============================================================
    # S8 - Kanban transitions (bulk) + history modal
    # ============================================================

    shiny::observeEvent(input$apply_bulk_status, {
      i18n <- get_i18n(app_state$language)
      target <- input$bulk_status %||% ""
      sel_rows <- input$action_table_rows_selected
      df <- actions_df_all()
      if (!nzchar(target)) {
        shiny::showNotification(i18n$t("action_plan_bulk_status_pick"),
                                type = "warning", duration = 4)
        return()
      }
      if (length(sel_rows) == 0L || nrow(df) == 0L) {
        shiny::showNotification(i18n$t("action_plan_no_selection"),
                                type = "warning", duration = 4)
        return()
      }

      cur_plan <- plan_rv()
      ids <- df$id[sel_rows]
      ids <- ids[!is.na(ids)]
      n_ok <- 0L
      n_skip <- 0L
      errors <- character()
      for (aid in ids) {
        idx <- find_action_index(cur_plan, aid)
        if (is.na(idx)) { n_skip <- n_skip + 1L; next }
        from <- cur_plan$actions[[idx]]$statut %||% "proposee"
        if (!is_valid_status_transition(from, target)) {
          errors <- c(errors, sprintf("%s: %s -> %s", aid, from, target))
          n_skip <- n_skip + 1L
          next
        }
        ok <- TRUE
        cur_plan <- tryCatch(
          update_action_in_plan(cur_plan, aid,
                                list(statut = target),
                                ug_ids = ug_ids(),
                                user = Sys.info()[["user"]] %||% "user"),
          error = function(e) {
            errors <<- c(errors, conditionMessage(e))
            ok <<- FALSE
            cur_plan
          }
        )
        if (ok) n_ok <- n_ok + 1L else n_skip <- n_skip + 1L
      }

      save_action_plan(app_state$current_project$id, cur_plan)
      plan_rv(cur_plan)
      shiny::showNotification(
        sprintf(i18n$t("action_plan_bulk_status_ok_fmt"), n_ok, n_skip),
        type = if (n_ok > 0L) "message" else "warning",
        duration = 6
      )
      if (length(errors) > 0L) {
        cli::cli_inform("Bulk status errors: {paste(errors, collapse = ' | ')}")
      }
    })

    shiny::observeEvent(input$show_history, {
      i18n <- get_i18n(app_state$language)
      sel_rows <- input$action_table_rows_selected
      df <- actions_df_all()
      if (length(sel_rows) != 1L || nrow(df) == 0L) {
        shiny::showNotification(i18n$t("action_plan_history_pick_one"),
                                type = "warning", duration = 4)
        return()
      }
      action_id <- df$id[sel_rows]
      audit <- get_action_audit(plan_rv(), action_id)
      body <- if (length(audit) == 0L) {
        shiny::p(class = "text-muted",
                 i18n$t("action_plan_history_empty"))
      } else {
        adf <- audit_to_dataframe(audit)
        # Compact HTML table -- audits are typically small.
        rows <- lapply(seq_len(nrow(adf)), function(i) {
          htmltools::tags$tr(
            htmltools::tags$td(adf$ts[i]),
            htmltools::tags$td(adf$user[i]),
            htmltools::tags$td(adf$op[i]),
            htmltools::tags$td(adf$champ[i]),
            htmltools::tags$td(adf$ancien[i]),
            htmltools::tags$td(adf$nouveau[i])
          )
        })
        htmltools::tags$table(
          class = "table table-sm table-striped",
          htmltools::tags$thead(htmltools::tags$tr(
            htmltools::tags$th("ts"),
            htmltools::tags$th("user"),
            htmltools::tags$th("op"),
            htmltools::tags$th("champ"),
            htmltools::tags$th("ancien"),
            htmltools::tags$th("nouveau")
          )),
          htmltools::tags$tbody(rows)
        )
      }
      shiny::showModal(shiny::modalDialog(
        title = sprintf(i18n$t("action_plan_history_title_fmt"), action_id),
        easyClose = TRUE, size = "l",
        body,
        footer = shiny::modalButton(i18n$t("close"))
      ))
    })

    # Manual add: simple modal
    shiny::observeEvent(input$add_action, {
      i18n <- get_i18n(app_state$language)
      project <- app_state$current_project
      if (is.null(project)) return()
      sel_ugs <- selected_ug_rv()
      shiny::showModal(shiny::modalDialog(
        title = i18n$t("action_plan_add_title"), size = "m",
        easyClose = TRUE,
        shiny::selectInput(ns("add_ug"), i18n$t("action_plan_ug"),
                           choices = ug_ids(),
                           selected = if (length(sel_ugs) == 1L) sel_ugs[1] else NULL),
        shiny::selectInput(ns("add_type"), i18n$t("action_plan_type"),
                           choices = ACTION_PLAN_TYPES,
                           selected = "observation"),
        shiny::numericInput(ns("add_year"), i18n$t("action_plan_year"),
                            value = 1L, min = 1L,
                            max = (plan_rv()$horizon_annees %||% 20L)),
        shiny::selectInput(ns("add_priority"),
                           i18n$t("action_plan_color_priority"),
                           choices = ACTION_PLAN_PRIORITES,
                           selected = "moyenne"),
        shiny::textInput(ns("add_comment"),
                         i18n$t("action_plan_comment"), value = ""),
        footer = htmltools::tagList(
          shiny::modalButton(i18n$t("cancel")),
          shiny::actionButton(ns("add_run"),
                              label = i18n$t("action_plan_add_run"),
                              icon = shiny::icon("plus"),
                              class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$add_run, {
      shiny::removeModal()
      i18n <- get_i18n(app_state$language)
      action <- list(
        ug_id = input$add_ug,
        type = input$add_type,
        annee_cible = as.integer(input$add_year),
        priorite = input$add_priority,
        statut = "proposee",
        commentaire = if (nzchar(input$add_comment %||% ""))
                        input$add_comment else NULL
      )
      cur_plan <- plan_rv()
      new_plan <- tryCatch(
        add_action_to_plan(cur_plan, action,
                           ug_ids = ug_ids(),
                           user = Sys.info()[["user"]] %||% "user"),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (is.null(new_plan)) return()
      save_action_plan(app_state$current_project$id, new_plan)
      plan_rv(new_plan)
      shiny::showNotification(i18n$t("action_plan_add_ok"),
                              type = "message", duration = 3)
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
  if (field %in% c("volume_m3", "surface_ha", "rdi", "cout_eur",
                   "revenu_eur")) {
    return(suppressWarnings(as.numeric(raw)))
  }
  as.character(raw)
}

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
#' Subsequent stories (S7 IA, S8 Kanban view, S10/11 ponts terrain,
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

  # Right-side action panel: same collapsible-card pattern as the
  # `Selection` tab cards (clickable colored header with icon + title +
  # chevron, body folds underneath). Drag-and-drop on the Kanban board
  # has replaced the bulk-status dropdown so that section is gone.
  action_panel_id <- ns("actions_collapse")
  action_panel <- htmltools::tags$div(
    class = "card mb-3",
    # Header: clickable, collapses the body
    htmltools::tags$div(
      class = "card-header bg-success text-white py-2",
      style = "cursor: pointer;",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = paste0("#", action_panel_id),
      `aria-expanded` = "true",
      `aria-controls` = action_panel_id,
      htmltools::div(
        class = "d-flex align-items-center justify-content-between",
        htmltools::div(
          class = "d-flex align-items-center",
          bsicons::bs_icon("clipboard-check", class = "me-2"),
          i18n$t("action_plan_actions_title")
        ),
        bsicons::bs_icon("chevron-down", class = "collapse-icon")
      )
    ),
    # Collapsible body
    htmltools::tags$div(
      id = action_panel_id,
      class = "collapse show",
      htmltools::tags$div(
        class = "card-body p-3",

        # ---- Selection ---------------------------------------------
        htmltools::tags$h6(class = "mt-1",
                           i18n$t("action_plan_section_selection")),
        shiny::actionButton(
          ns("clear_map_selection"),
          label = i18n$t("action_plan_clear_selection"),
          icon = shiny::icon("eraser"),
          class = "btn-sm btn-outline-secondary w-100 mb-2"
        ),
        shiny::actionButton(
          ns("show_history"),
          label = i18n$t("action_plan_show_history"),
          icon = shiny::icon("clock-rotate-left"),
          class = "btn-sm btn-outline-secondary w-100 mb-3"
        ),

        # ---- IA ----------------------------------------------------
        htmltools::tags$h6(i18n$t("action_plan_section_ia")),
        shiny::actionButton(
          ns("generate_all"),
          label = i18n$t("action_plan_generate_all"),
          icon = shiny::icon("wand-magic-sparkles"),
          class = "btn-sm btn-primary w-100 mb-3"
        ),
        # The Q/R chat used to live behind an "Ouvrir le chat" modal
        # button here; it now lives in the persistent left sidebar
        # (`chat_panel` below) so the conversation stays visible while
        # the user works on the map / table / Kanban.

        # ---- Manuel ------------------------------------------------
        htmltools::tags$h6(i18n$t("action_plan_section_manual")),
        shiny::actionButton(
          ns("add_action"),
          label = i18n$t("action_plan_add"),
          icon = shiny::icon("plus"),
          class = "btn-sm btn-outline-primary w-100 mb-3"
        ),

        # ---- Exports -----------------------------------------------
        htmltools::tags$h6(i18n$t("action_plan_section_exports")),
        shiny::actionButton(
          ns("export_terrain"),
          label = i18n$t("action_plan_export_terrain"),
          icon = shiny::icon("crosshairs"),
          class = "btn-sm btn-outline-success w-100 mb-2"
        ),
        htmltools::tagAppendAttributes(
          shiny::downloadButton(
            ns("download_gpkg"),
            label = i18n$t("action_plan_download_gpkg"),
            icon = shiny::icon("database"),
            class = "btn-sm btn-outline-success w-100 mb-2"
          ),
          onclick = sprintf(
            "nemetonShowDownloadToast(%s);",
            jsonlite::toJSON(i18n$t("action_plan_export_running_gpkg"),
                             auto_unbox = TRUE)
          )
        ),
        htmltools::tagAppendAttributes(
          shiny::downloadButton(
            ns("download_pdf"),
            label = i18n$t("action_plan_download_pdf"),
            icon = shiny::icon("file-pdf"),
            class = "btn-sm btn-outline-success w-100"
          ),
          onclick = sprintf(
            "nemetonShowDownloadToast(%s);",
            jsonlite::toJSON(i18n$t("action_plan_export_running_pdf"),
                             auto_unbox = TRUE)
          )
        )
      )
    )
  )

  # Persistent Q/R chat panel. Lives in the right-hand sidebar
  # below the `action_panel` ("Tableau des actions"), same
  # collapsible-header pattern. Replaces the former "Ouvrir le
  # chat" modal so the conversation stays visible while the user
  # navigates the map / table / Kanban. The history div carries a
  # stable id so the renderUI in the server can append a tiny
  # inline script that auto-scrolls it to the bottom on every
  # update.
  chat_panel_id   <- ns("chat_collapse")
  chat_history_id <- ns("chat_history")
  chat_panel <- htmltools::tags$div(
    class = "card mb-3",
    htmltools::tags$div(
      class = "card-header bg-info text-white py-2",
      style = "cursor: pointer;",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = paste0("#", chat_panel_id),
      `aria-expanded` = "true",
      `aria-controls` = chat_panel_id,
      htmltools::div(
        class = "d-flex align-items-center justify-content-between",
        htmltools::div(
          class = "d-flex align-items-center",
          bsicons::bs_icon("chat-dots", class = "me-2"),
          i18n$t("action_plan_chat_title")
        ),
        bsicons::bs_icon("chevron-down", class = "collapse-icon")
      )
    ),
    htmltools::tags$div(
      id = chat_panel_id,
      class = "collapse show",
      htmltools::tags$div(
        class = "card-body p-2",
        # Scrollable history. Capped via max-height so the textarea
        # + buttons stay visible even on long conversations; older
        # messages scroll out of view. The fixed id is what the
        # auto-scroll snippet in chat_history_ui targets.
        htmltools::div(
          id = chat_history_id,
          class = "chat-history mb-2",
          style = paste(
            "overflow-y: auto;",
            "max-height: 50vh;",
            "min-height: 160px;",
            "border: 1px solid rgba(0,0,0,.075);",
            "border-radius: .25rem;",
            "padding: .5rem;",
            "background: #fafafa;"
          ),
          shiny::uiOutput(ns("chat_history_ui"))
        ),
        # Scope + overwrite controls — same semantics as the
        # "Générer les actions (IA)" modal (gen_scope / gen_overwrite),
        # except the radio is always-visible so the user can flip it
        # between turns. Static "Sélection courante" label here (the
        # generate modal shows the count because it opens fresh each
        # time; the persistent panel would need a renderUI to keep it
        # in sync, which felt overkill — the user reads the count
        # from the orange polygons on the map).
        shiny::radioButtons(
          ns("chat_scope"),
          label = i18n$t("action_plan_generate_scope"),
          choices = stats::setNames(
            c("all", "selected"),
            c(i18n$t("action_plan_generate_scope_all"),
              i18n$t("action_plan_chat_scope_sel"))
          ),
          selected = "all",
          inline = TRUE
        ),
        shiny::checkboxInput(
          ns("chat_overwrite"),
          label = i18n$t("action_plan_generate_overwrite"),
          value = FALSE
        ),
        shiny::textAreaInput(
          ns("chat_input"),
          label = NULL,
          placeholder = i18n$t("action_plan_chat_placeholder"),
          rows = 3, width = "100%",
          resize = "vertical"
        ),
        htmltools::div(
          class = "d-flex gap-2 mt-2",
          shiny::actionButton(
            ns("chat_clear"),
            label = i18n$t("action_plan_chat_clear"),
            icon = shiny::icon("broom"),
            class = "btn-sm btn-outline-secondary flex-fill"
          ),
          shiny::actionButton(
            ns("chat_send"),
            label = i18n$t("action_plan_chat_send"),
            icon = shiny::icon("paper-plane"),
            class = "btn-sm btn-primary flex-fill"
          )
        )
      )
    )
  )

  bslib::layout_sidebar(
    fillable = TRUE,
    # Right-hand sidebar -- 350 px width. Contains the action
    # panel ("Tableau des actions") at the top and the persistent
    # chat panel below. The bslib sidebar exposes a built-in
    # collapse toggle in its border for narrow monitors.
    sidebar = bslib::sidebar(
      id = ns("action_sidebar"),
      width = 350,
      position = "right",
      open = TRUE,
      bg = "transparent",
      action_panel,
      chat_panel
    ),

    # Main pane: nav switcher (carte+tableau / kanban). Style scoped
    # to the inner DT to ellipsize long cells.
    htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(sprintf("
        #%s .dt-truncate {
          max-width: 220px;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        /* Push the action count (info) to the bottom-right; keep the
           length selector + pagination on the left. The default DT
           CSS uses floats which we neutralise inside the wrapper
           layout produced by `dom`. */
        #%s_wrapper .dt-bottom-row .dataTables_info,
        #%s_wrapper .dt-bottom-row .dataTables_length,
        #%s_wrapper .dt-bottom-row .dataTables_paginate {
          float: none !important;
          clear: none !important;
          padding-top: 0 !important;
          margin: 0 !important;
        }
        #%s_wrapper .dt-bottom-row .dataTables_info {
          text-align: right;
          white-space: nowrap;
        }
      ",
        ns("action_table"),
        ns("action_table"), ns("action_table"), ns("action_table"),
        ns("action_table")
      ))),

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
              shiny::textOutput(ns("table_count_inline"), inline = TRUE)
            ),
            bslib::card_body(
              # Read-only banner (S15) -- reactive on auth changes.
              shiny::uiOutput(ns("readonly_banner")),
              # Cumulative balance sparkline + totals strip on top
              htmltools::div(
                class = "border-bottom mb-2 pb-2",
                shiny::uiOutput(ns("balance_summary"))
              ),
              DT::dataTableOutput(ns("action_table"))
            )
          )
        )
      ),

      bslib::nav_panel(
        title = i18n$t("action_plan_view_kanban"),
        value = "kanban",
        icon = bsicons::bs_icon("kanban"),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(i18n$t("action_plan_view_kanban")),
          bslib::card_body(
            shiny::uiOutput(ns("kanban_board"))
          )
        )
      )
      )  # close navset_card_underline
    )    # close htmltools::tagList — action_panel + chat_panel live
         # in the right-hand sidebar above, not as positional args.
  )      # close bslib::layout_sidebar
}


# ---- Server ----------------------------------------------------------

#' Archive a rendered action-plan PDF into the project's `exports/` directory
#'
#' Mirrors the synthesis report's archiving (mod_synthesis.R): a single current
#' PDF per project under `<project_path>/exports/<slug>_action_plan.pdf`. Called
#' only on the success path of `output$download_pdf` (never for the failure
#' marker). Best-effort by design: a missing project path, an unwritable
#' directory or a failed copy is warned but never aborts the browser download.
#'
#' @param rendered_file Path to the freshly rendered PDF handed to the browser.
#' @param project The current project (list with `path` and `metadata$name`).
#' @return Invisibly `TRUE` when a copy was made, `FALSE` otherwise.
#' @noRd
.archive_action_plan_pdf <- function(rendered_file, project) {
  project_path <- tryCatch(project$path, error = function(e) NULL)
  if (is.null(project_path) || !dir.exists(project_path)) return(invisible(FALSE))
  exports_dir <- file.path(project_path, "exports")
  if (!dir.exists(exports_dir)) {
    dir.create(exports_dir, recursive = TRUE, showWarnings = FALSE)
  }
  nm <- tryCatch(project$metadata$name, error = function(e) NULL)
  export_name <- gsub("[^a-zA-Z0-9_-]", "_", nm %||% "nemeton_action_plan")
  export_file <- file.path(exports_dir, paste0(export_name, "_action_plan.pdf"))
  tryCatch(
    {
      ok <- file.copy(rendered_file, export_file, overwrite = TRUE)
      invisible(isTRUE(ok))
    },
    error = function(e) {
      cli::cli_warn("Action plan PDF archive failed: {conditionMessage(e)}")
      invisible(FALSE)
    }
  )
}

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

    # ------------------------------------------------------------
    # S15 -- permissions
    # ------------------------------------------------------------
    # Single source of truth for "may the current user mutate the
    # plan?". Re-evaluated whenever auth changes (login / logout /
    # token refresh). Mutation observers below all gate on this.
    can_edit <- shiny::reactive({
      can_edit_action_plan(app_state$auth)
    })

    # Helper: deny + toast if the user is read-only. Returns TRUE
    # when the action should be cancelled. Two orthogonal reasons:
    #  - the user's ROLE cannot edit action plans (`can_edit()`), or
    #  - the PROJECT is opened read-only because its edit lock is held
    #    by another user (or the user is anonymous) — see R/service_lock.R.
    deny_if_readonly <- function() {
      locked <- project_is_readonly(app_state)
      if (isTRUE(can_edit()) && !locked) return(FALSE)
      i18n <- get_i18n(app_state$language)
      msg <- if (locked) i18n$t("lock_readonly_action")
             else i18n$t("action_plan_readonly_locked")
      shiny::showNotification(msg, type = "warning", duration = 5)
      TRUE
    }

    # Bumped after a refused Kanban drag-and-drop drop so the
    # `kanban_board` renderUI reruns and puts the card back where the
    # data says it belongs (plan_rv is untouched in that case).
    kanban_render_token <- shiny::reactiveVal(0L)

    # Carries the action_id of the card being edited via the Kanban
    # double-click → modal flow (see input$kanban_edit_request /
    # input$kanban_edit_save below).
    kanban_edit_id_rv <- shiny::reactiveVal(NULL)

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
      if (nrow(df) == 0L) {
        df$ug_label <- character(0)
        df$annee_realisation <- integer(0)
        return(df)
      }
      if (!is.null(sf)) {
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

    # Per-UGF aggregation for map coloring (over the *visible* actions).
    # `annees` carries the calendar years of execution (annee_realisation),
    # not the storage offset.
    ugf_summary <- shiny::reactive({
      df <- actions_df()
      ugs <- ug_ids()
      lapply(stats::setNames(ugs, ugs), function(uid) {
        sub <- df[df$ug_id == uid & !is.na(df$ug_id), , drop = FALSE]
        list(
          n      = nrow(sub),
          annees = sort(unique(stats::na.omit(sub$annee_realisation))),
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

    # Helper to build a warm year palette (one swatch per distinct year).
    # Earliest year = pale yellow, latest year = deep red. Treated as a
    # discrete factor so the leaflet legend lists every year explicitly.
    year_factor_palette <- function(years) {
      years <- as.character(sort(unique(stats::na.omit(years))))
      if (length(years) == 0L) {
        return(stats::setNames(character(0), character(0)))
      }
      n <- length(years)
      cols <- if (n == 1L) {
        "#fc8d59"
      } else {
        grDevices::colorRampPalette(c("#fee08b", "#fc8d59", "#b91c1c"))(n)
      }
      stats::setNames(cols, years)
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
      i18n <- get_i18n(app_state$language)

      empty_result <- list(
        colors = rep("#bbbbbb", n_ug),
        legend_title = NULL,
        legend_pal   = NULL,
        legend_vals  = NULL
      )

      if (mode == "annee") {
        # Take the earliest action year per UGF (the "next" deadline).
        firsts <- vapply(sf$ug_id, function(uid) {
          ann <- summary_list[[uid]]$annees
          if (length(ann) == 0L) return(NA_integer_)
          as.integer(min(ann, na.rm = TRUE))
        }, integer(1))
        all_years <- sort(unique(stats::na.omit(firsts)))
        if (length(all_years) == 0L) return(empty_result)
        all_years_chr <- as.character(all_years)
        pal <- year_factor_palette(all_years)
        cols <- ifelse(is.na(firsts), "#bbbbbb",
                       pal[as.character(firsts)])
        legend_pal <- leaflet::colorFactor(
          palette = unname(pal),
          domain  = all_years_chr,
          ordered = TRUE
        )
        list(
          colors = unname(cols),
          legend_title = i18n$t("action_plan_col_annee"),
          legend_pal   = legend_pal,
          legend_vals  = all_years_chr
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
          legend_title = i18n$t("action_plan_col_type"),
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
          legend_title = i18n$t("action_plan_col_priorite"),
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
        # Prefer the UGF label (human readable); fall back to the id
        # only when the project has no label set.
        display_name <- if (!is.null(sf$label) &&
                            !is.na(sf$label[i]) &&
                            nzchar(sf$label[i])) {
          sf$label[i]
        } else {
          as.character(uid)
        }
        s <- summary_list[[uid]] %||% list(n = 0L, annees = integer(),
                                            types = character(), prios = character())
        sprintf(
          "<b>%s</b><br>%d action(s)<br>Annees: %s<br>Types: %s<br>Priorites: %s",
          display_name, s$n,
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

    # Map click on UGF -> select that UGF (toggle) AND propagate the
    # selection to the action table: every row whose ug_id is in the
    # current selection gets highlighted via DT::selectRows. The
    # reverse direction (table -> map) is handled by the
    # `input$action_table_rows_selected` observer below; together they
    # keep the two views in sync. `reactiveVal` dedupes by identical()
    # so the round-trip (map -> selected_ug_rv -> table -> back) does
    # not loop.
    shiny::observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      if (is.null(click$id)) return()
      uid <- as.character(click$id)
      cur <- selected_ug_rv()
      cur <- if (uid %in% cur) setdiff(cur, uid) else c(cur, uid)
      selected_ug_rv(cur)

      df <- actions_df_all()
      if (nrow(df) == 0L) return()
      rows <- which(df$ug_id %in% cur)
      DT::selectRows(DT::dataTableProxy("action_table"),
                     if (length(rows) > 0L) rows else NULL)
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
      # Visible columns first so FixedColumns(leftColumns = 2) freezes
      # exactly UGF + Année (DT's FixedColumns extension counts every
      # DOM column, including visible:FALSE ones, so hidden columns
      # must live at the tail to keep the freeze count meaningful).
      "ug_label", "annee_realisation",
      "type",
      "intensite", "priorite", "statut",
      "volume_m3", "surface_ha", "nb_tiges",
      "cout_eur", "revenu_eur", "bilan_eur",
      "commentaire",
      # Hidden columns (id, ug_id, annee_cible offset).
      "id", "ug_id", "annee_cible"
    )
    # Cells the user can edit. `annee_realisation` accepts a calendar
    # year and is converted back to an offset before persistence.
    #
    # v0.52.10 — `commentaire` retiré de l'édition inline DT : l'input
    # single-line dans une cellule étroite tronquait le texte sans
    # permettre de le lire en entier. Le commentaire passe désormais
    # par le modal d'édition (textarea 6 rangs, multi-ligne) — accessible
    # via dblclick sur la cellule commentaire (cf. JS callback ci-dessous).
    EDITABLE_COLS <- c(
      "annee_realisation", "type", "intensite", "priorite",
      "statut", "volume_m3", "surface_ha",
      "nb_tiges", "cout_eur", "revenu_eur"
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
        i18n$t("action_plan_col_ug_label"),
        i18n$t("action_plan_col_annee"),
        i18n$t("action_plan_col_type"),
        i18n$t("action_plan_col_intensite"),
        i18n$t("action_plan_col_priorite"),
        i18n$t("action_plan_col_statut"),
        i18n$t("action_plan_col_volume"),
        i18n$t("action_plan_col_surface"),
        i18n$t("action_plan_col_tiges"),
        i18n$t("action_plan_col_cout"),
        i18n$t("action_plan_col_revenu"),
        i18n$t("action_plan_col_bilan"),
        i18n$t("action_plan_col_commentaire"),
        i18n$t("action_plan_col_id"),
        i18n$t("action_plan_col_ug_id"),
        i18n$t("action_plan_col_annee_offset")
      )

      # Hidden columns: id, ug_id, annee_cible offset — sit at the tail
      # of DISPLAY_COLS, so targets are the last 3 indices.
      hidden_targets <- as.integer(c(13L, 14L, 15L))
      editable_idx <- which(DISPLAY_COLS %in% EDITABLE_COLS) - 1L

      # v0.52.10 — JS callback : dblclick sur la cellule commentaire
      # (className `action-comment-trigger`, cf. columnDefs) ouvre le
      # modal d'édition (réutilise `input$kanban_edit_request` via une
      # passerelle `row_edit_request`). data[13] est la colonne cachée
      # `id` (DISPLAY_COLS tail). `_ts` force Shiny à émettre l'event
      # même si on dblclick deux fois la même ligne.
      #
      # v0.52.12 — IMPORTANT : DT::datatable(callback = ...) attend le
      # CORPS de la fonction (DT wrap automatiquement avec
      # `function(table) { ... }`), PAS un wrapper complet. Une v0.52.10
      # qui passait `function(table) { ... }` créait un double wrapper
      # → fonction interne jamais invoquée → handler dblclick jamais
      # installé, ET pas de `return table;` → l'init DataTables cassait
      # silencieusement → tableau RENDU VIDE alors que data.frame avait
      # 31 lignes.
      row_edit_input_id <- session$ns("row_edit_request")
      js_dt_callback <- htmlwidgets::JS(sprintf(
        "var inputId = '%s';
        table.on('dblclick.dt', 'td.action-comment-trigger', function(e) {
          e.stopPropagation();
          var rowData = table.row(this).data();
          if (!rowData || rowData[13] == null) return;
          Shiny.setInputValue(inputId, {
            action_id: String(rowData[13]),
            _ts: Date.now()
          });
        });
        return table;",
        row_edit_input_id
      ))

      DT::datatable(
        display,
        colnames = colname_map,
        rownames = FALSE,
        # No per-column filters; the global "search" box (dom 'f') is
        # the only filter exposed to the user.
        filter = "none",
        # `compact` shrinks padding, `nowrap` forces single-line cells
        # so every row has the same height regardless of content length.
        class = "display compact stripe hover nowrap",
        selection = list(mode = "multiple", target = "row"),
        editable  = list(target = "cell",
                         disable = list(columns = setdiff(seq_len(ncol(display)) - 1L,
                                                          editable_idx))),
        extensions = c("FixedColumns"),
        callback = js_dt_callback,
        options = list(
          # 5 rows per page, paginate the rest. lengthMenu lets the
          # user expand to 10 / 25 / 50 / All if they want a denser
          # view; the search/filter still operates on the full plan.
          pageLength = 5,
          lengthMenu = list(c(5, 10, 25, 50, -1),
                            c("5", "10", "25", "50", "All")),
          # Bottom layout: search at the top alone; below the table a
          # flex row keeps the length selector + pagination on the
          # left and pushes the action count (`i`) to the right via
          # the `.dt-bottom-row` CSS rules above.
          dom = paste0(
            '<"top"f>rt',
            '<"d-flex justify-content-between align-items-center pt-2 dt-bottom-row"',
              '<"d-flex gap-3 align-items-center"lp>',
              'i',
            '>'
          ),
          # Horizontal scroll only — vertical scroll is replaced by
          # pagination, so dropping scrollY/scrollCollapse keeps the
          # 5-row table compact instead of padding it to 60vh.
          scrollX = TRUE,
          # Freeze only UGF + Ann\u00e9e (the first two visible columns).
          # Hidden columns now live at the tail of DISPLAY_COLS so this
          # count maps cleanly onto what the user actually sees.
          fixedColumns = list(leftColumns = 2L),
          # Enable regex on the global search box so the user can type
          # "eclaircie|plantation" for an OR filter (default smart
          # search uses AND across words). caseInsensitive keeps the
          # search forgiving on accents-less typing.
          search = list(regex = TRUE, caseInsensitive = TRUE,
                        smart = FALSE),
          columnDefs = list(
            list(visible = FALSE, targets = hidden_targets),
            # Cap visible-column widths so long commentaire / labels
            # ellipsize instead of wrapping to a second line.
            list(targets = "_all",
                 className = "dt-truncate"),
            # v0.52.10 — la colonne commentaire (index 12 visible)
            # est marquée comme « clickable » : dblclick ouvre le
            # modal d'édition (cf. JS callback du `callback =`).
            # Curseur pointer + soulignement pointillé donnent un
            # affordance visuel sans surcharger l'UI.
            list(targets = 12L,
                 className = "dt-truncate action-comment-trigger")
          ),
          language = if (identical(app_state$language, "en")) list(
            search = "Search (regex, e.g. eclaircie|plantation):"
          ) else list(
            search = "Rechercher (regex, ex. eclaircie|plantation) :",
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
      if (deny_if_readonly()) return()
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
    # SPARKLINE + totals (cumulative balance over years, on filtered)
    # ============================================================

    cumulative_balance <- shiny::reactive({
      df <- actions_df()
      if (nrow(df) == 0L) return(NULL)
      d <- df[!is.na(df$annee_realisation), c("annee_realisation",
                                              "cout_eur", "revenu_eur",
                                              "bilan_eur"), drop = FALSE]
      if (nrow(d) == 0L) return(NULL)
      agg <- stats::aggregate(
        cbind(cout_eur, revenu_eur, bilan_eur) ~ annee_realisation,
        data = d,
        FUN = function(x) sum(x, na.rm = TRUE),
        na.action = stats::na.pass
      )
      agg <- agg[order(agg$annee_realisation), , drop = FALSE]
      agg$cumul_bilan  <- cumsum(agg$bilan_eur)
      agg$cumul_revenu <- cumsum(agg$revenu_eur)
      agg$cumul_cout   <- cumsum(agg$cout_eur)
      agg
    })

    output$readonly_banner <- shiny::renderUI({
      if (isTRUE(can_edit())) return(NULL)
      i18n <- get_i18n(app_state$language)
      htmltools::div(
        class = "alert alert-warning py-1 px-2 mb-2 small",
        shiny::icon("lock"), " ",
        i18n$t("action_plan_readonly_banner")
      )
    })

    output$balance_summary <- shiny::renderUI({
      i18n <- get_i18n(app_state$language)
      df <- actions_df()
      if (nrow(df) == 0L) {
        return(htmltools::div(class = "text-muted small",
                              i18n$t("action_plan_balance_empty")))
      }
      total_cout    <- sum(df$cout_eur,   na.rm = TRUE)
      total_revenu  <- sum(df$revenu_eur, na.rm = TRUE)
      total_bilan   <- total_revenu - total_cout
      total_surface <- sum(df$surface_ha, na.rm = TRUE)
      bilan_class   <- if (total_bilan > 0) "text-success"
                       else if (total_bilan < 0) "text-danger"
                       else "text-muted"

      # `unit` adds a trailing label after the formatted number; default
      # is "EUR" for the monetary pills. Surface uses "ha" with two
      # decimals so a 0.32 ha eclaircie does not show as "0".
      pill <- function(label, value, css = "", unit = "EUR",
                       digits = 0L) {
        formatted <- if (digits == 0L) {
          format(round(value), big.mark = " ", scientific = FALSE)
        } else {
          formatC(round(value, digits), format = "f", digits = digits,
                  big.mark = " ")
        }
        htmltools::tags$span(
          class = paste("badge bg-light border me-2", css),
          style = "font-size: 0.85rem;",
          htmltools::tags$strong(label, ":", .noWS = "after"),
          " ",
          formatted, " ", unit
        )
      }

      htmltools::div(
        class = "d-flex align-items-center justify-content-between gap-3 flex-wrap",
        htmltools::div(
          pill(i18n$t("action_plan_total_cout"),    total_cout,    "text-danger"),
          pill(i18n$t("action_plan_total_revenu"),  total_revenu,  "text-success"),
          pill(i18n$t("action_plan_total_bilan"),   total_bilan,   bilan_class),
          pill(i18n$t("action_plan_total_surface"), total_surface,
               "text-primary", unit = "ha", digits = 2L)
        ),
        htmltools::div(
          style = "min-width: 320px; flex: 1; max-width: 600px;",
          plotly::plotlyOutput(session$ns("balance_sparkline"),
                               height = "60px")
        )
      )
    })

    output$balance_sparkline <- plotly::renderPlotly({
      d <- cumulative_balance()
      if (is.null(d) || nrow(d) == 0L) return(plotly::plotly_empty())
      pal <- ifelse(d$cumul_bilan >= 0, "#15803d", "#b91c1c")
      plotly::plot_ly(
        d,
        x = ~annee_realisation,
        y = ~cumul_bilan,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#374151", width = 2),
        marker = list(color = pal, size = 7),
        text = ~paste0(annee_realisation, " : ",
                       format(round(cumul_bilan), big.mark = " "), " EUR"),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          margin = list(l = 30, r = 5, t = 5, b = 25),
          xaxis = list(title = "", tickformat = "d", fixedrange = TRUE),
          yaxis = list(title = "", zeroline = TRUE,
                       zerolinecolor = "#999", fixedrange = TRUE),
          showlegend = FALSE,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ============================================================
    # KANBAN BOARD - 5 columns of cards by status, click to advance.
    # ============================================================

    KANBAN_HEADER_BG <- c(
      proposee   = "#e5e7eb",
      validee    = "#dbeafe",
      planifiee  = "#fde68a",
      realisee   = "#bbf7d0",
      abandonnee = "#fecaca"
    )

    output$kanban_board <- shiny::renderUI({
      i18n <- get_i18n(app_state$language)
      df <- actions_df_all()
      if (nrow(df) == 0L) {
        return(htmltools::div(class = "text-muted",
                              i18n$t("action_plan_table_empty")))
      }

      # Per-render token so we can force a re-render after a refused
      # drag-and-drop drop (status transition not allowed). When the
      # drop is valid, plan_rv() update already triggers re-render via
      # actions_df_all() invalidation; when refused, plan_rv() is
      # untouched, so we bump this to put the card back where the data
      # says it belongs.
      kanban_render_token()

      build_card <- function(row) {
        bilan <- row$bilan_eur
        bilan_color <- if (is.na(bilan)) "#6b7280"
                       else if (bilan > 0) "#15803d"
                       else if (bilan < 0) "#b91c1c"
                       else "#374151"
        action_id <- row$id

        type_label <- if (!is.na(row$type) && nzchar(row$type))
          row$type else "?"
        ug_label   <- if (!is.na(row$ug_label) && nzchar(row$ug_label))
          row$ug_label else as.character(row$ug_id)
        objs <- if (nzchar(row$objectifs_lies %||% ""))
          paste0(" \u2014 ", row$objectifs_lies) else ""
        commentaire <- row$commentaire %||% ""

        htmltools::div(
          class = "card mb-2 shadow-sm kanban-card",
          # `data-action-id` is read by SortableJS' onAdd handler and
          # by the delegated dblclick handler in action_plan_kanban.js
          # to identify the dragged or double-clicked card.
          `data-action-id` = action_id,
          title = i18n$t("action_plan_kanban_card_hint"),
          style = "cursor: grab;",
          htmltools::div(
            class = "card-body p-2",
            htmltools::div(
              class = "d-flex justify-content-between align-items-start gap-2",
              htmltools::div(
                htmltools::tags$strong(type_label),
                htmltools::tags$small(class = "text-muted",
                                      sprintf(" %s", row$annee_realisation)),
                htmltools::tags$br(),
                htmltools::tags$small(class = "text-muted",
                                      sprintf("%s%s", ug_label, objs))
              ),
              htmltools::tags$span(
                style = sprintf("color: %s; font-weight: bold; white-space: nowrap;",
                                bilan_color),
                if (is.na(bilan)) "\u2014"
                else sprintf("%s \u20ac", format(round(bilan), big.mark = " ",
                                             scientific = FALSE))
              )
            ),
            if (nzchar(commentaire)) {
              htmltools::div(
                class = "small text-muted mt-1 kanban-card-comment",
                style = "white-space: normal; word-break: break-word;",
                commentaire
              )
            }
          )
        )
      }

      build_col <- function(st, body_max_height) {
        sub <- df[!is.na(df$statut) & df$statut == st, , drop = FALSE]
        # Sort by execution year (ascending, NAs last) so each column
        # reads chronologically from top to bottom.
        if (nrow(sub) > 0L) {
          sub <- sub[order(sub$annee_realisation, na.last = TRUE), ,
                     drop = FALSE]
        }
        bslib::card(
          bslib::card_header(
            class = "py-2 small fw-bold",
            style = sprintf("background-color: %s;",
                            KANBAN_HEADER_BG[[st]] %||% "#e5e7eb"),
            sprintf("%s (%d)",
                    i18n$t(paste0("action_plan_status_", st)),
                    nrow(sub))
          ),
          bslib::card_body(
            class = "p-2 kanban-col-body",
            # `data-kanban-col` is the contract with action_plan_kanban.js:
            # SortableJS uses `evt.from`/`evt.to.getAttribute("data-kanban-col")`
            # to derive source and target statuses on drop.
            `data-kanban-col` = st,
            style = sprintf("max-height: %s; overflow-y: auto; min-height: 60px;",
                            body_max_height),
            if (nrow(sub) == 0L) {
              htmltools::div(class = "text-muted small text-center py-3",
                             i18n$t("action_plan_kanban_empty_col"))
            } else {
              htmltools::tagList(
                lapply(seq_len(nrow(sub)), function(i) build_card(sub[i, ]))
              )
            }
          )
        )
      }

      # Top row: the 4 active workflow stages side by side.
      # Bottom row: "Abandonnée" full-width as a separate, less prominent
      # archive lane.
      top_statuses <- c("proposee", "validee", "planifiee", "realisee")
      top_cols <- lapply(top_statuses, build_col, body_max_height = "55vh")
      bottom_col <- build_col("abandonnee", body_max_height = "30vh")

      # Inline init script: rebinds SortableJS to the freshly rendered
      # column bodies. Re-runs on every renderUI invalidation (plan_rv
      # update or kanban_render_token bump after a refused drop) so old
      # bindings are torn down inside initKanbanSortable() to avoid
      # double-firing onAdd.
      board_id   <- session$ns("kanban_board")
      drop_input <- session$ns("kanban_drop")
      edit_input <- session$ns("kanban_edit_request")
      init_script <- htmltools::tags$script(htmltools::HTML(sprintf(
        "if (window.initKanbanSortable) { window.initKanbanSortable(%s, %s, %s); }",
        jsonlite::toJSON(board_id, auto_unbox = TRUE),
        jsonlite::toJSON(drop_input, auto_unbox = TRUE),
        jsonlite::toJSON(edit_input, auto_unbox = TRUE)
      )))

      htmltools::tagList(
        do.call(bslib::layout_columns,
                c(list(col_widths = rep(3L, length(top_statuses)),
                       fillable = TRUE),
                  top_cols)),
        htmltools::div(class = "mt-3", bottom_col),
        init_script
      )
    })

    # ============================================================
    # KANBAN drag-and-drop dispatch (SortableJS in
    # inst/app/www/js/action_plan_kanban.js).
    # ============================================================
    #
    # Payload from JS: list(action_id, target_status, source_status,
    # nonce). The `nonce` defeats Shiny input deduplication so
    # consecutive drops between the same two columns still fire.
    shiny::observeEvent(input$kanban_drop, {
      if (deny_if_readonly()) {
        # Force a re-render so the dropped card snaps back visually.
        kanban_render_token(isolate(kanban_render_token()) + 1L)
        return()
      }
      payload <- input$kanban_drop
      i18n <- get_i18n(app_state$language)
      action_id <- payload$action_id
      target <- payload$target_status
      if (is.null(action_id) || is.null(target)) return()

      cur_plan <- plan_rv()
      idx <- find_action_index(cur_plan, action_id)
      if (is.na(idx)) {
        kanban_render_token(kanban_render_token() + 1L)
        return()
      }
      from <- cur_plan$actions[[idx]]$statut %||% "proposee"
      if (identical(from, target)) {
        # Drop within the same column — no-op, but the DOM may have
        # reordered cards; a re-render restores canonical (year-sorted)
        # order.
        kanban_render_token(kanban_render_token() + 1L)
        return()
      }
      new_plan <- tryCatch(
        update_action_in_plan(cur_plan, action_id, list(statut = target),
                              ug_ids = ug_ids(),
                              user = Sys.info()[["user"]] %||% "user"),
        error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (is.null(new_plan)) {
        kanban_render_token(kanban_render_token() + 1L)
        return()
      }
      save_action_plan(app_state$current_project$id, new_plan)
      plan_rv(new_plan)
      shiny::showNotification(
        sprintf(i18n$t("action_plan_kanban_moved_fmt"),
                i18n$t(paste0("action_plan_status_", target))),
        type = "message", duration = 3
      )
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Open the edit modal pre-filled with the action's current values.
    # Primary use-case : free-form editing of `commentaire`
    # (cell-edit on the table était single-line et tronquait le texte).
    # Le modal expose aussi statut / priorité / année / commentaire en
    # textarea multi-ligne.
    #
    # v0.52.10 — Extrait en helper pour être appelé depuis :
    #   * `input$kanban_edit_request` (dblclick sur carte kanban)
    #   * `input$row_edit_request`   (dblclick sur cellule
    #                                  commentaire du tableau DT)
    .open_action_edit_modal <- function(action_id) {
      if (deny_if_readonly()) return()
      if (is.null(action_id) || !nzchar(action_id)) return()
      cur_plan <- plan_rv()
      idx <- find_action_index(cur_plan, action_id)
      if (is.na(idx)) return()
      act <- cur_plan$actions[[idx]]
      i18n <- get_i18n(app_state$language)
      kanban_edit_id_rv(action_id)

      base_year <- PLAN_BASE_YEAR()
      annee_value <- if (!is.null(act$annee_cible) && !is.na(act$annee_cible))
        base_year + as.integer(act$annee_cible) else NA_integer_

      status_choices <- stats::setNames(
        ACTION_PLAN_STATUTS,
        vapply(ACTION_PLAN_STATUTS, function(s)
          i18n$t(paste0("action_plan_status_", s)), character(1))
      )

      shiny::showModal(shiny::modalDialog(
        title = i18n$t("action_plan_kanban_edit_title"),
        easyClose = TRUE,
        size = "l",
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          shiny::selectInput(
            ns("kanban_edit_statut"),
            label = i18n$t("action_plan_col_statut"),
            choices = status_choices,
            selected = act$statut %||% "proposee"
          ),
          shiny::selectInput(
            ns("kanban_edit_priorite"),
            label = i18n$t("action_plan_col_priorite"),
            choices = ACTION_PLAN_PRIORITES,
            selected = act$priorite %||% "moyenne"
          ),
          shiny::numericInput(
            ns("kanban_edit_annee"),
            label = i18n$t("action_plan_col_annee"),
            value = annee_value,
            min = base_year, step = 1L
          )
        ),
        shiny::textAreaInput(
          ns("kanban_edit_commentaire"),
          label = i18n$t("action_plan_col_commentaire"),
          value = act$commentaire %||% "",
          rows = 6, width = "100%",
          resize = "vertical"
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n$t("cancel")),
          shiny::actionButton(
            ns("kanban_edit_save"),
            label = i18n$t("action_plan_kanban_edit_save"),
            icon = shiny::icon("save"),
            class = "btn-primary"
          )
        )
      ))
    }

    # Observers : dblclick sur carte kanban ET dblclick sur cellule
    # commentaire du tableau DT déclenchent le même modal.
    shiny::observeEvent(input$kanban_edit_request, {
      .open_action_edit_modal(input$kanban_edit_request$action_id)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    shiny::observeEvent(input$row_edit_request, {
      .open_action_edit_modal(input$row_edit_request$action_id)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    shiny::observeEvent(input$kanban_edit_save, {
      if (deny_if_readonly()) return()
      i18n <- get_i18n(app_state$language)
      action_id <- kanban_edit_id_rv()
      if (is.null(action_id) || !nzchar(action_id)) return()
      cur_plan <- plan_rv()
      idx <- find_action_index(cur_plan, action_id)
      if (is.na(idx)) return()

      base_year <- PLAN_BASE_YEAR()
      annee_input <- input$kanban_edit_annee
      annee_cible <- if (is.null(annee_input) ||
                         is.na(suppressWarnings(as.integer(annee_input))))
        NULL
      else as.integer(annee_input) - base_year

      updates <- list(
        statut      = input$kanban_edit_statut,
        priorite    = input$kanban_edit_priorite,
        commentaire = input$kanban_edit_commentaire %||% ""
      )
      if (!is.null(annee_cible)) updates$annee_cible <- annee_cible

      new_plan <- tryCatch(
        update_action_in_plan(cur_plan, action_id, updates,
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
      kanban_edit_id_rv(NULL)
      shiny::removeModal()
      shiny::showNotification(i18n$t("action_plan_kanban_edit_saved"),
                              type = "message", duration = 3)
    })

    # ============================================================
    # S10 - Send observation actions to the Field tab as samples
    # ============================================================

    shiny::observeEvent(input$export_terrain, {
      if (deny_if_readonly()) return()
      i18n <- get_i18n(app_state$language)
      project <- app_state$current_project
      if (is.null(project)) {
        shiny::showNotification(i18n$t("no_project"), type = "warning")
        return()
      }
      plan <- plan_rv()
      if (is.null(plan) || length(plan$actions) == 0L) {
        shiny::showNotification(i18n$t("action_plan_export_terrain_empty"),
                                type = "warning", duration = 6)
        return()
      }
      obs <- Filter(function(a) identical(a$type, "observation"),
                    plan$actions)
      if (length(obs) == 0L) {
        shiny::showNotification(i18n$t("action_plan_export_terrain_no_obs"),
                                type = "warning", duration = 6)
        return()
      }
      sf_ug <- ug_sf_4326()
      if (is.null(sf_ug) || nrow(sf_ug) == 0L) return()

      # Build POINT sf at UGF centroid for each observation action
      centroids <- tryCatch(
        sf::st_centroid(sf::st_geometry(sf_ug)),
        error = function(e) NULL
      )
      if (is.null(centroids)) return()
      centroid_map <- stats::setNames(
        seq_len(nrow(sf_ug)), as.character(sf_ug$ug_id)
      )
      rows <- lapply(obs, function(a) {
        idx <- centroid_map[as.character(a$ug_id)]
        if (is.na(idx)) return(NULL)
        list(
          plot_id = paste0("obs_", a$id),
          action_id = a$id,
          ug_id = a$ug_id,
          type = a$type,
          annee_cible = as.integer(a$annee_cible %||% NA_integer_),
          priorite = a$priorite %||% NA_character_,
          geom = centroids[[idx]]
        )
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0L) return()

      df <- data.frame(
        plot_id     = vapply(rows, function(r) r$plot_id, character(1)),
        action_id   = vapply(rows, function(r) r$action_id, character(1)),
        ug_id       = vapply(rows, function(r) r$ug_id, character(1)),
        type        = vapply(rows, function(r) r$type, character(1)),
        annee_cible = vapply(rows, function(r) r$annee_cible, integer(1)),
        priorite    = vapply(rows, function(r) r$priorite, character(1)),
        stringsAsFactors = FALSE
      )
      sf_pts <- sf::st_sf(df,
                          geometry = sf::st_sfc(lapply(rows, function(r) r$geom),
                                                crs = sf::st_crs(sf_ug)))

      # Write to the dedicated "observations" layer so we never clobber
      # the Base/Over calibration plots produced by mod_sampling (which
      # live in the default "plots" layer of the same samples.gpkg).
      ok <- save_samples(project$id, sf_pts, layer = "observations")
      if (isTRUE(ok)) {
        # Bump the cross-module signal so mod_sampling refreshes.
        app_state$samples_refresh <- (app_state$samples_refresh %||% 0L) + 1L
        shiny::showNotification(
          sprintf(i18n$t("action_plan_export_terrain_ok_fmt"), nrow(sf_pts)),
          type = "message", duration = 6
        )
      } else {
        shiny::showNotification(i18n$t("action_plan_export_terrain_failed"),
                                type = "error", duration = 6)
      }
    })

    # ============================================================
    # S12 - GeoPackage export (respects DT visible-rows filtering)
    # ============================================================

    output$download_gpkg <- shiny::downloadHandler(
      filename = function() {
        project <- app_state$current_project
        base <- if (!is.null(project$metadata$name)) {
          gsub("[^a-zA-Z0-9_-]", "_", project$metadata$name)
        } else {
          "nemeton_action_plan"
        }
        paste0(base, "_action_plan.gpkg")
      },
      content = function(file) {
        i18n <- get_i18n(app_state$language)
        # The toast spinner is shown client-side by
        # nemetonShowDownloadToast on click and dismissed by the
        # paired nemetonHideDownloadToast message we always send at
        # the end of this content function (deferred via on.exit so
        # it fires even when an error short-circuits the rest).
        on.exit(
          session$sendCustomMessage("nemetonHideDownloadToast", list()),
          add = TRUE
        )

        plan <- plan_rv()
        sf_ug <- ug_sf_4326()
        if (is.null(plan) || is.null(sf_ug)) {
          writeLines("No data available", file)
          return()
        }
        # Honour the table's visible-rows filter if any.
        visible_ids <- actions_df()$id
        ok <- export_action_plan_gpkg(plan, sf_ug, file,
                                      filter_action_ids = visible_ids)
        if (!isTRUE(ok)) {
          shiny::showNotification(i18n$t("action_plan_export_terrain_failed"),
                                  type = "error", duration = NULL)
        }
      }
    )

    # ============================================================
    # S13 - Per-UGF PDF report
    # ============================================================

    output$download_pdf <- shiny::downloadHandler(
      filename = function() {
        project <- app_state$current_project
        base <- if (!is.null(project$metadata$name)) {
          gsub("[^a-zA-Z0-9_-]", "_", project$metadata$name)
        } else {
          "nemeton_action_plan"
        }
        paste0(base, "_action_plan.pdf")
      },
      content = function(file) {
        i18n <- get_i18n(app_state$language)
        # The toast spinner ("Génération PDF…") is shown client-side
        # by nemetonShowDownloadToast on click and is now sticky
        # (duration: null). on.exit guarantees the matching
        # nemetonHideDownloadToast is sent at the end of this
        # function so the spinner disappears synchronously with the
        # browser's save dialog — whether the export succeeds or
        # errors out.
        on.exit(
          session$sendCustomMessage("nemetonHideDownloadToast", list()),
          add = TRUE
        )

        plan <- plan_rv()
        project <- app_state$current_project
        sf_ug <- ug_sf_4326()
        if (is.null(plan) || is.null(project) || is.null(sf_ug)) {
          writeLines("No data available", file)
          return()
        }
        visible <- actions_df()
        result <- tryCatch(
          generate_action_plan_pdf(
            project = project, plan = plan, ug_sf = sf_ug,
            output_file = file,
            language = app_state$language %||% "fr",
            filter_action_ids = visible$id
          ),
          error = function(e) {
            # Log the real cause to the server console for ops and
            # also surface it to the user as a sticky error toast
            # so it is not missed amongst the (now sticky) generation
            # spinner. Quarto / LaTeX failures usually need the
            # message verbatim to diagnose.
            err_msg <- conditionMessage(e)
            cli::cli_warn("Action plan PDF export failed: {err_msg}")
            shiny::showNotification(
              paste(i18n$t("action_plan_pdf_failed"), ":", err_msg),
              type = "error", duration = NULL
            )
            NULL
          }
        )
        if (is.null(result)) {
          # The download response must produce a file. Writing a
          # plain-text marker keeps the symptom visible to the user
          # (the file won't open as a PDF) while the sticky error
          # toast above carries the actual diagnostic.
          writeLines(
            c("PDF generation failed.",
              "See the in-app error toast for the underlying cause."),
            file
          )
        } else {
          # Chemin succès uniquement (result non-NULL) : archive une copie dans
          # exports/ du projet, parité avec le rapport de synthèse. Best-effort,
          # ne casse jamais le download navigateur (cf. .archive_action_plan_pdf).
          .archive_action_plan_pdf(file, project)
        }
      }
    )

    # ============================================================
    # S11 - When mod_field_ingest reports a successful import, flip
    # observation actions to "realisee" for the imported UGFs.
    # ============================================================

    shiny::observeEvent(app_state$field_imported_at, {
      ts <- app_state$field_imported_at
      if (is.null(ts)) return()
      project <- app_state$current_project
      cur_plan <- plan_rv()
      if (is.null(project) || is.null(cur_plan)) return()
      if (length(cur_plan$actions) == 0L) return()

      imported_ugs <- app_state$field_imported_ugs %||% character()

      n_ok <- 0L
      for (a in cur_plan$actions) {
        if (!identical(a$type, "observation")) next
        if (!(a$ug_id %in% imported_ugs)) next
        from <- a$statut %||% "proposee"
        if (identical(from, "realisee") || identical(from, "abandonnee")) next
        # Allowed transitions toward "realisee": only from "planifiee".
        # If the action is in proposee / validee, walk through validee /
        # planifiee on the way to realisee so the audit logs the path.
        plan2 <- cur_plan
        steps <- character()
        if (from == "proposee")  steps <- c("validee", "planifiee", "realisee")
        if (from == "validee")   steps <- c("planifiee", "realisee")
        if (from == "planifiee") steps <- c("realisee")
        for (s in steps) {
          plan2 <- tryCatch(
            update_action_in_plan(plan2, a$id, list(statut = s),
                                  ug_ids = ug_ids(),
                                  user = "field_ingest"),
            error = function(e) plan2
          )
        }
        cur_plan <- plan2
        n_ok <- n_ok + 1L
      }
      if (n_ok > 0L) {
        save_action_plan(project$id, cur_plan)
        plan_rv(cur_plan)
        i18n <- get_i18n(app_state$language)
        shiny::showNotification(
          sprintf(i18n$t("action_plan_field_realised_fmt"), n_ok),
          type = "message", duration = 6
        )
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ============================================================
    # S7 - IA generation (all UGFs / current selection)
    # ============================================================

    # Build LLM context from project comments + current ug_ids.
    #
    # v0.52.9 \u2014 d\u00e9pendance explicite sur `app_state$comments_refresh`,
    # bump\u00e9 par mod_synthesis / mod_family apr\u00e8s chaque
    # `save_comments()`. Sans \u00e7a, le reactive lisait `load_comments()`
    # une seule fois au montage du module et restait sur le snapshot
    # initial (\u00ab pas de commentaires \u00bb) m\u00eame apr\u00e8s que l'utilisateur
    # ait g\u00e9n\u00e9r\u00e9 les commentaires via l'IA dans l'onglet Synth\u00e8se
    # \u2014 l'observer `input$generate_all` continuait \u00e0 refuser le
    # lancement IA Plan d'actions avec `action_plan_generate_no_comments`.
    plan_llm_context <- shiny::reactive({
      app_state$comments_refresh  # dep : relire les commentaires
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
      if (deny_if_readonly()) return()
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
      if (deny_if_readonly()) return()
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

    # The chat UI used to live in a modal triggered by `open_chat`;
    # it now lives in the persistent left sidebar (`chat_panel` in
    # the UI). The history / send / clear observers below operate
    # against the same input ids — they just no longer need to be
    # populated through showModal().

    output$chat_history_ui <- shiny::renderUI({
      hist <- chat_history_rv()
      # Inline script: re-attached on every render, scrolls the
      # parent .chat-history container to the bottom so the latest
      # message is visible without manual scrolling. setTimeout(0)
      # defers to the next tick so the DOM has the freshly-rendered
      # nodes when scrollHeight is measured.
      scroll_id <- session$ns("chat_history")
      scroll_script <- htmltools::tags$script(htmltools::HTML(sprintf(
        "setTimeout(function(){var el=document.getElementById(%s);if(el){el.scrollTop=el.scrollHeight;}},0);",
        jsonlite::toJSON(scroll_id, auto_unbox = TRUE)
      )))
      if (length(hist) == 0L) {
        i18n <- get_i18n(app_state$language)
        return(htmltools::tagList(
          htmltools::div(class = "text-muted small",
                         i18n$t("action_plan_chat_empty")),
          scroll_script
        ))
      }
      i18n_render <- get_i18n(app_state$language)
      htmltools::tagList(
        lapply(hist, function(msg) {
          # Translate the raw LLM role names ("user" / "assistant")
          # into the current UI language at display time. The
          # underlying data model keeps the English keys so the
          # prompt builder downstream stays unchanged.
          role_label <- if (identical(msg$role, "user"))
            i18n_render$t("action_plan_chat_role_user")
          else
            i18n_render$t("action_plan_chat_role_assistant")
          htmltools::div(
            class = paste0("p-2 mb-2 rounded ",
                           if (msg$role == "user") "bg-light" else "bg-success-subtle"),
            htmltools::tags$strong(role_label, " : "),
            htmltools::tags$pre(
              class = "mb-0",
              style = "white-space: pre-wrap; font-size: 0.85rem;",
              msg$text
            )
          )
        }),
        scroll_script
      )
    })

    shiny::observeEvent(input$chat_clear, {
      chat_history_rv(list())
    })

    shiny::observeEvent(input$chat_send, {
      if (deny_if_readonly()) return()
      i18n <- get_i18n(app_state$language)
      q <- trimws(input$chat_input %||% "")
      if (!nzchar(q)) return()
      ctx <- plan_llm_context()
      if (is.null(ctx)) return()

      # Resolve scope (all / selected) just like the generate flow:
      # narrow ctx$ug_ids to the targeted set so the prompt builder
      # only shows the relevant UGFs to the LLM. The chosen
      # target_ugs is stashed in rv_state to drive the optional
      # overwrite at apply time.
      sel_ugs <- selected_ug_rv()
      scope <- input$chat_scope %||% "all"
      target_ugs <- if (scope == "selected" && length(sel_ugs) > 0L) {
        sel_ugs
      } else {
        ctx$ug_ids
      }
      if (length(target_ugs) == 0L) {
        shiny::showNotification(i18n$t("action_plan_no_ug"),
                                type = "warning")
        return()
      }
      ctx$ug_ids <- target_ugs
      rv_state$pending_chat_target_ugs <- target_ugs

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

      # "L'IA r\u00e9fl\u00e9chit\u2026" toast at the bottom-right with a spinning
      # gear icon. `duration = NULL` + `closeButton = FALSE` keep it
      # visible until the LLM round-trip ends; `on.exit` guarantees
      # it is removed regardless of the return path (success, error,
      # early-return after a failed parse, etc.).
      thinking_id <- shiny::showNotification(
        htmltools::tagList(
          shiny::icon("gear", class = "fa-spin"),
          " ",
          i18n$t("action_plan_chat_thinking")
        ),
        duration = NULL,
        closeButton = FALSE,
        type = "default"
      )
      on.exit(shiny::removeNotification(thinking_id), add = TRUE)

      # Send the full action payload (including the `quantite` block:
      # volume, surface, nb_tiges, rdi, cout, revenu) so the LLM can
      # preserve those values when emitting an updated action. The
      # previous stripped summary forced the LLM to re-invent or null
      # them out, which left the refined plan rows empty.
      cur_actions_json <- jsonlite::toJSON(
        lapply(plan_rv()$actions, function(a) {
          list(
            id             = a$id,
            ug_id          = a$ug_id,
            type           = a$type,
            type_libre     = a$type_libre,
            intensite      = a$intensite,
            annee_cible    = a$annee_cible,
            duree          = a$duree,
            priorite       = a$priorite,
            statut         = a$statut,
            objectifs_lies = a$objectifs_lies,
            quantite       = a$quantite,
            commentaire    = a$commentaire
          )
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
        # Surface the overwrite intent in the apply modal: when the
        # user toggled `chat_overwrite` ON in the chat panel, every
        # existing action for the targeted UGFs will be wiped before
        # the new ones are inserted. We show a warning paragraph so
        # the user can still cancel.
        overwrite_hint <- if (isTRUE(input$chat_overwrite)) {
          shiny::p(class = "text-warning small",
                   sprintf(i18n$t("action_plan_chat_apply_overwrite_warn_fmt"),
                           length(target_ugs)))
        } else NULL
        shiny::showModal(shiny::modalDialog(
          title = i18n$t("action_plan_chat_apply_title"),
          size = "m", easyClose = TRUE,
          shiny::p(sprintf(i18n$t("action_plan_chat_apply_fmt"),
                           length(parsed$actions))),
          overwrite_hint,
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
      if (deny_if_readonly()) return()
      acts <- rv_state$pending_chat_actions
      if (is.null(acts) || length(acts) == 0L) return()
      i18n <- get_i18n(app_state$language)
      project <- app_state$current_project
      cur_plan <- plan_rv()
      # Optional overwrite: drop existing actions for the UGFs that
      # were targeted at send time (stashed in pending_chat_target_ugs)
      # before the bulk upsert. Mirrors the gen_run flow.
      target_ugs <- rv_state$pending_chat_target_ugs %||% character(0)
      if (isTRUE(input$chat_overwrite) && length(target_ugs) > 0L) {
        cur_plan$actions <- Filter(
          function(a) !(a$ug_id %in% target_ugs), cur_plan$actions
        )
      }
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
      rv_state$pending_chat_target_ugs <- NULL
      shiny::showNotification(
        sprintf(i18n$t("action_plan_chat_applied_fmt"), length(acts)),
        type = "message", duration = 4
      )
    })

    # ============================================================
    # S8 - History modal (bulk status transitions are now exclusively
    # driven by the Kanban board's drag-and-drop -- see kanban_drop)
    # ============================================================

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
      if (deny_if_readonly()) return()
      i18n <- get_i18n(app_state$language)
      project <- app_state$current_project
      if (is.null(project)) return()
      sel_ugs <- selected_ug_rv()

      # Map ug_id → human-readable label so the dropdown shows
      # "Parcelle 12 — La Lande" instead of the raw "ugf_42" id.
      sf <- ug_sf_4326()
      if (!is.null(sf) && nrow(sf) > 0L) {
        ids <- as.character(sf$ug_id)
        labels <- as.character(sf$label %||% sf$ug_id)
        ord <- order(labels, na.last = TRUE)
        ug_choices <- stats::setNames(ids[ord], labels[ord])
      } else {
        ug_choices <- ug_ids()
      }

      base_year <- PLAN_BASE_YEAR()
      horizon   <- plan_rv()$horizon_annees %||% 20L

      shiny::showModal(shiny::modalDialog(
        title = i18n$t("action_plan_add_title"), size = "m",
        easyClose = TRUE,
        shiny::selectInput(ns("add_ug"), i18n$t("action_plan_ug"),
                           choices = ug_choices,
                           selected = if (length(sel_ugs) == 1L) sel_ugs[1] else NULL),
        shiny::selectInput(ns("add_type"), i18n$t("action_plan_type"),
                           choices = ACTION_PLAN_TYPES,
                           selected = "observation"),
        # Real calendar year (e.g. 2027), not the internal offset (1).
        # Stored as an offset = year - base_year on save below.
        shiny::numericInput(ns("add_year"), i18n$t("action_plan_year"),
                            value = base_year + 1L,
                            min = base_year + 1L,
                            max = base_year + as.integer(horizon),
                            step = 1L),
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
      if (deny_if_readonly()) return()
      i18n <- get_i18n(app_state$language)
      base_year <- PLAN_BASE_YEAR()
      year_input <- suppressWarnings(as.integer(input$add_year))
      annee_cible <- if (is.na(year_input)) NA_integer_
                     else year_input - base_year
      action <- list(
        ug_id = input$add_ug,
        type = input$add_type,
        annee_cible = annee_cible,
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

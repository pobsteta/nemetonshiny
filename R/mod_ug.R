#' UG (Management Units) Module
#'
#' @description
#' Shiny module for managing Unités de Gestion (UG).
#' Allows users to view, merge, split, rename, and classify UGs.
#' Walking skeleton: table-based interface with merge/split/rename actions.
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

      # UG detail panel
      htmltools::div(
        id = ns("detail_panel"),
        shiny::h6(i18n$t("ug_composition")),
        shiny::uiOutput(ns("ug_detail"))
      )
    ),

    # === Main content: UG table ===
    bslib::card(
      bslib::card_header(
        htmltools::div(
          class = "d-flex justify-content-between align-items-center",
          shiny::h5(i18n$t("ug_title"), class = "mb-0"),
          shiny::textOutput(ns("ug_summary"), inline = TRUE)
        )
      ),
      bslib::card_body(
        DT::dataTableOutput(ns("ug_table"))
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
      projet_ug = NULL,       # projet list with $atomes, $ugs, $parcels
      needs_recompute = FALSE  # flag: UGs changed, indicators need refresh
    )

    # Initialize UG data when project loads
    shiny::observe({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$parcels)) {
        rv$projet_ug <- NULL
        return()
      }

      # Try to load existing UG data
      if (has_ug_data(project)) {
        rv$projet_ug <- project
      } else if (!is.null(project$metadata$id)) {
        # Try loading from files or migrate
        projet <- ensure_project_migrated(project$metadata$id, project)
        rv$projet_ug <- projet
        # Update app_state with UG data
        app_state$current_project$atomes <- projet$atomes
        app_state$current_project$ugs <- projet$ugs
      }
    }) |> shiny::bindEvent(app_state$current_project)

    # ================================================================
    # REACTIVE: UG listing for the table
    # ================================================================
    ug_listing <- shiny::reactive({
      projet <- rv$projet_ug
      if (is.null(projet) || !has_ug_data(projet)) {
        return(NULL)
      }
      ug_list(projet)
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

        htmltools::tagList(
          shiny::tags$strong(listing$label[sel]),
          shiny::br(),
          shiny::tags$span(
            class = "text-muted",
            sprintf("%s ha | %d atome(s)", surface_ha, listing$n_atomes[sel])
          ),
          if (!is.na(listing$groupe[sel])) {
            shiny::tags$span(
              class = "badge bg-primary ms-1",
              listing$groupe[sel]
            )
          },
          shiny::hr(),
          if (nrow(refs) > 0) {
            shiny::tags$ul(
              lapply(seq_len(nrow(refs)), function(i) {
                shiny::tags$li(sprintf(
                  "%s (section %s, n\u00b0%s) - %s m\u00b2",
                  refs$geo_parcelle[i],
                  refs$section[i],
                  refs$numero[i],
                  format(refs$surface_m2[i], big.mark = " ")
                ))
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
    # ACTION: Merge UGs
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

      # Show modal to get new label
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

        # Save and update state
        if (!is.null(projet$metadata$id)) {
          save_ug_data(projet$metadata$id, projet)
        }
        rv$projet_ug <- projet
        rv$needs_recompute <- TRUE
        app_state$current_project$atomes <- projet$atomes
        app_state$current_project$ugs <- projet$ugs

        shiny::showNotification(
          sprintf("%s : %d UG → 1", i18n()$t("ug_merge"), length(ug_ids)),
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
        shiny::showNotification(
          i18n()$t("ug_select_one_to_split"),
          type = "warning"
        )
        return()
      }

      uid <- listing$ug_id[sel]
      n_atomes <- listing$n_atomes[sel]

      if (n_atomes < 2) {
        shiny::showNotification(
          i18n()$t("ug_cannot_split_single"),
          type = "warning"
        )
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
        shiny::showNotification(
          i18n()$t("ug_select_one"),
          type = "warning"
        )
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
        shiny::showNotification(
          i18n()$t("ug_select_one"),
          type = "warning"
        )
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
        shiny::showNotification(
          i18n()$t("ug_no_indicators"),
          type = "warning"
        )
        return()
      }

      tryCatch({
        shiny::showNotification(
          i18n()$t("ug_recomputing"),
          type = "message",
          id = "ug_recompute_notif"
        )

        # Aggregate indicators to UG level
        indicators_ug <- aggregate_indicators_to_ug(project$indicators, projet)

        if (!is.null(indicators_ug)) {
          # Save UG indicators
          if (!is.null(project$metadata$id)) {
            save_indicators_ug(project$metadata$id, indicators_ug)
          }

          # Update app state
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
        shiny::showNotification(
          paste("Erreur :", e$message),
          type = "error"
        )
      })
    })

  })
}

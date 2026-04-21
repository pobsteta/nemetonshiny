#' Progress Module for nemetonApp
#'
#' @description
#' Shiny module for displaying computation progress.
#' Shows download and computation status with detailed progress.
#'
#' @name mod_progress
#' @keywords internal
NULL


#' Progress Module UI
#'
#' @param id Module namespace ID.
#'
#' @return Shiny UI elements.
#'
#' @noRd
mod_progress_ui <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language)

  htmltools::tagList(
    # Progress card (hidden by default, shown via JavaScript)
    # Sidebar: global progress bar + indicator counters only
    # Detailed task messages go to toast notifications (bottom-right)
    htmltools::div(
      id = ns("progress_card_wrapper"),
      style = "display: none;",
      bslib::card(
        id = ns("progress_card"),
        class = "border-primary",
        bslib::card_header(
          class = "bg-primary text-white d-flex align-items-center",
          bsicons::bs_icon("cpu", class = "me-2"),
          htmltools::span(
            id = ns("progress_title"),
            i18n$t("computing")
          )
        ),
        bslib::card_body(
          # Phase indicator
          htmltools::div(
            class = "mb-3",
            htmltools::tags$small(
              class = "text-muted",
              htmltools::span(id = ns("phase_text"))
            )
          ),

          # Main progress bar
          htmltools::div(
            class = "mb-3",
            shiny::tags$label(
              class = "form-label d-flex justify-content-between",
              htmltools::span(i18n$t("progress_overall")),
              htmltools::span(
                id = ns("progress_percent"),
                class = "text-primary fw-bold",
                "0%"
              )
            ),
            htmltools::div(
              class = "progress",
              style = "height: 24px;",
              htmltools::div(
                id = ns("progress_bar"),
                class = "progress-bar progress-bar-striped progress-bar-animated",
                role = "progressbar",
                style = "width: 0%;",
                `aria-valuenow` = "0",
                `aria-valuemin` = "0",
                `aria-valuemax` = "100"
              )
            )
          ),

          # Indicators summary
          htmltools::div(
            class = "d-flex justify-content-between mb-2",
            htmltools::div(
              class = "text-success",
              bsicons::bs_icon("check-circle", class = "me-1"),
              htmltools::span(id = ns("completed_count"), "0"),
              " ",
              i18n$t("completed")
            ),
            htmltools::div(
              class = "text-danger",
              bsicons::bs_icon("x-circle", class = "me-1"),
              htmltools::span(id = ns("failed_count"), "0"),
              " ",
              i18n$t("failed")
            ),
            htmltools::div(
              class = "text-muted",
              bsicons::bs_icon("clock", class = "me-1"),
              htmltools::span(id = ns("pending_count"), "0"),
              " ",
              i18n$t("pending")
            )
          )
        ),
        bslib::card_footer(
          class = "d-flex justify-content-between align-items-center",
          htmltools::div(
            id = ns("elapsed_time"),
            class = "text-muted small"
          ),
          shiny::actionButton(
            ns("cancel"),
            label = i18n$t("cancel"),
            icon = shiny::icon("times"),
            class = "btn-outline-danger btn-sm"
          )
        )
      )
    ),

    # Completion card (shown when done, via JavaScript)
    # Auto-hides after 5 seconds - main "View Results" button is in the interface
    htmltools::div(
      id = ns("complete_card_wrapper"),
      style = "display: none;",
      bslib::card(
        id = ns("complete_card"),
        class = "border-success",
        bslib::card_header(
          class = "bg-success text-white d-flex align-items-center",
          bsicons::bs_icon("check-circle", class = "me-2"),
          i18n$t("computation_complete")
        ),
        bslib::card_body(
          htmltools::div(
            class = "text-center py-2",
            htmltools::p(
              class = "mb-2",
              htmltools::span(id = ns("completion_message_text"))
            )
          ),
          htmltools::div(
            id = ns("indicators_table_container"),
            style = "max-height: 300px; overflow-y: auto;"
          )
        )
      )
    ),

    # Fixed toast for task notifications (bottom-right, no DOM churn)
    htmltools::div(
      id = ns("task_toast"),
      class = "position-fixed bottom-0 end-0 p-3",
      style = "z-index: 1080; display: none;",
      htmltools::div(
        class = "toast show align-items-center border-0",
        id = ns("task_toast_inner"),
        role = "status",
        `aria-live` = "polite",
        htmltools::div(
          class = "d-flex",
          htmltools::div(
            class = "toast-body",
            htmltools::span(id = ns("task_toast_icon"), class = "me-2"),
            htmltools::strong(id = ns("task_toast_text"))
          ),
          htmltools::tags$button(
            type = "button",
            class = "btn-close me-2 m-auto",
            `aria-label` = "Close",
            onclick = sprintf(
              "document.getElementById('%s').style.display='none';",
              ns("task_toast")
            )
          )
        )
      )
    ),

    # Error card (shown on fatal error, via JavaScript)
    htmltools::div(
      id = ns("error_card_wrapper"),
      style = "display: none;",
      bslib::card(
        id = ns("error_card"),
        class = "border-danger",
        bslib::card_header(
          class = "bg-danger text-white d-flex align-items-center",
          bsicons::bs_icon("exclamation-triangle", class = "me-2"),
          i18n$t("computation_error")
        ),
        bslib::card_body(
          htmltools::div(id = ns("error_message_content")),
          htmltools::div(
            class = "mt-3",
            shiny::actionButton(
              ns("retry"),
              label = i18n$t("retry"),
              icon = shiny::icon("redo"),
              class = "btn-outline-danger"
            )
          )
        )
      )
    )
  )
}


#' Progress Module Server
#'
#' @param id Character. Module namespace ID.
#' @param compute_state Reactive. Computation state object.
#' @param app_state Reactive values. Application state.
#'
#' @return List with reactive values for module state.
#'
#' @noRd
mod_progress_server <- function(id, compute_state, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get translations
    opts <- get_app_options()
    lang <- opts$language %||% "fr"
    i18n <- get_i18n(lang)

    # Local state
    rv <- shiny::reactiveValues(
      show_progress = FALSE,
      show_complete = FALSE,
      show_error = FALSE
    )

    # Plain environment for tracking state across polling calls.
    # Using a non-reactive env so send_running_update() can be called
    # from later::later without triggering reactive flush / busy state.
    tracking <- new.env(parent = emptyenv())
    tracking$start_time <- NULL
    tracking$last_task <- ""
    tracking$last_notif_id <- NULL
    tracking$last_error_count <- 0

    # ========================================
    # Visibility outputs (removed - unused, caused unnecessary reactivity)
    # Visibility is managed via JavaScript showElement/hideElement in mod_home.R
    # ========================================

    # ========================================
    # Progress updates (all via JavaScript to avoid re-renders)
    # ========================================

    # Translate a task identifier into a user-facing status line.
    # Delegates to the canonical translate_task_message() so that
    # new task kinds (CHM phases, Open-Canopy tiles, inference
    # tiles) are handled in one place. The extra paste() fallback
    # on an unrecognised task preserves the legacy behaviour:
    # instead of leaking a raw "chm_tile:rvb:2:28" to the UI,
    # show the base "Calcul en cours…" label with the task
    # string appended (useful for debugging).
    translate_task <- function(task) {
      out <- translate_task_message(task, i18n)
      if (identical(out, task)) {
        # translate_task_message returned the raw task → unknown kind.
        if (nzchar(task)) {
          return(paste(i18n$t("computing"), task))
        }
        return("")
      }
      out
    }

    # Helper: translate indicator name for the summary table
    translate_indicator_name <- function(key) {
      # Map internal names to readable French labels grouped by family
      indicator_labels <- list(
        indicateur_c1_biomasse = "Carbone - Biomasse",
        indicateur_c2_ndvi = "Carbone - NDVI",
        indicateur_b1_protection = "Biodiversit\u00e9 - Protection",
        indicateur_b2_structure = "Biodiversit\u00e9 - Structure",
        indicateur_b3_connectivite = "Biodiversit\u00e9 - Connectivit\u00e9",
        indicateur_w1_reseau = "Eau - R\u00e9seau hydrographique",
        indicateur_w2_zones_humides = "Eau - Zones humides",
        indicateur_w3_humidite = "Eau - Indice topographique",
        indicateur_a1_couverture = "Air - Couvert forestier tampon",
        indicateur_a2_qualite_air = "Air - Qualit\u00e9 de l'air",
        indicateur_f1_fertilite = "Fertilit\u00e9 - Sol",
        indicateur_f2_erosion = "Fertilit\u00e9 - \u00c9rosion",
        indicateur_l2_fragmentation = "Paysage - Fragmentation",
        indicateur_l1_sylvosphere = "Paysage - Ratio bordure",
        indicateur_t1_anciennete = "Temporel - Anciennet\u00e9",
        indicateur_t2_changement = "Temporel - Changement",
        indicateur_r1_feu = "Risque - Incendie",
        indicateur_r2_tempete = "Risque - Temp\u00eate",
        indicateur_r3_secheresse = "Risque - S\u00e9cheresse",
        indicateur_r4_abroutissement = "Risque - Abroutissement",
        indicateur_s1_routes = "Social - Sentiers",
        indicateur_s2_bati = "Social - Accessibilit\u00e9",
        indicateur_s3_population = "Social - Population",
        indicateur_p1_volume = "Production - Volume",
        indicateur_p2_station = "Production - Productivit\u00e9",
        indicateur_p3_qualite_bois = "Production - Qualit\u00e9",
        indicateur_e1_bois_energie = "Energie - Bois-\u00e9nergie",
        indicateur_e2_evitement = "Energie - CO2 \u00e9vit\u00e9",
        indicateur_n1_distance = "Naturalit\u00e9 - Distance",
        indicateur_n2_continuite = "Naturalit\u00e9 - Continuit\u00e9",
        indicateur_n3_naturalite = "Naturalit\u00e9 - Score"
      )
      indicator_labels[[key]] %||% key
    }

    # Helper: translate phase
    translate_phase <- function(phase) {
      switch(
        phase %||% "",
        "init" = i18n$t("phase_init"),
        "downloading" = i18n$t("phase_downloading"),
        "computing" = i18n$t("phase_computing"),
        "complete" = i18n$t("phase_complete"),
        ""
      )
    }

    # ========================================
    # send_running_update(): push progress to UI via sendCustomMessage.
    # Called from later::later polling loop in mod_home.R — runs OUTSIDE
    # the reactive system so it does NOT trigger Shiny busy state.
    # ========================================
    send_running_update <- function(state) {
      # First call: initialize elapsed timer
      if (is.null(tracking$start_time)) {
        tracking$start_time <- Sys.time()
        tracking$last_task <- ""
        tracking$last_notif_id <- NULL
        tracking$last_error_count <- 0
        session$sendCustomMessage("startElapsedTimer", list(
          id = ns("elapsed_time"),
          label = paste0(i18n$t("elapsed_time"), ": ")
        ))
      }

      # Sidebar: global progress bar + counters (via JavaScript)
      progress <- state$progress %||% 0
      progress_max <- state$progress_max %||% 1
      if (length(progress) != 1 || is.na(progress)) progress <- 0
      if (length(progress_max) != 1 || is.na(progress_max) || progress_max == 0) progress_max <- 1
      progress_pct <- round(progress / progress_max * 100)

      session$sendCustomMessage("updateProgressBar", list(
        barId = ns("progress_bar"),
        percentId = ns("progress_percent"),
        percent = progress_pct
      ))

      # Phase text (sidebar)
      session$sendCustomMessage("updateText", list(
        id = ns("phase_text"),
        text = translate_phase(state$phase)
      ))

      # Counters (sidebar)
      n_completed <- state$indicators_completed %||% 0
      n_failed <- state$indicators_failed %||% 0
      n_total <- state$indicators_total %||% 0
      if (length(n_completed) != 1 || is.na(n_completed)) n_completed <- 0
      if (length(n_failed) != 1 || is.na(n_failed)) n_failed <- 0
      if (length(n_total) != 1 || is.na(n_total)) n_total <- 0

      session$sendCustomMessage("updateText", list(
        id = ns("completed_count"),
        text = as.character(n_completed)
      ))
      session$sendCustomMessage("updateText", list(
        id = ns("failed_count"),
        text = as.character(n_failed)
      ))

      pending <- n_total - n_completed - n_failed
      session$sendCustomMessage("updateText", list(
        id = ns("pending_count"),
        text = as.character(pending)
      ))

      # Fixed toast notification (bottom-right) for task changes
      current_task <- state$current_task %||% ""
      if (length(current_task) != 1 || is.na(current_task)) current_task <- ""
      if (current_task != "" && current_task != tracking$last_task) {
        tracking$last_task <- current_task
        task_text <- translate_task(current_task)

        toast_type <- if (identical(current_task, "initializing")) "initializing"
                      else if (grepl("^download", current_task)) "info"
                      else "info"
        # Initialising stays visible until replaced by a real task.
        toast_duration <- if (identical(current_task, "initializing")) 0
                          else if (grepl("download_oso_progress", current_task)) 8000
                          else 4000

        session$sendCustomMessage("updateTaskToast", list(
          wrapperId = ns("task_toast"),
          innerId = ns("task_toast_inner"),
          textId = ns("task_toast_text"),
          iconId = ns("task_toast_icon"),
          visible = TRUE,
          text = task_text,
          type = toast_type,
          duration = toast_duration
        ))
      }

      # Error toast: show errors in the same fixed toast (warning style)
      n_errors <- length(state$errors)
      if (n_errors > tracking$last_error_count) {
        last_err <- state$errors[[n_errors]]
        prefix <- if (!is.null(last_err$indicator)) {
          paste0(last_err$indicator, ": ")
        } else if (!is.null(last_err$source)) {
          paste0(last_err$source, ": ")
        } else {
          ""
        }
        session$sendCustomMessage("updateTaskToast", list(
          wrapperId = ns("task_toast"),
          innerId = ns("task_toast_inner"),
          textId = ns("task_toast_text"),
          iconId = ns("task_toast_icon"),
          visible = TRUE,
          text = paste0(prefix, last_err$message),
          type = "warning",
          duration = 8000
        ))
        tracking$last_error_count <- n_errors
      }
    }

    # Reset tracking state for a new computation
    reset_tracking <- function() {
      tracking$start_time <- NULL
      tracking$last_task <- ""
      tracking$last_notif_id <- NULL
      tracking$last_error_count <- 0
    }

    # ========================================
    # Observer for terminal states only (completed, error).
    # These are one-time events so a single reactive flush is acceptable.
    # Running-state updates are handled by send_running_update() above.
    # ========================================
    shiny::observe({
      state <- compute_state()
      shiny::req(state)

      status <- state$status
      if (is.null(status) || length(status) != 1 || is.na(status)) return()

      new_show_complete <- identical(status, COMPUTE_STATUS$COMPLETED)
      new_show_error <- identical(status, COMPUTE_STATUS$ERROR)

      # Skip running states — handled by send_running_update via later::later
      if (!new_show_complete && !new_show_error) return()

      shiny::isolate({
        if (new_show_complete && !identical(rv$show_complete, TRUE)) {
          rv$show_complete <- TRUE
          rv$show_progress <- FALSE

          # Hide task toast
          session$sendCustomMessage("updateTaskToast", list(
            wrapperId = ns("task_toast"),
            visible = FALSE
          ))
          completed <- state$indicators_completed %||% 0L
          total <- state$indicators_total %||% 0L
          failed <- state$indicators_failed %||% 0L
          if (length(completed) != 1 || is.na(completed)) completed <- 0L
          if (length(total) != 1 || is.na(total)) total <- 0L
          if (length(failed) != 1 || is.na(failed)) failed <- 0L
          summary_text <- sprintf(
            i18n$t("computation_summary"),
            completed,
            total
          )
          if (failed > 0) {
            summary_text <- paste0(
              summary_text, " (",
              sprintf(i18n$t("computation_failed"), failed),
              ")"
            )
          }
          session$sendCustomMessage("updateText", list(
            id = ns("completion_message_text"),
            text = summary_text
          ))

          # Build indicator status table
          ind_status <- state$indicators_status
          if (!is.null(ind_status) && length(ind_status) > 0) {
            ind_names <- names(ind_status)
            indicators_list <- lapply(ind_names, function(nm) {
              s <- ind_status[[nm]]
              if (is.null(s) || length(s) != 1) s <- "pending"
              list(
                name = translate_indicator_name(nm),
                status = as.character(s)
              )
            })
            session$sendCustomMessage("updateIndicatorsTable", list(
              containerId = ns("indicators_table_container"),
              indicators = indicators_list,
              labels = list(
                indicator = i18n$t("indicator_column"),
                status = i18n$t("status_column"),
                completed = i18n$t("status_ok"),
                failed = i18n$t("status_error"),
                pending = i18n$t("status_not_computed")
              )
            ))
          }

          # Notification sync PostGIS si configuree
          if (is_db_configured()) {
            db_msg <- if (i18n$language == "fr") {
              "Projet synchronis\u00e9 avec la base de donn\u00e9es PostGIS"
            } else {
              "Project synced to PostGIS database"
            }
            shiny::showNotification(
              htmltools::tagList(shiny::icon("database", class = "me-1"), db_msg),
              type = "message", duration = 5
            )
          }

          # Reset tracking for next computation
          reset_tracking()
          session$sendCustomMessage("stopElapsedTimer", list())
        }

        if (new_show_error && !identical(rv$show_error, TRUE)) {
          rv$show_error <- TRUE
          rv$show_progress <- FALSE

          # Hide task toast
          session$sendCustomMessage("updateTaskToast", list(
            wrapperId = ns("task_toast"),
            visible = FALSE
          ))
          error_msg <- if (length(state$errors) > 0) {
            state$errors[[length(state$errors)]]$message
          } else {
            i18n$t("unknown_error")
          }
          extra <- if (length(state$errors) > 1) {
            paste0('<small class="text-muted">',
                   sprintf(i18n$t("and_n_more_errors"), length(state$errors) - 1),
                   '</small>')
          } else {
            ""
          }
          session$sendCustomMessage("updateHTML", list(
            id = ns("error_message_content"),
            html = paste0('<p class="text-danger">',
                          htmltools::htmlEscape(error_msg), '</p>', extra)
          ))

          reset_tracking()
        }
      })
    })

    # NOTE: Elapsed time is updated by a client-side JavaScript timer
    # (startElapsedTimer/stopElapsedTimer) to avoid server-side
    # invalidateLater(1000) which causes reactive flush flicker.

    # ========================================
    # Button handlers
    # ========================================

    # Cancel button
    shiny::observeEvent(input$cancel, {
      session$sendCustomMessage("stopElapsedTimer", list())
      app_state$cancel_computation <- Sys.time()
    })

    # Retry button
    shiny::observeEvent(input$retry, {
      rv$show_error <- FALSE
      reset_tracking()
      session$sendCustomMessage("stopElapsedTimer", list())
      app_state$retry_computation <- Sys.time()
    })

    # ========================================
    # Return
    # ========================================

    list(
      show_progress = shiny::reactive(rv$show_progress),
      show_complete = shiny::reactive(rv$show_complete),
      show_error = shiny::reactive(rv$show_error),
      # Called from later::later polling loop (outside reactive context)
      send_running_update = send_running_update,
      reset_tracking = reset_tracking
    )
  })
}

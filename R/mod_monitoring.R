#' Monitoring Module for nemetonApp (E6.b — phase 1 skeleton)
#'
#' @description
#' Continuous Sentinel-2 monitoring (NDVI / NBR drops) for the plots of
#' a registered zone. Phase 1 ships only the scaffolding:
#'
#' * Sidebar with the configuration inputs (zone, dates, bands,
#'   thresholds, rolling window).
#' * DB status card (connected / configuration hint).
#' * Zone selectInput populated from `monitoring_zone` via
#'   [list_monitoring_zones()].
#' * Disabled "Lancer ingestion" button (wired in phase 2).
#' * Placeholder cards for the time series + alerts views (phases 3-4).
#'
#' Persistence of the threshold inputs in `metadata.json` lands in
#' phase 5; the smoke test in phase 6.
#'
#' @name mod_monitoring
#' @keywords internal
NULL


# ---- UI --------------------------------------------------------------

#' Monitoring Module UI
#'
#' @param id Character. Module namespace ID.
#' @return Shiny UI elements.
#' @noRd
mod_monitoring_ui <- function(id) {
  ns <- shiny::NS(id)

  opts <- get_app_options()
  lang <- opts$language %||% "fr"
  i18n <- get_i18n(lang)

  bslib::layout_sidebar(
    fillable = TRUE,

    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      width = 360,
      open = TRUE,

      # Collapsible card — same pattern as mod_field_ingest.
      htmltools::tags$div(
        class = "card mb-3",
        htmltools::tags$div(
          class = "card-header bg-success text-white py-2",
          style = "cursor: pointer;",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", ns("monitor_collapse")),
          `aria-expanded` = "true",
          `aria-controls` = ns("monitor_collapse"),
          htmltools::div(
            class = "d-flex align-items-center justify-content-between",
            htmltools::div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("activity", class = "me-2"),
              i18n$t("monitoring_accordion_title")
            ),
            bsicons::bs_icon("chevron-down", class = "collapse-icon")
          )
        ),
        htmltools::tags$div(
          id = ns("monitor_collapse"),
          class = "collapse show",
          htmltools::tags$div(
            class = "card-body",

            htmltools::tags$p(class = "text-muted small",
                              i18n$t("monitoring_subtitle")),

            shiny::selectInput(
              ns("zone_id"), i18n$t("monitoring_zone_label"),
              choices = character(0),
              selected = NULL
            ),

            shiny::dateRangeInput(
              ns("date_range"), i18n$t("monitoring_date_range"),
              start = Sys.Date() - 365L,
              end   = Sys.Date(),
              language = lang
            ),

            shiny::checkboxGroupInput(
              ns("bands"), i18n$t("monitoring_bands"),
              choices  = c(NDVI = "NDVI", NBR = "NBR"),
              selected = c("NDVI", "NBR"),
              inline   = TRUE
            ),

            shiny::sliderInput(
              ns("threshold_ndvi"), i18n$t("monitoring_threshold_ndvi"),
              min = 0.05, max = 0.50, value = 0.15, step = 0.01
            ),
            shiny::sliderInput(
              ns("threshold_nbr"), i18n$t("monitoring_threshold_nbr"),
              min = 0.05, max = 0.50, value = 0.25, step = 0.01
            ),
            shiny::numericInput(
              ns("window_days"), i18n$t("monitoring_window_days"),
              value = 30L, min = 7L, max = 90L, step = 1L
            ),

            # The button starts disabled and is enabled server-side once
            # zones + bands are valid (and re-disabled while a task is
            # running). actionButton() drops unknown args from `...`, so
            # the bare HTML attribute is tagged explicitly here.
            htmltools::tagAppendAttributes(
              shiny::actionButton(
                ns("run"), i18n$t("monitoring_run_btn"),
                icon  = bsicons::bs_icon("play-fill"),
                class = "btn-primary w-100"
              ),
              disabled = NA
            )
          )
        )
      )
    ),

    # Main area: DB status + placeholders for time series & alerts map.
    htmltools::tags$div(
      class = "p-2",
      shiny::uiOutput(ns("db_status")),
      bslib::card(
        bslib::card_header(
          htmltools::div(
            class = "d-flex align-items-center",
            bsicons::bs_icon("graph-up", class = "me-2"),
            i18n$t("monitoring_timeseries_title")
          )
        ),
        bslib::card_body(
          htmltools::tags$p(class = "text-muted",
                            i18n$t("monitoring_timeseries_placeholder"))
        )
      ),
      bslib::card(
        bslib::card_header(
          htmltools::div(
            class = "d-flex align-items-center",
            bsicons::bs_icon("exclamation-triangle", class = "me-2"),
            i18n$t("monitoring_alerts_title")
          )
        ),
        bslib::card_body(
          htmltools::tags$p(class = "text-muted",
                            i18n$t("monitoring_alerts_placeholder"))
        )
      )
    )
  )
}


# ---- Server ----------------------------------------------------------

#' Monitoring Module Server
#'
#' @param id Character. Module namespace ID.
#' @param app_state reactiveValues carrying `language` and (later)
#'   monitoring config persisted on `current_project`.
#'
#' @return A list of reactives (currently `zones` only — phases 2+
#'   add ingestion result and alerts).
#' @noRd
mod_monitoring_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # Zones reactive: open a fresh connection, list, close. Re-runs on
    # session start and on demand via zones_refresh(). Keep cheap (one
    # query, ~ms).
    zones_refresh <- shiny::reactiveVal(0L)

    zones <- shiny::reactive({
      zones_refresh()  # invalidate dependency
      con <- get_monitoring_db_connection()
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      list_monitoring_zones(con)
    })

    # Push zones into the selectInput. Empty list → empty choices, the
    # status card explains why.
    shiny::observe({
      z <- zones()
      choices <- if (nrow(z)) stats::setNames(as.character(z$id), z$name)
                 else character(0)
      shiny::updateSelectInput(session, "zone_id",
                               choices  = choices,
                               selected = choices[1] %||% character(0))
    })

    # DB status card — three states: not configured, connected with
    # zero zones, connected with zones available.
    output$db_status <- shiny::renderUI({
      i18n <- i18n_r()
      con <- get_monitoring_db_connection()
      on.exit(close_monitoring_db_connection(con), add = TRUE)

      if (is.null(con)) {
        return(.monitoring_status_card(
          icon  = "exclamation-circle",
          class = "border-warning",
          title = i18n$t("monitoring_db_unavailable"),
          body  = i18n$t("monitoring_db_check_env")
        ))
      }
      n <- nrow(zones())
      if (n == 0L) {
        return(.monitoring_status_card(
          icon  = "info-circle",
          class = "border-info",
          title = i18n$t("monitoring_zone_none"),
          body  = i18n$t("monitoring_zone_register_hint")
        ))
      }
      .monitoring_status_card(
        icon  = "check-circle",
        class = "border-success",
        title = sprintf(i18n$t("monitoring_db_connected"), n),
        body  = NULL
      )
    })

    # ----- Async ingestion (phase 2) ----------------------------------

    ingest_task <- run_ingestion_async()

    # Reactive button state: enabled iff a zone is selected, at least one
    # band is checked, and no task is currently running. ExtendedTask$status()
    # is reactive, so this observer auto-fires when the task transitions
    # from initial → running → success/error.
    shiny::observe({
      has_zone  <- isTRUE(nzchar(input$zone_id))
      has_bands <- length(input$bands) > 0L
      is_running <- identical(ingest_task$status(), "running")

      shiny::updateActionButton(
        session, "run",
        disabled = !has_zone || !has_bands || is_running
      )
    })

    # Click handler: validate state, build window dates, fire the task.
    shiny::observeEvent(input$run, {
      i18n <- i18n_r()
      if (!isTRUE(nzchar(input$zone_id))) {
        shiny::showNotification(i18n$t("monitoring_validate_zone"),
                                type = "warning", duration = 4)
        return()
      }
      if (length(input$bands) == 0L) {
        shiny::showNotification(i18n$t("monitoring_validate_bands"),
                                type = "warning", duration = 4)
        return()
      }
      dr <- input$date_range
      if (is.null(dr) || length(dr) != 2L || any(is.na(dr))) {
        shiny::showNotification(i18n$t("monitoring_validate_dates"),
                                type = "warning", duration = 4)
        return()
      }

      shiny::showNotification(i18n$t("monitoring_ingest_starting"),
                              type = "message", duration = 4,
                              id = session$ns("ingest_starting_notif"))

      ingest_task$invoke(
        zone_id   = as.integer(input$zone_id),
        start     = as.Date(dr[1]),
        end       = as.Date(dr[2]),
        bands     = input$bands,
        max_cloud = 20
      )
    })

    # Result handler: re-runs whenever the task transitions to a
    # terminal state. While "running", task$result() throws
    # shiny.silent.error which we re-raise to keep the observer paused.
    shiny::observe({
      i18n <- i18n_r()
      result <- tryCatch(
        ingest_task$result(),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          shiny::showNotification(
            sprintf("%s : %s", i18n$t("monitoring_ingest_error"),
                    e$message),
            type = "error", duration = 8
          )
          NULL
        }
      )
      if (!is.null(result)) {
        shiny::showNotification(
          sprintf(i18n$t("monitoring_ingest_success"),
                  result$summary$n_scenes %||% 0L,
                  result$summary$n_obs_inserted %||% 0L),
          type = "message", duration = 6
        )
        zones_refresh(zones_refresh() + 1L)  # force re-fetch in case
                                             # the ingestion changed state
      }
    })

    list(
      zones        = zones,
      ingest_task  = ingest_task
    )
  })
}


# ---- Internal --------------------------------------------------------

.monitoring_status_card <- function(icon, class, title, body = NULL) {
  htmltools::tags$div(
    class = paste("card mb-3", class),
    htmltools::tags$div(
      class = "card-body py-3",
      htmltools::div(
        class = "d-flex align-items-center",
        bsicons::bs_icon(icon, class = "me-2 fs-4"),
        htmltools::tags$div(
          htmltools::tags$strong(title),
          if (!is.null(body)) htmltools::tags$div(class = "text-muted small", body)
        )
      )
    )
  )
}

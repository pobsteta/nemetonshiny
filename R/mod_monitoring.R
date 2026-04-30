#' Monitoring Module for nemetonApp (E6.b + E6.c.5 — health monitoring)
#'
#' @description
#' Two-mode forest health monitoring:
#'
#' * **Mode 1 — Surveillance rapide** (E6.b): rolling-window NDVI/NBR on
#'   Sentinel-2, detects recent shocks (cuts, windthrows, fires).
#'   Seconds-scale.
#' * **Mode 2 — Diagnostic sanitaire** (E6.c.5, spec 008): FORDEAD via
#'   reticulate (CRSWIR + harmonic model), detects progressive dieback
#'   (bark beetle, drought). Minutes-to-hours scale.
#'
#' Both pipelines write to the same `alert` table (alert_type
#' discriminant). The G3 garde-fou (geographic + species validity check
#' via `nemeton::check_fordead_validity`) is enforced before any FORDEAD
#' run, with a confirmation modal when the user forces an out-of-domain
#' run.
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

            # --- Mode toggle (E6.c.5 — T6app.1) ---------------------
            shiny::radioButtons(
              ns("mode"), i18n$t("monitoring_mode_label"),
              choices = stats::setNames(
                c("quick", "health"),
                c(i18n$t("monitoring_mode_quick"),
                  i18n$t("monitoring_mode_health"))
              ),
              selected = "quick",
              inline   = FALSE
            ),
            shiny::uiOutput(ns("mode_help")),

            # --- Common: zone + date range --------------------------
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

            # --- Quick-mode parameters (NDVI/NBR) -------------------
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'quick'", ns("mode")),
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
              htmltools::tagAppendAttributes(
                shiny::actionButton(
                  ns("run"), i18n$t("monitoring_run_btn"),
                  icon  = bsicons::bs_icon("play-fill"),
                  class = "btn-primary w-100"
                ),
                disabled = NA
              )
            ),

            # --- Health-mode parameters (FORDEAD) -------------------
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'health'", ns("mode")),
              shiny::dateRangeInput(
                ns("dates_training"),
                i18n$t("monitoring_dates_training_label"),
                start    = as.Date("2016-01-01"),
                end      = as.Date("2017-12-31"),
                language = lang
              ),
              shiny::selectInput(
                ns("vegetation_index"),
                i18n$t("monitoring_vegetation_index"),
                choices  = c(CRSWIR = "CRSWIR", NDVI = "NDVI", NDWI = "NDWI"),
                selected = "CRSWIR"
              ),
              shiny::sliderInput(
                ns("threshold_anomaly"),
                i18n$t("monitoring_threshold_anomaly"),
                min = 0.05, max = 0.50, value = 0.16, step = 0.01
              ),
              htmltools::tagAppendAttributes(
                shiny::actionButton(
                  ns("run_health"), i18n$t("monitoring_run_health_btn"),
                  icon  = bsicons::bs_icon("activity"),
                  class = "btn-primary w-100"
                ),
                disabled = NA
              )
            )
          )
        )
      )
    ),

    # Main area: DB status + G3 banners + time series & alerts map.
    htmltools::tags$div(
      class = "p-2",
      shiny::uiOutput(ns("db_status")),
      shiny::uiOutput(ns("validity_banners")),
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
            class = "d-flex align-items-center justify-content-between",
            htmltools::div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("exclamation-triangle", class = "me-2"),
              i18n$t("monitoring_alerts_title")
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'health'", ns("mode")),
              shiny::checkboxInput(
                ns("include_low"),
                i18n$t("monitoring_include_low"),
                value = FALSE
              )
            )
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("alerts_panel"))
        )
      ),
      # Health-mode QField generator (E6.c.5 — T6app.12, wired in C6)
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'health'", ns("mode")),
        shiny::uiOutput(ns("qfield_panel"))
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

    # Bumped after each FORDEAD run so the alerts reactive (wired in C5)
    # re-fetches from the DB.
    alerts_refresh <- shiny::reactiveVal(0L)

    # ----- Mode help text -------------------------------------------
    output$mode_help <- shiny::renderUI({
      i18n <- i18n_r()
      key <- if (identical(input$mode, "health"))
               "monitoring_mode_health_help" else "monitoring_mode_quick_help"
      htmltools::tags$p(class = "text-muted small fst-italic", i18n$t(key))
    })

    # ----- G3 — Validity banners (geo + species) --------------------
    # Recomputed when zone or mode changes. Quick mode does not need
    # the FORDEAD validity check, so banners only show in health mode.
    validity <- shiny::reactive({
      if (!identical(input$mode, "health")) return(NULL)
      zone <- input$zone_id
      if (!isTRUE(nzchar(zone))) return(NULL)
      con <- get_monitoring_db_connection()
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      units <- app_state$current_project$indicators_sf
      validity_check_for_zone(con, as.integer(zone), units = units)
    })

    output$validity_banners <- shiny::renderUI({
      v <- validity()
      if (is.null(v)) return(NULL)
      i18n <- i18n_r()
      banners <- list()
      if (isFALSE(v$geo_valid)) {
        pct <- (v$geo_intersection_pct %||% 0) * 100
        banners[[length(banners) + 1L]] <- .monitoring_validity_banner(
          icon  = "geo-alt",
          title = i18n$t("monitoring_warning_geo_title"),
          body  = sprintf(i18n$t("monitoring_warning_geo_body"), pct)
        )
      }
      if (isFALSE(v$species_valid)) {
        pct <- (v$species_resineux_pct %||% 0) * 100
        banners[[length(banners) + 1L]] <- .monitoring_validity_banner(
          icon  = "tree",
          title = i18n$t("monitoring_warning_species_title"),
          body  = sprintf(i18n$t("monitoring_warning_species_body"), pct)
        )
      }
      if (isTRUE(input$include_low)) {
        banners[[length(banners) + 1L]] <- .monitoring_validity_banner(
          icon  = "exclamation-triangle",
          title = i18n$t("monitoring_warning_low_classes"),
          body  = NULL
        )
      }
      if (!length(banners)) return(NULL)
      htmltools::tagList(banners)
    })

    # ----- Alerts panel placeholder (leaflet wired in C5) -----------
    output$alerts_panel <- shiny::renderUI({
      i18n <- i18n_r()
      htmltools::tags$p(class = "text-muted",
                        i18n$t("monitoring_alerts_placeholder"))
    })

    # ----- QField panel placeholder (wired in C6) -------------------
    output$qfield_panel <- shiny::renderUI({
      NULL
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

    # ----- Async FORDEAD diagnosis (E6.c.5 — health mode) -----------

    fordead_task <- run_fordead_async()

    # Reactive button state for run_health.
    shiny::observe({
      has_zone   <- isTRUE(nzchar(input$zone_id))
      is_running <- identical(fordead_task$status(), "running")

      shiny::updateActionButton(
        session, "run_health",
        disabled = !has_zone || is_running
      )
    })

    # Helper — kicks off the FORDEAD task with the current sidebar
    # values. Called both from the direct-click path (validity OK) and
    # from the modal "force anyway" path (G3 garde-fou).
    .invoke_fordead <- function() {
      i18n <- i18n_r()
      con <- get_monitoring_db_connection()
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      aoi <- get_monitoring_zone_aoi(con, as.integer(input$zone_id))
      if (is.null(aoi)) {
        shiny::showNotification(i18n$t("monitoring_validate_zone"),
                                type = "warning", duration = 4)
        return(invisible(FALSE))
      }
      shiny::showNotification(i18n$t("monitoring_health_starting"),
                              type = "message", duration = 6,
                              id = session$ns("fordead_starting_notif"))
      fordead_task$invoke(
        aoi               = aoi,
        dates_training    = as.character(input$dates_training),
        dates_monitoring  = as.character(input$date_range),
        threshold_anomaly = as.numeric(input$threshold_anomaly),
        vegetation_index  = input$vegetation_index,
        zone_id           = as.integer(input$zone_id)
      )
      invisible(TRUE)
    }

    # Click handler with G3 garde-fou: when the zone is out of validity
    # domain (geographic OR species), pop a confirmation modal before
    # invoking. The user can still proceed but knows the calibration
    # warranty doesn't apply.
    shiny::observeEvent(input$run_health, {
      i18n <- i18n_r()
      if (!isTRUE(nzchar(input$zone_id))) {
        shiny::showNotification(i18n$t("monitoring_validate_zone"),
                                type = "warning", duration = 4)
        return()
      }
      dr <- input$date_range
      if (is.null(dr) || length(dr) != 2L || any(is.na(dr))) {
        shiny::showNotification(i18n$t("monitoring_validate_dates"),
                                type = "warning", duration = 4)
        return()
      }
      dt <- input$dates_training
      if (is.null(dt) || length(dt) != 2L || any(is.na(dt))) {
        shiny::showNotification(i18n$t("monitoring_validate_dates"),
                                type = "warning", duration = 4)
        return()
      }

      v <- validity()
      if (!is.null(v) && isFALSE(v$overall_valid)) {
        shiny::showModal(shiny::modalDialog(
          title = i18n$t("monitoring_confirm_invalid_title"),
          i18n$t("monitoring_confirm_invalid_body"),
          easyClose = FALSE,
          footer = htmltools::tagList(
            shiny::modalButton(i18n$t("monitoring_confirm_no")),
            shiny::actionButton(session$ns("confirm_invalid_run"),
                                i18n$t("monitoring_confirm_yes"),
                                class = "btn-warning")
          )
        ))
        return()
      }
      .invoke_fordead()
    })

    # G3 modal "Run anyway" path.
    shiny::observeEvent(input$confirm_invalid_run, {
      shiny::removeModal()
      .invoke_fordead()
    })

    # FORDEAD result handler.
    shiny::observe({
      i18n <- i18n_r()
      result <- tryCatch(
        fordead_task$result(),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          shiny::showNotification(
            sprintf("%s : %s", i18n$t("monitoring_health_error"),
                    e$message),
            type = "error", duration = 10
          )
          NULL
        }
      )
      if (!is.null(result)) {
        if (identical(result$status, "error")) {
          shiny::showNotification(
            sprintf("%s : %s", i18n$t("monitoring_health_error"),
                    result$message %||% ""),
            type = "error", duration = 10
          )
        } else {
          shiny::showNotification(
            sprintf(i18n$t("monitoring_health_success"),
                    result$n_alerts_inserted %||% 0L,
                    result$duration_sec      %||% 0),
            type = "message", duration = 8
          )
          alerts_refresh(alerts_refresh() + 1L)
        }
      }
    })

    list(
      zones         = zones,
      ingest_task   = ingest_task,
      fordead_task  = fordead_task,
      validity      = validity
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

# G3 banner — used when zone is outside FORDEAD validity domain or when
# the user opts into low/medium classes (>50% FP per ONF/DSF 2024).
.monitoring_validity_banner <- function(icon, title, body = NULL) {
  htmltools::tags$div(
    class = "card border-warning mb-3",
    htmltools::tags$div(
      class = "card-body py-2",
      htmltools::div(
        class = "d-flex align-items-start",
        bsicons::bs_icon(icon, class = "me-2 fs-4 text-warning"),
        htmltools::tags$div(
          htmltools::tags$strong(title),
          if (!is.null(body)) htmltools::tags$div(class = "small", body)
        )
      )
    )
  )
}

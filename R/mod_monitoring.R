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

            # Phase 1: button is rendered disabled (the ingestion task
            # ships in phase 2). actionButton() drops unknown args from
            # `...`, so we tag the bare HTML attribute explicitly.
            htmltools::tagAppendAttributes(
              shiny::actionButton(
                ns("run"), i18n$t("monitoring_run_btn"),
                icon  = bsicons::bs_icon("play-fill"),
                class = "btn-primary w-100"
              ),
              disabled = NA
            ),
            htmltools::tags$p(class = "text-muted small fst-italic mt-2",
                              i18n$t("monitoring_run_disabled_hint"))
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

    list(
      zones = zones
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

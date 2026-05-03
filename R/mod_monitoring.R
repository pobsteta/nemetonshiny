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

            # Bridge between the loaded project (Home tab) and the
            # monitoring DB: registers the project's UGF union + the
            # placettes generated in the Sampling tab as a fresh
            # `monitoring_zone` row, then auto-selects it above.
            # Common to both modes (FAST + FORDEAD). Disabled until
            # project + samples + DB are all ready.
            htmltools::tagAppendAttributes(
              shiny::actionButton(
                ns("register"), i18n$t("monitoring_register_btn"),
                icon  = bsicons::bs_icon("geo-alt-fill"),
                class = "btn-outline-primary btn-sm w-100 mb-3"
              ),
              disabled = NA
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
          plotly::plotlyOutput(ns("timeseries"), height = "320px")
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
      # Health-mode QGIS generator (E6.c.5 — T6app.12, wired in C6)
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'health'", ns("mode")),
        shiny::uiOutput(ns("qgis_panel"))
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

    # ----- Restore monitoring config from project metadata ----------
    # Fires whenever the user opens a project. Mirrors the persistence
    # in .invoke_fordead() (and mode/threshold changes via the sidebar).
    shiny::observe({
      m <- app_state$current_project$metadata
      if (is.null(m)) return()
      if (!is.null(m$monitoring_mode)) {
        shiny::updateRadioButtons(session, "mode",
                                  selected = m$monitoring_mode)
      }
      if (!is.null(m$monitoring_threshold_anomaly)) {
        shiny::updateSliderInput(session, "threshold_anomaly",
                                 value = as.numeric(m$monitoring_threshold_anomaly))
      }
      if (!is.null(m$monitoring_vegetation_index)) {
        shiny::updateSelectInput(session, "vegetation_index",
                                 selected = m$monitoring_vegetation_index)
      }
      if (!is.null(m$monitoring_dates_training)) {
        dt <- as.Date(unlist(m$monitoring_dates_training))
        if (length(dt) == 2L && all(!is.na(dt))) {
          shiny::updateDateRangeInput(session, "dates_training",
                                      start = dt[1], end = dt[2])
        }
      }
    })

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

    # ----- Alerts reactive (FORDEAD) --------------------------------
    # G1: by default only classes 3-forte + 4-sol-nu. The "include_low"
    # checkbox flips to all classes (1-2 inclusive). Re-fetches whenever
    # the zone, the toggle, or alerts_refresh() change.
    alerts <- shiny::reactive({
      alerts_refresh()
      zone <- input$zone_id
      if (!isTRUE(nzchar(zone))) return(NULL)
      if (!identical(input$mode, "health")) return(NULL)
      classes <- if (isTRUE(input$include_low))
                   c("1-faible", "2-moyenne", "3-forte", "4-sol-nu")
                 else
                   c("3-forte", "4-sol-nu")
      con <- get_monitoring_db_connection()
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      list_alerts_for_zone(con, as.integer(zone), classes = classes)
    })

    # ----- Alerts leaflet (T6app.11) --------------------------------
    output$alerts_panel <- shiny::renderUI({
      ns <- session$ns
      i18n <- i18n_r()
      a <- alerts()
      if (is.null(a) || !nrow(a)) {
        return(htmltools::tags$p(class = "text-muted",
                                 i18n$t("monitoring_alerts_placeholder")))
      }
      leaflet::leafletOutput(ns("alerts_map"), height = "55vh")
    })

    output$alerts_map <- leaflet::renderLeaflet({
      a <- alerts()
      if (is.null(a) || !nrow(a)) return(NULL)
      i18n <- i18n_r()

      a_ll <- if (sf::st_crs(a)$epsg %||% 4326L != 4326L)
                sf::st_transform(a, 4326)
              else a

      colors <- c(
        "1-faible"  = "#FFD27F",  # pale orange
        "2-moyenne" = "#FF9933",  # orange
        "3-forte"   = "#D62728",  # red
        "4-sol-nu"  = "#222222"   # near-black
      )
      cls <- as.character(a_ll$confidence_class)
      cls[!cls %in% names(colors)] <- "3-forte"
      fill <- unname(colors[cls])

      popups <- vapply(seq_len(nrow(a_ll)), function(i) {
        sprintf(
          "<strong>%s</strong>: %s<br/>%s: %.2f<br/>%s: %s<br/>%s: %s<br/>%s: %s",
          i18n$t("monitoring_alert_popup_class"),
          htmltools::htmlEscape(cls[i]),
          i18n$t("monitoring_alert_popup_stress"),
          as.numeric(a_ll$stress_index[i] %||% NA_real_),
          i18n$t("monitoring_alert_popup_date"),
          as.character(a_ll$trigger_date[i] %||% NA),
          i18n$t("monitoring_alert_popup_status"),
          htmltools::htmlEscape(as.character(a_ll$validation_status[i] %||% "")),
          i18n$t("monitoring_alert_popup_disturbance"),
          htmltools::htmlEscape(as.character(a_ll$disturbance_type[i] %||% "-"))
        )
      }, character(1))

      legend_labels <- c(
        i18n$t("monitoring_class_1"),
        i18n$t("monitoring_class_2"),
        i18n$t("monitoring_class_3"),
        i18n$t("monitoring_class_4")
      )

      leaflet::leaflet(a_ll) |>
        leaflet::addProviderTiles("OpenStreetMap",   group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::addCircleMarkers(
          radius = 6, weight = 1, color = "#000",
          fillColor = fill, fillOpacity = 0.85,
          popup = popups
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors   = unname(colors),
          labels   = legend_labels,
          title    = i18n$t("monitoring_alert_popup_class"),
          opacity  = 0.85
        )
    })

    # ----- Time series plotly (T6app.10) ----------------------------
    # Mode rapide: NDVI/NBR (data wiring lands with E6.b phase 3, so
    # for now we render a "no data" plot with the right axis layout).
    # Mode sanitaire: alerts distribution per class (informative without
    # requiring per-pixel harmonic-model data, which isn't returned by
    # run_fordead_dieback).
    output$timeseries <- plotly::renderPlotly({
      i18n <- i18n_r()
      empty <- plotly::plot_ly(type = "scatter", mode = "lines") |>
        plotly::layout(
          annotations = list(list(
            text      = i18n$t("monitoring_timeseries_placeholder"),
            xref      = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font      = list(color = "#888")
          )),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )

      if (!identical(input$mode, "health")) return(empty)
      a <- alerts()
      if (is.null(a) || !nrow(a)) return(empty)

      counts <- table(factor(a$confidence_class,
                             levels = c("1-faible", "2-moyenne",
                                        "3-forte",  "4-sol-nu")))
      labels <- c(
        "1-faible"  = i18n$t("monitoring_class_1"),
        "2-moyenne" = i18n$t("monitoring_class_2"),
        "3-forte"   = i18n$t("monitoring_class_3"),
        "4-sol-nu"  = i18n$t("monitoring_class_4")
      )
      bar_colors <- c("#FFD27F", "#FF9933", "#D62728", "#222222")
      plotly::plot_ly(
        x = unname(labels[names(counts)]),
        y = as.integer(counts),
        type = "bar",
        marker = list(color = bar_colors)
      ) |>
        plotly::layout(
          margin = list(t = 20, b = 40, l = 40, r = 10),
          yaxis = list(title = "n alertes"),
          xaxis = list(title = "")
        )
    })

    # ----- QGIS sanitaire panel (T6app.12) --------------------------
    output$qgis_panel <- shiny::renderUI({
      ns <- session$ns
      i18n <- i18n_r()
      a <- alerts()
      bslib::card(
        bslib::card_header(
          htmltools::div(
            class = "d-flex align-items-center",
            bsicons::bs_icon("clipboard-check", class = "me-2"),
            i18n$t("monitoring_qgis_btn")
          )
        ),
        bslib::card_body(
          if (is.null(a) || !nrow(a))
            htmltools::tags$p(class = "text-muted",
                              i18n$t("monitoring_qgis_no_alerts"))
          else htmltools::tagList(
            shiny::numericInput(
              ns("qgis_n"),
              i18n$t("monitoring_qgis_n_label"),
              value = min(30L, nrow(a)),
              min = 1L, max = nrow(a), step = 1L
            ),
            shiny::radioButtons(
              ns("qgis_method"),
              i18n$t("monitoring_qgis_method"),
              choices  = c(GRTS = "grts", Random = "random"),
              selected = "grts",
              inline   = TRUE
            ),
            shiny::downloadButton(
              ns("qgis_download"),
              i18n$t("monitoring_qgis_btn"),
              icon  = bsicons::bs_icon("download"),
              class = "btn-success w-100"
            )
          )
        )
      )
    })

    # downloadHandler: build the placette plan + zip a .qgz on disk
    # then stream it back. generate_health_validation_plots needs the
    # *pending* alerts (not the leaflet-filtered ones), so we re-fetch
    # without the include_low / class filtering.
    output$qgis_download <- shiny::downloadHandler(
      filename = function() {
        sprintf("nemeton-health-validation-%s.qgz",
                format(Sys.time(), "%Y%m%d-%H%M%S"))
      },
      content = function(file) {
        i18n <- i18n_r()
        con <- get_monitoring_db_connection()
        on.exit(close_monitoring_db_connection(con), add = TRUE)
        zone <- input$zone_id
        a <- list_alerts_for_zone(
          con, as.integer(zone),
          classes           = c("1-faible", "2-moyenne",
                                "3-forte",  "4-sol-nu"),
          validation_status = "pending"
        )
        if (is.null(a) || !nrow(a)) {
          shiny::showNotification(i18n$t("monitoring_qgis_no_alerts"),
                                  type = "warning", duration = 6)
          file.create(file)
          return(invisible())
        }
        plots <- nemeton::generate_health_validation_plots(
          a,
          n      = as.integer(input$qgis_n %||% 30L),
          method = input$qgis_method %||% "grts",
          crs    = 2154
        )
        out_dir <- tempfile("qgis-health-")
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        qgz <- nemeton::create_qfield_project(
          plots,
          output_dir   = out_dir,
          project_name = "health-validation",
          region       = "BFC",
          lang         = app_state$language %||% "fr"
        )
        file.copy(qgz, file, overwrite = TRUE)
        shiny::showNotification(
          sprintf(i18n$t("monitoring_qgis_generated"), nrow(plots)),
          type = "message", duration = 6
        )
      }
    )

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
    # status card explains why. If the loaded project was previously
    # registered as a zone (`metadata$monitoring_zone_id`), pre-select
    # that zone instead of the first alphabetic one — saves the user a
    # click on every project re-open.
    shiny::observe({
      z <- zones()
      choices <- if (nrow(z)) stats::setNames(as.character(z$id), z$name)
                 else character(0)

      preferred <- character(0)
      pmeta_id <- app_state$current_project$metadata$monitoring_zone_id
      if (!is.null(pmeta_id) && length(pmeta_id) == 1L) {
        candidate <- as.character(pmeta_id)
        if (candidate %in% choices) preferred <- candidate
      }
      selected <- if (length(preferred)) preferred
                  else (choices[1] %||% character(0))

      shiny::updateSelectInput(session, "zone_id",
                               choices  = choices,
                               selected = selected)
    })

    # ----- Register-this-project-as-zone bridge ----------------------

    # Reactive: does the loaded project have a persisted sampling plan
    # on disk? Reads <project>/data/samples.gpkg directly so this also
    # works after an app restart. Bumped via `app_state$samples_refresh`
    # whenever mod_sampling writes a new plan, so the button enables
    # without forcing a project reload.
    samples_present <- shiny::reactive({
      app_state$samples_refresh  # invalidate dependency on sampling save
      project <- app_state$current_project
      if (is.null(project) || is.null(project$id)) return(FALSE)
      pp <- get_project_path(project$id)
      if (is.null(pp)) return(FALSE)
      file.exists(file.path(pp, "data", "samples.gpkg"))
    })

    register_running <- shiny::reactiveVal(FALSE)

    # Button state: enabled iff project is loaded, samples exist on
    # disk, the DB is configured, and no registration is in flight.
    shiny::observe({
      has_project <- !is.null(app_state$current_project) &&
                     !is.null(app_state$current_project$id)
      has_db <- {
        con <- get_monitoring_db_connection()
        on.exit(close_monitoring_db_connection(con), add = TRUE)
        !is.null(con)
      }
      shiny::updateActionButton(
        session, "register",
        disabled = !has_project || !samples_present() ||
                   !has_db || isTRUE(register_running())
      )
    })

    # Click handler: validate, register, persist zone_id, refresh
    # dropdown. Idempotent — clicking twice on the same project just
    # re-selects the existing zone (helper checks DB before inserting).
    shiny::observeEvent(input$register, {
      i18n <- i18n_r()
      project <- app_state$current_project
      if (is.null(project) || is.null(project$id)) {
        shiny::showNotification(i18n$t("monitoring_register_no_project"),
                                type = "warning", duration = 4)
        return()
      }
      if (!samples_present()) {
        shiny::showNotification(i18n$t("monitoring_register_no_samples"),
                                type = "warning", duration = 5)
        return()
      }
      con <- get_monitoring_db_connection()
      if (is.null(con)) {
        shiny::showNotification(i18n$t("monitoring_register_no_db"),
                                type = "error", duration = 5)
        return()
      }
      on.exit(close_monitoring_db_connection(con), add = TRUE)

      register_running(TRUE)
      shiny::showNotification(i18n$t("monitoring_register_running"),
                              type = "message", duration = 3,
                              id = session$ns("register_running_notif"))

      result <- tryCatch(
        register_project_as_zone(con, project),
        error = function(e) e
      )
      register_running(FALSE)

      if (inherits(result, "error")) {
        shiny::showNotification(
          sprintf("%s : %s", i18n$t("monitoring_register_error"),
                  conditionMessage(result)),
          type = "error", duration = 8
        )
        return()
      }

      # Refresh dropdown so the new (or re-found) zone appears, then
      # update the in-memory project metadata so auto-select picks the
      # right id on the next reactive flush.
      project$metadata$monitoring_zone_id <- result$zone_id
      app_state$current_project <- project
      zones_refresh(zones_refresh() + 1L)

      msg_key <- if (isTRUE(result$was_existing)) {
        "monitoring_register_already"
      } else {
        "monitoring_register_success"
      }
      shiny::showNotification(
        sprintf(i18n$t(msg_key), result$zone_name, result$n_plots),
        type = "message", duration = 5
      )
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
      .persist_monitoring_metadata()
      invisible(TRUE)
    }

    # Persist sidebar state in the project metadata.json so a reopen
    # restores the user's last FORDEAD configuration. Best-effort —
    # silent no-op if the project has no on-disk path or metadata.
    .persist_monitoring_metadata <- function() {
      project_id <- app_state$current_project$id
      if (is.null(project_id)) return(invisible(FALSE))
      v <- validity()
      tryCatch(
        update_project_metadata(project_id, list(
          monitoring_mode               = input$mode,
          monitoring_threshold_anomaly  = as.numeric(input$threshold_anomaly),
          monitoring_vegetation_index   = input$vegetation_index,
          monitoring_dates_training     = as.character(input$dates_training),
          monitoring_validity_geo_pct   = v$geo_intersection_pct %||% NA_real_,
          monitoring_validity_species_pct = v$species_resineux_pct %||% NA_real_
        )),
        error = function(e) {
          cli::cli_warn("Could not persist monitoring metadata: {conditionMessage(e)}")
          invisible(FALSE)
        }
      )
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

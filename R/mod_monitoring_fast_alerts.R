# mod_monitoring_fast_alerts.R — Sub-tab "Alertes FAST" of Suivi sanitaire.
#
# Wires `nemeton::list_fast_alerts_for_zone()` (shipped in nemeton@v0.25.0)
# into the monitoring navset. Renders a Leaflet map of plots whose NDVI or
# NBR ratio against the sidebar threshold falls into one of three severity
# buckets (the cœur classifier in `list_fast_alerts_for_zone()` uses the
# worse of the two band ratios) :
#
#   - critical  : ratio < 0.5     → red       (#D62728)
#   - warning   : ratio [0.5, 1)  → orange    (#FF9933)
#   - info      : ratio [1, 1.1)  → pale-orange corridor (#FFD27F)
#   - safe      : ratio >= 1.1    → NOT returned by the cœur
#
# Inputs from the parent module (`mod_monitoring`) are passed as reactives
# so this module stays decoupled from the sidebar widget ids — same pattern
# as `mod_monitoring_pixel_map_server(..., obs_pixel_data, mode_input)`.

#' Alertes FAST sub-tab UI
#'
#' @param id Module namespace id.
#' @return A `shiny.tag.list` with the panel + counters.
#' @noRd
mod_monitoring_fast_alerts_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::uiOutput(ns("counters")),
    shiny::uiOutput(ns("panel"))
  )
}

#' Alertes FAST sub-tab server
#'
#' @param id Module namespace id.
#' @param app_state Parent `reactiveValues` carrying `language`,
#'   `current_project`.
#' @param zone_id_r Reactive returning the active monitoring zone id (chr
#'   or int — coerced to integer inside).
#' @param date_range_r Reactive returning a length-2 Date vector
#'   `[date_from, date_to]`.
#' @param thresholds_r Reactive returning `list(ndvi, nbr, window_days)`.
#' @return invisible list with `alerts` reactive.
#' @noRd
mod_monitoring_fast_alerts_server <- function(id, app_state, zone_id_r,
                                              date_range_r, thresholds_r) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # ----- Core call ------------------------------------------------------
    # All four arguments are validated up-front so the cœur is only called
    # with a complete request. NULL on any missing piece — the panel
    # downstream renders an empty state.
    alerts <- shiny::reactive({
      zone <- zone_id_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return(NULL)
      dr <- date_range_r()
      if (length(dr) != 2L || any(is.na(dr))) return(NULL)
      th <- thresholds_r()
      if (is.null(th) || is.null(th$ndvi) || is.null(th$nbr)) return(NULL)
      proj <- app_state$current_project
      if (is.null(proj)) return(NULL)
      con <- get_monitoring_db_connection(project = proj)
      if (is.null(con)) return(NULL)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      tryCatch(
        nemeton::list_fast_alerts_for_zone(
          con,
          zone_id        = as.integer(zone),
          threshold_ndvi = as.numeric(th$ndvi),
          threshold_nbr  = as.numeric(th$nbr),
          window_days    = as.integer(th$window_days %||% 30L),
          date_from      = dr[1],
          date_to        = dr[2]
        ),
        error = function(e) {
          cli::cli_alert_warning(
            "list_fast_alerts_for_zone failed: {e$message}"
          )
          NULL
        }
      )
    })

    # ----- Counters card --------------------------------------------------
    # Quick visual of the severity distribution. Hidden when there's no
    # data so the empty-state placeholder owns the whole panel.
    output$counters <- shiny::renderUI({
      i18n <- i18n_r()
      a <- alerts()
      if (is.null(a) || !nrow(a)) return(NULL)
      counts <- as.list(table(factor(
        as.character(a$severity),
        levels = c("critical", "warning", "info")
      )))
      htmltools::div(
        class = "d-flex gap-3 align-items-center px-3 pt-2 small",
        .fast_severity_badge(i18n$t("monitoring_fast_severity_critical"),
                             "#D62728", counts$critical %||% 0L),
        .fast_severity_badge(i18n$t("monitoring_fast_severity_warning"),
                             "#FF9933", counts$warning %||% 0L),
        .fast_severity_badge(i18n$t("monitoring_fast_severity_info"),
                             "#FFD27F", counts$info %||% 0L),
        htmltools::tags$span(
          class = "ms-auto text-muted",
          sprintf("%s: %d", i18n$t("monitoring_fast_total"), nrow(a))
        )
      )
    })

    # ----- Panel + map ----------------------------------------------------
    output$panel <- shiny::renderUI({
      i18n <- i18n_r()
      a <- alerts()
      if (is.null(a) || !nrow(a)) {
        return(htmltools::div(
          class = "p-4 text-center text-muted",
          bsicons::bs_icon("check-circle",
                           class = "fs-1 d-block mx-auto mb-3 text-success"),
          htmltools::h5(class = "mt-3",
                        i18n$t("monitoring_fast_alerts_empty_title")),
          htmltools::p(class = "mb-0",
                       i18n$t("monitoring_fast_alerts_empty_body"))
        ))
      }
      leaflet::leafletOutput(session$ns("map"), height = "55vh")
    })

    output$map <- leaflet::renderLeaflet({
      a <- alerts()
      if (is.null(a) || !nrow(a)) return(NULL)
      i18n <- i18n_r()

      a_ll <- if ((sf::st_crs(a)$epsg %||% 4326L) != 4326L) {
        sf::st_transform(a, 4326)
      } else a

      colors <- c(
        critical = "#D62728",
        warning  = "#FF9933",
        info     = "#FFD27F"
      )
      sev <- as.character(a_ll$severity)
      sev[!sev %in% names(colors)] <- "warning"
      fill <- unname(colors[sev])

      popups <- vapply(seq_len(nrow(a_ll)), function(i) {
        sprintf(
          paste0(
            "<strong>%s</strong>: %s<br/>",
            "%s: %s<br/>",
            "%s: %.2f%s<br/>",
            "%s: %.2f%s<br/>",
            "%s: %s"
          ),
          i18n$t("monitoring_fast_alert_popup_plot"),
          htmltools::htmlEscape(as.character(a_ll$plot_id[i])),
          i18n$t("monitoring_fast_alert_popup_severity"),
          htmltools::htmlEscape(sev[i]),
          i18n$t("monitoring_fast_alert_popup_ndvi"),
          as.numeric(a_ll$ndvi_value[i] %||% NA_real_),
          .fmt_drop(a_ll$ndvi_drop[i]),
          i18n$t("monitoring_fast_alert_popup_nbr"),
          as.numeric(a_ll$nbr_value[i] %||% NA_real_),
          .fmt_drop(a_ll$nbr_drop[i]),
          i18n$t("monitoring_fast_alert_popup_last_obs"),
          as.character(a_ll$last_obs_date[i] %||% NA)
        )
      }, character(1))

      leaflet::leaflet(a_ll) |>
        leaflet::addProviderTiles("OpenStreetMap",   group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          options    = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::addCircleMarkers(
          radius = 7, weight = 1, color = "#000",
          fillColor = fill, fillOpacity = 0.85,
          popup = popups
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors   = unname(colors),
          labels   = c(
            i18n$t("monitoring_fast_severity_critical"),
            i18n$t("monitoring_fast_severity_warning"),
            i18n$t("monitoring_fast_severity_info")
          ),
          title    = i18n$t("monitoring_fast_alert_popup_severity"),
          opacity  = 0.85
        )
    })

    invisible(list(alerts = alerts))
  })
}

# Small inline badge for the severity counter row.
.fast_severity_badge <- function(label, color, n) {
  htmltools::tags$span(
    class = "d-inline-flex align-items-center gap-1",
    htmltools::tags$span(
      style = sprintf(
        "display:inline-block;width:10px;height:10px;border-radius:50%%;background:%s;",
        color
      )
    ),
    htmltools::tags$strong(sprintf("%d", as.integer(n))),
    htmltools::tags$span(class = "text-muted", label)
  )
}

# Drop column is the threshold - value margin (positive = above threshold,
# negative = below). Show a small parenthetical hint so the user can
# read severity in one glance. NA-safe.
.fmt_drop <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) != 1L || is.na(v)) return("")
  sprintf(" <span class='text-muted'>(Δ %+.2f)</span>", v)
}

# mod_monitoring_fast_alerts.R — Sub-tab "Alertes FAST" of Suivi sanitaire.
#
# v0.42.0 — Spec 013 wiring : the module now consumes
# `nemeton::read_fast_alert_raster()` (count / rolling modes, pixel
# resolution Sentinel-2 10 m, EPSG:2154) and renders it via
# `leaflet::addRasterImage()`. Replaces the previous markers-per-
# placette representation backed by `list_fast_alerts_for_zone()` —
# cohérent avec FORDEAD déjà raster (cf. mod_monitoring_fordead_map).
#
# Two display modes (radio button) :
#
#   - "count"   : integer raster, number of dates where NDVI<seuil OR
#                 NBR<seuil within [date_from, date_to]. Discrete palette
#                 0 transparent / 1-2 jaune / 3-5 orange / 6+ rouge.
#
#   - "rolling" : continuous raster, max(deficit_ndvi, deficit_nbr) sur
#                 la fenêtre roulante trailing de window_days jours
#                 (0 = pas en alerte, > 0 = magnitude). Palette continue
#                 jaune → rouge avec borne supérieure capée sur le
#                 percentile 95 pour stabiliser l'échelle face à une
#                 queue extrême minoritaire.
#
# Pas de popup au clic — l'exploration pixel-par-pixel se fait sur
# l'onglet « Carte FAST » (mod_monitoring_pixel_map).

#' Alertes FAST sub-tab UI
#'
#' @param id Module namespace id.
#' @return A `shiny.tag.list` with the mode toggle + the panel.
#' @noRd
mod_monitoring_fast_alerts_ui <- function(id) {
  ns <- shiny::NS(id)
  # Initial labels in the default language ; refreshed server-side on
  # language switch via updateRadioButtons.
  i18n <- get_i18n("fr")
  htmltools::tagList(
    htmltools::div(
      class = "px-3 pt-2 d-flex flex-wrap align-items-center gap-3 small",
      htmltools::tags$strong(i18n$t("monitoring_fast_alerts_mode_label")),
      shiny::radioButtons(
        ns("mode"),
        label  = NULL,
        inline = TRUE,
        choiceNames  = list(i18n$t("monitoring_fast_alerts_mode_count"),
                            i18n$t("monitoring_fast_alerts_mode_rolling")),
        choiceValues = list("count", "rolling"),
        selected     = "count"
      )
    ),
    shiny::uiOutput(ns("panel"))
  )
}

#' Alertes FAST sub-tab server
#'
#' @param id Module namespace id.
#' @param app_state Parent `reactiveValues` carrying `language`,
#'   `current_project`.
#' @param zone_id_r Reactive returning the active monitoring zone id
#'   (chr or int — coerced to integer inside).
#' @param date_range_r Reactive returning a length-2 Date vector
#'   `[date_from, date_to]`.
#' @param thresholds_r Reactive returning `list(ndvi, nbr, window_days)`.
#' @param refresh_r Reactive (length-1 integer). Bump-counter signalling
#'   that the source FAST data has changed (typically after a Sentinel-2
#'   ingestion success). Taken as an explicit dep by the raster reactive
#'   and the cache resolver so they re-evaluate without forcing the user
#'   to wiggle a slider. v0.42.0.
#' @return invisible list with `raster` reactive (the SpatRaster
#'   currently displayed, NULL when no data).
#' @noRd
mod_monitoring_fast_alerts_server <- function(id, app_state, zone_id_r,
                                              date_range_r, thresholds_r,
                                              refresh_r = shiny::reactive(0L)) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # Refresh radio labels on language change. Preserves selection by
    # echoing back input$mode (or default "count" before first click).
    shiny::observe({
      i18n <- i18n_r()
      shiny::updateRadioButtons(
        session, "mode",
        label = NULL,
        choiceNames  = list(i18n$t("monitoring_fast_alerts_mode_count"),
                            i18n$t("monitoring_fast_alerts_mode_rolling")),
        choiceValues = list("count", "rolling"),
        selected     = input$mode %||% "count"
      )
    })

    # ----- Cache dir + raster reactive --------------------------------
    # read_fast_alert_raster() reads from the on-disk Sentinel-2 cache
    # (`<project>/cache/layers/sentinel2`) rather than touching the DB
    # rows directly. NULL when no project is loaded or the cache hasn't
    # been populated yet by the ingestion worker.
    #
    # `refresh_r()` is taken as an explicit dep so the reactive
    # re-evaluates after an ingestion that may have just created the
    # cache directory — symmetric with mod_monitoring_pixel_map.
    cache_dir_r <- shiny::reactive({
      refresh_r()
      proj <- app_state$current_project
      if (is.null(proj)) return(NULL)
      ppath <- proj$path %||% proj$project_path
      if (is.null(ppath) || !nzchar(ppath)) return(NULL)
      cd <- file.path(ppath, "cache", "layers", "sentinel2")
      if (!dir.exists(cd)) return(NULL)
      cd
    })

    raster_r <- shiny::reactive({
      refresh_r()
      zone <- zone_id_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return(NULL)
      dr <- date_range_r()
      if (length(dr) != 2L || any(is.na(dr))) return(NULL)
      th <- thresholds_r()
      if (is.null(th) || is.null(th$ndvi) || is.null(th$nbr)) return(NULL)
      cd <- cache_dir_r()
      if (is.null(cd)) return(NULL)
      proj <- app_state$current_project
      if (is.null(proj)) return(NULL)
      con <- get_monitoring_db_connection(project = proj)
      if (is.null(con)) return(NULL)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      mode <- input$mode %||% "count"
      tryCatch(
        nemeton::read_fast_alert_raster(
          con,
          zone_id        = as.integer(zone),
          threshold_ndvi = as.numeric(th$ndvi),
          threshold_nbr  = as.numeric(th$nbr),
          date_from      = dr[1],
          date_to        = dr[2],
          mode           = mode,
          window_days    = as.integer(th$window_days %||% 30L),
          cache_dir      = cd
        ),
        error = function(e) {
          cli::cli_alert_warning(
            "read_fast_alert_raster failed: {e$message}"
          )
          NULL
        }
      )
    })

    # ----- Panel : empty state or leaflet output ----------------------
    output$panel <- shiny::renderUI({
      i18n <- i18n_r()
      r <- raster_r()
      if (is.null(r) || .fast_raster_is_empty(r)) {
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
      r <- raster_r()
      if (is.null(r) || .fast_raster_is_empty(r)) return(NULL)
      i18n <- i18n_r()
      mode <- input$mode %||% "count"

      # Zero = pas d'alerte → transparent. Le cœur retourne 0 (count)
      # ou 0.0 (rolling) hors alerte, qu'on masque ici pour ne pas
      # noircir / saturer la carte de pixels uniformes.
      r_show <- terra::ifel(r == 0, NA, r)

      # v0.45.0 — overlay UGF (polygones bleu vif) au-dessus du
      # raster d'alerte pour le repère spatial demandé après les
      # tests user villards.
      ugf_4326 <- .ugf_for_overlay(app_state$current_project)

      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",     group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonAlertRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          baseGroups    = c("OSM", "Satellite"),
          overlayGroups = if (!is.null(ugf_4326)) "UGF" else NULL,
          options       = leaflet::layersControlOptions(collapsed = TRUE)
        )
      if (!is.null(ugf_4326)) {
        m <- m |>
          leaflet::addPolygons(
            data        = ugf_4326,
            group       = "UGF",
            color       = "#1f78b4",
            weight      = 2,
            opacity     = 0.9,
            fillOpacity = 0
          )
      }

      if (identical(mode, "count")) {
        # v0.45.0 — classification adaptative par quartiles sur les
        # valeurs non-nulles. La version précédente (fixed bins
        # 1-2 / 3-5 / 6+) écrasait tout en rouge dès qu'une scène
        # avait > 10 alertes, masquant les gradients sur les zones
        # moyennement touchées. Les quartiles offrent un découpage
        # toujours lisible quel que soit le max.
        cls <- .classify_alert_count(r_show)
        if (length(cls$breaks) < 2L) return(m)
        cols <- .alert_count_palette(length(cls$labels))
        pal <- leaflet::colorBin(
          palette  = cols,
          domain   = range(cls$breaks),
          bins     = cls$breaks,
          na.color = "transparent"
        )
        m |>
          leaflet::addRasterImage(
            x = r_show, colors = pal, opacity = 0.75,
            method  = "ngb",
            options = leaflet::gridOptions(pane = "nemetonAlertRaster")
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            colors   = cols,
            labels   = cls$labels,
            title    = i18n$t("monitoring_fast_alerts_legend_count_title"),
            opacity  = 0.85
          )
      } else {
        # Rolling : magnitude continue. Borne supérieure = p95 pour
        # éviter qu'une queue minoritaire écrase l'échelle visuelle.
        vals <- terra::values(r_show)
        vals <- vals[!is.na(vals) & vals > 0]
        if (!length(vals)) return(m)
        upper <- as.numeric(stats::quantile(vals, 0.95, na.rm = TRUE))
        if (!is.finite(upper) || upper <= 0) upper <- max(vals, na.rm = TRUE)
        pal <- leaflet::colorNumeric(
          palette  = c("#FFD27F", "#FF9933", "#D62728"),
          domain   = c(0, upper),
          na.color = "transparent"
        )
        m |>
          leaflet::addRasterImage(
            x = r_show, colors = pal, opacity = 0.75,
            options = leaflet::gridOptions(pane = "nemetonAlertRaster")
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            pal      = pal,
            values   = c(0, upper),
            title    = i18n$t("monitoring_fast_alerts_legend_title"),
            opacity  = 0.85
          )
      }
    })

    # v0.37.1 — see mod_monitoring_fordead_map.R : the navset toggles
    # nav_panels with bslib::nav_show() / nav_hide(), which leaves
    # Shiny's visibility detection unreliable. Force the panel
    # uiOutput to evaluate unconditionally so empty-state / leaflet
    # wrapper render the moment the user opens the sub-tab.
    shiny::outputOptions(output, "panel", suspendWhenHidden = FALSE)

    invisible(list(raster = raster_r))
  })
}

# A raster is "empty" when no pixel carries an alert. NA-safe.
.fast_raster_is_empty <- function(r) {
  if (is.null(r)) return(TRUE)
  if (!inherits(r, "SpatRaster")) return(TRUE)
  if (terra::ncell(r) == 0L) return(TRUE)
  mx <- tryCatch(
    as.numeric(terra::global(r, "max", na.rm = TRUE)[[1]]),
    error = function(e) NA_real_
  )
  is.na(mx) || mx <= 0
}


# v0.45.0 — adaptive quartile classification for the count-mode
# alert raster. Computes 4 bins on the non-zero values' quartiles
# (Q0..Q25..Q50..Q75..Q100), rounds breaks to integers, fuses
# duplicates (collapses to fewer than 4 bins when the distribution
# is degenerate — e.g. constant value 1, or only two distinct
# counts). Returns NULL-shaped (length-1 breaks) when the raster
# has no positive value, so the caller can early-return.
#
# Pair with `.alert_count_palette(n)` for the matching color ramp.
#
# @param r SpatRaster with NA outside the AOI and zeros masked
#   upstream.
# @return list(breaks, labels). Breaks include a leading 0 so the
#   legend's lower bound matches the no-alert pixels. Labels are
#   integer ranges or `"M-N"` strings.
.classify_alert_count <- function(r) {
  vals <- tryCatch(terra::values(r), error = function(e) NULL)
  if (is.null(vals)) return(list(breaks = c(0, 1), labels = "1+"))
  vals <- vals[!is.na(vals) & vals > 0]
  if (!length(vals)) return(list(breaks = c(0, Inf), labels = character(0)))
  q <- as.numeric(stats::quantile(vals, probs = c(0, .25, .50, .75, 1.0),
                                  na.rm = TRUE))
  q <- unique(as.integer(round(q)))           # collapse duplicates
  q <- q[q > 0]                                # zero is the lower bound
  if (!length(q)) return(list(breaks = c(0, 1), labels = "1+"))
  if (length(q) == 1L) {
    # Single distinct value (constant raster) — one bucket only.
    return(list(breaks = c(0, q[1] + 0.5),
                labels = sprintf("%d", q[1])))
  }
  # Build labels for n+1 breakpoints (n bins).
  # First label : "Qmin" or "Qmin-Q1" depending on whether the
  # quartiles span a range.
  labels <- character(0)
  q_pad  <- c(0, q)
  for (i in seq_len(length(q_pad) - 1L)) {
    lo <- q_pad[i] + 1L
    hi <- q_pad[i + 1L]
    labels <- c(labels,
                if (lo == hi) sprintf("%d", lo) else sprintf("%d-%d", lo, hi))
  }
  # Top bucket gets a "Q4+" cue when the max quantile is the actual
  # max (informative for users : "this many alert-days max").
  if (length(labels)) {
    labels[length(labels)] <- sprintf("%d+", q_pad[length(q_pad) - 1L] + 1L)
  }
  # Bins for colorBin : c(0.5, Q1+.5, Q2+.5, Q3+.5, Q4+.5).
  bins <- c(0.5, q + 0.5)
  list(breaks = bins, labels = labels)
}


# Color ramp for the count-mode legend. `n` is the number of bins
# (length of `.classify_alert_count$labels`). Ramps from pale-orange
# to deep red ; gracefully degrades to fewer colors when the
# distribution collapses.
.alert_count_palette <- function(n) {
  full <- c("#FFEDA0", "#FED976", "#FEB24C", "#FC4E2A", "#B10026")
  if (n <= 1L) return(full[length(full)])  # single bucket → deepest red
  if (n >= length(full)) return(full)
  # Pick `n` evenly-spaced shades from the 5-color ramp.
  full[round(seq.int(1L, length(full), length.out = n))]
}


# v0.45.0 — best-effort UGF polygon resolver for the leaflet overlay.
# Returns the project UGFs reprojected to WGS84, or NULL when the
# project has no `indicators_sf` (no indicator computation yet) —
# in which case the map shows just the raster without spatial
# reference outlines.
.ugf_for_overlay <- function(project) {
  if (is.null(project)) return(NULL)
  geom <- project$indicators_sf
  if (is.null(geom) || !inherits(geom, "sf") || !nrow(geom)) return(NULL)
  tryCatch(sf::st_transform(geom, 4326), error = function(e) NULL)
}

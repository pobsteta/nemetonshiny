# mod_monitoring_pixel_map.R — Sub-tab "Carte pixel" of Suivi sanitaire
#
# Renders the cached Sentinel-2 NDVI / NBR rasters at native 10 m
# resolution as a leaflet layer, with a date slider, an index toggle,
# and a per-pixel time series plotly that opens on map click.
#
# Spec 010 (carte pixel + time series interactive). Consumes 4 read-
# only public functions from `nemeton@>=v0.22.0`:
#   * read_s2_band_stack()
#   * build_index_stack()
#   * extract_pixel_timeseries()
#   * read_obs_pixel()  (re-used for the placettes overlay only)
#
# All four go through `cache_dir = <project>/cache/layers/sentinel2`,
# zero HTTP and zero DB write — the map is fully offline once the
# ingestion has populated the cache.

#' Pixel map sub-tab UI
#'
#' @param id Module namespace.
#' @return A `bslib::card` tag list to drop into a `bslib::nav_panel`.
#' @noRd
mod_monitoring_pixel_map_ui <- function(id) {
  ns <- shiny::NS(id)
  i18n <- get_i18n(get_app_options()$language %||% "fr")

  bslib::card(
    bslib::card_header(
      htmltools::div(
        class = "d-flex align-items-center",
        bsicons::bs_icon("map", class = "me-2"),
        i18n$t("monitoring_pixel_map_title")
      )
    ),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 250L, position = "right", open = "always",
        shiny::radioButtons(
          ns("index"),
          i18n$t("monitoring_pixel_map_index"),
          choices  = c(NDVI = "NDVI", NBR = "NBR"),
          selected = "NDVI",
          inline   = TRUE
        ),
        shiny::uiOutput(ns("date_slider_ui")),
        shiny::helpText(
          class = "small text-muted fst-italic",
          i18n$t("monitoring_pixel_map_click_hint")
        ),
        shiny::uiOutput(ns("scene_count_hint"))
      ),
      htmltools::div(
        style = "position: relative;",
        leaflet::leafletOutput(ns("map"), height = "55vh"),
        shiny::uiOutput(ns("loading_overlay"))
      )
    )
  )
}

#' Pixel map sub-tab server
#'
#' @param id Module namespace.
#' @param app_state Shared `reactiveValues` (carries `language`,
#'   `current_project`).
#' @param obs_pixel_data Reactive returning the per-plot obs from
#'   `nemeton::read_obs_pixel()` (already wired in the parent
#'   `mod_monitoring_server`). Used to derive `scenes_df`
#'   (DISTINCT scene_id × obs_date) without a second SQL query.
#' @param mode_input Reactive returning the current mode
#'   (`"quick"` / `"health"`). The pixel map only makes sense in
#'   quick mode (FORDEAD doesn't expose per-pixel raster output).
#'
#' @noRd
mod_monitoring_pixel_map_server <- function(id, app_state,
                                            obs_pixel_data,
                                            mode_input) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # Loading flag — toggles the spinning gear overlay on the map.
    loading <- shiny::reactiveVal(FALSE)
    output$loading_overlay <- shiny::renderUI({
      if (!isTRUE(loading())) return(NULL)
      i18n <- i18n_r()
      htmltools::div(
        class = "position-absolute top-50 start-50 translate-middle",
        style = paste("z-index: 1000; padding: 12px 18px;",
                      "background: rgba(255,255,255,0.85);",
                      "border-radius: 6px;",
                      "box-shadow: 0 2px 6px rgba(0,0,0,0.15);"),
        shiny::icon("gear", class = "fa-spin me-2"),
        i18n$t("monitoring_pixel_map_loading")
      )
    })

    # Resolve <project>/cache/layers/sentinel2 from the active
    # project. NULL when no project is loaded.
    cache_dir_r <- shiny::reactive({
      proj <- app_state$current_project
      if (is.null(proj)) return(NULL)
      ppath <- proj$path %||% proj$project_path
      if (is.null(ppath) || !nzchar(ppath)) return(NULL)
      cd <- file.path(ppath, "cache", "layers", "sentinel2")
      if (!dir.exists(cd)) return(NULL)
      cd
    })

    # scenes_df = DISTINCT (scene_id, obs_date) from the per-plot
    # obs already fetched by the parent module's obs_pixel_data
    # reactive. Avoids a second roundtrip to the DB.
    scenes_df_r <- shiny::reactive({
      df <- obs_pixel_data()
      if (is.null(df) || !nrow(df)) return(NULL)
      out <- unique(df[, c("scene_id", "obs_date"), drop = FALSE])
      out <- out[order(out$obs_date), , drop = FALSE]
      rownames(out) <- NULL
      out
    })

    # Build the multi-temporal index stack. Heavy compute (reads N
    # GeoTIFFs from disk + arithmetic), debounced on (cache_dir,
    # scenes_df, index) — re-runs only when one of those changes.
    pixel_stack_r <- shiny::reactive({
      shiny::req(identical(mode_input(), "quick"))
      cd <- cache_dir_r()
      sdf <- scenes_df_r()
      if (is.null(cd) || is.null(sdf) || !nrow(sdf)) return(NULL)
      loading(TRUE)
      on.exit(loading(FALSE), add = TRUE)
      tryCatch(
        nemeton::build_index_stack(cd, sdf, index = input$index),
        error = function(e) {
          cli::cli_alert_warning(sprintf(
            "build_index_stack failed: %s", conditionMessage(e)))
          NULL
        }
      )
    })

    # Date slider built dynamically from the layer names of the
    # stack (one layer per obs_date).
    output$date_slider_ui <- shiny::renderUI({
      ns <- session$ns
      i18n <- i18n_r()
      st <- pixel_stack_r()
      if (is.null(st)) {
        return(shiny::helpText(
          class = "small text-muted",
          i18n$t("monitoring_pixel_map_no_cache")
        ))
      }
      dates <- as.Date(names(st))
      dates <- dates[!is.na(dates)]
      if (!length(dates)) return(NULL)
      shiny::sliderInput(
        ns("date"),
        i18n$t("monitoring_pixel_map_date"),
        min   = min(dates),
        max   = max(dates),
        value = max(dates),
        timeFormat = "%Y-%m-%d",
        step  = 1L,
        animate = shiny::animationOptions(interval = 800L,
                                          loop     = FALSE)
      )
    })

    # Snap the requested date to the closest scene actually present
    # in the stack (the slider lets the user drag day-by-day, but
    # the data is sparse — typically every 5 days max).
    current_layer_r <- shiny::reactive({
      st <- pixel_stack_r()
      if (is.null(st)) return(NULL)
      target <- input$date
      if (is.null(target)) return(NULL)
      dates <- as.Date(names(st))
      idx <- which.min(abs(as.numeric(dates - as.Date(target))))
      if (!length(idx)) return(NULL)
      st[[idx]]
    })

    output$scene_count_hint <- shiny::renderUI({
      i18n <- i18n_r()
      sdf <- scenes_df_r()
      if (is.null(sdf)) return(NULL)
      htmltools::tags$p(
        class = "small text-muted mb-0",
        sprintf(i18n$t("monitoring_pixel_map_scene_count_fmt"),
                nrow(sdf))
      )
    })

    # Internal group labels for clearGroup() bookkeeping in the
    # observes below. NOT listed in addLayersControl(overlayGroups=)
    # anymore (see v0.30.2 fix): when overlayGroups is set in
    # renderLeaflet BEFORE the layers are added via leafletProxy, the
    # control's checkboxes are bound to undefined layer references,
    # and depending on leaflet R / timing the overlays end up
    # invisible despite the checkboxes appearing checked. Same
    # approach as the alerts_map in this module — base-layer toggle
    # only, overlays always visible. (Same constraint that drove the
    # v0.28.4 attempt at overlayGroups; that fix never really worked,
    # only masked by other behaviors in some cases.)
    .ugf_overlay_group       <- "UGF"
    .pixel_overlay_group     <- "NDVI / NBR"
    .placettes_overlay_group <- "Placettes"

    # Static map skeleton: rendered ONCE. Re-rendering the whole map on
    # every date tick would reset the user's base-layer choice
    # (Satellite → OSM) because `baseGroups` re-applies the first entry
    # as default on each remount. Raster + markers + legend updates go
    # through leafletProxy() below — they appear on the map and stay
    # visible regardless of base-layer toggle (overlayPane > tilePane
    # by default in Leaflet).
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",   group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          options    = leaflet::layersControlOptions(collapsed = TRUE)
        )
    })

    # Index value range is roughly [-1, 1] for both NDVI and NBR.
    # Use a divergent palette anchored on 0; NA stays transparent.
    .pixel_palette <- leaflet::colorNumeric(
      palette  = c("#D62728", "#FFD27F", "#FFFFCC",
                   "#A8DDB5", "#2CA02C"),
      domain   = c(-1, 1),
      na.color = "transparent"
    )

    # Reactive raster + legend swap. Fires on date / index / project
    # changes; preserves the base-layer selection because the map
    # widget itself is not re-rendered.
    #
    # priority = 100 (above the default 0) so this observe runs FIRST
    # in any reactive flush where both this and the UGF observe are
    # dirty — guarantees the raster gets added BEFORE the UGF
    # polygons, so the polygons end up on top in overlayPane DOM
    # order (Leaflet renders within a pane in the order layers are
    # added). Without this, on project load Shiny could run the UGF
    # observe first, then the raster paint over the polygons.
    shiny::observe({
      r <- current_layer_r()
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearImages() |>
        leaflet::removeControl("pixel_legend")
      if (is.null(r)) return()
      proxy |>
        leaflet::addRasterImage(
          x       = r,
          colors  = .pixel_palette,
          # v0.31.1: bumped 0.75 → 0.85 because the NDVI/NBR palette
          # (greens / yellows / oranges) shares its tonal range with
          # Esri.WorldImagery forest/farmland imagery — at 0.75 the
          # raster was invisible on Satellite background. 0.85 is
          # enough to make the gradient pop without entirely hiding
          # the underlying tile (useful for spatial context).
          opacity = 0.85,
          group   = .pixel_overlay_group
        ) |>
        leaflet::addLegend(
          layerId  = "pixel_legend",
          position = "bottomright",
          pal      = .pixel_palette,
          values   = c(-1, 1),
          title    = input$index,
          opacity  = 0.85
        )
    }, priority = 100L)

    # UGF / study-area overlay: outline drawn over the raster to give
    # the user a visible boundary of the area being analyzed.
    #
    # Fallback chain (first non-NULL wins):
    #   1. `current_project$indicators_sf` — UGFs + indicator merge,
    #      built by service_project.R::load_project after indicator
    #      computation.
    #   2. `ug_build_sf(current_project)` — raw UGF polygons from
    #      `ugs.json` + `tenements`. Available as soon as UGFs are
    #      defined in the UG tab, even before indicators run.
    #   3. **Raster bbox** — extent of the current `pixel_stack_r()`
    #      as a single rectangle. Useful when the project ships
    #      placettes but no formally defined UGFs (the analysis area
    #      is implicit in the ingestion).
    #   4. **Placettes bbox** — extent of the current `placettes_sf_r()`.
    #      Last-resort fallback when even the raster isn't built yet.
    #
    # In cases (3) and (4), the "UGF" label is technically a misnomer
    # (we're drawing a study-area rectangle, not actual UGFs), but
    # the visible behavior — an orange outline around the analysis
    # area — is what users want, and it doesn't make sense to leave
    # the map without spatial context. The cli logs below tell you
    # which fallback fired.
    #
    # CRS typically EPSG:2154 for sources 1-2 — st_transform to 4326
    # for Leaflet. Sources 3-4 already in 4326.
    ugf_sf_r <- shiny::reactive({
      proj <- app_state$current_project
      if (is.null(proj)) return(NULL)
      to_wgs84 <- function(x) {
        if ((sf::st_crs(x)$epsg %||% 4326L) != 4326L) {
          return(tryCatch(sf::st_transform(x, 4326),
                          error = function(e) NULL))
        }
        x
      }

      # 1. indicators_sf
      geom <- proj$indicators_sf
      if (!is.null(geom) && inherits(geom, "sf") && nrow(geom)) {
        cli::cli_alert_info("UGF source: indicators_sf ({nrow(geom)} feature(s)).")
        return(to_wgs84(geom))
      }

      # 2. raw ug_build_sf
      geom <- tryCatch(ug_build_sf(proj), error = function(e) NULL)
      if (!is.null(geom) && inherits(geom, "sf") && nrow(geom)) {
        cli::cli_alert_info("UGF source: ug_build_sf ({nrow(geom)} feature(s)).")
        return(to_wgs84(geom))
      }

      # 3. raster bbox
      r_stack <- tryCatch(pixel_stack_r(), error = function(e) NULL)
      if (!is.null(r_stack)) {
        ext_vec <- tryCatch(as.vector(terra::ext(r_stack)),
                            error = function(e) NULL)
        crs_str <- tryCatch(terra::crs(r_stack, proj = TRUE),
                            error = function(e) NULL)
        if (!is.null(ext_vec) && !is.null(crs_str) && nzchar(crs_str)) {
          bb <- sf::st_bbox(
            c(xmin = ext_vec[1], xmax = ext_vec[2],
              ymin = ext_vec[3], ymax = ext_vec[4]),
            crs = sf::st_crs(crs_str)
          )
          geom <- tryCatch(sf::st_as_sf(sf::st_as_sfc(bb)),
                           error = function(e) NULL)
          if (!is.null(geom) && nrow(geom)) {
            cli::cli_alert_info("UGF source: raster bbox (fallback, no UGFs defined).")
            return(to_wgs84(geom))
          }
        }
      }

      # 4. placettes bbox
      ps <- tryCatch(placettes_sf_r(), error = function(e) NULL)
      if (!is.null(ps) && nrow(ps)) {
        bb <- tryCatch(sf::st_bbox(ps), error = function(e) NULL)
        if (!is.null(bb)) {
          geom <- tryCatch(sf::st_as_sf(sf::st_as_sfc(bb)),
                           error = function(e) NULL)
          if (!is.null(geom) && nrow(geom)) {
            cli::cli_alert_info("UGF source: placettes bbox (last-resort fallback).")
            return(geom)  # already WGS84 via placettes_sf_r
          }
        }
      }

      cli::cli_alert_info("UGF source: NONE — no indicators_sf, no ug_build_sf, no raster, no placettes.")
      NULL
    })

    # UGF polygons swap. Outline only (near-transparent fill) so the
    # raster underneath stays visible. Bright orange border (#FF6B35,
    # weight 3) — high contrast on both OSM (light bg) and Satellite
    # (forest greens). cli debug log on each fire so a developer
    # running from a terminal sees the reactive firing.
    #
    # IMPORTANT — overlay stacking order (v0.31.2 dep + v0.31.4
    # priority): the raster, UGF polygons and placette CircleMarkers
    # all live in Leaflet's `overlayPane` (CircleMarkers are Paths,
    # not L.Marker). DOM order within a pane determines click
    # routing: the last-added layer is on top AND receives the
    # clicks.
    #
    # Strict priority ladder ensures the order is deterministic:
    #   raster    priority = 100  → first  → bottom
    #   ugf       priority =  50  → second → middle (this observe)
    #   placettes priority =   0  → third  → top (clickable!)
    #
    # The `current_layer_r()` dummy read re-fires this observe after
    # each raster update so polygons get re-stacked above a freshly
    # painted raster (date slider, index toggle, etc.).
    shiny::observe({
      current_layer_r()  # re-fire after each raster update
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup(.ugf_overlay_group)
      ugf <- ugf_sf_r()
      if (is.null(ugf) || !nrow(ugf)) {
        cli::cli_alert_info("UGF overlay: ugf_sf_r() is NULL/empty, no polygons drawn.")
        return()
      }
      cli::cli_alert_info("UGF overlay: drawing {nrow(ugf)} polygon(s).")
      proxy |>
        leaflet::addPolygons(
          data        = ugf,
          group       = .ugf_overlay_group,
          color       = "#FF6B35",
          weight      = 3,
          opacity     = 1.0,
          fillColor   = "#FF6B35",
          fillOpacity = 0.05
        )
    }, priority = 50L)

    # Auto-fit on project load + when the user navigates to the Carte
    # pixel sub-tab. Same problem as mod_ug.R:744-794 — Leaflet's
    # fitBounds() is a no-op when the map container has zero size,
    # which happens whenever the sub-tab is hidden. On project load,
    # the user might be on any other tab; my proxy commands queue
    # but, even after the queue is replayed when the sub-tab becomes
    # visible, Leaflet silently drops fitBounds if the map hasn't
    # picked up its container dimensions yet.
    #
    # Pattern mirrors mod_ug:
    #   1. Detect navigation to Carte pixel via the parent session's
    #      main_nav + monitoring-subtab inputs (accessed through
    #      session$userData$root_session — populated by app_server.R).
    #   2. Schedule a delayed re-fit via later::later (0.3 s) to let
    #      the DOM settle.
    #   3. Issue leafletInvalidateSize (custom JS handler defined in
    #      inst/app/www/js/custom.js:169) so Leaflet re-detects the
    #      container size, then fitBounds on the UGF bbox.
    #
    # The .last_fitted_id guard is dropped — the navigation observe
    # is naturally throttled by tab visibility (it only fires when
    # the user is actually looking at Carte pixel), so re-fitting on
    # every sub-tab visit is fine and even desirable (covers the
    # case where the user resizes the window and the bounds drifted).
    shiny::observe({
      root_session <- session$userData$root_session
      if (is.null(root_session)) return()
      top_nav <- root_session$input$main_nav
      sub_nav <- root_session$input[["monitoring-subtab"]]
      if (is.null(top_nav) || top_nav != "monitoring") return()
      if (is.null(sub_nav) || sub_nav != "pixel_map") return()

      proj <- shiny::isolate(app_state$current_project)
      if (is.null(proj) || is.null(proj$id)) return()
      ugf <- shiny::isolate(ugf_sf_r())
      if (is.null(ugf) || !nrow(ugf)) {
        cli::cli_alert_info("Pixel map auto-zoom on tab visit: ugf_sf_r() NULL, skip.")
        return()
      }
      bb <- sf::st_bbox(ugf)
      cli::cli_alert_info(
        "Pixel map auto-zoom on tab visit: fitBounds [{round(bb[['xmin']], 4)}, {round(bb[['ymin']], 4)}] - [{round(bb[['xmax']], 4)}, {round(bb[['ymax']], 4)}] for project {proj$id}."
      )

      # Delay 300 ms so the DOM has time to lay out the (formerly
      # hidden) map container before invalidateSize + fitBounds fire.
      later::later(function() {
        session$sendCustomMessage("leafletInvalidateSize", list(
          id = session$ns("map")
        ))
        leaflet::leafletProxy("map", session = session) |>
          leaflet::fitBounds(
            lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
            lng2 = bb[["xmax"]], lat2 = bb[["ymax"]]
          )
      }, delay = 0.3)
    })

    # Placettes overlay: load_samples() returns sf POINT (EPSG:2154
    # by convention). Filter to plot_ids that actually have observations
    # in the current window so the markers reflect the same scope as the
    # rest of the sub-tab. NULL when no plan, no project, no obs.
    placettes_sf_r <- shiny::reactive({
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$id)) return(NULL)
      df <- obs_pixel_data()
      if (is.null(df) || !nrow(df)) return(NULL)
      plots <- tryCatch(load_samples(proj$id, layer = "plots"),
                        error = function(e) NULL)
      if (is.null(plots) || !inherits(plots, "sf") || !nrow(plots)) {
        return(NULL)
      }
      if (!"plot_id" %in% names(plots)) return(NULL)
      plots <- plots[as.character(plots$plot_id) %in%
                     unique(as.character(df$plot_id)), , drop = FALSE]
      if (!nrow(plots)) return(NULL)
      # Leaflet expects WGS84.
      if ((sf::st_crs(plots)$epsg %||% 4326L) != 4326L) {
        plots <- sf::st_transform(plots, 4326)
      }
      plots
    })

    # Markers swap. Same proxy pattern as the raster observe — clears
    # the overlay group then re-adds. layerId = plot_id so the click
    # handler downstream can identify which placette was tapped.
    # Radius bumped to 7 + black border so the markers stay readable
    # at the wide auto-fit zoom (a few km across).
    #
    # IMPORTANT — overlay stacking order (v0.31.4):
    #
    # CircleMarkers are Leaflet Paths (NOT L.Marker), so they live in
    # `overlayPane` along with the raster ImageOverlay and the UGF
    # polygons. Within a pane, Leaflet renders in DOM order (last
    # added = on top, and on top = receives clicks). So the placettes
    # observe MUST run AFTER the raster and UGF observes within the
    # same flush, otherwise the polygons end up over the markers and
    # intercept the user's clicks.
    #
    # Strict priority ladder across the three observes:
    #   raster    priority = 100  → first  → bottom (under polygons)
    #   ugf       priority =  50  → second → middle (under markers)
    #   placettes priority =   0  → third  → top    (clickable!)
    #
    # The dummy `current_layer_r()` read recreates the same dependency
    # the UGF observe has — so when the date slider moves, this observe
    # re-fires too and re-adds the markers AFTER the fresh raster /
    # polygons in DOM order, staying clickable.
    shiny::observe({
      current_layer_r()  # re-fire after each raster update
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup(.placettes_overlay_group)
      ps <- placettes_sf_r()
      if (is.null(ps) || !nrow(ps)) {
        cli::cli_alert_info("Placettes overlay: placettes_sf_r() is NULL/empty, no markers drawn.")
        return()
      }
      cli::cli_alert_info("Placettes overlay: drawing {nrow(ps)} marker(s).")
      proxy |>
        leaflet::addCircleMarkers(
          data        = ps,
          layerId     = ~as.character(plot_id),
          group       = .placettes_overlay_group,
          radius      = 7,
          weight      = 2,
          color       = "#000000",
          fillColor   = "#1F77B4",
          fillOpacity = 0.9,
          label       = ~as.character(plot_id)
        )
    }, priority = 0L)

    # Suppression flag for the pixel-click handler. CircleMarkers are
    # Leaflet Paths; their click events propagate to the map, so the
    # marker handler AND the pixel handler both fire on a single
    # marker tap. Without this, clicking a placette opens the placette
    # modal AND immediately stacks the pixel modal on top of it. The
    # marker handler stamps Sys.time() here; the pixel handler bails
    # if the timestamp is within 500 ms of its own invocation.
    marker_just_clicked <- shiny::reactiveVal(NULL)

    # Marker click handler: open a modal with the per-placette NDVI/NBR
    # time series. Reuses obs_pixel_data() — the values are already the
    # placette mean computed core-side, no re-aggregation needed.
    shiny::observeEvent(input$map_marker_click, {
      marker_just_clicked(Sys.time())
      i18n <- i18n_r()
      ev <- input$map_marker_click
      pid <- as.character(ev$id %||% "")
      if (!nzchar(pid)) return()
      df <- obs_pixel_data()
      if (is.null(df) || !nrow(df)) return()
      sub <- df[as.character(df$plot_id) == pid, , drop = FALSE]
      # Restrict to NDVI / NBR even if the user picked more bands —
      # consistent with the pixel-click modal.
      sub <- sub[as.character(sub$band) %in% c("NDVI", "NBR"), ,
                 drop = FALSE]
      if (!nrow(sub)) {
        shiny::showNotification(
          i18n$t("monitoring_pixel_map_no_placette_data"),
          type = "warning", duration = 4
        )
        return()
      }
      band_colors <- c(NDVI = "#2CA02C", NBR = "#D62728")
      p <- plotly::plot_ly(type = "scatter", mode = "lines+markers")
      for (b in unique(sub$band)) {
        bsub <- sub[sub$band == b, , drop = FALSE]
        bsub <- bsub[order(bsub$obs_date), , drop = FALSE]
        col  <- band_colors[b] %||% "#7F7F7F"
        p <- plotly::add_trace(
          p,
          x      = as.Date(bsub$obs_date),
          y      = as.numeric(bsub$value),
          name   = b,
          mode   = "lines+markers",
          line   = list(color = unname(col), width = 1.5),
          marker = list(color = unname(col), size = 5),
          hovertemplate = paste0(
            "<b>", b, "</b><br>",
            "%{x|%Y-%m-%d}<br>",
            b, " = %{y:.3f}<extra></extra>"
          )
        )
      }
      p <- plotly::layout(
        p,
        margin = list(t = 20, b = 40, l = 50, r = 10),
        xaxis  = list(title = i18n$t("monitoring_timeseries_xaxis"),
                      type = "date"),
        yaxis  = list(title = i18n$t("monitoring_timeseries_yaxis"),
                      range = c(-0.2, 1)),
        legend = list(orientation = "h", y = -0.25)
      )
      shiny::showModal(shiny::modalDialog(
        title = sprintf(
          i18n$t("monitoring_pixel_map_placette_modal_title_fmt"),
          pid
        ),
        size      = "l",
        easyClose = TRUE,
        plotly::plotlyOutput(session$ns("placette_ts_plot"),
                             height = "320px"),
        footer = shiny::modalButton(i18n$t("close"))
      ))
      output$placette_ts_plot <- plotly::renderPlotly(p)
    })

    # Click handler: extract the per-pixel time series at the clicked
    # coordinate and pop a modal with NDVI + NBR superimposed.
    shiny::observeEvent(input$map_click, {
      # Bail if a placette marker was just clicked — its click event
      # propagated up to map_click (CircleMarkers are Leaflet Paths
      # which bubble clicks by default). Without this guard, tapping a
      # placette opens the placette modal AND immediately stacks the
      # pixel modal on top.
      last_marker <- marker_just_clicked()
      if (!is.null(last_marker) &&
          as.numeric(Sys.time() - last_marker, units = "secs") < 0.5) {
        return()
      }
      i18n <- i18n_r()
      cd <- cache_dir_r()
      sdf <- scenes_df_r()
      if (is.null(cd) || is.null(sdf) || !nrow(sdf)) return()
      lat <- input$map_click$lat
      lng <- input$map_click$lng
      if (is.null(lat) || is.null(lng)) return()

      ts <- tryCatch(
        nemeton::extract_pixel_timeseries(
          cache_dir = cd, scenes_df = sdf,
          xy        = c(lng, lat),
          crs       = 4326,
          indices   = c("NDVI", "NBR")
        ),
        error = function(e) {
          cli::cli_alert_warning(sprintf(
            "extract_pixel_timeseries failed: %s",
            conditionMessage(e)))
          NULL
        }
      )
      if (is.null(ts) || !nrow(ts)) {
        shiny::showNotification(
          i18n$t("monitoring_pixel_map_no_pixel"),
          type = "warning", duration = 4
        )
        return()
      }

      # Stable per-band colors (same palette as the per-plot view
      # in v0.27.0 — keeps the user's mental model consistent).
      band_colors <- c(NDVI = "#2CA02C", NBR = "#D62728")
      p <- plotly::plot_ly(type = "scatter", mode = "lines+markers")
      for (b in unique(ts$index)) {
        sub <- ts[ts$index == b, , drop = FALSE]
        col <- band_colors[b] %||% "#7F7F7F"
        p <- plotly::add_trace(
          p,
          x      = as.Date(sub$obs_date),
          y      = as.numeric(sub$value),
          name   = b,
          mode   = "lines+markers",
          line   = list(color = unname(col), width = 1.5),
          marker = list(color = unname(col), size = 5),
          hovertemplate = paste0(
            "<b>", b, "</b><br>",
            "%{x|%Y-%m-%d}<br>",
            b, " = %{y:.3f}<extra></extra>"
          )
        )
      }
      p <- plotly::layout(
        p,
        margin = list(t = 20, b = 40, l = 50, r = 10),
        xaxis  = list(title = i18n$t("monitoring_timeseries_xaxis"),
                      type = "date"),
        yaxis  = list(title = i18n$t("monitoring_timeseries_yaxis"),
                      range = c(-0.2, 1)),
        legend = list(orientation = "h", y = -0.25)
      )

      shiny::showModal(shiny::modalDialog(
        title = sprintf(
          i18n$t("monitoring_pixel_map_modal_title_fmt"),
          round(lat, 5), round(lng, 5)
        ),
        size  = "l",
        easyClose = TRUE,
        plotly::plotlyOutput(session$ns("pixel_ts_plot"),
                             height = "320px"),
        footer = shiny::modalButton(i18n$t("close"))
      ))
      output$pixel_ts_plot <- plotly::renderPlotly(p)
    })

    invisible(list(
      pixel_stack    = pixel_stack_r,
      scenes_df      = scenes_df_r,
      cache_dir      = cache_dir_r
    ))
  })
}

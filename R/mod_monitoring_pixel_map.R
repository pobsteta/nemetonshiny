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

# Debug instrumentation gate (v0.38.4). The "UGF source" /
# "UGF overlay" / "Placettes overlay" cli_alert_info() lines in the
# overlay reactives below are developer instrumentation — handy when
# chasing a reactive-firing issue from a terminal, but noisy on every
# project load / zone change (they printed in bursts of 4-5 lines).
# Gated behind the NEMETON_PIXEL_MAP_DEBUG env var, same pattern as
# NEMETON_S2_CACHE_DEBUG in the nemeton core. Default (unset) → silent.
.pixel_map_debug_enabled <- function() {
  isTRUE(as.logical(Sys.getenv("NEMETON_PIXEL_MAP_DEBUG", "FALSE")))
}

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
#' @param refresh_r Reactive bump-counter (integer). Forces
#'   `cache_dir_r()` to re-evaluate when the on-disk cache state
#'   may have changed — typically after a Sentinel-2 ingestion
#'   completes and creates `<project>/cache/layers/sentinel2/` for
#'   the first time. Without it, the reactive stays frozen on its
#'   pre-ingest NULL value because `dir.exists()` is not a Shiny
#'   dep. v0.42.0.
#' @param thresholds_r Reactive returning `list(ndvi, nbr)` with the
#'   current sidebar NDVI / NBR thresholds. Used to draw alert-coloured
#'   horizontal reference lines on the per-pixel modal plot so the
#'   user sees at a glance which dates would have triggered an alert.
#'   v0.42.0.
#'
#' @noRd
mod_monitoring_pixel_map_server <- function(id, app_state,
                                            obs_pixel_data,
                                            mode_input,
                                            refresh_r = shiny::reactive(0L),
                                            thresholds_r = shiny::reactive(NULL)) {
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
    # project. NULL when no project is loaded or when the dir
    # doesn't exist yet.
    #
    # v0.42.0 — `refresh_r()` is taken as an explicit dep so the
    # reactive re-evaluates after a FAST ingestion that may have
    # just created the cache directory. Otherwise `dir.exists()`
    # is a plain filesystem check, not a Shiny dep, and the
    # reactive value stays stuck on its pre-ingest NULL.
    cache_dir_r <- shiny::reactive({
      refresh_r()  # invalidate on post-ingestion bump
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

    # v0.44.0 — disk inventory of the Sentinel-2 cache. Used to
    # distinguish "no cache yet" from "cache present but 0 obs in DB"
    # (typical symptom of HTTP 403 SAS-token-expiry races during the
    # crop phase of a long ingestion). The cache directory holds one
    # subdir per scene (named `<scene_id>/`), so counting them gives
    # the actual on-disk inventory regardless of DB state.
    disk_scenes_count_r <- shiny::reactive({
      refresh_r()  # invalidate on post-ingestion bump
      cd <- cache_dir_r()
      if (is.null(cd)) return(0L)
      entries <- tryCatch(list.dirs(cd, recursive = FALSE,
                                    full.names = FALSE),
                          error = function(e) character(0))
      length(entries)
    })

    # v0.44.0 — capture the last build_index_stack error so the UI
    # can surface it instead of the misleading "no cache disk" empty
    # state. Typical message : "[rast] extents do not match" when
    # cached rasters from different scenes / re-fetches don't share
    # a common extent.
    last_stack_error <- shiny::reactiveVal(NULL)

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
      out <- tryCatch(
        nemeton::build_index_stack(cd, sdf, index = input$index),
        error = function(e) {
          msg <- conditionMessage(e)
          cli::cli_alert_warning(sprintf(
            "build_index_stack failed: %s", msg))
          last_stack_error(msg)
          NULL
        }
      )
      if (!is.null(out)) last_stack_error(NULL)  # clear on success
      out
    })

    # Date slider built dynamically from the layer names of the
    # stack (one layer per obs_date).
    #
    # v0.44.0 — empty-state branches over three distinct failure modes
    # so the user gets an actionable diagnostic instead of the legacy
    # "no cache disk" catch-all :
    #
    #   1. cache directory absent              → "no cache" (legacy)
    #   2. cache present, 0 observation in DB  → "scenes downloaded
    #      but cropping failed — probably 403 SAS expirations"
    #   3. cache + obs OK, but build_index_stack errored → surface
    #      the cœur error message (typical : "[rast] extents do not
    #      match")
    output$date_slider_ui <- shiny::renderUI({
      ns <- session$ns
      i18n <- i18n_r()
      st <- pixel_stack_r()
      if (is.null(st)) {
        disk_n  <- disk_scenes_count_r()
        sdf     <- scenes_df_r()
        err_msg <- last_stack_error()

        # Case 1 — no cache directory or empty.
        if (is.null(cache_dir_r()) || disk_n == 0L) {
          return(shiny::helpText(
            class = "small text-muted",
            i18n$t("monitoring_pixel_map_no_cache")
          ))
        }
        # Case 2 — disk has scenes but DB has no observations.
        # SAS tokens expired during crop is the dominant cause for
        # long ingests (~100+ scenes). Surface scene count so the
        # user knows downloads actually happened.
        if (is.null(sdf) || !nrow(sdf)) {
          return(htmltools::div(
            class = "small alert alert-warning p-2 mb-0",
            sprintf(i18n$t("monitoring_pixel_map_cache_no_obs_fmt"),
                    as.integer(disk_n))
          ))
        }
        # Case 3 — build_index_stack failed (typically extent mismatch).
        if (!is.null(err_msg)) {
          return(htmltools::div(
            class = "small alert alert-danger p-2 mb-0",
            sprintf(i18n$t("monitoring_pixel_map_stack_failed_fmt"),
                    err_msg)
          ))
        }
        # Fallback : something else turned pixel_stack into NULL.
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
    #
    # v0.46.5 — mosaic des layers d'une même date. Pour une AOI à
    # cheval sur deux tuiles MGRS (cas villards : T31TFM + T31TGM),
    # `scenes_df` retourne 2 scene_ids par obs_date, donc le stack
    # contient 2 layers nommés avec la même date. Avant le fix, le
    # `which.min(abs(dates - target))` retournait un seul index et
    # l'affichage ne couvrait que la moitié de l'AOI (celle du tile
    # gagnant). Désormais on mosaïque tous les layers ayant la date
    # cible — terra::mosaic avec fun="mean" gère le recouvrement
    # éventuel proprement.
    current_layer_r <- shiny::reactive({
      st <- pixel_stack_r()
      if (is.null(st)) return(NULL)
      target <- input$date
      if (is.null(target)) return(NULL)
      dates <- as.Date(names(st))
      if (!length(dates)) return(NULL)
      closest <- dates[which.min(abs(as.numeric(dates - as.Date(target))))]
      idx <- which(dates == closest)
      if (!length(idx)) return(NULL)
      if (length(idx) == 1L) return(st[[idx]])
      # Multi-tile MGRS : mosaic per date.
      layers <- lapply(idx, function(i) st[[i]])
      tryCatch(
        Reduce(function(a, b) terra::mosaic(a, b, fun = "mean"), layers),
        error = function(e) {
          cli::cli_alert_warning(sprintf(
            "mosaic of %d MGRS layers failed for %s: %s",
            length(idx), closest, conditionMessage(e)))
          # Fallback : premier layer (ancien comportement).
          st[[idx[1]]]
        }
      )
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
    # as default on each remount.
    #
    # v0.34.0 — custom pane `nemetonRaster` à z-index 250, entre
    # `tilePane` (200, où vivent OSM / Satellite) et `overlayPane`
    # (400, où vivent les polygones UGF par défaut). Ainsi le raster
    # NDVI/NBR est toujours AU-DESSUS du fond de carte (sinon le
    # tile satellite ré-ajouté par le LayersControl masquait
    # NDVI/NBR) et EN-DESSOUS des polygones UGF.
    #
    # v0.36.3 — les CircleMarkers (placettes) sont par défaut des
    # Path dans `overlayPane` aux côtés des polygones UGF. Selon
    # l'ordre de re-draw, les polygones pouvaient finir en fin de
    # `<g>` SVG et masquer les markers (markers invisibles). On les
    # pousse explicitement dans `markerPane` (z=600) côté observe,
    # ce qui garantit la visibilité au-dessus des polygones.
    #
    # Z-stack final (du bas vers le haut) :
    #   tilePane         z=200  OSM / Satellite
    #   nemetonRaster    z=250  NDVI / NBR raster (épinglé custom)
    #   overlayPane      z=400  Polygones UGF (défaut Path)
    #   markerPane       z=600  CircleMarkers placettes (épinglées)
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",   group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          # v0.46.5 — UGF et Placettes toggleables via le LayersControl.
          # Par défaut UGF visible / Placettes cachées (request user :
          # « il ne devrait y avoir que les UGFs »). Les placettes
          # restent dans la liste pour les workflows qui en ont besoin
          # (clic placette → modal série temporelle agrégée), mais ne
          # surchargent plus la carte par défaut.
          baseGroups    = c("OSM", "Satellite"),
          overlayGroups = c("UGF", "Placettes"),
          options       = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::hideGroup("Placettes")
    })

    # Index value range is roughly [-1, 1] for both NDVI and NBR.
    # v0.45.0 — switch from a divergent red→yellow→green ramp to
    # `plasma` (sequential perceptually-uniform). Le vert "haut" du
    # divergent se confondait avec le fond OSM (parcelles forestières).
    # Plasma (violet → magenta → orange → jaune) ne traverse pas le
    # vert, donc reste contrasté sur l'OSM ; intuition conservée
    # "valeurs hautes = jaune vif, valeurs basses = violet sombre".
    # NA reste transparent.
    .pixel_palette <- leaflet::colorNumeric(
      palette  = "plasma",
      domain   = c(-1, 1),
      na.color = "transparent"
    )

    # v0.45.0 — couleurs centralisées des courbes plotly NDVI / NBR
    # (modal per-plot + modal per-pixel) et de leurs lignes de seuil
    # horizontales. Avant v0.45.0 elles étaient dupliquées inline dans
    # les deux observers + les seuils utilisaient des couleurs distinctes
    # (orange / rouge) qui rompaient l'association visuelle « le seuil
    # appartient à la même bande que sa courbe ». Centralisation =
    # garantie que toute évolution future palette + seuil reste
    # synchronisée.
    .pixel_band_colors <- c(NDVI = "#2CA02C", NBR = "#D62728")

    # Reactive raster + legend swap. Fires on date / index / project
    # changes; preserves the base-layer selection because the map
    # widget itself is not re-rendered.
    #
    # v0.34.0 — raster épinglé dans le pane custom `nemetonRaster`
    # (z-index 250), entre `tilePane` (200, fond OSM/Satellite) et
    # `overlayPane` (400, polygones + CircleMarkers). Avantages :
    #   - le raster reste visible quel que soit le fond choisi (le
    #     tile satellite, ré-ajouté en DOM par le LayersControl,
    #     reste dans tilePane à z=200, sous le raster)
    #   - polygones et markers restent au-dessus, donc cliquables
    #   - plus besoin de ré-empiler les overlays à chaque tick date :
    #     le z-order est maintenu par la séparation des panes
    # Sur 30 ticks d'animation slider, ça divise le coût de redraw
    # par ~3 (raster seul, plus de clearGroup/addPolygons UGF ni de
    # clearGroup/addCircleMarkers placettes — qui ne re-firent que
    # quand leur source change).
    shiny::observe({
      r <- current_layer_r()
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearImages() |>
        leaflet::removeControl("pixel_legend")
      if (is.null(r)) return()
      # v0.38.7 — clamp the index raster to the palette domain
      # [-1, 1] before handing it to addRasterImage(). NDVI / NBR /
      # CRSWIR are theoretically bounded to [-1, 1], but edge pixels
      # and reprojection artefacts occasionally produce values just
      # outside it. .pixel_palette is a colorNumeric anchored on
      # [-1, 1] → those out-of-range values triggered leaflet's
      # "Some values were outside the color scale" warning and were
      # drawn transparent. Clamping pins them to the palette extremes
      # (visually correct: a 1.02 NDVI is just "max green").
      r <- terra::clamp(r, lower = -1, upper = 1, values = TRUE)
      proxy |>
        leaflet::addRasterImage(
          x       = r,
          colors  = .pixel_palette,
          # v0.31.5: bumped 0.85 → 1.0. The conventional NDVI palette
          # uses pale greens (~#A8DDB5) for typical forest values
          # (~0.5 NBR) which are visually indistinguishable from the
          # Esri.WorldImagery dark-green forest imagery even at high
          # opacity. Going full-opaque guarantees the raster colors
          # are readable on both OSM and Satellite. Trade-off: the
          # satellite imagery is hidden under the raster bbox; the
          # user keeps satellite context AROUND the bbox and can
          # toggle to OSM to see roads/parcels inside the zone.
          opacity = 1.0,
          group   = .pixel_overlay_group,
          options = leaflet::gridOptions(pane = "nemetonRaster")
        ) |>
        leaflet::addLegend(
          layerId  = "pixel_legend",
          position = "bottomright",
          pal      = .pixel_palette,
          values   = c(-1, 1),
          title    = input$index,
          opacity  = 0.9
        )
    })

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
        if (.pixel_map_debug_enabled())
          cli::cli_alert_info("UGF source: indicators_sf ({nrow(geom)} feature(s)).")
        return(to_wgs84(geom))
      }

      # 2. raw ug_build_sf
      geom <- tryCatch(ug_build_sf(proj), error = function(e) NULL)
      if (!is.null(geom) && inherits(geom, "sf") && nrow(geom)) {
        if (.pixel_map_debug_enabled())
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
            if (.pixel_map_debug_enabled())
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
            if (.pixel_map_debug_enabled())
              cli::cli_alert_info("UGF source: placettes bbox (last-resort fallback).")
            return(geom)  # already WGS84 via placettes_sf_r
          }
        }
      }

      if (.pixel_map_debug_enabled())
        cli::cli_alert_info("UGF source: NONE — no indicators_sf, no ug_build_sf, no raster, no placettes.")
      NULL
    })

    # UGF polygons swap. Outline only (near-transparent fill) so the
    # raster underneath stays visible. Bright orange border (#FF6B35,
    # weight 3) — high contrast on both OSM (light bg) and Satellite
    # (forest greens). cli debug log on each fire so a developer
    # running from a terminal sees the reactive firing.
    #
    # v0.34.0 — observe ne dépend plus de `current_layer_r()` : le
    # raster vit dans le pane custom `nemetonRaster` (z-index 250),
    # sous `overlayPane` (z-index 400) où vivent les polygones. Le
    # z-order est garanti par la séparation des panes, plus par
    # l'ordre d'ajout DOM. Conséquence : ré-empiler à chaque tick
    # de date est inutile et l'observer ne re-fire que quand la
    # source UGF change réellement (`app_state$current_project`).
    shiny::observe({
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup(.ugf_overlay_group)
      ugf <- ugf_sf_r()
      if (is.null(ugf) || !nrow(ugf)) {
        if (.pixel_map_debug_enabled())
          cli::cli_alert_info("UGF overlay: ugf_sf_r() is NULL/empty, no polygons drawn.")
        return()
      }
      if (.pixel_map_debug_enabled())
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
    })

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
      if (is.null(sub_nav) || sub_nav != "pixel_map_fast") return()

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
    # v0.34.0 — observe ne dépend plus de `current_layer_r()` : le
    # raster vit dans `nemetonRaster` (z-index 250). Les markers ne
    # re-firent que quand `placettes_sf_r()` change (i.e. nouvelle
    # obs_pixel_data ou nouveau projet).
    #
    # v0.36.3 — markers pinned explicitly to `markerPane` (z-index
    # 600). CircleMarkers are Path layers and by default land in
    # `overlayPane` (z=400) along with UGF polygons. In some
    # browsers / leaflet 2.x reflows the polygons ended up at the
    # END of the SVG `<g>` in overlayPane (DOM order = z-order
    # within a pane), making them paint OVER the CircleMarkers and
    # rendering the blue dots invisible. Forcing markers up to
    # `markerPane` (a separate pane Leaflet defines at z=600 for
    # L.Marker icons) gives them their own pane above polygons,
    # guaranteeing visibility regardless of polygon redraw timing.
    # Clickability preserved (Paths emit `click` to `map_marker_click`
    # via `layerId`).
    shiny::observe({
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup(.placettes_overlay_group)
      ps <- placettes_sf_r()
      if (is.null(ps) || !nrow(ps)) {
        if (.pixel_map_debug_enabled())
          cli::cli_alert_info("Placettes overlay: placettes_sf_r() is NULL/empty, no markers drawn.")
        return()
      }
      if (.pixel_map_debug_enabled())
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
          label       = ~as.character(plot_id),
          options     = leaflet::pathOptions(pane = "markerPane")
        )
    })

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
      p <- plotly::plot_ly(type = "scatter", mode = "lines+markers")
      for (b in unique(sub$band)) {
        bsub <- sub[sub$band == b, , drop = FALSE]
        bsub <- bsub[order(bsub$obs_date), , drop = FALSE]
        col  <- .pixel_band_colors[b] %||% "#7F7F7F"
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

      # Stable per-band colors (centralisé en .pixel_band_colors
      # depuis v0.45.0 — pair direct avec les lignes de seuil
      # horizontales pour cohérence visuelle).
      p <- plotly::plot_ly(type = "scatter", mode = "lines+markers")
      for (b in unique(ts$index)) {
        sub <- ts[ts$index == b, , drop = FALSE]
        col <- .pixel_band_colors[b] %||% "#7F7F7F"
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
      # v0.42.0 / v0.45.0 — threshold reference lines, alignées sur la
      # couleur de la courbe correspondante (au lieu d'orange/rouge
      # arbitraire). Le dash style + annotation à droite suffit à
      # différencier la ligne (statique) de la courbe (mesures).
      # `thresholds_r` est threadé depuis la sidebar parent.
      th <- thresholds_r()
      shapes <- list()
      annotations <- list()
      if (!is.null(th)) {
        th_ndvi <- suppressWarnings(as.numeric(th$ndvi))
        th_nbr  <- suppressWarnings(as.numeric(th$nbr))
        col_ndvi <- unname(.pixel_band_colors[["NDVI"]])
        col_nbr  <- unname(.pixel_band_colors[["NBR"]])
        if (length(th_ndvi) == 1L && !is.na(th_ndvi)) {
          shapes <- c(shapes, list(
            list(type = "line",
                 xref = "paper", x0 = 0, x1 = 1,
                 yref = "y",     y0 = th_ndvi, y1 = th_ndvi,
                 line = list(color = col_ndvi, dash = "dash", width = 1))
          ))
          annotations <- c(annotations, list(
            list(xref = "paper", x = 1, xanchor = "right",
                 yref = "y",     y = th_ndvi, yanchor = "bottom",
                 text = sprintf(i18n$t("monitoring_pixel_plot_threshold_fmt"),
                                "NDVI", th_ndvi),
                 showarrow = FALSE,
                 font = list(color = col_ndvi, size = 11))
          ))
        }
        if (length(th_nbr) == 1L && !is.na(th_nbr)) {
          shapes <- c(shapes, list(
            list(type = "line",
                 xref = "paper", x0 = 0, x1 = 1,
                 yref = "y",     y0 = th_nbr, y1 = th_nbr,
                 line = list(color = col_nbr, dash = "dash", width = 1))
          ))
          annotations <- c(annotations, list(
            list(xref = "paper", x = 1, xanchor = "right",
                 yref = "y",     y = th_nbr, yanchor = "bottom",
                 text = sprintf(i18n$t("monitoring_pixel_plot_threshold_fmt"),
                                "NBR", th_nbr),
                 showarrow = FALSE,
                 font = list(color = col_nbr, size = 11))
          ))
        }
      }
      p <- plotly::layout(
        p,
        margin = list(t = 20, b = 40, l = 50, r = 10),
        xaxis  = list(title = i18n$t("monitoring_timeseries_xaxis"),
                      type = "date"),
        yaxis  = list(title = i18n$t("monitoring_timeseries_yaxis"),
                      range = c(-0.2, 1)),
        legend = list(orientation = "h", y = -0.25),
        shapes = if (length(shapes)) shapes else NULL,
        annotations = if (length(annotations)) annotations else NULL
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

    # v0.46.3 — force le rendu même quand l'onglet est masqué.
    # Le navset bslib utilise nav_show/nav_hide qui manipule le DOM
    # directement ; Shiny ne re-réveille pas les outputs marqués
    # suspended au démarrage de l'app. Sans ces 4 outputOptions, la
    # carte FAST reste vide la première fois qu'on clique sur l'onglet
    # parce que `output$map` n'a jamais été démarré. Symétrique avec
    # mod_monitoring_fast_alerts et mod_monitoring_fordead_map qui
    # ont déjà ce fix depuis v0.37.1.
    shiny::outputOptions(output, "map",             suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "date_slider_ui",  suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "scene_count_hint", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "loading_overlay", suspendWhenHidden = FALSE)

    invisible(list(
      pixel_stack    = pixel_stack_r,
      scenes_df      = scenes_df_r,
      cache_dir      = cache_dir_r
    ))
  })
}

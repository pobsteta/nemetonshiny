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

# Scene-id → cache subdir name. Mirror of nemeton's internal
# `.s2_safe_scene_id()` (kept local to avoid depending on a non-exported
# cœur helper). `read_s2_band_raster()` re-applies the same transform, so
# passing the already-safe directory name back as a scene_id is a no-op.
.pixel_safe_scene_id <- function(x) {
  gsub("[^A-Za-z0-9._-]", "_", as.character(x))
}

# Best-effort acquisition date (YYYY-MM-DD) parsed from a Sentinel-2
# scene id. S2 product ids embed the datetime as YYYYMMDDTHHMMSS, so the
# first 8-digit run is the acquisition date. Returns NA_character_ when
# no plausible date token is found.
.pixel_scene_date_from_id <- function(scene_id) {
  m <- regmatches(scene_id, regexpr("[0-9]{8}", scene_id))
  if (!length(m) || !nzchar(m)) return(NA_character_)
  d <- as.Date(m, format = "%Y%m%d")
  if (is.na(d)) NA_character_ else as.character(d)
}

#' Pixel map sub-tab UI
#'
#' @param id Module namespace.
#' @return A `bslib::card` tag list to drop into a `bslib::nav_panel`.
#' @noRd
mod_monitoring_pixel_map_ui <- function(id) {
  ns <- shiny::NS(id)
  i18n <- get_i18n(get_app_options()$language %||% "fr")

  # v0.52.11 — Le `bslib::card_header` (titre « Carte pixel — NDVI /
  # NBR à la résolution Sentinel-2 (10 m) ») mangeait une rangée
  # entière en haut de l'onglet et créait une dissymétrie avec
  # Alertes FAST voisin (qui n'a pas de header). Le titre passe en
  # bandeau `alert-info` inline au-dessus de la carte (style
  # symétrique avec le bandeau « Aucune alerte FAST » d'Alertes FAST),
  # plus discret et cohérent.
  bslib::card(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 250L, position = "right", open = "always",
        shiny::radioButtons(
          ns("index"),
          i18n$t("monitoring_pixel_map_index"),
          # NDMI listed first (moisture / water-stress), NDVI default.
          choices  = c(NDMI = "NDMI", NDVI = "NDVI", NBR = "NBR"),
          selected = "NDVI",
          inline   = TRUE
        ),
        shiny::uiOutput(ns("date_slider_ui")),
        # v0.61.0 — Le checkbox `raster_visible` est retiré : le toggle
        # de visibilité passe désormais entièrement par le LayersControl
        # Leaflet (entrée `"NDVI/NBR"`, cf. `addRasterImage(group=...)`
        # et `overlayGroups` plus bas). Le slider d'opacité reste.
        shiny::sliderInput(
          ns("raster_opacity"),
          label = i18n$t("monitoring_pixel_map_raster_opacity"),
          min = 0, max = 1, value = 1.0, step = 0.05
        ),
        shiny::helpText(
          class = "small text-muted fst-italic",
          i18n$t("monitoring_pixel_map_click_hint")
        ),
        shiny::uiOutput(ns("scene_count_hint"))
      ),
      htmltools::tagList(
        # v0.52.11 — bandeau titre inline (remplace le card_header).
        # Style `alert-info` léger, padding minimal, symétrique avec
        # la barre verte « Aucune alerte FAST » d'Alertes FAST.
        htmltools::div(
          class = "alert alert-info d-flex align-items-center gap-2 py-1 px-2 mb-2 small",
          bsicons::bs_icon("map", class = "fs-5 flex-shrink-0"),
          htmltools::tags$span(i18n$t("monitoring_pixel_map_title"))
        ),
        htmltools::div(
          style = "position: relative;",
          leaflet::leafletOutput(ns("map"), height = "55vh"),
          shiny::uiOutput(ns("loading_overlay"))
        )
      )
    )
  )
}

#' Pixel map sub-tab server
#'
#' @param id Module namespace.
#' @param app_state Shared `reactiveValues` (carries `language`,
#'   `current_project`).
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
#' @section v0.52.16 — obs_pixel-free:
#' Le module ne dépend plus de la table `obs_pixel` (spec 017 cœur +
#' demande utilisateur). Les marqueurs placettes et leur modale série
#' agrégée ont été supprimés : FAST est désormais une analyse pure
#' raster per-pixel, sans contact avec les placettes de calibration
#' Terrain. Le compteur de scènes dérive directement du cache COG.
#'
#' @noRd
mod_monitoring_pixel_map_server <- function(id, app_state,
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

    # v0.52.16 — `scenes_df_r` est désormais purement disk-driven : on
    # liste les sous-dossiers du cache COG, on filtre ceux qui ont au
    # moins un `.tif`, et on parse la date depuis le scene_id
    # (`S2A_MSIL2A_20250619T103041_...` → 2025-06-19). Avant v0.52.16 on
    # croisait avec un `db_scenes_df_r` issu de `obs_pixel_data()` qui
    # servait de source autoritaire pour `obs_date` — supprimé pour
    # éliminer toute dépendance à `obs_pixel` (FAST = pure raster
    # per-pixel, sans contact avec les placettes Terrain).
    scenes_df_r <- shiny::reactive({
      refresh_r()  # re-scan after an ingestion
      cd <- cache_dir_r()
      if (is.null(cd) || !dir.exists(cd)) return(NULL)
      dirs <- list.dirs(cd, recursive = FALSE, full.names = FALSE)
      if (!length(dirs)) return(NULL)
      populated <- vapply(dirs, function(s) {
        any(grepl("\\.tif$", list.files(file.path(cd, s))))
      }, logical(1))
      dirs <- dirs[populated]
      if (!length(dirs)) return(NULL)
      obs_date <- vapply(dirs, .pixel_scene_date_from_id, character(1))
      keep <- !is.na(obs_date)
      if (!any(keep)) return(NULL)
      out <- data.frame(scene_id = dirs[keep],
                        obs_date = as.Date(obs_date[keep]),
                        stringsAsFactors = FALSE)
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
      # Ne lancer le lourd `build_index_stack` (scan de centaines de scènes
      # S2, plusieurs secondes à froid) QUE lorsque l'utilisateur est
      # réellement sur l'onglet Suivi. `date_slider_ui` est marqué
      # `suspendWhenHidden = FALSE` (v0.46.3, pour que la carte s'affiche au
      # 1er clic d'onglet) — sans cette garde, l'output calcule cette
      # reactive à chaque changement de projet, y compris depuis l'Accueil,
      # bloquant le chargement. `active_main_tab` est exposé par app_server.
      shiny::req(identical(app_state$active_main_tab, "monitoring"))
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
        err_msg <- last_stack_error()

        # v0.53.1 — case 2 « disk has scenes but DB has no observations »
        # supprimée. Depuis v0.52.16 (refactor obs_pixel), il n'y a
        # plus de DB obs_pixel à interroger ; le compteur disk est la
        # seule source de vérité.
        # Case 1 — no cache directory or empty.
        if (is.null(cache_dir_r()) || disk_n == 0L) {
          return(shiny::helpText(
            class = "small text-muted",
            i18n$t("monitoring_pixel_map_no_cache")
          ))
        }
        # Case 2 — build_index_stack failed (typically extent mismatch).
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
      # Sentinel-2 revisite au mieux tous les 5 jours (2 satellites,
      # même tuile MGRS) : un pas jour-par-jour faisait défiler 4 crans
      # « morts » entre deux scènes. Le pas de 5 jours aligne le slider
      # sur la cadence nominale — chaque cran avance d'une scène à la
      # suivante. `current_layer_r` continue de snapper sur la scène
      # réelle la plus proche, donc un éventuel décalage (nuages, tuile
      # manquante) reste géré sans cran à vide.
      shiny::sliderInput(
        ns("date"),
        i18n$t("monitoring_pixel_map_date"),
        min   = min(dates),
        max   = max(dates),
        value = max(dates),
        timeFormat = "%Y-%m-%d",
        step  = 5L,
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
    .pixel_overlay_group     <- "NDVI/NBR"
    # v0.52.16 — `.placettes_overlay_group` retiré (les marqueurs
    # placettes ont disparu, FAST est pure-raster).

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
    # v0.52.16 — Le `markerPane` (z=600) et le toggle « Placettes »
    # du LayersControl ont été retirés : plus aucun marqueur placette
    # n'est dessiné (FAST = pure raster per-pixel).
    #
    # Z-stack final (du bas vers le haut) :
    #   tilePane         z=200  OSM / Satellite
    #   nemetonRaster    z=250  NDVI / NBR raster (épinglé custom)
    #   overlayPane      z=400  Polygones UGF (défaut Path)
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",   group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          baseGroups    = c("OSM", "Satellite"),
          overlayGroups = c("UGF", "NDVI/NBR"),
          options       = leaflet::layersControlOptions(collapsed = TRUE)
        )
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
    # v0.71.0 — Ajout de NDMI (3e indice, bleu intuitif pour
    # l'humidité/eau). `extract_pixel_timeseries` côté cœur
    # renvoie déjà les 3 indices (cf. appel l.770), mais NDMI
    # tombait sur `%||% "#7F7F7F"` (gris fallback) faute de
    # couleur dédiée + son seuil n'était pas tracé. Palette
    # standard Tableau pour cohérence visuelle :
    #   NDVI (vigueur)  = vert
    #   NBR  (brûlé)    = rouge
    #   NDMI (humidité) = bleu
    .pixel_band_colors <- c(NDVI = "#2CA02C",
                            NBR  = "#D62728",
                            NDMI = "#1F77B4")

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
      # v0.61.0 — Le toggle `raster_visible` est retiré. La visibilité
      # est gérée par le LayersControl Leaflet (entrée `"NDVI/NBR"`).
      # L'opacité est lue depuis le slider `raster_opacity`.
      opacity <- as.numeric(input$raster_opacity %||% 1.0)
      if (!is.finite(opacity) || opacity < 0) opacity <- 0
      if (opacity > 1) opacity <- 1
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
      # v0.46.6 — reprojection manuelle vers WGS84 avant addRasterImage,
      # avec `project = FALSE` pour bypasser la reprojection interne de
      # leaflet (qui décale visuellement les rasters UTM 31N — vu sur
      # villards : le raster apparaissait ~1 km à l'ouest de sa vraie
      # position en lon, tandis que les UGFs en EPSG:2154 reprojetés
      # par sf::st_transform étaient au bon endroit). terra::project
      # est fiable pour UTM ↔ WGS84 et préserve l'alignement avec les
      # UGFs / les tuiles OSM.
      r <- tryCatch(terra::project(r, "EPSG:4326"),
                    error = function(e) r)
      # v0.46.7 — clip raster aux UGFs : seuls les pixels DANS les
      # polygones UGFs sont colorés, le reste devient transparent.
      # Aide à se concentrer sur la zone d'analyse (forêt) et évite
      # de colorer des champs / routes voisins qui ne sont pas
      # pertinents. terra::mask met NA hors des polygones ; le NA
      # est rendu transparent par .pixel_palette(na.color =
      # "transparent"). En bonus, un clic sur un pixel hors UGF
      # tombe dans une zone transparente → le modal pixel ne
      # s'ouvre pas pour des coordonnées sans intérêt.
      ugf <- tryCatch(ugf_sf_r(), error = function(e) NULL)
      if (!is.null(ugf) && nrow(ugf) > 0) {
        r <- tryCatch(
          terra::mask(r, terra::vect(ugf)),
          error = function(e) {
            cli::cli_alert_warning(sprintf(
              "terra::mask UGF failed: %s", conditionMessage(e)))
            r
          }
        )
      }
      # v0.51.10 — clamp à [-1, 1] avant pal(). NDVI = (B08-B04)/(B08+B04)
      # et NBR = (B08-B12)/(B08+B12) sont théoriquement bornés à [-1, 1]
      # mais quelques cellules peuvent dépasser de ε (1.0001, -1.0001)
      # à cause du bruit numérique en bord de tuile / pixels d'ombre
      # → `.pixel_palette` (domain c(-1, 1)) les déclarait hors domaine
      # → 4 warnings « Some values were outside the color scale » par
      # re-render. terra::clamp les ramène à la borne (cap visuel).
      r <- terra::clamp(r, lower = -1, upper = 1, values = TRUE)
      proxy |>
        leaflet::addRasterImage(
          x       = r,
          colors  = .pixel_palette,
          project = FALSE,
          # v0.31.5 / v0.47.0 : opacité par défaut 1.0 (palette pale
          # greens NDVI ~0.5 indistinguable de l'Esri.WorldImagery
          # forêt à opacité plus basse). Désormais piloté par le
          # slider `raster_opacity` côté UI — le user peut descendre
          # à 0.3 pour voir l'OSM en dessous, ou monter à 1.0 pour
          # un masquage complet du fond.
          opacity = opacity,
          group   = .pixel_overlay_group,
          options = leaflet::gridOptions(pane = "nemetonRaster")
        ) |>
        leaflet::addLegend(
          layerId  = "pixel_legend",
          position = "bottomright",
          pal      = .pixel_palette,
          values   = c(-1, 1),
          # NDMI shows its moisture-flavoured label; NDVI/NBR stay bare.
          title    = if (identical(input$index, "NDMI"))
            i18n_r()$t("index_ndmi") else input$index,
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

      # v0.52.16 — fallback « placettes bbox » supprimé.
      # FAST n'a plus accès aux placettes (obs_pixel-free). Les 3
      # fallbacks restants (indicators_sf, ug_build_sf, raster bbox)
      # couvrent tous les cas usuels. Quand aucun n'est dispo, on
      # retourne NULL et le module reste sans overlay UGF — la carte
      # OSM/Satellite et le raster restent affichés.
      if (.pixel_map_debug_enabled())
        cli::cli_alert_info("UGF source: NONE — no indicators_sf, no ug_build_sf, no raster bbox.")
      NULL
    })

    # UGF polygons swap. Outline only (near-transparent fill) so the
    # raster underneath stays visible. v0.48.0 — bordure bleu vif
    # (#1f78b4, weight 2) harmonisée avec Alertes FAST et Plan de
    # validation (était #FF6B35 orange). Contraste correct sur OSM
    # (fond clair), Satellite (verts forêt) ET le raster plasma /
    # alerte. cli debug log on each fire so a developer running from
    # a terminal sees the reactive firing.
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
          color       = "#1f78b4",
          weight      = 2,
          opacity     = 0.9,
          fillColor   = "#1f78b4",
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

    # v0.52.16 — Bloc « placettes / marker click » supprimé.
    # FAST est désormais une analyse pure raster per-pixel (spec 017
    # cœur + clarification utilisateur 2026-06-02) : aucune interaction
    # avec les placettes de calibration du projet Terrain. Avant ce
    # bump, ce module rendait des `CircleMarkers` pour chaque placette
    # (filtrées via `obs_pixel_data()`) et ouvrait sur clic une modale
    # « série temporelle agrégée placette » alimentée par la même
    # source. Tout ce bloc (`placettes_sf_r`, observe addCircleMarkers,
    # marker_just_clicked, observeEvent input$map_marker_click +
    # output$placette_ts_plot) est retiré. Seul subsiste le clic-pixel
    # pur qui passe par `nemeton::extract_pixel_timeseries()` sur le
    # cache COG, donc obs_pixel-free.

    # Click handler: extract the per-pixel time series at the clicked
    # coordinate and pop a modal with NDVI + NBR superimposed.
    # v0.52.16 — plus de bail anti-marker (les markers ont disparu).
    # v0.85.16 — flag « calcul en cours » : ignore les clics carte tant
    # qu'un graphique pixel se calcule (évite les clics intempestifs).
    pixel_computing <- shiny::reactiveVal(FALSE)
    # Série pixel brute du dernier clic (data.frame obs_date/index/value).
    # Le graphe (lissé) est rendu réactivement par `output$pixel_ts_plot`
    # ci-dessous, qui re-appelle `nemeton::smooth_pixel_series()` quand la
    # fenêtre / méthode de lissage change.
    pixel_ts_rv <- shiny::reactiveVal(NULL)

    # Extrait la série pixel + affiche la modale. Lourd (lit la valeur du
    # pixel sur N scènes) → appelé en DIFFÉRÉ par l'observateur de clic.
    show_pixel_plot <- function(lat, lng, cd, sdf) {
      i18n <- i18n_r()
      ns   <- session$ns

      ts <- tryCatch(
        nemeton::extract_pixel_timeseries(
          cache_dir = cd, scenes_df = sdf,
          xy        = c(lng, lat),
          crs       = 4326,
          indices   = c("NDVI", "NBR", "NDMI")
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

      pixel_ts_rv(ts)  # le rendu (lissé) est réactif (output$pixel_ts_plot)

      shiny::showModal(shiny::modalDialog(
        # Titre + bouton « plein écran » ancré en HAUT À DROITE (même
        # pattern que la modale clés API / corpus RAG, mod_theia_config) :
        # un petit JS bascule la classe BS5 `.modal-fullscreen` sur la
        # `.modal-dialog` la plus proche — bord à bord, sans aller-retour
        # serveur. Le plot remplit la zone (height 100% + plotly
        # `responsive`) et grandit en plein écran via la règle CSS ci-bas.
        title = htmltools::tagList(
          htmltools::span(sprintf(
            i18n$t("monitoring_pixel_map_modal_title_fmt"),
            round(lat, 5), round(lng, 5)
          )),
          htmltools::tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-secondary",
            style = paste("position: absolute; top: 0.75rem;",
                          "right: 0.75rem; z-index: 2;"),
            title = i18n$t("monitoring_pixel_map_fullscreen"),
            # Toggle plein écran + `resize` différé : plotly (responsive)
            # n'écoute que window.resize ; sans cet événement, le graphe
            # garde sa taille initiale et ne remplit pas l'écran agrandi.
            onclick = paste0(
              "this.closest('.modal-dialog').classList.toggle('modal-fullscreen');",
              "setTimeout(function(){window.dispatchEvent(new Event('resize'));},250);"),
            bsicons::bs_icon("arrows-fullscreen")
          )
        ),
        size  = "l",
        easyClose = TRUE,
        footer = shiny::modalButton(i18n$t("close")),
        htmltools::tags$style(htmltools::HTML(
          ".modal-fullscreen .pixel-ts-wrap{height:calc(100vh - 200px) !important;}"
        )),
        # Lissage d'affichage (spec 026) : fenêtre + méthode. Le calcul
        # reste 100 % cœur (`nemeton::smooth_pixel_series`), réappelé par
        # `output$pixel_ts_plot` quand ces contrôles changent.
        shiny::sliderInput(
          ns("smooth_win"), i18n$t("monitoring_pixel_smooth_win_label"),
          min = 15, max = 90, value = 45, step = 5, width = "100%"
        ),
        bslib::accordion(
          open = FALSE,
          bslib::accordion_panel(
            i18n$t("monitoring_pixel_smooth_method_label"),
            shiny::radioButtons(
              ns("smooth_method"), label = NULL, inline = TRUE,
              choiceNames  = list(i18n$t("monitoring_pixel_smooth_rolling"),
                                  i18n$t("monitoring_pixel_smooth_loess")),
              choiceValues = list("rolling_median", "loess"),
              selected = "rolling_median"
            )
          )
        ),
        htmltools::div(
          class = "pixel-ts-wrap",
          style = "height: 55vh;",
          plotly::plotlyOutput(session$ns("pixel_ts_plot"), height = "100%")
        )
      ))
      invisible(NULL)
    }

    # Graphe « série pixel » lissé : points bruts estompés (marqueurs) +
    # ligne lissée par indice (`nemeton::smooth_pixel_series`, spec 026).
    # Réactif à la série cliquée + aux contrôles de lissage.
    output$pixel_ts_plot <- plotly::renderPlotly({
      ts <- pixel_ts_rv()
      shiny::req(ts, nrow(ts) > 0L)
      i18n <- i18n_r()
      win    <- as.integer(input$smooth_win %||% 45L)
      method <- input$smooth_method %||% "rolling_median"
      sm <- tryCatch(
        nemeton::smooth_pixel_series(ts, window_days = win, method = method),
        error = function(e) {
          cli::cli_alert_warning("smooth_pixel_series failed: {conditionMessage(e)}")
          ts$smoothed <- NA_real_
          ts
        }
      )

      p <- plotly::plot_ly(type = "scatter")
      for (b in unique(sm$index)) {
        sub <- sm[sm$index == b, , drop = FALSE]
        sub <- sub[order(as.Date(sub$obs_date)), , drop = FALSE]
        col <- unname(.pixel_band_colors[b] %||% "#7F7F7F")
        # Points bruts estompés (sans entrée de légende, groupés par indice).
        raw <- sub[!is.na(sub$value), , drop = FALSE]
        if (nrow(raw)) {
          p <- plotly::add_trace(
            p, x = as.Date(raw$obs_date), y = as.numeric(raw$value),
            type = "scatter", mode = "markers", name = b,
            legendgroup = b, showlegend = FALSE,
            marker = list(color = col, size = 4, opacity = 0.35),
            hovertemplate = paste0(
              "<b>", b, "</b><br>%{x|%Y-%m-%d}<br>",
              b, " = %{y:.3f}<extra></extra>")
          )
        }
        # Ligne lissée (entrée de légende = indice). connectgaps = FALSE :
        # les NA sont de vrais trous (fenêtre trop éparse).
        p <- plotly::add_trace(
          p, x = as.Date(sub$obs_date), y = as.numeric(sub$smoothed),
          type = "scatter", mode = "lines", name = b, legendgroup = b,
          line = list(color = col, width = 2), connectgaps = FALSE,
          hovertemplate = paste0(
            "<b>", b, " (", i18n$t("monitoring_pixel_smooth_hover"), ")</b>",
            "<br>%{x|%Y-%m-%d}<br>%{y:.3f}<extra></extra>")
        )
      }

      # Lignes de seuil horizontales (NDVI/NBR/NDMI) — inchangé.
      th <- thresholds_r()
      shapes <- list()
      annotations <- list()
      if (!is.null(th)) {
        defs <- list(
          list(v = suppressWarnings(as.numeric(th$ndvi)), lbl = "NDVI",
               col = unname(.pixel_band_colors[["NDVI"]])),
          list(v = suppressWarnings(as.numeric(th$nbr)),  lbl = "NBR",
               col = unname(.pixel_band_colors[["NBR"]])),
          list(v = suppressWarnings(as.numeric(th$ndmi)), lbl = "NDMI",
               col = unname(.pixel_band_colors[["NDMI"]]))
        )
        for (d in defs) {
          if (length(d$v) != 1L || is.na(d$v)) next
          shapes <- c(shapes, list(list(
            type = "line", xref = "paper", x0 = 0, x1 = 1,
            yref = "y", y0 = d$v, y1 = d$v,
            line = list(color = d$col, dash = "dash", width = 1))))
          annotations <- c(annotations, list(list(
            xref = "paper", x = 1, xanchor = "right",
            yref = "y", y = d$v, yanchor = "bottom",
            text = sprintf(i18n$t("monitoring_pixel_plot_threshold_fmt"),
                           d$lbl, d$v),
            showarrow = FALSE, font = list(color = d$col, size = 11))))
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
      plotly::config(p, responsive = TRUE)
    })

    shiny::observeEvent(input$map_click, {
      # Garde anti-multi-clics : on ignore tout nouveau clic tant que le
      # graphique précédent n'a pas fini de se calculer.
      if (isTRUE(shiny::isolate(pixel_computing()))) return()
      lat <- input$map_click$lat
      lng <- input$map_click$lng
      if (is.null(lat) || is.null(lng)) return()
      cd  <- cache_dir_r()
      sdf <- scenes_df_r()
      if (is.null(cd) || is.null(sdf) || !nrow(sdf)) return()

      notif_id <- session$ns("pixel_computing")
      pixel_computing(TRUE)
      # Message bas-droite « calcul en cours » affiché TOUT DE SUITE.
      shiny::showNotification(
        i18n_r()$t("monitoring_pixel_map_computing"),
        id = notif_id, type = "message", duration = NULL
      )
      # Calcul APRÈS le flush courant (`onFlushed`) : la notification part
      # au client AVANT le calcul lourd (un observateur synchrone ne flush
      # l'UI qu'à sa sortie), et le flag `pixel_computing` bloque les clics
      # intempestifs entre-temps. `isolate` car onFlushed s'exécute hors
      # consommateur réactif.
      session$onFlushed(function() {
        on.exit({
          shiny::removeNotification(notif_id, session = session)
          shiny::isolate(pixel_computing(FALSE))
        }, add = TRUE)
        shiny::isolate(show_pixel_plot(lat, lng, cd, sdf))
      }, once = TRUE)
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

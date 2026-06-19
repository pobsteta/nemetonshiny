# mod_monitoring_fordead_map.R — Sub-tab "Carte FORDEAD" of Suivi sanitaire.
#
# Wires `nemeton::read_fordead_dieback_mask()` (shipped in nemeton@v0.25.0)
# into the monitoring navset. Renders the categorical 0..4 dieback mask :
#
#   0 = sain     → green   (#2CA02C)
#   1 = faible   → pale-orange (#FFD27F)
#   2 = moyenne  → orange  (#FF9933)
#   3 = forte    → red     (#D62728)
#   4 = sol nu   → near-black (#222222)
#   NA = hors mask forestier → transparent
#
# Raster pinned to the custom `nemetonRaster` Leaflet pane (z-index 250),
# same pattern as Carte FAST so the basemap toggle (OSM / Satellite) keeps
# the mask visible without re-stacking the overlay on every interaction.
#
# Caveat (nemeton@v0.25.0) : `run_fordead_dieback()` does NOT yet persist
# the classified mask to disk — the persist hook ships in a later cœur
# release. Until then this reader returns NULL and the panel shows an
# empty state. The wiring is in place so the panel activates automatically
# the day the cœur writer ships.

# Max class value of a categorical FORDEAD dieback mask, NA-safe.
# Returns NA_real_ when the raster is NULL / not a SpatRaster / empty /
# all-NA. Used (Phase A, décision D2) to decide healthy-vs-affected
# FROM THE RASTER rather than from a DB alert count :
#   max >= 1  → au moins un pixel classe >= 1 → zone affectée ;
#   max < 1 ou NA → tout sain (classe 0) ou tout NA → zone saine.
.fordead_raster_max <- function(r) {
  if (is.null(r) || !inherits(r, "SpatRaster")) return(NA_real_)
  if (terra::ncell(r) == 0L) return(NA_real_)
  tryCatch(
    as.numeric(terra::global(r, "max", na.rm = TRUE)[[1]]),
    error = function(e) NA_real_
  )
}

# Bbox (EPSG:4326) pour cadrer la carte FORDEAD : préfère l'emprise des
# UGF (déjà en 4326), sinon retombe sur l'étendue du raster reprojetée.
# Renvoie NULL si rien d'exploitable.
.fordead_view_bbox <- function(ugf_4326, r) {
  if (!is.null(ugf_4326) && inherits(ugf_4326, "sf") && nrow(ugf_4326)) {
    return(tryCatch(sf::st_bbox(ugf_4326), error = function(e) NULL))
  }
  if (!is.null(r) && inherits(r, "SpatRaster")) {
    return(tryCatch({
      e <- terra::ext(r)
      poly <- sf::st_as_sfc(sf::st_bbox(
        c(xmin = e[1], xmax = e[2], ymin = e[3], ymax = e[4]),
        crs = sf::st_crs(terra::crs(r))))
      sf::st_bbox(sf::st_transform(poly, 4326))
    }, error = function(e) NULL))
  }
  NULL
}

#' Carte FORDEAD sub-tab UI
#'
#' @param id Module namespace id.
#' @return A `shiny.tag`.
#' @noRd
mod_monitoring_fordead_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("panel"))
}

#' Carte FORDEAD sub-tab server
#'
#' @param id Module namespace id.
#' @param app_state Parent `reactiveValues` carrying `language`,
#'   `current_project`.
#' @param zone_id_r Reactive returning the active monitoring zone id.
#' @param refresh_r Reactive bumped whenever a FORDEAD run completes
#'   (the parent's `alerts_refresh` counter). `mask_r` reads it so the
#'   sub-tab re-reads the freshly-persisted mask without the user
#'   having to reload the project or re-pick the zone. Optional —
#'   defaults to a constant reactive for back-compat / tests.
#' @param opacity_r Reactive returning the raster opacity (0..1) from the
#'   tab's right sidebar slider (parité FAST). Optional — defaults to a
#'   constant 0.75 for back-compat / tests.
#' @return invisible list with `mask` reactive.
#' @noRd
mod_monitoring_fordead_map_server <- function(id, app_state, zone_id_r,
                                              refresh_r = shiny::reactive(0L),
                                              opacity_r = shiny::reactive(0.75)) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # Phase A (spec 008 §15, D2) — FORDEAD est calculé sur la zone `_tot`
    # (union des UGFs). Le masque ET la série pixel vivent donc sous
    # `cache/layers/fordead/zone_<id_tot>/`. Ce helper résout l'id `_tot`
    # du projet (find_zones_by_project + convention de nommage `_tot`,
    # spec 020) ; fallback sur la zone passée (back-compat pré-spec-020).
    # Utilisé par `mask_r` (lecture masque) ET par le clic-pixel (lecture
    # série CRSWIR) pour qu'ils pointent sur la MÊME zone que le run.
    .fordead_tot_id <- function(con, proj, fallback_zone) {
      id_tot <- tryCatch({
        zdf <- nemeton::find_zones_by_project(con, project_uuid = proj$id)
        idx <- grep("_tot$", as.character(zdf$name))
        if (length(idx)) as.integer(zdf$id[idx[1]]) else NA_integer_
      }, error = function(e) NA_integer_)
      if (!is.na(id_tot)) id_tot else suppressWarnings(as.integer(fallback_zone))
    }

    # ----- Core call ------------------------------------------------------
    # cache_dir is resolved from the active project — nemeton@v0.41.0
    # persists the categorical 0-4 mask to
    # <project>/cache/layers/fordead/zone_<id>/dieback_mask_<ts>.tif
    # after the postprocess phase.
    #
    # v0.38.6 — `refresh_r()` is read here so a completed FORDEAD run
    # invalidates mask_r. Without it the reactive only depended on
    # zone_id + current_project: it evaluated once (before the run,
    # when cache/layers/fordead/ did not exist yet → NULL) and stayed
    # frozen, so the sub-tab kept showing the empty-state even after
    # the mask had been written to disk.
    mask_r <- shiny::reactive({
      refresh_r()  # re-read after each FORDEAD run completion
      zone <- zone_id_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return(NULL)
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return(NULL)
      cd <- file.path(proj$path, "cache", "layers", "fordead")
      if (!dir.exists(cd)) return(NULL)
      con <- .perf_time("fordead mask: db_connect",
                        get_monitoring_db_connection(project = proj, read_only = TRUE))
      if (is.null(con)) return(NULL)
      on.exit(close_monitoring_db_connection(con), add = TRUE)

      # Phase A (D2) — on lit TOUJOURS le masque de `_tot` ; l'affichage
      # par strate n'est qu'un masquage spatial du raster `_tot`.
      id_tot   <- .perf_time("fordead mask: resolve _tot (find_zones)",
                             .fordead_tot_id(con, proj, zone))
      read_zone <- id_tot

      r <- tryCatch(
        .perf_time("fordead mask: read_fordead_dieback_mask (TIF)",
          nemeton::read_fordead_dieback_mask(
            con,
            zone_id   = read_zone,
            run_id    = NULL,  # latest
            cache_dir = cd
          )),
        error = function(e) {
          cli::cli_alert_warning(
            "read_fordead_dieback_mask failed: {e$message}"
          )
          NULL
        }
      )
      if (is.null(r)) return(NULL)

      # Masquage par strate : si la strate sélectionnée n'est PAS `_tot`,
      # clipper le raster `_tot` à l'AOI de la strate (les pixels hors
      # strate passent en NA → transparents dans addRasterImage). C'est
      # de la présentation pure (clip d'affichage), aucun calcul métier
      # (règle CLAUDE.md #3). CRS : le masque FORDEAD est en EPSG:2154 et
      # get_monitoring_zone_aoi rend du 2154 ; on transforme l'AOI au CRS
      # du raster par sécurité avant terra::mask.
      sel <- suppressWarnings(as.integer(zone))
      if (!is.na(id_tot) && !is.na(sel) && !identical(sel, id_tot)) {
        aoi <- .perf_time("fordead mask: get_zone_aoi",
                          get_monitoring_zone_aoi(con, sel))
        if (!is.null(aoi)) {
          r <- tryCatch(
            .perf_time("fordead mask: terra::mask (clip strate)",
              terra::mask(r, terra::vect(
                sf::st_transform(aoi, terra::crs(r))))),
            error = function(e) {
              cli::cli_alert_warning(
                "strata mask failed (zone={sel}): {conditionMessage(e)}")
              r
            }
          )
        }
      }
      r
    })

    # ----- Panel : either map or empty state -----------------------------
    output$panel <- shiny::renderUI({
      i18n <- i18n_r()
      r <- mask_r()
      # État (c) — aucun masque sur disque (run jamais lancé pour ce
      # projet) → placeholder « lancer un diagnostic ».
      if (is.null(r)) {
        return(htmltools::div(
          class = "p-4 text-center text-muted",
          bsicons::bs_icon("hourglass-split",
                           class = "fs-1 d-block mx-auto mb-3"),
          htmltools::h5(class = "mt-3",
                        i18n$t("monitoring_fordead_map_empty_title")),
          htmltools::p(class = "mb-0",
                       i18n$t("monitoring_fordead_map_empty_body"))
        ))
      }
      # Décision sain/affecté PILOTÉE PAR LE RASTER masqué (Phase A, D2),
      # pas par un compte d'alertes DB. Classe >= 1 = pixel affecté.
      mx <- .fordead_raster_max(r)
      if (!is.finite(mx) || mx < 1) {
        # État (b) — raster présent mais tout classe 0 / tout NA dans la
        # strate → carte « zone saine » (vrai négatif).
        return(bslib::card(
          class = "border-success mt-2",
          bslib::card_header(htmltools::div(
            class = "d-flex align-items-center",
            bsicons::bs_icon("check-circle-fill",
                             class = "me-2 text-success fs-4"),
            htmltools::tags$strong(
              i18n$t("monitoring_fordead_no_alerts_title")))),
          bslib::card_body(htmltools::tags$p(
            i18n$t("monitoring_fordead_no_alerts_body")))
        ))
      }
      # État (a) — au moins un pixel classe >= 1 → carte raster 0-4.
      leaflet::leafletOutput(session$ns("map"), height = "55vh")
    })

    output$map <- leaflet::renderLeaflet({
      r <- mask_r()
      if (is.null(r)) return(NULL)
      i18n <- i18n_r()
      # Parité FAST : couche UGF (contour) + couche « Alertes » (le
      # raster) togglables via LayersControl, opacité réglable via le
      # slider de la sidebar droite (opacity_r).
      ugf_4326 <- .ugf_for_overlay(app_state$current_project)
      # PERF/UX (parité FAST) — l'opacité est lue en isolate() : un coup
      # de slider ne doit PAS re-render toute la carte (sinon zoom + fond
      # OSM/Satellite réinitialisés). Le render de base dessine le raster
      # à l'opacité courante ; l'observer leafletProxy ci-dessous met à
      # jour l'opacité sans reconstruire la carte.
      op <- as.numeric(shiny::isolate(opacity_r()) %||% 0.75)
      if (!is.finite(op)) op <- 0.75
      op <- max(0, min(1, op))

      # v0.38.7 — `levels` numériques 0:4, alignés sur les valeurs
      # entières du raster catégoriel (le masque FORDEAD stocke
      # 0=sain, 1=faible, 2=moyenne, 3=forte, 4=sol-nu).
      cats   <- 0:4
      colors <- c("#2CA02C", "#FFD27F", "#FF9933", "#D62728", "#222222")
      pal <- leaflet::colorFactor(colors, levels = cats,
                                  na.color = "transparent")
      # Parité FAST : la classe 0 (sain) est rendue TRANSPARENTE plutôt
      # qu'en vert opaque — sinon le « sain » recouvre toute la zone et
      # noie les quelques pixels d'alerte (classe >= 1). On ne peint donc
      # que les pixels affectés ; fond de carte + contour UGF restent
      # visibles dessous. La légende garde la classe 0 (référence).
      r_show <- terra::ifel(is.na(r) | r <= 0, NA, r)

      legend_labels <- c(
        sprintf("0 - %s", i18n$t("monitoring_fordead_class_0")),
        sprintf("1 - %s", i18n$t("monitoring_fordead_class_1")),
        sprintf("2 - %s", i18n$t("monitoring_fordead_class_2")),
        sprintf("3 - %s", i18n$t("monitoring_fordead_class_3")),
        sprintf("4 - %s", i18n$t("monitoring_fordead_class_4"))
      )
      overlays <- c(if (!is.null(ugf_4326)) "UGF" else NULL, "Alertes")

      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",   group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          baseGroups    = c("OSM", "Satellite"),
          overlayGroups = overlays,
          options    = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::addRasterImage(
          x       = r_show,
          colors  = pal,
          opacity = op,
          # v0.38.7 — nearest-neighbour resampling. addRasterImage()
          # reprojects to web-mercator and defaults to method =
          # "bilinear", which interpolates BETWEEN the discrete
          # classes of a categorical raster (0.7, 2.3…). Those
          # fractional values match no colorFactor level → leaflet
          # logs "Some values were outside the color scale and will
          # be treated as NA" once per raster block. "ngb" keeps the
          # integer classes intact.
          method  = "ngb",
          group   = "Alertes",
          options = leaflet::gridOptions(pane = "nemetonRaster")
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors   = colors,
          labels   = legend_labels,
          title    = i18n$t("monitoring_fordead_class_title"),
          opacity  = 0.85
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
      m
    })

    # Mise à jour de l'OPACITÉ du raster via leafletProxy (parité FAST) :
    # ne touche que le group "Alertes" → préserve le zoom utilisateur et
    # le fond (OSM/Satellite). Dépend de opacity_r() ; lit le raster en
    # isolate() pour ne pas re-déclencher au changement de masque (le
    # render de base gère, lui, le redraw complet sur changement de
    # masque). La légende (control) n'est pas dans le group "Alertes",
    # elle persiste donc intacte.
    shiny::observe({
      op <- as.numeric(opacity_r() %||% 0.75)
      if (!is.finite(op)) op <- 0.75
      op <- max(0, min(1, op))
      r <- shiny::isolate(mask_r())
      if (is.null(r)) return()
      cats   <- 0:4
      colors <- c("#2CA02C", "#FFD27F", "#FF9933", "#D62728", "#222222")
      pal <- leaflet::colorFactor(colors, levels = cats,
                                  na.color = "transparent")
      # Classe 0 (sain) transparente : seuls les pixels d'alerte (>= 1)
      # sont peints (cf. render de base).
      r_show <- terra::ifel(is.na(r) | r <= 0, NA, r)
      leaflet::leafletProxy("map") |>
        leaflet::clearGroup("Alertes") |>
        leaflet::addRasterImage(
          x       = r_show,
          colors  = pal,
          opacity = op,
          method  = "ngb",
          group   = "Alertes",
          options = leaflet::gridOptions(pane = "nemetonRaster")
        )
    })

    # v0.37.1 — force the panel renderUI to evaluate even while the
    # sub-tab is hidden. The Suivi sanitaire navset toggles its
    # nav_panels with bslib::nav_show() / nav_hide() (mode-driven
    # visibility). That mechanism leaves Shiny's per-output
    # visibility detection unreliable : the `uiOutput(ns("panel"))`
    # stayed suspended (suspendWhenHidden defaults to TRUE) and the
    # empty-state never rendered even after the user clicked the
    # Carte FORDEAD tab — the panel showed up blank. Disabling the
    # suspend makes the empty-state / leaflet wrapper render
    # unconditionally.
    shiny::outputOptions(output, "panel", suspendWhenHidden = FALSE)
    # Même raison pour la carte elle-même : sans ça, le widget leaflet
    # de ce sous-onglet non-défaut (affiché via nav_show/nav_hide) peut
    # rester suspendu / s'initialiser à taille 0 → clics et leafletProxy
    # (opacité) inopérants. Parité avec Carte FAST (mod_monitoring_pixel_map).
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)

    # Carte FORDEAD est un sous-onglet non-défaut : quand l'utilisateur y
    # navigue, le conteneur leaflet vient seulement de devenir visible.
    # On force alors `invalidateSize` (Leaflet re-détecte ses dimensions,
    # sinon clics + proxy opacité tombent dans le vide) + un fitBounds.
    # Mêmes mécanisme et handler JS que Carte FAST (custom.js).
    shiny::observe({
      root_session <- session$userData$root_session
      if (is.null(root_session)) return()
      top_nav <- root_session$input$main_nav
      sub_nav <- root_session$input[["monitoring-subtab"]]
      if (is.null(top_nav) || top_nav != "monitoring") return()
      if (is.null(sub_nav) || sub_nav != "pixel_map_fordead") return()
      bb <- .fordead_view_bbox(
        shiny::isolate(.ugf_for_overlay(app_state$current_project)),
        shiny::isolate(mask_r()))
      later::later(function() {
        session$sendCustomMessage("leafletInvalidateSize",
                                  list(id = session$ns("map")))
        if (!is.null(bb)) {
          leaflet::leafletProxy("map", session = session) |>
            leaflet::fitBounds(
              lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
              lng2 = bb[["xmax"]], lat2 = bb[["ymax"]]
            )
        }
      }, delay = 0.3)
    })

    # v0.59.0 (TODO #3) — diagnostic pixel CRSWIR au clic sur la
    # carte FORDEAD. Parité fonctionnelle avec la « Carte pixel
    # FAST » (mod_monitoring_pixel_map.R::observeEvent(input$map_click)).
    # Le clic extrait via `nemeton::read_fordead_pixel_series()` la
    # série CRSWIR observée (points) et la prédiction harmonique
    # (ligne) au pixel cliqué, puis affiche un plotly dans un modal
    # avec un marqueur vertical sur la date de 1re anomalie
    # (`attr(., "premiere_detection")`) si présente.
    shiny::observeEvent(input$map_click, {
      i18n <- i18n_r()
      zone <- zone_id_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return()
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return()
      cd <- file.path(proj$path, "cache", "layers", "fordead")
      if (!dir.exists(cd)) return()
      lat <- input$map_click$lat
      lng <- input$map_click$lng
      if (is.null(lat) || is.null(lng)) return()

      # Phase A (D2) — la série pixel doit être lue sur la zone `_tot`
      # (là où FORDEAD a tourné), pas sur la strate sélectionnée : le
      # pixel cliqué est un pixel du raster `_tot` (masqué à l'affichage).
      # Sans ça, cliquer sur une strate ≠ `_tot` ne renvoyait aucune série
      # (cache sous zone_<id_tot>) → graphe absent.
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
      zone_tot <- if (!is.null(con)) {
        on.exit(close_monitoring_db_connection(con), add = TRUE)
        .fordead_tot_id(con, proj, zone)
      } else {
        suppressWarnings(as.integer(zone))
      }

      ts <- tryCatch(
        nemeton::read_fordead_pixel_series(
          con       = NULL,  # spec : con reserved, NULL accepté
          zone_id   = zone_tot,
          xy        = c(lng, lat),
          crs       = 4326,
          run_id    = NULL,  # dernier run
          cache_dir = cd
        ),
        error = function(e) {
          cli::cli_alert_warning(sprintf(
            "read_fordead_pixel_series failed: %s",
            conditionMessage(e)))
          NULL
        }
      )
      if (is.null(ts) || !nrow(ts)) {
        # v0.72.0 — duration ↑ 4 → 8 s + wording plus explicite.
        # Cas typique : clic hors zone modélisée (extent du bundle
        # FORDEAD plus petit que l'AOI rendue sur la carte).
        shiny::showNotification(
          i18n$t("monitoring_fordead_pixel_no_data"),
          type = "warning", duration = 8
        )
        return()
      }

      # v0.72.0 — Enrichissement du modal CRSWIR (consume les 5
      # colonnes de `nemeton::read_fordead_pixel_series()` au lieu
      # de juste obs/pred) :
      #   * Bande `seuil_haut` (`crswir_pred + threshold_anomaly`)
      #     → ligne pointillée orange : enveloppe de détection.
      #   * Points en anomalie (`anomalie == TRUE`) surlignés rouge
      #     vif marker size 8 par-dessus la trace observée.
      #   * `attr(ts, "premiere_detection")` → marqueur vertical
      #     (existant depuis v0.59.0).
      #   * `attr(ts, "dans_zone_validite")` → annotation discrète
      #     en haut à gauche si FALSE (pixel hors zone de validité
      #     pour l'essence dominante).
      premiere      <- attr(ts, "premiere_detection")
      in_validity   <- attr(ts, "dans_zone_validite")
      veg_index     <- attr(ts, "vegetation_index") %||% "CRSWIR"

      col_obs     <- "#2C7FB8"  # bleu observé (existant)
      col_pred    <- "#D62728"  # rouge prédiction harmonique (existant)
      col_seuil   <- "#FF7F0E"  # orange seuil_haut (nouveau v0.72.0)
      col_anomaly <- "#9C0E0E"  # rouge foncé points anomalie (nouveau)

      p <- plotly::plot_ly(type = "scatter")
      # 1. Observé (points bleus, mode = markers).
      p <- plotly::add_trace(
        p,
        x      = as.Date(ts$obs_date),
        y      = as.numeric(ts$crswir_obs),
        name   = i18n$t("monitoring_fordead_pixel_observed"),
        mode   = "markers",
        marker = list(color = col_obs, size = 5),
        hovertemplate = paste0(
          "<b>", i18n$t("monitoring_fordead_pixel_observed"), "</b><br>",
          "%{x|%Y-%m-%d}<br>",
          veg_index, " = %{y:.3f}<extra></extra>"
        )
      )
      # 2. Prédiction harmonique (ligne rouge).
      p <- plotly::add_trace(
        p,
        x      = as.Date(ts$obs_date),
        y      = as.numeric(ts$crswir_pred),
        name   = i18n$t("monitoring_fordead_pixel_predicted"),
        mode   = "lines",
        line   = list(color = col_pred, width = 1.5),
        hovertemplate = paste0(
          "<b>", i18n$t("monitoring_fordead_pixel_predicted"), "</b><br>",
          "%{x|%Y-%m-%d}<br>",
          veg_index, " = %{y:.3f}<extra></extra>"
        )
      )
      # 3. v0.72.0 — Seuil haut (= crswir_pred + threshold_anomaly).
      # Ligne orange pointillée. Quand un point observé dépasse ce
      # seuil, c'est une anomalie candidate.
      if ("seuil_haut" %in% names(ts)) {
        p <- plotly::add_trace(
          p,
          x      = as.Date(ts$obs_date),
          y      = as.numeric(ts$seuil_haut),
          name   = i18n$t("monitoring_fordead_pixel_threshold"),
          mode   = "lines",
          line   = list(color = col_seuil, width = 1.2,
                        dash = "dash"),
          hovertemplate = paste0(
            "<b>", i18n$t("monitoring_fordead_pixel_threshold"), "</b><br>",
            "%{x|%Y-%m-%d}<br>",
            veg_index, " = %{y:.3f}<extra></extra>"
          )
        )
      }
      # 4. v0.72.0 — Points en anomalie (rouge foncé, taille 8),
      # surlignés par-dessus la trace observée. Filtre :
      # ts$anomalie == TRUE && !is.na(ts$crswir_obs).
      if ("anomalie" %in% names(ts)) {
        anom_rows <- which(isTRUE(any(ts$anomalie)) |
                           (!is.na(ts$anomalie) & ts$anomalie))
        if (length(anom_rows)) {
          p <- plotly::add_trace(
            p,
            x      = as.Date(ts$obs_date[anom_rows]),
            y      = as.numeric(ts$crswir_obs[anom_rows]),
            name   = i18n$t("monitoring_fordead_pixel_anomaly"),
            mode   = "markers",
            marker = list(color = col_anomaly, size = 8,
                          line = list(color = "#FFFFFF", width = 1)),
            hovertemplate = paste0(
              "<b>", i18n$t("monitoring_fordead_pixel_anomaly"), "</b><br>",
              "%{x|%Y-%m-%d}<br>",
              veg_index, " = %{y:.3f}<extra></extra>"
            )
          )
        }
      }

      shapes <- list()
      annotations <- list()
      if (!is.null(premiere) && length(premiere) == 1L &&
          inherits(premiere, "Date") && !is.na(premiere)) {
        # Marqueur vertical sur la date de 1re anomalie détectée.
        shapes <- list(list(
          type = "line",
          xref = "x",   x0 = premiere, x1 = premiere,
          yref = "paper", y0 = 0, y1 = 1,
          line = list(color = "#000000", dash = "dash", width = 1.5)
        ))
        annotations <- list(list(
          xref = "x",  x = premiere, xanchor = "left",
          yref = "paper", y = 1, yanchor = "top",
          text = sprintf("%s : %s",
                         i18n$t("monitoring_fordead_pixel_first_anomaly"),
                         format(premiere, "%Y-%m-%d")),
          showarrow = FALSE,
          font = list(color = "#000000", size = 11)
        ))
      }
      # v0.72.0 — Annotation discrète en haut à gauche si le pixel
      # est hors zone de validité de l'essence dominante (calibration
      # FORDEAD non-applicable strictement). Visible quand
      # `attr(ts, "dans_zone_validite") == FALSE`.
      if (!is.null(in_validity) && length(in_validity) == 1L &&
          !is.na(in_validity) && isFALSE(in_validity)) {
        annotations <- c(annotations, list(list(
          xref = "paper", x = 0, xanchor = "left",
          yref = "paper", y = 1, yanchor = "top",
          text = i18n$t("monitoring_fordead_pixel_outside_validity"),
          showarrow = FALSE,
          font = list(color = "#FF7F0E", size = 11),
          bgcolor = "rgba(255, 255, 220, 0.85)",
          borderpad = 4
        )))
      }

      p <- plotly::layout(
        p,
        margin = list(t = 20, b = 40, l = 50, r = 10),
        xaxis  = list(title = i18n$t("monitoring_timeseries_xaxis"),
                      type = "date"),
        yaxis  = list(title = sprintf("%s (%s)",
                                       i18n$t("monitoring_fordead_pixel_yaxis"),
                                       veg_index)),
        legend = list(orientation = "h", y = -0.25),
        shapes = if (length(shapes)) shapes else NULL,
        annotations = if (length(annotations)) annotations else NULL
      )

      shiny::showModal(shiny::modalDialog(
        title = sprintf(
          i18n$t("monitoring_fordead_pixel_modal_title_fmt"),
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

    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)

    invisible(list(mask = mask_r))
  })
}

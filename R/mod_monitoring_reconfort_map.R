# mod_monitoring_reconfort_map.R — Sous-onglet "Carte RECONFORT" du Suivi
# sanitaire (spec 021, L6). Miroir de mod_monitoring_fordead_map.R, adapté
# au pipeline RECONFORT (dépérissement feuillus, méthode RECONFORT/DSF).
#
# Mise en page (parité FORDEAD / FAST) : `bslib::layout_sidebar` avec une
# sidebar DROITE portant les contrôles (choix de la couche + curseur
# d'opacité des rasters) et, à gauche, la carte Leaflet STATIQUE surmontée
# d'un overlay d'état (empty-state). La carte est en UI statique pour
# préserver le binding `input$map_click` (clic → diagnostic pixel).
#
# AFFICHAGE 100 % RASTER (parité FAST / FORDEAD) — le module expose les
# couches du manifeste : score, classes de santé, probabilité. AUCUNE
# sémantique métier ici : ids, palettes, domaines, sens (reverse) et
# visibilité par défaut viennent tous du manifeste (CLAUDE.md §2-4). Le
# manifeste vient soit du `result` en mémoire d'un run de la session
# (`reconfort_layer_manifest()`), soit du cache disque d'un run persisté
# (`reconfort_cache_manifest()`, parité FORDEAD) — schéma identique.
#
# v0.106.4 — La couche VECTORIELLE « Alertes » (marqueurs lus en base via
# `nemeton::list_alerts()`) est SUPPRIMÉE. Elle était le dernier vestige de
# la notion de « placette » dans le suivi sanitaire, alors que FAST
# (a18f3ab2) et FORDEAD (Phase A, décision D2) sont passés en pur raster ;
# elle dupliquait de surcroît le signal de la couche « Classes de santé ».
# La table `alerts` de la base de suivi reste écrite par le cœur et lue par
# `service_r5.R` (indicateur R5) — seul l'affichage cartographique disparaît.
#
# Couche UGF (parité FORDEAD / FAST) : les Unités de Gestion Forestière du
# projet (`project$indicators_sf`) sont dessinées en overlay toggleable via
# le LayersControl natif de leaflet.
#
# Clip à la zone de suivi : les résultats (rasters) sont masqués à l'AOI de
# la zone sélectionnée dans la liste déroulante (`zone_id_r`) — strates
# `_tot` / `_res` / `_feu` / `_mix` — exactement comme FORDEAD masque le
# raster `_tot` par strate. Présentation pure (clip d'affichage), aucun
# calcul métier (CLAUDE.md §3).
#
# cache_dir RECONFORT : <project>/cache/layers/reconfort (la phase persist
# du run y écrit zone_<id>/run_<run_id>/ ; même répertoire passé à
# read_reconfort_pixel_series()).

#' Couleurs des classes du raster de classification RECONFORT (codes 1-2-3).
#' 1-sain (vert), 2-deperissant (orange), 3-tres-deperissant (rouge). Mappé
#' sur les codes entiers du raster `classification` du manifeste.
#' @noRd
.RECONFORT_CLASSIF_COLORS <- c(
  "1" = "#1A9850",  # vert    (sain)
  "2" = "#FF9933",  # orange  (dépérissant)
  "3" = "#D62728"   # rouge   (très dépérissant)
)

#' Libellé d'une couche (case à cocher) avec une icône « i » (tooltip bslib)
#' qui explique ce que la couche affiche. Parité avec
#' `.fordead_layer_choice()` de la Carte FORDEAD.
#' @noRd
.reconfort_layer_choice <- function(label, info) {
  htmltools::tagList(
    label,
    bslib::tooltip(
      bsicons::bs_icon("info-circle",
                       class = "ms-1 text-primary",
                       style = "cursor: help;"),
      info
    )
  )
}

#' Carte RECONFORT sub-tab UI
#'
#' @param id Module namespace id.
#' @return A `shiny.tag`.
#' @noRd
mod_monitoring_reconfort_map_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::layout_sidebar(
      # Sidebar DROITE — contrôles (cases à cocher des couches + opacité),
      # rendus côté serveur car pilotés par le manifeste (choix dynamiques).
      sidebar = bslib::sidebar(
        width = 250L, position = "right", open = "always",
        shiny::uiOutput(ns("controls"))
      ),
      # Carte STATIQUE + overlay empty-state. Le bandeau « hors domaine de
      # calibration » est désormais rendu au niveau PARENT (mod_monitoring,
      # output$reconfort_validity_banner), juste sous « Base de suivi
      # connectée » et au-dessus des sous-onglets — parité de placement et de
      # style avec le bandeau FORDEAD. Le module expose `validity` (voir le
      # `invisible(list(...))` de retour) que le parent consomme.
      htmltools::div(
        style = "position: relative;",
        leaflet::leafletOutput(ns("map"), height = "55vh"),
        shiny::uiOutput(ns("overlay"))
      )
    )
  )
}

#' Carte RECONFORT sub-tab server
#'
#' @param id Module namespace id.
#' @param app_state Parent `reactiveValues` carrying `language`,
#'   `current_project`.
#' @param zone_id_r Reactive returning the active monitoring zone id.
#' @param refresh_r Reactive bumped whenever a RECONFORT run completes (the
#'   parent's refresh counter). Read by `manifest_r` so a completed run
#'   re-discovers the freshly-persisted raster cache without a project
#'   reload. Optional — defaults to a constant reactive for back-compat.
#' @param result_r Reactive returning the in-memory `result` list of the last
#'   `nemeton::run_reconfort_dieback()` of this session (carries `$rasters`),
#'   or `NULL`. When NULL, the module falls back to the cached run discovered
#'   on disk. Optional — defaults to a constant `NULL` reactive.
#' @return invisible list with a `validity` reactive.
#' @noRd
mod_monitoring_reconfort_map_server <- function(id, app_state, zone_id_r,
                                                refresh_r = shiny::reactive(0L),
                                                result_r = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # ----- cache_dir resolver -------------------------------------------
    .reconfort_cache_dir <- function(proj) {
      if (is.null(proj) || is.null(proj$path)) return(NA_character_)
      file.path(proj$path, "cache", "layers", "reconfort")
    }

    # ----- Layer manifest (cœur) ----------------------------------------
    # Le contrat stable : le module ne lit JAMAIS result$rasters en dur, il
    # délègue au cœur la liste des couches + palettes/domaines/sens.
    #
    # Deux sources, schéma identique (interchangeable) :
    #   1. `result` EN MÉMOIRE d'un run de la session → reconfort_layer_manifest() ;
    #   2. sinon (projet rechargé, plus de result) → DÉCOUVERTE CACHE via
    #      reconfort_cache_manifest(cache_dir, zone_id) (nemeton >= 0.100.0) :
    #      le run persisté est lu depuis le disque, parité FORDEAD — les
    #      rasters réapparaissent après rechargement, pas seulement après un
    #      run frais.
    # NULL quand ni run mémoire ni run caché ne sont disponibles.
    manifest_r <- shiny::reactive({
      refresh_r()   # un run terminé → re-découverte du cache disque
      res <- result_r()
      if (!is.null(res)) {
        m <- tryCatch(
          nemeton::reconfort_layer_manifest(res, include_range = TRUE),
          error = function(e) {
            cli::cli_alert_warning("reconfort_layer_manifest failed: {e$message}")
            NULL
          }
        )
        if (!is.null(m) && nrow(m)) return(m)
      }
      # Fallback cache (parité FORDEAD).
      zone <- zone_id_r()
      proj <- app_state$current_project
      if (is.null(zone) || !isTRUE(nzchar(zone)) ||
          is.null(proj) || is.null(proj$path)) return(NULL)
      cd <- .reconfort_cache_dir(proj)
      if (is.na(cd) || !dir.exists(cd)) return(NULL)
      m <- tryCatch(
        nemeton::reconfort_cache_manifest(cd, zone_id = as.integer(zone),
                                          include_range = TRUE),
        error = function(e) {
          cli::cli_alert_warning("reconfort_cache_manifest failed: {e$message}")
          NULL
        }
      )
      if (is.null(m) || !nrow(m)) return(NULL)
      m
    })

    raster_manifest_r <- shiny::reactive({
      m <- manifest_r()
      if (is.null(m)) return(NULL)
      rm <- m[m$type == "raster", , drop = FALSE]
      if (!nrow(rm)) return(NULL)
      rm
    })

    # ----- Selected-zone AOI (for raster clipping) ----------------------
    # `get_monitoring_zone_aoi()` returns the polygon (EPSG:2154) of the
    # zone picked in the dropdown (`_tot` / `_res` / `_feu` / `_mix`). The
    # result rasters are clipped to it before display (parity FORDEAD). NULL
    # when unresolved → no clip (full raster shown).
    aoi_r <- shiny::reactive({
      zone <- zone_id_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return(NULL)
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return(NULL)
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
      if (is.null(con)) return(NULL)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      tryCatch(get_monitoring_zone_aoi(con, as.integer(zone)),
               error = function(e) NULL)
    })

    # ----- UGF overlay (parité FORDEAD / FAST) --------------------------
    ugf_r <- shiny::reactive({
      .ugf_for_overlay(app_state$current_project)
    })

    # ----- BD Forêt v2 du projet (repli composition d'essences) ----------
    # `<project>/cache/layers/bdforet.gpkg`, écrit pendant le calcul projet
    # (même source que l'overlay de l'onglet Échantillonnage). Lue telle
    # quelle et passée AU CŒUR : aucun traitement métier ici (CLAUDE.md §1).
    # NULL quand le cache est absent.
    bdforet_r <- shiny::reactive({
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return(NULL)
      gpkg <- file.path(proj$path, "cache", "layers", "bdforet.gpkg")
      if (!file.exists(gpkg)) return(NULL)
      bd <- tryCatch(sf::st_read(gpkg, quiet = TRUE), error = function(e) NULL)
      if (is.null(bd) || !nrow(bd)) return(NULL)
      bd
    })

    # ----- Validity (G3, advisory) --------------------------------------
    # `check_reconfort_validity()` is advisory (`advisory = TRUE`) : it
    # warns when the zone sits outside the calibration domain but never
    # blocks. Returns NULL when the AOI cannot be resolved.
    #
    # v0.106.4 — `units` (les UGF du projet) et `bdforet` sont désormais
    # PASSÉS au cœur. Sans eux, le cœur ne peut pas évaluer la composition
    # d'essences : `species_valid` restait NA en toutes circonstances, et le
    # bandeau « composition hors domaine validé » ne pouvait donc JAMAIS
    # s'afficher. Le cœur lit la colonne essence des UGF et, à défaut,
    # retombe sur la BD Forêt (`enrich_parcels_bdforet()`).
    validity_r <- shiny::reactive({
      aoi <- aoi_r()
      if (is.null(aoi)) return(NULL)
      proj  <- app_state$current_project
      units <- proj$indicators_sf
      if (!inherits(units, "sf") || !nrow(units)) units <- NULL
      tryCatch(
        nemeton::check_reconfort_validity(aoi, units = units,
                                          bdforet = bdforet_r()),
        error = function(e) {
          cli::cli_alert_warning("check_reconfort_validity failed: {e$message}")
          NULL
        }
      )
    })

    # Toggleable layers = the manifest's RASTER rows (run result OR cache).
    # NULL when no run is available → empty state (parity FAST / FORDEAD).
    available_layers_r <- shiny::reactive({
      rm <- raster_manifest_r()
      if (is.null(rm) || !nrow(rm)) return(NULL)
      data.frame(
        id              = as.character(rm$id),
        label_key       = as.character(rm$label_key),
        default_visible = rm$default_visible %in% TRUE,
        stringsAsFactors = FALSE
      )
    })

    # ----- Sidebar controls (manifest-driven, dynamic choices) ----------
    output$controls <- shiny::renderUI({
      i18n <- i18n_r()
      lay <- available_layers_r()
      if (is.null(lay)) {
        # Ni run en mémoire ni run caché — pas de toggles ; rappel.
        return(htmltools::p(
          class = "text-muted small mb-0",
          i18n$t("monitoring_reconfort_map_empty_body")
        ))
      }
      # Chaque couche porte une icône « i » (tooltip) décrivant ce qu'elle
      # affiche — parité FORDEAD. La clé d'info = `<label_key>_info`.
      choice_names <- lapply(seq_len(nrow(lay)), function(k) {
        .reconfort_layer_choice(
          i18n$t(lay$label_key[k]),
          i18n$t(paste0(lay$label_key[k], "_info"))
        )
      })
      # Couches EXCLUSIVES (une seule à la fois) : radioButtons, parité
      # FORDEAD. Défaut = première couche `default_visible` (sinon la 1re).
      dv <- as.character(lay$id[lay$default_visible])
      selected <- if (length(dv)) dv[1L] else as.character(lay$id[1L])
      htmltools::tagList(
        shiny::radioButtons(
          session$ns("layers"), i18n$t("reconfort_couches"),
          choiceNames = choice_names,
          choiceValues = as.character(lay$id),
          selected = selected
        ),
        # Toutes les couches sont des rasters → le curseur d'opacité
        # s'applique toujours.
        shiny::sliderInput(
          session$ns("opacity"), i18n$t("reconfort_opacite"),
          min = 0, max = 1, value = 0.8, step = 0.05
        )
      )
    })

    # ----- Validity banner (G3 advisory) --------------------------------
    # v0.94.x — Le rendu du bandeau « hors domaine de calibration » a été
    # déplacé au niveau PARENT (mod_monitoring::output$reconfort_validity_banner)
    # pour être affiché sous « Base de suivi connectée », au même emplacement
    # et dans le même style (`.monitoring_validity_banner`) que le bandeau
    # FORDEAD. Le module se contente d'exposer `validity_r` dans son retour.

    # ----- Empty-state overlay (no raster layer at all) -----------------
    output$overlay <- shiny::renderUI({
      i18n <- i18n_r()
      if (!is.null(available_layers_r())) return(NULL)
      htmltools::div(
        style = paste(
          "position: absolute; inset: 0; z-index: 500;",
          "background: #fff; display: flex; align-items: center;",
          "justify-content: center;"),
        htmltools::div(
          class = "p-4 text-center text-muted",
          bsicons::bs_icon("hourglass-split",
                           class = "fs-1 d-block mx-auto mb-3"),
          htmltools::h5(class = "mt-3",
                        i18n$t("monitoring_reconfort_map_empty_title")),
          htmltools::p(class = "mb-0",
                       i18n$t("monitoring_reconfort_map_empty_body"))
        )
      )
    })

    # ----- Raster layer helper (shared by base render + proxy) -----------
    # Draws one manifest raster row (continuous or categorical) + its legend
    # at the requested opacity. `r` is the SpatRaster ALREADY read and masked
    # to the UGF zone by the core reader (`masked_rasters_r`) — the module
    # carries no spatial masking of its own (spec 021 L7, CLAUDE.md §1-3).
    # Palettes / domain / reverse come from the manifest — no business
    # semantics here.
    .add_raster <- function(map, r, row, opacity, i18n) {
      if (is.null(r)) return(map)
      if (terra::nlyr(r) > 1L) r <- r[[1L]]

      if (isTRUE(row$categorical)) {
        cols <- .RECONFORT_CLASSIF_COLORS
        lv   <- as.integer(names(cols))
        pal  <- leaflet::colorFactor(unname(cols), levels = lv,
                                     na.color = "transparent")
        # Classe 1 (1-sain) rendue TRANSPARENTE — n'afficher que les pixels
        # affectés (2-deperissant / 3-tres-deperissant), parité avec la
        # sévérité FORDEAD qui rend la classe 0 (sain) transparente. Le fond
        # de carte reste ainsi visible sous la forêt saine.
        r_show <- terra::ifel(is.na(r) | r <= 1, NA, r)
        map <- leaflet::addRasterImage(
          map, x = r_show, colors = pal, opacity = opacity,
          method = "ngb", project = TRUE, group = row$id,
          options = leaflet::gridOptions(pane = "nemetonRaster")
        )
        # v0.106.4 — La légende ne liste QUE les classes réellement peintes
        # (2 et 3). Elle affichait auparavant un carré vert « 1-sain » que la
        # carte ne dessine jamais (classe masquée juste au-dessus) : la légende
        # promettait une couleur absente de la carte.
        painted <- names(cols) %in% c("2", "3")
        map <- leaflet::addLegend(
          map, position = "bottomright", colors = unname(cols[painted]),
          labels = c(i18n$t("reconfort_class_label_2"),
                     i18n$t("reconfort_class_label_3")),
          title = i18n$t("reconfort_couche_classes"),
          opacity = opacity, layerId = paste0("legend_", row$id)
        )
      } else {
        palname <- if (!is.na(row$palette)) row$palette else "viridis"
        # Échelle de couleur PAR QUANTILES (parité avec la carte FAST),
        # calculée sur les valeurs réelles du raster affiché : une
        # distribution continue concentrée (score / probabilité) utilise
        # alors toute la palette uniformément, au lieu d'une rampe linéaire
        # min/max qui délave les valeurs groupées. Le vmin/vmax générique
        # du manifeste (score 1-100, proba 0-1000) n'est PAS utilisé.
        # Repli sur une rampe linéaire réelle si la distribution est trop
        # dégénérée pour des bornes de quantiles.
        vals <- tryCatch(terra::values(r, na.rm = TRUE, mat = FALSE),
                         error = function(e) numeric(0))
        vals <- vals[is.finite(vals)]
        qbreaks <- if (length(vals)) unique(as.numeric(stats::quantile(
          vals, probs = seq(0, 1, length.out = 6L), na.rm = TRUE))) else numeric(0)
        if (length(qbreaks) >= 3L) {
          pal <- leaflet::colorBin(palname, domain = range(vals),
                                   bins = qbreaks, reverse = isTRUE(row$reverse),
                                   na.color = "transparent")
          legend_vals <- range(vals)
        } else {
          dom <- if (length(vals)) range(vals) else c(0, 1)
          if (dom[1L] == dom[2L]) dom <- dom + c(0, 1)
          pal <- leaflet::colorNumeric(palname, domain = dom,
                                       reverse = isTRUE(row$reverse),
                                       na.color = "transparent")
          legend_vals <- dom
        }
        map <- leaflet::addRasterImage(
          map, x = r, colors = pal, opacity = opacity,
          method = "bilinear", project = TRUE, group = row$id,
          options = leaflet::gridOptions(pane = "nemetonRaster")
        )
        map <- leaflet::addLegend(
          map, position = "bottomright", pal = pal, values = legend_vals,
          title = i18n$t(row$label_key), opacity = opacity,
          layerId = paste0("legend_", row$id)
        )
      }
      map
    }

    # Currently selected layer id(s) : the radio state (a single id since
    # layers are EXCLUSIVE), defaulting to the first default-visible layer
    # until the user interacts. Kept as a vector so the drawing observer
    # (which iterates `id %in% sel`) stays unchanged.
    selected_ids_r <- shiny::reactive({
      lay <- available_layers_r()
      if (is.null(lay)) return(character())
      sel <- input$layers
      if (is.null(sel)) {
        dv  <- as.character(lay$id[lay$default_visible])
        sel <- if (length(dv)) dv[1L] else as.character(lay$id[1L])
      }
      as.character(sel)
    })

    opacity_r <- shiny::reactive({
      op <- suppressWarnings(as.numeric(input$opacity))
      if (length(op) != 1L || !is.finite(op)) op <- 0.8
      max(0, min(1, op))
    })

    # ----- Masked rasters (READ + UGF MASK delegated to the core) -------
    # `nemeton::read_reconfort_layer(layer = row, mask_polygon = aoi)`
    # returns a SpatRaster already masked to the selected monitoring-zone
    # polygon (spec 021 L7) : the module no longer performs ANY spatial
    # masking (no terra::mask) — strict parity with FAST / FORDEAD readers
    # (CLAUDE.md §1-3). Cached on manifest / zone (`aoi_r`), NOT on opacity :
    # a slider move only re-adds the cached rasters at the new opacity
    # (parity FORDEAD — the mask lives in a reactive, opacity is a render
    # param). `aoi` is the mask polygon ; passing it avoids a per-tick DB
    # round-trip (the reader would otherwise resolve it from con + zone_id).
    # Returns a named list id → SpatRaster for every `type == "raster"` row
    # (the `vector` alert row is never passed to the reader — it rejects it).
    masked_rasters_r <- shiny::reactive({
      rm <- raster_manifest_r()
      if (is.null(rm)) return(NULL)
      aoi <- aoi_r()
      out <- list()
      for (k in seq_len(nrow(rm))) {
        row <- rm[k, ]
        r <- tryCatch(
          nemeton::read_reconfort_layer(
            layer           = row,
            mask_polygon    = aoi,
            apply_zone_mask = TRUE
          ),
          error = function(e) {
            cli::cli_alert_warning(
              "read_reconfort_layer ({row$id}) failed: {conditionMessage(e)}")
            NULL
          }
        )
        if (!is.null(r)) out[[as.character(row$id)]] <- r
      }
      if (!length(out)) return(NULL)
      out
    })

    # ----- Base map (stable widget — parity FORDEAD) --------------------
    # Depends ONLY on the current project (re-render on project change, not
    # on zone / layer / opacity). A re-render recreates the widget and would
    # otherwise drop the `input$map_click` binding + reset zoom. Tiles +
    # panes + LayersControl (UGF overlay) + framing only ; the data layers
    # (rasters) are drawn by the observer below via leafletProxy.
    output$map <- leaflet::renderLeaflet({
      proj <- app_state$current_project          # SEUL dep réactif
      i18n <- shiny::isolate(i18n_r())
      ugf  <- shiny::isolate(ugf_r())

      # « UGF » TOUJOURS dans le contrôle (parité Carte pixel FAST) : ses
      # polygones sont (re)dessinés par l'observer réactif plus bas dès que
      # `indicators_sf` est disponible (attache différée au chargement → l'UGF
      # pouvait manquer si le render de base précédait l'attache).
      overlays <- "UGF"
      map <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",     group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          baseGroups    = c("OSM", "Satellite"),
          overlayGroups = overlays,
          options       = leaflet::layersControlOptions(collapsed = TRUE)
        )

      if (!is.null(ugf)) {
        map <- leaflet::addPolygons(
          map, data = ugf, group = "UGF", color = "#1f78b4",
          weight = 2, opacity = 0.9, fillOpacity = 0
        )
      }

      # Framing : UGF en priorité, sinon AOI de la zone de suivi.
      bb <- NULL
      if (!is.null(ugf)) {
        bb <- tryCatch(sf::st_bbox(ugf), error = function(e) NULL)
      }
      if (is.null(bb)) {
        aoi <- shiny::isolate(aoi_r())
        if (!is.null(aoi))
          bb <- tryCatch(sf::st_bbox(sf::st_transform(aoi, 4326)),
                         error = function(e) NULL)
      }
      if (!is.null(bb)) {
        map <- leaflet::fitBounds(
          map, lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
          lng2 = bb[["xmax"]], lat2 = bb[["ymax"]]
        )
      }
      map
    })

    # ----- Data layers → leafletProxy (toggles + opacity, re-render léger)
    # addRasterImage has no dynamic opacity setter, so a slider move (or a
    # checkbox toggle, a zone change → new AOI clip, a completed run → new
    # manifest) re-draws the checked layers via leafletProxy. The base map,
    # zoom, base layer and UGF overlay are preserved. Mirror of FORDEAD.
    shiny::observe({
      sel     <- selected_ids_r()
      op      <- opacity_r()
      i18n    <- i18n_r()
      rm      <- raster_manifest_r()
      rasters <- masked_rasters_r()

      proxy <- leaflet::leafletProxy("map")
      # Clear every known raster group + its legend (NOT the UGF overlay),
      # then re-add the selected one.
      if (!is.null(rm)) {
        for (g in as.character(rm$id)) {
          proxy <- proxy |>
            leaflet::clearGroup(g) |>
            leaflet::removeControl(paste0("legend_", g))
        }
      }
      if (!is.null(rm) && !is.null(rasters)) {
        for (k in seq_len(nrow(rm))) {
          id <- as.character(rm$id[k])
          if (id %in% sel && !is.null(rasters[[id]])) {
            proxy <- .add_raster(proxy, rasters[[id]], rm[k, ], op, i18n)
          }
        }
      }
      proxy
    })

    # UGF overlay via leafletProxy (réactif sur le projet) : (re)dessine les
    # polygones UGF dès que `indicators_sf` est attaché — corrige la
    # disparition de l'UGF quand le render de base précédait l'attache
    # différée. Parité avec la Carte pixel FAST / FORDEAD.
    shiny::observe({
      ugf <- ugf_r()
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup("UGF")
      if (is.null(ugf) || !nrow(ugf)) return()
      leaflet::addPolygons(
        proxy, data = ugf, group = "UGF", color = "#1f78b4",
        weight = 2, opacity = 0.9, fillOpacity = 0)
    })

    # Force the controls / map / overlay to render even while the sub-tab is
    # hidden (the Suivi navset toggles nav_panels via nav_show/nav_hide,
    # which leaves Shiny's per-output visibility detection unreliable). Same
    # rationale as the FORDEAD / FAST map modules.
    shiny::outputOptions(output, "controls", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "map",      suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "overlay",  suspendWhenHidden = FALSE)

    # ----- Pixel diagnostic on map click --------------------------------
    # Mirror of the FORDEAD pixel click, adapted to RECONFORT : the series
    # carries CRSWIR + CRre observed only (no harmonic prediction / seuil).
    shiny::observeEvent(input$map_click, {
      i18n <- i18n_r()
      zone <- zone_id_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return()
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return()
      cd <- .reconfort_cache_dir(proj)
      if (is.na(cd) || !dir.exists(cd)) {
        shiny::showNotification(
          i18n$t("monitoring_reconfort_pixel_no_data"),
          type = "warning", duration = 8
        )
        return()
      }
      lat <- input$map_click$lat
      lng <- input$map_click$lng
      if (is.null(lat) || is.null(lng)) return()

      # Message « calcul en cours » affiché TOUT DE SUITE (parité Cartes
      # FAST / FORDEAD). La lecture de la série CRSWIR/CRre + le tracé plotly
      # sont déférés via `session$onFlushed` pour que la notification parte au
      # client AVANT le calcul (un observateur synchrone ne flushe l'UI qu'à
      # sa sortie). `on.exit` retire la notif quoi qu'il arrive (succès, « pas
      # de données », erreur).
      .notif_id <- session$ns("reconfort_pixel_loading")
      shiny::showNotification(
        i18n$t("monitoring_pixel_map_computing"),
        id = .notif_id, type = "message", duration = NULL
      )
      session$onFlushed(function() {
        on.exit(shiny::removeNotification(.notif_id, session = session),
                add = TRUE)
      # `onFlushed` s'exécute HORS consommateur réactif : on enveloppe tout le
      # calcul dans `shiny::isolate()` (parité Cartes FAST / FORDEAD). Les
      # valeurs réactives (zone, proj, cd, lat, lng, i18n) ont déjà été
      # capturées plus haut en contexte réactif.
      shiny::isolate({
      s <- tryCatch(
        nemeton::read_reconfort_pixel_series(
          con       = NULL,   # spec : con réservé, NULL accepté
          zone_id   = as.integer(zone),
          xy        = c(lng, lat),
          crs       = 4326,
          run_id    = NULL,   # dernier run
          cache_dir = cd
        ),
        error = function(e) {
          cli::cli_alert_warning(sprintf(
            "read_reconfort_pixel_series failed: %s", conditionMessage(e)))
          NULL
        }
      )
      if (is.null(s) || !nrow(s)) {
        shiny::showNotification(
          i18n$t("monitoring_reconfort_pixel_no_data"),
          type = "warning", duration = 8
        )
        return()
      }

      species <- attr(s, "species")
      v_model <- attr(s, "v_model")
      subtitle_bits <- c(
        if (!is.null(species) && length(species) == 1L && !is.na(species))
          sprintf("%s : %s", i18n$t("monitoring_reconfort_species"), species),
        if (!is.null(v_model) && length(v_model) == 1L && !is.na(v_model))
          sprintf("%s : %s", i18n$t("monitoring_reconfort_model"), v_model)
      )
      subtitle <- if (length(subtitle_bits))
        paste(subtitle_bits, collapse = " — ") else NULL

      # Planche 4 panneaux (Partie B) : TOUT le dérivé vient du cœur
      # (`nemeton::prepare_pixel_dieback_series()`), le tracé pur est délégué
      # à `plot_pixel_dieback()` (règles 1-3 : zéro calcul dans l'app). Le
      # rendu est réactif aux contrôles `pixel_smooth` / `pixel_points` de la
      # modale : basculer le lissage / les points brutes re-render la planche
      # sans nouveau clic (le `s` du pixel cliqué est capturé par la closure).
      output$pixel_ts_plot <- plotly::renderPlotly({
        prepared <- nemeton::prepare_pixel_dieback_series(
          s, smooth = input$pixel_smooth %||% "light"
        )
        plotly::config(
          plot_pixel_dieback(
            prepared,
            opts = list(show_points = isTRUE(input$pixel_points %||% TRUE),
                        species = species, v_model = v_model,
                        lat = lat, lng = lng),
            i18n = i18n_r()
          ),
          responsive = TRUE
        )
      })
      # Repli non-graphique (WCAG 2.1 AA) : la série brute du pixel en table DT.
      output$pixel_ts_table <- DT::renderDT({
        tbl <- data.frame(
          date   = as.Date(s$obs_date),
          CRswir = round(as.numeric(s$crswir_obs), 4L),
          CRre   = round(as.numeric(s$crre_obs), 4L)
        )
        DT::datatable(tbl, rownames = FALSE,
                      options = list(pageLength = 8L, dom = "tip"))
      })

      # Modale plein écran (parité FORDEAD). Contrôles lissage/points en tête,
      # sous-titre espèce/modèle conservé, planche agrandie (~760 px) + table
      # DT équivalente en repli accessible.
      shiny::showModal(shiny::modalDialog(
        title = htmltools::tagList(
          htmltools::span(sprintf(
            i18n$t("monitoring_reconfort_pixel_modal_title_fmt"),
            round(lat, 5), round(lng, 5)
          )),
          htmltools::tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-secondary",
            style = paste("position: absolute; top: 0.75rem;",
                          "right: 0.75rem; z-index: 2;"),
            title = i18n$t("monitoring_pixel_map_fullscreen"),
            onclick = paste0(
              "this.closest('.modal-dialog').classList.toggle('modal-fullscreen');",
              "setTimeout(function(){window.dispatchEvent(new Event('resize'));},250);"),
            bsicons::bs_icon("arrows-fullscreen")
          )
        ),
        size  = "xl",
        easyClose = TRUE,
        footer = shiny::modalButton(i18n$t("close")),
        htmltools::tags$style(htmltools::HTML(
          ".modal-fullscreen .pixel-ts-wrap{height:calc(100vh - 220px) !important;}"
        )),
        # Contrôles d'affichage (aucun impact métier : smooth passé au cœur,
        # points = simple toggle de traces). Lissage fort volontairement absent
        # (le cœur n'offre que none/light).
        #
        # v0.106.4 — TOUT SUR UNE SEULE LIGNE : « Lissage : Aucun / Léger »
        # + « Observations brutes ». Shiny rend chaque input dans un
        # form-group bloc (label AU-DESSUS des options, marge basse), d'où les
        # deux lignes précédentes. Le CSS `.pixel-ctl` ci-dessous remet le
        # label du groupe radio en ligne avec ses options et annule les marges.
        htmltools::tags$style(htmltools::HTML(paste0(
          ".pixel-ctl .shiny-input-container,.pixel-ctl .form-group",
          "{margin-bottom:0 !important;}",
          ".pixel-ctl .control-label",
          "{display:inline-block;margin:0 .5rem 0 0;}",
          ".pixel-ctl .shiny-options-group",
          "{display:inline-flex;gap:.75rem;vertical-align:middle;}",
          ".pixel-ctl .checkbox,.pixel-ctl .form-check{margin:0;}"
        ))),
        htmltools::div(
          class = "pixel-ctl d-flex align-items-center gap-3 mb-2",
          shiny::radioButtons(
            session$ns("pixel_smooth"), i18n$t("pixel_smooth_label"),
            choiceNames  = list(i18n$t("pixel_smooth_none"),
                                i18n$t("pixel_smooth_light")),
            choiceValues = list("none", "light"),
            selected = "light", inline = TRUE
          ),
          shiny::checkboxInput(
            session$ns("pixel_points"), i18n$t("pixel_points_label"), TRUE
          )
        ),
        if (!is.null(subtitle))
          htmltools::div(class = "small text-muted mb-2", subtitle),
        htmltools::div(
          class = "pixel-ts-wrap", role = "img",
          `aria-label` = i18n$t("pixel_plot_alt"),
          style = "height: 760px;",
          plotly::plotlyOutput(session$ns("pixel_ts_plot"), height = "100%")
        ),
        htmltools::tags$details(
          class = "mt-2",
          htmltools::tags$summary(i18n$t("pixel_table_label")),
          DT::DTOutput(session$ns("pixel_ts_table"))
        )
      ))
      })  # fin shiny::isolate (calcul différé hors contexte réactif)
      }, once = TRUE)  # fin session$onFlushed
    })

    # `validity` : exposé pour que le PARENT (mod_monitoring) rende le bandeau
    # « hors domaine de calibration » sous « Base de suivi connectée », au même
    # emplacement/style que FORDEAD (parité). NULL si l'AOI est irrésolue.
    invisible(list(validity = validity_r))
  })
}

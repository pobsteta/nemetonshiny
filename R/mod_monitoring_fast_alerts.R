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

#' Shared NDMI hint + B11 re-ingestion note for the FAST tabs
#'
#' NDMI needs Sentinel-2 band B11, which is only cached from ingestions
#' run with nemeton >= 0.64.0. For a zone ingested earlier, the NDMI map
#' is empty (B11-less scenes are skipped) until the S2 series is
#' re-ingested with `skip_cached = FALSE`. This note surfaces that.
#'
#' @param i18n An i18n translator object.
#' @return An HTML div (or NULL is never returned — callers gate on the
#'   selected index).
#' @noRd
.fast_ndmi_note <- function(i18n) {
  htmltools::div(
    class = "small fst-italic mt-1",
    htmltools::tags$div(class = "text-muted",
                        i18n$t("monitoring_fast_ndmi_hint")),
    htmltools::tags$div(class = "text-warning",
                        i18n$t("monitoring_fast_ndmi_b11_note"))
  )
}

#' Alertes FAST sub-tab UI
#'
#' @param id Module namespace id.
#' @return A `shiny.tag.list` with the mode toggle + the panel.
#' @noRd
mod_monitoring_fast_alerts_ui <- function(id) {
  ns <- shiny::NS(id)
  # Initial labels in the default language ; refreshed server-side on
  # language switch via updateRadioButtons / updateCheckboxInput /
  # updateSliderInput.
  i18n <- get_i18n("fr")
  # v0.52.8 — Layout migré du flex-wrap horizontal (contrôles au-dessus
  # de la carte) vers un `bslib::layout_sidebar(position = "right")`
  # symétrique avec Carte FAST. La carte gagne en lisibilité (zone
  # rectangulaire complète) et les contrôles vivent dans une sidebar
  # de largeur fixe à droite, plus cohérente avec l'onglet voisin.
  # `output$panel` continue de rendre un bandeau optionnel « zone
  # saine » au-dessus de la carte — il vit désormais dans le slot
  # principal de `layout_sidebar`.
  bslib::card(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 250L, position = "right", open = "always",
        # v0.52.14 — Radio « Indice FAST » dans le sidebar de cet
        # onglet (symétrique avec Carte FAST qui a son propre
        # `input$index`). v0.52.13 l'avait posé dans le sidebar
        # parent ; on le rapatrie ici pour que chaque onglet pilote
        # son indice indépendamment, comme Carte FAST.
        # `nemeton::read_fast_alert_raster()` (spec 017 mono-index)
        # ne consomme qu'un seul indice à la fois — le radio choisit
        # lequel, et le `threshold` correspondant est lu depuis le
        # sidebar parent (`thresholds_r$ndvi` ou `thresholds_r$nbr`).
        htmltools::tagAppendAttributes(
          shiny::radioButtons(
            ns("index"),
            label = i18n$t("monitoring_fast_index_label"),
            # NDMI first (moisture / water-stress), NDVI default.
            choices = c(NDMI = "NDMI", NDVI = "NDVI", NBR = "NBR"),
            selected = "NDVI",
            inline = TRUE
          ),
          class = "mb-2"
        ),
        # NDMI hint + B11 re-ingestion note (shown only when NDMI active).
        shiny::uiOutput(ns("ndmi_note")),
        # v0.46.2 — `mb-0` retire le margin-bottom 1rem du form-group
        # par défaut. Le label est maintenant porté par `label =` (et
        # non plus par un `<strong>` sibling) puisqu'on est en sidebar
        # verticale — l'alignement « label - radios » est naturel.
        htmltools::tagAppendAttributes(
          shiny::radioButtons(
            ns("mode"),
            label  = i18n$t("monitoring_fast_alerts_mode_label"),
            inline = TRUE,
            choiceNames  = list(i18n$t("monitoring_fast_alerts_mode_count"),
                                i18n$t("monitoring_fast_alerts_mode_rolling")),
            choiceValues = list("count", "rolling"),
            selected     = "count"
          ),
          class = "mb-2"
        ),
        # v0.61.0 — Le checkbox `raster_visible` est retiré. Le toggle
        # de visibilité du raster d'alerte passe désormais par le
        # LayersControl Leaflet (entrée « Alertes » sous la couche UGF
        # dans le contrôle de couches de la carte).
        shiny::sliderInput(
          ns("raster_opacity"),
          label = i18n$t("monitoring_fast_alerts_opacity_label"),
          min = 0, max = 1, value = 0.75, step = 0.05
        )
      ),
      # v0.53.0 — Le banner « zone saine » et le leafletOutput sont
      # désormais 2 outputs SÉPARÉS. Avant ce bump, ils vivaient dans
      # un seul `output$panel` qui dépendait de `raster_r` (pour
      # décider d'afficher le banner ou non) → à chaque changement
      # de raster_r (switch index NDVI/NBR, slider seuil, etc.), le
      # `renderUI` du panel re-render → le `leafletOutput` est
      # détruit + recréé → la map est ré-initialisée → l'observer
      # `leafletProxy::addRasterImage` peint dans le vide → user
      # devait bouger un autre slider pour forcer un repaint. Ce
      # split rend la map immortelle (renderLeaflet appelé UNE FOIS)
      # et seul le banner re-render quand raster_r change.
      # Bandeau `alert-info` bleu symétrique de Carte FAST : rappelle la
      # résolution Sentinel-2 (10 m) et décrit ce qui est peint selon le
      # mode (fréquence / intensité) et l'indice. Output SÉPARÉ du
      # leafletOutput (comme `banner`) → re-render sans détruire la map.
      shiny::uiOutput(ns("resolution_badge")),
      shiny::uiOutput(ns("banner")),
      leaflet::leafletOutput(ns("map"), height = "55vh")
    )
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

    # v0.53.0 — capture la dernière cause d'échec du raster (NULL =
    # OK, sinon chaîne descriptive). Lu par output$banner pour
    # distinguer « zone saine » (raster calculé, 0 alerte) de
    # « impossible à calculer » (cache incomplet pour cet indice).
    last_raster_error <- shiny::reactiveVal(NULL)

    # Refresh radio labels on language change. Preserves selection by
    # echoing back input$mode (or default "count" before first click).
    # v0.46.1 — `inline = TRUE` doit être passé explicitement ;
    # `updateRadioButtons` ne mémorise pas la valeur `inline` du
    # render initial (default FALSE → re-stack en vertical à chaque
    # changement de langue).
    # v0.52.8 — le label « Mode du raster » est désormais porté par
    # le contrôle lui-même (sidebar verticale, plus de `<strong>`
    # sibling), donc il faut le rafraîchir aussi sur changement de
    # langue. Idem pour le checkbox visibility et le slider opacité.
    shiny::observe({
      i18n <- i18n_r()
      # v0.67.1 — Bug d'oscillation : sans `isolate()`, lire
      # `input$index` et `input$mode` dans cet observer crée une
      # dépendance reactive → le user clique NDVI → NBR → input$index
      # change → l'observer re-fire → `updateRadioButtons(selected =
      # "NBR")` re-broadcast → le client re-bind l'event change →
      # boucle infinie de cliquetis NDVI ↔ NBR / count ↔ rolling.
      # L'observer doit dépendre UNIQUEMENT de la langue
      # (`i18n_r()`). `isolate()` lit la sélection courante au
      # moment du re-render i18n sans la traquer comme dépendance.
      cur_index <- shiny::isolate(input$index %||% "NDVI")
      cur_mode  <- shiny::isolate(input$mode  %||% "count")
      # v0.52.14 — rafraîchir aussi le label du radio `index` (label
      # « Indice FAST » → « FAST index » sur switch FR/EN).
      shiny::updateRadioButtons(
        session, "index",
        label = i18n$t("monitoring_fast_index_label"),
        choices = c(NDMI = "NDMI", NDVI = "NDVI", NBR = "NBR"),
        selected = cur_index,
        inline = TRUE
      )
      shiny::updateRadioButtons(
        session, "mode",
        label = i18n$t("monitoring_fast_alerts_mode_label"),
        choiceNames  = list(i18n$t("monitoring_fast_alerts_mode_count"),
                            i18n$t("monitoring_fast_alerts_mode_rolling")),
        choiceValues = list("count", "rolling"),
        selected     = cur_mode,
        inline       = TRUE
      )
      shiny::updateSliderInput(
        session, "raster_opacity",
        label = i18n$t("monitoring_fast_alerts_opacity_label")
      )
      # v0.61.0 — Le label du checkbox `raster_visible` n'est plus
      # rafraîchi car le checkbox a été retiré (visibilité gérée
      # par le LayersControl Leaflet).
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
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
      if (is.null(con)) return(NULL)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      mode <- input$mode %||% "count"
      # v0.52.13 → v0.52.14 — `nemeton@v0.55.0` (spec 017) : API
      # mono-index. L'indice vient désormais du radio LOCAL de
      # l'onglet (`input$index`, dans le sidebar droit) — pas du
      # sidebar parent. Le `threshold` correspondant reste lu depuis
      # le sidebar parent via `thresholds_r$ndvi` / `$nbr`.
      idx <- input$index %||% "NDVI"
      # Per-index threshold from the parent sidebar. NDMI uses its own
      # slider (th$ndmi), falling back to the NDVI threshold if absent.
      thr <- switch(idx,
                    NBR  = th$nbr,
                    NDMI = th$ndmi %||% th$ndvi,
                    th$ndvi)
      # v0.57.0 — Passage du raster CONTINU au mask catégoriel 0-4
      # (quartiles, spec 017 D1-D2 nemeton@v0.55.0+). On appelle
      # désormais `nemeton::compute_fast_alert_mask()` qui produit
      # un SpatRaster avec valeurs discrètes :
      #   0 = sain (pas d'alerte)
      #   1 = faible  (1er quartile au-dessus du seuil)
      #   2 = modéré  (2e quartile)
      #   3 = fort    (3e quartile)
      #   4 = sévère  (4e quartile)
      # La fonction persiste le TIF résultat sous `mask_cache_dir` et
      # renvoie son path invisiblement. On le charge ensuite via
      # `terra::rast()` pour passer au reactive en aval (banner,
      # leafletProxy::addRasterImage).
      #
      # Cache D6 (`result_cache_dir`) reste actif : il cache le raster
      # continu intermédiaire calculé par `read_fast_alert_raster()`
      # à l'intérieur de `compute_fast_alert_mask()`. Le mask 0-4
      # final est persisté sous son propre cache (`mask_cache_dir`).
      result_cache <- .fast_alert_cache_dir(
        app_state$current_project$path
      )
      mask_cache <- .fast_alert_mask_cache_dir(
        app_state$current_project$path
      )
      # v0.60.0 — `parallel = TRUE` en dur (retrait du checkbox UI
      # introduit en v0.58.0). Le cœur fait un fallback séquentiel
      # silencieux si `furrr` est absent, donc aucune dégradation.
      # Spec 017 D4 nemeton@v0.57.0+.
      mask_path <- tryCatch(
        nemeton::compute_fast_alert_mask(
          con              = con,
          zone_id          = as.integer(zone),
          index            = idx,
          threshold        = as.numeric(thr),
          date_from        = dr[1],
          date_to          = dr[2],
          mode             = mode,
          window_days      = as.integer(th$window_days %||% 30L),
          cache_dir        = cd,
          mask_cache_dir   = mask_cache,
          cache_result     = TRUE,
          result_cache_dir = result_cache,
          parallel         = TRUE
        ),
        error = function(e) {
          cli::cli_alert_warning(
            "compute_fast_alert_mask failed (index={idx}): {e$message}"
          )
          last_raster_error(paste0(idx, ": ", conditionMessage(e)))
          NULL
        }
      )
      if (is.null(mask_path) || !nzchar(mask_path)) {
        # v0.68.0 — Message i18n (brief FAST 6 cartes nemeton@v0.65.0).
        # Cas typique : aucune scène cachée ne porte les bandes de
        # l'indice dans la fenêtre (par ex. NDMI demande B08+B11,
        # une zone sans B11 produira NULL).
        last_raster_error(sprintf(
          i18n_r()$t("monitoring_fast_alerts_no_scene"),
          idx
        ))
        return(NULL)
      }
      out <- tryCatch(
        terra::rast(mask_path),
        error = function(e) {
          cli::cli_alert_warning(
            "terra::rast() failed on mask path: {e$message}"
          )
          last_raster_error(paste0(idx, ": ", conditionMessage(e)))
          NULL
        }
      )
      if (!is.null(out)) {
        last_raster_error(NULL)
      }
      out
    })

    # ----- Banner : seul ce slot re-render selon raster_r() ----------
    # v0.46.3 — la carte (OSM + UGFs) est désormais affichée même
    # quand le raster d'alerte est vide. Un bandeau « zone saine »
    # vert s'affiche au-dessus pour donner l'info, mais l'utilisateur
    # conserve le repère spatial (où est la zone, où sont les UGFs).
    # v0.53.0 — Banner extrait dans son propre uiOutput (était inclus
    # dans output$panel qui contenait aussi le leafletOutput → à
    # chaque raster_r change, la map était détruite/recréée et le
    # `addRasterImage` peignait dans le vide). Maintenant le banner
    # re-render indépendamment, la map est rendue UNE FOIS.
    # Blue resolution / context badge (mirrors Carte FAST). Reacts to
    # the mode + index so the message stays pertinent to what is drawn.
    output$resolution_badge <- shiny::renderUI({
      i18n  <- i18n_r()
      mode  <- input$mode %||% "count"
      index <- input$index %||% "NDVI"
      key <- if (identical(mode, "rolling")) {
        "monitoring_fast_alerts_badge_rolling"
      } else {
        "monitoring_fast_alerts_badge_count"
      }
      htmltools::div(
        class = paste("alert alert-info d-flex align-items-center",
                      "gap-2 py-1 px-2 mb-2 small"),
        bsicons::bs_icon("map", class = "fs-5 flex-shrink-0"),
        htmltools::tags$span(sprintf(i18n$t(key), index))
      )
    })

    # NDMI hint + B11 re-ingestion note — only when NDMI is selected.
    output$ndmi_note <- shiny::renderUI({
      if (!identical(input$index %||% "NDVI", "NDMI")) return(NULL)
      .fast_ndmi_note(i18n_r())
    })

    output$banner <- shiny::renderUI({
      i18n <- i18n_r()
      r <- raster_r()
      err <- last_raster_error()
      # v0.53.0 — 2 cas distincts au lieu d'un message vert
      # « zone saine » générique :
      #   * Erreur réelle (cache S2 incomplet pour cet indice,
      #     exception cœur) → bandeau JAUNE warning avec la cause.
      #   * Raster calculé mais 0 cellule en alerte → bandeau VERT
      #     « zone saine » classique.
      if (!is.null(err)) {
        return(htmltools::div(
          class = "alert alert-warning d-flex align-items-center gap-3 m-2 py-2 small",
          bsicons::bs_icon("exclamation-triangle-fill", class = "fs-3 flex-shrink-0"),
          htmltools::div(
            htmltools::tags$strong(i18n$t("monitoring_fast_alerts_error_title")),
            htmltools::tags$br(),
            htmltools::tags$span(err)
          )
        ))
      }
      if (is.null(r) || .fast_raster_is_empty(r)) {
        return(htmltools::div(
          class = "alert alert-success d-flex align-items-center gap-3 m-2 py-2 small",
          bsicons::bs_icon("check-circle", class = "fs-3 flex-shrink-0"),
          htmltools::div(
            htmltools::tags$strong(i18n$t("monitoring_fast_alerts_empty_title")),
            htmltools::tags$br(),
            htmltools::tags$span(class = "text-white",
                                 i18n$t("monitoring_fast_alerts_empty_body"))
          )
        ))
      }
      NULL
    })

    # v0.51.5 — la carte de base (tuiles + UGF + fitBounds) est rendue
    # UNE seule fois ; le raster d'alerte est ajouté/mis à jour via
    # leafletProxy dans l'observe ci-dessous. Avant, chaque déplacement
    # de slider (seuils, opacité) ou changement de mode déclenchait un
    # renderLeaflet complet → le zoom utilisateur et le fond sélectionné
    # (OSM / Satellite) étaient perdus à chaque tick. Symétrique avec
    # la Carte FAST.
    output$map <- leaflet::renderLeaflet({
      ugf_4326 <- .ugf_for_overlay(app_state$current_project)
      # v0.61.0 — Le raster d'alerte apparaît désormais dans le
      # LayersControl Leaflet (groupe `"Alertes"`, sous « UGF »).
      # Quand le user décoche, Leaflet masque le group ; l'observer
      # leafletProxy plus bas ajoute l'image dans `group = "Alertes"`.
      overlays <- c(
        if (!is.null(ugf_4326)) "UGF" else NULL,
        "Alertes"  # = .alert_raster_group (défini dans le server)
      )
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",     group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonAlertRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          baseGroups    = c("OSM", "Satellite"),
          overlayGroups = overlays,
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
        bb <- tryCatch(sf::st_bbox(ugf_4326), error = function(e) NULL)
        if (!is.null(bb)) {
          m <- m |> leaflet::fitBounds(
            lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
            lng2 = bb[["xmax"]], lat2 = bb[["ymax"]]
          )
        }
      }
      m
    })

    # Raster overlay update — fires on raster/mode/opacity changes
    # via leafletProxy, preserving user zoom and selected base
    # layer (OSM vs Satellite). The legend is added with a stable
    # layerId so we can remove and re-add it without touching the
    # layers control.
    #
    # v0.61.0 — `.alert_raster_group` est utilisé comme libellé
    # visible dans le LayersControl Leaflet (entrée « Alertes »
    # sous « UGF »). Le checkbox UI `raster_visible` est retiré :
    # Leaflet gère seul la visibilité via le contrôle de couches.
    .alert_raster_group <- "Alertes"
    .alert_legend_id    <- "alert-legend"
    shiny::observe({
      i18n    <- i18n_r()
      r       <- raster_r()
      mode    <- input$mode %||% "count"

      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup(.alert_raster_group) |>
        leaflet::removeControl(.alert_legend_id)

      if (is.null(r) || .fast_raster_is_empty(r)) return()

      raster_opacity <- as.numeric(input$raster_opacity %||% 0.75)
      if (!is.finite(raster_opacity) || raster_opacity < 0) raster_opacity <- 0
      if (raster_opacity > 1) raster_opacity <- 1

      # v0.57.0 — `raster_r()` renvoie désormais un mask catégoriel
      # 0-4 produit par `nemeton::compute_fast_alert_mask()` (spec
      # 017 D2). Plus de branchement count/rolling ici : les 2 modes
      # passent par la même palette discrète. Le `mode` côté cœur
      # change comment les quartiles sont CALCULÉS (compte de jours
      # vs intensité rolling), pas comment ils sont AFFICHÉS.
      #
      # Mask 0-4 :
      #   0 = sain    (transparent, on voit OSM/UGF dessous)
      #   1 = faible  → jaune
      #   2 = modéré  → orange
      #   3 = fort    → rouge-orangé
      #   4 = sévère  → rouge foncé
      # La classe 0 reste TRANSPARENTE (na.color = transparent +
      # domain démarrant à 1) — le banner vert « zone saine » prend
      # le relais visuel quand le raster entier est en classe 0.
      r_show <- terra::ifel(is.na(r) | r <= 0, NA, r)

      cols <- c("#fee08b", "#fdae61", "#f46d43", "#d73027")
      labels <- c(
        i18n$t("fast_alert_class_1"),
        i18n$t("fast_alert_class_2"),
        i18n$t("fast_alert_class_3"),
        i18n$t("fast_alert_class_4")
      )
      # `colorBin` avec 4 bornes pour 4 classes (1,2,3,4 inclus à
      # droite). `bins = c(0.5, 1.5, 2.5, 3.5, 4.5)` mappe chaque
      # valeur entière à sa classe sans ambiguïté.
      pal <- leaflet::colorBin(
        palette  = cols,
        domain   = c(0.5, 4.5),
        bins     = c(0.5, 1.5, 2.5, 3.5, 4.5),
        na.color = "transparent"
      )
      proxy |>
        leaflet::addRasterImage(
          x       = r_show, colors = pal, opacity = raster_opacity,
          method  = "ngb",
          group   = .alert_raster_group,
          options = leaflet::gridOptions(pane = "nemetonAlertRaster")
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors   = cols,
          labels   = labels,
          title    = i18n$t("fast_alert_legend_title"),
          opacity  = 0.85,
          layerId  = .alert_legend_id
        )
    })

    # v0.37.1 — see mod_monitoring_fordead_map.R : the navset toggles
    # nav_panels with bslib::nav_show() / nav_hide(), which leaves
    # Shiny's visibility detection unreliable. Force the panel
    # uiOutput to evaluate unconditionally so empty-state / leaflet
    # wrapper render the moment the user opens the sub-tab.
    # v0.53.0 — `panel` renommé en `banner` + leafletOutput direct.
    shiny::outputOptions(output, "banner", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "map",    suspendWhenHidden = FALSE)

    # v0.52.14 — `index_r` exporté pour que le module
    # validation_sampling (qui consomme aussi `read_fast_alert_raster`
    # pour la prévisualisation) suive l'indice choisi par
    # l'utilisateur côté Alertes FAST. Sans ce wiring,
    # validation_sampling serait coincé sur NDVI ou NBR en dur.
    invisible(list(
      raster  = raster_r,
      index_r = shiny::reactive(input$index %||% "NDVI")
    ))
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
# alert raster.
#
# v0.45.0 (suite, label unification) — produit 4 classes alignées
# avec les classes 1-4 du masque catégoriel utilisé par
# `nemeton::compute_fast_alert_mask()` (qui découpe sur les
# quartiles par défaut). Cohérence avec Plan de validation FAST :
# les checkbox classes 1-4 du formulaire affichent EXACTEMENT les
# mêmes intervalles que la légende Alertes FAST.
#
# Labels préfixés par le numéro de classe + unité optionnelle. Ex :
#   "1 — 1-12 j" / "2 — 13-25 j" / "3 — 26-37 j" / "4 — >37 j"
#
# Distribution dégénérée (constant value, < 4 valeurs distinctes)
# collapse à moins de 4 classes — l'appelant adapte la palette via
# `.alert_count_palette(length(labels))`.
#
# @param r SpatRaster avec NA hors AOI et zéros masqués upstream.
# @param unit Suffixe optionnel pour chaque label (ex. "j" / "d").
#   Pas d'espace ajouté ; passer `" j"` pour avoir « 1-12 j ».
# @return list(breaks, labels). Breaks : 5 points pour 4 classes
#   (ou moins si la distribution est dégénérée).
.classify_alert_count <- function(r, unit = "") {
  vals <- tryCatch(terra::values(r), error = function(e) NULL)
  if (is.null(vals)) return(list(breaks = c(0, 1),
                                 labels = sprintf("1+%s", unit)))
  vals <- vals[!is.na(vals) & vals > 0]
  if (!length(vals)) return(list(breaks = c(0, Inf),
                                 labels = character(0)))
  q <- as.numeric(stats::quantile(vals, probs = c(0, .25, .50, .75, 1.0),
                                  na.rm = TRUE))
  q <- unique(as.integer(round(q)))
  q <- q[q > 0]
  if (!length(q)) return(list(breaks = c(0, 1),
                              labels = sprintf("1+%s", unit)))
  if (length(q) == 1L) {
    return(list(breaks = c(0, q[1] + 0.5),
                labels = sprintf("1 — %d%s", q[1], unit)))
  }
  # 4 quartiles produce up to 4 bins. Labels carry the class
  # number prefix (1..n) + interval + optional unit.
  n_bins <- length(q) - 1L
  labels <- character(n_bins)
  for (i in seq_len(n_bins)) {
    lo <- q[i]
    hi <- q[i + 1L] - 1L
    rng <- if (i == n_bins) sprintf(">%d%s", q[i], unit)
           else if (lo > hi) sprintf("%d%s", lo, unit)
           else if (lo == hi) sprintf("%d%s", lo, unit)
           else sprintf("%d-%d%s", lo, hi, unit)
    labels[i] <- sprintf("%d — %s", i, rng)
  }
  # Bins for colorBin : 5 points for 4 classes. Lower edges of
  # bins 2..n on `q[i] - 0.5`, upper edge of the top bin on
  # `q_last + 0.5` to include the max value.
  bins <- c(0.5, q[-1] - 0.5)
  bins[length(bins)] <- q[length(q)] + 0.5
  list(breaks = bins, labels = labels)
}


# v0.45.0 — wrapper produisant des labels par classe FAST/FORDEAD
# pour les consommateurs externes (mod_validation_sampling au moins).
# Pour FORDEAD, retourne les libellés biologiques fixes (1=faible,
# 2=moyenne, 3=forte, 4=sol nu). Pour FAST, dérive les labels des
# quartiles du raster fourni ; fallback statique générique quand le
# raster est NULL.
#
# @param r SpatRaster mono-couche (count) ou NULL.
# @param source "FAST" ou "FORDEAD".
# @param mode "count" ou "rolling" — drive l'unité affichée.
# @param i18n L'objet `get_i18n(lang)`.
# @return named list "1".."4" -> label string.
.fast_class_labels <- function(r, source = c("FAST", "FORDEAD"),
                                mode   = c("count", "rolling"),
                                i18n) {
  source <- match.arg(source)
  mode   <- match.arg(mode)
  if (identical(source, "FORDEAD")) {
    return(list(
      "1" = i18n$t("validation_class_fordead_1"),
      "2" = i18n$t("validation_class_fordead_2"),
      "3" = i18n$t("validation_class_fordead_3"),
      "4" = i18n$t("validation_class_fordead_4")
    ))
  }
  if (is.null(r) || .fast_raster_is_empty(r)) {
    return(list(
      "1" = i18n$t("validation_class_fast_1"),
      "2" = i18n$t("validation_class_fast_2"),
      "3" = i18n$t("validation_class_fast_3"),
      "4" = i18n$t("validation_class_fast_4")
    ))
  }
  unit_key <- if (identical(mode, "count")) "validation_class_unit_days"
              else                          "validation_class_unit_deficit"
  unit <- i18n$t(unit_key)
  unit_sfx <- if (nzchar(unit)) paste0(" ", unit) else ""
  cls <- .classify_alert_count(r, unit = unit_sfx)
  # Pad to exactly 4 entries with NA-safe fallbacks when the
  # distribution is degenerate.
  out <- list("1" = NA_character_, "2" = NA_character_,
              "3" = NA_character_, "4" = NA_character_)
  for (i in seq_along(cls$labels)) {
    if (i <= 4L) out[[as.character(i)]] <- cls$labels[i]
  }
  # Substitute NA with the static fallback for the missing classes.
  for (i in 1:4) {
    k <- as.character(i)
    if (is.na(out[[k]])) {
      out[[k]] <- i18n$t(sprintf("validation_class_fast_%d", i))
    }
  }
  out
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

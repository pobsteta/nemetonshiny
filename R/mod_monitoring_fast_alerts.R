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
#   - "trend"   : déclin chronique pluriannuel (dépérissement feuillus).
#                 Composite saisonnier annuel (months) → Theil-Sen
#                 (pente) + Mann-Kendall (significativité, alpha) par
#                 pixel (nemeton spec 023). Indices red-edge / humidité
#                 (NDMI défaut, NDRE) ; threshold / window_days ignorés ;
#                 paramètres months / min_years / alpha exposés en sidebar.
#                 Même sortie 0-4 que count/rolling après discrétisation.
#
# Pas de popup au clic — l'exploration pixel-par-pixel se fait sur
# l'onglet « Carte FAST » (mod_monitoring_pixel_map).

#' FAST index choices for a given mode (mode-dependent vocabulary)
#'
#' count / rolling expose the short-term shock indices NDMI / NDVI / NBR ;
#' trend exposes the moisture / red-edge indices NDMI / NDRE (chronic
#' decline — NDRE only makes sense for a pluriannual decline trend, not for
#' acute threshold exceedances).
#'
#' @param mode "count", "rolling" or "trend".
#' @return Named character vector for `radioButtons(choices=)`.
#' @noRd
.fast_index_choices <- function(mode) {
  if (identical(mode, "trend")) {
    c(NDMI = "NDMI", NDRE = "NDRE")
  } else {
    c(NDMI = "NDMI", NDVI = "NDVI", NBR = "NBR")
  }
}

#' Default FAST index for a given mode
#' @noRd
.fast_index_default <- function(mode) {
  if (identical(mode, "trend")) "NDMI" else "NDVI"
}

#' Compute (and disk-cache) one FAST alert mask
#'
#' Thin wrapper around `nemeton::compute_fast_alert_mask()` centralising the
#' argument plumbing shared by the displayed raster (`raster_r`) and the trend
#' cache pre-warming observer, so the two call sites never drift. Persists a
#' TIF per (index, parameters) and returns its path (idempotent: a cached
#' result is reused, no recompute). No error handling here — callers wrap it
#' so they can route failures to their own UI feedback.
#' @noRd
.compute_fast_mask <- function(con, zone, index, threshold, dr, mode,
                               window_days, months, min_years, alpha,
                               cache_dir, mask_cache, result_cache) {
  nemeton::compute_fast_alert_mask(
    con              = con,
    zone_id          = as.integer(zone),
    index            = index,
    threshold        = as.numeric(threshold),
    date_from        = dr[1],
    date_to          = dr[2],
    mode             = mode,
    window_days      = as.integer(window_days %||% 30L),
    months           = months,
    min_years        = min_years,
    alpha            = alpha,
    cache_dir        = cache_dir,
    mask_cache_dir   = mask_cache,
    cache_result     = TRUE,
    result_cache_dir = result_cache,
    parallel         = TRUE
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
            # Choix initiaux du mode count/rolling (défaut "count") : NDMI
            # (humidité), NDVI (défaut), NBR. NDRE (red-edge) n'est proposé
            # qu'en mode Tendance (déclin chronique) — l'observer `mode`
            # bascule le vocabulaire via .fast_index_choices().
            choices = c(NDMI = "NDMI", NDVI = "NDVI", NBR = "NBR"),
            selected = "NDVI",
            inline = TRUE
          ),
          class = "mb-2"
        ),
        # NDMI hint + B11 re-ingestion note (shown only when NDMI active).
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
                                i18n$t("monitoring_fast_alerts_mode_rolling"),
                                i18n$t("monitoring_fast_alerts_mode_trend")),
            choiceValues = list("count", "rolling", "trend"),
            selected     = "count"
          ),
          class = "mb-2"
        ),
        # Paramètres trend-only (Theil-Sen + Mann-Kendall, nemeton
        # spec 023). Masqués hors mode `trend` ; threshold / window_days
        # n'ont pas de sens en trend (ignorés côté cœur).
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'trend'", ns("mode")),
          shiny::sliderInput(
            ns("trend_months"), i18n$t("monitoring_trend_months"),
            min = 1, max = 12, value = c(6, 9), step = 1
          ),
          shiny::numericInput(
            ns("trend_min_years"), i18n$t("monitoring_trend_min_years"),
            value = 4, min = 2, max = 20, step = 1
          ),
          shiny::numericInput(
            ns("trend_alpha"), i18n$t("monitoring_trend_alpha"),
            value = 0.05, min = 0.001, max = 0.2, step = 0.005
          )
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
      cur_mode  <- shiny::isolate(input$mode  %||% "count")
      cur_index <- shiny::isolate(input$index %||% .fast_index_default(cur_mode))
      # v0.52.14 — rafraîchir aussi le label du radio `index` (label
      # « Indice FAST » → « FAST index » sur switch FR/EN).
      # Les choix d'indices dépendent du mode (trend → NDMI/NDRE) ; on
      # garde la sélection courante si elle reste valide, sinon défaut.
      idx_choices <- .fast_index_choices(cur_mode)
      if (!(cur_index %in% idx_choices)) cur_index <- .fast_index_default(cur_mode)
      shiny::updateRadioButtons(
        session, "index",
        label = i18n$t("monitoring_fast_index_label"),
        choices = idx_choices,
        selected = cur_index,
        inline = TRUE
      )
      shiny::updateRadioButtons(
        session, "mode",
        label = i18n$t("monitoring_fast_alerts_mode_label"),
        choiceNames  = list(i18n$t("monitoring_fast_alerts_mode_count"),
                            i18n$t("monitoring_fast_alerts_mode_rolling"),
                            i18n$t("monitoring_fast_alerts_mode_trend")),
        choiceValues = list("count", "rolling", "trend"),
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

    # Mode change → adapte le vocabulaire d'indices (trend → NDMI/NDRE ;
    # count/rolling → NDMI/NDVI/NBR). Préserve l'indice courant s'il
    # reste valide pour le nouveau mode, sinon retombe sur le défaut.
    # `ignoreInit = TRUE` : le rendu initial pose déjà les bons choix.
    shiny::observeEvent(input$mode, {
      i18n <- i18n_r()
      mode <- input$mode %||% "count"
      choices <- .fast_index_choices(mode)
      cur <- shiny::isolate(input$index %||% .fast_index_default(mode))
      if (!(cur %in% choices)) cur <- .fast_index_default(mode)
      shiny::updateRadioButtons(
        session, "index",
        label = i18n$t("monitoring_fast_index_label"),
        choices = choices, selected = cur, inline = TRUE
      )
    }, ignoreInit = TRUE)

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

    # Inventaire disque des scènes S2 cachées (scene_id + obs_date), requis
    # par `nemeton::extract_pixel_trend()` au clic. Même logique que la Carte
    # FAST (mod_monitoring_pixel_map) ; on réutilise le helper package-interne
    # `.pixel_scene_date_from_id`.
    scenes_df_r <- shiny::reactive({
      refresh_r()
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
      out[order(out$obs_date), , drop = FALSE]
    })

    # Calcul effectif du mask d'alerte (lourd). Extrait de `raster_r` pour
    # être appelé en DIFFÉRÉ (onFlushed) par le trigger ci-dessous, afin que
    # la notification « calcul en cours » s'affiche AVANT le calcul. Lit les
    # réactifs directement → l'appelant l'enveloppe dans isolate/domaine.
    compute_fast_raster <- function() {
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
      idx <- input$index %||% .fast_index_default(mode)
      # Per-index threshold from the parent sidebar. NDMI uses its own
      # slider (th$ndmi), falling back to the NDVI threshold if absent.
      # NDRE n'existe qu'en mode trend, où le threshold est ignoré côté
      # cœur (Theil-Sen / Mann-Kendall) — on retombe donc sur th$ndmi.
      thr <- switch(idx,
                    NBR  = th$nbr,
                    NDMI = th$ndmi %||% th$ndvi,
                    NDRE = th$ndmi %||% th$ndvi,
                    th$ndvi)
      # Paramètres trend-only (Theil-Sen + Mann-Kendall, nemeton spec
      # 023). En count/rolling ces valeurs valent les défauts cœur et
      # sont ignorées ; en trend elles viennent de la sidebar.
      is_trend <- identical(mode, "trend")
      tm <- if (is_trend) input$trend_months %||% c(6L, 9L) else c(6L, 9L)
      months_v    <- seq.int(as.integer(tm[1]), as.integer(tm[2]))
      min_years_v <- if (is_trend) as.integer(input$trend_min_years %||% 4L) else 4L
      alpha_v     <- if (is_trend) as.numeric(input$trend_alpha %||% 0.05) else 0.05
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
      compute_errored <- FALSE
      mask_path <- tryCatch(
        # Args trend-only (months / min_years / alpha) ignorés en
        # count/rolling côté cœur.
        .compute_fast_mask(con, zone, idx, thr, dr, mode,
                           th$window_days, months_v, min_years_v, alpha_v,
                           cd, mask_cache, result_cache),
        error = function(e) {
          cli::cli_alert_warning(
            "compute_fast_alert_mask failed (index={idx}): {e$message}"
          )
          # v0.85.14 — surface le VRAI message d'erreur cœur (ex.
          # « [mosaic] resolution does not match » sur un cache
          # multi-tuiles MGRS) au lieu du message générique « aucune
          # scène » ci-dessous, qui le masquait (le 2e setter écrasait
          # le 1er). On note l'erreur pour ne PAS la réécraser.
          compute_errored <<- TRUE
          last_raster_error(sprintf("%s : %s", idx, conditionMessage(e)))
          NULL
        }
      )
      if (is.null(mask_path) || !nzchar(mask_path)) {
        # v0.68.0 — Message i18n (brief FAST 6 cartes nemeton@v0.65.0).
        # Cas « aucune scène cachée ne porte les bandes » UNIQUEMENT
        # quand le cœur a renvoyé NULL SANS lever d'erreur. Si le calcul
        # a échoué (compute_errored), on conserve le vrai message déjà
        # posé dans le handler ci-dessus.
        if (!compute_errored) {
          last_raster_error(sprintf(
            i18n_r()$t("monitoring_fast_alerts_no_scene"),
            idx
          ))
        }
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
    }

    # Résultat du mask, alimenté par le trigger déféré ci-dessous. Les
    # consommateurs (bandeau, peinture carte, clic trend) lisent `raster_r()`
    # qui retourne cette valeur — sans déclencher le calcul lourd.
    raster_rv <- shiny::reactiveVal(NULL)
    raster_r  <- shiny::reactive(raster_rv())

    # v0.88.x — Notification « calcul en cours » + calcul différé : au
    # changement d'indice / mode / zone / dates / seuils / params trend, on
    # affiche d'abord un message bas-droite, PUIS on calcule le mask via
    # onFlushed (un calcul synchrone ne flush l'UI qu'à sa sortie → le
    # message ne serait pas visible). Évite les clics intempestifs avant
    # l'affichage de la nouvelle carte.
    shiny::observe({
      # Dépendances : recalcul quand l'un de ces réactifs change.
      refresh_r(); zone_id_r(); date_range_r(); thresholds_r()
      input$index; input$mode
      input$trend_months; input$trend_min_years; input$trend_alpha
      # Dépendance sur l'onglet principal actif : on ne calcule (et ne
      # notifie) que lorsque « Suivi sanitaire » est ouvert. Au chargement
      # d'un projet, `zone_id_r()` se remplit (zone monitoring auto-
      # sélectionnée) alors que l'utilisateur est encore sur « Sélection »
      # — sans ce garde, le message « Calcul du raster d'alerte… »
      # s'affichait à tort. Quand l'utilisateur bascule sur l'onglet, ce
      # réactif change et l'observer recalcule.
      active_tab <- app_state$active_main_tab
      zone <- shiny::isolate(zone_id_r())
      if (is.null(zone) || !isTRUE(nzchar(zone))) {
        raster_rv(NULL)
        return()
      }
      if (!identical(active_tab, "monitoring")) {
        return()
      }
      notif_id <- session$ns("fast_raster_busy")
      shiny::showNotification(
        i18n_r()$t("monitoring_fast_raster_computing"),
        id = notif_id, type = "message", duration = NULL
      )
      session$onFlushed(function() {
        on.exit(
          shiny::removeNotification(notif_id, session = session),
          add = TRUE
        )
        shiny::withReactiveDomain(
          session, shiny::isolate(raster_rv(compute_fast_raster()))
        )
      }, once = TRUE)
    })

    # ----- Pré-calcul des DEUX rasters de Tendance (NDMI + NDRE) ------
    # En mode trend, `raster_r` calcule/affiche l'indice sélectionné ;
    # cet observateur réchauffe en arrière-plan (later) le cache disque
    # des DEUX indices trend, de sorte que basculer le radio NDMI ↔ NDRE
    # soit instantané (cache hit). `compute_fast_alert_mask` persiste un
    # TIF par (index, paramètres) → idempotent (pas de recalcul si déjà
    # en cache). N'a d'effet qu'en trend : count/rolling restent
    # mono-index à la demande. Le trigger est débouncé pour ne pas
    # empiler des calculs lourds pendant le réglage des sliders trend.
    trend_prewarm_trigger <- shiny::reactive({
      list(input$mode, zone_id_r(), date_range_r(),
           input$trend_months, input$trend_min_years, input$trend_alpha,
           refresh_r())
    }) |> shiny::debounce(800)

    shiny::observe({
      if (!identical(input$mode %||% "count", "trend")) return()
      zone <- zone_id_r()
      dr   <- date_range_r()
      th   <- thresholds_r()
      proj <- app_state$current_project
      cd   <- cache_dir_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return()
      if (length(dr) != 2L || any(is.na(dr))) return()
      if (is.null(th) || is.null(proj) || is.null(cd)) return()

      tm           <- input$trend_months %||% c(6L, 9L)
      months_v     <- seq.int(as.integer(tm[1]), as.integer(tm[2]))
      min_years_v  <- as.integer(input$trend_min_years %||% 4L)
      alpha_v      <- as.numeric(input$trend_alpha %||% 0.05)
      thr          <- th$ndmi %||% th$ndvi          # ignoré en trend (Theil-Sen)
      window_v     <- th$window_days %||% 30L
      result_cache <- .fast_alert_cache_dir(proj$path)
      mask_cache   <- .fast_alert_mask_cache_dir(proj$path)

      # Différé : laisse `raster_r` afficher l'indice sélectionné d'abord,
      # puis réchauffe le cache des deux indices hors du cycle de rendu.
      later::later(function() {
        for (idx in c("NDMI", "NDRE")) {
          con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
          if (is.null(con)) next
          tryCatch(
            .compute_fast_mask(con, zone, idx, thr, dr, "trend",
                               window_v, months_v, min_years_v, alpha_v,
                               cd, mask_cache, result_cache),
            error = function(e)
              cli::cli_alert_warning(
                "trend pre-warm failed (index={idx}): {conditionMessage(e)}"
              )
          )
          close_monitoring_db_connection(con)
        }
      }, delay = 0.4)
    }) |> shiny::bindEvent(trend_prewarm_trigger())

    # ===== Clic carte (mode trend) → graphe de tendance par pixel ======
    # En mode Tendance, un clic sur la carte ouvre une modale montrant,
    # pour le pixel cliqué, les composites saisonniers annuels de l'indice
    # + la droite Theil-Sen + la significativité — i.e. POURQUOI ce pixel a
    # cette couleur. Toute la statistique vient du cœur
    # (`nemeton::extract_pixel_trend`, garanti cohérent avec le raster) ;
    # l'app ne recalcule rien (règle 1).
    pixel_trend_computing <- shiny::reactiveVal(FALSE)

    # Sévérité 0-4 du pixel : lue directement dans le raster mask affiché
    # (les quartiles sont zone-wide → non recalculables au pixel).
    .severity_at <- function(rast, lng, lat) {
      if (is.null(rast)) return(NA_integer_)
      tryCatch({
        pt <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(lng, lat)),
                                              crs = 4326))
        pt <- sf::st_transform(pt, terra::crs(rast))
        v <- terra::extract(rast, terra::vect(pt))
        as.integer(v[1, 2])
      }, error = function(e) NA_integer_)
    }

    # Construit + affiche la modale du graphe trend. Lourd (lit la valeur du
    # pixel sur N scènes) → appelé en différé (onFlushed) par l'observateur
    # de clic, pour afficher d'abord la notification « calcul en cours ».
    show_pixel_trend <- function(lng, lat, cd, sdf, idx, months_v,
                                 min_years_v, alpha_v, rast) {
      i18n <- i18n_r()
      res <- tryCatch(
        nemeton::extract_pixel_trend(
          cache_dir = cd, scenes_df = sdf, xy = c(lng, lat), crs = 4326,
          index = idx, months = months_v, min_years = min_years_v,
          alpha = alpha_v
        ),
        error = function(e) {
          cli::cli_alert_warning("extract_pixel_trend failed: {conditionMessage(e)}")
          NULL
        }
      )
      if (is.null(res) || is.null(res$composites) ||
          !any(!is.na(res$composites$value))) {
        shiny::showNotification(
          i18n$t("fast_trend_pixel_no_data"), type = "warning", duration = 5
        )
        return(invisible(NULL))
      }

      comp   <- res$composites[!is.na(res$composites$value), , drop = FALSE]
      enough <- isTRUE(res$enough_years)
      signif <- isTRUE(res$significant_decline)
      sev    <- .severity_at(rast, lng, lat)

      p <- plotly::plot_ly()
      p <- plotly::add_trace(
        p, x = comp$year, y = comp$value, type = "scatter",
        mode = "lines+markers", name = idx,
        line = list(color = "#1B6B1B", width = 1.5),
        marker = list(color = "#1B6B1B", size = 6),
        hovertemplate = paste0("%{x}<br>", idx, " = %{y:.3f}<extra></extra>")
      )
      # Droite Theil-Sen (seulement si assez d'années) — rouge si déclin
      # significatif, gris sinon.
      if (enough && !is.na(res$theil_sen_slope)) {
        line_col <- if (signif) "#C0392B" else "#7F7F7F"
        yfit <- res$theil_sen_intercept + res$theil_sen_slope * comp$year
        p <- plotly::add_trace(
          p, x = comp$year, y = yfit, type = "scatter", mode = "lines",
          name = i18n$t("fast_trend_pixel_theilsen"),
          line = list(color = line_col, width = 2, dash = "dash"),
          hoverinfo = "skip"
        )
      }
      p <- plotly::layout(
        p,
        margin = list(t = 20, b = 40, l = 50, r = 10),
        xaxis  = list(title = i18n$t("fast_trend_pixel_xaxis"), dtick = 1),
        yaxis  = list(title = sprintf(i18n$t("fast_trend_pixel_yaxis"), idx)),
        legend = list(orientation = "h", y = -0.25),
        showlegend = TRUE
      )
      p <- plotly::config(p, responsive = TRUE)

      # Bandeau d'annotations sous le titre (pente / p / significativité /
      # années / classe sévérité).
      slope_txt <- if (is.na(res$theil_sen_slope)) "–" else
        sprintf("%+.4f", res$theil_sen_slope)
      p_txt <- if (is.na(res$mann_kendall_p)) "–" else
        sprintf("%.3f", res$mann_kendall_p)
      sev_txt <- if (is.na(sev)) "–" else as.character(sev)
      info <- htmltools::div(
        class = "small text-muted mb-2 d-flex flex-wrap gap-3",
        htmltools::span(sprintf(i18n$t("fast_trend_pixel_slope_fmt"), slope_txt)),
        htmltools::span(sprintf(i18n$t("fast_trend_pixel_pvalue_fmt"), p_txt)),
        htmltools::span(
          class = if (signif) "badge text-bg-danger" else "badge text-bg-secondary",
          sprintf(i18n$t("fast_trend_pixel_signif_fmt"),
                  if (signif) i18n$t("yes") else i18n$t("no"))
        ),
        htmltools::span(sprintf(i18n$t("fast_trend_pixel_nyears_fmt"), res$n_years)),
        htmltools::span(sprintf(i18n$t("fast_trend_pixel_severity_fmt"), sev_txt))
      )

      shiny::showModal(shiny::modalDialog(
        title = htmltools::tagList(
          htmltools::span(sprintf(
            i18n$t("fast_trend_pixel_title_fmt"), idx,
            round(lat, 5), round(lng, 5)
          )),
          htmltools::tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-secondary",
            style = paste("position: absolute; top: 0.75rem;",
                          "right: 0.75rem; z-index: 2;"),
            title = i18n$t("fast_trend_pixel_fullscreen"),
            # Toggle plein écran + `resize` différé : plotly (responsive)
            # n'écoute que window.resize ; sans cet événement, le graphe
            # garde sa taille initiale et ne remplit pas l'écran agrandi.
            onclick = paste0(
              "this.closest('.modal-dialog').classList.toggle('modal-fullscreen');",
              "setTimeout(function(){window.dispatchEvent(new Event('resize'));},250);"),
            bsicons::bs_icon("arrows-fullscreen")
          )
        ),
        size = "l", easyClose = TRUE,
        footer = shiny::modalButton(i18n$t("close")),
        info,
        htmltools::tags$style(htmltools::HTML(
          ".modal-fullscreen .fast-trend-wrap{height:calc(100vh - 170px) !important;}"
        )),
        htmltools::div(
          class = "fast-trend-wrap", style = "height: 55vh;",
          plotly::plotlyOutput(session$ns("pixel_trend_plot"), height = "100%")
        )
      ))
      output$pixel_trend_plot <- plotly::renderPlotly(p)
      invisible(NULL)
    }

    shiny::observeEvent(input$map_click, {
      # Uniquement en mode Tendance (les composites/Theil-Sen n'ont de sens
      # qu'en trend) ; et garde anti-multi-clics pendant le calcul.
      if (!identical(input$mode %||% "count", "trend")) return()
      if (isTRUE(shiny::isolate(pixel_trend_computing()))) return()
      lat <- input$map_click$lat
      lng <- input$map_click$lng
      if (is.null(lat) || is.null(lng)) return()
      cd  <- cache_dir_r()
      sdf <- scenes_df_r()
      if (is.null(cd) || is.null(sdf) || !nrow(sdf)) return()

      idx <- input$index %||% .fast_index_default("trend")
      tm  <- input$trend_months %||% c(6L, 9L)
      months_v    <- seq.int(as.integer(tm[1]), as.integer(tm[2]))
      min_years_v <- as.integer(input$trend_min_years %||% 4L)
      alpha_v     <- as.numeric(input$trend_alpha %||% 0.05)
      rast <- raster_r()

      notif_id <- session$ns("pixel_trend_computing")
      pixel_trend_computing(TRUE)
      shiny::showNotification(
        i18n_r()$t("fast_trend_pixel_computing"),
        id = notif_id, type = "message", duration = NULL
      )
      # onFlushed : la notif part au client AVANT le calcul lourd (un
      # observateur synchrone ne flush qu'à sa sortie).
      session$onFlushed(function() {
        on.exit({
          shiny::removeNotification(notif_id, session = session)
          shiny::isolate(pixel_trend_computing(FALSE))
        }, add = TRUE)
        shiny::isolate(show_pixel_trend(lng, lat, cd, sdf, idx, months_v,
                                        min_years_v, alpha_v, rast))
      }, once = TRUE)
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
      # Assainir l'indice contre les choix valides du mode : au moment
      # du switch count/rolling -> trend, ce slot se re-render sur
      # `input$mode` AVANT que `updateRadioButtons` n'ait renvoye NDMI
      # depuis le client, donc `input$index` vaut encore l'ancien
      # indice (ex. NBR), invalide en trend. On retombe alors sur le
      # defaut du mode (NDMI en trend) plutot que d'afficher un libelle
      # « declin NBR » qui n'existe pas en tendance pluriannuelle.
      index <- input$index %||% .fast_index_default(mode)
      if (!(index %in% .fast_index_choices(mode))) {
        index <- .fast_index_default(mode)
      }
      key <- if (identical(mode, "trend")) {
        "monitoring_fast_alerts_badge_trend"
      } else if (identical(mode, "rolling")) {
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
      # v0.76.0 — l'indice courant (NDMI / NDVI / NBR) est injecté dans
      # le titre de la légende pour rappeler à l'utilisateur quel indice
      # alimente les classes de sévérité affichées (symétrie avec le
      # badge de résolution qui mentionne déjà l'indice).
      index   <- input$index %||% "NDVI"

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
          title    = sprintf(i18n$t("fast_alert_legend_title"), index),
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
      index_r = shiny::reactive(input$index %||% "NDVI"),
      # v0.87.x — params trend exportés pour que le « Plan de validation
      # FAST » réutilise la MÊME définition de tendance que celle affichée
      # ici (mois saison / années min / alpha), sans dupliquer les contrôles.
      trend_params_r = shiny::reactive({
        tm <- input$trend_months %||% c(6L, 9L)
        list(
          months    = seq.int(as.integer(tm[1]), as.integer(tm[2])),
          min_years = as.integer(input$trend_min_years %||% 4L),
          alpha     = as.numeric(input$trend_alpha %||% 0.05)
        )
      })
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
                                mode   = c("count", "rolling", "trend"),
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
              else if (identical(mode, "trend")) "validation_class_unit_trend"
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

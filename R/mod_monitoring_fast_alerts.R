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
      # v0.46.2 — `mb-0` retire le margin-bottom 1rem du form-group
      # par défaut, qui causait un léger décalage visuel vertical
      # entre « Mode du raster » et les radios « Fréquence / Intensité »
      # (align-items-center centre la boîte avec sa marge, pas le
      # contenu visuel).
      htmltools::tagAppendAttributes(
        shiny::radioButtons(
          ns("mode"),
          label  = NULL,
          inline = TRUE,
          choiceNames  = list(i18n$t("monitoring_fast_alerts_mode_count"),
                              i18n$t("monitoring_fast_alerts_mode_rolling")),
          choiceValues = list("count", "rolling"),
          selected     = "count"
        ),
        class = "mb-0"
      ),
      # v0.48.0 — toggle visibilité + slider opacité du raster
      # d'alerte, symétrique avec Carte FAST (v0.47.0).
      # `top: 2px` abaisse légèrement la case pour l'aligner sur les
      # radios « Fréquence / Intensité » voisines (la case rendait un
      # poil plus haut que le texte des radios).
      htmltools::tagAppendAttributes(
        shiny::checkboxInput(
          ns("raster_visible"),
          label = i18n$t("monitoring_fast_alerts_raster_visible"),
          value = TRUE
        ),
        class = "mb-0",
        style = "position: relative; top: 2px;"
      ),
      # Label « Opacité du raster » à GAUCHE du slider (inline) plutôt
      # qu'au-dessus.
      htmltools::div(
        class = "d-flex align-items-center gap-2",
        htmltools::tags$span(i18n$t("monitoring_fast_alerts_opacity_label")),
        htmltools::div(
          style = "min-width: 160px;",
          htmltools::tagAppendAttributes(
            shiny::sliderInput(
              ns("raster_opacity"),
              label = NULL,
              min = 0, max = 1, value = 0.75, step = 0.05
            ),
            class = "mb-0"
          )
        )
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
    # v0.46.1 — `inline = TRUE` doit être passé explicitement ;
    # `updateRadioButtons` ne mémorise pas la valeur `inline` du
    # render initial (default FALSE → re-stack en vertical à chaque
    # changement de langue).
    shiny::observe({
      i18n <- i18n_r()
      shiny::updateRadioButtons(
        session, "mode",
        label = NULL,
        choiceNames  = list(i18n$t("monitoring_fast_alerts_mode_count"),
                            i18n$t("monitoring_fast_alerts_mode_rolling")),
        choiceValues = list("count", "rolling"),
        selected     = input$mode %||% "count",
        inline       = TRUE
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
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
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

    # ----- Panel : map (always rendered) + optional empty-state banner -----
    # v0.46.3 — la carte (OSM + UGFs) est désormais affichée même
    # quand le raster d'alerte est vide. Un bandeau « zone saine »
    # vert s'affiche au-dessus pour donner l'info, mais l'utilisateur
    # conserve le repère spatial (où est la zone, où sont les UGFs).
    output$panel <- shiny::renderUI({
      i18n <- i18n_r()
      r <- raster_r()
      banner <- if (is.null(r) || .fast_raster_is_empty(r)) {
        htmltools::div(
          class = "alert alert-success d-flex align-items-center gap-3 m-2 py-2 small",
          bsicons::bs_icon("check-circle", class = "fs-3 flex-shrink-0"),
          htmltools::div(
            htmltools::tags$strong(i18n$t("monitoring_fast_alerts_empty_title")),
            htmltools::tags$br(),
            htmltools::tags$span(class = "text-white",
                                 i18n$t("monitoring_fast_alerts_empty_body"))
          )
        )
      } else NULL
      htmltools::tagList(
        banner,
        leaflet::leafletOutput(session$ns("map"), height = "55vh")
      )
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

    # Raster overlay update — fires on raster/mode/visibility/opacity
    # changes via leafletProxy, preserving user zoom and selected base
    # layer (OSM vs Satellite). The legend is added with a stable
    # layerId so we can remove and re-add it without touching the
    # layers control.
    .alert_raster_group <- "alert-raster"
    .alert_legend_id    <- "alert-legend"
    shiny::observe({
      i18n    <- i18n_r()
      r       <- raster_r()
      mode    <- input$mode %||% "count"
      visible <- isTRUE(input$raster_visible %||% TRUE)

      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup(.alert_raster_group) |>
        leaflet::removeControl(.alert_legend_id)

      if (!visible || is.null(r) || .fast_raster_is_empty(r)) return()

      raster_opacity <- as.numeric(input$raster_opacity %||% 0.75)
      if (!is.finite(raster_opacity) || raster_opacity < 0) raster_opacity <- 0
      if (raster_opacity > 1) raster_opacity <- 1

      # Tout ce qui ≤ 0 ou NA = pas d'alerte → transparent. v0.51.9 :
      # avant on ne masquait QUE r == 0, mais des valeurs négatives
      # résiduelles (bruit numérique en mode rolling, ou cellules hors
      # tuile MGRS croppées différemment) passaient à `pal()` qui les
      # déclarait hors domaine → warning « Some values were outside
      # the color scale » + cellules NA + raster invisible quand les
      # valeurs négatives dominaient. Le masque ≤ 0 garantit qu'aucune
      # cellule traversant la palette n'est hors domaine côté bas.
      r_show <- terra::ifel(is.na(r) | r <= 0, NA, r)

      if (identical(mode, "count")) {
        unit <- i18n$t("validation_class_unit_days")
        unit_sfx <- if (nzchar(unit)) paste0(" ", unit) else ""
        cls <- .classify_alert_count(r_show, unit = unit_sfx)
        if (length(cls$breaks) < 2L) return()
        cols <- .alert_count_palette(length(cls$labels))
        pal <- leaflet::colorBin(
          palette  = cols,
          domain   = range(cls$breaks),
          bins     = cls$breaks,
          na.color = "transparent"
        )
        proxy |>
          leaflet::addRasterImage(
            x = r_show, colors = pal, opacity = raster_opacity,
            method  = "ngb",
            group   = .alert_raster_group,
            options = leaflet::gridOptions(pane = "nemetonAlertRaster")
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            colors   = cols,
            labels   = cls$labels,
            title    = i18n$t("monitoring_fast_alerts_legend_count_title"),
            opacity  = 0.85,
            layerId  = .alert_legend_id
          )
      } else {
        vals <- terra::values(r_show)
        vals <- vals[!is.na(vals) & vals > 0]
        if (!length(vals)) return()
        upper <- as.numeric(stats::quantile(vals, 0.95, na.rm = TRUE))
        if (!is.finite(upper) || upper <= 0) upper <- max(vals, na.rm = TRUE)
        # v0.51.9 : clamp à `upper` (cap visuel p95) AVANT pal(), pour
        # éviter le warning « Some values were outside the color scale »
        # sur les ~5 % de cellules > p95. Le cap reste visible (couleur
        # max saturée) sans hors-domaine.
        r_capped <- terra::ifel(r_show > upper, upper, r_show)
        pal <- leaflet::colorNumeric(
          palette  = c("#FFD27F", "#FF9933", "#D62728"),
          domain   = c(0, upper),
          na.color = "transparent"
        )
        proxy |>
          leaflet::addRasterImage(
            x = r_capped, colors = pal, opacity = raster_opacity,
            group   = .alert_raster_group,
            options = leaflet::gridOptions(pane = "nemetonAlertRaster")
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            pal      = pal,
            values   = c(0, upper),
            title    = i18n$t("monitoring_fast_alerts_legend_title"),
            opacity  = 0.85,
            layerId  = .alert_legend_id
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

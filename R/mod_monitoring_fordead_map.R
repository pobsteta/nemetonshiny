# mod_monitoring_fordead_map.R - Sub-tab "Carte FORDEAD" of Suivi sanitaire.
#
# Wires `nemeton::read_fordead_dieback_mask()` (shipped in nemeton@v0.25.0)
# into the monitoring navset. Renders the categorical 0..4 dieback mask :
#
#   0 = sain     -> green   (#2CA02C)
#   1 = faible   -> pale-orange (#FFD27F)
#   2 = moyenne  -> orange  (#FF9933)
#   3 = forte    -> red     (#D62728)
#   4 = sol nu   -> near-black (#222222)
#   NA = hors mask forestier -> transparent
#
# Raster pinned to the custom `nemetonRaster` Leaflet pane (z-index 250),
# same pattern as Carte FAST so the basemap toggle (OSM / Satellite) keeps
# the mask visible without re-stacking the overlay on every interaction.
#
# Caveat (nemeton@v0.25.0) : `run_fordead_dieback()` does NOT yet persist
# the classified mask to disk - the persist hook ships in a later coeur
# release. Until then this reader returns NULL and the panel shows an
# empty state. The wiring is in place so the panel activates automatically
# the day the coeur writer ships.

# Max class value of a categorical FORDEAD dieback mask, NA-safe.
# Returns NA_real_ when the raster is NULL / not a SpatRaster / empty /
# all-NA. Used (Phase A, decision D2) to decide healthy-vs-affected
# FROM THE RASTER rather than from a DB alert count :
#   max >= 1  -> au moins un pixel classe >= 1 -> zone affectee ;
#   max < 1 ou NA -> tout sain (classe 0) ou tout NA -> zone saine.
.fordead_raster_max <- function(r) {
  if (is.null(r) || !inherits(r, "SpatRaster")) return(NA_real_)
  if (terra::ncell(r) == 0L) return(NA_real_)
  tryCatch(
    as.numeric(terra::global(r, "max", na.rm = TRUE)[[1]]),
    error = function(e) NA_real_
  )
}

# Bbox (EPSG:4326) pour cadrer la carte FORDEAD : prefere l'emprise des
# UGF (deja en 4326), sinon retombe sur l'etendue du raster reprojetee.
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

# Partie B - specification d'affichage par couche pixel FORDEAD.
# Renvoie pour un raster `r` et une `layer` : le raster a peindre
# (`r_show`), la palette leaflet, la methode de reechantillonnage et la
# specification de legende (discrete ou continue). Centralise pour rester
# identique entre le render de base et l'observer d'opacite.
#   severity        -> masque 0-4, classe 0 transparente, palette discrete.
#   first_anomaly   -> date (jours depuis 1970), palette viridis, legende dates.
#   anomaly_index   -> severite continue, palette YlOrRd.
#   modelled_pixels -> binaire 0/1 (non modelise / modelise), palette discrete.
.fordead_layer_spec <- function(r, layer, i18n) {
  .num_range <- function(r) {
    v <- tryCatch(terra::values(r, na.rm = TRUE), error = function(e) numeric(0))
    if (!length(v) || !is.finite(min(v)) || min(v) == max(v))
      return(if (length(v)) c(min(v), min(v) + 1) else c(0, 1))
    range(v)
  }
  if (identical(layer, "first_anomaly")) {
    rng <- .num_range(r)
    # Rampe bleu -> rouge des dates : `RdYlBu` inverse mappe les valeurs
    # basses (detections ANCIENNES) sur le bleu et les valeurs hautes
    # (detections RECENTES) sur le rouge.
    pal <- leaflet::colorNumeric("RdYlBu", domain = rng, reverse = TRUE,
                                 na.color = "transparent")
    return(list(
      r_show = r, pal = pal, method = "bilinear",
      legend = list(type = "continuous", pal = pal, values = rng,
                    title = i18n$t("monitoring_fordead_layer_legend_date"),
                    # Echelle en annee seule (%Y) - plus lisible qu'une date
                    # complete sur une legende continue pluriannuelle.
                    labFormat = function(type, cuts, p)
                      format(as.Date(cuts, origin = "1970-01-01"), "%Y"))))
  }
  if (identical(layer, "anomaly_index")) {
    rng <- .num_range(r)
    pal <- leaflet::colorNumeric("YlOrRd", domain = rng,
                                 na.color = "transparent")
    return(list(
      r_show = r, pal = pal, method = "bilinear",
      legend = list(type = "continuous", pal = pal, values = rng,
                    title = i18n$t("monitoring_fordead_layer_legend_index"),
                    labFormat = NULL)))
  }
  if (identical(layer, "modelled_pixels")) {
    cols <- c("#BBBBBB", "#1f78b4")
    return(list(
      r_show = r,
      pal    = leaflet::colorFactor(cols, levels = 0:1, na.color = "transparent"),
      method = "ngb",
      legend = list(type = "discrete", colors = cols,
                    labels = c(i18n$t("monitoring_fordead_layer_modelled_no"),
                               i18n$t("monitoring_fordead_layer_modelled_yes")),
                    title = i18n$t("monitoring_fordead_layer_confidence"))))
  }
  # severity (defaut) - classe 0 transparente.
  cols <- c("#2CA02C", "#FFD27F", "#FF9933", "#D62728", "#222222")
  list(
    r_show = terra::ifel(is.na(r) | r <= 0, NA, r),
    pal    = leaflet::colorFactor(cols, levels = 0:4, na.color = "transparent"),
    method = "ngb",
    legend = list(type = "discrete", colors = cols,
                  labels = c(sprintf("0 - %s", i18n$t("monitoring_fordead_class_0")),
                             sprintf("1 - %s", i18n$t("monitoring_fordead_class_1")),
                             sprintf("2 - %s", i18n$t("monitoring_fordead_class_2")),
                             sprintf("3 - %s", i18n$t("monitoring_fordead_class_3")),
                             sprintf("4 - %s", i18n$t("monitoring_fordead_class_4"))),
                  title = i18n$t("monitoring_fordead_class_title")))
}

# Construit le libelle d'un choix de couche (radio) avec le " i " d'information
# de l'app - `info_popover_in_label()`, variant sur dans un <label> de radio :
# s'informer sur une couche ne doit pas la selectionner (chaque selection
# declenche une lecture de raster).
.fordead_layer_choice <- function(label, info) {
  htmltools::tagList(label, " ", info_popover_in_label(info))
}

# Ajoute la legende correspondant a la spec (.fordead_layer_spec$legend).
# `layerId` permet de retirer/re-poser la legende via leafletProxy (le
# raster est mis a jour par proxy sans re-render de la carte).
.fordead_add_legend <- function(map, lg, layerId = NULL) {
  if (identical(lg$type, "discrete")) {
    leaflet::addLegend(map, position = "bottomright", colors = lg$colors,
                       labels = lg$labels, title = lg$title, opacity = 0.85,
                       layerId = layerId)
  } else {
    leaflet::addLegend(map, position = "bottomright", pal = lg$pal,
                       values = lg$values, title = lg$title, opacity = 0.85,
                       labFormat = lg$labFormat %||% leaflet::labelFormat(),
                       layerId = layerId)
  }
}

#' Carte FORDEAD sub-tab UI
#'
#' @param id Module namespace id.
#' @return A `shiny.tag`.
#' @noRd
mod_monitoring_fordead_map_ui <- function(id) {
  ns <- shiny::NS(id)
  # Le `leafletOutput` est en UI STATIQUE (jamais recree) - parite avec la
  # Carte FAST (mod_monitoring_pixel_map). Auparavant il vivait dans
  # `output$panel` (renderUI) et etait recree a chaque changement de
  # masque / couche / langue, ce qui detruisait le binding `input$map_click`
  # (clic-pixel inoperant). Les etats " placeholder / zone saine / couche
  # indisponible " s'affichent desormais via un overlay positionne par-dessus
  # la carte, sans toucher au widget leaflet.
  htmltools::div(
    style = "position: relative;",
    leaflet::leafletOutput(ns("map"), height = "55vh"),
    shiny::uiOutput(ns("overlay"))
  )
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
#'   having to reload the project or re-pick the zone. Optional -
#'   defaults to a constant reactive for back-compat / tests.
#' @param opacity_r Reactive returning the raster opacity (0..1) from the
#'   tab's right sidebar slider (parite FAST). Optional - defaults to a
#'   constant 0.75 for back-compat / tests.
#' @param con_provider Optional function returning a (cached, read-only)
#'   monitoring DB connection to reuse instead of opening a new one per
#'   evaluation (perf - a remote PostGIS connect costs ~0.4-1.2 s). When
#'   `NULL` (default, tests/back-compat) the module opens and closes its
#'   own connection. When provided, the connection is reused and NOT
#'   closed here (the provider owns its lifecycle).
#' @param layer_r Reactive returning the displayed pixel layer (Partie B) :
#'   one of `"severity"` (mask 0-4, defaut), `"first_anomaly"`,
#'   `"anomaly_index"`, `"modelled_pixels"`. Optional - defaults to a
#'   constant `"severity"` for back-compat / tests.
#' @return invisible list with `mask` reactive.
#' @noRd
mod_monitoring_fordead_map_server <- function(id, app_state, zone_id_r,
                                              refresh_r = shiny::reactive(0L),
                                              opacity_r = shiny::reactive(0.75),
                                              layer_r   = shiny::reactive("severity"),
                                              date_r    = shiny::reactive(NULL),
                                              con_provider = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # Phase A (spec 008 sect.15, D2) - FORDEAD est calcule sur la zone `_tot`
    # (union des UGFs). Le masque ET la serie pixel vivent donc sous
    # `cache/layers/fordead/zone_<id_tot>/`. Ce helper resout l'id `_tot`
    # du projet (find_zones_by_project + convention de nommage `_tot`,
    # spec 020) ; fallback sur la zone passee (back-compat pre-spec-020).
    # Utilise par `mask_r` (lecture masque) ET par le clic-pixel (lecture
    # serie CRSWIR) pour qu'ils pointent sur la MEME zone que le run.
    .fordead_tot_id <- function(con, proj, fallback_zone) {
      id_tot <- tryCatch({
        zdf <- nemeton::find_zones_by_project(con, project_uuid = proj$id)
        idx <- grep("_tot$", as.character(zdf$name))
        if (length(idx)) as.integer(zdf$id[idx[1]]) else NA_integer_
      }, error = function(e) NA_integer_)
      if (!is.na(id_tot)) id_tot else suppressWarnings(as.integer(fallback_zone))
    }

    # ----- Core call ------------------------------------------------------
    # cache_dir is resolved from the active project - nemeton@v0.41.0
    # persists the categorical 0-4 mask to
    # <project>/cache/layers/fordead/zone_<id>/dieback_mask_<ts>.tif
    # after the postprocess phase.
    #
    # v0.38.6 - `refresh_r()` is read here so a completed FORDEAD run
    # invalidates mask_r. Without it the reactive only depended on
    # zone_id + current_project: it evaluated once (before the run,
    # when cache/layers/fordead/ did not exist yet -> NULL) and stayed
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
      # perf - reutilise la connexion RO du parent si fournie ; sinon
      # ouvre la sienne (back-compat / tests) et la ferme.
      own_con <- is.null(con_provider)
      con <- .perf_time("fordead mask: db_connect",
                        if (own_con)
                          get_monitoring_db_connection(project = proj, read_only = TRUE)
                        else con_provider())
      if (is.null(con)) return(NULL)
      if (own_con) on.exit(close_monitoring_db_connection(con), add = TRUE)

      # Phase A (D2) - on lit TOUJOURS sur la zone `_tot` ; l'affichage
      # par strate n'est qu'un masquage spatial du raster `_tot`.
      id_tot   <- .perf_time("fordead mask: resolve _tot (find_zones)",
                             .fordead_tot_id(con, proj, zone))
      read_zone <- id_tot

      # Partie B - lecture branchee sur la couche selectionnee :
      #   severity -> masque categoriel 0-4 (read_fordead_dieback_mask) ;
      #   first_anomaly / anomaly_index / modelled_pixels -> couche pixel
      #   (read_fordead_layer, nemeton >= 0.94.0). NULL si la couche est
      #   absente du bundle (anciens runs) -> empty-state " indisponible ".
      lyr <- layer_r() %||% "severity"
      r <- tryCatch(
        .perf_time(sprintf("fordead mask: read layer=%s", lyr),
          if (identical(lyr, "severity"))
            nemeton::read_fordead_dieback_mask(
              con, zone_id = read_zone, run_id = NULL, cache_dir = cd)
          else
            nemeton::read_fordead_layer(
              con, zone_id = read_zone, layer = lyr,
              run_id = NULL, cache_dir = cd)),
        error = function(e) {
          cli::cli_alert_warning(
            "read FORDEAD layer ({lyr}) failed: {e$message}")
          NULL
        }
      )
      if (is.null(r)) return(NULL)

      # Masquage par strate : si la strate selectionnee n'est PAS `_tot`,
      # clipper le raster `_tot` a l'AOI de la strate (les pixels hors
      # strate passent en NA -> transparents dans addRasterImage). C'est
      # de la presentation pure (clip d'affichage), aucun calcul metier
      # (regle CLAUDE.md #3). CRS : le masque FORDEAD est en EPSG:2154 et
      # get_monitoring_zone_aoi rend du 2154 ; on transforme l'AOI au CRS
      # du raster par securite avant terra::mask.
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

    # Couche " date de 1re anomalie " (jours depuis 1970) lue UNE fois par
    # zone (deps zone/refresh, PAS la date ni la couche affichee). Sert (a)
    # au domaine du slider temporel, (b) au filtrage cumulatif de la
    # severite. NULL si le bundle ne contient pas la couche (anciens runs).
    first_anomaly_r <- shiny::reactive({
      refresh_r()
      zone <- zone_id_r()
      if (is.null(zone) || !isTRUE(nzchar(zone))) return(NULL)
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return(NULL)
      cd <- file.path(proj$path, "cache", "layers", "fordead")
      if (!dir.exists(cd)) return(NULL)
      own_con <- is.null(con_provider)
      con <- if (own_con)
        get_monitoring_db_connection(project = proj, read_only = TRUE)
      else con_provider()
      if (is.null(con)) return(NULL)
      if (own_con) on.exit(close_monitoring_db_connection(con), add = TRUE)
      id_tot <- .fordead_tot_id(con, proj, zone)
      tryCatch(
        nemeton::read_fordead_layer(con, zone_id = id_tot,
                                    layer = "first_anomaly",
                                    run_id = NULL, cache_dir = cd),
        error = function(e) NULL)
    })

    # Domaine temporel du slider : etendue des dates de 1re detection.
    date_domain_r <- shiny::reactive({
      fa <- first_anomaly_r()
      if (is.null(fa)) return(NULL)
      rng <- suppressWarnings(range(terra::values(fa, mat = FALSE),
                                    na.rm = TRUE))
      if (length(rng) != 2L || any(!is.finite(rng))) return(NULL)
      as.Date(rng, origin = "1970-01-01")
    })

    # Raster effectivement DESSINE sur la Carte FORDEAD. Pour les couches
    # " severite " ET " date de 1re detection ", filtrage cumulatif par la
    # date du slider : seuls les pixels dont la 1re detection <= date choisie
    # restent visibles (progression du deperissement dans le temps - parite
    # conceptuelle avec le slider de date de la Carte FAST). Les autres
    # couches (resumes non temporels) ne sont pas filtrees.
    display_r <- shiny::reactive({
      r <- mask_r()
      if (is.null(r)) return(NULL)
      lyr <- layer_r() %||% "severity"
      if (!lyr %in% c("severity", "first_anomaly")) return(r)
      sel_date <- date_r()
      # Pas de date selectionnee (slider absent / non encore rendu) -> on
      # affiche la severite complete SANS lire `first_anomaly` (evite une
      # lecture disque inutile).
      if (is.null(sel_date)) return(r)
      fa <- first_anomaly_r()
      if (is.null(fa)) return(r)
      tnum <- suppressWarnings(as.numeric(as.Date(sel_date)))
      if (!is.finite(tnum)) return(r)
      dom <- date_domain_r()
      # Slider au maximum -> aucun filtrage (affiche tout).
      if (!is.null(dom) && tnum >= as.numeric(dom[2])) return(r)
      tryCatch(terra::ifel(is.na(fa) | fa > tnum, NA, r),
               error = function(e) r)
    })

    # ----- Overlay : etats placeholder / zone saine / indisponible -------
    # Rendu PAR-DESSUS la carte (jamais a la place : le `leafletOutput` vit
    # en UI statique pour preserver le binding `input$map_click`). Retourne
    # NULL quand un raster affichable est present -> la carte est visible nue.
    .fordead_overlay <- function(...) {
      htmltools::div(
        style = paste(
          "position: absolute; inset: 0; z-index: 500;",
          "background: #fff; display: flex; align-items: center;",
          "justify-content: center;"),
        htmltools::div(...)
      )
    }
    output$overlay <- shiny::renderUI({
      i18n <- i18n_r()
      lyr  <- layer_r() %||% "severity"
      r <- mask_r()
      if (is.null(r)) {
        # Couche NON-severity absente du bundle (anciens runs < coeur
        # v0.94.0) -> message dedie " couche indisponible ". Pour severity,
        # NULL = aucun masque sur disque -> placeholder " lancer un
        # diagnostic " (etat c).
        if (!identical(lyr, "severity")) {
          return(.fordead_overlay(
            class = "p-4 text-center text-muted",
            bsicons::bs_icon("slash-circle",
                             class = "fs-1 d-block mx-auto mb-3"),
            htmltools::p(class = "mb-0",
                         i18n$t("monitoring_fordead_layer_unavailable"))
          ))
        }
        return(.fordead_overlay(
          class = "p-4 text-center text-muted",
          bsicons::bs_icon("hourglass-split",
                           class = "fs-1 d-block mx-auto mb-3"),
          htmltools::h5(class = "mt-3",
                        i18n$t("monitoring_fordead_map_empty_title")),
          htmltools::p(class = "mb-0",
                       i18n$t("monitoring_fordead_map_empty_body"))
        ))
      }
      # Le court-circuit " zone saine " ne vaut QUE pour la severite
      # (Phase A, D2 : classe >= 1 = affecte). Les autres couches
      # (date / indice / zone modelisee) s'affichent toujours.
      if (identical(lyr, "severity")) {
        mx <- .fordead_raster_max(r)
        if (!is.finite(mx) || mx < 1) {
          # Etat (b) - tout classe 0 / NA -> carte " zone saine ".
          return(.fordead_overlay(
            bslib::card(
              class = "border-success",
              bslib::card_header(htmltools::div(
                class = "d-flex align-items-center",
                bsicons::bs_icon("check-circle-fill",
                                 class = "me-2 text-success fs-4"),
                htmltools::tags$strong(
                  i18n$t("monitoring_fordead_no_alerts_title")))),
              bslib::card_body(htmltools::tags$p(
                i18n$t("monitoring_fordead_no_alerts_body"))))
          ))
        }
      }
      # Raster affichable -> pas d'overlay, la carte est visible.
      NULL
    })

    # CARTE DE BASE STABLE (parite Carte FAST) : `output$map` ne depend
    # QUE du projet courant. Le masque, la couche et l'opacite sont lus en
    # `isolate()` -> le widget leaflet n'est JAMAIS re-rendu quand ils
    # changent. C'est essentiel : un re-render recree le widget et fait
    # perdre le binding `input$map_click` (-> le clic-pixel ne se declenche
    # plus) en plus de reinitialiser le zoom et le fond. Les mises a jour
    # du raster/legende passent par l'observer `leafletProxy` ci-dessous.
    output$map <- leaflet::renderLeaflet({
      proj     <- app_state$current_project   # SEUL dep reactif
      ugf_4326 <- .ugf_for_overlay(proj)
      r        <- shiny::isolate(display_r())  # raster filtre par la date
      i18n     <- shiny::isolate(i18n_r())
      op       <- as.numeric(shiny::isolate(opacity_r()) %||% 0.75)
      if (!is.finite(op)) op <- 0.75
      op <- max(0, min(1, op))
      # " UGF " TOUJOURS dans le controle (parite Carte pixel FAST) : son
      # trace est (re)dessine par l'observer reactif plus bas des que
      # `indicators_sf` est disponible (l'attache est differee au chargement,
      # donc l'UGF pouvait manquer si le render de base precedait l'attache).
      # Le groupe porte le RASTER de deperissement (severity / confidence /
      # date...) : nomme " Raster " - v0.106.4, ex-" Alertes ", un nom herite
      # de l'epoque ou la couche portait des marqueurs placettes. Litteral
      # neutre FR/EN (comme " UGF ", " Indice ") : un group id Leaflet doit
      # rester stable au changement de langue.
      overlays <- c("UGF", "Raster")

      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",   group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          baseGroups    = c("OSM", "Satellite"),
          overlayGroups = overlays,
          options    = leaflet::layersControlOptions(collapsed = TRUE)
        )
      # Raster + legende dessines a l'etat courant (isolate) pour etre
      # presents des l'affichage ; l'observer ci-dessous les met a jour.
      if (!is.null(r)) {
        spec <- .fordead_layer_spec(
          r, shiny::isolate(layer_r()) %||% "severity", i18n)
        m <- m |>
          leaflet::addRasterImage(
            x = spec$r_show, colors = spec$pal, opacity = op,
            method = spec$method, group = "Raster",
            options = leaflet::gridOptions(pane = "nemetonRaster"))
        m <- .fordead_add_legend(m, spec$legend, layerId = "fordead_legend")
      }
      if (!is.null(ugf_4326)) {
        m <- m |>
          leaflet::addPolygons(
            data = ugf_4326, group = "UGF", color = "#1f78b4",
            weight = 2, opacity = 0.9, fillOpacity = 0)
        bb <- tryCatch(sf::st_bbox(ugf_4326), error = function(e) NULL)
        if (!is.null(bb)) {
          m <- m |> leaflet::fitBounds(
            lng1 = bb[["xmin"]], lat1 = bb[["ymin"]],
            lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])
        }
      }
      m
    })

    # Mise a jour RASTER + LEGENDE via leafletProxy (parite Carte FAST) :
    # la carte de base n'est jamais re-rendue, donc `input$map_click`, le
    # zoom et le fond (OSM/Satellite) sont preserves. Depend de la couche,
    # du masque, de l'opacite et de la langue -> redessine le seul group
    # "Raster" + la legende (layerId stable) sans reconstruire la carte.
    # `display_r()` (et non `mask_r()`) -> le deplacement du slider de date
    # redessine le raster filtre via leafletProxy, sans re-render complet.
    shiny::observe({
      r    <- display_r()
      lyr  <- layer_r() %||% "severity"
      i18n <- i18n_r()
      op   <- as.numeric(opacity_r() %||% 0.75)
      if (!is.finite(op)) op <- 0.75
      op <- max(0, min(1, op))
      # Groupes overlay actuellement COCHES cote client (leaflet renvoie
      # `input$<id>_groups`). Lu ici pour respecter la decoche : sans ca,
      # re-dessiner le raster via proxy le re-affichait meme decoche.
      # `isolate()` : leaflet renvoie cet input a chaque ajout/retrait de
      # groupe, et cet observe en ajoute - une lecture reactive le rendrait
      # auto-declenchant (peintures multiples). Cf. mod_accessibility.
      shown <- shiny::isolate(input$map_groups)
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup("Raster") |>
        leaflet::removeControl("fordead_legend")
      if (is.null(r)) return()
      spec <- .fordead_layer_spec(r, lyr, i18n)
      proxy <- proxy |>
        leaflet::addRasterImage(
          x = spec$r_show, colors = spec$pal, opacity = op,
          method = spec$method, group = "Raster",
          options = leaflet::gridOptions(pane = "nemetonRaster"))
      proxy <- .fordead_add_legend(proxy, spec$legend, layerId = "fordead_legend")
      # Respecter la decoche utilisateur : si " Raster " n'est pas coche,
      # masquer le group apres l'avoir re-dessine.
      if (!is.null(shown) && !("Raster" %in% shown)) {
        leaflet::hideGroup(proxy, "Raster")
      }
    })

    # UGF overlay via leafletProxy (reactif sur le projet) : (re)dessine les
    # polygones UGF des que `indicators_sf` est attache - corrige la
    # disparition de l'UGF quand le render de base precedait l'attache
    # differee. Parite avec la Carte pixel FAST.
    shiny::observe({
      ugf <- .ugf_for_overlay(app_state$current_project)
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup("UGF")
      if (is.null(ugf) || !nrow(ugf)) return()
      leaflet::addPolygons(
        proxy, data = ugf, group = "UGF", color = "#1f78b4",
        weight = 2, opacity = 0.9, fillOpacity = 0)
    })

    # v0.37.1 - force the overlay renderUI to evaluate even while the
    # sub-tab is hidden. The Suivi sanitaire navset toggles its
    # nav_panels with bslib::nav_show() / nav_hide() (mode-driven
    # visibility). That mechanism leaves Shiny's per-output
    # visibility detection unreliable : the `uiOutput(ns("overlay"))`
    # stayed suspended (suspendWhenHidden defaults to TRUE) and the
    # empty-state never rendered even after the user clicked the
    # Carte FORDEAD tab - the panel showed up blank. Disabling the
    # suspend makes the empty-state overlay render unconditionally.
    shiny::outputOptions(output, "overlay", suspendWhenHidden = FALSE)
    # Meme raison pour la carte elle-meme : sans ca, le widget leaflet
    # de ce sous-onglet non-defaut (affiche via nav_show/nav_hide) peut
    # rester suspendu / s'initialiser a taille 0 -> clics et leafletProxy
    # (opacite) inoperants. Parite avec Carte FAST (mod_monitoring_pixel_map).
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)

    # Carte FORDEAD est un sous-onglet non-defaut : quand l'utilisateur y
    # navigue, le conteneur leaflet vient seulement de devenir visible.
    # On force alors `invalidateSize` (Leaflet re-detecte ses dimensions,
    # sinon clics + proxy opacite tombent dans le vide) + un fitBounds.
    # Memes mecanisme et handler JS que Carte FAST (custom.js).
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

    # v0.59.0 (TODO #3) - diagnostic pixel CRSWIR au clic sur la
    # carte FORDEAD. Parite fonctionnelle avec la " Carte pixel
    # FAST " (mod_monitoring_pixel_map.R::observeEvent(input$map_click)).
    # Le clic extrait via `nemeton::read_fordead_pixel_series()` la
    # serie CRSWIR observee (points) et la prediction harmonique
    # (ligne) au pixel clique, puis affiche un plotly dans un modal
    # avec un marqueur vertical sur la date de 1re anomalie
    # (`attr(., "premiere_detection")`) si presente.
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

      # IMPORTANT : resoudre la connexion ICI (contexte reactif de
      # l'observeEvent), PAS dans le `onFlushed` ci-dessous. `con_provider`
      # est `mon_con()` du parent, qui lit `app_state$current_project$id`
      # pour cle de cache - l'appeler depuis le callback `onFlushed` (hors
      # contexte reactif) leve " Can't access reactive value
      # 'current_project' outside of reactive consumer ". On capture donc
      # `con` (et `own_con`) maintenant, et le callback les reutilise.
      own_con <- is.null(con_provider)
      con <- if (own_con)
        get_monitoring_db_connection(project = proj, read_only = TRUE)
      else con_provider()

      # Message " calcul en cours " affiche TOUT DE SUITE (parite Carte
      # FAST). Le calcul lourd (lecture serie CRSWIR + trace plotly) est
      # defere via `session$onFlushed` pour que la notification parte au
      # client AVANT le calcul (un observateur synchrone ne flushe l'UI
      # qu'a sa sortie). `on.exit` retire la notif quoi qu'il arrive
      # (succes, " pas de donnees ", erreur).
      .notif_id <- session$ns("fordead_pixel_loading")
      shiny::showNotification(
        i18n$t("monitoring_pixel_map_computing"),
        id = .notif_id, type = "message", duration = NULL
      )
      session$onFlushed(function() {
        on.exit(shiny::removeNotification(.notif_id, session = session),
                add = TRUE)
        # Ferme la connexion seulement si on l'a ouverte nous-memes (sinon
        # elle appartient au parent via `con_provider`/`mon_con`).
        if (own_con && !is.null(con)) {
          on.exit(close_monitoring_db_connection(con), add = TRUE)
        }

      # `onFlushed` s'execute HORS consommateur reactif : on enveloppe tout
      # le calcul dans `shiny::isolate()` (parite exacte avec la Carte FAST,
      # mod_monitoring_pixel_map.R). Toutes les valeurs reactives (con, proj,
      # zone, cd, lat, lng, i18n) ont deja ete capturees plus haut en
      # contexte reactif ; l'isolate est une ceinture-bretelles contre tout
      # acces reactif residuel.
      shiny::isolate({
      # Phase A (D2) - la serie pixel doit etre lue sur la zone `_tot`
      # (la ou FORDEAD a tourne), pas sur la strate selectionnee : le
      # pixel clique est un pixel du raster `_tot` (masque a l'affichage).
      # Sans ca, cliquer sur une strate != `_tot` ne renvoyait aucune serie
      # (cache sous zone_<id_tot>) -> graphe absent.
      zone_tot <- if (!is.null(con)) {
        .fordead_tot_id(con, proj, zone)
      } else {
        suppressWarnings(as.integer(zone))
      }

      ts <- tryCatch(
        nemeton::read_fordead_pixel_series(
          con       = NULL,  # spec : con reserved, NULL accepte
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
        # v0.72.0 - duration ^ 4 -> 8 s + wording plus explicite.
        # Cas typique : clic hors zone modelisee (extent du bundle
        # FORDEAD plus petit que l'AOI rendue sur la carte).
        shiny::showNotification(
          i18n$t("monitoring_fordead_pixel_no_data"),
          type = "warning", duration = 8
        )
        return()
      }

      # v0.72.0 - Enrichissement du modal CRSWIR (consume les 5
      # colonnes de `nemeton::read_fordead_pixel_series()` au lieu
      # de juste obs/pred) :
      #   * Bande `seuil_haut` (`crswir_pred + threshold_anomaly`)
      #     -> ligne pointillee orange : enveloppe de detection.
      #   * Points en anomalie (`anomalie == TRUE`) surlignes rouge
      #     vif marker size 8 par-dessus la trace observee.
      #   * `attr(ts, "premiere_detection")` -> marqueur vertical
      #     (existant depuis v0.59.0).
      #   * `attr(ts, "dans_zone_validite")` -> annotation discrete
      #     en haut a gauche si FALSE (pixel hors zone de validite
      #     pour l'essence dominante).
      premiere      <- attr(ts, "premiere_detection")
      in_validity   <- attr(ts, "dans_zone_validite")
      veg_index     <- attr(ts, "vegetation_index") %||% "CRSWIR"

      # Couleurs du rendu canonique FORDEAD (cf. plot FORDEAD natif) :
      # prediction + seuil en bleu (ligne pleine / pointillee), points
      # categorises training / healthy / anomaly / confirmed.
      col_pred      <- "#1F4EA8"  # bleu prediction harmonique (ligne pleine)
      col_limit     <- "#1F4EA8"  # bleu seuil (ligne pointillee)
      col_training  <- "#1F77B4"  # bleu x points d'entrainement
      col_healthy   <- "#2CA02C"  # vert x points sains
      col_anomaly   <- "#FF7F0E"  # orange x anomalie (avant confirmation)
      col_confirmed <- "#D62728"  # rouge x anomalie confirmee (deperissement)

      obs_date <- as.Date(ts$obs_date)
      obs_val  <- as.numeric(ts$crswir_obs)
      has_obs  <- !is.na(obs_val)
      anom     <- if ("anomalie" %in% names(ts))
        (!is.na(ts$anomalie) & ts$anomalie) else rep(FALSE, length(obs_val))

      # Fenetre d'entrainement du run (persistee dans les metadonnees
      # projet au lancement du diagnostic). Sert a colorer les points
      # d'entrainement (bleu) distinctement des points de suivi.
      train_dates <- tryCatch(
        as.Date(unlist(proj$metadata$monitoring_dates_training)),
        error = function(e) NULL)
      has_train_win <- length(train_dates) == 2L && all(!is.na(train_dates))
      is_train <- has_obs & has_train_win &
        obs_date >= train_dates[1] & obs_date <= train_dates[2]

      # Confirmation du deperissement : tout point de SUIVI a partir de la
      # date de 1re detection (`premiere_detection`) est " confirme "
      # (le pixel est entre en etat deperissement) ; avant cette date, un
      # point au-dessus du seuil est une " anomalie " (orange) et un point
      # sous le seuil est " sain " (vert).
      has_premiere <- !is.null(premiere) && length(premiere) == 1L &&
        inherits(premiere, "Date") && !is.na(premiere)
      is_confirmed <- has_obs & !is_train & has_premiere & obs_date >= premiere
      is_anomaly   <- has_obs & !is_train & !is_confirmed & anom
      is_healthy   <- has_obs & !is_train & !is_confirmed & !anom

      p <- plotly::plot_ly(type = "scatter")
      # 1. Prediction harmonique (ligne bleue pleine).
      p <- plotly::add_trace(
        p, x = obs_date, y = as.numeric(ts$crswir_pred),
        name = i18n$t("monitoring_fordead_pixel_predicted"),
        mode = "lines", line = list(color = col_pred, width = 1.8),
        hovertemplate = paste0(
          "<b>", i18n$t("monitoring_fordead_pixel_predicted"), "</b><br>",
          "%{x|%Y-%m-%d}<br>", veg_index, " = %{y:.3f}<extra></extra>"))
      # 2. Seuil de detection (= predit + Delta) - ligne bleue pointillee.
      if ("seuil_haut" %in% names(ts)) {
        p <- plotly::add_trace(
          p, x = obs_date, y = as.numeric(ts$seuil_haut),
          name = i18n$t("monitoring_fordead_pixel_threshold"),
          mode = "lines", line = list(color = col_limit, width = 1.4,
                                       dash = "dash"),
          hovertemplate = paste0(
            "<b>", i18n$t("monitoring_fordead_pixel_threshold"), "</b><br>",
            "%{x|%Y-%m-%d}<br>", veg_index, " = %{y:.3f}<extra></extra>"))
      }
      # 3-6. Points categorises (marqueurs " x "), traces du moins au plus
      # critique pour que les confirmes ressortent au-dessus.
      .add_cat <- function(p, sel, key, color) {
        rows <- which(sel)
        if (!length(rows)) return(p)
        plotly::add_trace(
          p, x = obs_date[rows], y = obs_val[rows],
          name = i18n$t(key), mode = "markers",
          marker = list(color = color, size = 7, symbol = "x"),
          hovertemplate = paste0(
            "<b>", i18n$t(key), "</b><br>",
            "%{x|%Y-%m-%d}<br>", veg_index, " = %{y:.3f}<extra></extra>"))
      }
      p <- .add_cat(p, is_train,     "monitoring_fordead_pixel_training",  col_training)
      p <- .add_cat(p, is_healthy,   "monitoring_fordead_pixel_healthy",   col_healthy)
      p <- .add_cat(p, is_anomaly,   "monitoring_fordead_pixel_anomaly",   col_anomaly)
      p <- .add_cat(p, is_confirmed, "monitoring_fordead_pixel_confirmed", col_confirmed)

      shapes <- list()
      annotations <- list()
      if (!is.null(premiere) && length(premiere) == 1L &&
          inherits(premiere, "Date") && !is.na(premiere)) {
        # Marqueur vertical sur la date de 1re anomalie detectee.
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
          font = list(color = "#000000", size = 15)
        ))
      }
      # v0.72.0 - Annotation discrete en haut a gauche si le pixel
      # est hors zone de validite de l'essence dominante (calibration
      # FORDEAD non-applicable strictement). Visible quand
      # `attr(ts, "dans_zone_validite") == FALSE`.
      if (!is.null(in_validity) && length(in_validity) == 1L &&
          !is.na(in_validity) && isFALSE(in_validity)) {
        annotations <- c(annotations, list(list(
          xref = "paper", x = 0, xanchor = "left",
          yref = "paper", y = 1, yanchor = "top",
          text = i18n$t("monitoring_fordead_pixel_outside_validity"),
          showarrow = FALSE,
          font = list(color = "#FF7F0E", size = 15),
          bgcolor = "rgba(255, 255, 220, 0.85)",
          borderpad = 4
        )))
      }

      p <- plotly::layout(
        p,
        # Police globale agrandie : axes, ticks, legende et hover heritent de
        # cette taille, lisibles en plein ecran (spec UX). Les annotations
        # in-plot (1re anomalie, hors zone de validite) fixent leur taille.
        font   = list(size = 16),
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
        # Titre + bouton " plein ecran " ancre en HAUT A DROITE (parite
        # exacte avec la Carte FAST, mod_monitoring_pixel_map) : un petit JS
        # bascule la classe BS5 `.modal-fullscreen` sur la `.modal-dialog`
        # la plus proche - bord a bord, sans aller-retour serveur. Le plot
        # remplit la zone (height 100% + plotly `responsive`) et grandit en
        # plein ecran via la regle CSS ci-dessous.
        title = htmltools::tagList(
          htmltools::span(sprintf(
            i18n$t("monitoring_fordead_pixel_modal_title_fmt"),
            round(lat, 5), round(lng, 5)
          )),
          htmltools::tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-secondary",
            style = paste("position: absolute; top: 0.75rem;",
                          "right: 0.75rem; z-index: 2;"),
            title = i18n$t("monitoring_pixel_map_fullscreen"),
            # Toggle plein ecran + `resize` differe : plotly (responsive)
            # n'ecoute que window.resize ; sans cet evenement, le graphe
            # garde sa taille initiale et ne remplit pas l'ecran agrandi.
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
        htmltools::div(
          class = "pixel-ts-wrap",
          style = "height: 320px;",
          plotly::plotlyOutput(session$ns("pixel_ts_plot"), height = "100%")
        )
      ))
      # `responsive = TRUE` pour que le graphe se redimensionne quand le
      # modal passe en plein ecran (le bouton dispatch un event `resize`).
      output$pixel_ts_plot <- plotly::renderPlotly(
        plotly::config(p, responsive = TRUE))
      })  # fin shiny::isolate (calcul differe hors contexte reactif)
      }, once = TRUE)  # fin session$onFlushed
    })

    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)

    invisible(list(mask = mask_r, date_domain = date_domain_r))
  })
}

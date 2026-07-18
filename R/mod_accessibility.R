# ===========================================================================
# Module — Accessibilité forestière (ForêtAccess), sous-onglet de « Terrain »
# ===========================================================================
#
# Présentation Shiny du service `R/service_accessibility.R` (adaptateur autour
# de `foretaccess`). Aucune logique métier ici (règle 2) : le module orchestre
# l'UI, l'exécution asynchrone (worker `future`) et le rendu carte/tableau.
#
# Premier incrément : moteurs TERRESTRES (skidder, porteur, camion DFCI). Le
# calcul est long → `ExtendedTask` + `future_promise`, même patron que le moteur
# reGénération (notif persistante bas-droite avec chrono, retour immédiat).

#' Colours for a categorical accessibility raster's levels
#'
#' Prefers the raster's own colour table (`terra::coltab`) when it carries a
#' meaningful one — e.g. the `classes_debardage` raster ships a Sylvaccess
#' green→red distance ramp. Falls back to a fixed qualitative palette (cycled)
#' when there is no usable colour table. Returns one colour per `codes` entry,
#' as `#RRGGBB` (fallback) or `#RRGGBBAA` (from the colour table; `…00` alpha
#' means transparent, e.g. `hors_foret`).
#' @noRd
.acc_level_colors <- function(rast, codes) {
  pal <- c("#2E7D32", "#9CCC65", "#FDD835", "#FB8C00", "#C62828",
           "#6D4C41", "#9E9E9E", "#455A64")
  ct <- tryCatch(terra::coltab(rast)[[1]], error = function(e) NULL)
  if (is.data.frame(ct) && all(c("red", "green", "blue") %in% names(ct))) {
    # terra nomme la 1re colonne « values » (ou « value » selon la version) : on
    # prend la colonne d'index par position pour être robuste.
    idx <- match(codes, ct[[1]])
    a <- if ("alpha" %in% names(ct)) ct$alpha[idx] else rep(255L, length(idx))
    a[is.na(a)] <- 255L
    hex <- grDevices::rgb(ct$red[idx], ct$green[idx], ct$blue[idx],
                          alpha = a, maxColorValue = 255)
    # Ignorer une coltab dégénérée (terra en pose parfois une toute noire).
    if (length(unique(hex[!is.na(hex)])) > 1L) return(hex)
  }
  pal[((seq_along(codes) - 1L) %% length(pal)) + 1L]
}

#' Human-friendly legend labels for accessibility classes
#'
#' Maps the raw `foretaccess` class names to display labels. The DFCI
#' `defendable_cN` classes become **distance bands** read from
#' `foretaccess_config()$dfci$classes_distance_m` (e.g. « 0 à 120 m défendable »),
#' so the legend shows the actual defence distances instead of opaque C1/C2/C3
#' codes. The other DFCI classes get plain-language labels. Unknown labels pass
#' through unchanged (skidder/forwarder classes, skidding-distance bands already
#' expressed as ranges).
#' @noRd
.acc_legend_labels <- function(labs, i18n) {
  b <- tryCatch(foretaccess::foretaccess_config()$dfci$classes_distance_m,
                error = function(e) c(0, 120, 280, 440))
  dfci_band <- function(n) {
    if (length(b) < n + 1L) return(NA_character_)
    sprintf(i18n$t("acc_dfci_defendable_fmt"), as.integer(b[n]), as.integer(b[n + 1L]))
  }
  map <- c(
    inaccessible         = i18n$t("acc_dfci_inaccessible"),
    non_defendable_pente = i18n$t("acc_dfci_non_defendable_pente"),
    defendable_c1        = dfci_band(1L),
    defendable_c2        = dfci_band(2L),
    defendable_c3        = dfci_band(3L))
  out <- unname(map[labs])
  out[is.na(out)] <- labs[is.na(out)]
  out
}

#' @noRd
mod_accessibility_ui <- function(id) {
  ns <- shiny::NS(id)
  i18n <- get_i18n(get_app_options()$language %||% "fr")

  bslib::layout_sidebar(
    # Barre latérale GAUCHE : sélection des moteurs, lancement, export. Ce sont
    # les commandes du CALCUL.
    sidebar = bslib::sidebar(
      width = 320, open = "always", position = "left",
      htmltools::tags$p(class = "text-muted small", i18n$t("acc_intro")),

      shiny::checkboxGroupInput(
        ns("engines"), i18n$t("acc_engines_label"),
        choices = stats::setNames(
          ACCESSIBILITY_ENGINES,
          c(i18n$t("acc_engine_skidder"),
            i18n$t("acc_engine_porteur"),
            i18n$t("acc_engine_dfci"))),
        selected = ACCESSIBILITY_ENGINES),

      bslib::input_task_button(
        ns("run"), i18n$t("acc_run"),
        label_busy = i18n$t("acc_running"),
        icon = bsicons::bs_icon("play-fill"),
        type = "primary", class = "w-100 mb-3"),
      # Roue dentée + chrono SOUS le bouton (parité FAST/FORDEAD/RECONFORT) : le
      # run peut durer, le toast bas-droite peut être manqué/fermé.
      shiny::uiOutput(ns("run_status"))
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(i18n$t("acc_map_title")),
      # Barre latérale DROITE (contre la carte) : les résultats et leur AFFICHAGE
      # — sélecteur de raster calculé, zone tampon et opacité.
      bslib::layout_sidebar(
        fillable = TRUE,
        sidebar = bslib::sidebar(
          position = "right", open = "always", width = 280,
          htmltools::tags$strong(i18n$t("acc_layer_label")),
          shiny::uiOutput(ns("layer_ui")),
          htmltools::tags$hr(class = "my-2"),
          shiny::numericInput(
            ns("buffer_km"), i18n$t("acc_buffer"),
            value = 1, min = 0, max = 20, step = 1),
          htmltools::tags$p(class = "text-muted small", i18n$t("acc_buffer_help")),
          shiny::sliderInput(
            ns("opacity"), i18n$t("acc_opacity"),
            min = 0, max = 1, value = 0.7, step = 0.05, ticks = FALSE),
          htmltools::tags$hr(class = "my-2"),
          # Exports regroupés dans un accordéon repliable « Exports » (replié par
          # défaut), même présentation que l'onglet reGénération.
          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              title = i18n$t("action_plan_section_exports"),
              icon = bsicons::bs_icon("box-arrow-up"),
              shiny::downloadButton(
                ns("export_gpkg"), i18n$t("acc_download_gpkg"),
                icon = shiny::icon("database"),
                class = "btn-outline-success btn-sm w-100")))
        ),
        # Badge de provenance DFCI (au-dessus de la carte) : n'apparaît que
        # lorsque la couche « Camion DFCI » est affichée.
        shiny::uiOutput(ns("dfci_badge")),
        leaflet::leafletOutput(ns("map"), height = "72vh")
      )
    )
  )
}

#' @noRd
mod_accessibility_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    i18n <- get_i18n(get_app_options()$language %||% "fr")

    rv <- shiny::reactiveValues(result = NULL, running = FALSE, start = NULL)

    # Chemin du paquet en dev (pkgload) : rejoué dans le worker pour disposer des
    # fonctions internes `run_accessibility()`.
    .dev_pkg_path <- tryCatch(
      if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
        find.package("nemetonshiny") else NULL,
      error = function(e) NULL)

    # AOI (forêt) du projet en EPSG:2154 — repli indicators_sf -> UGF -> parcelles.
    units_sf <- shiny::reactive({
      .resolve_accessibility_aoi(app_state$current_project)
    })

    # --- Worker asynchrone : acquisition desserte + prétraitement + moteurs -----
    acc_task <- shiny::ExtendedTask$new(
      function(aoi_path, engines, cache_dir, buffer_m, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          plan_classes <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
            future::plan("multisession")
          }
        }
        promises::future_promise({
          on.exit(nemetonshiny:::.release_worker_memory(), add = TRUE)
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          nemetonshiny:::run_accessibility(aoi_path, engines, cache_dir, buffer_m)
        }, seed = TRUE)
      })

    busy <- shiny::reactive(identical(acc_task$status(), "running"))

    # Lie le bouton à la tâche : sans ce binding, bslib remet `input_task_button`
    # à l'état « ready » au flush réactif suivant le clic — le bouton ne reste
    # donc PAS grisé pendant tout le calcul async. Avec le binding, il affiche le
    # spinner + libellé « busy » tant que la tâche tourne (comme dans reGénération).
    bslib::bind_task_button(acc_task, "run")

    # --- Lancement -------------------------------------------------------------
    shiny::observeEvent(input$run, {
      if (isTRUE(rv$running)) {
        shiny::showNotification(i18n$t("acc_busy_already"), type = "warning",
                                duration = 5)
        return()
      }
      if (deny_if_readonly(app_state, i18n)) {
        bslib::update_task_button("run", state = "ready")
        return()
      }
      project_path <- tryCatch(app_state$current_project$path,
                               error = function(e) NULL)
      aoi <- units_sf()
      if (is.null(aoi) || is.null(project_path)) {
        bslib::update_task_button("run", state = "ready")
        shiny::showNotification(i18n$t("acc_need_project"), type = "warning")
        return()
      }
      engines <- intersect(input$engines %||% character(0), ACCESSIBILITY_ENGINES)
      if (length(engines) == 0L) {
        bslib::update_task_button("run", state = "ready")
        shiny::showNotification(i18n$t("acc_need_engine"), type = "warning")
        return()
      }
      # L'AOI est passée au worker `future` PAR FICHIER, jamais comme `sf` vivant :
      # une géométrie sf peut porter un pointeur externe qui casse la
      # sérialisation inter-process ("external pointer is not valid"). On l'écrit
      # ici (process principal, pointeur valide) ; le worker la relit.
      cache_dir <- .accessibility_cache_dir(project_path)
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      aoi_path <- file.path(cache_dir, "aoi_input.gpkg")
      ok <- tryCatch({
        sf::st_write(aoi, aoi_path, layer = "foret", quiet = TRUE,
                     delete_dsn = TRUE)
        TRUE
      }, error = function(e) FALSE)
      if (!isTRUE(ok)) {
        bslib::update_task_button("run", state = "ready")
        shiny::showNotification(i18n$t("acc_need_project"), type = "warning")
        return()
      }

      rv$running <- TRUE
      rv$start <- Sys.time()
      shiny::showNotification(
        .running_notif_content(i18n$t("acc_running"), rv$start),
        id = session$ns("acc_notif"), type = "message", duration = NULL)
      # Garde-fou : un échec SYNCHRONE d'invoke (sérialisation d'un argument) ne
      # doit pas laisser le bouton figé « busy » ni la notif collée.
      buffer_m <- max(0, (suppressWarnings(as.numeric(input$buffer_km)) %||% 1)) * 1000
      tryCatch(
        acc_task$invoke(aoi_path, engines, cache_dir, buffer_m,
                        .dev_pkg_path, get_app_options()),
        error = function(e) {
          rv$running <- FALSE
          rv$start <- NULL
          shiny::removeNotification(session$ns("acc_notif"))
          bslib::update_task_button("run", state = "ready")
          shiny::showNotification(
            paste0(i18n$t("accessibility_engine_failed"), " — ",
                   .strip_ansi(conditionMessage(e))),
            type = "error", duration = NULL)
        })
    })

    # Rafraîchit le chrono de la notif persistante tant que le worker tourne.
    shiny::observe({
      if (!isTRUE(rv$running)) return()
      shiny::invalidateLater(1000)
      shiny::showNotification(
        .running_notif_content(i18n$t("acc_running"), shiny::isolate(rv$start)),
        id = session$ns("acc_notif"), type = "message", duration = NULL)
    })

    # Message inline SOUS le bouton (roue dentée + chrono MM:SS), parité
    # FAST/FORDEAD/RECONFORT. Même source (`rv$start`) que le toast bas-droite :
    # disparaît tout seul en fin de run (rv$start remis à NULL). suspendWhenHidden
    # = FALSE pour que le chrono continue si l'onglet n'est pas au premier plan.
    output$run_status <- shiny::renderUI({
      if (!isTRUE(rv$running) || is.null(rv$start)) return(NULL)
      shiny::invalidateLater(1000)
      htmltools::div(
        class = "small text-info mt-1 text-center",
        .running_notif_content(i18n$t("acc_running"), rv$start))
    })
    shiny::outputOptions(output, "run_status", suspendWhenHidden = FALSE)

    # --- Fin de tâche ----------------------------------------------------------
    shiny::observeEvent(acc_task$status(), {
      st <- acc_task$status()
      if (!identical(st, "success") && !identical(st, "error")) return()
      rv$running <- FALSE
      rv$start <- NULL
      shiny::removeNotification(session$ns("acc_notif"))

      res <- tryCatch(acc_task$result(), error = function(e) {
        list(status = "error", reason = "accessibility_engine_failed",
             detail = conditionMessage(e))
      })
      if (!is.list(res) || !identical(res$status, "success")) {
        reason <- tryCatch(res$reason, error = function(e) NULL) %||%
          "accessibility_engine_failed"
        msg <- i18n$t(reason)
        detail <- tryCatch(res$detail, error = function(e) NULL)
        if (!is.null(detail) && nzchar(detail)) {
          msg <- paste0(msg, " — ", .strip_ansi(as.character(detail)))
        }
        shiny::showNotification(msg, type = "error", duration = NULL)
        return()
      }
      # Recharger depuis le cache disque : le sélecteur de couche liste ainsi
      # TOUS les rasters déjà calculés du projet (tous moteurs + classes de
      # débardage), pas seulement ceux du run courant. Un run partiel (ex.
      # « porteur » seul) n'efface donc plus l'affichage des couches calculées
      # précédemment — elles restent sélectionnables. Repli sur `res` si le
      # rechargement échoue.
      project_path <- tryCatch(app_state$current_project$path,
                               error = function(e) NULL)
      # Recharge l'union des rasters du cache, mais conserve la provenance DFCI
      # du run courant (`dfci_source`, non persistée sur disque) pour le badge.
      cached <- .load_cached_accessibility(project_path)
      if (!is.null(cached)) cached$dfci_source <- res$dfci_source
      rv$result <- cached %||% res
      shiny::showNotification(
        sprintf(i18n$t("acc_done_fmt"), length(res$engines)),
        type = "message", duration = 6)
    })

    # Au changement de projet (et au montage de l'onglet), restaure les rasters
    # DÉJÀ calculés depuis le cache disque du projet, s'il y en a — sinon repart
    # d'un état vide. Évite d'afficher un run périmé d'un autre projet et permet
    # de retrouver une analyse récente sans relancer le calcul.
    shiny::observeEvent(app_state$current_project, {
      project_path <- tryCatch(app_state$current_project$path,
                               error = function(e) NULL)
      cached <- tryCatch(.load_cached_accessibility(project_path),
                         error = function(e) NULL)
      rv$result <- cached
      if (!is.null(cached)) {
        shiny::showNotification(
          sprintf(i18n$t("acc_cache_loaded_fmt"), length(cached$raster_paths)),
          type = "message", duration = 5)
      }
    }, ignoreNULL = FALSE)

    # --- Sélecteur de couche (raster affiché) : rendu après un run -------------
    # Les choix sont les rasters disponibles : un par moteur (leurs classes
    # d'accessibilité) + « Classes de débardage » (bandes de distance Sylvaccess)
    # quand le skidder a tourné.
    output$layer_ui <- shiny::renderUI({
      res <- rv$result
      layers <- if (is.null(res)) NULL else names(res$raster_paths)
      if (is.null(layers) || length(layers) == 0L) {
        return(htmltools::tags$p(class = "text-muted small",
                                 i18n$t("acc_no_result_yet")))
      }
      lyr_label <- c(
        skidder = i18n$t("acc_engine_skidder"),
        porteur = i18n$t("acc_engine_porteur"),
        camion_dfci = i18n$t("acc_engine_dfci"),
        classes_debardage = i18n$t("acc_layer_debardage"))
      labs <- unname(lyr_label[layers])
      labs[is.na(labs)] <- layers[is.na(labs)]
      shiny::radioButtons(
        ns("layer"), NULL,
        choices = stats::setNames(layers, labs),
        selected = layers[[1]])
    })

    # --- Badge de provenance DFCI (au-dessus de la carte) ----------------------
    # N'apparaît que si la couche « Camion DFCI » est affichée. Avertissement
    # (jaune) quand les sources DFCI sont estimées par l'heuristique app
    # (aucune desserte taguée ref:FR:DFCI, ni repli géométrique) ; info (bleu)
    # quand le vrai réseau OSM ref:FR:DFCI a servi.
    output$dfci_badge <- shiny::renderUI({
      res <- rv$result
      layer <- input$layer %||%
        (if (!is.null(res)) names(res$raster_paths)[[1]] else NULL)
      if (is.null(res) || !identical(layer, "camion_dfci")) return(NULL)
      src <- tryCatch(res$dfci_source, error = function(e) NULL)
      if (identical(src, "heuristique")) {
        shiny::div(class = "alert alert-warning acc-dfci-badge py-2 mb-2 small",
          role = "status",
          shiny::icon("triangle-exclamation"), " ",
          i18n$t("acc_dfci_heuristic_badge"))
      } else if (identical(src, "osm")) {
        shiny::div(class = "alert alert-info acc-dfci-badge py-2 mb-2 small",
          role = "status",
          shiny::icon("circle-info"), " ", i18n$t("acc_dfci_osm_badge"))
      } else {
        NULL
      }
    })

    # --- Carte : fonds + UGF + raster de classes en overlay --------------------
    # Même patron que la Carte « Alertes FAST » (mod_monitoring_fast_alerts) : la
    # carte de base (tuiles OSM/Satellite + UGF + fitBounds) est rendue UNE seule
    # fois ; le raster est ajouté/mis à jour via leafletProxy dans l'observe plus
    # bas. Le raster vit dans un MAP PANE dédié (zIndex fixe) : sans lui, changer
    # de fond (OSM ↔ Satellite) fait disparaître l'image raster et perd le zoom /
    # l'opacité. Le groupe « Accessibilite » est enregistré dans le LayersControl
    # dès le rendu (l'utilisateur peut le décocher).
    output$map <- leaflet::renderLeaflet({
      aoi <- units_sf()
      geo <- if (!is.null(aoi)) {
        tryCatch(sf::st_transform(aoi, 4326), error = function(e) NULL)
      }
      # « Desserte » (routes/pistes DFCI ayant servi au calcul) est enregistrée
      # dans le LayersControl SOUS « UGF » et « Accessibilite » ; l'observe plus
      # bas la dessine via leafletProxy (dépend du run, pas de la carte de base).
      overlays <- c(if (!is.null(geo)) "UGF" else NULL, "Accessibilite", "Desserte")
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonAccRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          overlayGroups = overlays,
          options = leaflet::layersControlOptions(collapsed = TRUE))
      if (!is.null(geo)) {
        m <- leaflet::addPolygons(m, data = geo, group = "UGF",
          color = "#1f78b4", weight = 2, opacity = 0.9, fillOpacity = 0)
        bb <- tryCatch(as.numeric(sf::st_bbox(geo)), error = function(e) NULL)
        if (!is.null(bb) && all(is.finite(bb))) {
          m <- leaflet::fitBounds(m, bb[1], bb[2], bb[3], bb[4])
        }
      }
      m
    })
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)

    # Opacité du raster affiché : débouncée pour ne pas redessiner à chaque tick
    # du slider pendant un glissement.
    opacity_d <- shiny::debounce(
      shiny::reactive(suppressWarnings(as.numeric(input$opacity)) %||% 0.7), 250)

    # Overlay du raster de classes via leafletProxy : préserve le zoom et le fond
    # sélectionné (OSM/Satellite). Le raster est peint dans le pane dédié
    # `nemetonAccRaster` (cf. renderLeaflet) — c'est ce qui le rend stable au
    # changement de fond. `method = "ngb"` : pas d'interpolation des codes de
    # classe (raster catégoriel).
    shiny::observe({
      res <- rv$result
      first_layer <- if (!is.null(res)) names(res$raster_paths)[[1]] else NULL
      layer <- input$layer %||% first_layer
      op <- opacity_d()
      shown <- input$map_groups   # groupes overlay cochés côté client
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup("Accessibilite") |>
        leaflet::removeControl("acc_legend")
      if (is.null(res) || is.null(layer)) return()
      rp <- tryCatch(res$raster_paths[[layer]], error = function(e) NULL)
      if (is.null(rp) || !file.exists(rp)) return()
      rast <- tryCatch(terra::rast(rp), error = function(e) NULL)
      if (is.null(rast)) return()

      # Raster catégoriel : couleurs via la coltab du raster (ex. rampe Sylvaccess
      # des classes de débardage) ou palette de repli. Légende sans les classes
      # transparentes (alpha 00, ex. hors_foret).
      lv <- tryCatch(terra::levels(rast)[[1]], error = function(e) NULL)
      if (is.data.frame(lv) && nrow(lv) > 0L) {
        codes <- as.numeric(lv[[1]]); labs <- as.character(lv[[2]])
        cols <- .acc_level_colors(rast, codes)
        # « hors_foret » TOUJOURS transparent, quel que soit le raster. On NE se
        # fie PAS à l'alpha de la palette : `colorFactor` + `addRasterImage`
        # perdent le canal alpha d'un `#RRGGBBAA` (d'où l'ancien blanc opaque).
        # Fix robuste : masquer les cellules hors_foret à NA dans le raster —
        # elles passent alors par `na.color = "transparent"` (garanti). Le code
        # est repéré par le LABEL (variable : 4 chez skidder/porteur, 9 chez les
        # classes de débardage), jamais en dur. Le filtre `keep` retire la classe
        # de la légende (on garde l'alpha 00 sur `cols` pour ce filtre).
        hf <- !is.na(labs) & labs == "hors_foret"
        if (any(hf)) cols[hf] <- "#FFFFFF00"
        code_hf <- codes[hf]
        if (length(code_hf)) {
          # `terra::subst` échoue sur un raster catégoriel (il matche les libellés,
          # pas les codes). `ifel(rast %in% code, NA, …)` masque de façon fiable.
          rast <- tryCatch(terra::ifel(rast %in% code_hf, NA, rast),
                           error = function(e) rast)
        }
        cmap <- leaflet::colorFactor(cols, domain = codes, na.color = "transparent")
        keep <- !is.na(cols) & substr(cols, 8L, 9L) != "00"
        proxy |>
          leaflet::addRasterImage(rast, colors = cmap, opacity = op,
            method = "ngb", group = "Accessibilite",
            options = leaflet::gridOptions(pane = "nemetonAccRaster")) |>
          leaflet::addLegend("bottomright", colors = cols[keep],
            labels = .acc_legend_labels(labs[keep], i18n),
            title = i18n$t("acc_legend_title"), layerId = "acc_legend",
            opacity = 0.8)
      } else {
        proxy |> leaflet::addRasterImage(rast, opacity = op, method = "ngb",
          group = "Accessibilite",
          options = leaflet::gridOptions(pane = "nemetonAccRaster"))
      }
      # Respecter la décoche du groupe « Accessibilite » après re-dessin proxy.
      if (!is.null(shown) && !("Accessibilite" %in% shown)) {
        leaflet::hideGroup(proxy, "Accessibilite")
      }
    })

    # Overlay « Desserte » : les routes/pistes (sources DFCI) qui ont servi au
    # calcul, lues depuis la couche `desserte` du GeoPackage du run. Dépend de
    # `rv$result` uniquement (pas de l'opacité ni du raster choisi) → observe
    # dédié. Polylignes colorées par classe (route/piste), au-dessus du raster.
    shiny::observe({
      res <- rv$result
      shown <- input$map_groups
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup("Desserte")
      gp <- tryCatch(res$gpkg_path, error = function(e) NULL)
      if (is.null(gp) || !file.exists(gp)) return()
      d <- tryCatch(sf::st_read(gp, layer = "desserte", quiet = TRUE),
                    error = function(e) NULL)
      if (!inherits(d, "sf") || nrow(d) == 0L) return()
      d <- tryCatch(sf::st_transform(d, 4326), error = function(e) d)
      cl <- tolower(as.character(d[["classe"]] %||% rep("", nrow(d))))
      cols <- ifelse(cl == "piste", "#8D6E63", "#37474F")   # piste brun / route gris
      proxy |>
        leaflet::addPolylines(data = d, group = "Desserte",
          color = cols, weight = 2, opacity = 0.9,
          label = ~ as.character(classe))
      # Respecter la décoche du groupe « Desserte » après re-dessin proxy.
      if (!is.null(shown) && !("Desserte" %in% shown)) {
        leaflet::hideGroup(proxy, "Desserte")
      }
    })

    # --- Export GeoPackage -----------------------------------------------------
    output$export_gpkg <- shiny::downloadHandler(
      filename = function() {
        paste0(.project_export_slug(app_state$current_project, "nemeton"),
               "_accessibilite.gpkg")
      },
      content = function(file) {
        on.exit(session$sendCustomMessage("nemetonHideDownloadToast", list()),
                add = TRUE)
        res <- rv$result
        if (is.null(res) || !isTRUE(export_accessibility_geopackage(res, file))) {
          shiny::showNotification(i18n$t("acc_export_empty"), type = "warning")
          if (!file.exists(file)) writeLines("No data available", file)
        }
      }
    )
  })
}

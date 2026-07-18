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

      htmltools::tags$h6(class = "mt-3", i18n$t("action_plan_section_exports")),
      shiny::downloadButton(
        ns("export_gpkg"), i18n$t("acc_download_gpkg"),
        icon = shiny::icon("database"),
        class = "btn-outline-success btn-sm w-100")
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
            value = 5, min = 0, max = 20, step = 1),
          htmltools::tags$p(class = "text-muted small", i18n$t("acc_buffer_help")),
          shiny::sliderInput(
            ns("opacity"), i18n$t("acc_opacity"),
            min = 0, max = 1, value = 0.7, step = 0.05, ticks = FALSE)
        ),
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
      buffer_m <- max(0, (suppressWarnings(as.numeric(input$buffer_km)) %||% 5)) * 1000
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
      rv$result <- res
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

    # --- Carte : contour AOI de base + raster de classes en overlay ------------
    output$map <- leaflet::renderLeaflet({
      aoi <- units_sf()
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          overlayGroups = c("AOI", "Accessibilite"),
          options = leaflet::layersControlOptions(collapsed = TRUE))
      if (!is.null(aoi)) {
        geo <- tryCatch(sf::st_transform(aoi, 4326), error = function(e) aoi)
        m <- leaflet::addPolygons(m, data = geo, group = "AOI", weight = 1,
          color = "#1B6B1B", fillColor = "#1B6B1B", fillOpacity = 0.05)
        bb <- tryCatch(as.numeric(sf::st_bbox(geo)), error = function(e) NULL)
        if (!is.null(bb) && all(is.finite(bb))) {
          m <- leaflet::fitBounds(m, bb[1], bb[2], bb[3], bb[4])
        }
      }
      m
    })

    # Opacité du raster affiché : débouncée pour ne pas redessiner à chaque tick
    # du slider pendant un glissement.
    opacity_d <- shiny::debounce(
      shiny::reactive(suppressWarnings(as.numeric(input$opacity)) %||% 0.7), 250)

    # Overlay du raster de classes choisi, via proxy (préserve zoom).
    shiny::observe({
      res <- rv$result
      first_layer <- if (!is.null(res)) names(res$raster_paths)[[1]] else NULL
      layer <- input$layer %||% first_layer
      op <- opacity_d()
      proxy <- leaflet::leafletProxy("map")
      leaflet::clearGroup(proxy, "Accessibilite")
      leaflet::removeControl(proxy, "acc_legend")
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
        cmap <- leaflet::colorFactor(cols, domain = codes, na.color = "transparent")
        keep <- !is.na(cols) & substr(cols, 8L, 9L) != "00"
        proxy |>
          leaflet::addRasterImage(rast, colors = cmap, opacity = op,
            project = TRUE, group = "Accessibilite") |>
          leaflet::addLegend("bottomright", colors = cols[keep], labels = labs[keep],
            title = i18n$t("acc_legend_title"), layerId = "acc_legend",
            opacity = 0.8)
      } else {
        proxy |> leaflet::addRasterImage(rast, opacity = op, project = TRUE,
          group = "Accessibilite")
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

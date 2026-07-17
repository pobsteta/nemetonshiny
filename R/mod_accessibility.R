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

#' @noRd
mod_accessibility_ui <- function(id) {
  ns <- shiny::NS(id)
  i18n <- get_i18n(get_app_options()$language %||% "fr")

  bslib::layout_sidebar(
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

      htmltools::tags$hr(class = "my-2"),
      shiny::uiOutput(ns("layer_ui")),

      htmltools::tags$h6(class = "mt-3", i18n$t("action_plan_section_exports")),
      shiny::downloadButton(
        ns("export_gpkg"), i18n$t("acc_download_gpkg"),
        icon = shiny::icon("database"),
        class = "btn-outline-success btn-sm w-100")
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(i18n$t("acc_map_title")),
      leaflet::leafletOutput(ns("map"), height = 440)
    ),
    bslib::card(
      bslib::card_header(i18n$t("acc_recap_title")),
      DT::dataTableOutput(ns("recap"))
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
      function(aoi_path, engines, cache_dir, dev_path, app_opts) {
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
          nemetonshiny:::run_accessibility(aoi_path, engines, cache_dir)
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
      tryCatch(
        acc_task$invoke(aoi_path, engines, cache_dir,
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

    # Purge du résultat au changement de projet (évite d'afficher un run périmé).
    shiny::observeEvent(app_state$current_project, {
      rv$result <- NULL
    }, ignoreNULL = FALSE)

    # --- Sélecteur de couche (moteur affiché) : rendu après un run -------------
    output$layer_ui <- shiny::renderUI({
      res <- rv$result
      if (is.null(res) || length(res$engines) == 0L) return(NULL)
      eng_label <- c(
        skidder = i18n$t("acc_engine_skidder"),
        porteur = i18n$t("acc_engine_porteur"),
        camion_dfci = i18n$t("acc_engine_dfci"))
      shiny::radioButtons(
        ns("layer"), i18n$t("acc_layer_label"),
        choices = stats::setNames(res$engines, eng_label[res$engines]),
        selected = res$engines[[1]])
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

    # Overlay du raster de classes du moteur choisi, via proxy (préserve zoom).
    shiny::observe({
      res <- rv$result
      layer <- input$layer %||% (if (!is.null(res)) res$engines[[1]] else NULL)
      proxy <- leaflet::leafletProxy("map")
      leaflet::clearGroup(proxy, "Accessibilite")
      leaflet::removeControl(proxy, "acc_legend")
      if (is.null(res) || is.null(layer)) return()
      rp <- tryCatch(res$raster_paths[[layer]], error = function(e) NULL)
      if (is.null(rp) || !file.exists(rp)) return()
      rast <- tryCatch(terra::rast(rp), error = function(e) NULL)
      if (is.null(rast)) return()

      # Raster catégoriel : palette par niveau + légende étiquetée.
      lv <- tryCatch(terra::levels(rast)[[1]], error = function(e) NULL)
      pal <- c("#2E7D32", "#9CCC65", "#FDD835", "#FB8C00", "#C62828",
               "#6D4C41", "#9E9E9E", "#455A64")
      if (is.data.frame(lv) && nrow(lv) > 0L) {
        codes <- as.numeric(lv[[1]]); labs <- as.character(lv[[2]])
        cols <- pal[((seq_along(codes) - 1L) %% length(pal)) + 1L]
        cmap <- leaflet::colorFactor(cols, domain = codes, na.color = "transparent")
        proxy |>
          leaflet::addRasterImage(rast, colors = cmap, opacity = 0.7,
            project = TRUE, group = "Accessibilite") |>
          leaflet::addLegend("bottomright", colors = cols, labels = labs,
            title = i18n$t("acc_legend_title"), layerId = "acc_legend",
            opacity = 0.8)
      } else {
        proxy |> leaflet::addRasterImage(rast, opacity = 0.7, project = TRUE,
          group = "Accessibilite")
      }
    })

    # --- Tableau récapitulatif (ha par classe et par moteur) -------------------
    output$recap <- DT::renderDataTable({
      res <- rv$result
      df <- if (is.null(res)) NULL else .accessibility_recap_table(res$recaps, i18n)
      if (is.null(df)) {
        return(DT::datatable(
          data.frame(x = i18n$t("acc_recap_empty")),
          rownames = FALSE, colnames = "",
          options = list(dom = "t", ordering = FALSE)))
      }
      DT::datatable(df, rownames = FALSE,
        options = list(dom = "t", paging = FALSE, ordering = FALSE))
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

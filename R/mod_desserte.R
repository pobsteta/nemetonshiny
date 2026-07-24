# ===========================================================================
# Module — Création de desserte forestière (ForêtAccess), sous-onglet Terrain
# ===========================================================================
#
# Présentation Shiny du service `R/service_desserte.R` (adaptateur autour des
# moteurs de création de réseau de `foretaccess`). Aucune logique métier (règle
# 2) : le module orchestre l'UI, l'exécution asynchrone (worker `future`) et le
# rendu carte/badges.
#
# v1 : moteur GLOUTON seul, OPT-IN (calcul ~11,5 min sur 30 parcelles). Même
# patron que reGénération / Accessibilité : `ExtendedTask` + `future_promise`,
# notif persistante bas-droite avec chrono, retour immédiat. Le réseau créé est
# affiché en overlay RASTER (léger) ; les lignes vectorielles détaillées partent
# à l'export GeoPackage.

#' @noRd
mod_desserte_ui <- function(id) {
  ns <- shiny::NS(id)
  i18n <- get_i18n(get_app_options()$language %||% "fr")

  bslib::layout_sidebar(
    # Barre latérale GAUCHE : commandes du CALCUL.
    sidebar = bslib::sidebar(
      width = 320, open = "always", position = "left",
      htmltools::tags$p(class = "text-muted small", i18n$t("dess_intro")),

      shiny::radioButtons(
        ns("engine"), i18n$t("dess_engine_label"),
        choices = stats::setNames(DESSERTE_ENGINES, i18n$t("dess_engine_glouton")),
        selected = DESSERTE_ENGINES[[1]]),
      # Avertissement « calcul long » (parité câble) : le glouton trace une
      # route par parcelle, le temps croît avec le nombre de parcelles.
      htmltools::div(
        class = "alert alert-warning py-2 small",
        shiny::icon("triangle-exclamation"), " ", i18n$t("dess_slow_help")),

      shiny::numericInput(
        ns("buffer_km"), i18n$t("dess_buffer"),
        value = 1, min = 0, max = 20, step = 1),
      htmltools::tags$p(class = "text-muted small", i18n$t("dess_buffer_help")),

      # Empreinte mémoire estimée de l'emprise courante : le pic du glouton est
      # prévisible à partir de la seule grille (cf. .desserte_memory_check), donc
      # affiché AVANT le clic — un dépassement se paie sinon par un OOM au bout
      # d'un quart d'heure de calcul.
      shiny::uiOutput(ns("mem_estimate")),

      bslib::input_task_button(
        ns("run"), i18n$t("dess_run"),
        label_busy = i18n$t("dess_running"),
        icon = bsicons::bs_icon("play-fill"),
        type = "primary", class = "w-100 mb-3"),
      shiny::uiOutput(ns("run_status"))
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(i18n$t("dess_map_title")),
      bslib::layout_sidebar(
        fillable = TRUE,
        sidebar = bslib::sidebar(
          position = "right", open = "always", width = 280,
          # Bilan du réseau créé (badges).
          htmltools::tags$strong(i18n$t("dess_summary_title")),
          shiny::uiOutput(ns("summary")),
          htmltools::tags$hr(class = "my-2"),
          shiny::sliderInput(
            ns("opacity"), i18n$t("dess_opacity"),
            min = 0, max = 1, value = 0.8, step = 0.05, ticks = FALSE),
          htmltools::tags$hr(class = "my-2"),
          bslib::accordion(
            open = FALSE,
            # Typage du réseau : flux de bois mobilisé -> primaire/secondaire/
            # tertiaire (nemeton::volume_mobilisable -> foretaccess::typer_desserte).
            bslib::accordion_panel(
              title = i18n$t("dess_typage_title"),
              icon = bsicons::bs_icon("diagram-2"),
              htmltools::tags$p(class = "text-muted small", i18n$t("dess_typage_intro")),
              shiny::numericInput(
                ns("typage_taux"), i18n$t("dess_typage_taux"),
                value = 0.5, min = 0, max = 5, step = 0.1),
              shiny::numericInput(
                ns("typage_horizon"), i18n$t("dess_typage_horizon"),
                value = 30, min = 1, max = 200, step = 1),
              shiny::actionButton(
                ns("run_typage"), i18n$t("dess_typage_run"),
                icon = shiny::icon("diagram-project"),
                class = "btn-outline-primary btn-sm w-100 mb-2"),
              shiny::uiOutput(ns("typage_result"))),
            bslib::accordion_panel(
              title = i18n$t("action_plan_section_exports"),
              icon = bsicons::bs_icon("box-arrow-up"),
              shiny::downloadButton(
                ns("export_gpkg"), i18n$t("dess_download_gpkg"),
                icon = shiny::icon("database"),
                class = "btn-outline-success btn-sm w-100")))
        ),
        leaflet::leafletOutput(ns("map"), height = "72vh")
      )
    )
  )
}

#' @noRd
mod_desserte_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    i18n <- get_i18n(get_app_options()$language %||% "fr")

    rv <- shiny::reactiveValues(result = NULL, running = FALSE, start = NULL)

    .dev_pkg_path <- tryCatch(
      if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
        find.package("nemetonshiny") else NULL,
      error = function(e) NULL)

    # Parcelles à desservir = AOI projet (EPSG:2154), repli indicators_sf ->
    # UGF -> parcelles (helper partagé avec l'accessibilité).
    units_sf <- shiny::reactive({
      .resolve_project_aoi_2154(app_state$current_project)
    })

    # Estimation de l'empreinte mémoire pour l'emprise courante (parcelles +
    # tampon), recalculée à chaque changement du tampon. Sert d'avertissement
    # amont ; le refus effectif reste côté service (run_desserte), qui est la
    # seule barrière fiable (rule 2 : pas de décision métier dans le module).
    output$mem_estimate <- shiny::renderUI({
      aoi <- units_sf()
      if (is.null(aoi)) return(NULL)
      buffer_m <- max(0, (suppressWarnings(as.numeric(input$buffer_km)) %||% 1)) * 1000
      # Le tampon est appliqué à la BBOX, pas aux géométries : c'est la seule
      # chose dont dépend la grille, et ça évite un st_buffer() à chaque frappe.
      mem <- .desserte_memory_check(aoi, res_m = 5, buffer_m = buffer_m)
      if (!is.finite(mem$cells) || !is.finite(mem$bytes)) return(NULL)
      fmt <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = " ")
      txt <- sprintf(i18n$t("dess_mem_estimate_fmt"),
                     formatC(mem$cells, format = "d", big.mark = " "),
                     fmt(mem$bytes / 1024^3),
                     if (is.finite(mem$available)) fmt(mem$available / 1024^3) else "?")
      htmltools::div(
        class = if (isTRUE(mem$ok)) "alert alert-light py-2 small mb-2"
                else "alert alert-danger py-2 small mb-2",
        htmltools::tags$div(txt),
        htmltools::tags$div(
          class = "fw-semibold",
          i18n$t(if (isTRUE(mem$ok)) "dess_mem_ok" else "dess_mem_risk")))
    })

    # --- Worker asynchrone : acquisition + coût + moteur de création ----------
    dess_task <- shiny::ExtendedTask$new(
      function(aoi_path, engine, cache_dir, buffer_m, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          plan_classes <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
            .ensure_async_plan()
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
          nemetonshiny:::run_desserte(aoi_path, engine, cache_dir, buffer_m)
        }, seed = TRUE)
      })

    bslib::bind_task_button(dess_task, "run")

    # --- Lancement -------------------------------------------------------------
    shiny::observeEvent(input$run, {
      if (isTRUE(rv$running)) {
        shiny::showNotification(i18n$t("dess_busy_already"), type = "warning",
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
        shiny::showNotification(i18n$t("dess_need_project"), type = "warning")
        return()
      }
      engine <- intersect(input$engine %||% character(0), DESSERTE_ENGINES)[1]
      if (is.na(engine) || length(engine) == 0L) {
        bslib::update_task_button("run", state = "ready")
        shiny::showNotification(i18n$t("dess_need_engine"), type = "warning")
        return()
      }
      # AOI passée au worker PAR FICHIER (pointeur externe sf non sérialisable).
      cache_dir <- .desserte_cache_dir(project_path)
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      aoi_path <- file.path(cache_dir, "aoi_input.gpkg")
      ok <- tryCatch({
        sf::st_write(aoi, aoi_path, layer = "parcelles", quiet = TRUE,
                     delete_dsn = TRUE)
        TRUE
      }, error = function(e) FALSE)
      if (!isTRUE(ok)) {
        bslib::update_task_button("run", state = "ready")
        shiny::showNotification(i18n$t("dess_need_project"), type = "warning")
        return()
      }

      rv$running <- TRUE
      rv$start <- Sys.time()
      shiny::showNotification(
        .running_notif_content(i18n$t("dess_running"), rv$start),
        id = session$ns("dess_notif"), type = "message", duration = NULL)
      buffer_m <- max(0, (suppressWarnings(as.numeric(input$buffer_km)) %||% 1)) * 1000
      tryCatch(
        dess_task$invoke(aoi_path, engine, cache_dir, buffer_m,
                         .dev_pkg_path, get_app_options()),
        error = function(e) {
          rv$running <- FALSE
          rv$start <- NULL
          shiny::removeNotification(session$ns("dess_notif"))
          bslib::update_task_button("run", state = "ready")
          shiny::showNotification(
            paste0(i18n$t("desserte_engine_failed"), " — ",
                   .strip_ansi(conditionMessage(e))),
            type = "error", duration = NULL)
        })
    })

    # Rafraîchit le chrono de la notif persistante tant que le worker tourne.
    shiny::observe({
      if (!isTRUE(rv$running)) return()
      shiny::invalidateLater(1000)
      shiny::showNotification(
        .running_notif_content(i18n$t("dess_running"), shiny::isolate(rv$start)),
        id = session$ns("dess_notif"), type = "message", duration = NULL)
    })

    output$run_status <- shiny::renderUI({
      if (!isTRUE(rv$running) || is.null(rv$start)) return(NULL)
      shiny::invalidateLater(1000)
      htmltools::div(
        class = "small text-info mt-1 text-center",
        .running_notif_content(i18n$t("dess_running"), rv$start))
    })
    shiny::outputOptions(output, "run_status", suspendWhenHidden = FALSE)

    # --- Fin de tâche ----------------------------------------------------------
    shiny::observeEvent(dess_task$status(), {
      st <- dess_task$status()
      if (!identical(st, "success") && !identical(st, "error")) return()
      rv$running <- FALSE
      rv$start <- NULL
      shiny::removeNotification(session$ns("dess_notif"))

      res <- tryCatch(dess_task$result(), error = function(e) {
        list(status = "error", reason = "desserte_engine_failed",
             detail = conditionMessage(e))
      })
      if (!is.list(res) || !identical(res$status, "success")) {
        reason <- tryCatch(res$reason, error = function(e) NULL) %||%
          "desserte_engine_failed"
        msg <- i18n$t(reason)
        detail <- tryCatch(res$detail, error = function(e) NULL)
        if (!is.null(detail) && nzchar(detail)) {
          msg <- paste0(msg, " — ", .strip_ansi(as.character(detail)))
        }
        shiny::showNotification(msg, type = "error", duration = NULL)
        return()
      }
      # Recharger depuis le cache disque (chemins + sidecar de scalaires).
      project_path <- tryCatch(app_state$current_project$path,
                               error = function(e) NULL)
      rv$result <- .load_cached_desserte(project_path) %||% res
      shiny::showNotification(
        sprintf(i18n$t("dess_done_fmt"),
                res$n_desservies %||% NA_integer_, res$n_parcelles %||% NA_integer_),
        type = "message", duration = 6)
    })

    # Restaure un réseau DÉJÀ calculé depuis le cache — PARESSEUSEMENT : lecture au
    # premier affichage de l'onglet Desserte seulement (une fois par projet), pour
    # que le clic sur un projet récent reste rapide. Observer unique (main_nav +
    # terrain_nav + projet), même patron que mod_accessibility.
    dess_loaded_for <- shiny::reactiveVal(NULL)
    shiny::observeEvent(
      list(app_state$active_main_tab, app_state$active_terrain_tab,
           app_state$current_project),
      {
        project_path <- tryCatch(app_state$current_project$path,
                                 error = function(e) NULL)
        key <- project_path %||% ""
        if (identical(dess_loaded_for(), key)) return()
        on_tab <- identical(app_state$active_main_tab, "terrain") &&
          identical(app_state$active_terrain_tab, "desserte")
        if (!on_tab) {
          rv$result <- NULL
          return()
        }
        dess_loaded_for(key)
        cached <- tryCatch(.load_cached_desserte(project_path),
                           error = function(e) NULL)
        rv$result <- cached
        if (!is.null(cached)) {
          shiny::showNotification(i18n$t("dess_cache_loaded"), type = "message",
                                  duration = 5)
        }
      }, ignoreNULL = FALSE)

    # --- Badges du réseau créé -------------------------------------------------
    output$summary <- shiny::renderUI({
      res <- rv$result
      if (is.null(res) || !identical(res$status %||% "success", "success")) {
        return(htmltools::tags$p(class = "text-muted small",
                                 i18n$t("dess_no_result_yet")))
      }
      badge <- function(label, value, cls = "bg-secondary") {
        htmltools::div(class = "d-flex justify-content-between align-items-center mb-1",
          htmltools::tags$span(class = "small", label),
          htmltools::tags$span(class = paste("badge", cls), value))
      }
      nd <- res$n_desservies %||% NA_integer_
      np <- res$n_parcelles %||% NA_integer_
      # `raccorde` (foretaccess >= 1.11) est le VRAI indicateur qualité : « toutes
      # les routes créées sont-elles rattachées au réseau existant ? ». On l'affiche
      # à la place de `connexe` (presque toujours FALSE car dominé par la
      # fragmentation du réseau existant — trompeur pour l'utilisateur).
      raccorde <- res$raccorde %||% NA
      cout <- res$cout %||% NA_real_
      htmltools::tagList(
        badge(i18n$t("dess_badge_desservies"),
              if (is.na(nd) || is.na(np)) "—" else sprintf("%d / %d", nd, np),
              if (!is.na(nd) && !is.na(np) && nd >= np) "bg-success" else "bg-warning"),
        badge(i18n$t("dess_badge_raccorde"),
              if (is.na(raccorde)) "—" else if (isTRUE(raccorde)) i18n$t("dess_yes") else i18n$t("dess_no"),
              if (isTRUE(raccorde)) "bg-success" else "bg-warning"),
        badge(i18n$t("dess_badge_cout"),
              if (is.na(cout)) "—" else format(round(cout), big.mark = " ")))
    })

    # --- Carte : fonds + parcelles + desserte existante + réseau créé (raster) -
    output$map <- leaflet::renderLeaflet({
      aoi <- units_sf()
      geo <- if (!is.null(aoi)) {
        tryCatch(sf::st_transform(aoi, 4326), error = function(e) NULL)
      }
      overlays <- c(if (!is.null(geo)) "Parcelles" else NULL,
                    "Desserte existante", "Reseau cree", "Reseau type",
                    "Places de depot")
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonDessRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          overlayGroups = overlays,
          options = leaflet::layersControlOptions(collapsed = TRUE))
      if (!is.null(geo)) {
        m <- leaflet::addPolygons(m, data = geo, group = "Parcelles",
          color = "#1f78b4", weight = 2, opacity = 0.9, fillOpacity = 0)
        bb <- tryCatch(as.numeric(sf::st_bbox(geo)), error = function(e) NULL)
        if (!is.null(bb) && all(is.finite(bb))) {
          m <- leaflet::fitBounds(m, bb[1], bb[2], bb[3], bb[4])
        }
      }
      m
    })
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)

    opacity_d <- shiny::debounce(
      shiny::reactive(suppressWarnings(as.numeric(input$opacity)) %||% 0.8), 250)

    # Overlay du réseau créé (raster) via leafletProxy : peint dans le pane dédié
    # `nemetonDessRaster`, stable au changement de fond. Raster masque 1 = route.
    shiny::observe({
      res <- rv$result
      op <- opacity_d()
      shown <- input$map_groups
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup("Reseau cree")
      rp <- tryCatch(res$reseau_path, error = function(e) NULL)
      if (is.null(rp) || !file.exists(rp)) return()
      rast <- tryCatch(terra::rast(rp), error = function(e) NULL)
      if (is.null(rast)) return()
      cmap <- leaflet::colorFactor("#B71C1C", domain = 1, na.color = "transparent")
      proxy |>
        leaflet::addRasterImage(rast, colors = cmap, opacity = op, method = "ngb",
          group = "Reseau cree",
          options = leaflet::gridOptions(pane = "nemetonDessRaster"))
      if (!is.null(shown) && !("Reseau cree" %in% shown)) {
        leaflet::hideGroup(proxy, "Reseau cree")
      }
    })

    # Overlay « Desserte existante » (réseau à raccorder), lu depuis le GPKG.
    shiny::observe({
      res <- rv$result
      shown <- input$map_groups
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup("Desserte existante")
      gp <- tryCatch(res$gpkg_path, error = function(e) NULL)
      if (is.null(gp) || !file.exists(gp)) return()
      d <- tryCatch(sf::st_read(gp, layer = "desserte_existante", quiet = TRUE),
                    error = function(e) NULL)
      if (!inherits(d, "sf") || nrow(d) == 0L) return()
      d <- tryCatch(sf::st_transform(d, 4326), error = function(e) d)
      proxy |>
        leaflet::addPolylines(data = d, group = "Desserte existante",
          color = "#37474F", weight = 1.5, opacity = 0.7)
      if (!is.null(shown) && !("Desserte existante" %in% shown)) {
        leaflet::hideGroup(proxy, "Desserte existante")
      }
    })

    # Overlay « Places de dépôt » : points calculés par la correction LiDAR de la
    # desserte côté Accessibilité (couche `places_depot` du GeoPackage
    # d'accessibilité du projet). Affichés ici aussi pour situer les dépôts vis-à-vis
    # du réseau créé/typé. Se relit à l'arrivée sur l'onglet (active_terrain_tab).
    shiny::observe({
      app_state$active_terrain_tab  # dépendance : relire en arrivant sur l'onglet
      shown <- input$map_groups
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup("Places de depot")
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      pd <- .acc_read_places_depot(.accessibility_gpkg_path(project_path))
      if (is.null(pd)) return()
      proxy |>
        leaflet::addCircleMarkers(data = pd, group = "Places de depot",
          radius = 5, color = "#B71C1C", weight = 1, fillColor = "#E53935",
          fillOpacity = 0.85, label = i18n$t("acc_places_depot"))
      if (!is.null(shown) && !("Places de depot" %in% shown)) {
        leaflet::hideGroup(proxy, "Places de depot")
      }
    })

    # --- Typage du réseau (flux de bois mobilisé) ------------------------------
    # Chaîne nemeton::volume_mobilisable(m3_total) -> foretaccess::calculer_flux ->
    # typer_desserte, sur l'objet reseau persisté par le run desserte. Calcul court
    # (le glouton n'est PAS relancé) : à la demande avec notification.
    rv_typage <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$run_typage, {
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      parcelles <- units_sf()
      if (is.null(project_path) || is.null(parcelles)) {
        shiny::showNotification(i18n$t("dess_typage_no_parcelles"), type = "warning")
        return()
      }
      cache_dir <- .desserte_cache_dir(project_path)
      rv_typage(list(status = "running"))
      nid <- shiny::showNotification(i18n$t("dess_typage_running"), duration = NULL,
                                     type = "message")
      on.exit(shiny::removeNotification(nid), add = TRUE)
      res <- tryCatch(
        run_desserte_typage(cache_dir, parcelles,
                            taux_prelevement = input$typage_taux,
                            horizon_ans = input$typage_horizon),
        error = function(e) list(status = "error", reason = "desserte_typage_failed",
                                 detail = conditionMessage(e)))
      rv_typage(res)
      if (!identical(res$status, "success")) {
        msg <- i18n$t(res$reason %||% "desserte_typage_failed")
        det <- tryCatch(res$detail, error = function(e) NULL)
        if (!is.null(det) && nzchar(det)) msg <- paste0(msg, " — ", .strip_ansi(det))
        shiny::showNotification(msg, type = "error", duration = NULL)
      }
    })

    output$typage_result <- shiny::renderUI({
      res <- rv_typage()
      if (is.null(res)) {
        return(htmltools::tags$p(class = "text-muted small", i18n$t("dess_typage_hint")))
      }
      if (identical(res$status, "running")) return(NULL)
      if (!identical(res$status, "success")) return(NULL)
      rec <- res$recap
      if (!is.data.frame(rec) || nrow(rec) == 0L) return(NULL)
      # Table type -> longueur (km).
      km <- round(suppressWarnings(as.numeric(rec$longueur)) / 1000, 2)
      rows <- lapply(seq_len(nrow(rec)), function(i) {
        htmltools::tags$tr(
          htmltools::tags$td(class = "small", as.character(rec$type[i])),
          htmltools::tags$td(class = "small text-end", sprintf("%.2f km", km[i])))
      })
      htmltools::tags$table(
        class = "table table-sm table-striped small mb-0",
        htmltools::tags$thead(htmltools::tags$tr(
          htmltools::tags$th(i18n$t("dess_typage_col_type")),
          htmltools::tags$th(class = "text-end", i18n$t("dess_typage_col_long")))),
        htmltools::tags$tbody(rows))
    })

    # Overlay « Réseau typé » : polylignes colorées par classe (primaire/secondaire/
    # tertiaire), lues depuis le GPKG du typage.
    dess_type_cols <- c(primaire = "#C62828", secondaire = "#FB8C00",
                        tertiaire = "#2E7D32")
    shiny::observe({
      res <- rv_typage()
      shown <- input$map_groups
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup("Reseau type")
      gp <- tryCatch(res$gpkg_path, error = function(e) NULL)
      if (is.null(gp) || !file.exists(gp)) return()
      d <- tryCatch(sf::st_read(gp, layer = "reseau_type", quiet = TRUE),
                    error = function(e) NULL)
      if (!inherits(d, "sf") || nrow(d) == 0L) return()
      d <- tryCatch(sf::st_transform(d, 4326), error = function(e) d)
      ty <- tolower(as.character(d[["type"]] %||% rep("", nrow(d))))
      cols <- unname(dess_type_cols[ty]); cols[is.na(cols)] <- "#607D8B"
      proxy |>
        leaflet::addPolylines(data = d, group = "Reseau type",
          color = cols, weight = 3, opacity = 0.9, label = ~ as.character(type))
      if (!is.null(shown) && !("Reseau type" %in% shown)) {
        leaflet::hideGroup(proxy, "Reseau type")
      }
    })

    # --- Export GeoPackage -----------------------------------------------------
    output$export_gpkg <- shiny::downloadHandler(
      filename = function() {
        paste0(.project_export_slug(app_state$current_project, "nemeton"),
               "_desserte.gpkg")
      },
      content = function(file) {
        on.exit(session$sendCustomMessage("nemetonHideDownloadToast", list()),
                add = TRUE)
        res <- rv$result
        if (is.null(res) || !isTRUE(export_desserte_geopackage(res, file))) {
          shiny::showNotification(i18n$t("dess_export_empty"), type = "warning")
          if (!file.exists(file)) writeLines("No data available", file)
        }
      }
    )
  })
}

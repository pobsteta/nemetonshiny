# reGénération — module Shiny (spec 027, L4)
#
# Onglet de lecture de vulnérabilité climatique : exposition microclimatique ×
# stress hydrique du sol, pour prioriser les interventions de régénération.
# Pattern golem : mod_regeneration_ui(id) + mod_regeneration_server(id, app_state).
# Aucune logique métier ici — tout passe par service_regeneration (→ nemeton).

# `.fmt_elapsed()` (chrono MM:SS des boutons/notifs async) est désormais partagé
# dans R/utils_notif.R avec le rendu unifié `.running_notif_content()`.

# --- Phase en cours du moteur reGénération (canal fichier, spec 027) ---------
# Le worker future écrit sa phase dans cache/regeneration/engine_status.json ;
# la session principale le poll (invalidateLater 1000) et rend la phase dans la
# notif bas-droite. Cf. brief engine-phase-status.

# Lit engine_status.json ; NULL si absent/illisible/périmé (> 2 min sans MAJ).
.regen_read_phase <- function(project_path) {
  if (is.null(project_path)) return(NULL)
  f <- file.path(project_path, "cache", "regeneration", "engine_status.json")
  if (!file.exists(f)) return(NULL)
  st <- tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
  if (is.null(st) || is.null(st$phase)) return(NULL)
  if (!is.null(st$ts) && as.integer(Sys.time()) - st$ts > 120L) return(NULL)
  st
}

# « base {year} ({i}/{n}) » seulement si l'événement era5 a fourni l'année.
.regen_micro_lbl <- function(i18n, key, st) {
  base <- i18n$t(key)
  if (!is.null(st$year) && !is.null(st$i) && !is.null(st$n))
    sprintf("%s %s (%d/%d)", base, st$year, as.integer(st$i), as.integer(st$n))
  else base
}

# Supprime engine_status.json en fin de tâche (success/error). Le worker écrit
# `done` mais ne supprime PAS (course avec le poll) : c'est le module qui nettoie.
.regen_cleanup_status <- function(app_state) {
  project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
  if (!is.null(project_path))
    unlink(file.path(project_path, "cache", "regeneration", "engine_status.json"))
}

# Libellé i18n d'une phase (voir modèle 6 phases + états terminaux du brief).
.regen_phase_label <- function(i18n, st) {
  switch(st$phase %||% "",
    "grille"     = i18n$t("regen_phase_grille"),
    "pai"        = sprintf(i18n$t("regen_phase_pai"),
                           i18n$t(switch(st$source %||% "",
                             "cache" = "regen_phase_pai_cache",   # hit disque : phase éclair
                             "lidar" = "regen_phase_pai_lidar",   # dérivé du nuage COPC : long
                             "regen_phase_pai_raster"))),         # repli satellite S2/PROSAIL
    "microclimf_moyenne"  = .regen_micro_lbl(i18n, "regen_phase_micro_moy", st),
    "microclimf_canicule" = .regen_micro_lbl(i18n, "regen_phase_micro_can", st),
    "exposition" = i18n$t("regen_phase_exposition"),
    "biljou"     = i18n$t("regen_phase_biljou"),
    "microclimf_skipped" = sprintf(i18n$t("regen_phase_micro_skip"),
                                   st$reason %||% ""),
    "done"       = "",
    "")
}

#' reGénération tab UI
#' @param id Module id.
#' @noRd
mod_regeneration_ui <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language %||% "fr")

  # Label + info tooltip (même pattern que mod_sampling / mod_synthesis).
  label_tt <- function(label, tooltip) {
    htmltools::tagList(label, " ",
      bslib::tooltip(bsicons::bs_icon("info-circle", class = "text-muted ms-1"),
                     tooltip, placement = "right"))
  }

  # Variante pour les entrées du radio « Couche affichée » : le tooltip décrit la
  # variable puis rappelle la lecture de l'échelle de couleurs, commune aux 4
  # couches (dégradé continu borné aux min/max des UG affichées).
  layer_tt <- function(label, tooltip) {
    htmltools::tagList(label, " ",
      bslib::tooltip(bsicons::bs_icon("info-circle", class = "text-muted ms-1"),
                     htmltools::tagList(
                       htmltools::tags$div(tooltip),
                       htmltools::tags$div(class = "mt-1 fst-italic",
                                           i18n$t("regen_map_legend_scale"))),
                     placement = "left"))
  }

  bslib::layout_sidebar(
    fillable = TRUE,
    sidebar = bslib::sidebar(
      id = ns("sidebar"), width = 360, open = TRUE,

      htmltools::tags$p(class = "text-muted small", i18n$t("regen_intro")),

      # --- Années de référence — placées en tête -------------------------
      # Le bloc année moyenne / caniculaire + le bouton Auto (E-OBS) + l'indice
      # E-OBS précèdent le moteur : l'utilisateur choisit d'abord les millésimes
      # de référence, puis lance le moteur microclimf qui les consomme.
      htmltools::tags$strong(i18n$t("regen_years_section")),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("year_moyenne"),
          i18n$t("regen_year_moyenne"), value = 2018, min = 1990, max = 2100, step = 1)),
        shiny::column(6, shiny::numericInput(ns("year_canicule"),
          i18n$t("regen_year_canicule"), value = 2022, min = 1990, max = 2100, step = 1))
      ),
      bslib::tooltip(
        bslib::input_task_button(ns("auto_years"), i18n$t("regen_year_auto"),
          icon = bsicons::bs_icon("magic"),
          label_busy = i18n$t("regen_auto_running_short"),
          type = "outline-secondary", class = "btn-sm mb-2"),
        i18n$t("regen_year_auto_tip"), placement = "right"),
      shiny::uiOutput(ns("eobs_status")),
      shiny::uiOutput(ns("eobs_index_display")),
      htmltools::tags$hr(class = "my-2"),

      # --- Moteur microclimf réel (opt-in, coûteux) ----------------------
      # Lance le vrai run microclimf (LiDAR HD + ERA5) via nemeton, en async.
      # Désactivé tant que les prérequis (grille LiDAR HD + identifiants CDS)
      # ne sont pas réunis ; le statut est affiché sous le bouton.
      htmltools::tags$small(class = "text-muted d-block mb-1", i18n$t("regen_engine_section")),
      bslib::tooltip(
        bslib::input_task_button(ns("run_engine"), i18n$t("regen_engine_run"),
          icon = bsicons::bs_icon("cpu"),
          label_busy = i18n$t("regen_engine_running_short"),
          type = "outline-primary", class = "btn-sm w-100"),
        i18n$t("regen_engine_tip"), placement = "right"),
      shiny::uiOutput(ns("engine_status")),
      htmltools::tags$hr(class = "my-2"),

      # --- Peuplement ----------------------------------------------------
      htmltools::tags$strong(i18n$t("regen_stand_section")),
      shiny::radioButtons(ns("forest_type"), i18n$t("regen_forest_type"),
        choices = stats::setNames(c("feuillu", "resineux"),
          c(i18n$t("regen_forest_feuillu"), i18n$t("regen_forest_resineux"))),
        selected = "feuillu", inline = TRUE),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("budburst"),
          i18n$t("regen_budburst"), value = 105, min = 1, max = 200)),
        shiny::column(6, shiny::numericInput(ns("leaf_fall"),
          i18n$t("regen_leaf_fall"), value = 300, min = 200, max = 366))
      ),
      shiny::numericInput(ns("lai_max"), label_tt(i18n$t("regen_lai_max"),
          i18n$t("regen_lai_tip")),
        value = NA, min = 0, max = 12, step = 0.1),
      htmltools::tags$small(class = "text-muted d-block mb-2", i18n$t("regen_lai_auto")),

      # --- Sol -----------------------------------------------------------
      htmltools::tags$strong(i18n$t("regen_soil_section")),
      shiny::numericInput(ns("ewm"), i18n$t("regen_ewm"),
        value = 150, min = 10, max = 400, step = 5),

      # --- Forçage / résolution / essence / buffer -----------------------
      shiny::radioButtons(ns("forcing"), i18n$t("regen_forcing"),
        choices = stats::setNames(c("safran", "era5"),
          c(i18n$t("regen_forcing_safran"), i18n$t("regen_forcing_era5"))),
        selected = "safran", inline = TRUE),
      shiny::radioButtons(ns("resolution"), i18n$t("regen_resolution"),
        choices = stats::setNames(c("2", "5"),
          c(i18n$t("regen_res_2m"), i18n$t("regen_res_5m"))),
        selected = "2", inline = TRUE),
      shiny::selectInput(ns("species"), label_tt(i18n$t("regen_species_target"),
          i18n$t("regen_species_tip")),
        choices = stats::setNames("", i18n$t("regen_species_generic"))),
      shiny::numericInput(ns("buffer_km"), i18n$t("regen_buffer"),
        value = 25, min = 5, max = 100, step = 5),

      shiny::checkboxInput(ns("hydric_only"), i18n$t("regen_run_hydric_only"),
        value = FALSE),
      shiny::actionButton(ns("run"), i18n$t("regen_run"),
        class = "btn-primary w-100", icon = bsicons::bs_icon("play-fill")),

      # Provenance de la donnée canopée effectivement utilisée (spec 033 D5) :
      # LiDAR HD (PAI structural) ou repli satellite S2/PROSAIL (NDP 0). Lu
      # depuis nemeton::detect_ndp() ; ne s'affiche que si une canopée est utilisée.
      shiny::uiOutput(ns("canopy_provenance")),

      # --- Export / persistance (sous le bouton Lancer) ------------------
      htmltools::tags$hr(class = "my-2"),
      htmltools::tags$small(class = "text-muted d-block mb-1", i18n$t("regen_results_section")),
      shiny::downloadButton(ns("export_gpkg"), i18n$t("regen_export_gpkg"),
        class = "btn-outline-primary btn-sm w-100 mb-2"),
      shiny::actionButton(ns("persist_db"), i18n$t("regen_persist_db"),
        class = "btn-outline-secondary btn-sm w-100", icon = bsicons::bs_icon("database"))
    ),

    # --- Résultats -------------------------------------------------------
    shiny::uiOutput(ns("status")),
    bslib::navset_card_tab(
      bslib::nav_panel(
        i18n$t("regen_tab_map"),
        # Carte + sidebar DROITE dédiée (parité panneau droit des cartes FAST) :
        # le radio « Couche affichée » vit à droite de la carte ; les fonds
        # OSM/Satellite/UGF restent dans le contrôle natif Leaflet (coin carte).
        bslib::layout_sidebar(
          fillable = TRUE,
          sidebar = bslib::sidebar(
            position = "right", open = "always", width = 260,
            htmltools::tags$strong(i18n$t("regen_map_layer")),
            # Chaque couche porte un « i » qui explique la variable cartographiée
            # et comment lire sa légende. Tooltip à gauche : le sidebar est collé
            # au bord droit de la fenêtre.
            shiny::radioButtons(ns("map_layer"), NULL,
              choiceValues = c("indice_priorite_regen", "sensibilite", "njstress", "d_tmax"),
              choiceNames = list(
                layer_tt(i18n$t("regen_map_priorite"), i18n$t("regen_map_priorite_info")),
                layer_tt(i18n$t("regen_map_sensibilite"), i18n$t("regen_map_sensibilite_info")),
                layer_tt(i18n$t("regen_map_njstress"), i18n$t("regen_map_njstress_info")),
                layer_tt(i18n$t("regen_map_dtmax"), i18n$t("regen_map_dtmax_info"))),
              selected = "indice_priorite_regen")
          ),
          leaflet::leafletOutput(ns("map"), height = "70vh")
        )
      ),
      bslib::nav_panel(i18n$t("regen_map_context"),
        leaflet::leafletOutput(ns("context_map"), height = "70vh")),
      bslib::nav_panel(i18n$t("regen_table_section"),
        shiny::checkboxInput(ns("filter_coverage"), i18n$t("regen_filter_coverage"),
          value = TRUE),
        DT::dataTableOutput(ns("table"))),
      bslib::nav_panel(i18n$t("regen_parcel_sheet"),
        shiny::uiOutput(ns("parcel_sheet")))
    )
  )
}

#' reGénération tab server
#' @param id Module id.
#' @param app_state reactiveValues carrying `current_project` (whose
#'   `indicators_sf` holds the UGF units).
#' @noRd
mod_regeneration_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    i18n <- get_i18n(get_app_options()$language %||% "fr")

    rv <- shiny::reactiveValues(
      result = NULL, years = NULL, warnings = character(0),
      running = FALSE, eobs = NULL,
      # Chrono des boutons async (spec 027 feedback) : instant de départ, remis
      # à NULL à la fin de la tâche. NULL = pas de run en cours.
      engine_running = FALSE, eobs_running = FALSE,
      engine_start = NULL, eobs_start = NULL
    )

    # UGF units of the current project (EPSG:2154), with the same geometry
    # fallback chain as mod_monitoring_pixel_map : indicators_sf → UGF →
    # parcelles. Sans ce repli, un projet Reconfort/monitoring qui n'a pas
    # (encore) calculé les 31 indicateurs n'a pas d'indicators_sf, et l'onglet
    # tombait sur le message trompeur « besoin d'un projet ».
    units_sf <- shiny::reactive({
      project <- app_state$current_project
      if (is.null(project)) return(NULL)
      to_2154 <- function(x) tryCatch(sf::st_transform(x, 2154), error = function(e) x)

      sfx <- project$indicators_sf
      if (inherits(sfx, "sf") && nrow(sfx) > 0L) return(to_2154(sfx))

      ugf <- tryCatch(if (has_ug_data(project)) ug_build_sf(project) else NULL,
                      error = function(e) NULL)
      if (inherits(ugf, "sf") && nrow(ugf) > 0L) return(to_2154(ugf))

      parc <- project$parcels
      if (inherits(parc, "sf") && nrow(parc) > 0L) return(to_2154(parc))
      NULL
    })

    # Populate the optional target-species selector from the core
    # (nemeton::regen_species_choices). Réactif à l'AOI : les essences présentes
    # sur les parcelles (BD Forêt v2 → classe via map_tfv_to_species_class) sont
    # listées en tête (groupe « présentes »), puis les essences d'adaptation.
    shiny::observe({
      units <- tryCatch(units_sf(), error = function(e) NULL)
      df <- regeneration_species_choices(units = units, lang = i18n$language)
      generic_lbl <- i18n$t("regen_species_generic")
      if (is.null(df) || !is.data.frame(df) || !all(c("code", "label") %in% names(df))) {
        shiny::updateSelectInput(session, "species",
          choices = stats::setNames("", generic_lbl))
        return()
      }
      named <- function(sub) stats::setNames(sub$code, sub$label)
      choices <- list()
      choices[[generic_lbl]] <- ""   # option générique (toutes essences) en tête
      if ("groupe" %in% names(df)) {
        present <- df[df$groupe == "present", , drop = FALSE]
        others  <- df[df$groupe != "present", , drop = FALSE]
        if (nrow(present)) choices[[i18n$t("regen_species_group_present")]] <- named(present)
        if (nrow(others))  choices[[i18n$t("regen_species_group_adaptation")]] <- named(others)
      } else {
        choices[[i18n$t("regen_species_group_adaptation")]] <- named(df)
      }
      shiny::updateSelectInput(session, "species", choices = choices,
                               selected = shiny::isolate(input$species) %||% "")
    })

    # Auto-detect the average / heatwave years from E-OBS for this AOI.
    # Async : l'acquisition E-OBS (CDS) est déléguée à un worker future (cf.
    # eobs_task) ; le résultat met à jour les deux champs Année.
    shiny::observeEvent(input$auto_years, {
      units <- units_sf()
      if (is.null(units)) {
        # Le bouton passe « busy » dès le clic : le remettre prêt sur ce retour
        # anticipé, sinon il reste grisé.
        bslib::update_task_button("auto_years", state = "ready")
        shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
        return()
      }
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      rv$eobs_running <- TRUE
      rv$eobs_start <- Sys.time()
      # Notif persistante en bas à droite (retirée en fin de tâche) — le run
      # E-OBS (CDS) peut durer plusieurs minutes.
      shiny::showNotification(i18n$t("regen_auto_running"), type = "message",
                              duration = NULL, id = session$ns("eobs_notif"))
      eobs_task$invoke(units, project_path, .dev_pkg_path, get_app_options())
    })

    shiny::observeEvent(eobs_task$status(), {
      st <- eobs_task$status()
      if (identical(st, "success")) {
        rv$eobs_running <- FALSE
        rv$eobs_start <- NULL
        shiny::removeNotification(session$ns("eobs_notif"))
        yrs <- tryCatch(eobs_task$result(), error = function(e) NULL)
        if (!is.null(yrs) && !is.null(yrs$year_moyenne)) {
          shiny::updateNumericInput(session, "year_moyenne", value = yrs$year_moyenne)
          shiny::updateNumericInput(session, "year_canicule", value = yrs$year_canicule)
          rv$eobs <- yrs
          shiny::showNotification(
            sprintf(i18n$t("regen_auto_done"), yrs$year_moyenne, yrs$year_canicule),
            type = "message", duration = 6)
        } else {
          shiny::showNotification(i18n$t("regen_auto_none"), type = "warning", duration = 6)
        }
      } else if (identical(st, "error")) {
        rv$eobs_running <- FALSE
        rv$eobs_start <- NULL
        shiny::removeNotification(session$ns("eobs_notif"))
        shiny::showNotification(i18n$t("regen_auto_none"), type = "warning", duration = 6)
      }
    })

    # Statut/chrono sous le bouton Auto (E-OBS) — tick chaque seconde tant que
    # la détection tourne (spec 027 feedback). Le sablier « en cours » vit
    # désormais ici (retiré de eobs_index_display pour ne pas doubler).
    output$eobs_status <- shiny::renderUI({
      if (!isTRUE(rv$eobs_running)) return(NULL)
      shiny::invalidateLater(1000)
      htmltools::div(class = "small text-info mt-1",
        bsicons::bs_icon("hourglass-split", class = "me-1"),
        i18n$t("regen_auto_running_short"),
        htmltools::tags$span(class = "ms-1 font-monospace",
                             .fmt_elapsed(rv$eobs_start)))
    })

    output$eobs_index_display <- shiny::renderUI({
      if (isTRUE(rv$eobs_running)) return(NULL)   # chrono affiché par eobs_status
      if (is.null(rv$eobs)) return(NULL)
      htmltools::tags$small(class = "text-muted d-block mb-2",
        sprintf("%s : %s / %s", i18n$t("regen_eobs_index"),
                rv$eobs$year_moyenne %||% "?", rv$eobs$year_canicule %||% "?"))
    })

    # --- Run --------------------------------------------------------------
    shiny::observeEvent(input$run, {
      units <- units_sf()
      if (is.null(units)) {
        shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
        return()
      }
      rv$running <- TRUE
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      # NA/NULL numeric inputs → NULL (let the core auto-detect / default).
      na_null <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) NULL else x
      cfg <- list(
        year_moyenne = na_null(input$year_moyenne),
        year_canicule = na_null(input$year_canicule),
        forest_type = input$forest_type %||% "feuillu",
        lai_max = na_null(input$lai_max),
        species = if (nzchar(input$species %||% "")) input$species else NULL,
        hydric_only = isTRUE(input$hydric_only)
      )

      # withProgress affiche un overlay immédiat (retour visible avant le calcul
      # synchrone) et empêche les reclics pendant le run.
      res <- shiny::withProgress(message = i18n$t("regen_running"), value = 0.2, {
        precomputed <- load_regeneration_precomputed(project_path)
        shiny::incProgress(0.3)
        out <- tryCatch(
          run_regeneration(units, cfg = cfg, precomputed = precomputed),
          error = function(e) {
            shiny::showNotification(
              sprintf("%s: %s", i18n$t("error"), conditionMessage(e)), type = "error")
            NULL
          })
        shiny::incProgress(0.5)
        out
      })
      rv$running <- FALSE
      if (!is.null(res)) {
        rv$result <- res$units
        rv$years <- res$years
        rv$warnings <- res$warnings %||% character(0)
        # Provenance canopée lue sur le résultat (repli satellite / LiDAR HD).
        rv$canopy_source <- regen_canopy_provenance(res$units)
        # Publier le résultat pour l'export PDF (section reGénération de mod_synthesis).
        app_state$regeneration_result <- res$units
        nw <- length(rv$warnings)
        if (nw > 0L) {
          shiny::showNotification(sprintf(i18n$t("regen_run_done_warn"), nw),
                                  type = "warning", duration = 8)
        } else {
          shiny::showNotification(i18n$t("regen_run_done"), type = "message", duration = 5)
        }
      }
    })

    # --- Moteur microclimf réel (option B, async) -------------------------
    # Provenance identique à la session principale pour recharger le namespace
    # dans le worker future (cf. mod_home compute_task). Le run est lourd
    # (LiDAR HD + ERA5 + microclimf) : il tourne dans un process séparé, ne
    # renvoie qu'un sf, et la sortie est cachée (sensibilite.gpkg) pour être
    # consommée en fast-path par run_regeneration au run suivant.
    .dev_pkg_path <- tryCatch(
      if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
        find.package("nemetonshiny") else NULL,
      error = function(e) NULL)

    engine_task <- shiny::ExtendedTask$new(
      function(units, project_path, cfg, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          plan_classes <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
            future::plan("multisession")
          }
        }
        promises::future_promise({
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          fn <- getFromNamespace("run_regeneration_engine", "nemetonshiny")
          fn(units, project_path, cfg)
        }, seed = TRUE)
      })

    # Auto E-OBS async : l'acquisition E-OBS (CDS, même clé qu'ERA5) peut être
    # lente / mise en file → worker future, ne renvoie que la paire d'années.
    eobs_task <- shiny::ExtendedTask$new(
      function(units, project_path, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          plan_classes <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
            future::plan("multisession")
          }
        }
        promises::future_promise({
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          fn <- getFromNamespace("run_regeneration_detect_years", "nemetonshiny")
          fn(units, project_path)
        }, seed = TRUE)
      })

    # Lier chaque input_task_button à sa tâche : bslib désactive le bouton +
    # affiche le spinner tant que la tâche tourne, puis le réactive (succès OU
    # erreur). Empêche les runs concurrents (réécriture sensibilite/biljou.gpkg).
    # `bind_task_button()` prend l'id local au module (sans ns()).
    bslib::bind_task_button(engine_task, "run_engine")
    bslib::bind_task_button(eobs_task, "auto_years")

    # Invalidation manuelle du cache PAI (§3 brief pai-cache) : supprime
    # cache/regeneration/pai.tif → le prochain run recalcule la structure de
    # végétation depuis le nuage LiDAR. Sans effet si le fichier n'existe pas.
    shiny::observeEvent(input$recompute_pai, {
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(project_path)) return()
      f <- file.path(project_path, "cache", "regeneration", "pai.tif")
      if (file.exists(f)) {
        unlink(f)
        shiny::showNotification(i18n$t("regen_pai_cache_cleared"), type = "message")
      }
    })

    shiny::observeEvent(input$run_engine, {
      units <- units_sf()
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(units) || is.null(project_path)) {
        bslib::update_task_button("run_engine", state = "ready")
        shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
        return()
      }
      forcing <- input$forcing %||% "safran"
      pre <- regen_engine_prereqs(project_path, forcing)
      if (!isTRUE(pre$ok)) {
        bslib::update_task_button("run_engine", state = "ready")
        shiny::showNotification(i18n$t(pre$reason), type = "warning", duration = 8)
        return()
      }
      na_null <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) NULL else x
      cfg <- list(
        year_moyenne = na_null(input$year_moyenne),
        year_canicule = na_null(input$year_canicule),
        forest_type = input$forest_type %||% "feuillu",
        forcing = forcing,
        ewm = na_null(input$ewm),
        lai_max = na_null(input$lai_max),
        budburst = na_null(input$budburst),
        leaf_fall = na_null(input$leaf_fall)
      )
      # Supprimer un engine_status.json périmé d'un run précédent : sans ça, le
      # poll afficherait une phase fantôme du run antérieur au démarrage.
      unlink(file.path(project_path, "cache", "regeneration", "engine_status.json"))
      rv$engine_running <- TRUE
      rv$engine_start <- Sys.time()
      # Notif persistante bas-droite (retirée en fin de tâche) — le moteur réel
      # (LiDAR HD + ERA5 + microclimf) peut durer plusieurs minutes. Le libellé
      # est ensuite rafraîchi phase par phase par l'observe de poll ci-dessous.
      shiny::showNotification(i18n$t("regen_engine_running"), type = "message",
                              duration = NULL, id = session$ns("engine_notif"))
      engine_task$invoke(units, project_path, cfg, .dev_pkg_path, get_app_options())
    })

    # Poll (1 s) du fichier d'état écrit par le worker : rafraîchit la notif
    # persistante bas-droite (même id → Shiny remplace le contenu en place) avec
    # le libellé de la phase en cours + chrono. Cf. brief engine-phase-status §3.3.
    shiny::observe({
      if (!isTRUE(rv$engine_running)) return()
      shiny::invalidateLater(1000)
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      st  <- .regen_read_phase(project_path)
      lbl <- if (is.null(st)) i18n$t("regen_engine_running") else .regen_phase_label(i18n, st)
      if (!nzchar(lbl)) lbl <- i18n$t("regen_engine_running")   # done / illisible
      shiny::showNotification(
        .running_notif_content(lbl, rv$engine_start),
        id = session$ns("engine_notif"), type = "message", duration = NULL)
    })

    shiny::observeEvent(engine_task$status(), {
      st <- engine_task$status()
      if (identical(st, "success")) {
        rv$engine_running <- FALSE
        rv$engine_start <- NULL
        shiny::removeNotification(session$ns("engine_notif"))
        .regen_cleanup_status(app_state)
        # Le worker a écrit sensibilite.gpkg / biljou.gpkg : recharger le
        # precomputed et relancer l'analyse normale (fast-path) pour rafraîchir
        # carte/table. Les avertissements spécifiques du moteur (échec réel
        # microclimf/BILJOU) sont remontés en toast.
        eng <- tryCatch(engine_task$result(), error = function(e) NULL)
        project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
        units <- units_sf()
        if (!is.null(units) && !is.null(project_path)) {
          precomputed <- load_regeneration_precomputed(project_path)
          na_null <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) NULL else x
          cfg <- list(
            year_moyenne = na_null(input$year_moyenne),
            year_canicule = na_null(input$year_canicule),
            forest_type = input$forest_type %||% "feuillu",
            lai_max = na_null(input$lai_max),
            species = if (nzchar(input$species %||% "")) input$species else NULL,
            hydric_only = isTRUE(input$hydric_only))
          res <- tryCatch(run_regeneration(units, cfg = cfg, precomputed = precomputed),
                          error = function(e) NULL)
          if (!is.null(res)) {
            rv$result <- res$units
            rv$years <- res$years
            rv$warnings <- res$warnings %||% character(0)
            app_state$regeneration_result <- res$units
          }
        }
        # Provenance rapportée par le moteur (fiable : survit au cache gpkg qui
        # perdrait l'attribut lai_source lu par detect_ndp).
        rv$canopy_source <- eng$canopy %||% NA_character_
        eng_warns <- eng$warnings %||% character(0)
        if (length(eng_warns)) {
          shiny::showNotification(
            htmltools::tags$span(paste(eng_warns, collapse = " — ")),
            type = "warning", duration = 10)
        }
        shiny::showNotification(i18n$t("regen_engine_done"), type = "message", duration = 6)
      } else if (identical(st, "error")) {
        rv$engine_running <- FALSE
        rv$engine_start <- NULL
        shiny::removeNotification(session$ns("engine_notif"))
        .regen_cleanup_status(app_state)
        err <- tryCatch(engine_task$result(), error = function(e) conditionMessage(e))
        shiny::showNotification(
          sprintf("%s: %s", i18n$t("error"), .strip_ansi(as.character(err))),
          type = "error", duration = 10)
      }
    })

    output$engine_status <- shiny::renderUI({
      if (isTRUE(rv$engine_running)) {
        shiny::invalidateLater(1000)   # chrono re-rendu chaque seconde
        return(htmltools::div(class = "small text-info mt-1",
          bsicons::bs_icon("hourglass-split", class = "me-1"),
          i18n$t("regen_engine_running_short"),
          htmltools::tags$span(class = "ms-1 font-monospace",
                               .fmt_elapsed(rv$engine_start))))
      }
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(project_path)) return(NULL)
      pre <- regen_engine_prereqs(project_path, input$forcing %||% "safran")
      if (isTRUE(pre$reason == "regen_engine_prereq_core")) {
        return(htmltools::div(class = "small text-muted mt-1",
          bsicons::bs_icon("info-circle", class = "me-1"), i18n$t("regen_engine_prereq_core")))
      }
      # Une ligne par moteur : prêt (vert) ou prérequis manquant (gris).
      line <- function(ready, ok_key, ko_key) {
        if (isTRUE(ready)) {
          htmltools::div(class = "small text-success",
            bsicons::bs_icon("check-circle", class = "me-1"), i18n$t(ok_key))
        } else {
          htmltools::div(class = "small text-muted",
            bsicons::bs_icon("dash-circle", class = "me-1"), i18n$t(ko_key))
        }
      }
      htmltools::div(class = "mt-1",
        line(pre$microclimf, "regen_engine_ready_micro", "regen_engine_status_micro"),
        line(pre$biljou, "regen_engine_ready_biljou", "regen_engine_status_biljou_era5"))
    })

    # Badge de provenance canopée (spec 033 D5). Priorité à la source rapportée
    # par le moteur (rv$canopy_source) ; sinon lecture de detect_ndp() sur le
    # résultat. Rien affiché tant qu'aucune canopée n'est utilisée.
    output$canopy_provenance <- shiny::renderUI({
      src <- rv$canopy_source
      if (is.null(src) || is.na(src)) {
        src <- if (!is.null(rv$result)) regen_canopy_provenance(rv$result) else NA_character_
      }
      if (is.na(src)) return(NULL)
      if (identical(src, "satellite")) {
        bslib::tooltip(
          htmltools::tags$span(class = "badge text-bg-warning mt-2 d-inline-block",
            bsicons::bs_icon("badge-sd", class = "me-1"), i18n$t("regen_canopee_satellite")),
          i18n$t("regen_canopee_satellite_info"), placement = "right")
      } else {
        # Canopée LiDAR HD : le PAI structural a été dérivé du nuage et mis en
        # cache (cache/regeneration/pai.tif). On expose ici l'invalidation manuelle
        # du cache — pertinente seulement dans ce cas (le repli satellite n'écrit
        # pas de pai.tif). Le lien n'apparaît donc qu'après un run LiDAR réussi.
        htmltools::tagList(
          htmltools::tags$span(class = "badge text-bg-success mt-2 d-inline-block",
            bsicons::bs_icon("badge-hd", class = "me-1"), i18n$t("regen_canopee_lidar")),
          bslib::tooltip(
            shiny::actionLink(ns("recompute_pai"), i18n$t("regen_pai_recompute"),
              icon = bsicons::bs_icon("arrow-repeat"), class = "small d-block mt-1"),
            i18n$t("regen_pai_recompute_tip"), placement = "right")
        )
      }
    })

    output$status <- shiny::renderUI({
      if (isTRUE(rv$running)) {
        return(htmltools::div(class = "alert alert-info py-2",
          bsicons::bs_icon("hourglass-split", class = "me-1"), i18n$t("regen_running")))
      }
      if (is.null(rv$result)) {
        return(htmltools::div(class = "text-muted small fst-italic mb-2",
                              i18n$t("regen_need_project")))
      }
      warns <- if (length(rv$warnings)) {
        htmltools::div(class = "alert alert-warning py-2 small",
          bsicons::bs_icon("exclamation-triangle", class = "me-1"),
          htmltools::tags$ul(class = "mb-0",
            lapply(rv$warnings, function(w) htmltools::tags$li(w))))
      }
      htmltools::div(
        htmltools::div(class = "alert alert-secondary py-2 small mb-2",
          bsicons::bs_icon("info-circle", class = "me-1"), i18n$t("regen_ndp_model")),
        warns
      )
    })

    # Base map — STABLE widget (parité cartes FORDEAD/FAST) : fonds OSM/Satellite
    # + contrôle de couches natif (boîtier coin haut-droit). Ne dépend que du
    # cadrage ; les polygones colorés (groupe « UGF ») sont (re)dessinés par
    # l'observer leafletProxy ci-dessous selon la couche choisie dans la sidebar.
    output$map <- leaflet::renderLeaflet({
      units <- units_sf()
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(
          baseGroups    = c("OSM", "Satellite"),
          overlayGroups = "UGF",
          options       = leaflet::layersControlOptions(collapsed = TRUE))
      if (!is.null(units)) {
        # Contour UGF de base, visible immédiatement (avant toute analyse).
        # L'observer proxy le remplace par le choroplèthe dès qu'un résultat existe.
        geo <- tryCatch(sf::st_transform(units, 4326), error = function(e) units)
        m <- leaflet::addPolygons(m, data = geo, group = "UGF", weight = 1,
          color = "#1B6B1B", fillColor = "#1B6B1B", fillOpacity = 0.10)
        bb <- tryCatch(as.numeric(sf::st_bbox(geo)), error = function(e) NULL)
        if (!is.null(bb) && all(is.finite(bb))) {
          m <- leaflet::fitBounds(m, bb[1], bb[2], bb[3], bb[4])
        }
      }
      m
    })

    # Data layer via proxy — swap the choropleth + legend without recreating the
    # widget (préserve le LayersControl et le zoom). Réagit au résultat et au
    # radio « Couche affichée » de la sidebar.
    shiny::observe({
      res <- rv$result
      units <- units_sf()
      col <- input$map_layer %||% "indice_priorite_regen"
      proxy <- leaflet::leafletProxy("map")
      leaflet::clearGroup(proxy, "UGF")
      leaflet::removeControl(proxy, "regen_legend")
      # Pas (encore) de résultat exploitable → garder le contour UGF de base
      # visible plutôt qu'une carte vide.
      if (is.null(res) || !col %in% names(res)) {
        if (!is.null(units)) {
          geo <- tryCatch(sf::st_transform(units, 4326), error = function(e) units)
          leaflet::addPolygons(proxy, data = geo, group = "UGF", weight = 1,
            color = "#1B6B1B", fillColor = "#1B6B1B", fillOpacity = 0.10)
        }
        return()
      }

      vals <- suppressWarnings(as.numeric(res[[col]]))
      geo <- tryCatch(sf::st_transform(res, 4326), error = function(e) res)
      if (all(is.na(vals))) {
        leaflet::addPolygons(proxy, data = geo, group = "UGF", weight = 1,
          color = "#888", fillOpacity = 0.2)
        return()
      }
      pal <- leaflet::colorNumeric("RdYlGn", domain = vals, na.color = "#cccccc")
      proxy |>
        leaflet::addPolygons(data = geo, group = "UGF", weight = 1, color = "#333",
          fillColor = pal(vals), fillOpacity = 0.75) |>
        leaflet::addLegend(pal = pal, values = vals, position = "bottomright",
          layerId = "regen_legend",
          title = i18n$t(paste0("regen_map_",
            switch(col, indice_priorite_regen = "priorite", sensibilite = "sensibilite",
                   njstress = "njstress", d_tmax = "dtmax", "priorite"))))
    })

    output$context_map <- leaflet::renderLeaflet({
      units <- units_sf()
      shiny::req(units)
      cells <- regeneration_context_eobs(
        units, precomputed = load_regeneration_precomputed(
          tryCatch(app_state$current_project$path, error = function(e) NULL)),
        buffer_m = (input$buffer_km %||% 25) * 1000)
      m <- leaflet::leaflet() |> leaflet::addTiles()
      if (is.null(cells) || !inherits(cells, "sf") || nrow(cells) == 0) return(m)
      geo <- tryCatch(sf::st_transform(cells, 4326), error = function(e) cells)
      cls <- if ("classe_bivariee" %in% names(cells)) as.numeric(cells$classe_bivariee) else rep(5, nrow(cells))
      pal <- leaflet::colorNumeric("viridis", domain = c(1, 9), na.color = "#ccc")
      m |> leaflet::addCircleMarkers(data = geo, radius = 4, stroke = FALSE,
        fillColor = pal(cls), fillOpacity = 0.8)
    })

    output$table <- DT::renderDataTable({
      res <- rv$result
      shiny::req(res)
      df <- sf::st_drop_geometry(res)
      cols <- intersect(c("ug_id", "priorite", "indice_priorite_regen", "sensibilite",
        "rang_sensibilite", "njstress", "istress", "deb_stress", "rew_min",
        "d_tmax", "d_vpd", "couverture_pct"), names(df))
      df <- df[, cols, drop = FALSE]
      if (isTRUE(input$filter_coverage) && "couverture_pct" %in% names(df)) {
        keep <- is.na(df$couverture_pct) | df$couverture_pct >= 50
        df <- df[keep, , drop = FALSE]
      }
      order_col <- if ("rang_sensibilite" %in% names(df)) "rang_sensibilite" else NULL
      DT::datatable(df, rownames = FALSE, selection = "single",
        options = list(pageLength = 15, scrollX = TRUE,
          order = if (!is.null(order_col)) list(list(which(names(df) == order_col) - 1, "asc")) else list()))
    })

    output$parcel_sheet <- shiny::renderUI({
      res <- rv$result
      if (is.null(res)) return(htmltools::div(class = "text-muted fst-italic",
                                              i18n$t("regen_need_project")))
      sel <- input$table_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        return(htmltools::div(class = "text-muted fst-italic", i18n$t("regen_select_ug")))
      }
      df <- sf::st_drop_geometry(res)
      row <- df[sel[1], , drop = FALSE]
      fields <- intersect(c("indice_priorite_regen", "sensibilite", "njstress",
        "istress", "deb_stress", "rew_min", "d_tmax", "d_vpd", "couverture_pct"), names(row))
      htmltools::tags$table(class = "table table-sm",
        lapply(fields, function(f) htmltools::tags$tr(
          htmltools::tags$th(i18n$t(paste0("regen_col_",
            switch(f, indice_priorite_regen = "indice", sensibilite = "sensibilite",
              njstress = "njstress", istress = "istress", deb_stress = "deb_stress",
              rew_min = "rew_min", d_tmax = "dtmax", d_vpd = "dvpd",
              couverture_pct = "couverture", f)))),
          htmltools::tags$td(format(row[[f]], digits = 3)))))
    })

    # --- Export GPKG (§7) -------------------------------------------------
    output$export_gpkg <- shiny::downloadHandler(
      filename = function() "regeneration.gpkg",
      content = function(file) {
        res <- rv$result
        if (is.null(res)) {
          shiny::showNotification(i18n$t("regen_export_empty"), type = "warning")
          return(invisible(NULL))
        }
        export_regeneration_geopackage(res, file)
      }
    )

    # --- Persistance versionnée en base (§6A) -----------------------------
    shiny::observeEvent(input$persist_db, {
      res <- rv$result
      if (is.null(res)) {
        shiny::showNotification(i18n$t("regen_export_empty"), type = "warning")
        return()
      }
      if (!is_db_configured()) {
        shiny::showNotification(i18n$t("regen_db_unavailable"), type = "warning")
        return()
      }
      pid <- tryCatch(app_state$current_project$id, error = function(e) NULL)
      ver <- tryCatch({
        con <- get_db_connection()
        on.exit(close_db_connection(con), add = TRUE)
        db_save_regeneration(con, pid, res)
      }, error = function(e) {
        shiny::showNotification(
          sprintf("%s: %s", i18n$t("error"), conditionMessage(e)), type = "error")
        NULL
      })
      if (!is.null(ver)) {
        shiny::showNotification(sprintf(i18n$t("regen_persisted"), ver), type = "message")
      }
    })

    list(result = shiny::reactive(rv$result))
  })
}

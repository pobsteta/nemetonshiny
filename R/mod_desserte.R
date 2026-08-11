# ===========================================================================
# Module — Création de desserte forestière (ForêtAccess), sous-onglet Terrain
# ===========================================================================
#
# Présentation Shiny du service `R/service_desserte.R` (adaptateur autour des
# moteurs de création de réseau de `foretaccess`). Aucune logique métier (règle
# 2) : le module orchestre l'UI, l'exécution asynchrone (worker `future`) et le
# rendu carte/badges.
#
# v1 : moteur GLOUTON seul, OPT-IN. La durée dépend de la SURFACE de l'emprise
# et de `skidding_m` (cf. service_desserte.R), pas du nombre de parcelles. Même
# patron que reGénération / Accessibilité : `ExtendedTask` + `future_promise`,
# notif persistante bas-droite avec chrono, retour immédiat. Le réseau créé est
# affiché en overlay RASTER (léger) ; les lignes vectorielles détaillées partent
# à l'export GeoPackage.

# Lit `engine_status.json` du cache desserte, écrit par le worker à chaque
# changement d'étape (`.dess_write_phase`). NULL si absent, illisible ou périmé
# (> 2 min sans mise à jour) — même contrat que `.regen_read_phase()`. Le seuil
# de péremption évite d'afficher indéfiniment la phase d'un worker mort.
.dess_read_phase <- function(project_path) {
  if (is.null(project_path)) return(NULL)
  f <- file.path(project_path, "cache", "desserte", "engine_status.json")
  if (!file.exists(f)) return(NULL)
  st <- tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
  if (is.null(st) || is.null(st$phase)) return(NULL)
  if (!is.null(st$ts) && as.integer(Sys.time()) - st$ts > 120L) return(NULL)
  as.character(st$phase)[1]
}

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
        choices = stats::setNames(DESSERTE_ENGINES,
                                  c(i18n$t("dess_engine_glouton"),
                                    i18n$t("dess_engine_steiner"))),
        selected = DESSERTE_ENGINES[[1]]),
      # Avertissement « calcul long » (parité câble) : le glouton trace un A*
      # par CELLULE de parcelle non desservie, donc le temps croît avec la
      # surface de l'emprise et décroît avec `skidding_m`.
      htmltools::div(
        class = "alert alert-warning py-2 small",
        shiny::icon("triangle-exclamation"), " ", i18n$t("dess_slow_help")),

      shiny::numericInput(
        ns("buffer_km"), i18n$t("dess_buffer"),
        value = 1, min = 0, max = 20, step = 1),
      htmltools::tags$p(class = "text-muted small", i18n$t("dess_buffer_help")),

      # Distance de débardage : paramètre MÉTIER, pas un réglage de performance.
      # Il change le résultat — sur Dabo, 39 routes à 100 m contre aucune à
      # 300 m — donc il doit être visible, sinon « rien à construire » est
      # inintelligible. Paliers repris de `foretaccess_config()$skidder$
      # classes_distance_m`.
      shiny::numericInput(
        ns("skidding_m"), i18n$t("dess_skidding"),
        value = DESSERTE_SKIDDING_DEFAULT_M, min = 0, max = 2000, step = 50),
      htmltools::tags$p(class = "text-muted small", i18n$t("dess_skidding_help")),

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
            # Intégrité du réseau (spec 025). Action SÉPARÉE et non une étape du
            # calcul : mesuré 376,8 s sur Dabo (3 122 tronçons) contre 39,7 s
            # pour la création entière — l'inclure rendrait « Générer la
            # desserte » dix fois plus lent.
            bslib::accordion_panel(
              title = i18n$t("dess_integrite_title"),
              icon = bsicons::bs_icon("diagram-3-fill"),
              htmltools::tags$p(class = "text-muted small",
                                i18n$t("dess_integrite_intro")),
              bslib::input_task_button(
                ns("run_integrite"), i18n$t("dess_integrite_run"),
                label_busy = i18n$t("dess_integrite_running"),
                icon = bsicons::bs_icon("check2-square"),
                class = "btn-outline-primary btn-sm w-100 mb-2"),
              shiny::uiOutput(ns("integrite_status"))),
            # Optimisation du réseau créé. Action séparée : chaque essai est une
            # construction gloutonne complète. Mesuré sur Dabo — glouton 82,2 s /
            # coût 16 673 contre multistart 100,2 s / coût 15 002, soit 1,2x le
            # temps pour -10 % de coût.
            bslib::accordion_panel(
              title = i18n$t("dess_optim_title"),
              icon = bsicons::bs_icon("stars"),
              htmltools::tags$p(class = "text-muted small", i18n$t("dess_optim_intro")),
              shiny::selectInput(
                ns("optim_strategie"), i18n$t("dess_optim_strategie"),
                choices = stats::setNames(
                  DESSERTE_OPTIM_STRATEGIES,
                  c(i18n$t("dess_optim_multistart"), i18n$t("dess_optim_recuit"),
                    i18n$t("dess_optim_riprute"))),
                selected = DESSERTE_OPTIM_STRATEGIES[[1]]),
              shiny::numericInput(ns("optim_n_start"), i18n$t("dess_optim_n_start"),
                                  value = DESSERTE_OPTIM_N_START, min = 2, max = 32, step = 2),
              bslib::input_task_button(
                ns("run_optim"), i18n$t("dess_optim_run"),
                label_busy = i18n$t("dess_optim_running"),
                icon = bsicons::bs_icon("stars"),
                class = "btn-outline-primary btn-sm w-100 mb-2"),
              shiny::uiOutput(ns("optim_result"))),
            # Complément OSM de la BD TOPO (spec 028).
            bslib::accordion_panel(
              title = i18n$t("dess_osm_title"),
              icon = bsicons::bs_icon("signpost-2"),
              htmltools::tags$p(class = "text-muted small", i18n$t("dess_osm_intro")),
              bslib::input_task_button(
                ns("run_osm"), i18n$t("dess_osm_run"),
                label_busy = i18n$t("dess_osm_running"),
                icon = bsicons::bs_icon("cloud-download"),
                class = "btn-outline-primary btn-sm w-100 mb-2"),
              shiny::uiOutput(ns("osm_result"))),
            # Détection de routes absentes de la BD TOPO (dessertR, spec 026).
            # La plus lourde du panneau : mesuré 7,91 Go de pic et 189 s SANS
            # nuage LiDAR sur 1 855 ha, et > 10 min avec. D'où le garde-fou
            # mémoire côté service et l'avertissement ci-dessous.
            bslib::accordion_panel(
              title = i18n$t("dess_detect_title"),
              icon = bsicons::bs_icon("search"),
              htmltools::tags$p(class = "text-muted small", i18n$t("dess_detect_intro")),
              htmltools::div(
                class = "alert alert-warning py-2 small",
                shiny::icon("triangle-exclamation"), " ", i18n$t("dess_detect_warn")),
              shiny::checkboxInput(ns("detect_lidar"), i18n$t("dess_detect_lidar"),
                                   value = TRUE),
              bslib::input_task_button(
                ns("run_detect"), i18n$t("dess_detect_run"),
                label_busy = i18n$t("dess_detect_running"),
                icon = bsicons::bs_icon("search"),
                class = "btn-outline-primary btn-sm w-100 mb-2"),
              shiny::uiOutput(ns("detect_result"))),
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
      function(aoi_path, engine, cache_dir, buffer_m, skidding_m, dev_path, app_opts) {
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
          nemetonshiny:::run_desserte(aoi_path, engine, cache_dir, buffer_m,
                                      skidding_m = skidding_m)
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
      skidding_m <- suppressWarnings(as.numeric(input$skidding_m))
      if (!isTRUE(is.finite(skidding_m)) || skidding_m < 0) {
        skidding_m <- DESSERTE_SKIDDING_DEFAULT_M
      }
      tryCatch(
        dess_task$invoke(aoi_path, engine, cache_dir, buffer_m, skidding_m,
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

    # Libellé « en cours » enrichi de la phase publiée par le worker sur le canal
    # disque. Le moteur glouton peut tourner des dizaines de minutes sur de
    # grandes parcelles : sans la phase, l'utilisateur ne voit qu'un chrono et
    # conclut que rien ne se passe.
    dess_running_label <- function() {
      pp <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      ph <- .dess_read_phase(pp)
      base <- i18n$t("dess_running")
      if (is.null(ph)) return(base)
      i <- match(ph, DESSERTE_PHASES)
      lbl <- i18n$t(paste0("dess_phase_", ph))
      if (is.na(i)) return(paste0(base, " — ", lbl))
      sprintf("%s — %s (%d/%d)", base, lbl, i, length(DESSERTE_PHASES))
    }

    # Rafraîchit le chrono ET la phase de la notif persistante tant que le
    # worker tourne.
    shiny::observe({
      if (!isTRUE(rv$running)) return()
      shiny::invalidateLater(1000)
      shiny::showNotification(
        .running_notif_content(dess_running_label(), shiny::isolate(rv$start)),
        id = session$ns("dess_notif"), type = "message", duration = NULL)
    })

    output$run_status <- shiny::renderUI({
      if (!isTRUE(rv$running) || is.null(rv$start)) return(NULL)
      shiny::invalidateLater(1000)
      htmltools::div(
        class = "small text-info mt-1 text-center",
        .running_notif_content(dess_running_label(), rv$start))
    })
    shiny::outputOptions(output, "run_status", suspendWhenHidden = FALSE)

    # --- Fin de tâche ----------------------------------------------------------
    shiny::observeEvent(dess_task$status(), {
      st <- dess_task$status()
      if (!identical(st, "success") && !identical(st, "error")) return()
      rv$running <- FALSE
      rv$start <- NULL
      shiny::removeNotification(session$ns("dess_notif"))
      # Retire le canal de phase : un `engine_status.json` laissé sur disque
      # ferait afficher une phase périmée au prochain lancement, avant que le
      # worker n'ait publié la sienne.
      tryCatch({
        pp <- app_state$current_project$path
        if (!is.null(pp)) unlink(file.path(pp, "cache", "desserte",
                                           "engine_status.json"))
      }, error = function(e) invisible(NULL))

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
      # Zéro route créée est un SUCCÈS, pas un résultat vide : à `skidding_m`
      # réaliste, une forêt bien desservie n'a rien à construire (mesuré sur
      # Dabo : 39 routes à 100 m, aucune à 300 m). Sans message dédié,
      # l'utilisateur lirait « desserte créée » devant une carte sans route.
      nr <- suppressWarnings(as.integer(res$n_routes %||% NA_integer_))
      shiny::showNotification(
        if (!is.na(nr) && nr == 0L) {
          sprintf(i18n$t("dess_done_none_fmt"),
                  suppressWarnings(as.numeric(res$skidding_m %||% NA_real_)))
        } else {
          sprintf(i18n$t("dess_done_fmt"),
                  res$n_desservies %||% NA_integer_, res$n_parcelles %||% NA_integer_)
        },
        type = "message", duration = 8)
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
      nroutes <- suppressWarnings(as.integer(res$n_routes %||% NA_integer_))
      integ <- res$integrite
      htmltools::tagList(
        badge(i18n$t("dess_badge_desservies"),
              if (is.na(nd) || is.na(np)) "—" else sprintf("%d / %d", nd, np),
              if (!is.na(nd) && !is.na(np) && nd >= np) "bg-success" else "bg-warning"),
        badge(i18n$t("dess_badge_raccorde"),
              if (is.na(raccorde)) "—" else if (isTRUE(raccorde)) i18n$t("dess_yes") else i18n$t("dess_no"),
              if (isTRUE(raccorde)) "bg-success" else "bg-warning"),
        badge(i18n$t("dess_badge_routes"),
              if (is.na(nroutes)) "—" else format(nroutes, big.mark = " "),
              if (!is.na(nroutes) && nroutes == 0L) "bg-success" else "bg-secondary"),
        badge(i18n$t("dess_badge_cout"),
              if (is.na(cout)) "—" else format(round(cout), big.mark = " ")),
        # Intégrité du réseau OBTENU (existant ∪ créé), spec 025. Complète
        # `raccorde`, qui ne dit que « les routes créées sont-elles rattachées ? »
        # et reste muet sur la cohérence du graphe résultant. Absent = contrôle
        # indisponible (dessertR injoignable), surtout PAS « 0 infraction ».
        if (is.null(integ)) {
          badge(i18n$t("dess_badge_integrite"), i18n$t("dess_integrite_na"),
                "bg-light text-dark")
        } else {
          htmltools::tagList(
            badge(i18n$t("dess_badge_infractions"),
                  format(integ$n_infractions, big.mark = " "),
                  if (isTRUE(integ$n_infractions == 0L)) "bg-success" else "bg-warning"),
            badge(i18n$t("dess_badge_orphelins"),
                  sprintf("%s / %s",
                          format(integ$n_composants_orphelins, big.mark = " "),
                          format(integ$n_composants, big.mark = " ")),
                  if (isTRUE(integ$n_composants_orphelins == 0L)) "bg-success" else "bg-warning"))
        },
        if (!is.na(nroutes) && nroutes == 0L) {
          htmltools::div(class = "alert alert-success py-2 small mt-2 mb-0",
                         i18n$t("dess_no_road_needed"))
        })
    })

    # --- Carte : fonds + parcelles + desserte existante + réseau créé (raster) -
    output$map <- leaflet::renderLeaflet({
      aoi <- units_sf()
      geo <- if (!is.null(aoi)) {
        tryCatch(sf::st_transform(aoi, 4326), error = function(e) NULL)
      }
      # Fond relief CVAT (overlay semi-transparent) quand un CVAT existe déjà pour
      # le projet — même helper que la carte Accessibilité.
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      cvat_bg <- .acc_cvat_overlay_raster(project_path)
      overlays <- c(if (!is.null(geo)) "Parcelles" else NULL,
                    if (!is.null(cvat_bg)) "Relief CVAT" else NULL,
                    "Desserte existante", "Reseau cree", "Reseau type",
                    "Places de depot")
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonCvatBase", zIndex = 230) |>
        leaflet::addMapPane("nemetonDessRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          overlayGroups = overlays,
          options = leaflet::layersControlOptions(collapsed = TRUE))
      if (!is.null(cvat_bg)) {
        grey <- leaflet::colorNumeric(grDevices::grey.colors(64, 0, 1),
          domain = c(0, 1), na.color = "transparent")
        m <- leaflet::addRasterImage(m, cvat_bg, colors = grey, opacity = 0.6,
          group = "Relief CVAT", maxBytes = 16 * 1024^2,
          options = leaflet::gridOptions(pane = "nemetonCvatBase"))
      }
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
    # --- Intégrité du réseau : worker dédié (376,8 s mesurés sur Dabo) ---------
    # Asynchrone obligatoirement : le typage voisin tourne en synchrone, ce qui
    # est tenable pour lui (quelques secondes) mais gèlerait toute l'app ici.
    integ_start <- shiny::reactiveVal(NULL)
    integ_task <- shiny::ExtendedTask$new(
      function(cache_dir, aoi_path, dev_path, app_opts) {
        if (requireNamespace("future", quietly = TRUE)) {
          pc <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% pc)) .ensure_async_plan()
        }
        promises::future_promise({
          on.exit(nemetonshiny:::.release_worker_memory(), add = TRUE)
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          nemetonshiny:::run_desserte_integrite(cache_dir, aoi_path)
        }, seed = TRUE)
      })
    bslib::bind_task_button(integ_task, "run_integrite")

    shiny::observeEvent(input$run_integrite, {
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(project_path)) {
        bslib::update_task_button("run_integrite", state = "ready")
        shiny::showNotification(i18n$t("dess_need_project"), type = "warning")
        return()
      }
      cache_dir <- .desserte_cache_dir(project_path)
      integ_start(Sys.time())
      shiny::showNotification(
        .running_notif_content(i18n$t("dess_integrite_running"), integ_start()),
        id = session$ns("integ_notif"), type = "message", duration = NULL)
      integ_task$invoke(cache_dir, file.path(cache_dir, "aoi_input.gpkg"),
                        .dev_pkg_path, get_app_options())
    })

    # Tick 1 s : chrono de la notif d'intégrité.
    shiny::observe({
      if (is.null(integ_start())) return()
      shiny::invalidateLater(1000)
      shiny::showNotification(
        .running_notif_content(i18n$t("dess_integrite_running"),
                               shiny::isolate(integ_start())),
        id = session$ns("integ_notif"), type = "message", duration = NULL)
    })

    shiny::observeEvent(integ_task$status(), {
      st <- integ_task$status()
      if (!st %in% c("success", "error")) return()
      integ_start(NULL)
      shiny::removeNotification(session$ns("integ_notif"))
      res <- tryCatch(integ_task$result(), error = function(e) {
        list(status = "error", reason = "desserte_integrite_failed")
      })
      if (!is.list(res) || !identical(res$status, "success")) {
        shiny::showNotification(i18n$t(res$reason %||% "desserte_integrite_failed"),
                                type = "error", duration = NULL)
        return()
      }
      # Réinjecte dans le résultat courant pour que les badges se rafraîchissent.
      cur <- rv$result
      if (is.list(cur)) { cur$integrite <- res$integrite; rv$result <- cur }
      shiny::showNotification(i18n$t("dess_integrite_done"), type = "message",
                              duration = 6)
    })

    output$integrite_status <- shiny::renderUI({
      res <- rv$result
      if (is.null(res) || !identical(res$status %||% "success", "success")) {
        return(htmltools::tags$p(class = "text-muted small",
                                 i18n$t("dess_integrite_hint")))
      }
      if (is.null(res$integrite)) {
        return(htmltools::tags$p(class = "text-muted small",
                                 i18n$t("dess_integrite_hint")))
      }
      NULL   # résultat rendu par les badges du bilan
    })

    # --- Optimisation et complément OSM : deux workers du même patron ---------
    # Facteur commun : action séparée + notif engrenage/chrono + sidecar relu.
    .async_panel <- function(id_btn, notif_id, label_key, invoke_fun, on_success) {
      start <- shiny::reactiveVal(NULL)
      task <- shiny::ExtendedTask$new(function(...) {
        args <- list(...)
        if (requireNamespace("future", quietly = TRUE)) {
          pc <- class(future::plan())
          if (!any(c("multisession", "multicore", "cluster") %in% pc)) .ensure_async_plan()
        }
        dev_path <- .dev_pkg_path; app_opts <- get_app_options()
        promises::future_promise({
          on.exit(nemetonshiny:::.release_worker_memory(), add = TRUE)
          if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
            pkgload::load_all(dev_path, quiet = TRUE)
          } else {
            loadNamespace("nemetonshiny")
          }
          options(nemeton.app_options = app_opts)
          do.call(invoke_fun, args)
        }, seed = TRUE)
      })
      bslib::bind_task_button(task, id_btn)
      shiny::observe({
        if (is.null(start())) return()
        shiny::invalidateLater(1000)
        shiny::showNotification(
          .running_notif_content(i18n$t(label_key), shiny::isolate(start())),
          id = session$ns(notif_id), type = "message", duration = NULL)
      })
      shiny::observeEvent(task$status(), {
        st <- task$status()
        if (!st %in% c("success", "error")) return()
        start(NULL)
        shiny::removeNotification(session$ns(notif_id))
        res <- tryCatch(task$result(), error = function(e) NULL)
        if (!is.list(res) || !identical(res$status, "success")) {
          shiny::showNotification(
            i18n$t(tryCatch(res$reason, error = function(e) NULL) %||%
                     "desserte_engine_failed"),
            type = "error", duration = NULL)
          return()
        }
        on_success(res)
      })
      list(task = task, start = start)
    }

    rv_optim <- shiny::reactiveVal(NULL)
    optim_panel <- .async_panel(
      "run_optim", "optim_notif", "dess_optim_running",
      function(...) nemetonshiny:::run_desserte_optimiser(...),
      function(res) {
        rv_optim(res)
        shiny::showNotification(i18n$t("dess_optim_done"), type = "message", duration = 6)
      })
    shiny::observeEvent(input$run_optim, {
      pp <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(pp)) {
        bslib::update_task_button("run_optim", state = "ready")
        shiny::showNotification(i18n$t("dess_need_project"), type = "warning"); return()
      }
      cd <- .desserte_cache_dir(pp)
      bm <- max(0, (suppressWarnings(as.numeric(input$buffer_km)) %||% 1)) * 1000
      sk <- suppressWarnings(as.numeric(input$skidding_m))
      if (!isTRUE(is.finite(sk)) || sk < 0) sk <- DESSERTE_SKIDDING_DEFAULT_M
      ns_ <- suppressWarnings(as.integer(input$optim_n_start))
      if (!isTRUE(is.finite(ns_)) || ns_ < 2L) ns_ <- DESSERTE_OPTIM_N_START
      optim_panel$start(Sys.time())
      shiny::showNotification(
        .running_notif_content(i18n$t("dess_optim_running"), optim_panel$start()),
        id = session$ns("optim_notif"), type = "message", duration = NULL)
      optim_panel$task$invoke(cd, file.path(cd, "aoi_input.gpkg"),
                              input$optim_strategie, ns_,
                              DESSERTE_OPTIM_N_ITER, bm, sk)
    })
    output$optim_result <- shiny::renderUI({
      r <- rv_optim() %||% tryCatch(
        .load_cached_optim(.desserte_cache_dir(app_state$current_project$path)),
        error = function(e) NULL)
      if (is.null(r)) {
        return(htmltools::tags$p(class = "text-muted small", i18n$t("dess_optim_hint")))
      }
      base <- suppressWarnings(as.numeric(rv$result$cout))
      gain <- if (is.finite(base) && base > 0 && is.finite(r$cout)) {
        sprintf(" (%+.1f %%)", 100 * (r$cout - base) / base)
      } else ""
      htmltools::div(class = "small",
        htmltools::tags$div(sprintf("%s : %s%s", i18n$t("dess_badge_cout"),
                                    format(round(r$cout), big.mark = " "), gain)),
        htmltools::tags$div(sprintf("%s : %s", i18n$t("dess_badge_routes"),
                                    format(r$n_routes, big.mark = " "))))
    })

    rv_osm <- shiny::reactiveVal(NULL)
    osm_panel <- .async_panel(
      "run_osm", "osm_notif", "dess_osm_running",
      function(...) nemetonshiny:::run_desserte_osm(...),
      function(res) {
        rv_osm(res)
        shiny::showNotification(sprintf(i18n$t("dess_osm_done_fmt"), res$n_osm),
                                type = "message", duration = 6)
      })
    shiny::observeEvent(input$run_osm, {
      pp <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(pp)) {
        bslib::update_task_button("run_osm", state = "ready")
        shiny::showNotification(i18n$t("dess_need_project"), type = "warning"); return()
      }
      cd <- .desserte_cache_dir(pp)
      bm <- max(0, (suppressWarnings(as.numeric(input$buffer_km)) %||% 1)) * 1000
      osm_panel$start(Sys.time())
      shiny::showNotification(
        .running_notif_content(i18n$t("dess_osm_running"), osm_panel$start()),
        id = session$ns("osm_notif"), type = "message", duration = NULL)
      osm_panel$task$invoke(cd, file.path(cd, "aoi_input.gpkg"), bm)
    })
    output$osm_result <- shiny::renderUI({
      r <- rv_osm() %||% tryCatch(
        .load_cached_osm(.desserte_cache_dir(app_state$current_project$path)),
        error = function(e) NULL)
      if (is.null(r)) {
        return(htmltools::tags$p(class = "text-muted small", i18n$t("dess_osm_hint")))
      }
      rows <- if (is.list(r$resume) && length(r$resume)) {
        lapply(names(r$resume), function(k) htmltools::tags$tr(
          htmltools::tags$td(class = "small", k),
          htmltools::tags$td(class = "small text-end",
                             format(r$resume[[k]], big.mark = " "))))
      } else NULL
      htmltools::tagList(
        htmltools::tags$div(class = "small mb-1",
                            sprintf(i18n$t("dess_osm_done_fmt"), r$n_osm)),
        if (!is.null(rows)) htmltools::tags$table(
          class = "table table-sm table-striped small mb-0",
          htmltools::tags$tbody(rows)))
    })

    rv_detect <- shiny::reactiveVal(NULL)
    detect_panel <- .async_panel(
      "run_detect", "detect_notif", "dess_detect_running",
      function(...) nemetonshiny:::run_desserte_detection(...),
      function(res) {
        rv_detect(res)
        shiny::showNotification(sprintf(i18n$t("dess_detect_done_fmt"), res$n_detecte),
                                type = "message", duration = 8)
      })
    shiny::observeEvent(input$run_detect, {
      pp <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      if (is.null(pp)) {
        bslib::update_task_button("run_detect", state = "ready")
        shiny::showNotification(i18n$t("dess_need_project"), type = "warning"); return()
      }
      cd <- .desserte_cache_dir(pp)
      bm <- max(0, (suppressWarnings(as.numeric(input$buffer_km)) %||% 1)) * 1000
      detect_panel$start(Sys.time())
      shiny::showNotification(
        .running_notif_content(i18n$t("dess_detect_running"), detect_panel$start()),
        id = session$ns("detect_notif"), type = "message", duration = NULL)
      detect_panel$task$invoke(cd, file.path(cd, "aoi_input.gpkg"), bm,
                               isTRUE(input$detect_lidar), pp)
    })
    output$detect_result <- shiny::renderUI({
      r <- rv_detect() %||% tryCatch(
        .load_cached_detection(.desserte_cache_dir(app_state$current_project$path)),
        error = function(e) NULL)
      if (is.null(r)) {
        return(htmltools::tags$p(class = "text-muted small", i18n$t("dess_detect_hint")))
      }
      htmltools::tagList(
        htmltools::tags$div(class = "small",
                            sprintf(i18n$t("dess_detect_done_fmt"), r$n_detecte)),
        # Sans canal de surface le cœur avertit que la détection est « nettement
        # moins sûre » : ne pas laisser lire un « 0 détection » comme un constat.
        if (!isTRUE(r$avec_lidar)) {
          htmltools::div(class = "alert alert-warning py-2 small mt-2 mb-0",
                         i18n$t("dess_detect_sans_lidar"))
        })
    })

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

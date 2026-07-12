#' nemetonApp Server
#'
#' @description
#' Main server function for the nemetonApp Shiny application.
#' Orchestrates all modules and manages application state.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return NULL (called for side effects)
#'
#' @noRd
app_server <- function(input, output, session) {

  # ============================================================
  # AUTHENTICATION
  # ============================================================

  auth_state <- mod_auth_server("auth")

  # ============================================================
  # ASYNC COMPUTATION SETUP
  # ============================================================

  # Ensure future::multisession is active for ExtendedTask async computation.
  # run_app() already sets this, but if the app is started via shiny::runApp()
  # or devtools::load_all(), the default plan is "sequential" which blocks the
  # Shiny main loop during computation (no UI updates, no progress polling).
  if (requireNamespace("future", quietly = TRUE)) {
    plan_classes <- class(future::plan())
    is_parallel <- any(c("multisession", "multicore", "cluster") %in% plan_classes)
    if (!is_parallel) {
      future::plan("multisession")
      cli::cli_alert_info("Switched to future::multisession for async computation")
    }
  }

  # PERF — pré-chauffe la pile géo (arrow/geoarrow/sf) ~1,5 s après le
  # démarrage, sur le thread principal mais HORS du chemin critique : la
  # page d'accueil est déjà rendue et l'utilisateur la parcourt. Quand il
  # clique enfin un projet récent, arrow est déjà chaud, ce qui retire le
  # ~1,6 s de chargement paresseux qui plombait le tout premier clic.
  # Idempotent (ne tourne qu'une fois par process R, cf. warmup_geo_stack).
  if (requireNamespace("later", quietly = TRUE)) {
    later::later(function() warmup_geo_stack(), delay = 1.5)
    # Pré-chauffe aussi les WORKERS future (chargement du namespace nemetonshiny
    # dans les process worker, ~5–6 s au tout 1er future) : sans ça, ce coût
    # frappe la 1re tâche async — souvent le db_sync du 1er projet ouvert, ou le
    # 1er calcul/moteur. Dans des process séparés → aucune compétition avec le
    # rendu principal, donc déclenché tôt (0,3 s) pour être chaud avant le clic.
    later::later(function() warmup_async_workers(), delay = 0.3)
  }

  # ============================================================
  # REACTIVE VALUES - Application State
  # ============================================================

  # App-wide state
  app_state <- shiny::reactiveValues(
    language = get_app_options()$language,
    project_dir = get_app_options()$project_dir,
    current_project = NULL,
    project_status = "none",  # none, draft, downloading, computing, completed
    samples_refresh = 0L,     # bumped by mod_sampling after save_samples()
                              # so mod_monitoring re-checks samples on disk
    comments_refresh = 0L,    # v0.52.9 — bumped by mod_synthesis /
                              # mod_family after save_comments() so
                              # mod_action_plan's plan_llm_context()
                              # re-reads from disk (sinon le contexte
                              # IA reste vide après un edit/AI dans
                              # Synthèse jusqu'au reload projet).
    # Auth carries `authenticated`, `user_name`, `user_email`,
    # `user_roles` (filled by mod_auth_server). Other modules use
    # `can_edit_action_plan(app_state$auth)` to decide permissions.
    auth = auth_state,
    # Project edit lock (serveur multi-utilisateurs) — driven by the lifecycle
    # observers below. `readonly` is the single flag modules read via
    # `project_is_readonly(app_state)`; `lock_info` carries the current holder
    # (for the banner) when someone else holds it.
    readonly = FALSE,
    lock_held_pid = NULL,
    lock_holder_id = NULL,
    lock_info = NULL,
    # Motif de la lecture seule pour le bandeau : "anonymous" (serveur OAuth,
    # pas encore loggué), "role" (rôle lecteur), "held" (verrou d'autrui).
    lock_reason = NULL
  )

  # Selection state
  selection_state <- shiny::reactiveValues(
    commune_code = NULL,
    commune_geometry = NULL,
    parcelles = NULL,
    selected_ids = character(0)
  )

  # Computation state
  compute_state <- shiny::reactiveValues(
    indicators = NULL,
    progress = 0,
    current_indicator = NULL,
    errors = list()
  )


  # ============================================================
  # STARTUP CONFIGURATION CHECKS
  # ============================================================

  # Warn the user once per session when required external services are not
  # configured (LLM API key, PostGIS database). These warnings surface silent
  # fallbacks (local storage) or features that will fail on use (AI analysis).
  #
  # session$onFlushed() fires outside a reactive consumer, so reading
  # app_state$language directly raises "Can't access reactive value ...
  # outside of reactive consumer". Wrap the read in shiny::isolate() --
  # we just need the current value, we don't need to react to language
  # changes (the notification only fires once per session anyway).
  session$onFlushed(function() {
    i18n <- get_i18n(shiny::isolate(app_state$language))

    provider <- get_app_config("llm_provider", "anthropic")
    key_var <- get_llm_api_key_var(provider)
    if (!is.null(key_var) && nchar(Sys.getenv(key_var)) == 0) {
      msg <- gsub("\\{key_var\\}", key_var, i18n$t("ai_no_api_key"))
      shiny::showNotification(msg, type = "warning", duration = 10)
    }

    if (!is_db_configured()) {
      shiny::showNotification(
        i18n$t("db_not_configured"),
        type = "warning",
        duration = 10
      )
    }
  }, once = TRUE)


  # ============================================================
  # LANGUAGE HANDLING
  # ============================================================

  # Update language when changed.
  # `app_ui` reads the language once from `getOption("nemeton.app_options")$language`
  # at session start, so static strings (navbar titles, button labels,
  # sidebar headers) only update on a fresh UI build. We therefore:
  #   1. persist the new language inside `nemeton.app_options` so the
  #      rebuilt UI on reload picks it up (the previous code wrote to
  #      `nemeton.app_language` — a different key — which left the
  #      reload reverting to the startup language);
  #   2. trigger an automatic page reload via `session$reload()` so
  #      the user does not have to refresh the browser manually.
  shiny::observeEvent(input$app_language, {
    new_lang <- input$app_language
    if (identical(new_lang, shiny::isolate(app_state$language))) return()
    app_state$language <- new_lang
    app_opts <- get_app_options()
    app_opts$language <- new_lang
    options(nemeton.app_options = app_opts)
    session$reload()
  })


  # ============================================================
  # GUIDED TOUR (now handled in mod_home)
  # ============================================================

  # Manual tour trigger
  shiny::observeEvent(input$show_help, {
    # Show help modal or restart tour
    shiny::showModal(
      shiny::modalDialog(
        title = get_i18n(app_state$language)$t("help"),
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton(get_i18n(app_state$language)$t("close")),

        htmltools::div(
          shiny::h4(get_i18n(app_state$language)$t("help_title")),
          shiny::p(get_i18n(app_state$language)$t("help_intro")),

          shiny::hr(),

          shiny::h5(get_i18n(app_state$language)$t("help_steps_title")),
          shiny::tags$ol(
            shiny::tags$li(get_i18n(app_state$language)$t("help_step1")),
            shiny::tags$li(get_i18n(app_state$language)$t("help_step2")),
            shiny::tags$li(get_i18n(app_state$language)$t("help_step3")),
            shiny::tags$li(get_i18n(app_state$language)$t("help_step4")),
            shiny::tags$li(get_i18n(app_state$language)$t("help_step5"))
          ),

          shiny::hr(),

          shiny::actionButton(
            "restart_tour",
            get_i18n(app_state$language)$t("tour_restart"),
            icon = shiny::icon("play"),
            class = "btn-outline-primary"
          ),

          shiny::hr(),

          shiny::p(
            shiny::tags$a(
              href = "https://github.com/pobsteta/nemeton",
              target = "_blank",
              get_i18n(app_state$language)$t("documentation_link")
            )
          )
        )
      )
    )
  })

  # Restart tour from help modal
  shiny::observeEvent(input$restart_tour, {
    shiny::removeModal()
    # Reset localStorage flag so tour can restart
    session$sendCustomMessage("resetTourSeen", list())
    # Delay to let modal close before starting tour
    later::later(function() {
      app_state$restart_tour <- Sys.time()
    }, delay = 0.5)
  })


  # ============================================================
  # TAB NAVIGATION CONTROL
  # ============================================================

  # Expose the active top-level tab so modules can gate heavy reactives on
  # their tab being visible. Notamment, mod_monitoring_pixel_map ne doit
  # PAS lancer `nemeton::build_index_stack` (scan de centaines de scènes
  # S2, plusieurs secondes à froid) tant que l'utilisateur n'est pas sur
  # l'onglet Suivi — sinon ce calcul bloque chaque chargement de projet
  # depuis l'Accueil.
  shiny::observeEvent(input$main_nav, {
    app_state$active_main_tab <- input$main_nav
  }, ignoreNULL = FALSE)

  # Redirect to selection tab only if user tries to navigate to
  # restricted tabs (synthesis, families) before project is completed.
  shiny::observeEvent(input$main_nav, {
    tab <- input$main_nav
    status <- app_state$project_status

    # Synthesis and family tabs require completed project
    restricted <- c("synthesis", grep("^famille_", tab, value = TRUE))
    if (tab %in% restricted && status != "completed") {
      shiny::updateNavbarPage(session, "main_nav", selected = "selection")
    }
  })


  # ============================================================
  # MODULE SERVERS
  # ============================================================

  # Store root session so child modules can navigate top-level tabs
  # (modules receive a namespaced session that cannot update main_nav directly)
  session$userData$root_session <- session

  # Theia / DATA TERRA configuration (navbar modal)
  mod_theia_config_server("theia_config", app_state)

  # Home module (search, map, project form)
  home_result <- mod_home_server("home", app_state)

  # Update selection state from home module
  shiny::observe({
    selection_state$commune_code <- home_result$selected_commune()
    selection_state$selected_ids <- if (!is.null(home_result$selected_parcels())) {
      home_result$selected_parcels()$id
    } else {
      character(0)
    }
  })

  # Synthesis module
  mod_synthesis_server("synthesis", app_state)

  # Action Plan module
  mod_action_plan_server("action_plan", app_state)

  # UG (Management Units) module
  mod_ug_server("ug", app_state)

  # Sampling / QField module (E5.a)
  mod_sampling_server("sampling", app_state)

  # Field ingest module (E5.b — QField return path)
  mod_field_ingest_server("field_ingest", app_state)

  # Monitoring module (E6.b phase 1 — Sentinel-2 continuous monitoring)
  mod_monitoring_server("monitoring", app_state)

  mod_regeneration_server("regeneration", app_state)

  # Note: the RAG / knowledge corpus admin (spec 009.2 — E7) now lives
  # INSIDE the settings (gear) modal, wired by mod_theia_config_server —
  # it is no longer a top-level navbar tab.

  # ============================================================
  # LAZY LOADING: Family modules initialized on-demand
  # ============================================================

  # Track which family modules have been initialized
  initialized_families <- shiny::reactiveVal(character(0))

  # Initialize family module only when user navigates to its tab
  shiny::observeEvent(input$main_nav, {
    tab <- input$main_nav
    if (is.null(tab)) return()

    # Vérifier si c'est un onglet famille (format NMT : "famille_*")
    if (grepl("^famille_", tab)) {
      # Reverse lookup : famille_carbone -> "C"
      famille_rev <- stats::setNames(names(FAMILLE_NMT_MAP), unname(FAMILLE_NMT_MAP))
      family_code <- famille_rev[[tab]]
      if (is.null(family_code)) return()
      already_init <- initialized_families()

      # Initialize only if not already done
      if (!(family_code %in% already_init)) {
        cli::cli_alert_info("Lazy loading family module: {family_code}")
        mod_family_server(tab, family_code, app_state)
        initialized_families(c(already_init, family_code))
      }
    }
  })

  # Note: Selection info, map, and compute button are now handled by mod_home


  # ============================================================
  # VERROU DE PROJET (serveur multi-utilisateurs)
  # ============================================================
  # Un projet ouvert est verrouillé en édition sur un seul utilisateur ; les
  # autres l'ouvrent en lecture seule. Cycle de vie centralisé ici : c'est le seul
  # endroit qui voit à la fois `app_state$project_id` (posé à toutes les entrées
  # d'ouverture, mod_home.R:443) et `app_state$auth`.

  # 1. Acquisition — un seul point de branchement couvre toutes les ouvertures.
  shiny::observeEvent(app_state$project_id, {
    pid <- app_state$project_id

    # Relâcher le verrou du projet PRÉCÉDENT (changement / fermeture de projet).
    prev <- shiny::isolate(app_state$lock_held_pid)
    if (!is.null(prev) && !identical(prev, pid)) {
      hid <- shiny::isolate(app_state$lock_holder_id)
      if (!is.null(hid)) try(lock_release(prev, hid), silent = TRUE)
      app_state$lock_held_pid <- NULL
      app_state$lock_holder_id <- NULL
    }
    if (is.null(pid)) { app_state$readonly <- FALSE; app_state$lock_info <- NULL; return() }

    # La lecture seule dépend du RÔLE, jamais de la présence d'un email.
    # `can_edit_action_plan()` est la convention de permission de l'app :
    #   - non authentifié (serveur OAuth, pas encore loggué) → lecture seule
    #   - rôle `lecteur` seul → lecture seule
    #   - anonyme sans rôle (mono-utilisateur / dev / admin local), `editeur`,
    #     `admin`, `proprietaire`… → éditeur.
    auth  <- shiny::isolate(app_state$auth)
    email <- tryCatch(auth[["user_email"]], error = function(e) NULL)
    name  <- tryCatch(auth[["user_name"]],  error = function(e) NULL)
    if (!isTRUE(can_edit_action_plan(auth))) {
      app_state$readonly <- TRUE
      app_state$lock_info <- NULL
      app_state$lock_reason <- if (isTRUE(tryCatch(auth[["authenticated"]],
                                                   error = function(e) FALSE)))
        "role" else "anonymous"
      return()
    }

    # Le verrou multi-utilisateurs n'a de sens qu'avec une identité STABLE
    # (email OAuth). Sans email — mono-utilisateur / dev / admin local / pas de
    # fournisseur d'identité — l'app est éditable, aucun verrou n'est posé en base.
    if (is.null(email) || !nzchar(email)) {
      app_state$readonly <- FALSE
      app_state$lock_held_pid <- NULL
      app_state$lock_holder_id <- NULL
      app_state$lock_info <- NULL
      app_state$lock_reason <- NULL
      return()
    }

    res <- tryCatch(lock_acquire(pid, email, name), error = function(e) NULL)
    i18n <- get_i18n(shiny::isolate(app_state$language))
    if (isTRUE(res$ok) || is.null(res)) {
      # `is.null(res)` = pas de DB (dev local) → éditable, pas de verrou.
      app_state$readonly <- FALSE
      app_state$lock_held_pid <- if (is.null(res)) NULL else pid
      app_state$lock_holder_id <- if (is.null(res)) NULL else email
      app_state$lock_info <- NULL
      app_state$lock_reason <- NULL
      if (isTRUE(res$stolen)) {
        shiny::showNotification(i18n$t("lock_stolen_notice"), type = "warning", duration = 8)
      }
    } else {
      # Tenu, frais, par un autre → lecture seule + bandeau.
      app_state$readonly <- TRUE
      app_state$lock_held_pid <- NULL
      app_state$lock_holder_id <- NULL
      app_state$lock_info <- res
      app_state$lock_reason <- "held"
      shiny::showNotification(
        sprintf(i18n$t("lock_readonly_notice"), res$holder_label %||% res$holder_id),
        type = "message", duration = 8)
    }
  }, ignoreNULL = FALSE)

  # 2. Heartbeat — tant qu'on tient le verrou, rafraîchir toutes les 45 s
  #    (TTL cœur 120 s → tolère 2 heartbeats manqués).
  shiny::observe({
    pid <- app_state$lock_held_pid
    if (is.null(pid)) return()
    shiny::invalidateLater(45000, session)
    hid <- shiny::isolate(app_state$lock_holder_id)
    ok <- tryCatch(lock_heartbeat(pid, hid), error = function(e) FALSE)
    if (!isTRUE(ok)) {
      # Verrou perdu (volé après péremption). Tenter de le reprendre, sinon
      # basculer en lecture seule.
      res <- tryCatch(lock_acquire(pid, hid, shiny::isolate(app_state$auth$user_name)),
                      error = function(e) list(ok = FALSE))
      if (!isTRUE(res$ok)) {
        app_state$readonly <- TRUE
        app_state$lock_held_pid <- NULL
        app_state$lock_info <- res
        app_state$lock_reason <- "held"
        i18n <- get_i18n(shiny::isolate(app_state$language))
        shiny::showNotification(i18n$t("lock_lost_notice"), type = "warning", duration = NULL)
      }
    }
  })

  # 3. Bandeau lecture seule — verrou d'autrui, rôle lecteur, ou non authentifié.
  output$lock_banner <- shiny::renderUI({
    if (!isTRUE(app_state$readonly)) return(NULL)
    if (is.null(app_state$project_id)) return(NULL)   # pas de projet ouvert
    i18n <- get_i18n(app_state$language)
    info <- app_state$lock_info
    reason <- app_state$lock_reason
    msg <- if (identical(reason, "held") && !is.null(info) && !is.null(info$holder_id)) {
      # Tenu par un autre utilisateur.
      sprintf(i18n$t("lock_readonly_banner"), info$holder_label %||% info$holder_id)
    } else if (identical(reason, "role")) {
      # Authentifié mais rôle lecteur.
      i18n$t("lock_role_banner")
    } else {
      # Non authentifié (serveur OAuth, pas encore loggué).
      i18n$t("lock_anonymous_banner")
    }
    htmltools::div(
      class = "alert alert-warning mb-0 rounded-0 py-2 px-3 d-flex align-items-center",
      bsicons::bs_icon("lock-fill", class = "me-2"),
      htmltools::span(msg)
    )
  })

  # ============================================================
  # SESSION CLEANUP
  # ============================================================

  session$onSessionEnded(function() {
    # `onSessionEnded` s'exécute HORS contexte réactif : capturer par isolate.
    # Best-effort — si l'onglet est tué, le TTL de 120 s reprend la main.
    pid <- shiny::isolate(app_state$lock_held_pid)
    hid <- shiny::isolate(app_state$lock_holder_id)
    if (!is.null(pid) && !is.null(hid)) try(lock_release(pid, hid), silent = TRUE)
    cli::cli_alert_info("nemetonApp session ended")
  })

}


# ============================================================
# HELPER FUNCTIONS
# ============================================================

#' Get departments list
#' @noRd
get_departments_list <- function() {
  # French departments with codes
  c(
    "01 - Ain" = "01",
    "02 - Aisne" = "02",
    "03 - Allier" = "03",
    "04 - Alpes-de-Haute-Provence" = "04",
    "05 - Hautes-Alpes" = "05",
    "06 - Alpes-Maritimes" = "06",
    "07 - Ard\u00e8che" = "07",
    "08 - Ardennes" = "08",
    "09 - Ari\u00e8ge" = "09",
    "10 - Aube" = "10",
    "11 - Aude" = "11",
    "12 - Aveyron" = "12",
    "13 - Bouches-du-Rh\u00f4ne" = "13",
    "14 - Calvados" = "14",
    "15 - Cantal" = "15",
    "16 - Charente" = "16",
    "17 - Charente-Maritime" = "17",
    "18 - Cher" = "18",
    "19 - Corr\u00e8ze" = "19",
    "21 - C\u00f4te-d'Or" = "21",
    "22 - C\u00f4tes-d'Armor" = "22",
    "23 - Creuse" = "23",
    "24 - Dordogne" = "24",
    "25 - Doubs" = "25",
    "26 - Dr\u00f4me" = "26",
    "27 - Eure" = "27",
    "28 - Eure-et-Loir" = "28",
    "29 - Finist\u00e8re" = "29",
    "2A - Corse-du-Sud" = "2A",
    "2B - Haute-Corse" = "2B",
    "30 - Gard" = "30",
    "31 - Haute-Garonne" = "31",
    "32 - Gers" = "32",
    "33 - Gironde" = "33",
    "34 - H\u00e9rault" = "34",
    "35 - Ille-et-Vilaine" = "35",
    "36 - Indre" = "36",
    "37 - Indre-et-Loire" = "37",
    "38 - Is\u00e8re" = "38",
    "39 - Jura" = "39",
    "40 - Landes" = "40",
    "41 - Loir-et-Cher" = "41",
    "42 - Loire" = "42",
    "43 - Haute-Loire" = "43",
    "44 - Loire-Atlantique" = "44",
    "45 - Loiret" = "45",
    "46 - Lot" = "46",
    "47 - Lot-et-Garonne" = "47",
    "48 - Loz\u00e8re" = "48",
    "49 - Maine-et-Loire" = "49",
    "50 - Manche" = "50",
    "51 - Marne" = "51",
    "52 - Haute-Marne" = "52",
    "53 - Mayenne" = "53",
    "54 - Meurthe-et-Moselle" = "54",
    "55 - Meuse" = "55",
    "56 - Morbihan" = "56",
    "57 - Moselle" = "57",
    "58 - Ni\u00e8vre" = "58",
    "59 - Nord" = "59",
    "60 - Oise" = "60",
    "61 - Orne" = "61",
    "62 - Pas-de-Calais" = "62",
    "63 - Puy-de-D\u00f4me" = "63",
    "64 - Pyr\u00e9n\u00e9es-Atlantiques" = "64",
    "65 - Hautes-Pyr\u00e9n\u00e9es" = "65",
    "66 - Pyr\u00e9n\u00e9es-Orientales" = "66",
    "67 - Bas-Rhin" = "67",
    "68 - Haut-Rhin" = "68",
    "69 - Rh\u00f4ne" = "69",
    "70 - Haute-Sa\u00f4ne" = "70",
    "71 - Sa\u00f4ne-et-Loire" = "71",
    "72 - Sarthe" = "72",
    "73 - Savoie" = "73",
    "74 - Haute-Savoie" = "74",
    "75 - Paris" = "75",
    "76 - Seine-Maritime" = "76",
    "77 - Seine-et-Marne" = "77",
    "78 - Yvelines" = "78",
    "79 - Deux-S\u00e8vres" = "79",
    "80 - Somme" = "80",
    "81 - Tarn" = "81",
    "82 - Tarn-et-Garonne" = "82",
    "83 - Var" = "83",
    "84 - Vaucluse" = "84",
    "85 - Vend\u00e9e" = "85",
    "86 - Vienne" = "86",
    "87 - Haute-Vienne" = "87",
    "88 - Vosges" = "88",
    "89 - Yonne" = "89",
    "90 - Territoire de Belfort" = "90",
    "91 - Essonne" = "91",
    "92 - Hauts-de-Seine" = "92",
    "93 - Seine-Saint-Denis" = "93",
    "94 - Val-de-Marne" = "94",
    "95 - Val-d'Oise" = "95",
    "971 - Guadeloupe" = "971",
    "972 - Martinique" = "972",
    "973 - Guyane" = "973",
    "974 - La R\u00e9union" = "974",
    "976 - Mayotte" = "976"
  )
}


#' Get tour preference
#' @noRd
get_tour_preference <- function() {
  # Check if tour was already seen (stored in options)
  getOption("nemeton.tour_seen", FALSE)
}


# Tour is now handled in mod_home_server

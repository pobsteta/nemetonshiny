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

  # ============================================================
  # REACTIVE VALUES - Application State
  # ============================================================

  # App-wide state
  app_state <- shiny::reactiveValues(
    language = get_app_options()$language,
    project_dir = get_app_options()$project_dir,
    current_project = NULL,
    project_status = "none"  # none, draft, downloading, computing, completed
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
  # LANGUAGE HANDLING
  # ============================================================

  # Update language when changed
  shiny::observeEvent(input$app_language, {
    app_state$language <- input$app_language
    # Store preference
    options(nemeton.app_language = input$app_language)
    # Refresh UI would require page reload
    shiny::showNotification(
      get_i18n(input$app_language)$t("language_changed"),
      type = "message"
    )
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

  # UG (Management Units) module
  mod_ug_server("ug", app_state)

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
  # SESSION CLEANUP
  # ============================================================

  session$onSessionEnded(function() {
    # Cleanup any background jobs if needed
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

#' Home Module for nemetonApp
#'
#' @description
#' Main home/selection page module that integrates:
#' - Recent projects list
#' - Commune search
#' - Map with parcel selection
#' - Project creation form
#'
#' @name mod_home
#' @keywords internal
NULL


#' Home Module UI
#'
#' @param id Character. Module namespace ID.
#'
#' @return Shiny UI elements.
#'
#' @noRd
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  # Get translations
  opts <- get_app_options()
  lang <- opts$language %||% "fr"
  i18n <- get_i18n(lang)

  bslib::layout_sidebar(
    fillable = TRUE,

    # ========================================
    # Sidebar: Search + Project Form
    # ========================================
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      width = 350,
      open = TRUE,

      # Recent Projects Section (collapsible)
      htmltools::tags$div(
        id = ns("recent_projects_section"),
        class = "card mb-3",
        htmltools::tags$div(
          class = "card-header bg-secondary text-white py-2",
          style = "cursor: pointer;",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", ns("recent_collapse")),
          `aria-expanded` = "true",
          `aria-controls` = ns("recent_collapse"),
          htmltools::div(
            class = "d-flex align-items-center justify-content-between",
            htmltools::div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("clock-history", class = "me-2"),
              i18n$t("recent_projects")
            ),
            bsicons::bs_icon("chevron-down", class = "collapse-icon")
          )
        ),
        htmltools::tags$div(
          id = ns("recent_collapse"),
          class = "collapse show",
          htmltools::tags$div(
            class = "card-body p-2",
            shiny::uiOutput(ns("recent_projects_list"))
          )
        )
      ),

      # Search Section (collapsible)
      htmltools::tags$div(
        class = "card mb-3",
        htmltools::tags$div(
          class = "card-header bg-primary text-white py-2",
          style = "cursor: pointer;",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", ns("search_collapse")),
          `aria-expanded` = "true",
          `aria-controls` = ns("search_collapse"),
          htmltools::div(
            class = "d-flex align-items-center justify-content-between",
            htmltools::div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("search", class = "me-2"),
              i18n$t("search_commune")
            ),
            bsicons::bs_icon("chevron-down", class = "collapse-icon")
          )
        ),
        htmltools::tags$div(
          id = ns("search_collapse"),
          class = "collapse show",
          htmltools::tags$div(
            class = "card-body",
            mod_search_ui(ns("search"))
          )
        )
      ),

      htmltools::hr(class = "my-3"),

      # Project Form Section (shown after parcel selection)
      htmltools::div(
        id = ns("project_form_section"),
        mod_project_ui(ns("project"))
      ),

      htmltools::hr(class = "my-3"),

      # Compute Button Section (shown when project exists)
      htmltools::div(
        id = ns("compute_section"),
        shiny::uiOutput(ns("compute_button_ui"))
      ),

      # Progress Module (shown during computation)
      mod_progress_ui(ns("progress")),

      htmltools::hr(class = "my-3"),

      # Carte UGF actions (map-based + global actions)
      htmltools::tags$div(
        id = ns("ug_map_actions_section"),
        class = "card mb-3",
        htmltools::tags$div(
          class = "card-header bg-success text-white py-2",
          style = "cursor: pointer;",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", ns("ug_map_actions_collapse")),
          `aria-expanded` = "false",
          `aria-controls` = ns("ug_map_actions_collapse"),
          htmltools::div(
            class = "d-flex align-items-center justify-content-between",
            htmltools::div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("diagram-3", class = "me-2"),
              i18n$t("ug_sidebar_title")
            ),
            bsicons::bs_icon("chevron-down", class = "collapse-icon")
          )
        ),
        htmltools::tags$div(
          id = ns("ug_map_actions_collapse"),
          class = "collapse",
          htmltools::tags$div(
            class = "card-body p-2",
            mod_ug_map_actions_bar("ug")
          )
        )
      ),

      # Tableau UGF actions (table-based actions: merge, split, rename, groupe)
      htmltools::tags$div(
        id = ns("ug_table_actions_section"),
        class = "card mb-3",
        htmltools::tags$div(
          class = "card-header bg-info text-white py-2",
          style = "cursor: pointer;",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", ns("ug_table_actions_collapse")),
          `aria-expanded` = "false",
          `aria-controls` = ns("ug_table_actions_collapse"),
          htmltools::div(
            class = "d-flex align-items-center justify-content-between",
            htmltools::div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("table", class = "me-2"),
              i18n$t("ug_table_sidebar_title")
            ),
            bsicons::bs_icon("chevron-down", class = "collapse-icon")
          )
        ),
        htmltools::tags$div(
          id = ns("ug_table_actions_collapse"),
          class = "collapse",
          htmltools::tags$div(
            class = "card-body p-2",
            mod_ug_table_actions_bar("ug")
          )
        )
      )
    ),

    # ========================================
    # Main: Tabbed maps + UG table
    # ========================================
    bslib::navset_card_tab(
      id = ns("main_tabs"),
      bslib::nav_panel(
        title = i18n$t("tab_carte_cadastrale"),
        icon = bsicons::bs_icon("geo-alt"),
        value = "cadastral",
        mod_map_ui(ns("map"))
      ),
      bslib::nav_panel(
        title = i18n$t("tab_carte_tenements"),
        icon = bsicons::bs_icon("diagram-3"),
        value = "tenements",
        mod_ug_map_panel("ug")
      ),
      bslib::nav_panel(
        title = i18n$t("tab_tableau_ug"),
        icon = bsicons::bs_icon("table"),
        value = "table_ug",
        mod_ug_table_panel("ug")
      )
    )
  )
}


#' Home Module Server
#'
#' @param id Character. Module namespace ID.
#' @param app_state Reactive values. Application state.
#'
#' @return List with reactive values for project and selection state.
#'
#' @noRd
mod_home_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get translations
    opts <- get_app_options()
    lang <- opts$language %||% "fr"
    i18n <- get_i18n(lang)

    # ========================================
    # Recent Projects
    # ========================================

    output$recent_projects_list <- shiny::renderUI({
      # Refresh when requested or when active project changes
      app_state$refresh_projects
      current_id <- app_state$project_id

      projects <- list_recent_projects(limit = 50)

      if (nrow(projects) == 0) {
        return(htmltools::div(
          class = "text-muted small fst-italic",
          i18n$t("no_recent_projects")
        ))
      }

      # Create project cards
      project_items <- lapply(seq_len(nrow(projects)), function(i) {
        proj <- projects[i, ]

        # Check if this is the currently loaded project
        is_active <- !is.null(current_id) && identical(proj$id, current_id)

        # Determine status badge color
        status_class <- switch(
          proj$status,
          "completed" = "bg-success",
          "computing" = "bg-warning",
          "downloading" = "bg-info",
          "error" = "bg-danger",
          "bg-secondary"
        )

        # Corrupted project styling
        card_class <- if (proj$is_corrupted) {
          "border-danger"
        } else if (is_active) {
          "border-primary bg-light"
        } else {
          "border-light"
        }

        icon <- if (proj$is_corrupted) {
          bsicons::bs_icon("exclamation-triangle", class = "text-danger me-1")
        } else if (is_active) {
          bsicons::bs_icon("folder-fill", class = "text-primary me-1")
        } else {
          bsicons::bs_icon("folder", class = "text-muted me-1")
        }

        # Disable click on active project
        onclick_handler <- if (is_active) {
          NULL
        } else if (proj$is_corrupted) {
          sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'});",
                  ns("delete_corrupted"), proj$id)
        } else {
          sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
            ns("load_project"), proj$id
          )
        }

        cursor_class <- if (is_active) "project-card-active" else "cursor-pointer project-card"

        htmltools::div(
          class = paste("card mb-2", cursor_class, card_class),
          id = ns(paste0("project_", proj$id)),
          `data-project-id` = proj$id,
          `data-corrupted` = tolower(as.character(proj$is_corrupted)),
          style = if (is_active) "opacity: 0.7; pointer-events: none;" else NULL,
          onclick = onclick_handler,

          htmltools::div(
            class = "card-body py-2 px-3",
            htmltools::div(
              class = "d-flex justify-content-between align-items-start",
              htmltools::div(
                htmltools::div(
                  class = "fw-semibold small",
                  icon,
                  proj$name
                ),
                htmltools::div(
                  class = "text-muted smaller",
                  sprintf("%d %s", proj$parcels_count,
                    if (proj$parcels_count <= 1) i18n$t("parcel") else i18n$t("parcels"))
                )
              ),
              if (is_active) {
                htmltools::span(
                  class = "badge bg-primary smaller",
                  i18n$t("active") %||% "Actif"
                )
              } else {
                htmltools::span(
                  class = paste("badge", status_class, "smaller"),
                  if (proj$is_corrupted) i18n$t("corrupted") else i18n$t(paste0("status_", proj$status))
                )
              }
            )
          )
        )
      })

      htmltools::div(
        style = "max-height: 240px; overflow-y: auto;",
        project_items
      )
    })

    # ========================================
    # Load Project Handler
    # ========================================

    shiny::observe({
      project_id <- input$load_project
      shiny::req(project_id)

      # Skip if this project is already loaded
      if (!is.null(app_state$project_id) && identical(app_state$project_id, project_id)) {
        return()
      }

      # Immediate feedback: show loading notification (removed later
      # in the flow once the project is fully loaded and rendered).
      shiny::showNotification(
        htmltools::tagList(
          shiny::icon("spinner", class = "fa-spin me-2"),
          sprintf("%s...", i18n$t("project_loading"))
        ),
        type = "message",
        duration = NULL,
        id = "project_loading_notif",
        session = session
      )

      # Load the project from disk (sync PostGIS si configure)
      project <- load_project(project_id)
      shiny::req(project)

      # Notification sync PostGIS au chargement
      if (is_db_configured() && isTRUE(project$metadata$indicators_computed)) {
        db_msg <- if (i18n$language == "fr") {
          "Projet synchronis\u00e9 avec la base PostGIS"
        } else {
          "Project synced to PostGIS database"
        }
        shiny::showNotification(
          htmltools::tagList(shiny::icon("database", class = "me-1"), db_msg),
          type = "message", duration = 5, session = session
        )
      }

      # Update app state with loaded project
      app_state$current_project <- project
      app_state$project_id <- project$id
      app_state$project_status <- project$metadata$status %||% "draft"

      # Extract commune code from parcels if available
      if (!is.null(project$parcels) && nrow(project$parcels) > 0 &&
          "code_insee" %in% names(project$parcels)) {
        commune_code <- unique(project$parcels$code_insee)[1]

        # Guard against NA/empty commune code (corrupted project data)
        if (!is.na(commune_code) && nzchar(commune_code)) {
          # Extract department from commune code (first 2 or 3 chars)
          dept_code <- if (grepl("^97", commune_code)) {
            substr(commune_code, 1, 3)  # DOM-TOM
          } else {
            substr(commune_code, 1, 2)
          }

          # Flag for other modules to detect restore-in-progress
          # Cleared by mod_search.R's restore_task result handler
          app_state$restore_in_progress <- TRUE

          # NOTE: Do NOT set app_state$clear_map_selection here.
          # It runs in the same expression as parcels_data() and
          # restore_project, so all downstream observers fire in
          # the same flush in non-deterministic order. If the
          # clear_map_selection observer fires AFTER the combined
          # observer restores selection, it wipes rv$selected_ids.
          # The combined observer already calls clearGroup("selection")
          # every time it re-renders, so cleanup is handled there.

          # Set parcels directly from stored project data — no API call needed.
          # This makes parcels() available immediately for mod_map.R's restore
          # observer, avoiding the slow async chain (restore_task → commune_task
          # → parcels_task) which could get stuck or take several seconds.
          parcels_data(project$parcels)

          # Signal to restore location and parcels
          app_state$restore_project <- list(
            commune_code = commune_code,
            department_code = dept_code,
            parcels = project$parcels,
            selected_ids = project$parcels$id,  # All saved parcels were selected
            timestamp = Sys.time()  # Force reactivity
          )
        } else {
          cli::cli_warn("Project {project$id}: invalid commune code in parcels")
          session$sendCustomMessage("showMapLoading", list(
            loadingId = ns("map-map_loading_overlay"),
            show = FALSE
          ))
        }
      }

      # Replace loading notification with success
      shiny::showNotification(
        htmltools::tagList(
          shiny::icon("check-circle", class = "me-2"),
          sprintf("%s: %s", i18n$t("project_loaded"),
                  project$metadata$name %||% project$id)
        ),
        type = "message",
        duration = 5,
        id = "project_loading_notif",
        session = session
      )

      # If project has indicators, stay on Selection tab so the user
      # can see parcels on the map first. The "Voir les résultats" button
      # lets them navigate to Synthesis when ready.
    }) |> shiny::bindEvent(input$load_project)

    # ========================================
    # Delete Corrupted Project Handler
    # ========================================

    shiny::observe({
      project_id <- input$delete_corrupted
      shiny::req(project_id)

      # Show confirmation modal
      shiny::showModal(shiny::modalDialog(
        title = htmltools::div(
          class = "text-danger",
          bsicons::bs_icon("exclamation-triangle", class = "me-2"),
          i18n$t("delete_corrupted_project")
        ),
        htmltools::p(i18n$t("delete_corrupted_confirm")),
        footer = htmltools::tagList(
          shiny::modalButton(i18n$t("cancel")),
          shiny::actionButton(
            ns("confirm_delete"),
            i18n$t("delete"),
            class = "btn-danger",
            `data-project-id` = project_id
          )
        )
      ))
    }) |> shiny::bindEvent(input$delete_corrupted)

    shiny::observe({
      # Get project ID from button attribute
      project_id <- input$delete_corrupted

      if (delete_project(project_id)) {
        shiny::removeModal()
        shiny::showNotification(
          i18n$t("project_deleted"),
          type = "message"
        )

        # Refresh project list
        app_state$refresh_projects <- Sys.time()
      }
    }) |> shiny::bindEvent(input$confirm_delete)

    # ========================================
    # Search Module
    # ========================================

    search_result <- mod_search_server("search", app_state)

    # ========================================
    # Cadastre Parcels
    # ========================================

    # ExtendedTask for parcel loading (non-blocking)
    parcels_task <- shiny::ExtendedTask$new(function(commune, commune_geom) {
      if (requireNamespace("future", quietly = TRUE)) {
        plan_classes <- class(future::plan())
        is_parallel <- any(c("multisession", "multicore", "cluster") %in% plan_classes)
        if (!is_parallel) future::plan("multisession")
      }
      promises::future_promise({
        if (!is.null(.pkg_path) && requireNamespace("pkgload", quietly = TRUE)) {
          pkgload::load_all(.pkg_path, quiet = TRUE)
        } else if (requireNamespace("nemeton", quietly = TRUE)) {
          loadNamespace("nemeton")
        }
        get_cadastral_parcels(commune, commune_geom)
      }, seed = TRUE)
    })

    # Hold parcel data in a reactiveVal (updated by task result observer)
    parcels_data <- shiny::reactiveVal(NULL)

    # Invoke parcels task when commune is selected
    shiny::observeEvent(search_result$selected_commune(), {
      commune <- search_result$selected_commune()
      if (is.null(commune) || commune == "") {
        parcels_data(NULL)
        return()
      }

      # During project restore, parcels are set directly — skip API call
      if (isTRUE(app_state$restore_in_progress)) return()

      commune_geom <- search_result$commune_geometry()
      shiny::req(commune_geom)

      # Clear stale parcels while loading new ones.
      # Also clear the transitioning flag so the combined observer in mod_map
      # can render once fresh parcels arrive.
      parcels_data(NULL)
      app_state$commune_transitioning <- FALSE
      parcels_task$invoke(commune, commune_geom)
    }, ignoreInit = TRUE)

    # Handle parcels task result
    # Use observe() + tryCatch for proper ExtendedTask error handling.
    # observeEvent(result()) silently swallows task errors because result()
    # re-raises the error in the event expression, preventing the handler
    # from ever executing.
    shiny::observe({
      result <- tryCatch(
        parcels_task$result(),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          shiny::showNotification(
            sprintf("%s: %s", i18n$t("error_loading_parcels"), e$message),
            type = "error"
          )
          NULL
        }
      )
      parcels_data(result)
      is_restoring <- isTRUE(shiny::isolate(app_state$restore_in_progress))
      if (!is.null(result) && nrow(result) > 0 && !is_restoring) {
        shiny::showNotification(
          sprintf("%d %s", nrow(result), i18n$t("parcels_loaded")),
          type = "message",
          duration = 3
        )
      }
    })

    # Parcels reactive (consumed by mod_map_server)
    parcels <- shiny::reactive(parcels_data())

    # ========================================
    # Map Module
    # ========================================

    map_result <- mod_map_server(
      "map",
      app_state = app_state,
      commune_geometry = search_result$commune_geometry,
      parcels = parcels
    )

    # ========================================
    # Project Module
    # ========================================

    project_result <- mod_project_server(
      "project",
      app_state = app_state,
      selected_parcels = map_result$selected_parcels
    )

    # ========================================
    # Compute Button
    # ========================================

    output$compute_button_ui <- shiny::renderUI({
      project <- app_state$current_project
      if (is.null(project)) return(NULL)

      # Hide button while computation is running
      if (!is.null(computing_project_id())) return(NULL)

      # Check project status
      status <- project$metadata$status %||% "draft"

      # Show button only for draft status (not yet computed)
      if (status %in% c("draft", "error")) {
        shiny::actionButton(
          ns("start_compute"),
          label = i18n$t("compute_button"),
          class = "btn-primary w-100",
          icon = bsicons::bs_icon("cpu")
        )
      } else if (status == "completed") {
        # Show "view results" button
        htmltools::div(
          class = "d-grid gap-2",
          shiny::actionButton(
            ns("view_results"),
            label = i18n$t("view_results"),
            class = "btn-success w-100",
            icon = bsicons::bs_icon("bar-chart")
          ),
          shiny::actionButton(
            ns("recompute"),
            label = i18n$t("retry"),
            class = "btn-outline-secondary btn-sm w-100 mt-2",
            icon = bsicons::bs_icon("arrow-repeat")
          )
        )
      } else {
        NULL
      }
    })

    # ========================================
    # Progress Module (ExtendedTask for async computation)
    # ========================================

    # Computation state (reactive)
    compute_state <- shiny::reactiveVal(NULL)

    # Track active computation project
    computing_project_id <- shiny::reactiveVal(NULL)

    # Progress module server
    progress_result <- mod_progress_server(
      "progress",
      compute_state = compute_state,
      app_state = app_state
    )

    # ========================================
    # Reset state when commune or department changes
    # ========================================

    # Helper to clear all project/computation state
    reset_project_state <- function() {
      # Clear current project
      app_state$current_project <- NULL
      app_state$project_id <- NULL

      # Clear all comments (synthesis + families)
      app_state$family_comments <- list()
      app_state$clear_all_comments <- Sys.time()

      # Reset computation state (setting computing_project_id to NULL
      # also stops the later::later polling loop)
      compute_state(NULL)
      computing_project_id(NULL)
      poll_last_key <<- ""
      progress_result$reset_tracking()

      # Stop elapsed timer and computing mode
      session$sendCustomMessage("stopElapsedTimer", list())
      session$sendCustomMessage("setComputingMode", list(active = FALSE))

      # Hide all progress/completion/error cards
      session$sendCustomMessage("hideElement", list(
        id = ns("progress-progress_card_wrapper")
      ))
      session$sendCustomMessage("hideElement", list(
        id = ns("progress-complete_card_wrapper")
      ))
      session$sendCustomMessage("hideElement", list(
        id = ns("progress-error_card_wrapper")
      ))

      # Clear map selection
      app_state$clear_map_selection <- Sys.time()
    }

    # When the user selects a different commune, reset project state.
    # No need for a department_changed observer: when the department
    # changes, the commune dropdown is cleared asynchronously, which
    # triggers this observer. This avoids touching the map prematurely
    # on department selection (the map stays unchanged until the commune
    # dropdown is actually updated).
    shiny::observeEvent(search_result$selected_commune(), {
      if (isTRUE(app_state$restore_in_progress)) return()
      reset_project_state()
    }, ignoreInit = TRUE)

    # Create ExtendedTask for async computation
    # Uses future_promise() to run in a separate R process via future::multisession
    # This prevents blocking the Shiny main loop during computation
    #
    # The future worker is a fresh R process that doesn't have nemeton loaded.
    # We detect the package source path here (main process) and load it in the worker.
    # This works both in dev mode (devtools::load_all) and production (installed pkg).
    .pkg_path <- tryCatch(pkgload::pkg_path(), error = function(e) NULL)
    compute_task <- shiny::ExtendedTask$new(function(project_id, app_opts) {
      # Ensure multisession plan is active before creating the future.
      # Without this, the future runs sequentially in the main process,
      # blocking the Shiny event loop (no UI updates, no progress polling).
      if (requireNamespace("future", quietly = TRUE)) {
        plan_classes <- class(future::plan())
        is_parallel <- any(c("multisession", "multicore", "cluster") %in% plan_classes)
        if (!is_parallel) {
          future::plan("multisession")
        }
      }
      promises::future_promise({
        # Load nemeton package in the worker process
        if (!is.null(.pkg_path) && requireNamespace("pkgload", quietly = TRUE)) {
          # Dev mode: reload from source directory
          pkgload::load_all(.pkg_path, quiet = TRUE)
        } else {
          # Production: attach package so all exports are on the search path
          library(nemeton, quietly = TRUE)
        }

        # Restore app options in the future process
        # (needed by get_project_path and other functions)
        options(nemeton.app_options = app_opts)

        # This runs in a separate R process
        # project_path is resolved internally by start_computation() via
        # get_project_path(), which works because app_opts are restored above.
        start_computation(
          project_id = project_id,
          indicators = "all",
          progress_callback = NULL,
          use_file_progress = TRUE
        )
      }, seed = TRUE)
    })

    # ========================================
    # Resume progress tracking on project load
    # ========================================
    # When a project is loaded, check if computation is in progress
    shiny::observeEvent(app_state$current_project, {
      project <- app_state$current_project
      if (is.null(project)) return()

      # Don't interfere if we already have an active computation
      if (!is.null(computing_project_id())) return()

      # Check if there's an ongoing computation for this project
      progress_state <- read_progress_state(project$id)

      if (!is.null(progress_state) &&
          progress_state$status %in% c("downloading", "computing")) {

        cli::cli_alert_info("Resuming progress tracking for project {project$id}")

        # Set computing project ID and start non-reactive polling
        computing_project_id(project$id)
        progress_result$reset_tracking()
        start_progress_polling(project$id)

        # Push initial state to UI directly (no reactive flush)
        progress_result$send_running_update(progress_state)

        # Suppress busy bar flicker during computation
        session$sendCustomMessage("setComputingMode", list(active = TRUE))

        # Show progress card
        session$sendCustomMessage("showElement", list(
          id = ns("progress-progress_card_wrapper")
        ))

        # Hide completion/error cards
        session$sendCustomMessage("hideElement", list(
          id = ns("progress-complete_card_wrapper")
        ))
        session$sendCustomMessage("hideElement", list(
          id = ns("progress-error_card_wrapper")
        ))
      }
    }, ignoreInit = TRUE)

    # Start computation handler - show confirmation modal (T088)
    shiny::observeEvent(input$start_compute, {
      project <- app_state$current_project
      shiny::req(project)

      # Get counts for the confirmation dialog — indicators are
      # computed on UGF geometries, so the UGF count is what really
      # drives runtime. Keep the cadastral count for context.
      n_parcels <- if (!is.null(project$parcels)) nrow(project$parcels) else 0
      n_ugs     <- if (!is.null(project$ugs))     nrow(project$ugs)     else 0

      # Show confirmation modal
      shiny::showModal(shiny::modalDialog(
        title = i18n$t("confirm_computation_title"),
        htmltools::div(
          htmltools::p(i18n$t("confirm_computation_message")),
          htmltools::tags$ul(
            htmltools::tags$li(
              htmltools::strong(i18n$t("project_name"), ": "),
              project$metadata$name %||% project$id
            ),
            htmltools::tags$li(
              htmltools::strong("Unit\u00e9s de Gestion Foresti\u00e8res : "),
              n_ugs
            ),
            htmltools::tags$li(
              htmltools::strong("Parcelles cadastrales : "),
              n_parcels
            )
          ),
          htmltools::p(
            class = "text-warning",
            bsicons::bs_icon("exclamation-triangle", class = "me-1"),
            i18n$t("confirm_computation_warning")
          )
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n$t("cancel")),
          shiny::actionButton(
            ns("confirm_compute"),
            label = i18n$t("start_computation"),
            class = "btn-primary",
            icon = bsicons::bs_icon("cpu")
          )
        ),
        size = "m",
        easyClose = TRUE
      ))
    })

    # Confirmed computation handler
    shiny::observeEvent(input$confirm_compute, {
      shiny::removeModal()

      project <- app_state$current_project
      shiny::req(project)

      # Block parcel modification (T089)
      app_state$computation_running <- TRUE

      # Get project path for async mode
      project_path <- get_project_path(project$id)

      # Initialize computation state
      state <- init_compute_state(project$id)
      compute_state(state)

      # Write initial "pending" state to progress file BEFORE launching
      # the async future. Without this, the polling observer reads the
      # stale progress file (e.g. status="error" from a previous run)
      # and immediately stops polling, preventing any UI updates.
      save_progress_state(project$id, state)

      # Store project ID and start non-reactive polling
      computing_project_id(project$id)
      progress_result$reset_tracking()
      start_progress_polling(project$id)

      # Suppress busy bar flicker during computation
      session$sendCustomMessage("setComputingMode", list(active = TRUE))

      # Show progress card immediately
      session$sendCustomMessage("showElement", list(
        id = ns("progress-progress_card_wrapper")
      ))

      # Hide completion/error cards if visible
      session$sendCustomMessage("hideElement", list(
        id = ns("progress-complete_card_wrapper")
      ))
      session$sendCustomMessage("hideElement", list(
        id = ns("progress-error_card_wrapper")
      ))

      # Start the async computation with project_id and app options
      # App options are passed explicitly because future runs in a separate R process
      cli::cli_alert_info("Starting computation for project {project$id}")
      compute_task$invoke(project$id, get_app_options())
    })

    # Watch for ExtendedTask errors (handles failures before progress file is written)
    shiny::observe({
      # Only check when we have an active computation
      project_id <- computing_project_id()
      shiny::req(project_id)

      # Get ExtendedTask status
      task_status <- compute_task$status()

      if (task_status == "error") {
        cli::cli_alert_danger("ExtendedTask failed")

        # Try to read the final state from progress file
        progress_state <- read_progress_state(project_id)
        if (!is.null(progress_state)) {
          compute_state(progress_state)
        }

        # Unblock parcel modification (T089)
        app_state$computation_running <- FALSE

        # End computing mode, hide progress card, show error card
        session$sendCustomMessage("setComputingMode", list(active = FALSE))
        session$sendCustomMessage("hideElement", list(
          id = ns("progress-progress_card_wrapper")
        ))
        session$sendCustomMessage("showElement", list(
          id = ns("progress-error_card_wrapper")
        ))

        # Try to get the error message from task result or progress state
        error_msg <- tryCatch({
          result <- compute_task$result()
          if (inherits(result, "error")) {
            result$message
          } else if (!is.null(progress_state) && length(progress_state$errors) > 0) {
            last_error <- progress_state$errors[[length(progress_state$errors)]]
            last_error$message %||% "Unknown error"
          } else {
            "Unknown error"
          }
        }, error = function(e) e$message)

        shiny::showNotification(
          paste("Erreur de calcul:", error_msg),
          type = "error",
          duration = 10
        )

        # Reset computing state
        computing_project_id(NULL)
      }
    })

    # ========================================
    # Progress polling via later::later (non-reactive)
    #
    # Uses later::later instead of invalidateLater to avoid triggering
    # Shiny's reactive flush cycle every 2 seconds. The flush sets
    # data-shiny-busy on <html>, which activates bslib's white overlay
    # despite CSS/JS suppression (timing gap in MutationObserver).
    #
    # Running-state updates go directly through sendCustomMessage via
    # progress_result$send_running_update() — no reactive values change,
    # no busy state, no white flash.
    #
    # Terminal states (completed, error, cancelled) update compute_state()
    # for a single reactive flush, which is acceptable for one-time events.
    # ========================================

    # Non-reactive variable for change detection in the polling loop
    poll_last_key <- ""

    start_progress_polling <- function(project_id) {
      # Reset key for new computation
      poll_last_key <<- ""

      poll_fn <- function() {
        # Check if still computing (isolate — we're outside reactive context)
        current_id <- shiny::isolate(computing_project_id())
        if (is.null(current_id) || current_id != project_id) return()

        # Read progress from file
        progress_state <- read_progress_state(project_id)

        if (is.null(progress_state)) {
          # No progress file yet — schedule next poll
          later::later(poll_fn, delay = 2)
          return()
        }

        is_running <- progress_state$status %in% c("pending", "downloading", "computing")

        if (is_running) {
          # Create a key based on important state values to detect changes
          progress_pct <- round(
            (progress_state$progress %||% 0) /
            (progress_state$progress_max %||% 1) * 100
          )
          current_key <- paste(
            progress_state$status,
            progress_pct,
            progress_state$indicators_completed %||% 0,
            progress_state$indicators_failed %||% 0,
            progress_state$current_task %||% "",
            sep = "|"
          )

          # Only push UI update if something changed
          if (current_key != poll_last_key) {
            poll_last_key <<- current_key
            # Direct JS update — no reactive flush, no busy state
            progress_result$send_running_update(progress_state)
          }

          # Schedule next poll
          later::later(poll_fn, delay = 2)

        } else if (progress_state$status == "completed") {
          # Terminal: update compute_state for mod_progress completion handler
          compute_state(progress_state)

          session$sendCustomMessage("setComputingMode", list(active = FALSE))
          session$sendCustomMessage("hideElement", list(
            id = ns("progress-progress_card_wrapper")
          ))
          session$sendCustomMessage("showElement", list(
            id = ns("progress-complete_card_wrapper")
          ))

          # Unblock parcel modification (T089)
          app_state$computation_running <- FALSE

          app_state$current_project <- load_project(project_id)
          app_state$project_status <- "completed"
          app_state$refresh_projects <- Sys.time()

          shiny::showNotification(
            i18n$t("computation_complete"),
            type = "message",
            session = session
          )

          # Notification sync PostGIS
          if (is_db_configured()) {
            db_msg <- if (i18n$language == "fr") {
              "Projet synchronis\u00e9 avec la base PostGIS"
            } else {
              "Project synced to PostGIS database"
            }
            shiny::showNotification(
              htmltools::tagList(shiny::icon("database", class = "me-1"), db_msg),
              type = "message", duration = 5, session = session
            )
          }

          computing_project_id(NULL)

        } else if (progress_state$status == "error") {
          compute_state(progress_state)

          session$sendCustomMessage("setComputingMode", list(active = FALSE))
          session$sendCustomMessage("hideElement", list(
            id = ns("progress-progress_card_wrapper")
          ))
          session$sendCustomMessage("showElement", list(
            id = ns("progress-error_card_wrapper")
          ))

          # Unblock parcel modification (T089)
          app_state$computation_running <- FALSE

          error_msg <- if (length(progress_state$errors) > 0) {
            progress_state$errors[[length(progress_state$errors)]]$message
          } else {
            "Unknown error"
          }

          shiny::showNotification(
            paste(i18n$t("computation_error"), error_msg),
            type = "error",
            duration = 10,
            session = session
          )

          computing_project_id(NULL)

        } else if (progress_state$status == "cancelled") {
          session$sendCustomMessage("setComputingMode", list(active = FALSE))
          session$sendCustomMessage("hideElement", list(
            id = ns("progress-progress_card_wrapper")
          ))

          # Unblock parcel modification (T089)
          app_state$computation_running <- FALSE

          shiny::showNotification(
            i18n$t("computation_cancelled"),
            type = "warning",
            session = session
          )

          computing_project_id(NULL)

        } else {
          session$sendCustomMessage("setComputingMode", list(active = FALSE))
          cli::cli_warn("Unknown computation status: {progress_state$status}")
          session$sendCustomMessage("hideElement", list(
            id = ns("progress-progress_card_wrapper")
          ))
          computing_project_id(NULL)
        }
      }

      # Start polling after a short delay
      later::later(poll_fn, delay = 0.5)
    }

    # Note: ExtendedTask completion is handled by the polling observer above
    # which detects status changes from the progress_state.json file

    # Recompute handler
    shiny::observeEvent(input$recompute, {
      project <- app_state$current_project
      shiny::req(project)

      # Show notification with spinner while preparing
      recompute_notif_id <- shiny::showNotification(
        htmltools::tagList(
          shiny::icon("spinner", class = "fa-spin me-2"),
          i18n$t("recomputing") %||% "Relance du calcul en cours\u2026"
        ),
        type = "message",
        duration = NULL,
        closeButton = FALSE
      )

      # Clear indicator cache to force full recomputation
      clear_computation_cache(project$id)

      # Reset status to allow recomputation
      update_project_status(project$id, "draft")
      app_state$current_project <- load_project(project$id)

      # Initialize computation state
      state <- init_compute_state(project$id)
      compute_state(state)

      # Store project ID and start non-reactive polling
      computing_project_id(project$id)
      progress_result$reset_tracking()
      start_progress_polling(project$id)

      # Suppress busy bar flicker during computation
      session$sendCustomMessage("setComputingMode", list(active = TRUE))

      # Show progress card
      session$sendCustomMessage("showElement", list(
        id = ns("progress-progress_card_wrapper")
      ))
      session$sendCustomMessage("hideElement", list(
        id = ns("progress-complete_card_wrapper")
      ))
      session$sendCustomMessage("hideElement", list(
        id = ns("progress-error_card_wrapper")
      ))

      # Start the async computation
      compute_task$invoke(project$id, get_app_options())

      # Remove notification now that progress card is visible
      shiny::removeNotification(recompute_notif_id)
    })

    # View results handler
    shiny::observeEvent(input$view_results, {
      # Navigate to synthesis tab
      shiny::updateNavbarPage(
        session = session$userData$root_session %||% session,
        inputId = "main_nav",
        selected = "synthesis"
      )
    })

    # Handle cancel from progress module
    shiny::observeEvent(app_state$cancel_computation, {
      project_id <- computing_project_id()

      # Write cancelled status to progress file (so async process can detect it)
      if (!is.null(project_id)) {
        cancel_computation(project_id)

        # Reset project status to draft so "Lancer les calculs" reappears
        update_project_status(project_id, "draft")
        app_state$current_project <- load_project(project_id)
      }

      # Cancel the ExtendedTask process if running
      tryCatch({
        if (compute_task$status() == "running") {
          compute_task$cancel()
        }
      }, error = function(e) {
        # ExtendedTask may not be running (resumed tracking mode)
      })

      # End computing mode and hide progress card
      session$sendCustomMessage("setComputingMode", list(active = FALSE))
      session$sendCustomMessage("hideElement", list(
        id = ns("progress-progress_card_wrapper")
      ))

      # Reset computing state
      computing_project_id(NULL)

      shiny::showNotification(
        i18n$t("computation_cancelled") %||% "Calcul annul\u00e9",
        type = "warning"
      )
    }, ignoreInit = TRUE)

    # Handle retry from progress module
    shiny::observeEvent(app_state$retry_computation, {
      project <- app_state$current_project
      shiny::req(project)

      # Get project path for async mode
      project_path <- get_project_path(project$id)

      # Clear indicator cache to force full recomputation
      clear_computation_cache(project$id)

      # Reset status to allow recomputation
      update_project_status(project$id, "draft")

      # Initialize computation state
      state <- init_compute_state(project$id)
      compute_state(state)

      # Store project ID and start non-reactive polling
      computing_project_id(project$id)
      progress_result$reset_tracking()
      start_progress_polling(project$id)

      # Suppress busy bar flicker during computation
      session$sendCustomMessage("setComputingMode", list(active = TRUE))

      # Show progress card, hide error card
      session$sendCustomMessage("showElement", list(
        id = ns("progress-progress_card_wrapper")
      ))
      session$sendCustomMessage("hideElement", list(
        id = ns("progress-error_card_wrapper")
      ))
      session$sendCustomMessage("hideElement", list(
        id = ns("progress-complete_card_wrapper")
      ))

      # Start the async computation
      compute_task$invoke(project$id, get_app_options())
    }, ignoreInit = TRUE)

    # Handle view_results from progress module
    shiny::observeEvent(app_state$view_results, {
      shiny::updateNavbarPage(
        session = session$userData$root_session %||% session,
        inputId = "main_nav",
        selected = "synthesis"
      )
    }, ignoreInit = TRUE)

    # ========================================
    # Guided Tour (cicerone)
    # ========================================

    if (requireNamespace("cicerone", quietly = TRUE)) {
      # Track if tour has been shown in this session
      tour_shown_this_session <- shiny::reactiveVal(FALSE)

      # Function to open collapsed sections
      # Note: must pass session explicitly because this may be called from
      # later::later() which runs outside the Shiny reactive context
      open_collapsed_sections <- function() {
        shiny::insertUI(
          selector = "body",
          where = "beforeEnd",
          ui = htmltools::tags$script(htmltools::HTML(sprintf("
            $('#%s').collapse('show');
            $('#%s').collapse('show');
          ", ns("search_collapse"), "home-project-project_collapse"))),
          immediate = TRUE,
          session = session
        )
      }

      # Function to create and start tour
      do_start_tour <- function() {
        # Target elements - use element IDs
        el_search <- ns("search_collapse")
        el_map <- "home-map-map_card"
        el_name <- "home-project-name"
        el_desc <- "home-project-description"
        el_owner <- "home-project-owner"
        el_create <- "home-project-create_project"

        tryCatch({
          # Create guide and chain all steps
          cicerone::Cicerone$
            new()$
            step(
              el = el_search,
              title = i18n$t("tour_search_title"),
              description = i18n$t("tour_search_desc")
            )$
            step(
              el = el_map,
              title = i18n$t("tour_map_title"),
              description = i18n$t("tour_map_desc", max = get_app_config("max_parcels", 30L))
            )$
            step(
              el = el_name,
              title = i18n$t("tour_project_title"),
              description = i18n$t("tour_project_desc")
            )$
            step(
              el = el_desc,
              title = i18n$t("tour_description_title"),
              description = i18n$t("tour_description_desc")
            )$
            step(
              el = el_owner,
              title = i18n$t("tour_owner_title"),
              description = i18n$t("tour_owner_desc")
            )$
            step(
              el = el_create,
              title = i18n$t("tour_create_title"),
              description = i18n$t("tour_create_desc")
            )$
            init(session = session)$
            start()
        }, error = function(e) {
          warning("[Tour] Could not start: ", e$message)
        })
      }

      # Combined function: open sections then start tour with delay
      # Note: must pass session explicitly because this is called from
      # later::later() which runs outside the Shiny reactive context
      start_tour <- function() {
        open_collapsed_sections()
        # Mark tour as seen in browser localStorage
        session$sendCustomMessage("markTourSeen", list())
        # Add delay to allow UI to render before starting tour
        shiny::insertUI(
          selector = "body",
          where = "beforeEnd",
          ui = htmltools::tags$script(htmltools::HTML("
            setTimeout(function() {
              // Trigger Shiny to start tour after UI is ready
              Shiny.setInputValue('home-tour_ready', Date.now());
            }, 500);
          ")),
          immediate = TRUE,
          session = session
        )
      }

      # Start tour when UI is ready (triggered by JavaScript)
      shiny::observeEvent(input$tour_ready, {
        do_start_tour()
      }, ignoreInit = TRUE)

      # Auto-start tour only if not already seen in browser localStorage.
      # The 'tour_seen_browser' input is sent by custom.js on shiny:connected.
      shiny::observeEvent(input$tour_seen_browser, {
        if (!isTRUE(input$tour_seen_browser) && !tour_shown_this_session()) {
          tour_shown_this_session(TRUE)
          # Delay to let UI fully render before starting tour
          later::later(function() {
            start_tour()
          }, delay = 2)
        }
      }, once = TRUE)

      # Restart tour when requested from app_server
      shiny::observeEvent(app_state$restart_tour, {
        start_tour()
      }, ignoreInit = TRUE)
    } else {
      message("[TOUR] Cicerone package not available!")
    }

    # ========================================
    # Return Values
    # ========================================

    list(
      selected_commune = search_result$selected_commune,
      selected_parcels = map_result$selected_parcels,
      selection_count = map_result$selection_count,
      current_project = project_result$current_project
    )
  })
}

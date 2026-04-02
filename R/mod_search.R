#' Search Module for nemetonApp
#'
#' @description
#' Shiny module for searching and selecting French communes.
#' Provides department filtering and commune selection.
#'
#' @name mod_search
#' @keywords internal
NULL


#' Search Module UI
#'
#' @param id Module namespace ID.
#'
#' @return Shiny UI elements.
#'
#' @noRd
mod_search_ui <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language)

  htmltools::tagList(
    # Department selector
    shiny::selectInput(
      inputId = ns("departement"),
      label = htmltools::tagList(
        bsicons::bs_icon("geo-alt"),
        i18n$t("department")
      ),
      choices = NULL,
      selected = NULL,
      width = "100%"
    ),

    # Commune search with autocomplete
    shiny::selectizeInput(
      inputId = ns("commune"),
      label = htmltools::tagList(
        bsicons::bs_icon("building"),
        i18n$t("commune")
      ),
      choices = NULL,
      options = list(
        placeholder = i18n$t("search_commune"),
        maxOptions = 500
      ),
      width = "100%"
    ),

    # Results info
    shiny::uiOutput(ns("search_info")),

    # Loading indicator
    shiny::conditionalPanel(
      condition = sprintf("input['%s']", ns("commune")),
      ns = ns,
      htmltools::div(
        class = "text-center py-2",
        id = ns("loading_indicator"),
        style = "display: none;",
        htmltools::div(
          class = "spinner-border spinner-border-sm text-primary",
          role = "status"
        ),
        htmltools::span(class = "ms-2 text-muted", i18n$t("loading_commune"))
      )
    )
  )
}


#' Search Module Server
#'
#' @param id Module namespace ID.
#' @param app_state Reactive values for app state.
#'
#' @return List of reactive values:
#'   - selected_commune: Reactive with selected commune code
#'   - commune_info: Reactive with commune metadata
#'   - commune_geometry: Reactive with commune sf geometry
#'
#' @noRd
mod_search_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get language with fallback
    get_lang <- function() {
      lang <- app_state$language
      if (is.null(lang) || lang == "") "fr" else lang
    }

    # ========================================
    # Reactive Values
    # ========================================

    rv <- shiny::reactiveValues(
      selected_commune = NULL,
      commune_info = NULL,
      commune_geometry = NULL,
      is_loading = FALSE,
      is_restoring = FALSE,
      restore_gen = 0L,  # Generation counter: incremented on each new restore
      department_changed = NULL  # Signal when department changes (timestamp)
    )


    # ========================================
    # ExtendedTask: non-blocking API calls
    #
    # Unlike future_promise() in observers (which still blocks the
    # current session), ExtendedTask truly frees the session so the
    # browser stays responsive during the API call.
    # ========================================

    .pkg_path <- tryCatch(pkgload::pkg_path(), error = function(e) NULL)

    # Helper: ensure future plan is parallel in the worker
    ensure_future_plan <- function() {
      if (requireNamespace("future", quietly = TRUE)) {
        plan_classes <- class(future::plan())
        is_parallel <- any(c("multisession", "multicore", "cluster") %in% plan_classes)
        if (!is_parallel) future::plan("multisession")
      }
    }

    # Helper: load nemeton in the future worker process
    load_nemeton_in_worker <- function() {
      if (!is.null(.pkg_path) && requireNamespace("pkgload", quietly = TRUE)) {
        pkgload::load_all(.pkg_path, quiet = TRUE)
      } else if (requireNamespace("nemeton", quietly = TRUE)) {
        loadNamespace("nemeton")
      }
    }

    # ExtendedTask: fetch communes for a department
    dept_task <- shiny::ExtendedTask$new(function(dept) {
      ensure_future_plan()
      promises::future_promise({
        load_nemeton_in_worker()
        get_communes_in_department(dept)
      }, seed = TRUE)
    })

    # ExtendedTask: fetch commune geometry + info
    commune_task <- shiny::ExtendedTask$new(function(code, dept) {
      ensure_future_plan()
      promises::future_promise({
        load_nemeton_in_worker()
        geometry <- get_commune_geometry(code)
        communes <- get_communes_in_department(dept)
        list(geometry = geometry, communes = communes, code = code)
      }, seed = TRUE)
    })

    # ExtendedTask: fetch communes + commune geometry for project restore.
    # By fetching geometry here (instead of relying on input$commune ->
    # commune_task chain), we eliminate the fragile 4-step async chain
    # and set commune_geometry directly in the result handler.
    restore_task <- shiny::ExtendedTask$new(function(dept_code, commune_code) {
      ensure_future_plan()
      promises::future_promise({
        load_nemeton_in_worker()
        communes <- get_communes_in_department(dept_code)
        geometry <- get_commune_geometry(commune_code)
        list(communes = communes, commune_code = commune_code, geometry = geometry)
      }, seed = TRUE)
    })


    # ========================================
    # Initialize Department Dropdown (once on start)
    # ========================================

    shiny::observeEvent(TRUE, {
      i18n <- get_i18n(get_lang())
      depts <- get_departments()

      shiny::updateSelectInput(
        session,
        "departement",
        choices = c(
          stats::setNames("", i18n$t("select_department")),
          depts
        )
      )
    }, once = TRUE, ignoreInit = FALSE)


    # ========================================
    # Update Communes When Department Changes
    # ========================================

    # Step 1: invoke the ExtendedTask (non-blocking)
    shiny::observeEvent(input$departement, {
      dept <- input$departement

      # During project restore, the restore observer handles commune loading
      # directly via later::later - skip redundant API call here
      if (rv$is_restoring) return()

      # Signal that department changed (so other modules can reset state)
      rv$department_changed <- Sys.time()

      if (is.null(dept) || dept == "") {
        shiny::updateSelectizeInput(
          session,
          "commune",
          choices = character(0),
          server = FALSE
        )
        return()
      }

      # Show loading and invoke async task
      rv$is_loading <- TRUE
      dept_task$invoke(dept)
    }, ignoreInit = TRUE)

    # Step 2: handle the result when the task completes
    #
    # Use observe() + tryCatch instead of observeEvent(result()) to properly
    # handle ExtendedTask errors. See restore_task handler for full explanation.
    shiny::observe({
      communes <- tryCatch(
        dept_task$result(),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          e
        }
      )

      # Safety net: if a restore is still in progress, the dept_task was
      # triggered by a stale department change. Don't overwrite the commune
      # dropdown that restore_task already populated.
      # isolate() prevents is_restoring from becoming a reactive dependency
      # of this observe (it's just a guard, not a trigger).
      if (shiny::isolate(rv$is_restoring)) return()

      i18n <- get_i18n(get_lang())

      # Handle task error
      if (inherits(communes, "error")) {
        cli::cli_alert_danger("Error loading communes: {communes$message}")
        shiny::showNotification(
          paste(i18n$t("error_loading_communes"), communes$message),
          type = "error",
          duration = 8
        )
        communes <- NULL
      }

      if (is.null(communes)) {
        shiny::updateSelectizeInput(
          session, "commune",
          choices = character(0), selected = "", server = FALSE
        )
        rv$is_loading <- FALSE
        return()
      }

      # Check for errors (returned as attribute)
      error_type <- attr(communes, "error")
      if (!is.null(error_type)) {
        if (error_type == "network") {
          shiny::showNotification(
            i18n$t("error_no_internet"),
            type = "error",
            duration = 8
          )
        } else {
          shiny::showNotification(
            paste(i18n$t("error_loading_communes"), attr(communes, "error_message")),
            type = "error",
            duration = 8
          )
        }
        shiny::updateSelectizeInput(
          session, "commune",
          choices = character(0), selected = "", server = FALSE
        )
      } else if (!is.null(communes) && nrow(communes) > 0) {
        choices <- format_communes_for_selectize(communes)
        shiny::updateSelectizeInput(
          session, "commune",
          choices = choices, selected = "", server = FALSE
        )
      } else {
        shiny::updateSelectizeInput(
          session, "commune",
          choices = character(0), selected = "", server = FALSE
        )
      }

      rv$is_loading <- FALSE
    })


    # ========================================
    # Handle Commune Selection
    # ========================================

    # Step 1: invoke the ExtendedTask (non-blocking)
    shiny::observeEvent(input$commune, {
      code <- input$commune

      if (is.null(code) || code == "") {
        # During restore, selectize may briefly fire "" when choices are
        # updated before the selected value is applied. Don't clear
        # geometry that restore_task just set.
        if (rv$is_restoring) return()
        rv$selected_commune <- NULL
        rv$commune_info <- NULL
        rv$commune_geometry <- NULL
        return()
      }

      # During restore, geometry + selected_commune are set directly by
      # restore_task result handler -- skip redundant commune_task call.
      # Delay clearing is_restoring so the department observer
      # (triggered by the updateSelectInput roundtrip that hasn't arrived
      # yet) still sees is_restoring = TRUE and skips dept_task$invoke().
      if (rv$is_restoring) {
        # Capture current generation so stale callbacks from a previous
        # restore don't clear is_restoring during a newer restore.
        gen_snapshot <- rv$restore_gen
        later::later(function() {
          if (shiny::isolate(rv$restore_gen) == gen_snapshot) {
            rv$is_restoring <- FALSE
          }
        }, delay = 1)
        return()
      }

      # Also skip if geometry is already available for this exact commune
      # (browser roundtrip from updateSelectizeInput fires input$commune
      # AFTER restore_task already set everything).
      if (!is.null(rv$commune_geometry) && identical(rv$selected_commune, code)) return()

      rv$is_loading <- TRUE
      commune_task$invoke(code, input$departement)
    }, ignoreInit = TRUE)

    # Step 2: handle the result when the task completes
    # Use observe() + tryCatch for proper ExtendedTask error handling.
    shiny::observe({
      result <- tryCatch(
        commune_task$result(),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          cli::cli_alert_danger("Error loading commune: {e$message}")
          NULL
        }
      )

      if (!is.null(result) && !is.null(result$geometry)) {
        # Signal that commune is transitioning -- prevents the combined observer
        # in mod_map from rendering stale parcels with the new geometry during
        # the flush cycle (observer execution order is non-deterministic).
        app_state$commune_transitioning <- TRUE

        rv$selected_commune <- result$code
        rv$commune_geometry <- result$geometry

        communes <- result$communes
        if (!is.null(communes) && nrow(communes) > 0) {
          info <- communes[communes$code_insee == result$code, ]
          if (nrow(info) > 0) {
            rv$commune_info <- as.list(info[1, ])
          }
        }
      }

      # isolate() prevents is_restoring from becoming a reactive dependency
      if (shiny::isolate(rv$is_restoring)) {
        rv$is_restoring <- FALSE
      }
      rv$is_loading <- FALSE
    })


    shiny::observeEvent(app_state$restore_project, {
      restore <- app_state$restore_project
      if (is.null(restore) || is.null(restore$commune_code)) return()

      dept_code <- restore$department_code
      commune_code <- restore$commune_code

      # Increment generation counter. All later::later callbacks and
      # result handlers snapshot this value and compare before acting,
      # ensuring stale callbacks from a previous restore are no-ops.
      rv$restore_gen <- shiny::isolate(rv$restore_gen) + 1L
      gen_snapshot <- rv$restore_gen

      # Clear stale commune geometry from previous commune/project.
      # Without this, the combined observer in mod_map fires with new
      # parcels (set by load_project handler) + OLD geometry, rendering
      # everything prematurely. When restore_task completes and sets the
      # correct geometry, the combined observer fires AGAIN -- clearGroup
      # wipes the map (white flash) then re-renders. By nullifying the
      # geometry here, the combined observer sees NULL geometry on its
      # first firing and returns early (line 330), avoiding the double-
      # render entirely.
      rv$commune_geometry <- NULL

      cli::cli_alert_info("Restoring location: dept={dept_code}, commune={commune_code} (gen={gen_snapshot})")

      # Flag to prevent department observer from making redundant API calls
      rv$is_restoring <- TRUE

      # Update department dropdown
      shiny::updateSelectInput(session, "departement", selected = dept_code)

      # Fetch communes asynchronously via ExtendedTask.
      # If the task is already running (rapid project switching), invoke()
      # throws. Don't clear flags -- the running task's result handler will
      # detect the generation mismatch and re-invoke for the current project.
      tryCatch(
        restore_task$invoke(dept_code, commune_code),
        error = function(e) {
          cli::cli_warn("restore_task busy (gen={gen_snapshot}): {e$message}")
          # Schedule a retry after a short delay. The running task should
          # complete soon; when it does, its result handler re-invokes.
          # This is a fallback in case the result handler doesn't fire.
          later::later(function() {
            if (shiny::isolate(rv$restore_gen) != gen_snapshot) return()
            current <- shiny::isolate(app_state$restore_project)
            if (is.null(current)) return()
            tryCatch(
              restore_task$invoke(current$department_code, current$commune_code),
              error = function(e2) {
                cli::cli_warn("restore_task retry also failed (gen={gen_snapshot}): {e2$message}")
              }
            )
          }, delay = 3)
        }
      )

      # Safety timeout: if restore_task never completes (worker crash, etc.),
      # clear flags after 30 seconds so the spinner doesn't spin forever.
      later::later(function() {
        # Only clear if this is still the same restore generation
        if (shiny::isolate(rv$restore_gen) != gen_snapshot) return()
        if (isTRUE(shiny::isolate(rv$is_restoring))) {
          cli::cli_warn("Restore safety timeout (gen={gen_snapshot}) -- clearing stale flags")
          rv$is_restoring <- FALSE
        }
        if (isTRUE(shiny::isolate(app_state$restore_in_progress))) {
          app_state$restore_in_progress <- FALSE
        }
      }, delay = 30)
    }, ignoreInit = TRUE)

    # Step 2: handle restore task result
    #
    # IMPORTANT: We use observe() + tryCatch instead of observeEvent(result()).
    # When an ExtendedTask fails, result() re-raises the error. observeEvent
    # sees this error in the event expression and NEVER fires the handler.
    # The tryCatch inside the handler is useless because the handler code
    # never executes. This left flags stuck and the spinner spinning forever.
    #
    # With observe(), we catch the error ourselves and handle it properly.
    # - "shiny.silent.error" (from req()) = task not ready -> re-raise to suspend
    # - Regular error = task failed -> handle and clear flags
    # - Normal value = task succeeded -> process result
    shiny::observe({
      result <- tryCatch(
        restore_task$result(),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) {
            # Task not yet invoked or still running -- re-raise to suspend
            stop(e)
          }
          # Task error -- return error object for handling below
          e
        }
      )

      # Handle task error
      if (inherits(result, "error")) {
        cli::cli_alert_danger("Error restoring location: {result$message}")
        rv$is_restoring <- FALSE
        later::later(function() {
          app_state$restore_in_progress <- FALSE
        }, delay = 0)
        return()
      }

      # Check if this result is still relevant. If the user switched
      # projects while restore_task was running, app_state$restore_project
      # now points to a different commune. Discard stale result and
      # re-invoke for the current project.
      current_restore <- shiny::isolate(app_state$restore_project)
      if (!is.null(current_restore) &&
          !identical(result$commune_code, current_restore$commune_code)) {
        cli::cli_alert_info(
          "Discarding stale restore result for {result$commune_code}, current target is {current_restore$commune_code}"
        )
        tryCatch(
          restore_task$invoke(
            current_restore$department_code,
            current_restore$commune_code
          ),
          error = function(e) {
            cli::cli_warn("Re-invoke for current project failed: {e$message}")
            rv$is_restoring <- FALSE
            later::later(function() {
              app_state$restore_in_progress <- FALSE
            }, delay = 0)
          }
        )
        return()
      }

      communes <- result$communes
      commune_code <- result$commune_code

      # Set commune geometry and selection DIRECTLY -- no need to go
      # through input$commune -> commune_task chain. This makes
      # commune_geometry() available immediately for the combined
      # observer in mod_map (together with parcels_data set earlier).
      if (!is.null(result$geometry)) {
        rv$commune_geometry <- result$geometry
        rv$selected_commune <- commune_code

        if (!is.null(communes) && nrow(communes) > 0) {
          info <- communes[communes$code_insee == commune_code, ]
          if (nrow(info) > 0) {
            rv$commune_info <- as.list(info[1, ])
          }
        }
      } else {
        # Geometry fetch failed -- clear flags so spinner doesn't spin forever
        cli::cli_warn("Restore: commune geometry is NULL for {commune_code}")
        rv$is_restoring <- FALSE
        later::later(function() {
          app_state$restore_in_progress <- FALSE
        }, delay = 0)
        return()
      }

      if (!is.null(communes) && nrow(communes) > 0) {
        choices <- format_communes_for_selectize(communes)
        cli::cli_alert_info("Updating commune dropdown with {nrow(communes)} choices")

        # Update dropdown (for display only -- geometry already set above)
        shiny::updateSelectizeInput(
          session,
          "commune",
          choices = choices,
          selected = commune_code,
          server = FALSE
        )

        cli::cli_alert_success("Location restored successfully")
      }

      # Clear restore_in_progress AFTER this flush cycle.
      # The mod_home selected_commune observer runs in the SAME flush;
      # it must see TRUE to skip reset_project_state(). Using delay = 0
      # defers the write to the NEXT event loop iteration.
      # Use generation check so a stale callback from a previous restore
      # doesn't clear flags during a newer restore.
      gen_snapshot <- shiny::isolate(rv$restore_gen)
      later::later(function() {
        if (shiny::isolate(rv$restore_gen) != gen_snapshot) return()
        app_state$restore_in_progress <- FALSE
      }, delay = 0)

      # NOTE: Don't clear rv$is_restoring here. The updateSelectizeInput
      # above queues a message to the browser. The selectize may briefly
      # fire input$commune="" before the selected value. If is_restoring
      # is already FALSE, the commune observer clears rv$commune_geometry.
      # is_restoring is cleared by the commune observer when the final
      # input$commune value arrives.
      rv$is_loading <- FALSE
    })


    # ========================================
    # Return Values
    # ========================================

    list(
      selected_commune = shiny::reactive(rv$selected_commune),
      commune_info = shiny::reactive(rv$commune_info),
      commune_geometry = shiny::reactive(rv$commune_geometry),
      is_loading = shiny::reactive(rv$is_loading),
      department_changed = shiny::reactive(rv$department_changed)
    )
  })
}

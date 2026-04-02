#' Project Module for nemetonApp
#'
#' @description
#' Shiny module for project metadata form.
#' Handles project name, description, owner, and creation date.
#'
#' @name mod_project
#' @keywords internal
NULL


#' Project Module UI
#'
#' @param id Character. Module namespace ID.
#'
#' @return Shiny UI elements.
#'
#' @noRd
mod_project_ui <- function(id) {
  ns <- shiny::NS(id)

  # Get language for translations
  opts <- get_app_options()
  lang <- opts$language %||% "fr"
  i18n <- get_i18n(lang)

  # Collapsible card using bslib::card with Bootstrap collapse
  htmltools::tagList(
    # Header that toggles collapse
    htmltools::tags$div(
      class = "card mb-3",
      htmltools::tags$div(
        class = "card-header bg-success text-white py-2",
        style = "cursor: pointer;",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = paste0("#", ns("project_collapse")),
        `aria-expanded` = "false",
        `aria-controls` = ns("project_collapse"),
        htmltools::div(
          class = "d-flex align-items-center justify-content-between",
          htmltools::div(
            class = "d-flex align-items-center",
            bsicons::bs_icon("folder-plus", class = "me-2"),
            i18n$t("project_info")
          ),
          bsicons::bs_icon("chevron-down", class = "collapse-icon")
        )
      ),
      # Collapsible body
      htmltools::tags$div(
        id = ns("project_collapse"),
        class = "collapse",
        htmltools::tags$div(
          class = "card-body",
          # Project name (required)
          htmltools::div(
            class = "mb-3",
            shiny::textInput(
              ns("name"),
              label = htmltools::tagList(
                i18n$t("project_name"),
                htmltools::span("*", class = "text-danger")
              ),
              placeholder = i18n$t("project_name_placeholder"),
              width = "100%"
            ),
            htmltools::div(
              id = ns("name_feedback"),
              class = "invalid-feedback d-none",
              i18n$t("field_required")
            ),
            htmltools::tags$small(
              class = "text-muted",
              sprintf("%s: 100 %s", i18n$t("max_chars"), i18n$t("characters"))
            )
          ),

          # Description (optional)
          htmltools::div(
            class = "mb-3",
            shiny::textAreaInput(
              ns("description"),
              label = i18n$t("project_description"),
              placeholder = i18n$t("project_description_placeholder"),
              rows = 3,
              width = "100%"
            ),
            htmltools::tags$small(
              class = "text-muted",
              sprintf("%s: 500 %s", i18n$t("max_chars"), i18n$t("characters"))
            )
          ),

          # Owner (optional)
          htmltools::div(
            class = "mb-3",
            shiny::textInput(
              ns("owner"),
              label = i18n$t("project_owner"),
              placeholder = i18n$t("project_owner_placeholder"),
              width = "100%"
            ),
            htmltools::tags$small(
              class = "text-muted",
              sprintf("%s: 100 %s", i18n$t("max_chars"), i18n$t("characters"))
            )
          ),

          # Creation date (auto)
          htmltools::div(
            class = "mb-3",
            htmltools::tags$label(
              class = "form-label",
              i18n$t("project_date")
            ),
            shiny::uiOutput(ns("date_display")),
            htmltools::tags$small(
              class = "text-muted",
              i18n$t("auto_generated")
            )
          )
        ),
        # Footer
        htmltools::tags$div(
          class = "card-footer d-flex justify-content-between align-items-center",
          shiny::uiOutput(ns("validation_message")),
          shiny::uiOutput(ns("action_button"))
        )
      )
    ),
    # JavaScript for character limits and collapse icon rotation
    htmltools::tags$script(htmltools::HTML(sprintf("
      $(document).ready(function() {
        // Character limits
        $('#%s').on('input', function() {
          if ($(this).val().length > 100) {
            $(this).val($(this).val().substring(0, 100));
          }
        });
        $('#%s').on('input', function() {
          if ($(this).val().length > 500) {
            $(this).val($(this).val().substring(0, 500));
          }
        });
        $('#%s').on('input', function() {
          if ($(this).val().length > 100) {
            $(this).val($(this).val().substring(0, 100));
          }
        });
      });
    ", ns("name"), ns("description"), ns("owner"))))
  )
}


#' Project Module Server
#'
#' @param id Character. Module namespace ID.
#' @param app_state Reactive values. Application state.
#' @param selected_parcels Reactive. Selected parcels sf object.
#'
#' @return List with reactive values:
#'   - project_created: Reactive that fires when project is created
#'   - current_project: Reactive with current project info
#'
#' @noRd
mod_project_server <- function(id, app_state, selected_parcels) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get translations
    opts <- get_app_options()
    lang <- opts$language %||% "fr"
    i18n <- get_i18n(lang)

    # Reactive values
    rv <- shiny::reactiveValues(
      current_project = NULL,
      editing_project_id = NULL,  # NULL = create mode, ID = edit mode
      validation_errors = character(0),
      project_date = format(Sys.time(), "%d/%m/%Y %H:%M"),
      last_opened_project_id = NULL  # Track to avoid re-opening panel on same project
    )

    # ========================================
    # Validation
    # ========================================

    validate_form <- function() {
      errors <- character(0)

      # Name is required
      name <- trimws(input$name %||% "")
      if (nchar(name) == 0) {
        errors <- c(errors, i18n$t("name_required"))
      } else if (nchar(name) > 100) {
        errors <- c(errors, i18n$t("name_too_long"))
      }

      # Description length
      description <- input$description %||% ""
      if (nchar(description) > 500) {
        errors <- c(errors, i18n$t("description_too_long"))
      }

      # Owner length
      owner <- input$owner %||% ""
      if (nchar(owner) > 100) {
        errors <- c(errors, i18n$t("owner_too_long"))
      }

      rv$validation_errors <- errors
      length(errors) == 0
    }

    # ========================================
    # UI Outputs
    # ========================================

    # Date display
    output$date_display <- shiny::renderUI({
      htmltools::div(
        class = "form-control-plaintext",
        rv$project_date
      )
    })

    # Action button (Create/Update)
    # Force rendering even when the collapse panel is hidden, otherwise
    # the button's disabled state won't update until the panel opens.
    output$action_button <- shiny::renderUI({
      parcels <- selected_parcels()

      # Check if we have parcels
      has_parcels <- !is.null(parcels) && nrow(parcels) > 0

      if (is.null(rv$editing_project_id)) {
        # Create mode
        shiny::actionButton(
          ns("create_project"),
          label = i18n$t("create_project"),
          class = "btn-success",
          icon = bsicons::bs_icon("plus-circle"),
          disabled = !has_parcels
        )
      } else {
        # Edit mode
        htmltools::tagList(
          shiny::actionButton(
            ns("update_project"),
            label = i18n$t("update_project"),
            class = "btn-primary",
            icon = bsicons::bs_icon("save")
          ),
          shiny::actionButton(
            ns("delete_project"),
            label = i18n$t("delete"),
            class = "btn-outline-danger",
            icon = bsicons::bs_icon("trash")
          )
        )
      }
    })
    shiny::outputOptions(output, "action_button", suspendWhenHidden = FALSE)

    # ========================================
    # Restore Project Form When Loading
    # ========================================

    shiny::observeEvent(app_state$current_project, {
      project <- app_state$current_project
      if (is.null(project) || is.null(project$metadata)) {
        # Reset form fields when project is cleared (e.g., commune change)
        shiny::updateTextInput(session, "name", value = "")
        shiny::updateTextAreaInput(session, "description", value = "")
        shiny::updateTextInput(session, "owner", value = "")
        rv$editing_project_id <- NULL
        rv$current_project <- NULL
        rv$project_date <- format(Sys.time(), "%d/%m/%Y %H:%M")
        # Collapse the panel
        shiny::insertUI(
          selector = "body",
          where = "beforeEnd",
          ui = htmltools::tags$script(htmltools::HTML(sprintf(
            "$('#%s').collapse('hide');", ns("project_collapse")
          ))),
          immediate = TRUE
        )
        return()
      }

      # Fill in form fields with project data
      shiny::updateTextInput(
        session,
        "name",
        value = project$metadata$name %||% ""
      )
      shiny::updateTextAreaInput(
        session,
        "description",
        value = project$metadata$description %||% ""
      )
      shiny::updateTextInput(
        session,
        "owner",
        value = project$metadata$owner %||% ""
      )

      # Store date for display update
      if (!is.null(project$metadata$created_at)) {
        rv$project_date <- format(as.POSIXct(project$metadata$created_at), "%d/%m/%Y %H:%M")
      }

      # Set editing mode with project ID
      rv$editing_project_id <- project$id
      rv$current_project <- project

      # Open the collapse panel only when a NEW project is loaded for
      # the first time (different project ID). Skip when the same project
      # is reloaded (after computation, retry, cancel, etc.) to avoid
      # the panel popping open unexpectedly.
      is_new_project <- !identical(project$id, rv$last_opened_project_id)
      rv$last_opened_project_id <- project$id
      if (is_new_project && !isTRUE(app_state$restore_in_progress)) {
        shiny::insertUI(
          selector = "body",
          where = "beforeEnd",
          ui = htmltools::tags$script(htmltools::HTML(sprintf("
            $('#%s').collapse('show');
          ", ns("project_collapse")))),
          immediate = TRUE
        )
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Open collapse when parcels are selected (new project mode)
    shiny::observeEvent(selected_parcels(), {
      parcels <- selected_parcels()
      if (!is.null(parcels) && nrow(parcels) > 0 && is.null(rv$editing_project_id)) {
        # New parcels selected and not in edit mode - open the form
        shiny::insertUI(
          selector = "body",
          where = "beforeEnd",
          ui = htmltools::tags$script(htmltools::HTML(sprintf("
            $('#%s').collapse('show');
          ", ns("project_collapse")))),
          immediate = TRUE
        )
      }
    }, ignoreInit = TRUE)


    # ========================================
    # Create Project
    # ========================================

    # Validation message output
    output$validation_message <- shiny::renderUI({
      errors <- rv$validation_errors
      if (length(errors) == 0) return(NULL)
      htmltools::div(
        class = "text-danger small",
        htmltools::HTML(paste(errors, collapse = "<br>"))
      )
    })

    shiny::observe({
      if (!validate_form()) {
        # Validation errors are displayed via renderUI above
        return()
      }

      # Get parcels
      parcels <- selected_parcels()

      # Check if parcels are selected
      if (is.null(parcels) || nrow(parcels) == 0) {
        shiny::showNotification(
          i18n$t("no_parcels_selected"),
          type = "warning"
        )
        return()
      }

      # Guard against renderUI button creation (NULL → 0 transition)
      # When the form switches from create to edit mode, the new button's
      # input value goes from NULL to 0, which triggers bindEvent.
      # Only proceed if a button was actually clicked (value > 0).
      create_clicked <- isTRUE(input$create_project > 0)
      update_clicked <- isTRUE(input$update_project > 0)
      shiny::req(create_clicked || update_clicked)

      # Validate parcels are sf objects
      if (!inherits(parcels, "sf")) {
        cli::cli_alert_danger("Parcels are not sf objects! Class: {paste(class(parcels), collapse=', ')}")
        shiny::showNotification(
          "Erreur: les parcelles ne sont pas au format spatial",
          type = "error"
        )
        return()
      }

      cli::cli_alert_info("Saving {nrow(parcels)} parcels (class: {paste(class(parcels), collapse=', ')})")

      tryCatch({
        if (is.null(rv$editing_project_id)) {
          # CREATE mode
          project <- create_project(
            name = trimws(input$name),
            description = input$description %||% "",
            owner = input$owner %||% "",
            parcels = parcels
          )

          rv$current_project <- project
          rv$editing_project_id <- project$id

          # Update app state
          app_state$current_project <- project
          app_state$project_id <- project$id
          app_state$refresh_projects <- Sys.time()  # Refresh recent projects list

          shiny::showNotification(
            sprintf("%s: %s", i18n$t("project_created"), project$metadata$name),
            type = "message"
          )
        } else {
          # UPDATE mode
          project <- update_project(
            project_id = rv$editing_project_id,
            name = trimws(input$name),
            description = input$description %||% "",
            owner = input$owner %||% "",
            parcels = parcels
          )

          rv$current_project <- project

          # Update app state
          app_state$current_project <- project
          app_state$refresh_projects <- Sys.time()

          shiny::showNotification(
            sprintf("%s: %s", i18n$t("project_updated"), project$metadata$name),
            type = "message"
          )
        }
      }, error = function(e) {
        cli::cli_alert_danger("Error saving project: {e$message}")
        shiny::showNotification(
          paste(i18n$t("error"), e$message),
          type = "error"
        )
      })
    }) |> shiny::bindEvent(input$create_project, input$update_project)


    # ========================================
    # Delete Project
    # ========================================

    shiny::observeEvent(input$delete_project, {
      shiny::showModal(shiny::modalDialog(
        title = htmltools::div(
          class = "text-danger",
          bsicons::bs_icon("exclamation-triangle", class = "me-2"),
          i18n$t("delete_project")
        ),
        htmltools::p(i18n$t("confirm_delete_project")),
        htmltools::p(class = "text-muted small", i18n$t("delete_project_warning")),
        footer = htmltools::tagList(
          shiny::modalButton(i18n$t("cancel")),
          shiny::actionButton(
            ns("confirm_delete_project"),
            i18n$t("delete"),
            class = "btn-danger"
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_delete_project, {
      shiny::removeModal()

      tryCatch({
        delete_project(rv$editing_project_id)

        # Reset state
        rv$current_project <- NULL
        rv$editing_project_id <- NULL
        rv$project_date <- format(Sys.time(), "%d/%m/%Y %H:%M")

        # Clear form
        shiny::updateTextInput(session, "name", value = "")
        shiny::updateTextAreaInput(session, "description", value = "")
        shiny::updateTextInput(session, "owner", value = "")

        # Update app state
        app_state$current_project <- NULL
        app_state$project_id <- NULL
        app_state$refresh_projects <- Sys.time()

        shiny::showNotification(
          i18n$t("project_deleted"),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n$t("error"), e$message),
          type = "error"
        )
      })
    })


    # ========================================
    # Return Values
    # ========================================

    list(
      current_project = shiny::reactive(rv$current_project),
      project_created = shiny::reactive(rv$editing_project_id)
    )
  })
}

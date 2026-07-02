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

          # UGF classification profile (ONF / CRPF / OFB / Generic, configurable)
          htmltools::div(
            class = "mb-3",
            shiny::selectInput(
              ns("groupes_profile"),
              label = i18n$t("project_groupes_profile"),
              choices = get_groupes_profile_choices(lang = lang),
              selected = get_default_groupes_profile(),
              width = "100%"
            ),
            htmltools::tags$small(
              class = "text-muted",
              i18n$t("project_groupes_profile_help")
            )
          ),

          # Forêt ancienne → N2 continuité (spec 031). Optional per-project
          # historical source; rendered server-side so params adapt to the
          # detected file type and to whether a project exists yet.
          shiny::uiOutput(ns("foret_ancienne_block")),

          # Optional "Clear-cuts (SUFOSAT)" block — opt-in national Theia
          # source feeding the T3 indicator (spec 030). No upload: a toggle +
          # two parameters, gated on Theia credentials being configured.
          shiny::uiOutput(ns("sufosat_block")),

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
          ),

          # Storage directory (visible only when a project is loaded)
          shiny::uiOutput(ns("path_display"))
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
#' @param commune_geometry Reactive. Current commune boundary sf object,
#'   persisted with the project so it can be restored without a refetch.
#'
#' @return List with reactive values:
#'   - project_created: Reactive that fires when project is created
#'   - current_project: Reactive with current project info
#'
#' @noRd
mod_project_server <- function(id, app_state, selected_parcels,
                               commune_geometry = shiny::reactive(NULL)) {
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
    # Forêt ancienne → N2 continuité (spec 031)
    # ========================================

    # Class values present in an uploaded classified raster (for the
    # forest_class multi-select). Cheap terra::unique on the source raster.
    fa_raster_classes <- shiny::reactive({
      f <- input$fa_file
      shiny::req(f)
      if (!tolower(tools::file_ext(f$name)) %in% c("tif", "tiff")) return(NULL)
      tryCatch({
        u <- terra::unique(terra::rast(f$datapath))[[1]]
        sort(u[!is.na(u)])
      }, error = function(e) NULL)
    })

    # Parameters block — adapts to the detected source type.
    output$fa_params <- shiny::renderUI({
      min_area <- shiny::numericInput(
        ns("fa_min_area"), i18n$t("foret_ancienne_min_area"),
        value = 5000, min = 0, step = 500, width = "100%")

      f <- input$fa_file
      if (is.null(f)) return(htmltools::div(min_area))

      if (tolower(tools::file_ext(f$name)) %in% c("tif", "tiff")) {
        htmltools::div(
          shiny::radioButtons(
            ns("fa_mode"), NULL,
            choices = stats::setNames(
              c("class", "threshold"),
              c(i18n$t("foret_ancienne_mode_class"),
                i18n$t("foret_ancienne_mode_threshold"))),
            selected = "class", inline = TRUE),
          shiny::uiOutput(ns("fa_class_or_threshold")),
          min_area
        )
      } else {
        htmltools::div(min_area)
      }
    })

    output$fa_class_or_threshold <- shiny::renderUI({
      if (identical(input$fa_mode %||% "class", "threshold")) {
        shiny::numericInput(
          ns("fa_threshold"), i18n$t("foret_ancienne_threshold"),
          value = 1, width = "100%")
      } else {
        shiny::selectizeInput(
          ns("fa_forest_class"), i18n$t("foret_ancienne_forest_class"),
          choices = fa_raster_classes(), multiple = TRUE, width = "100%")
      }
    })

    # The optional block itself (header + current source + upload + actions).
    output$foret_ancienne_block <- shiny::renderUI({
      header <- htmltools::tags$label(
        class = "form-label fw-semibold", i18n$t("foret_ancienne_section"))
      hint <- htmltools::tags$small(
        class = "text-muted d-block mb-2", i18n$t("foret_ancienne_hint"))

      pid <- rv$editing_project_id
      if (is.null(pid)) {
        return(htmltools::div(
          class = "mb-3", header,
          htmltools::div(class = "text-muted small fst-italic",
                         i18n$t("foret_ancienne_need_project"))))
      }

      proj <- rv$current_project %||% app_state$current_project
      fa <- proj$metadata$foret_ancienne
      current <- if (!is.null(fa) && !is.null(fa$source_name)) {
        htmltools::div(
          class = "small mb-2",
          bsicons::bs_icon("check-circle-fill", class = "text-success me-1"),
          sprintf("%s : %s", i18n$t("foret_ancienne_current"), fa$source_name))
      } else {
        htmltools::div(class = "small text-muted mb-2 fst-italic",
                       i18n$t("foret_ancienne_none"))
      }

      htmltools::div(
        class = "mb-3 p-2 border rounded",
        header, hint, current,
        shiny::fileInput(
          ns("fa_file"), i18n$t("foret_ancienne_source"),
          accept = c(".tif", ".tiff", ".gpkg", ".shp", ".geojson"),
          width = "100%"),
        shiny::uiOutput(ns("fa_params")),
        htmltools::div(
          class = "d-flex gap-2",
          shiny::actionButton(
            ns("fa_save"), i18n$t("foret_ancienne_save"),
            class = "btn-primary btn-sm", icon = bsicons::bs_icon("save")),
          if (!is.null(fa)) shiny::actionButton(
            ns("fa_clear"), i18n$t("foret_ancienne_clear"),
            class = "btn-outline-danger btn-sm",
            icon = bsicons::bs_icon("trash"))
        )
      )
    })

    # Refresh the loaded project metadata after a foret ancienne change.
    .fa_refresh_project <- function(pid) {
      refreshed <- tryCatch(load_project(pid), error = function(e) NULL)
      if (!is.null(refreshed)) {
        rv$current_project <- refreshed
        app_state$current_project <- refreshed
      }
    }

    shiny::observeEvent(input$fa_save, {
      pid <- rv$editing_project_id
      if (is.null(pid)) {
        shiny::showNotification(i18n$t("foret_ancienne_need_project"),
                                type = "warning")
        return()
      }
      f <- input$fa_file
      if (is.null(f)) {
        shiny::showNotification(i18n$t("foret_ancienne_source"),
                                type = "warning")
        return()
      }
      is_raster <- tolower(tools::file_ext(f$name)) %in% c("tif", "tiff")
      fc <- NULL; th <- NULL
      if (is_raster) {
        if (identical(input$fa_mode %||% "class", "threshold")) {
          th <- input$fa_threshold
        } else {
          fc <- input$fa_forest_class
        }
      }
      tryCatch({
        set_project_foret_ancienne(
          pid, f$datapath, f$name,
          forest_class = fc, threshold = th,
          min_area_m2 = input$fa_min_area %||% 0)
        .fa_refresh_project(pid)
        shiny::showNotification(i18n$t("foret_ancienne_saved"), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste(i18n$t("error"), conditionMessage(e)),
                                type = "error")
      })
    })

    shiny::observeEvent(input$fa_clear, {
      pid <- rv$editing_project_id
      shiny::req(pid)
      clear_project_foret_ancienne(pid)
      .fa_refresh_project(pid)
      shiny::showNotification(i18n$t("foret_ancienne_cleared"), type = "message")
    })

    # ========================================
    # Coupes rases → T3 (SUFOSAT, spec 030)
    # ========================================

    # Opt-in national source (no upload) : a toggle + window_years / min_proba,
    # gated on Theia credentials. Writes metadata$sufosat ; the T3 axis then
    # appears on the T family radar (inversion handled in the core).
    output$sufosat_block <- shiny::renderUI({
      header <- htmltools::tags$label(
        class = "form-label fw-semibold", i18n$t("sufosat_section"))
      hint <- htmltools::tags$small(
        class = "text-muted d-block mb-2", i18n$t("sufosat_hint"))

      pid <- rv$editing_project_id
      if (is.null(pid)) {
        return(htmltools::div(
          class = "mb-3", header,
          htmltools::div(class = "text-muted small fst-italic",
                         i18n$t("foret_ancienne_need_project"))))
      }

      # T3 needs the SUFOSAT rasters from Theia — gate on S3 credentials.
      theia_ok <- isTRUE(tryCatch(theia_api_key_configured(),
                                  error = function(e) FALSE))
      if (!theia_ok) {
        return(htmltools::div(
          class = "mb-3 p-2 border rounded", header, hint,
          htmltools::div(
            class = "small text-warning fst-italic",
            bsicons::bs_icon("exclamation-triangle", class = "me-1"),
            i18n$t("sufosat_need_theia"))))
      }

      proj    <- rv$current_project %||% app_state$current_project
      sc      <- proj$metadata$sufosat
      enabled <- isTRUE(sc$enabled)
      status  <- if (enabled) {
        htmltools::div(
          class = "small mb-2",
          bsicons::bs_icon("check-circle-fill", class = "text-success me-1"),
          i18n$t("sufosat_active"))
      } else {
        htmltools::div(class = "small text-muted mb-2 fst-italic",
                       i18n$t("sufosat_none"))
      }

      htmltools::div(
        class = "mb-3 p-2 border rounded",
        header, hint, status,
        shiny::checkboxInput(ns("sufosat_enabled"), i18n$t("sufosat_enable"),
                             value = enabled),
        shiny::sliderInput(
          ns("sufosat_window"), i18n$t("sufosat_window"),
          min = 1, max = 8, value = sc$window_years %||% 5, step = 1,
          width = "100%"),
        shiny::sliderInput(
          ns("sufosat_min_proba"), i18n$t("sufosat_min_proba"),
          min = 0.5, max = 1.0, value = sc$min_proba %||% 0.9, step = 0.05,
          width = "100%"),
        shiny::actionButton(
          ns("sufosat_save"), i18n$t("sufosat_save"),
          class = "btn-primary btn-sm", icon = bsicons::bs_icon("save"))
      )
    })

    shiny::observeEvent(input$sufosat_save, {
      pid <- rv$editing_project_id
      if (is.null(pid)) {
        shiny::showNotification(i18n$t("foret_ancienne_need_project"),
                                type = "warning")
        return()
      }
      tryCatch({
        set_project_sufosat(
          pid,
          enabled      = isTRUE(input$sufosat_enabled),
          window_years = input$sufosat_window %||% 5,
          min_proba    = input$sufosat_min_proba %||% 0.9)
        .fa_refresh_project(pid)
        shiny::showNotification(i18n$t("sufosat_saved"), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste(i18n$t("error"), conditionMessage(e)),
                                type = "error")
      })
    })

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

    # Storage directory (only meaningful for a loaded/edited project)
    output$path_display <- shiny::renderUI({
      pid <- rv$editing_project_id
      if (is.null(pid)) return(NULL)
      path <- get_project_path(pid)
      if (is.null(path)) return(NULL)
      htmltools::div(
        class = "mb-3",
        htmltools::tags$label(
          class = "form-label",
          i18n$t("project_path_label")
        ),
        htmltools::tags$code(
          class = "d-block text-break",
          style = "background:#f8f9fa;padding:.4rem .6rem;border-radius:.25rem;",
          path
        )
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
        shiny::updateSelectInput(session, "groupes_profile",
                                 selected = get_default_groupes_profile())
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

      # Restore UGF groupes profile (fall back to config default)
      shiny::updateSelectInput(
        session,
        "groupes_profile",
        choices = get_groupes_profile_choices(lang = lang),
        selected = project$metadata$groupes_profile %||% get_default_groupes_profile()
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

      # Snapshot the current commune boundary (cached for instant restore).
      commune_geom <- tryCatch(commune_geometry(), error = function(e) NULL)

      tryCatch({
        if (is.null(rv$editing_project_id)) {
          # CREATE mode
          project <- create_project(
            name = trimws(input$name),
            description = input$description %||% "",
            owner = input$owner %||% "",
            parcels = parcels,
            groupes_profile = input$groupes_profile %||% NULL,
            commune_geometry = commune_geom
          )

          rv$current_project <- project
          rv$editing_project_id <- project$id

          # Update app state — reload through load_project() so the UGF
          # tab receives a fully-populated object (parcels + tenements +
          # ugs auto-generated via ensure_project_migrated). Without
          # this, create_project() only returns list(id, path, metadata)
          # and the mod_ug observer short-circuits on is.null(parcels),
          # leaving the UGF map empty.
          full_project <- tryCatch(
            load_project(project$id),
            error = function(e) project
          )
          app_state$current_project <- full_project %||% project
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
            parcels = parcels,
            groupes_profile = input$groupes_profile %||% NULL,
            commune_geometry = commune_geom
          )

          rv$current_project <- project

          # Update app state — see CREATE branch above. update_project()
          # returns parcels + indicators but no tenements/ugs, so we
          # also reload through load_project() to keep the UGF map in
          # sync (especially after a parcel change that may have made
          # the previous UGF layout obsolete).
          full_project <- tryCatch(
            load_project(project$id),
            error = function(e) project
          )
          app_state$current_project <- full_project %||% project
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
        shiny::updateSelectInput(session, "groupes_profile",
                                 selected = get_default_groupes_profile())

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

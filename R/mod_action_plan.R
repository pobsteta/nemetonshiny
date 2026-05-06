#' Action Plan Module for nemetonApp
#'
#' @description
#' Skeleton of the "Plan d'actions" tab. Loads the project's action plan via
#' `service_action_plan.R`, exposes a placeholder layout with a top map and a
#' bottom table, and an internal nav switching between the map+table view and
#' the future Gantt view.
#'
#' Data persistence and validation: see `R/service_action_plan.R`.
#' LLM extraction: see `build_action_plan_prompt` / `parse_action_plan_response`
#' in `R/llm_prompts.R`.
#'
#' Subsequent stories (S4..S15) will wire the carte / tableau / synchro / IA /
#' Gantt / exports.
#'
#' @name mod_action_plan
#' @keywords internal
NULL


# ---- UI --------------------------------------------------------------

#' Action Plan Module UI
#'
#' @param id Character. Module namespace ID.
#' @return Shiny UI elements.
#' @noRd
mod_action_plan_ui <- function(id) {
  ns <- shiny::NS(id)

  opts <- get_app_options()
  lang <- opts$language %||% "fr"
  i18n <- get_i18n(lang)

  bslib::layout_sidebar(
    fillable = TRUE,
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      width = 320,
      open = TRUE,
      shiny::tags$h6(i18n$t("action_plan_filters_title")),
      shiny::sliderInput(
        ns("filter_horizon"),
        label = i18n$t("action_plan_horizon"),
        min = 1L, max = 20L, value = c(1L, 20L), step = 1L
      ),
      shiny::selectizeInput(
        ns("filter_type"),
        label = i18n$t("action_plan_type"),
        choices = NULL, multiple = TRUE
      ),
      shiny::selectizeInput(
        ns("filter_statut"),
        label = i18n$t("action_plan_statut"),
        choices = NULL, multiple = TRUE
      ),
      shiny::selectizeInput(
        ns("filter_famille"),
        label = i18n$t("action_plan_famille"),
        choices = NULL, multiple = TRUE
      ),
      shiny::selectizeInput(
        ns("filter_ug"),
        label = i18n$t("action_plan_ug"),
        choices = NULL, multiple = TRUE
      ),
      shiny::actionButton(
        ns("reset_filters"),
        label = i18n$t("action_plan_reset_filters"),
        icon = shiny::icon("rotate-left"),
        class = "btn-sm btn-outline-secondary"
      )
    ),

    bslib::navset_card_underline(
      id = ns("inner_nav"),

      bslib::nav_panel(
        title = i18n$t("action_plan_view_map_table"),
        value = "map_table",
        icon = bsicons::bs_icon("map"),
        bslib::layout_columns(
          col_widths = c(12, 12),
          row_heights = c(1, 1),

          bslib::card(
            bslib::card_header(
              class = "d-flex justify-content-between align-items-center",
              i18n$t("action_plan_map_title"),
              shiny::radioButtons(
                ns("map_color_by"),
                label = NULL,
                choices = stats::setNames(
                  c("annee", "type", "priorite"),
                  c(i18n$t("action_plan_color_year"),
                    i18n$t("action_plan_color_type"),
                    i18n$t("action_plan_color_priority"))
                ),
                selected = "annee",
                inline = TRUE
              )
            ),
            bslib::card_body(
              shiny::uiOutput(ns("map_placeholder"))
            )
          ),

          bslib::card(
            bslib::card_header(
              class = "d-flex justify-content-between align-items-center",
              i18n$t("action_plan_table_title"),
              htmltools::div(
                class = "d-flex gap-2",
                shiny::actionButton(
                  ns("generate_all"),
                  label = i18n$t("action_plan_generate_all"),
                  icon = shiny::icon("wand-magic-sparkles"),
                  class = "btn-sm btn-primary"
                ),
                shiny::actionButton(
                  ns("add_action"),
                  label = i18n$t("action_plan_add"),
                  icon = shiny::icon("plus"),
                  class = "btn-sm btn-outline-primary"
                )
              )
            ),
            bslib::card_body(
              shiny::uiOutput(ns("table_placeholder"))
            )
          )
        )
      ),

      bslib::nav_panel(
        title = i18n$t("action_plan_view_gantt"),
        value = "gantt",
        icon = bsicons::bs_icon("calendar-range"),
        bslib::card(
          bslib::card_body(
            htmltools::div(class = "text-muted",
                           i18n$t("action_plan_gantt_placeholder"))
          )
        )
      )
    )
  )
}


# ---- Server ----------------------------------------------------------

#' Action Plan Module Server
#'
#' @param id Character. Module namespace ID.
#' @param app_state reactiveValues. Application state, must expose
#'   `current_project` and `language`.
#' @return Invisible NULL.
#' @noRd
mod_action_plan_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive: current plan (loaded from disk for the active project,
    # initialized empty otherwise).
    plan_rv <- shiny::reactiveVal(NULL)

    shiny::observe({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$id)) {
        plan_rv(NULL)
        return()
      }
      plan <- tryCatch(
        load_action_plan(project$id),
        error = function(e) {
          cli::cli_warn("Failed to load action plan: {e$message}")
          init_empty_action_plan(project$id)
        }
      )
      plan_rv(plan)
    })

    # Helper: list of UG ids for the current project (character()).
    ug_ids <- shiny::reactive({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$ugs)) return(character())
      ugs <- project$ugs
      if (inherits(ugs, "sf")) {
        ugs <- tryCatch(sf::st_drop_geometry(ugs),
                        error = function(e) as.data.frame(ugs))
      }
      if ("ug_id" %in% names(ugs)) {
        as.character(ugs$ug_id)
      } else {
        character()
      }
    })

    # Update filter choices when language / plan changes.
    shiny::observe({
      i18n <- get_i18n(app_state$language)
      shiny::updateSelectizeInput(session, "filter_type",
                                  choices = ACTION_PLAN_TYPES,
                                  server = FALSE)
      shiny::updateSelectizeInput(session, "filter_statut",
                                  choices = ACTION_PLAN_STATUTS,
                                  server = FALSE)
      shiny::updateSelectizeInput(session, "filter_famille",
                                  choices = ACTION_PLAN_FAMILY_CODES,
                                  server = FALSE)
      shiny::updateSelectizeInput(session, "filter_ug",
                                  choices = ug_ids(),
                                  server = FALSE)
      plan <- plan_rv()
      horizon <- plan$horizon_annees %||% 20L
      shiny::updateSliderInput(session, "filter_horizon",
                               min = 1L, max = as.integer(horizon),
                               value = c(1L, as.integer(horizon)))
    })

    # Reset filters
    shiny::observeEvent(input$reset_filters, {
      shiny::updateSelectizeInput(session, "filter_type",  selected = character())
      shiny::updateSelectizeInput(session, "filter_statut", selected = character())
      shiny::updateSelectizeInput(session, "filter_famille", selected = character())
      shiny::updateSelectizeInput(session, "filter_ug",    selected = character())
      plan <- plan_rv()
      horizon <- plan$horizon_annees %||% 20L
      shiny::updateSliderInput(session, "filter_horizon",
                               value = c(1L, as.integer(horizon)))
    })

    # Filtered actions (used downstream by carte/tableau/Gantt).
    filtered_actions <- shiny::reactive({
      plan <- plan_rv()
      if (is.null(plan)) return(list())
      h <- input$filter_horizon
      filter_actions(
        plan,
        ug_id    = if (length(input$filter_ug)      > 0) input$filter_ug      else NULL,
        type     = if (length(input$filter_type)    > 0) input$filter_type    else NULL,
        statut   = if (length(input$filter_statut)  > 0) input$filter_statut  else NULL,
        famille  = if (length(input$filter_famille) > 0) input$filter_famille else NULL,
        annee_min = if (!is.null(h)) as.integer(h[1]) else NULL,
        annee_max = if (!is.null(h)) as.integer(h[2]) else NULL
      )
    })

    # Placeholders rendered until S4/S5 land.
    output$map_placeholder <- shiny::renderUI({
      i18n <- get_i18n(app_state$language)
      project <- app_state$current_project
      if (is.null(project)) {
        return(htmltools::div(class = "text-muted", i18n$t("no_project")))
      }
      htmltools::div(class = "text-muted",
                     i18n$t("action_plan_map_pending"))
    })

    output$table_placeholder <- shiny::renderUI({
      i18n <- get_i18n(app_state$language)
      project <- app_state$current_project
      if (is.null(project)) {
        return(htmltools::div(class = "text-muted", i18n$t("no_project")))
      }
      n <- length(filtered_actions())
      htmltools::div(
        htmltools::p(class = "text-muted",
                     i18n$t("action_plan_table_pending")),
        htmltools::p(class = "small",
                     sprintf(i18n$t("action_plan_table_count_fmt"), n))
      )
    })

    # IA / add buttons: handlers land in S7 / S8 / S5. Clicks today are
    # acknowledged via a minimal toast so users get a clear "skeleton" signal.
    shiny::observeEvent(input$generate_all, {
      i18n <- get_i18n(app_state$language)
      shiny::showNotification(i18n$t("action_plan_generate_pending"),
                              type = "message", duration = 4)
    })
    shiny::observeEvent(input$add_action, {
      i18n <- get_i18n(app_state$language)
      shiny::showNotification(i18n$t("action_plan_add_pending"),
                              type = "message", duration = 4)
    })

    invisible(NULL)
  })
}

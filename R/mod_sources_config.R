#' Optional Theia sources configuration module
#'
#' @description
#' The two **opt-in Theia sources** of the application, grouped in their own
#' tab of the settings (gear) modal:
#'
#'   * **Coupes rases (SUFOSAT)** — national Sentinel-1 clear-cut detection
#'     feeding the T3 indicator (spec 030): toggle + `window_years` /
#'     `min_proba`.
#'   * **Rafraîchissement urbain (LST)** — Theia/Thermocity surface coolness
#'     feeding the A5 indicator (spec 032): toggle + `buffer_m`.
#'
#' Both blocks used to live in the project card (`mod_project`), where they
#' stretched an already long form and were easy to miss. They belong with the
#' other external-service settings, next to the Theia credentials they depend
#' on — hence this module, mounted as a tab of `mod_theia_config`'s modal.
#'
#' Both sources are **enabled by default** (see `project_sufosat_enabled()` /
#' `project_lst_enabled()`): a project that never visited this tab still gets
#' T3 and A5. The Theia fetch stays gated on credentials being configured, and
#' a failed / out-of-coverage fetch degrades to `NA` per unit — never an error.
#'
#' @name mod_sources_config
#' @keywords internal
NULL


#' Optional sources configuration UI
#'
#' @param id Character. Module namespace ID.
#'
#' @return A `div` holding the two source blocks.
#' @noRd
mod_sources_config_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    class = "pt-3",
    shiny::uiOutput(ns("intro")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::uiOutput(ns("sufosat_block")),
      shiny::uiOutput(ns("lst_block"))
    )
  )
}


#' Optional sources configuration server
#'
#' @param id Character. Module namespace ID.
#' @param app_state reactiveValues. Shared application state (uses
#'   `$language`, `$current_project`, `$project_id`).
#'
#' @return Invisible NULL.
#' @noRd
mod_sources_config_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # La langue peut changer en cours de session : i18n en reactive, pas en
    # valeur figée à l'instanciation du module.
    i18n_r <- shiny::reactive(get_i18n(app_state$language %||% "fr"))

    # Bumpé après un enregistrement pour re-rendre les deux blocs.
    refresh <- shiny::reactiveVal(0)

    # Le projet courant porte son id ; `app_state$project_id` sert de repli.
    .pid <- function() {
      proj <- app_state$current_project
      pid <- proj$id %||% app_state$project_id
      if (is.null(pid) || !nzchar(as.character(pid))) NULL else as.character(pid)
    }

    # Recharge le projet après écriture des métadonnées, pour que le reste de
    # l'app (calcul, radar) voie la nouvelle configuration sans rouvrir.
    .refresh_project <- function(pid) {
      refreshed <- tryCatch(load_project(pid), error = function(e) NULL)
      if (!is.null(refreshed)) app_state$current_project <- refreshed
      refresh(refresh() + 1)
    }

    output$intro <- shiny::renderUI({
      shiny::p(class = "text-muted small", i18n_r()$t("sources_config_intro"))
    })

    # ========================================
    # Coupes rases → T3 (SUFOSAT, spec 030)
    # ========================================

    output$sufosat_block <- shiny::renderUI({
      i18n <- i18n_r()
      refresh()
      header <- htmltools::tags$label(
        class = "form-label fw-semibold", i18n$t("sufosat_section"))
      hint <- htmltools::tags$small(
        class = "text-muted d-block mb-2", i18n$t("sufosat_hint"))

      pid <- .pid()
      if (is.null(pid)) {
        return(htmltools::div(
          class = "mb-3 p-2 border rounded", header, hint,
          htmltools::div(class = "text-muted small fst-italic",
                         i18n$t("sources_need_project"))))
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

      proj    <- app_state$current_project
      sc      <- proj$metadata$sufosat
      enabled <- project_sufosat_enabled(proj$metadata)
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
        class = "mb-3 p-2 border rounded h-100",
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
      i18n <- i18n_r()
      if (deny_if_readonly(app_state)) return()
      pid <- .pid()
      if (is.null(pid)) {
        shiny::showNotification(i18n$t("sources_need_project"), type = "warning")
        return()
      }
      tryCatch({
        set_project_sufosat(
          pid,
          enabled      = isTRUE(input$sufosat_enabled),
          window_years = input$sufosat_window %||% 5,
          min_proba    = input$sufosat_min_proba %||% 0.9)
        .refresh_project(pid)
        shiny::showNotification(i18n$t("sufosat_saved"), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste(i18n$t("error"), conditionMessage(e)),
                                type = "error")
      })
    })

    # ========================================
    # Rafraîchissement urbain → A5 (LST, spec 032)
    # ========================================

    output$lst_block <- shiny::renderUI({
      i18n <- i18n_r()
      refresh()
      header <- htmltools::tags$label(
        class = "form-label fw-semibold", i18n$t("lst_section"))
      hint <- htmltools::tags$small(
        class = "text-muted d-block mb-2", i18n$t("lst_hint"))

      pid <- .pid()
      if (is.null(pid)) {
        return(htmltools::div(
          class = "mb-3 p-2 border rounded", header, hint,
          htmltools::div(class = "text-muted small fst-italic",
                         i18n$t("sources_need_project"))))
      }

      # A5 needs the LST raster from Theia — gate on S3 credentials.
      theia_ok <- isTRUE(tryCatch(theia_api_key_configured(),
                                  error = function(e) FALSE))
      if (!theia_ok) {
        return(htmltools::div(
          class = "mb-3 p-2 border rounded", header, hint,
          htmltools::div(
            class = "small text-warning fst-italic",
            bsicons::bs_icon("exclamation-triangle", class = "me-1"),
            i18n$t("lst_need_theia"))))
      }

      proj    <- app_state$current_project
      lc      <- proj$metadata$lst_urbain
      enabled <- project_lst_enabled(proj$metadata)
      status  <- if (enabled) {
        htmltools::div(
          class = "small mb-2",
          bsicons::bs_icon("check-circle-fill", class = "text-success me-1"),
          i18n$t("lst_active"))
      } else {
        htmltools::div(class = "small text-muted mb-2 fst-italic",
                       i18n$t("lst_none"))
      }

      htmltools::div(
        class = "mb-3 p-2 border rounded h-100",
        header, hint, status,
        shiny::checkboxInput(ns("lst_enabled"), i18n$t("lst_enable"),
                             value = enabled),
        shiny::sliderInput(
          ns("lst_buffer"), i18n$t("lst_buffer"),
          min = 100, max = 2000, value = lc$buffer_m %||% 500, step = 100,
          width = "100%"),
        shiny::actionButton(
          ns("lst_save"), i18n$t("lst_save"),
          class = "btn-primary btn-sm", icon = bsicons::bs_icon("save"))
      )
    })

    shiny::observeEvent(input$lst_save, {
      i18n <- i18n_r()
      if (deny_if_readonly(app_state)) return()
      pid <- .pid()
      if (is.null(pid)) {
        shiny::showNotification(i18n$t("sources_need_project"), type = "warning")
        return()
      }
      tryCatch({
        set_project_lst_urbain(
          pid,
          enabled  = isTRUE(input$lst_enabled),
          buffer_m = input$lst_buffer %||% 500)
        .refresh_project(pid)
        shiny::showNotification(i18n$t("lst_saved"), type = "message")
      }, error = function(e) {
        shiny::showNotification(paste(i18n$t("error"), conditionMessage(e)),
                                type = "error")
      })
    })

    invisible(NULL)
  })
}

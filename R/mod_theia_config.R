#' External API keys configuration module
#'
#' @description
#' Navbar entry that opens a tabbed modal to configure the API keys
#' used by the application :
#'
#'   * **Theia / DATA TERRA** — access + secret key for the Python
#'     `teledetection` SDK, plus the Python prerequisite status and
#'     the provenance / licensing of the Theia data sources.
#'   * **LLM** — API key for one of Mistral / Anthropic / OpenAI,
#'     used by `ellmer` to drive the expert perspectives.
#'
#' The module is named `mod_theia_config` for historical reasons —
#' it was originally Theia-only. The wiring (`run_app.R`) still calls
#' that name ; only the modal layout has been widened to a tabset.
#'
#' @name mod_theia_config
#' @keywords internal
NULL


#' Theia configuration UI (navbar item)
#'
#' @param id Character. Module namespace ID.
#'
#' @return A `bslib::nav_item` with a gear action link.
#' @noRd
mod_theia_config_ui <- function(id) {
  ns <- shiny::NS(id)

  opts <- get_app_options()
  i18n <- get_i18n(opts$language %||% "fr")

  bslib::nav_item(
    shiny::actionLink(
      inputId = ns("open"),
      label = NULL,
      icon = bsicons::bs_icon("gear"),
      class = "nav-link",
      title = i18n$t("api_keys_config_open")
    )
  )
}


#' Build the status alert for a Theia readiness entry
#'
#' @param ok Logical. Whether the prerequisite is satisfied.
#' @param label Character. Prerequisite label.
#' @param detail Character. Detail message.
#'
#' @return An HTML div.
#' @noRd
.theia_status_alert <- function(ok, label, detail) {
  cls <- if (isTRUE(ok)) "alert alert-success" else "alert alert-warning"
  icon <- if (isTRUE(ok)) "check-circle-fill" else "exclamation-triangle-fill"
  htmltools::div(
    class = cls,
    role = "alert",
    bsicons::bs_icon(icon),
    htmltools::tags$strong(htmltools::HTML("&nbsp;"), label),
    htmltools::tags$div(detail)
  )
}


#' Theia configuration server
#'
#' @param id Character. Module namespace ID.
#' @param app_state reactiveValues. Shared application state (uses
#'   `$language`).
#'
#' @return Invisible NULL.
#' @noRd
mod_theia_config_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Bumped after a successful save / delete to refresh the modal.
    status_refresh <- shiny::reactiveVal(0)
    # Theia edit form revealed even though a key is already saved.
    theia_edit_mode <- shiny::reactiveVal(FALSE)
    # LLM edit form revealed for the currently selected provider.
    llm_edit_mode <- shiny::reactiveVal(FALSE)
    # Preserve the active tab / selected provider across modal
    # re-renders (every save/edit/delete re-shows the modal — without
    # these, the user would jump back to the first tab and the first
    # provider on every click).
    active_tab <- shiny::reactiveVal("tab_theia")
    active_provider <- shiny::reactiveVal("mistral")

    # ----- Theia tab content ------------------------------------------
    .render_theia_tab <- function(i18n) {
      status <- theia_status()
      python_detail <- if (isTRUE(status$python_ok))
        i18n$t("theia_python_ok") else i18n$t("theia_error_reticulate")
      key_detail <- if (isTRUE(status$key_ok))
        i18n$t("theia_key_ok") else i18n$t("theia_error_no_key")

      provenance <- tryCatch(theia_source_provenance(),
                             error = function(e) NULL)

      key_section <- if (isTRUE(status$key_ok) && !isTRUE(theia_edit_mode())) {
        htmltools::div(
          htmltools::tags$p(class = "text-muted small mb-2",
                            i18n$t("theia_key_configured_hint")),
          shiny::actionButton(
            ns("edit_key"), i18n$t("theia_key_edit"),
            icon = shiny::icon("pencil"), class = "btn-outline-primary"),
          " ",
          shiny::actionButton(
            ns("delete_key"), i18n$t("theia_key_delete"),
            icon = shiny::icon("trash"), class = "btn-outline-danger")
        )
      } else {
        htmltools::div(
          shiny::textInput(ns("access_key"),
                           i18n$t("theia_key_label_access"),
                           width = "100%"),
          shiny::passwordInput(ns("secret_key"),
                               i18n$t("theia_key_label_secret"),
                               width = "100%"),
          shiny::helpText(i18n$t("theia_key_help")),
          shiny::actionButton(ns("save_key"), i18n$t("theia_key_save"),
                              icon = shiny::icon("key"),
                              class = "btn-primary"),
          if (isTRUE(theia_edit_mode())) " ",
          if (isTRUE(theia_edit_mode())) shiny::actionButton(
            ns("cancel_edit"), i18n$t("cancel"),
            class = "btn-outline-secondary")
        )
      }

      provenance_section <- if (is.null(provenance) || nrow(provenance) == 0) {
        shiny::p(shiny::em(i18n$t("theia_provenance_empty")))
      } else {
        prov <- provenance[, c("name", "provenance",
                               "consumed_by", "license"),
                           drop = FALSE]
        headers <- c(i18n$t("theia_col_source"),
                     i18n$t("theia_col_provenance"),
                     i18n$t("theia_col_consumed_by"),
                     i18n$t("theia_col_license"))
        htmltools::div(
          class = "table-responsive",
          htmltools::tags$table(
            class = "table table-sm table-striped small mb-0",
            htmltools::tags$thead(
              htmltools::tags$tr(lapply(headers, htmltools::tags$th))),
            htmltools::tags$tbody(
              lapply(seq_len(nrow(prov)), function(i) {
                htmltools::tags$tr(
                  lapply(seq_len(ncol(prov)), function(j) {
                    htmltools::tags$td(as.character(prov[i, j]))
                  })
                )
              })
            )
          )
        )
      }

      htmltools::div(
        class = "pt-3",
        shiny::p(i18n$t("theia_config_intro")),
        .theia_status_alert(status$python_ok,
                            i18n$t("theia_python_status_label"),
                            python_detail),
        .theia_status_alert(status$key_ok,
                            i18n$t("theia_key_status_label"),
                            key_detail),
        shiny::hr(),
        key_section,
        shiny::hr(),
        shiny::h5(i18n$t("theia_provenance_title")),
        provenance_section
      )
    }

    # ----- LLM tab content --------------------------------------------
    .render_llm_tab <- function(i18n) {
      providers <- llm_providers()
      choices   <- stats::setNames(names(providers),
                                   vapply(providers, `[[`, character(1),
                                          "label"))
      sel_provider <- active_provider()
      if (!(sel_provider %in% names(providers))) {
        sel_provider <- names(providers)[1]
      }
      st_all <- llm_status_all()
      st <- st_all[[sel_provider]]
      configured <- isTRUE(st$configured)

      # Status alert summarises configured-or-not plus the source
      # (env var or local file). Matches the Theia per-prerequisite
      # alert shape so the modal stays visually coherent.
      status_label <- i18n$t("llm_status_label")
      if (configured) {
        src_msg <- if (isTRUE(st$env_ok))
          sprintf(i18n$t("llm_status_source_env"), st$env_name)
        else i18n$t("llm_status_source_file")
        status_detail <- htmltools::tagList(
          sprintf(i18n$t("llm_status_ok"), st$label),
          htmltools::tags$br(),
          htmltools::tags$small(class = "text-muted", src_msg)
        )
      } else {
        status_detail <- sprintf(i18n$t("llm_status_ko"), st$label)
      }

      key_section <- if (configured && !isTRUE(llm_edit_mode())) {
        htmltools::div(
          htmltools::tags$p(class = "text-muted small mb-2",
                            i18n$t("llm_key_configured_hint")),
          shiny::actionButton(
            ns("llm_edit_key"), i18n$t("llm_key_edit"),
            icon = shiny::icon("pencil"),
            class = "btn-outline-primary"),
          " ",
          shiny::actionButton(
            ns("llm_delete_key"), i18n$t("llm_key_delete"),
            icon = shiny::icon("trash"),
            class = "btn-outline-danger")
        )
      } else {
        htmltools::div(
          shiny::passwordInput(
            ns("llm_key"), i18n$t("llm_key_label"),
            width = "100%"),
          shiny::helpText(i18n$t("llm_key_help")),
          shiny::actionButton(
            ns("llm_save_key"), i18n$t("llm_key_save"),
            icon = shiny::icon("key"),
            class = "btn-primary"),
          if (isTRUE(llm_edit_mode())) " ",
          if (isTRUE(llm_edit_mode())) shiny::actionButton(
            ns("llm_cancel_edit"), i18n$t("cancel"),
            class = "btn-outline-secondary")
        )
      }

      htmltools::div(
        class = "pt-3",
        shiny::p(i18n$t("llm_config_intro")),
        shiny::selectInput(
          ns("llm_provider"), i18n$t("llm_provider_label"),
          choices = choices, selected = sel_provider,
          width = "240px"),
        .theia_status_alert(configured, status_label, status_detail),
        shiny::hr(),
        key_section
      )
    }

    render_modal <- function() {
      i18n <- get_i18n(app_state$language %||% "fr")
      status_refresh()  # take a dependency so the modal reflects saves

      shiny::modalDialog(
        title = i18n$t("api_keys_config_title"),
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton(i18n$t("close")),
        shiny::p(i18n$t("api_keys_config_intro")),
        shiny::tabsetPanel(
          id = ns("config_tab"),
          selected = active_tab(),
          type = "tabs",
          shiny::tabPanel(
            title = i18n$t("api_keys_tab_theia"),
            value = "tab_theia",
            .render_theia_tab(i18n)),
          shiny::tabPanel(
            title = i18n$t("api_keys_tab_llm"),
            value = "tab_llm",
            .render_llm_tab(i18n))
        )
      )
    }

    shiny::observeEvent(input$open, {
      shiny::showModal(render_modal())
    })

    # Preserve tab + provider selection across re-renders.
    shiny::observeEvent(input$config_tab, {
      active_tab(input$config_tab)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$llm_provider, {
      active_provider(input$llm_provider)
      # Cancel any pending edit when switching providers — the form
      # would otherwise carry over to a different provider's context.
      llm_edit_mode(FALSE)
    }, ignoreInit = TRUE)

    # ----- Theia observers --------------------------------------------
    shiny::observeEvent(input$save_key, {
      i18n <- get_i18n(app_state$language %||% "fr")
      access <- input$access_key %||% ""
      secret <- input$secret_key %||% ""
      if (!nzchar(trimws(access)) || !nzchar(trimws(secret))) {
        shiny::showNotification(i18n$t("theia_key_missing"),
                                type = "warning")
        return()
      }
      ok <- theia_save_api_key(access, secret)
      if (isTRUE(ok)) {
        shiny::showNotification(i18n$t("theia_key_saved"), type = "message")
        theia_edit_mode(FALSE)
        status_refresh(status_refresh() + 1)
        shiny::showModal(render_modal())
      } else {
        shiny::showNotification(i18n$t("theia_key_save_failed"),
                                type = "error")
      }
    })
    shiny::observeEvent(input$edit_key, {
      theia_edit_mode(TRUE)
      shiny::showModal(render_modal())
    })
    shiny::observeEvent(input$cancel_edit, {
      theia_edit_mode(FALSE)
      shiny::showModal(render_modal())
    })
    shiny::observeEvent(input$delete_key, {
      i18n <- get_i18n(app_state$language %||% "fr")
      cleared <- theia_clear_api_key()
      if (isTRUE(cleared)) {
        shiny::showNotification(i18n$t("theia_key_deleted"),
                                type = "warning")
      }
      theia_edit_mode(FALSE)
      status_refresh(status_refresh() + 1)
      shiny::showModal(render_modal())
    })

    # ----- LLM observers ----------------------------------------------
    shiny::observeEvent(input$llm_save_key, {
      i18n <- get_i18n(app_state$language %||% "fr")
      provider <- input$llm_provider %||% active_provider()
      key <- input$llm_key %||% ""
      if (!nzchar(trimws(key))) {
        shiny::showNotification(i18n$t("llm_key_missing"), type = "warning")
        return()
      }
      ok <- llm_save_api_key(provider, key)
      if (isTRUE(ok)) {
        shiny::showNotification(i18n$t("llm_key_saved"), type = "message")
        llm_edit_mode(FALSE)
        status_refresh(status_refresh() + 1)
        shiny::showModal(render_modal())
      } else {
        shiny::showNotification(i18n$t("llm_key_save_failed"),
                                type = "error")
      }
    })
    shiny::observeEvent(input$llm_edit_key, {
      llm_edit_mode(TRUE)
      shiny::showModal(render_modal())
    })
    shiny::observeEvent(input$llm_cancel_edit, {
      llm_edit_mode(FALSE)
      shiny::showModal(render_modal())
    })
    shiny::observeEvent(input$llm_delete_key, {
      i18n <- get_i18n(app_state$language %||% "fr")
      provider <- input$llm_provider %||% active_provider()
      cleared <- llm_clear_api_key(provider)
      if (isTRUE(cleared)) {
        shiny::showNotification(i18n$t("llm_key_deleted"),
                                type = "warning")
      }
      llm_edit_mode(FALSE)
      status_refresh(status_refresh() + 1)
      shiny::showModal(render_modal())
    })

    invisible(NULL)
  })
}

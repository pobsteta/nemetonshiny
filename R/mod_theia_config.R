#' Theia / DATA TERRA configuration module
#'
#' @description
#' Navbar entry that opens a modal to configure the Theia API key,
#' shows the Python / reticulate prerequisite status, and displays
#' the provenance / licensing of the Theia data sources.
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
      title = i18n$t("theia_config_open")
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

    # Bumped after a successful key save to refresh the status panel.
    status_refresh <- shiny::reactiveVal(0)

    render_modal <- function() {
      i18n <- get_i18n(app_state$language %||% "fr")
      status_refresh()  # take a dependency so the modal reflects saves
      status <- theia_status()

      python_detail <- if (isTRUE(status$python_ok)) {
        i18n$t("theia_status_ready")
      } else if (identical(status$python_reason, "reticulate")) {
        i18n$t("theia_error_reticulate")
      } else {
        i18n$t("theia_error_python_modules")
      }
      key_detail <- if (isTRUE(status$key_ok)) {
        i18n$t("theia_status_ready")
      } else {
        i18n$t("theia_error_no_key")
      }

      provenance <- tryCatch(theia_source_provenance(),
                             error = function(e) NULL)

      shiny::modalDialog(
        title = i18n$t("theia_config_title"),
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton(i18n$t("close")),

        shiny::p(i18n$t("theia_config_intro")),

        .theia_status_alert(
          status$python_ok, i18n$t("theia_python_status_label"),
          python_detail
        ),
        .theia_status_alert(
          status$key_ok, i18n$t("theia_key_status_label"),
          key_detail
        ),

        shiny::hr(),

        shiny::textInput(
          ns("access_key"), i18n$t("theia_key_label_access"),
          width = "100%"
        ),
        shiny::passwordInput(
          ns("secret_key"), i18n$t("theia_key_label_secret"),
          width = "100%"
        ),
        shiny::helpText(i18n$t("theia_key_help")),
        shiny::actionButton(
          ns("save_key"), i18n$t("theia_key_save"),
          icon = shiny::icon("key"),
          class = "btn-primary"
        ),

        shiny::hr(),

        shiny::h5(i18n$t("theia_provenance_title")),
        if (is.null(provenance) || nrow(provenance) == 0) {
          shiny::p(shiny::em(i18n$t("theia_provenance_empty")))
        } else {
          # Drop the technical key column, keep the human-readable name.
          prov <- provenance[, c("name", "provenance", "consumed_by", "license"),
                             drop = FALSE]
          names(prov) <- c(
            i18n$t("theia_col_source"), i18n$t("theia_col_provenance"),
            i18n$t("theia_col_consumed_by"), i18n$t("theia_col_license")
          )
          htmltools::div(
            class = "table-responsive",
            DT::datatable(
              prov,
              rownames = FALSE,
              options = list(dom = "t", paging = FALSE, ordering = FALSE),
              selection = "none"
            )
          )
        }
      )
    }

    shiny::observeEvent(input$open, {
      shiny::showModal(render_modal())
    })

    shiny::observeEvent(input$save_key, {
      i18n <- get_i18n(app_state$language %||% "fr")
      access <- input$access_key %||% ""
      secret <- input$secret_key %||% ""
      if (!nzchar(trimws(access)) || !nzchar(trimws(secret))) {
        shiny::showNotification(
          i18n$t("theia_key_missing"), type = "warning"
        )
        return()
      }
      ok <- theia_save_api_key(access, secret)
      if (isTRUE(ok)) {
        shiny::showNotification(i18n$t("theia_key_saved"), type = "message")
        status_refresh(status_refresh() + 1)
        shiny::showModal(render_modal())
      } else {
        shiny::showNotification(
          i18n$t("theia_key_save_failed"), type = "error"
        )
      }
    })

    invisible(NULL)
  })
}

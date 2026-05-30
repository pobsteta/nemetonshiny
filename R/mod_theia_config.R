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

    # Bumped after a successful key save / delete to refresh the modal.
    status_refresh <- shiny::reactiveVal(0)
    # When TRUE, force the edit form to render even though a key is
    # already configured (user clicked "Modifier la clé").
    edit_mode <- shiny::reactiveVal(FALSE)

    render_modal <- function() {
      i18n <- get_i18n(app_state$language %||% "fr")
      status_refresh()  # take a dependency so the modal reflects saves
      status <- theia_status()

      # v0.46.4 — wording honnête : python_ok signifie « reticulate
      # installé », les modules Python sont provisionnés à l'usage
      # (cf. theia_python_ready). Plus de fausse promesse « Python
      # prerequisite ready » alors qu'on n'a pas pu le vérifier.
      python_detail <- if (isTRUE(status$python_ok)) {
        i18n$t("theia_python_ok")
      } else {
        i18n$t("theia_error_reticulate")
      }
      key_detail <- if (isTRUE(status$key_ok)) {
        i18n$t("theia_key_ok")
      } else {
        i18n$t("theia_error_no_key")
      }

      provenance <- tryCatch(theia_source_provenance(),
                             error = function(e) NULL)

      # v0.51.6 — section clé contextuelle :
      #   • clé déjà configurée + pas en mode édition →
      #     bandeau "configurée" + boutons Modifier / Supprimer
      #     (plus de formulaire vide invitant à écraser).
      #   • sinon (jamais configurée OU édition demandée) → formulaire
      #     access + secret + Enregistrer (+ Annuler en mode édition).
      key_section <- if (isTRUE(status$key_ok) && !isTRUE(edit_mode())) {
        htmltools::div(
          htmltools::tags$p(
            class = "text-muted small mb-2",
            i18n$t("theia_key_configured_hint")
          ),
          shiny::actionButton(
            ns("edit_key"), i18n$t("theia_key_edit"),
            icon  = shiny::icon("pencil"),
            class = "btn-outline-primary"
          ),
          " ",
          shiny::actionButton(
            ns("delete_key"), i18n$t("theia_key_delete"),
            icon  = shiny::icon("trash"),
            class = "btn-outline-danger"
          )
        )
      } else {
        htmltools::div(
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
            icon  = shiny::icon("key"),
            class = "btn-primary"
          ),
          if (isTRUE(edit_mode())) " ",
          if (isTRUE(edit_mode())) shiny::actionButton(
            ns("cancel_edit"), i18n$t("cancel"),
            class = "btn-outline-secondary"
          )
        )
      }

      # v0.51.6 — provenance : table Bootstrap statique (HTML pur). Le
      # DT::datatable inline dans un modalDialog ne s'initialisait pas
      # (htmlwidget JS pas câblé hors d'un DTOutput) → table invisible.
      # On n'a besoin ni de tri ni de pagination (info statique) → une
      # table HTML simple est strictement meilleure ici.
      provenance_section <- if (is.null(provenance) || nrow(provenance) == 0) {
        shiny::p(shiny::em(i18n$t("theia_provenance_empty")))
      } else {
        prov <- provenance[, c("name", "provenance", "consumed_by", "license"),
                           drop = FALSE]
        headers <- c(
          i18n$t("theia_col_source"), i18n$t("theia_col_provenance"),
          i18n$t("theia_col_consumed_by"), i18n$t("theia_col_license")
        )
        htmltools::div(
          class = "table-responsive",
          htmltools::tags$table(
            class = "table table-sm table-striped small mb-0",
            htmltools::tags$thead(
              htmltools::tags$tr(lapply(headers, htmltools::tags$th))
            ),
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
        key_section,
        shiny::hr(),

        shiny::h5(i18n$t("theia_provenance_title")),
        provenance_section
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
        edit_mode(FALSE)
        status_refresh(status_refresh() + 1)
        shiny::showModal(render_modal())
      } else {
        shiny::showNotification(
          i18n$t("theia_key_save_failed"), type = "error"
        )
      }
    })

    # « Modifier la clé » → révèle le formulaire (clé déjà configurée).
    shiny::observeEvent(input$edit_key, {
      edit_mode(TRUE)
      shiny::showModal(render_modal())
    })

    # « Annuler » en mode édition → referme le formulaire sans rien changer.
    shiny::observeEvent(input$cancel_edit, {
      edit_mode(FALSE)
      shiny::showModal(render_modal())
    })

    # « Supprimer la clé » → unlink du .apikey + Sys.unsetenv des TLD_*,
    # toast de confirmation, modal re-rendu.
    shiny::observeEvent(input$delete_key, {
      i18n <- get_i18n(app_state$language %||% "fr")
      cleared <- theia_clear_api_key()
      if (isTRUE(cleared)) {
        shiny::showNotification(i18n$t("theia_key_deleted"), type = "warning")
      }
      edit_mode(FALSE)
      status_refresh(status_refresh() + 1)
      shiny::showModal(render_modal())
    })

    invisible(NULL)
  })
}

# Module "effet de la gestion" de la Synthese (mission 067, spec 068 du coeur).
#
# Un bouton lance, a la demande, `nemeton::effet_gestion()` (via
# `R/service_effet_gestion.R`) sur le type de gestion declare par UGF ; le
# tableau « ce que la gestion a produit sur ce massif » s'affiche seulement si au
# moins deux types sont renseignes. Aucune logique metier : l'effet, l'intervalle
# a 90 % et le statut viennent du coeur. Le calcul est long (quelques dizaines de
# secondes avec ranger) : `ExtendedTask`, retour immediat, bouton lie a la tache.

#' Effet de la gestion - UI
#' @param id Identifiant du module.
#' @noRd
mod_effet_gestion_ui <- function(id) {
  ns <- shiny::NS(id)
  i18n <- get_i18n(get_app_options()$language)
  bslib::card(
    id = ns("carte"),
    bslib::card_header(i18n$t("eg_title")),
    bslib::card_body(
      shiny::uiOutput(ns("intro")),
      shiny::uiOutput(ns("exclues")),
      htmltools::div(
        class = "mb-3",
        bslib::input_task_button(
          ns("run"), i18n$t("eg_run"),
          icon = shiny::icon("scale-balanced"),
          label_busy = i18n$t("eg_busy"),
          # Bouton a bordure : le PDF reste l'action principale de la Synthese.
          type = "outline-primary"
        )
      ),
      shiny::uiOutput(ns("reference")),
      shiny::uiOutput(ns("etat")),
      shiny::tableOutput(ns("tableau")),
      shiny::uiOutput(ns("mention"))
    )
  )
}

#' Signature des types de gestion d'un projet (detecte un resultat perime)
#' @noRd
.eg_signature <- function(ugs) {
  if (is.null(ugs) || !is.data.frame(ugs) || nrow(ugs) == 0L) return("")
  ugs <- .ug_gestion_normaliser(ugs)
  paste(ugs$ug_id, ugs$type_gestion, sep = "=", collapse = ";")
}

#' Effet de la gestion - serveur
#'
#' @param id Identifiant du module.
#' @param app_state reactiveValues de l'application.
#' @return Une liste de reactives : `status` (etat de la tache), `resultat`
#'   (dernier resultat du service, ou `NULL`).
#' @noRd
mod_effet_gestion_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(resultat = NULL, pid = NULL, signature = NULL,
                                ref_calc = NULL)

    i18n_r <- shiny::reactive(get_i18n(app_state$language %||% "fr"))
    ugs_r <- shiny::reactive(app_state$current_project$ugs)
    signature_r <- shiny::reactive(.eg_signature(ugs_r()))
    types_r <- shiny::reactive(effet_gestion_types_renseignes(ugs_r()))

    # Tache longue : worker `future` (jamais dans la boucle Shiny).
    .dev_pkg_path <- tryCatch(
      if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
        find.package("nemetonshiny") else NULL,
      error = function(e) NULL)
    task <- shiny::ExtendedTask$new(function(units, reference, dev_path, app_opts) {
      # Tests : execution dans le processus (le coeur est simule), toujours
      # derriere une promesse - meme sequence d'etats que le worker.
      if (isTRUE(getOption("nemetonshiny.effet_gestion_inline"))) {
        return(promises::promise(function(resolve, reject) {
          .later_sur(function() {
            tryCatch(resolve(effet_gestion_calculer(units, reference)),
                     error = function(e) reject(e))
          })
        }))
      }
      if (requireNamespace("future", quietly = TRUE)) {
        plan_classes <- class(future::plan())
        if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
          .ensure_async_plan()
        }
      }
      promises::future_promise({
        on.exit(utils::getFromNamespace(".release_worker_memory", "nemetonshiny")(), add = TRUE)
        if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
          pkgload::load_all(dev_path, quiet = TRUE)
        } else {
          loadNamespace("nemetonshiny")
        }
        options(nemeton.app_options = app_opts)
        utils::getFromNamespace("effet_gestion_calculer", "nemetonshiny")(units, reference)
      }, seed = TRUE)
    })
    # Le bouton reste grise pendant tout le calcul, meme apres un flush reactif.
    bslib::bind_task_button(task, "run")

    # Reference choisie : l'entree si elle est encore un type renseigne, sinon le
    # plus frequent (le meme que le coeur prendrait).
    reference_r <- shiny::reactive({
      tp <- types_r()
      if (length(tp) < 1L) return(NULL)
      choix <- input$reference
      if (!is.null(choix) && choix %in% names(tp)) choix
      else effet_gestion_reference_defaut(ugs_r())
    })

    # Changer de reference : le tableau calcule sous l'autre reference s'efface.
    shiny::observeEvent(input$reference, {
      if (!identical(input$reference, rv$ref_calc)) rv$resultat <- NULL
    }, ignoreInit = TRUE)

    # Un autre projet, ou d'autres types declares : le resultat n'est plus le bon.
    shiny::observeEvent(list(app_state$project_id, signature_r()), {
      if (!identical(rv$signature, signature_r()) || !identical(rv$pid, app_state$project_id)) {
        rv$resultat <- NULL
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run, {
      i18n <- i18n_r()
      if (!effet_gestion_calculable(ugs_r())) {
        # Moins de deux types : message de saisie, aucun appel au coeur.
        shiny::showNotification(i18n$t("eg_need_types"), type = "warning", duration = 8)
        return()
      }
      project <- shiny::isolate(app_state$current_project)
      units <- effet_gestion_unites(project_family_scores(project), project$ugs)
      if (is.null(units)) {
        shiny::showNotification(i18n$t("eg_no_data"), type = "warning", duration = 8)
        return()
      }
      rv$pid <- shiny::isolate(app_state$project_id %||% project$id)
      rv$signature <- shiny::isolate(signature_r())
      reference <- shiny::isolate(reference_r())
      rv$ref_calc <- reference
      rv$resultat <- NULL
      shiny::showNotification(i18n$t("eg_running"), id = session$ns("notif"),
                              type = "message", duration = NULL)
      task$invoke(units, reference, .dev_pkg_path, get_app_options())
    })

    shiny::observeEvent(task$status(), {
      st <- task$status()
      if (!st %in% c("success", "error")) return()
      i18n <- i18n_r()
      shiny::removeNotification(session$ns("notif"))
      if (!.est_projet_courant(app_state, rv$pid)) return()
      res <- if (identical(st, "success")) {
        tryCatch(task$result(), error = function(e) list(erreur = conditionMessage(e)))
      } else {
        list(erreur = tryCatch({ task$result(); "" }, error = function(e) conditionMessage(e)))
      }
      rv$resultat <- res
      if (is.null(res$erreur)) {
        shiny::showNotification(i18n$t("eg_done"), type = "message", duration = 5)
      } else {
        shiny::showNotification(sprintf(i18n$t("eg_error_fmt"), .eg_erreur_lisible(res$erreur, i18n)),
                                type = "error", duration = 10)
      }
    })

    output$intro <- shiny::renderUI({
      i18n <- i18n_r()
      tp <- types_r()
      htmltools::tagList(
        htmltools::p(class = "text-muted", i18n$t("eg_intro")),
        if (length(tp) < 2L) {
          htmltools::div(class = "alert alert-info", shiny::icon("circle-info"), " ",
                         i18n$t("eg_need_types"))
        },
        if (length(tp) > 0L) {
          htmltools::p(class = "small text-muted", sprintf(
            i18n$t("eg_types_fmt"),
            paste(sprintf("%s (%d)", effet_gestion_libelle_type(names(tp), i18n), tp),
                  collapse = ", ")))
        }
      )
    })

    output$reference <- shiny::renderUI({
      i18n <- i18n_r()
      tp <- types_r()
      if (length(tp) < 2L) return(NULL)
      choix <- stats::setNames(
        names(tp), sprintf("%s (%d)", effet_gestion_libelle_type(names(tp), i18n), tp))
      htmltools::div(
        class = "d-flex align-items-center gap-2 flex-wrap mb-2",
        htmltools::tags$label(`for` = session$ns("reference"), class = "mb-0 fw-semibold",
                              i18n$t("eg_ref_label")),
        shiny::selectInput(session$ns("reference"), label = NULL, choices = choix,
                           selected = shiny::isolate(reference_r()), width = "auto"))
    })

    # UGF sans type declare : laissees hors du contraste, et dit avant le calcul.
    output$exclues <- shiny::renderUI({
      n <- effet_gestion_ugf_exclues(ugs_r())
      if (n < 1L) return(NULL)
      i18n <- i18n_r()
      htmltools::p(class = "small text-muted", shiny::icon("circle-info"), " ",
                   if (n == 1L) i18n$t("eg_exclues_un") else sprintf(i18n$t("eg_exclues_fmt"), n))
    })

    # Le tableau n'existe que si au moins deux types sont renseignes, que le
    # resultat correspond aux types declares maintenant ET a la reference choisie.
    resultat_valide <- shiny::reactive({
      if (!effet_gestion_calculable(ugs_r())) return(NULL)
      res <- rv$resultat
      if (is.null(res) || !identical(rv$signature, signature_r())) return(NULL)
      if (!identical(rv$ref_calc, reference_r())) return(NULL)
      res
    })

    output$etat <- shiny::renderUI({
      i18n <- i18n_r()
      if (!is.null(rv$resultat) && !is.null(rv$signature) &&
          !identical(rv$signature, signature_r()) &&
          effet_gestion_calculable(ugs_r())) {
        return(htmltools::div(class = "alert alert-warning", i18n$t("eg_perime")))
      }
      res <- resultat_valide()
      if (is.null(res)) return(NULL)
      if (!is.null(res$erreur)) {
        return(htmltools::div(class = "alert alert-warning",
                              .eg_erreur_lisible(res$erreur, i18n)))
      }
      htmltools::p(class = "small", sprintf(
        i18n$t("eg_ref_fmt"), effet_gestion_libelle_type(res$reference, i18n)))
    })

    output$tableau <- shiny::renderTable({
      res <- resultat_valide()
      if (is.null(res) || !is.null(res$erreur)) return(NULL)
      effet_gestion_tableau(res$resume, i18n_r())
    }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

    output$mention <- shiny::renderUI({
      res <- resultat_valide()
      if (is.null(res) || !is.null(res$erreur)) return(NULL)
      htmltools::p(class = "small fst-italic text-muted", i18n_r()$t("eg_mention"))
    })

    list(status = shiny::reactive(task$status()),
         resultat = shiny::reactive(rv$resultat))
  })
}

#' Message d'erreur du calcul, traduit quand c'est un code du service
#' @noRd
.eg_erreur_lisible <- function(erreur, i18n) {
  switch(as.character(erreur),
    sans_covariable = i18n$t("eg_no_covar"),
    sans_unites = i18n$t("eg_no_data"),
    strip_ansi(as.character(erreur)))
}

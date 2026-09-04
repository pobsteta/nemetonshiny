#' Chained-run module ("Tout calculer")
#'
#' @description
#' One button in the Selection tab that runs every engine of the app one after
#' another, then the AI generations, and reports what happened.
#'
#' This module owns the *ordering* and the *reporting*. It never runs an
#' engine itself: it posts a request on `app_state$pipeline_request` and the
#' owning module answers on `app_state$pipeline_answer` (see the protocol
#' section of `service_pipeline.R`). That is what lets each engine keep its own
#' guards, its own tab options and its own progress feedback.
#'
#' @name mod_pipeline
#' @keywords internal
NULL


#' Chained-run UI (button + progress panel)
#'
#' @param id Module namespace ID.
#' @param actions_ui Optional UI rendered INSIDE the collapsible body, above the
#'   chain button. `mod_home` y place ses actions de projet (« Voir les
#'   resultats », « Reessayer », « Lancer le calcul ») : elles flottaient
#'   au-dessus du bloc alors qu'elles relevent de la meme famille de geste.
#'   Le bloc porte le chrome (entete, repli, chevron), l'appelant garde SON
#'   namespace - d'ou le passage par argument plutot qu'un `uiOutput` cable ici.
#' @return Shiny UI.
#' @noRd
mod_pipeline_ui <- function(id, actions_ui = NULL) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language)

  # Section retractable, comme les autres blocs de cette sidebar (projets
  # recents, recherche...) : meme entete cliquable `data-bs-toggle="collapse"`
  # et meme chevron. Le panneau de progression peut lister dix-sept etapes -
  # sans repli, il pousse tout le reste de la sidebar hors de l'ecran une fois
  # la chaine lancee.
  htmltools::tags$div(
    id = ns("pipeline_section"),
    class = "card mb-3",
    htmltools::tags$div(
      class = "card-header bg-secondary text-white py-2",
      style = "cursor: pointer;",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = paste0("#", ns("pipeline_collapse")),
      `aria-expanded` = "true",
      `aria-controls` = ns("pipeline_collapse"),
      htmltools::div(
        class = "d-flex align-items-center justify-content-between",
        htmltools::div(
          class = "d-flex align-items-center",
          bsicons::bs_icon("play-circle", class = "me-2"),
          i18n$t("pipeline_section_title")
        ),
        bsicons::bs_icon("chevron-down", class = "collapse-icon")
      )
    ),
    htmltools::tags$div(
      id = ns("pipeline_collapse"),
      class = "collapse show",
      htmltools::tags$div(
        class = "card-body p-2",
        # Actions de l'appelant en tete : elles dependent de l'etat du projet
        # (brouillon / calcule) et sont donc les plus proches de ce que
        # l'utilisateur vient de faire. Le lancement de la chaine, lui, est
        # toujours la - il ferme la liste.
        if (!is.null(actions_ui)) htmltools::div(class = "mb-2", actions_ui),
        htmltools::div(
          class = "d-grid",
          # v0.143.17 - BLANC A BORDURE VERTE, plus vert plein.
          #
          # Depuis que les actions de projet ont rejoint ce bloc, deux boutons
          # verts s'y touchaient : « Voir les resultats » (`btn-success`) et
          # celui-ci (`btn-primary`) - meme vert `#1B6B1B`, les deux classes
          # ayant ete fusionnees. La regle normative dit une seule action
          # principale par vue.
          #
          # C'est celui-ci qui cede, et pas l'autre : a l'etat `completed`,
          # consulter les resultats est le geste ATTENDU, relancer toute la
          # chaine est l'exception. `outline-primary` garde l'intention
          # positive dans la bordure et rend l'emphase pleine a l'action que
          # l'utilisateur veut vraiment a ce moment-la.
          #
          # Toujours PAS d'ambre : l'ambre signale une PROVENANCE (contenu
          # genere), pas un niveau d'action - et ce bouton lance surtout des
          # calculs, dont l'IA n'est que la derniere etape.
          shiny::actionButton(
            ns("open"),
            label = i18n$t("pipeline_button"),
            class = "btn-outline-primary w-100",
            icon = bsicons::bs_icon("play-circle")
          )
        ),
        shiny::uiOutput(ns("panel"))
      )
    )
  )
}


#' Chained-run server
#'
#' @param id Module namespace ID.
#' @param app_state Shared `reactiveValues`.
#' @return Invisible NULL.
#' @noRd
mod_pipeline_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    i18n_r <- shiny::reactive(get_i18n(app_state$language %||% "fr"))

    rv <- shiny::reactiveValues(
      state = NULL,      # etat du run (cf. service_pipeline.R)
      last_answer = NULL # horodatage de la derniere reponse traitee
    )

    # ------------------------------------------------------------------
    # Modale de lancement
    # ------------------------------------------------------------------
    shiny::observeEvent(input$open, {
      i18n <- i18n_r()
      # Le bouton est grise pendant la chaine, donc ce chemin n'est pas
      # atteignable a la souris. La garde couvre le reste : un clic reste en
      # vol au moment ou la chaine demarre, un `updateActionButton` perdu,
      # un test qui pousse l'input directement. Sans elle, un second
      # `pipeline_new_run()` ecraserait le run en cours et ses reponses
      # seraient rejetees sur le `run_id` - une chaine orpheline.
      if (!is.null(rv$state) && !pipeline_is_done(rv$state)) {
        shiny::showNotification(i18n$t("pipeline_running_toast"),
                                type = "message", duration = 5)
        return()
      }
      if (is.null(app_state$current_project)) {
        shiny::showNotification(i18n$t("pipeline_no_project"),
                                type = "warning", duration = 5)
        return()
      }

      choix <- stats::setNames(
        pipeline_all_step_ids(),
        vapply(PIPELINE_STEPS, function(s) i18n$t(s$label), character(1))
      )

      shiny::showModal(shiny::modalDialog(
        title = htmltools::div(
          bsicons::bs_icon("play-circle", class = "me-2"),
          i18n$t("pipeline_title")
        ),
        size = "l",
        htmltools::p(class = "text-muted small", i18n$t("pipeline_help")),
        htmltools::div(
          class = "alert alert-warning py-2 small",
          bsicons::bs_icon("hourglass-split", class = "me-1"),
          i18n$t("pipeline_duration_warning")
        ),
        shiny::checkboxGroupInput(
          ns("scope"),
          label = i18n$t("pipeline_scope"),
          choices = choix,
          selected = unname(choix)
        ),
        shiny::selectInput(
          ns("profil"),
          label = i18n$t("pipeline_profil"),
          choices = get_expert_choices(i18n$language),
          selected = "generalist"
        ),
        footer = htmltools::tagList(
          shiny::modalButton(i18n$t("cancel")),
          shiny::actionButton(ns("start"), i18n$t("pipeline_run"),
                              class = "btn-primary",
                              icon = bsicons::bs_icon("play-fill"))
        )
      ))
    })

    # ------------------------------------------------------------------
    # Demarrage
    # ------------------------------------------------------------------
    shiny::observeEvent(input$start, {
      i18n <- i18n_r()
      etapes <- input$scope %||% character(0)
      if (length(etapes) == 0L) {
        shiny::showNotification(i18n$t("pipeline_no_step"),
                                type = "warning", duration = 5)
        return()
      }
      shiny::removeModal()
      rv$state <- pipeline_new_run(etapes, profil = input$profil)
      # Retour immediat (regle stricte #9) : bouton grise + toast, le temps de
      # l'operation. Les deux sont pilotes par l'ETAT DU RUN et non par le clic
      # sur "Tout calculer" : ce clic-la n'ouvre que la modale, et l'annuler
      # laisserait un bouton mort si on grisait des l'ouverture. Le toast est
      # persistant (`duration = NULL`) - `.cloturer()` le retire.
      shiny::updateActionButton(session, "open", disabled = TRUE)
      shiny::showNotification(
        i18n$t("pipeline_running_toast"),
        type = "message", duration = NULL, id = ns("running_notif")
      )
      cli::cli_alert_info(
        "Pipeline {rv$state$run_id}: {length(etapes)} etape{?s}, profil {input$profil %||% '?'}")
      .emettre()
    })

    # Poste la requete de l'etape courante, ou cloture le run.
    .emettre <- function() {
      etat <- rv$state
      cur <- pipeline_current_step(etat)
      if (is.null(cur)) {
        .cloturer()
        return(invisible(NULL))
      }
      rv$state <- pipeline_mark_running(etat)
      app_state$pipeline_request <- list(
        run_id  = etat$run_id,
        step_id = cur,
        profil  = etat$profil,
        ts      = Sys.time()
      )
      invisible(NULL)
    }

    # ------------------------------------------------------------------
    # Reponses des modules
    # ------------------------------------------------------------------
    shiny::observeEvent(app_state$pipeline_answer, {
      rep <- app_state$pipeline_answer
      etat <- rv$state
      if (is.null(rep) || is.null(etat)) return()
      # Une reponse d'un run precedent (moteur qui finit apres une annulation
      # puis un relancement) ferait avancer le run COURANT d'une etape.
      if (!identical(rep$run_id, etat$run_id)) return()
      if (identical(rv$last_answer, rep$ts)) return()
      rv$last_answer <- rep$ts

      cli::cli_alert_info(
        "Pipeline {etat$run_id}: {rep$step_id} -> {rep$status}")
      rv$state <- pipeline_record(etat, rep$step_id, rep$status, rep$message)
      .emettre()
    })

    # ------------------------------------------------------------------
    # Arret manuel
    # ------------------------------------------------------------------
    shiny::observeEvent(input$cancel, {
      i18n <- i18n_r()
      if (is.null(rv$state)) return()
      rv$state <- pipeline_cancel(rv$state)
      app_state$pipeline_request <- NULL
      shiny::showNotification(i18n$t("pipeline_cancelled_notice"),
                              type = "warning", duration = 6)
      .cloturer()
    })

    .cloturer <- function() {
      etat <- rv$state
      if (is.null(etat)) return(invisible(NULL))
      i18n <- i18n_r()
      # Passage OBLIGE des deux sorties - fin naturelle (`.emettre()` sans
      # etape suivante) et arret manuel (`input$cancel`). Rendre le bouton
      # ailleurs laisserait un des deux chemins avec un bouton mort.
      shiny::removeNotification(ns("running_notif"))
      shiny::updateActionButton(session, "open", disabled = FALSE)
      compte <- pipeline_tally(etat)
      duree <- if (!is.null(etat$ended)) {
        format_elapsed(as.numeric(difftime(etat$ended, etat$started, units = "secs")))
      } else "?"
      shiny::showNotification(
        htmltools::tagList(
          bsicons::bs_icon("check2-circle", class = "me-2"),
          sprintf("%s — %s", i18n$t("pipeline_done"),
                  sprintf(i18n$t("pipeline_report_summary_fmt"),
                          compte[["ok"]], compte[["error"]],
                          compte[["skipped"]], duree))
        ),
        type = "message", duration = 12, id = ns("done_notif")
      )
      invisible(NULL)
    }

    # ------------------------------------------------------------------
    # Panneau de progression / rapport
    # ------------------------------------------------------------------
    output$panel <- shiny::renderUI({
      etat <- rv$state
      if (is.null(etat)) return(NULL)
      i18n <- i18n_r()
      rep <- pipeline_report(etat)
      en_cours <- !pipeline_is_done(etat)

      lignes <- lapply(seq_len(nrow(rep)), function(i) {
        st <- rep$status[i]
        htmltools::div(
          class = "d-flex justify-content-between align-items-start smaller py-1",
          htmltools::div(
            htmltools::span(class = "me-1", .pipeline_icone(st)),
            i18n$t(rep$label[i]),
            if (!is.na(rep$message[i])) {
              htmltools::div(class = "text-muted fst-italic", rep$message[i])
            }
          ),
          htmltools::span(
            class = paste("badge", .pipeline_badge(st)),
            if (!is.na(rep$seconds[i]) && st %in% c("ok", "error")) {
              format_elapsed(rep$seconds[i])
            } else {
              i18n$t(paste0("pipeline_status_", st))
            }
          )
        )
      })

      bslib::card(
        class = "mt-2",
        bslib::card_header(
          class = "py-2 d-flex justify-content-between align-items-center",
          htmltools::span(
            class = "small fw-semibold",
            if (en_cours) {
              sprintf(i18n$t("pipeline_running_fmt"),
                      etat$index, length(etat$steps),
                      i18n$t(pipeline_step_def(pipeline_current_step(etat))$label))
            } else {
              i18n$t("pipeline_report_title")
            }
          ),
          if (en_cours) {
            shiny::actionButton(ns("cancel"), i18n$t("pipeline_cancel"),
                                class = "btn-outline-danger btn-sm")
          }
        ),
        bslib::card_body(class = "py-2", lignes)
      )
    })

    invisible(NULL)
  })
}


#' Icon for a step status
#' @noRd
.pipeline_icone <- function(status) {
  switch(
    status,
    ok        = bsicons::bs_icon("check-circle-fill", class = "text-success"),
    error     = bsicons::bs_icon("x-circle-fill", class = "text-danger"),
    skipped   = bsicons::bs_icon("skip-forward-fill", class = "text-muted"),
    cancelled = bsicons::bs_icon("dash-circle", class = "text-muted"),
    running   = bsicons::bs_icon("arrow-repeat", class = "text-primary"),
    bsicons::bs_icon("circle", class = "text-muted")
  )
}

#' Badge class for a step status
#' @noRd
.pipeline_badge <- function(status) {
  switch(
    status,
    ok        = "bg-success",
    error     = "bg-danger",
    skipped   = "bg-secondary",
    cancelled = "bg-secondary",
    running   = "bg-primary",
    "bg-light text-dark"
  )
}

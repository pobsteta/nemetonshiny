# Module "ce que les modeles voient" de la Synthese (mission 078, spec 066 du
# coeur, ecart n° 25).
#
# Un bouton lance, a la demande, `nemeton::probe_indicator()` (via
# `R/service_sondes.R`) sur les indicateurs deja calcules du projet et
# l'embedding AlphaEarth deja en cache ; la table triee par R2 dit quels
# indicateurs un modele de fondation retrouve et lesquels il ne voit pas.
# Aucune logique metier : R2, lecture, statut et `source_commune` viennent du
# coeur. Calcul long : `ExtendedTask`, retour immediat, bouton lie a la tache.
# Une sonde est un diagnostic, pas un estimateur (spec 066, D-3).

#' Ce que les modeles voient - UI
#' @param id Identifiant du module.
#' @noRd
mod_sondes_embedding_ui <- function(id) {
  ns <- shiny::NS(id)
  i18n <- get_i18n(get_app_options()$language)
  bslib::card(
    id = ns("carte"),
    bslib::card_header(i18n$t("sd_title")),
    bslib::card_body(
      shiny::uiOutput(ns("intro")),
      htmltools::div(
        class = "mb-3",
        bslib::input_task_button(
          ns("run"), i18n$t("sd_run"),
          icon = shiny::icon("magnifying-glass-chart"),
          label_busy = i18n$t("sd_busy"),
          type = "outline-primary"
        )
      ),
      shiny::uiOutput(ns("etat")),
      shiny::tableOutput(ns("tableau")),
      shiny::uiOutput(ns("mention"))
    )
  )
}

#' Ce que les modeles voient - serveur
#'
#' @param id Identifiant du module.
#' @param app_state reactiveValues de l'application.
#' @return Une liste de reactives : `status` (etat de la tache), `resultat`
#'   (dernier resultat du service, ou `NULL`).
#' @noRd
mod_sondes_embedding_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(resultat = NULL, pid = NULL, signature = NULL)

    i18n_r <- shiny::reactive(get_i18n(app_state$language %||% "fr"))
    indicators_r <- shiny::reactive(app_state$current_project$indicators_sf)
    chemin_r <- shiny::reactive(app_state$current_project$path)
    unites_r <- shiny::reactive(sondes_unites(indicators_r()))
    embedding_r <- shiny::reactive(sondes_embedding(indicators_r(), chemin_r()))
    signature_r <- shiny::reactive(sondes_signature(unites_r(), embedding_r()))

    # Tache longue : worker `future` (jamais dans la boucle Shiny).
    .dev_pkg_path <- tryCatch(
      if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
        find.package("nemetonshiny") else NULL,
      error = function(e) NULL)
    task <- shiny::ExtendedTask$new(function(units, fichier, annee, dev_path, app_opts) {
      # Tests : execution dans le processus (le coeur est simule), toujours
      # derriere une promesse - meme sequence d'etats que le worker.
      if (isTRUE(getOption("nemetonshiny.sondes_inline"))) {
        return(promises::promise(function(resolve, reject) {
          .later_sur(function() {
            tryCatch(resolve(sondes_calculer(units, fichier, annee)),
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
        utils::getFromNamespace("sondes_calculer", "nemetonshiny")(units, fichier, annee)
      }, seed = TRUE)
    })
    bslib::bind_task_button(task, "run")

    # Un autre projet : le resultat n'est plus le bon. Des indicateurs ou un
    # embedding changes dans le meme projet : la table disparait (signature) et
    # l'ecran dit de relancer.
    shiny::observeEvent(app_state$project_id, {
      if (!identical(rv$pid, app_state$project_id)) {
        rv$resultat <- NULL
        rv$signature <- NULL
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run, {
      i18n <- i18n_r()
      units <- shiny::isolate(unites_r())
      if (is.null(units)) {
        shiny::showNotification(i18n$t("sd_no_data"), type = "warning", duration = 8)
        return()
      }
      emb <- shiny::isolate(embedding_r())
      if (is.null(emb)) {
        # Sans embedding : message, aucun appel au coeur.
        shiny::showNotification(i18n$t("sd_no_embedding"), type = "warning", duration = 8)
        return()
      }
      project <- shiny::isolate(app_state$current_project)
      rv$pid <- shiny::isolate(app_state$project_id %||% project$id)
      rv$signature <- shiny::isolate(signature_r())
      rv$resultat <- NULL
      # Deja calcule sur les memes donnees : on relit le cache du projet.
      en_cache <- sondes_cache_lire(project$path, rv$signature)
      if (!is.null(en_cache)) {
        rv$resultat <- en_cache
        shiny::showNotification(i18n$t("sd_done"), type = "message", duration = 5)
        return()
      }
      shiny::showNotification(i18n$t("sd_running"), id = session$ns("notif"),
                              type = "message", duration = NULL)
      task$invoke(units, emb$fichier, emb$annee, .dev_pkg_path, get_app_options())
    })

    shiny::observeEvent(task$status(), {
      st <- task$status()
      if (!st %in% c("success", "error")) return()
      i18n <- i18n_r()
      shiny::removeNotification(session$ns("notif"))
      if (!.est_projet_courant(app_state, rv$pid)) return()
      res <- if (identical(st, "success")) {
        tryCatch(task$result(), error = function(e) list(table = NULL, erreur = conditionMessage(e)))
      } else {
        list(table = NULL,
             erreur = tryCatch({ task$result(); "" }, error = function(e) conditionMessage(e)))
      }
      rv$resultat <- res
      if (is.null(res$erreur)) {
        sondes_cache_ecrire(shiny::isolate(chemin_r()), rv$signature, res)
        shiny::showNotification(i18n$t("sd_done"), type = "message", duration = 5)
      } else {
        shiny::showNotification(sprintf(i18n$t("sd_error_fmt"), .sd_erreur_lisible(res$erreur, i18n)),
                                type = "error", duration = 10)
      }
    })

    output$intro <- shiny::renderUI({
      i18n <- i18n_r()
      emb <- embedding_r()
      htmltools::tagList(
        htmltools::p(class = "text-muted", i18n$t("sd_intro")),
        if (is.null(emb)) {
          htmltools::div(class = "alert alert-info", shiny::icon("circle-info"), " ",
                         i18n$t("sd_no_embedding"))
        } else if (!is.na(emb$annee)) {
          htmltools::p(class = "small text-muted", sprintf(i18n$t("sd_source_fmt"), emb$annee))
        }
      )
    })

    # Le tableau n'existe que si le resultat correspond aux donnees du moment.
    resultat_valide <- shiny::reactive({
      res <- rv$resultat
      if (is.null(res) || !identical(rv$signature, signature_r())) return(NULL)
      res
    })

    output$etat <- shiny::renderUI({
      i18n <- i18n_r()
      if (!is.null(rv$resultat) && !is.null(rv$signature) &&
          !identical(rv$signature, signature_r())) {
        return(htmltools::div(class = "alert alert-warning", i18n$t("sd_perime")))
      }
      res <- resultat_valide()
      if (is.null(res)) return(NULL)
      if (!is.null(res$erreur)) {
        return(htmltools::div(class = "alert alert-warning",
                              .sd_erreur_lisible(res$erreur, i18n)))
      }
      NULL
    })

    output$tableau <- shiny::renderTable({
      res <- resultat_valide()
      if (is.null(res) || !is.null(res$erreur)) return(NULL)
      sondes_tableau(res$table, i18n_r())
    }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")

    output$mention <- shiny::renderUI({
      res <- resultat_valide()
      if (is.null(res) || !is.null(res$erreur)) return(NULL)
      i18n <- i18n_r()
      htmltools::tagList(
        htmltools::p(class = "small text-muted", i18n$t("sd_seuils")),
        htmltools::p(class = "small fst-italic text-muted", i18n$t("sd_mention"))
      )
    })

    list(status = shiny::reactive(task$status()),
         resultat = shiny::reactive(rv$resultat))
  })
}

#' Message d'erreur du calcul, traduit quand c'est un code du service
#' @noRd
.sd_erreur_lisible <- function(erreur, i18n) {
  switch(as.character(erreur),
    sans_unites = i18n$t("sd_no_data"),
    sans_embedding = i18n$t("sd_no_embedding"),
    reponse_inattendue = i18n$t("sd_reponse_inattendue"),
    strip_ansi(as.character(erreur)))
}

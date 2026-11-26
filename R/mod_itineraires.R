# Module Itineraires du plan d'actions (mission 066, specs 065 du coeur).
# Presentation seule : le calcul est dans R/service_itineraires.R, le jugement
# dans nemeton (itineraires_admissibles / comparer_itineraires / profils_poids).

#' Itineraires module UI
#'
#' @param id Character. Module namespace ID.
#' @return Shiny UI elements.
#' @noRd
mod_itineraires_ui <- function(id) {
  ns <- shiny::NS(id)
  lang <- tryCatch(get_app_options()$language, error = function(e) "fr") %||% "fr"
  i18n <- get_i18n(lang)
  choix <- .itin_profils_choix(lang)
  defaut <- if ("profil_gestionnaire_expert" %in% choix) {
    "profil_gestionnaire_expert"
  } else if (length(choix)) unname(choix[1]) else NULL
  htmltools::tagList(
    htmltools::div(
      class = "d-flex align-items-center gap-2 flex-wrap mb-2",
      htmltools::tags$label(`for` = ns("profil"), class = "mb-0 fw-semibold",
                            i18n$t("itin_profil_label")),
      shiny::selectInput(ns("profil"), label = NULL, choices = choix,
                         selected = defaut, width = "auto")
    ),
    shiny::uiOutput(ns("contenu"))
  )
}


# Une table HTML simple (pas de DT : le contenu est court et testable).
# `infobulles` : liste nommee colonne -> vecteur de textes, poses en `title`
# (infobulle) sur les cellules de cette colonne quand le texte n'est pas vide.
.itin_html_table <- function(df, colonnes, entetes, classes_ligne = NULL,
                             infobulles = list()) {
  tete <- htmltools::tags$tr(lapply(entetes, htmltools::tags$th))
  lignes <- lapply(seq_len(nrow(df)), function(i) {
    htmltools::tags$tr(
      class = if (!is.null(classes_ligne)) classes_ligne[i],
      lapply(colonnes, function(cn) {
        tip <- infobulles[[cn]][i]
        htmltools::tags$td(
          title = if (!is.null(tip) && !is.na(tip) && nzchar(tip)) tip,
          as.character(df[[cn]][i]))
      }))
  })
  htmltools::div(
    class = "table-responsive",
    htmltools::tags$table(class = "table table-sm table-striped align-middle",
                          htmltools::tags$thead(tete),
                          htmltools::tags$tbody(lignes)))
}


#' Contenu de l'onglet Itineraires
#'
#' Pur (testable hors session) : prend le resultat de [itineraires_calculer()].
#'
#' @param res Sortie de `itineraires_calculer()`.
#' @param i18n Traducteur.
#' @return Tag list.
#' @noRd
itineraires_rendre <- function(res, i18n) {
  info <- function(cle, classe = "alert alert-info small mb-2") {
    htmltools::div(class = classe, i18n$t(cle))
  }
  if (!identical(res$statut, "ok")) {
    if (identical(res$statut, "erreur")) {
      return(htmltools::div(class = "alert alert-warning small",
                            i18n$t("itin_erreur")))
    }
    cle <- switch(res$statut, sans_projet = "itin_aucun_projet",
                  sans_ugf = "itin_aucune_ugf", noyau_absent = "itin_noyau_absent",
                  "itin_aucune_ugf")
    return(info(cle))
  }
  cat_etat <- .itin_etat_catalogue(res$catalogue)
  avis <- list(
    # Les poids sont provisoires (decision de Pascal, 2026-11-25).
    info("itin_poids_provisoires", "alert alert-warning small mb-2"),
    if (cat_etat[["a_sourcer"]] > 0L) {
      htmltools::div(
        class = "alert alert-warning small mb-2",
        sprintf(i18n$t("itin_catalogue_sourcage"),
                cat_etat[["a_sourcer"]], cat_etat[["total"]]))
    },
    if (!isTRUE(res$desserte)) info("itin_desserte_absente", "alert alert-secondary small mb-2")
  )
  adm <- itineraires_table_admissibles(res$admissibles, res$catalogue, i18n)
  cls <- c(admissible = "", exclu = "text-muted", manquant = "table-warning")
  bloc_adm <- if (is.null(adm)) info("itin_aucun_comparable") else
    .itin_html_table(adm, c("ugf", "itineraire", "etat", "raison"),
                     c(i18n$t("itin_col_ugf"), i18n$t("itin_col_itineraire"),
                       i18n$t("itin_col_etat"), i18n$t("itin_col_raison")),
                     unname(cls[adm$statut]),
                     infobulles = list(raison = adm$detail))
  clas <- itineraires_table_classement(res$comparaison, res$catalogue, i18n)
  bloc_cmp <- if (is.null(clas)) info("itin_aucun_comparable") else
    .itin_html_table(clas, c("ugf", "rang", "itineraire", "evaluees", "gagnantes",
                             "perdantes", "pareto"),
                     c(i18n$t("itin_col_ugf"), i18n$t("itin_col_rang"),
                       i18n$t("itin_col_itineraire"), i18n$t("itin_col_evaluees"),
                       i18n$t("itin_col_gagnantes"), i18n$t("itin_col_perdantes"),
                       i18n$t("itin_col_pareto")))
  htmltools::tagList(
    avis,
    htmltools::tags$h6(class = "mt-2", i18n$t("itin_h_classement")),
    htmltools::p(class = "small text-muted", i18n$t("itin_rang_note")),
    bloc_cmp,
    htmltools::tags$h6(class = "mt-3", i18n$t("itin_h_admissibles")),
    bloc_adm
  )
}


#' Itineraires module server
#'
#' @param id Character. Module namespace ID.
#' @param app_state Shared `reactiveValues` (`current_project`, `language`).
#' @noRd
mod_itineraires_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    resultat <- shiny::reactive({
      itineraires_calculer(app_state$current_project,
                           input$profil %||% "profil_gestionnaire_expert")
    })
    output$contenu <- shiny::renderUI({
      i18n <- get_i18n(app_state$language %||% "fr")
      itineraires_rendre(resultat(), i18n)
    })
  })
}

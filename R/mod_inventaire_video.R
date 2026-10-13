# mod_inventaire_video.R - Terrain > Inventaire video (brief groma 2026-10-12)
#
# Trois blocs : Preparer (projet QField de l'inventaire video), Suivre (etat des
# placettes sur le serveur Groma) et Integrer (retour terrain + resultats
# definitifs dans `data/field_data.gpkg`). Aucune logique metier : tout passe
# par `R/service_inventaire_video.R`, qui appelle le coeur.

# Calculateur de declinaison magnetique (aucun modele geomagnetique embarque).
.VIDEO_URL_DECLINAISON <- "https://www.ngdc.noaa.gov/geomag/calculators/magcalc.shtml"

#' Video inventory sub-tab - UI
#'
#' @param id Module namespace ID.
#' @return Shiny UI.
#' @noRd
mod_inventaire_video_ui <- function(id) {
  ns <- shiny::NS(id)
  i18n <- get_i18n(get_app_options()$language %||% "fr")

  htmltools::tagList(shiny::uiOutput(ns("coeur_absent")), bslib::accordion(
    id = ns("blocs"),
    multiple = TRUE,
    bslib::accordion_panel(
      title = i18n$t("video_bloc_preparer"),
      value = "preparer",
      icon = bsicons::bs_icon("qr-code"),
      bslib::layout_columns(
        col_widths = c(5, 7),
        htmltools::div(
          shiny::numericInput(ns("rayon"), i18n$t("video_rayon"),
                              value = 20, min = 5, max = 50, step = 1),
          shiny::numericInput(ns("seuil"), i18n$t("video_seuil_diam"),
                              value = 7.5, min = 0, max = 100, step = 0.5),
          shiny::radioButtons(ns("unite"), i18n$t("video_unite_azimut"),
                              inline = TRUE,
                              choices = stats::setNames(
                                c("deg", "gr"),
                                c(i18n$t("video_degres"), i18n$t("video_grades")))),
          shiny::numericInput(ns("declinaison"), i18n$t("video_declinaison"),
                              value = NA, min = -30, max = 30, step = 0.1),
          htmltools::tags$a(href = .VIDEO_URL_DECLINAISON, target = "_blank",
                            rel = "noopener", class = "small d-block mb-3",
                            i18n$t("video_declinaison_aide")),
          shiny::checkboxInput(ns("lidar"), i18n$t("video_avec_lidar"), value = TRUE),
          shiny::uiOutput(ns("lidar_info")),
          shiny::actionButton(ns("generer"), i18n$t("video_generer"),
                              icon = bsicons::bs_icon("phone"),
                              class = "btn-primary w-100 mb-2"),
          shiny::uiOutput(ns("telechargement"))
        ),
        htmltools::div(
          shiny::uiOutput(ns("placettes_ui"))
        )
      )
    ),
    bslib::accordion_panel(
      title = i18n$t("video_bloc_suivre"),
      value = "suivre",
      icon = bsicons::bs_icon("cloud-arrow-up"),
      shiny::uiOutput(ns("serveur_info")),
      shiny::actionButton(ns("actualiser"), i18n$t("video_actualiser"),
                          icon = bsicons::bs_icon("arrow-clockwise"),
                          class = "btn-outline-secondary mb-2"),
      shiny::uiOutput(ns("etats"))
    ),
    bslib::accordion_panel(
      title = i18n$t("video_bloc_integrer"),
      value = "integrer",
      icon = bsicons::bs_icon("box-arrow-in-down"),
      bslib::layout_columns(
        col_widths = c(5, 7),
        htmltools::div(
          shiny::fileInput(ns("gpkg_terrain"), i18n$t("video_gpkg_terrain"),
                           accept = c(".gpkg", "application/geopackage+sqlite3",
                                      "application/octet-stream"),
                           buttonLabel = i18n$t("field_ingest_browse"),
                           placeholder = i18n$t("field_ingest_placeholder")),
          shiny::actionButton(ns("recuperer"), i18n$t("video_recuperer_resultats"),
                              icon = bsicons::bs_icon("cloud-download"),
                              class = "btn-outline-secondary w-100 mb-2"),
          shiny::fileInput(ns("gpkg_resultats"), i18n$t("video_resultats_serveur"),
                           multiple = TRUE,
                           accept = c(".gpkg", "application/geopackage+sqlite3",
                                      "application/octet-stream"),
                           buttonLabel = i18n$t("field_ingest_browse"),
                           placeholder = i18n$t("field_ingest_placeholder")),
          shiny::uiOutput(ns("resultats_info")),
          shiny::actionButton(ns("integrer"), i18n$t("video_integrer"),
                              icon = bsicons::bs_icon("check2-circle"),
                              class = "btn-primary w-100")
        ),
        htmltools::div(
          shiny::uiOutput(ns("bilan")),
          shiny::uiOutput(ns("renvoi")))
      )
    )
  ))
}

#' Video inventory sub-tab - server
#'
#' @param id Module namespace ID.
#' @param app_state Shared reactiveValues (`current_project`, `language`).
#' @return A reactiveValues (`qgz`, `etats`, `resultats`, `integration`,
#'   `import`, `qgz_retour`), for tests.
#' @noRd
mod_inventaire_video_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    i18n <- shiny::reactive(get_i18n(app_state$language %||% "fr"))
    rv <- shiny::reactiveValues(qgz = NULL, etats = NULL, resultats = NULL,
                                integration = NULL, import = NULL,
                                qgz_retour = NULL)

    project_path <- shiny::reactive({
      p <- app_state$current_project
      if (is.null(p) || is.null(p$id)) return(NULL)
      tryCatch(get_project_path(p$id), error = function(e) NULL)
    })

    # Changement de projet : rien de l'ancien ne doit survivre (un .qgz ou une
    # integration d'un projet servis sous le nom d'un autre).
    # Seulement sur un VRAI changement : `load_project()` apres l'integration
    # reaffecte `current_project` sans changer de projet, et effacait le bilan.
    projet_vu <- NULL
    shiny::observeEvent(project_path(), {
      if (identical(project_path(), projet_vu)) return()
      projet_vu <<- project_path()
      rv$qgz <- NULL; rv$etats <- NULL; rv$resultats <- NULL
      rv$integration <- NULL; rv$import <- NULL; rv$qgz_retour <- NULL
    }, ignoreNULL = FALSE)

    plan <- shiny::reactive(video_placettes_plan(project_path()))
    chm <- shiny::reactive({
      p <- app_state$current_project
      if (is.null(p) || is.null(p$id)) return(NULL)
      tryCatch(.project_chm(p$id), error = function(e) NULL)
    })
    serveur <- shiny::reactive(video_config_serveur(app_state$current_project$metadata))

    output$coeur_absent <- shiny::renderUI({
      if (video_coeur_disponible()) return(NULL)
      htmltools::div(class = "alert alert-warning small m-2",
                     i18n()$t("video_coeur_absent"))
    })

    # --- Preparer ------------------------------------------------------
    output$placettes_ui <- shiny::renderUI({
      tr <- i18n()
      p <- plan()
      if (is.null(p)) {
        return(htmltools::div(class = "alert alert-info small",
                              tr$t("video_sans_plan")))
      }
      ids <- as.character(p$plot_id)
      shiny::checkboxGroupInput(ns("placettes"), tr$t("video_placettes"),
                                choices = ids, selected = ids, inline = TRUE)
    })

    shiny::observeEvent(chm(), {
      ok <- !is.null(chm())
      if (!ok) shiny::updateCheckboxInput(session, "lidar", value = FALSE)
      session$sendCustomMessage("nemetonSetDisabled",
                                list(id = ns("lidar"), disabled = !ok))
    }, ignoreNULL = FALSE)

    output$lidar_info <- shiny::renderUI({
      if (!is.null(chm())) return(NULL)
      htmltools::p(class = "text-muted small fst-italic",
                   i18n()$t("video_lidar_absent"))
    })

    dev_path <- tryCatch(
      if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
        find.package("nemetonshiny") else NULL,
      error = function(e) NULL)

    tache <- shiny::ExtendedTask$new(function(args, dev, app_opts) {
      if (requireNamespace("future", quietly = TRUE)) {
        plan_classes <- class(future::plan())
        if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
          .ensure_async_plan()
        }
      }
      promises::future_promise({
        on.exit(utils::getFromNamespace(".release_worker_memory", "nemetonshiny")(), add = TRUE)
        if (!is.null(dev) && requireNamespace("pkgload", quietly = TRUE)) {
          pkgload::load_all(dev, quiet = TRUE)
        } else {
          loadNamespace("nemetonshiny")
        }
        options(nemeton.app_options = app_opts)
        do.call(asNamespace("nemetonshiny")$video_creer_projet, args)
      }, seed = TRUE)
    })

    # Arguments of `creer_projet_inventaire_video()` from the inputs
    # (NULL with a notification when something is missing).
    .args_generation <- function() {
      pp <- project_path()
      # Le MNT est resolu par le coeur depuis `project_path`.
      res <- video_args_generation(
        project_path = pp, placettes = plan(), plot_ids = input$placettes,
        rayon_m = input$rayon, seuil_diam_cm = input$seuil,
        unite_azimut = input$unite, declinaison_deg = input$declinaison,
        avec_lidar = isTRUE(input$lidar), chm = chm(),
        lang = app_state$language %||% "fr")
      if (is.character(res)) {
        shiny::showNotification(i18n()$t(res), type = "warning")
        return(NULL)
      }
      res
    }

    shiny::observeEvent(input$generer, {
      if (identical(tache$status(), "running")) return()
      tr <- i18n()
      if (!video_coeur_disponible()) {
        shiny::showNotification(tr$t("video_coeur_absent"), type = "error")
        return()
      }
      args <- .args_generation()
      if (is.null(args)) return()
      rv$qgz <- NULL
      shiny::showNotification(
        htmltools::tagList(shiny::icon("spinner", class = "fa-spin me-2"),
                           tr$t("video_en_cours")),
        type = "message", duration = NULL, closeButton = FALSE,
        id = "video_en_cours", session = session)
      session$sendCustomMessage("nemetonSetDisabled",
                                list(id = ns("generer"), disabled = TRUE))
      video_lancer_generation(tache, args, dev_path)
    })

    shiny::observeEvent(tache$status(), {
      st <- tache$status()
      if (!st %in% c("success", "error")) return()
      tr <- i18n()
      shiny::removeNotification("video_en_cours", session = session)
      session$sendCustomMessage("nemetonSetDisabled",
                                list(id = ns("generer"), disabled = FALSE))
      qgz <- tryCatch(tache$result(), error = function(e) e)
      if (inherits(qgz, "error") || !is.character(qgz) || !file.exists(qgz[1])) {
        msg <- if (inherits(qgz, "error")) conditionMessage(qgz) else ""
        shiny::showNotification(paste(tr$t("error"), msg), type = "error",
                                duration = 15)
        return()
      }
      rv$qgz <- qgz[1]
      shiny::showNotification(tr$t("qfield_ready"), type = "message", duration = 6)
    })

    output$telechargement <- shiny::renderUI({
      if (is.null(rv$qgz)) return(NULL)
      shiny::downloadButton(ns("qgz"), i18n()$t("video_telecharger"),
                            class = "btn-outline-primary w-100")
    })

    output$qgz <- shiny::downloadHandler(
      filename = function() basename(rv$qgz %||% "inventaire_video.qgz"),
      content = function(file) file.copy(rv$qgz, file, overwrite = TRUE),
      contentType = "application/zip"
    )

    # --- Suivre --------------------------------------------------------
    output$serveur_info <- shiny::renderUI({
      cfg <- serveur()
      tr <- i18n()
      if (!nzchar(cfg$url) || !nzchar(cfg$jeton)) {
        return(htmltools::div(class = "alert alert-info small",
                              tr$t("video_serveur_absent")))
      }
      htmltools::p(class = "text-muted small",
                   paste0(tr$t("video_serveur"), " : ", cfg$url))
    })

    # Server call with an i18n message on failure (network, missing core).
    .appel_serveur <- function(expr) {
      tr <- i18n()
      cfg <- serveur()
      if (!nzchar(cfg$url) || !nzchar(cfg$jeton)) {
        shiny::showNotification(tr$t("video_serveur_absent"), type = "warning")
        return(NULL)
      }
      tryCatch(expr(cfg), error = function(e) {
        msg <- if (inherits(e, "video_coeur_absent")) tr$t("video_coeur_absent")
               else sprintf(tr$t("video_erreur_serveur"), conditionMessage(e))
        shiny::showNotification(msg, type = "error", duration = 10)
        NULL
      })
    }

    shiny::observeEvent(input$actualiser, {
      srv <- .appel_serveur(function(cfg) video_etat_serveur(cfg$url, cfg$jeton))
      if (is.null(srv)) return()
      rv$etats <- video_tableau_etats(srv, video_etat_local(project_path()))
    })

    output$etats <- shiny::renderUI({
      tr <- i18n()
      et <- rv$etats
      if (is.null(et) || !nrow(et)) return(NULL)
      lib_etat <- c(a_faire = "video_etat_a_faire", provisoire = "video_etat_provisoire",
                    envoye = "video_etat_envoye", definitif = "video_etat_definitif")
      lib_verdict <- c(conforme = "video_verdict_conforme",
                       a_verifier = "video_verdict_a_verifier")
      .lib <- function(v, table) {
        k <- table[as.character(v)]
        ifelse(is.na(k), ifelse(is.na(v), "", as.character(v)),
               vapply(k, function(x) if (is.na(x)) "" else tr$t(x), character(1)))
      }
      en_tetes <- c("video_col_placette", "video_col_etat", "video_col_date",
                    "video_col_verdict", "video_col_arbres", "video_col_alertes")
      htmltools::tags$table(
        class = "table table-sm table-hover",
        htmltools::tags$thead(htmltools::tags$tr(
          lapply(en_tetes, function(k) htmltools::tags$th(tr$t(k))))),
        htmltools::tags$tbody(lapply(seq_len(nrow(et)), function(i) {
          htmltools::tags$tr(
            style = "cursor: pointer;",
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
              ns("rapport_plot"), et$plot_id[i]),
            htmltools::tags$td(et$plot_id[i]),
            htmltools::tags$td(.lib(et$etat[i], lib_etat)),
            htmltools::tags$td(if (is.na(et$date[i])) "" else as.character(et$date[i])),
            htmltools::tags$td(.lib(et$verdict[i], lib_verdict)),
            htmltools::tags$td(if (is.na(et$n_arbres[i])) "" else as.character(et$n_arbres[i])),
            htmltools::tags$td(if (is.na(et$alertes[i])) "" else as.character(et$alertes[i])))
        })))
    })

    shiny::observeEvent(input$rapport_plot, {
      tr <- i18n()
      id <- input$rapport_plot
      fichiers <- .appel_serveur(function(cfg) video_telecharger(
        cfg$url, cfg$jeton, plot_ids = id,
        dossier = tempfile(paste0("groma_", id, "_"))))
      if (is.null(fichiers)) return()
      html <- fichiers[grepl("\\.html?$", fichiers, ignore.case = TRUE)]
      alertes <- rv$etats$alertes[match(id, rv$etats$plot_id)]
      shiny::showModal(shiny::modalDialog(
        title = paste(tr$t("video_rapport"), "\u2014", id),
        size = "xl", easyClose = TRUE,
        footer = shiny::modalButton(tr$t("close")),
        if (length(alertes) && !is.na(alertes) && nzchar(alertes))
          htmltools::div(class = "alert alert-warning small", alertes),
        if (length(html)) {
          htmltools::tags$iframe(
            srcdoc = paste(readLines(html[1], warn = FALSE, encoding = "UTF-8"),
                           collapse = "\n"),
            sandbox = "", style = "width: 100%; height: 70vh; border: 0;")
        } else {
          htmltools::p(class = "text-muted", tr$t("video_rapport_absent"))
        }
      ))
    })

    # --- Integrer ------------------------------------------------------
    shiny::observeEvent(input$recuperer, {
      tr <- i18n()
      et <- rv$etats
      if (is.null(et)) {
        srv <- .appel_serveur(function(cfg) video_etat_serveur(cfg$url, cfg$jeton))
        if (is.null(srv)) return()
        et <- rv$etats <- video_tableau_etats(srv, video_etat_local(project_path()))
      }
      ids <- et$plot_id[et$etat == "definitif"]
      if (!length(ids)) {
        shiny::showNotification(tr$t("video_aucun_definitif"), type = "warning")
        return()
      }
      f <- .appel_serveur(function(cfg) video_telecharger(
        cfg$url, cfg$jeton, plot_ids = ids, dossier = tempfile("groma_resultats_")))
      if (is.null(f)) return()
      rv$resultats <- f[grepl("\\.gpkg$", f, ignore.case = TRUE)]
    })

    shiny::observeEvent(input$gpkg_resultats, {
      up <- input$gpkg_resultats
      # Copies sous le nom d'origine : Shiny nomme les datapath 0.gpkg, 1.gpkg...
      d <- tempfile("groma_resultats_")
      dir.create(d)
      dest <- file.path(d, basename(up$name))
      file.copy(up$datapath, dest)
      rv$resultats <- dest[file.exists(dest)]
    })

    output$resultats_info <- shiny::renderUI({
      n <- length(rv$resultats)
      if (!n) return(NULL)
      htmltools::p(class = "small text-muted",
                   paste0(i18n()$t("video_resultats_serveur"), " : ",
                          paste(basename(rv$resultats), collapse = ", ")))
    })

    shiny::observeEvent(input$integrer, {
      if (deny_if_readonly(app_state)) return()
      tr <- i18n()
      project <- app_state$current_project
      pp <- project_path()
      if (is.null(project) || is.null(pp)) {
        shiny::showNotification(tr$t("field_ingest_no_project"), type = "warning")
        return()
      }
      up <- input$gpkg_terrain
      if (is.null(up) || !nrow(up)) {
        shiny::showNotification(tr$t("field_ingest_no_file"), type = "warning")
        return()
      }
      session$sendCustomMessage("nemetonSetDisabled",
                                list(id = ns("integrer"), disabled = TRUE))
      on.exit(session$sendCustomMessage("nemetonSetDisabled",
                                        list(id = ns("integrer"), disabled = FALSE)),
              add = TRUE)
      res <- tryCatch(video_integrer(up$datapath, rv$resultats, pp),
                      error = function(e) e)
      if (inherits(res, "error")) {
        msg <- if (inherits(res, "video_coeur_absent")) tr$t("video_coeur_absent")
               else paste(tr$t("error"), conditionMessage(res))
        shiny::showNotification(msg, type = "error", duration = 12)
        return()
      }
      bilan <- video_bilan(res)
      rv$integration <- bilan
      rv$import <- res
      rv$qgz_retour <- NULL
      if (!isTRUE(bilan$validation$ok)) {
        shiny::showNotification(
          sprintf(tr$t("field_ingest_errors"), nrow(bilan$validation$errors)),
          type = "error", duration = 8)
        return()
      }
      maj <- list(field_data_imported_at = Sys.time(),
                  field_data_source = "inventaire_video")
      if (!is.na(bilan$ndp)) maj$ndp_level <- bilan$ndp
      ok <- tryCatch({
        update_project_metadata(project$id, maj)
        TRUE
      }, error = function(e) {
        shiny::showNotification(paste("persist:", conditionMessage(e)),
                                type = "error", duration = 8)
        FALSE
      })
      if (!ok) return()
      app_state$current_project <- load_project(project$id)
      app_state$field_imported_at <- Sys.time()
      shiny::showNotification(tr$t("video_integration_ok"), type = "message",
                              duration = 6)
    })

    output$bilan <- shiny::renderUI(
      video_bilan_ui(rv$integration, i18n(), app_state$language %||% "fr"))

    # Renvoi sur le terrain des arbres a completer / a verifier (option du
    # brief groma, section 4).
    output$renvoi <- shiny::renderUI({
      if (!video_a_renvoyer(rv$integration) || !video_renvoi_disponible()) {
        return(NULL)
      }
      tr <- i18n()
      htmltools::div(
        class = "border-top pt-2 mt-2",
        htmltools::p(class = "small text-muted mb-2", tr$t("video_renvoi_aide")),
        shiny::actionButton(ns("renvoyer"), tr$t("video_renvoyer"),
                            icon = bsicons::bs_icon("arrow-repeat"),
                            class = "btn-outline-primary w-100 mb-2"),
        if (!is.null(rv$qgz_retour))
          shiny::downloadButton(ns("qgz_retour_dl"), tr$t("video_renvoi_telecharger"),
                                class = "btn-outline-primary w-100"))
    })

    shiny::observeEvent(input$renvoyer, {
      if (deny_if_readonly(app_state)) return()
      tr <- i18n()
      pp <- project_path()
      if (is.null(rv$import) || is.null(pp)) return()
      session$sendCustomMessage("nemetonSetDisabled",
                                list(id = ns("renvoyer"), disabled = TRUE))
      on.exit(session$sendCustomMessage("nemetonSetDisabled",
                                        list(id = ns("renvoyer"), disabled = FALSE)),
              add = TRUE)
      qgz <- tryCatch(video_renvoyer(rv$import, pp), error = function(e) e)
      if (inherits(qgz, "error") || !is.character(qgz) || !file.exists(qgz[1])) {
        msg <- if (inherits(qgz, "video_coeur_absent")) tr$t("video_coeur_absent")
               else paste(tr$t("error"),
                          if (inherits(qgz, "error")) conditionMessage(qgz) else "")
        shiny::showNotification(msg, type = "error", duration = 12)
        return()
      }
      rv$qgz_retour <- qgz[1]
      shiny::showNotification(
        sprintf(tr$t("video_renvoi_pret"),
                as.integer(attr(qgz, "n_arbres") %||% NA_integer_),
                length(attr(qgz, "plot_ids"))),
        type = "message", duration = 8)
    })

    output$qgz_retour_dl <- shiny::downloadHandler(
      filename = function() basename(rv$qgz_retour %||% "inventaire_video_retour.qgz"),
      content = function(file) file.copy(rv$qgz_retour, file, overwrite = TRUE),
      contentType = "application/zip"
    )

    rv
  })
}

#' Summary of a video inventory integration
#'
#' @param bilan Result of [video_bilan()], or `NULL`.
#' @param i18n Translator.
#' @param lang Language code, for the NDP badge.
#' @return A tag list.
#' @noRd
video_bilan_ui <- function(bilan, i18n, lang = "fr") {
  if (is.null(bilan)) return(NULL)
  table_df <- function(df, titre) {
    if (is.null(df) || !nrow(df)) return(NULL)
    htmltools::tagList(
      htmltools::tags$h6(class = "mt-2", sprintf("%s (%d)", titre, nrow(df))),
      htmltools::div(
        class = "table-responsive", style = "max-height: 200px;",
        htmltools::tags$table(
          class = "table table-sm",
          htmltools::tags$thead(htmltools::tags$tr(
            lapply(colnames(df), htmltools::tags$th))),
          htmltools::tags$tbody(lapply(seq_len(nrow(df)), function(i)
            htmltools::tags$tr(lapply(df[i, , drop = TRUE], function(v)
              htmltools::tags$td(as.character(v)))))))))
  }
  val <- bilan$validation
  n_err <- nrow(val$errors); n_warn <- nrow(val$warnings)
  classe <- if (val$ok && n_warn == 0) "alert alert-success"
            else if (val$ok) "alert alert-warning" else "alert alert-danger"
  nm <- bilan$numeros_modifies
  numeros <- if (nrow(nm) && all(c("ancien", "nouveau") %in% names(nm))) {
    htmltools::tagList(
      htmltools::tags$h6(class = "mt-2", i18n$t("video_numeros_modifies")),
      htmltools::tags$ul(class = "small", lapply(seq_len(nrow(nm)), function(i)
        htmltools::tags$li(paste0(
          if ("plot_id" %in% names(nm)) paste0(nm$plot_id[i], " : "),
          sprintf("%s \u2192 %s", nm$ancien[i], nm$nouveau[i]))))))
  }
  htmltools::tagList(
    htmltools::div(
      class = classe,
      htmltools::tags$strong(
        sprintf(i18n$t("field_ingest_report_header"), n_err, n_warn)),
      table_df(val$errors, i18n$t("field_ingest_report_errors")),
      table_df(val$warnings, i18n$t("field_ingest_report_warnings"))),
    if (bilan$classique_conserve > 0L)
      htmltools::div(class = "alert alert-info small",
                     sprintf(i18n$t("video_fusion_classique"),
                             bilan$classique_conserve)),
    if (!is.na(bilan$ndp))
      htmltools::div(class = "mb-2",
                     htmltools::tags$small(class = "text-muted me-2",
                                           i18n$t("field_ingest_ndp_after")),
                     ndp_badge(bilan$ndp, lang)),
    table_df(bilan$a_completer, i18n$t("video_a_completer")),
    table_df(bilan$a_verifier, i18n$t("video_a_verifier")),
    numeros)
}

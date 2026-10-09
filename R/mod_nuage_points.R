# mod_nuage_points.R - Terrain > Import > Nuage de points drone (spec 059)
#
# Depot d'un nuage LiDAR ou photogrammetrique de drone, traitement par le coeur
# (`nemeton::traiter_nuage_points()`, ExtendedTask) et affichage du MNS / MNH
# avec les controles de qualite. Le passage en NDP 2 est automatique
# (`detect_ndp_from_cache()`), comme la priorite des produits drone dans
# `resolve_project_dem()` / `resolve_project_chm()`.

#' Drone point cloud panel - UI
#'
#' @param id Module namespace ID.
#' @param i18n Translator.
#' @noRd
mod_nuage_points_ui <- function(id, i18n) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    htmltools::div(
      htmltools::tags$p(class = "text-muted small", i18n$t("nuage_intro")),
      shiny::fileInput(ns("fichiers"), i18n$t("nuage_fichiers"), multiple = TRUE,
                       accept = c(".las", ".laz"),
                       buttonLabel = i18n$t("field_ingest_browse"),
                       placeholder = i18n$t("field_ingest_placeholder")),
      shiny::radioButtons(ns("type"), i18n$t("nuage_type"),
                          choices = stats::setNames(
                            c("lidar_drone", "photogrammetrie"),
                            c(i18n$t("nuage_type_lidar"), i18n$t("nuage_type_photo")))),
      htmltools::tags$p(class = "text-muted small fst-italic",
                        i18n$t("nuage_photo_aide")),
      shiny::actionButton(ns("traiter"), i18n$t("nuage_traiter"),
                          icon = bsicons::bs_icon("layers"),
                          class = "btn-primary w-100")
    ),
    htmltools::div(
      shiny::uiOutput(ns("bilan")),
      shiny::radioButtons(ns("couche"), NULL, inline = TRUE,
                          choices = stats::setNames(
                            c("mnh", "mns", "mnt"),
                            c(i18n$t("nuage_couche_mnh"), i18n$t("nuage_couche_mns"),
                              i18n$t("nuage_couche_mnt")))),
      leaflet::leafletOutput(ns("carte"), height = "50vh")
    )
  )
}

#' Drone point cloud panel - server
#'
#' @param id Module namespace ID.
#' @param app_state Shared reactiveValues (`current_project`, `language`).
#' @return The reactive holding the last processing result (for tests).
#' @noRd
mod_nuage_points_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    i18n <- shiny::reactive(get_i18n(app_state$language %||% "fr"))
    resultat <- shiny::reactiveVal(NULL)

    projet_path <- shiny::reactive(app_state$current_project$path)

    # Le dernier traitement du projet ouvert, relu a chaque changement de projet.
    shiny::observeEvent(projet_path(), {
      resultat(nuage_dernier_traitement(projet_path()))
    }, ignoreNULL = FALSE)

    dev_path <- tryCatch(
      if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
        find.package("nemetonshiny") else NULL,
      error = function(e) NULL)

    tache <- shiny::ExtendedTask$new(function(path, type, dev, app_opts) {
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
        asNamespace("nemetonshiny")$nuage_traiter(path, type)
      }, seed = TRUE)
    })

    shiny::observeEvent(input$traiter, {
      if (identical(tache$status(), "running")) return()
      path <- projet_path()
      tr <- i18n()
      if (is.null(path)) {
        shiny::showNotification(tr$t("nuage_sans_projet"), type = "warning")
        return()
      }
      f <- input$fichiers
      if (!is.null(f) && nrow(f)) {
        dep <- nuage_deposer(path, f$datapath, f$name)
        if (length(dep$refuses)) {
          shiny::showNotification(
            paste(tr$t("nuage_refuses"), paste(dep$refuses, collapse = ", ")),
            type = "warning", duration = 12)
        }
      }
      if (length(nuage_fichiers(path)) == 0L) {
        shiny::showNotification(tr$t("nuage_sans_nuage"), type = "warning")
        return()
      }
      shiny::showNotification(
        htmltools::tagList(shiny::icon("spinner", class = "fa-spin me-2"),
                           tr$t("nuage_en_cours")),
        type = "message", duration = NULL, closeButton = FALSE,
        id = "nuage_en_cours", session = session)
      session$sendCustomMessage("nemetonSetDisabled",
                                list(id = ns("traiter"), disabled = TRUE))
      tache$invoke(path, input$type, dev_path, get_app_options())
    })

    shiny::observeEvent(tache$status(), {
      st <- tache$status()
      if (!st %in% c("success", "error")) return()
      tr <- i18n()
      shiny::removeNotification("nuage_en_cours", session = session)
      session$sendCustomMessage("nemetonSetDisabled",
                                list(id = ns("traiter"), disabled = FALSE))
      res <- tryCatch(tache$result(), error = function(e)
        list(status = "error", message = conditionMessage(e)))
      if (identical(res$status, "ok")) {
        resultat(res)
        shiny::showNotification(tr$t("nuage_ok"), type = "message", duration = 10)
      } else {
        msg <- switch(res$status,
                      sans_nuage = tr$t("nuage_sans_nuage"),
                      sans_mnt = tr$t("nuage_sans_mnt"),
                      paste(tr$t("error"), res$message %||% ""))
        shiny::showNotification(msg, type = "error", duration = 15)
      }
    })

    output$bilan <- shiny::renderUI(nuage_bilan_ui(resultat(), i18n()))

    output$carte <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery) |>
        leaflet::setView(lng = 2.5, lat = 46.6, zoom = 5)
    })

    shiny::observe({
      res <- resultat()
      couche <- input$couche %||% "mnh"
      proxy <- leaflet::leafletProxy(ns("carte"))
      proxy |> leaflet::clearImages() |> leaflet::clearControls()
      r <- .nuage_raster_affichage(res[[couche]])
      if (is.null(r)) return()
      pal <- leaflet::colorNumeric(
        if (identical(couche, "mnh")) "Greens" else "viridis",
        terra::values(r, mat = FALSE), na.color = "transparent")
      e <- terra::ext(terra::project(r, "EPSG:4326"))
      proxy |>
        leaflet::addRasterImage(r, colors = pal, opacity = 0.8, project = TRUE) |>
        leaflet::addLegend(pal = pal, values = terra::values(r, mat = FALSE),
                           title = "m", position = "bottomright") |>
        leaflet::fitBounds(e$xmin, e$ymin, e$xmax, e$ymax)
    }) |> shiny::bindEvent(resultat(), input$couche)

    resultat
  })
}

#' Downsampled copy of a product raster, light enough for leaflet
#' @noRd
.nuage_raster_affichage <- function(path, max_cellules = 1e6) {
  if (is.null(path) || length(path) != 1L || !file.exists(path)) return(NULL)
  r <- tryCatch(terra::rast(path)[[1]], error = function(e) NULL)
  if (is.null(r)) return(NULL)
  f <- ceiling(sqrt(terra::ncell(r) / max_cellules))
  if (f > 1) r <- terra::aggregate(r, fact = f, fun = "mean", na.rm = TRUE)
  r
}

#' Quality summary of a processed cloud
#'
#' @param res Result of [nuage_traiter()] / [nuage_dernier_traitement()].
#' @param i18n Translator.
#' @return A tag list, or a short hint when nothing has been processed.
#' @noRd
nuage_bilan_ui <- function(res, i18n) {
  if (is.null(res) || !identical(res$status, "ok")) {
    return(htmltools::p(class = "text-muted small", i18n$t("nuage_aucun")))
  }
  q <- res$qualite %||% list()
  pct <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) != 1L || !is.finite(x)) "\u2014" else sprintf("%.1f %%", 100 * x)
  }
  nb <- function(x, d = 1) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) != 1L || !is.finite(x)) return("\u2014")
    formatC(x, format = "f", digits = d, big.mark = "\u202f",
            decimal.mark = if (identical(i18n$language, "fr")) "," else ".")
  }
  lignes <- list(
    c(i18n$t("nuage_points"), nb(q$n_points, 0)),
    c(i18n$t("nuage_densite"), nb(q$densite)),
    c(i18n$t("nuage_part_sol"), pct(q$part_sol)),
    c(i18n$t("nuage_part_bruit"), pct(q$part_bruit)),
    c(i18n$t("nuage_mnh_negatif"), pct(q$part_mnh_negatif)))
  if (identical(res$type, "photogrammetrie")) {
    lignes <- c(lignes, list(
      c(i18n$t("nuage_decalage"), nb(q$decalage_vertical, 2)),
      c(i18n$t("nuage_decalage_iqr"), nb(q$decalage_iqr, 2)),
      c(i18n$t("nuage_sol_nu"), nb(q$n_sol_nu, 0))))
  }
  avert <- res$avertissements %||% character(0)
  htmltools::tagList(
    htmltools::div(class = "alert alert-success py-2 small", i18n$t("nuage_ndp2")),
    htmltools::tags$table(
      class = "table table-sm mb-2",
      htmltools::tags$tbody(lapply(lignes, function(l) htmltools::tags$tr(
        htmltools::tags$th(class = "fw-normal", l[1]),
        htmltools::tags$td(class = "text-end", l[2]))))),
    if (length(avert)) htmltools::div(
      class = "alert alert-warning py-2 small",
      htmltools::tags$b(i18n$t("nuage_avertissements")),
      htmltools::tags$ul(class = "mb-0", lapply(avert, htmltools::tags$li))))
}

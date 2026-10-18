# mod_regeneration_climat.R - sous-onglets " Contexte climatique " de
# reGeneration (spec 063 coeur, brief 2026-10-18 cablage).
#
# Quatre sous-onglets : tendances E-OBS / CHELSA / SAFRAN (cartes bivariees
# leaflet, a la resolution native de la source) et nuages annuels (plotly).
# Sidebar commune : rayon du cercle, periode, opacite, Calculer / Recalculer.
# Les calculs sont lances au CLIC seulement (E-OBS = 8 Go au premier usage) ;
# sans clic, l'onglet ne fait que relire le cache du projet.
#
# Ces fonctions sont appelees depuis `mod_regeneration_ui()` /
# `mod_regeneration_server()` : elles partagent leur `session` (memes ids,
# meme verrou `busy()` des calculs).

# ---- UI -----------------------------------------------------------------------

#' UI of the climate-context panel (spec 063)
#' @param ns Namespace function of the reGeneration module.
#' @param i18n Translator.
#' @noRd
.regen_climat_ui <- function(ns, i18n) {
  an_max <- as.integer(format(Sys.Date(), "%Y")) - 1L
  onglet_carte <- function(source) {
    bslib::nav_panel(i18n$t(paste0("regen_climat_onglet_", source)), value = source,
      leaflet::leafletOutput(ns(paste0("climat_carte_", source)), height = "68vh"))
  }
  sur_nuages <- sprintf("input['%s'] == 'nuages'", ns("climat_onglet"))
  hors_nuages <- sprintf("input['%s'] != 'nuages'", ns("climat_onglet"))

  bslib::nav_panel(i18n$t("regen_climat"),
    icon = bsicons::bs_icon("cloud-sun"),
    bslib::layout_sidebar(
      fillable = TRUE,
      sidebar = bslib::sidebar(
        position = "right", open = "always", width = 260,
        shiny::sliderInput(ns("climat_rayon_km"), i18n$t("regen_climat_rayon"),
          min = 10, max = 100, value = 25, step = 5, ticks = FALSE),
        shiny::conditionalPanel(hors_nuages,
          shiny::radioButtons(ns("climat_periode"), i18n$t("regen_climat_periode"),
            # Choix des annees par defaut, sur 2006-2020 : la periode de la
            # Figure 1 de L'IF n. 49.
            choiceValues = c("libre", "complete"),
            choiceNames = list(i18n$t("regen_climat_periode_libre"),
                               i18n$t("regen_climat_periode_complete")),
            selected = "libre"),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'libre'", ns("climat_periode")),
            shiny::sliderInput(ns("climat_periode_libre"), NULL,
              min = 1950, max = an_max, value = .REGEN_CLIMAT_PERIODE_DEFAUT, step = 1,
              sep = "", ticks = FALSE)),
          shiny::sliderInput(ns("climat_opacite"), i18n$t("regen_climat_opacite"),
            min = 0, max = 1, value = 0.8, step = 0.05, ticks = FALSE)),
        shiny::conditionalPanel(sur_nuages,
          shiny::radioButtons(ns("climat_nuages_source"), i18n$t("regen_climat_nuages_source"),
            choiceValues = c("chelsa", "safran", "eobs"),
            choiceNames = list("CHELSA (1 km)", "SAFRAN (8 km)", "E-OBS (0,1\u00b0)"),
            selected = "chelsa"),
          shiny::radioButtons(ns("climat_nuages_mode"), i18n$t("regen_climat_nuages_mode"),
            choiceValues = c("anomalie", "tendance_cumulee"),
            choiceNames = list(i18n$t("regen_climat_mode_anomalie"),
                               i18n$t("regen_climat_mode_tendance")),
            selected = "anomalie")),
        # Series CDS du projet (precipitations, T moyenne) : seul le diagramme
        # ombrothermique du clic (graphe 4, spec 036) en a encore besoin.
        shiny::conditionalPanel(sprintf("input['%s'] == 'eobs'", ns("climat_onglet")),
          bslib::input_task_button(ns("fetch_eobs_rr"), i18n$t("regen_eobs_rr_fetch"),
            icon = bsicons::bs_icon("cloud-download"),
            label_busy = i18n$t("regen_busy_generic"),
            type = "outline-secondary", class = "btn-sm w-100 mb-2 regen-calc-btn"),
          bslib::input_task_button(ns("fetch_eobs_tg"), i18n$t("regen_fetch_tg"),
            icon = bsicons::bs_icon("thermometer-half"),
            label_busy = i18n$t("regen_busy_generic"),
            type = "outline-secondary", class = "btn-sm w-100 mb-2 regen-calc-btn")),
        htmltools::tags$hr(class = "my-2"),
        bslib::input_task_button(ns("climat_calculer"), i18n$t("regen_climat_calculer"),
          icon = bsicons::bs_icon("play-fill"),
          label_busy = i18n$t("regen_busy_generic"),
          type = "primary", class = "btn-sm w-100 mb-2 regen-calc-btn"),
        shiny::actionButton(ns("climat_recalculer"), i18n$t("regen_climat_recalculer"),
          icon = bsicons::bs_icon("arrow-repeat"),
          class = "btn-outline-warning btn-sm w-100 regen-calc-btn")
      ),
      shiny::uiOutput(ns("climat_statut")),
      bslib::navset_card_underline(
        id = ns("climat_onglet"),
        onglet_carte("eobs"),
        onglet_carte("chelsa"),
        onglet_carte("safran"),
        bslib::nav_panel(i18n$t("regen_climat_onglet_nuages"), value = "nuages",
          plotly::plotlyOutput(ns("climat_nuages"), height = "60vh"),
          htmltools::tags$h6(class = "mt-3", i18n$t("regen_climat_nuages_grille")),
          shiny::plotOutput(ns("climat_nuages_grille"), height = "auto"))
      ),
      shiny::uiOutput(ns("climat_attribution"))
    ))
}

# ---- Tache de calcul ------------------------------------------------------------

#' ExtendedTask of the climate context (maps and yearly clouds)
#'
#' One task for both kinds (`type = "carte"` or `"nuages"`): a single task
#' button (`climat_calculer`) can only be bound to one task. Persistent `future`
#' worker, like the other reGeneration tasks; the inline option runs it in the
#' session for the tests.
#' @noRd
.regen_climat_task <- function() {
  shiny::ExtendedTask$new(
    function(type, units, project_path, source, rayon_km, choix, dev_path, app_opts) {
      run <- function() {
        if (identical(type, "nuages")) {
          run_regeneration_nuages(units, project_path, source, rayon_km, choix)
        } else {
          run_regeneration_climat(units, project_path, source, rayon_km, choix)
        }
      }
      if (isTRUE(getOption("nemetonshiny.analyse_inline"))) {
        return(promises::promise(function(resolve, reject) {
          .later_sur(function() tryCatch(resolve(run()), error = function(e) reject(e)))
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
        fn <- utils::getFromNamespace(
          if (identical(type, "nuages")) "run_regeneration_nuages" else "run_regeneration_climat",
          "nemetonshiny")
        fn(units, project_path, source, rayon_km, choix)
      }, seed = TRUE)
    })
}

# ---- Helpers de presentation (purs) -----------------------------------------------

#' Toast label of a climate progress step, or NULL
#' @noRd
.regen_climat_step_label <- function(i18n, st) {
  if (is.null(st) || is.null(st$phase)) return(NULL)
  nom <- c(eobs = "E-OBS", chelsa = "CHELSA", safran = "SAFRAN")
  num <- function(x) suppressWarnings(as.integer(x %||% NA)[1])
  switch(st$phase,
    "climat:telechargement" = sprintf(i18n$t("regen_climat_etape_dl"), st$var %||% ""),
    "climat:mensualisation" = sprintf(i18n$t("regen_climat_etape_mensuel"),
                                      num(st$annee), num(st$i), num(st$n)),
    "climat:annee" = sprintf(i18n$t("regen_climat_etape_annee"),
                             nom[[st$source %||% "eobs"]] %||% "", num(st$annee),
                             num(st$i), num(st$n)),
    "climat:cache_hit" = i18n$t("regen_climat_etape_cache"),
    NULL)
}

#' Bivariate 10 x 10 legend of a climate map (HTML string)
#' @noRd
.regen_climat_legende <- function(meta, i18n) {
  pal <- meta$palette %||% list()
  per <- suppressWarnings(as.integer(meta$periode))
  mode <- meta$bornes$mode %||% "emprise"
  lib_mode <- switch(mode,
    france = i18n$t("regen_climat_bornes_france"),
    i18n$t("regen_climat_bornes_emprise"))
  sous_titre <- if (length(per) == 2L && all(is.finite(per)))
    sprintf("%d-%d \u00b7 %s", per[1], per[2], lib_mode) else lib_mode
  leg <- bivariate_legend_html(
    palette = stats::setNames(pal$colors, pal$classes),
    axis_x = i18n$t("regen_climat_axe_precip"),
    axis_y = i18n$t("regen_climat_axe_tmax"),
    title = i18n$t("regen_climat_legende_titre"),
    subtitle = sous_titre,
    ncol = pal$ncol,
    zero = pal$zero,
    x_range = meta$bornes$precip,
    y_range = meta$bornes$tmax)
  if (identical(meta$bornes$reason, "france_eobs_cache_missing")) {
    leg <- htmltools::tagList(leg, htmltools::tags$div(
      class = "small text-warning-emphasis mt-1", style = "max-width:220px;",
      i18n$t("regen_climat_bornes_relatives")))
  }
  as.character(leg)
}

#' Add the class raster of a climate map to a leaflet map or proxy
#'
#' Nearest neighbour only (`method = "ngb"`): a bilinear warp would invent
#' colours between two classes. The grid is the source's native one; leaflet's
#' Web Mercator projection is the only reprojection.
#' @noRd
.regen_climat_add_raster <- function(m, etat, op, groupe) {
  if (!identical(etat$status, "ok") || !inherits(etat$raster, "SpatRaster")) return(m)
  pal <- etat$meta$palette
  r <- terra::round(etat$raster)
  cmap <- leaflet::colorFactor(pal$colors, domain = pal$classes, na.color = "transparent")
  leaflet::addRasterImage(m, r, colors = cmap, opacity = op, project = TRUE,
    method = "ngb", group = groupe,
    options = leaflet::gridOptions(pane = "climatRaster"))
}

#' Axis titles of the yearly clouds
#' @noRd
.regen_nuages_axes <- function(meta, i18n) {
  if (identical(meta$mode, "tendance_cumulee")) {
    ans <- suppressWarnings(as.integer(meta$annees))
    debut <- if (length(ans)) min(ans) - (as.integer(meta$recul_min %||% 10L) - 1L) else NA
    list(x = sprintf(i18n$t("regen_climat_nuages_x_tendance"), debut),
         y = sprintf(i18n$t("regen_climat_nuages_y_tendance"), debut))
  } else {
    list(x = i18n$t("regen_climat_nuages_x_anomalie"),
         y = i18n$t("regen_climat_nuages_y_anomalie"))
  }
}

#' Animated plotly cloud (one frame per year, shared axes)
#' @noRd
.regen_nuages_plot <- function(res, i18n) {
  ax <- .regen_nuages_axes(res$meta, i18n)
  pts <- res$points; moy <- res$moyennes
  moy <- moy[is.finite(moy$x) & is.finite(moy$y), , drop = FALSE]
  zero_l <- list(color = "#9e9e9e", width = 1)
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, data = pts, x = ~x, y = ~y, frame = ~annee,
    type = "scattergl", mode = "markers", name = i18n$t("regen_climat_nuages_mailles"),
    marker = list(size = 4, opacity = 0.3, color = "#A3195B"), hoverinfo = "skip")
  p <- plotly::add_trace(p, data = moy, x = ~x, y = ~y, frame = ~annee,
    type = "scattergl", mode = "markers", name = i18n$t("regen_climat_nuages_moyenne"),
    marker = list(size = 11, color = "#FFFFFF", line = list(color = "#000000", width = 1.5)))
  p <- plotly::layout(p,
    xaxis = list(title = ax$x, range = res$limites$x, zeroline = TRUE,
                 zerolinecolor = zero_l$color),
    yaxis = list(title = ax$y, range = res$limites$y, zeroline = TRUE,
                 zerolinecolor = zero_l$color),
    showlegend = TRUE, legend = list(orientation = "h", y = -0.2))
  # scattergl n'anime qu'avec redraw = TRUE (seul `scatter` s'en passe).
  p <- plotly::animation_opts(p, frame = 600, transition = 0, redraw = TRUE)
  plotly::animation_slider(p, currentvalue = list(prefix = ""))
}

#' Small multiples of the yearly clouds (one panel per year, shared axes)
#' @noRd
.regen_nuages_grille <- function(res, i18n) {
  ax <- .regen_nuages_axes(res$meta, i18n)
  moy <- res$moyennes[is.finite(res$moyennes$x) & is.finite(res$moyennes$y), , drop = FALSE]
  ggplot2::ggplot(res$points, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
    ggplot2::geom_point(size = 0.4, alpha = 0.3, stroke = 0, colour = "#A3195B") +
    ggplot2::geom_point(data = moy, shape = 21, size = 2, fill = "white",
                        colour = "black", stroke = 0.5) +
    ggplot2::facet_wrap(~annee, ncol = 6) +
    ggplot2::coord_cartesian(xlim = res$limites$x, ylim = res$limites$y) +
    ggplot2::labs(x = ax$x, y = ax$y) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold"))
}

# ---- Serveur ------------------------------------------------------------------

#' Server side of the climate-context panel
#'
#' Called from `mod_regeneration_server()` with its session and lock helpers.
#'
#' @param task The `.regen_climat_task()` (bound to `climat_calculer` and part
#'   of the module's `busy()` lock by the caller).
#' @param units_sf Reactive of the project units.
#' @param deny_if_busy Lock guard of the module.
#' @param dev_path Package path in dev mode (worker reload), or `NULL`.
#' @return The `reactiveValues` state (one entry per source + `nuages`), used
#'   by the map click.
#' @noRd
.regen_climat_server <- function(input, output, session, i18n, app_state,
                                 units_sf, task, deny_if_busy, dev_path) {
  cl <- shiny::reactiveValues(eobs = NULL, chelsa = NULL, safran = NULL,
                              nuages = NULL, running = FALSE, start = NULL,
                              pid = NULL, job = NULL, refresh = 0L)
  notif_id <- session$ns("climat_notif")
  couche <- i18n$t("regen_climat_couche")
  cercle_grp <- i18n$t("regen_climat_cercle")

  projet <- function() tryCatch(app_state$current_project$path, error = function(e) NULL)
  onglet <- function() input$climat_onglet %||% "eobs"
  rayon <- function() as.numeric(input$climat_rayon_km %||% 25)
  periode <- function() {
    p <- input$climat_periode %||% "libre"
    if (identical(p, "libre")) input$climat_periode_libre %||% .REGEN_CLIMAT_PERIODE_DEFAUT else p
  }
  source_nuages <- function() input$climat_nuages_source %||% "chelsa"
  mode_nuages <- function() input$climat_nuages_mode %||% "anomalie"
  # Ce que le sous-onglet courant demande : type, source, choix, cle d'etat.
  demande <- function() {
    o <- onglet()
    if (identical(o, "nuages")) {
      list(type = "nuages", source = source_nuages(), choix = mode_nuages(),
           cle = paste(projet(), source_nuages(), rayon(), mode_nuages()), etat = "nuages")
    } else {
      list(type = "carte", source = o, choix = periode(),
           cle = paste(projet(), o, rayon(), .regen_climat_periode(periode())$tag), etat = o)
    }
  }

  # --- Chargement paresseux DEPUIS LE CACHE seulement (aucun calcul) ---------
  shiny::observeEvent(
    list(input$climat_onglet, input$climat_rayon_km, input$climat_periode,
         input$climat_periode_libre, input$climat_nuages_source,
         input$climat_nuages_mode, units_sf(), cl$refresh), {
      pp <- projet(); if (is.null(pp) || is.null(units_sf())) return()
      d <- demande()
      cur <- shiny::isolate(cl[[d$etat]])
      if (!is.null(cur) && identical(cur$cle, d$cle)) return()
      if (identical(d$type, "nuages")) {
        res <- regeneration_nuages_cached(pp, d$source, rayon(), d$choix)
        cl$nuages <- if (!is.null(res))
          list(cle = d$cle, status = "ok", res = res, meta = res$meta) else
          list(cle = d$cle, status = "a_calculer", source = d$source)
      } else {
        x <- regeneration_climat_cached(pp, d$source, rayon(), d$choix)
        cl[[d$etat]] <- if (!is.null(x))
          c(list(cle = d$cle, status = "ok"), x) else
          list(cle = d$cle, status = "a_calculer", source = d$source)
      }
    }, ignoreNULL = FALSE)

  # --- Calcul au clic -------------------------------------------------------------
  lancer <- function() {
    if (deny_if_busy()) return(invisible(FALSE))
    if (deny_if_readonly(app_state, i18n)) {
      bslib::update_task_button("climat_calculer", state = "ready")
      return(invisible(FALSE))
    }
    pp <- projet(); units <- units_sf()
    if (is.null(pp) || is.null(units)) {
      bslib::update_task_button("climat_calculer", state = "ready")
      shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
      return(invisible(FALSE))
    }
    d <- demande()
    unlink(file.path(.regen_climat_dir(pp), "climat_status.json"))
    cl$running <- TRUE; cl$start <- Sys.time(); cl$job <- d
    cl$pid <- shiny::isolate(app_state$project_id %||% app_state$current_project$id)
    shiny::showNotification(
      .running_notif_content(i18n$t("regen_climat_en_cours"), cl$start),
      id = notif_id, type = "message", duration = NULL)
    task$invoke(d$type, units, pp, d$source, rayon(), d$choix, dev_path, get_app_options())
    invisible(TRUE)
  }
  shiny::observeEvent(input$climat_calculer, lancer())

  # Recalcul : purge le cache PROJET du couple courant (jamais le cache machine
  # E-OBS ni les series du cercle), puis relance.
  shiny::observeEvent(input$climat_recalculer, {
    if (deny_if_busy()) return()
    if (deny_if_readonly(app_state, i18n)) return()
    pp <- projet(); if (is.null(pp)) {
      shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
      return()
    }
    d <- demande()
    if (identical(d$type, "nuages")) {
      unlink(.regen_nuages_path(pp, d$source, rayon(), d$choix))
    } else {
      regeneration_climat_purge(pp, d$source, rayon(), d$choix)
    }
    cl[[d$etat]] <- NULL
    shiny::showNotification(i18n$t("regen_climat_recalcul_lance"), type = "message", duration = 4)
    lancer()
  })

  # Chrono + etape du coeur dans le toast, chaque seconde.
  shiny::observe({
    if (!isTRUE(cl$running)) return()
    shiny::invalidateLater(1000)
    etape <- .regen_climat_step_label(i18n, .regen_read_climat_step(shiny::isolate(projet())))
    lib <- if (is.null(etape)) i18n$t("regen_climat_en_cours") else
      paste0(i18n$t("regen_climat_en_cours"), " \u2014 ", etape)
    shiny::showNotification(.running_notif_content(lib, shiny::isolate(cl$start)),
      id = notif_id, type = "message", duration = NULL)
  })

  shiny::observeEvent(task$status(), {
    st <- task$status()
    if (!st %in% c("success", "error")) return()
    cl$running <- FALSE; cl$start <- NULL
    shiny::removeNotification(notif_id)
    d <- cl$job; cl$job <- NULL
    if (is.null(d)) return()
    if (!.est_projet_courant(app_state, cl$pid)) {
      # Resultat d'un autre projet : relire le cache du projet ouvert.
      cl[[d$etat]] <- NULL
      cl$refresh <- (shiny::isolate(cl$refresh) %||% 0L) + 1L
      return()
    }
    if (identical(st, "error")) {
      msg <- tryCatch({ task$result(); "" }, error = function(e) conditionMessage(e))
      shiny::showNotification(sprintf(i18n$t("regen_climat_echec"), msg),
                              type = "error", duration = 10)
      cl[[d$etat]] <- list(cle = d$cle, status = "indisponible", message = msg,
                           source = d$source)
      return()
    }
    res <- tryCatch(task$result(), error = function(e) NULL)
    if (identical(d$type, "nuages")) {
      ok <- identical(res$meta$status, "ok")
      cl$nuages <- c(list(cle = d$cle, source = d$source,
                          status = res$meta$status %||% "indisponible"),
                     if (ok) list(res = res), list(meta = res$meta))
    } else {
      meta <- res$carte %||% res$series %||% list(status = "indisponible")
      x <- if (identical(meta$status, "ok"))
        regeneration_climat_cached(projet(), d$source, rayon(), d$choix)
      cl[[d$etat]] <- if (!is.null(x)) c(list(cle = d$cle, status = "ok"), x) else
        list(cle = d$cle, status = if (identical(meta$status, "ok")) "indisponible" else
          meta$status %||% "indisponible", meta = meta, source = d$source,
          message = meta$message %||% NULL)
    }
    if (identical(cl[[d$etat]]$status, "ok")) {
      shiny::showNotification(i18n$t("regen_climat_termine"), type = "message", duration = 4)
    }
  })

  # --- Bandeau de statut du sous-onglet courant ---------------------------------
  output$climat_statut <- shiny::renderUI({
    if (isTRUE(cl$running)) return(NULL)
    o <- onglet()
    e <- cl[[if (identical(o, "nuages")) "nuages" else o]]
    if (is.null(e)) return(NULL)
    boite <- function(classe, icone, ...) htmltools::div(
      class = paste("alert py-2 small d-flex align-items-center", classe),
      bsicons::bs_icon(icone, class = "me-2 flex-shrink-0"),
      htmltools::div(class = "flex-grow-1", ...))
    detail <- function() {
      x <- e$message %||% e$meta$message %||% e$meta$reason
      if (!is.null(x) && nzchar(x)) paste0(" : ", x) else ""
    }
    switch(e$status,
      a_calculer = boite("alert-info", "info-circle",
        i18n$t(if (identical(o, "nuages")) "regen_climat_nuages_a_calculer" else "regen_climat_a_calculer"),
        if (identical(e$source, "eobs") && !regen_climat_eobs_pret())
          htmltools::div(class = "mt-1 fw-semibold", i18n$t("regen_climat_eobs_1er_usage"))),
      ok = {
        per <- suppressWarnings(as.integer(e$meta$periode))
        if (!identical(o, "nuages") && length(per) == 2L && all(is.finite(per))) {
          htmltools::div(class = "text-muted small mb-1",
            sprintf(i18n$t("regen_climat_periode_utilisee"), per[1], per[2],
                    as.integer(e$meta$n_annees %||% NA)),
            " \u00b7 ", i18n$t("regen_climat_unite_totale"))
        }
      },
      hors_couverture = boite("alert-warning", "exclamation-triangle-fill",
                              i18n$t("regen_climat_hors_couverture")),
      donnees_insuffisantes = boite("alert-warning", "exclamation-triangle-fill",
                                    paste0(i18n$t("regen_climat_insuffisant"), detail())),
      boite("alert-warning", "exclamation-triangle-fill",
            paste0(i18n$t("regen_climat_indisponible"), detail())))
  })

  output$climat_attribution <- shiny::renderUI({
    o <- onglet()
    e <- cl[[if (identical(o, "nuages")) "nuages" else o]]
    att <- e$meta$attribution
    src <- if (identical(o, "nuages")) source_nuages() else o
    htmltools::tags$p(class = "text-muted small mt-1 mb-0",
      bsicons::bs_icon("c-circle", class = "me-1"),
      if (identical(src, "eobs")) i18n$t("regen_eobs_attribution") else att %||% "")
  })

  # --- Cartes : une par source, re-rendue quand son etat change ------------------
  lapply(.REGEN_CLIMAT_SOURCES, function(s) {
    id <- paste0("climat_carte_", s)
    output[[id]] <- leaflet::renderLeaflet({
      units <- units_sf(); shiny::req(units)
      etat <- cl[[s]]
      op <- max(0, min(1, as.numeric(shiny::isolate(input$climat_opacite) %||% 0.8)))
      geo_ugf <- tryCatch(sf::st_transform(units, 4326), error = function(e) units)
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("climatRaster", zIndex = 250) |>
        leaflet::addLayersControl(baseGroups = c("OSM", "Satellite"),
          overlayGroups = c(couche, cercle_grp, "UGF"),
          options = leaflet::layersControlOptions(collapsed = TRUE))
      m <- .regen_climat_add_raster(m, etat, op, couche)
      m <- leaflet::addPolygons(m, data = geo_ugf, group = "UGF", weight = 1.5,
        color = "#1f6feb", fillColor = "#1f6feb", fillOpacity = 0.05)
      cercle <- etat$cercle
      if (inherits(cercle, "sf")) {
        cercle <- tryCatch(sf::st_transform(cercle, 4326), error = function(e) cercle)
        m <- leaflet::addPolygons(m, data = cercle, group = cercle_grp, color = "#000000",
          weight = 1.5, fill = FALSE, opacity = 1)
      }
      if (identical(etat$status, "ok")) {
        m <- leaflet::addControl(m, html = .regen_climat_legende(etat$meta, i18n),
          position = "bottomright", layerId = paste0(id, "_legende"),
          className = "nmt-bivariate-control")
      }
      bb <- tryCatch(as.numeric(sf::st_bbox(if (inherits(cercle, "sf")) cercle else geo_ugf)),
                     error = function(e) NULL)
      if (!is.null(bb) && all(is.finite(bb))) m <- leaflet::fitBounds(m, bb[1], bb[2], bb[3], bb[4])
      m
    })
  })

  # Opacite : proxy sur la carte visible, sans re-rendu (zoom conserve).
  shiny::observeEvent(input$climat_opacite, {
    s <- onglet(); if (!s %in% .REGEN_CLIMAT_SOURCES) return()
    etat <- cl[[s]]; if (!identical(etat$status, "ok")) return()
    op <- max(0, min(1, as.numeric(input$climat_opacite)))
    proxy <- leaflet::leafletProxy(paste0("climat_carte_", s)) |> leaflet::clearGroup(couche)
    .regen_climat_add_raster(proxy, etat, op, couche)
  }, ignoreInit = TRUE)

  # --- Nuages annuels ------------------------------------------------------------
  output$climat_nuages <- plotly::renderPlotly({
    e <- cl$nuages
    if (is.null(e$res)) return(.regen_ctx_empty_plot(i18n$t("regen_climat_nuages_a_calculer")))
    p <- .regen_nuages_plot(e$res, i18n)
    m <- e$res$meta
    plotly::layout(p, annotations = list(list(
      text = sprintf(i18n$t("regen_climat_nuages_legende"), m$attribution %||% "",
                     as.integer(m$n_cellules %||% 0L)),
      xref = "paper", yref = "paper", x = 0, y = 1.06, showarrow = FALSE,
      xanchor = "left", font = list(size = 10, color = "#555555"))))
  })

  output$climat_nuages_grille <- shiny::renderPlot({
    e <- cl$nuages
    shiny::req(e$res)
    .regen_nuages_grille(e$res, i18n)
  }, height = function() {
    e <- cl$nuages
    n <- length(unique(e$res$moyennes$annee %||% integer(0)))
    max(200L, 150L * ceiling(max(n, 1L) / 6L) + 60L)
  })

  cl
}

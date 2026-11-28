# mod_monitoring_icd.R - Sous-onglet "Carte ICD" du Suivi sanitaire.
#
# Presentation pure de l'indicateur composite de decoloration : le calcul, la
# lecture des rasters et des series, la comparaison avec FAST sont au coeur
# (voir R/service_icd.R). Ici : carte Leaflet du niveau de l'annee choisie,
# clic -> modale a deux graphiques, bloc de comparaison avec FAST.

# Palette ordonnee des niveaux 0 (normal) a 4 (alerte repetee).
.ICD_COULEURS_NIVEAUX <- c("0" = "#ffffb2", "1" = "#fecc5c", "2" = "#fd8d3c",
                           "3" = "#e31a1c", "4" = "#800026")

#' @param id Module namespace id.
#' @return A `shiny.tag`.
#' @noRd
mod_monitoring_icd_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 250L, position = "right", open = "always",
        shiny::uiOutput(ns("controls"))
      ),
      shiny::uiOutput(ns("source_s2")),
      shiny::uiOutput(ns("bandeau")),
      htmltools::div(
        style = "position: relative;",
        leaflet::leafletOutput(ns("map"), height = "55vh"),
        shiny::uiOutput(ns("overlay"))
      ),
      shiny::uiOutput(ns("sans_icd")),
      shiny::uiOutput(ns("comparaison"))
    )
  )
}

# Tableau croise FAST (lignes) x ICD (colonnes), en hectares.
.icd_table_croisement <- function(cr, i18n) {
  tab <- stats::xtabs(surface_ha ~ fast + icd, data = cr)
  ent <- htmltools::tags$tr(
    htmltools::tags$th(i18n$t("icd_cmp_fast_icd")),
    lapply(colnames(tab), function(j) htmltools::tags$th(
      class = "text-end", sprintf(i18n$t("icd_cmp_ent_icd"), j))))
  lignes <- lapply(rownames(tab), function(i) htmltools::tags$tr(
    htmltools::tags$th(sprintf(i18n$t("icd_cmp_ent_fast"), i)),
    lapply(colnames(tab), function(j) htmltools::tags$td(
      class = "text-end", format(round(tab[i, j], 2), nsmall = 2)))))
  htmltools::tags$table(
    class = "table table-sm table-bordered w-auto",
    htmltools::tags$thead(ent), htmltools::tags$tbody(lignes))
}

# Rendu du bloc de comparaison a partir du resultat du coeur.
.icd_rendu_comparaison <- function(cmp, i18n) {
  if (is.null(cmp)) return(NULL)
  if (!identical(cmp$statut, "ok")) {
    return(htmltools::div(
      class = "alert alert-info mt-2",
      i18n$t(paste0("icd_cmp_", cmp$statut))))
  }
  se <- cmp$surface_egale
  pct <- function(x) sprintf("%.1f %%", 100 * x)
  htmltools::tagList(
    .icd_table_croisement(cmp$croisement, i18n),
    htmltools::p(sprintf(i18n$t("icd_cmp_accord"), pct(cmp$accord_global),
                         format(round(cmp$kappa, 3), nsmall = 3))),
    htmltools::p(class = "small text-muted", i18n$t("icd_cmp_kappa_note")),
    if (!is.null(se)) htmltools::div(
      htmltools::tags$strong(i18n$t("icd_cmp_egale_titre")),
      htmltools::p(sprintf(i18n$t("icd_cmp_egale"), pct(se$part_alertee),
                           pct(se$accord),
                           format(round(se$kappa, 3), nsmall = 3))),
      htmltools::p(class = "small", sprintf(
        i18n$t("icd_cmp_egale_ha"), format(round(se$les_deux, 2), nsmall = 2),
        format(round(se$fast_seul, 2), nsmall = 2),
        format(round(se$icd_seul, 2), nsmall = 2))))
  )
}

#' @param id Module namespace id.
#' @param app_state Parent `reactiveValues` (`language`, `current_project`).
#' @param zone_id_r Reactive: the zone the ICD is computed on (`_tot`).
#' @param refresh_r Reactive bumped when a run ends.
#' @return invisible list of reactives (`obsolete`, `annee`).
#' @noRd
mod_monitoring_icd_server <- function(id, app_state, zone_id_r,
                                      refresh_r = shiny::reactive(0L)) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive(get_i18n(app_state$language %||% "fr"))

    zone_r <- shiny::reactive({
      z <- suppressWarnings(as.integer(zone_id_r()))
      if (length(z) != 1L || is.na(z)) NA_integer_ else z
    })

    # Mission 099 : avec la source THEIA, les resultats sont dans
    # `<result_dir>_theia` ; sans choix, le dossier d'avant.
    result_dir_r <- shiny::reactive({
      refresh_r()
      proj <- app_state$current_project
      z <- zone_r()
      .source_s2_dossier_methode("icd", proj$path, project_source_s2(proj$metadata),
                                 if (is.na(z)) NULL else z)
    })

    output$source_s2 <- shiny::renderUI({
      refresh_r()
      z <- zone_r()
      proj <- app_state$current_project
      if (is.null(proj) || is.na(z)) return(NULL)
      .source_s2_ligne_ui(.source_s2_info_projet("icd", proj, z), i18n_r())
    })

    annees_r <- shiny::reactive({
      refresh_r()
      rd <- result_dir_r(); z <- zone_r()
      if (is.null(rd) || is.na(z)) return(NULL)
      icd_annees_disponibles(rd, z)
    })

    # Les poids du projet ont-ils change depuis le dernier calcul ?
    obsolete_r <- shiny::reactive({
      refresh_r()
      rd <- result_dir_r(); z <- zone_r()
      if (is.null(rd) || is.na(z)) return(FALSE)
      icd_poids_obsoletes(rd, z, project_icd_poids(app_state$current_project$metadata))
    })

    output$controls <- shiny::renderUI({
      i18n <- i18n_r()
      a <- annees_r()
      if (is.null(a)) {
        return(htmltools::p(class = "text-muted small mb-0",
                            i18n$t("icd_carte_vide_corps")))
      }
      etiq <- as.character(a$annees)
      etiq[a$annees %in% a$provisoire] <- sprintf(
        "%d (%s)", a$provisoire, i18n$t("icd_provisoire"))
      choix <- stats::setNames(as.character(a$annees), etiq)
      cur <- shiny::isolate(input$annee)
      sel <- if (!is.null(cur) && cur %in% choix) cur else utils::tail(choix, 1L)
      htmltools::tagList(
        shiny::selectInput(session$ns("annee"), i18n$t("icd_annee_lbl"),
                           choices = choix, selected = sel),
        shiny::sliderInput(session$ns("opacity"), i18n$t("icd_opacite"),
                           min = 0, max = 1, value = 0.8, step = 0.05),
        htmltools::hr(),
        shiny::actionButton(session$ns("cmp_run"), i18n$t("icd_cmp_btn"),
                            icon = bsicons::bs_icon("bar-chart"),
                            class = "btn-outline-primary btn-sm w-100")
      )
    })

    raster_r <- shiny::reactive({
      refresh_r()
      rd <- result_dir_r(); z <- zone_r(); an <- input$annee
      if (is.null(rd) || is.na(z) || is.null(an) || !nzchar(an)) return(NULL)
      icd_lire_carte(z, an, rd)
    })

    output$overlay <- shiny::renderUI({
      i18n <- i18n_r()
      msg <- if (is.null(raster_r())) {
        list(t = "icd_carte_vide_titre", b = "icd_carte_vide_corps")
      } else NULL
      if (is.null(msg)) return(NULL)
      htmltools::div(
        style = paste("position: absolute; inset: 0; z-index: 500;",
                      "background: #fff; display: flex; align-items: center;",
                      "justify-content: center;"),
        htmltools::div(
          class = "p-4 text-center text-muted",
          bsicons::bs_icon("hourglass-split", class = "fs-1 d-block mx-auto mb-3"),
          htmltools::h5(class = "mt-3", i18n$t(msg$t)),
          htmltools::p(class = "mb-0", i18n$t(msg$b))))
    })

    # Poids modifies depuis le calcul : la carte reste visible (c'est celle du
    # dernier calcul), un bandeau dit de relancer.
    output$bandeau <- shiny::renderUI({
      if (!isTRUE(obsolete_r())) return(NULL)
      htmltools::div(class = "alert alert-warning py-2 mb-2",
                     bsicons::bs_icon("exclamation-triangle", class = "me-1"),
                     i18n_r()$t("icd_poids_modifies"))
    })
    shiny::outputOptions(output, "bandeau", suspendWhenHidden = FALSE)

    # Coupes rases : retirees ou non au dernier calcul (dit par le temoin).
    .note_coupes <- function() {
      rd <- result_dir_r(); z <- zone_r()
      if (is.null(rd) || is.na(z)) return(NULL)
      if (isFALSE(icd_coupes_retirees(rd, z)))
        htmltools::p(class = "small text-muted mb-0",
                     i18n_r()$t("icd_coupes_non_retirees"))
    }

    # Pixels sans ICD : combien et pourquoi.
    output$sans_icd <- shiny::renderUI({
      r <- raster_r()
      if (is.null(r)) return(NULL)
      n <- attr(r, "n_pixels_sans_icd"); tot <- attr(r, "n_pixels_zone")
      if (is.null(n) || is.na(n)) return(NULL)
      htmltools::tagList(
        htmltools::p(class = "small text-muted mt-2 mb-0",
                     sprintf(i18n_r()$t("icd_sans_icd"),
                             format(as.integer(n), big.mark = " "),
                             format(as.integer(tot), big.mark = " "))),
        .note_coupes())
    })

    output$map <- leaflet::renderLeaflet({
      proj <- app_state$current_project
      ugf <- shiny::isolate(.ugf_for_overlay(proj))
      map <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap", group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addMapPane("nemetonRaster", zIndex = 250) |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"), overlayGroups = "UGF",
          options = leaflet::layersControlOptions(collapsed = TRUE))
      if (!is.null(ugf)) {
        map <- leaflet::addPolygons(map, data = ugf, group = "UGF",
                                    color = "#1f78b4", weight = 2, opacity = 0.9,
                                    fillOpacity = 0)
        bb <- tryCatch(sf::st_bbox(ugf), error = function(e) NULL)
        if (!is.null(bb)) {
          map <- leaflet::fitBounds(map, bb[["xmin"]], bb[["ymin"]],
                                    bb[["xmax"]], bb[["ymax"]])
        }
      }
      map
    })

    # Couche du niveau + legende, redessinees par proxy.
    shiny::observe({
      r <- raster_r(); i18n <- i18n_r()
      op <- suppressWarnings(as.numeric(input$opacity %||% 0.8))
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup("icd") |> leaflet::removeControl("legend_icd")
      if (is.null(r)) return(proxy)
      cols <- .ICD_COULEURS_NIVEAUX
      pal <- leaflet::colorFactor(unname(cols), levels = as.integer(names(cols)),
                                  na.color = "transparent")
      proxy <- leaflet::addRasterImage(
        proxy, x = r, colors = pal, opacity = op, method = "ngb",
        project = TRUE, group = "icd",
        options = leaflet::gridOptions(pane = "nemetonRaster"))
      # Sans UGF pour cadrer la carte, on cadre sur l'emprise du resultat.
      if (is.null(.ugf_for_overlay(shiny::isolate(app_state$current_project)))) {
        bb <- tryCatch(
          sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(r)), 4326)),
          error = function(e) NULL)
        if (!is.null(bb)) {
          proxy <- leaflet::fitBounds(proxy, bb[["xmin"]], bb[["ymin"]],
                                      bb[["xmax"]], bb[["ymax"]])
        }
      }
      leaflet::addLegend(
        proxy, position = "bottomright", colors = unname(cols),
        labels = vapply(0:4, function(k) i18n$t(paste0("icd_niv_", k)), character(1)),
        title = i18n$t("icd_legende_titre"), opacity = op, layerId = "legend_icd")
    })

    shiny::outputOptions(output, "controls", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "overlay", suspendWhenHidden = FALSE)

    # --- Clic : modale a deux graphiques ---------------------------------
    shiny::observeEvent(input$map_click, {
      i18n <- i18n_r()
      rd <- result_dir_r(); z <- zone_r()
      if (is.null(rd) || is.na(z) || is.null(raster_r())) return()
      lat <- input$map_click$lat; lng <- input$map_click$lng
      if (is.null(lat) || is.null(lng)) return()
      s <- icd_lire_pixel(z, lng, lat, rd)
      if (is.null(s)) {
        shiny::showNotification(i18n$t("icd_pixel_hors_grille"),
                                type = "warning", duration = 8)
        return()
      }
      annee <- suppressWarnings(as.integer(input$annee))
      titre <- sprintf(i18n$t("icd_modal_titre"), round(lat, 5), round(lng, 5))
      if (is.na(s$essence)) {
        shiny::showModal(shiny::modalDialog(
          title = titre, easyClose = TRUE,
          footer = shiny::modalButton(i18n$t("close")),
          htmltools::p(i18n$t("icd_pas_icd"))))
        return()
      }
      output$icd_g1 <- plotly::renderPlotly(
        plotly::config(plot_icd_indices(s, input$valeur %||% "z", i18n_r()),
                       responsive = TRUE))
      output$icd_g2 <- plotly::renderPlotly(
        plotly::config(plot_icd_composite(s, i18n_r()), responsive = TRUE))
      entete <- icd_entete_pixel(s, annee, i18n)
      shiny::showModal(shiny::modalDialog(
        title = titre, size = "xl", easyClose = TRUE,
        footer = shiny::modalButton(i18n$t("close")),
        htmltools::div(class = "small mb-2",
                       lapply(entete, function(l) htmltools::div(l)),
                       .note_coupes()),
        shiny::radioButtons(
          session$ns("valeur"), i18n$t("icd_val_lbl"), inline = TRUE,
          choiceNames = list(i18n$t("icd_val_z"), i18n$t("icd_val_brut")),
          choiceValues = list("z", "brut"), selected = "z"),
        plotly::plotlyOutput(session$ns("icd_g1"), height = "340px"),
        plotly::plotlyOutput(session$ns("icd_g2"), height = "340px")))
    })

    # --- Comparaison avec FAST (a la demande) ----------------------------
    cmp_r <- shiny::reactiveVal(NULL)
    shiny::observeEvent(list(input$annee, zone_r(), refresh_r()),
                        cmp_r(NULL), ignoreInit = TRUE)

    shiny::observeEvent(input$cmp_run, {
      i18n <- i18n_r()
      rd <- result_dir_r(); z <- zone_r()
      proj <- app_state$current_project
      if (is.null(rd) || is.na(z) || is.null(input$annee)) return()
      con <- get_monitoring_db_connection(project = proj, read_only = TRUE)
      nid <- session$ns("icd_cmp_calcul")
      shiny::showNotification(i18n$t("icd_cmp_calcul"), id = nid,
                              type = "message", duration = NULL)
      tryCatch(
        cmp_r(icd_comparer_fast(z, input$annee, rd, con = con,
                                cache_dir = .fast_alert_cache_dir(proj$path))),
        finally = {
          shiny::removeNotification(nid)
          if (!is.null(con)) close_monitoring_db_connection(con)
        })
    })

    output$comparaison <- shiny::renderUI({
      i18n <- i18n_r()
      cmp <- cmp_r()
      if (is.null(cmp)) return(NULL)
      htmltools::div(
        class = "mt-3",
        htmltools::h6(i18n$t("icd_cmp_titre")),
        .icd_rendu_comparaison(cmp, i18n))
    })

    invisible(list(obsolete = obsolete_r, annee = shiny::reactive(input$annee),
                   comparaison = cmp_r))
  })
}

# Dispatcher des evenements de progression du calcul : `s2:*` (telechargement
# des scenes) et `icd:*` (composites, anomalies, annees, alertes). Meme contrat
# que `.reconfort_handle_progress_event()` : effets de bord seuls, `on_msg`
# alimente le chrono, les evenements tardifs d'un run arrete sont ignores et
# les evenements terminaux retirent toujours le toast.
.icd_handle_progress_event <- function(ev, session, i18n, start = NULL,
                                       on_msg = NULL, est_actif = NULL) {
  current <- as.character(ev$current %||% "")
  persistants <- c("s2:scene", "s2:scene_cached", "icd:composite",
                   "icd:anomalies", "icd:annuel", "icd:alertes")
  if (.evenement_tardif(est_actif, current, persistants)) {
    return(invisible(NULL))
  }
  msg <- switch(
    current,
    "s2:scene" = , "s2:scene_cached" = i18n$t(
      "icd_phase_scenes", n = as.integer(ev$completed %||% 0L),
      total = as.integer(ev$total %||% 0L)),
    "icd:composite" = i18n$t(
      "icd_phase_composites", n = as.integer(ev$completed %||% 0L),
      total = as.integer(ev$total %||% 0L)),
    "icd:anomalies" = i18n$t("icd_phase_anomalies"),
    "icd:annuel"    = i18n$t("icd_phase_annuel", annee = as.integer(ev$annee %||% 0L)),
    "icd:alertes"   = i18n$t("icd_phase_alertes"),
    NULL)
  if (!is.null(msg)) {
    if (is.function(on_msg)) on_msg(msg)
    shiny::showNotification(
      .monitoring_spinning_msg(msg, start),
      id = session$ns("icd_progress"), type = "message",
      duration = NULL, closeButton = FALSE)
    return(invisible(NULL))
  }
  if (current %in% c("icd:complete", "icd:cancelled", "icd:error")) {
    if (is.function(on_msg)) on_msg(NULL)
    shiny::removeNotification(session$ns("icd_progress"))
  }
  invisible(NULL)
}

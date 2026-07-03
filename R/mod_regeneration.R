# reGénération — module Shiny (spec 027, L4)
#
# Onglet de lecture de vulnérabilité climatique : exposition microclimatique ×
# stress hydrique du sol, pour prioriser les interventions de régénération.
# Pattern golem : mod_regeneration_ui(id) + mod_regeneration_server(id, app_state).
# Aucune logique métier ici — tout passe par service_regeneration (→ nemeton).

#' reGénération tab UI
#' @param id Module id.
#' @noRd
mod_regeneration_ui <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language %||% "fr")

  # Label + info tooltip (même pattern que mod_sampling / mod_synthesis).
  label_tt <- function(label, tooltip) {
    htmltools::tagList(label, " ",
      bslib::tooltip(bsicons::bs_icon("info-circle", class = "text-muted ms-1"),
                     tooltip, placement = "right"))
  }

  bslib::layout_sidebar(
    fillable = TRUE,
    sidebar = bslib::sidebar(
      id = ns("sidebar"), width = 360, open = TRUE,

      htmltools::tags$p(class = "text-muted small", i18n$t("regen_intro")),

      # --- Années de référence -------------------------------------------
      htmltools::tags$strong(i18n$t("regen_years_section")),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("year_moyenne"),
          i18n$t("regen_year_moyenne"), value = 2018, min = 1990, max = 2100, step = 1)),
        shiny::column(6, shiny::numericInput(ns("year_canicule"),
          i18n$t("regen_year_canicule"), value = 2022, min = 1990, max = 2100, step = 1))
      ),
      bslib::tooltip(
        shiny::actionButton(ns("auto_years"), i18n$t("regen_year_auto"),
          class = "btn-outline-secondary btn-sm mb-2", icon = bsicons::bs_icon("magic")),
        i18n$t("regen_year_auto_tip"), placement = "right"),
      shiny::uiOutput(ns("eobs_index_display")),

      # --- Peuplement ----------------------------------------------------
      htmltools::tags$strong(i18n$t("regen_stand_section")),
      shiny::radioButtons(ns("forest_type"), i18n$t("regen_forest_type"),
        choices = stats::setNames(c("feuillu", "resineux"),
          c(i18n$t("regen_forest_feuillu"), i18n$t("regen_forest_resineux"))),
        selected = "feuillu", inline = TRUE),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("budburst"),
          i18n$t("regen_budburst"), value = 105, min = 1, max = 200)),
        shiny::column(6, shiny::numericInput(ns("leaf_fall"),
          i18n$t("regen_leaf_fall"), value = 300, min = 200, max = 366))
      ),
      shiny::numericInput(ns("lai_max"), label_tt(i18n$t("regen_lai_max"),
          i18n$t("regen_lai_tip")),
        value = NA, min = 0, max = 12, step = 0.1),
      htmltools::tags$small(class = "text-muted d-block mb-2", i18n$t("regen_lai_auto")),

      # --- Sol -----------------------------------------------------------
      htmltools::tags$strong(i18n$t("regen_soil_section")),
      shiny::numericInput(ns("ewm"), i18n$t("regen_ewm"),
        value = 150, min = 10, max = 400, step = 5),

      # --- Forçage / résolution / essence / buffer -----------------------
      shiny::radioButtons(ns("forcing"), i18n$t("regen_forcing"),
        choices = stats::setNames(c("safran", "era5"),
          c(i18n$t("regen_forcing_safran"), i18n$t("regen_forcing_era5"))),
        selected = "safran", inline = TRUE),
      shiny::radioButtons(ns("resolution"), i18n$t("regen_resolution"),
        choices = stats::setNames(c("2", "5"),
          c(i18n$t("regen_res_2m"), i18n$t("regen_res_5m"))),
        selected = "2", inline = TRUE),
      shiny::selectInput(ns("species"), label_tt(i18n$t("regen_species_target"),
          i18n$t("regen_species_tip")),
        choices = stats::setNames("", i18n$t("regen_species_generic"))),
      shiny::numericInput(ns("buffer_km"), i18n$t("regen_buffer"),
        value = 25, min = 5, max = 100, step = 5),

      shiny::checkboxInput(ns("hydric_only"), i18n$t("regen_run_hydric_only"),
        value = FALSE),
      shiny::actionButton(ns("run"), i18n$t("regen_run"),
        class = "btn-primary w-100", icon = bsicons::bs_icon("play-fill")),

      # --- Export / persistance (sous le bouton Lancer) ------------------
      htmltools::tags$hr(class = "my-2"),
      htmltools::tags$small(class = "text-muted d-block mb-1", i18n$t("regen_results_section")),
      shiny::downloadButton(ns("export_gpkg"), i18n$t("regen_export_gpkg"),
        class = "btn-outline-primary btn-sm w-100 mb-2"),
      shiny::actionButton(ns("persist_db"), i18n$t("regen_persist_db"),
        class = "btn-outline-secondary btn-sm w-100", icon = bsicons::bs_icon("database"))
    ),

    # --- Résultats -------------------------------------------------------
    shiny::uiOutput(ns("status")),
    bslib::navset_card_tab(
      bslib::nav_panel(
        i18n$t("regen_tab_map"),
        # Sélecteur de couche : menu rétractable superposé dans la carte.
        htmltools::div(
          style = "position: relative;",
          leaflet::leafletOutput(ns("map"), height = "70vh"),
          htmltools::tags$details(
            class = "position-absolute bg-white rounded shadow-sm p-2",
            style = "top: 12px; right: 12px; z-index: 1000; max-width: 240px;",
            open = NA,
            htmltools::tags$summary(class = "small fw-semibold",
              style = "cursor: pointer;", i18n$t("regen_map_layer")),
            shiny::radioButtons(ns("map_layer"), NULL,
              choices = stats::setNames(
                c("indice_priorite_regen", "sensibilite", "njstress", "d_tmax"),
                c(i18n$t("regen_map_priorite"), i18n$t("regen_map_sensibilite"),
                  i18n$t("regen_map_njstress"), i18n$t("regen_map_dtmax"))),
              selected = "indice_priorite_regen")
          )
        )
      ),
      bslib::nav_panel(i18n$t("regen_map_context"),
        leaflet::leafletOutput(ns("context_map"), height = "70vh")),
      bslib::nav_panel(i18n$t("regen_table_section"),
        shiny::checkboxInput(ns("filter_coverage"), i18n$t("regen_filter_coverage"),
          value = TRUE),
        DT::dataTableOutput(ns("table"))),
      bslib::nav_panel(i18n$t("regen_parcel_sheet"),
        shiny::uiOutput(ns("parcel_sheet")))
    )
  )
}

#' reGénération tab server
#' @param id Module id.
#' @param app_state reactiveValues carrying `current_project` (whose
#'   `indicators_sf` holds the UGF units).
#' @noRd
mod_regeneration_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    i18n <- get_i18n(get_app_options()$language %||% "fr")

    rv <- shiny::reactiveValues(
      result = NULL, years = NULL, warnings = character(0),
      running = FALSE, eobs = NULL
    )

    # UGF units of the current project (EPSG:2154).
    units_sf <- shiny::reactive({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$indicators_sf) ||
          !inherits(project$indicators_sf, "sf") ||
          nrow(project$indicators_sf) == 0L) {
        return(NULL)
      }
      tryCatch(sf::st_transform(project$indicators_sf, 2154),
               error = function(e) project$indicators_sf)
    })

    # Populate the optional target-species selector from the core tolerance
    # table (code = value parametrisable, label = libellé affiché).
    shiny::observe({
      tol <- regeneration_species_choices()
      generic <- stats::setNames("", i18n$t("regen_species_generic"))
      if (!is.null(tol) && is.data.frame(tol) && all(c("code", "label") %in% names(tol))) {
        sp <- stats::setNames(tol$code, tol$label)
        shiny::updateSelectInput(session, "species", choices = c(generic, sp))
      }
    })

    # Auto-detect the average / heatwave years from E-OBS for this AOI.
    shiny::observeEvent(input$auto_years, {
      units <- units_sf()
      if (is.null(units)) {
        shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
        return()
      }
      yrs <- tryCatch(
        nemeton::microclimate_detect_years(aoi = units, year_window = 10),
        error = function(e) NULL)
      if (!is.null(yrs) && !is.null(yrs$year_moyenne)) {
        shiny::updateNumericInput(session, "year_moyenne", value = yrs$year_moyenne)
        shiny::updateNumericInput(session, "year_canicule", value = yrs$year_canicule)
        rv$eobs <- yrs
        shiny::showNotification(
          sprintf(i18n$t("regen_auto_done"), yrs$year_moyenne, yrs$year_canicule),
          type = "message", duration = 6)
      } else {
        shiny::showNotification(i18n$t("regen_auto_none"), type = "warning", duration = 6)
      }
    })

    output$eobs_index_display <- shiny::renderUI({
      if (is.null(rv$eobs)) return(NULL)
      htmltools::tags$small(class = "text-muted d-block mb-2",
        sprintf("%s : %s / %s", i18n$t("regen_eobs_index"),
                rv$eobs$year_moyenne %||% "?", rv$eobs$year_canicule %||% "?"))
    })

    # --- Run --------------------------------------------------------------
    shiny::observeEvent(input$run, {
      units <- units_sf()
      if (is.null(units)) {
        shiny::showNotification(i18n$t("regen_need_project"), type = "warning")
        return()
      }
      rv$running <- TRUE
      project_path <- tryCatch(app_state$current_project$path, error = function(e) NULL)
      # NA/NULL numeric inputs → NULL (let the core auto-detect / default).
      na_null <- function(x) if (is.null(x) || (length(x) == 1 && is.na(x))) NULL else x
      cfg <- list(
        year_moyenne = na_null(input$year_moyenne),
        year_canicule = na_null(input$year_canicule),
        forest_type = input$forest_type %||% "feuillu",
        lai_max = na_null(input$lai_max),
        species = if (nzchar(input$species %||% "")) input$species else NULL,
        hydric_only = isTRUE(input$hydric_only)
      )

      # withProgress affiche un overlay immédiat (retour visible avant le calcul
      # synchrone) et empêche les reclics pendant le run.
      res <- shiny::withProgress(message = i18n$t("regen_running"), value = 0.2, {
        precomputed <- load_regeneration_precomputed(project_path)
        shiny::incProgress(0.3)
        out <- tryCatch(
          run_regeneration(units, cfg = cfg, precomputed = precomputed),
          error = function(e) {
            shiny::showNotification(
              sprintf("%s: %s", i18n$t("error"), conditionMessage(e)), type = "error")
            NULL
          })
        shiny::incProgress(0.5)
        out
      })
      rv$running <- FALSE
      if (!is.null(res)) {
        rv$result <- res$units
        rv$years <- res$years
        rv$warnings <- res$warnings %||% character(0)
        nw <- length(rv$warnings)
        if (nw > 0L) {
          shiny::showNotification(sprintf(i18n$t("regen_run_done_warn"), nw),
                                  type = "warning", duration = 8)
        } else {
          shiny::showNotification(i18n$t("regen_run_done"), type = "message", duration = 5)
        }
      }
    })

    output$status <- shiny::renderUI({
      if (isTRUE(rv$running)) {
        return(htmltools::div(class = "alert alert-info py-2",
          bsicons::bs_icon("hourglass-split", class = "me-1"), i18n$t("regen_running")))
      }
      if (is.null(rv$result)) {
        return(htmltools::div(class = "text-muted small fst-italic mb-2",
                              i18n$t("regen_need_project")))
      }
      warns <- if (length(rv$warnings)) {
        htmltools::div(class = "alert alert-warning py-2 small",
          bsicons::bs_icon("exclamation-triangle", class = "me-1"),
          htmltools::tags$ul(class = "mb-0",
            lapply(rv$warnings, function(w) htmltools::tags$li(w))))
      }
      htmltools::div(
        htmltools::div(class = "alert alert-secondary py-2 small mb-2",
          bsicons::bs_icon("info-circle", class = "me-1"), i18n$t("regen_ndp_model")),
        warns
      )
    })

    # Column selected for the choropleth (guarded against absence).
    .map_col <- shiny::reactive({
      res <- rv$result
      col <- input$map_layer %||% "indice_priorite_regen"
      if (is.null(res) || !col %in% names(res)) return(NULL)
      col
    })

    output$map <- leaflet::renderLeaflet({
      res <- rv$result
      shiny::req(res)
      col <- .map_col()
      m <- leaflet::leaflet() |> leaflet::addTiles()
      if (is.null(col)) return(m)
      vals <- suppressWarnings(as.numeric(res[[col]]))
      geo <- tryCatch(sf::st_transform(res, 4326), error = function(e) res)
      if (all(is.na(vals))) {
        return(m |> leaflet::addPolygons(data = geo, weight = 1,
          color = "#888", fillOpacity = 0.2))
      }
      pal <- leaflet::colorNumeric("RdYlGn", domain = vals, na.color = "#cccccc")
      m |>
        leaflet::addPolygons(data = geo, weight = 1, color = "#333",
          fillColor = pal(vals), fillOpacity = 0.75) |>
        leaflet::addLegend(pal = pal, values = vals, position = "bottomright",
          title = i18n$t(paste0("regen_map_",
            switch(col, indice_priorite_regen = "priorite", sensibilite = "sensibilite",
                   njstress = "njstress", d_tmax = "dtmax", "priorite"))))
    })

    output$context_map <- leaflet::renderLeaflet({
      units <- units_sf()
      shiny::req(units)
      cells <- regeneration_context_eobs(
        units, precomputed = load_regeneration_precomputed(
          tryCatch(app_state$current_project$path, error = function(e) NULL)),
        buffer_m = (input$buffer_km %||% 25) * 1000)
      m <- leaflet::leaflet() |> leaflet::addTiles()
      if (is.null(cells) || !inherits(cells, "sf") || nrow(cells) == 0) return(m)
      geo <- tryCatch(sf::st_transform(cells, 4326), error = function(e) cells)
      cls <- if ("classe_bivariee" %in% names(cells)) as.numeric(cells$classe_bivariee) else rep(5, nrow(cells))
      pal <- leaflet::colorNumeric("viridis", domain = c(1, 9), na.color = "#ccc")
      m |> leaflet::addCircleMarkers(data = geo, radius = 4, stroke = FALSE,
        fillColor = pal(cls), fillOpacity = 0.8)
    })

    output$table <- DT::renderDataTable({
      res <- rv$result
      shiny::req(res)
      df <- sf::st_drop_geometry(res)
      cols <- intersect(c("ug_id", "priorite", "indice_priorite_regen", "sensibilite",
        "rang_sensibilite", "njstress", "istress", "deb_stress", "rew_min",
        "d_tmax", "d_vpd", "couverture_pct"), names(df))
      df <- df[, cols, drop = FALSE]
      if (isTRUE(input$filter_coverage) && "couverture_pct" %in% names(df)) {
        keep <- is.na(df$couverture_pct) | df$couverture_pct >= 50
        df <- df[keep, , drop = FALSE]
      }
      order_col <- if ("rang_sensibilite" %in% names(df)) "rang_sensibilite" else NULL
      DT::datatable(df, rownames = FALSE, selection = "single",
        options = list(pageLength = 15, scrollX = TRUE,
          order = if (!is.null(order_col)) list(list(which(names(df) == order_col) - 1, "asc")) else list()))
    })

    output$parcel_sheet <- shiny::renderUI({
      res <- rv$result
      if (is.null(res)) return(htmltools::div(class = "text-muted fst-italic",
                                              i18n$t("regen_need_project")))
      sel <- input$table_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        return(htmltools::div(class = "text-muted fst-italic", i18n$t("regen_select_ug")))
      }
      df <- sf::st_drop_geometry(res)
      row <- df[sel[1], , drop = FALSE]
      fields <- intersect(c("indice_priorite_regen", "sensibilite", "njstress",
        "istress", "deb_stress", "rew_min", "d_tmax", "d_vpd", "couverture_pct"), names(row))
      htmltools::tags$table(class = "table table-sm",
        lapply(fields, function(f) htmltools::tags$tr(
          htmltools::tags$th(i18n$t(paste0("regen_col_",
            switch(f, indice_priorite_regen = "indice", sensibilite = "sensibilite",
              njstress = "njstress", istress = "istress", deb_stress = "deb_stress",
              rew_min = "rew_min", d_tmax = "dtmax", d_vpd = "dvpd",
              couverture_pct = "couverture", f)))),
          htmltools::tags$td(format(row[[f]], digits = 3)))))
    })

    # --- Export GPKG (§7) -------------------------------------------------
    output$export_gpkg <- shiny::downloadHandler(
      filename = function() "regeneration.gpkg",
      content = function(file) {
        res <- rv$result
        if (is.null(res)) {
          shiny::showNotification(i18n$t("regen_export_empty"), type = "warning")
          return(invisible(NULL))
        }
        export_regeneration_geopackage(res, file)
      }
    )

    # --- Persistance versionnée en base (§6A) -----------------------------
    shiny::observeEvent(input$persist_db, {
      res <- rv$result
      if (is.null(res)) {
        shiny::showNotification(i18n$t("regen_export_empty"), type = "warning")
        return()
      }
      if (!is_db_configured()) {
        shiny::showNotification(i18n$t("regen_db_unavailable"), type = "warning")
        return()
      }
      pid <- tryCatch(app_state$current_project$id, error = function(e) NULL)
      ver <- tryCatch({
        con <- get_db_connection()
        on.exit(close_db_connection(con), add = TRUE)
        db_save_regeneration(con, pid, res)
      }, error = function(e) {
        shiny::showNotification(
          sprintf("%s: %s", i18n$t("error"), conditionMessage(e)), type = "error")
        NULL
      })
      if (!is.null(ver)) {
        shiny::showNotification(sprintf(i18n$t("regen_persisted"), ver), type = "message")
      }
    })

    list(result = shiny::reactive(rv$result))
  })
}

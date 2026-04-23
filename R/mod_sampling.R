#' Sampling Module for nemetonApp
#'
#' @description
#' Field sampling plan module. Lets the user generate sampling plots
#' over the current project's study area and download a QField project
#' (.qgz) for on-field data capture. The .qgz is produced by
#' \code{nemeton::create_qfield_project()} and bundles placettes +
#' empty arbres layers with pre-configured forms.
#'
#' First iteration uses a spatial random draw (sf::st_sample). A full
#' stratified GRTS pipeline lives in the 09-sampling tutorial and will
#' be lifted to nemeton as create_sampling_plan() in a follow-up
#' iteration.
#'
#' @name mod_sampling
#' @keywords internal
NULL


#' Sampling Module UI
#'
#' @param id Character. Module namespace ID.
#'
#' @return Shiny UI elements.
#'
#' @noRd
mod_sampling_ui <- function(id) {
  ns <- shiny::NS(id)

  opts <- get_app_options()
  lang <- opts$language %||% "fr"
  i18n <- get_i18n(lang)

  bslib::layout_sidebar(
    fillable = TRUE,

    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      width = 360,
      open = TRUE,

      htmltools::tags$h5(i18n$t("sampling_title")),
      htmltools::tags$p(class = "text-muted small", i18n$t("sampling_subtitle")),

      shiny::numericInput(ns("n_base"),  i18n$t("sampling_n_base"),
                          value = 20, min = 1, max = 1000, step = 1),
      shiny::numericInput(ns("n_over"),  i18n$t("sampling_n_over"),
                          value = 5,  min = 0, max = 1000, step = 1),
      shiny::numericInput(ns("seed"),    i18n$t("sampling_seed"),
                          value = 42,  min = 0, max = 1e6, step = 1),

      shiny::selectInput(
        ns("region"), i18n$t("sampling_region"),
        choices = tryCatch(nemeton::list_species_regions(),
                           error = function(e) c("BFC", "EU")),
        selected = "BFC"
      ),

      shiny::textInput(ns("project_name"), i18n$t("qfield_project_name"),
                       value = "echantillon"),

      shiny::actionButton(
        ns("generate"), i18n$t("sampling_generate"),
        icon = bsicons::bs_icon("crosshair"),
        class = "btn-primary w-100 mb-2"
      ),

      shiny::downloadButton(
        ns("download_qgz"), i18n$t("qfield_download"),
        icon = bsicons::bs_icon("download"),
        class = "btn-success w-100"
      ),

      htmltools::tags$p(class = "text-muted small mt-3 fst-italic",
                       i18n$t("sampling_method_note"))
    ),

    # Main area: status + map
    htmltools::tags$div(
      class = "p-2",
      shiny::uiOutput(ns("status")),
      leaflet::leafletOutput(ns("map"), height = "70vh")
    )
  )
}


#' Sampling Module Server
#'
#' @param id Character. Module namespace ID.
#' @param app_state reactiveValues. Application state carrying
#'   \code{current_project} whose \code{indicators_sf} holds the study
#'   area as a collection of polygons in Lambert-93.
#'
#' @return A list with reactive \code{sample_plots} and \code{zone_etude}.
#'
#' @noRd
mod_sampling_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sampling_rv <- shiny::reactiveValues(
      plots = NULL,  # sf POINT (Base + Over)
      zone  = NULL   # sf POLYGON (union of project geometry)
    )

    # --- Zone d'étude from current project ---------------------------
    zone_etude <- shiny::reactive({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$indicators_sf)) return(NULL)

      geom <- project$indicators_sf
      if (!inherits(geom, "sf") || nrow(geom) == 0) return(NULL)

      # Reproject to Lambert-93 and union to a single polygon
      geom <- sf::st_transform(geom, 2154)
      zone <- sf::st_union(geom)
      sf::st_sf(geometry = sf::st_sfc(zone, crs = 2154))
    })

    # --- Generate plots on click -------------------------------------
    shiny::observeEvent(input$generate, {
      i18n <- get_i18n(app_state$language %||% "fr")
      zone <- zone_etude()

      if (is.null(zone)) {
        shiny::showNotification(i18n$t("sampling_no_project"),
                                type = "warning", duration = 5)
        return()
      }

      n_base <- max(1L, as.integer(input$n_base %||% 20))
      n_over <- max(0L, as.integer(input$n_over %||% 5))
      seed   <- as.integer(input$seed %||% 42)

      set.seed(seed)
      n_total <- n_base + n_over
      pts_sfc <- sf::st_sample(zone, size = n_total, type = "random")
      # st_sample may return fewer points than requested; clamp.
      n_got <- length(pts_sfc)
      types <- c(rep("Base", min(n_base, n_got)),
                 rep("Over", max(0L, n_got - n_base)))

      plots <- sf::st_sf(
        plot_id     = sprintf("P%03d", seq_len(n_got)),
        type        = types,
        visit_order = seq_len(n_got),
        geometry    = pts_sfc
      )
      sf::st_crs(plots) <- 2154

      sampling_rv$plots <- plots
      sampling_rv$zone  <- zone

      shiny::showNotification(
        sprintf(i18n$t("sampling_generated_count"),
                n_got, sum(types == "Base"), sum(types == "Over")),
        type = "message", duration = 5
      )
    })

    # --- Status banner -----------------------------------------------
    output$status <- shiny::renderUI({
      i18n <- get_i18n(app_state$language %||% "fr")
      if (is.null(app_state$current_project)) {
        return(htmltools::div(class = "alert alert-warning",
                              i18n$t("sampling_no_project")))
      }
      plots <- sampling_rv$plots
      if (is.null(plots) || nrow(plots) == 0) {
        return(htmltools::div(class = "alert alert-info",
                              i18n$t("sampling_empty")))
      }
      n_base <- sum(plots$type == "Base")
      n_over <- sum(plots$type == "Over")
      htmltools::div(class = "alert alert-success",
                     sprintf(i18n$t("sampling_generated_count"),
                             nrow(plots), n_base, n_over))
    })

    # --- Map ---------------------------------------------------------
    output$map <- leaflet::renderLeaflet({
      base <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",  group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        )

      zone  <- sampling_rv$zone %||% zone_etude()
      plots <- sampling_rv$plots

      if (!is.null(zone)) {
        zone_ll <- sf::st_transform(zone, 4326)
        base <- leaflet::addPolygons(
          base, data = zone_ll,
          color = "#1B6B1B", weight = 2, fillColor = "#1B6B1B",
          fillOpacity = 0.08, group = "Zone"
        )
      }
      if (!is.null(plots) && nrow(plots) > 0) {
        plots_ll <- sf::st_transform(plots, 4326)
        pal <- leaflet::colorFactor(c("#1f77b4", "#ff7f0e"),
                                    domain = c("Base", "Over"))
        base <- leaflet::addCircleMarkers(
          base, data = plots_ll,
          radius = 5, weight = 1, color = "#333",
          fillColor = ~pal(type), fillOpacity = 0.9,
          label = ~sprintf("%s (%s) — ordre %d", plot_id, type, visit_order),
          group = "Placettes"
        )
        base <- leaflet::addLegend(
          base, position = "bottomright",
          pal = pal, values = c("Base", "Over"),
          title = "Placettes"
        )
      }
      base
    })

    # --- Download handler: .qgz --------------------------------------
    output$download_qgz <- shiny::downloadHandler(
      filename = function() {
        nm <- gsub("[^a-zA-Z0-9_-]", "_", input$project_name %||% "echantillon")
        paste0(nm, ".qgz")
      },
      content = function(file) {
        i18n <- get_i18n(app_state$language %||% "fr")
        plots <- sampling_rv$plots
        zone  <- sampling_rv$zone %||% zone_etude()

        if (is.null(plots) || nrow(plots) == 0) {
          shiny::showNotification(i18n$t("sampling_empty"),
                                  type = "warning", duration = 5)
          writeLines("No plots generated yet.", file)
          return()
        }

        td <- tempfile("qfield_stage_")
        dir.create(td, recursive = TRUE)
        on.exit(unlink(td, recursive = TRUE), add = TRUE)

        nm <- gsub("[^a-zA-Z0-9_-]", "_",
                   input$project_name %||% "echantillon")
        qgz <- nemeton::create_qfield_project(
          placettes    = plots,
          zone_etude   = zone,
          output_dir   = td,
          project_name = nm,
          region       = input$region %||% "BFC",
          lang         = app_state$language %||% "fr"
        )
        file.copy(qgz, file, overwrite = TRUE)

        shiny::showNotification(i18n$t("qfield_ready"),
                                type = "message", duration = 5)
      },
      contentType = "application/zip"
    )

    # --- Return (useful for tests / consumers) -----------------------
    list(
      sample_plots = shiny::reactive(sampling_rv$plots),
      zone_etude   = zone_etude
    )
  })
}

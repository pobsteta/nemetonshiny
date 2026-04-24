#' Field Ingest Module for nemetonApp
#'
#' @description
#' QField return path (E5.b). Takes a GeoPackage produced by a QField
#' field session, validates it against the placette / arbre schemas,
#' aggregates per-plot dendrometric metrics and attaches them to the
#' project's forest units. Persisting the field data bumps the NDP
#' level (0 -> 2 with placettes only, 0 -> 3 once trees are captured
#' at a sufficient density) via \code{nemeton::detect_ndp()}.
#'
#' This MVP persists the validated field data and the new NDP level
#' on the project, but does \emph{not} rerun \code{compute_all_indicators}.
#' The user triggers a recompute from the Home tab afterwards; the
#' indicators that consume field aggregates (P1, P2, B2, C1, R2) will
#' then pick them up through the project layers.
#'
#' @name mod_field_ingest
#' @keywords internal
NULL


# ---- UI --------------------------------------------------------------

#' Field Ingest Module UI
#'
#' @param id Character. Module namespace ID.
#'
#' @return Shiny UI elements.
#' @noRd
mod_field_ingest_ui <- function(id) {
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

      # Collapsible card — mirrors the "Informations projet" accordion
      # (mod_project.R) and the sampling form accordion (mod_sampling.R).
      htmltools::tags$div(
        class = "card mb-3",
        htmltools::tags$div(
          class = "card-header bg-success text-white py-2",
          style = "cursor: pointer;",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", ns("ingest_collapse")),
          `aria-expanded` = "true",
          `aria-controls` = ns("ingest_collapse"),
          htmltools::div(
            class = "d-flex align-items-center justify-content-between",
            htmltools::div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("upload", class = "me-2"),
              i18n$t("field_ingest_accordion_title")
            ),
            bsicons::bs_icon("chevron-down", class = "collapse-icon")
          )
        ),
        htmltools::tags$div(
          id = ns("ingest_collapse"),
          class = "collapse show",
          htmltools::tags$div(
            class = "card-body",

            htmltools::tags$p(class = "text-muted small",
                              i18n$t("field_ingest_subtitle")),

            shiny::fileInput(
              ns("gpkg"),
              i18n$t("field_ingest_upload"),
              accept = c(".gpkg", "application/geopackage+sqlite3",
                         "application/octet-stream"),
              buttonLabel = i18n$t("field_ingest_browse"),
              placeholder  = i18n$t("field_ingest_placeholder")
            ),

            shiny::selectInput(
              ns("region"), i18n$t("sampling_region"),
              choices = tryCatch(nemeton::list_species_regions(),
                                 error = function(e) c("BFC", "EU")),
              selected = "BFC"
            ),

            shiny::actionButton(
              ns("validate"), i18n$t("field_ingest_validate"),
              icon = bsicons::bs_icon("check2-circle"),
              class = "btn-primary w-100 mb-2"
            ),

            shiny::actionButton(
              ns("attach"), i18n$t("field_ingest_attach"),
              icon = bsicons::bs_icon("link-45deg"),
              class = "btn-success w-100"
            ),

            htmltools::tags$p(class = "text-muted small mt-3 fst-italic",
                              i18n$t("field_ingest_note"))
          )
        )
      )
    ),

    # Main area: status, NDP before/after, validation report, map
    htmltools::tags$div(
      class = "p-2",
      shiny::uiOutput(ns("status")),
      shiny::uiOutput(ns("ndp_cards")),
      shiny::uiOutput(ns("report")),
      leaflet::leafletOutput(ns("map"), height = "55vh")
    )
  )
}


# ---- Server ----------------------------------------------------------

#' Field Ingest Module Server
#'
#' @param id Character. Module namespace ID.
#' @param app_state reactiveValues. Application state carrying
#'   \code{current_project} and \code{language}.
#'
#' @return A list of reactives (import result, validation, attached
#'   units) exposed for testability.
#'
#' @noRd
mod_field_ingest_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    field_rv <- shiny::reactiveValues(
      import     = NULL,   # list(placettes, arbres)
      validation = NULL,   # list(ok, errors, warnings)
      field_agg  = NULL,   # sf placettes + field_* columns
      attached   = NULL,   # units sf + field_* columns (in-memory preview)
      new_ndp    = NULL    # integer, computed post-attach
    )

    # --- Helpers ---------------------------------------------------------
    current_units_sf <- shiny::reactive({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$indicators_sf)) return(NULL)
      sf <- project$indicators_sf
      if (!inherits(sf, "sf") || nrow(sf) == 0) return(NULL)
      sf::st_transform(sf, 2154)
    })

    current_ndp <- shiny::reactive({
      as.integer(app_state$current_project$metadata$ndp_level %||% 0L)
    })

    # --- Validate on click ---------------------------------------------
    shiny::observeEvent(input$validate, {
      i18n <- get_i18n(app_state$language %||% "fr")
      up <- input$gpkg
      if (is.null(up) || nrow(up) == 0) {
        shiny::showNotification(i18n$t("field_ingest_no_file"),
                                type = "warning", duration = 5)
        return()
      }

      imported <- tryCatch(
        nemeton::import_qfield_gpkg(up$datapath),
        error = function(e) {
          shiny::showNotification(
            paste("import_qfield_gpkg():", conditionMessage(e)),
            type = "error", duration = 8
          )
          NULL
        }
      )
      if (is.null(imported)) return()

      field_rv$import <- imported

      val <- nemeton::validate_field_data(
        placettes = imported$placettes,
        arbres    = imported$arbres,
        region    = input$region %||% "BFC",
        lang      = app_state$language %||% "fr"
      )
      field_rv$validation <- val

      if (val$ok) {
        # Pre-compute aggregates so the map can show them immediately.
        field_rv$field_agg <- tryCatch(
          nemeton::aggregate_plot_metrics(
            placettes = imported$placettes,
            arbres    = imported$arbres
          ),
          error = function(e) NULL
        )
        shiny::showNotification(
          sprintf(i18n$t("field_ingest_validated"),
                  nrow(imported$placettes),
                  if (!is.null(imported$arbres)) nrow(imported$arbres) else 0L),
          type = "message", duration = 5
        )
      } else {
        field_rv$field_agg <- NULL
        shiny::showNotification(
          sprintf(i18n$t("field_ingest_errors"), nrow(val$errors)),
          type = "error", duration = 8
        )
      }
    })

    # --- Attach on click -----------------------------------------------
    shiny::observeEvent(input$attach, {
      i18n <- get_i18n(app_state$language %||% "fr")
      project <- app_state$current_project
      if (is.null(project)) {
        shiny::showNotification(i18n$t("field_ingest_no_project"),
                                type = "warning", duration = 5)
        return()
      }
      if (is.null(field_rv$validation) || !isTRUE(field_rv$validation$ok)) {
        shiny::showNotification(i18n$t("field_ingest_not_validated"),
                                type = "warning", duration = 5)
        return()
      }

      units <- current_units_sf()
      if (is.null(units)) {
        shiny::showNotification(i18n$t("field_ingest_no_units"),
                                type = "warning", duration = 5)
        return()
      }

      agg <- field_rv$field_agg %||% nemeton::aggregate_plot_metrics(
        placettes = field_rv$import$placettes,
        arbres    = field_rv$import$arbres
      )

      attached <- tryCatch(
        nemeton::attach_field_data_to_units(units, agg),
        error = function(e) {
          shiny::showNotification(
            paste("attach_field_data_to_units():", conditionMessage(e)),
            type = "error", duration = 8
          )
          NULL
        }
      )
      if (is.null(attached)) return()
      field_rv$attached <- attached

      # Tag + recompute NDP along the alternative field path.
      tagged <- nemeton::tag_field_data_sources(
        attached,
        placettes = field_rv$import$placettes,
        arbres    = field_rv$import$arbres
      )
      new_ndp <- as.integer(nemeton::detect_ndp(tagged))
      field_rv$new_ndp <- new_ndp

      # Persist the GPKG and metadata bump.
      project_path <- tryCatch(get_project_path(project$id),
                               error = function(e) NULL)
      if (is.null(project_path)) {
        shiny::showNotification(i18n$t("field_ingest_persist_failed"),
                                type = "error", duration = 8)
        return()
      }

      persisted <- tryCatch({
        data_dir <- file.path(project_path, "data")
        if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
        dest <- file.path(data_dir, "field_data.gpkg")
        file.copy(input$gpkg$datapath, dest, overwrite = TRUE)

        update_project_metadata(project$id, list(
          ndp_level = new_ndp,
          field_data_imported_at = Sys.time(),
          field_data_plots = nrow(field_rv$import$placettes),
          field_data_trees = if (!is.null(field_rv$import$arbres))
            nrow(field_rv$import$arbres) else 0L
        ))
        TRUE
      }, error = function(e) {
        shiny::showNotification(
          paste("persist:", conditionMessage(e)),
          type = "error", duration = 8
        )
        FALSE
      })
      if (!isTRUE(persisted)) return()

      # Reload project so downstream modules see the new NDP.
      app_state$current_project <- load_project(project$id)

      shiny::showNotification(
        sprintf(i18n$t("field_ingest_attached"), new_ndp),
        type = "message", duration = 6
      )
    })

    # --- Status banner -------------------------------------------------
    output$status <- shiny::renderUI({
      i18n <- get_i18n(app_state$language %||% "fr")
      if (is.null(app_state$current_project)) {
        return(htmltools::div(class = "alert alert-warning",
                              i18n$t("field_ingest_no_project")))
      }
      if (is.null(field_rv$import)) {
        return(htmltools::div(class = "alert alert-info",
                              i18n$t("field_ingest_waiting")))
      }
      p <- field_rv$import$placettes
      a <- field_rv$import$arbres
      htmltools::div(class = "alert alert-light",
                     sprintf(i18n$t("field_ingest_loaded"),
                             nrow(p),
                             if (!is.null(a)) nrow(a) else 0L))
    })

    # --- NDP before/after cards ---------------------------------------
    output$ndp_cards <- shiny::renderUI({
      i18n <- get_i18n(app_state$language %||% "fr")
      if (is.null(app_state$current_project)) return(NULL)
      lang <- app_state$language %||% "fr"

      before <- current_ndp()
      after  <- field_rv$new_ndp %||% before

      htmltools::div(
        class = "d-flex gap-3 mb-2",
        htmltools::div(
          class = "card p-2 flex-fill",
          htmltools::tags$small(class = "text-muted",
                                i18n$t("field_ingest_ndp_before")),
          htmltools::br(),
          ndp_badge(before, lang),
          ndp_progress_bar(before, lang)
        ),
        htmltools::div(
          class = "card p-2 flex-fill",
          htmltools::tags$small(class = "text-muted",
                                i18n$t("field_ingest_ndp_after")),
          htmltools::br(),
          ndp_badge(after, lang),
          ndp_progress_bar(after, lang)
        )
      )
    })

    # --- Validation report --------------------------------------------
    output$report <- shiny::renderUI({
      i18n <- get_i18n(app_state$language %||% "fr")
      val <- field_rv$validation
      if (is.null(val)) return(NULL)

      n_err  <- nrow(val$errors)
      n_warn <- nrow(val$warnings)
      class  <- if (val$ok && n_warn == 0) "alert alert-success"
                else if (val$ok) "alert alert-warning"
                else "alert alert-danger"

      render_table <- function(df, title) {
        if (nrow(df) == 0) return(NULL)
        htmltools::tagList(
          htmltools::tags$h6(title),
          htmltools::tags$div(
            class = "table-responsive",
            style = "max-height: 200px;",
            htmltools::tags$table(
              class = "table table-sm",
              htmltools::tags$thead(htmltools::tags$tr(
                lapply(colnames(df), htmltools::tags$th)
              )),
              htmltools::tags$tbody(
                lapply(seq_len(nrow(df)), function(i) {
                  htmltools::tags$tr(
                    lapply(df[i, , drop = TRUE], function(v) {
                      htmltools::tags$td(as.character(v))
                    })
                  )
                })
              )
            )
          )
        )
      }

      htmltools::div(
        class = class,
        htmltools::tags$strong(
          sprintf(i18n$t("field_ingest_report_header"), n_err, n_warn)
        ),
        render_table(val$errors, i18n$t("field_ingest_report_errors")),
        render_table(val$warnings, i18n$t("field_ingest_report_warnings"))
      )
    })

    # --- Map -----------------------------------------------------------
    output$map <- leaflet::renderLeaflet({
      base <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",  group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          options = leaflet::layersControlOptions(collapsed = TRUE)
        )

      units <- current_units_sf()
      if (!is.null(units)) {
        units_ll <- sf::st_transform(units, 4326)
        base <- leaflet::addPolygons(
          base, data = units_ll,
          color = "#1B6B1B", weight = 1, fillColor = "#1B6B1B",
          fillOpacity = 0.06, group = "Unites"
        )
      }

      placettes <- field_rv$import$placettes
      if (!is.null(placettes) && nrow(placettes) > 0) {
        p_ll <- sf::st_transform(placettes, 4326)
        base <- leaflet::addCircleMarkers(
          base, data = p_ll,
          radius = 5, weight = 1, color = "#333",
          fillColor = "#1f77b4", fillOpacity = 0.9,
          label = ~as.character(plot_id),
          group = "Placettes"
        )
      }

      arbres <- field_rv$import$arbres
      if (!is.null(arbres) && inherits(arbres, "sf") && nrow(arbres) > 0) {
        a_ll <- sf::st_transform(arbres, 4326)
        base <- leaflet::addCircleMarkers(
          base, data = a_ll,
          radius = 2, weight = 0.5, color = "#8B4513",
          fillColor = "#8B4513", fillOpacity = 0.6,
          group = "Arbres"
        )
      }
      base
    })

    # --- Return reactives (useful for tests) --------------------------
    list(
      import     = shiny::reactive(field_rv$import),
      validation = shiny::reactive(field_rv$validation),
      field_agg  = shiny::reactive(field_rv$field_agg),
      attached   = shiny::reactive(field_rv$attached),
      new_ndp    = shiny::reactive(field_rv$new_ndp)
    )
  })
}

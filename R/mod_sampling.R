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

      # Collapsible card — mirrors the "Informations projet" accordion
      # in the Selection tab (see mod_project.R).
      htmltools::tags$div(
        class = "card mb-3",
        htmltools::tags$div(
          class = "card-header bg-success text-white py-2",
          style = "cursor: pointer;",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", ns("sampling_collapse")),
          `aria-expanded` = "true",
          `aria-controls` = ns("sampling_collapse"),
          htmltools::div(
            class = "d-flex align-items-center justify-content-between",
            htmltools::div(
              class = "d-flex align-items-center",
              bsicons::bs_icon("crosshair", class = "me-2"),
              i18n$t("sampling_title")
            ),
            bsicons::bs_icon("chevron-down", class = "collapse-icon")
          )
        ),
        htmltools::tags$div(
          id = ns("sampling_collapse"),
          class = "collapse show",
          htmltools::tags$div(
            class = "card-body",

            htmltools::tags$p(class = "text-muted small",
                              i18n$t("sampling_subtitle")),

            shiny::radioButtons(
              ns("sizing_mode"), i18n$t("sampling_sizing_mode"),
              choices = stats::setNames(
                c("fixed", "target_error"),
                c(i18n$t("sampling_mode_fixed"),
                  i18n$t("sampling_mode_error"))
              ),
              selected = "fixed",
              inline = TRUE
            ),

            # --- Mode: fixed size ---------------------------------------
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'fixed'", ns("sizing_mode")),
              shiny::numericInput(ns("n_base"), i18n$t("sampling_n_base"),
                                  value = 20, min = 1, max = 10000, step = 1),
              shiny::numericInput(ns("n_over"), i18n$t("sampling_n_over"),
                                  value = 5,  min = 0, max = 10000, step = 1)
            ),

            # --- Mode: target error + CV --------------------------------
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'target_error'", ns("sizing_mode")),
              shiny::numericInput(ns("target_error_pct"),
                                  i18n$t("sampling_target_error_label"),
                                  value = 10, min = 1, max = 50, step = 1),
              shiny::numericInput(ns("alpha_pct"),
                                  i18n$t("sampling_alpha_label"),
                                  value = 5, min = 1, max = 20, step = 1),
              shiny::numericInput(ns("over_ratio_pct"),
                                  i18n$t("sampling_over_ratio_label"),
                                  value = 20, min = 0, max = 100, step = 5),

              shiny::radioButtons(
                ns("cv_source"), i18n$t("sampling_cv_source_label"),
                choices = stats::setNames(
                  c("manual", "bdforet"),
                  c(i18n$t("sampling_cv_source_manual"),
                    i18n$t("sampling_cv_source_bdforet"))
                ),
                selected = "manual",
                inline = TRUE
              ),

              # CV manual path
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'manual'", ns("cv_source")),
                shiny::numericInput(ns("cv_pct"),
                                    i18n$t("sampling_cv_manual_label"),
                                    value = 35, min = 1, max = 200, step = 1)
              ),

              # CV from BD Forêt v2
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'bdforet'", ns("cv_source")),
                shiny::selectInput(
                  ns("cv_position"), i18n$t("sampling_cv_position"),
                  choices = stats::setNames(
                    c("low", "mid", "high"),
                    c(i18n$t("sampling_cv_position_low"),
                      i18n$t("sampling_cv_position_mid"),
                      i18n$t("sampling_cv_position_high"))
                  ),
                  selected = "mid"
                ),
                shiny::actionButton(
                  ns("compute_cv"), i18n$t("sampling_cv_compute"),
                  icon = bsicons::bs_icon("calculator"),
                  class = "btn-outline-secondary w-100 mb-2"
                ),
                htmltools::tags$p(class = "text-muted small fst-italic",
                                  i18n$t("sampling_cv_bdforet_hint"))
              ),

              shiny::uiOutput(ns("sizing_report"))
            ),

            shiny::numericInput(ns("seed"), i18n$t("sampling_seed"),
                                value = 42, min = 0, max = 1e6, step = 1),

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
          )
        )
      )
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
      plots      = NULL,  # sf POINT (Base + Over)
      zone       = NULL,  # sf POLYGON (union of project geometry)
      cv_result  = NULL,  # output of nemeton::cv_from_bdforet()
      size_info  = NULL   # output of nemeton::compute_sample_size()
    )

    # --- Default the QField project_name to the current project -----
    # Keeps the exported .qgz filename in sync with the project the
    # user is working on (falls back to "echantillon" when no project
    # is active). Users can still overwrite the field manually.
    default_project_name <- shiny::reactive({
      project <- app_state$current_project
      nm <- if (!is.null(project)) {
        project$metadata$name %||% project$id %||% "echantillon"
      } else {
        "echantillon"
      }
      gsub("[^a-zA-Z0-9_-]", "_", nm)
    })

    shiny::observe({
      shiny::updateTextInput(
        session, "project_name",
        value = default_project_name()
      )
    })

    # --- UGF layer from current project (per-unit polygons, 2154) ----
    # Used by the map (one polygon per UGF, matches the Import terrain
    # sub-tab style). The zone consumed by create_sampling_plan() is
    # the union of these, computed separately in zone_etude().
    units_sf <- shiny::reactive({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$indicators_sf)) return(NULL)
      sf <- project$indicators_sf
      if (!inherits(sf, "sf") || nrow(sf) == 0) return(NULL)
      sf::st_transform(sf, 2154)
    })

    # --- Zone d'étude from current project ---------------------------
    zone_etude <- shiny::reactive({
      units <- units_sf()
      if (is.null(units)) return(NULL)
      zone <- sf::st_union(units)
      sf::st_sf(geometry = sf::st_sfc(zone, crs = 2154))
    })

    # --- Helpers for the target_error path ---------------------------
    # Return the current CV as a fraction (from manual input or from
    # the cached cv_from_bdforet result), or NULL when not available.
    current_cv <- shiny::reactive({
      src <- input$cv_source %||% "manual"
      if (src == "manual") {
        cv_pct <- input$cv_pct %||% 35
        if (is.null(cv_pct) || is.na(cv_pct) || cv_pct <= 0) return(NULL)
        cv_pct / 100
      } else {
        sampling_rv$cv_result$cv
      }
    })

    # Derive (n_base, n_over) from the selected sizing mode. Returns
    # NULL when nothing is ready yet (e.g. target_error mode with no
    # CV available), letting the generate observer bail cleanly.
    sized_inputs <- shiny::reactive({
      mode <- input$sizing_mode %||% "fixed"
      if (mode == "fixed") {
        list(
          n_base = max(1L, as.integer(input$n_base %||% 20)),
          n_over = max(0L, as.integer(input$n_over %||% 5)),
          source = "fixed",
          size_info = NULL
        )
      } else {
        cv <- current_cv()
        if (is.null(cv)) return(NULL)
        target <- (input$target_error_pct %||% 10) / 100
        alpha  <- (input$alpha_pct %||% 5) / 100
        over_r <- (input$over_ratio_pct %||% 20) / 100
        res <- tryCatch(
          nemeton::compute_sample_size(cv = cv, target_error = target,
                                       alpha = alpha),
          error = function(e) NULL
        )
        if (is.null(res)) return(NULL)
        list(
          n_base = res$n,
          n_over = as.integer(ceiling(res$n * over_r)),
          source = "target_error",
          size_info = res
        )
      }
    })

    # --- Compute CV from BD Forêt v2 cache ---------------------------
    # We reuse the cache populated by service_compute during a project
    # compute run: <project_path>/cache/layers/bdforet.gpkg. When
    # absent, surface a friendly notification pointing to the manual
    # CV mode.
    shiny::observeEvent(input$compute_cv, {
      i18n <- get_i18n(app_state$language %||% "fr")
      project <- app_state$current_project
      if (is.null(project)) {
        shiny::showNotification(i18n$t("sampling_no_project"),
                                type = "warning", duration = 5)
        return()
      }
      project_path <- tryCatch(get_project_path(project$id),
                               error = function(e) NULL)
      if (is.null(project_path)) {
        shiny::showNotification(i18n$t("sampling_cv_bdforet_missing"),
                                type = "warning", duration = 6)
        return()
      }
      gpkg <- file.path(project_path, "cache", "layers", "bdforet.gpkg")
      if (!file.exists(gpkg)) {
        shiny::showNotification(i18n$t("sampling_cv_bdforet_missing"),
                                type = "warning", duration = 6)
        return()
      }

      bd <- tryCatch(
        sf::st_read(gpkg, quiet = TRUE),
        error = function(e) NULL
      )
      if (is.null(bd) || nrow(bd) == 0) {
        shiny::showNotification(i18n$t("sampling_cv_bdforet_missing"),
                                type = "warning", duration = 6)
        return()
      }
      # Detect the TFV column (IGN WFS varies: TFV / tfv / CODE_TFV /
      # code_tfv; some older exports only expose essence / ESSENCE).
      tfv_candidates <- c("TFV", "tfv", "CODE_TFV", "code_tfv",
                          "essence", "ESSENCE", "LIB_FV", "LIBELLE")
      tfv_col <- intersect(tfv_candidates, names(bd))[1]
      if (is.na(tfv_col)) {
        shiny::showNotification(
          sprintf("TFV column not found. Columns present: %s",
                  paste(head(names(bd), 10), collapse = ", ")),
          type = "error", duration = 10
        )
        return()
      }

      aoi <- zone_etude()
      res <- tryCatch(
        nemeton::cv_from_bdforet(
          bdforet_sf = sf::st_transform(bd, 2154),
          position   = input$cv_position %||% "mid",
          aoi        = aoi,
          tfv_col    = tfv_col
        ),
        error = function(e) {
          shiny::showNotification(
            paste("cv_from_bdforet():", conditionMessage(e)),
            type = "error", duration = 8
          )
          NULL
        }
      )
      if (is.null(res)) return()
      sampling_rv$cv_result <- res

      # Diagnostic path: if no polygon was mappable, surface the
      # actual column used + a sample of unmapped values so the user
      # can see the mismatch (common cause: IGN WFS returns the label
      # libellé_tfv instead of the code, or codes formatted without
      # the hyphens).
      if (is.na(res$cv) || isTRUE(res$coverage == 0)) {
        sample_vals <- head(unique(as.character(res$unmapped)), 5)
        shiny::showNotification(
          sprintf(
            paste0("CV non calculable. Colonne utilisée : %s. ",
                   "Codes non mappés (%d uniques) : %s."),
            tfv_col,
            length(unique(res$unmapped)),
            if (length(sample_vals)) paste(sample_vals, collapse = ", ")
            else "(aucune valeur)"
          ),
          type = "warning", duration = 12
        )
        return()
      }

      cv_pct <- round(res$cv * 100, 1)
      cov_pct <- round((res$coverage %||% NA) * 100, 1)
      shiny::showNotification(
        sprintf(i18n$t("sampling_cv_computed"), cv_pct, cov_pct),
        type = "message", duration = 6
      )
    })

    # --- Sizing report shown below the target_error inputs -----------
    output$sizing_report <- shiny::renderUI({
      i18n <- get_i18n(app_state$language %||% "fr")
      if ((input$sizing_mode %||% "fixed") != "target_error") return(NULL)

      cv <- current_cv()
      if (is.null(cv)) {
        return(htmltools::div(class = "alert alert-info py-2 mb-2",
                              i18n$t("sampling_n_computed_missing")))
      }
      target <- (input$target_error_pct %||% 10) / 100
      alpha  <- (input$alpha_pct %||% 5) / 100
      res <- tryCatch(
        nemeton::compute_sample_size(cv = cv, target_error = target,
                                     alpha = alpha),
        error = function(e) NULL
      )
      sampling_rv$size_info <- res
      if (is.null(res)) return(NULL)

      parts <- list(
        htmltools::div(
          class = "alert alert-secondary py-2 mb-2",
          sprintf(i18n$t("sampling_n_computed"),
                  res$n, res$t_used, res$df)
        )
      )

      cv_res <- sampling_rv$cv_result
      if (!is.null(cv_res) && (input$cv_source %||% "manual") == "bdforet") {
        cv_pct <- round(cv_res$cv * 100, 1)
        cov_pct <- round((cv_res$coverage %||% NA) * 100, 1)
        parts <- c(parts, list(htmltools::div(
          class = "alert alert-light py-2 mb-2",
          sprintf(i18n$t("sampling_cv_computed"), cv_pct, cov_pct)
        )))
        n_amb <- nrow(cv_res$ambiguous %||% data.frame())
        if (n_amb > 0L) {
          parts <- c(parts, list(htmltools::div(
            class = "alert alert-warning py-2 mb-2",
            sprintf(i18n$t("sampling_cv_ambiguous"), n_amb)
          )))
        }
        n_unm <- length(cv_res$unmapped %||% character(0))
        if (n_unm > 0L) {
          parts <- c(parts, list(htmltools::div(
            class = "alert alert-warning py-2 mb-2",
            sprintf(i18n$t("sampling_cv_unmapped"), n_unm)
          )))
        }
      }
      htmltools::tagList(parts)
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

      sized <- sized_inputs()
      if (is.null(sized)) {
        shiny::showNotification(i18n$t("sampling_n_computed_missing"),
                                type = "warning", duration = 5)
        return()
      }
      n_base <- sized$n_base
      n_over <- sized$n_over
      seed   <- as.integer(input$seed %||% 42)

      plots <- tryCatch(
        nemeton::create_sampling_plan(
          zone    = zone,
          n_base  = n_base,
          n_over  = n_over,
          seed    = seed
        ),
        error = function(e) {
          shiny::showNotification(
            paste("create_sampling_plan():", conditionMessage(e)),
            type = "error", duration = 8
          )
          NULL
        }
      )
      if (is.null(plots)) return()

      sampling_rv$plots <- plots
      sampling_rv$zone  <- zone

      method <- attr(plots, "method") %||% "random"
      shiny::showNotification(
        sprintf("%s (%s)",
                sprintf(i18n$t("sampling_generated_count"),
                        nrow(plots), sum(plots$type == "Base"),
                        sum(plots$type == "Over")),
                toupper(method)),
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

      units <- units_sf()
      plots <- sampling_rv$plots

      if (!is.null(units)) {
        units_ll <- sf::st_transform(units, 4326)
        base <- leaflet::addPolygons(
          base, data = units_ll,
          color = "#1B6B1B", weight = 1, fillColor = "#1B6B1B",
          fillOpacity = 0.06, group = "Unites"
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
      sample_plots         = shiny::reactive(sampling_rv$plots),
      zone_etude           = zone_etude,
      default_project_name = default_project_name
    )
  })
}

# mod_validation_sampling.R — Spec 014 phase B (sous-onglet UI).
#
# Cinquième sous-onglet de Suivi sanitaire : « Plan de validation ».
# Toujours visible quand une `monitoring_zone_id` est posée (pas
# mode-driven comme les couples FAST / FORDEAD). L'utilisateur choisit
# la source d'alerte (FORDEAD ou FAST) et génère un plan
# d'échantillonnage de validation terrain via le cœur
# `nemeton::create_validation_sampling_plan()`, encapsulé côté app par
# `generate_validation_plan()`.
#
# Sortie : carte Leaflet (raster d'alerte + markers Validation / Témoin),
# table DT du plan, bouton « Persister dans samples.gpkg » et bouton
# « Exporter pour QGIS » qui produit un .qgz dans <project>/exports/.


#' Plan de validation sub-tab UI
#'
#' @param id Module namespace id.
#' @noRd
mod_validation_sampling_ui <- function(id) {
  ns <- shiny::NS(id)
  i18n <- get_i18n("fr")
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      title = i18n$t("validation_sampling_title"),

      shiny::radioButtons(
        ns("source"),
        label  = i18n$t("validation_source_label"),
        inline = TRUE,
        choices = c("FORDEAD" = "FORDEAD", "FAST" = "FAST"),
        selected = "FORDEAD"
      ),
      shiny::numericInput(
        ns("n_validation"),
        label = i18n$t("validation_n_validation_label"),
        value = 20L, min = 1L, max = 200L, step = 1L
      ),
      shiny::numericInput(
        ns("n_control"),
        label = i18n$t("validation_n_control_label"),
        value = 5L, min = 0L, max = 100L, step = 1L
      ),
      shiny::checkboxGroupInput(
        ns("classes"),
        label   = i18n$t("validation_classes_label"),
        choices = stats::setNames(
          c("3", "4", "1", "2"),
          c("3 — forte", "4 — sol nu",
            "1 — faible (moins fiable)", "2 — moyenne (moins fiable)")
        ),
        selected = c("3", "4"),
        inline = FALSE
      ),
      shiny::numericInput(
        ns("buffer_m"),
        label = i18n$t("validation_buffer_label"),
        value = 0, min = 0, max = 100, step = 5
      ),
      shiny::numericInput(
        ns("seed"),
        label = i18n$t("validation_seed_label"),
        value = 42L, min = 0L, max = 1e6L, step = 1L
      ),
      shiny::actionButton(
        ns("generate"),
        label = i18n$t("validation_generate_btn"),
        icon  = shiny::icon("play"),
        class = "btn-primary mt-2"
      ),
      shiny::hr(),
      shiny::actionButton(
        ns("persist"),
        label = i18n$t("validation_persist_btn"),
        icon  = shiny::icon("save"),
        class = "btn-outline-secondary",
        disabled = NA
      ),
      shiny::downloadButton(
        ns("export_qgis"),
        label = i18n$t("validation_export_qgis_btn"),
        icon  = shiny::icon("file-export"),
        class = "btn-outline-secondary",
        disabled = NA
      )
    ),
    shiny::uiOutput(ns("panel")),
    shiny::uiOutput(ns("table_panel"))
  )
}


#' Plan de validation sub-tab server
#'
#' @param id Module namespace id.
#' @param app_state Parent reactiveValues with `language`,
#'   `current_project`.
#' @param zone_id_r Reactive returning the active monitoring zone id.
#' @param mode_r Reactive returning the parent sidebar mode
#'   (`"quick"` / `"health"`). Drives the default `source` radio.
#' @param thresholds_r Reactive returning the FAST sidebar thresholds
#'   `list(ndvi, nbr, window_days)`.
#' @param date_range_r Reactive returning the parent date range.
#' @noRd
mod_validation_sampling_server <- function(id, app_state,
                                           zone_id_r,
                                           mode_r,
                                           thresholds_r,
                                           date_range_r) {
  shiny::moduleServer(id, function(input, output, session) {

    i18n_r <- shiny::reactive({
      get_i18n(app_state$language %||% "fr")
    })

    # Default the source radio to whatever the parent sidebar mode is
    # at first sub-tab open. After that the user owns the choice — the
    # observer only fires once per parent-mode flip.
    last_mode <- shiny::reactiveVal(NULL)
    shiny::observe({
      m <- mode_r()
      if (is.null(m) || identical(m, last_mode())) return()
      last_mode(m)
      target <- if (identical(m, "quick")) "FAST" else "FORDEAD"
      if (!identical(input$source, target)) {
        shiny::updateRadioButtons(session, "source", selected = target)
      }
    })

    # The plan reactive : only fires on the Generate button. eventReactive
    # makes input changes invalidate the value silently — the user must
    # re-click Generate to get a fresh plan.
    plan_rv <- shiny::reactiveVal(NULL)
    plan_error <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$generate, {
      plan_rv(NULL)
      plan_error(NULL)
      proj <- app_state$current_project
      if (is.null(proj)) return()
      con <- get_monitoring_db_connection(project = proj)
      if (is.null(con)) {
        plan_error(list(class = "validation_no_db",
                        message = "Monitoring DB not configured"))
        return()
      }
      on.exit(close_monitoring_db_connection(con), add = TRUE)

      classes_int <- as.integer(input$classes %||% c("3", "4"))
      th <- thresholds_r()
      dr <- date_range_r()
      res <- tryCatch(
        generate_validation_plan(
          con            = con,
          project        = proj,
          source         = input$source,
          n_validation   = as.integer(input$n_validation %||% 20L),
          n_control      = as.integer(input$n_control    %||% 5L),
          classes        = classes_int,
          buffer_m       = as.numeric(input$buffer_m %||% 0),
          seed           = as.integer(input$seed %||% 42L),
          ndvi_threshold = th$ndvi,
          nbr_threshold  = th$nbr,
          window_days    = th$window_days %||% 30L,
          date_from      = if (length(dr) == 2L) dr[1] else NULL,
          date_to        = if (length(dr) == 2L) dr[2] else NULL
        ),
        error = function(e) e
      )
      if (inherits(res, "error")) {
        plan_error(list(class = setdiff(class(res), c("error",
                                                      "condition")),
                        message = conditionMessage(res)))
        return()
      }
      plan_rv(res)
    })

    # Invalidate the result silently if the user touches any input
    # after the plan was generated — the toast hint is rendered in the
    # panel state.
    shiny::observe({
      input$source; input$n_validation; input$n_control;
      input$classes; input$buffer_m; input$seed
      # Only invalidate if a plan exists already.
      if (!is.null(plan_rv())) {
        plan_rv(NULL)
        plan_error(NULL)
      }
    }) |> shiny::bindEvent(
      input$source, input$n_validation, input$n_control,
      input$classes, input$buffer_m, input$seed,
      ignoreInit = TRUE
    )

    # Cache_dir resolver for the alert raster overlay on the map.
    alert_raster_r <- shiny::reactive({
      plan <- plan_rv()
      if (is.null(plan)) return(NULL)
      proj <- app_state$current_project
      if (is.null(proj) || is.null(proj$path)) return(NULL)
      zone_id <- as.integer(plan$zone_id[1])
      src <- as.character(plan$source[1])
      cd <- if (identical(src, "FORDEAD")) {
        file.path(proj$path, "cache", "layers", "fordead")
      } else {
        file.path(proj$path, "cache", "layers", "fast")
      }
      con <- get_monitoring_db_connection(project = proj)
      if (is.null(con)) return(NULL)
      on.exit(close_monitoring_db_connection(con), add = TRUE)
      tryCatch(
        if (identical(src, "FORDEAD")) {
          nemeton::read_fordead_dieback_mask(con, zone_id,
                                             cache_dir = cd)
        } else {
          nemeton::read_fast_alert_mask(con, zone_id, cache_dir = cd)
        },
        error = function(e) NULL
      )
    })

    # Panel : empty state, error state, generating state, or the map.
    output$panel <- shiny::renderUI({
      i18n <- i18n_r()
      err  <- plan_error()
      plan <- plan_rv()

      if (!is.null(err)) {
        title_key <- if ("validation_empty_mask" %in% err$class) {
          "validation_empty_mask_title"
        } else if ("validation_no_mask" %in% err$class) {
          "validation_no_mask_title"
        } else {
          "validation_no_mask_title"
        }
        body_key <- if ("validation_empty_mask" %in% err$class) {
          "validation_empty_mask_body"
        } else {
          "validation_no_mask_body"
        }
        css <- if ("validation_empty_mask" %in% err$class)
          "alert alert-success" else "alert alert-warning"
        return(htmltools::div(
          class = paste("p-4 m-3", css),
          htmltools::h5(i18n$t(title_key)),
          htmltools::p(i18n$t(body_key)),
          htmltools::tags$small(class = "text-muted", err$message)
        ))
      }
      if (is.null(plan)) {
        return(htmltools::div(
          class = "p-4 text-center text-muted",
          bsicons::bs_icon("compass",
                           class = "fs-1 d-block mx-auto mb-3"),
          htmltools::p(i18n$t("validation_idle_hint"))
        ))
      }
      leaflet::leafletOutput(session$ns("map"), height = "55vh")
    })

    output$map <- leaflet::renderLeaflet({
      plan <- plan_rv()
      if (is.null(plan) || !nrow(plan)) return(NULL)
      i18n <- i18n_r()

      plan_ll <- sf::st_transform(plan, 4326)
      colors <- c(Validation = "#2CA02C", Temoin = "#7F7F7F")
      types  <- as.character(plan_ll$type)
      types[!types %in% names(colors)] <- "Validation"
      fill <- unname(colors[types])
      coords <- sf::st_coordinates(plan_ll)

      popups <- vapply(seq_len(nrow(plan_ll)), function(i) {
        sprintf("<strong>%s</strong><br/>%s — class %s<br/>visit_order: %d",
                htmltools::htmlEscape(as.character(plan_ll$plot_id[i])),
                htmltools::htmlEscape(types[i]),
                htmltools::htmlEscape(as.character(
                  plan_ll$alert_class[i] %||% NA)),
                as.integer(plan_ll$visit_order[i] %||% NA))
      }, character(1))

      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",     group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
        leaflet::addLayersControl(
          baseGroups = c("OSM", "Satellite"),
          options    = leaflet::layersControlOptions(collapsed = TRUE)
        )

      # Optional : overlay the alert raster underneath the markers.
      r <- alert_raster_r()
      if (!is.null(r) && inherits(r, "SpatRaster") &&
          terra::ncell(r) > 0L) {
        r_show <- terra::ifel(r == 0, NA, r)
        cats <- 0:4
        cols <- c("#2CA02C", "#FFD27F", "#FF9933", "#D62728", "#222222")
        pal <- leaflet::colorFactor(cols, levels = cats,
                                    na.color = "transparent")
        m <- m |>
          leaflet::addRasterImage(r_show, colors = pal, opacity = 0.55,
                                  method = "ngb")
      }

      m |>
        leaflet::addCircleMarkers(
          lng = coords[, "X"], lat = coords[, "Y"],
          radius = 7, weight = 1, color = "#000",
          fillColor = fill, fillOpacity = 0.95,
          popup = popups
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors   = unname(colors),
          labels   = c(i18n$t("validation_legend_validation"),
                       i18n$t("validation_legend_temoin")),
          title    = NULL,
          opacity  = 0.95
        )
    })

    output$table_panel <- shiny::renderUI({
      if (is.null(plan_rv())) return(NULL)
      htmltools::div(
        class = "p-3",
        DT::DTOutput(session$ns("table"))
      )
    })

    output$table <- DT::renderDT({
      plan <- plan_rv()
      if (is.null(plan)) return(NULL)
      df <- sf::st_drop_geometry(plan)
      keep <- intersect(
        c("plot_id", "type", "alert_class", "visit_order",
          "source", "source_run_id", "generated_at"),
        names(df)
      )
      DT::datatable(df[, keep, drop = FALSE], rownames = FALSE,
                    options = list(pageLength = 10, dom = "tip"))
    })

    # Persist + Export QGIS buttons : disable state mirrors plan_rv().
    shiny::observe({
      enabled <- !is.null(plan_rv())
      shiny::updateActionButton(session, "persist",
                                disabled = !enabled)
      session$sendCustomMessage(
        "shiny:disabled",
        list(id = session$ns("export_qgis"), disabled = !enabled)
      )
    })

    shiny::observeEvent(input$persist, {
      i18n <- i18n_r()
      plan <- plan_rv()
      proj <- app_state$current_project
      if (is.null(plan) || is.null(proj) || is.null(proj$path)) return()
      n <- tryCatch(
        persist_validation_plan(plan, proj$path),
        error = function(e) e
      )
      if (inherits(n, "error")) {
        shiny::showNotification(
          sprintf("%s: %s", i18n$t("validation_persist_btn"),
                  conditionMessage(n)),
          type = "error", duration = 8
        )
        return()
      }
      shiny::showNotification(
        sprintf(i18n$t("validation_persisted_toast"), as.integer(n)),
        type = "message", duration = 5
      )
    })

    output$export_qgis <- shiny::downloadHandler(
      filename = function() {
        sprintf("validation_%s.qgz", format(Sys.time(), "%Y%m%dT%H%M%S"))
      },
      content = function(file) {
        i18n <- i18n_r()
        plan <- plan_rv()
        proj <- app_state$current_project
        if (is.null(plan) || is.null(proj)) {
          file.create(file)
          return(invisible())
        }
        out_dir <- tempfile("validation-qgis-")
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        zone_etude <- if (!is.null(proj$indicators_sf)) {
          tryCatch(sf::st_transform(proj$indicators_sf, 2154),
                   error = function(e) NULL)
        } else NULL
        qgz <- tryCatch(
          nemeton::create_qgis_project(
            placettes    = plan,
            zone_etude   = zone_etude,
            parcours_tsp = NULL,
            output_dir   = out_dir,
            project_name = sprintf("validation_%s",
                                   format(Sys.time(), "%Y%m%dT%H%M%S")),
            crs          = 2154,
            region       = "BFC",
            lang         = app_state$language %||% "fr",
            overwrite    = TRUE
          ),
          error = function(e) {
            cli::cli_alert_warning(
              "create_qgis_project failed: {e$message}"
            )
            NULL
          }
        )
        if (is.null(qgz) || !file.exists(qgz)) {
          file.create(file)
          return(invisible())
        }
        file.copy(qgz, file, overwrite = TRUE)
        shiny::showNotification(
          sprintf(i18n$t("validation_qgis_exported_toast"),
                  basename(qgz)),
          type = "message", duration = 6
        )
      }
    )

    # Force the panel renderUI to evaluate while the sub-tab is
    # hidden, same defensive pattern as the other Suivi sanitaire
    # sub-modules (bslib nav_show/hide is unreliable for output
    # suspension).
    shiny::outputOptions(output, "panel",       suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "table_panel", suspendWhenHidden = FALSE)

    invisible(list(plan = plan_rv))
  })
}

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

  # Helper: combine a label with an info icon that carries a tooltip,
  # matching the pattern used elsewhere in the app (mod_synthesis /
  # mod_family).
  label_tt <- function(label, tooltip) {
    htmltools::tagList(
      label,
      " ",
      bslib::tooltip(
        bsicons::bs_icon("info-circle", class = "text-muted ms-1"),
        tooltip,
        placement = "right"
      )
    )
  }

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
                                  label_tt(i18n$t("sampling_target_error_label"),
                                           i18n$t("sampling_tt_target_error")),
                                  value = 10, min = 1, max = 50, step = 1),
              shiny::numericInput(ns("alpha_pct"),
                                  label_tt(i18n$t("sampling_alpha_label"),
                                           i18n$t("sampling_tt_alpha")),
                                  value = 5, min = 1, max = 20, step = 1),
              shiny::numericInput(ns("over_ratio_pct"),
                                  label_tt(i18n$t("sampling_over_ratio_label"),
                                           i18n$t("sampling_tt_over_ratio")),
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
                  ns("cv_position"),
                  label_tt(i18n$t("sampling_cv_position"),
                           i18n$t("sampling_tt_cv_position")),
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

            shiny::numericInput(ns("seed"),
                                label_tt(i18n$t("sampling_seed"),
                                         i18n$t("sampling_tt_seed")),
                                value = 42, min = 0, max = 1e6, step = 1),

            shiny::selectInput(
              ns("region"),
              label_tt(i18n$t("sampling_region"),
                       i18n$t("sampling_tt_region")),
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

    # --- BD Forêt v2 overlay (cached during project compute) ---------
    # Reads <project>/cache/layers/bdforet.gpkg, normalises the TFV
    # key (trim + dashify + upper) and joins against
    # nemeton::bdforet_v2_mapping() via both the code and the label
    # (IGN WFS often exposes the French libellé in the `tfv` field).
    # Returns sf in EPSG:4326 with a `context_key` column the map
    # uses for colouring, or NULL when the cache is missing.
    bdforet_sf <- shiny::reactive({
      project <- app_state$current_project
      if (is.null(project)) return(NULL)
      project_path <- tryCatch(get_project_path(project$id),
                               error = function(e) NULL)
      if (is.null(project_path)) return(NULL)
      gpkg <- file.path(project_path, "cache", "layers", "bdforet.gpkg")
      if (!file.exists(gpkg)) return(NULL)

      bd <- tryCatch(sf::st_read(gpkg, quiet = TRUE),
                     error = function(e) NULL)
      if (is.null(bd) || nrow(bd) == 0) return(NULL)

      # Detect the TFV column (same heuristic as the compute_cv path).
      tfv_candidates <- c("TFV", "tfv", "CODE_TFV", "code_tfv",
                          "essence", "ESSENCE", "LIB_FV", "LIBELLE")
      tfv_col <- intersect(tfv_candidates, names(bd))[1]
      if (is.na(tfv_col)) return(NULL)

      # Normalize and resolve context_key via the mapping (code -> label
      # fallback).
      map <- tryCatch(nemeton::bdforet_v2_mapping(),
                      error = function(e) NULL)
      if (is.null(map)) return(NULL)
      key <- toupper(gsub("[_[:space:]]+", "-", trimws(bd[[tfv_col]])))
      ctx <- map$context_key[match(key, map$tfv_code)]
      if ("label_key" %in% names(map)) {
        fb <- map$context_key[match(key, map$label_key)]
        ctx[is.na(ctx)] <- fb[is.na(ctx)]
      }
      bd$context_key <- ctx

      # Drop rows we cannot colour (non-forest, unknown) for a cleaner
      # overlay.
      bd <- bd[!is.na(bd$context_key), , drop = FALSE]
      if (nrow(bd) == 0) return(NULL)

      sf::st_transform(bd, 4326)
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
        # Look up the TFV codes + labels + alt context so the user
        # sees WHICH rows are ambiguous and what the alternative
        # mapping would be. Handles the case where cv_from_bdforet()
        # resolved via label_key (the amb$tfv_code field then holds a
        # normalized label rather than a code).
        map <- tryCatch(nemeton::bdforet_v2_mapping(),
                        error = function(e) NULL)
        find_row <- function(key) {
          if (is.null(map)) return(NULL)
          r <- map[map$tfv_code == key | map$label_key == key, , drop = FALSE]
          if (nrow(r) == 0L) NULL else r[1L, ]
        }
        render_row <- function(key, ha) {
          r <- find_row(key)
          label <- if (!is.null(r)) r$label_fr else NA_character_
          code  <- if (!is.null(r)) r$tfv_code else key
          alt   <- if (!is.null(r)) r$alt_context_key else NA_character_
          main  <- if (!is.na(label)) sprintf("%s (%s)", label, code) else code
          htmltools::tags$li(
            htmltools::tags$strong(main),
            if (!is.null(r)) sprintf(
              " — %.2f ha, contexte : %s%s",
              ha, r$context_key,
              if (!is.na(alt)) paste0(" (alt : ", alt, ")") else ""
            ) else sprintf(" — %.2f ha", ha)
          )
        }

        amb <- cv_res$ambiguous %||% data.frame()
        n_amb <- nrow(amb)
        if (n_amb > 0L) {
          items <- lapply(seq_len(n_amb), function(i) {
            render_row(amb$tfv_code[i], amb$area_ha[i])
          })
          parts <- c(parts, list(htmltools::div(
            class = "alert alert-warning py-2 mb-2",
            htmltools::tags$strong(
              sprintf(i18n$t("sampling_cv_ambiguous"), n_amb)
            ),
            htmltools::tags$ul(class = "mb-0 ps-3", items)
          )))
        }

        unmapped <- unique(cv_res$unmapped %||% character(0))
        n_unm <- length(unmapped)
        if (n_unm > 0L) {
          items <- lapply(unmapped, function(k) {
            htmltools::tags$li(htmltools::tags$code(k))
          })
          parts <- c(parts, list(htmltools::div(
            class = "alert alert-warning py-2 mb-2",
            htmltools::tags$strong(
              sprintf(i18n$t("sampling_cv_unmapped"), n_unm)
            ),
            htmltools::tags$ul(class = "mb-0 ps-3", items)
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
    # Overlays (bottom to top on z-order): BD Forêt v2 (by context),
    # UGF polygons, sampled plots. Each is drawn in its own leaflet
    # group so the layersControl lets the user toggle them.
    bdforet_palette <- function() {
      ctx <- c("futaie_reguliere_resineuse",
               "futaie_reguliere_feuillue",
               "futaie_irreguliere",
               "tsf_melange_futaie_taillis",
               "taillis_simple")
      cols <- c("#2E7D32",  # sapin foncé
                "#8BC34A",  # vert clair
                "#00897B",  # sarcelle
                "#827717",  # olive
                "#FBC02D")  # ambre
      leaflet::colorFactor(cols, domain = ctx, na.color = "#9E9E9E")
    }

    # --- Orienteering icons (Start triangle, Finish double-circle) ---
    # Inline SVG as a base64 data URI so the icons travel with the
    # Shiny page: no new dependency, no static file to ship. Colour
    # is the classic orienteering magenta (#E91E63).
    svg_data_uri <- function(svg) {
      b64 <- jsonlite::base64_enc(charToRaw(svg))
      paste0("data:image/svg+xml;base64,", b64)
    }
    ori_icons <- function() {
      start_svg <- paste0(
        "<svg xmlns='http://www.w3.org/2000/svg' width='28' height='28' ",
        "viewBox='0 0 28 28'>",
        "<polygon points='14,3 25,25 3,25' fill='white' ",
        "fill-opacity='0.5' stroke='#E91E63' stroke-width='3' />",
        "</svg>"
      )
      finish_svg <- paste0(
        "<svg xmlns='http://www.w3.org/2000/svg' width='28' height='28' ",
        "viewBox='0 0 28 28'>",
        "<circle cx='14' cy='14' r='12' fill='white' fill-opacity='0.5' ",
        "stroke='#E91E63' stroke-width='3' />",
        "<circle cx='14' cy='14' r='7' fill='none' ",
        "stroke='#E91E63' stroke-width='3' />",
        "</svg>"
      )
      list(
        start = leaflet::makeIcon(
          iconUrl    = svg_data_uri(start_svg),
          iconWidth  = 28, iconHeight = 28,
          iconAnchorX = 14, iconAnchorY = 14
        ),
        finish = leaflet::makeIcon(
          iconUrl    = svg_data_uri(finish_svg),
          iconWidth  = 28, iconHeight = 28,
          iconAnchorX = 14, iconAnchorY = 14
        )
      )
    }

    output$map <- leaflet::renderLeaflet({
      i18n <- get_i18n(app_state$language %||% "fr")
      units <- units_sf()
      plots <- sampling_rv$plots
      bd    <- bdforet_sf()

      base <- leaflet::leaflet() |>
        leaflet::addProviderTiles("OpenStreetMap",  group = "OSM") |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite")

      overlays <- character(0)

      if (!is.null(bd) && nrow(bd) > 0) {
        pal_bd <- bdforet_palette()
        base <- leaflet::addPolygons(
          base, data = bd,
          color = ~pal_bd(context_key), weight = 0,
          fillColor = ~pal_bd(context_key), fillOpacity = 0.35,
          label = ~as.character(context_key),
          group = "BD Foret"
        )
        base <- leaflet::addLegend(
          base, position = "topright",
          pal = pal_bd,
          values = c("futaie_reguliere_resineuse",
                     "futaie_reguliere_feuillue",
                     "futaie_irreguliere",
                     "tsf_melange_futaie_taillis",
                     "taillis_simple"),
          title = "BD Forêt v2", opacity = 0.8
        )
        overlays <- c(overlays, "BD Foret")
      }

      if (!is.null(units)) {
        units_ll <- sf::st_transform(units, 4326)
        base <- leaflet::addPolygons(
          base, data = units_ll,
          color = "#1B6B1B", weight = 1, fillColor = "#1B6B1B",
          fillOpacity = 0.06, group = "Unites"
        )
        overlays <- c(overlays, "Unites")
      }

      if (!is.null(plots) && nrow(plots) > 0) {
        plots_ll <- sf::st_transform(plots, 4326)
        pal <- leaflet::colorFactor(c("#1f77b4", "#ff7f0e"),
                                    domain = c("Base", "Over"))

        # TSP route: connect Base plots in visit_order. First Base ->
        # start triangle; last Base -> finish double-circle; all
        # intermediate Base + every Over remain standard circles.
        base_plots <- plots_ll[plots_ll$type == "Base", , drop = FALSE]
        over_plots <- plots_ll[plots_ll$type == "Over", , drop = FALSE]
        if (nrow(base_plots) >= 2L) {
          base_plots <- base_plots[order(base_plots$visit_order), , drop = FALSE]
          coords <- sf::st_coordinates(base_plots)
          line_sf <- sf::st_sf(
            geometry = sf::st_sfc(sf::st_linestring(coords), crs = 4326)
          )
          base <- leaflet::addPolylines(
            base, data = line_sf,
            color = "#E91E63", weight = 2, opacity = 0.8,
            dashArray = "6,6",
            group = "Parcours"
          )
          # TSP legend — custom HTML control, since leaflet::addLegend
          # only handles categorical / numeric palettes; we want inline
          # glyphs matching the map (dashed line + orienteering icons).
          tsp_legend_html <- sprintf(paste0(
            "<div style='background: white; padding: 6px 10px; ",
            "border: 1px solid #aaa; border-radius: 4px; ",
            "font-size: 12px; line-height: 1.5;'>",
            "<div style='font-weight: bold; margin-bottom: 4px;'>%s</div>",
            "<div><svg width='28' height='8'>",
            "<line x1='2' y1='4' x2='26' y2='4' ",
            "stroke='#E91E63' stroke-width='2' stroke-dasharray='4,3'/>",
            "</svg> %s</div>",
            "<div><svg width='16' height='16' viewBox='0 0 28 28' ",
            "style='vertical-align: middle;'>",
            "<polygon points='14,3 25,25 3,25' fill='white' ",
            "fill-opacity='0.5' stroke='#E91E63' stroke-width='3'/>",
            "</svg> %s</div>",
            "<div><svg width='16' height='16' viewBox='0 0 28 28' ",
            "style='vertical-align: middle;'>",
            "<circle cx='14' cy='14' r='12' fill='white' ",
            "fill-opacity='0.5' stroke='#E91E63' stroke-width='3'/>",
            "<circle cx='14' cy='14' r='7' fill='none' ",
            "stroke='#E91E63' stroke-width='3'/>",
            "</svg> %s</div>",
            "</div>"),
            i18n$t("sampling_legend_tsp_title"),
            i18n$t("sampling_legend_tsp_line"),
            i18n$t("sampling_legend_tsp_start"),
            i18n$t("sampling_legend_tsp_end")
          )
          base <- leaflet::addControl(
            base, html = htmltools::HTML(tsp_legend_html),
            position = "bottomleft",
            className = "leaflet-tsp-legend"
          )
          overlays <- c(overlays, "Parcours")
        }

        # Middle Base plots (exclude first & last).
        mid <- if (nrow(base_plots) >= 3L) {
          base_plots[-c(1L, nrow(base_plots)), , drop = FALSE]
        } else {
          base_plots[0L, , drop = FALSE]
        }
        if (nrow(mid) > 0L) {
          base <- leaflet::addCircleMarkers(
            base, data = mid,
            radius = 5, weight = 1, color = "#333",
            fillColor = ~pal(type), fillOpacity = 0.9,
            label = ~sprintf("%s (%s) — ordre %d",
                             plot_id, type, visit_order),
            group = "Placettes"
          )
        }
        if (nrow(over_plots) > 0L) {
          base <- leaflet::addCircleMarkers(
            base, data = over_plots,
            radius = 5, weight = 1, color = "#333",
            fillColor = ~pal(type), fillOpacity = 0.9,
            label = ~sprintf("%s (%s)", plot_id, type),
            group = "Placettes"
          )
        }

        # Orienteering start / finish icons.
        if (nrow(base_plots) >= 1L) {
          icons <- ori_icons()
          start_pt <- base_plots[1L, , drop = FALSE]
          base <- leaflet::addMarkers(
            base, data = start_pt,
            icon = icons$start,
            label = ~sprintf("Départ — %s", plot_id),
            group = "Placettes"
          )
          if (nrow(base_plots) >= 2L) {
            end_pt <- base_plots[nrow(base_plots), , drop = FALSE]
            base <- leaflet::addMarkers(
              base, data = end_pt,
              icon = icons$finish,
              label = ~sprintf("Arrivée — %s", plot_id),
              group = "Placettes"
            )
          }
        }

        base <- leaflet::addLegend(
          base, position = "bottomright",
          pal = pal, values = c("Base", "Over"),
          title = "Placettes"
        )
        overlays <- c(overlays, "Placettes")
      }

      base <- leaflet::addLayersControl(
        base,
        baseGroups = c("OSM", "Satellite"),
        overlayGroups = overlays,
        options = leaflet::layersControlOptions(collapsed = TRUE)
      )
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

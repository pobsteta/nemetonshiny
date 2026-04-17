#' Family Indicator Module - Server
#'
#' @description
#' Generic server module for displaying one indicator family.
#' Used for all 12 families (C, B, W, A, F, L, T, R, S, P, E, N).
#'
#' @param id Character. Module namespace ID.
#' @param family_code Character. Single-letter family code.
#' @param app_state reactiveValues. Application state containing current_project.
#'
#' @return NULL (called for side effects)
#'
#' @noRd
mod_family_server <- function(id, family_code, app_state) {
  shiny::moduleServer(id, function(input, output, session) {

    family_config <- get_family_config(family_code)

    # ================================================================
    # REACTIVE: Extract family indicator columns from project
    # ================================================================
    indicators_data <- shiny::reactive({
      project <- app_state$current_project
      if (is.null(project) || is.null(project$indicators)) return(NULL)

      df <- project$indicators

      # Drop geometry if sf
      if (inherits(df, "sf")) {
        df <- tryCatch(sf::st_drop_geometry(df),
                       error = function(e) {
                         geo_col <- attr(df, "sf_column") %||% "geometry"
                         result <- df[, setdiff(names(df), geo_col), drop = FALSE]
                         class(result) <- "data.frame"
                         result
                       })
      }

      all_cols <- names(df)

      # Try both short codes (C1, C2) and long-form column names
      candidates <- c(family_config$indicators, family_config$column_names)
      matched <- character(0)
      for (col in candidates) {
        norm_col <- paste0(col, "_norm")
        if (norm_col %in% all_cols) {
          matched <- c(matched, norm_col)
        } else if (col %in% all_cols) {
          matched <- c(matched, col)
        }
      }

      matched <- unique(matched)
      if (length(matched) == 0) return(NULL)

      # Keep the UGF identifier + display metadata + matched indicators.
      # Rows are always keyed by ug_id with label / groupe / refs.
      meta_cols <- intersect(
        c("ug_id", "label", "groupe", "cadastral_refs",
          "surface_m2", "surface_sig_m2", "n_tenements"),
        all_cols
      )
      df[, c(meta_cols, matched), drop = FALSE]
    })

    # ================================================================
    # REACTIVE: UGF sf (geometry + selected indicator columns)
    # ================================================================
    indicators_sf <- shiny::reactive({
      ind_data <- indicators_data()
      if (is.null(ind_data)) return(NULL)

      project <- app_state$current_project
      if (is.null(project$indicators_sf) ||
          !inherits(project$indicators_sf, "sf")) return(NULL)

      sf_all <- project$indicators_sf
      matched <- setdiff(names(ind_data),
                         c("ug_id", "label", "groupe", "cadastral_refs",
                           "surface_m2", "surface_sig_m2", "n_tenements"))
      keep <- intersect(c("ug_id", "label", "groupe", "cadastral_refs",
                          matched, attr(sf_all, "sf_column") %||% "geometry"),
                        names(sf_all))
      sf_all[, keep, drop = FALSE]
    })

    # ================================================================
    # OUTPUT: Dynamic maps row - adapts to number of indicators
    # ================================================================
    output$maps_row <- shiny::renderUI({
      ns <- session$ns
      i18n <- get_i18n(app_state$language)
      sf_data <- indicators_sf()

      if (is.null(sf_data)) {
        return(bslib::layout_columns(
          col_widths = c(12),
          bslib::card(
            bslib::card_body(
              htmltools::div(
                class = "text-muted text-center p-4",
                i18n$t("no_data")
              )
            )
          )
        ))
      }

      ind_cols <- get_indicator_cols(sf_data)
      n <- length(ind_cols)
      if (n == 0) {
        return(bslib::layout_columns(
          col_widths = c(12),
          bslib::card(
            bslib::card_body(
              htmltools::div(
                class = "text-muted text-center p-4",
                i18n$t("no_data")
              )
            )
          )
        ))
      }

      # Build one col per indicator using plain Bootstrap grid
      col_class <- if (n == 1) "col-12" else if (n == 2) "col-6" else if (n >= 4) "col-3" else "col-4"

      map_cols <- lapply(seq_len(n), function(i) {
        map_id <- paste0("map", i)
        htmltools::div(
          class = col_class,
          htmltools::div(
            class = "family-map-wrapper",
            leaflet::leafletOutput(ns(map_id), height = "400px")
          )
        )
      })

      htmltools::div(
        class = "row mb-3",
        map_cols
      )
    })

    # Render maps dynamically
    shiny::observe({
      sf_data <- indicators_sf()
      if (is.null(sf_data)) return()

      i18n <- get_i18n(app_state$language)
      ind_cols <- get_indicator_cols(sf_data)

      lapply(seq_along(ind_cols), function(i) {
        map_id <- paste0("map", i)
        output[[map_id]] <- leaflet::renderLeaflet({
          make_indicator_leaflet(sf_data, ind_cols[i],
                                 clean_indicator_label(ind_cols[i], i18n))
        })
      })
    })

    # ================================================================
    # OUTPUT: Indicator data table
    # ================================================================
    output$indicator_table <- DT::renderDataTable({
      ind_data <- indicators_data()
      if (is.null(ind_data)) return(NULL)

      # Drop geometry if present
      if (inherits(ind_data, "sf")) {
        ind_data <- sf::st_drop_geometry(ind_data)
      }

      # Keep only: label (UGF), groupe, and indicator values.
      # Indicator columns are everything numeric that's not UGF /
      # parcel metadata (same rule as get_indicator_cols()).
      ind_cols <- get_indicator_cols(ind_data)
      keep <- intersect(c("label", "groupe", ind_cols), names(ind_data))
      ind_data <- ind_data[, keep, drop = FALSE]

      # Round numeric columns
      num_cols <- vapply(ind_data, is.numeric, logical(1))
      ind_data[num_cols] <- lapply(ind_data[num_cols], round, digits = 3)

      # Clean column names: use short codes (B1, B2, C1, etc.) for
      # indicators; keep Label / Groupe headers in plain French.
      i18n <- get_i18n(app_state$language)
      clean_names <- vapply(names(ind_data), function(col) {
        if (col == "label")  return("Label UGF")
        if (col == "groupe") return(get_groupes_field_label(
          app_state$current_project$metadata$groupes_profile,
          lang = app_state$language %||% "fr"
        ))
        base <- sub("_norm$", "", col)
        for (fam in INDICATOR_FAMILIES) {
          if (!is.null(fam$column_names) && base %in% fam$column_names) {
            idx <- which(fam$column_names == base)
            if (idx <= length(fam$indicators)) return(fam$indicators[idx])
          }
        }
        base
      }, character(1))
      names(ind_data) <- clean_names

      DT::datatable(ind_data,
                     selection = "single",
                     rownames = TRUE,
                     options = list(
                       pageLength = 10,
                       scrollX = TRUE,
                       autoWidth = FALSE,
                       columnDefs = list(
                         list(width = "80px", targets = "_all")
                       ),
                       language = list(
                         url = if (identical(app_state$language, "fr"))
                           "//cdn.datatables.net/plug-ins/1.13.7/i18n/fr-FR.json"
                         else ""
                       )
                     ),
                     class = "table table-striped table-hover table-bordered")
    })

    # ================================================================
    # OBSERVER: Click on table row → zoom to parcel on maps
    # ================================================================
    shiny::observeEvent(input$indicator_table_rows_selected, {
      row_idx <- input$indicator_table_rows_selected
      if (is.null(row_idx) || length(row_idx) == 0) return()

      sf_data <- indicators_sf()
      if (is.null(sf_data)) return()

      # Resolve selected parcel via ID (not row index, which changes after sort)
      ind_data <- indicators_data()
      if (is.null(ind_data) || row_idx > nrow(ind_data)) return()

      # Rows are keyed by ug_id — resolve selection through it, not
      # by row index (which changes after sort / search in DataTables).
      if ("ug_id" %in% names(ind_data) && "ug_id" %in% names(sf_data)) {
        selected_id <- ind_data[["ug_id"]][row_idx]
        sf_match <- which(sf_data[["ug_id"]] == selected_id)
        if (length(sf_match) == 0) return()
        selected <- sf_data[sf_match[1], ]
      } else {
        if (row_idx > nrow(sf_data)) return()
        selected <- sf_data[row_idx, ]
      }
      selected_wgs84 <- sf::st_transform(selected, 4326)
      bbox <- sf::st_bbox(selected_wgs84)

      # Expand bbox by 20% in each direction for comfortable view
      dx <- (bbox[["xmax"]] - bbox[["xmin"]]) * 0.2
      dy <- (bbox[["ymax"]] - bbox[["ymin"]]) * 0.2
      # Ensure a minimum padding for very small parcels
      dx <- max(dx, 0.001)
      dy <- max(dy, 0.001)

      lng1 <- bbox[["xmin"]] - dx
      lat1 <- bbox[["ymin"]] - dy
      lng2 <- bbox[["xmax"]] + dx
      lat2 <- bbox[["ymax"]] + dy

      # Compute popup content and centroid
      i18n <- get_i18n(app_state$language)
      centroid <- sf::st_coordinates(suppressWarnings(sf::st_centroid(selected_wgs84)))
      popup_lng <- centroid[1, 1]
      popup_lat <- centroid[1, 2]

      ind_cols <- get_indicator_cols(sf_data)
      # Popup header: the UGF label (user-assigned, e.g. "TSF-Sud"),
      # with ug_id as fallback and row index as last resort.
      ug_label <- if ("label" %in% names(sf_data)) {
        as.character(selected_wgs84[["label"]])
      } else if ("ug_id" %in% names(sf_data)) {
        as.character(selected_wgs84[["ug_id"]])
      } else {
        as.character(row_idx)
      }
      dropped <- sf::st_drop_geometry(selected_wgs84)

      popup_lines <- vapply(ind_cols, function(col) {
        label <- clean_indicator_label(col, i18n)
        val <- dropped[[col]]
        sprintf("%s: %s", label, if (is.na(val)) "NA" else round(val, 3))
      }, character(1))
      popup_html <- paste0("<strong>", ug_label, "</strong><br/>",
                           paste(popup_lines, collapse = "<br/>"))

      # Zoom all maps and show popup
      for (i in seq_along(ind_cols)) {
        local({
          map_id <- paste0("map", i)
          leaflet::leafletProxy(map_id, session) |>
            leaflet::fitBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2) |>
            leaflet::clearPopups() |>
            leaflet::addPopups(lng = popup_lng, lat = popup_lat, popup = popup_html)
        })
      }
    })

    # ================================================================
    # OUTPUT: Analysis - descriptive statistics + alerts
    # ================================================================
    output$analysis_stats <- shiny::renderUI({
      i18n <- get_i18n(app_state$language)
      ind_data <- indicators_data()
      if (is.null(ind_data)) return(NULL)

      if (inherits(ind_data, "sf")) {
        ind_data <- sf::st_drop_geometry(ind_data)
      }

      ind_cols <- get_indicator_cols(ind_data)
      if (length(ind_cols) == 0) return(NULL)

      # Build stats table for each indicator
      stats_rows <- lapply(ind_cols, function(col) {
        vals <- ind_data[[col]]
        n_total <- length(vals)
        n_na <- sum(is.na(vals))
        vals_clean <- vals[!is.na(vals)]
        n <- length(vals_clean)

        if (n == 0) return(NULL)

        mn <- min(vals_clean)
        mx <- max(vals_clean)
        avg <- mean(vals_clean)
        med <- stats::median(vals_clean)
        sd_val <- if (n > 1) stats::sd(vals_clean) else 0
        cv <- if (avg != 0) abs(sd_val / avg) * 100 else 0

        label <- clean_indicator_label(col, i18n)
        tooltip_text <- get_indicator_tooltip(col, app_state$language)

        # Alerts
        alerts <- list()
        if (cv > 50) {
          alerts <- c(alerts, list(htmltools::div(
            class = "text-warning small",
            shiny::icon("exclamation-triangle"),
            " ", i18n$t("alert_high_variability")
          )))
        }
        if (n_na > 0) {
          msg <- gsub("\\{n\\}", n_na, gsub("\\{total\\}", n_total,
                       i18n$t("alert_many_na")))
          alerts <- c(alerts, list(htmltools::div(
            class = "text-danger small",
            shiny::icon("exclamation-circle"),
            " ", msg
          )))
        }

        # Build label with tooltip if available
        label_element <- if (!is.null(tooltip_text)) {
          htmltools::tagList(
            htmltools::tags$strong(label),
            bslib::tooltip(
              htmltools::tags$span(
                class = "ms-1 text-info",
                style = "cursor: help;",
                shiny::icon("circle-info", class = "fa-sm")
              ),
              tooltip_text
            )
          )
        } else {
          htmltools::tags$strong(label)
        }

        htmltools::div(
          class = "mb-3",
          label_element,
          htmltools::tags$table(
            class = "table table-sm table-borderless mb-1",
            style = "font-size: 0.85em;",
            htmltools::tags$tbody(
              htmltools::tags$tr(
                htmltools::tags$td(i18n$t("stat_n")),
                htmltools::tags$td(class = "text-end", n),
                htmltools::tags$td(i18n$t("stat_min")),
                htmltools::tags$td(class = "text-end", round(mn, 3))
              ),
              htmltools::tags$tr(
                htmltools::tags$td(i18n$t("stat_max")),
                htmltools::tags$td(class = "text-end", round(mx, 3)),
                htmltools::tags$td(i18n$t("stat_mean")),
                htmltools::tags$td(class = "text-end", round(avg, 3))
              ),
              htmltools::tags$tr(
                htmltools::tags$td(i18n$t("stat_median")),
                htmltools::tags$td(class = "text-end", round(med, 3)),
                htmltools::tags$td(i18n$t("stat_sd")),
                htmltools::tags$td(class = "text-end", round(sd_val, 3))
              )
            )
          ),
          if (length(alerts) > 0) htmltools::tagList(alerts)
        )
      })

      htmltools::div(
        htmltools::tagList(stats_rows)
      )
    })

    # ================================================================
    # AI ANALYSIS: Generate analysis via ellmer/Claude
    # ================================================================
    shiny::observeEvent(input$ai_generate, {
      i18n <- get_i18n(app_state$language)

      # Check API key for providers that require one
      provider <- get_app_config("llm_provider", "anthropic")
      key_var <- get_llm_api_key_var(provider)
      if (!is.null(key_var) && nchar(Sys.getenv(key_var)) == 0) {
        msg <- gsub("\\{key_var\\}", key_var, i18n$t("ai_no_api_key"))
        shiny::showNotification(
          msg,
          type = "warning",
          duration = 8
        )
        return()
      }

      ind_data <- indicators_data()
      if (is.null(ind_data)) return()

      # Disable button during call
      shiny::updateActionButton(session, "ai_generate",
                                label = i18n$t("ai_generating"),
                                icon = shiny::icon("spinner", class = "fa-spin"))

      # Show notification while AI is thinking
      notif_id <- shiny::showNotification(
        htmltools::div(
          shiny::icon("spinner", class = "fa-spin me-2"),
          i18n$t("ai_generating")
        ),
        type = "message",
        duration = NULL
      )

      language <- if (identical(app_state$language, "fr")) "fran\u00e7ais" else "English"
      prompt <- build_analysis_prompt(family_config, ind_data, language)
      expert <- input$expert_profile %||% "generalist"
      system_prompt <- build_system_prompt(language, expert = expert)

      tryCatch({
        chat <- create_llm_chat(system_prompt)
        response <- as.character(chat$chat(prompt, echo = FALSE))

        shiny::updateTextAreaInput(session, "analysis_comments", value = response)
        shiny::removeNotification(notif_id)
      }, error = function(e) {
        shiny::removeNotification(notif_id)
        shiny::showNotification(
          paste(i18n$t("ai_error"), ":", strip_ansi(conditionMessage(e))),
          type = "error",
          duration = 8
        )
      })

      # Restore button
      shiny::updateActionButton(session, "ai_generate",
                                label = i18n$t("ai_generate"),
                                icon = shiny::icon("robot"))
    })

    # ================================================================
    # OUTPUT: Missing indicators warning
    # ================================================================
    output$missing_warning <- shiny::renderUI({
      i18n <- get_i18n(app_state$language)
      project <- app_state$current_project

      if (is.null(project) || is.null(project$indicators)) {
        return(htmltools::div(
          class = "alert alert-info",
          shiny::icon("info-circle"),
          " ", i18n$t("no_data")
        ))
      }

      df <- project$indicators
      all_cols <- names(df)

      # Check both short codes and long-form column names
      candidates <- c(family_config$indicators, family_config$column_names)

      # Check which indicators are missing entirely
      missing <- character(0)
      all_na <- character(0)
      # Check in pairs: short code + long-form name
      for (i in seq_along(family_config$indicators)) {
        code <- family_config$indicators[i]
        long_name <- if (!is.null(family_config$column_names) &&
                         i <= length(family_config$column_names)) {
          family_config$column_names[i]
        } else {
          NULL
        }
        # Check if any form exists
        found_col <- NULL
        for (col in c(paste0(code, "_norm"), code,
                      if (!is.null(long_name)) c(paste0(long_name, "_norm"), long_name))) {
          if (col %in% all_cols) { found_col <- col; break }
        }
        if (is.null(found_col)) {
          missing <- c(missing, code)
        } else if (all(is.na(df[[found_col]]))) {
          all_na <- c(all_na, code)
        }
      }

      warnings <- list()
      if (length(missing) > 0) {
        warnings <- c(warnings, list(htmltools::p(
          class = "mb-1",
          shiny::icon("exclamation-triangle"),
          sprintf(" %s: %s", i18n$t("missing_data"), paste(missing, collapse = ", "))
        )))
      }
      if (length(all_na) > 0) {
        warnings <- c(warnings, list(htmltools::p(
          class = "mb-1",
          shiny::icon("exclamation-triangle"),
          sprintf(" %s: %s", i18n$t("no_data"), paste(all_na, collapse = ", "))
        )))
      }

      if (length(warnings) == 0) return(NULL)

      htmltools::div(
        class = "alert alert-warning mt-2",
        warnings
      )
    })

    # ================================================================
    # OBSERVER: Save comments to app_state when changed
    # ================================================================
    shiny::observeEvent(input$analysis_comments, {
      # Initialize family_comments list if not exists
      if (is.null(app_state$family_comments)) {
        app_state$family_comments <- list()
      }
      # Save comment for this family
      app_state$family_comments[[family_code]] <- input$analysis_comments

      # Persist to disk
      project_id <- app_state$project_id
      if (!is.null(project_id)) {
        save_comments(project_id,
                      synthesis = NULL,
                      families = app_state$family_comments)
      }
    }, ignoreInit = TRUE)

    # ================================================================
    # OBSERVER: Load saved comments when module initializes
    # ================================================================
    shiny::observe({
      # Only run once when module is first accessed
      shiny::isolate({
        if (!is.null(app_state$family_comments) &&
            !is.null(app_state$family_comments[[family_code]])) {
          saved_comment <- app_state$family_comments[[family_code]]
          if (nchar(saved_comment) > 0) {
            shiny::updateTextAreaInput(session, "analysis_comments", value = saved_comment)
          }
        }
      })
    }) |> shiny::bindEvent(app_state$current_project, once = TRUE)

    # ================================================================
    # OBSERVER: Reload comment from app_state (after fill-all from synthesis)
    # ================================================================
    shiny::observeEvent(app_state$refresh_family_comments, {
      comment <- app_state$family_comments[[family_code]]
      shiny::updateTextAreaInput(session, "analysis_comments",
                                 value = if (!is.null(comment)) comment else "")
    }, ignoreInit = TRUE)

    # ================================================================
    # OBSERVER: Clear comment when commune/department changes
    # ================================================================
    shiny::observeEvent(app_state$clear_all_comments, {
      shiny::updateTextAreaInput(session, "analysis_comments", value = "")
    }, ignoreInit = TRUE)

  })
}


# ================================================================
# HELPER FUNCTIONS
# ================================================================

#' Create an LLM chat object based on configured provider
#'
#' @description
#' Factory function that reads `llm_provider` from app config and dispatches
#' to the appropriate `ellmer::chat_*()` constructor.
#'
#' @param system_prompt Character. System prompt for the chat.
#' @return An ellmer chat object.
#' @noRd
create_llm_chat <- function(system_prompt) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required for AI analysis. Install it with: install.packages('ellmer')",
         call. = FALSE)
  }

  provider <- get_app_config("llm_provider", "anthropic")
  models <- get_app_config("llm_models", list())
  model <- models[[provider]]

  switch(provider,
    anthropic = ellmer::chat_anthropic(system_prompt = system_prompt, model = model),
    mistral = ellmer::chat_mistral(system_prompt = system_prompt, model = model),
    openai = ellmer::chat_openai(system_prompt = system_prompt, model = model),
    google = ellmer::chat_google_gemini(system_prompt = system_prompt, model = model),
    deepseek = ellmer::chat_deepseek(system_prompt = system_prompt, model = model),
    ollama = ellmer::chat_ollama(system_prompt = system_prompt, model = model),
    stop(sprintf("Unknown LLM provider: '%s'", provider))
  )
}

#' Get the environment variable name for an LLM provider's API key
#'
#' @param provider Character. Provider name.
#' @return Character. Environment variable name, or NULL for providers without keys.
#' @noRd
get_llm_api_key_var <- function(provider) {
  key_map <- list(
    anthropic = "ANTHROPIC_API_KEY",
    mistral = "MISTRAL_API_KEY",
    openai = "OPENAI_API_KEY",
    google = "GOOGLE_API_KEY",
    deepseek = "DEEPSEEK_API_KEY"
  )
  key_map[[provider]]
}

#' Strip ANSI escape codes from a string
#'
#' Errors from `ellmer`/`cli` can include ANSI color codes intended for the
#' terminal. These codes render as garbage in a Shiny notification, so strip
#' them before display.
#'
#' @param text Character.
#' @return Character with ANSI escape sequences removed.
#' @noRd
strip_ansi <- function(text) {
  gsub("\033\\[[0-9;]*[a-zA-Z]", "", text, perl = TRUE)
}

#' Create a Leaflet choropleth map for an indicator column
#' @param sf_data An sf object with indicator data and geometries.
#' @param ind_col Character. Column name of the indicator to map.
#' @param title Character. Title for the legend.
#' @return A leaflet map object.
#' @noRd
make_indicator_leaflet <- function(sf_data, ind_col, title) {
  # Transform to WGS84 for Leaflet
  sf_wgs84 <- sf::st_transform(sf_data, 4326)

  vals <- sf_wgs84[[ind_col]]

  # Handle case where all values are NA
  if (all(is.na(vals))) {
    return(leaflet::leaflet(sf_wgs84) |>
      leaflet::addTiles() |>
      leaflet::addPolygons(color = "#999", weight = 1, fillOpacity = 0.3))
  }

  # Auto-scale domain to actual data range for family detail view
  # When all values are identical (or nearly so), expand domain to avoid
  # colorNumeric producing erratic colors from a zero-width range
  val_range <- range(vals, na.rm = TRUE)
  if (isTRUE(val_range[2] - val_range[1] < 1e-6)) {
    # All values identical: center the domain around the value
    mid <- val_range[1]
    val_range <- c(max(0, mid - 1), mid + 1)
  }
  # Use YlOrRd palette for risk indicators, viridis otherwise
  is_risk <- grepl("^R[1-4]|^famille_risque|^risk_", ind_col)
  pal_name <- if (is_risk) "YlOrRd" else "viridis"
  pal <- leaflet::colorNumeric(pal_name, domain = val_range, na.color = "#cccccc")

  # Build hover labels — prefer the user-assigned UGF label, fall
  # back to ug_id, then to row index as last resort.
  ids <- if ("label" %in% names(sf_wgs84)) {
    as.character(sf_wgs84[["label"]])
  } else if ("ug_id" %in% names(sf_wgs84)) {
    as.character(sf_wgs84[["ug_id"]])
  } else {
    as.character(seq_len(nrow(sf_wgs84)))
  }
  labels <- sprintf("<strong>%s</strong><br/>%s: %s",
                    ids, title, round(vals, 3)) |>
    lapply(htmltools::HTML)

  # Build layerId from the UGF identifier when available (stable
  # across filter/sort in the DataTable); fall back to the display
  # label (also unique within a project).
  layer_ids <- if ("ug_id" %in% names(sf_wgs84)) {
    as.character(sf_wgs84[["ug_id"]])
  } else {
    as.character(ids)
  }

  leaflet::leaflet(sf_wgs84) |>
    leaflet::addTiles() |>
    leaflet::addPolygons(
      fillColor = ~pal(vals),
      weight = 1,
      color = "#333",
      fillOpacity = 0.7,
      layerId = layer_ids,
      highlightOptions = leaflet::highlightOptions(
        weight = 2, color = "#000", fillOpacity = 0.9, bringToFront = TRUE
      ),
      label = labels,
      labelOptions = leaflet::labelOptions(
        style = list("font-size" = "12px", "padding" = "4px 8px"),
        textsize = "12px",
        direction = "auto"
      )
    ) |>
    leaflet::addLegend(
      position = "topright",
      pal = pal,
      values = vals,
      title = title,
      opacity = 0.7
    )
}

#' Get indicator columns from an sf/data.frame (exclude id/geometry cols)
#' @noRd
get_indicator_cols <- function(data) {
  all_cols <- names(data)
  exclude <- c(
    # UGF metadata columns injected by ug_build_sf() / load_project()
    "ug_id", "label", "groupe", "cadastral_refs",
    "surface_m2", "surface_sig_m2", "n_tenements",
    # Parcel-compat aliases added by start_computation() so the
    # nemeton::indicateur_* functions accept the UGF sf
    "id", "nemeton_id", "geo_parcelle",
    "contenance", "code_insee", "section", "numero",
    # Geometry
    "geometry", "geom"
  )
  cols <- setdiff(all_cols, exclude)
  # Extra safety: keep only numeric columns. Any future metadata
  # that slips through won't poison mean/range/sd downstream.
  is_num <- vapply(cols, function(c) is.numeric(data[[c]]), logical(1))
  cols[is_num]
}


#' Clean indicator label for display
#' @noRd
clean_indicator_label <- function(col_name, i18n) {
  # Strip _norm suffix for display
  base <- sub("_norm$", "", col_name)

  # Try matching long-form name against INDICATOR_FAMILIES config first
  # (maps column_names -> short codes -> i18n keys with richer labels)
  for (fam in INDICATOR_FAMILIES) {
    if (!is.null(fam$column_names) && base %in% fam$column_names) {
      idx <- which(fam$column_names == base)
      if (idx <= length(fam$indicators)) {
        code <- fam$indicators[idx]
        short_key <- paste0("indicator_", code)
        if (i18n$has(short_key)) {
          return(paste0(code, " - ", i18n$t(short_key)))
        }
      }
    }
  }

  # Try i18n key directly (works for short codes like C1, B2)
  key <- paste0("indicator_", base)
  if (i18n$has(key)) {
    return(paste0(base, " - ", i18n$t(key)))
  }

  # Fallback: humanize the column name
  gsub("_", " ", base)
}


#' Get indicator tooltip for display (T178)
#' @noRd
get_indicator_tooltip <- function(col_name, lang = "fr") {
  # Strip _norm suffix
  base <- sub("_norm$", "", col_name)

  # Search in INDICATOR_FAMILIES config
 for (fam in INDICATOR_FAMILIES) {
    if (!is.null(fam$column_names) && base %in% fam$column_names) {
      idx <- which(fam$column_names == base)
      if (idx <= length(fam$indicators)) {
        code <- fam$indicators[idx]
        if (!is.null(fam$indicator_tooltips) && !is.null(fam$indicator_tooltips[[code]])) {
          tooltip <- fam$indicator_tooltips[[code]][[lang]]
          if (!is.null(tooltip)) return(tooltip)
          # Fallback to English
          return(fam$indicator_tooltips[[code]][["en"]])
        }
      }
    }
    # Also check short codes directly
    if (base %in% fam$indicators) {
      if (!is.null(fam$indicator_tooltips) && !is.null(fam$indicator_tooltips[[base]])) {
        tooltip <- fam$indicator_tooltips[[base]][[lang]]
        if (!is.null(tooltip)) return(tooltip)
        return(fam$indicator_tooltips[[base]][["en"]])
      }
    }
  }

  # No tooltip found
  NULL
}

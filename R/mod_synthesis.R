#' Synthesis Module - Server
#'
#' @description
#' Server module for the project synthesis view.
#' Displays radar chart, summary table, and download buttons.
#'
#' @param id Character. Module namespace ID.
#' @param app_state reactiveValues. Application state containing current_project.
#'
#' @return NULL (called for side effects)
#'
#' @noRd
mod_synthesis_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {

    # ================================================================
    # REACTIVE: Project indicators
    # ================================================================
    project_indicators <- shiny::reactive({
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

      df
    })

    # ================================================================
    # REACTIVE: Build sf with family scores (one row per UGF)
    # ================================================================
    family_scores <- shiny::reactive({
      project <- app_state$current_project
      if (is.null(project)) return(NULL)

      # project$indicators_sf is always built by load_project(): one
      # row per UGF with geometry + indicator columns + label/groupe.
      base_sf <- project$indicators_sf
      if (is.null(base_sf) || !inherits(base_sf, "sf") || nrow(base_sf) == 0) {
        return(NULL)
      }

      tryCatch(
        create_family_index(base_sf, method = "mean", na.rm = TRUE),
        error = function(e) {
          cli::cli_warn("Failed to compute family index: {conditionMessage(e)}")
          NULL
        }
      )
    })

    # ================================================================
    # OUTPUT: Project summary
    # ================================================================
    output$project_summary <- shiny::renderUI({
      i18n <- get_i18n(app_state$language)
      project <- app_state$current_project

      if (is.null(project)) {
        return(htmltools::div(
          class = "text-muted",
          i18n$t("no_project")
        ))
      }

      meta <- project$metadata
      nb_parcels <- if (!is.null(project$parcels)) nrow(project$parcels) else 0L
      nb_ugs <- if (!is.null(project$ugs)) nrow(project$ugs) else 0L

      htmltools::div(
        shiny::h5(meta$name),
        if (!is.null(meta$description) && nzchar(meta$description)) {
          shiny::p(meta$description)
        },
        shiny::p(
          class = "small text-muted",
          sprintf("%s: %s", i18n$t("created_at"), meta$created_at)
        ),
        shiny::p(
          class = "small",
          sprintf("%d UGF / %d %s", nb_ugs, nb_parcels,
                  if (nb_parcels <= 1) i18n$t("parcel") else i18n$t("parcels"))
        ),
        shiny::tags$span(
          class = paste0("badge bg-", if (meta$status == "completed") "success" else "secondary"),
          i18n$t(paste0("status_", meta$status))
        )
      )
    })

    # ================================================================
    # OUTPUT: Global score (mean of all family scores)
    # ================================================================
    output$global_score <- shiny::renderUI({
      i18n <- get_i18n(app_state$language)
      sf_data <- family_scores()

      if (is.null(sf_data)) {
        return(htmltools::div(
          class = "text-center text-muted py-4",
          shiny::icon("chart-line", class = "fa-2x"),
          shiny::p(i18n$t("no_data"))
        ))
      }

      family_cols <- grep("^famille_[a-z]", names(sf_data), value = TRUE)
      if (length(family_cols) == 0) {
        return(htmltools::div(class = "text-muted", i18n$t("no_data")))
      }

      # Compute global score: Fibonacci-weighted via NDP system
      df <- sf::st_drop_geometry(sf_data)
      family_means <- vapply(family_cols, function(col) {
        mean(df[[col]], na.rm = TRUE)
      }, numeric(1))
      # NDP depuis les metadonnees du projet (les attributs sf sont perdus par merge)
      ndp_level <- as.integer(app_state$current_project$metadata$ndp_level %||% 0L)
      ndp_result <- nemeton::compute_general_index(family_means, ndp = ndp_level)
      global <- ndp_result$score

      # Color based on score
      score_color <- if (global >= 60) "#228B22" else if (global >= 40) "#FF8C00" else "#DC143C"

      htmltools::div(
        class = "text-center py-3",
        htmltools::div(
          class = "d-flex align-items-center justify-content-center gap-2 mb-1",
          shiny::p(class = "text-muted mb-0", i18n$t("global_score")),
          bslib::popover(
            htmltools::tags$span(
              class = "text-info",
              style = "cursor: help;",
              shiny::icon("circle-info", class = "fa-sm")
            ),
            htmltools::div(
              style = "max-height: 450px; overflow-y: auto; font-size: 0.85rem; line-height: 1.5;",
              # Title
              htmltools::div(
                style = "border-left: 4px solid #4a7c3f; padding: 10px 12px; background: #f4f8f2; margin-bottom: 10px;",
                htmltools::tags$div(
                  style = "font-weight: bold; font-size: 1rem; color: #3a6330; margin-bottom: 2px;",
                  htmltools::HTML("&#9670; "), i18n$t("score_tip_title")
                ),
                htmltools::tags$em(style = "color: #555;", i18n$t("score_tip_subtitle"))
              ),
              # Score display
              htmltools::div(
                class = "text-center",
                style = "margin: 10px 0;",
                htmltools::tags$span(
                  style = paste0("font-size: 2.5rem; font-weight: bold; color: ", score_color, ";"),
                  global
                ),
                htmltools::tags$span(
                  style = "font-size: 1.2rem; color: #888;",
                  "/ 100"
                ),
                htmltools::div(
                  style = "font-size: 0.8rem; color: #888;",
                  i18n$t("score_tip_index_label")
                )
              ),
              # Intro
              htmltools::p(
                style = "margin: 8px 0; text-align: justify;",
                htmltools::HTML(i18n$t("score_tip_intro"))
              ),
              # Section: Ce que l'indice ne dit pas
              htmltools::div(
                style = "background: #eaf1e6; padding: 5px 10px; margin: 10px 0 6px 0; border-radius: 3px; font-weight: bold; color: #3a6330;",
                htmltools::HTML("&#10006; &nbsp;"), i18n$t("score_tip_not_title")
              ),
              htmltools::p(
                style = "margin: 0 0 8px 0; text-align: justify;",
                htmltools::HTML(i18n$t("score_tip_not_text"))
              ),
              # Section: Ce que l'indice dit
              htmltools::div(
                style = "background: #eaf1e6; padding: 5px 10px; margin: 10px 0 6px 0; border-radius: 3px; font-weight: bold; color: #3a6330;",
                htmltools::HTML("&#10004; &nbsp;"), i18n$t("score_tip_yes_title")
              ),
              htmltools::p(
                style = "margin: 0 0 8px 0; text-align: justify;",
                htmltools::HTML(i18n$t("score_tip_yes_text"))
              ),
              # Section: Comment l'utiliser
              htmltools::div(
                style = "background: #eaf1e6; padding: 5px 10px; margin: 10px 0 6px 0; border-radius: 3px; font-weight: bold; color: #3a6330;",
                htmltools::HTML("&#9658; &nbsp;"), i18n$t("score_tip_how_title")
              ),
              htmltools::p(
                style = "margin: 0 0 10px 0; text-align: justify;",
                htmltools::HTML(i18n$t("score_tip_how_text"))
              ),
              # Conclusion
              htmltools::tags$p(
                style = "font-style: italic; color: #555; margin: 10px 0 0 0; padding-top: 8px; border-top: 1px solid #ddd; text-align: justify;",
                htmltools::HTML(i18n$t("score_tip_conclusion"))
              )
            ),
            options = list(customClass = "popover-lg"),
            title = NULL
          )
        ),
        htmltools::div(
          style = paste0(
            "font-size: 4rem; font-weight: bold; color: ", score_color,
            "; line-height: 1;"
          ),
          global
        ),
        shiny::p(
          class = "text-muted mt-1 mb-0",
          sprintf("/ 100 (%d %s)", length(family_cols),
                  if (i18n$language == "fr") "familles" else "families")
        ),
        # NDP badge + popover info + barre de confiance
        htmltools::div(
          class = "mt-2",
          htmltools::div(
            class = "d-flex align-items-center justify-content-center gap-2",
            ndp_badge(ndp_result$ndp, lang = i18n$language),
            bslib::popover(
              htmltools::tags$span(
                class = "text-info",
                style = "cursor: help;",
                shiny::icon("circle-info", class = "fa-sm")
              ),
              htmltools::div(
                style = "max-height: 450px; overflow-y: auto; font-size: 0.85rem; line-height: 1.5;",
                # Titre
                htmltools::div(
                  style = "border-left: 4px solid #4a7c3f; padding: 10px 12px; background: #f4f8f2; margin-bottom: 10px;",
                  htmltools::tags$div(
                    style = "font-weight: bold; font-size: 1rem; color: #3a6330; margin-bottom: 2px;",
                    htmltools::HTML("&#9670; "), i18n$t("ndp_tip_title")
                  ),
                  htmltools::tags$em(style = "color: #555;", i18n$t("ndp_tip_subtitle"))
                ),
                # Introduction
                htmltools::p(
                  style = "margin: 8px 0; text-align: justify;",
                  htmltools::HTML(i18n$t("ndp_tip_intro"))
                ),
                # Section : Les 5 niveaux
                htmltools::div(
                  style = "background: #eaf1e6; padding: 5px 10px; margin: 10px 0 6px 0; border-radius: 3px; font-weight: bold; color: #3a6330;",
                  i18n$t("ndp_tip_levels_title")
                ),
                htmltools::HTML(i18n$t("ndp_tip_levels")),
                # Section : Ponderation Fibonacci
                htmltools::div(
                  style = "background: #eaf1e6; padding: 5px 10px; margin: 10px 0 6px 0; border-radius: 3px; font-weight: bold; color: #3a6330;",
                  htmltools::HTML("&#9733; &nbsp;"), i18n$t("ndp_tip_fibonacci_title")
                ),
                htmltools::p(
                  style = "margin: 0 0 8px 0; text-align: justify;",
                  i18n$t("ndp_tip_fibonacci_text")
                ),
                # Section : Confiance phi
                htmltools::div(
                  style = "background: #eaf1e6; padding: 5px 10px; margin: 10px 0 6px 0; border-radius: 3px; font-weight: bold; color: #3a6330;",
                  htmltools::HTML("&#966; &nbsp;"), i18n$t("ndp_tip_confidence_title")
                ),
                htmltools::p(
                  style = "margin: 0 0 8px 0; text-align: justify;",
                  i18n$t("ndp_tip_confidence_text")
                ),
                # Conclusion
                htmltools::tags$p(
                  style = "font-style: italic; color: #555; margin: 10px 0 0 0; padding-top: 8px; border-top: 1px solid #ddd; text-align: justify;",
                  i18n$t("ndp_tip_conclusion")
                )
              ),
              options = list(customClass = "popover-lg"),
              title = NULL
            )
          ),
          ndp_progress_bar(ndp_result$ndp, lang = i18n$language)
        )
      )
    })

    # ================================================================
    # OUTPUT: Radar plot
    # ================================================================
    output$radar_plot <- shiny::renderPlot({
      i18n <- get_i18n(app_state$language)
      sf_data <- family_scores()

      if (is.null(sf_data)) {
        plot.new()
        text(0.5, 0.5, i18n$t("no_data"), cex = 1.5, col = "gray50")
        return()
      }

      # Check that family columns exist
      family_cols <- grep("^famille_[a-z]", names(sf_data), value = TRUE)
      if (length(family_cols) == 0) {
        plot.new()
        text(0.5, 0.5, i18n$t("no_data"), cex = 1.5, col = "gray50")
        return()
      }

      ndp_level <- as.integer(app_state$current_project$metadata$ndp_level %||% 0L)
      ndp_info <- nemeton::get_ndp_level(ndp_level)
      confidence_pct <- round(ndp_info$confidence * 100, 1)
      ndp_subtitle <- sprintf("NDP %d \u2013 %s | %s : %s%%",
                               ndp_level, ndp_info$name,
                               i18n$t("ndp_confidence"), confidence_pct)

      # 1. Aggregate to single row (mean per family) — 20 parcels = 20 overlapping polygons
      df <- sf::st_drop_geometry(sf_data)
      family_means <- as.data.frame(lapply(df[, family_cols, drop = FALSE],
                                           function(x) mean(x, na.rm = TRUE)))

      # 2. Reorder columns to match nemeton_radar axis order (F,A,W,B,N,C,E,P,S,R,T,L)
      radar_axis_order <- c("F", "A", "W", "B", "N", "C", "E", "P", "S", "R", "T", "L")
      ordered_cols <- vapply(radar_axis_order, get_famille_col, character(1))
      ordered_cols <- intersect(ordered_cols, names(family_means))
      family_means <- family_means[, ordered_cols, drop = FALSE]

      # 3. Wrap as sf (nemeton_radar requires sf)
      family_means_sf <- sf::st_as_sf(
        family_means,
        geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
      )

      p <- nemeton_radar(family_means_sf, mode = "family", normalize = FALSE,
                         title = i18n$t("radar_title"))
      p + ggplot2::labs(subtitle = ndp_subtitle) +
        ggplot2::theme(
          plot.subtitle = ggplot2::element_text(
            hjust = 0.5, size = 11, color = "gray40",
            margin = ggplot2::margin(b = 10)
          )
        )
    })

    # ================================================================
    # OUTPUT: Summary table (one row per family)
    # ================================================================
    output$summary_table <- shiny::renderTable({
      i18n <- get_i18n(app_state$language)
      sf_data <- family_scores()

      if (is.null(sf_data)) return(NULL)

      lang <- app_state$language
      families <- INDICATOR_FAMILIES
      codes <- names(families)

      # Build summary data.frame
      rows <- lapply(codes, function(code) {
        col_name <- get_famille_col(code)
        fam <- families[[code]]
        fam_name <- if (lang == "fr") fam$name_fr else fam$name_en

        if (col_name %in% names(sf_data)) {
          vals <- sf::st_drop_geometry(sf_data)[[col_name]]
          score <- mean(vals, na.rm = TRUE)
        } else {
          score <- NA_real_
        }

        data.frame(
          Family = fam_name,
          Code = code,
          Score = round(score, 2),
          NDP = as.integer(app_state$current_project$metadata$ndp_level %||% 0L),
          Indicators = length(fam$indicators),
          stringsAsFactors = FALSE
        )
      })

      result <- do.call(rbind, rows)

      # Rename columns for display
      col_names <- c(
        i18n$t("famille_carbone"),  # reuse as generic "Family" label
        "Code",
        "Score",
        i18n$t("indicator_column")
      )
      # Simpler: just use standard names
      names(result) <- c(
        if (lang == "fr") "Famille" else "Family",
        "Code",
        "Score",
        "NDP",
        if (lang == "fr") "Nb indicateurs" else "Nb indicators"
      )

      result
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ================================================================
    # AI ANALYSIS: Generate synthesis analysis via ellmer
    # ================================================================
    shiny::observeEvent(input$ai_generate, {
      i18n <- get_i18n(app_state$language)

      # Check API key for providers that require one
      provider <- get_app_config("llm_provider", "anthropic")
      key_var <- get_llm_api_key_var(provider)
      if (!is.null(key_var) && nchar(Sys.getenv(key_var)) == 0) {
        msg <- gsub("\\{key_var\\}", key_var, i18n$t("ai_no_api_key"))
        shiny::showNotification(msg, type = "warning", duration = 8)
        return()
      }

      sf_data <- family_scores()
      if (is.null(sf_data)) return()

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
      prompt <- build_synthesis_prompt(sf_data, language)
      expert <- input$expert_profile %||% "generalist"
      system_prompt <- build_system_prompt(language, expert = expert)

      synthesis_response <- tryCatch({
        chat <- create_llm_chat(system_prompt)
        resp <- as.character(chat$chat(prompt, echo = FALSE))

        shiny::updateTextAreaInput(session, "synthesis_comments", value = resp)
        shiny::removeNotification(notif_id)
        resp
      }, error = function(e) {
        shiny::removeNotification(notif_id)
        shiny::showNotification(
          paste(i18n$t("ai_error"), ":", strip_ansi(conditionMessage(e))),
          type = "error",
          duration = 8
        )
        NULL
      })

      # Fill all family comments if switch is checked
      family_comments_local <- as.list(shiny::isolate(app_state$family_comments))
      failed_families <- character(0)
      skipped_families <- character(0)
      if (isTRUE(input$fill_all_comments)) {
        all_indicators <- project_indicators()
        if (!is.null(all_indicators)) {
          family_codes <- get_family_codes()

          for (i in seq_along(family_codes)) {
            fc <- family_codes[i]
            fam_config <- get_family_config(fc)
            fam_name <- if (identical(app_state$language, "fr")) fam_config$name_fr else fam_config$name_en

            # Notification with progress
            shiny::removeNotification(notif_id)
            notif_id <- shiny::showNotification(
              htmltools::div(shiny::icon("spinner", class = "fa-spin me-2"),
                             sprintf("%s (%d/%d)...", fam_name, i, length(family_codes))),
              type = "message", duration = NULL)

            # Extract family indicator data (same logic as mod_family.R)
            all_cols <- names(all_indicators)
            candidates <- c(fam_config$indicators, fam_config$column_names)
            matched <- character(0)
            for (col in candidates) {
              norm_col <- paste0(col, "_norm")
              if (norm_col %in% all_cols) matched <- c(matched, norm_col)
              else if (col %in% all_cols) matched <- c(matched, col)
            }
            matched <- unique(matched)
            if (length(matched) == 0) {
              cli::cli_warn("Skipping family {fc} ({fam_name}): no matching indicator columns")
              skipped_families <- c(skipped_families, fam_name)
              next
            }

            meta_cols <- intersect(c("ug_id", "label", "groupe"), all_cols)
            fam_ind_data <- all_indicators[, c(meta_cols, matched), drop = FALSE]

            # Generate LLM comment. Retry once on transient errors
            # (rate-limit 429 / timeout / empty response) — Mistral's
            # free tier in particular throttles bursts of consecutive
            # requests, which is exactly what fill-all does.
            fam_prompt <- build_analysis_prompt(fam_config, fam_ind_data, language)
            fam_response <- NULL
            for (attempt in seq_len(2L)) {
              fam_response <- tryCatch({
                fam_chat <- create_llm_chat(system_prompt)
                out <- as.character(fam_chat$chat(fam_prompt, echo = FALSE))
                # Treat empty / whitespace-only responses as a failure
                # so the retry kicks in — without this, nchar == 0 would
                # overwrite a previously-good comment with an empty
                # string and the family tab would simply show nothing.
                if (length(out) == 0L || !any(nzchar(trimws(out)))) {
                  stop("Empty response from LLM", call. = FALSE)
                }
                paste(out, collapse = "\n")
              }, error = function(e) {
                cli::cli_warn(
                  "AI attempt {attempt}/2 failed for family {fc} ({fam_name}): {conditionMessage(e)}"
                )
                NULL
              })
              if (!is.null(fam_response)) break
              if (attempt < 2L) Sys.sleep(1)  # tiny backoff before retry
            }

            if (!is.null(fam_response)) {
              family_comments_local[[fc]] <- fam_response
              cli::cli_alert_success("AI comment generated for family {fc} ({fam_name})")
            } else {
              failed_families <- c(failed_families, fam_name)
            }
          }
          shiny::removeNotification(notif_id)

          # Commit all family comments to app_state in a single atomic
          # write. Per-iteration deep assignments (app_state$family_comments[[fc]] <- ...)
          # used to invalidate observers bound to app_state$family_comments
          # mid-loop and occasionally lost the most recent comment. Writing
          # the full list at the end keeps the reactive graph in a single
          # consistent state and fires mod_family observers once.
          app_state$family_comments <- family_comments_local
          app_state$refresh_family_comments <- Sys.time()

          # Surface failures to the user — the old code only logged to the
          # console, so a failed family (e.g. Mistral 429 rate limit) looked
          # indistinguishable from a successful blank comment.
          if (length(failed_families) > 0L || length(skipped_families) > 0L) {
            msg_parts <- character(0)
            if (length(failed_families) > 0L) {
              msg_parts <- c(
                msg_parts,
                if (identical(app_state$language, "fr")) {
                  sprintf("\u00c9chec IA : %s", paste(failed_families, collapse = ", "))
                } else {
                  sprintf("AI failed: %s", paste(failed_families, collapse = ", "))
                }
              )
            }
            if (length(skipped_families) > 0L) {
              msg_parts <- c(
                msg_parts,
                if (identical(app_state$language, "fr")) {
                  sprintf("Sans donn\u00e9es : %s", paste(skipped_families, collapse = ", "))
                } else {
                  sprintf("No data: %s", paste(skipped_families, collapse = ", "))
                }
              )
            }
            shiny::showNotification(
              paste(msg_parts, collapse = " \u2014 "),
              type = "warning", duration = 12
            )
          }
        }
      }

      # Save comments to disk using local variables (not reactive values)
      # to avoid async issues with Shiny's reactive system
      project_id <- app_state$project_id
      n_fam <- length(family_comments_local)
      has_syn <- !is.null(synthesis_response)
      cli::cli_inform("Saving comments: project_id={project_id}, synthesis={has_syn}, families={n_fam}")
      if (!is.null(project_id)) {
        save_comments(project_id,
                      synthesis = synthesis_response,
                      families = family_comments_local)
      } else {
        cli::cli_warn("Cannot save comments: project_id is NULL")
      }

      # Restore button
      shiny::updateActionButton(session, "ai_generate",
                                label = i18n$t("ai_generate"),
                                icon = shiny::icon("robot"))
    })

    # ================================================================
    # OBSERVER: Save synthesis comment on manual edit (debounced)
    # ================================================================
    shiny::observeEvent(input$synthesis_comments, {
      project_id <- app_state$project_id
      if (!is.null(project_id)) {
        save_comments(project_id,
                      synthesis = input$synthesis_comments,
                      families = app_state$family_comments)
      }
    }, ignoreInit = TRUE)

    # ================================================================
    # OBSERVER: Restore comments when project loads
    # ================================================================
    shiny::observeEvent(app_state$current_project, {
      project <- app_state$current_project

      # Always reset comments first to avoid stale data from previous project
      shiny::updateTextAreaInput(session, "synthesis_comments", value = "")
      app_state$family_comments <- list()

      # Then restore from new project if available
      if (!is.null(project$comments)) {
        if (!is.null(project$comments$synthesis) &&
            nchar(project$comments$synthesis) > 0) {
          shiny::updateTextAreaInput(session, "synthesis_comments",
                                     value = project$comments$synthesis)
        }
        if (is.list(project$comments$families)) {
          app_state$family_comments <- project$comments$families
        }
      }

      # Signal family modules to reload (cleared or restored)
      app_state$refresh_family_comments <- Sys.time()
    }, ignoreInit = TRUE)

    # ================================================================
    # OBSERVER: Clear comments when commune/department changes
    # ================================================================
    shiny::observeEvent(app_state$clear_all_comments, {
      shiny::updateTextAreaInput(session, "synthesis_comments", value = "")
    }, ignoreInit = TRUE)

    # ================================================================
    # DOWNLOAD: GeoPackage export
    # ================================================================
    output$download_gpkg <- shiny::downloadHandler(
      filename = function() {
        project <- app_state$current_project
        name <- if (!is.null(project$metadata$name)) {
          gsub("[^a-zA-Z0-9_-]", "_", project$metadata$name)
        } else {
          "nemeton_export"
        }
        paste0(name, ".gpkg")
      },
      content = function(file) {
        sf_data <- family_scores()
        if (is.null(sf_data)) {
          # Write empty file
          writeLines("No data available", file)
          return()
        }

        sf::st_write(sf_data, file, driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
      }
    )

    # ================================================================
    # REACTIVE: Cover image path
    # ================================================================
    cover_image_path <- shiny::reactive({
      img <- input$cover_image
      if (is.null(img)) return(NULL)
      img$datapath
    })

    # ================================================================
    # DOWNLOAD: PDF report
    # ================================================================
    output$download_pdf <- shiny::downloadHandler(
      filename = function() {
        project <- app_state$current_project
        name <- if (!is.null(project$metadata$name)) {
          gsub("[^a-zA-Z0-9_-]", "_", project$metadata$name)
        } else {
          "nemeton_report"
        }
        paste0(name, "_report.pdf")
      },
      content = function(file) {
        i18n <- get_i18n(app_state$language)
        sf_data <- family_scores()
        project <- app_state$current_project

        if (is.null(sf_data) || is.null(project)) {
          shiny::showNotification(
            i18n$t("no_data"),
            type = "warning"
          )
          writeLines("No data available", file)
          return()
        }

        # Show progress notification
        notif_id <- shiny::showNotification(
          htmltools::div(
            shiny::icon("spinner", class = "fa-spin me-2"),
            if (app_state$language == "fr") "Generation du rapport PDF..." else "Generating PDF report..."
          ),
          type = "message",
          duration = NULL
        )

        tryCatch({
          # Get synthesis comments if available
          comments <- input$synthesis_comments
          if (!is.null(comments) && nchar(trimws(comments)) == 0) {
            comments <- NULL
          }

          # Get cover image if uploaded
          cover_img <- cover_image_path()

          # Collect family comments from app_state
          fam_comments <- app_state$family_comments
          if (!is.null(fam_comments)) {
            # Clean empty comments
            fam_comments <- lapply(fam_comments, function(x) {
              if (is.null(x) || nchar(trimws(x)) == 0) NULL else x
            })
            # Remove NULL entries
            fam_comments <- fam_comments[!vapply(fam_comments, is.null, logical(1))]
            if (length(fam_comments) == 0) fam_comments <- NULL
          }

          # Generate PDF
          generate_report_pdf(
            project = project,
            family_scores = sf_data,
            output_file = file,
            language = app_state$language,
            synthesis_comments = comments,
            family_comments = fam_comments,
            cover_image = cover_img,
            use_quarto = TRUE
          )

          # Also save a copy in the project's exports/ directory
          project_path <- project$path
          if (!is.null(project_path) && dir.exists(project_path)) {
            exports_dir <- file.path(project_path, "exports")
            if (!dir.exists(exports_dir)) {
              dir.create(exports_dir, showWarnings = FALSE)
            }
            export_name <- gsub("[^a-zA-Z0-9_-]", "_", project$metadata$name %||% "nemeton_report")
            export_file <- file.path(exports_dir, paste0(export_name, "_report.pdf"))
            file.copy(file, export_file, overwrite = TRUE)
          }

          shiny::removeNotification(notif_id)
          shiny::showNotification(
            if (app_state$language == "fr") "Rapport PDF genere avec succes" else "PDF report generated successfully",
            type = "message",
            duration = 3
          )
        }, error = function(e) {
          shiny::removeNotification(notif_id)
          shiny::showNotification(
            paste(i18n$t("error"), ":", conditionMessage(e)),
            type = "error",
            duration = 8
          )
          writeLines(paste("Error generating report:", conditionMessage(e)), file)
        })
      }
    )

  })
}

#' Export Services
#'
#' @description
#' Functions for exporting nemeton project data and generating reports.
#'
#' @name service_export
#' @keywords internal
NULL


#' Check if Quarto is installed
#'
#' @description
#' Checks if both Quarto CLI and the quarto R package are available and working.
#'
#' @return Logical. TRUE if Quarto is fully available.
#' @noRd
is_quarto_installed <- function() {
  # Check R package first
  if (!requireNamespace("quarto", quietly = TRUE)) {
    return(FALSE)
  }

  # Check CLI exists
  quarto_path <- Sys.which("quarto")
  if (nchar(quarto_path) == 0) {
    return(FALSE)
  }

  # Verify CLI actually works
 tryCatch({
    result <- system2("quarto", "--version", stdout = TRUE, stderr = TRUE)
    return(length(result) > 0 && !any(grepl("error|not found", result, ignore.case = TRUE)))
  }, error = function(e) {
    return(FALSE)
  })
}


#' Ensure Quarto is installed
#'
#' @description
#' Checks if Quarto is installed.
#'
#' @return Logical. TRUE if Quarto is available.
#' @noRd
ensure_quarto_installed <- function() {
  is_quarto_installed()
}


#' Generate PDF report for a project
#'
#' @description
#' Generates a PDF report using Quarto with all project data,
#' including radar chart, family scores, and indicator maps.
#'
#' @param project List. The project object with metadata, parcels, and indicators.
#' @param family_scores sf. Family scores computed by create_family_index().
#' @param output_file Character. Path to output PDF file.
#' @param language Character. Language code ("fr" or "en").
#' @param synthesis_comments Character. Optional synthesis comments to include.
#' @param family_comments Named list. Optional comments per family (keyed by family code).
#' @param cover_image Character. Optional path to cover image for first page.
#'
#' @return Character. Path to generated PDF file, or NULL on failure.
#'
#' @noRd
generate_pdf_report <- function(project,
                                family_scores,
                                output_file,
                                language = "fr",
                                synthesis_comments = NULL,
                                family_comments = NULL,
                                cover_image = NULL) {
  # Check Quarto installation
  if (!ensure_quarto_installed()) {
    stop("Quarto is required for PDF report generation but is not available.",
         call. = FALSE)
  }

  # Create temporary directory for report generation
  temp_dir <- tempfile("nemeton_report_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Copy template to temp directory
  template_path <- system.file("quarto", "report_template.qmd", package = "nemeton")
  if (!file.exists(template_path)) {
    stop("Report template not found. Package may be incomplete.", call. = FALSE)
  }

  report_qmd <- file.path(temp_dir, "report.qmd")
  file.copy(template_path, report_qmd)

  # Set title/subtitle in the copied qmd (inline R in YAML not supported in Quarto 1.8+)
  qmd_content <- readLines(report_qmd, encoding = "UTF-8")
  report_title <- if (language == "fr") "Diagnostic Forestier N\u00e9meton" else "N\u00e9meton Forest Diagnostic"
  report_subtitle <- if (language == "fr") "Rapport de synth\u00e8se" else "Synthesis Report"
  qmd_content <- gsub('^title: "Diagnostic Forestier Nemeton"$',
                       sprintf('title: "%s"', report_title), qmd_content)
  qmd_content <- gsub('^subtitle: "Rapport de synthese"$',
                       sprintf('subtitle: "%s"', report_subtitle), qmd_content)
  report_lang <- if (language == "fr") "fr" else "en"
  qmd_content <- gsub('^lang: en$', sprintf('lang: %s', report_lang), qmd_content)
  writeLines(qmd_content, report_qmd, useBytes = TRUE)

  # Create directory for family maps
  family_maps_dir <- file.path(temp_dir, "family_maps")
  dir.create(family_maps_dir, recursive = TRUE)

  # Generate family map images
  generate_family_maps(family_scores, family_maps_dir, language)

  # Prepare data for the report
  report_data <- prepare_report_data(project, family_scores, language,
                                     synthesis_comments, family_comments)

  # Save data as RDS for the template to load
  data_file <- file.path(temp_dir, "report_data.rds")
  saveRDS(report_data, data_file)

  # Generate radar plot as static image
  radar_file <- file.path(temp_dir, "radar_plot.png")
  generate_radar_image(family_scores, radar_file, language,
                       ndp_level = report_data$ndp_level)

  # Handle cover image: copy to temp_dir so relative path works
  cover_image_param <- ""
  if (!is.null(cover_image) && file.exists(cover_image)) {
    ext <- tolower(tools::file_ext(cover_image))
    cover_dest <- file.path(temp_dir, paste0("cover_image.", ext))
    file.copy(cover_image, cover_dest, overwrite = TRUE)
    cover_image_param <- cover_dest
  }

  # Render with Quarto
  tryCatch({
    quarto::quarto_render(
      input = report_qmd,
      output_format = "pdf",
      execute_params = list(
        data_file = data_file,
        radar_file = radar_file,
        family_maps_dir = family_maps_dir,
        language = language,
        cover_image = cover_image_param
      ),
      quiet = FALSE
    )

    # Find and copy output file
    pdf_output <- sub("\\.qmd$", ".pdf", report_qmd)
    if (file.exists(pdf_output)) {
      file.copy(pdf_output, output_file, overwrite = TRUE)
      return(output_file)
    } else {
      stop("PDF file was not generated")
    }
  }, error = function(e) {
    message("Quarto render error: ", conditionMessage(e))
    NULL
  })
}


#' Prepare report data structure
#'
#' @description
#' Prepares all data needed for the report template.
#'
#' @param project List. Project object.
#' @param family_scores sf. Family scores.
#' @param language Character. Language code.
#' @param synthesis_comments Character. Optional synthesis comments.
#' @param family_comments Named list. Optional comments per family.
#'
#' @return List with all report data.
#' @noRd
prepare_report_data <- function(project, family_scores, language,
                                synthesis_comments = NULL,
                                family_comments = NULL) {
  i18n <- get_i18n(language)

  # Drop geometry for data processing
  scores_df <- if (inherits(family_scores, "sf")) {
    sf::st_drop_geometry(family_scores)
  } else {
    family_scores
  }

  # Get family columns
  family_cols <- grep("^famille_[a-z]", names(scores_df), value = TRUE)

  # Calculate family statistics
  family_stats <- lapply(family_cols, function(col) {
    code <- get_famille_code(col)
    fam <- INDICATOR_FAMILIES[[code]]
    vals <- scores_df[[col]]

    list(
      code = code,
      name = if (language == "fr") fam$name_fr else fam$name_en,
      mean = round(mean(vals, na.rm = TRUE), 1),
      min = round(min(vals, na.rm = TRUE), 1),
      max = round(max(vals, na.rm = TRUE), 1),
      sd = round(sd(vals, na.rm = TRUE), 1),
      color = fam$color,
      icon = fam$icon
    )
  })
  names(family_stats) <- vapply(family_cols, get_famille_code, character(1))

  # Build family descriptions from INDICATOR_FAMILIES
  family_descriptions <- lapply(names(INDICATOR_FAMILIES), function(code) {
    fam <- INDICATOR_FAMILIES[[code]]
    indicators <- fam$indicators
    indicator_names <- vapply(indicators, function(ind) {
      labels <- fam$indicator_labels[[ind]]
      if (language == "fr") labels$fr else labels$en
    }, character(1))

    if (language == "fr") {
      sprintf("Cette famille regroupe %d indicateurs : %s.",
              length(indicators), paste(indicator_names, collapse = ", "))
    } else {
      sprintf("This family includes %d indicators: %s.",
              length(indicators), paste(indicator_names, collapse = ", "))
    }
  })
  names(family_descriptions) <- names(INDICATOR_FAMILIES)

  # Calculate indicator statistics for each family
  indicator_stats <- lapply(names(INDICATOR_FAMILIES), function(code) {
    fam <- INDICATOR_FAMILIES[[code]]
    # Find indicator columns in data
    ind_data <- data.frame(
      Indicateur = character(),
      Moyenne = numeric(),
      Min = numeric(),
      Max = numeric(),
      stringsAsFactors = FALSE
    )

    for (i in seq_along(fam$indicators)) {
      ind_code <- fam$indicators[i]
      col_name <- fam$column_names[i]

      if (col_name %in% names(scores_df)) {
        vals <- scores_df[[col_name]]
        labels <- fam$indicator_labels[[ind_code]]
        label <- if (language == "fr") labels$fr else labels$en

        ind_data <- rbind(ind_data, data.frame(
          Indicateur = paste0(ind_code, " - ", label),
          Moyenne = round(mean(vals, na.rm = TRUE), 1),
          Min = round(min(vals, na.rm = TRUE), 1),
          Max = round(max(vals, na.rm = TRUE), 1),
          stringsAsFactors = FALSE
        ))
      }
    }

    # Rename columns for language
    if (language == "en") {
      names(ind_data) <- c("Indicator", "Mean", "Min", "Max")
    }

    ind_data
  })
  names(indicator_stats) <- names(INDICATOR_FAMILIES)

  # Global score
  family_means <- vapply(family_cols, function(col) {
    mean(scores_df[[col]], na.rm = TRUE)
  }, numeric(1))
  global_score <- round(mean(family_means, na.rm = TRUE), 1)

  # Metadata
  meta <- project$metadata
  n_parcels <- nrow(family_scores)

  # Ensure family_comments is a named list with all family codes
  if (is.null(family_comments)) {
    family_comments <- stats::setNames(
      rep("", length(INDICATOR_FAMILIES)),
      names(INDICATOR_FAMILIES)
    )
  }

  # Build family colors mapping for template
  family_colors <- vapply(names(INDICATOR_FAMILIES), function(code) {
    INDICATOR_FAMILIES[[code]]$color
  }, character(1))

  # Build per-parcel score data for detailed tables
  parcel_scores <- list()
  for (code in names(INDICATOR_FAMILIES)) {
    fam_col <- get_famille_col(code)
    if (fam_col %in% names(scores_df)) {
      parcel_ids <- if ("id" %in% names(scores_df)) {
        as.character(scores_df[["id"]])
      } else {
        as.character(seq_len(nrow(scores_df)))
      }
      # Use last 6 chars of ID for display
      short_ids <- substr(parcel_ids, pmax(1, nchar(parcel_ids) - 5), nchar(parcel_ids))
      parcel_scores[[code]] <- data.frame(
        id = short_ids,
        score = round(scores_df[[fam_col]], 1),
        stringsAsFactors = FALSE
      )
    }
  }

  # Calculer les donnees NDP
  ndp_level <- detect_ndp(family_scores)
  ndp_info <- get_ndp_level(ndp_level)
  ndp_result <- compute_general_index(
    vapply(family_stats, `[[`, numeric(1), "mean"),
    ndp = ndp_level
  )

  list(
    # Metadata
    project_name = meta$name,
    project_description = meta$description,
    project_owner = meta$owner,
    created_at = meta$created_at,
    n_parcels = n_parcels,

    # Scores
    global_score = ndp_result$score,
    family_stats = family_stats,
    family_colors = family_colors,
    parcel_scores = parcel_scores,

    # NDP
    ndp_level = ndp_level,
    ndp_name = ndp_info$name,
    ndp_confidence = ndp_info$confidence,
    ndp_weight = ndp_info$fibonacci,

    # Family details for extended report
    family_descriptions = family_descriptions,
    indicator_stats = indicator_stats,

    # Comments (markdown)
    synthesis_comments = synthesis_comments,
    family_comments = family_comments,

    # Language
    language = language,

    # Translations
    labels = list(
      title = if (language == "fr") "Diagnostic Forestier N\u00e9meton" else "N\u00e9meton Forest Diagnostic",
      subtitle = if (language == "fr") "Rapport de synth\u00e8se" else "Synthesis Report",
      project_info = i18n$t("project_info"),
      project_name = if (language == "fr") "Nom du projet" else "Project name",
      owner = if (language == "fr") "Propri\u00e9taire" else "Owner",
      global_score_label = i18n$t("global_score"),
      parcels = if (n_parcels > 1) i18n$t("parcels") else i18n$t("parcel"),
      created_at = i18n$t("created_at"),
      family_scores = if (language == "fr") "Scores par famille" else "Scores by family",
      radar_title = i18n$t("radar_title"),
      comments_title = i18n$t("comments_title"),
      generated_by = if (language == "fr") "G\u00e9n\u00e9r\u00e9 par nemeton" else "Generated by nemeton",
      ndp_label = if (language == "fr") "Niveau de Pr\u00e9cision" else "Precision Level",
      ndp_confidence_label = if (language == "fr") "Confiance \u03c6" else "Confidence \u03c6"
    )
  )
}


#' Generate radar plot as PNG image
#'
#' @param family_scores sf. Family scores data.
#' @param output_file Character. Path to output PNG file.
#' @param language Character. Language code.
#'
#' @return Invisible NULL.
#' @noRd
generate_radar_image <- function(family_scores, output_file, language,
                                 ndp_level = 0L) {
  grDevices::png(output_file, width = 1600, height = 1600, res = 150)
  on.exit(grDevices::dev.off(), add = TRUE)

  i18n <- get_i18n(language)

  # Construire le sous-titre NDP
  ndp_level <- as.integer(ndp_level %||% 0L)
  ndp_info <- get_ndp_level(ndp_level)
  confidence_pct <- round(ndp_info$confidence * 100, 1)
  ndp_subtitle <- sprintf("NDP %d \u2013 %s | Confiance \u03c6 : %s%%",
                           ndp_level, ndp_info$name, confidence_pct)

  tryCatch({
    p <- nemeton_radar(family_scores, mode = "family", normalize = FALSE,
                       title = i18n$t("radar_title"))
    # Ajouter le sous-titre NDP
    p <- p + ggplot2::labs(subtitle = ndp_subtitle) +
      ggplot2::theme(
        plot.subtitle = ggplot2::element_text(
          hjust = 0.5, size = 11, color = "gray40",
          margin = ggplot2::margin(b = 10)
        )
      )
    print(p)
  }, error = function(e) {
    # Fallback: empty plot with message
    plot.new()
    text(0.5, 0.5, "Radar plot unavailable", cex = 1.5)
  })

  invisible(NULL)
}


#' Generate family map images
#'
#' @description
#' Generates a map image for each indicator family showing
#' the spatial distribution of scores.
#'
#' @param family_scores sf. Family scores with geometry.
#' @param output_dir Character. Directory to save map images.
#' @param language Character. Language code.
#'
#' @return Invisible NULL.
#' @noRd
generate_family_maps <- function(family_scores, output_dir, language) {
  if (!inherits(family_scores, "sf")) {
    return(invisible(NULL))
  }

  # Get family columns present in data
  family_cols <- grep("^famille_[a-z]", names(family_scores), value = TRUE)

  has_maptiles <- requireNamespace("maptiles", quietly = TRUE)

  # Pre-download OSM tiles once (shared across all family maps)
  tiles <- NULL
  data_proj <- NULL
  if (has_maptiles) {
    tryCatch({
      data_wgs84 <- sf::st_transform(family_scores, 4326)
      bbox <- sf::st_bbox(data_wgs84)
      extent_size <- max(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"])
      auto_zoom <- min(17, max(13, round(17 - log2(extent_size * 100))))
      tiles <- maptiles::get_tiles(data_wgs84, provider = "OpenTopoMap",
                                   zoom = auto_zoom, crop = TRUE, cachedir = tempdir())
      data_proj <- sf::st_transform(family_scores, sf::st_crs(tiles))
    }, error = function(e) {
      cli::cli_warn("Failed to download OSM tiles: {conditionMessage(e)}")
    })
  }

  for (col in family_cols) {
    code <- get_famille_code(col)
    fam <- INDICATOR_FAMILIES[[code]]

    if (is.null(fam)) next

    output_file <- file.path(output_dir, paste0(get_famille_col(code), "_map.png"))

    tryCatch({
      grDevices::png(output_file, width = 800, height = 600, res = 150)

      # Get family info
      family_name <- if (language == "fr") fam$name_fr else fam$name_en
      family_color <- fam$color
      map_title <- paste0(family_name, " (", code, ")")

      # Create color palette from white to family color
      vals <- family_scores[[col]]
      if (all(is.na(vals))) {
        plot.new()
        graphics::text(0.5, 0.5,
                       if (language == "fr") "Donn\u00e9es non disponibles" else "Data not available",
                       cex = 1.2)
        grDevices::dev.off()
        next
      }

      n_breaks <- 5
      breaks <- seq(0, 100, length.out = n_breaks + 1)
      legend_labels <- paste0(utils::head(breaks, -1), "-", utils::tail(breaks, -1))

      if (!is.null(tiles)) {
        # OSM background map
        graphics::par(mar = c(4, 4, 3, 2))
        maptiles::plot_tiles(tiles)

        colors_transparent <- grDevices::colorRampPalette(c(
          grDevices::adjustcolor("#f0f0f0", alpha.f = 0.75),
          grDevices::adjustcolor(family_color, alpha.f = 0.85)
        ))(n_breaks)

        val_class <- cut(vals, breaks = breaks, include.lowest = TRUE, labels = FALSE)
        fill_colors <- colors_transparent[val_class]
        fill_colors[is.na(fill_colors)] <- grDevices::adjustcolor("#cccccc", alpha.f = 0.7)

        plot(sf::st_geometry(data_proj), col = fill_colors, border = "gray20",
             lwd = 1.2, add = TRUE)

        add_parcel_labels(data_proj)
        graphics::title(main = map_title, cex.main = 1.2, font.main = 2)

        # Legend
        colors_opaque <- grDevices::colorRampPalette(c("#f0f0f0", family_color))(n_breaks)
        old_par <- graphics::par(no.readonly = TRUE)
        graphics::par(fig = c(0, 0.25, 0, 0.35), new = TRUE, mar = c(0, 0, 0, 0))
        plot.new()
        graphics::legend("center",
                         legend = legend_labels,
                         fill = colors_opaque,
                         title = "Score",
                         cex = 0.8,
                         bty = "o",
                         bg = "white",
                         box.col = "gray40")
        graphics::par(fig = old_par$fig, new = FALSE, mar = old_par$mar)

        graphics::mtext("(c) OpenTopoMap", side = 1, line = 2, adj = 1, cex = 0.5, col = "gray50")
        graphics::par(mar = c(5.1, 4.1, 4.1, 2.1))
      } else {
        # Fallback: simple sf plot
        colors <- grDevices::colorRampPalette(c("#f0f0f0", family_color))(n_breaks)
        val_class <- cut(vals, breaks = breaks, include.lowest = TRUE, labels = FALSE)
        fill_colors <- colors[val_class]
        fill_colors[is.na(fill_colors)] <- "#cccccc"

        plot(sf::st_geometry(family_scores),
             col = fill_colors,
             border = "gray40",
             lwd = 0.5,
             main = map_title)

        graphics::legend("bottomright",
                         legend = legend_labels,
                         fill = colors,
                         title = "Score",
                         cex = 0.7,
                         bty = "n")
      }

      grDevices::dev.off()

    }, error = function(e) {
      tryCatch(grDevices::dev.off(), error = function(e2) NULL)
      cli::cli_warn("Failed to generate map for family {code}: {conditionMessage(e)}")
    })
  }

  invisible(NULL)
}


#' Export project data to GeoPackage
#'
#' @description
#' Exports the project data with all indicators and family scores
#' to a GeoPackage file.
#'
#' @param family_scores sf. Family scores with geometry.
#' @param output_file Character. Path to output .gpkg file.
#' @param layer_name Character. Layer name in GeoPackage.
#'
#' @return Character. Path to created file.
#'
#' @noRd
export_geopackage <- function(family_scores, output_file, layer_name = "nemeton_results") {
  if (!inherits(family_scores, "sf")) {
    stop("family_scores must be an sf object", call. = FALSE)
  }

  sf::st_write(
    family_scores,
    output_file,
    layer = layer_name,
    driver = "GPKG",
    delete_dsn = TRUE,
    quiet = TRUE
  )

  output_file
}


#' Clean text for PDF output
#'
#' @description
#' Cleans text by replacing problematic Unicode characters with ASCII equivalents.
#' Does NOT strip markdown formatting - use render_markdown_text for that.
#'
#' @param text Character. Text to clean.
#' @return Character. Cleaned text.
#' @noRd
clean_text_for_pdf <- function(text) {
  if (is.null(text) || !nchar(text)) return(text)

  # Replace curly quotes with straight quotes
  text <- gsub("\u2018", "'", text)  # Left single quote
  text <- gsub("\u2019", "'", text)  # Right single quote (apostrophe)
  text <- gsub("\u201C", "\"", text) # Left double quote
  text <- gsub("\u201D", "\"", text) # Right double quote

  # Replace dashes
  text <- gsub("\u2013", "-", text)  # En dash
  text <- gsub("\u2014", "-", text)  # Em dash

  # Replace other common Unicode
  text <- gsub("\u2026", "...", text) # Ellipsis
  text <- gsub("\u00A0", " ", text)   # Non-breaking space

  text
}


#' Render markdown text in base R graphics
#'
#' @description
#' Renders text with markdown formatting in base R graphics.
#' Supports: bold (**), italic (*), strikethrough (~~), inline code (`),
#' bullet lists (-), numbered lists (1.), blockquotes (>), headers (#).
#'
#' @param text Character. Text with markdown formatting.
#' @param x Numeric. X position.
#' @param y Numeric. Starting Y position.
#' @param cex Numeric. Character expansion.
#' @param col Character. Text color.
#' @param adj Numeric. Text adjustment (0 = left, 0.5 = center, 1 = right).
#' @param line_height Numeric. Line height multiplier.
#' @param width Integer. Maximum characters per line for wrapping.
#'
#' @return Numeric. Final Y position after rendering.
#' @noRd
render_markdown_text <- function(text, x, y, cex = 0.7, col = "black",
                                  adj = 0, line_height = 0.025, width = 95) {
  if (is.null(text) || !nchar(text)) return(y)

  # Clean Unicode
  text <- clean_text_for_pdf(text)

  # Clean HTML tags that LLMs sometimes generate
  text <- gsub("<br\\s*/?>", "\n", text, ignore.case = TRUE)
  text <- gsub("-->", " ", text)

  # Pre-process: add space after closing bold/italic markers if followed by non-space
  # This fixes text like "**bold**text" -> "**bold** text"
  # Also handles "**bold**:text" -> "**bold**: text" and "**bold:text" stays as-is
  text <- gsub("\\*\\*([^*]+)\\*\\*([^[:space:]])", "**\\1** \\2", text)
  text <- gsub("(?<!\\*)\\*([^*]+)\\*([^*[:space:]])", "*\\1* \\2", text, perl = TRUE)

  # Split into lines first
  lines <- strsplit(text, "\n")[[1]]

  i <- 1
  while (i <= length(lines)) {
    if (y < 0.05) {
      plot.new()
      y <- 0.92
    }

    line <- lines[i]

    # Skip empty lines (paragraph break)
    if (!nchar(trimws(line))) {
      y <- y - line_height * 0.5
      i <- i + 1
      next
    }

    # Check for markdown headers (# ## ###)
    if (grepl("^#{1,3}\\s*", line) && grepl("^#{1,3}[^#]", line)) {
      level <- nchar(gsub("^(#{1,3}).*", "\\1", line))
      header_text <- gsub("^#{1,3}\\s*", "", line)
      header_text <- strip_markdown_for_display(header_text)
      header_cex <- cex * (1.3 - level * 0.1)
      graphics::text(x, y, header_text, cex = header_cex, col = col, adj = adj, font = 2)
      y <- y - line_height * 1.5
      i <- i + 1
      next
    }

    # Check for markdown table (lines starting with |)
    if (grepl("^\\|", line)) {
      # Collect all consecutive table lines
      table_lines <- character(0)
      while (i <= length(lines) && grepl("^\\|", lines[i])) {
        table_lines <- c(table_lines, lines[i])
        i <- i + 1
      }

      # Filter out separator lines (|---|---|, | :--- | ---: |, etc.)
      is_separator <- grepl("^\\|[\\s:|-]+\\|\\s*$", table_lines, perl = TRUE)
      data_lines <- table_lines[!is_separator]

      if (length(data_lines) > 0) {
        # Parse cells from each line
        parse_row <- function(row_line) {
          # Clean HTML tags: <br>, <br/>, <br />, and --> arrows
          row_line <- gsub("<br\\s*/?>", " ", row_line, ignore.case = TRUE)
          row_line <- gsub("-->", " ", row_line)
          row_line <- gsub("->", " ", row_line)
          cells <- strsplit(row_line, "\\|")[[1]]
          cells <- trimws(cells)
          # Remove empty first/last elements from leading/trailing |
          cells <- cells[nzchar(cells)]
          cells
        }

        header_cells <- parse_row(data_lines[1])
        n_cols <- length(header_cells)

        if (n_cols > 0) {
          # Calculate column layout using actual rendered text widths
          table_width <- 0.90  # available width
          col_gap <- 0.02      # gap between columns
          all_cells <- lapply(data_lines, parse_row)

          # Compute minimum width per column = widest single word (bold)
          # This ensures no word is forced to overflow its column
          min_widths <- rep(0, n_cols)
          for (row_cells in all_cells) {
            for (ci in seq_along(row_cells)) {
              if (ci <= n_cols) {
                cell_text <- strip_markdown_for_display(row_cells[ci])
                words <- strsplit(cell_text, "\\s+")[[1]]
                for (w in words) {
                  ww <- graphics::strwidth(w, cex = cex, font = 2)
                  min_widths[ci] <- max(min_widths[ci], ww)
                }
              }
            }
          }
          # Add small padding to minimum widths
          min_widths <- min_widths + 0.005

          # Available width after gaps
          total_gaps <- col_gap * (n_cols - 1)
          usable_width <- table_width - total_gaps

          # Start with minimum widths, distribute remaining space proportionally
          # based on average content length (character count)
          col_widths <- min_widths
          remaining <- usable_width - sum(min_widths)
          if (remaining > 0) {
            # Weight by average character count per column
            avg_chars <- rep(1, n_cols)
            for (row_cells in all_cells) {
              for (ci in seq_along(row_cells)) {
                if (ci <= n_cols) {
                  avg_chars[ci] <- max(avg_chars[ci], nchar(row_cells[ci]))
                }
              }
            }
            extra <- avg_chars / sum(avg_chars) * remaining
            col_widths <- col_widths + extra
          } else if (remaining < 0) {
            # Proportional shrink but keep minimum viable
            col_widths <- min_widths / sum(min_widths) * usable_width
          }
          col_widths <- pmax(col_widths, 0.05)

          col_starts <- numeric(n_cols)
          col_starts[1] <- x
          if (n_cols > 1) {
            for (ci in 2:n_cols) {
              col_starts[ci] <- col_starts[ci - 1] + col_widths[ci - 1] + col_gap
            }
          }

          # Helper: find wrap width (chars) that fits column using strwidth
          find_wrap_width <- function(col_idx) {
            target_w <- col_widths[col_idx]
            # Binary search for the right character count
            lo <- 4L
            hi <- 120L
            best <- lo
            # Gather all cell texts for this column
            texts <- character(0)
            for (row_cells in all_cells) {
              if (col_idx <= length(row_cells)) {
                texts <- c(texts, strip_markdown_for_display(row_cells[col_idx]))
              }
            }
            while (lo <= hi) {
              mid <- (lo + hi) %/% 2L
              ok <- TRUE
              for (txt in texts) {
                wrapped <- strwrap(txt, width = mid)
                for (wl in wrapped) {
                  if (graphics::strwidth(wl, cex = cex, font = 2) > target_w) {
                    ok <- FALSE
                    break
                  }
                }
                if (!ok) break
              }
              if (ok) {
                best <- mid
                lo <- mid + 1L
              } else {
                hi <- mid - 1L
              }
            }
            best
          }
          col_wrap <- vapply(seq_len(n_cols), find_wrap_width, integer(1))

          # Helper: render table header row with underline, returns new y
          table_end <- col_starts[n_cols] + col_widths[n_cols]
          render_table_header <- function(y) {
            wrapped_cells <- vector("list", n_cols)
            max_lines <- 1L
            for (ci in seq_len(n_cols)) {
              cell_text <- if (ci <= length(header_cells)) {
                strip_markdown_for_display(header_cells[ci])
              } else ""
              wrapped <- strwrap(cell_text, width = col_wrap[ci])
              if (length(wrapped) == 0) wrapped <- ""
              wrapped_cells[[ci]] <- wrapped
              max_lines <- max(max_lines, length(wrapped))
            }
            for (li in seq_len(max_lines)) {
              for (ci in seq_len(n_cols)) {
                txt <- if (li <= length(wrapped_cells[[ci]])) wrapped_cells[[ci]][li] else ""
                graphics::text(col_starts[ci], y, txt,
                               cex = cex, col = col, adj = c(0, 0.5), font = 2)
              }
              y <- y - line_height
            }
            # Draw line under header
            y <- y + line_height * 0.4
            graphics::segments(x, y, table_end, y, col = "gray60", lwd = 0.5)
            y <- y - line_height * 0.4
            y
          }

          # Helper: render a body table row, returns new y
          # Repeats header if a page break occurs
          render_table_row <- function(cells, y) {
            # Wrap each cell and find max lines needed
            wrapped_cells <- vector("list", n_cols)
            max_lines <- 1L
            for (ci in seq_len(n_cols)) {
              cell_text <- if (ci <= length(cells)) {
                strip_markdown_for_display(cells[ci])
              } else ""
              wrapped <- strwrap(cell_text, width = col_wrap[ci])
              if (length(wrapped) == 0) wrapped <- ""
              wrapped_cells[[ci]] <- wrapped
              max_lines <- max(max_lines, length(wrapped))
            }
            # Render line by line
            for (li in seq_len(max_lines)) {
              if (y < 0.05) {
                plot.new()
                y <- 0.92
                # Repeat header on new page
                y <- render_table_header(y)
              }
              for (ci in seq_len(n_cols)) {
                txt <- if (li <= length(wrapped_cells[[ci]])) wrapped_cells[[ci]][li] else ""
                graphics::text(col_starts[ci], y, txt,
                               cex = cex, col = col, adj = c(0, 0.5), font = 1)
              }
              y <- y - line_height
            }
            y
          }

          # Render header row (bold)
          if (y < 0.05) {
            plot.new()
            y <- 0.92
          }
          y <- render_table_header(y)

          # Render body rows
          if (length(data_lines) > 1) {
            for (row_line in data_lines[-1]) {
              row_cells <- parse_row(row_line)
              y <- render_table_row(row_cells, y)
            }
          }

          y <- y - line_height * 0.5  # spacing after table
        }
      }
      next
    }

    # Check for blockquote (>)
    if (grepl("^>\\s*", line)) {
      quote_text <- gsub("^>\\s*", "", line)
      quote_text <- strip_markdown_for_display(quote_text)
      # Draw quote bar
      graphics::segments(x - 0.01, y + line_height * 0.3, x - 0.01, y - line_height * 0.3,
                         col = "gray60", lwd = 2)
      wrapped <- strwrap(quote_text, width = width - 5)
      for (wline in wrapped) {
        if (y < 0.05) {
          plot.new()
          y <- 0.92
        }
        graphics::text(x + 0.02, y, wline, cex = cex, col = "gray40", adj = adj, font = 3)
        y <- y - line_height
      }
      i <- i + 1
      next
    }

    # Check for bullet list (- or *)
    if (grepl("^[-*]\\s+", line)) {
      list_text <- gsub("^[-*]\\s+", "", line)
      # Strip markdown for wrapping calculation
      plain_text <- strip_markdown_for_display(list_text)
      wrapped <- strwrap(plain_text, width = width - 8)
      # Draw bullet on first line
      graphics::points(x + 0.01, y, pch = 16, cex = 0.3, col = col)
      # Render first line
      if (length(wrapped) > 0) {
        graphics::text(x + 0.03, y, wrapped[1], cex = cex, col = col, adj = c(0, 0.5))
        y <- y - line_height
      }
      # Render continuation lines (indented)
      if (length(wrapped) > 1) {
        for (wline in wrapped[-1]) {
          if (y < 0.05) {
            plot.new()
            y <- 0.92
          }
          graphics::text(x + 0.03, y, wline, cex = cex, col = col, adj = c(0, 0.5))
          y <- y - line_height
        }
      }
      i <- i + 1
      next
    }

    # Check for numbered list (1. 2. etc.)
    if (grepl("^[0-9]+\\.\\s+", line)) {
      num <- gsub("^([0-9]+)\\..*", "\\1", line)
      list_text <- gsub("^[0-9]+\\.\\s+", "", line)
      # Strip markdown for wrapping calculation
      plain_text <- strip_markdown_for_display(list_text)
      wrapped <- strwrap(plain_text, width = width - 8)
      # Draw number on first line
      graphics::text(x + 0.01, y, paste0(num, "."), cex = cex, col = col, adj = c(0, 0.5))
      # Render first line
      if (length(wrapped) > 0) {
        graphics::text(x + 0.04, y, wrapped[1], cex = cex, col = col, adj = c(0, 0.5))
        y <- y - line_height
      }
      # Render continuation lines (indented)
      if (length(wrapped) > 1) {
        for (wline in wrapped[-1]) {
          if (y < 0.05) {
            plot.new()
            y <- 0.92
          }
          graphics::text(x + 0.04, y, wline, cex = cex, col = col, adj = c(0, 0.5))
          y <- y - line_height
        }
      }
      i <- i + 1
      next
    }

    # Regular paragraph line - wrap and render with formatting
    wrapped <- strwrap(line, width = width)
    n_wrapped <- length(wrapped)
    for (wi in seq_along(wrapped)) {
      if (y < 0.05) {
        plot.new()
        y <- 0.92
      }
      wline <- wrapped[wi]
      # Justify all lines except the last line of the paragraph
      if (wi < n_wrapped) {
        wline <- justify_text_line(wline, width)
      }
      render_formatted_line(wline, x, y, cex, col, width)
      y <- y - line_height
    }

    i <- i + 1
  }

  return(y)
}


#' Strip markdown markers for plain display
#' @noRd
strip_markdown_for_display <- function(text) {
  # Remove formatting markers but keep text
  text <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", text)  # Bold
text <- gsub("(?<!\\*)\\*([^*]+)\\*(?!\\*)", "\\1", text, perl = TRUE)  # Italic
  text <- gsub("~~([^~]+)~~", "\\1", text)  # Strikethrough
  text <- gsub("`([^`]+)`", "\\1", text)    # Inline code
  text <- gsub("\\[([^]]+)\\]\\([^)]+\\)", "\\1", text)  # Links
  text
}


#' Justify a text line by distributing extra spaces between words
#' @param line Character. The line to justify.
#' @param target_width Integer. Target character width.
#' @return Character. Justified line with extra spaces.
#' @noRd
justify_text_line <- function(line, target_width) {
  words <- strsplit(line, " ")[[1]]
  words <- words[nzchar(words)]
  if (length(words) <= 1) return(line)

  current_len <- sum(nchar(words)) + length(words) - 1
  deficit <- target_width - current_len
  if (deficit <= 0) return(line)

  # Distribute extra spaces evenly between word gaps
  n_gaps <- length(words) - 1
  base_extra <- deficit %/% n_gaps
  remainder <- deficit %% n_gaps

  parts <- character(0)
  for (gi in seq_len(n_gaps)) {
    extra <- base_extra + if (gi <= remainder) 1 else 0
    parts <- c(parts, words[gi], strrep(" ", 1 + extra))
  }
  parts <- c(parts, words[length(words)])
  paste0(parts, collapse = "")
}


#' Render a single line with inline markdown formatting
#' @noRd
render_formatted_line <- function(line, x, y, cex, col, width) {
  # Check if line has any formatting
  has_formatting <- grepl("\\*\\*|(?<!\\*)\\*(?!\\*)|~~|`|\\[.*\\]\\(.*\\)", line, perl = TRUE)

  if (!has_formatting) {
    graphics::text(x, y, line, cex = cex, col = col, adj = c(0, 0.5), font = 1)
    return(invisible(NULL))
  }

  # Parse and render segments with different formatting
  # This is a simplified parser - handles non-nested formatting
  current_x <- x
  remaining <- line

  while (nchar(remaining) > 0) {
    # Find the next formatting marker
    bold_pos <- regexpr("\\*\\*", remaining)
    italic_pos <- regexpr("(?<!\\*)\\*(?!\\*)", remaining, perl = TRUE)
    strike_pos <- regexpr("~~", remaining)
    code_pos <- regexpr("`", remaining)
    link_pos <- regexpr("\\[", remaining)

    positions <- c(
      bold = if (bold_pos > 0) bold_pos else Inf,
      italic = if (italic_pos > 0) italic_pos else Inf,
      strike = if (strike_pos > 0) strike_pos else Inf,
      code = if (code_pos > 0) code_pos else Inf,
      link = if (link_pos > 0) link_pos else Inf
    )

    min_pos <- min(positions)

    if (is.infinite(min_pos)) {
      # No more formatting, render rest as plain text
      graphics::text(current_x, y, remaining, cex = cex, col = col, adj = c(0, 0.5), font = 1)
      break
    }

    # Render text before the marker
    if (min_pos > 1) {
      before <- substr(remaining, 1, min_pos - 1)
      graphics::text(current_x, y, before, cex = cex, col = col, adj = c(0, 0.5), font = 1)
      current_x <- current_x + graphics::strwidth(before, cex = cex)
      remaining <- substr(remaining, min_pos, nchar(remaining))
    }

    # Process the formatting
    marker_type <- names(which.min(positions))

    if (marker_type == "bold" && grepl("^\\*\\*([^*]+)\\*\\*", remaining)) {
      content <- gsub("^\\*\\*([^*]+)\\*\\*.*", "\\1", remaining)
      graphics::text(current_x, y, content, cex = cex, col = col, adj = c(0, 0.5), font = 2)
      current_x <- current_x + graphics::strwidth(content, cex = cex, font = 2)
      remaining <- sub("^\\*\\*[^*]+\\*\\*", "", remaining)
      # Always add explicit space after bold if next char is not whitespace
      if (nchar(remaining) > 0 && !grepl("^[[:space:]]", remaining)) {
        graphics::text(current_x, y, " ", cex = cex, col = col, adj = c(0, 0.5), font = 1)
        current_x <- current_x + graphics::strwidth(" ", cex = cex)
      }

    } else if (marker_type == "italic" && grepl("^\\*([^*]+)\\*", remaining)) {
      content <- gsub("^\\*([^*]+)\\*.*", "\\1", remaining)
      graphics::text(current_x, y, content, cex = cex, col = col, adj = c(0, 0.5), font = 3)
      current_x <- current_x + graphics::strwidth(content, cex = cex, font = 3)
      remaining <- sub("^\\*[^*]+\\*", "", remaining)
      # Always add explicit space after italic if next char is not whitespace
      if (nchar(remaining) > 0 && !grepl("^[[:space:]]", remaining)) {
        graphics::text(current_x, y, " ", cex = cex, col = col, adj = c(0, 0.5), font = 1)
        current_x <- current_x + graphics::strwidth(" ", cex = cex)
      }

    } else if (marker_type == "strike" && grepl("^~~([^~]+)~~", remaining)) {
      content <- gsub("^~~([^~]+)~~.*", "\\1", remaining)
      # Draw text then strikethrough line
      graphics::text(current_x, y, content, cex = cex, col = "gray50", adj = c(0, 0.5), font = 1)
      content_width <- graphics::strwidth(content, cex = cex)
      graphics::segments(current_x, y, current_x + content_width, y, col = "gray50", lwd = 1)
      current_x <- current_x + content_width
      remaining <- sub("^~~[^~]+~~", "", remaining)

    } else if (marker_type == "code" && grepl("^`([^`]+)`", remaining)) {
      content <- gsub("^`([^`]+)`.*", "\\1", remaining)
      # Draw with monospace-like appearance (gray background simulation)
      graphics::text(current_x, y, content, cex = cex * 0.9, col = "#c7254e", adj = c(0, 0.5),
                     family = "mono")
      current_x <- current_x + graphics::strwidth(content, cex = cex * 0.9, family = "mono")
      remaining <- sub("^`[^`]+`", "", remaining)
      # Add explicit space after code if next char is not whitespace
      if (nchar(remaining) > 0 && !grepl("^[[:space:]]", remaining)) {
        graphics::text(current_x, y, " ", cex = cex, col = col, adj = c(0, 0.5), font = 1)
        current_x <- current_x + graphics::strwidth(" ", cex = cex)
      }

    } else if (marker_type == "link" && grepl("^\\[([^]]+)\\]\\(([^)]+)\\)", remaining)) {
      link_text <- gsub("^\\[([^]]+)\\]\\(([^)]+)\\).*", "\\1", remaining)
      # Render link text in blue, underlined
      graphics::text(current_x, y, link_text, cex = cex, col = "#0066cc", adj = c(0, 0.5), font = 1)
      link_width <- graphics::strwidth(link_text, cex = cex)
      # Draw underline with fixed offset
      underline_offset <- graphics::strheight("x", cex = cex) * 0.3
      graphics::segments(current_x, y - underline_offset, current_x + link_width,
                         y - underline_offset, col = "#0066cc", lwd = 0.5)
      current_x <- current_x + link_width
      remaining <- sub("^\\[[^]]+\\]\\([^)]+\\)", "", remaining)

    } else {
      # Marker found but pattern not matched, skip one character
      graphics::text(current_x, y, substr(remaining, 1, 1), cex = cex, col = col,
                     adj = c(0, 0.5), font = 1)
      current_x <- current_x + graphics::strwidth(substr(remaining, 1, 1), cex = cex)
      remaining <- substr(remaining, 2, nchar(remaining))
    }
  }

  invisible(NULL)
}


#' Generate indicator map with OSM background
#'
#' @description
#' Generates a map for a specific indicator column with OSM Topo background.
#' Uses maptiles package if available, otherwise falls back to simple sf plot.
#'
#' @param data sf. Spatial data with indicator values.
#' @param column Character. Column name for the indicator.
#' @param title Character. Map title.
#' @param color Character. Base color for the gradient.
#' @param output_file Character. Path to output PNG file (optional).
#'
#' @return Invisible NULL if output_file provided, otherwise ggplot object.
#' @noRd
generate_indicator_map <- function(data, column, title, color = "#228B22",
                                   output_file = NULL) {
  if (!inherits(data, "sf") || !column %in% names(data)) {
    return(invisible(NULL))
  }

  vals <- data[[column]]
  if (all(is.na(vals))) {
    return(invisible(NULL))
  }

  # Simple sf plot (base R graphics)
  if (!is.null(output_file)) {
    grDevices::png(output_file, width = 800, height = 600, res = 150)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # Create color breaks
  n_breaks <- 5
  breaks <- seq(0, 100, length.out = n_breaks + 1)
  colors <- grDevices::colorRampPalette(c("#f0f0f0", color))(n_breaks)

  # Classify values
  val_class <- cut(vals, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  fill_colors <- colors[val_class]
  fill_colors[is.na(fill_colors)] <- "#cccccc"

  # Plot
  plot(sf::st_geometry(data),
       col = fill_colors,
       border = "gray40",
       lwd = 0.5,
       main = title)

  # Add legend
  legend_labels <- paste0(utils::head(breaks, -1), "-", utils::tail(breaks, -1))
  graphics::legend("bottomright",
                   legend = legend_labels,
                   fill = colors,
                   title = "Score",
                   cex = 0.7,
                   bty = "n")

  invisible(NULL)
}


#' Draw parcel table for indicator
#'
#' @description
#' Draws a table showing indicator values per parcel.
#'
#' @param data sf. Data with indicator values.
#' @param column Character. Column name for the indicator.
#' @param parcel_id_col Character. Column name for parcel ID.
#' @param language Character. Language code.
#' @param indicator_title Character. Optional indicator title to display in header.
#'
#' @return Invisible NULL.
#' @noRd
draw_parcel_table <- function(data, column, parcel_id_col = "id", language = "fr",
                              indicator_title = NULL) {
  if (!column %in% names(data)) return(invisible(NULL))

  # Get parcel IDs
  if (parcel_id_col %in% names(data)) {
    ids <- data[[parcel_id_col]]
  } else {
    ids <- seq_len(nrow(data))
  }

  vals <- if (inherits(data, "sf")) {
    sf::st_drop_geometry(data)[[column]]
  } else {
    data[[column]]
  }

  # Start new page for table
  plot.new()

  # Title with indicator name and code
  if (!is.null(indicator_title) && nchar(indicator_title) > 0) {
    # Main title: indicator name
    graphics::text(0.5, 0.96, indicator_title, font = 2, cex = 1.2, col = "gray20")
    # Subtitle
    table_subtitle <- if (language == "fr") "Valeurs par parcelle" else "Values per parcel"
    graphics::text(0.5, 0.91, table_subtitle, font = 1, cex = 0.9, col = "gray50")
    y <- 0.84
  } else {
    table_title <- if (language == "fr") "Valeurs par parcelle" else "Values per parcel"
    graphics::text(0.5, 0.95, table_title, font = 2, cex = 1.1)
    y <- 0.88
  }

  # Table headers
  graphics::text(0.15, y, if (language == "fr") "Parcelle" else "Parcel", font = 2, cex = 0.85, adj = 0)
  graphics::text(0.55, y, "Score", font = 2, cex = 0.85)
  graphics::text(0.8, y, if (language == "fr") "Qualit\u00e9" else "Quality", font = 2, cex = 0.85)
  graphics::abline(h = y - 0.015, col = "gray70", lwd = 1.5)

  y <- y - 0.04
  max_rows <- 20  # Limit rows per page

  for (i in seq_along(vals)) {
    if (i > max_rows || y < 0.1) break

    val <- vals[i]
    id_str <- as.character(ids[i])

    # Determine quality label and color
    if (is.na(val)) {
      quality <- "N/A"
      col <- "gray50"
    } else if (val >= 80) {
      quality <- if (language == "fr") "Excellent" else "Excellent"
      col <- "#228B22"
    } else if (val >= 60) {
      quality <- if (language == "fr") "Bon" else "Good"
      col <- "#32CD32"
    } else if (val >= 40) {
      quality <- if (language == "fr") "Moyen" else "Average"
      col <- "#FF8C00"
    } else if (val >= 20) {
      quality <- if (language == "fr") "Faible" else "Low"
      col <- "#DC143C"
    } else {
      quality <- if (language == "fr") "Tres faible" else "Very low"
      col <- "#8B0000"
    }

    # Use last 6 characters of parcel ID
    short_id <- substr(id_str, pmax(1, nchar(id_str) - 5), nchar(id_str))
    graphics::text(0.15, y, short_id, cex = 0.75, adj = 0)
    graphics::text(0.55, y, if (is.na(val)) "N/A" else sprintf("%.1f", val), cex = 0.75)
    graphics::text(0.8, y, quality, cex = 0.75, col = col, font = 2)

    y <- y - 0.035
  }

  # Note if more rows exist
  if (length(vals) > max_rows) {
    graphics::text(0.5, y - 0.02,
                   sprintf("... %d %s",
                           length(vals) - max_rows,
                           if (language == "fr") "parcelles suppl\u00e9mentaires" else "additional parcels"),
                   cex = 0.7, col = "gray50", font = 3)
  }

  invisible(NULL)
}


#' Draw standard cover page
#'
#' @description
#' Draws the standard cover page without background image.
#'
#' @param report_data List. Report data structure.
#' @param language Character. Language code.
#'
#' @return Invisible NULL.
#' @noRd
draw_standard_cover_page <- function(report_data, language) {
  plot.new()
  graphics::text(0.5, 0.9, report_data$labels$title,
                 cex = 2, font = 2, col = "#2E7D32")
  graphics::text(0.5, 0.85, report_data$labels$subtitle,
                 cex = 1.2, col = "gray50")

  # Project info
  graphics::text(0.5, 0.75, report_data$project_name, cex = 1.5, font = 2)
  if (!is.null(report_data$project_description) && nchar(report_data$project_description) > 0) {
    graphics::text(0.5, 0.70, clean_text_for_pdf(report_data$project_description), cex = 1)
  }

  # Stats
  info_y <- 0.60
  graphics::text(0.5, info_y,
                 sprintf("%s: %d", report_data$labels$parcels, report_data$n_parcels),
                 cex = 1)
  graphics::text(0.5, info_y - 0.05,
                 sprintf("%s: %s", report_data$labels$created_at, report_data$created_at),
                 cex = 1)

  # Global score with color
  score_color <- if (report_data$global_score >= 60) "#228B22"
  else if (report_data$global_score >= 40) "#FF8C00"
  else "#DC143C"

  graphics::text(0.5, 0.40, report_data$labels$global_score_label, cex = 1.2)
  graphics::text(0.5, 0.30, sprintf("%.1f / 100", report_data$global_score),
                 cex = 3, font = 2, col = score_color)

  # Footer
  graphics::text(0.5, 0.05, report_data$labels$generated_by, cex = 0.8, col = "gray50")

  invisible(NULL)
}


#' Generate simple PDF report without Quarto
#'
#' @description
#' Fallback PDF generation using base R graphics when Quarto is not available.
#' Creates a multi-page PDF with project summary, radar chart, family maps,
#' and detailed analysis per family.
#'
#' @param project List. Project object.
#' @param family_scores sf. Family scores.
#' @param output_file Character. Path to output PDF.
#' @param language Character. Language code.
#' @param synthesis_comments Character. Optional comments.
#' @param family_comments Named list. Optional comments per family.
#' @param cover_image Character. Optional path to cover image for first page.
#'
#' @return Character. Path to generated PDF.
#' @noRd
generate_simple_pdf_report <- function(project,
                                       family_scores,
                                       output_file,
                                       language = "fr",
                                       synthesis_comments = NULL,
                                       family_comments = NULL,
                                       cover_image = NULL) {
  i18n <- get_i18n(language)

  # Prepare data
  report_data <- prepare_report_data(project, family_scores, language,
                                     synthesis_comments, family_comments)

  # Family order
  family_order <- c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")

  # Open PDF device
  grDevices::pdf(output_file, width = 8.27, height = 11.69, paper = "a4")
  on.exit(grDevices::dev.off(), add = TRUE)

  # ==========================================================================
  # Page 1: Cover page with optional background image
  # ==========================================================================
  if (!is.null(cover_image) && file.exists(cover_image)) {
    # Cover page with background image
    tryCatch({
      # Read the image
      img <- NULL
      ext <- tolower(tools::file_ext(cover_image))
      if (ext %in% c("png")) {
        if (requireNamespace("png", quietly = TRUE)) {
          img <- png::readPNG(cover_image)
        }
      } else if (ext %in% c("jpg", "jpeg")) {
        if (requireNamespace("jpeg", quietly = TRUE)) {
          img <- jpeg::readJPEG(cover_image)
        }
      }

      if (!is.null(img)) {
        # Set up plot with no margins
        graphics::par(mar = c(0, 0, 0, 0))
        plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "",
             axes = FALSE, asp = NA)

        # Draw image as background (fill entire page)
        graphics::rasterImage(img, 0, 0, 1, 1)

        # Add semi-transparent overlay for text readability
        graphics::rect(0, 0, 1, 0.35, col = grDevices::adjustcolor("white", alpha.f = 0.85),
                       border = NA)
        graphics::rect(0, 0.75, 1, 1, col = grDevices::adjustcolor("white", alpha.f = 0.85),
                       border = NA)

        # Title at top
        graphics::text(0.5, 0.92, report_data$labels$title,
                       cex = 2.2, font = 2, col = "#2E7D32")
        graphics::text(0.5, 0.85, report_data$labels$subtitle,
                       cex = 1.3, col = "gray40")

        # Project info at bottom
        graphics::text(0.5, 0.28, report_data$project_name, cex = 1.8, font = 2, col = "gray20")

        # Global score
        score_color <- if (report_data$global_score >= 60) "#228B22"
        else if (report_data$global_score >= 40) "#FF8C00"
        else "#DC143C"
        graphics::text(0.5, 0.18, sprintf("Score global: %.1f / 100", report_data$global_score),
                       cex = 1.5, font = 2, col = score_color)

        # Parcels count
        graphics::text(0.5, 0.10, sprintf("%d %s", report_data$n_parcels,
                                          if (report_data$n_parcels > 1) {
                                            if (language == "fr") "parcelles" else "parcels"
                                          } else {
                                            if (language == "fr") "parcelle" else "parcel"
                                          }),
                       cex = 1, col = "gray50")

        # Footer
        graphics::text(0.5, 0.03, report_data$labels$generated_by, cex = 0.7, col = "gray50")

        # Reset margins
        graphics::par(mar = c(5.1, 4.1, 4.1, 2.1))
      } else {
        # Image couldn't be read, fall back to standard cover
        draw_standard_cover_page(report_data, language)
      }
    }, error = function(e) {
      # Error loading image, fall back to standard cover
      draw_standard_cover_page(report_data, language)
    })
  } else {
    # No cover image provided, use standard cover
    draw_standard_cover_page(report_data, language)
  }

  # ==========================================================================
  # Page 2: Table of Contents
  # ==========================================================================
  plot.new()
  toc_title <- if (language == "fr") "Table des matieres" else "Table of Contents"
  graphics::text(0.5, 0.95, toc_title, cex = 1.8, font = 2)

  y <- 0.85
  page_num <- 3  # Start counting from page 3

  # TOC entries
  graphics::text(0.1, y, "1.", cex = 0.9, adj = 0)
  graphics::text(0.15, y, report_data$labels$radar_title, cex = 0.9, adj = 0)
  graphics::text(0.9, y, sprintf("p. %d", page_num), cex = 0.9, adj = 1)
  y <- y - 0.04
  page_num <- page_num + 1

  graphics::text(0.1, y, "2.", cex = 0.9, adj = 0)
  graphics::text(0.15, y, report_data$labels$family_scores, cex = 0.9, adj = 0)
  graphics::text(0.9, y, sprintf("p. %d", page_num), cex = 0.9, adj = 1)
  y <- y - 0.04
  page_num <- page_num + 1

  if (!is.null(synthesis_comments) && nchar(synthesis_comments) > 0) {
    graphics::text(0.1, y, "3.", cex = 0.9, adj = 0)
    graphics::text(0.15, y, report_data$labels$comments_title, cex = 0.9, adj = 0)
    graphics::text(0.9, y, sprintf("p. %d", page_num), cex = 0.9, adj = 1)
    y <- y - 0.04
    page_num <- page_num + 1
  }

  # Family sections
  section_num <- if (!is.null(synthesis_comments) && nchar(synthesis_comments) > 0) 4 else 3
  graphics::text(0.1, y, sprintf("%d.", section_num), cex = 0.9, adj = 0)
  detail_title <- if (language == "fr") "Detail par famille d'indicateurs" else "Detail by indicator family"
  graphics::text(0.15, y, detail_title, cex = 0.9, adj = 0, font = 2)
  y <- y - 0.05

  for (code in family_order) {
    if (!code %in% names(report_data$family_stats)) next
    fam <- report_data$family_stats[[code]]
    graphics::text(0.18, y, paste0(code, " - ", fam$name), cex = 0.85, adj = 0)
    graphics::text(0.9, y, sprintf("p. %d", page_num), cex = 0.85, adj = 1)
    y <- y - 0.035
    page_num <- page_num + 2  # Each family has 2 pages (info + map)
  }

  # ==========================================================================
  # Page 3: Radar Chart
  # ==========================================================================
  tryCatch({
    nemeton_radar(family_scores, mode = "family", normalize = FALSE,
                  title = report_data$labels$radar_title)
  }, error = function(e) {
    plot.new()
    graphics::text(0.5, 0.5, "Radar plot unavailable", cex = 1.5)
  })

  # ==========================================================================
  # Page 4: Family Scores Table
  # ==========================================================================
  plot.new()
  graphics::text(0.5, 0.95, report_data$labels$family_scores, cex = 1.5, font = 2)

  # Draw table
  y_start <- 0.85
  y_step <- 0.06

  # Header
  graphics::text(0.15, y_start, "Code", font = 2, cex = 0.9)
  graphics::text(0.45, y_start, if (language == "fr") "Famille" else "Family", font = 2, cex = 0.9)
  graphics::text(0.75, y_start, "Score", font = 2, cex = 0.9)
  graphics::text(0.90, y_start, "Min-Max", font = 2, cex = 0.9)

  graphics::abline(h = y_start - 0.02, col = "gray70")

  # Data rows
  y <- y_start - y_step
  for (fam in report_data$family_stats) {
    graphics::text(0.15, y, fam$code, cex = 0.85)
    graphics::text(0.45, y, fam$name, cex = 0.85)
    graphics::text(0.75, y, sprintf("%.1f", fam$mean), cex = 0.85, col = fam$color, font = 2)
    graphics::text(0.90, y, sprintf("%.0f-%.0f", fam$min, fam$max), cex = 0.75, col = "gray50")
    y <- y - y_step
  }

  # Score interpretation legend
  y <- y - 0.08
  interp_title <- if (language == "fr") "Interpretation des scores" else "Score interpretation"
  graphics::text(0.5, y, interp_title, font = 2, cex = 0.9)
  y <- y - 0.04

  interp <- if (language == "fr") {
    c("80-100: Excellent", "60-79: Bon", "40-59: Moyen", "20-39: Faible", "0-19: Tres faible")
  } else {
    c("80-100: Excellent", "60-79: Good", "40-59: Average", "20-39: Low", "0-19: Very low")
  }
  for (line in interp) {
    graphics::text(0.5, y, line, cex = 0.75, col = "gray40")
    y <- y - 0.03
  }

  # ==========================================================================
  # Page 5: Synthesis Comments (if provided) - justified text with markdown
  # ==========================================================================
  if (!is.null(synthesis_comments) && nchar(synthesis_comments) > 0) {
    plot.new()
    graphics::text(0.08, 0.95, report_data$labels$comments_title, cex = 1.5, font = 2, adj = 0)

    # Render with markdown support (bold, headers, etc.)
    render_markdown_text(synthesis_comments, x = 0.08, y = 0.88, cex = 0.75,
                         col = "black", adj = 0, line_height = 0.025, width = 95)
  }

  # ==========================================================================
  # Family Details: Summary + each indicator with map and parcel table
  # ==========================================================================
  for (code in family_order) {
    if (!code %in% names(report_data$family_stats)) next

    fam <- report_data$family_stats[[code]]
    fam_config <- INDICATOR_FAMILIES[[code]]

    # ----- Page A: Family Summary -----
    plot.new()
    # Family header with colored background
    graphics::rect(0, 0.90, 1, 1, col = fam$color, border = NA)
    graphics::text(0.5, 0.95, paste0(fam$name, " (", code, ")"),
                   cex = 1.6, font = 2, col = "white")

    # Score summary
    graphics::text(0.5, 0.82,
                   sprintf("%s: %.1f / 100",
                           if (language == "fr") "Score moyen" else "Mean score",
                           fam$mean),
                   cex = 1.3, font = 2)
    graphics::text(0.5, 0.77,
                   sprintf("(Min: %.1f, Max: %.1f)", fam$min, fam$max),
                   cex = 1, col = "gray50")

    # Indicator summary table
    y <- 0.68
    ind_data <- report_data$indicator_stats[[code]]
    if (!is.null(ind_data) && nrow(ind_data) > 0) {
      ind_title <- if (language == "fr") "Resume des indicateurs" else "Indicator summary"
      graphics::text(0.5, y, ind_title, font = 2, cex = 1.1)
      y <- y - 0.04

      # Table header
      graphics::text(0.1, y, if (language == "fr") "Indicateur" else "Indicator", font = 2, cex = 0.8, adj = 0)
      graphics::text(0.7, y, if (language == "fr") "Moyenne" else "Mean", font = 2, cex = 0.8)
      graphics::text(0.82, y, "Min", font = 2, cex = 0.8)
      graphics::text(0.92, y, "Max", font = 2, cex = 0.8)
      graphics::abline(h = y - 0.015, col = "gray70")
      y <- y - 0.04

      for (i in seq_len(nrow(ind_data))) {
        graphics::text(0.1, y, ind_data[i, 1], cex = 0.75, adj = 0)
        graphics::text(0.7, y, sprintf("%.1f", ind_data[i, 2]), cex = 0.75)
        graphics::text(0.82, y, sprintf("%.1f", ind_data[i, 3]), cex = 0.75)
        graphics::text(0.92, y, sprintf("%.1f", ind_data[i, 4]), cex = 0.75)
        y <- y - 0.035
      }
    }

    # Family comments if available (justified text with markdown support)
    y <- y - 0.04
    fam_comment <- report_data$family_comments[[code]]
    if (!is.null(fam_comment) && nchar(fam_comment) > 0) {
      analysis_title <- if (language == "fr") "Analyse" else "Analysis"
      graphics::text(0.08, y, analysis_title, font = 2, cex = 1, adj = 0)
      y <- y - 0.04

      # Render with markdown support (bold, headers, etc.)
      y <- render_markdown_text(fam_comment, x = 0.08, y = y, cex = 0.7,
                                col = "black", adj = 0, line_height = 0.025,
                                width = 95)
    }

    # ----- Page B: Family aggregate map -----
    if (inherits(family_scores, "sf")) {
      col_name <- get_famille_col(code)
      if (col_name %in% names(family_scores)) {
        draw_indicator_map_page(family_scores, col_name, fam$name, fam$color, language)
        # Parcel table for family aggregate with indicator title
        fam_title <- paste0(code, " - ", fam$name)
        draw_parcel_table(family_scores, col_name, "id", language, indicator_title = fam_title)
      }
    }

    # ----- Pages C+: Each indicator in the family -----
    if (!is.null(fam_config) && inherits(family_scores, "sf")) {
      indicators <- fam_config$indicators
      col_names <- fam_config$column_names
      ind_labels <- fam_config$indicator_labels

      for (i in seq_along(indicators)) {
        ind_code <- indicators[i]
        col_name <- col_names[i]

        if (!col_name %in% names(family_scores)) next

        # Get indicator label
        labels <- ind_labels[[ind_code]]
        ind_label <- if (language == "fr") labels$fr else labels$en
        ind_title <- paste0(ind_code, " - ", ind_label)

        # Draw indicator map page
        draw_indicator_map_page(family_scores, col_name, ind_title, fam$color, language)

        # Draw parcel table for this indicator with title
        draw_parcel_table(family_scores, col_name, "id", language, indicator_title = ind_title)
      }
    }
  }

  output_file
}


#' Draw indicator map page
#'
#' @description
#' Helper function to draw a map page for an indicator with OSM background.
#' Includes border around map and visible legend.
#'
#' @param data sf. Spatial data.
#' @param column Character. Column name.
#' @param title Character. Map title.
#' @param color Character. Base color.
#' @param language Character. Language code.
#'
#' @return Invisible NULL.
#' @noRd
draw_indicator_map_page <- function(data, column, title, color, language) {
  if (!column %in% names(data)) {
    plot.new()
    na_msg <- if (language == "fr") "Donnees non disponibles" else "Data not available"
    graphics::text(0.5, 0.5, na_msg, cex = 1.2, col = "gray50")
    return(invisible(NULL))
  }

  vals <- data[[column]]

  if (all(is.na(vals))) {
    plot.new()
    na_msg <- if (language == "fr") "Carte non disponible" else "Map not available"
    graphics::text(0.5, 0.5, na_msg, cex = 1.2, col = "gray50")
    return(invisible(NULL))
  }

  # Create color palette
  n_breaks <- 5
  breaks <- seq(0, 100, length.out = n_breaks + 1)
  legend_labels <- paste0(utils::head(breaks, -1), "-", utils::tail(breaks, -1))

  # Try to use maptiles for OSM background
  has_maptiles <- requireNamespace("maptiles", quietly = TRUE)

  if (has_maptiles) {
    tryCatch({
      # Transform to WGS84 for tile download
      data_wgs84 <- sf::st_transform(data, 4326)

      # Calculate optimal zoom based on extent (higher zoom = better resolution)
      bbox <- sf::st_bbox(data_wgs84)
      extent_size <- max(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"])
      # Auto-calculate zoom: smaller extent = higher zoom (max 17 for good detail)
      auto_zoom <- min(17, max(13, round(17 - log2(extent_size * 100))))

      # Download OSM Topo tiles with higher resolution
      tiles <- maptiles::get_tiles(data_wgs84, provider = "OpenTopoMap",
                                   zoom = auto_zoom, crop = TRUE, cachedir = tempdir())

      # Plot tiles first
      graphics::par(mar = c(4, 4, 3, 2))
      maptiles::plot_tiles(tiles)

      # Create semi-transparent colors for overlay
      colors_transparent <- grDevices::colorRampPalette(c(
        grDevices::adjustcolor("#f0f0f0", alpha.f = 0.75),
        grDevices::adjustcolor(color, alpha.f = 0.85)
      ))(n_breaks)

      val_class <- cut(vals, breaks = breaks, include.lowest = TRUE, labels = FALSE)
      fill_colors <- colors_transparent[val_class]
      fill_colors[is.na(fill_colors)] <- grDevices::adjustcolor("#cccccc", alpha.f = 0.7)

      # Transform data to match tiles CRS and overlay
      data_proj <- sf::st_transform(data, sf::st_crs(tiles))
      plot(sf::st_geometry(data_proj), col = fill_colors, border = "gray20",
           lwd = 1.2, add = TRUE)

      # Add parcel labels (last 6 characters of ID)
      add_parcel_labels(data_proj)

      # Add title
      graphics::title(main = title, cex.main = 1.2, font.main = 2)

      # Add legend using figure coordinates (works regardless of map projection)
      colors_opaque <- grDevices::colorRampPalette(c("#f0f0f0", color))(n_breaks)

      # Save current par and use figure coordinates
      old_par <- graphics::par(no.readonly = TRUE)
      graphics::par(fig = c(0, 0.25, 0, 0.35), new = TRUE, mar = c(0, 0, 0, 0))
      plot.new()
      graphics::legend("center",
                       legend = legend_labels,
                       fill = colors_opaque,
                       title = "Score",
                       cex = 0.8,
                       bty = "o",
                       bg = "white",
                       box.col = "gray40")
      graphics::par(fig = old_par$fig, new = FALSE, mar = old_par$mar)

      # Add attribution
      graphics::mtext("(c) OpenTopoMap", side = 1, line = 2, adj = 1, cex = 0.5, col = "gray50")

      # Reset margins
      graphics::par(mar = c(5.1, 4.1, 4.1, 2.1))

      return(invisible(NULL))

    }, error = function(e) {
      # Fall through to simple plot
    })
  }

  # Fallback: simple sf plot without OSM background
  tryCatch({
    # Set margins
    graphics::par(mar = c(4, 4, 3, 2))

    colors <- grDevices::colorRampPalette(c("#f0f0f0", color))(n_breaks)

    # Classify values
    val_class <- cut(vals, breaks = breaks, include.lowest = TRUE, labels = FALSE)
    fill_colors <- colors[val_class]
    fill_colors[is.na(fill_colors)] <- "#cccccc"

    # Plot map
    plot(sf::st_geometry(data),
         col = fill_colors,
         border = "gray40",
         lwd = 0.8,
         main = title)

    # Add parcel labels (last 6 characters of ID)
    add_parcel_labels(data)

    # Add legend using figure coordinates
    old_par <- graphics::par(no.readonly = TRUE)
    graphics::par(fig = c(0, 0.25, 0, 0.35), new = TRUE, mar = c(0, 0, 0, 0))
    plot.new()
    graphics::legend("center",
                     legend = legend_labels,
                     fill = colors,
                     title = "Score",
                     cex = 0.8,
                     bty = "o",
                     bg = "white",
                     box.col = "gray40")
    graphics::par(fig = old_par$fig, new = FALSE, mar = old_par$mar)

    # Reset margins
    graphics::par(mar = c(5.1, 4.1, 4.1, 2.1))

  }, error = function(e) {
    plot.new()
    err_msg <- if (language == "fr") "Erreur lors de la generation de la carte" else "Error generating map"
    graphics::text(0.5, 0.5, err_msg, cex = 1, col = "gray50")
  })

  invisible(NULL)
}


#' Add parcel labels to map
#'
#' @description
#' Adds labels showing the last 6 characters of parcel IDs at centroids.
#'
#' @param data sf. Spatial data with parcel geometries.
#' @param id_col Character. Column name for parcel ID. Default "id".
#'
#' @return Invisible NULL.
#' @noRd
add_parcel_labels <- function(data, id_col = "id") {
  if (!inherits(data, "sf")) return(invisible(NULL))

  # Get parcel IDs
  if (id_col %in% names(data)) {
    ids <- as.character(data[[id_col]])
  } else {
    ids <- as.character(seq_len(nrow(data)))
  }

  # Get last 6 characters of each ID
  short_ids <- substr(ids, pmax(1, nchar(ids) - 5), nchar(ids))

  # Calculate centroids for label placement
  centroids <- tryCatch({
    suppressWarnings(sf::st_centroid(sf::st_geometry(data)))
  }, error = function(e) {
    # Fallback to point on surface if centroid fails
    suppressWarnings(sf::st_point_on_surface(sf::st_geometry(data)))
  })

  # Extract coordinates
  coords <- sf::st_coordinates(centroids)

  # Add labels with white halo effect for readability
  for (i in seq_len(nrow(coords))) {
    x <- coords[i, 1]
    y <- coords[i, 2]
    label <- short_ids[i]

    # White outline (halo effect)
    for (dx in c(-0.5, 0, 0.5)) {
      for (dy in c(-0.5, 0, 0.5)) {
        if (dx != 0 || dy != 0) {
          graphics::text(x, y, label, cex = 0.6, col = "white", font = 2,
                         adj = c(0.5 + dx * 0.02, 0.5 + dy * 0.02))
        }
      }
    }
    # Main label in dark color
    graphics::text(x, y, label, cex = 0.6, col = "gray20", font = 2)
  }

  invisible(NULL)
}


#' Generate PDF report (main entry point)
#'
#' @description
#' Generates a PDF report, using Quarto if available or falling back
#' to simple PDF generation.
#'
#' @param project List. Project object.
#' @param family_scores sf. Family scores.
#' @param output_file Character. Path to output PDF.
#' @param language Character. Language code.
#' @param synthesis_comments Character. Optional synthesis comments (supports markdown).
#' @param family_comments Named list. Optional comments per family (keyed by family code, supports markdown).
#' @param cover_image Character. Optional path to cover image for first page.
#' @param use_quarto Logical. Whether to try Quarto first. Default TRUE.
#'
#' @return Character. Path to generated PDF.
#' @export
generate_report_pdf <- function(project,
                                family_scores,
                                output_file,
                                language = "fr",
                                synthesis_comments = NULL,
                                family_comments = NULL,
                                cover_image = NULL,
                                use_quarto = TRUE) {
  if (use_quarto && is_quarto_installed()) {
    cli::cli_inform("Generating PDF with Quarto...")
    result <- tryCatch(
      generate_pdf_report(project, family_scores, output_file, language,
                          synthesis_comments, family_comments, cover_image),
      error = function(e) {
        message("Quarto report failed, falling back to simple PDF: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(result)) return(result)
  } else {
    cli::cli_inform("Quarto not available, using simple PDF renderer.")
  }

  # Fallback to simple PDF
  cli::cli_inform("Generating PDF with base R graphics (fallback)...")
  generate_simple_pdf_report(project, family_scores, output_file, language,
                             synthesis_comments, family_comments, cover_image)
}

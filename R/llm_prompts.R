#' LLM Prompts
#'
#' @description
#' System and user prompts for LLM-based analysis in nemeton.
#'
#' @name llm_prompts
#' @keywords internal
NULL


#' Load expert profiles from YAML files
#'
#' @description
#' Reads `.yml` files from `inst/experts/` (package profiles) and from
#' the user directory (default `~/.local/share/nemeton/experts/`). User profiles
#' override package profiles when filenames match.
#'
#' @return Named list of expert profiles, each with `label` and `prompt` sublists.
#' @noRd
load_expert_profiles <- function() {
  profiles <- list()

  # 1. Package profiles
  pkg_dir <- system.file("experts", package = "nemetonshiny")
  if (pkg_dir != "") {
    pkg_files <- list.files(pkg_dir, pattern = "\\.ya?ml$", full.names = TRUE)
    for (f in pkg_files) {
      key <- tools::file_path_sans_ext(basename(f))
      prof <- yaml::read_yaml(f)
      if (is.list(prof) && all(c("label", "prompt") %in% names(prof))) {
        profiles[[key]] <- prof
      }
    }
  }

  # 2. User profiles (override package ones)
  default_base <- if (requireNamespace("rappdirs", quietly = TRUE)) {
    rappdirs::user_data_dir("nemeton", "nemeton")
  } else {
    file.path(Sys.getenv("HOME"), ".nemeton")
  }
  user_dir <- getOption("nemeton.experts_dir", file.path(default_base, "experts"))
  if (!dir.exists(user_dir)) {
    dir.create(user_dir, recursive = TRUE, showWarnings = FALSE)
    # Copy example template if freshly created
    example_src <- system.file("experts", "_example.yml.template", package = "nemetonshiny")
    if (example_src != "") {
      file.copy(example_src, file.path(user_dir, "_example.yml.template"),
                overwrite = FALSE)
    }
  }
  if (dir.exists(user_dir)) {
    user_files <- list.files(user_dir, pattern = "\\.ya?ml$", full.names = TRUE)
    for (f in user_files) {
      key <- tools::file_path_sans_ext(basename(f))
      prof <- tryCatch(
        yaml::read_yaml(f),
        error = function(e) {
          warning(sprintf("Skipping invalid expert profile '%s': %s", f, e$message))
          NULL
        }
      )
      if (is.list(prof) && all(c("label", "prompt") %in% names(prof))) {
        profiles[[key]] <- prof
      }
    }
  }

  profiles
}


#' Get expert profiles (cached)
#'
#' @description
#' Returns expert profiles, loading from YAML on first call.
#' Results are cached in `.nemeton_env$expert_profiles`.
#'
#' @return Named list of expert profiles.
#' @noRd
get_expert_profiles <- function() {
  if (!exists("expert_profiles", envir = .nemeton_env) ||
      is.null(.nemeton_env$expert_profiles)) {
    .nemeton_env$expert_profiles <- load_expert_profiles()
  }
  .nemeton_env$expert_profiles
}


#' Reload expert profiles
#'
#' @description
#' Forces a fresh reload from YAML files. Useful for development and testing.
#'
#' @return Named list of expert profiles (invisible).
#' @noRd
reload_expert_profiles <- function() {
  .nemeton_env$expert_profiles <- load_expert_profiles()
  invisible(.nemeton_env$expert_profiles)
}


#' Get expert choices for selectInput
#'
#' @param lang Character. Language code ("fr" or "en").
#' @return Named character vector (names = translated labels, values = profile keys).
#' @noRd
get_expert_choices <- function(lang = "fr") {
  profiles <- get_expert_profiles()
  keys <- names(profiles)
  labels <- vapply(profiles, function(p) {
    lbl <- p$label[[lang]]
    if (is.null(lbl)) lbl <- p$label[["en"]]
    if (is.null(lbl)) lbl <- "?"
    lbl
  }, character(1))
  stats::setNames(keys, labels)
}



#' Build the system prompt for family indicator analysis
#'
#' @param language Character. "fran\u00e7ais" or "English".
#' @param expert Character. Expert profile key. Default "generalist".
#' @return Character string system prompt.
#' @noRd
build_system_prompt <- function(language, expert = "generalist") {
  profiles <- get_expert_profiles()
  if (!expert %in% names(profiles)) {
    expert <- "generalist"
  }
  lang_key <- if (language == "fran\u00e7ais") "fr" else "en"
  prompt <- profiles[[expert]]$prompt[[lang_key]]
  paste0(prompt, " R\u00e9ponds en ", language, ".")
}


#' Build analysis prompt for AI generation
#'
#' @param family_config List. Family configuration from INDICATOR_FAMILIES.
#' @param ind_data data.frame. Indicator data for the family.
#' @param language Character. "fran\u00e7ais" or "English".
#' @return Character string prompt.
#' @noRd
build_analysis_prompt <- function(family_config, ind_data, language) {
  if (inherits(ind_data, "sf")) {
    ind_data <- sf::st_drop_geometry(ind_data)
  }

  family_name <- if (language == "fran\u00e7ais") family_config$name_fr else family_config$name_en
  ind_cols <- get_indicator_cols(ind_data)
  n_parcels <- nrow(ind_data)

  # Build per-indicator stats summary
  stats_lines <- vapply(ind_cols, function(col) {
    vals <- ind_data[[col]]
    vals_clean <- vals[!is.na(vals)]
    n <- length(vals_clean)
    if (n == 0) return(paste0("- ", col, ": no data"))
    mn <- round(min(vals_clean), 3)
    mx <- round(max(vals_clean), 3)
    avg <- round(mean(vals_clean), 3)
    sd_val <- if (n > 1) round(stats::sd(vals_clean), 3) else 0
    cv <- if (avg != 0) round(abs(sd_val / avg) * 100, 1) else 0
    paste0("- ", col, ": n=", n, ", min=", mn, ", max=", mx,
           ", mean=", avg, ", sd=", sd_val, ", CV=", cv, "%")
  }, character(1))

  paste0(
    "Analyse les r\u00e9sultats de la famille ", family_name,
    " (code: ", family_config$code, ") pour ", n_parcels, " parcelles.\n\n",
    "Statistiques des indicateurs :\n",
    paste(stats_lines, collapse = "\n")
  )
}


#' Build synthesis prompt for AI generation (all 12 families)
#'
#' @param family_scores_df data.frame. Family scores (famille_carbone, famille_biodiversite, ...).
#' @param language Character. "fran\u00e7ais" or "English".
#' @return Character string prompt.
#' @noRd
build_synthesis_prompt <- function(family_scores_df, language) {
  if (inherits(family_scores_df, "sf")) {
    family_scores_df <- sf::st_drop_geometry(family_scores_df)
  }

  n_parcels <- nrow(family_scores_df)
  family_cols <- grep("^famille_[a-z]", names(family_scores_df), value = TRUE)

  # Build per-family stats summary
  stats_lines <- vapply(family_cols, function(col) {
    code <- get_famille_code(col)
    fam <- INDICATOR_FAMILIES[[code]]
    if (is.null(fam)) return(paste0("- ", col, ": unknown family"))
    fam_name <- if (language == "fran\u00e7ais") fam$name_fr else fam$name_en

    vals <- family_scores_df[[col]]
    vals_clean <- vals[!is.na(vals)]
    n <- length(vals_clean)
    if (n == 0) return(paste0("- ", fam_name, " (", code, "): no data"))
    mn <- round(min(vals_clean), 1)
    mx <- round(max(vals_clean), 1)
    avg <- round(mean(vals_clean), 1)
    paste0("- ", fam_name, " (", code, "): mean=", avg,
           ", min=", mn, ", max=", mx, " (n=", n, ")")
  }, character(1))

  # Global score
  family_means <- vapply(family_cols, function(col) {
    mean(family_scores_df[[col]], na.rm = TRUE)
  }, numeric(1))
  global <- round(mean(family_means, na.rm = TRUE), 1)

  paste0(
    if (language == "fran\u00e7ais") {
      paste0("Analyse la synth\u00e8se globale du diagnostic N\u00e9m\u00e9ton pour ",
             n_parcels, " parcelles.\n\n",
             "Score global : ", global, "/100\n\n",
             "Scores par famille :\n")
    } else {
      paste0("Analyze the overall N\u00e9m\u00e9ton diagnostic synthesis for ",
             n_parcels, " parcels.\n\n",
             "Global score: ", global, "/100\n\n",
             "Scores by family:\n")
    },
    paste(stats_lines, collapse = "\n"),
    "\n\n",
    if (language == "fran\u00e7ais") {
      "Identifie les points forts, faiblesses, antagonismes entre familles, et recommandations prioritaires."
    } else {
      "Identify strengths, weaknesses, antagonisms between families, and priority recommendations."
    }
  )
}

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


#' Build action plan extraction prompt
#'
#' @description
#' Builds the user prompt asking the LLM to extract a structured action plan
#' from textual synthesis and family comments. Output is constrained to JSON
#' (parsed by `parse_action_plan_response`).
#'
#' @param comments List with `synthesis` (character) and `families` (named
#'   list of family-code -> character).
#' @param ug_ids Character. Allowed UGF identifiers for the project.
#' @param horizon_annees Integer. Planning horizon (years from now).
#' @param language Character. "fran\u00e7ais" or "English".
#' @param scope Character. "all" (all UGFs) or "by_ug".
#' @param ug_id Character. UGF id when `scope = "by_ug"`.
#' @return Character. Prompt text.
#' @noRd
build_action_plan_prompt <- function(comments, ug_ids,
                                     horizon_annees = 20L,
                                     language = "fran\u00e7ais",
                                     scope = c("all", "by_ug"),
                                     ug_id = NULL) {
  scope <- match.arg(scope)
  fr <- identical(language, "fran\u00e7ais")

  syn <- comments$synthesis %||% ""
  fams <- comments$families %||% list()

  fam_blocks <- vapply(names(fams), function(code) {
    txt <- as.character(fams[[code]] %||% "")
    if (!nzchar(trimws(txt))) return(NA_character_)
    paste0("[family_", code, "]\n", txt)
  }, character(1))
  fam_blocks <- fam_blocks[!is.na(fam_blocks)]

  ug_list_str <- paste(ug_ids, collapse = ", ")

  schema <- paste(
    "JSON schema (return ONLY this object, no prose):",
    "{",
    '  "actions": [',
    "    {",
    '      "ug_id": <one of the allowed ug_ids>,',
    '      "type": <one of: coupe_rase, eclaircie, depressage, plantation,',
    "                regeneration, cloisonnement, desserte, observation,",
    "                protection, entretien, autre>,",
    '      "type_libre": <free text if type=autre, else null>,',
    '      "intensite": <e.g. faible|moderee|forte|null>,',
    '      "annee_cible": <integer in 1..HORIZON>,',
    '      "duree": <integer years or null>,',
    '      "priorite": <haute|moyenne|basse>,',
    '      "objectifs_lies": [<family codes among C,B,W,A,F,L,T,R,S,P,E,N>],',
    '      "quantite": {',
    '        "volume_m3": <number or null>,',
    '        "surface_ha": <number or null>,',
    '        "nb_tiges": <integer or null>,',
    '        "rdi": <number or null>,',
    '        "cout_eur": <number or null>',
    "      },",
    '      "source": {',
    '        "origine": <"synthesis" | "family_C" | "family_B" | ...>,',
    '        "extrait_texte": <verbatim short excerpt that justifies the action>',
    "      },",
    '      "commentaire": <short note or null>',
    "    }",
    "  ]",
    "}",
    sep = "\n"
  )

  intro <- if (fr) {
    paste0("Extrais un plan d'actions sylvicoles \u00e0 partir des commentaires ci-dessous. ",
           "Horizon : ", horizon_annees, " ans (pas annuel).")
  } else {
    paste0("Extract a silvicultural action plan from the comments below. ",
           "Horizon: ", horizon_annees, " years (annual step).")
  }

  scope_str <- if (scope == "by_ug" && !is.null(ug_id)) {
    if (fr) paste0("Limite-toi strictement \u00e0 l'UGF '", ug_id, "'.")
    else    paste0("Restrict strictly to UGF '", ug_id, "'.")
  } else {
    if (fr) "Couvre toutes les UGF list\u00e9es."
    else    "Cover all listed UGFs."
  }

  ug_str <- if (fr) {
    paste0("UGF autoris\u00e9es (utilise ces ug_id, jamais d'autres) : ", ug_list_str, ".")
  } else {
    paste0("Allowed UGFs (use these ug_id values, never others): ", ug_list_str, ".")
  }

  syn_block <- if (nzchar(trimws(syn))) {
    paste0("[synthesis]\n", syn, "\n")
  } else {
    if (fr) "[synthesis]\n(aucun commentaire de synth\u00e8se fourni)\n"
    else    "[synthesis]\n(no synthesis comment provided)\n"
  }

  fams_section <- if (length(fam_blocks) > 0) {
    paste(fam_blocks, collapse = "\n\n")
  } else {
    if (fr) "(aucun commentaire de famille fourni)"
    else    "(no family comment provided)"
  }

  paste(
    intro,
    scope_str,
    ug_str,
    "",
    syn_block,
    fams_section,
    "",
    sub("HORIZON", as.character(horizon_annees), schema, fixed = TRUE),
    sep = "\n"
  )
}


#' Build a Q/R chat prompt for plan refinement
#'
#' @param question Character. User's question / request.
#' @param ctx List with `comments`, `ug_ids`, `horizon`.
#' @param plan_summary_json Character. JSON snapshot of the current plan
#'   actions (id, ug_id, type, annee_cible, statut, priorite).
#' @param language Character. "fran\u00e7ais" or "English".
#' @return Character prompt.
#' @noRd
build_action_plan_chat_prompt <- function(question, ctx,
                                          plan_summary_json,
                                          language = "fran\u00e7ais") {
  fr <- identical(language, "fran\u00e7ais")
  intro <- if (fr) {
    paste0(
      "L'utilisateur pose une question ou formule une demande sur le plan d'actions ",
      "sylvicole en cours. Si la demande implique d'ajouter ou modifier des actions, ",
      "renvoie EN PLUS de ta r\u00e9ponse texte un bloc JSON ",
      "`{ \"actions\": [ ... ] }` strictement conforme au sch\u00e9ma utilis\u00e9 par ",
      "build_action_plan_prompt (m\u00eames champs, m\u00eames contraintes), ",
      "encapsul\u00e9 dans un fence ```json ... ```. ",
      "Sinon, r\u00e9ponds en texte simple sans bloc JSON."
    )
  } else {
    "The user asks a question or makes a request about the current silvicultural action plan. If the request implies adding or modifying actions, return - in addition to your text answer - a JSON block `{ \"actions\": [ ... ] }` strictly matching the schema used by build_action_plan_prompt (same fields, same constraints), wrapped in a ```json ... ``` fence. Otherwise reply in plain text only."
  }

  syn <- ctx$comments$synthesis %||% ""
  fams <- ctx$comments$families %||% list()
  fam_block <- if (length(fams) > 0L) {
    paste(vapply(names(fams), function(code) {
      txt <- as.character(fams[[code]] %||% "")
      if (!nzchar(trimws(txt))) return(NA_character_)
      sprintf("[family_%s]\n%s", code, txt)
    }, character(1)) |> stats::na.omit(), collapse = "\n\n")
  } else {
    ""
  }

  paste(
    intro,
    "",
    if (fr) "UGF disponibles :" else "Available UGFs:",
    paste(ctx$ug_ids, collapse = ", "),
    "",
    if (fr) "Horizon (an) :" else "Horizon (years):",
    as.character(ctx$horizon),
    "",
    if (fr) "Plan courant (resume JSON) :" else "Current plan (JSON summary):",
    plan_summary_json,
    "",
    if (fr) "Commentaires de synthese :" else "Synthesis comments:",
    if (nzchar(trimws(syn))) syn else if (fr) "(aucun)" else "(none)",
    "",
    if (fr) "Commentaires par famille :" else "Per-family comments:",
    if (nzchar(fam_block)) fam_block else if (fr) "(aucun)" else "(none)",
    "",
    if (fr) "Question :" else "Question:",
    question,
    sep = "\n"
  )
}


#' Parse and validate an LLM action plan response
#'
#' @description
#' Parses a (potentially noisy) JSON string returned by the LLM, extracts the
#' `actions` array and runs each entry through `validate_action`. Invalid
#' actions are dropped (with a warning collected in the result).
#'
#' @param response Character. Raw text returned by the LLM.
#' @param ug_ids Character. Allowed UGF IDs.
#' @param horizon_annees Integer. Planning horizon.
#' @return List with `actions` (list of validated actions, statut forced to
#'   `proposee`) and `errors` (character \u2014 one entry per dropped action).
#' @noRd
parse_action_plan_response <- function(response, ug_ids,
                                       horizon_annees = 20L) {
  if (!is.character(response) || length(response) == 0L ||
      !nzchar(response)) {
    return(list(actions = list(), errors = "empty response"))
  }

  # Heuristic: the LLM may wrap the JSON in markdown fences or prose.
  txt <- response
  txt <- sub("(?s).*?```(?:json)?\\s*", "", txt, perl = TRUE)
  txt <- sub("(?s)```.*$", "", txt, perl = TRUE)
  # If the above did nothing useful, try to grab the outermost {...}
  m <- regmatches(txt, regexpr("\\{[\\s\\S]*\\}", txt, perl = TRUE))
  if (length(m) > 0 && nzchar(m)) txt <- m

  parsed <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed) || is.null(parsed$actions)) {
    return(list(actions = list(),
                errors = "could not parse JSON or 'actions' missing"))
  }

  actions <- list()
  errors  <- character()
  for (i in seq_along(parsed$actions)) {
    a <- parsed$actions[[i]]
    if (!is.list(a)) {
      errors <- c(errors, sprintf("action #%d not an object", i))
      next
    }
    a$statut <- "proposee"
    if (is.null(a$priorite)) a$priorite <- "moyenne"
    v <- validate_action(a, ug_ids = ug_ids,
                         horizon_annees = horizon_annees)
    if (!v$valid) {
      errors <- c(errors,
                  sprintf("action #%d dropped: %s",
                          i, paste(v$errors, collapse = "; ")))
      next
    }
    actions <- c(actions, list(a))
  }

  list(actions = actions, errors = errors)
}

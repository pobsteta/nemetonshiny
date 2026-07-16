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
# Internal-only expert profiles : NOT user-selectable perspectives.
# `planificateur` is a JSON-only profile (« Tu produis exclusivement du
# JSON valide… ») consumed by the action-plan generation, which hardcodes
# `build_system_prompt(expert = "planificateur")` (mod_action_plan). It
# must stay in `get_expert_profiles()` (the action plan needs it) but be
# excluded from the perspective dropdown — otherwise selecting it in
# Synthèse makes « Générer par IA » return raw JSON instead of prose.
# `regeneration` is the reGénération « Affiner » profile, hardcoded by
# mod_regeneration ; likewise internal, not a general perspective.
.INTERNAL_EXPERT_KEYS <- c("planificateur", "regeneration")

get_expert_choices <- function(lang = "fr") {
  profiles <- get_expert_profiles()
  profiles <- profiles[setdiff(names(profiles), .INTERNAL_EXPERT_KEYS)]
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


#' Build a regeneration-advice prompt from the deterministic species ranking
#'
#' P2 of the hybrid species-recommendation feature (spec 039): the deterministic
#' top-N ranking (nemeton::regen_rank_species) plus each UGF's station conditions
#' are summarized into a prompt asking the LLM for a justified regeneration advice.
#' The species RANKING itself is never delegated to the LLM \u2014 it only writes the
#' narrative around the numbers, tuned by the expert profile (system prompt).
#'
#' @param ranking_df Long ranking data.frame (ug_id, rank, label, suitability,
#'   limiting_factor, confidence).
#' @param station_df The reG\u00e9n\u00e9ration result (per-UGF conditions); sf or data.frame.
#' @param language "fran\u00e7ais" or "English".
#' @param question Optional free-text user instruction / question to steer the
#'   advice (from the "Affiner" panel). Empty / NULL means the default advice.
#' @return Character prompt.
#' @noRd
build_regen_advice_prompt <- function(ranking_df, station_df, language,
                                      question = NULL) {
  fr <- language == "fran\u00e7ais"
  if (inherits(station_df, "sf")) station_df <- sf::st_drop_geometry(station_df)
  ids <- unique(as.character(ranking_df$ug_id))
  # Borne le prompt : au plus 20 UGF d\u00e9taill\u00e9es (au-del\u00e0, l'IA g\u00e9n\u00e9ralise mal et
  # le co\u00fbt explose). Signale la troncature.
  truncated <- length(ids) > 20L
  ids <- utils::head(ids, 20L)
  st_val <- function(st, col, dig) {
    if (!col %in% names(st) || !nrow(st)) return(NULL)
    v <- suppressWarnings(as.numeric(st[[col]][1]))
    if (!is.finite(v)) NULL else round(v, dig)
  }
  lines <- vapply(ids, function(id) {
    rk <- ranking_df[as.character(ranking_df$ug_id) == id, , drop = FALSE]
    rk <- rk[order(rk$rank), , drop = FALSE]
    sp <- paste(sprintf("%d. %s (%d/100, %s)", rk$rank, rk$label,
      round(suppressWarnings(as.numeric(rk$suitability))),
      as.character(rk$limiting_factor)), collapse = " ; ")
    st <- station_df[as.character(station_df$ug_id) == id, , drop = FALSE]
    parts <- c(
      if (!is.null(v <- st_val(st, "njstress", 0))) paste0("jours stress=", v),
      if (!is.null(v <- st_val(st, "rew_min", 2))) paste0("REW min=", v),
      if (!is.null(v <- st_val(st, "d_tmax", 1))) paste0("dTmax=", v),
      if (!is.null(v <- st_val(st, "r7_gel_days", 0))) paste0("gel tardif=", v, "j"))
    cond <- paste(parts, collapse = ", ")
    paste0("- UGF ", id, if (nzchar(cond)) paste0(" [", cond, "]") else "",
      " -> ", sp)
  }, character(1))
  intro <- if (fr) {
    paste0("Conseille la r\u00e9g\u00e9n\u00e9ration pour ", length(ids), " UGF. Pour chacune, ",
      "un classement d\u00e9terministe des essences les plus adapt\u00e9es (score 0-100 + ",
      "facteur limitant) et ses conditions de station sont fournis :\n\n")
  } else {
    paste0("Advise on regeneration for ", length(ids), " management units. For each, ",
      "a deterministic ranking of the best-suited species (0-100 score + limiting ",
      "factor) and its site conditions are given:\n\n")
  }
  ask <- if (fr) {
    paste0("\n\nR\u00e9dige un conseil de r\u00e9g\u00e9n\u00e9ration concis et justifi\u00e9 : pertinence ",
      "des essences en t\u00eate, risques li\u00e9s au facteur limitant, m\u00e9lange conseill\u00e9 et ",
      "r\u00e9serves. Ne recommande jamais une essence absente des classements ci-dessus.")
  } else {
    paste0("\n\nWrite a concise, justified regeneration advice: relevance of the top ",
      "species, risks from the limiting factor, recommended mixing and caveats. Never ",
      "recommend a species absent from the rankings above.")
  }
  note <- if (truncated) {
    if (fr) "\n\n(Note : seules les 20 premi\u00e8res UGF sont d\u00e9taill\u00e9es.)"
    else "\n\n(Note: only the first 20 units are detailed.)"
  } else ""
  # Consigne libre de l'utilisateur (panneau \u00ab Affiner \u00bb) : prioritaire sur le
  # conseil g\u00e9n\u00e9rique, sans jamais autoriser une essence hors classement.
  q <- trimws(question %||% "")
  user_ask <- if (nzchar(q)) {
    if (fr) paste0("\n\nConsigne de l'utilisateur (r\u00e9ponds-y en priorit\u00e9) : ", q)
    else paste0("\n\nUser instruction (address it first): ", q)
  } else ""
  paste0(intro, paste(lines, collapse = "\n"), ask, user_ask, note)
}

#' Shared JSON schema for action-plan LLM prompts
#'
#' Used both by the generation prompt (`build_action_plan_prompt`) and by
#' the chat refinement prompt (`build_action_plan_chat_prompt`) so the LLM
#' always sees the same field list -- in particular the `quantite` block
#' that drives surface/volume/cost columns in the action table.
#'
#' @noRd
.action_plan_json_schema <- function() {
  paste(
    "JSON schema (return ONLY this object, no prose):",
    "{",
    '  "actions": [',
    "    {",
    '      "id": <existing action id when updating an action, else null>,',
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
    '        "cout_eur": <number or null, expense the action incurs>,',
    '        "revenu_eur": <number or null, revenue the action generates, e.g. timber sales>',
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
}


#' Economic constraint hint shared by action-plan prompts
#' @noRd
.action_plan_econ_hint <- function(fr) {
  if (fr) {
    paste0(
      "Contrainte economique : maximise le bilan cumule (somme des revenu_eur ",
      "moins somme des cout_eur) sur l'horizon. Quand c'est possible, place ",
      "une action genereuse en revenus AVANT une grosse depense, pour que les ",
      "recettes financent les investissements. Evite les annees ou la depense ",
      "cumulee depasse fortement la recette cumulee."
    )
  } else {
    paste0(
      "Economic constraint: maximise the cumulative balance (sum of revenu_eur ",
      "minus sum of cout_eur) over the horizon. When possible, schedule a ",
      "revenue-generating action BEFORE a large expense so that revenue funds ",
      "investments. Avoid years where cumulative spending strongly exceeds ",
      "cumulative income."
    )
  }
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

  schema <- .action_plan_json_schema()

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

  econ_hint <- .action_plan_econ_hint(fr)

  paste(
    intro,
    scope_str,
    ug_str,
    "",
    econ_hint,
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
      "`{ \"actions\": [ ... ] }` strictement conforme au sch\u00e9ma ci-dessous ",
      "(m\u00eames champs, m\u00eames contraintes), ",
      "encapsul\u00e9 dans un fence ```json ... ```. ",
      "Sinon, r\u00e9ponds en texte simple sans bloc JSON."
    )
  } else {
    "The user asks a question or makes a request about the current silvicultural action plan. If the request implies adding or modifying actions, return - in addition to your text answer - a JSON block `{ \"actions\": [ ... ] }` strictly matching the schema below (same fields, same constraints), wrapped in a ```json ... ``` fence. Otherwise reply in plain text only."
  }

  rules <- if (fr) {
    paste0(
      "R\u00e8gles pour le bloc JSON :\n",
      "- Pour chaque action retourn\u00e9e (nouvelle OU modifi\u00e9e), remplis ",
      "TOUS les champs du sch\u00e9ma, en particulier le bloc `quantite` ",
      "(volume_m3, surface_ha, nb_tiges, rdi, cout_eur, revenu_eur). ",
      "Mets une valeur num\u00e9rique chaque fois que l'information est ",
      "disponible ou peut \u00eatre estim\u00e9e \u00e0 partir du plan courant et ",
      "des commentaires ; n'utilise `null` que si l'estimation est ",
      "vraiment impossible.\n",
      "- Si tu modifies une action existante, reprends son `id` du plan ",
      "courant et r\u00e9emets l'action enti\u00e8re (tous les champs), pas ",
      "seulement les champs touch\u00e9s : les valeurs absentes seraient ",
      "perdues.\n",
      "- Pour une nouvelle action, laisse `id` \u00e0 null."
    )
  } else {
    paste0(
      "Rules for the JSON block:\n",
      "- For every returned action (new OR updated), fill ALL schema ",
      "fields, especially the `quantite` block (volume_m3, surface_ha, ",
      "nb_tiges, rdi, cout_eur, revenu_eur). Use a numeric value ",
      "whenever the information is available or can be estimated from ",
      "the current plan and the comments; only use `null` when an ",
      "estimate is truly impossible.\n",
      "- When updating an existing action, reuse its `id` from the ",
      "current plan and re-emit the whole action (all fields), not ",
      "just the touched ones: missing values would be lost.\n",
      "- For a new action, leave `id` as null."
    )
  }

  econ_hint <- .action_plan_econ_hint(fr)
  schema <- .action_plan_json_schema()

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
    rules,
    "",
    econ_hint,
    "",
    if (fr) "UGF disponibles :" else "Available UGFs:",
    paste(ctx$ug_ids, collapse = ", "),
    "",
    if (fr) "Horizon (an) :" else "Horizon (years):",
    as.character(ctx$horizon),
    "",
    if (fr) "Plan courant (JSON) :" else "Current plan (JSON):",
    plan_summary_json,
    "",
    if (fr) "Commentaires de synthese :" else "Synthesis comments:",
    if (nzchar(trimws(syn))) syn else if (fr) "(aucun)" else "(none)",
    "",
    if (fr) "Commentaires par famille :" else "Per-family comments:",
    if (nzchar(fam_block)) fam_block else if (fr) "(aucun)" else "(none)",
    "",
    sub("HORIZON", as.character(ctx$horizon %||% 20L), schema, fixed = TRUE),
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

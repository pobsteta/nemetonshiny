#' nemetonApp Configuration
#'
#' @description
#' Configuration constants and settings for the nemetonApp Shiny application.
#'
#' @name app_config
#' @keywords internal
NULL


#' Application configuration constants
#'
#' @noRd
APP_CONFIG <- list(
  # App metadata
  app_name = "N\u00e9m\u00e9ton",
  app_version = "0.11.0",
  app_title_fr = "N\u00e9m\u00e9ton - Diagnostic Forestier",
  app_title_en = "N\u00e9m\u00e9ton - Forest Diagnostic",

  # Limits
  # Note: can be overridden at runtime via run_app(max_parcels = ...)
  max_parcels = 30L,
  max_project_name_length = 100L,
  max_description_length = 500L,

  # Timeouts (milliseconds)
  api_timeout = 30000L,
  wfs_timeout = 60000L,
  computation_timeout = 600000L,  # 10 minutes

  # Retry settings
  max_retries = 3L,
  retry_delay = 2000L,  # 2 seconds

  # Performance
  # Taille du pool `future` (workers persistants). NULL = defaut borne
  # min(4, availableCores() - 2), surchargeable par NEMETON_PARALLEL_WORKERS.
  # Resolu par .resolve_parallel_workers() (service_compute.R).
  parallel_workers = NULL,

  # Resolution de travail (m) des indicateurs derives du terrain, appliquee par
  # `nemeton::.dem_working_res()` (coeur >= 0.169.0) a R1/R2/R3/W2/W3/F2/S1/S2.
  # Le coeur defaut a 2 m ; l'app impose 1 m - arbitrage produit, plus fidele au
  # MNT LiDAR HD 0,5 m livre par l'IGN.
  #
  # Mesure sur le MNT reel de Dabo (cgroup borne, terra memmax = 3 Go) :
  #   R3 : 1 m -> 32,9 s / 3,49 Go   | 2 m -> 10,3 s / 1,39 Go
  #   R2 : 1 m -> 20,0 s / 4,92 Go   | 2 m ->  7,7 s / 1,51 Go
  # Ecart de score R3 vs reference 0,5 m : 0,81 pt a 1 m, 1,40 pt a 2 m (/100).
  # R2 est le plus lourd des huit (neuf couches empilees) : c'est lui qui fixe
  # le pic. Le `memmax` du coeur borne terra, mais PAS le process R - surveiller
  # si une AOI nettement plus grande que Dabo (3 000 ha) apparait.
  topo_target_res = 1,

  # Resolution (m) de calcul du fond relief CVAT. Le defaut de
  # `foretaccess::build_cvat_precomputed()` est 0,5 m : sur l'emprise de Dabo
  # (AOI + 250 m) cela fait ~81 M cellules et `vat_combined()` y consomme
  # ~230 octets/cellule, soit une vingtaine de Go - OOM.
  #
  # C'est du gaspillage pur : `.paint_rvt_fond()` re-agrege le raster a 2000 px
  # de cote AVANT affichage. Sur une emprise de ~4,6 km, 2 m donne deja 2300 px,
  # soit juste au-dessus du plafond d'affichage.
  #
  # Mesure sur l'emprise de Dabo (cgroup borne a 12 Go) :
  #   0,5 m -> OOM   | 1 m -> 162 s / 4,38 Go | 2 m -> 45 s / 1,52 Go
  cvat_res_m = 2,

  # Cache settings
  cache_format = "parquet",

  # Project states
  project_states = c("draft", "downloading", "computing", "completed", "error"),

  # CRS (ADR-008)
  # Stockage interne : ETRS89/LAEA paneuropeen
  storage_crs = 3035L,  # EPSG:3035 ETRS89/LAEA (paneuropeen)
  # Calculs metriques : reprojection automatique en CRS national
  default_crs = 2154L,  # EPSG:2154 Lambert-93 (France, fallback)

  # LLM settings
  llm_provider = "mistral",
  llm_models = list(
    anthropic = "claude-sonnet-4-5-20250929",
    mistral = "mistral-large-latest",
    openai = "gpt-4o",
    google = "gemini-2.0-flash",
    deepseek = "deepseek-chat",
    ollama = "llama3.1"
  )
)


#' Get app configuration value
#'
#' @description
#' Looks up a configuration value. Runtime overrides set by
#' \code{\link{run_app}} (stored in \code{getOption("nemeton.app_options")})
#' take precedence over the static \code{APP_CONFIG} defaults, so callers can
#' pass \code{max_parcels} (and any other future knob) through
#' \code{run_app(max_parcels = ...)}.
#'
#' @param key Character. Configuration key to retrieve.
#' @param default Default value if key not found.
#' @return Configuration value.
#' @noRd
get_app_config <- function(key, default = NULL) {
  # Runtime overrides (from run_app) take precedence
  app_options <- getOption("nemeton.app_options")
  if (is.list(app_options) && key %in% names(app_options) &&
      !is.null(app_options[[key]])) {
    return(app_options[[key]])
  }
  if (key %in% names(APP_CONFIG)) {
    return(APP_CONFIG[[key]])
  }
  return(default)
}


#' Build the indicator-families table from the core
#'
#' @description
#' The 12 families, their indicators, labels and tooltips - **read from
#' `nemeton`**, not restated here.
#'
#' This file used to carry its own copy of the table, and that copy had drifted
#' in two ways that reached the screen:
#'
#'   * **A5 was missing from family A.** The indicator was computed by
#'     `service_compute.R` then filtered out at display time, so everything
#'     delivered for the urban-cooling indicator stayed invisible in the Air tab.
#'   * **The code-to-column pairing is positional**, and it used to be crossed
#'     for F and L (`F1` pointed at `indicateur_f2_erosion`). A local copy that
#'     compensated in its own `indicator_labels` but not in the
#'     `indicator_<code>` i18n keys produced a label that depended on which copy
#'     the reader hit - the erosion map came out as "F1 - Fertilite des sols".
#'     The core has since uncrossed both families - L in v0.176.0 (spec 045), F
#'     in v0.182.0 (spec 049) - so nothing is crossed today. What reading from
#'     the core prevents is the *drift*, not that one crossing: the next rename
#'     lands here for free.
#'
#' The core pairs code, column and label **explicitly, row by row**
#' (`nemeton::indicator_labels()`), so reading from it removes the class of bug
#' rather than one instance of it.
#'
#' The returned shape is the one the former literal had, so the twelve consumers
#' (`get_family_config()`, `mod_family`, `utils_theme`, `service_export`,
#' `llm_prompts`, `mod_synthesis`...) need no change.
#'
#' `indicator_docs` (spec 052) follows the same rule for the long-form fact
#' sheets: it carries only what the core declares, keyed by indicator code, and
#' is empty for every family that has no sheet - which is most of them today.
#' Reading it here rather than hard-coding a URL is what makes the icon appear
#' by itself the day the core publishes a second sheet.
#'
#' @return Named list of 12 families, ordered as the core orders them. Each
#'   carries `indicator_labels`, `indicator_tooltips` and `indicator_docs`,
#'   keyed by indicator code.
#'
#' @noRd
.build_indicator_families <- function() {
  fam <- nemeton::indicator_families()
  lab <- nemeton::indicator_labels(lang = "fr")
  # Deuxieme lecture, pour les fiches uniquement (spec 052). `doc_url` /
  # `doc_lang` dependent de la langue DEMANDEE, pas seulement de la langue
  # servie : quand la fiche n'existe pas dans la langue courante, le coeur rend
  # celle de l'autre langue et le dit dans `doc_lang`. Les colonnes
  # `doc_url_fr` / `doc_url_en` portent deja ce repli et ne permettent donc pas
  # de reconstruire `doc_lang` - il faut la reponse du coeur pour chaque langue.
  # Deux appels sur une table statique de 41 lignes, lus une seule fois par
  # session (`delayedAssign`).
  lab_en <- nemeton::indicator_labels(lang = "en")

  pick <- function(df, col) {
    if (col %in% names(df)) df[[col]] else rep(NA_character_, nrow(df))
  }

  out <- lapply(seq_len(nrow(fam)), function(i) {
    code  <- fam$code[i]
    codes <- unlist(fam$indicators[[i]], use.names = FALSE)
    cols  <- unlist(fam$column_names[[i]], use.names = FALSE)

    rows <- lab[lab$family == code, , drop = FALSE]
    rows_en <- lab_en[lab_en$family == code, , drop = FALSE]
    bilingual <- function(fr_col, en_col) {
      vals <- lapply(codes, function(cd) {
        r <- rows[rows$code == cd, , drop = FALSE]
        if (nrow(r) == 0L) return(NULL)
        list(fr = pick(r, fr_col)[1], en = pick(r, en_col)[1])
      })
      names(vals) <- codes
      vals[!vapply(vals, is.null, logical(1))]
    }

    # Fiches longues (spec 052). Absentes des coeurs < 0.192.0 : `pick()` rend
    # alors des NA et aucune entree ne survit - l'icone ne s'affiche pas, ce
    # qui est le comportement voulu, pas une erreur.
    doc_entry <- function(r) {
      if (nrow(r) == 0L) return(NULL)
      url <- pick(r, "doc_url")[1]
      if (is.na(url) || !nzchar(url)) return(NULL)
      list(url = url, lang = pick(r, "doc_lang")[1])
    }
    docs <- function() {
      vals <- lapply(codes, function(cd) {
        entry <- list(
          fr = doc_entry(rows[rows$code == cd, , drop = FALSE]),
          en = doc_entry(rows_en[rows_en$code == cd, , drop = FALSE])
        )
        if (is.null(entry$fr) && is.null(entry$en)) return(NULL)
        entry
      })
      names(vals) <- codes
      vals[!vapply(vals, is.null, logical(1))]
    }

    list(
      code = code,
      name_fr = fam$name_fr[i],
      name_en = fam$name_en[i],
      icon = fam$icon[i],
      color = fam$color[i],
      indicators = codes,
      column_names = cols,
      indicator_labels = bilingual("label_fr", "label_en"),
      indicator_tooltips = bilingual("tooltip_fr", "tooltip_en"),
      indicator_docs = docs()
    )
  })

  names(out) <- fam$code
  out
}


#' Indicator families configuration
#'
#' @description
#' Assembled from the core on first use. `delayedAssign()` rather than an eager
#' call: this file is evaluated while the package is being built, before
#' `nemeton` is necessarily available, and every consumer uses the name as a
#' plain variable - a promise keeps all of them unchanged.
#'
#' @noRd
delayedAssign("INDICATOR_FAMILIES", .build_indicator_families())


#' Get all indicator family codes
#'
#' @return Character vector of family codes
#' @noRd
get_family_codes <- function() {
  names(INDICATOR_FAMILIES)
}


#' Get family configuration
#'
#' @param code Character. Family code (e.g., "C", "B", "W")
#' @return List with family configuration, or NULL if not found
#' @noRd
get_family_config <- function(code) {
  INDICATOR_FAMILIES[[toupper(code)]]
}


#' Get all indicator codes
#'
#' @return Character vector of all indicator codes
#' @noRd
get_all_indicator_codes <- function() {
  unlist(lapply(INDICATOR_FAMILIES, function(f) f$indicators), use.names = FALSE)
}


#' Get all indicator column names
#'
#' @return Character vector of all long-form column names
#' @noRd
get_all_column_names <- function() {
  unlist(lapply(INDICATOR_FAMILIES, function(f) f$column_names), use.names = FALSE)
}


#' Label an indicator column, read from the core
#'
#' @description
#' Resolves a long-form column name (`indicateur_f2_erosion`) to the label of
#' the quantity that column actually carries, in the requested language.
#'
#' A column is named after the **function that fills it**, and that name used
#' to contradict the code: `F1` pointed at `indicateur_f2_erosion`, which
#' carries erosion. The core has uncrossed both families since - L in v0.176.0
#' (spec 045), F in v0.182.0 (spec 049) - so code, column and slug agree today.
#' The helper stays because the agreement is not a property one can rely on: it
#' pairs code, column and label explicitly row by row
#' (`nemeton::indicator_labels()`), so the label describes the values on screen
#' whatever the core renames next. A local table indexed by column name follows
#' the *slug*, and inverts again at the first rename - which is what this
#' helper replaces.
#'
#' @param col_name Character. Column name, with or without the `_norm` suffix.
#' @param lang Character. `"fr"` or `"en"`.
#' @param with_family Logical. When `TRUE`, prefix with the family name
#'   (`"Paysage - Sylvosphere (effet lisiere)"`).
#'
#' @return Character label, or `NULL` when the column is unknown to the core -
#'   the caller decides what to fall back to.
#'
#' @noRd
indicator_label_by_column <- function(col_name, lang = "fr", with_family = FALSE) {
  if (is.null(col_name) || !nzchar(col_name)) return(NULL)
  base <- sub("_norm$", "", col_name)
  lang <- if (identical(lang, "en")) "en" else "fr"

  for (fam in INDICATOR_FAMILIES) {
    idx <- match(base, fam$column_names %||% character(0))
    if (is.na(idx) || idx > length(fam$indicators)) next

    lbl <- fam$indicator_labels[[fam$indicators[idx]]]
    txt <- lbl[[lang]] %||% lbl[["fr"]] %||% lbl[["en"]]
    if (is.null(txt) || is.na(txt) || !nzchar(txt)) return(NULL)

    if (!isTRUE(with_family)) return(txt)
    fam_name <- (if (identical(lang, "en")) fam$name_en else fam$name_fr) %||% fam$code
    return(paste0(fam_name, " - ", txt))
  }
  NULL
}


#' Get column-to-family mapping
#'
#' @description
#' Returns a named character vector mapping column names to family codes.
#' Supports both short codes (C1, B2) and long-form names (indicateur_c1_biomasse).
#'
#' @return Named character vector (names = column names, values = family codes)
#' @noRd
get_column_family_map <- function() {
  result <- character(0)
  for (fam in INDICATOR_FAMILIES) {
    # Map long-form column_names to family code
    if (!is.null(fam$column_names)) {
      names_vec <- rep(fam$code, length(fam$column_names))
      names(names_vec) <- fam$column_names
      result <- c(result, names_vec)
    }
    # Map short indicators to family code
    names_vec2 <- rep(fam$code, length(fam$indicators))
    names(names_vec2) <- fam$indicators
    result <- c(result, names_vec2)
  }
  result
}


#' Data sources configuration
#'
#' @noRd
DATA_SOURCES <- list(
  cadastre = list(
    name = "cadastre",
    primary = "api_cadastre",
    fallback = "happign",
    required = TRUE
  ),
  bdforet = list(
    name = "bdforet",
    primary = "ign_wfs",
    fallback = "local_cache",
    required = TRUE
  ),
  protection = list(
    name = "protection",
    primary = "inpn_wfs",
    fallback = "local_cache",
    required = FALSE
  ),
  oso = list(
    name = "oso",
    primary = "recherche_data_gouv",
    fallback = "local_cache",
    required = FALSE
  ),
  hydro = list(
    name = "hydro",
    primary = "sandre_wfs",
    fallback = "local_cache",
    required = FALSE
  ),
  mnt = list(
    name = "mnt",
    primary = "ign_wfs",
    fallback = "local_cache",
    required = FALSE
  )
)


#' Get data source configuration
#'
#' @param name Character. Data source name
#' @return List with source configuration
#' @noRd
get_data_source_config <- function(name) {
  DATA_SOURCES[[name]]
}

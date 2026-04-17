#' Management Group Profiles Configuration
#'
#' @description
#' Loads and exposes the configurable profiles used to classify UGF
#' (Unités de Gestion Forestière). The profile selected for a project
#' determines:
#'   - the dropdown label (e.g. "Groupe d'aménagement" for ONF,
#'     "Groupe de gestion" for CRPF, "Zone" for OFB/ENS)
#'   - the list of allowed codes + human-readable labels
#'   - the color palette used on the map
#'   - which codes are considered "hors production" (excluded from
#'     production-oriented indicators)
#'
#' Profiles are defined in `inst/config/groupes_amenagement.yaml` and can
#' be overridden by a user-level file in
#' `~/.local/share/nemeton/groupes_amenagement.yaml` (or via the
#' `nemeton.groupes_config` option).
#'
#' @name config_groupes
#' @keywords internal
NULL


# ==============================================================================
# Loading
# ==============================================================================

#' Load the groupes configuration from YAML
#'
#' @description
#' Reads the package-shipped YAML first, then applies the user-level override
#' if one exists. Validates the structure minimally.
#'
#' @return List with `default_profile` and `profiles`.
#' @noRd
load_groupes_config <- function() {
  # 1. Package config (always present)
  pkg_path <- system.file("config", "groupes_amenagement.yaml",
                          package = "nemetonshiny")
  cfg <- if (nzchar(pkg_path) && file.exists(pkg_path)) {
    tryCatch(
      yaml::read_yaml(pkg_path),
      error = function(e) {
        warning(sprintf("Failed to load package groupes config: %s", e$message))
        NULL
      }
    )
  } else {
    NULL
  }

  if (is.null(cfg)) {
    # Fallback to a minimal hardcoded default
    cfg <- list(
      default_profile = "generic",
      profiles = list(
        generic = list(
          label = list(fr = "Generique", en = "Generic"),
          field_label = list(fr = "Categorie", en = "Category"),
          description = list(fr = "", en = ""),
          groupes = list()
        )
      )
    )
  }

  # 2. User override
  default_base <- if (requireNamespace("rappdirs", quietly = TRUE)) {
    rappdirs::user_data_dir("nemeton", "nemeton")
  } else {
    file.path(Sys.getenv("HOME"), ".nemeton")
  }
  user_path <- getOption(
    "nemeton.groupes_config",
    file.path(default_base, "groupes_amenagement.yaml")
  )
  if (file.exists(user_path)) {
    user_cfg <- tryCatch(
      yaml::read_yaml(user_path),
      error = function(e) {
        warning(sprintf("Failed to load user groupes config: %s", e$message))
        NULL
      }
    )
    if (!is.null(user_cfg)) {
      # Merge: user default_profile wins if provided; user profiles extend/override
      if (!is.null(user_cfg$default_profile)) {
        cfg$default_profile <- user_cfg$default_profile
      }
      if (is.list(user_cfg$profiles)) {
        for (k in names(user_cfg$profiles)) {
          cfg$profiles[[k]] <- user_cfg$profiles[[k]]
        }
      }
    }
  }

  cfg
}


#' Get groupes configuration (cached)
#' @noRd
get_groupes_config <- function() {
  if (!exists("groupes_config", envir = .nemeton_env) ||
      is.null(.nemeton_env$groupes_config)) {
    .nemeton_env$groupes_config <- load_groupes_config()
  }
  .nemeton_env$groupes_config
}


#' Force reload of the groupes configuration (useful for development)
#' @noRd
reload_groupes_config <- function() {
  .nemeton_env$groupes_config <- load_groupes_config()
  invisible(.nemeton_env$groupes_config)
}


# ==============================================================================
# Accessors (profile-level)
# ==============================================================================

#' Default profile key
#' @noRd
get_default_groupes_profile <- function() {
  cfg <- get_groupes_config()
  cfg$default_profile %||% "generic"
}


#' Resolve a profile key to a profile definition
#'
#' @description
#' Falls back to the default profile, then to the first available profile,
#' so a bad or missing key never crashes the UI.
#' @noRd
resolve_groupes_profile <- function(profile_key = NULL) {
  cfg <- get_groupes_config()
  profiles <- cfg$profiles

  if (!is.null(profile_key) && profile_key %in% names(profiles)) {
    return(profiles[[profile_key]])
  }
  default_key <- get_default_groupes_profile()
  if (default_key %in% names(profiles)) {
    return(profiles[[default_key]])
  }
  if (length(profiles) > 0) {
    return(profiles[[1]])
  }
  NULL
}


#' List profile keys
#' @noRd
list_groupes_profiles <- function() {
  names(get_groupes_config()$profiles %||% list())
}


#' Choices for a profile selectInput
#'
#' @param lang Character. "fr" or "en".
#' @return Named character vector (labels -> profile keys).
#' @noRd
get_groupes_profile_choices <- function(lang = "fr") {
  cfg <- get_groupes_config()
  keys <- names(cfg$profiles %||% list())
  if (length(keys) == 0) return(stats::setNames(character(0), character(0)))

  labels <- vapply(keys, function(k) {
    prof <- cfg$profiles[[k]]
    lbl <- prof$label[[lang]] %||% prof$label[["fr"]] %||% prof$label[["en"]] %||% k
    as.character(lbl)
  }, character(1))

  stats::setNames(keys, labels)
}


# ==============================================================================
# Accessors (groupe-level within a profile)
# ==============================================================================

#' Dropdown field label for the groupe selector
#'
#' @param profile_key Character. Profile key (or NULL for default).
#' @param lang Character. "fr" or "en".
#' @return Character string (e.g. "Groupe d'amenagement", "Zone").
#' @noRd
get_groupes_field_label <- function(profile_key = NULL, lang = "fr") {
  prof <- resolve_groupes_profile(profile_key)
  if (is.null(prof)) return(if (lang == "fr") "Categorie" else "Category")
  lbl <- prof$field_label[[lang]] %||% prof$field_label[["fr"]] %||%
    prof$field_label[["en"]] %||% (if (lang == "fr") "Categorie" else "Category")
  as.character(lbl)
}


#' List of groupe codes for a profile
#' @noRd
get_groupes_codes <- function(profile_key = NULL) {
  prof <- resolve_groupes_profile(profile_key)
  if (is.null(prof) || is.null(prof$groupes)) return(character(0))
  vapply(prof$groupes, function(g) as.character(g$code), character(1))
}


#' Named choices (label -> code) for a groupe selectInput
#'
#' @param profile_key Character. Profile key.
#' @param lang Character. "fr" or "en".
#' @param include_empty Logical. Prepend an empty choice.
#' @return Named character vector.
#' @noRd
get_groupes_choices <- function(profile_key = NULL, lang = "fr",
                                include_empty = TRUE) {
  prof <- resolve_groupes_profile(profile_key)
  if (is.null(prof) || is.null(prof$groupes) || length(prof$groupes) == 0) {
    if (include_empty) return(c("---" = ""))
    return(stats::setNames(character(0), character(0)))
  }

  codes <- vapply(prof$groupes, function(g) as.character(g$code), character(1))
  labels <- vapply(prof$groupes, function(g) {
    lbl <- g$label[[lang]] %||% g$label[["fr"]] %||% g$label[["en"]] %||% g$code
    # Show both code and label for clarity
    sprintf("%s - %s", g$code, lbl)
  }, character(1))

  out <- stats::setNames(codes, labels)
  if (include_empty) out <- c("---" = "", out)
  out
}


#' Color for a single groupe code in a profile
#'
#' @noRd
get_groupe_color <- function(code, profile_key = NULL,
                             default = "#808080") {
  if (is.null(code) || is.na(code) || !nzchar(code)) return(default)
  prof <- resolve_groupes_profile(profile_key)
  if (is.null(prof) || is.null(prof$groupes)) return(default)
  for (g in prof$groupes) {
    if (identical(as.character(g$code), as.character(code))) {
      col <- g$color %||% default
      return(as.character(col))
    }
  }
  default
}


#' Named color vector (code -> color) for a profile
#'
#' @noRd
get_groupes_colors <- function(profile_key = NULL) {
  prof <- resolve_groupes_profile(profile_key)
  if (is.null(prof) || is.null(prof$groupes)) {
    return(stats::setNames(character(0), character(0)))
  }
  codes <- vapply(prof$groupes, function(g) as.character(g$code), character(1))
  colors <- vapply(prof$groupes, function(g) as.character(g$color %||% "#808080"),
                   character(1))
  stats::setNames(colors, codes)
}


#' Label for a single groupe code
#'
#' @noRd
get_groupe_label <- function(code, profile_key = NULL, lang = "fr") {
  if (is.null(code) || is.na(code) || !nzchar(code)) return("")
  prof <- resolve_groupes_profile(profile_key)
  if (is.null(prof) || is.null(prof$groupes)) return(as.character(code))
  for (g in prof$groupes) {
    if (identical(as.character(g$code), as.character(code))) {
      lbl <- g$label[[lang]] %||% g$label[["fr"]] %||% g$label[["en"]] %||% g$code
      return(as.character(lbl))
    }
  }
  as.character(code)
}


#' Hors-production flag for a single groupe code
#'
#' @noRd
is_groupe_hors_production <- function(code, profile_key = NULL) {
  if (is.null(code) || is.na(code) || !nzchar(code)) return(FALSE)
  prof <- resolve_groupes_profile(profile_key)
  if (is.null(prof) || is.null(prof$groupes)) return(FALSE)
  for (g in prof$groupes) {
    if (identical(as.character(g$code), as.character(code))) {
      return(isTRUE(g$hors_production))
    }
  }
  FALSE
}

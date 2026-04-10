#' NDP System (Niveau De Precision)
#'
#' @description
#' NDP measures the QUALITY of input data, not the number of families calculated.
#' All 12 families are always calculated, but with increasing precision as NDP rises.
#'
#' The system uses Fibonacci weighting (1, 1, 2, 3, 5) and golden ratio confidence
#' (phi) to express data reliability.
#'
#' Copied from nemeton core package to make nemetonShiny self-contained for NDP.
#'
#' @name ndp
#' @keywords internal
NULL


# ================================================================
# NDP_LEVELS: Configuration des 5 niveaux de precision
# ================================================================

#' NDP level definitions
#'
#' Each level defines its Fibonacci weight, cumulative confidence, and
#' the data sources that characterize it.
#'
#' @format A list of 5 elements, each with: ndp, key, name, fibonacci,
#'   confidence, sources.
#' @keywords internal
NDP_LEVELS <- list(
  list(
    ndp = 0L,
    key = "ndp_decouverte",
    name = "D\u00e9couverte",
    fibonacci = 1L,
    confidence = 1 / 12,
    sources = c("sentinel_2", "worldclim", "bd_topo", "mnt_25m")
  ),
  list(
    ndp = 1L,
    key = "ndp_observation",
    name = "Observation",
    fibonacci = 1L,
    confidence = 2 / 12,
    sources = c("ign_rge_alti", "bd_ortho", "lidar_hd")
  ),
  list(
    ndp = 2L,
    key = "ndp_exploration",
    name = "Exploration",
    fibonacci = 2L,
    confidence = 4 / 12,
    sources = c("drone_rgb", "lidar_drone")
  ),
  list(
    ndp = 3L,
    key = "ndp_diagnostic",
    name = "Diagnostic",
    fibonacci = 3L,
    confidence = 7 / 12,
    sources = c("inventaire_terrain")
  ),
  list(
    ndp = 4L,
    key = "ndp_jumeau",
    name = "Jumeau",
    fibonacci = 5L,
    confidence = 12 / 12,
    sources = c("scanner_terrestre", "modele_3d")
  )
)


# ================================================================
# Accessors
# ================================================================

#' Get NDP level configuration
#'
#' @param ndp Integer. NDP level (0-4).
#' @return A list with elements: ndp, key, name, fibonacci, confidence, sources.
#' @noRd
get_ndp_level <- function(ndp) {
  ndp <- as.integer(ndp)
  if (length(ndp) != 1 || is.na(ndp) || ndp < 0L || ndp > 4L) {
    stop("ndp must be a single integer between 0 and 4", call. = FALSE)
  }
  NDP_LEVELS[[ndp + 1L]]
}


#' Get NDP level name
#' @param ndp Integer. NDP level (0-4).
#' @return Character. French name of the level.
#' @noRd
get_ndp_name <- function(ndp) {
  get_ndp_level(ndp)$name
}


#' Get NDP Fibonacci weight
#' @param ndp Integer. NDP level (0-4).
#' @return Integer. Fibonacci weight (1, 1, 2, 3, or 5).
#' @noRd
get_ndp_weight <- function(ndp) {
  get_ndp_level(ndp)$fibonacci
}


#' Get NDP confidence ratio
#' @param ndp Integer. NDP level (0-4).
#' @return Numeric. Confidence ratio between 0 and 1.
#' @noRd
get_ndp_confidence <- function(ndp) {
  get_ndp_level(ndp)$confidence
}


# ================================================================
# ndp_table
# ================================================================

#' NDP levels as a data.frame
#' @return A data.frame with columns: ndp, key, name, fibonacci, confidence.
#' @noRd
ndp_table <- function() {
  data.frame(
    ndp = vapply(NDP_LEVELS, `[[`, integer(1), "ndp"),
    key = vapply(NDP_LEVELS, `[[`, character(1), "key"),
    name = vapply(NDP_LEVELS, `[[`, character(1), "name"),
    fibonacci = vapply(NDP_LEVELS, `[[`, integer(1), "fibonacci"),
    confidence = vapply(NDP_LEVELS, `[[`, numeric(1), "confidence"),
    stringsAsFactors = FALSE
  )
}


# ================================================================
# detect_ndp
# ================================================================

#' Detect NDP level from data
#'
#' @param data An sf object or data.frame with source attributes.
#' @return Integer. Detected NDP level (0-4).
#' @noRd
detect_ndp <- function(data) {
  if (is.null(data)) return(0L)

  level_checks <- list(
    function(d) isTRUE(attr(d, "has_lidar_hd")),
    function(d) isTRUE(attr(d, "has_drone_rgb")) || isTRUE(attr(d, "has_lidar_drone")),
    function(d) isTRUE(attr(d, "has_inventaire_terrain")),
    function(d) isTRUE(attr(d, "has_scanner_terrestre")) || isTRUE(attr(d, "has_modele_3d"))
  )

  ndp <- 0L
  for (check in level_checks) {
    if (check(data)) {
      ndp <- ndp + 1L
    } else {
      break
    }
  }

  ndp
}


#' Detect NDP level from nemeton_layers object
#' @param layers A nemeton_layers object.
#' @return Integer. Detected NDP level (0-4).
#' @noRd
detect_ndp_from_layers <- function(layers) {
  if (!inherits(layers, "nemeton_layers")) return(0L)

  has_lidar_hd <- !is.null(layers$rasters$lidar_mnh) ||
                  !is.null(layers$rasters$lidar_mnt) ||
                  length(layers$point_clouds) > 0

  if (!has_lidar_hd) return(0L)
  1L
}


#' Set NDP source attributes on a data object
#' @param data An sf object or data.frame.
#' @param layers A nemeton_layers object, or NULL.
#' @return The data object with NDP attributes set.
#' @noRd
set_ndp_attributes <- function(data, layers = NULL) {
  if (!is.null(layers) && inherits(layers, "nemeton_layers")) {
    attr(data, "has_lidar_hd") <- !is.null(layers$rasters$lidar_mnh) ||
                                   !is.null(layers$rasters$lidar_mnt) ||
                                   length(layers$point_clouds) > 0
  } else {
    attr(data, "has_lidar_hd") <- FALSE
  }

  attr(data, "has_drone_rgb") <- FALSE
  attr(data, "has_lidar_drone") <- FALSE
  attr(data, "has_inventaire_terrain") <- FALSE
  attr(data, "has_scanner_terrestre") <- FALSE
  attr(data, "has_modele_3d") <- FALSE

  attr(data, "ndp_detected") <- detect_ndp(data)

  data
}


#' Restore NDP attributes from project metadata
#'
#' After loading indicators from parquet (which strips R attributes),
#' restores the NDP attributes from the persisted metadata.
#'
#' @param data An sf object or data.frame.
#' @param ndp_level Integer. The NDP level from project metadata.
#' @return The data object with NDP attributes restored.
#' @noRd
restore_ndp_attributes <- function(data, ndp_level) {
  ndp_level <- as.integer(ndp_level %||% 0L)
  if (is.na(ndp_level)) ndp_level <- 0L

  attr(data, "has_lidar_hd") <- ndp_level >= 1L
  attr(data, "has_drone_rgb") <- ndp_level >= 2L
  attr(data, "has_lidar_drone") <- ndp_level >= 2L
  attr(data, "has_inventaire_terrain") <- ndp_level >= 3L
  attr(data, "has_scanner_terrestre") <- ndp_level >= 4L
  attr(data, "has_modele_3d") <- ndp_level >= 4L
  attr(data, "ndp_detected") <- ndp_level

  data
}


#' Detect NDP level from project cache directory
#' @param project_path Character. Path to the project directory.
#' @return Integer. Detected NDP level (0-4).
#' @noRd
detect_ndp_from_cache <- function(project_path) {
  if (is.null(project_path) || !dir.exists(project_path)) return(0L)

  cache_dir <- file.path(project_path, "cache", "layers")
  if (!dir.exists(cache_dir)) return(0L)

  has_lidar_hd <- dir.exists(file.path(cache_dir, "lidar_mnh")) ||
                  dir.exists(file.path(cache_dir, "lidar_mnt")) ||
                  dir.exists(file.path(cache_dir, "lidar_nuage")) ||
                  any(grepl("lidar_mn[ht]", list.files(cache_dir), ignore.case = TRUE))

  if (!has_lidar_hd) return(0L)
  1L
}


# ================================================================
# compute_general_index
# ================================================================

#' Compute Fibonacci-weighted general index
#'
#' @param family_scores Named numeric vector of family scores (0-100).
#' @param ndp Integer. NDP level (0-4). Default 0.
#' @return A list with: score, ndp, confidence, weight, n_families.
#' @noRd
compute_general_index <- function(family_scores, ndp = 0L) {
  ndp <- as.integer(ndp)
  level <- get_ndp_level(ndp)

  nms <- names(family_scores)
  if (!is.null(nms)) {
    nms <- vapply(nms, function(n) {
      code <- get_famille_code(n)
      if (!is.na(code)) code else sub("^famille_", "", n)
    }, character(1), USE.NAMES = FALSE)
    names(family_scores) <- nms
  }

  valid <- !is.na(family_scores)
  if (!any(valid)) {
    return(list(
      score = NA_real_,
      ndp = ndp,
      confidence = level$confidence,
      weight = level$fibonacci,
      n_families = 0L
    ))
  }

  valid_scores <- family_scores[valid]
  score <- round(mean(valid_scores, na.rm = TRUE), 1)

  list(
    score = score,
    ndp = ndp,
    confidence = level$confidence,
    weight = level$fibonacci,
    n_families = length(valid_scores)
  )
}


#' Compute general index with mixed NDP per indicator
#'
#' @param family_scores Named numeric vector of family scores (0-100).
#' @param ndp_per_indicator Named integer vector mapping family codes to NDP levels.
#' @return A list with: score, confidence, n_families, weights_used.
#' @noRd
compute_general_index_mixed <- function(family_scores, ndp_per_indicator) {
  nms <- names(family_scores)
  if (!is.null(nms)) {
    nms <- vapply(nms, function(n) {
      code <- get_famille_code(n)
      if (!is.na(code)) code else sub("^famille_", "", n)
    }, character(1), USE.NAMES = FALSE)
    names(family_scores) <- nms
  }

  ndp_nms <- names(ndp_per_indicator)
  if (!is.null(ndp_nms)) {
    ndp_nms <- vapply(ndp_nms, function(n) {
      code <- get_famille_code(n)
      if (!is.na(code)) code else sub("^famille_", "", n)
    }, character(1), USE.NAMES = FALSE)
    names(ndp_per_indicator) <- ndp_nms
  }

  common <- intersect(names(family_scores), names(ndp_per_indicator))
  if (length(common) == 0) {
    return(list(score = NA_real_, confidence = 0, n_families = 0L, weights_used = integer(0)))
  }

  scores <- family_scores[common]
  ndps <- ndp_per_indicator[common]

  valid <- !is.na(scores) & !is.na(ndps)
  if (!any(valid)) {
    return(list(score = NA_real_, confidence = 0, n_families = 0L, weights_used = integer(0)))
  }

  scores <- scores[valid]
  ndps <- ndps[valid]

  weights <- vapply(ndps, get_ndp_weight, integer(1))
  score <- round(sum(scores * weights) / sum(weights), 1)
  confidences <- vapply(ndps, get_ndp_confidence, numeric(1))
  avg_confidence <- mean(confidences)

  list(
    score = score,
    confidence = avg_confidence,
    n_families = length(scores),
    weights_used = weights
  )
}


# ================================================================
# Widgets HTML pour Shiny
# ================================================================

#' NDP badge HTML widget
#'
#' @param ndp Integer. NDP level (0-4).
#' @param lang Character. Language ("fr" or "en"). Default "fr".
#' @return An htmltools tag object.
#' @noRd
ndp_badge <- function(ndp, lang = "fr") {
  ndp <- as.integer(ndp)
  level <- get_ndp_level(ndp)

  colors <- c(
    "#6c757d",  # NDP 0 - gray
    "#17a2b8",  # NDP 1 - cyan
    "#28a745",  # NDP 2 - green
    "#ffc107",  # NDP 3 - yellow
    "#fd7e14"   # NDP 4 - orange
  )
  color <- colors[ndp + 1L]

  names_en <- c("Discovery", "Observation", "Exploration", "Diagnostic", "Digital Twin")
  name <- if (lang == "en") names_en[ndp + 1L] else level$name

  htmltools::tags$span(
    class = "badge",
    style = paste0(
      "background-color: ", color, "; ",
      "color: white; ",
      "font-size: 0.85rem; ",
      "padding: 4px 10px;"
    ),
    paste0("NDP ", ndp, " \u2013 ", name)
  )
}


#' NDP confidence progress bar widget
#'
#' @param ndp Integer. NDP level (0-4).
#' @param lang Character. Language ("fr" or "en"). Default "fr".
#' @return An htmltools tag object.
#' @noRd
ndp_progress_bar <- function(ndp, lang = "fr") {
  ndp <- as.integer(ndp)
  level <- get_ndp_level(ndp)
  pct <- round(level$confidence * 100, 1)

  label <- if (lang == "en") {
    paste0("Confidence \u03c6: ", pct, "%")
  } else {
    paste0("Confiance \u03c6 : ", pct, "%")
  }

  htmltools::div(
    class = "mt-1",
    htmltools::tags$small(class = "text-muted", label),
    htmltools::div(
      class = "progress",
      style = "height: 8px;",
      htmltools::div(
        class = "progress-bar bg-success",
        role = "progressbar",
        style = paste0("width: ", pct, "%;"),
        `aria-valuenow` = pct,
        `aria-valuemin` = "0",
        `aria-valuemax` = "100"
      )
    )
  )
}

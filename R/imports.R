# Import internal functions from nemeton core package
# These functions are not exported by nemeton but are needed by the Shiny app

# Family system
get_famille_col <- nemeton:::get_famille_col
get_famille_code <- nemeton:::get_famille_code
FAMILLE_NMT_MAP <- nemeton:::FAMILLE_NMT_MAP

# i18n (messages CLI)
msg_info <- nemeton:::msg_info
msg_warn <- nemeton:::msg_warn
msg_error <- nemeton:::msg_error
msg_success <- nemeton:::msg_success
msg <- nemeton:::msg
get_language <- nemeton:::get_language

# Normalization
normalize_indicator <- nemeton:::normalize_indicator

# Utilities
resolve_raster_layer <- nemeton:::resolve_raster_layer
resolve_vector_layer <- nemeton:::resolve_vector_layer
safe_extract <- nemeton:::safe_extract
as_pure_sf <- nemeton:::as_pure_sf
enrich_parcels_bdforet <- nemeton:::enrich_parcels_bdforet
map_essence_to_species <- nemeton:::map_essence_to_species
get_allometric_coefficients <- nemeton:::get_allometric_coefficients
clean_indicator_name <- nemeton:::clean_indicator_name


#' Extract DEM raster from nemeton_layers
#'
#' Prefers LiDAR MNT over BD ALTI DEM. Returns NULL if no DEM available.
#'
#' @param layers A nemeton_layers object.
#' @return A SpatRaster or NULL.
#' @noRd
get_dem_raster <- function(layers) {
  rasters <- layers$rasters
  if (!is.null(rasters$lidar_mnt)) return(rasters$lidar_mnt)
  if (!is.null(rasters$dem)) return(rasters$dem)
  NULL
}


#' Set NDP attributes on indicator results
#'
#' Annotates results with data source availability flags from the layers
#' object. These attributes are later read by detect_ndp() to determine
#' the evidence level (NDP 0-4).
#'
#' @param results data.frame or sf object with indicator columns.
#' @param layers nemeton_layers object with rasters/vectors/point_clouds.
#' @return The results object with ndp source attributes set.
#' @noRd
set_ndp_attributes <- function(results, layers) {
  rasters <- layers$rasters %||% list()
  vectors <- layers$vectors %||% list()
  point_clouds <- layers$point_clouds %||% list()

  attr(results, "ndp_has_ndvi") <- !is.null(rasters$ndvi)
  attr(results, "ndp_has_dem") <- !is.null(rasters$dem) || !is.null(rasters$lidar_mnt)
  attr(results, "ndp_has_lidar_mnh") <- !is.null(rasters$lidar_mnh)
  attr(results, "ndp_has_lidar_mnt") <- !is.null(rasters$lidar_mnt)
  attr(results, "ndp_has_lidar_copc") <- length(point_clouds) > 0
  attr(results, "ndp_has_bdforet") <- !is.null(vectors$bdforet)
  attr(results, "ndp_has_field_data") <- any(c("species", "dbh", "density") %in% names(results))

  results
}


#' Detect NDP level from indicator attributes
#'
#' Reads the ndp source flags set by set_ndp_attributes() and determines
#' the evidence level:
#'
#' - NDP 0: No spatial data
#' - NDP 1: Remote sensing only (NDVI, satellite)
#' - NDP 2: + Cartographic data (BD Foret, IGN BD TOPO)
#' - NDP 3: + LiDAR data
#' - NDP 4: + Field inventory data
#'
#' @param indicators data.frame with ndp_* attributes.
#' @return Integer NDP level (0-4).
#' @noRd
detect_ndp <- function(indicators) {
  if (isTRUE(attr(indicators, "ndp_has_field_data"))) return(4L)
  if (isTRUE(attr(indicators, "ndp_has_lidar_mnh")) ||
      isTRUE(attr(indicators, "ndp_has_lidar_copc"))) return(3L)
  if (isTRUE(attr(indicators, "ndp_has_bdforet"))) return(2L)
  if (isTRUE(attr(indicators, "ndp_has_ndvi")) ||
      isTRUE(attr(indicators, "ndp_has_dem"))) return(1L)
  0L
}


#' Detect NDP level from cached indicator files
#'
#' Fallback when attributes are not available (e.g., after parquet load).
#' Checks which indicator files exist on disk to infer data availability.
#'
#' @param project_path Character. Path to the project directory.
#' @return Integer NDP level (0-4).
#' @noRd
detect_ndp_from_cache <- function(project_path) {
  data_dir <- file.path(project_path, "data")
  if (!dir.exists(data_dir)) return(0L)

  files <- list.files(data_dir, pattern = "^indicateur_.*\\.parquet$")

  # LiDAR-dependent indicators suggest NDP >= 3
  lidar_indicators <- c("indicateur_b2_structure")
  if (any(lidar_indicators %in% tools::file_path_sans_ext(files))) return(3L)

  # BD Foret-dependent indicators suggest NDP >= 2
  bdforet_indicators <- c("indicateur_c1_biomasse", "indicateur_t1_anciennete")
  if (any(bdforet_indicators %in% tools::file_path_sans_ext(files))) return(2L)

  # Basic remote sensing indicators suggest NDP >= 1
  if (length(files) > 0) return(1L)

  0L
}


#' Get NDP level metadata
#'
#' Returns the name and confidence coefficient for a given NDP level.
#'
#' @param ndp_level Integer NDP level (0-4).
#' @return List with \code{name} (character) and \code{confidence} (numeric 0-1).
#' @noRd
get_ndp_level <- function(ndp_level) {
  ndp_level <- as.integer(ndp_level %||% 0L)
  levels <- list(
    `0` = list(name = "Aucune donnee spatiale", confidence = 0.2),
    `1` = list(name = "Teledetection", confidence = 0.4),
    `2` = list(name = "Cartographique", confidence = 0.6),
    `3` = list(name = "LiDAR", confidence = 0.8),
    `4` = list(name = "Terrain", confidence = 1.0)
  )
  levels[[as.character(ndp_level)]] %||% levels[["0"]]
}


#' Compute general Nemeton index
#'
#' Computes the weighted global score from family means, adjusted by NDP
#' confidence coefficient.
#'
#' @param family_means Named numeric vector of family mean scores (0-100).
#' @param ndp Integer NDP level (0-4).
#' @return List with \code{score} (numeric 0-100) and \code{confidence} (numeric 0-1).
#' @noRd
compute_general_index <- function(family_means, ndp = 0L) {
  ndp_info <- get_ndp_level(ndp)
  raw_score <- mean(family_means, na.rm = TRUE)
  list(
    score = round(raw_score, 1),
    confidence = ndp_info$confidence,
    ndp_level = as.integer(ndp)
  )
}


#' Restore NDP attributes after parquet deserialization
#'
#' Parquet round-trip strips R attributes. This function restores the
#' ndp_level attribute on indicator results loaded from disk.
#'
#' @param results data.frame or sf object with indicator columns.
#' @param ndp_level Integer NDP level (0-4).
#' @return The results object with ndp_level attribute restored.
#' @noRd
restore_ndp_attributes <- function(results, ndp_level) {
  attr(results, "ndp_level") <- as.integer(ndp_level)
  results
}

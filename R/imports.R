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

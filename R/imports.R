# Import internal functions from nemeton core package
# These functions are not exported by nemeton but are needed by the Shiny app.
# We use utils::getFromNamespace() instead of the ::: operator so that
# R CMD check does not emit a NOTE about "Objets non exportes importes par
# appels ':::'".

.get <- function(name) utils::getFromNamespace(name, "nemeton")

# Family system
get_famille_col <- .get("get_famille_col")
get_famille_code <- .get("get_famille_code")
FAMILLE_NMT_MAP <- .get("FAMILLE_NMT_MAP")

# i18n (messages CLI)
msg_info <- .get("msg_info")
msg_warn <- .get("msg_warn")
msg_error <- .get("msg_error")
msg_success <- .get("msg_success")
msg <- .get("msg")
get_language <- .get("get_language")

# Normalization (exported - kept here for backward compatibility with callers)
normalize_indicator <- nemeton::normalize_indicator

# Utilities
resolve_raster_layer <- .get("resolve_raster_layer")
resolve_vector_layer <- .get("resolve_vector_layer")
safe_extract <- .get("safe_extract")
as_pure_sf <- .get("as_pure_sf")
enrich_parcels_bdforet <- .get("enrich_parcels_bdforet")
map_essence_to_species <- .get("map_essence_to_species")
get_allometric_coefficients <- .get("get_allometric_coefficients")
clean_indicator_name <- .get("clean_indicator_name")
get_dem_raster <- .get("get_dem_raster")

# NDP system (Niveau de Donnees Probant)
set_ndp_attributes <- .get("set_ndp_attributes")
detect_ndp_from_cache <- .get("detect_ndp_from_cache")
restore_ndp_attributes <- .get("restore_ndp_attributes")

# ndp_badge() and ndp_progress_bar() live in R/ndp.R -- they are
# Shiny-specific widgets, not simple aliases of the core package.

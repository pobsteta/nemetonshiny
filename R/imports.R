# Import internal functions from nemeton core package
# These functions are not exported by nemeton but are needed by the Shiny app.
#
# Historical note: earlier versions used `foo <- nemeton:::foo` which (a) emits
# a NOTE about ':::' on non-exported objects, and (b) more importantly *inlines*
# nemeton's function body into nemetonShiny's namespace at install time --
# meaning R CMD check's codetools then sees every package that those inlined
# functions use internally (e.g. exactextractr) and reports them as undeclared
# imports on nemetonShiny. To avoid both issues we now wrap each internal
# helper in a thin closure that only resolves the symbol *at runtime*:
# codetools sees only the wrapper, not the underlying implementation.

# Build a runtime-resolving wrapper for a function in nemeton's namespace.
.nemeton_fn <- function(name) {
  force(name)
  function(...) {
    fn <- utils::getFromNamespace(name, "nemeton")
    fn(...)
  }
}

# Family system
get_famille_col    <- .nemeton_fn("get_famille_col")
get_famille_code   <- .nemeton_fn("get_famille_code")

# Data object (not a function) -- resolved eagerly is fine, it is just data.
FAMILLE_NMT_MAP <- utils::getFromNamespace("FAMILLE_NMT_MAP", "nemeton")

# i18n (messages CLI)
msg_info     <- .nemeton_fn("msg_info")
msg_warn     <- .nemeton_fn("msg_warn")
msg_error    <- .nemeton_fn("msg_error")
msg_success  <- .nemeton_fn("msg_success")
msg          <- .nemeton_fn("msg")
get_language <- .nemeton_fn("get_language")

# Normalization (exported by nemeton - use :: directly, no wrapper needed)
normalize_indicator <- nemeton::normalize_indicator

# Utilities
resolve_raster_layer        <- .nemeton_fn("resolve_raster_layer")
resolve_vector_layer        <- .nemeton_fn("resolve_vector_layer")
safe_extract                <- .nemeton_fn("safe_extract")
as_pure_sf                  <- .nemeton_fn("as_pure_sf")
enrich_parcels_bdforet      <- .nemeton_fn("enrich_parcels_bdforet")
map_essence_to_species      <- .nemeton_fn("map_essence_to_species")
get_allometric_coefficients <- .nemeton_fn("get_allometric_coefficients")
clean_indicator_name        <- .nemeton_fn("clean_indicator_name")
get_dem_raster              <- .nemeton_fn("get_dem_raster")

# NDP system (Niveau de Donnees Probant)
set_ndp_attributes      <- .nemeton_fn("set_ndp_attributes")
detect_ndp_from_cache   <- .nemeton_fn("detect_ndp_from_cache")
restore_ndp_attributes  <- .nemeton_fn("restore_ndp_attributes")

# ndp_badge() and ndp_progress_bar() live in R/ndp.R -- they are
# Shiny-specific widgets, not simple aliases of the core package.

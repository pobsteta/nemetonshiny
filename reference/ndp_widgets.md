# NDP Shiny Widgets

Shiny-specific NDP widgets (badge, progress bar). These are UI
components that belong in nemetonshiny, not in the nemeton core package.

All other NDP functions (NDP_LEVELS, detect_ndp, compute_general_index,
get_ndp_level, set/restore_ndp_attributes, etc.) live in the nemeton
core package. Exported ones are called via nemeton::, while internal
helpers are imported once in R/imports.R via utils::getFromNamespace().

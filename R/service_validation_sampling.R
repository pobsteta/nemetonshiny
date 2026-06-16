# service_validation_sampling.R — Spec 014 phase B (app-side wrapper).
#
# `generate_validation_plan()` is the single entry point consumed by
# `mod_validation_sampling`. It encapsulates the FORDEAD vs FAST mask
# resolution, calls `nemeton::create_validation_sampling_plan()`,
# enriches the result with app provenance columns and turns the cœur
# error `nemeton_empty_alert_mask` plus a handful of preconditions
# into typed errors the UI maps to friendly messages.
#
# Error classes :
#   - validation_no_project    : no project loaded
#   - validation_no_zone       : project has no monitoring_zone_id
#   - validation_no_zone_geom  : zone polygon could not be derived
#   - validation_no_mask       : no FORDEAD/FAST mask available
#                                (cache absent, read failed, or
#                                FAST compute parameters missing)
#   - validation_empty_mask    : mask is well-formed but contains
#                                no alert cell in the chosen classes
#                                (zone saine — wrapper of the cœur
#                                `nemeton_empty_alert_mask`)


#' Generate a field validation sampling plan
#'
#' @param con DBI connection to the monitoring DB.
#' @param project The loaded project list (`app_state$current_project`).
#'   Must carry `id`, `path` and `metadata$monitoring_zone_id`.
#' @param source One of `"FORDEAD"` / `"FAST"`. Drives which alert
#'   mask the cœur planner consumes.
#' @param n_validation Integer. Number of validation placettes.
#' @param n_control Integer. Number of control placettes.
#' @param classes Integer vector — alert classes to retain. Default
#'   `c(3L, 4L)` matches the G1 guard-rail of spec 008.
#' @param control_classes Integer vector — classes from which control
#'   (témoin) plots are drawn. Default `c(0L)` (healthy pixels). Relaxed
#'   to a higher class by the UI when the alert raster has no class-0
#'   cell (e.g. every pixel dropped below threshold at least once).
#' @param buffer_m Numeric. Tampon around alert cells in meters.
#' @param seed Optional integer. Forwarded to the cœur planner.
#' @param ndvi_threshold,nbr_threshold,mode,window_days,date_from,date_to
#'   FAST mask parameters. Required (non-NULL) when `source == "FAST"`
#'   and no recent mask exists in the project cache.
#'
#' @return An sf POINT (EPSG:2154) enriched with the columns
#'   `plot_id`, `type` (Validation / Temoin), `alert_class`,
#'   `visit_order`, `source`, `classes`, `seed`, plus the app
#'   provenance columns `zone_id`, `source_run_id`, `generated_at`.
#' @noRd
generate_validation_plan <- function(con, project,
                                     source = c("FORDEAD", "FAST", "RECONFORT"),
                                     n_validation = 20L,
                                     n_control    = 5L,
                                     classes      = c(3L, 4L),
                                     control_classes = c(0L),
                                     buffer_m     = 0,
                                     seed         = NULL,
                                     ndvi_threshold = NULL,
                                     nbr_threshold  = NULL,
                                     mode           = "count",
                                     window_days    = 30L,
                                     date_from      = NULL,
                                     date_to        = NULL,
                                     # v0.52.15 — mono-indice (spec 017 cœur).
                                     # NULL → "NDVI" (défaut cœur).
                                     index          = NULL) {
  source <- match.arg(source)
  if (is.null(project) || is.null(project$id)) {
    rlang::abort("No project loaded.", class = "validation_no_project")
  }
  zone_id_raw <- project$metadata$monitoring_zone_id
  if (is.null(zone_id_raw) || !nzchar(as.character(zone_id_raw))) {
    rlang::abort("Project has no monitoring zone.",
                 class = "validation_no_zone")
  }
  zone_id <- suppressWarnings(as.integer(zone_id_raw))
  if (is.na(zone_id)) {
    rlang::abort("monitoring_zone_id is not an integer.",
                 class = "validation_no_zone")
  }

  zone <- .resolve_validation_zone(project, con, zone_id)
  if (is.null(zone)) {
    rlang::abort("Could not derive zone polygon for validation plan.",
                 class = "validation_no_zone_geom")
  }

  alert_raster <- .resolve_alert_raster(
    project, con, zone_id, source,
    ndvi_threshold = ndvi_threshold,
    nbr_threshold  = nbr_threshold,
    mode           = mode,
    window_days    = window_days,
    date_from      = date_from,
    date_to        = date_to,
    index          = index
  )
  if (is.null(alert_raster)) {
    rlang::abort("No alert mask available for this zone.",
                 class = "validation_no_mask")
  }

  plan <- tryCatch(
    nemeton::create_validation_sampling_plan(
      zone         = zone,
      alert_raster = alert_raster,
      n_validation = as.integer(n_validation),
      n_control    = as.integer(n_control),
      classes      = as.integer(classes),
      control_classes = as.integer(control_classes),
      buffer_m     = as.numeric(buffer_m),
      source       = source,
      seed         = seed
    ),
    error = function(e) {
      if (inherits(e, "nemeton_empty_alert_mask")) {
        rlang::abort("No alert cell in the chosen classes.",
                     class = "validation_empty_mask",
                     parent = e)
      }
      stop(e)
    }
  )

  plan$zone_id       <- zone_id
  plan$source_run_id <- .extract_mask_run_id(project, source, zone_id)
  plan$generated_at  <- Sys.time()
  plan
}


#' Generate a FAST trend sanitary sampling plan (spec 025, Option A)
#'
#' App-side wrapper around `nemeton::create_trend_sanitary_plan()` for the
#' « Plan de validation FAST » sub-tab: sanitary plots weighted by the
#' continuous decline severity (|Theil-Sen slope| of the pluriannual NDRE
#' trend) + control plots on stable cells. No categorical classes, no buffer,
#' no TSP — distinct from the count/rolling `generate_validation_plan()` path.
#'
#' @return sf POINT (EPSG:2154) with `plot_id`, `type` (Sanitaire/Temoin),
#'   `alert_value`, `index`, `source` ("FAST_TREND"), `seed`, plus app
#'   columns `zone_id`, `generated_at`. Raises the typed condition
#'   `validation_empty_trend` when no significant decline is found.
#' @noRd
generate_trend_sanitary_plan <- function(con, project,
                                         index = "NDRE",
                                         n_plots = 20L, n_control = 5L,
                                         date_from = NULL, date_to = NULL,
                                         months = 6:9, min_years = 4L,
                                         min_obs_per_year = 2L, alpha = 0.05,
                                         seed = NULL) {
  if (is.null(project) || is.null(project$id)) {
    rlang::abort("No project loaded.", class = "validation_no_project")
  }
  zone_id_raw <- project$metadata$monitoring_zone_id
  if (is.null(zone_id_raw) || !nzchar(as.character(zone_id_raw))) {
    rlang::abort("Project has no monitoring zone.", class = "validation_no_zone")
  }
  zone_id <- suppressWarnings(as.integer(zone_id_raw))
  if (is.na(zone_id)) {
    rlang::abort("monitoring_zone_id is not an integer.",
                 class = "validation_no_zone")
  }
  cd <- file.path(project$path, "cache", "layers", "sentinel2")
  if (!dir.exists(cd)) {
    rlang::abort("Sentinel-2 cache not found for this project.",
                 class = "validation_no_mask")
  }

  plan <- tryCatch(
    nemeton::create_trend_sanitary_plan(
      con              = con,
      zone_id          = zone_id,
      date_from        = date_from,
      date_to          = date_to,
      cache_dir        = cd,
      index            = index,
      n_plots          = as.integer(n_plots),
      n_control        = as.integer(n_control),
      months           = months,
      min_years        = as.integer(min_years),
      min_obs_per_year = as.integer(min_obs_per_year),
      alpha            = as.numeric(alpha),
      seed             = seed
    ),
    error = function(e) {
      # Garde-fou cœur : aucun déclin significatif (ou raster trend vide).
      if (inherits(e, "nemeton_empty_alert_mask")) {
        rlang::abort("No significant decline on the analysis window.",
                     class = "validation_empty_trend", parent = e)
      }
      stop(e)
    }
  )

  plan$zone_id      <- zone_id
  plan$generated_at <- Sys.time()
  plan
}


# Derive the zone polygon (EPSG:2154 sf POLYGON) for the validation
# planner. Tries the on-disk monitoring DB lookup first (mirrors what
# nemeton uses internally) and falls back to dissolving the project
# UGF layer when the DB read fails or returns NULL (zone row deleted
# out-of-band, cache wipe, …).
.resolve_validation_zone <- function(project, con, zone_id) {
  aoi <- tryCatch(get_monitoring_zone_aoi(con, zone_id),
                  error = function(e) NULL)
  if (!is.null(aoi) && inherits(aoi, "sf") && nrow(aoi) > 0L) {
    return(sf::st_transform(aoi, 2154))
  }
  ug <- project$indicators_sf
  if (is.null(ug) || !inherits(ug, "sf") || !nrow(ug)) return(NULL)
  poly <- tryCatch(
    sf::st_union(sf::st_transform(ug, 2154)),
    error = function(e) NULL
  )
  if (is.null(poly)) return(NULL)
  sf::st_sf(zone_id = zone_id,
            geometry = sf::st_sfc(poly, crs = 2154))
}


# Resolve the SpatRaster used to feed create_validation_sampling_plan().
# - FORDEAD : read the dieback mask from <project>/cache/layers/fordead.
# - FAST    : try the FAST alert mask cache first (cache/layers/fast).
#             If absent and the FAST compute parameters are complete,
#             call compute_fast_alert_mask() then re-read. Returns NULL
#             when no usable mask can be produced — caller turns this
#             into a typed error.
.resolve_alert_raster <- function(project, con, zone_id, source,
                                  ndvi_threshold = NULL,
                                  nbr_threshold  = NULL,
                                  mode           = "count",
                                  window_days    = 30L,
                                  date_from      = NULL,
                                  date_to        = NULL,
                                  # v0.52.15 — index mono-indice (spec 017).
                                  # NULL → fallback "NDVI" (défaut cœur).
                                  index          = NULL) {
  if (is.null(project$path) || !nzchar(project$path)) return(NULL)
  if (identical(source, "FORDEAD")) {
    cd <- file.path(project$path, "cache", "layers", "fordead")
    if (!dir.exists(cd)) return(NULL)
    return(tryCatch(
      nemeton::read_fordead_dieback_mask(con, zone_id, cache_dir = cd),
      error = function(e) {
        cli::cli_alert_warning(
          "read_fordead_dieback_mask failed: {e$message}"
        )
        NULL
      }
    ))
  }
  # RECONFORT path (spec 021 G4). Mirror of FORDEAD : the categorical
  # 1-sain / 2-deperissant / 3-tres-deperissant mask is read from the
  # RECONFORT cache. Unlike FAST there is NO on-the-fly compute — the
  # mask must have been persisted by a RECONFORT run. Absent → NULL,
  # surfaced as `validation_no_mask` (same UX as FORDEAD without a run).
  if (identical(source, "RECONFORT")) {
    cd <- file.path(project$path, "cache", "layers", "reconfort")
    if (!dir.exists(cd)) return(NULL)
    return(tryCatch(
      nemeton::read_reconfort_alert_mask(con, zone_id, cache_dir = cd),
      error = function(e) {
        cli::cli_alert_warning(
          "read_reconfort_alert_mask failed: {e$message}"
        )
        NULL
      }
    ))
  }
  # FAST path. v0.69.0 — répertoire renommé `fast/` → `fast_sampling/`
  # pour clarifier le scope (validation d'échantillonnage), à
  # distinguer du cache `fast_alert/` et `fast_alert_mask/` du
  # contexte monitoring.
  cd <- file.path(project$path, "cache", "layers", "fast_sampling")
  if (!dir.exists(cd)) {
    dir.create(cd, recursive = TRUE, showWarnings = FALSE)
  }
  r <- tryCatch(
    nemeton::read_fast_alert_mask(con, zone_id, cache_dir = cd),
    error = function(e) NULL
  )
  if (!is.null(r)) return(r)

  # Need to compute. Validate FAST parameters first.
  if (is.null(ndvi_threshold) || is.null(nbr_threshold) ||
      is.null(date_from) || is.null(date_to)) {
    cli::cli_alert_warning(
      "FAST mask compute requires NDVI / NBR thresholds and date range."
    )
    return(NULL)
  }
  s2_cache <- file.path(project$path, "cache", "layers", "sentinel2")
  if (!dir.exists(s2_cache)) {
    cli::cli_alert_warning(
      "FAST mask compute requires a populated Sentinel-2 cache."
    )
    return(NULL)
  }
  # v0.52.15 — `nemeton@v0.55.0` (spec 017) a unifié l'API mono-index :
  # `compute_fast_alert_mask()` accepte désormais `index` + `threshold`,
  # plus `threshold_ndvi`/`threshold_nbr` (call site oublié en v0.52.13).
  # v0.57.0 ajoute le cache D6 du résultat (`cache_result = TRUE`,
  # `result_cache_dir`) — on l'active sur `cache/layers/fast` pour
  # qu'une revisite à paramètres identiques soit instantanée.
  fast_idx <- toupper(index %||% "NDVI")
  fast_thr <- if (identical(fast_idx, "NBR")) nbr_threshold else ndvi_threshold
  tryCatch(
    nemeton::compute_fast_alert_mask(
      con              = con,
      zone_id          = zone_id,
      index            = fast_idx,
      threshold        = as.numeric(fast_thr),
      date_from        = as.Date(date_from),
      date_to          = as.Date(date_to),
      mode             = as.character(mode),
      window_days      = as.integer(window_days),
      cache_dir        = s2_cache,
      mask_cache_dir   = cd,
      cache_result     = TRUE,
      result_cache_dir = cd
    ),
    error = function(e) {
      cli::cli_alert_warning(
        "compute_fast_alert_mask failed: {e$message}"
      )
    }
  )
  tryCatch(
    nemeton::read_fast_alert_mask(con, zone_id, cache_dir = cd),
    error = function(e) NULL
  )
}


# Best-effort extraction of the mask source filename's timestamp.
# FORDEAD : <project>/cache/layers/fordead/zone_<id>/dieback_mask_<TS>.tif
# FAST    : <project>/cache/layers/fast/zone_<id>/fast_alert_<TS>.tif
# Returns the most recent timestamp suffix as a character or NA when
# the path doesn't exist / no matching file is found.
.extract_mask_run_id <- function(project, source, zone_id) {
  if (is.null(project$path)) return(NA_character_)
  base <- if (identical(source, "FORDEAD")) {
    file.path(project$path, "cache", "layers", "fordead",
              sprintf("zone_%d", zone_id))
  } else if (identical(source, "RECONFORT")) {
    file.path(project$path, "cache", "layers", "reconfort",
              sprintf("zone_%d", zone_id))
  } else {
    file.path(project$path, "cache", "layers", "fast_sampling",
              sprintf("zone_%d", zone_id))
  }
  if (!dir.exists(base)) return(NA_character_)
  pattern <- if (identical(source, "FORDEAD")) {
    "^dieback_mask_(.+)\\.tif$"
  } else if (identical(source, "RECONFORT")) {
    "^reconfort_mask_(.+)\\.tif$"
  } else {
    "^fast_alert_(.+)\\.tif$"
  }
  files <- list.files(base, pattern = pattern, full.names = FALSE)
  if (!length(files)) return(NA_character_)
  ts <- sub(pattern, "\\1", files)
  ts[order(ts, decreasing = TRUE)][1]
}

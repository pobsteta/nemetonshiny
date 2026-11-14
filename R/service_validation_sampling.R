# service_validation_sampling.R - Spec 014 phase B (app-side wrapper).
#
# `generate_validation_plan()` is the single entry point consumed by
# `mod_validation_sampling`. It encapsulates the FORDEAD vs FAST mask
# resolution, calls `nemeton::create_validation_sampling_plan()`,
# enriches the result with app provenance columns and turns the coeur
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
#                                (zone saine - wrapper of the coeur
#                                `nemeton_empty_alert_mask`)


#' Generate a field validation sampling plan
#'
#' @param con DBI connection to the monitoring DB.
#' @param project The loaded project list (`app_state$current_project`).
#'   Must carry `id`, `path` and `metadata$monitoring_zone_id`.
#' @param source One of `"FORDEAD"` / `"FAST"`. Drives which alert
#'   mask the coeur planner consumes.
#' @param n_validation Integer. Number of validation placettes.
#' @param n_control Integer. Number of control placettes.
#' @param classes Integer vector - alert classes to retain. Default
#'   `c(3L, 4L)` matches the G1 guard-rail of spec 008.
#' @param control_classes Integer vector - classes from which control
#'   (temoin) plots are drawn. Default `c(0L)` (healthy pixels). Relaxed
#'   to a higher class by the UI when the alert raster has no class-0
#'   cell (e.g. every pixel dropped below threshold at least once).
#' @param buffer_m Numeric. Tampon around alert cells in meters.
#' @param weighting One of `"uniform"` (default, weight = alert class - the
#'   historical behaviour) or `"continuous"` (weight proportional to a
#'   continuous severity raster - FORDEAD `anomaly_index` / RECONFORT `score`,
#'   parity with the FAST trend plan). Only honoured for FORDEAD / RECONFORT;
#'   degrades cleanly to `"uniform"` when the severity layer is unavailable.
#' @param run_zone_id Optional integer. For `source == "FAST"` only: the zone
#'   the FAST mask is computed and cached on (the `_tot` zone). `zone_id` (the
#'   stratum) then only bounds the draw via the zone polygon. NULL = same as
#'   `zone_id` (historical behaviour). Ignored for FORDEAD (its `_tot` zone is
#'   resolved from the project, mission 019) and RECONFORT (mission 021).
#' @param seed Optional integer. Forwarded to the coeur planner.
#' @param ndvi_threshold,nbr_threshold,mode,window_days,date_from,date_to
#'   FAST mask parameters. Required (non-NULL) when `source == "FAST"`
#'   and no recent mask exists in the project cache.
#'
#' @return An sf POINT (EPSG:2154) enriched with the columns
#'   `plot_id`, `type` (Validation / Temoin), `alert_class`,
#'   `visit_order`, `source`, `classes`, `seed`, plus the app
#'   provenance columns `zone_id`, `source_run_id`, `generated_at`.
#'   In `weighting = "continuous"` mode the core adds an `alert_weight`
#'   column (raw severity at the drawn point).
#' @noRd
generate_validation_plan <- function(con, project,
                                     source = c("FORDEAD", "FAST", "RECONFORT"),
                                     zone_id = NULL,
                                     n_validation = 20L,
                                     n_control    = 5L,
                                     classes      = c(3L, 4L),
                                     control_classes = c(0L),
                                     buffer_m     = 0,
                                     # spec 014 - ponderation du tirage : "uniform"
                                     # (poids = classe, historique) ou "continuous"
                                     # (poids prop raster de severite continu, parite FAST).
                                     weighting    = c("uniform", "continuous"),
                                     seed         = NULL,
                                     run_zone_id  = NULL,
                                     ndvi_threshold = NULL,
                                     nbr_threshold  = NULL,
                                     mode           = "count",
                                     window_days    = 30L,
                                     date_from      = NULL,
                                     date_to        = NULL,
                                     # v0.52.15 - mono-indice (spec 017 coeur).
                                     # NULL -> "NDVI" (defaut coeur).
                                     index          = NULL) {
  source <- match.arg(source)
  weighting <- match.arg(weighting)
  if (is.null(project) || is.null(project$id)) {
    rlang::abort("No project loaded.", class = "validation_no_project")
  }
  # Zone d'echantillonnage = selecteur " Zone de suivi " (`zone_id` passe
  # par le module) ; repli sur la metadata projet si rien n'est selectionne.
  zone_id <- suppressWarnings(as.integer(zone_id))
  if (length(zone_id) != 1L || is.na(zone_id)) {
    zone_id <- suppressWarnings(as.integer(project$metadata$monitoring_zone_id))
  }
  if (length(zone_id) != 1L || is.na(zone_id)) {
    rlang::abort("No monitoring zone selected.",
                 class = "validation_no_zone")
  }

  zone <- .resolve_validation_zone(project, con, zone_id)
  if (is.null(zone)) {
    rlang::abort("Could not derive zone polygon for validation plan.",
                 class = "validation_no_zone_geom")
  }

  # FAST : masque calcule sur `_tot` (mission 017) ; le polygone `zone` ci-dessus
  # reste celui de la strate et borne le tirage (le coeur y restreint le raster).
  mask_zone_id <- .fast_run_zone(source, run_zone_id, zone_id)

  # FORDEAD (mission 019) et RECONFORT (mission 021) : le masque est calcule et
  # mis en cache sur `_tot` ; repli sur la strate si `_tot` n'a pas de masque.
  # Le coeur restreint deja le raster au polygone `zone` avant le tirage :
  # aucune decoupe ici. `read_zone_id` = zone reellement lue (provenance, poids).
  read_zone_id <- NULL
  if (source %in% c("FORDEAD", "RECONFORT")) {
    fd <- .read_categorical_mask_chain(con, project, zone_id, source)
    alert_raster <- fd$raster
    if (!is.null(fd$zone_id)) {
      mask_zone_id <- fd$zone_id
      read_zone_id <- fd$zone_id
    }
  } else {
    alert_raster <- .resolve_alert_raster(
      project, con, mask_zone_id, source,
      ndvi_threshold = ndvi_threshold,
      nbr_threshold  = nbr_threshold,
      mode           = mode,
      window_days    = window_days,
      date_from      = date_from,
      date_to        = date_to,
      index          = index
    )
  }
  if (is.null(alert_raster)) {
    rlang::abort("No alert mask available for this zone.",
                 class = "validation_no_mask")
  }

  # Ponderation continue (spec 014) : resoudre le raster de severite continu
  # (FORDEAD anomaly_index / RECONFORT score). Repli propre decide PAR L'APP
  # (plus permissif que le garde-fou coeur) : couche absente -> tirage uniforme.
  weight_raster <- NULL
  if (identical(weighting, "continuous") &&
      source %in% c("FORDEAD", "RECONFORT")) {
    weight_raster <- .resolve_weight_raster(project, con, zone_id, source,
                                            read_zone_id = read_zone_id)
    if (is.null(weight_raster)) {
      cli::cli_alert_info(
        "No continuous severity layer for {source}; falling back to uniform weighting.")
      weighting <- "uniform"
    }
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
      weighting    = weighting,
      weight_raster = weight_raster,
      seed         = seed
    ),
    error = function(e) {
      if (inherits(e, "nemeton_empty_alert_mask")) {
        rlang::abort("No alert cell in the chosen classes.",
                     class = "validation_empty_mask",
                     parent = e)
      }
      if (inherits(e, "validation_weight_raster_mismatch")) {
        rlang::abort("Severity raster not georeferenced / not alignable.",
                     class = "validation_weight_mismatch",
                     parent = e)
      }
      stop(e)
    }
  )

  plan$zone_id       <- zone_id
  plan$source_run_id <- .extract_mask_run_id(project, source, mask_zone_id)
  plan$generated_at  <- Sys.time()
  plan
}


# Zone sur laquelle le masque FAST est calcule/lu : `run_zone_id` (`_tot`) pour
# FAST quand il est valide, sinon la zone demandee. FORDEAD et RECONFORT ne passent
# plus par ici (`.read_categorical_mask_chain()` resout `_tot`).
.fast_run_zone <- function(source, run_zone_id, zone_id) {
  if (!identical(source, "FAST")) return(zone_id)
  run <- suppressWarnings(as.integer(run_zone_id))
  if (length(run) != 1L || is.na(run)) zone_id else run
}


# Lit le masque categoriel d'un moteur (FORDEAD 0-4, RECONFORT 1-3) sur `_tot`
# (la ou il est calcule et mis en cache), avec repli sur la strate : (a) aucune
# zone `_tot` en base -> `.monitoring_tot_zone_id()` rend deja la strate ; (b)
# `_tot` sans masque sur disque (ancien run fait sur la strate) -> relecture sur
# la strate. Rend list(raster, zone_id) ; `zone_id` = zone reellement lue
# (provenance, poids continu), NULL si rien n'a pu etre lu.
.read_categorical_mask_chain <- function(con, project, zone_id,
                                         source = c("FORDEAD", "RECONFORT")) {
  source <- match.arg(source)
  none <- list(raster = NULL, zone_id = NULL)
  if (is.null(project$path) || !nzchar(project$path)) return(none)
  sub <- if (identical(source, "FORDEAD")) "fordead" else "reconfort"
  cd <- file.path(project$path, "cache", "layers", sub)
  if (!dir.exists(cd)) return(none)
  strate <- suppressWarnings(as.integer(zone_id))
  ids <- unique(c(.monitoring_tot_zone_id(con, project, strate), strate))
  for (id in ids[!is.na(ids)]) {
    r <- tryCatch(
      if (identical(source, "FORDEAD")) {
        nemeton::read_fordead_dieback_mask(con, id, cache_dir = cd)
      } else {
        nemeton::read_reconfort_alert_mask(con, id, cache_dir = cd)
      },
      error = function(e) {
        cli::cli_alert_warning(
          "{source} mask read failed: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.null(r)) return(list(raster = r, zone_id = id))
  }
  none
}


# Masque categoriel lu sur `_tot` puis decoupe a la strate choisie
# (presentation pure, parite FAST). Sert l'histogramme, l'auto-relachement des
# temoins et la surcouche des Plans de validation FORDEAD / RECONFORT. Repli
# double : voir `.read_categorical_mask_chain()` ; si la zone lue est la strate,
# pas de decoupe.
.read_categorical_mask_stratum <- function(con, project, zone_id, source) {
  fd <- .read_categorical_mask_chain(con, project, zone_id, source)
  if (is.null(fd$raster)) return(NULL)
  .clip_fast_to_stratum(fd$raster,
                        .fast_stratum_aoi(con, zone_id, fd$zone_id))
}

.read_fordead_mask_stratum <- function(con, project, zone_id) {
  .read_categorical_mask_stratum(con, project, zone_id, "FORDEAD")
}

.read_reconfort_mask_stratum <- function(con, project, zone_id) {
  .read_categorical_mask_stratum(con, project, zone_id, "RECONFORT")
}


# Lit le masque FAST 0-4 sur la zone du calcul (`_tot`) puis le decoupe a la
# strate choisie (presentation pure : les classes sont celles de `_tot`, parite
# FORDEAD). Sert l'histogramme et la surcouche du Plan de validation FAST.
# NULL si le cache est absent ; si la strate = zone du calcul, pas de decoupe.
.read_fast_mask_stratum <- function(con, zone_id, run_zone_id, cache_dir) {
  run <- .fast_run_zone("FAST", run_zone_id, zone_id)
  r <- tryCatch(
    nemeton::read_fast_alert_mask(con, run, cache_dir = cache_dir),
    error = function(e) NULL
  )
  if (is.null(r)) return(NULL)
  .clip_fast_to_stratum(r, .fast_stratum_aoi(con, zone_id, run))
}


#' Generate a FAST trend sanitary sampling plan (spec 025, Option A)
#'
#' App-side wrapper around `nemeton::create_trend_sanitary_plan()` for the
#' " Plan de validation FAST " sub-tab: sanitary plots weighted by the
#' continuous decline severity (|Theil-Sen slope| of the pluriannual NDRE
#' trend) + control plots on stable cells. No categorical classes, no buffer,
#' no TSP - distinct from the count/rolling `generate_validation_plan()` path.
#'
#' @return sf POINT (EPSG:2154) with `plot_id`, `type` (Sanitaire/Temoin),
#'   `alert_value`, `index`, `source` ("FAST_TREND"), `seed`, plus app
#'   columns `zone_id`, `generated_at`. Raises the typed condition
#'   `validation_empty_trend` when no significant decline is found.
#' @noRd
generate_trend_sanitary_plan <- function(con, project,
                                         zone_id = NULL,
                                         index = "NDRE",
                                         n_plots = 20L, n_control = 5L,
                                         date_from = NULL, date_to = NULL,
                                         months = 6:9, min_years = 4L,
                                         min_obs_per_year = 2L, alpha = 0.05,
                                         seed = NULL,
                                         run_zone_id = NULL) {
  if (is.null(project) || is.null(project$id)) {
    rlang::abort("No project loaded.", class = "validation_no_project")
  }
  # La zone d'echantillonnage vient du selecteur " Zone de suivi "
  # (`zone_id` passe par le module). Repli sur `metadata$monitoring_zone_id`
  # uniquement si aucune zone n'est selectionnee.
  zone_id <- suppressWarnings(as.integer(zone_id))
  if (length(zone_id) != 1L || is.na(zone_id)) {
    zone_id <- suppressWarnings(as.integer(project$metadata$monitoring_zone_id))
  }
  if (length(zone_id) != 1L || is.na(zone_id)) {
    rlang::abort("No monitoring zone selected.", class = "validation_no_zone")
  }
  cd <- file.path(project$path, "cache", "layers", "sentinel2")
  if (!dir.exists(cd)) {
    rlang::abort("Sentinel-2 cache not found for this project.",
                 class = "validation_no_mask")
  }

  # Mission 017 : la pente est calculee pixel par pixel (sans quartiles), donc
  # sur `_tot` les valeurs dans la strate ne changent pas ; la strate ne fait
  # que BORNER le tirage via `mask_polygon`. `plan$zone_id` reste la strate.
  calc_zone_id <- .fast_run_zone("FAST", run_zone_id, zone_id)
  mask_polygon <- NULL
  if (!identical(calc_zone_id, zone_id)) {
    mask_polygon <- tryCatch(get_monitoring_zone_aoi(con, zone_id),
                             error = function(e) NULL)
    if (is.null(mask_polygon)) {
      rlang::abort("Could not derive zone polygon for validation plan.",
                   class = "validation_no_zone_geom")
    }
  }

  plan <- tryCatch(
    nemeton::create_trend_sanitary_plan(
      con              = con,
      zone_id          = calc_zone_id,
      mask_polygon     = mask_polygon,
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
      # Garde-fou coeur : aucun declin significatif (ou raster trend vide).
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
# out-of-band, cache wipe, ...).
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
# - FORDEAD, RECONFORT : not handled here, see `.read_categorical_mask_chain()`
#             (read on `_tot`, fallback on the stratum).
# - FAST    : try the FAST alert mask cache first (cache/layers/fast).
#             If absent and the FAST compute parameters are complete,
#             call compute_fast_alert_mask() then re-read. Returns NULL
#             when no usable mask can be produced - caller turns this
#             into a typed error.
.resolve_alert_raster <- function(project, con, zone_id, source,
                                  ndvi_threshold = NULL,
                                  nbr_threshold  = NULL,
                                  mode           = "count",
                                  window_days    = 30L,
                                  date_from      = NULL,
                                  date_to        = NULL,
                                  # v0.52.15 - index mono-indice (spec 017).
                                  # NULL -> fallback "NDVI" (defaut coeur).
                                  index          = NULL) {
  if (is.null(project$path) || !nzchar(project$path)) return(NULL)
  # FAST path. v0.69.0 - repertoire renomme `fast/` -> `fast_sampling/`
  # pour clarifier le scope (validation d'echantillonnage), a
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
  # v0.52.15 - `nemeton@v0.55.0` (spec 017) a unifie l'API mono-index :
  # `compute_fast_alert_mask()` accepte desormais `index` + `threshold`,
  # plus `threshold_ndvi`/`threshold_nbr` (call site oublie en v0.52.13).
  # v0.57.0 ajoute le cache D6 du resultat (`cache_result = TRUE`,
  # `result_cache_dir`) - on l'active sur `cache/layers/fast` pour
  # qu'une revisite a parametres identiques soit instantanee.
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


# Resolve the `_tot` zone id (union of UGF strata, spec 020) : FORDEAD and
# RECONFORT compute and cache their layers under
# `cache/layers/<moteur>/zone_<id_tot>/`, like their map viewers. Falls back
# to the passed zone_id (back-compat pre-spec-020). Mirror of the module-local
# `.fordead_tot_id` in `mod_monitoring_fordead_map.R`.
.monitoring_tot_zone_id <- function(con, project, fallback_zone) {
  id_tot <- tryCatch({
    zdf <- nemeton::find_zones_by_project(con, project_uuid = project$id)
    idx <- grep("_tot$", as.character(zdf$name))
    if (length(idx)) as.integer(zdf$id[idx[1]]) else NA_integer_
  }, error = function(e) NA_integer_)
  if (!is.na(id_tot)) id_tot else suppressWarnings(as.integer(fallback_zone))
}


# Resolve the CONTINUOUS severity raster used to weight the validation draw
# (`weighting = "continuous"`), the analogue of the FAST |slope| trend raster.
# - FORDEAD : `anomaly_index` pixel layer, read on the `_tot` stratum (the core
#   re-aligns it onto the categorical dieback mask grid).
# - RECONFORT : the continuous `score` raster, resolved from the on-disk cache
#   manifest of `read_zone_id` (the zone the categorical mask was actually read
#   on, `_tot` or the stratum on fallback ; default = `zone_id`), cut to the
#   stratum polygon by the core ; manifest (parity with the reloaded-project path of the RECONFORT map viewer
#   - no in-memory run result required).
# Returns NULL when the severity layer is unavailable (older run predating the
# layer) - the caller then degrades cleanly to uniform weighting.
.resolve_weight_raster <- function(project, con, zone_id, source,
                                    read_zone_id = NULL) {
  if (is.null(project$path) || !nzchar(project$path)) return(NULL)

  if (identical(source, "FORDEAD")) {
    cd <- file.path(project$path, "cache", "layers", "fordead")
    if (!dir.exists(cd)) return(NULL)
    id_tot <- .monitoring_tot_zone_id(con, project, zone_id)
    return(tryCatch(
      nemeton::read_fordead_layer(con, zone_id = id_tot,
                                  layer = "anomaly_index", cache_dir = cd),
      error = function(e) {
        cli::cli_alert_warning("read_fordead_layer (anomaly_index) failed: {e$message}")
        NULL
      }
    ))
  }

  if (identical(source, "RECONFORT")) {
    cd <- file.path(project$path, "cache", "layers", "reconfort")
    if (!dir.exists(cd)) return(NULL)
    manifest_zone <- if (is.null(read_zone_id)) zone_id else read_zone_id
    man <- tryCatch(
      nemeton::reconfort_cache_manifest(cd, zone_id = as.integer(manifest_zone),
                                        include_range = TRUE),
      error = function(e) {
        cli::cli_alert_warning("reconfort_cache_manifest failed: {e$message}")
        NULL
      }
    )
    if (is.null(man) || !nrow(man)) return(NULL)
    # Couche continue = `score` (le categoriel 1/2/3 = `classification` sert
    # d'alert_raster). Ne pas confondre.
    row <- man[man$id == "score" & man$type == "raster", , drop = FALSE]
    if (!nrow(row)) return(NULL)
    aoi <- .resolve_validation_zone(project, con, zone_id)
    return(tryCatch(
      nemeton::read_reconfort_layer(layer = row, mask_polygon = aoi,
                                    apply_zone_mask = TRUE),
      error = function(e) {
        cli::cli_alert_warning("read_reconfort_layer (score) failed: {e$message}")
        NULL
      }
    ))
  }

  NULL   # FAST : chemin trend separe (generate_trend_sanitary_plan).
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

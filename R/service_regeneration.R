# reGénération — orchestration service (spec 027, L4)
#
# Non-Shiny application layer for the climate-vulnerability read. All business
# logic (microclimate exposure, soil water balance, priority index, and the
# inversion of any indicator) lives in the core `nemeton` package (>= 0.118.0);
# this service only *orchestrates* the exported functions and *forwards* their
# outputs. Every engine has a `precomputed` path, so the app runs today by
# passing pre-computed engine outputs (no microclimf / biljouR / LiDAR run
# needed here). When neither a precomputed input nor the engine is available,
# the core raises a clean, actionable error that we surface as-is.
#
# See CLAUDE.md rule #1/#2: no business logic in the app; modules go through
# services; services call nemeton::*.

#' Column contract of a reGénération run (spec 027 §7)
#'
#' The full set of per-UGF columns produced by a run, persisted (PostGIS) and
#' exported (GeoPackage). A given run may leave some as NA when the
#' corresponding engine/precomputed input is missing.
#' @noRd
REGEN_OUTPUT_COLUMNS <- c(
  # Microclimatic exposure (regen_sensibilite)
  "tmax_moyenne", "tmax_canicule", "vpd_moyenne", "vpd_canicule",
  "d_tmax", "d_vpd", "sensibilite", "rang_sensibilite",
  "robustesse", "signal_robuste", "couverture_pct",
  # Soil water balance (regen_bilan_hydrique)
  "rew_min", "njstress", "istress", "deb_stress",
  # Priority index (indice_priorite_regen)
  "indice_priorite_regen", "regen_exposition", "regen_hydrique",
  "parcelle_sensible", "priorite", "regen_essence",
  # Normalised sub-indicators feeding the A/W/R radar
  "indicateur_a3_microclimat", "indicateur_a4_tamponnement",
  "indicateur_w4_vpd", "indicateur_r6_sensibilite",
  "indicateur_r3_secheresse"
)

#' Run one core step, degrading gracefully on failure
#'
#' Calls `fn()` (a nemeton engine/indicator). On success returns its `units`
#' result; on error, records an actionable message in `env$warnings` and
#' returns `units` unchanged (the missing columns stay absent → NA downstream,
#' no crash). Engine-missing errors from the core are already actionable.
#' @noRd
# Core engine messages are formatted with cli and carry ANSI colour escapes;
# strip them so they render cleanly in the HTML status panel.
.strip_ansi <- function(x) gsub("\033\\[[0-9;]*m", "", x)

.regen_step <- function(fn, units, env, label) {
  out <- tryCatch(fn(), error = function(e) {
    env$warnings <- c(env$warnings, .strip_ansi(sprintf("%s: %s", label, conditionMessage(e))))
    NULL
  })
  if (is.null(out) || !inherits(out, "sf")) units else out
}

#' Orchestrate a full reGénération analysis for a set of UGF
#'
#' @description
#' Sequences the core calls (spec 027 §1-2): year detection → microclimatic
#' sensitivity → soil water balance → microclimate sub-indicators (A3/A4/W4/R6)
#' → BILJOU-enriched R3 → priority index. Each engine step is fed from
#' `precomputed` outputs when provided and degrades gracefully otherwise. No
#' re-inversion is done here (R3/R6 keep their sense; the core owns inversion).
#'
#' @param units An sf of the project UGF (EPSG:2154 expected).
#' @param cfg List of run parameters (year_moyenne, year_canicule, year_window,
#'   forest_type, lai_max, species, hydric_only, ...). All optional.
#' @param precomputed Optional list of engine outputs, any subset of:
#'   `sensibilite`, `biljou`, `micro`, `micro_moyenne`, `micro_canicule`,
#'   `dem`, `eobs`.
#' @param progress Optional callback `function(list(message=, frac=))` for the
#'   background-task progress bar.
#'
#' @return A list with `units` (enriched sf), `years` (detected/overridden
#'   pair), and `warnings` (character vector of actionable engine messages).
#' @noRd
run_regeneration <- function(units, cfg = list(), precomputed = NULL,
                             progress = NULL) {
  if (!inherits(units, "sf")) {
    stop("run_regeneration: `units` must be an sf object", call. = FALSE)
  }
  pc  <- precomputed %||% list()
  env <- new.env(parent = emptyenv())
  env$warnings <- character(0)
  report <- function(message, frac) {
    if (is.function(progress)) progress(list(message = message, frac = frac))
  }
  hydric_only <- isTRUE(cfg$hydric_only)
  i18n <- get_i18n(get_app_options()$language %||% "fr")

  # 1. Reference years (average / heatwave) — user override or E-OBS detection.
  report("regen_detect_years", 0.05)
  years <- if (!is.null(cfg$year_moyenne) && !is.null(cfg$year_canicule)) {
    list(year_moyenne = cfg$year_moyenne, year_canicule = cfg$year_canicule)
  } else {
    tryCatch(
      nemeton::microclimate_detect_years(
        eobs = pc$eobs, aoi = units, year_window = cfg$year_window %||% 10),
      error = function(e) {
        env$warnings <- c(env$warnings, .strip_ansi(sprintf("detect_years: %s", conditionMessage(e))))
        list(year_moyenne = cfg$year_moyenne, year_canicule = cfg$year_canicule)
      }
    )
  }

  # 2. Microclimatic sensitivity (microclimf) — skipped in water-balance-only.
  #    Option A (garde) : n'appeler le moteur que si une sortie microclimf est
  #    fournie via le cache (`precomputed`). Sinon message i18n propre plutôt
  #    que l'erreur cœur brute. Le run réel du moteur (LiDAR) = option B opt-in.
  if (!hydric_only) {
    report("regen_sensibilite", 0.20)
    if (!is.null(pc$sensibilite)) {
      units <- .regen_step(function() nemeton::regen_sensibilite(
        units, precomputed = pc$sensibilite,
        annees_moy = years$year_moyenne, annees_canic = years$year_canicule),
        units, env, "regen_sensibilite")
    } else {
      env$warnings <- c(env$warnings, i18n$t("regen_guard_sensibilite"))
    }
  }

  # 3. Soil water balance (biljouR, SAFRAN forcing). Même garde : sans sortie
  #    BILJOU précalculée, on saute proprement (option B = entrées météo/sol).
  report("regen_bilan_hydrique", 0.40)
  if (!is.null(pc$biljou)) {
    units <- .regen_step(function() nemeton::regen_bilan_hydrique(
      units, precomputed = pc$biljou, lai_max = cfg$lai_max,
      forest_type = cfg$forest_type %||% "feuillu"),
      units, env, "regen_bilan_hydrique")
  } else {
    env$warnings <- c(env$warnings, i18n$t("regen_guard_hydrique"))
  }

  # 4. Microclimate sub-indicators feeding the A/W/R radar (need micro rasters).
  if (!hydric_only && !is.null(pc$micro)) {
    report("regen_indicateurs_micro", 0.60)
    units <- .regen_step(function() nemeton::indicateur_a3_microclimat(units, micro = pc$micro),
                         units, env, "indicateur_a3_microclimat")
    units <- .regen_step(function() nemeton::indicateur_a4_tamponnement(units, micro = pc$micro),
                         units, env, "indicateur_a4_tamponnement")
    units <- .regen_step(function() nemeton::indicateur_w4_vpd(units, micro = pc$micro),
                         units, env, "indicateur_w4_vpd")
    units <- .regen_step(function() nemeton::indicateur_r6_sensibilite(
      units, micro_moyenne = pc$micro_moyenne, micro_canicule = pc$micro_canicule),
      units, env, "indicateur_r6_sensibilite")
  }

  # 5. Drought R3 enriched by the BILJOU water balance (or njstress on units).
  report("regen_r3_secheresse", 0.75)
  units <- .regen_step(function() nemeton::indicateur_r3_secheresse(
    units, dem = pc$dem, biljou = pc$biljou),
    units, env, "indicateur_r3_secheresse")

  # 6. Priority index (tab head) — generic by default; species = optional.
  report("regen_indice_priorite", 0.90)
  units <- .regen_step(function() nemeton::indice_priorite_regen(
    units, species = cfg$species),
    units, env, "indice_priorite_regen")

  report("regen_done", 1)
  list(units = units, years = years, warnings = env$warnings)
}

#' Regional summer-trend context (E-OBS) for the UGF footprint (branche A)
#'
#' Thin wrapper over `nemeton::tendances_estivales_eobs()` for the bivariate
#' context map (spec 027 §2.6). Returns an sf of points with trend + bivariate
#' class columns, or NULL on failure (map simply not shown).
#' @noRd
regeneration_context_eobs <- function(ugf, precomputed = NULL, buffer_m = 25000) {
  if (!inherits(ugf, "sf")) return(NULL)
  pc <- precomputed %||% list()
  tryCatch(
    nemeton::tendances_estivales_eobs(
      aoi = ugf, tx = pc$eobs_tx, rr = pc$eobs_rr,
      buffer_m = buffer_m, precomputed = pc$context),
    error = function(e) NULL
  )
}

#' Target-species option list for the reGénération selector
#'
#' @description
#' Delegates to `nemeton::regen_species_choices()` (the core owns the TFV →
#' species-class mapping and the FRM/atlas tables). When `units` carry a BD Forêt
#' v2 TFV column (auto-detected among common names) the species actually present
#' on the parcels are flagged (`present = TRUE`, `groupe = "present"`) and listed
#' first. Falls back to the raw tolerance table if the core function is
#' unavailable, and to `NULL` on total failure.
#'
#' @param units Optional `sf`/data.frame of UGF for presence flagging.
#' @param lang UI language ("fr"/"en").
#' @return A data.frame with at least `code`, `label`, `present`, `groupe`, or
#'   `NULL`.
#' @noRd
regeneration_species_choices <- function(units = NULL, lang = "fr") {
  # Auto-détection d'une colonne TFV BD Forêt v2 (présence), sinon NULL.
  tfv_col <- NULL
  if (inherits(units, c("sf", "data.frame"))) {
    cand <- c("tfv_code", "code_tfv", "CODE_TFV", "tfv", "TFV", "CODE_TFV11", "codetfv")
    hit <- intersect(cand, names(units))
    if (length(hit)) tfv_col <- hit[[1]]
  }
  out <- tryCatch(
    nemeton::regen_species_choices(units = units, tfv_col = tfv_col,
                                   level = "species", lang = lang),
    error = function(e) NULL)
  if (is.data.frame(out) && nrow(out) > 0L) return(out)
  # Repli : table de tolérances brute (comportement historique).
  tryCatch(nemeton::regeneration_tolerances(), error = function(e) NULL)
}

#' Load precomputed engine outputs from a project cache
#'
#' @description
#' Looks under \code{<project>/cache/regeneration/} for engine outputs produced
#' offline (microclimf sensitivity, biljou water balance, PAI raster, summer
#' micro rasters, E-OBS). Returns a named list with whatever is present — an
#' empty list when nothing is cached, in which case \code{run_regeneration()}
#' degrades gracefully (engine errors surfaced as actionable warnings). This is
#' the app's forward hook: drop real engine outputs here and the run picks them
#' up without code change.
#'
#' @param project_path Character. Project root, or NULL.
#' @return A named list (possibly empty) of precomputed inputs.
#' @noRd
load_regeneration_precomputed <- function(project_path) {
  if (is.null(project_path)) return(list())
  dir <- file.path(project_path, "cache", "regeneration")
  if (!dir.exists(dir)) return(list())

  read_vec <- function(name) {
    gpkg <- file.path(dir, paste0(name, ".gpkg"))
    if (file.exists(gpkg)) {
      return(tryCatch(sf::st_read(gpkg, quiet = TRUE), error = function(e) NULL))
    }
    NULL
  }
  read_ras <- function(name) {
    tif <- file.path(dir, paste0(name, ".tif"))
    if (file.exists(tif)) {
      return(tryCatch(terra::rast(tif), error = function(e) NULL))
    }
    NULL
  }

  pc <- list(
    sensibilite    = read_vec("sensibilite"),
    biljou         = read_vec("biljou"),
    micro          = read_ras("micro"),
    micro_moyenne  = read_ras("micro_moyenne"),
    micro_canicule = read_ras("micro_canicule"),
    dem            = read_ras("dem"),
    eobs_tx        = read_ras("eobs_tx"),
    eobs_rr        = read_ras("eobs_rr")
  )
  # Drop absent entries so the run's `%||% list()` / is.null checks are clean.
  pc[!vapply(pc, is.null, logical(1))]
}

# ============================================================================
# Moteur microclimf réel (option B, spec 027 L1) — opt-in, coûteux, async.
#
# Le chemin *moteur* de `nemeton::regen_sensibilite` (portage du prototype
# validé sur données réelles) construit la grille LiDAR-HD, télécharge
# ERA5-Land (mcera5/ecmwfr → identifiants CDS) et lance microclimf par année.
# On le câble en réutilisant la grille LiDAR HD déjà produite par le pipeline
# d'indices (`<project>/cache/layers/lidar_mnt` + `lidar_mnh`), en passant des
# RÉPERTOIRES de tuiles (chemins sérialisables pour un worker `future`) et en
# ne renvoyant qu'un `sf`. La sortie est écrite comme entrée `precomputed`
# (`sensibilite.gpkg`) pour que le run normal la consomme en fast-path.
#
# BILJOU reste sur la garde option A tant que l'acquisition météo/sol n'est pas
# exposée par le cœur (brief cœur dédié).
# ============================================================================

#' Canopy-data provenance of a reGénération result (spec 033 D5)
#'
#' @description
#' Reads (never recomputes — rule #1) the canopy structure source from the core
#' \code{nemeton::detect_ndp()} augmentation flags:
#' \itemize{
#'   \item \code{"satellite"} when the ML LAI fallback is active — the
#'     augmentation set contains \code{"lai_ml"} (Sentinel-2 / PROSAIL, NDP 0).
#'   \item \code{"lidar"} when a LiDAR-HD canopy / microclimate flag is present
#'     (\code{"height_lidar"} / \code{"microclimate_model"}).
#'   \item \code{NA} when no canopy was used (no badge shown).
#' }
#' The core owns the source decision and sets the flags; the app only displays.
#'
#' @param units A reGénération result \code{sf} (carries the NDP flags/attrs).
#' @return \code{"lidar"}, \code{"satellite"}, or \code{NA_character_}.
#' @noRd
regen_canopy_provenance <- function(units) {
  if (!inherits(units, "sf") || nrow(units) == 0L) return(NA_character_)
  aug <- tryCatch(nemeton::detect_ndp(units)$augmented, error = function(e) NULL)
  if (is.null(aug)) return(NA_character_)
  aug <- as.character(aug)
  if (any(grepl("lai_ml", aug, fixed = TRUE))) return("satellite")
  if (any(grepl("height_lidar|microclimate_model", aug))) return("lidar")
  NA_character_
}

#' Locate the project's LiDAR-HD DTM/CHM tile grid (spec 027 L1)
#'
#' The index-computation pipeline caches per-tile DTM (`lidar_mnt`) and canopy
#' height (`lidar_mnh`) GeoTIFFs under `<project>/cache/layers/` whenever a run
#' resolves the CHM from LiDAR HD (source `lidar_hd`/`lasr`). The microclimf
#' engine consumes those two directories directly (VRT-mosaicked by the core).
#'
#' @return `list(mnt_dir=, mnh_dir=)` when both directories hold at least one
#'   `.tif`, otherwise `NULL`.
#' @noRd
resolve_regen_lidar_grid <- function(project_path) {
  if (is.null(project_path)) return(NULL)
  base <- file.path(project_path, "cache", "layers")
  mnt_dir <- file.path(base, "lidar_mnt")
  mnh_dir <- file.path(base, "lidar_mnh")
  has_tif <- function(d) dir.exists(d) &&
    length(list.files(d, pattern = "\\.tif$", ignore.case = TRUE)) > 0L
  if (has_tif(mnt_dir) && has_tif(mnh_dir)) {
    return(list(mnt_dir = mnt_dir, mnh_dir = mnh_dir))
  }
  NULL
}

#' Whether CDS/ERA5 credentials are configured for the microclimf engine
#'
#' The engine path downloads ERA5-Land through `mcera5`/`ecmwfr`, which needs a
#' Copernicus CDS key. Credentials are considered ready when `ecmwfr` is
#' installed and a key is resolvable — from the environment (`CDSAPI_KEY` /
#' `ECMWFR_CDS_KEY`) or from the user's `ecmwfr` keyring. No secret is stored in
#' the repo (rule #8): the deployment injects the key and calls
#' `ecmwfr::wf_set_key()` at startup.
#' @noRd
regen_cds_credentials_ready <- function() {
  if (!requireNamespace("ecmwfr", quietly = TRUE)) return(FALSE)
  env_key <- Sys.getenv("CDSAPI_KEY", unset = Sys.getenv("ECMWFR_CDS_KEY", unset = ""))
  if (nzchar(env_key)) return(TRUE)
  # ecmwfr stocke la clé en variable d'environnement `ecmwfr_<user>` (ex.
  # `ecmwfr_ecmwfr` posé dans ~/.Renviron) : détecter ce motif directement,
  # `wf_get_key()` ne le remontant pas toujours sans argument `user`.
  env_all <- Sys.getenv()
  if (any(grepl("^ecmwfr_", names(env_all)) & nzchar(env_all))) return(TRUE)
  key <- tryCatch(ecmwfr::wf_get_key(), error = function(e) "")
  isTRUE(nzchar(key %||% ""))
}

#' Prerequisites for the real microclimf engine run (spec 027 L1)
#'
#' @param project_path Project directory (holds the LiDAR HD tile cache).
#' @return `list(ok=logical, reason=<i18n key or NULL>, grid=<list or NULL>)`.
#'   The first failing check sets `reason`; UI surfaces it as a clean message.
#' @noRd
regen_engine_prereqs <- function(project_path, forcing = "safran") {
  exports <- tryCatch(getNamespaceExports("nemeton"), error = function(e) character(0))
  needed <- c("regen_sensibilite", "regen_bilan_hydrique",
              "load_biljou_forcing", "build_biljou_soil")
  if (!all(needed %in% exports)) {
    return(list(ok = FALSE, reason = "regen_engine_prereq_core",
                grid = NULL, microclimf = FALSE, biljou = FALSE))
  }
  grid <- resolve_regen_lidar_grid(project_path)
  cds  <- regen_cds_credentials_ready()
  # microclimf : toujours LiDAR HD + ERA5 (clé CDS). BILJOU : SAFRAN sans clé,
  # ERA5 avec clé (même que microclimf). Le moteur est lançable dès qu'AU MOINS
  # un des deux est prêt (SAFRAN débloque le bilan hydrique sans Copernicus).
  micro_ok  <- !is.null(grid) && cds
  biljou_ok <- !identical(forcing, "era5") || cds
  ok <- micro_ok || biljou_ok
  reason <- if (ok) NULL
            else if (identical(forcing, "era5")) "regen_engine_prereq_cds"
            else "regen_engine_prereq_lidar"
  list(ok = ok, reason = reason, grid = grid,
       microclimf = micro_ok, biljou = biljou_ok)
}

#' Run the real microclimf engine for a project and persist its output
#'
#' @description
#' Heavy, opt-in path (LiDAR HD + ERA5 + microclimf, minutes→hours). Designed to
#' run inside a `future` worker: every input is serialisable (an `sf`, directory
#' paths, a config list); no in-memory raster crosses the worker boundary. On
#' success the per-UGF sensitivity `sf` is written to the project's regeneration
#' cache (`sensibilite.gpkg`) so the normal `run_regeneration()` fast-path picks
#' it up on the next analysis.
#'
#' @param units An `sf` of the UGF (EPSG:2154 expected).
#' @param project_path Project directory (LiDAR grid + cache location).
#' @param cfg Run parameters; `year_moyenne` / `year_canicule` drive the
#'   average-vs-heatwave summers.
#' @return The enriched `sf` (regen_sensibilite output). Stops on engine error.
#' @noRd
run_regeneration_engine <- function(units, project_path, cfg = list()) {
  if (!inherits(units, "sf")) {
    stop("run_regeneration_engine: `units` must be an sf object", call. = FALSE)
  }
  i18n <- get_i18n(get_app_options()$language %||% "fr")
  out_dir <- file.path(project_path, "cache", "regeneration")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  warnings <- character(0)
  cached   <- character(0)
  canopy   <- NA_character_       # provenance canopée effective (spec 033 D5)
  res <- units
  years <- c(cfg$year_moyenne, cfg$year_canicule)  # c() drops NULLs
  years <- if (length(years)) years else NULL

  # --- microclimf (LiDAR HD + ERA5) : grille LiDAR HD + clé CDS requises. ---
  grid <- resolve_regen_lidar_grid(project_path)
  if (!is.null(grid)) canopy <- "lidar"   # structure LiDAR HD disponible
  if (!is.null(grid) && regen_cds_credentials_ready()) {
    micro_cache <- file.path(out_dir, "microclimf")
    if (!dir.exists(micro_cache)) dir.create(micro_cache, recursive = TRUE)
    sens <- tryCatch(
      nemeton::regen_sensibilite(res, mnt = grid$mnt_dir, mnh = grid$mnh_dir,
        annees_moy = cfg$year_moyenne, annees_canic = cfg$year_canicule,
        cache_dir = micro_cache),
      error = function(e) {
        warnings <<- c(warnings, .strip_ansi(sprintf("microclimf: %s", conditionMessage(e))))
        NULL
      })
    if (inherits(sens, "sf")) {
      res <- sens
      tryCatch(sf::st_write(res, file.path(out_dir, "sensibilite.gpkg"),
                            quiet = TRUE, delete_dsn = TRUE),
               error = function(e) cli::cli_warn("regen engine: sensibilite cache not written"))
      cached <- c(cached, "sensibilite")
    }
  }

  # --- BILJOU (SAFRAN par défaut, sans clé ; ERA5 => clé CDS). ---
  forcing <- cfg$forcing %||% "safran"
  if (identical(forcing, "era5") && !regen_cds_credentials_ready()) {
    warnings <- c(warnings, i18n$t("regen_engine_prereq_cds"))
  } else {
    biljou_cache <- file.path(out_dir, "biljou")
    if (!dir.exists(biljou_cache)) dir.create(biljou_cache, recursive = TRUE)

    # lai_max : valeur UI si fournie ; sinon, en l'absence de LiDAR HD, repli
    # NDP 0 sur le LAI Sentinel-2/PROSAIL agrégé par UGF (proxy dégradé). Le
    # cœur calcule le LAI ; l'app orchestre l'agrégation et pose la provenance.
    lai_max <- cfg$lai_max
    if (is.null(lai_max) && is.null(grid)) {
      lai_r <- .regen_lai_fallback(res, out_dir, cfg)
      if (!is.null(lai_r)) {
        lai_max <- tryCatch(.regen_lai_per_unit(res, lai_r), error = function(e) NULL)
        if (!is.null(lai_max)) {
          canopy <- "satellite"
          attr(res, "lai_source") <- "prosail_s2"
        }
      }
    }

    bil <- tryCatch({
      meteo <- nemeton::load_biljou_forcing(res, years = years, source = forcing,
                                            cache_dir = biljou_cache)
      sol   <- nemeton::build_biljou_soil(res, ewm = cfg$ewm)
      nemeton::regen_bilan_hydrique(res, meteo = meteo, sol = sol,
        lai_max = lai_max, forest_type = cfg$forest_type %||% "feuillu",
        years = years, budburst = cfg$budburst, leaf_fall = cfg$leaf_fall)
    }, error = function(e) {
      warnings <<- c(warnings, .strip_ansi(sprintf("BILJOU: %s", conditionMessage(e))))
      NULL
    })
    if (inherits(bil, "sf")) {
      if (identical(canopy, "satellite")) attr(bil, "lai_source") <- "prosail_s2"
      res <- bil
      tryCatch(sf::st_write(res, file.path(out_dir, "biljou.gpkg"),
                            quiet = TRUE, delete_dsn = TRUE),
               error = function(e) cli::cli_warn("regen engine: biljou cache not written"))
      cached <- c(cached, "biljou")
    } else {
      warnings <- c(warnings, i18n$t("regen_guard_biljou"))
    }
  }

  list(units = res, warnings = warnings, cached = cached, canopy = canopy)
}

#' Detect reference years from E-OBS for an AOI (spec 027 L2 / 034)
#'
#' @description
#' Builds the per-year summer E-OBS field for the units' footprint via the core
#' \code{nemeton::load_eobs_source()} (best-effort CDS download, same key as
#' ERA5; cached on disk), then derives the average / heatwave reference years
#' with \code{nemeton::microclimate_detect_years()}. Heavy (E-OBS acquisition)
#' and network-bound: run it in a \code{future} worker (serialisable inputs:
#' an \code{sf} + a path). Returns \code{NULL} on any failure (no key, no scene,
#' CI) so the UI falls back to manual year entry.
#'
#' An explicit \code{years} window is passed to \code{load_eobs_source()}: the
#' core CDS fetch bails out to \code{NULL} \emph{before downloading} when both
#' \code{years} and \code{period} are \code{NULL}. The window ends at
#' \code{current year - 2} (E-OBS publication lag; the core pins version 30.0e,
#' last full year 2024) and is clamped to start no earlier than 2011 so it fits
#' a single CDS period block (one download).
#'
#' @param units An \code{sf} of the UGF footprint.
#' @param project_path Project directory (E-OBS cache location).
#' @param year_window Integer window for the detection (default 10).
#' @return \code{list(year_moyenne=, year_canicule=)} or \code{NULL}.
#' @noRd
run_regeneration_detect_years <- function(units, project_path, year_window = 10) {
  if (!inherits(units, "sf")) return(NULL)
  cache_dir <- NULL
  if (!is.null(project_path)) {
    cache_dir <- file.path(project_path, "cache", "regeneration", "eobs")
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  }
  # E-OBS/CDS exige une plage d'années explicite : sans `years`, le cœur
  # (`.eobs_cds_fetch`) sort en NULL AVANT tout téléchargement (le bouton
  # affichait alors « indisponible » sans rien télécharger). On demande une
  # fenêtre récente se terminant à `année_courante - 2` (latence de publication
  # E-OBS ; le cœur pin la version 30.0e dont la dernière année pleine est
  # 2024). `start` est borné à 2011 pour rester dans un seul bloc de période CDS
  # (2011-2100) et ne déclencher qu'un téléchargement.
  end_year   <- as.integer(format(Sys.Date(), "%Y")) - 2L
  win        <- max(as.integer(year_window %||% 10L) + 2L, 7L)
  start_year <- max(2011L, end_year - win + 1L)
  years      <- seq.int(start_year, end_year)
  eobs <- tryCatch(
    nemeton::load_eobs_source(aoi = units, var = "tx", years = years,
                              cache_dir = cache_dir),
    error = function(e) {
      cli::cli_warn("regen detect_years: load_eobs_source failed: {conditionMessage(e)}")
      NULL
    })
  if (is.null(eobs)) return(NULL)
  yrs <- tryCatch(
    nemeton::microclimate_detect_years(eobs = eobs, aoi = units,
                                       year_window = year_window),
    error = function(e) {
      cli::cli_warn("regen detect_years: microclimate_detect_years failed: {conditionMessage(e)}")
      NULL
    })
  if (is.null(yrs) || is.null(yrs$year_moyenne)) return(NULL)
  list(year_moyenne = yrs$year_moyenne, year_canicule = yrs$year_canicule)
}

#' Satellite LAI fallback (Sentinel-2 / PROSAIL) for reGénération (spec 033 D5)
#'
#' Delegates the LAI inversion to \code{nemeton::lai_sentinel2()} (the core owns
#' the model) over the units' footprint for the reference summer, caching the
#' result under \code{<project>/cache/regeneration/lai_prosail.tif}. Returns the
#' \code{terra::SpatRaster} or \code{NULL} on any failure (graceful, no badge).
#' @noRd
.regen_lai_fallback <- function(units, out_dir, cfg = list()) {
  lai_cache <- file.path(out_dir, "lai_prosail.tif")
  if (file.exists(lai_cache)) {
    r <- tryCatch(terra::rast(lai_cache), error = function(e) NULL)
    if (!is.null(r)) return(r)
  }
  yr <- cfg$year_moyenne %||% cfg$year_canicule
  start <- if (!is.null(yr)) sprintf("%d-06-01", as.integer(yr)) else NULL
  end   <- if (!is.null(yr)) sprintf("%d-09-30", as.integer(yr)) else NULL
  r <- tryCatch(nemeton::lai_sentinel2(aoi = units, start = start, end = end),
                error = function(e) NULL)
  if (is.null(r)) return(NULL)
  tryCatch(terra::writeRaster(r, lai_cache, overwrite = TRUE),
           error = function(e) cli::cli_warn("regen engine: LAI cache not written"))
  r
}

#' Aggregate a LAI raster to a per-unit maximum-LAI vector
#' @noRd
.regen_lai_per_unit <- function(units, lai_r) {
  v <- terra::vect(sf::st_transform(units, terra::crs(lai_r)))
  ex <- terra::extract(lai_r, v, fun = mean, na.rm = TRUE, ID = FALSE)
  as.numeric(ex[[1]])
}

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

#' Species tolerance table for the optional target-species selector
#'
#' @return A data.frame from `nemeton::regeneration_tolerances()`, or NULL.
#' @noRd
regeneration_species_choices <- function() {
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

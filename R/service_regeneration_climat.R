# service_regeneration_climat.R - contexte climatique estival sur un cercle
# autour du projet (spec 063 coeur, brief 2026-10-18 cablage).
#
# Remplace, pour l'onglet " Contexte climatique " de reGeneration, la chaine
# E-OBS 12 ans + krigeage sur le relief (run_regeneration_context_raster) :
# variation totale sur la periode, a la resolution native de chaque source
# (E-OBS, CHELSA, SAFRAN), palette 10 x 10 de L'IF n. 49. Tout le calcul est dans
# le coeur (`nemeton::climat_*`) ; ce fichier ne fait que choisir les chemins de
# cache, lancer les calculs dans un worker et relire les sorties disque.

#' Sources of the climate-context sub-tabs (spec 063)
#' @noRd
.REGEN_CLIMAT_SOURCES <- c("eobs", "chelsa", "safran")

#' Grid side of the current bivariate climate palette (10 x 10, L'IF n. 49)
#' @noRd
.REGEN_CLIMAT_N <- 10L

#' Directory of the climate-context cache of a project
#' @noRd
.regen_climat_dir <- function(project_path) {
  file.path(project_path, "cache", "regeneration", "climat")
}

#' Normalise the period of a climate map into the core argument and a file tag
#'
#' @param periode `"complete"`, `"if49"`, or a length-2 numeric `c(debut, fin)`.
#' @return `list(arg, tag)`: `arg` is passed to
#'   `nemeton::climat_tendance_bivariee(periode =)`, `tag` names the cache file
#'   (`complete`, `if49` or `AAAA-AAAA`).
#' @noRd
.regen_climat_periode <- function(periode) {
  if (identical(periode, "if49")) return(list(arg = "if49", tag = "if49"))
  p <- suppressWarnings(as.integer(periode))
  if (length(p) == 2L && all(is.finite(p))) {
    p <- sort(p)
    return(list(arg = p, tag = sprintf("%d-%d", p[1], p[2])))
  }
  list(arg = "complete", tag = "complete")
}

#' Cache paths of a bivariate climate map
#'
#' `cache/regeneration/climat/carte_<source>_r<rayon>_<periode>.tif` ; the core
#' writes `_tendances.tif` and `.meta.json` next to it, the app adds
#' `.series.json` (paths of the series, read back on a map click).
#' @noRd
.regen_climat_paths <- function(project_path, source, rayon_km, periode) {
  dir <- .regen_climat_dir(project_path)
  base <- file.path(dir, sprintf("carte_%s_r%g_%s", source, as.numeric(rayon_km),
                                 .regen_climat_periode(periode)$tag))
  list(dir = dir, tif = paste0(base, ".tif"),
       tendances = paste0(base, "_tendances.tif"),
       meta = paste0(base, ".meta.json"),
       series = paste0(base, ".series.json"))
}

#' Cache path of a yearly point-cloud result
#' @noRd
.regen_nuages_path <- function(project_path, source, rayon_km, mode) {
  file.path(.regen_climat_dir(project_path),
            sprintf("nuages_%s_r%g_%s.rds", source, as.numeric(rayon_km), mode))
}

#' Is a cached climate map built on the current scheme?
#'
#' A map is stale when its palette is not the 10 x 10 one or when it does not
#' hold a total change over the period (an earlier per-decade scheme).
#' @noRd
.regen_climat_cache_current <- function(meta) {
  n <- suppressWarnings(as.integer(meta$palette$ncol %||% NA)[1])
  isTRUE(n == .REGEN_CLIMAT_N) && identical(meta$statistique, "variation_totale")
}

#' Read a cached bivariate climate map without recomputing
#'
#' @return `NULL` when absent or stale, else `list(raster, tendances, meta,
#'   series, cercle)`: `series` the cached series paths (`tx`, `rr`, `meta`,
#'   `cercle`) or `NULL`, `cercle` an `sf` or `NULL`.
#' @noRd
regeneration_climat_cached <- function(project_path, source, rayon_km, periode) {
  if (is.null(project_path)) return(NULL)
  p <- .regen_climat_paths(project_path, source, rayon_km, periode)
  if (!file.exists(p$tif) || !file.exists(p$meta)) return(NULL)
  meta <- tryCatch(jsonlite::read_json(p$meta, simplifyVector = TRUE),
                   error = function(e) NULL)
  if (is.null(meta) || !.regen_climat_cache_current(meta)) return(NULL)
  r <- tryCatch(terra::rast(p$tif), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  tend <- if (file.exists(p$tendances))
    tryCatch(terra::rast(p$tendances), error = function(e) NULL)
  series <- if (file.exists(p$series))
    tryCatch(jsonlite::read_json(p$series, simplifyVector = TRUE), error = function(e) NULL)
  cercle <- if (!is.null(series$cercle) && file.exists(series$cercle))
    tryCatch(sf::st_read(series$cercle, quiet = TRUE), error = function(e) NULL)
  list(raster = r, tendances = tend, meta = meta, series = series, cercle = cercle)
}

#' Purge the project cache of one (source, radius, period) climate map
#'
#' Removes the map files only: the series of the circle and the machine-wide
#' E-OBS cache are kept (the recompute reclassifies, it does not re-download).
#' @noRd
regeneration_climat_purge <- function(project_path, source, rayon_km, periode) {
  p <- .regen_climat_paths(project_path, source, rayon_km, periode)
  unlink(c(p$tif, p$tendances, p$meta, p$series))
  invisible(TRUE)
}

#' Has the machine-wide E-OBS monthly France cache been built?
#'
#' Reads the core's documented location (`get_global_cache_dir()/eobs`, see
#' `nemeton::climat_ete_series()`): its absence means the first E-OBS map
#' downloads about 8 GB.
#' @noRd
regen_climat_eobs_pret <- function(version = "33.0e") {
  dir <- tryCatch(file.path(nemeton::get_global_cache_dir(), "eobs"),
                  error = function(e) NULL)
  if (is.null(dir)) return(FALSE)
  all(file.exists(file.path(dir, sprintf("eobs_%s_mensuel_france_v%s.tif",
                                         c("tx", "rr"), version))))
}

#' Progress callback of the climate computations (worker -> session)
#'
#' Same channel as `.regen_eobs_progress_cb()`: each core step is written to
#' `climat_status.json`, polled each second by the module.
#' @param dir Climate cache directory.
#' @noRd
.regen_climat_progress_cb <- function(dir) {
  function(p) {
    tryCatch({
      if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
      payload <- list(phase = as.character(p$current %||% ""),
                      source = p$source %||% NULL, var = p$var %||% NULL,
                      annee = p$annee %||% NULL, i = p$i %||% NULL,
                      n = p$n %||% NULL, ts = as.integer(Sys.time()))
      tmp <- file.path(dir, ".climat_status.json.tmp")
      writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"), tmp)
      file.rename(tmp, file.path(dir, "climat_status.json"))
    }, error = function(e) invisible(NULL))
    invisible(NULL)
  }
}

#' Last progress step of a climate computation, or NULL
#' @noRd
.regen_read_climat_step <- function(project_path) {
  if (is.null(project_path)) return(NULL)
  f <- file.path(.regen_climat_dir(project_path), "climat_status.json")
  if (!file.exists(f)) return(NULL)
  tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
}

#' Summer series of the circle (core, cached in the project)
#' @noRd
.regen_climat_series <- function(units, project_path, source, rayon_km) {
  nemeton::climat_ete_series(
    units, source = source, rayon_km = rayon_km,
    cache_dir = file.path(project_path, "cache", "regeneration"),
    progress_callback = .regen_climat_progress_cb(.regen_climat_dir(project_path)))
}

#' Bivariate climate map of the circle (worker)
#'
#' Runs in a `future` worker: the `SpatRaster`s stay on disk (written by the
#' core under `cache_path`), only the two metas come back. The series paths are
#' kept next to the map (`.series.json`) for the map click.
#'
#' @return `list(series, carte)` - the `meta` of
#'   `nemeton::climat_ete_series()` and of `nemeton::climat_tendance_bivariee()`.
#' @noRd
run_regeneration_climat <- function(units, project_path, source, rayon_km, periode) {
  source <- match.arg(source, .REGEN_CLIMAT_SOURCES)
  p <- .regen_climat_paths(project_path, source, rayon_km, periode)
  if (!dir.exists(p$dir)) dir.create(p$dir, recursive = TRUE)
  s <- .regen_climat_series(units, project_path, source, rayon_km)
  b <- nemeton::climat_tendance_bivariee(
    s, periode = .regen_climat_periode(periode)$arg, cache_path = p$tif)
  if (identical(b$meta$status, "ok") && !is.null(s$meta$fichiers)) {
    tryCatch(jsonlite::write_json(s$meta$fichiers, p$series, auto_unbox = TRUE),
             error = function(e) NULL)
  }
  list(series = s$meta, carte = b$meta)
}

#' Yearly summer point clouds of the circle (worker), memoised on disk
#'
#' @return `list(points, moyennes, limites, meta)` (plain data frames, safe to
#'   send back from a worker).
#' @noRd
run_regeneration_nuages <- function(units, project_path, source, rayon_km, mode) {
  source <- match.arg(source, .REGEN_CLIMAT_SOURCES)
  f <- .regen_nuages_path(project_path, source, rayon_km, mode)
  if (file.exists(f)) {
    res <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.null(res)) return(res)
  }
  s <- .regen_climat_series(units, project_path, source, rayon_km)
  res <- nemeton::climat_nuages_annuels(s, mode = mode)
  if (identical(res$meta$status, "ok")) {
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    tryCatch(saveRDS(res, f), error = function(e) NULL)
  }
  res
}

#' Read memoised point clouds without computing, or NULL
#' @noRd
regeneration_nuages_cached <- function(project_path, source, rayon_km, mode) {
  if (is.null(project_path)) return(NULL)
  f <- .regen_nuages_path(project_path, source, rayon_km, mode)
  if (!file.exists(f)) return(NULL)
  tryCatch(readRDS(f), error = function(e) NULL)
}

#' Read the summer series of a cached map, for the map click
#' @return `list(tx, rr)` of `SpatRaster`s, or `NULL`.
#' @noRd
.regen_climat_series_rasters <- function(series) {
  if (is.null(series$tx) || is.null(series$rr) ||
      !file.exists(series$tx) || !file.exists(series$rr)) return(NULL)
  tryCatch(list(tx = terra::rast(series$tx), rr = terra::rast(series$rr)),
           error = function(e) NULL)
}

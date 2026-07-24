# ===========================================================================
# Service — IO partagées ForêtAccess (onglets « Terrain accessible »)
# ===========================================================================
#
# Helpers d'acquisition/résolution communs aux onglets qui s'appuient sur le
# paquet `foretaccess` : Accessibilité (`service_accessibility.R`) et, à venir,
# Desserte (`service_desserte.R`). Non-Shiny, sûrs en worker `future`.
#
# Règle 1/2 : aucune logique métier ici — on résout la géométrie du projet et on
# télécharge/met en cache les couches d'entrée ; tout le calcul reste dans
# `foretaccess`. Ce fichier n'existe que pour éviter de dupliquer ces IO entre
# les deux services (extraction ADR-neutre, sans changement de comportement).

#' Resolve a project's area of interest as polygons (EPSG:2154)
#'
#' Mirrors the `units_sf` fallback used across the app (indicators_sf -> UGF ->
#' parcels), projected to the French national CRS Lambert-93 (EPSG:2154) which
#' `foretaccess` expects. Both the accessibility engines (as the forest AOI) and
#' the road-network engines (as the parcels to serve) consume it. Returns `NULL`
#' when the project carries no usable geometry.
#'
#' @param project The current project (list-like) or `NULL`.
#' @return An `sf` of polygons in EPSG:2154, or `NULL`.
#' @noRd
.resolve_project_aoi_2154 <- function(project) {
  if (is.null(project)) return(NULL)
  to_2154 <- function(x) tryCatch(sf::st_transform(x, 2154), error = function(e) x)

  sfx <- project$indicators_sf
  if (inherits(sfx, "sf") && nrow(sfx) > 0L) return(to_2154(sfx))

  ugf <- tryCatch(if (has_ug_data(project)) ug_build_sf(project) else NULL,
                  error = function(e) NULL)
  if (inherits(ugf, "sf") && nrow(ugf) > 0L) return(to_2154(ugf))

  parc <- project$parcels
  if (inherits(parc, "sf") && nrow(parc) > 0L) return(to_2154(parc))
  NULL
}

#' Acquire a genuine 5 m terrain DEM for an AOI via the IGN HIGH-RES layer
#'
#' `foretaccess::acquire_mnt()` fetches the WMS coverage
#' `ELEVATION.ELEVATIONGRIDCOVERAGE`, which the Géoplateforme caps at WMTS zoom
#' 11 (~25 m effective). Resampled to 5 m it is a **blocky staircase** whose
#' edges yield spurious steep slopes — surfacing as axis-aligned `inexploitable`
#' artefacts in the class rasters. The **`.HIGHRES`** variant of the same
#' coverage goes to zoom 14 and returns a genuine 5 m terrain DEM (measured 0 %
#' flat neighbours, realistic slopes, vs 43 % / 71° for the base layer). Both are
#' `image/x-bil;bits=32` (raw elevation), France-wide, fetched on demand.
#'
#' This helper fetches `.HIGHRES` via `happign` and caches it next to the other
#' per-emprise artefacts (`mnt_highres.tif`). Returns the file path, or `NULL` on
#' failure (missing `happign`, network error) so the caller can fall back to
#' `foretaccess::acquire_mnt()`.
#'
#' TODO(foretaccess): once `foretaccess::get_layer_service("dem", "FR")` points
#' at `…HIGHRES`, drop this override and call `acquire_mnt()` directly.
#'
#' @param aoi An `sf`/`sfc` AOI (any CRS; reprojected to `crs`).
#' @param res_m Target resolution in metres (Sylvaccess is calibrated for 5 m).
#' @param crs Target EPSG (Lambert-93 / 2154 for France).
#' @param cache_dir Directory holding the cached DEM.
#' @param overwrite Force a re-fetch even if the cache file exists.
#' @return Path to the cached DEM GeoTIFF, or `NULL`.
#' @noRd
.acquire_mnt_highres <- function(aoi, res_m = 5, crs = 2154, cache_dir = tempdir(),
                                 overwrite = FALSE) {
  if (!requireNamespace("happign", quietly = TRUE)) return(NULL)
  # La RÉSOLUTION entre dans le nom du cache. Sans ça, une demande à 1 m (chemin
  # de correction LiDAR, cf. .acquire_mnt_desserte) écraserait le MNT à 5 m de
  # l'analyse d'accessibilité, qui partage le même `cache_dir` d'emprise — et
  # réciproquement, l'un servirait l'autre en silence à la mauvaise résolution.
  # `mnt_highres.tif` (sans suffixe) reste lu en repli pour ne pas invalider les
  # caches déjà sur disque, qui sont tous à 5 m.
  path <- file.path(cache_dir, sprintf("mnt_highres_%gm.tif", res_m))
  if (file.exists(path) && !isTRUE(overwrite)) return(path)
  legacy <- file.path(cache_dir, "mnt_highres.tif")
  if (identical(as.numeric(res_m), 5) && file.exists(legacy) &&
      !isTRUE(overwrite)) {
    return(legacy)
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  aoi <- tryCatch(sf::st_transform(aoi, crs), error = function(e) aoi)
  ok <- tryCatch({
    happign::get_wms_raster(
      x = aoi, layer = "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES",
      res = res_m, crs = crs, rgb = FALSE,
      filename = path, overwrite = TRUE, verbose = FALSE)
    file.exists(path)
  }, error = function(e) FALSE)
  if (isTRUE(ok)) path else NULL
}

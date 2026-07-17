# ===========================================================================
# Service — Accessibilité forestière (ForêtAccess, onglet Terrain)
# ===========================================================================
#
# Adaptateur applicatif (non-Shiny) autour du paquet `foretaccess` (cartographie
# de l'accessibilité, réimplémentation de Sylvaccess — INRAE). Conformément aux
# règles 1/2 (aucune logique métier ici), ce fichier ne fait qu'orchestrer :
#   - résoudre l'AOI (géométrie du projet) et le MNT (déjà sur disque) ;
#   - acquérir la desserte (OSM) pour l'emprise ;
#   - appeler `foretaccess::preprocess()` + les moteurs terrestres exportés ;
#   - persister les rasters de classes + un GeoPackage vecteur exportable.
#
# Premier incrément : moteurs TERRESTRES uniquement (skidder, porteur, camion
# DFCI), tous en R pur. Le moteur câble (noyau Rust) sera ajouté ensuite ; son
# ajout ne changera pas la dépendance (foretaccess est déjà en Imports).
#
# Le calcul est LONG (rasterisation, focal, propagation least-cost) : il tourne
# dans un worker `future` (cf. mod_accessibility.R). Un `SpatRaster` terra n'est
# PAS sérialisable entre process (pointeur externe) : le worker ÉCRIT les
# rasters sur disque et ne renvoie que des CHEMINS + les data.frames de récap
# (sérialisables). Le process principal relit les `.tif` pour l'affichage.

#' Terrestrial accessibility engines exposed by the app (first increment)
#'
#' The three pure-R engines from `foretaccess`. The cable engine (Rust core) is
#' intentionally excluded here and will be added in a later increment.
#' @noRd
ACCESSIBILITY_ENGINES <- c("skidder", "porteur", "camion_dfci")

#' Resolve a project's area of interest as forest polygons (EPSG:2154)
#'
#' Mirrors the `units_sf` fallback used elsewhere (indicators_sf -> UGF ->
#' parcels), projected to the French national CRS Lambert-93 (EPSG:2154) which
#' `foretaccess` expects. Returns `NULL` when the project carries no usable
#' geometry.
#'
#' @param project The current project (list-like) or `NULL`.
#' @return An `sf` of polygons in EPSG:2154, or `NULL`.
#' @noRd
.resolve_accessibility_aoi <- function(project) {
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

#' Resolve the on-disk terrain DEM (MNT) for accessibility
#'
#' Reuses the reGénération resolver (LiDAR HD MNT mosaic / BD ALTI, searched in
#' the project root and the canonical `cache/layers/` dir). Returns the DEM path
#' or `NULL` when none is available — in which case the caller surfaces a
#' "compute indicators first" message rather than acquiring a fresh DEM.
#'
#' @param project_path Project directory.
#' @return A DEM GeoTIFF path, or `NULL`.
#' @noRd
.resolve_accessibility_mnt <- function(project_path) {
  if (is.null(project_path)) return(NULL)
  tryCatch(.resolve_regen_dem(project_path), error = function(e) NULL)
}

#' Directory holding the accessibility artefacts of a project
#' @noRd
.accessibility_cache_dir <- function(project_path) {
  file.path(project_path, "cache", "accessibility")
}

#' Run the terrestrial accessibility engines for a project (worker-side)
#'
#' Heavy, self-contained function meant to run in a `future` worker. Acquires
#' the road network (OSM) for the AOI, runs `foretaccess::preprocess()` then the
#' requested terrestrial engines, writes each engine's categorical class raster
#' to `cache/accessibility/acc_<engine>.tif`, and writes an exportable
#' GeoPackage (`foret` + `desserte` layers). Returns only serialisable data
#' (paths + recap data.frames), never terra/sf objects tied to this process.
#'
#' Best-effort and structured: every failure path returns
#' `list(status = "error", message = ...)` instead of throwing, so the caller's
#' status observer can surface a clean toast.
#'
#' @param aoi Forest polygons (`sf`, EPSG:2154).
#' @param mnt_path Terrain DEM GeoTIFF path.
#' @param engines Character vector, subset of `ACCESSIBILITY_ENGINES`.
#' @param cache_dir Destination directory for the artefacts.
#' @return A named list describing the run (see details).
#' @noRd
run_accessibility <- function(aoi, mnt_path, engines, cache_dir) {
  if (!requireNamespace("foretaccess", quietly = TRUE)) {
    return(list(status = "error", reason = "accessibility_no_foretaccess"))
  }
  if (is.null(aoi) || !inherits(aoi, "sf") || nrow(aoi) == 0L) {
    return(list(status = "error", reason = "accessibility_need_project"))
  }
  if (is.null(mnt_path) || !file.exists(mnt_path)) {
    return(list(status = "error", reason = "accessibility_no_mnt"))
  }
  engines <- intersect(engines, ACCESSIBILITY_ENGINES)
  if (length(engines) == 0L) {
    return(list(status = "error", reason = "accessibility_need_engine"))
  }

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # Aligner l'AOI sur le CRS du MNT : preprocess() valide strictement l'égalité
  # des CRS (mnt vs vecteurs) et abort sinon. Le MNT fait foi (grille de calcul).
  mnt <- tryCatch(terra::rast(mnt_path), error = function(e) NULL)
  if (is.null(mnt)) return(list(status = "error", reason = "accessibility_no_mnt"))
  mnt_crs <- tryCatch(sf::st_crs(terra::crs(mnt)), error = function(e) sf::st_crs(2154))
  if (is.na(mnt_crs)) mnt_crs <- sf::st_crs(2154)
  aoi <- tryCatch(sf::st_transform(aoi, mnt_crs), error = function(e) aoi)
  epsg <- tryCatch(mnt_crs$epsg %||% 2154L, error = function(e) 2154L)
  if (is.null(epsg) || is.na(epsg)) epsg <- 2154L

  # 1. Desserte (réseau routier/pistes) via OSM sur l'emprise de l'AOI.
  desserte <- tryCatch(
    foretaccess::acquire_desserte(aoi, crs = epsg, cache_dir = cache_dir),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(desserte, "acc_err")) {
    return(list(status = "error", reason = "accessibility_desserte_failed",
                detail = desserte$msg))
  }
  if (!inherits(desserte, "sf") || nrow(desserte) == 0L) {
    return(list(status = "error", reason = "accessibility_desserte_empty"))
  }

  # 2. Prétraitement commun (pente, exposition, masques, rasterisation).
  pre <- tryCatch(
    foretaccess::preprocess(mnt = mnt, desserte = desserte, foret = aoi),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(pre, "acc_err")) {
    return(list(status = "error", reason = "accessibility_preprocess_failed",
                detail = pre$msg))
  }

  # 3. Moteurs terrestres : raster de classes -> disque ; recap -> mémoire.
  engine_fun <- list(
    skidder = foretaccess::skidder,
    porteur = foretaccess::porteur,
    camion_dfci = foretaccess::camion_dfci)
  recaps <- list()
  raster_paths <- list()
  for (eng in engines) {
    res <- tryCatch(engine_fun[[eng]](pre),
      error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
    if (inherits(res, "acc_err")) {
      return(list(status = "error", reason = "accessibility_engine_failed",
                  detail = paste0(eng, ": ", res$msg)))
    }
    rp <- file.path(cache_dir, paste0("acc_", eng, ".tif"))
    ok <- tryCatch({
      terra::writeRaster(res$accessibilite, rp, overwrite = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) raster_paths[[eng]] <- rp
    recaps[[eng]] <- res$recap
  }

  # 4. GeoPackage exportable : AOI (foret) + desserte. Best-effort.
  gpkg_path <- file.path(cache_dir, "accessibilite.gpkg")
  unlink(gpkg_path)
  tryCatch({
    sf::st_write(sf::st_transform(aoi, 2154), gpkg_path, layer = "foret",
                 quiet = TRUE, delete_dsn = TRUE)
    sf::st_write(sf::st_transform(desserte, 2154), gpkg_path, layer = "desserte",
                 quiet = TRUE, append = TRUE)
  }, error = function(e) cli::cli_warn(
    "accessibility GPKG write failed: {conditionMessage(e)}"))

  list(
    status = "success",
    engines = engines,
    recaps = recaps,
    raster_paths = raster_paths,
    gpkg_path = if (file.exists(gpkg_path)) gpkg_path else NULL,
    n_desserte = nrow(desserte))
}

#' Combine per-engine recap tables into a single display data.frame
#'
#' One row per accessibility class, one `surface_ha` column per engine
#' (translated engine header). Classes are unioned across engines and NA-safe.
#'
#' @param recaps Named list of recap data.frames (`classe`, `surface_ha`).
#' @param i18n An i18n object (for the engine column headers).
#' @return A data.frame ready for `DT::datatable`, or `NULL` when empty.
#' @noRd
.accessibility_recap_table <- function(recaps, i18n) {
  recaps <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, recaps %||% list())
  if (length(recaps) == 0L) return(NULL)
  classes <- unique(unlist(lapply(recaps, function(d) as.character(d$classe))))
  out <- data.frame(classe = classes, stringsAsFactors = FALSE)
  eng_label <- c(
    skidder = i18n$t("acc_engine_skidder"),
    porteur = i18n$t("acc_engine_porteur"),
    camion_dfci = i18n$t("acc_engine_dfci"))
  for (eng in names(recaps)) {
    d <- recaps[[eng]]
    idx <- match(out$classe, as.character(d$classe))
    col <- round(as.numeric(d$surface_ha)[idx], 2)
    col[is.na(col)] <- 0
    out[[eng_label[[eng]] %||% eng]] <- col
  }
  out
}

#' Export the accessibility GeoPackage produced by a run
#'
#' Copies the cached `accessibilite.gpkg` to the download target. Returns
#' `TRUE` on success, `FALSE` (best-effort) otherwise.
#'
#' @param result A `run_accessibility()` result list.
#' @param file Destination path handed to the browser.
#' @return Invisibly `TRUE`/`FALSE`.
#' @noRd
export_accessibility_geopackage <- function(result, file) {
  src <- tryCatch(result$gpkg_path, error = function(e) NULL)
  if (is.null(src) || !file.exists(src)) return(invisible(FALSE))
  invisible(isTRUE(tryCatch(file.copy(src, file, overwrite = TRUE),
                            error = function(e) FALSE)))
}

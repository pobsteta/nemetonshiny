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

#' Directory holding the accessibility artefacts of a project
#' @noRd
.accessibility_cache_dir <- function(project_path) {
  file.path(project_path, "cache", "accessibility")
}

#' Run the terrestrial accessibility engines for a project (worker-side)
#'
#' Heavy, self-contained function meant to run in a `future` worker. Acquires
#' the **IGN RGE ALTI 5 m** DEM (Sylvaccess-calibrated resolution) and the road
#' network (OSM) for the AOI, runs `foretaccess::preprocess()` then the
#' requested terrestrial engines, writes each engine's categorical class raster
#' to `cache/accessibility/acc_<engine>.tif`, and writes an exportable
#' GeoPackage (`foret` + `desserte` layers). Returns only serialisable data
#' (paths + recap data.frames), never terra/sf objects tied to this process.
#'
#' The DEM and road network are cached under `cache_dir` by `acquire_mnt()` /
#' `acquire_desserte()`: a second run on the same project reuses them (no
#' re-download).
#'
#' Best-effort and structured: every failure path returns
#' `list(status = "error", reason = ...)` instead of throwing, so the caller's
#' status observer can surface a clean toast.
#'
#' @param aoi_path GeoPackage path holding the forest polygons (AOI). The AOI is
#'   passed as a FILE, never as a live `sf`: an `sf`/geometry can carry an
#'   external pointer that fails to serialise across the `future` process
#'   boundary ("external pointer is not valid"). The worker reads it here.
#' @param engines Character vector, subset of `ACCESSIBILITY_ENGINES`.
#' @param cache_dir Destination directory for the artefacts (and DEM/road cache).
#' @return A named list describing the run (see details).
#' @noRd
run_accessibility <- function(aoi_path, engines, cache_dir) {
  if (!requireNamespace("foretaccess", quietly = TRUE)) {
    return(list(status = "error", reason = "accessibility_no_foretaccess"))
  }
  if (is.null(aoi_path) || !file.exists(aoi_path)) {
    return(list(status = "error", reason = "accessibility_need_project"))
  }
  aoi <- tryCatch(sf::st_read(aoi_path, quiet = TRUE), error = function(e) NULL)
  if (is.null(aoi) || !inherits(aoi, "sf") || nrow(aoi) == 0L) {
    return(list(status = "error", reason = "accessibility_need_project"))
  }
  engines <- intersect(engines, ACCESSIBILITY_ENGINES)
  if (length(engines) == 0L) {
    return(list(status = "error", reason = "accessibility_need_engine"))
  }

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # Tout le pipeline travaille en Lambert-93 (EPSG:2154), CRS national FR et
  # celui dans lequel acquire_mnt fournit le MNT : preprocess() valide l'égalité
  # stricte des CRS (mnt vs vecteurs).
  epsg <- 2154L
  aoi <- tryCatch(sf::st_transform(aoi, epsg), error = function(e) aoi)

  # 1. MNT IGN RGE ALTI 5 m (résolution pour laquelle Sylvaccess/ForêtAccess est
  # calibré). acquire_mnt met en cache dans cache_dir : un 2e run le réutilise.
  mnt_path <- tryCatch(
    foretaccess::acquire_mnt(aoi, res_m = 5, crs = epsg, cache_dir = cache_dir),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(mnt_path, "acc_err")) {
    return(list(status = "error", reason = "accessibility_mnt_failed",
                detail = mnt_path$msg))
  }
  mnt <- tryCatch(terra::rast(mnt_path), error = function(e) NULL)
  if (is.null(mnt)) return(list(status = "error", reason = "accessibility_mnt_failed"))

  # 2. Desserte (réseau routier/pistes) via OSM sur l'emprise de l'AOI (cachée).
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

  # 3. Prétraitement commun (pente, exposition, masques, rasterisation).
  pre <- tryCatch(
    foretaccess::preprocess(mnt = mnt, desserte = desserte, foret = aoi),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(pre, "acc_err")) {
    return(list(status = "error", reason = "accessibility_preprocess_failed",
                detail = pre$msg))
  }

  # 4. Moteurs terrestres : raster de classes -> disque ; recap -> mémoire.
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

    # Le skidder produit AUSSI le raster « classes de débardage » : les bandes de
    # distance Sylvaccess (0-250, …, > 2000 m) + inaccessible / inexploitable /
    # hors_foret, prêt à l'affichage avec sa table de couleurs (vert proche ->
    # rouge lointain). `pre` fournit le masque d'exclusion (classe inexploitable).
    if (identical(eng, "skidder")) {
      dbg <- tryCatch(foretaccess::classes_debardage(res, pre),
        error = function(e) NULL)
      if (!is.null(dbg)) {
        rpd <- file.path(cache_dir, "acc_classes_debardage.tif")
        okd <- tryCatch({
          terra::writeRaster(dbg, rpd, overwrite = TRUE)
          TRUE
        }, error = function(e) FALSE)
        if (isTRUE(okd)) raster_paths[["classes_debardage"]] <- rpd
      }
    }
  }

  # 5. GeoPackage exportable : AOI (foret) + desserte. Best-effort.
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

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

#' Reconstruct a run result from a project's cached accessibility rasters
#'
#' Lets the tab show a **previously computed** analysis without recomputing:
#' scans `cache/accessibility/` for the known class rasters (`acc_<engine>.tif`
#' and `acc_classes_debardage.tif`) and rebuilds a minimal `run_accessibility()`
#' result (same shape as a live run, marked `from_cache = TRUE`). The per-engine
#' recap tables are NOT persisted, so `recaps` is empty — only the display layers
#' (rasters + exportable GeoPackage) are restored. Returns `NULL` when the project
#' has no cached raster yet.
#'
#' @param project_path Project directory, or `NULL`.
#' @return A result list compatible with the map/layer UI, or `NULL`.
#' @noRd
.load_cached_accessibility <- function(project_path) {
  if (is.null(project_path) || !nzchar(project_path)) return(NULL)
  cache_dir <- .accessibility_cache_dir(project_path)
  if (!dir.exists(cache_dir)) return(NULL)
  # Engines d'abord (dans l'ordre canonique), puis les classes de débardage :
  # le 1er élément devient la couche sélectionnée par défaut dans l'UI.
  known <- c(
    stats::setNames(paste0("acc_", ACCESSIBILITY_ENGINES, ".tif"),
                    ACCESSIBILITY_ENGINES),
    classes_debardage = "acc_classes_debardage.tif")
  raster_paths <- list()
  engines <- character(0)
  for (nm in names(known)) {
    p <- file.path(cache_dir, known[[nm]])
    if (file.exists(p)) {
      raster_paths[[nm]] <- p
      if (nm %in% ACCESSIBILITY_ENGINES) engines <- c(engines, nm)
    }
  }
  if (length(raster_paths) == 0L) return(NULL)
  gpkg <- file.path(cache_dir, "accessibilite.gpkg")
  list(
    status = "success",
    engines = engines,
    recaps = list(),
    raster_paths = raster_paths,
    gpkg_path = if (file.exists(gpkg)) gpkg else NULL,
    n_desserte = NA_integer_,
    from_cache = TRUE)
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
#' @param buffer_m Numeric buffer, in metres, grown around the forest AOI for the
#'   DEM and road-network acquisition. Access to a stand comes from roads that
#'   lie OUTSIDE it, and the least-cost propagation needs the surrounding terrain;
#'   without a buffer the road network is clipped at the parcel edge and access
#'   is truncated. The forest **mask** stays the original AOI — only the analysed
#'   emprise widens. `0` (default) keeps the historical behaviour (forest extent
#'   only). Each buffer value acquires the DEM/roads in its own sub-cache so
#'   changing it re-fetches cleanly instead of reusing a stale emprise.
#' @return A named list describing the run (see details).
#' @noRd
run_accessibility <- function(aoi_path, engines, cache_dir, buffer_m = 0) {
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

  # Emprise d'ACQUISITION : AOI dilatée d'une zone tampon (buffer_m). L'accès à un
  # peuplement vient des routes situées HORS de lui, et la propagation least-cost
  # a besoin du relief alentour ; sans tampon, la desserte est coupée au bord de
  # la parcelle et l'accès est tronqué. Le MASQUE forêt (foret =) reste l'AOI
  # d'origine ci-dessous : seule l'emprise analysée s'élargit.
  buffer_m <- suppressWarnings(as.numeric(buffer_m %||% 0))
  if (!is.finite(buffer_m) || buffer_m < 0) buffer_m <- 0
  aoi_ext <- aoi
  if (buffer_m > 0) {
    aoi_ext <- tryCatch(sf::st_buffer(aoi, buffer_m), error = function(e) aoi)
  }
  # Chaque valeur de tampon acquiert MNT/desserte dans son propre sous-cache : un
  # changement de tampon re-télécharge sur la nouvelle emprise au lieu de réutiliser
  # un MNT/desserte figé à l'ancienne emprise (acquire_* cache par nom de fichier).
  acq_dir <- file.path(cache_dir, sprintf("emprise_%gm", buffer_m))
  dir.create(acq_dir, recursive = TRUE, showWarnings = FALSE)

  # 1. MNT IGN RGE ALTI 5 m (résolution pour laquelle Sylvaccess/ForêtAccess est
  # calibré). acquire_mnt met en cache dans acq_dir : un 2e run le réutilise.
  mnt_path <- tryCatch(
    foretaccess::acquire_mnt(aoi_ext, res_m = 5, crs = epsg, cache_dir = acq_dir),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(mnt_path, "acc_err")) {
    return(list(status = "error", reason = "accessibility_mnt_failed",
                detail = mnt_path$msg))
  }
  mnt <- tryCatch(terra::rast(mnt_path), error = function(e) NULL)
  if (is.null(mnt)) return(list(status = "error", reason = "accessibility_mnt_failed"))

  # 2. Desserte (réseau routier/pistes) via OSM sur l'emprise tamponnée (cachée).
  desserte <- tryCatch(
    foretaccess::acquire_desserte(aoi_ext, crs = epsg, cache_dir = acq_dir),
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

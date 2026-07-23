# ===========================================================================
# Service — Création de desserte forestière (ForêtAccess, onglet Terrain)
# ===========================================================================
#
# Adaptateur applicatif (non-Shiny) autour des moteurs de CRÉATION de réseau de
# `foretaccess` (conception de desserte, réimplémentation de SylvaRoad — INRAE).
# Règles 1/2 (aucune logique métier ici) : ce fichier ne fait qu'orchestrer —
# résoudre parcelles + entrées terrain (MNT / desserte existante / forêt),
# appeler `surface_cout_construction()` puis un moteur de création, et persister
# le réseau (raster + GeoPackage vecteur exportable).
#
# v1 : moteur GLOUTON uniquement. Mesuré sur AOI réelle (Chastel-Nouvel, 30
# parcelles / 31 ha) : `reseau_desserte(mode="glouton")` = ~11,5 min (un tracé
# A* par parcelle sur ~302k cellules), `preprocess`/`surface_cout_construction`
# < 2 s. Le calcul tourne donc dans un worker `future` (cf. mod_desserte.R),
# comme l'accessibilité, et le moteur est OPT-IN avec avertissement « calcul
# long ». Le mode STEINER (N² tracés → estimé > 5 h à 30 parcelles) et les
# optimiseurs (`optimiser_reseau`) sont volontairement NON exposés tant qu'un
# travail perf n'a pas eu lieu côté `foretaccess` (brief cœur).
#
# Un `SpatRaster` terra n'est PAS sérialisable entre process : le worker ÉCRIT
# le raster réseau + le GeoPackage sur disque et ne renvoie que des CHEMINS +
# des scalaires (coût, connexité, parcelles desservies). Le process principal
# relit pour l'affichage.

#' Road-network creation engines exposed by the app (v1)
#'
#' Only the **greedy** (`glouton`) engine of `foretaccess::reseau_desserte()` is
#' exposed. The Steiner mode (`reseau_desserte(mode="steiner")`, N² traces) and
#' the optimisers (`optimiser_reseau()`, strategies multistart/recuit/riprute)
#' are intentionally excluded until their cost is brought to interactive scale
#' upstream: measured on a 30-parcel / 31 ha AOI, greedy already takes ~11.5 min,
#' Steiner would be N× that. Documented as a core brief.
#' @noRd
DESSERTE_ENGINES <- c("glouton")

#' Directory holding the desserte artefacts of a project
#' @noRd
.desserte_cache_dir <- function(project_path) {
  file.path(project_path, "cache", "desserte")
}

#' Reconstruct a run result from a project's cached desserte network
#'
#' Lets the tab show a **previously computed** network without recomputing (a
#' run is ~11.5 min): scans `cache/desserte/` for the network raster
#' (`reseau_<engine>.tif`) and its sidecar metadata (`reseau_<engine>.rds`,
#' holding the scalars — cost, connectedness, served parcels — that a raster
#' cannot carry) and rebuilds a minimal `run_desserte()` result marked
#' `from_cache = TRUE`. Returns `NULL` when the project has no cached network.
#'
#' @param project_path Project directory, or `NULL`.
#' @return A result list compatible with the map/badge UI, or `NULL`.
#' @noRd
.load_cached_desserte <- function(project_path) {
  if (is.null(project_path) || !nzchar(project_path)) return(NULL)
  cache_dir <- .desserte_cache_dir(project_path)
  if (!dir.exists(cache_dir)) return(NULL)
  for (eng in DESSERTE_ENGINES) {
    rp <- file.path(cache_dir, paste0("reseau_", eng, ".tif"))
    if (!file.exists(rp)) next
    meta <- tryCatch(readRDS(file.path(cache_dir, paste0("reseau_", eng, ".rds"))),
                     error = function(e) list())
    gpkg <- file.path(cache_dir, "desserte.gpkg")
    return(list(
      status = "success",
      engine = eng,
      reseau_path = rp,
      gpkg_path = if (file.exists(gpkg)) gpkg else NULL,
      cout = meta$cout %||% NA_real_,
      connexe = meta$connexe %||% NA,
      raccorde = meta$raccorde %||% NA,
      n_desservies = meta$n_desservies %||% NA_integer_,
      n_parcelles = meta$n_parcelles %||% NA_integer_,
      from_cache = TRUE))
  }
  NULL
}

#' Run a road-network creation engine for a project (worker-side)
#'
#' Heavy, self-contained function meant to run in a `future` worker. Acquires
#' the **IGN RGE ALTI 5 m** DEM (HIGHRES, `.acquire_mnt_highres`), the existing
#' road network (**IGN BD TOPO V3**) and the forest mask (**IGN BD Forêt V2**)
#' for the buffered AOI, runs `foretaccess::preprocess()` +
#' `surface_cout_construction()`, then the requested creation engine
#' (`reseau_desserte()`), writes the network class raster to
#' `cache/desserte/reseau_<engine>.tif` (+ a `.rds` sidecar with the scalars)
#' and an exportable GeoPackage (`parcelles` + `desserte_existante` +
#' `reseau_cree` layers). Returns only serialisable data.
#'
#' Best-effort and structured: every failure path returns
#' `list(status = "error", reason = ...)` instead of throwing.
#'
#' @param aoi_path GeoPackage path holding the parcels to serve (AOI). Passed as
#'   a FILE, never a live `sf` (external-pointer serialisation across the
#'   `future` boundary).
#' @param engine Character, one of `DESSERTE_ENGINES`.
#' @param cache_dir Destination directory for the artefacts (and DEM/road cache).
#' @param buffer_m Numeric buffer (m) grown around the AOI for the DEM and road
#'   acquisition: access to a stand comes from roads OUTSIDE it, and the trace
#'   solver needs the surrounding terrain. The parcels served stay the original
#'   AOI — only the analysed emprise widens.
#' @return A named list describing the run.
#' @noRd
run_desserte <- function(aoi_path, engine, cache_dir, buffer_m = 0) {
  if (!requireNamespace("foretaccess", quietly = TRUE)) {
    return(list(status = "error", reason = "desserte_no_foretaccess"))
  }
  if (is.null(aoi_path) || !file.exists(aoi_path)) {
    return(list(status = "error", reason = "desserte_need_project"))
  }
  parcelles <- tryCatch(sf::st_read(aoi_path, quiet = TRUE), error = function(e) NULL)
  if (is.null(parcelles) || !inherits(parcelles, "sf") || nrow(parcelles) == 0L) {
    return(list(status = "error", reason = "desserte_need_project"))
  }
  engine <- intersect(engine, DESSERTE_ENGINES)[1]
  if (is.na(engine) || length(engine) == 0L) {
    return(list(status = "error", reason = "desserte_need_engine"))
  }

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # Tout le pipeline travaille en Lambert-93 (EPSG:2154) : preprocess() valide
  # l'égalité stricte des CRS (MNT vs vecteurs).
  epsg <- 2154L
  parcelles <- tryCatch(sf::st_transform(parcelles, epsg), error = function(e) parcelles)

  buffer_m <- suppressWarnings(as.numeric(buffer_m %||% 0))
  if (!is.finite(buffer_m) || buffer_m < 0) buffer_m <- 0
  aoi_ext <- parcelles
  if (buffer_m > 0) {
    aoi_ext <- tryCatch(sf::st_buffer(parcelles, buffer_m), error = function(e) parcelles)
  }
  acq_dir <- file.path(cache_dir, sprintf("emprise_%gm", buffer_m))
  dir.create(acq_dir, recursive = TRUE, showWarnings = FALSE)

  # 1. MNT 5 m HIGHRES (repli acquire_mnt), partagé avec l'accessibilité.
  mnt_path <- .acquire_mnt_highres(aoi_ext, res_m = 5, crs = epsg, cache_dir = acq_dir)
  if (is.null(mnt_path)) {
    mnt_path <- tryCatch(
      foretaccess::acquire_mnt(aoi_ext, res_m = 5, crs = epsg, cache_dir = acq_dir),
      error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  }
  if (inherits(mnt_path, "acc_err")) {
    return(list(status = "error", reason = "desserte_mnt_failed", detail = mnt_path$msg))
  }
  mnt <- tryCatch(terra::rast(mnt_path), error = function(e) NULL)
  if (is.null(mnt)) return(list(status = "error", reason = "desserte_mnt_failed"))

  # 2. Desserte EXISTANTE (réseau à raccorder) via IGN BD TOPO V3.
  desserte <- tryCatch(
    foretaccess::acquire_desserte(aoi_ext, crs = epsg, cache_dir = acq_dir),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(desserte, "acc_err")) {
    return(list(status = "error", reason = "desserte_desserte_failed", detail = desserte$msg))
  }
  if (!inherits(desserte, "sf") || nrow(desserte) == 0L) {
    return(list(status = "error", reason = "desserte_desserte_empty"))
  }

  # 3. Masque forêt (IGN BD Forêt V2 ∩ emprise), repli géométrie projet.
  foret_bd <- tryCatch(
    foretaccess::acquire_foret(aoi_ext, crs = epsg, cache_dir = acq_dir),
    error = function(e) NULL)
  foret_mask <- if (inherits(foret_bd, "sf") && nrow(foret_bd) > 0L) foret_bd else parcelles

  # 4. Prétraitement commun (pente, franchissabilité, rasterisation).
  pre <- tryCatch(
    foretaccess::preprocess(mnt = mnt, desserte = desserte, foret = foret_mask),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(pre, "acc_err")) {
    return(list(status = "error", reason = "desserte_preprocess_failed", detail = pre$msg))
  }

  # 5. Surface de coût de construction (base + surcharge de pente ; couches eau/
  # sol optionnelles laissées à NULL en v1 — cf. plan de dev).
  cout <- tryCatch(
    foretaccess::surface_cout_construction(pre),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(cout, "acc_err")) {
    return(list(status = "error", reason = "desserte_cout_failed", detail = cout$msg))
  }

  # 6. Moteur de création : GLOUTON (parcelles = AOI d'origine, réseau à
  # raccorder = desserte existante).
  res <- tryCatch(
    foretaccess::reseau_desserte(pre, cout, parcelles = parcelles,
                                 desserte_existante = desserte, mode = engine),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(res, "acc_err")) {
    return(list(status = "error", reason = "desserte_engine_failed", detail = res$msg))
  }

  # 7. Raster réseau -> disque. Normalisation défensive en masque 1 = route /
  # NA = reste : `is.na()` et `<=` sont des PRIMITIVES (dispatch S4 correct sans
  # import terra), contrairement à `%in%` (cf. fix hors_foret) — sûr côté worker.
  reseau_path <- file.path(cache_dir, paste0("reseau_", engine, ".tif"))
  net <- tryCatch(
    terra::ifel(is.na(res$reseau) | res$reseau <= 0, NA, 1L),
    error = function(e) res$reseau)
  ok <- tryCatch({ terra::writeRaster(net, reseau_path, overwrite = TRUE); TRUE },
                 error = function(e) FALSE)
  if (!isTRUE(ok)) {
    return(list(status = "error", reason = "desserte_engine_failed",
                detail = "raster write failed"))
  }

  # Scalaires de badge (non portés par le raster) -> sidecar RDS pour le cache.
  # `raccorde` (foretaccess >= 1.11) est le VRAI indicateur qualité : « toutes les
  # routes créées sont-elles rattachées ? ». `connexe` (une seule composante pour
  # existant ∪ créé) vaut presque toujours FALSE — dominé par la fragmentation du
  # réseau EXISTANT à la résolution de la grille, pas par un défaut du réseau créé.
  n_parcelles <- nrow(parcelles)
  n_desservies <- suppressWarnings(sum(as.logical(res$desservies), na.rm = TRUE))
  connexe <- isTRUE(res$connexe)
  raccorde <- if ("raccorde" %in% names(res)) isTRUE(res$raccorde) else NA
  cout_total <- suppressWarnings(as.numeric(res$cout))
  saveRDS(list(cout = cout_total, connexe = connexe, raccorde = raccorde,
               n_desservies = n_desservies, n_parcelles = n_parcelles),
          file.path(cache_dir, paste0("reseau_", engine, ".rds")))

  # 8. GeoPackage exportable : parcelles + desserte existante + réseau créé.
  gpkg_path <- file.path(cache_dir, "desserte.gpkg")
  unlink(gpkg_path)
  tryCatch({
    sf::st_write(sf::st_transform(parcelles, epsg), gpkg_path, layer = "parcelles",
                 quiet = TRUE, delete_dsn = TRUE)
    sf::st_write(sf::st_transform(desserte, epsg), gpkg_path, layer = "desserte_existante",
                 quiet = TRUE, append = TRUE)
    if (inherits(res$lignes, "sf") && nrow(res$lignes) > 0L) {
      sf::st_write(sf::st_transform(res$lignes, epsg), gpkg_path, layer = "reseau_cree",
                   quiet = TRUE, append = TRUE)
    }
  }, error = function(e) cli::cli_warn(
    "desserte GPKG write failed: {conditionMessage(e)}"))

  list(
    status = "success",
    engine = engine,
    reseau_path = reseau_path,
    gpkg_path = if (file.exists(gpkg_path)) gpkg_path else NULL,
    cout = cout_total,
    connexe = connexe,
    raccorde = raccorde,
    n_desservies = n_desservies,
    n_parcelles = n_parcelles)
}

#' Export the desserte GeoPackage produced by a run
#'
#' Copies the cached `desserte.gpkg` to the download target. Returns `TRUE` on
#' success, `FALSE` (best-effort) otherwise.
#'
#' @param result A `run_desserte()` result list.
#' @param file Destination path handed to the browser.
#' @return Invisibly `TRUE`/`FALSE`.
#' @noRd
export_desserte_geopackage <- function(result, file) {
  src <- tryCatch(result$gpkg_path, error = function(e) NULL)
  if (is.null(src) || !file.exists(src)) return(invisible(FALSE))
  invisible(isTRUE(tryCatch(file.copy(src, file, overwrite = TRUE),
                            error = function(e) FALSE)))
}

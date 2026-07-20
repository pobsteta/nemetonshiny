# ===========================================================================
# Service — Accessibilité forestière (ForêtAccess, onglet Terrain)
# ===========================================================================
#
# Adaptateur applicatif (non-Shiny) autour du paquet `foretaccess` (cartographie
# de l'accessibilité, réimplémentation de Sylvaccess — INRAE). Conformément aux
# règles 1/2 (aucune logique métier ici), ce fichier ne fait qu'orchestrer :
#   - résoudre l'AOI (géométrie du projet) et le MNT (déjà sur disque) ;
#   - acquérir la desserte (IGN BD TOPO V3) pour l'emprise ;
#   - appeler `foretaccess::preprocess()` + les moteurs terrestres exportés ;
#   - persister les rasters de classes + un GeoPackage vecteur exportable.
#
# Moteurs TERRESTRES (skidder, porteur, camion DFCI), en R pur, plus le moteur
# CÂBLE-MÂT (`potentiel_cable`, noyau Rust de foretaccess) : balayage 360°/pixel
# depuis les places de dépôt, très coûteux (plusieurs minutes à la dizaine de
# minutes selon l'emprise) — d'où son opt-in dans l'UI.
#
# Le calcul est LONG (rasterisation, focal, propagation least-cost) : il tourne
# dans un worker `future` (cf. mod_accessibility.R). Un `SpatRaster` terra n'est
# PAS sérialisable entre process (pointeur externe) : le worker ÉCRIT les
# rasters sur disque et ne renvoie que des CHEMINS + les data.frames de récap
# (sérialisables). Le process principal relit les `.tif` pour l'affichage.

#' Terrestrial accessibility engines exposed by the app
#'
#' The three pure-R engines from `foretaccess`.
#'
#' The cable-crane engine (`foretaccess::potentiel_cable()`, Rust core) is
#' deliberately still NOT exposed, because the app has no **landing sites**
#' layer (`departs`) to feed it. Measured on a 2.65 x 2.85 km AOI (Chastel-Nouvel,
#' 530x571 px at 5 m) with `departs = NULL`, i.e. the documented fallback onto the
#' whole road network:
#'
#' * `foretaccess` falls back to **10 681 departure cells** x 360 azimuths
#'   (`pas_angulaire_deg = 1`) ~ 3.8 M rays: the run did **not complete in one
#'   hour**, against ~2 s for `preprocess()` and seconds for each terrestrial
#'   engine;
#' * `potentiel_cable()` itself warns the result would be *"optimiste -- une piste
#'   n'accueille pas un cable-mat"*, i.e. knowingly wrong.
#'
#' Wiring it in is a one-liner (`cable = foretaccess::potentiel_cable` below, same
#' `f(pre)` signature and same `$accessibilite`/`$recap` shape as the others). The
#' blocker is upstream of the app: a places-de-depot layer must exist first —
#' deciding what qualifies as one is forest-management logic and belongs in
#' `foretaccess`/`nemeton`, not here (rule 1).
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
  path <- file.path(cache_dir, "mnt_highres.tif")
  if (file.exists(path) && !isTRUE(overwrite)) return(path)
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

#' Flag the DFCI source tronçons on a road network (with provenance)
#'
#' `camion_dfci()` needs starting points (the `dfci` flag on `desserte`, turned
#' into `dfci_source_mask` by `preprocess()`) or it aborts. Resolution order:
#' 1. **OSM `ref:FR:DFCI`** network via `foretaccess::acquire_dfci()` +
#'    `flag_dfci()` — the authoritative source (`"osm"`).
#' 2. **Geometric fallback** built into `flag_dfci()` when no OSM DFCI line is
#'    found (emprise / traversal / turn-around heuristics) — `"geometrique"`.
#' 3. **App heuristic** of last resort when nothing was flagged: forest
#'    roads/tracks (`classe` in route/piste) are treated as sources —
#'    `"heuristique"` (surfaced to the user as a warning badge).
#'
#' @param desserte Road network `sf` (carries a `classe` column).
#' @param aoi_ext The buffered AOI (for the OSM DFCI fetch).
#' @param epsg Target EPSG (2154).
#' @param cache_dir Cache directory for the OSM DFCI fetch.
#' @return `list(desserte = <sf with dfci flag>, source = "osm"|"geometrique"|"heuristique")`.
#' @noRd
.resolve_desserte_dfci <- function(desserte, aoi_ext, epsg, cache_dir) {
  dfci_l <- tryCatch(
    foretaccess::acquire_dfci(aoi_ext, crs = epsg, cache_dir = cache_dir),
    error = function(e) NULL)
  desserte <- tryCatch(
    foretaccess::flag_dfci(desserte, dfci_l),
    error = function(e) { desserte$dfci <- 0L; desserte })
  n_flag <- suppressWarnings(sum(as.numeric(desserte[["dfci"]]), na.rm = TRUE))
  if (is.finite(n_flag) && n_flag > 0) {
    src <- if (inherits(dfci_l, "sf") && nrow(dfci_l) > 0L) "osm" else "geometrique"
  } else {
    cl <- as.character(desserte[["classe"]])
    desserte$dfci <- as.integer(!is.na(cl) & cl %in% c("route", "piste"))
    src <- "heuristique"
  }
  list(desserte = desserte, source = src)
}

#' Run the accessibility engines for a project (worker-side)
#'
#' Heavy, self-contained function meant to run in a `future` worker. Acquires
#' the **IGN RGE ALTI 5 m** DEM (Sylvaccess-calibrated resolution) and the road
#' network (**IGN BD TOPO V3**) for the buffered AOI, derives the forest mask
#' from **IGN BD Forêt V2** clipped to that emprise, runs
#' `foretaccess::preprocess()` then the requested engines, writes
#' each engine's categorical class raster to
#' `cache/accessibility/acc_<engine>.tif`, and writes an exportable GeoPackage
#' (`foret` + `desserte` layers). Returns only serialisable data (paths + recap
#' data.frames), never terra/sf objects tied to this process.
#'
#' The DEM, road network and BD Forêt layer are cached under
#' `cache_dir/emprise_<m>m/` by `acquire_mnt()` / `acquire_desserte()` /
#' `acquire_foret()`: a second run with the same buffer reuses them (no
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
  # calibré). On privilégie la couche HAUTE RÉSOLUTION `…HIGHRES` (MNT 5 m
  # genuine, cf. .acquire_mnt_highres) car la couche standard utilisée par
  # `foretaccess::acquire_mnt` est plafonnée au zoom 11 (~25 m ré-échantillonné)
  # et produit un escalier -> pentes parasites -> artefacts « inexploitable ».
  # Repli sur acquire_mnt (couche standard) si le fetch HIGHRES échoue. Les deux
  # mettent en cache dans acq_dir : un 2e run réutilise.
  mnt_path <- .acquire_mnt_highres(aoi_ext, res_m = 5, crs = epsg, cache_dir = acq_dir)
  if (is.null(mnt_path)) {
    mnt_path <- tryCatch(
      foretaccess::acquire_mnt(aoi_ext, res_m = 5, crs = epsg, cache_dir = acq_dir),
      error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  }
  if (inherits(mnt_path, "acc_err")) {
    return(list(status = "error", reason = "accessibility_mnt_failed",
                detail = mnt_path$msg))
  }
  mnt <- tryCatch(terra::rast(mnt_path), error = function(e) NULL)
  if (is.null(mnt)) return(list(status = "error", reason = "accessibility_mnt_failed"))

  # 2. Desserte (réseau routier/pistes) via IGN BD TOPO V3 sur l'emprise
  # tamponnée (cachée).
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

  # Flag DFCI sur la desserte : sans lui, `camion_dfci()` s'arrête (« Aucune
  # desserte-source DFCI »), car `preprocess()` construit `dfci_source_mask` à
  # partir de la colonne `dfci` de la desserte. Source OSM `ref:FR:DFCI` en
  # priorité (foretaccess::acquire_dfci + flag_dfci), repli géométrique de
  # flag_dfci, puis heuristique app routes/pistes en dernier recours. La
  # provenance (`dfci_source`) est remontée pour signaler le cas heuristique à
  # l'utilisateur. Posé seulement si le moteur DFCI est demandé.
  dfci_source <- NA_character_
  if ("camion_dfci" %in% engines) {
    fl <- .resolve_desserte_dfci(desserte, aoi_ext, epsg, acq_dir)
    desserte <- fl$desserte
    dfci_source <- fl$source
  }

  # 3. Masque forêt = forêt réelle (IGN BD Forêt V2) restreinte à l'emprise
  # tamponnée, PAS la simple géométrie déclarée du projet. `acquire_foret`
  # clippe la BD Forêt sur `aoi_ext` par st_intersection : le résultat est
  # exactement (emprise projet + buffer) ∩ forêt BD Forêt V2. Ainsi l'analyse
  # couvre toute la forêt accessible dans le tampon (y compris hors parcelles du
  # projet) et ne peint pas en « forêt » des zones non boisées. Repli sur la
  # géométrie projet si la BD Forêt est indisponible ou vide sur l'emprise.
  foret_bd <- tryCatch(
    foretaccess::acquire_foret(aoi_ext, crs = epsg, cache_dir = acq_dir),
    error = function(e) NULL)
  foret_mask <- if (inherits(foret_bd, "sf") && nrow(foret_bd) > 0L) foret_bd else aoi

  # 4. Prétraitement commun (pente, exposition, masques, rasterisation).
  pre <- tryCatch(
    foretaccess::preprocess(mnt = mnt, desserte = desserte, foret = foret_mask),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(pre, "acc_err")) {
    return(list(status = "error", reason = "accessibility_preprocess_failed",
                detail = pre$msg))
  }

  # 5. Moteurs terrestres : raster de classes -> disque ; recap -> mémoire.
  # `cable = foretaccess::potentiel_cable` s'ajouterait ici tel quel (même
  # signature, même forme de retour) une fois la couche de places de dépôt
  # disponible — cf. ACCESSIBILITY_ENGINES.
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

  # 6. GeoPackage exportable : masque forêt (BD Forêt V2 ∩ emprise) + desserte.
  # Best-effort.
  gpkg_path <- file.path(cache_dir, "accessibilite.gpkg")
  unlink(gpkg_path)
  tryCatch({
    sf::st_write(sf::st_transform(foret_mask, 2154), gpkg_path, layer = "foret",
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
    n_desserte = nrow(desserte),
    dfci_source = dfci_source)
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

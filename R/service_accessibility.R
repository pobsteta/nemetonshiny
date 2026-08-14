# ===========================================================================
# Service - Accessibilite forestiere (ForetAccess, onglet Terrain)
# ===========================================================================
#
# Adaptateur applicatif (non-Shiny) autour du paquet `foretaccess` (cartographie
# de l'accessibilite, reimplementation de Sylvaccess - INRAE). Conformement aux
# regles 1/2 (aucune logique metier ici), ce fichier ne fait qu'orchestrer :
#   - resoudre l'AOI (geometrie du projet) et le MNT (deja sur disque) ;
#   - acquerir la desserte (IGN BD TOPO V3) pour l'emprise ;
#   - appeler `foretaccess::preprocess()` + les moteurs terrestres exportes ;
#   - persister les rasters de classes + un GeoPackage vecteur exportable.
#
# Moteurs TERRESTRES (skidder, porteur, camion DFCI), en R pur, plus le moteur
# CABLE-MAT (`potentiel_cable`, noyau Rust de foretaccess) : balayage 360deg/pixel
# depuis les places de depot, tres couteux (plusieurs minutes a la dizaine de
# minutes selon l'emprise) - d'ou son opt-in dans l'UI.
#
# Le calcul est LONG (rasterisation, focal, propagation least-cost) : il tourne
# dans un worker `future` (cf. mod_accessibility.R). Un `SpatRaster` terra n'est
# PAS serialisable entre process (pointeur externe) : le worker ECRIT les
# rasters sur disque et ne renvoie que des CHEMINS + les data.frames de recap
# (serialisables). Le process principal relit les `.tif` pour l'affichage.

#' Accessibility engines exposed by the app
#'
#' The three pure-R terrestrial engines + the **cable-crane** engine
#' (`foretaccess::potentiel_cable()`, Rust core), unblocked by foretaccess 1.19.0.
#'
#' The cable was long excluded for lack of a **landing-sites** layer (`departs`):
#' `potentiel_cable(departs = NULL)` fell back onto the whole road network
#' (~10 681 cells x 360 azimuths, > 1 h, knowingly optimistic). foretaccess now
#' produces that layer: `qualifier_desserte()` (NDP 1, LiDAR width) ->
#' `places_depot()` (selective) -> `departs` fed to `potentiel_cable()`. Without a
#' LiDAR point cloud the app falls back to `places_depot()` on the raw BD TOPO
#' (less selective, but correct). See `run_accessibility()`.
#' @noRd
ACCESSIBILITY_ENGINES <- c("skidder", "porteur", "camion_dfci", "cable")

# NOTE : `.resolve_project_aoi_2154()` (resolution AOI projet -> EPSG:2154) et
# `.acquire_mnt_highres()` (MNT 5 m HIGHRES) vivent desormais dans
# `R/service_foretaccess_io.R`, partages avec le futur onglet Desserte.

#' Directory holding the accessibility artefacts of a project
#' @noRd
.accessibility_cache_dir <- function(project_path) {
  file.path(project_path, "cache", "accessibility")
}

#' Path to a project's accessibility-run GeoPackage (may not exist yet)
#'
#' Single source of the run's vector layers (`foret`, `desserte`, `places_depot`).
#' Used by both the Accessibility and Desserte maps to overlay the landing points.
#' @noRd
.accessibility_gpkg_path <- function(project_path) {
  if (is.null(project_path) || !nzchar(project_path)) return(NULL)
  file.path(.accessibility_cache_dir(project_path), "accessibilite.gpkg")
}

#' Path to a project's LiDAR-corrected road network (NDP 1), written by
#' `run_desserte_lidar_correction()` and consumed by `run_accessibility()`
#'
#' Decoupling the (heavy, ~2-3 h) LiDAR correction from the (light) engine runs:
#' the correction persists the qualified desserte here ONCE; engine runs then reuse
#' it without re-qualifying. `cache_dir` is the accessibility cache directory.
#' @noRd
.corrected_desserte_path <- function(cache_dir) {
  file.path(cache_dir, "desserte_corrigee.gpkg")
}

#' Read the `places_depot` layer (landing points) from a run GeoPackage, in WGS84
#'
#' Returns the landing points as an sf in EPSG:4326 (ready for Leaflet), or NULL
#' when the GeoPackage is missing, has no `places_depot` layer (NDP 0 / no cable
#' run), or the layer is empty.
#' @noRd
.acc_read_places_depot <- function(gpkg_path) {
  if (is.null(gpkg_path) || !file.exists(gpkg_path)) return(NULL)
  layers <- tryCatch(sf::st_layers(gpkg_path)$name, error = function(e) character(0))
  if (!("places_depot" %in% layers)) return(NULL)
  pd <- tryCatch(sf::st_read(gpkg_path, layer = "places_depot", quiet = TRUE),
                 error = function(e) NULL)
  if (!inherits(pd, "sf") || nrow(pd) == 0L) return(NULL)
  tryCatch(sf::st_transform(pd, 4326), error = function(e) pd)
}

#' Read one layer of the corrected-desserte GeoPackage, in WGS84
#'
#' Shared by the swipe comparator overlays. Returns the layer as an sf in
#' EPSG:4326 (ready for Leaflet), or NULL when the file/layer is missing or empty.
#'
#' @param corrected_path Path to `desserte_corrigee.gpkg`, or NULL.
#' @param layer One of `"desserte_origine"` / `"desserte_corrigee"`.
#' @noRd
.acc_read_desserte_layer <- function(corrected_path, layer) {
  if (is.null(corrected_path) || !file.exists(corrected_path)) return(NULL)
  layers <- tryCatch(sf::st_layers(corrected_path)$name,
                     error = function(e) character(0))
  if (!(layer %in% layers)) return(NULL)
  d <- tryCatch(sf::st_read(corrected_path, layer = layer, quiet = TRUE),
                error = function(e) NULL)
  if (!inherits(d, "sf") || nrow(d) == 0L) return(NULL)
  tryCatch(sf::st_transform(d, 4326), error = function(e) d)
}

#' Locate the DEM to feed the RVT relief background
#'
#' Preference order - **best terrain source first** :
#'   1. `cache/layers/lidar_mnt_mosaic.tif` - the **native 0.5 m IGN LiDAR HD
#'      DTM** (bare ground). A CVAT on it is striping-free and reveals the true
#'      micro-relief (road embankments, ditches). This is the right source.
#'   2. `cache/accessibility/emprise_<b>m/mnt_highres_1m.tif` - the WMS RGE ALTI
#'      HIGHRES 1 m (resampled, carries a tile striping the CVAT amplifies).
#'      Fallback only, for projects without a LiDAR DTM.
#'   3. any `mnt_highres_*.tif`.
#'
#' NB: `lidar_mnh_mosaic.tif` is a canopy height model (MNH), NOT terrain -
#' deliberately excluded (a relief RVT must run on bare ground).
#'
#' @param project_path Project directory, or NULL.
#' @return Path to a DEM GeoTIFF, or NULL.
#' @noRd
.acc_rvt_mnt_path <- function(project_path) {
  if (is.null(project_path) || !nzchar(project_path)) return(NULL)
  # 1. MNT LiDAR HD 0.5 m natif (meilleure source).
  lidar <- file.path(project_path, "cache", "layers", "lidar_mnt_mosaic.tif")
  if (file.exists(lidar)) return(lidar)
  # 2/3. Repli WMS.
  acc <- .accessibility_cache_dir(project_path)
  if (!dir.exists(acc)) return(NULL)
  cand <- list.files(acc, pattern = "^mnt_highres_1m\\.tif$",
                     recursive = TRUE, full.names = TRUE)
  if (length(cand) == 0L) {
    cand <- list.files(acc, pattern = "^mnt_highres.*\\.tif$",
                       recursive = TRUE, full.names = TRUE)
  }
  if (length(cand) == 0L) return(NULL)
  cand[order(file.mtime(cand), decreasing = TRUE)][1]
}

#' Ready-to-paint CVAT relief overlay for a project's maps
#'
#' Returns the project's CVAT relief as a display-ready `SpatRaster` (aggregated
#' to ~2000 px so `addRasterImage` stays under its size cap), **only when a CVAT
#' already exists** (precomputed or cached - `.rvt_is_cheap`). Never triggers the
#' ~1 min live `vat_combined()` at map-render time. `NULL` when no CVAT is ready.
#'
#' Used as a semi-transparent overlay over OSM/Satellite on the Accessibility and
#' Desserte maps (the relief covers only the LiDAR emprise; NA elsewhere ->
#' transparent, so OSM shows through outside it).
#'
#' @param project_path Project directory, or NULL.
#' @return An aggregated `SpatRaster` in `[0, 1]`, or NULL.
#' @noRd
.acc_cvat_overlay_raster <- function(project_path) {
  if (is.null(project_path) || !nzchar(project_path)) return(NULL)
  mnt <- .acc_rvt_mnt_path(project_path)
  if (is.null(mnt) || !.rvt_is_cheap(mnt)) return(NULL)
  p <- tryCatch(generate_rvt(mnt), error = function(e) NULL)   # cheap -> rapide
  if (is.null(p) || !file.exists(p)) return(NULL)
  rr <- tryCatch(terra::rast(p), error = function(e) NULL)
  if (is.null(rr)) return(NULL)
  maxdim <- max(terra::nrow(rr), terra::ncol(rr))
  if (is.finite(maxdim) && maxdim > 2000L) {
    fact <- as.integer(ceiling(maxdim / 2000))
    rr <- tryCatch(terra::aggregate(rr, fact = fact, fun = "mean", na.rm = TRUE),
                   error = function(e) rr)
  }
  rr
}

#' Reconstruct a run result from a project's cached accessibility rasters
#'
#' Lets the tab show a **previously computed** analysis without recomputing:
#' scans `cache/accessibility/` for the known class rasters (`acc_<engine>.tif`
#' and `acc_classes_debardage.tif`) and rebuilds a minimal `run_accessibility()`
#' result (same shape as a live run, marked `from_cache = TRUE`). The per-engine
#' recap tables are NOT persisted, so `recaps` is empty - only the display layers
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
  # Engines d'abord (dans l'ordre canonique), puis les classes de debardage :
  # le 1er element devient la couche selectionnee par defaut dans l'UI.
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
  # Raster ACCESSFOR (IGN) deja calcule et ecrit lors du run (validation
  # systematique) : s'il est sur disque, on le restaure pour que la couche
  # " Classes de debardage/ACCESSFOR (IGN) " reaffiche le volet sans requete WFS.
  af <- file.path(cache_dir, "accessfor_skidder.tif")
  list(
    status = "success",
    engines = engines,
    recaps = list(),
    raster_paths = raster_paths,
    gpkg_path = if (file.exists(gpkg)) gpkg else NULL,
    n_desserte = NA_integer_,
    accessfor_raster_path = if (file.exists(af)) af else NULL,
    from_cache = TRUE)
}

#' Flag the DFCI source troncons on a road network (with provenance)
#'
#' `camion_dfci()` needs starting points (the `dfci` flag on `desserte`, turned
#' into `dfci_source_mask` by `preprocess()`) or it aborts. Resolution order:
#' 1. **OSM `ref:FR:DFCI`** network via `foretaccess::acquire_dfci()` +
#'    `flag_dfci()` - the authoritative source (`"osm"`).
#' 2. **Geometric fallback** built into `flag_dfci()` when no OSM DFCI line is
#'    found (emprise / traversal / turn-around heuristics) - `"geometrique"`.
#' 3. **App heuristic** of last resort when nothing was flagged: forest
#'    roads/tracks (`classe` in route/piste) are treated as sources -
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

#' Acquire the DEM + raw road network for an accessibility AOI (shared preamble)
#'
#' Factored out of `run_accessibility()` so the LiDAR correction step reuses the
#' exact same acquisition (buffered emprise, HIGHRES DEM with fallback, BD TOPO
#' desserte, per-buffer sub-cache). Returns `list(status = "ok", aoi, aoi_ext,
#' epsg, acq_dir, mnt, desserte)` or a structured error list.
#' @noRd
# --- Garde-fou memoire de la correction LiDAR --------------------------------
#
# `foretaccess::qualifier_desserte()` mesure les largeurs troncon par troncon en
# exploitant correctement le `LAScatalog` (hors memoire, avec filtre de
# couverture depuis 1.19.1). MAIS `.mnt_alsroads()` (desserte_lidar.R), quand le
# MNT fourni depasse 1,5 m, derive un MNT a 1 m par
# `readLAS(ctg$filename, filter = "-keep_class 2")` : le vecteur COMPLET des
# dalles, donc tout le nuage sol en memoire d'un coup, puis une triangulation
# (`rasterize_terrain(tin())`) par-dessus. Le catalogue est court-circuite, alors
# que `rasterize_terrain()` sait travailler directement sur un `LAScatalog`.
#
# Mesure sur le projet ForetAccess : 4 dalles LiDAR HD = 165,5 M de points
# (908 Mo compresses) -> worker a 16,8 Go en 15 min, puis OOM machine (RStudio et
# navigateur emportes). Le chemin normal est d'eviter la derivation (MNT <= 1,5 m
# fourni) ; ce garde-fou n'est que le filet quand on n'y arrive pas.

#' Estimate the peak memory of the ALSroads DTM derivation for a point cloud
#'
#' Ground points (ASPRS class 2) are typically ~20-35 % of an IGN LiDAR HD tile;
#' a `lidR` point costs ~60 bytes with its attributes, and the Delaunay
#' triangulation of `rasterize_terrain(tin())` roughly doubles that. Deliberately
#' coarse - it only has to tell " comfortable " from " this will kill the
#' machine ".
#'
#' @param laz_dir Directory of `.laz`/`.copc.laz` tiles, or a `LAScatalog`.
#' @param ground_frac Assumed share of ground points.
#' @param bytes_per_point Assumed in-memory cost per point.
#' @param tin_factor Multiplier covering the triangulation.
#' @return A list: `points`, `bytes`, `available`, `ok`.
#' @noRd
.lidar_memory_estimate <- function(laz_dir, ground_frac = 0.30,
                                   bytes_per_point = 60, tin_factor = 2) {
  if (!requireNamespace("lidR", quietly = TRUE)) return(NULL)
  ctg <- tryCatch(
    if (inherits(laz_dir, "LAScatalog")) laz_dir else lidR::readLAScatalog(laz_dir),
    error = function(e) NULL)
  if (is.null(ctg)) return(NULL)
  n <- tryCatch(sum(as.numeric(ctg$Number.of.point.records), na.rm = TRUE),
                error = function(e) NA_real_)
  if (!is.finite(n) || n <= 0) return(NULL)
  list(points = n, bytes = n * ground_frac * bytes_per_point * tin_factor)
}

#' Pre-flight memory check for the ALSroads DTM derivation
#'
#' Bypass with `NEMETON_LIDAR_SKIP_GUARD=1`.
#'
#' @param laz_dir Directory of tiles, or a `LAScatalog`.
#' @param frac Fraction of available RAM the derivation may claim.
#' @return A list: `ok`, `points`, `bytes`, `available`.
#' @noRd
.lidar_memory_check <- function(laz_dir, frac = 0.8) {
  est <- .lidar_memory_estimate(laz_dir)
  avail <- .available_memory_bytes()
  skip <- tolower(Sys.getenv("NEMETON_LIDAR_SKIP_GUARD", "")) %in%
    c("1", "true", "yes", "oui")
  if (is.null(est)) {
    return(list(ok = TRUE, points = NA_real_, bytes = NA_real_, available = avail))
  }
  ok <- skip || !is.finite(avail) || est$bytes <= avail * frac
  list(ok = isTRUE(ok), points = est$points, bytes = est$bytes, available = avail)
}

.acquire_mnt_desserte <- function(aoi_path, cache_dir, buffer_m = 0,
                                  res_m = 5) {
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
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  epsg <- 2154L
  aoi <- tryCatch(sf::st_transform(aoi, epsg), error = function(e) aoi)
  buffer_m <- suppressWarnings(as.numeric(buffer_m %||% 0))
  if (!is.finite(buffer_m) || buffer_m < 0) buffer_m <- 0
  aoi_ext <- aoi
  if (buffer_m > 0) {
    aoi_ext <- tryCatch(sf::st_buffer(aoi, buffer_m), error = function(e) aoi)
  }
  acq_dir <- file.path(cache_dir, sprintf("emprise_%gm", buffer_m))
  dir.create(acq_dir, recursive = TRUE, showWarnings = FALSE)

  res_m <- suppressWarnings(as.numeric(res_m))
  if (!is.finite(res_m) || res_m <= 0) res_m <- 5
  mnt_path <- .acquire_mnt_highres(aoi_ext, res_m = res_m, crs = epsg,
                                   cache_dir = acq_dir)
  if (is.null(mnt_path)) {
    mnt_path <- tryCatch(
      foretaccess::acquire_mnt(aoi_ext, res_m = res_m, crs = epsg,
                               cache_dir = acq_dir),
      error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  }
  if (inherits(mnt_path, "acc_err")) {
    return(list(status = "error", reason = "accessibility_mnt_failed",
                detail = mnt_path$msg))
  }
  mnt <- tryCatch(terra::rast(mnt_path), error = function(e) NULL)
  if (is.null(mnt)) return(list(status = "error", reason = "accessibility_mnt_failed"))

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
  list(status = "ok", aoi = aoi, aoi_ext = aoi_ext, epsg = epsg,
       acq_dir = acq_dir, mnt = mnt, desserte = desserte)
}

#' Map an OSM `highway` value onto the BD TOPO `classe` vocabulary
#'
#' `preprocess()`/`.rasteriser_desserte` require `classe`, which
#' `acquire_desserte_osm()` does not produce (it returns `highway`, plus
#' `tracktype`/`surface`/`access`). The default OSM types are `track`,
#' `unclassified` and `service`.
#'
#' Unknown values fall back to `piste`, the LOWER-grade class: an OSM segment we
#' cannot categorise must not enter the engines as a full-fledged road.
#'
#' @param highway Character vector of OSM `highway` values.
#' @return Character vector of BD TOPO classes.
#' @noRd
.osm_highway_vers_classe <- function(highway) {
  h <- tolower(as.character(highway %||% character()))
  out <- rep("piste", length(h))
  out[h %in% c("unclassified", "service", "residential")] <- "route"
  out
}

#' Complete the BD TOPO network with the OSM segments it lacks
#'
#' INVARIANT: the corrected network ALWAYS carries the whole declared BD TOPO.
#' OSM is a **complement, never a substitute** - the core states the same rule
#' (`acquire_desserte_osm()`: " Source complementaire de la BD TOPO, jamais
#' substitutive "). This helper can therefore only ever ADD rows.
#'
#' An OSM segment is kept for the part of it lying OUTSIDE a `corridor_m` buffer
#' around the BD TOPO, and only when that part reaches `min_ajout_m`. Without the
#' length floor, a mere digitising offset on an already-declared road would enter
#' as a duplicate; without the clipping, its overlapping half would too.
#'
#' Best-effort by construction: Overpass is rate-limited (measured > 10 min under
#' throttling), and an unreachable third-party service must never block a
#' correction. Every failure path returns the BD TOPO untouched, with a `statut`
#' naming the cause so the UI can say what happened instead of staying silent.
#'
#' @param bdtopo BD TOPO network (`acquire_desserte()` output).
#' @param aoi_ext Buffered AOI.
#' @param acq_dir Acquisition cache directory.
#' @param corridor_m Half-width (m) of the corridor deeming an OSM segment
#'   already declared by the BD TOPO.
#' @param min_ajout_m Minimum outside length (m) for a segment to be added.
#' @return `list(reseau, n_bdtopo, n_ajoutes, n_osm, statut)`.
#' @noRd
.desserte_complement_osm <- function(bdtopo, aoi_ext, acq_dir,
                                     corridor_m = 15, min_ajout_m = 30) {
  base <- bdtopo
  base$source <- "bdtopo"
  seule <- function(statut) {
    list(reseau = base, n_bdtopo = nrow(base), n_ajoutes = 0L,
         n_osm = 0L, statut = statut)
  }
  if (!requireNamespace("foretaccess", quietly = TRUE)) return(seule("osm_indisponible"))

  # Injoignable et vide ne sont PAS le meme diagnostic : le premier est une
  # panne reseau (bride Overpass, mesuree > 10 min), le second un constat.
  osm <- tryCatch(
    foretaccess::acquire_desserte_osm(aoi_ext, crs = 2154, cache_dir = acq_dir),
    error = function(e) structure(list(), class = "acc_err"))
  if (inherits(osm, "acc_err")) return(seule("osm_injoignable"))
  if (!inherits(osm, "sf") || nrow(osm) == 0L) return(seule("osm_vide"))

  ajout <- tryCatch({
    osm <- sf::st_transform(osm, sf::st_crs(base))
    corridor <- sf::st_union(sf::st_buffer(sf::st_geometry(base), corridor_m))
    # `st_difference` CLIPPE : on n'ajoute que la portion hors corridor, pas le
    # troncon entier - sa moitie deja declaree ferait doublon avec la BD TOPO.
    hors <- suppressWarnings(sf::st_difference(osm, corridor))
    if (nrow(hors) == 0L) return(seule("osm_rien_a_ajouter"))
    hors <- suppressWarnings(sf::st_cast(hors, "MULTILINESTRING"))
    assez <- as.numeric(sf::st_length(hors)) >= min_ajout_m
    hors <- hors[which(assez), , drop = FALSE]
    if (nrow(hors) == 0L) return(seule("osm_rien_a_ajouter"))
    a <- sf::st_sf(classe = .osm_highway_vers_classe(hors[["highway"]]),
                   largeur = NA_real_, source = "osm",
                   geometry = sf::st_geometry(hors))
    # `rbind.sf` exige le MEME nom de colonne geometrique des deux cotes.
    gcol <- attr(base, "sf_column")
    if (!identical(gcol, "geometry")) {
      names(a)[names(a) == "geometry"] <- gcol
      attr(a, "sf_column") <- gcol
    }
    a
  }, error = function(e) NULL)
  # NB : les `return(seule(...))` du bloc ci-dessus sortent bien de CETTE
  # fonction - l'expression d'un `tryCatch()` s'evalue dans le frame appelant.
  if (is.null(ajout) || !inherits(ajout, "sf")) return(seule("osm_fusion_echouee"))

  garder <- intersect(names(base), names(ajout))
  fusion <- tryCatch(
    rbind(base[, garder, drop = FALSE], ajout[, garder, drop = FALSE]),
    error = function(e) NULL)
  if (!inherits(fusion, "sf") || nrow(fusion) < nrow(base)) {
    return(seule("osm_fusion_echouee"))
  }
  list(reseau = fusion, n_bdtopo = nrow(base), n_ajoutes = nrow(ajout),
       n_osm = nrow(osm), statut = "ok")
}

#' Correct a project's road network with LiDAR HD (NDP 1) - standalone step
#'
#' The HEAVY part (`qualifier_desserte()` : re-aligned geometry, measured widths)
#' is run ON ITS OWN, decoupled from the engine runs, and
#' the corrected desserte is persisted to `desserte_corrigee.gpkg`. Engine runs
#' then reuse it (via `run_accessibility(use_corrected_desserte = TRUE)`) with NO
#' re-qualification - keeping them light. Requires a LiDAR point cloud + foretaccess
#' >= 1.19.1 (the version that fixed the qualification segfault). Best-effort and
#' structured (returns `list(status = "error", reason = ...)` instead of throwing).
#'
#' @param aoi_path Path to the AOI GeoPackage (written by the app before invoke).
#' @param cache_dir Accessibility cache directory of the project.
#' @param buffer_m Buffer (m) around the forest AOI - MUST match the engine run.
#' @param project_path Project root (to resolve the LiDAR point-cloud cache).
#' @return `list(status, n_troncons, n_bdtopo, n_osm_ajoutes, osm_statut,
#'   corrected_path)` on success, or a structured error list.
#' @noRd
run_desserte_lidar_correction <- function(aoi_path, cache_dir, buffer_m = 0,
                                          project_path = NULL) {
  laz_dir <- if (!is.null(project_path)) {
    file.path(project_path, "cache", "layers", "lidar_nuage")
  } else {
    file.path(dirname(cache_dir), "layers", "lidar_nuage")
  }
  has_laz <- dir.exists(laz_dir) &&
    length(list.files(laz_dir, pattern = "\\.(copc\\.)?laz$")) > 0L
  if (!has_laz) return(list(status = "error", reason = "acc_correct_no_lidar"))
  # Deux gardes retires ici, tous deux devenus faux :
  #
  # 1. `packageVersion("foretaccess") >= "1.19.1"` - `Imports:` impose >= 2.0.1,
  #    la condition ne pouvait plus echouer.
  # 2. `requireNamespace("lidR") && requireNamespace("ALSroads")` - c'etait un
  #    FAUX REFUS depuis `foretaccess` 1.27.0, qui a retire le moteur ALSroads au
  #    profit de dessertR : son NEWS dit " ALSroads et lidR ne sont plus utilises
  #    du tout ". Le garde refusait donc la correction LiDAR sur toute machine ne
  #    les ayant pas installes, alors que le coeur n'en a plus besoin. Il ne
  #    passait ici que parce que les deux paquets trainaient encore sur le poste
  #    de dev.

  # MNT pour le recalage ALSroads. `foretaccess` (`.mnt_alsroads`,
  # desserte_lidar.R) renvoie le MNT fourni tel quel des qu'il est <= 1,5 m ;
  # au-dela il en DERIVE un en lisant TOUTES les dalles d'un coup
  # (`readLAS(ctg$filename, ...)`) - 165,5 M de points -> OOM (16,8 Go). On fournit
  # donc toujours un MNT <= 1,5 m (aucun point lu).
  #
  # PREFERENCE : le MNT LiDAR HD 0,5 m NATIF (`lidar_mnt_mosaic.tif`) quand il
  # existe, plutot que le WMS RGE ALTI 1 m. Trois gains : (1) recalage ALSroads
  # bien plus precis (0,5 m natif vs 1 m reechantillonne, strie) ; (2) COHERENCE
  # avec le fond relief RVT du comparateur, calcule sur le meme MNT ; (3) toujours
  # pas d'OOM (0,5 m <= 1,5 m -> pas de derivation). Repli WMS 1 m sinon.
  acq <- .acquire_mnt_desserte(aoi_path, cache_dir, buffer_m, res_m = 1)
  if (!identical(acq$status, "ok")) return(acq)

  lidar_mnt_path <- file.path(project_path %||% dirname(cache_dir),
                              "cache", "layers", "lidar_mnt_mosaic.tif")
  use_lidar_mnt <- !is.null(project_path) && file.exists(lidar_mnt_path)
  mnt_alsroads <- if (use_lidar_mnt) {
    tryCatch(terra::rast(lidar_mnt_path), error = function(e) acq$mnt)
  } else acq$mnt

  # Filet : si le MNT retenu reste trop grossier (> 1,5 m - WMS degrade, repli
  # acquire_mnt, ou LiDAR absent), la derivation se declenchera cote foretaccess.
  # On estime alors son cout depuis le nuage et on refuse plutot que de partir en
  # OOM.
  if (max(terra::res(mnt_alsroads)) > 1.5) {
    chk <- .lidar_memory_check(laz_dir)
    if (!isTRUE(chk$ok)) {
      return(list(status = "error", reason = "acc_correct_memory_guard",
                  detail = sprintf(
                    "MNT a %.1f m (> 1,5 m) : foretaccess derivera un MNT en lisant %s points ; pic estime %.1f Go, RAM disponible %.1f Go",
                    max(terra::res(mnt_alsroads)),
                    format(chk$points, big.mark = " "),
                    chk$bytes / 1024^3, chk$available / 1024^3)))
    }
  }

  # Cache PERSISTANT des mesures par troncon. `qualifier_desserte()` sans
  # `cache_dir` ecrit son `desserte_lidar.rds` dans `tempdir()`, VOLATILE : chaque
  # correction repayait ~4-5 h de mesure ALSroads. foretaccess memoise par troncon
  # (cle = WKT de la LINESTRING, stable pour une emprise donnee, meme les echecs)
  # et relit ce fichier au demarrage : en le posant dans le cache d'emprise, une
  # relance retrouve les troncons deja mesures et ne rappelle `measure_road` que
  # sur les nouveaux - la 2e correction d'un meme projet est quasi immediate.
  # Sous-repertoire DEDIE (pas `acq_dir` directement) : le cache foretaccess est
  # keye par WKT SANS versionner le DTM ni les parametres. Un `desserte_lidar.rds`
  # trainant a la racine de `acq_dir` (run anterieur, DTM potentiellement
  # different - p.ex. le `dtm_alsroads` derive au lieu du MNT a 1 m) serait
  # reutilise a tort et rendrait des largeurs incoherentes. On isole donc le cache
  # de CE chemin (MNT a 1 m). Invalidation naturelle si l'emprise change (acq_dir
  # suit le buffer) ou si les troncons changent (nouveaux WKT).
  # Cache DEDIE a la source du MNT : le cache foretaccess est keye par WKT sans
  # versionner le DTM. Reutiliser un cache fait avec un autre MNT rendrait des
  # largeurs incoherentes -> un sous-repertoire par source (`_lidar` vs `_wms`).
  qualif_cache <- file.path(acq$acq_dir,
                            if (use_lidar_mnt) "qualif_cache_lidar" else "qualif_cache")
  dir.create(qualif_cache, recursive = TRUE, showWarnings = FALSE)
  # RESEAU A QUALIFIER = BD TOPO INTEGRALE + ce qu'OSM porte en plus.
  cplt <- .desserte_complement_osm(acq$desserte, acq$aoi_ext, acq$acq_dir)
  reseau <- cplt$reseau

  # INVARIANT - `retirer_disparues = FALSE`, le defaut du coeur.
  #
  # Nous passions ici `TRUE` : la correction RETIRAIT les troncons dont l'etat
  # mesure vaut `abandonnee` ou `hors_route`. Mesure sur ForetAccess, elle
  # supprimait 280 troncons sur 373 - 84 % du lineaire, dont UNE `route` sur
  # DEUX. Ce n'etait pas un constat de terrain : `hors_route` signifie " les deux
  # conductivites faibles ", c'est-a-dire AUCUN signal, ce qui designe un echec
  # de mesure bien plus souvent qu'une route effacee - une plateforme routiere
  # laisse une empreinte dans le terrain pendant des decennies. `dsr_etat()`
  # avertit d'ailleurs que l'etat " n'est reellement interpretable que le long
  # d'un trace retenu par le pathfinder ".
  #
  # La regle, desormais, ne se contourne pas : la desserte corrigee CONSERVE
  # l'integralite de la BD TOPO, s'enrichit d'OSM, qualifie l'ensemble et le
  # rend. La qualification RENSEIGNE (etat, largeur, geometrie recalee) ; elle
  # ne DECIDE pas de l'existence. Un troncon juge abandonne reste dans la
  # couche, porteur de son etat, et c'est l'utilisateur qui en tire les
  # consequences.
  dq <- tryCatch(
    foretaccess::qualifier_desserte(reseau, las_source = laz_dir,
                                    mnt = mnt_alsroads, cache_dir = qualif_cache,
                                    retirer_disparues = FALSE),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(dq, "acc_err")) {
    return(list(status = "error", reason = "acc_correct_failed", detail = dq$msg))
  }
  # Garde-fou : `preprocess()`/`.rasteriser_desserte` exigent la colonne `classe`.
  if (!inherits(dq, "sf") || nrow(dq) == 0L || !("classe" %in% names(dq))) {
    return(list(status = "error", reason = "acc_correct_attrs_lost"))
  }
  # Garde-fou d'INVARIANT : perdre un troncon declare est une erreur, pas un
  # resultat. Mieux vaut refuser la correction que rendre un reseau ampute qui
  # se lira comme une desserte complete - c'est exactement le mode d'echec que
  # ce correctif supprime.
  if (nrow(dq) < nrow(reseau)) {
    return(list(status = "error", reason = "acc_correct_invariant_broken",
                detail = sprintf("%d troncons en entree, %d en sortie",
                                 nrow(reseau), nrow(dq))))
  }
  # `qualifier_desserte()` ne s'engage pas a transporter nos colonnes : on
  # reattache `source` par position, et seulement si la correspondance est sure.
  if (!("source" %in% names(dq)) && nrow(dq) == nrow(reseau)) {
    dq$source <- reseau$source
  }

  out <- .corrected_desserte_path(cache_dir)
  ok <- tryCatch({
    sf::st_write(sf::st_transform(dq, 2154), out, layer = "desserte_corrigee",
                 quiet = TRUE, delete_dsn = TRUE)
    # Desserte BD TOPO D'ORIGINE (avant recalage) dans le MEME fichier, couche
    # `desserte_origine` : le comparateur swipe (mod_accessibility) l'affiche a
    # gauche, la corrigee a droite, pour donner a voir le decalage recale par
    # ALSroads. Best-effort - l'ecriture de la corrigee reste l'objectif premier.
    tryCatch(
      sf::st_write(sf::st_transform(acq$desserte, 2154), out,
                   layer = "desserte_origine", quiet = TRUE, append = TRUE),
      error = function(e) cli::cli_warn(
        "desserte_origine layer not written: {conditionMessage(e)}"))
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) return(list(status = "error", reason = "acc_correct_write_failed"))

  list(status = "success", corrected_path = out, n_troncons = nrow(dq),
       n_bdtopo = cplt$n_bdtopo, n_osm_ajoutes = cplt$n_ajoutes,
       osm_statut = cplt$statut)
}

#' Run the accessibility engines for a project (worker-side)
#'
#' Heavy, self-contained function meant to run in a `future` worker. Acquires
#' the **IGN RGE ALTI 5 m** DEM (Sylvaccess-calibrated resolution) and the road
#' network (**IGN BD TOPO V3**) for the buffered AOI, derives the forest mask
#' from **IGN BD Foret V2** clipped to that emprise, runs
#' `foretaccess::preprocess()` then the requested engines, writes
#' each engine's categorical class raster to
#' `cache/accessibility/acc_<engine>.tif`, and writes an exportable GeoPackage
#' (`foret` + `desserte` layers). Returns only serialisable data (paths + recap
#' data.frames), never terra/sf objects tied to this process.
#'
#' The DEM, road network and BD Foret layer are cached under
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
#'   is truncated. The forest **mask** stays the original AOI - only the analysed
#'   emprise widens. `0` (default) keeps the historical behaviour (forest extent
#'   only). Each buffer value acquires the DEM/roads in its own sub-cache so
#'   changing it re-fetches cleanly instead of reusing a stale emprise.
#' @return A named list describing the run (see details).
#' @noRd
run_accessibility <- function(aoi_path, engines, cache_dir, buffer_m = 0,
                              use_corrected_desserte = FALSE, project_path = NULL) {
  engines <- intersect(engines %||% character(0), ACCESSIBILITY_ENGINES)
  if (length(engines) == 0L) {
    return(list(status = "error", reason = "accessibility_need_engine"))
  }
  # Acquisition commune (AOI tamponnee, MNT HIGHRES + repli, desserte BD TOPO) -
  # factorisee avec l'etape de correction LiDAR (cf. .acquire_mnt_desserte).
  acq <- .acquire_mnt_desserte(aoi_path, cache_dir, buffer_m)
  if (!identical(acq$status, "ok")) return(acq)
  aoi <- acq$aoi; aoi_ext <- acq$aoi_ext; epsg <- acq$epsg
  acq_dir <- acq$acq_dir; mnt <- acq$mnt; desserte <- acq$desserte

  # DESSERTE CORRIGEE (NDP 1) - DECOUPLEE : la qualification LiDAR (~2-3 h, lourde
  # en memoire) N'EST PLUS lancee ici. Elle est produite au prealable et a la
  # demande par `run_desserte_lidar_correction()` (bouton dedie), qui persiste
  # `desserte_corrigee.gpkg`. Ici on se contente de LA CHARGER si l'utilisateur a
  # coche " utiliser la desserte corrigee " et qu'elle existe : elle remplace la
  # brute en entree de `preprocess()` -> tous les moteurs. Les runs moteurs
  # restent donc legers (aucun pic memoire, pas de navigateur tue).
  desserte_source <- NA_character_
  if (isTRUE(use_corrected_desserte)) {
    cp <- .corrected_desserte_path(cache_dir)
    dc <- if (file.exists(cp)) {
      tryCatch(sf::st_read(cp, quiet = TRUE), error = function(e) NULL)
    } else NULL
    # Garde-fou : `preprocess()`/`.rasteriser_desserte` exigent la colonne `classe`.
    if (inherits(dc, "sf") && nrow(dc) > 0L && "classe" %in% names(dc)) {
      desserte <- tryCatch(sf::st_transform(dc, epsg), error = function(e) dc)
      desserte_source <- "ndp1_lidar"
    } else {
      desserte_source <- "ndp0_brute"   # demandee mais indisponible/invalide -> brut
    }
  }

  # Flag DFCI sur la desserte : sans lui, `camion_dfci()` s'arrete (" Aucune
  # desserte-source DFCI "), car `preprocess()` construit `dfci_source_mask` a
  # partir de la colonne `dfci` de la desserte. Source OSM `ref:FR:DFCI` en
  # priorite (foretaccess::acquire_dfci + flag_dfci), repli geometrique de
  # flag_dfci, puis heuristique app routes/pistes en dernier recours. La
  # provenance (`dfci_source`) est remontee pour signaler le cas heuristique a
  # l'utilisateur. Pose seulement si le moteur DFCI est demande.
  dfci_source <- NA_character_
  if ("camion_dfci" %in% engines) {
    fl <- .resolve_desserte_dfci(desserte, aoi_ext, epsg, acq_dir)
    desserte <- fl$desserte
    dfci_source <- fl$source
  }

  # 3. Masque foret = foret reelle (IGN BD Foret V2) restreinte a l'emprise
  # tamponnee, PAS la simple geometrie declaree du projet. `acquire_foret`
  # clippe la BD Foret sur `aoi_ext` par st_intersection : le resultat est
  # exactement (emprise projet + buffer) inter foret BD Foret V2. Ainsi l'analyse
  # couvre toute la foret accessible dans le tampon (y compris hors parcelles du
  # projet) et ne peint pas en " foret " des zones non boisees. Repli sur la
  # geometrie projet si la BD Foret est indisponible ou vide sur l'emprise.
  foret_bd <- tryCatch(
    foretaccess::acquire_foret(aoi_ext, crs = epsg, cache_dir = acq_dir),
    error = function(e) NULL)
  foret_mask <- if (inherits(foret_bd, "sf") && nrow(foret_bd) > 0L) foret_bd else aoi

  # 4. Pretraitement commun (pente, exposition, masques, rasterisation).
  pre <- tryCatch(
    foretaccess::preprocess(mnt = mnt, desserte = desserte, foret = foret_mask),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(pre, "acc_err")) {
    return(list(status = "error", reason = "accessibility_preprocess_failed",
                detail = pre$msg))
  }

  # 5bis. Couche `departs` (places de depot) pour le moteur cable. La desserte a
  # DEJA ete corrigee au LiDAR en amont (sect.2bis) le cas echeant : on ne re-qualifie
  # PAS ici, on place les depots sur la desserte courante. En NDP 1, la largeur
  # mesuree (`largeur_carrossable_m`) rend `places_depot()` selective (departs
  # realistes ~1189 vs ~1877) ; en NDP 0 (colonne absente), `largeur_champ = NULL`.
  departs <- NULL
  if ("cable" %in% engines) {
    lc <- if (identical(desserte_source, "ndp1_lidar") &&
              "largeur_carrossable_m" %in% names(desserte))
      "largeur_carrossable_m" else NULL
    departs <- tryCatch(
      foretaccess::places_depot(desserte, mnt, foret = foret_mask, largeur_champ = lc),
      error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
    if (inherits(departs, "acc_err")) {
      return(list(status = "error", reason = "accessibility_cable_departs_failed",
                  detail = departs$msg))
    }
  }

  # 5. Moteurs : raster de classes -> disque ; recap -> memoire.
  engine_fun <- list(
    skidder = foretaccess::skidder,
    porteur = foretaccess::porteur,
    camion_dfci = foretaccess::camion_dfci,
    # Signature differente (departs) mais meme forme de retour ($accessibilite /
    # $recap) : closure pour l'aligner sur le contrat f(pre) de la boucle.
    cable = function(pre) foretaccess::potentiel_cable(pre, departs = departs))
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

    # Le skidder produit AUSSI le raster " classes de debardage " : les bandes de
    # distance Sylvaccess (0-250, ..., > 2000 m) + inaccessible / inexploitable /
    # hors_foret, pret a l'affichage avec sa table de couleurs (vert proche ->
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

  # 6. GeoPackage exportable : masque foret (BD Foret V2 inter emprise) + desserte.
  # Best-effort.
  gpkg_path <- file.path(cache_dir, "accessibilite.gpkg")
  unlink(gpkg_path)
  tryCatch({
    sf::st_write(sf::st_transform(foret_mask, 2154), gpkg_path, layer = "foret",
                 quiet = TRUE, delete_dsn = TRUE)
    sf::st_write(sf::st_transform(desserte, 2154), gpkg_path, layer = "desserte",
                 quiet = TRUE, append = TRUE)
    # Places de depot calculees par `places_depot()` le long de la desserte
    # (corrigee au LiDAR en NDP 1) : on PERSISTE la geometrie (pas seulement le
    # nombre) pour l'afficher en couche " Places de depot " sur les cartes
    # Accessibilite et Desserte.
    if (inherits(departs, "sf") && nrow(departs) > 0L) {
      sf::st_write(sf::st_transform(departs, 2154), gpkg_path,
                   layer = "places_depot", quiet = TRUE, append = TRUE)
    }
  }, error = function(e) cli::cli_warn(
    "accessibility GPKG write failed: {conditionMessage(e)}"))

  # 7. Validation ACCESSFOR (IGN) SYSTEMATIQUE des que les classes de debardage
  # ont ete produites (skidder) : recupere la couche nationale IGN (WFS happign),
  # la reclasse sur NOTRE grille + emprise, calcule l'accord et ecrit le raster
  # ACCESSFOR affichable. Execute ICI (dans le worker `future`) pour ne pas bloquer
  # le thread principal. Best-effort reseau : un echec (WFS indisponible, happign
  # absent) ne fait PAS echouer l'analyse - le rendu " classes de debardage "
  # retombe alors sur un raster simple (pas de volet ACCESSFOR).
  accessfor <- NULL
  if (!is.null(raster_paths[["classes_debardage"]])) {
    accessfor <- tryCatch(
      run_accessfor_validation(raster_paths[["classes_debardage"]], "skidder"),
      error = function(e) NULL)
    if (!is.list(accessfor) || !identical(accessfor$status, "success")) {
      accessfor <- NULL
    }
  }

  list(
    status = "success",
    engines = engines,
    recaps = recaps,
    raster_paths = raster_paths,
    gpkg_path = if (file.exists(gpkg_path)) gpkg_path else NULL,
    n_desserte = nrow(desserte),
    dfci_source = dfci_source,
    # Provenance de la desserte utilisee : "ndp1_lidar" (desserte corrigee LiDAR
    # chargee depuis le cache) / "ndp0_brute" (corrigee demandee mais absente ->
    # repli brut) / NA (desserte corrigee non demandee).
    desserte_source = desserte_source,
    n_departs = if (inherits(departs, "sf")) nrow(departs) else NA_integer_,
    # ACCESSFOR (reference IGN) : chemin du raster affichable (vis-a-vis des classes
    # de debardage sous le volet) + resume d'accord pour le panneau de validation.
    accessfor_raster_path = accessfor$accessfor_raster_path,
    accessfor = accessfor)
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

#' Cross-section of the road segment nearest to a map click (spec 030)
#'
#' Adapter over [foretaccess::profil_travers()]: resolves the project's corrected
#' road network, LiDAR point cloud and DTM, converts the clicked WGS84 coordinates
#' to the working CRS, and hands everything to the core. Computes nothing itself
#' (CLAUDE.md rules 1-2) and draws nothing — the plate lives in
#' `fct_plot_desserte_profil.R`.
#'
#' Runs inside a `future` worker: no Shiny, no global state, no plotting.
#'
#' @param project_path Project directory.
#' @param lng,lat Clicked point, WGS84 degrees (what leaflet gives).
#' @param crs Working EPSG code. Default 2154.
#' @param tolerance_m Snapping radius, in metres.
#' @return The `profil_travers()` list, or a `list(status = "error", reason =)`
#'   naming the missing ingredient — never a bare `NULL`, so the caller can tell
#'   "no segment there" from "no LiDAR in this project".
#' @noRd
acc_profil_travers <- function(project_path, lng, lat, crs = 2154,
                               tolerance_m = 25) {
  if (is.null(project_path) || !nzchar(project_path)) {
    return(list(status = "error", reason = "acc_profil_no_project"))
  }
  cache_dir <- .accessibility_cache_dir(project_path)
  corrected <- .corrected_desserte_path(cache_dir)
  if (!file.exists(corrected)) {
    return(list(status = "error", reason = "acc_profil_no_desserte"))
  }
  laz_dir <- file.path(project_path, "cache", "layers", "lidar_nuage")
  if (!dir.exists(laz_dir) ||
      length(list.files(laz_dir, pattern = "\\.(copc\\.)?laz$")) == 0L) {
    return(list(status = "error", reason = "acc_profil_no_lidar"))
  }
  mnt <- .acc_rvt_mnt_path(project_path)
  if (is.null(mnt)) return(list(status = "error", reason = "acc_profil_no_mnt"))

  # Le clic arrive en WGS84 (leaflet) ; le coeur travaille dans `crs`.
  pt <- tryCatch({
    p <- sf::st_sfc(sf::st_point(c(lng, lat)), crs = 4326)
    sf::st_transform(p, crs)
  }, error = function(e) NULL)
  if (is.null(pt)) return(list(status = "error", reason = "acc_profil_bad_point"))

  reseau <- tryCatch(sf::st_read(corrected, layer = "desserte_corrigee",
                                 quiet = TRUE),
                     error = function(e) NULL)
  if (!inherits(reseau, "sf") || nrow(reseau) == 0L) {
    return(list(status = "error", reason = "acc_profil_no_desserte"))
  }

  out <- tryCatch(
    foretaccess::profil_travers(
      desserte = reseau, xy = pt, las_source = laz_dir, mnt = mnt,
      crs = crs, tolerance_m = tolerance_m,
      cache_dir = file.path(cache_dir, "profil_travers")),
    error = function(e) structure(list(msg = conditionMessage(e)),
                                  class = "acc_err"))
  if (inherits(out, "acc_err")) {
    return(list(status = "error", reason = "acc_profil_failed", detail = out$msg))
  }
  # `NULL` du coeur = aucun troncon dans le rayon d'accrochage. Ce n'est pas une
  # panne : c'est une reponse, et l'utilisateur doit la lire comme telle.
  if (is.null(out)) return(list(status = "empty", reason = "acc_profil_no_segment"))
  out$status <- "success"
  out
}

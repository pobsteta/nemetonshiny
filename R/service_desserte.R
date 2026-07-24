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

# --- Garde-fou mémoire du glouton ------------------------------------------
#
# `foretaccess::reseau_desserte()` matérialise une table de voisinage
# (`NeibTable.neighbors`, un `Vec<Vec<Neighbor>>` Rust) : UNE allocation tas par
# cellule franchissable, contenant tout le disque de rayon `d_neighborhood_m`
# (42 m par défaut). Le pic mémoire est donc PROPORTIONNEL à la grille et
# QUADRATIQUE en `d_neighborhood / résolution` — pas une fuite (la mémoire est
# rendue), un coût structurel.
#
# Mesures (grille synthétique, foretaccess 1.21.0, 2026-07-24) :
#   600x600 @ 5 m, d = 42 m (220 voisins) -> 1 537 Mo  soit 4,37 Ko/cellule
#   600x600 @ 5 m, d = 30 m (112 voisins) ->   841 Mo  soit 2,39 Ko/cellule
#   600x600 @ 5 m, d = 21 m ( 56 voisins) ->   493 Mo  soit 1,40 Ko/cellule
#   800x800 @ 5 m, d = 42 m               -> 2 520 Mo  soit 4,03 Ko/cellule
# Sans garde-fou, une emprise de ~10 km x 10 km à 5 m (≈ 3,9 M cellules) demande
# ~17 Go et emporte la machine par OOM après ~15 min de calcul (observé).

#' Number of disc offsets in the solver's extended neighbourhood
#'
#' Mirrors `build_offsets()` (foretaccess `src/rust/src/desserte/neighborhood.rs`):
#' every integer offset of the square `[-nb, nb]^2` except the centre, kept when
#' its planimetric distance is within `d_neighborhood`.
#'
#' @param d_neighborhood_m Neighbourhood radius in metres.
#' @param csize Cell size in metres.
#' @return Integer count of offsets.
#' @noRd
.desserte_n_offsets <- function(d_neighborhood_m = 42, csize = 5) {
  d <- suppressWarnings(as.numeric(d_neighborhood_m))
  cs <- suppressWarnings(as.numeric(csize))
  if (!is.finite(d) || !is.finite(cs) || d <= 0 || cs <= 0) return(0L)
  nb <- as.integer(d / cs + 0.5)
  if (nb < 1L) return(0L)
  g <- expand.grid(dr = -nb:nb, dc = -nb:nb)
  g <- g[!(g$dr == 0L & g$dc == 0L), , drop = FALSE]
  sum(sqrt(g$dr^2 + g$dc^2) * cs <= d)
}

#' Estimate the greedy engine's peak memory for a grid
#'
#' Per cell the solver holds: the neighbour list (`n_offsets * 16` bytes), the
#' `Vec` header (24), the `NodeState` search array (80), the heuristic grid (8)
#' and the id/coord tables (20). The 1.25 factor is the measured allocator
#' overhead (jemalloc), calibrated on the runs quoted above.
#'
#' @param n_cells Number of grid cells.
#' @param d_neighborhood_m Neighbourhood radius (m).
#' @param csize Cell size (m).
#' @return Estimated peak in bytes (numeric), or `NA_real_` on bad input.
#' @noRd
.desserte_memory_estimate <- function(n_cells, d_neighborhood_m = 42, csize = 5) {
  n <- suppressWarnings(as.numeric(n_cells))
  if (!is.finite(n) || n <= 0) return(NA_real_)
  n_off <- .desserte_n_offsets(d_neighborhood_m, csize)
  per_cell <- n_off * 16 + 24 + 80 + 8 + 20
  n * per_cell * 1.25
}

#' Grid cell count of an extent at a given resolution
#'
#' `buffer_m` widens the **bounding box**, which is all the grid depends on:
#' buffering the geometries (per feature or after a union) yields the exact same
#' box, so the caller never has to pay for `st_buffer()` just to size the grid.
#'
#' @param aoi An `sf`/`sfc` object; its bounding box drives the grid.
#' @param res_m Resolution in metres.
#' @param buffer_m Buffer in metres grown on every side of the box.
#' @return Number of cells (numeric), or `NA_real_`.
#' @noRd
.desserte_grid_cells <- function(aoi, res_m = 5, buffer_m = 0) {
  bb <- tryCatch(sf::st_bbox(aoi), error = function(e) NULL)
  r <- suppressWarnings(as.numeric(res_m))
  b <- suppressWarnings(as.numeric(buffer_m))
  if (!is.finite(b) || b < 0) b <- 0
  if (is.null(bb) || !is.finite(r) || r <= 0) return(NA_real_)
  w <- as.numeric(bb$xmax - bb$xmin) + 2 * b
  h <- as.numeric(bb$ymax - bb$ymin) + 2 * b
  if (!is.finite(w) || !is.finite(h) || w <= 0 || h <= 0) return(NA_real_)
  ceiling(w / r) * ceiling(h / r)
}

#' Memory currently available on the host, in bytes
#'
#' Reads `MemAvailable` from `/proc/meminfo` (Linux). Best-effort: returns
#' `NA_real_` elsewhere, which disables the guard rather than blocking a run.
#'
#' @return Available bytes (numeric) or `NA_real_`.
#' @noRd
.available_memory_bytes <- function() {
  if (!file.exists("/proc/meminfo")) return(NA_real_)
  lines <- tryCatch(readLines("/proc/meminfo", n = 60L), error = function(e) NULL)
  if (is.null(lines)) return(NA_real_)
  hit <- grep("^MemAvailable:", lines, value = TRUE)
  if (length(hit) == 0L) return(NA_real_)
  kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", hit[1])))
  if (!is.finite(kb)) return(NA_real_)
  kb * 1024
}

#' Pre-flight memory check for a desserte run
#'
#' Compares the estimated peak against a fraction of the available RAM. Set
#' `NEMETON_DESSERTE_SKIP_GUARD=1` to bypass (documented escape hatch: the
#' estimate is a model, the user may know better).
#'
#' @param aoi Extent to be analysed.
#' @param res_m Grid resolution (m).
#' @param d_neighborhood_m Neighbourhood radius (m).
#' @param frac Fraction of available RAM the run may claim.
#' @param buffer_m Buffer (m) to grow around `aoi` before sizing the grid; pass
#'   `0` when `aoi` is already the buffered extent.
#' @return A list with `ok`, `cells`, `bytes`, `available`.
#' @noRd
.desserte_memory_check <- function(aoi, res_m = 5, d_neighborhood_m = 42,
                                   frac = 0.8, buffer_m = 0) {
  cells <- .desserte_grid_cells(aoi, res_m, buffer_m)
  bytes <- .desserte_memory_estimate(cells, d_neighborhood_m, res_m)
  avail <- .available_memory_bytes()
  skip <- tolower(Sys.getenv("NEMETON_DESSERTE_SKIP_GUARD", "")) %in%
    c("1", "true", "yes", "oui")
  ok <- skip || !is.finite(bytes) || !is.finite(avail) || bytes <= avail * frac
  list(ok = isTRUE(ok), cells = cells, bytes = bytes, available = avail)
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

  # Garde-fou mémoire AVANT toute acquisition : le pic du glouton est connu à
  # partir de la seule emprise (cf. .desserte_memory_check). Sans ça, l'échec
  # arrive après ~15 min de calcul, sous forme d'OOM qui emporte la machine —
  # pas d'une condition R rattrapable.
  mem <- .desserte_memory_check(aoi_ext, res_m = 5)
  if (!isTRUE(mem$ok)) {
    return(list(status = "error", reason = "desserte_memory_guard",
                detail = sprintf(
                  "grille %.1f M cellules, pic estime %.1f Go, RAM disponible %.1f Go",
                  mem$cells / 1e6, mem$bytes / 1024^3, mem$available / 1024^3),
                cells = mem$cells, bytes = mem$bytes, available = mem$available))
  }

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

  # Persiste l'OBJET `foretaccess_reseau` complet (pas seulement le raster) pour le
  # typage : `vectoriser_reseau()` l'exige, et le glouton est trop long pour être
  # relancé. Le `$reseau` (SpatRaster) porte un pointeur externe non sérialisable :
  # on le `terra::wrap()` avant `saveRDS` (le typage l'`unwrap()`era).
  tryCatch({
    res_save <- res
    res_save$reseau <- terra::wrap(res$reseau)
    saveRDS(res_save, file.path(cache_dir, paste0("reseau_obj_", engine, ".rds")))
  }, error = function(e) cli::cli_warn(
    "desserte reseau object persist failed: {conditionMessage(e)}"))

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

#' Resolve the standing-volume (P1) column on a project's units
#'
#' The core and the app do NOT agree on the column name, which used to surface
#' as a false « volume P1 absent » error even on a fully computed project:
#'
#'   * `nemeton:::indicateur_p1_volume()` writes **`P1`** (its `column_name`
#'     default) — that is what a freshly computed `sf` carries in memory;
#'   * the project's `indicators.parquet` persists it as
#'     **`indicateur_p1_volume`**, aligned with the 30 other `indicateur_*`
#'     columns — and `.resolve_project_aoi_2154()` returns exactly that.
#'
#' Candidates are tried in order, then a case-insensitive match, and a column is
#' only accepted when it holds at least one finite value.
#'
#' @param parcelles An `sf`/data.frame of units.
#' @return The resolved column name, or `NULL` when no usable column exists.
#' @noRd
.resolve_volume_col <- function(parcelles) {
  if (!is.data.frame(parcelles) || ncol(parcelles) == 0L) return(NULL)
  nms <- names(parcelles)
  usable <- function(col) {
    v <- suppressWarnings(as.numeric(parcelles[[col]]))
    any(is.finite(v))
  }
  for (cand in c("P1", "indicateur_p1_volume")) {
    if (cand %in% nms && usable(cand)) return(cand)
  }
  hit <- nms[tolower(nms) %in% c("p1", "indicateur_p1_volume")]
  for (cand in hit) if (usable(cand)) return(cand)
  NULL
}

#' Default wood-flux thresholds for road typing (m³ total)
#'
#' Named ascending numeric vector of class lower bounds consumed by
#' `foretaccess::typer_desserte()`. Each tronçon gets the highest class whose
#' accumulated flux bound it reaches.
#' @noRd
DESSERTE_TYPAGE_SEUILS <- c(tertiaire = 0, secondaire = 100, primaire = 500)

#' Type a project's created road network by mobilisable wood flux (worker-side)
#'
#' Chains `nemeton::volume_mobilisable()` (P1 -> mobilised volume) into the
#' `foretaccess` typing pipeline, per the spec-040 integration brief:
#'
#'   parcelles (+P1) -> volume_mobilisable(unite = "m3_total")   [nemeton]
#'                   -> calculer_flux(volume_champ = "volume_mobilisable")
#'                   -> typer_desserte(seuils_flux)                [foretaccess]
#'
#' **Unit trap (brief §3): `unite = "m3_total"` for typing** — `calculer_flux()`
#' distributes then accumulates a TOTAL m³ per parcel; an `m3_ha` density would
#' underestimate flux by a factor equal to the parcel area. (The `m3_ha` unit is
#' for weighting the glouton, a different consumer — not here.)
#'
#' Reuses the `foretaccess_reseau` object persisted by `run_desserte()` (the
#' glouton is too slow to re-run just to vectorise), so typing runs in seconds.
#' No app business logic (rules 1-3): two package calls.
#'
#' @param cache_dir The project's `cache/desserte` directory.
#' @param parcelles An `sf` of the parcels to serve, carrying a volume column.
#' @param taux_prelevement Numeric annual removal rate (voie « saisi »).
#' @param horizon_ans Numeric horizon in years.
#' @param engine Engine whose persisted network to type (default `"glouton"`).
#' @param seuils_flux Named ascending numeric vector of flux class bounds.
#' @param volume_col Name of the standing-volume column on `parcelles`, or
#'   `NULL` (default) to resolve it with `.resolve_volume_col()` — the core and
#'   the persisted project do NOT use the same name.
#' @return A named list: `status`, `recap` (length per type), `gpkg_path`,
#'   `seuils`, or an error list.
#' @noRd
run_desserte_typage <- function(cache_dir, parcelles, taux_prelevement,
                                horizon_ans, engine = "glouton",
                                seuils_flux = DESSERTE_TYPAGE_SEUILS,
                                volume_col = NULL) {
  if (!requireNamespace("foretaccess", quietly = TRUE) ||
      !requireNamespace("nemeton", quietly = TRUE)) {
    return(list(status = "error", reason = "desserte_typage_no_pkg"))
  }
  obj_path <- file.path(cache_dir, paste0("reseau_obj_", engine, ".rds"))
  if (!file.exists(obj_path)) {
    return(list(status = "error", reason = "desserte_typage_no_reseau"))
  }
  if (!inherits(parcelles, "sf") || nrow(parcelles) == 0L) {
    return(list(status = "error", reason = "desserte_typage_no_parcelles"))
  }
  # Résolution du nom de colonne : `P1` (sortie cœur en mémoire) OU
  # `indicateur_p1_volume` (nom persisté dans indicators.parquet, et donc ce que
  # renvoie .resolve_project_aoi_2154). Chercher « P1 » en dur produisait un
  # « volume P1 absent » sur un projet pourtant entièrement calculé.
  volume_col <- volume_col %||% .resolve_volume_col(parcelles)
  if (is.null(volume_col) || !volume_col %in% names(parcelles) ||
      !any(is.finite(suppressWarnings(as.numeric(parcelles[[volume_col]]))))) {
    return(list(status = "error", reason = "desserte_typage_no_volume"))
  }
  taux_prelevement <- suppressWarnings(as.numeric(taux_prelevement))
  horizon_ans <- suppressWarnings(as.numeric(horizon_ans))
  if (!is.finite(taux_prelevement) || !is.finite(horizon_ans) ||
      taux_prelevement <= 0 || horizon_ans <= 0) {
    return(list(status = "error", reason = "desserte_typage_bad_params"))
  }

  # Réseau persisté par run_desserte : unwrap du SpatRaster puis vectorisation.
  reseau <- tryCatch({
    r <- readRDS(obj_path); r$reseau <- terra::unwrap(r$reseau); r
  }, error = function(e) NULL)
  if (is.null(reseau)) {
    return(list(status = "error", reason = "desserte_typage_no_reseau"))
  }
  graphe <- tryCatch(foretaccess::vectoriser_reseau(reseau),
                     error = function(e) structure(list(msg = conditionMessage(e)),
                                                   class = "acc_err"))
  if (inherits(graphe, "acc_err")) {
    return(list(status = "error", reason = "desserte_typage_failed", detail = graphe$msg))
  }

  # Volume MOBILISÉ, unité m3_total (piège §3), voie « saisi » (taux + horizon).
  parc_vol <- tryCatch(
    nemeton::volume_mobilisable(parcelles, volume_col = volume_col,
                                unite = "m3_total",
                                taux_prelevement = taux_prelevement,
                                horizon_ans = horizon_ans),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(parc_vol, "acc_err")) {
    return(list(status = "error", reason = "desserte_typage_volume_failed",
                detail = parc_vol$msg))
  }

  # Flux accumulé puis typage par seuils.
  typee <- tryCatch({
    g <- foretaccess::calculer_flux(graphe, parc_vol,
                                    volume_champ = "volume_mobilisable")
    foretaccess::typer_desserte(g, seuils_flux = seuils_flux)
  }, error = function(e) structure(list(msg = conditionMessage(e)), class = "acc_err"))
  if (inherits(typee, "acc_err")) {
    return(list(status = "error", reason = "desserte_typage_failed", detail = typee$msg))
  }

  # Réseau typé -> GeoPackage (cache), pour l'affichage carte et l'export.
  gpkg_path <- file.path(cache_dir, paste0("typage_", engine, ".gpkg"))
  ok <- tryCatch({
    unlink(gpkg_path)
    sf::st_write(sf::st_transform(typee$troncons, 2154), gpkg_path,
                 layer = "reseau_type", quiet = TRUE, delete_dsn = TRUE)
    TRUE
  }, error = function(e) FALSE)

  list(
    status = "success",
    engine = engine,
    recap = typee$recap,
    gpkg_path = if (isTRUE(ok) && file.exists(gpkg_path)) gpkg_path else NULL,
    seuils = seuils_flux)
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

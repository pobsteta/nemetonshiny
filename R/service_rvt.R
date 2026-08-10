# ===========================================================================
# Service — Rendu relief RVT / VAT pour le comparateur de desserte
# ===========================================================================
#
# Génère un raster de VISUALISATION du relief à partir d'un MNT, pour servir de
# fond au comparateur « swipe » desserte BD TOPO vs desserte corrigée LiDAR
# (mod_accessibility). Le micro-relief y fait ressortir l'assiette réelle des
# routes (talus, plateforme), ce qui permet de juger visuellement du recalage
# opéré par ALSroads.
#
# Ce n'est PAS de la logique métier (règle 1 : indicateurs / familles / NDP) —
# c'est de la PRÉSENTATION cartographique d'un MNT, donc un service app.
#
# Trois moteurs (par ordre de préférence) :
#   * `foretaccess::vat_combined()` (>= 1.24.0) — le CVAT (Combined VAT), la
#     combinaison PAR DÉFAUT du plugin QGIS RVT (0,5·VAT_general + 0,5·VAT_flat),
#     validée pixel à pixel contre le plugin (99,998 % identiques). Le rendu
#     archéo de référence, sans dépendance Python.
#   * `rvt-py` (via reticulate) — le VAT canonique (Sky-View Factor + Openness +
#     Slope, ZRC SAZU). Repli si foretaccess est trop ancien.
#   * terra (repli garanti) — relief ombré (hillshade classique sur MNT débruité).
#     Pas le VAT canonique, mais suffit à révéler l'assiette des routes. Nommé
#     honnêtement « relief ombré » dans l'UI.
#
# Le résultat est un GeoTIFF mono-bande normalisé [0, 1] (0 = sombre, 1 = clair),
# mis en cache à côté du MNT. Best-effort : renvoie `NULL` sur échec (l'UI retombe
# alors sans fond relief).

#' Cached RVT relief-visualization path for a DEM
#'
#' @param mnt_path Path to the source DEM GeoTIFF.
#' @return Path of the cached RVT raster (next to the DEM), regardless of
#'   existence.
#' @noRd
.rvt_cache_path <- function(mnt_path) {
  d <- dirname(mnt_path)
  base <- tools::file_path_sans_ext(basename(mnt_path))
  file.path(d, paste0("rvt_", base, ".tif"))
}

#' Is the Python `rvt` module (rvt-py) available?
#'
#' @return `TRUE` when reticulate can see the `rvt` module.
#' @noRd
.rvt_py_available <- function() {
  requireNamespace("reticulate", quietly = TRUE) &&
    isTRUE(tryCatch(reticulate::py_module_available("rvt"),
                    error = function(e) FALSE))
}

#' terra fallback: classic shaded relief on a denoised DEM
#'
#' A single-azimuth hillshade (NW 315° / 45°) on a smoothed DEM. The MNT WMS RGE
#' ALTI HIGHRES carries a vertical striping artefact (tile bands): a
#' multi-directional hillshade *amplifies* it into parasitic streaks, whereas a
#' classic single hillshade on a lightly smoothed DEM reads cleanly (verified on
#' the ForêtAccess DEM). Returns a single-band `SpatRaster` normalized to
#' `[0, 1]`, or `NULL` on failure. Not the canonical VAT — see `.rvt_vat_py()`.
#'
#' @param mnt A `SpatRaster` DEM.
#' @noRd
.rvt_terra <- function(mnt) {
  tryCatch({
    # Lissage 5x5 (moyenne) : atténue le striping de colonne du MNT WMS avant le
    # calcul de pente/exposition. Best-effort : MNT brut si `focal` échoue.
    mnt <- tryCatch(terra::focal(mnt, w = 5, fun = "mean", na.policy = "omit",
                                 na.rm = TRUE), error = function(e) mnt)
    slope <- terra::terrain(mnt, "slope", unit = "radians")
    aspect <- terra::terrain(mnt, "aspect", unit = "radians")
    # Hillshade classique NW (azimut 315°, hauteur 45°) — la convention
    # cartographique, et le rendu le plus doux sur ce MNT.
    hs <- terra::shade(slope, aspect, angle = 45, direction = 315,
                       normalize = TRUE)
    # Légère composante pente pour marquer les talus, très allégée (le hillshade
    # porte déjà l'essentiel ; trop de pente réveille le bruit).
    smax <- tryCatch(terra::global(slope, "max", na.rm = TRUE)[[1]],
                     error = function(e) NA_real_)
    if (is.finite(smax) && smax > 0) {
      slope_dark <- 1 - (slope / smax)
      vat <- hs * 0.9 + slope_dark * 0.1
    } else {
      vat <- hs
    }
    # Normalisation robuste [0, 1] sur les quantiles (évite qu'un pixel extrême
    # écrase le contraste).
    qs <- tryCatch(terra::global(vat, fun = function(x)
      stats::quantile(x, c(0.02, 0.98), na.rm = TRUE)), error = function(e) NULL)
    if (!is.null(qs) && is.data.frame(qs) && ncol(qs) >= 2L) {
      lo <- qs[[1]][1]; hi <- qs[[2]][1]
      if (is.finite(lo) && is.finite(hi) && hi > lo) {
        vat <- (vat - lo) / (hi - lo)
        vat <- terra::clamp(vat, 0, 1)
      }
    }
    vat
  }, error = function(e) NULL)
}

#' rvt-py engine: canonical VAT blend via reticulate
#'
#' Computes the Visualization for Archaeological Topography default blend
#' (Sky-View Factor + positive openness + slope) with `rvt-py`. Returns a
#' single-band `SpatRaster` in `[0, 1]`, or `NULL` on any failure (caller falls
#' back to terra).
#'
#' @param mnt_path Path to the DEM GeoTIFF.
#' @param mnt A `SpatRaster` DEM (for georeferencing the result).
#' @noRd
.rvt_vat_py <- function(mnt_path, mnt) {
  tryCatch({
    rvt_vis <- reticulate::import("rvt.vis")
    rvt_blend <- reticulate::import("rvt.default")
    np <- reticulate::import("numpy")
    dem <- np$array(matrix(terra::values(mnt, mat = TRUE),
                           nrow = terra::nrow(mnt), byrow = FALSE))
    res_x <- terra::res(mnt)[1]
    dd <- rvt_blend$DefaultValues()
    arr <- dd$get_vat_general(dem, resolution = res_x)
    # arr : matrice [0,1]. La recopier dans la géométrie du MNT.
    out <- mnt
    terra::values(out) <- as.numeric(reticulate::py_to_r(arr))
    out
  }, error = function(e) NULL)
}

#' Is `foretaccess::vat_combined()` (CVAT engine) available?
#'
#' @return `TRUE` when foretaccess (>= 1.24.0) exports `vat_combined()`.
#' @noRd
.rvt_cvat_available <- function() {
  requireNamespace("foretaccess", quietly = TRUE) &&
    exists("vat_combined", where = asNamespace("foretaccess"))
}

#' foretaccess engine: CVAT (combined VAT) — RVT QGIS default
#'
#' Computes the CVAT (Combined Visualization for Archaeological Topography) with
#' `foretaccess::vat_combined()`, validated pixel-to-pixel against the RVT QGIS
#' plugin. Returns a single-band `SpatRaster` in `[0, 1]`, or `NULL` on failure
#' (caller falls back to rvt-py / terra).
#'
#' @param mnt A `SpatRaster` DEM.
#' @noRd
.rvt_cvat_ft <- function(mnt) {
  if (!.rvt_cvat_available()) return(NULL)
  tryCatch(foretaccess::vat_combined(mnt, as_byte = FALSE),
           error = function(e) NULL)
}

#' Reuse a CVAT already computed next to the DEM (instant, no recompute)
#'
#' `vat_combined()` on a full 0.5 m LiDAR mosaic (~4000×4000) is slow (~350 s),
#' which would freeze the Shiny loop if run synchronously in the comparator. But
#' the foretaccess / QGIS-RVT pipeline often persists the CVAT next to the DEM
#' (`<base>_CVAT_8bit[_foretaccess].tif`). When present, adopt it directly —
#' instant, and the reference rendering. 8-bit `[0, 255]` is rescaled to the
#' `[0, 1]` the grey renderer expects (a float source is passed through).
#'
#' @param mnt_path Path to the source DEM.
#' @return A `[0, 1]` `SpatRaster`, or `NULL` when no precomputed CVAT exists.
#' @noRd
.rvt_precomputed_path <- function(mnt_path) {
  if (is.null(mnt_path) || !nzchar(mnt_path)) return(NULL)
  d <- dirname(mnt_path); base <- tools::file_path_sans_ext(basename(mnt_path))
  cand <- c(file.path(d, paste0(base, "_CVAT_8bit_foretaccess.tif")),
            file.path(d, paste0(base, "_CVAT_8bit.tif")))
  cand <- cand[file.exists(cand)]
  if (length(cand) == 0L) NULL else cand[1]
}

.rvt_precomputed <- function(mnt_path) {
  p <- .rvt_precomputed_path(mnt_path)
  if (is.null(p)) return(NULL)
  r <- tryCatch(terra::rast(p), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  mx <- tryCatch(terra::global(r, "max", na.rm = TRUE)[[1]], error = function(e) NA_real_)
  if (is.finite(mx) && mx > 1.5) r <- r / 255      # 8-bit -> [0, 1]
  r
}

#' Will `generate_rvt()` return quickly for this DEM?
#'
#' `TRUE` when the RVT cache already exists or a precomputed CVAT sits next to the
#' DEM — in which case the comparator can paint the relief synchronously. `FALSE`
#' means a live `vat_combined()` (~1 min on a full LiDAR mosaic) is needed, which
#' the comparator runs asynchronously so the Shiny loop never freezes.
#'
#' @param mnt_path Path to the source DEM, or NULL.
#' @return `TRUE`/`FALSE`.
#' @noRd
.rvt_is_cheap <- function(mnt_path) {
  if (is.null(mnt_path) || !nzchar(mnt_path)) return(FALSE)
  file.exists(.rvt_cache_path(mnt_path)) ||
    !is.null(.rvt_precomputed_path(mnt_path))
}

#' Generate (or load from cache) an RVT relief raster for a DEM
#'
#' @param mnt_path Path to the source DEM GeoTIFF (e.g. the 1 m
#'   `mnt_highres_1m.tif` produced by the LiDAR-correction path).
#' @param overwrite Force regeneration even if the cache exists.
#' @return Path to the cached RVT GeoTIFF, or `NULL` on failure (no DEM, no
#'   engine, write error).
#' @noRd
generate_rvt <- function(mnt_path, overwrite = FALSE) {
  if (is.null(mnt_path) || !file.exists(mnt_path)) return(NULL)
  if (!requireNamespace("terra", quietly = TRUE)) return(NULL)
  out <- .rvt_cache_path(mnt_path)
  if (file.exists(out) && !isTRUE(overwrite)) return(out)

  mnt <- tryCatch(terra::rast(mnt_path), error = function(e) NULL)
  if (is.null(mnt)) return(NULL)

  # CVAT pré-calculé (instantané) > CVAT live foretaccess > rvt-py > terra.
  vat <- .rvt_precomputed(mnt_path) %||%
    .rvt_cvat_ft(mnt) %||%
    (if (.rvt_py_available()) .rvt_vat_py(mnt_path, mnt) else NULL) %||%
    .rvt_terra(mnt)
  if (is.null(vat)) return(NULL)

  ok <- tryCatch({
    terra::writeRaster(vat, out, overwrite = TRUE,
                       datatype = "FLT4S", gdal = c("COMPRESS=DEFLATE"))
    TRUE
  }, error = function(e) FALSE)
  if (isTRUE(ok) && file.exists(out)) out else NULL
}

#' Sidecar de provenance d'un CVAT : pour quels paramètres a-t-il été construit ?
#'
#' `.cvat_covers()` seul ne suffit pas à décider s'il faut reconstruire. Sur une
#' AOI dont la couverture LiDAR HD s'arrête avant `aoi + buffer`,
#' `acquire_mnt()` rend une emprise plus courte que demandée — mesuré sur Dabo :
#' 4454 × 4162 m produits pour 4617 × 4381 m demandés. Le CVAT résultant échoue
#' donc `.cvat_covers()` **quoi qu'on fasse**, et l'observe le reconstruit à
#' chaque entrée dans l'onglet : boucle infinie sur un calcul de plusieurs
#' minutes.
#'
#' Le sidecar enregistre ce qui a été DEMANDÉ (bbox AOI, buffer, résolution). On
#' ne relance donc pas une construction déjà tentée avec les mêmes paramètres,
#' que la donnée source ait pu couvrir l'emprise ou non. Même esprit que les
#' sidecars de provenance de `foretaccess` (spec 027).
#'
#' @param out_path Chemin du CVAT.
#' @return Chemin du sidecar JSON.
#' @noRd
.cvat_sidecar_path <- function(out_path) paste0(out_path, ".build.json")

#' Signature des paramètres de construction d'un CVAT
#' @noRd
.cvat_build_signature <- function(aoi, buffer_m, res_m) {
  bb <- tryCatch(as.numeric(sf::st_bbox(aoi)), error = function(e) NULL)
  list(
    bbox = if (is.null(bb)) NULL else round(bb, 1),
    buffer_m = as.numeric(buffer_m),
    res_m = as.numeric(res_m)
  )
}

#' Un CVAT a-t-il déjà été construit pour ces paramètres ?
#'
#' `TRUE` seulement si le raster existe ET que son sidecar porte exactement la
#' même signature. Un CVAT sans sidecar (antérieur à ce mécanisme) est considéré
#' comme à reconstruire — une fois, puisque la construction écrit le sidecar.
#'
#' @noRd
.cvat_built_for <- function(out_path, aoi, buffer_m, res_m) {
  if (is.null(out_path) || !file.exists(out_path)) return(FALSE)
  sc <- .cvat_sidecar_path(out_path)
  if (!file.exists(sc)) return(FALSE)
  prev <- tryCatch(jsonlite::fromJSON(sc), error = function(e) NULL)
  if (!is.list(prev)) return(FALSE)
  want <- .cvat_build_signature(aoi, buffer_m, res_m)
  isTRUE(identical(as.numeric(prev$buffer_m), want$buffer_m) &&
         identical(as.numeric(prev$res_m), want$res_m) &&
         !is.null(prev$bbox) && !is.null(want$bbox) &&
         isTRUE(all.equal(as.numeric(prev$bbox), want$bbox, tolerance = 1e-6)))
}

#' Écrit le sidecar de provenance après une construction réussie
#' @noRd
.cvat_write_sidecar <- function(out_path, aoi, buffer_m, res_m) {
  tryCatch({
    sig <- .cvat_build_signature(aoi, buffer_m, res_m)
    sig$built_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    jsonlite::write_json(sig, .cvat_sidecar_path(out_path), auto_unbox = TRUE)
  }, error = function(e) invisible(NULL))
  invisible(out_path)
}

#' Does an existing CVAT raster cover an AOI + buffer?
#'
#' `build_cvat_precomputed()` (and foretaccess) do NOT re-check an already-written
#' CVAT for coverage. When the buffer grows, the cached CVAT can be too short.
#' This compares the CVAT's extent to the AOI bounding box grown by `buffer_m`
#' (both in the CVAT's CRS). A small tolerance of one cell absorbs rounding.
#'
#' @param out_path Path to the CVAT raster.
#' @param aoi AOI (`sf`/`sfc` or path).
#' @param buffer_m Buffer (m) grown around the AOI.
#' @return `TRUE` if the CVAT extent contains the AOI + buffer, else `FALSE`.
#' @noRd
.cvat_covers <- function(out_path, aoi, buffer_m = 0) {
  if (is.null(out_path) || !file.exists(out_path)) return(FALSE)
  r <- tryCatch(terra::rast(out_path), error = function(e) NULL)
  if (is.null(r)) return(FALSE)
  a <- tryCatch(
    if (is.character(aoi)) sf::st_read(aoi, quiet = TRUE) else aoi,
    error = function(e) NULL)
  if (is.null(a) || (inherits(a, c("sf", "sfc")) && length(sf::st_geometry(a)) == 0L)) {
    return(FALSE)
  }
  a_bb <- tryCatch(
    sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(a)), terra::crs(r))),
    error = function(e) NULL)
  if (is.null(a_bb)) return(FALSE)
  bm <- max(0, suppressWarnings(as.numeric(buffer_m)))
  if (!is.finite(bm)) bm <- 0
  cov <- as.vector(terra::ext(r))                     # xmin xmax ymin ymax
  tol <- max(terra::res(r))
  isTRUE(cov[1] <= a_bb[["xmin"]] - bm + tol &&
         cov[2] >= a_bb[["xmax"]] + bm - tol &&
         cov[3] <= a_bb[["ymin"]] - bm + tol &&
         cov[4] >= a_bb[["ymax"]] + bm - tol)
}

#' Which relief engine will `generate_rvt()` use?
#'
#' For the UI label (« CVAT (relief archéo) » vs « VAT » vs « relief ombré »).
#'
#' @return `"cvat"` (foretaccess), `"vat"` (rvt-py) or `"hillshade"` (terra).
#' @noRd
rvt_engine <- function() {
  if (.rvt_cvat_available()) "cvat"
  else if (.rvt_py_available()) "vat"
  else "hillshade"
}

#' Cache root to hand to `foretaccess` for a file living in the project cache
#'
#' `foretaccess` appends its OWN `layers/<couche>/` segment under the `cache_dir`
#' it is given (`.chemin_cache()`), so passing `<projet>/cache/layers` — the
#' directory holding `lidar_mnt_mosaic.tif` — produced a duplicated
#' `cache/layers/layers/mnt/mnt.tif`. We therefore hand it the PARENT
#' (`<projet>/cache`) whenever the file sits directly in `cache/layers`, so its
#' re-acquired DEM lands in `cache/layers/mnt/` alongside our own subfolders
#' (`sentinel2/`, `lidar_mnt/`, …). Any other location is passed through
#' unchanged (tests, tempdirs) — never climb above a directory we don't own.
#'
#' @param path A file path inside the project cache.
#' @return The directory to use as `cache_dir`.
#' @noRd
.foretaccess_cache_root <- function(path) {
  d <- dirname(path)
  if (identical(basename(d), "layers")) dirname(d) else d
}

#' Materialize the precomputed CVAT next to a LiDAR DEM (idempotent)
#'
#' Writes `<base>_CVAT_8bit_foretaccess.tif` = `foretaccess::vat_combined()` in
#' 8-bit, next to the DEM — the file `.rvt_precomputed_path()` adopts first (so
#' the comparator / relief overlay paint instantly). Until now that file only
#' existed when written by hand; this is its real producer, meant to run in a
#' background worker when a LiDAR project is opened (the ~1 min compute must not
#' freeze the Shiny loop). Best-effort: `NULL` if foretaccess < 1.24.0, the DEM is
#' unreadable, or the write fails. Idempotent: no recompute if the file exists
#' (unless `overwrite`).
#'
#' @param mnt_path Source DEM path (ideally the native 0.5 m LiDAR HD DTM).
#' @param aoi Optional AOI (`sf`/`sfc` or path). When provided AND foretaccess
#'   >= 1.25.0 is present, delegates to `foretaccess::build_cvat_precomputed()`
#'   which GUARANTEES coverage of the AOI + buffer (re-acquires the LiDAR HD DEM
#'   if the mosaic is too short, then recomputes). Without `aoi`: current
#'   behaviour, CVAT on the DEM as-is (safe fallback).
#' @param buffer_m Buffer (m) grown around the AOI. Default 0.
#' @param overwrite Force recompute even if the output exists.
#' @return Path to the 8-bit CVAT, or `NULL`.
#' @noRd
build_cvat_precomputed <- function(mnt_path, aoi = NULL, buffer_m = 0,
                                   overwrite = FALSE) {
  if (is.null(mnt_path) || !file.exists(mnt_path)) return(NULL)
  if (!.rvt_cvat_available()) return(NULL)            # foretaccess >= 1.24.0 requis
  out <- file.path(
    dirname(mnt_path),
    paste0(tools::file_path_sans_ext(basename(mnt_path)),
           "_CVAT_8bit_foretaccess.tif"))
  # Avec une AOI : le CVAT doit COUVRIR aoi+buffer. Ni notre code ni foretaccess
  # ne re-vérifient un `out` déjà présent (sauf overwrite) — un buffer agrandi
  # laisserait donc un CVAT trop court, non détecté. On vérifie la couverture ici
  # et on FORCE le recalcul si insuffisant. foretaccess ré-acquiert le MNT si sa
  # mosaïque est trop courte, puis recalcule.
  if (!is.null(aoi) &&
      exists("build_cvat_precomputed", where = asNamespace("foretaccess"))) {
    res_m <- suppressWarnings(as.numeric(APP_CONFIG$cvat_res_m))
    if (!isTRUE(is.finite(res_m)) || res_m <= 0) res_m <- 2
    # Reprise : soit le CVAT couvre déjà aoi+buffer, soit il a DÉJÀ été construit
    # pour exactement ces paramètres. Le second cas est indispensable — sur une
    # AOI où la couverture LiDAR HD s'arrête avant aoi+buffer, `.cvat_covers()`
    # ne peut jamais être satisfait et l'observe relancerait sans fin (cf.
    # `.cvat_built_for`).
    if (!isTRUE(overwrite) && file.exists(out) &&
        (isTRUE(.cvat_covers(out, aoi, buffer_m)) ||
         isTRUE(.cvat_built_for(out, aoi, buffer_m, res_m)))) {
      return(out)
    }
    built <- tryCatch(
      foretaccess::build_cvat_precomputed(
        aoi = aoi, cache_dir = .foretaccess_cache_root(mnt_path),
        buffer_m = buffer_m,
        # `res_lidar_m` : le défaut cœur (0,5 m) fait ~81 M cellules sur
        # l'emprise de Dabo et sature la mémoire. Le rendu ré-agrège à 2000 px,
        # donc cette finesse est perdue à l'affichage (cf. APP_CONFIG$cvat_res_m).
        res_lidar_m = res_m,
        mnt_existant = mnt_path, out = out, overwrite = TRUE),  # force le recalcul
      error = function(e) NULL)
    # Sidecar écrit même si la couverture reste partielle : il mémorise ce qui a
    # été DEMANDÉ, ce qui suffit à ne pas retenter la même construction.
    if (!is.null(built)) .cvat_write_sidecar(built, aoi, buffer_m, res_m)
    return(built)
  }
  # Sans AOI : court-circuit idempotent (CVAT sur le MNT tel quel).
  if (file.exists(out) && !isTRUE(overwrite)) return(out)
  mnt <- tryCatch(terra::rast(mnt_path), error = function(e) NULL)
  if (is.null(mnt)) return(NULL)
  cvat <- tryCatch(foretaccess::vat_combined(mnt, as_byte = TRUE),
                   error = function(e) NULL)
  if (is.null(cvat)) return(NULL)
  ok <- tryCatch({
    terra::writeRaster(cvat, out, overwrite = TRUE, datatype = "INT1U",
                       gdal = c("COMPRESS=DEFLATE"))
    TRUE
  }, error = function(e) FALSE)
  if (isTRUE(ok) && file.exists(out)) out else NULL
}

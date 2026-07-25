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

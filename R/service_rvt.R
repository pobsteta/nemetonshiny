# ===========================================================================
# Service - Rendu relief RVT / VAT pour le comparateur de desserte
# ===========================================================================
#
# Genere un raster de VISUALISATION du relief a partir d'un MNT, pour servir de
# fond au comparateur " swipe " desserte BD TOPO vs desserte corrigee LiDAR
# (mod_accessibility). Le micro-relief y fait ressortir l'assiette reelle des
# routes (talus, plateforme), ce qui permet de juger visuellement du recalage
# opere par ALSroads.
#
# Ce n'est PAS de la logique metier (regle 1 : indicateurs / familles / NDP) -
# c'est de la PRESENTATION cartographique d'un MNT, donc un service app.
#
# Trois moteurs (par ordre de preference) :
#   * `foretaccess::vat_combined()` (>= 1.24.0) - le CVAT (Combined VAT), la
#     combinaison PAR DEFAUT du plugin QGIS RVT (0,5.VAT_general + 0,5.VAT_flat),
#     validee pixel a pixel contre le plugin (99,998 % identiques). Le rendu
#     archeo de reference, sans dependance Python.
#   * `rvt-py` (via reticulate) - le VAT canonique (Sky-View Factor + Openness +
#     Slope, ZRC SAZU). Repli si foretaccess est trop ancien.
#   * terra (repli garanti) - relief ombre (hillshade classique sur MNT debruite).
#     Pas le VAT canonique, mais suffit a reveler l'assiette des routes. Nomme
#     honnetement " relief ombre " dans l'UI.
#
# Le resultat est un GeoTIFF mono-bande normalise [0, 1] (0 = sombre, 1 = clair),
# mis en cache a cote du MNT. Best-effort : renvoie `NULL` sur echec (l'UI retombe
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
#' A single-azimuth hillshade (NW 315deg / 45deg) on a smoothed DEM. The MNT WMS RGE
#' ALTI HIGHRES carries a vertical striping artefact (tile bands): a
#' multi-directional hillshade *amplifies* it into parasitic streaks, whereas a
#' classic single hillshade on a lightly smoothed DEM reads cleanly (verified on
#' the ForetAccess DEM). Returns a single-band `SpatRaster` normalized to
#' `[0, 1]`, or `NULL` on failure. Not the canonical VAT - see `.rvt_vat_py()`.
#'
#' @param mnt A `SpatRaster` DEM.
#' @noRd
.rvt_terra <- function(mnt) {
  tryCatch({
    # Lissage 5x5 (moyenne) : attenue le striping de colonne du MNT WMS avant le
    # calcul de pente/exposition. Best-effort : MNT brut si `focal` echoue.
    mnt <- tryCatch(terra::focal(mnt, w = 5, fun = "mean", na.policy = "omit",
                                 na.rm = TRUE), error = function(e) mnt)
    slope <- terra::terrain(mnt, "slope", unit = "radians")
    aspect <- terra::terrain(mnt, "aspect", unit = "radians")
    # Hillshade classique NW (azimut 315deg, hauteur 45deg) - la convention
    # cartographique, et le rendu le plus doux sur ce MNT.
    hs <- terra::shade(slope, aspect, angle = 45, direction = 315,
                       normalize = TRUE)
    # Legere composante pente pour marquer les talus, tres allegee (le hillshade
    # porte deja l'essentiel ; trop de pente reveille le bruit).
    smax <- tryCatch(terra::global(slope, "max", na.rm = TRUE)[[1]],
                     error = function(e) NA_real_)
    if (is.finite(smax) && smax > 0) {
      slope_dark <- 1 - (slope / smax)
      vat <- hs * 0.9 + slope_dark * 0.1
    } else {
      vat <- hs
    }
    # Normalisation robuste [0, 1] sur les quantiles (evite qu'un pixel extreme
    # ecrase le contraste).
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
    # arr : matrice [0,1]. La recopier dans la geometrie du MNT.
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

#' foretaccess engine: CVAT (combined VAT) - RVT QGIS default
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
#' `vat_combined()` on a full 0.5 m LiDAR mosaic (~4000x4000) is slow (~350 s),
#' which would freeze the Shiny loop if run synchronously in the comparator. But
#' the foretaccess / QGIS-RVT pipeline often persists the CVAT next to the DEM
#' (`<base>_CVAT_8bit[_foretaccess].tif`). When present, adopt it directly -
#' instant, and the reference rendering. 8-bit `[0, 255]` is rescaled to the
#' `[0, 1]` the grey renderer expects (a float source is passed through).
#'
#' @param mnt_path Path to the source DEM.
#' @return A `[0, 1]` `SpatRaster`, or `NULL` when no precomputed CVAT exists.
#' @noRd
#' Chemin canonique du CVAT d'un MNT, qu'il existe ou non
#'
#' Le producteur (`build_cvat_precomputed()`) et les gardes qui interrogent son
#' sidecar ont besoin du MEME chemin, y compris quand le fichier n'existe pas
#' encore - c'est justement le cas ou il faut savoir si une construction a deja
#' ete tentee. `.rvt_precomputed_path()` ne convient pas la : il ne rend que des
#' fichiers existants.
#'
#' @param mnt_path Chemin du MNT source.
#' @return Chemin du CVAT, sans garantie d'existence.
#' @noRd
.rvt_cvat_out_path <- function(mnt_path) {
  if (is.null(mnt_path) || !nzchar(mnt_path)) return(NULL)
  file.path(dirname(mnt_path),
            paste0(tools::file_path_sans_ext(basename(mnt_path)),
                   "_CVAT_8bit_foretaccess.tif"))
}

.rvt_precomputed_path <- function(mnt_path) {
  if (is.null(mnt_path) || !nzchar(mnt_path)) return(NULL)
  d <- dirname(mnt_path); base <- tools::file_path_sans_ext(basename(mnt_path))
  cand <- c(.rvt_cvat_out_path(mnt_path),
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
#' DEM - in which case the comparator can paint the relief synchronously. `FALSE`
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

  # CVAT pre-calcule (instantane) > CVAT live foretaccess > rvt-py > terra.
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

#' Sidecar de provenance d'un CVAT : pour quels parametres a-t-il ete construit ?
#'
#' `.cvat_covers()` seul ne suffit pas a decider s'il faut reconstruire. Sur une
#' AOI dont la couverture LiDAR HD s'arrete avant `aoi + buffer`,
#' `acquire_mnt()` rend une emprise plus courte que demandee - mesure sur Dabo :
#' 4454 x 4162 m produits pour 4617 x 4381 m demandes. Le CVAT resultant echoue
#' donc `.cvat_covers()` **quoi qu'on fasse**, et l'observe le reconstruit a
#' chaque entree dans l'onglet : boucle infinie sur un calcul de plusieurs
#' minutes.
#'
#' Le sidecar enregistre ce qui a ete DEMANDE (bbox AOI, buffer, resolution). On
#' ne relance donc pas une construction deja tentee avec les memes parametres,
#' que la donnee source ait pu couvrir l'emprise ou non. Meme esprit que les
#' sidecars de provenance de `foretaccess` (spec 027).
#'
#' @param out_path Chemin du CVAT.
#' @return Chemin du sidecar JSON.
#' @noRd
.cvat_sidecar_path <- function(out_path) paste0(out_path, ".build.json")

#' Signature des parametres de construction d'un CVAT
#' @noRd
.cvat_build_signature <- function(aoi, buffer_m, res_m) {
  bb <- tryCatch(as.numeric(sf::st_bbox(aoi)), error = function(e) NULL)
  list(
    bbox = if (is.null(bb)) NULL else round(bb, 1),
    buffer_m = as.numeric(buffer_m),
    res_m = as.numeric(res_m)
  )
}

#' Le sidecar porte-t-il exactement cette signature ?
#'
#' Facteur commun de `.cvat_built_for()` et `.cvat_failed_for()` : les deux
#' posent la meme question de correspondance et ne different que par le statut
#' attendu.
#'
#' @param out_path Chemin du CVAT.
#' @param aoi,buffer_m,res_m Parametres demandes.
#' @return Le sidecar lu (liste) si la signature correspond, sinon `NULL`.
#' @noRd
.cvat_sidecar_match <- function(out_path, aoi, buffer_m, res_m) {
  if (is.null(out_path)) return(NULL)
  sc <- .cvat_sidecar_path(out_path)
  if (!file.exists(sc)) return(NULL)
  prev <- tryCatch(jsonlite::fromJSON(sc), error = function(e) NULL)
  if (!is.list(prev)) return(NULL)
  want <- .cvat_build_signature(aoi, buffer_m, res_m)
  ok <- isTRUE(identical(as.numeric(prev$buffer_m), want$buffer_m) &&
               identical(as.numeric(prev$res_m), want$res_m) &&
               !is.null(prev$bbox) && !is.null(want$bbox) &&
               isTRUE(all.equal(as.numeric(prev$bbox), want$bbox,
                                tolerance = 1e-6)))
  if (ok) prev else NULL
}

#' Un CVAT a-t-il deja ete construit AVEC SUCCES pour ces parametres ?
#'
#' `TRUE` seulement si le raster existe ET que son sidecar porte exactement la
#' meme signature ET que cette tentative avait abouti. Un CVAT sans sidecar
#' (anterieur a ce mecanisme) est considere comme a reconstruire - une fois,
#' puisque la construction ecrit le sidecar.
#'
#' Un sidecar d'ECHEC ne compte pas : sans ce test, un echec enregistre ferait
#' passer une construction ratee pour une construction faite.
#'
#' @noRd
.cvat_built_for <- function(out_path, aoi, buffer_m, res_m) {
  if (is.null(out_path) || !file.exists(out_path)) return(FALSE)
  prev <- .cvat_sidecar_match(out_path, aoi, buffer_m, res_m)
  if (is.null(prev)) return(FALSE)
  !identical(as.character(prev$statut %||% "ok"), "echec")
}

#' Combien de temps un echec de construction reste-t-il opposable ?
#'
#' Un echec est memorise pour ne pas relancer sans fin une construction qui vient
#' d'echouer (une acquisition WMS peut couter des dizaines de minutes). Mais une
#' panne de service est TRANSITOIRE : la memoriser indefiniment desactiverait le
#' CVAT pour toujours. Six heures : assez pour couvrir une session de travail,
#' assez court pour qu'un incident amont se rattrape le lendemain.
#' @noRd
CVAT_ECHEC_TTL_S <- 6 * 3600

#' Une construction a-t-elle DEJA ECHOUE recemment pour ces parametres ?
#'
#' Contrepartie de `.cvat_built_for()`. Ne depend PAS de l'existence du raster :
#' c'est justement quand il n'existe pas qu'on doit se souvenir d'avoir essaye.
#'
#' @noRd
.cvat_failed_for <- function(out_path, aoi, buffer_m, res_m,
                             ttl_s = CVAT_ECHEC_TTL_S) {
  prev <- .cvat_sidecar_match(out_path, aoi, buffer_m, res_m)
  if (is.null(prev)) return(FALSE)
  if (!identical(as.character(prev$statut %||% "ok"), "echec")) return(FALSE)
  quand <- suppressWarnings(as.POSIXct(prev$built_at %||% NA_character_,
                                       tz = "", format = "%Y-%m-%d %H:%M:%S"))
  if (!is.finite(as.numeric(quand))) return(TRUE)   # date illisible : opposable
  as.numeric(difftime(Sys.time(), quand, units = "secs")) < ttl_s
}

#' Ecrit le sidecar de provenance d'une tentative de construction
#'
#' `echec = TRUE` enregistre une tentative INFRUCTUEUSE. C'est indispensable :
#' sans trace d'echec, l'observe de l'onglet relance la meme construction a
#' chaque entree - boucle infinie sur un calcul de plusieurs dizaines de minutes,
#' exactement le mode d'echec que le sidecar existe pour supprimer.
#'
#' @noRd
.cvat_write_sidecar <- function(out_path, aoi, buffer_m, res_m, echec = FALSE) {
  tryCatch({
    sig <- .cvat_build_signature(aoi, buffer_m, res_m)
    sig$statut <- if (isTRUE(echec)) "echec" else "ok"
    sig$built_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    jsonlite::write_json(sig, .cvat_sidecar_path(out_path), auto_unbox = TRUE)
  }, error = function(e) invisible(NULL))
  invisible(out_path)
}

#' Plafonne le buffer du pre-calcul CVAT a ce que le MNT local couvre
#'
#' Au-dela de la marge disponible autour de l'AOI dans la mosaique LiDAR,
#' `foretaccess::build_cvat_precomputed()` RE-ACQUIERT le MNT par le WMS IGN -
#' mesure sur Reconfort : une fenetre WMS France entiere, plusieurs dizaines de
#' minutes, pour un fond de relief que l'affichage re-agrege de toute facon a
#' ~2000 px. Le jeu n'en vaut pas la chandelle.
#'
#' Mesure Reconfort (2026-08-13) : mosaique 5000 x 5000 m, AOI 3285 x 3509 m,
#' marges O 894 / E 821 / S 864 / N 627 m -> tout buffer > 627 m declenchait le
#' WMS.
#'
#' Si la mosaique ne couvre MEME PAS l'AOI (marge negative), on ne plafonne pas :
#' la re-acquisition est alors legitime, c'est le seul moyen d'avoir un fond.
#'
#' @param mnt_path Chemin de la mosaique MNT locale.
#' @param aoi AOI (`sf`/`sfc`).
#' @param buffer_m Buffer demande (m).
#' @return Le buffer effectif (m), au plus `buffer_m`.
#' @noRd
.cvat_buffer_plafonne <- function(mnt_path, aoi, buffer_m) {
  b <- suppressWarnings(as.numeric(buffer_m))
  if (!isTRUE(is.finite(b)) || b <= 0) return(b)
  if (!requireNamespace("terra", quietly = TRUE)) return(b)
  marge <- tryCatch({
    r <- terra::rast(mnt_path)
    e <- terra::ext(r)
    bb <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(aoi)),
                                       terra::crs(r)))
    min(as.numeric(bb["xmin"]) - e[1], e[2] - as.numeric(bb["xmax"]),
        as.numeric(bb["ymin"]) - e[3], e[4] - as.numeric(bb["ymax"]))
  }, error = function(e) NA_real_)
  if (!isTRUE(is.finite(marge)) || marge < 0) return(b)   # pas de plafond
  min(b, floor(marge))
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
#' For the UI label (" CVAT (relief archeo) " vs " VAT " vs " relief ombre ").
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
#' it is given (`.chemin_cache()`), so passing `<projet>/cache/layers` - the
#' directory holding `lidar_mnt_mosaic.tif` - produced a duplicated
#' `cache/layers/layers/mnt/mnt.tif`. We therefore hand it the PARENT
#' (`<projet>/cache`) whenever the file sits directly in `cache/layers`, so its
#' re-acquired DEM lands in `cache/layers/mnt/` alongside our own subfolders
#' (`sentinel2/`, `lidar_mnt/`, ...). Any other location is passed through
#' unchanged (tests, tempdirs) - never climb above a directory we don't own.
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
#' 8-bit, next to the DEM - the file `.rvt_precomputed_path()` adopts first (so
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
  out <- .rvt_cvat_out_path(mnt_path)
  # Avec une AOI : le CVAT doit COUVRIR aoi+buffer. Ni notre code ni foretaccess
  # ne re-verifient un `out` deja present (sauf overwrite) - un buffer agrandi
  # laisserait donc un CVAT trop court, non detecte. On verifie la couverture ici
  # et on FORCE le recalcul si insuffisant. foretaccess re-acquiert le MNT si sa
  # mosaique est trop courte, puis recalcule.
  if (!is.null(aoi) &&
      exists("build_cvat_precomputed", where = asNamespace("foretaccess"))) {
    res_m <- suppressWarnings(as.numeric(APP_CONFIG$cvat_res_m))
    if (!isTRUE(is.finite(res_m)) || res_m <= 0) res_m <- 2
    # PLAFOND : au-dela de ce que la mosaique locale couvre, foretaccess
    # re-acquiert le MNT par WMS IGN - des dizaines de minutes pour un fond
    # re-agrege a 2000 px de toute facon (cf. `.cvat_buffer_plafonne`).
    buffer_eff <- .cvat_buffer_plafonne(mnt_path, aoi, buffer_m)

    # Reprise : soit le CVAT couvre deja aoi+buffer, soit il a DEJA ete construit
    # pour exactement ces parametres. Le second cas est indispensable - sur une
    # AOI ou la couverture LiDAR HD s'arrete avant aoi+buffer, `.cvat_covers()`
    # ne peut jamais etre satisfait et l'observe relancerait sans fin (cf.
    # `.cvat_built_for`).
    if (!isTRUE(overwrite) && file.exists(out) &&
        (isTRUE(.cvat_covers(out, aoi, buffer_eff)) ||
         isTRUE(.cvat_built_for(out, aoi, buffer_eff, res_m)))) {
      return(out)
    }
    # Une tentative RECENTE a deja echoue avec ces parametres : ne pas la rejouer.
    # Sans ce test, un echec relance le calcul a chaque entree dans l'onglet.
    if (!isTRUE(overwrite) && .cvat_failed_for(out, aoi, buffer_eff, res_m)) {
      return(if (file.exists(out)) out else NULL)
    }

    # CONSTRUCTION ATOMIQUE. On ecrivait naguere directement dans `out` avec
    # `overwrite = TRUE` : un echec DETRUISAIT un CVAT valide sans rien mettre a
    # la place (constate sur Reconfort - relief affichable perdu, puis rebati en
    # boucle). On construit donc a cote, et on ne remplace qu'une fois le
    # resultat en main.
    tmp <- paste0(out, ".tmp.tif")
    unlink(tmp)
    built <- tryCatch(
      foretaccess::build_cvat_precomputed(
        aoi = aoi, cache_dir = .foretaccess_cache_root(mnt_path),
        buffer_m = buffer_eff,
        # `res_lidar_m` : le defaut coeur (0,5 m) fait ~81 M cellules sur
        # l'emprise de Dabo et sature la memoire. Le rendu re-agrege a 2000 px,
        # donc cette finesse est perdue a l'affichage (cf. APP_CONFIG$cvat_res_m).
        res_lidar_m = res_m,
        mnt_existant = mnt_path, out = tmp, overwrite = TRUE),
      error = function(e) NULL)
    if (is.null(built) || !file.exists(tmp)) {
      unlink(tmp)
      # Echec MEMORISE, et l'ancien CVAT - s'il y en avait un - est intact.
      .cvat_write_sidecar(out, aoi, buffer_eff, res_m, echec = TRUE)
      return(if (file.exists(out)) out else NULL)
    }
    if (!isTRUE(tryCatch(file.rename(tmp, out), error = function(e) FALSE))) {
      unlink(tmp)
      .cvat_write_sidecar(out, aoi, buffer_eff, res_m, echec = TRUE)
      return(if (file.exists(out)) out else NULL)
    }
    # Sidecar ecrit meme si la couverture reste partielle : il memorise ce qui a
    # ete DEMANDE, ce qui suffit a ne pas retenter la meme construction.
    .cvat_write_sidecar(out, aoi, buffer_eff, res_m)
    return(out)
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

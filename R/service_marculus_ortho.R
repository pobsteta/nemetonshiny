# service_marculus_ortho.R - fond orthophoto 20 cm des GeoPackages Marculus
#
# Marculus prend la PREMIERE table de tuiles d'un GeoPackage comme fond de
# carte hors ligne, et la reprojette en Web Mercator au premier affichage
# (`marculus/docs/specs/couches-gpkg.md`, section Ortho). Une seule table donc,
# et deja dans la grille Web Mercator standard : la reprojection du telephone
# devient alors quasi triviale.
#
# Le fond est celui de CHAQUE chantier (parcelles de l'UGF + 50 m), pas celui
# du projet : l'ortho entiere pese des gigaoctets. Il coute environ 40 s et
# 11 Mo pour 64 ha (mesure sur « Reconfort », parcelle 1116) - trop pour le
# telechargement, qui est synchrone. D'ou la separation :
#
#   * la PREPARATION (`marculus_ortho_preparer()`) tourne dans un worker
#     `future`, lancee par le module au clic, et ecrit un GeoPackage de tuiles
#     par UGF dans `cache/layers/ortho_marculus/` ;
#   * l'EXPORT (`marculus_export_bundle()`) copie ce fond en cache comme base de
#     chaque GeoPackage de chantier, puis y ajoute les couches vecteur. Il reste
#     rapide.
#
# Le nom du fichier en cache porte une empreinte de l'emprise alignee sur la
# grille du zoom 19 : une UGF redecoupee change d'empreinte, son ancien fond
# n'est plus lu.


# ---- Constantes -------------------------------------------------------

#' Parameters of the Marculus orthophoto basemap
#'
#' `HR.ORTHOIMAGERY.ORTHOPHOTOS` is the IGN 20 cm natural-colour orthophoto on
#' the Geoplateforme WMS. Zoom 19 is the Web Mercator level closest to 20 cm at
#' French latitudes (0.2986 m at the equator, ~0.20 m at 48 degrees N). The WMS
#' caps an image at 5 010 px a side, hence 4 000 px request tiles.
#'
#' @noRd
MARCULUS_ORTHO <- list(
  wms        = "https://data.geopf.fr/wms-r/wms",
  couche     = "HR.ORTHOIMAGERY.ORTHOPHOTOS",
  zoom       = 19L,
  marge_m    = 50,
  dalle_px   = 4000L,
  table      = "ortho",
  qualite    = 80L,
  paralleles = 4L,
  version    = 1L
)

# Resolution d'un pixel Web Mercator au zoom z (m).
.marculus_ortho_res <- function(zoom = MARCULUS_ORTHO$zoom) {
  2 * pi * 6378137 / 256 / 2^zoom
}


# ---- Emprise et cache -------------------------------------------------

#' Outline of a context's basemap, aligned on the zoom grid
#'
#' Parcels of the context, repaired in Lambert-93 (a self-crossing ring makes
#' s2 reject the union - see `.marculus_aoi()`), buffered by 50 m, then taken
#' to Web Mercator and snapped outward on the pixel grid of the target zoom:
#' the WMS then returns pixels that the tile writer does not resample.
#'
#' @param parcelles An `sf` of the context's parcels.
#' @return Numeric `c(xmin, ymin, xmax, ymax)` in EPSG:3857, or `NULL`.
#' @noRd
.marculus_ortho_bbox <- function(parcelles) {
  if (!inherits(parcelles, "sf") || nrow(parcelles) == 0L) return(NULL)
  tryCatch({
    g <- sf::st_make_valid(sf::st_transform(sf::st_geometry(parcelles), 2154))
    z <- sf::st_buffer(sf::st_union(g), MARCULUS_ORTHO$marge_m)
    bb <- as.numeric(sf::st_bbox(sf::st_transform(z, 3857)))
    res <- .marculus_ortho_res()
    o <- -pi * 6378137
    c(o + floor((bb[1:2] - o) / res) * res,
      o + ceiling((bb[3:4] - o) / res) * res)
  }, error = function(e) NULL)
}

#' Cache path of a context's basemap
#'
#' @param project_path Character. Project directory.
#' @param ug_id Character. The UGF the context works on.
#' @param bbox The aligned outline from [.marculus_ortho_bbox()].
#' @return Character path (the file may not exist yet).
#' @noRd
.marculus_ortho_chemin <- function(project_path, ug_id, bbox) {
  cle <- rlang::hash(list(round(bbox, 3), MARCULUS_ORTHO$couche,
                          MARCULUS_ORTHO$zoom, MARCULUS_ORTHO$version))
  file.path(project_path, "cache", "layers", "ortho_marculus",
            sprintf("%s_%s.gpkg", gsub("[^A-Za-z0-9_-]+", "_", ug_id),
                    substr(cle, 1L, 12L)))
}

#' Basemap jobs of a project: one per UGF carrying a marking context
#'
#' Several actions on the same UGF (a clear-cut and a respacing on parcel
#' 1117, say) share one basemap.
#'
#' @param project The loaded project.
#' @param actions Eligible actions ([marculus_eligible_actions()]).
#' @return A list of `list(ug_id, bbox, chemin)`, one per distinct UGF whose
#'   outline could be computed.
#' @noRd
marculus_ortho_travaux <- function(project, actions) {
  path <- project$path %||% project$project_path
  if (is.null(path)) return(list())
  ugs <- unique(vapply(actions, function(a) a$ug_id %||% NA_character_,
                       character(1)))
  ugs <- ugs[!is.na(ugs)]
  out <- lapply(ugs, function(ug) {
    bb <- .marculus_ortho_bbox(.marculus_parcelles(project, ug))
    if (is.null(bb)) return(NULL)
    list(ug_id = ug, bbox = bb, chemin = .marculus_ortho_chemin(path, ug, bb))
  })
  Filter(Negate(is.null), out)
}

#' Basemap jobs whose tiles are not cached yet
#'
#' @inheritParams marculus_ortho_travaux
#' @return The subset of [marculus_ortho_travaux()] with no file on disk.
#' @noRd
marculus_ortho_manquants <- function(project, actions) {
  Filter(function(t) !file.exists(t$chemin),
         marculus_ortho_travaux(project, actions))
}


# ---- Construction -----------------------------------------------------

#' WMS request tiles covering an aligned outline
#'
#' @param bbox Aligned outline in EPSG:3857.
#' @return A list of `list(url, bbox, w, h)`.
#' @noRd
.marculus_ortho_dalles <- function(bbox) {
  res <- .marculus_ortho_res()
  pas <- MARCULUS_ORTHO$dalle_px * res
  xs <- seq(bbox[1], bbox[3] - res / 2, by = pas)
  ys <- seq(bbox[2], bbox[4] - res / 2, by = pas)
  out <- list()
  for (x in xs) for (y in ys) {
    x2 <- min(x + pas, bbox[3]); y2 <- min(y + pas, bbox[4])
    w <- round((x2 - x) / res); h <- round((y2 - y) / res)
    if (w < 1L || h < 1L) next
    b <- c(x, y, x2, y2)
    out[[length(out) + 1L]] <- list(
      bbox = b, w = w, h = h,
      url = paste0(
        MARCULUS_ORTHO$wms,
        "?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap",
        "&LAYERS=", MARCULUS_ORTHO$couche, "&STYLES=",
        "&CRS=EPSG:3857&BBOX=", paste(sprintf("%.6f", b), collapse = ","),
        "&WIDTH=", w, "&HEIGHT=", h, "&FORMAT=image/jpeg"))
  }
  out
}

#' Build the tile GeoPackage of one basemap
#'
#' Downloads the WMS tiles (in parallel, a handful at a time), georeferences
#' each JPEG with a world file, mosaics them in a VRT, writes one
#' `GoogleMapsCompatible` JPEG tile table and its lower zoom levels. The file
#' lands in the cache by an atomic rename: an interrupted build never leaves a
#' half-written basemap that the export would then ship.
#'
#' @param bbox Aligned outline in EPSG:3857.
#' @param chemin Destination path.
#' @return `chemin` on success, `NULL` otherwise (warned).
#' @noRd
marculus_ortho_construire <- function(bbox, chemin) {
  tmp <- tempfile("ortho_marculus_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  tryCatch({
    dalles <- .marculus_ortho_dalles(bbox)
    fichiers <- file.path(tmp, sprintf("d%03d.jpg", seq_along(dalles)))
    reqs <- lapply(dalles, function(d) {
      httr2::req_timeout(httr2::request(d$url), 300)
    })
    resps <- httr2::req_perform_parallel(
      reqs, paths = fichiers, on_error = "continue", progress = FALSE,
      max_active = MARCULUS_ORTHO$paralleles)
    ok <- vapply(resps, function(r) {
      inherits(r, "httr2_response") && httr2::resp_status(r) == 200L
    }, logical(1)) & file.exists(fichiers) & file.size(fichiers) > 1000
    if (!all(ok)) {
      cli::cli_abort("{sum(!ok)} dalle(s) WMS sur {length(ok)} en \u00e9chec.")
    }
    res <- .marculus_ortho_res()
    for (i in seq_along(dalles)) {
      b <- dalles[[i]]$bbox
      writeLines(sprintf("%.10f\n0\n0\n%.10f\n%.10f\n%.10f",
                         res, -res, b[1] + res / 2, b[4] - res / 2),
                 sub("\\.jpg$", ".jgw", fichiers[i]))
    }
    vrt <- file.path(tmp, "ortho.vrt")
    sf::gdal_utils("buildvrt", fichiers, vrt,
                   options = c("-a_srs", "EPSG:3857"), quiet = TRUE)
    dir.create(dirname(chemin), recursive = TRUE, showWarnings = FALSE)
    # `.part.gpkg` et non `.gpkg.part` : GDAL exige l'extension .gpkg.
    part <- sub("\\.gpkg$", ".part.gpkg", chemin)
    unlink(part)
    sf::gdal_utils("translate", vrt, part, quiet = TRUE, options = c(
      "-of", "GPKG",
      "-co", paste0("RASTER_TABLE=", MARCULUS_ORTHO$table),
      "-co", "TILING_SCHEME=GoogleMapsCompatible",
      "-co", "ZOOM_LEVEL_STRATEGY=UPPER",
      "-co", "TILE_FORMAT=JPEG",
      "-co", paste0("QUALITY=", MARCULUS_ORTHO$qualite)),
      config_options = c(OGR_SQLITE_SYNCHRONOUS = "OFF"))
    # Niveaux inferieurs : sans eux, le fond disparait des qu'on dezoome.
    sf::gdal_addo(part, overviews = c(2, 4, 8, 16, 32, 64), method = "AVERAGE",
                  options = c(TABLE = MARCULUS_ORTHO$table),
                  config_options = c(OGR_SQLITE_SYNCHRONOUS = "OFF"))
    if (!.marculus_ortho_matrices_pleines(part)) {
      cli::cli_abort("Matrices de tuiles vides non retir\u00e9es.")
    }
    if (!file.rename(part, chemin)) cli::cli_abort("Renommage impossible.")
    chemin
  }, error = function(e) {
    cli::cli_warn("Fond ortho non pr\u00e9par\u00e9 : {conditionMessage(e)}")
    unlink(sub("\\.gpkg$", ".part.gpkg", chemin))
    NULL
  })
}

#' Drop the tile matrices that hold no tile
#'
#' @description
#' With `TILING_SCHEME=GoogleMapsCompatible`, GDAL declares a tile matrix for
#' EVERY zoom level from 0 to the native one (19), while tiles only exist from
#' the lowest overview up (13). Marculus reprojects the table on first display
#' (`TileReprojection.reproject()`, NGA geopackage 6.7.4), which walks every
#' matrix: on an empty one `TileDao.getBoundingBox(zoom)` is `null`, and
#' `TileBoundingBoxUtils.getTileGrid()` dereferences it - a
#' `NullPointerException` that `ouvrirOrtho()` swallows, so "Ortho" never
#' appears on the phone (v0.146.x). Reproduced with the desktop NGA library on
#' a "Reconfort" basemap; once the 13 empty matrices are gone, the same call
#' reprojects its 106 tiles, zooms 13 to 19.
#'
#' Needs `RSQLite` (Suggests). Without it the basemap is NOT shipped: a tile
#' table the phone cannot open is worse than none.
#'
#' @param gpkg Path to the basemap GeoPackage.
#' @return `TRUE` when every remaining matrix holds tiles.
#' @noRd
.marculus_ortho_matrices_pleines <- function(gpkg) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
      !requireNamespace("RSQLite", quietly = TRUE)) {
    cli::cli_warn("Fond ortho : {.pkg RSQLite} requis pour un GeoPackage lisible par Marculus.")
    return(FALSE)
  }
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), gpkg)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    table <- MARCULUS_ORTHO$table
    DBI::dbExecute(con, sprintf(
      "DELETE FROM gpkg_tile_matrix WHERE table_name = '%s' AND zoom_level NOT IN (SELECT DISTINCT zoom_level FROM \"%s\")",
      table, table))
    vides <- DBI::dbGetQuery(con, sprintf(
      "SELECT COUNT(*) AS n FROM gpkg_tile_matrix WHERE table_name = '%s' AND zoom_level NOT IN (SELECT DISTINCT zoom_level FROM \"%s\")",
      table, table))$n
    identical(as.integer(vides), 0L)
  }, error = function(e) {
    cli::cli_warn("Fond ortho : {conditionMessage(e)}")
    FALSE
  })
}

#' Prepare the missing basemaps of a set of jobs
#'
#' The worker-side entry point: plain arguments in, a count out - nothing
#' reactive crosses the process boundary.
#'
#' @param travaux Jobs from [marculus_ortho_manquants()].
#' @return A list: `n_total`, `n_ok`.
#' @noRd
marculus_ortho_preparer <- function(travaux) {
  n_ok <- 0L
  for (t in travaux) {
    if (file.exists(t$chemin) ||
        !is.null(marculus_ortho_construire(t$bbox, t$chemin))) {
      n_ok <- n_ok + 1L
    }
  }
  list(n_total = length(travaux), n_ok = n_ok)
}

#' Repair basemaps cached by v0.146.0 / v0.146.1
#'
#' Those still carry the empty tile matrices that make Marculus fail (see
#' [.marculus_ortho_matrices_pleines()]). Cheap - one SQL statement per file,
#' no download - so the export runs it on every basemap it ships; a basemap
#' that cannot be repaired is left out rather than shipped broken.
#'
#' @param chemin Path to a cached basemap.
#' @return `chemin`, or `NULL` when it could not be made readable.
#' @noRd
.marculus_ortho_reparer <- function(chemin) {
  if (is.null(chemin) || !file.exists(chemin)) return(NULL)
  if (.marculus_ortho_matrices_pleines(chemin)) chemin else NULL
}

# service_nuage_points.R - nuage de points de drone -> MNT, MNS, MNH (spec 059)
#
# L'app ne traite rien elle-meme : elle range le nuage dans le cache du projet,
# choisit les references LiDAR HD pour la photogrammetrie et appelle
# `nemeton::traiter_nuage_points()` (coeur >= 2.1.0). Range dans
# `cache/layers/drone_nuage/`, le nuage donne `drone_mnt|mns|mnh/`, que
# `resolve_project_dem()` / `resolve_project_chm()` rendent ensuite en premier
# et que `detect_ndp_from_cache()` compte comme NDP 2.

# Extensions acceptees pour un nuage de points (.las, .laz, .copc.laz).
.NUAGE_EXTENSIONS <- "\\.(las|laz)$"

#' Directory holding a project's drone point cloud
#' @param project_path Project directory.
#' @return Path (not created).
#' @noRd
nuage_dossier <- function(project_path) {
  file.path(project_path, "cache", "layers", "drone_nuage")
}

#' Point cloud files already stored in a project
#' @noRd
nuage_fichiers <- function(project_path) {
  d <- nuage_dossier(project_path)
  if (!dir.exists(d)) return(character(0))
  list.files(d, pattern = .NUAGE_EXTENSIONS, full.names = TRUE,
             ignore.case = TRUE)
}

#' Store uploaded point cloud files in the project cache
#'
#' Files that are not `.las` / `.laz` are refused. A file with the same name
#' replaces the previous one.
#'
#' @param project_path Project directory.
#' @param chemins Character. Paths of the uploaded files (Shiny `datapath`).
#' @param noms Character. Original file names.
#' @return A list: `deposes` (stored paths), `refuses` (original names).
#' @noRd
nuage_deposer <- function(project_path, chemins, noms = basename(chemins)) {
  ok <- grepl(.NUAGE_EXTENSIONS, noms, ignore.case = TRUE)
  d <- nuage_dossier(project_path)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  # Le nom d'origine, sans chemin : jamais d'ecriture hors du dossier.
  cibles <- file.path(d, basename(noms[ok]))
  copie <- file.copy(chemins[ok], cibles, overwrite = TRUE)
  list(deposes = cibles[copie], refuses = c(noms[!ok], noms[ok][!copie]))
}

#' LiDAR HD references of a project, for a photogrammetric cloud
#'
#' A photogrammetric cloud has no ground under the canopy: its DTM comes from
#' the project's LiDAR HD DTM, and its vertical shift is measured on bare
#' ground, where the LiDAR HD CHM is under 0.5 m. Only rasters with valid
#' pixels are kept (the IGN serves NoData-only tiles before publication).
#' Order: published mosaic, then the lasR derivative of the COPC cloud; the
#' DTM falls back on BD ALTI.
#'
#' @param project_path Project directory.
#' @return `list(mnt = path or NULL, mnh = path or NULL)`.
#' @noRd
nuage_references_lidar <- function(project_path) {
  couches <- file.path(project_path, "cache", "layers")
  premier <- function(cands) {
    for (f in cands) {
      if (file.exists(f) && .raster_part_valide(f) >= .RASTER_PART_MIN) return(f)
    }
    NULL
  }
  list(
    mnt = premier(file.path(couches, c("lidar_mnt_mosaic.tif",
                                        file.path("lidar_mnt", "dtm.tif"),
                                        "dem.tif"))),
    mnh = premier(file.path(couches, c("lidar_mnh_mosaic.tif",
                                        file.path("lidar_mnh", "chm.tif")))))
}

#' Process a project's drone point cloud into DTM, DSM and CHM
#'
#' @param project_path Project directory.
#' @param type `"lidar_drone"` or `"photogrammetrie"`.
#' @param ncores Integer. Files processed concurrently.
#' @return A list: `status` (`"ok"`, `"sans_nuage"`, `"sans_mnt"`, `"error"`),
#'   `mnt`, `mns`, `mnh` (paths), `qualite`, `classes`, `avertissements`
#'   (messages of the core's warnings), `message` on error.
#' @noRd
nuage_traiter <- function(project_path, type = c("lidar_drone", "photogrammetrie"),
                          ncores = .lasr_ncores()) {
  type <- match.arg(type)
  if (length(nuage_fichiers(project_path)) == 0L) {
    return(list(status = "sans_nuage"))
  }
  refs <- if (identical(type, "photogrammetrie")) {
    nuage_references_lidar(project_path)
  } else list()
  if (identical(type, "photogrammetrie") && is.null(refs$mnt)) {
    return(list(status = "sans_mnt"))
  }
  avert <- character(0)
  res <- tryCatch(
    withCallingHandlers(
      nemeton::traiter_nuage_points(
        nuage_dossier(project_path), type = type,
        mnt_externe = refs$mnt, mnh_reference = refs$mnh,
        dossier = file.path(project_path, "cache", "layers"),
        ncores = ncores),
      warning = function(w) {
        avert <<- c(avert, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
    error = function(e) list(status = "error", message = conditionMessage(e)))
  if (identical(res$status, "error")) return(res)
  out <- list(status = "ok", type = type, mnt = res$mnt, mns = res$mns,
              mnh = res$mnh, qualite = res$qualite, classes = res$classes,
              avertissements = unique(avert),
              reference_mnt = refs$mnt, reference_mnh = refs$mnh,
              date = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  .nuage_sauver_bilan(project_path, out)
  out
}

#' Record the last processing, to show it again when the project reopens
#' @noRd
.nuage_sauver_bilan <- function(project_path, res) {
  f <- file.path(nuage_dossier(project_path), "traitement.json")
  bilan <- res[c("type", "mnt", "mns", "mnh", "qualite", "avertissements",
                 "reference_mnt", "reference_mnh", "date")]
  tryCatch(.write_json_atomic(bilan, f, auto_unbox = TRUE, null = "null",
                              na = "null", digits = NA),
           error = function(e) NULL)
  invisible(f)
}

#' Last processing of a project's drone cloud, if its rasters still exist
#'
#' @param project_path Project directory.
#' @return The recorded list (see [nuage_traiter()]) with `status = "ok"`, or
#'   `NULL`.
#' @noRd
nuage_dernier_traitement <- function(project_path) {
  if (is.null(project_path)) return(NULL)
  f <- file.path(nuage_dossier(project_path), "traitement.json")
  if (!file.exists(f)) return(NULL)
  b <- tryCatch(jsonlite::read_json(f, simplifyVector = TRUE),
                error = function(e) NULL)
  if (is.null(b) || !all(vapply(c(b$mns, b$mnh), file.exists, logical(1)))) {
    return(NULL)
  }
  c(list(status = "ok"), b)
}

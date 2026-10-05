# Ecritures atomiques des fichiers de projet.
#
# Un projet est un dossier sur disque, ecrit par la session ET par des workers
# de calcul qui peuvent etre tues en cours de route (plafond memoire, OOM). Une
# ecriture en place interrompue laissait un `metadata.json` tronque (projet
# " corrompu ", propose a la suppression) ou un GeoPackage supprime avant
# d'etre reecrit. Regle : on ecrit une copie a cote, puis on la renomme sur la
# cible - un renommage dans le meme dossier est atomique sous POSIX.


#' Replace a file by its freshly written temporary copy
#'
#' @description
#' `file.rename()` replaces atomically on POSIX ; on Windows it refuses an
#' existing target, which is then removed first (a short, unavoidable window).
#'
#' @param tmp Path of the written copy, in the same directory as `dest`.
#' @param dest Final path.
#' @return `TRUE` invisibly, or an error.
#' @noRd
.replace_file <- function(tmp, dest) {
  if (file.rename(tmp, dest)) return(invisible(TRUE))
  if (file.exists(dest)) unlink(dest)
  if (!file.rename(tmp, dest)) {
    stop(sprintf("Could not replace %s.", dest), call. = FALSE)
  }
  invisible(TRUE)
}


#' Path of a temporary sibling of a file
#'
#' Same directory (so the final rename stays atomic), same extension (GDAL
#' picks its driver from it), unique name.
#'
#' @param path Final path.
#' @return Character path.
#' @noRd
.tmp_sibling <- function(path) {
  ext <- tools::file_ext(path)
  base <- basename(tempfile(paste0(".", tools::file_path_sans_ext(basename(path)), "-")))
  file.path(dirname(path), if (nzchar(ext)) paste0(base, ".", ext) else base)
}


#' Write JSON atomically
#'
#' @param x Object to serialise.
#' @param path Destination.
#' @param ... Passed to [jsonlite::write_json()].
#' @return `TRUE` invisibly, or an error (the destination is then untouched).
#' @noRd
.write_json_atomic <- function(x, path, ...) {
  tmp <- .tmp_sibling(path)
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  jsonlite::write_json(x, tmp, ...)
  .replace_file(tmp, path)
}


#' Write a single-layer GeoPackage atomically
#'
#' @description
#' Replaces the `unlink(path)` + `st_write(path)` pattern, which left NO file
#' when the write failed.
#'
#' @param x `sf` object.
#' @param path Destination `.gpkg`.
#' @param ... Passed to [sf::st_write()].
#' @return `TRUE` invisibly, or an error (the destination is then untouched).
#' @noRd
.st_write_atomic <- function(x, path, ...) {
  tmp <- .tmp_sibling(path)
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  # Nom de couche : celui du fichier FINAL (GDAL le deduirait sinon du nom
  # temporaire, qui commence par un point et qu'il refuse).
  args <- list(...)
  if (is.null(args$layer)) args$layer <- tools::file_path_sans_ext(basename(path))
  do.call(sf::st_write, c(list(obj = x, dsn = tmp, driver = "GPKG", quiet = TRUE), args))
  .replace_file(tmp, path)
}


#' Write a secret JSON file, owner-only from its creation
#'
#' The file used to be written with the default umask and only then passed to
#' 0600: during that instant another local account could read the keys. The
#' umask is tightened to 077 for the write (temporary sibling, then atomic
#' replace), so the file never exists with wider permissions.
#'
#' @param x Object to serialise.
#' @param path Destination path.
#' @param ... Passed to [jsonlite::write_json()].
#' @return Invisible `path`.
#' @noRd
.write_json_private <- function(x, path, ...) {
  old <- Sys.umask("077")
  on.exit(Sys.umask(old), add = TRUE)
  .write_json_atomic(x, path, ...)
  tryCatch(Sys.chmod(path, mode = "0600"), error = function(e) NULL)
  invisible(path)
}


#' Write a raster atomically
#'
#' Temporary sibling then rename: a process killed mid-write leaves no
#' truncated file that a later run would take for a valid cache.
#'
#' @param r `SpatRaster`.
#' @param path Final path.
#' @param ... Passed to [terra::writeRaster()].
#' @return Invisible `path`.
#' @noRd
.write_raster_atomic <- function(r, path, ...) {
  tmp <- .tmp_sibling(path)
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  terra::writeRaster(r, tmp, overwrite = TRUE, ...)
  .replace_file(tmp, path)
  invisible(path)
}


#' Schedule a callback that survives the end of its session
#'
#' Drop-in for [later::later()]. Since shiny 1.14, reading a reactive of a
#' module whose session is closed raises `shiny.destroyed.error`. Inside a
#' `later` callback, i.e. outside any reactive context, that error escaped to
#' `runApp()` and **stopped the whole server** - every user's session - when a
#' tab was closed while a project was still restoring (reproduced
#' 2026-10-05). The session is gone, so its callback simply has nothing left to
#' do. Any other error keeps propagating.
#'
#' @param func Callback, no argument.
#' @param delay Seconds, passed to [later::later()].
#' @param loop Event loop, passed to [later::later()].
#' @return The cancellation function returned by [later::later()].
#' @noRd
.later_sur <- function(func, delay = 0, loop = later::current_loop()) {
  force(func)
  later::later(function() {
    tryCatch(func(), shiny.destroyed.error = function(e) invisible(NULL))
  }, delay = delay, loop = loop)
}

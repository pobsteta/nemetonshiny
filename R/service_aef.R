# service_aef.R - source d'embedding AlphaEarth pour N5, B4 et L3 (mission 080)
#
# L'app ne calcule rien : `alphaearthnemeton::aef_fetch()` rend l'embedding
# annuel (64 bandes, 10 m, Lambert-93) sur l'emprise du projet, l'app le pose
# dans `layers$rasters$aef_embedding` et `layers$embedding_source = "aef"`, et le
# coeur fait le reste (N5, B4/L3 en mode embedding, drapeau NDP `embedding_ml`).
#
# Source FACULTATIVE : le paquet n'est qu'en `Suggests` (aucun tag de release a
# ce jour, `Remotes` sans tag) et l'option est eteinte par defaut. Sans l'un ou
# l'autre, rien ne change.

#' Is the `alphaearthnemeton` package installed?
#' @return Logical scalar.
#' @noRd
aef_disponible <- function() {
  requireNamespace("alphaearthnemeton", quietly = TRUE)
}


#' Is the AlphaEarth source switched on for a project?
#'
#' @description
#' OFF by default (absent metadata means disabled): the download is not
#' negligible (about 1 MB per km2, measured), unlike SUFOSAT or LST which are on
#' by default.
#'
#' @param metadata List. A project's `metadata` (or `NULL`).
#' @return Logical scalar.
#' @noRd
project_aef_enabled <- function(metadata) {
  isTRUE(metadata$aef$enabled %||% FALSE)
}


#' Is the AlphaEarth source usable for a project (switched on AND installed)?
#' @param metadata List. A project's `metadata` (or `NULL`).
#' @return Logical scalar.
#' @noRd
project_aef_active <- function(metadata) {
  project_aef_enabled(metadata) && aef_disponible()
}


#' Persist the AlphaEarth source toggle on a project
#'
#' @param project_id Character.
#' @param enabled Logical.
#' @return TRUE on success.
#' @noRd
set_project_aef <- function(project_id, enabled) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }
  enabled <- isTRUE(enabled)
  update_project_metadata(
    project_id,
    list(aef = list(enabled = enabled,
                    set_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))),
    project_path = project_path)
  TRUE
}


#' Directory of the AlphaEarth cache of a project
#' @param project_path Character.
#' @return Character path.
#' @noRd
.aef_cache_dir <- function(project_path) {
  file.path(project_path, "cache", "layers", "aef")
}


#' The `aef_fetch()` function of the package (indirection for the tests)
#' @return A function.
#' @noRd
.aef_fetch_fn <- function() {
  getExportedValue("alphaearthnemeton", "aef_fetch")
}


#' Cache key of an AlphaEarth acquisition: extent (metres, Lambert-93) + CRS
#' @param aoi sf object.
#' @return Character hash.
#' @noRd
.aef_cle <- function(aoi) {
  bb <- sf::st_bbox(sf::st_transform(aoi, 2154))
  rlang::hash(list(round(as.numeric(bb)), "EPSG:2154"))
}


#' Name the layers `A00`..`A63` and tag the raster with its provenance
#' @param r SpatRaster.
#' @param annee Integer year or `NA`.
#' @return SpatRaster.
#' @noRd
.aef_etiqueter <- function(r, annee) {
  attr(r, "embedding_source") <- "aef"
  if (!is.na(annee)) attr(r, "annee") <- as.integer(annee)
  r
}


#' Fetch the AlphaEarth embedding over the project's extent
#'
#' @description
#' Calls `alphaearthnemeton::aef_fetch()` once per extent: the file and its year
#' are recorded in `<project>/cache/layers/aef/courant.json`, and a second call
#' over the same extent reads the COG back without calling the package again
#' (the package has its own cache, but asking it still costs a tile index
#' read). Any failure (network, tile absent, package error) is returned as a
#' reason, never raised: the computation goes on without N5.
#'
#' @param aoi sf. The computation units.
#' @param project_path Character.
#' @return A list: `raster` (SpatRaster or `NULL`), `annee` (integer or `NA`),
#'   `depuis_cache` (logical) and, on failure, `erreur` (character).
#' @noRd
build_aef_layer <- function(aoi, project_path) {
  echec <- function(msg) {
    list(raster = NULL, annee = NA_integer_, depuis_cache = FALSE, erreur = msg)
  }
  if (!aef_disponible()) return(echec("package alphaearthnemeton absent"))

  dir <- .aef_cache_dir(project_path)
  courant <- file.path(dir, "courant.json")
  cle <- tryCatch(.aef_cle(aoi), error = function(e) NULL)
  if (is.null(cle)) return(echec("emprise illisible"))

  # 1. Deja acquis pour cette emprise : relecture sans appel au paquet.
  if (file.exists(courant)) {
    prec <- tryCatch(jsonlite::read_json(courant, simplifyVector = TRUE),
                     error = function(e) NULL)
    fichier <- if (!is.null(prec)) file.path(dir, as.character(prec$fichier))
    if (!is.null(prec) && identical(as.character(prec$cle), cle) &&
        file.exists(fichier)) {
      r <- tryCatch(terra::rast(fichier), error = function(e) NULL)
      if (!is.null(r)) {
        annee <- suppressWarnings(as.integer(prec$annee))
        return(list(raster = .aef_etiqueter(r, annee), annee = annee,
                    depuis_cache = TRUE))
      }
    }
  }

  # 2. Acquisition : l'annee la plus recente disponible (years = NULL).
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  r <- tryCatch(
    .aef_fetch_fn()(aoi, years = NULL, crs = 2154, cache_dir = dir),
    error = function(e) e)
  if (inherits(r, "error")) return(echec(conditionMessage(r)))
  if (is.list(r) && !inherits(r, "SpatRaster")) {
    # Plusieurs annees (hors contrat de years = NULL) : la plus recente.
    if (!length(r) || is.null(names(r))) return(echec("reponse vide"))
    r <- r[[names(r)[which.max(suppressWarnings(as.integer(names(r))))]]]
  }
  if (!inherits(r, "SpatRaster")) return(echec("reponse inattendue"))

  annee <- suppressWarnings(as.integer(attr(r, "annee") %||% NA_integer_))
  src <- terra::sources(r)
  src <- src[nzchar(src)]
  if (length(src) >= 1L && file.exists(src[[1]]) &&
      identical(normalizePath(dirname(src[[1]])), normalizePath(dir))) {
    tryCatch(
      jsonlite::write_json(
        list(cle = cle, fichier = basename(src[[1]]), annee = annee),
        courant, auto_unbox = TRUE, na = "null"),
      error = function(e) NULL)
  }
  list(raster = .aef_etiqueter(r, annee), annee = annee, depuis_cache = FALSE)
}


#' Stage the AlphaEarth source of a computation
#'
#' @description
#' Called by `start_computation()` between the downloads and the indicators.
#' Puts the embedding in `layers$rasters$aef_embedding` and
#' `layers$embedding_source` when it succeeds. When the source is on and the
#' fetch fails, the layer stays absent (N5 then reports
#' `skipped_no_embedding`, B4/L3 keep their other method) and a warning entry
#' is returned for the UI.
#'
#' @param layers `nemeton_layers`.
#' @param aoi sf. The computation units.
#' @param project_path Character.
#' @param metadata List. Project metadata.
#' @return `list(layers = <layers>, warnings = <list>)`.
#' @noRd
.aef_etape <- function(layers, aoi, project_path, metadata) {
  if (!project_aef_active(metadata)) return(list(layers = layers, warnings = list()))

  res <- build_aef_layer(aoi, project_path)
  if (is.null(res$raster)) {
    msg <- res$erreur %||% "inconnue"
    cli::cli_warn("AlphaEarth embedding unavailable: {msg}")
    return(list(
      layers = layers,
      warnings = list(list(
        type = "warning", source = "AlphaEarth",
        key = "aef_fetch_echec", args = list(msg),
        message = paste0("Embedding AlphaEarth non disponible: ", msg),
        time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ))))
  }
  layers$rasters$aef_embedding <- res$raster
  layers$embedding_source <- "aef"
  list(layers = layers, warnings = list())
}

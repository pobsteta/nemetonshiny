# service_aef.R - source d'embedding AlphaEarth pour N5, B4 et L3 (mission 080)
#
# L'app ne calcule rien : `alphaearthnemeton::aef_fetch()` rend l'embedding
# annuel (64 bandes, 10 m, Lambert-93) sur l'emprise du projet, l'app le pose
# dans `layers$rasters$aef_embedding` et `layers$embedding_source = "aef"`, et le
# coeur fait le reste (N5, B4/L3 en mode embedding, drapeau NDP `embedding_ml`).
#
# Source FACULTATIVE : le paquet n'est qu'en `Suggests` (aucun tag de release a
# ce jour sur `main`, `Remotes` en `@*release`) et l'option est eteinte par defaut. Sans l'un ou
# l'autre, rien ne change.

# Seuil d'avertissement (decision de Pascal, 2026-11-26) : au-dela de cette
# emprise, l'app previent avant de telecharger. Taille estimee : ~1 Mo par km2
# (mesure : 14,5 Mo pour 18 km2 en 21 s).
AEF_SEUIL_KM2 <- 100
AEF_MO_PAR_KM2 <- 1

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


#' Read the AlphaEarth embedding back from the cache of a project
#'
#' @description
#' Reads `cache/layers/aef/courant.json` and the COG it names, when they hold
#' the same extent as `aoi`. Never calls the package and never downloads: the
#' package need not be installed.
#'
#' @param aoi sf. The computation units.
#' @param project_path Character.
#' @return `list(raster, annee, depuis_cache = TRUE)`, or `NULL` when the cache
#'   does not hold this extent (or cannot be read).
#' @noRd
.aef_relire_cache <- function(aoi, project_path) {
  dir <- .aef_cache_dir(project_path)
  courant <- file.path(dir, "courant.json")
  if (!file.exists(courant)) return(NULL)
  cle <- tryCatch(.aef_cle(aoi), error = function(e) NULL)
  if (is.null(cle)) return(NULL)
  prec <- tryCatch(jsonlite::read_json(courant, simplifyVector = TRUE),
                   error = function(e) NULL)
  if (is.null(prec) || !identical(as.character(prec$cle), cle)) return(NULL)
  fichier <- file.path(dir, as.character(prec$fichier))
  if (!file.exists(fichier)) return(NULL)
  r <- tryCatch(terra::rast(fichier), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  annee <- suppressWarnings(as.integer(prec$annee))
  list(raster = .aef_etiqueter(r, annee), annee = annee, depuis_cache = TRUE,
       fichier = fichier)
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
  rel <- .aef_relire_cache(aoi, project_path)
  if (!is.null(rel)) return(rel)

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


#' Extent of the computation units, in km2 (bounding box, Lambert-93)
#'
#' The download is asked for the extent, not for the polygons: the box is what
#' is paid for.
#' @param aoi sf object.
#' @return Numeric scalar, `NA` if unreadable.
#' @noRd
.aef_emprise_km2 <- function(aoi) {
  tryCatch({
    bb <- sf::st_bbox(sf::st_transform(aoi, 2154))
    as.numeric((bb[["xmax"]] - bb[["xmin"]]) * (bb[["ymax"]] - bb[["ymin"]])) / 1e6
  }, error = function(e) NA_real_)
}


#' Is the extent already acquired (same key as the cache of the project)?
#' @noRd
.aef_en_cache <- function(aoi, project_path) {
  !is.null(.aef_relire_cache(aoi, project_path))
}


#' Warning before a heavy download (extent above [AEF_SEUIL_KM2])
#'
#' @param aoi sf. The computation units.
#' @param project_path Character.
#' @return A warning entry for the UI (same shape as `aef_fetch_echec`), or
#'   `NULL` when the extent is small, unreadable, or already in the cache (no
#'   download to announce).
#' @noRd
.aef_avertissement_taille <- function(aoi, project_path) {
  km2 <- .aef_emprise_km2(aoi)
  if (is.na(km2) || km2 <= AEF_SEUIL_KM2) return(NULL)
  if (.aef_en_cache(aoi, project_path)) return(NULL)
  args <- list(format(round(km2), big.mark = " ", trim = TRUE),
               format(AEF_SEUIL_KM2, trim = TRUE),
               format(round(km2 * AEF_MO_PAR_KM2), big.mark = " ", trim = TRUE))
  list(type = "warning", source = "AlphaEarth",
       key = "aef_taille_avertissement", args = args,
       message = sprintf(
         "AlphaEarth: extent of %s km2 (above %s km2), download estimated at about %s MB",
         args[[1]], args[[2]], args[[3]]),
       time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}


#' Year of the AlphaEarth embedding used by a project
#'
#' @description
#' Read from the `annee` attribute of the raster when one is given, else from
#' the cache record (`courant.json`). `NA` when unknown (no acquisition yet, or
#' a package that does not tell the year).
#'
#' @param project_path Character or `NULL`.
#' @param raster SpatRaster or `NULL`.
#' @return Integer scalar (possibly `NA`).
#' @noRd
aef_annee <- function(project_path = NULL, raster = NULL) {
  an <- NA_integer_
  if (!is.null(raster)) {
    an <- suppressWarnings(as.integer(attr(raster, "annee")[1]))
  }
  if (is.na(an) && is.character(project_path) && length(project_path) == 1L &&
      !is.na(project_path)) {
    f <- file.path(.aef_cache_dir(project_path), "courant.json")
    if (file.exists(f)) {
      prec <- tryCatch(jsonlite::read_json(f, simplifyVector = TRUE),
                       error = function(e) NULL)
      an <- suppressWarnings(as.integer(prec$annee[1]))
    }
  }
  if (length(an) != 1L) NA_integer_ else an
}


#' Stage the AlphaEarth source of a computation
#'
#' @description
#' Called by `start_computation()` between the downloads and the indicators.
#' Puts the embedding in `layers$rasters$aef_embedding` and
#' `layers$embedding_source` when it succeeds. When the source is on and the
#' fetch fails, the layer stays absent (N5 then reports
#' `skipped_no_embedding`, B4/L3 keep their other method) and a warning entry
#' is returned for the UI. When the extent is above [AEF_SEUIL_KM2] and not yet
#' in the cache, a warning is raised BEFORE the download (through `prevenir`
#' when given, so the UI shows it at once; else it is returned in `warnings`).
#'
#' @param layers `nemeton_layers`.
#' @param aoi sf. The computation units.
#' @param project_path Character.
#' @param metadata List. Project metadata.
#' @param prevenir Function taking a warning entry, or `NULL`.
#' @return `list(layers = <layers>, warnings = <list>)`.
#' @noRd
.aef_etape <- function(layers, aoi, project_path, metadata, prevenir = NULL) {
  if (!project_aef_active(metadata)) return(list(layers = layers, warnings = list()))

  avertissements <- list()
  taille <- .aef_avertissement_taille(aoi, project_path)
  if (!is.null(taille)) {
    if (is.function(prevenir)) prevenir(taille) else avertissements <- list(taille)
  }

  res <- build_aef_layer(aoi, project_path)
  if (is.null(res$raster)) {
    msg <- res$erreur %||% "inconnue"
    cli::cli_warn("AlphaEarth embedding unavailable: {msg}")
    return(list(
      layers = layers,
      warnings = c(avertissements, list(list(
        type = "warning", source = "AlphaEarth",
        key = "aef_fetch_echec", args = list(msg),
        message = paste0("Embedding AlphaEarth non disponible: ", msg),
        time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )))))
  }
  layers$rasters$aef_embedding <- res$raster
  layers$embedding_source <- "aef"
  list(layers = layers, warnings = avertissements)
}


# ---- Etat AlphaEarth d'un calcul : invalidation de B4 / L3 / N5 -----------

# Indicateurs dont le resultat depend de l'etat de la source.
.AEF_INDICATEURS <- c("indicateur_b4_div_spectrale", "indicateur_l3_het_spectrale",
                      "indicateur_n5_distance_reference")

#' AlphaEarth state of a computation, as a string
#' @param layers `nemeton_layers`.
#' @return `"none"` without embedding, else `"aef:<year>"` (year `NA` if unknown).
#' @noRd
.aef_etat_courant <- function(layers) {
  src <- layers$embedding_source
  if (!is.character(src) || length(src) != 1L || is.na(src) || !identical(src, "aef") ||
      is.null(layers$rasters$aef_embedding)) {
    return("none")
  }
  an <- suppressWarnings(as.integer(attr(layers$rasters$aef_embedding, "annee")))
  paste0("aef:", if (length(an) == 1L && !is.na(an)) an else "NA")
}

#' Marker file holding the AlphaEarth state of the stored results
#' @param project_id Character.
#' @return Path, or `NULL` when the project has no directory.
#' @noRd
.aef_etat_chemin <- function(project_id) {
  pp <- get_project_path(project_id)
  if (is.null(pp)) return(NULL)
  file.path(pp, "data", "aef_etat.json")
}

#' Stored AlphaEarth state (`"none"` when no marker: projects before the source)
#' @noRd
.aef_etat_lire <- function(project_id) {
  f <- .aef_etat_chemin(project_id)
  if (is.null(f) || !file.exists(f)) return("none")
  v <- tryCatch(jsonlite::read_json(f, simplifyVector = TRUE)$etat,
                error = function(e) NULL)
  if (is.character(v) && length(v) == 1L && nzchar(v)) v else "none"
}

#' Record the AlphaEarth state of the results just computed
#' @noRd
.aef_etat_ecrire <- function(project_id, etat) {
  f <- .aef_etat_chemin(project_id)
  if (is.null(f) || !dir.exists(dirname(f))) return(invisible(FALSE))
  tryCatch({
    jsonlite::write_json(list(etat = etat), f, auto_unbox = TRUE)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

#' B4 / L3 / N5 already computed under another AlphaEarth state
#'
#' @description
#' The resume skips every column that already holds data. Switching the source
#' on (or off, or to another year) changes what B4, L3 and N5 mean, so they are
#' recomputed. A project with no marker counts as `"none"`: nothing changes for
#' projects that never used the source.
#'
#' @param project_id Character.
#' @param computed Character. Indicators considered already computed.
#' @param etat Character. Current state ([.aef_etat_courant()]).
#' @return Subset of `computed` to recompute.
#' @noRd
.aef_stale <- function(project_id, computed, etat) {
  if (identical(.aef_etat_lire(project_id), etat)) return(character(0))
  intersect(computed, .AEF_INDICATEURS)
}

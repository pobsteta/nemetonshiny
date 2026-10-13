# service_inventaire_video.R - Terrain > Inventaire video (brief groma 2026-10-12)
#
# L'app ne fait que l'interface : la creation du projet QField, le client du
# serveur Groma et l'integration (rapprochement, validation, agregats, fusion
# par plot_id sans ecraser l'inventaire classique) sont dans le coeur
# (`nemeton::creer_projet_inventaire_video()`, `groma_etat_placettes()`,
# `groma_telecharger_resultats()`, `importer_inventaire_video()`,
# `renvoyer_inventaire_video()`), experimentales depuis nemeton 2026.10.5.
#
# Les fonctions coeur sont resolues a l'execution (`.video_coeur()`) : le module
# reste chargeable, et testable, avec un coeur qui ne les exporte pas encore ; il
# affiche alors un message au lieu de planter. Les tests simulent les enveloppes
# `video_*` ci-dessous, pas le coeur.

# Etats d'une placette, dans l'ordre du cycle de vie.
.VIDEO_ETATS <- c("a_faire", "provisoire", "envoye", "definitif")

#' Exported nemeton function used by the video inventory, if available
#'
#' @param nom Character. Function name in the nemeton namespace.
#' @return The function, or `NULL` when the installed core does not export it.
#' @noRd
.video_coeur <- function(nom) {
  if (!requireNamespace("nemeton", quietly = TRUE)) return(NULL)
  if (!nom %in% getNamespaceExports("nemeton")) return(NULL)
  getExportedValue("nemeton", nom)
}

#' Does the installed core provide the video inventory?
#' @noRd
video_coeur_disponible <- function() {
  all(vapply(c("creer_projet_inventaire_video", "groma_etat_placettes",
               "groma_telecharger_resultats", "importer_inventaire_video"),
             function(n) !is.null(.video_coeur(n)), logical(1)))
}

.video_appeler <- function(nom, ...) {
  f <- .video_coeur(nom)
  if (is.null(f)) {
    stop(structure(
      class = c("video_coeur_absent", "error", "condition"),
      list(message = sprintf("nemeton::%s() is not available", nom), call = NULL)
    ))
  }
  f(...)
}

#' Build the QField project of the video inventory
#'
#' Thin wrapper over `nemeton::creer_projet_inventaire_video()`.
#' @return Path of the `.qgz`.
#' @noRd
video_creer_projet <- function(...) {
  .video_appeler("creer_projet_inventaire_video", ...)
}

#' State of the plots on the Groma server
#' @noRd
video_etat_serveur <- function(url, jeton) {
  .video_appeler("groma_etat_placettes", url = url, jeton = jeton)
}

#' Download the final results (GeoPackage, reports) of some plots
#' @return Paths of the downloaded files.
#' @noRd
video_telecharger <- function(url, jeton, plot_ids, dossier) {
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  res <- .video_appeler("groma_telecharger_resultats", url = url, jeton = jeton,
                        plot_ids = plot_ids, dossier = dossier)
  chemins <- unlist(res, use.names = FALSE)
  if (!is.character(chemins) || !length(chemins)) {
    chemins <- list.files(dossier, recursive = TRUE, full.names = TRUE)
  }
  chemins[file.exists(chemins)]
}

#' Integrate a video inventory into the project field data
#'
#' Calls `nemeton::importer_inventaire_video()` with the project's
#' `data/field_data.gpkg`. The core merges by `plot_id` and never overwrites a
#' classic inventory.
#'
#' @param gpkg Path of the `inventaire_video.gpkg` back from the field.
#' @param resultats Paths of the server's final GeoPackages, or `NULL`.
#' @param project_path Project directory.
#' @return The core's result (a list), with `field_data_gpkg` added.
#' @noRd
video_integrer <- function(gpkg, resultats, project_path) {
  data_dir <- file.path(project_path, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(data_dir, "field_data.gpkg")
  if (length(resultats)) resultats <- resultats[grepl("\\.gpkg$", resultats,
                                                      ignore.case = TRUE)]
  res <- .video_appeler("importer_inventaire_video", gpkg = gpkg,
                        resultats = if (length(resultats)) resultats else NULL,
                        field_data_gpkg = dest)
  # Le dernier GeoPackage integre : source de `etat_traitement` pour " Suivre ".
  vdir <- video_dossier(project_path)
  dir.create(vdir, recursive = TRUE, showWarnings = FALSE)
  file.copy(gpkg, file.path(vdir, "terrain_integre.gpkg"), overwrite = TRUE)
  res <- as.list(res)
  res$field_data_gpkg <- dest
  res
}

#' Arguments of `creer_projet_inventaire_video()` from the form
#'
#' Checks what the core cannot guess (project, plots, magnetic declination)
#' and passes the settings through unchanged. Rasters are passed as file paths:
#' a terra pointer does not survive the trip to the `future` worker.
#'
#' @param project_path Project directory, or `NULL`.
#' @param placettes sf of the sampling plan, or `NULL`.
#' @param plot_ids Character. Selected plots.
#' @param rayon_m,seuil_diam_cm,unite_azimut,declinaison_deg Form settings.
#' @param avec_lidar Logical. Include DTM, CHM and crowns.
#' @param dem,chm `SpatRaster` (or path) of the project, or `NULL`.
#' @param lang Language code.
#' @return The argument list, or an i18n key naming what is missing.
#' @noRd
video_args_generation <- function(project_path, placettes, plot_ids,
                                  rayon_m = 20, seuil_diam_cm = 7.5,
                                  unite_azimut = "deg", declinaison_deg = NA,
                                  avec_lidar = FALSE, dem = NULL, chm = NULL,
                                  lang = "fr") {
  if (is.null(project_path)) return("field_ingest_no_project")
  if (is.null(placettes) || !length(plot_ids)) return("video_sans_plan")
  decl <- suppressWarnings(as.numeric(declinaison_deg))
  if (length(decl) != 1L || !is.finite(decl)) return("video_declinaison_requise")
  .source <- function(r) {
    if (is.null(r)) return(NULL)
    if (is.character(r)) return(r[1])
    s <- tryCatch(terra::sources(r), error = function(e) "")
    if (length(s) && nzchar(s[1])) s[1] else NULL
  }
  avec_lidar <- isTRUE(avec_lidar) && !is.null(chm)
  list(
    placettes = placettes, plot_ids = as.character(plot_ids),
    rayon_m = rayon_m, seuil_diam_cm = seuil_diam_cm,
    unite_azimut = unite_azimut %||% "deg", declinaison_deg = decl,
    avec_lidar = avec_lidar,
    dem = if (avec_lidar) .source(dem), chm = if (avec_lidar) .source(chm),
    # Le coeur resout le MNT manquant depuis le projet.
    project_path = project_path,
    output_dir = video_dossier(project_path), lang = lang
  )
}

#' Can the core send trees back to the field?
#' @noRd
video_renvoi_disponible <- function() {
  !is.null(.video_coeur("renvoyer_inventaire_video"))
}

#' Does an integration leave trees to complete or to check?
#' @param bilan Result of [video_bilan()], or `NULL`.
#' @noRd
video_a_renvoyer <- function(bilan) {
  !is.null(bilan) && isTRUE(bilan$validation$ok) &&
    (nrow(bilan$a_completer) + nrow(bilan$a_verifier)) > 0L
}

#' Build the field return project of an integration
#'
#' Thin wrapper over `nemeton::renvoyer_inventaire_video()`: a new `.qgz` for
#' the plots that still hold trees to complete or to check. The other layers
#' come from the last integrated GeoPackage, the per-plot rasters from the
#' first `.qgz` of the project when it is still there.
#'
#' @param import Raw result of `importer_inventaire_video()`.
#' @param project_path Project directory.
#' @return Path of the `.qgz`, with attributes `plot_ids` and `n_arbres`.
#' @noRd
video_renvoyer <- function(import, project_path) {
  vdir <- video_dossier(project_path)
  gpkg <- file.path(vdir, "terrain_integre.gpkg")
  rasters <- NULL
  qgz <- file.path(vdir, "inventaire_video.qgz")
  if (file.exists(qgz)) {
    noms <- tryCatch(utils::unzip(qgz, list = TRUE)$Name, error = function(e) character(0))
    tifs <- grep("^rasters/.+\\.tif$", noms, value = TRUE)
    if (length(tifs)) {
      d <- tempfile("rasters_retour_")
      utils::unzip(qgz, files = tifs, exdir = d)
      rasters <- file.path(d, "rasters")
    }
  }
  .video_appeler("renvoyer_inventaire_video", import = import,
                 gpkg = if (file.exists(gpkg)) gpkg else NULL,
                 rasters = rasters, output_dir = vdir)
}

#' Start the QField project build in the background
#' @param tache The module's `ExtendedTask`.
#' @noRd
video_lancer_generation <- function(tache, args, dev_path = NULL) {
  tache$invoke(args, dev_path, get_app_options())
}

#' Directory of the video inventory files of a project
#' @noRd
video_dossier <- function(project_path) {
  file.path(project_path, "data", "inventaire_video")
}

#' Groma server settings
#'
#' The URL comes from the project metadata (`groma_url`) or the `GROMA_URL`
#' environment variable; the token only from `GROMA_JETON`, never from the
#' project nor the interface (secrets live in `.Renviron`).
#'
#' @param metadata Project metadata list, or `NULL`.
#' @return `list(url, jeton)`, empty strings when unset.
#' @noRd
video_config_serveur <- function(metadata = NULL) {
  url <- metadata$groma_url %||% ""
  if (!is.character(url) || length(url) != 1L || !nzchar(url)) {
    url <- Sys.getenv("GROMA_URL", "")
  }
  list(url = sub("/+$", "", url), jeton = Sys.getenv("GROMA_JETON", ""))
}

#' Plots of the project's sampling plan
#'
#' Same source as `mod_sampling`: layer `plots` of `data/samples.gpkg`.
#' @return An sf, or `NULL`.
#' @noRd
video_placettes_plan <- function(project_path) {
  if (is.null(project_path)) return(NULL)
  gpkg <- file.path(project_path, "data", "samples.gpkg")
  if (!file.exists(gpkg)) return(NULL)
  couches <- tryCatch(sf::st_layers(gpkg)$name, error = function(e) character(0))
  if (!"plots" %in% couches) return(NULL)
  p <- tryCatch(sf::st_read(gpkg, layer = "plots", quiet = TRUE),
                error = function(e) NULL)
  if (is.null(p) || !nrow(p)) NULL else p
}

#' `etat_traitement` of the plots in the last integrated GeoPackage
#' @return Named character vector (names = plot_id).
#' @noRd
video_etat_local <- function(project_path) {
  if (is.null(project_path)) return(character(0))
  gpkg <- file.path(video_dossier(project_path), "terrain_integre.gpkg")
  if (!file.exists(gpkg)) return(character(0))
  p <- tryCatch(sf::st_drop_geometry(
    sf::st_read(gpkg, layer = "placettes", quiet = TRUE)),
    error = function(e) NULL)
  if (is.null(p) || !all(c("plot_id", "etat_traitement") %in% names(p))) {
    return(character(0))
  }
  stats::setNames(as.character(p$etat_traitement), as.character(p$plot_id))
}

#' Table of the plots for the "Track" block
#'
#' Crosses the server state with the local `etat_traitement`: the most advanced
#' of the two wins. Columns missing on the server side are added empty.
#'
#' @param serveur data.frame from `groma_etat_placettes()`, or `NULL`.
#' @param local Named character vector from [video_etat_local()].
#' @return data.frame `plot_id, etat, date, verdict, n_arbres, alertes`.
#' @noRd
video_tableau_etats <- function(serveur, local = character(0)) {
  cols <- c("plot_id", "etat", "date", "verdict", "n_arbres", "alertes")
  s <- if (is.null(serveur)) data.frame() else as.data.frame(serveur)
  if ("geom" %in% names(s) || inherits(serveur, "sf")) s <- sf::st_drop_geometry(s)
  # Le serveur Groma nomme `maj` la date de derniere mise a jour.
  if (!"date" %in% names(s) && "maj" %in% names(s)) s$date <- s$maj
  ids <- unique(c(as.character(s$plot_id), names(local)))
  out <- data.frame(plot_id = ids, stringsAsFactors = FALSE)
  for (cl in setdiff(cols, "plot_id")) {
    v <- if (cl %in% names(s)) s[[cl]][match(ids, s$plot_id)] else rep(NA, length(ids))
    if (is.list(v)) v <- vapply(v, function(x) paste(unlist(x), collapse = "; "),
                                character(1))
    out[[cl]] <- v
  }
  rang <- function(e) match(e, .VIDEO_ETATS)
  loc <- unname(local[ids])
  r_s <- rang(out$etat); r_l <- rang(loc)
  out$etat <- ifelse(is.na(r_s) | (!is.na(r_l) & r_l > r_s), loc, out$etat)
  out$etat[is.na(out$etat)] <- "a_faire"
  out[, cols]
}

#' Tabular parts of an integration result, with safe defaults
#' @noRd
video_bilan <- function(res) {
  df <- function(x) if (is.null(x)) data.frame() else as.data.frame(
    if (inherits(x, "sf")) sf::st_drop_geometry(x) else x)
  val <- res$validation %||% list(ok = TRUE)
  list(
    validation = list(ok = isTRUE(val$ok), errors = df(val$errors),
                      warnings = df(val$warnings)),
    a_completer = df(res$a_completer),
    a_verifier = df(res$a_verifier),
    numeros_modifies = df(res$numeros_modifies),
    # Le coeur peut rendre les plot_id conserves ou directement leur nombre.
    classique_conserve = {
      cc <- res$classique_conserve %||% character(0)
      if (is.numeric(cc) && length(cc) == 1L) as.integer(cc) else length(cc)
    },
    ndp = suppressWarnings(as.integer(res$ndp %||% NA_integer_))
  )
}

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

# Etats d'une placette, dans l'ordre du cycle de vie. `sans_paquet` (schema
# Groma 1.5.0) : la saisie du terrain est arrivee, pas la video ; la placette
# est plus avancee qu'une placette a faire, mais pas encore en traitement.
.VIDEO_ETATS <- c("a_faire", "sans_paquet", "provisoire", "envoye", "definitif")

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

#' Does the installed core report the progress of the QField project build?
#'
#' True when `creer_projet_inventaire_video()` has a `progress_callback`
#' argument (nemeton mission 056).
#' @noRd
video_coeur_accepte_progression <- function() {
  f <- .video_coeur("creer_projet_inventaire_video")
  !is.null(f) && "progress_callback" %in% names(formals(f))
}

#' Write one progress event of the core to the progress file (never fatal)
#'
#' One line `etape<TAB>i<TAB>n<TAB>secondes`, written to a temporary file then
#' renamed so the session never reads a half-written line.
#' @param evt List `list(etape, i, n, secondes)` of the core callback.
#' @noRd
video_progression_ecrire <- function(chemin, evt) {
  tryCatch({
    ligne <- paste(evt$etape, as.integer(evt$i), as.integer(evt$n),
                   format(as.numeric(evt$secondes), digits = 6), sep = "\t")
    tmp <- paste0(chemin, ".tmp")
    writeLines(ligne, tmp)
    file.rename(tmp, chemin)
  }, error = function(e) NULL, warning = function(w) NULL)
  invisible(NULL)
}

#' Read the last progress event, or `NULL` when absent or unreadable
#' @return List `(etape, i, n, secondes)` or `NULL`.
#' @noRd
video_progression_lire <- function(chemin) {
  if (is.null(chemin) || !length(chemin) || is.na(chemin[1]) || !file.exists(chemin[1])) {
    return(NULL)
  }
  x <- tryCatch(readLines(chemin[1], n = 1L, warn = FALSE), error = function(e) character(0))
  if (!length(x)) return(NULL)
  p <- strsplit(x, "\t", fixed = TRUE)[[1]]
  if (length(p) != 4L || !nzchar(p[1])) return(NULL)
  v <- suppressWarnings(as.numeric(p[2:4]))
  if (anyNA(v)) return(NULL)
  list(etape = p[1], i = as.integer(v[1]), n = as.integer(v[2]), secondes = v[3])
}

#' Line shown under the "Build" button while the project is being built
#'
#' @param prog Event of [video_progression_lire()], or `NULL` (nothing received).
#' @param ecoule Seconds since the click.
#' @param tr i18n object.
#' @noRd
video_progression_ligne <- function(prog, ecoule, tr) {
  mm <- floor(ecoule / 60)
  duree <- sprintf("%d:%02d", as.integer(mm), as.integer(floor(ecoule - 60 * mm)))
  if (is.null(prog)) {
    return(sprintf(tr$t("video_pg_ligne"), tr$t("video_pg_attente"), duree))
  }
  cle <- paste("video", "pg", prog$etape, sep = "_")  # cle composee
  nom <- if (cle %in% names(TRANSLATIONS)) tr$t(cle) else prog$etape
  if (prog$etape %in% c("placettes", "dalles_mnt", "houppiers") && prog$n > 0L) {
    nom <- sprintf("%s (%d/%d)", nom, prog$i, prog$n)
  }
  sprintf(tr$t("video_pg_ligne"), nom, duree)
}

#' Build the QField project of the video inventory
#'
#' Thin wrapper over `nemeton::creer_projet_inventaire_video()`.
#'
#' When the core refuses the CVAT for its encoding (condition class
#' `nemeton_erreur_cvat_codage`, raised by core releases that check it), the
#' project is built once more **without** `cvat_path`: a badly encoded relief
#' never deprives the forester of the project. The app converts nothing itself
#' (the encoding is the core's business). Any other error, or a second failure,
#' is raised as is (no loop). The retried result carries the attribute
#' `cvat_refuse = TRUE` so the screen can say it.
#' @return Path of the `.qgz` (attribute `cvat_refuse` when retried).
#' @noRd
video_creer_projet <- function(...) {
  args <- list(...)
  # Relais de la progression : le worker ne peut pas toucher a l'interface, le
  # rappel du coeur ecrit l'etape dans un petit fichier que la session lit. Le
  # rappel n'est passe QUE si le coeur installe l'accepte (avant la mission 056
  # de nemeton, rien ne change).
  fichier <- args$progression_fichier
  args["progression_fichier"] <- NULL
  if (!is.null(fichier) && video_coeur_accepte_progression()) {
    args$progress_callback <- function(evt) video_progression_ecrire(fichier, evt)
  }
  res <- tryCatch(
    do.call(.video_appeler, c(list("creer_projet_inventaire_video"), args)),
    nemeton_erreur_cvat_codage = function(e) e)
  if (!inherits(res, "nemeton_erreur_cvat_codage")) return(res)
  # Sans CVAT passe, un second appel serait identique : l'erreur remonte.
  if (is.null(args$cvat_path)) stop(res)
  args["cvat_path"] <- NULL
  qgz <- do.call(.video_appeler, c(list("creer_projet_inventaire_video"), args))
  attr(qgz, "cvat_refuse") <- TRUE
  qgz
}

#' State of the plots on the Groma server
#' @noRd
video_etat_serveur <- function(url, jeton) {
  .video_appeler("groma_etat_placettes", url = url, jeton = jeton)
}

#' Download the final results (GeoPackage, reports, field data) of some plots
#'
#' By default the core downloads the four files of a plot, `terrain.zip`
#' (field data entered in Groma, schema 1.5.0) included. `fichiers` restricts
#' the download, e.g. to `terrain.zip` for a plot that has no video result.
#'
#' @param fichiers Files per plot, or `NULL` for the core's default.
#' @param etat Raw table of [video_etat_serveur()], or `NULL`. Since core
#'   2026.11.30 an unchanged `terrain.zip` (same `terrain_envoye_le` as at the
#'   last download, kept in `dossier`) is not requested again. Only passed to
#'   the core when not `NULL`, to stay compatible with older cores.
#' @return Paths of the downloaded files.
#' @noRd
video_telecharger <- function(url, jeton, plot_ids, dossier, fichiers = NULL,
                              etat = NULL) {
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  args <- list(url = url, jeton = jeton, plot_ids = plot_ids, dossier = dossier)
  if (!is.null(fichiers)) args$fichiers <- fichiers
  if (!is.null(etat)) args$etat <- etat
  res <- do.call(.video_appeler, c(list("groma_telecharger_resultats"), args))
  chemins <- unlist(res, use.names = FALSE)
  if (!is.character(chemins) || !length(chemins)) {
    # Repli : seulement les fichiers de CET appel (le dossier peut en contenir
    # d'un appel precedent), `<plot_id>_<fichier>`.
    tous <- list.files(dossier, recursive = TRUE, full.names = TRUE)
    nom <- basename(tous)
    de_ces_placettes <- Reduce(`|`, lapply(plot_ids, function(id)
      startsWith(nom, paste0(id, "_"))), rep(FALSE, length(nom)))
    de_ces_fichiers <- if (is.null(fichiers)) rep(TRUE, length(nom)) else
      Reduce(`|`, lapply(fichiers, function(x) endsWith(nom, paste0("_", x))),
             rep(FALSE, length(nom)))
    chemins <- tous[de_ces_placettes & de_ces_fichiers]
  }
  chemins[file.exists(chemins)]
}

#' Integrate a video inventory into the project field data
#'
#' Calls `nemeton::importer_inventaire_video()` with the project's
#' `data/field_data.gpkg`. The core merges by `plot_id` and never overwrites a
#' classic inventory. Since Groma 1.5.0 the field data entered on the server
#' (`terrain.zip`) replaces the QField copy for its plots, so `gpkg` is
#' optional as soon as `terrain` is given. The core copies the packed photos
#' under `photos/<plot_id>/` next to `field_data_gpkg` (`data/photos/`).
#'
#' @param gpkg Path of the `inventaire_video.gpkg` back from the field (QField
#'   copy), or `NULL` when only Groma field data is available.
#' @param resultats Paths of the server's final GeoPackages, or `NULL`.
#' @param project_path Project directory.
#' @param terrain Paths of `*terrain.zip` archives (or of directories holding
#'   them), or `NULL`.
#' @return The core's result (a list), with `field_data_gpkg` added.
#' @noRd
video_integrer <- function(gpkg, resultats, project_path, terrain = NULL) {
  if (is.null(gpkg) && !length(terrain)) {
    stop("Give the QField copy, the Groma field data, or both.")
  }
  data_dir <- file.path(project_path, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(data_dir, "field_data.gpkg")
  if (length(resultats)) resultats <- resultats[grepl("\\.gpkg$", resultats,
                                                      ignore.case = TRUE)]
  args <- list(gpkg = gpkg,
               resultats = if (length(resultats)) resultats else NULL,
               field_data_gpkg = dest)
  if (length(terrain)) args$terrain <- terrain
  res <- do.call(.video_appeler, c(list("importer_inventaire_video"), args))
  # Le dernier GeoPackage integre : source de `etat_traitement` pour " Suivre ".
  # Sans copie de QField (saisie Groma seule), on garde la precedente.
  vdir <- video_dossier(project_path)
  dir.create(vdir, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(gpkg)) {
    file.copy(gpkg, file.path(vdir, "terrain_integre.gpkg"), overwrite = TRUE)
  }
  res <- as.list(res)
  res$field_data_gpkg <- dest
  res
}

#' Default generation settings of the video inventory
#'
#' Radius 15 m, minimum diameter 7.5 cm, azimuths in degrees, and an empty
#' magnetic declination (`NA`): Groma then computes it at each plot with its
#' embedded WMM2025 model.
#' @noRd
VIDEO_PARAMS_DEFAULT <- list(rayon_m = 15, seuil_diam_cm = 7.5,
                             unite_azimut = "deg", declinaison_deg = NA_real_)

#' Generation settings of the video inventory, from the project metadata
#'
#' Set once per massif in Settings > Sources & parameters (`video_params`
#' in the project metadata), like the other calibration blocks. A missing,
#' malformed or out-of-range value falls back on its default, never on an error.
#'
#' @param metadata Project metadata list, or `NULL`.
#' @return `list(rayon_m, seuil_diam_cm, unite_azimut, declinaison_deg)`;
#'   `declinaison_deg` is `NA` when empty.
#' @noRd
project_video_params <- function(metadata) {
  cfg <- metadata$video_params
  d <- VIDEO_PARAMS_DEFAULT
  num <- function(x, def, lo, hi) {
    v <- suppressWarnings(as.numeric(x))
    if (length(v) != 1L || !is.finite(v) || v < lo || v > hi) def else v
  }
  unite <- as.character(cfg$unite_azimut %||% d$unite_azimut)[1]
  if (is.na(unite) || !unite %in% c("deg", "gr")) unite <- d$unite_azimut
  list(
    rayon_m = num(cfg$rayon_m, d$rayon_m, 5, 50),
    seuil_diam_cm = num(cfg$seuil_diam_cm, d$seuil_diam_cm, 0, 100),
    unite_azimut = unite,
    declinaison_deg = num(cfg$declinaison_deg, NA_real_, -30, 30)
  )
}

#' Persist the video inventory settings on a project
#' @param project_id Character.
#' @param rayon_m,seuil_diam_cm,unite_azimut,declinaison_deg Settings, validated
#'   by [project_video_params()] (out of range -> default).
#' @return Invisible `TRUE`.
#' @noRd
set_project_video_params <- function(project_id, rayon_m = NULL,
                                     seuil_diam_cm = NULL, unite_azimut = NULL,
                                     declinaison_deg = NULL) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }
  cfg <- project_video_params(list(video_params = list(
    rayon_m = rayon_m, seuil_diam_cm = seuil_diam_cm,
    unite_azimut = unite_azimut, declinaison_deg = declinaison_deg)))
  # Declinaison vide : la cle est omise (la liste remplace l'ancienne en bloc).
  if (is.na(cfg$declinaison_deg)) cfg$declinaison_deg <- NULL
  cfg$set_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  update_project_metadata(project_id, list(video_params = cfg),
                          project_path = project_path)
  invisible(TRUE)
}

#' Settings that [project_video_params()] replaces by their default
#'
#' Tells the user which submitted values were out of range (or unreadable) and
#' fell back on the default. An empty declination is legitimate (Groma computes
#' it), not a replacement.
#' @param rayon_m,seuil_diam_cm,unite_azimut,declinaison_deg Submitted values.
#' @return Character vector of i18n label keys of the replaced settings.
#' @noRd
video_params_remplaces <- function(rayon_m = NULL, seuil_diam_cm = NULL,
                                   unite_azimut = NULL, declinaison_deg = NULL) {
  ok <- project_video_params(list(video_params = list(
    rayon_m = rayon_m, seuil_diam_cm = seuil_diam_cm,
    unite_azimut = unite_azimut, declinaison_deg = declinaison_deg)))
  vide <- function(x) is.null(x) || !length(x) || is.na(x[1])
  rem <- character(0)
  if (!identical(ok$rayon_m, suppressWarnings(as.numeric(rayon_m))[1]))
    rem <- c(rem, "video_rayon")
  if (!identical(ok$seuil_diam_cm, suppressWarnings(as.numeric(seuil_diam_cm))[1]))
    rem <- c(rem, "video_seuil_diam")
  if (!identical(ok$unite_azimut, as.character(unite_azimut)[1]))
    rem <- c(rem, "video_unite_azimut")
  if (!vide(declinaison_deg) && is.na(ok$declinaison_deg)) rem <- c(rem, "video_declinaison")
  rem
}

#' Arguments of `creer_projet_inventaire_video()` from the form
#'
#' Checks what the core cannot guess (project, plots) and passes the settings
#' through unchanged. The settings (radius, minimum diameter, azimuth unit,
#' declination) come from `metadata` ([project_video_params()]) unless given
#' explicitly. The magnetic declination is optional: left empty, it goes
#' as `NULL` (the core writes `NA`, never 0) and Groma computes it at the plot
#' with its embedded WMM2025 model; a value typed in wins. Rasters are passed as file paths:
#' a terra pointer does not survive the trip to the `future` worker.
#'
#' @param project_path Project directory, or `NULL`.
#' @param placettes sf of the sampling plan, or `NULL`.
#' @param plot_ids Character. Selected plots.
#' @param rayon_m,seuil_diam_cm,unite_azimut,declinaison_deg Settings; `NULL`
#'   (default) reads them from `metadata`.
#' @param avec_lidar Logical. Include DTM, CHM and crowns.
#' @param dem,chm `SpatRaster` (or path) of the project, or `NULL`.
#' @param lang Language code.
#' @param metadata Project metadata list, or `NULL`.
#' @return The argument list, or an i18n key naming what is missing.
#' @noRd
video_args_generation <- function(project_path, placettes, plot_ids,
                                  rayon_m = NULL, seuil_diam_cm = NULL,
                                  unite_azimut = NULL, declinaison_deg = NULL,
                                  avec_lidar = FALSE, dem = NULL, chm = NULL,
                                  lang = "fr", metadata = NULL) {
  if (is.null(project_path)) return("field_ingest_no_project")
  if (is.null(placettes) || !length(plot_ids)) return("video_sans_plan")
  prm <- project_video_params(metadata)
  rayon_m <- rayon_m %||% prm$rayon_m
  seuil_diam_cm <- seuil_diam_cm %||% prm$seuil_diam_cm
  unite_azimut <- unite_azimut %||% prm$unite_azimut
  declinaison_deg <- declinaison_deg %||% prm$declinaison_deg
  decl <- suppressWarnings(as.numeric(declinaison_deg))
  decl <- if (length(decl) == 1L && is.finite(decl)) decl else NULL
  .source <- function(r) {
    if (is.null(r)) return(NULL)
    if (is.character(r)) return(r[1])
    s <- tryCatch(terra::sources(r), error = function(e) "")
    if (length(s) && nzchar(s[1])) s[1] else NULL
  }
  avec_lidar <- isTRUE(avec_lidar) && !is.null(chm)
  args <- list(
    placettes = placettes, plot_ids = as.character(plot_ids),
    rayon_m = rayon_m, seuil_diam_cm = seuil_diam_cm,
    unite_azimut = unite_azimut %||% "deg",
    # `list(x = NULL)` garde la cle : le coeur recoit bien NULL.
    declinaison_deg = decl,
    avec_lidar = avec_lidar,
    dem = if (avec_lidar) .source(dem), chm = if (avec_lidar) .source(chm),
    # Le coeur resout le MNT manquant depuis le projet.
    project_path = project_path,
    output_dir = video_dossier(project_path), lang = lang
  )
  # CVAT deja calcule : le coeur l'ecrit en rasters/cvat.tif. Absent : on ne
  # passe pas l'argument (aucun calcul pour l'occasion).
  cvat <- .video_cvat_path(project_path)
  if (!is.null(cvat)) args$cvat_path <- cvat
  args
}

#' Path of the project's precomputed CVAT, if any (never computes it)
#'
#' Looks only next to the project DEM for the Byte 0-255 file written by
#' `build_cvat_precomputed()`. Deliberately not `generate_rvt()`, which may
#' return a float [0, 1] cache and may trigger a computation.
#'
#' @param project_path Project directory, or `NULL`.
#' @return Path of the CVAT file, or `NULL`.
#' @noRd
.video_cvat_path <- function(project_path) {
  mnt <- .acc_rvt_mnt_path(project_path)
  if (is.null(mnt)) return(NULL)
  .rvt_precomputed_path(mnt)
}

#' What to do about the project's CVAT relief
#'
#' Decides, without computing anything, whether the QField project will carry
#' the CVAT relief. Same prudence as the Accessibility tab: the relief is only
#' computed on the native LiDAR HD DTM (`lidar_mnt_mosaic.tif`), never on the
#' WMS fallback (its striping would be amplified), and a recent failure with
#' the same parameters is not replayed.
#'
#' @param project_path Project directory, or `NULL`.
#' @param aoi Project AOI (`sf`, EPSG:2154), or `NULL`.
#' @param buffer_m Buffer around the AOI, in metres.
#' @return A list: `etat` (`"joint"`, `"a_calculer"` or `"absent"`), `cvat`
#'   (path when joined), `mnt`, `buffer_m` and, when absent, `raison` (an i18n
#'   key).
#' @noRd
video_cvat_plan <- function(project_path, aoi = NULL, buffer_m = 250) {
  cvat <- .video_cvat_path(project_path)
  if (!is.null(cvat)) return(list(etat = "joint", cvat = cvat))
  mnt <- .acc_rvt_mnt_path(project_path)
  if (is.null(mnt)) return(list(etat = "absent", raison = "video_cvat_sans_mnt"))
  if (!identical(basename(mnt), "lidar_mnt_mosaic.tif")) {
    return(list(etat = "absent", raison = "video_cvat_mnt_wms"))
  }
  if (!isTRUE(.rvt_cvat_available())) {
    return(list(etat = "absent", raison = "video_cvat_sans_outil"))
  }
  buffer_eff <- buffer_m
  if (!is.null(aoi)) {
    buffer_eff <- tryCatch(.cvat_buffer_plafonne(mnt, aoi, buffer_m),
                           error = function(e) buffer_m)
    res_m <- suppressWarnings(as.numeric(APP_CONFIG$cvat_res_m))
    if (!isTRUE(is.finite(res_m)) || res_m <= 0) res_m <- 2
    if (isTRUE(.cvat_failed_for(.rvt_cvat_out_path(mnt), aoi, buffer_eff, res_m))) {
      return(list(etat = "absent", raison = "video_cvat_echec", mnt = mnt))
    }
  }
  list(etat = "a_calculer", mnt = mnt, buffer_m = buffer_eff)
}

#' Compute the project's CVAT (runs in the background worker)
#'
#' Thin wrapper over [build_cvat_precomputed()], which is idempotent and
#' atomic. Kept here so that the module holds no logic.
#' @param mnt_path Native LiDAR DTM.
#' @param aoi Project AOI, or `NULL`.
#' @param buffer_m Buffer around the AOI, in metres.
#' @return Path of the CVAT, or `NULL`.
#' @noRd
video_cvat_calculer <- function(mnt_path, aoi = NULL, buffer_m = 0) {
  build_cvat_precomputed(mnt_path, aoi = aoi, buffer_m = buffer_m)
}

#' Start the CVAT computation in the background
#' @param tache The module's CVAT `ExtendedTask`.
#' @param plan Result of [video_cvat_plan()] (state `"a_calculer"`).
#' @param aoi Project AOI, or `NULL`.
#' @noRd
video_lancer_cvat <- function(tache, plan, aoi, dev_path = NULL) {
  tache$invoke(plan$mnt, aoi, plan$buffer_m, dev_path, get_app_options())
}

#' Plots of the plan ready for the map (EPSG:4326)
#'
#' One point per plot with a usable geometry. Plots without geometry (empty or
#' missing) or without a declared CRS are left out: a position guessed would
#' put a plot at the wrong place. Polygons are replaced by an interior point.
#'
#' @param placettes sf of the sampling plan, or `NULL`.
#' @param selection Character. Selected `plot_id`s.
#' @return An sf (`plot_id`, `selectionnee`, geometry) in EPSG:4326, or `NULL`.
#' @noRd
video_carte_donnees <- function(placettes, selection) {
  if (is.null(placettes) || !nrow(placettes) || !"plot_id" %in% names(placettes)) {
    return(NULL)
  }
  if (is.na(sf::st_crs(placettes))) return(NULL)
  g <- sf::st_geometry(placettes)
  ok <- !sf::st_is_empty(g)
  if (!any(ok)) return(NULL)
  p <- placettes[ok, "plot_id"]
  if (!all(sf::st_geometry_type(p) == "POINT")) {
    sf::st_geometry(p) <- suppressWarnings(
      sf::st_point_on_surface(sf::st_geometry(p)))
  }
  p <- sf::st_transform(p, 4326)
  p$plot_id <- as.character(p$plot_id)
  p$selectionnee <- p$plot_id %in% as.character(selection)
  p
}

#' Toggle one plot in the selection
#'
#' @param selection Character. Currently selected `plot_id`s.
#' @param id The clicked plot.
#' @param tous Character. All the plots of the plan, in plan order.
#' @return The new selection, in plan order. An unknown `id` changes nothing.
#' @noRd
video_basculer_selection <- function(selection, id, tous) {
  tous <- as.character(tous)
  selection <- intersect(tous, as.character(selection))
  id <- as.character(id)[1]
  if (is.na(id) || !id %in% tous) return(selection)
  nouvelle <- if (id %in% selection) setdiff(selection, id) else c(selection, id)
  tous[tous %in% nouvelle]
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

#' Can the core publish a QField project on the Groma server?
#'
#' `nemeton::groma_envoyer_projet()` (brief groma du 2026-10-17) is not in
#' every core: without it the "Send to phone" button stays disabled.
#' @noRd
video_envoi_disponible <- function() {
  !is.null(.video_coeur("groma_envoyer_projet"))
}

#' Publish a QField project on the Groma server
#'
#' Thin wrapper over `nemeton::groma_envoyer_projet()`, which takes the Groma
#' plugin `.qml` next to the `.qgz` and reads the project id inside it. The
#' phone then fetches it from Groma (menu > Server projects).
#'
#' @param qgz Path of the `.qgz`.
#' @param url,jeton Groma server settings ([video_config_serveur()]).
#' @return `list(projet_id, version, taille)`.
#' @noRd
video_envoyer_projet <- function(qgz, url, jeton) {
  .video_appeler("groma_envoyer_projet", qgz = qgz, url = url, jeton = jeton)
}

#' Start the upload to the Groma server in the background
#' @param tache The module's upload `ExtendedTask`.
#' @noRd
video_lancer_envoi <- function(tache, qgz, cfg, dev_path = NULL) {
  tache$invoke(qgz, cfg$url, cfg$jeton, dev_path, get_app_options())
}

#' End-of-upload notification
#'
#' @param res Result of [video_envoyer_projet()], or an error condition.
#' @param i18n Translator.
#' @return `list(message, type)`.
#' @noRd
video_message_envoi <- function(res, i18n) {
  if (inherits(res, "error")) {
    # Premiere ligne seulement : la chaine " Caused by " de rlang (curl,
    # httr2) ne se lit pas dans une notification ; la raison du coeur
    # (jeton refuse, projet trop gros, serveur injoignable) est en tete.
    msg <- if (inherits(res, "video_coeur_absent")) i18n$t("video_envoi_coeur_absent")
           else .video_traduire_envoi(
             strsplit(conditionMessage(res), "\n", fixed = TRUE)[[1]][1], i18n)
    return(list(message = paste(i18n$t("error"), msg), type = "error"))
  }
  # Le coeur rend `version = NA` quand le serveur n'en donne pas.
  version <- res$version %||% NA
  list(message = sprintf(i18n$t("video_envoi_ok"),
                         if (length(version) == 1L && !is.na(version))
                           as.character(version) else "?"),
       type = "message")
}

# Messages d'erreur de `nemeton::groma_envoyer_projet()` : en anglais
# (convention du coeur), classe non typee mais textes stables (reponse du
# coeur du 2026-10-17). Motif -> cle i18n ; un message inconnu reste tel quel.
VIDEO_ERREURS_ENVOI <- c(
  "refused the token" = "video_envoi_jeton_refuse",
  "too large" = "video_envoi_trop_gros",
  "altered during the upload" = "video_envoi_altere",
  "unreachable" = "video_envoi_injoignable"
)

#' Translate a known Groma send error from the core
#' @param msg First line of the core error message.
#' @param i18n Translator.
#' @return The translated message, or `msg` when it is not recognised.
#' @noRd
.video_traduire_envoi <- function(msg, i18n) {
  trouve <- names(VIDEO_ERREURS_ENVOI)[vapply(
    names(VIDEO_ERREURS_ENVOI), grepl, logical(1), x = msg, fixed = TRUE)]
  if (length(trouve) == 0L) return(msg)
  cle <- VIDEO_ERREURS_ENVOI[[trouve[1]]]
  if (cle == "video_envoi_injoignable") {
    url <- regmatches(msg, regexpr("(?<=<)[^>]+(?=>)", msg, perl = TRUE))
    return(sprintf(i18n$t(cle), if (length(url)) url else "?"))
  }
  if (cle == "video_envoi_jeton_refuse") {
    code <- regmatches(msg, regexpr("(?<=HTTP )[0-9]{3}", msg, perl = TRUE))
    return(sprintf(i18n$t(cle), if (length(code)) code else "?"))
  }
  i18n$t(cle)
}

#' Why the "Send to phone" button is disabled, or `NULL` when it is enabled
#' @param cfg Groma server settings.
#' @return An i18n key, or `NULL`.
#' @noRd
video_raison_envoi_inactif <- function(cfg) {
  if (!video_envoi_disponible()) return("video_envoi_coeur_absent")
  if (!nzchar(cfg$url %||% "") || !nzchar(cfg$jeton %||% "")) {
    return("video_envoi_sans_serveur")
  }
  NULL
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

#' Stable download directory of the Groma server files of a project
#'
#' Keeps `<plot_id>_terrain.envoye_le` (core witness) between two downloads,
#' so that an unchanged `terrain.zip` is not requested again. Falls back on a
#' temporary directory when no project is open.
#' @noRd
video_dossier_serveur <- function(project_path) {
  if (is.null(project_path) || !length(project_path) || is.na(project_path[1]) ||
      !nzchar(project_path[1])) {
    return(tempfile("groma_resultats_"))
  }
  d <- file.path(video_dossier(project_path[1]), "serveur")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
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

#' `etat_traitement` of the plots already integrated
#'
#' Read-only. First source: the last integrated QField copy
#' (`terrain_integre.gpkg`). Complement: layer `placettes` of the project's
#' `data/field_data.gpkg`, which also holds the plots integrated from Groma
#' field data alone (no QField copy); the QField copy wins for a plot found in
#' both.
#' @return Named character vector (names = plot_id).
#' @noRd
video_etat_local <- function(project_path) {
  if (is.null(project_path)) return(character(0))
  lire <- function(gpkg) {
    if (!file.exists(gpkg)) return(character(0))
    p <- tryCatch(sf::st_drop_geometry(
      sf::st_read(gpkg, layer = "placettes", quiet = TRUE)),
      error = function(e) NULL)
    if (is.null(p) || !all(c("plot_id", "etat_traitement") %in% names(p))) {
      return(character(0))
    }
    stats::setNames(as.character(p$etat_traitement), as.character(p$plot_id))
  }
  integre <- lire(file.path(video_dossier(project_path), "terrain_integre.gpkg"))
  projet <- lire(file.path(project_path, "data", "field_data.gpkg"))
  c(integre, projet[setdiff(names(projet), names(integre))])
}

#' Table of the plots for the "Track" block
#'
#' Crosses the server state with the local `etat_traitement`: the most advanced
#' of the two wins. Columns missing on the server side are added empty.
#'
#' @param serveur data.frame from `groma_etat_placettes()`, or `NULL`.
#' @param local Named character vector from [video_etat_local()].
#' @return data.frame `plot_id, etat, date, verdict, n_arbres, alertes,
#'   terrain_envoye_le` (ISO 8601 UTC or `NA`: nothing received from the field).
#' @noRd
video_tableau_etats <- function(serveur, local = character(0)) {
  cols <- c("plot_id", "etat", "date", "verdict", "n_arbres", "alertes",
            "terrain_envoye_le")
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
    if (cl == "terrain_envoye_le") v <- as.character(v)
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
    alertes = df(res$alertes),
    dbh_ruban = df(res$validation$dbh_ruban),
    # Le coeur peut rendre les plot_id conserves ou directement leur nombre.
    classique_conserve = {
      cc <- res$classique_conserve %||% character(0)
      if (is.numeric(cc) && length(cc) == 1L) as.integer(cc) else length(cc)
    },
    ndp = suppressWarnings(as.integer(res$ndp %||% NA_integer_))
  )
}


#' Files to deliver to the phone with a QField project
#'
#' Since core 2026.10.7 the Groma project plugin `<name>.qml` is written next
#' to `<name>.qgz` (never inside: QField looks for it on disk, under the
#' project's name). Without it the Groma button does not exist in QField.
#' Older cores write no `.qml`: the `.qgz` then goes alone.
#'
#' @param qgz Path of the `.qgz`.
#' @return Paths of the files that exist, `.qgz` first.
#' @noRd
video_fichiers_livraison <- function(qgz) {
  if (is.null(qgz) || !length(qgz)) return(character(0))
  f <- c(qgz[1], sub("\\.qgz$", ".qml", qgz[1]))
  unique(f[file.exists(f)])
}

#' Zip a QField project with its plugin, side by side
#'
#' Since core 2026.10.12 an openable folder `<name>/` (`<name>.qgs`,
#' `inventaire_video.gpkg`, `rasters/`, `<name>.qml`) is written next to the
#' `.qgz`. QGIS resolves `./inventaire_video.gpkg` from the folder of the
#' project file, not inside the archive: once unzipped, the `.qgz` alone opens
#' with every layer missing. The folder is then zipped instead, at the root of
#' the archive. Older cores write no folder: the `.qgz` and its `.qml` go flat.
#'
#' @param qgz Path of the `.qgz`.
#' @param file Destination `.zip`.
#' @return `file`, invisibly.
#' @noRd
video_zip_livraison <- function(qgz, file) {
  f <- video_fichiers_livraison(qgz)
  if (!length(f)) stop("QField project not found")
  # Sans dossier ouvrable (coeur ancien) : le `.qgz` et son `.qml` a plat,
  # comme QField les attend.
  qgis_zip_livraison(f[1], file, a_plat = f)
}

#' Name of the delivered archive (`<name>.zip`)
#' @noRd
video_nom_livraison <- function(qgz, defaut) {
  sub("\\.qgz$", ".zip", basename(qgz %||% defaut))
}

#' Validation warnings that are not mere information
#'
#' `valider_inventaire_video()` reports several measured centres per plot with
#' `severity = "info"`: shown, but neither counted nor coloured as a warning.
#'
#' @param warnings The `warnings` data.frame of a validation.
#' @return `list(avertissements, infos)`.
#' @noRd
video_separer_infos <- function(warnings) {
  if (is.null(warnings) || !is.data.frame(warnings) || !nrow(warnings) ||
      !"severity" %in% names(warnings)) {
    return(list(avertissements = warnings, infos = NULL))
  }
  info <- !is.na(warnings$severity) & warnings$severity == "info"
  list(avertissements = warnings[!info, , drop = FALSE],
       infos = warnings[info, , drop = FALSE])
}

#' Date the field data reached Groma, in Paris time
#'
#' @param x Character. `terrain_envoye_le` (ISO 8601, UTC) or `NA`.
#' @return Character, `"dd/mm/yyyy HH:MM"` (one per element), `""` for `NA` or
#'   an unreadable date (nothing received, nothing shown).
#' @noRd
video_terrain_recu <- function(x) {
  x <- as.character(x)
  # Suffixe `Z` ou decalage nul retire : la date est lue comme UTC.
  x <- sub("(Z|[+-]00:?00)$", "", x)
  t <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  ifelse(is.na(t), "", format(t, "%d/%m/%Y %H:%M", tz = "Europe/Paris"))
}

#' Plots whose field data must be downloaded
#'
#' A plot with a final video result gets all its files; a plot that only sent
#' field data (`sans_paquet`, or any other state with `terrain_envoye_le`) gets
#' `terrain.zip` alone, so that no 404 warning is raised for a missing result.
#'
#' @param etats Table from [video_tableau_etats()].
#' @return `list(definitifs, terrain_seul)`.
#' @noRd
video_plots_a_recuperer <- function(etats) {
  if (is.null(etats) || !nrow(etats)) {
    return(list(definitifs = character(0), terrain_seul = character(0)))
  }
  def <- etats$plot_id[etats$etat == "definitif"]
  recu <- etats$plot_id[!is.na(etats$terrain_envoye_le) &
                          nzchar(etats$terrain_envoye_le)]
  list(definitifs = def, terrain_seul = setdiff(recu, def))
}

# Motifs d'alerte de l'import (`$alertes$motif`) traduits par l'app : motif du
# coeur -> cle i18n. Un motif sans cle est regroupe sous `video_motif_autre`.
VIDEO_MOTIFS_ALERTE <- c(
  terrain_serveur_prefere = "video_motif_serveur_pref",
  saisie_qfield_ignoree = "video_motif_saisie_ignoree",
  centre_telephone = "video_motif_centre_tel"
)

#' Import alerts, with translated reasons
#'
#' @param alertes The `alertes` data.frame of an import (`plot_id`, `uuid`,
#'   `motif`, `dbh_provisoire`, `dbh_definitif`, `detail`).
#' @param i18n Translator.
#' @return data.frame `plot_id, motif, detail` (`detail` empty when the core
#'   gives none), possibly with zero rows.
#' @noRd
video_alertes_traduites <- function(alertes, i18n) {
  vide <- data.frame(plot_id = character(0), motif = character(0),
                     detail = character(0), stringsAsFactors = FALSE)
  if (is.null(alertes) || !is.data.frame(alertes) || !nrow(alertes) ||
      !"motif" %in% names(alertes)) return(vide)
  col <- function(nm) {
    if (nm %in% names(alertes)) as.character(alertes[[nm]])
    else rep(NA_character_, nrow(alertes))
  }
  motif <- col("motif")
  cle <- unname(VIDEO_MOTIFS_ALERTE[motif])
  # Motif sans cle i18n : etiquette generique traduite suivie du code, jamais
  # le code brut seul.
  lib <- ifelse(is.na(cle),
                ifelse(is.na(motif) | !nzchar(motif), "",
                       sprintf(i18n$t("video_motif_autre"), motif)),
                vapply(cle, function(k) if (is.na(k)) "" else i18n$t(k), character(1)))
  detail <- col("detail")
  detail[is.na(detail)] <- ""
  data.frame(plot_id = col("plot_id"), motif = lib, detail = detail,
             stringsAsFactors = FALSE)
}

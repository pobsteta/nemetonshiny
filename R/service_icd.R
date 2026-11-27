# service_icd.R - Indicateur composite de decoloration (ICD) du Suivi sanitaire.
#
# Aucune logique metier ici (regles 1-3) : le calcul, les poids par defaut, leur
# validation (renormalisation a 1), la lecture des rasters et des series, la
# comparaison avec FAST sont des fonctions exportees du coeur
# (`nemeton::run_icd_decoloration()`, `icd_poids_defaut()`, `icd_poids_valider()`,
# `read_icd_raster()`, `read_icd_pixel_series()`, `comparer_fast_icd()`). Ce
# fichier ne fait que : persister les poids du projet, preparer les chemins,
# lancer le calcul en tache asynchrone et rendre des erreurs lisibles.

# --- Chemins ------------------------------------------------------------------

# Racine des resultats ICD d'un projet : `<projet>/cache/layers/icd`. Le coeur y
# range un dossier `zone_<id>/` par zone de suivi. NULL sans projet.
.icd_result_dir <- function(project_path, creer = FALSE) {
  if (is.null(project_path) || length(project_path) != 1L ||
      is.na(project_path) || !nzchar(project_path)) return(NULL)
  d <- file.path(project_path, "cache", "layers", "icd")
  if (isTRUE(creer) && !dir.exists(d)) {
    tryCatch(dir.create(d, recursive = TRUE, showWarnings = FALSE),
             error = function(e) NULL)
  }
  d
}

# BD Foret V2 du projet (ecrite au calcul des indicateurs). L'ICD n'existe pas
# sans elle : les essences en viennent. Chemin seul, la lecture se fait dans le
# worker.
.icd_bdforet_path <- function(project_path) {
  if (is.null(project_path) || length(project_path) != 1L ||
      is.na(project_path) || !nzchar(project_path)) return(NULL)
  p <- file.path(project_path, "cache", "layers", "bdforet.gpkg")
  if (file.exists(p)) p else NULL
}

# Fichier-temoin ecrit par l'app a cote des sorties du coeur : les poids
# EFFECTIVEMENT utilises par le dernier calcul (apres renormalisation), les
# annees calculees et l'annee provisoire. Il sert a dire si les poids du projet
# ont change depuis, et a lister les annees du selecteur sans lire les fichiers
# prives du coeur.
.icd_temoin_path <- function(result_dir, zone_id) {
  if (is.null(result_dir) || !nzchar(result_dir)) return(NULL)
  file.path(result_dir, sprintf("zone_%d", as.integer(zone_id)), "app_icd.json")
}

# --- Poids du projet ----------------------------------------------------------

# Colonnes d'indices et groupes d'essences : lus dans la table du coeur, jamais
# recopies ici.
.icd_defaut <- function() nemeton::icd_poids_defaut()

.icd_indices_noms <- function() {
  d <- .icd_defaut()
  setdiff(names(d), c("essence", "mois_debut", "mois_fin"))
}

# Indices nuls par construction pour les resineux (CRre), d'apres les poids par
# defaut du coeur : l'app desactive ces champs, le coeur refuse une valeur non
# nulle.
.icd_cases_figees <- function() {
  d <- .icd_defaut()
  idx <- .icd_indices_noms()
  m <- as.matrix(d[, idx, drop = FALSE])
  rownames(m) <- d$essence
  m == 0
}

#' Weights of the ICD formula stored on a project
#'
#' Absent or aberrant metadata yields the core defaults, cell by cell, so a
#' project that never opened the settings behaves like the method note.
#'
#' @param metadata List. A project's `metadata` (or `NULL`).
#' @return A `data.frame`: `essence` and one numeric column per index.
#' @noRd
project_icd_poids <- function(metadata) {
  def <- .icd_defaut()
  idx <- .icd_indices_noms()
  out <- def[, c("essence", idx), drop = FALSE]
  cfg <- metadata$icd_poids$poids
  if (is.null(cfg)) return(out)
  for (i in seq_len(nrow(out))) {
    ligne <- cfg[[out$essence[i]]]
    if (is.null(ligne)) next
    for (k in idx) {
      v <- suppressWarnings(as.numeric(ligne[[k]] %||% NA))
      if (length(v) == 1L && is.finite(v) && v >= 0) out[i, k] <- v
    }
  }
  out
}

#' Weights table checked and rescaled by the core
#'
#' Wraps [nemeton::icd_poids_valider()] and turns its refusals into a value, so
#' the settings tab can say why instead of failing. The core's rescaling notice
#' is silenced: the tab shows the entered sum and the weights used itself.
#'
#' @param poids A `data.frame` (see [project_icd_poids()]).
#' @return A list: `valide` (the core's table, or `NULL`), `erreur` (character
#'   or `NA`), `somme_saisie` (named numeric), `renormalise` (character).
#' @noRd
icd_poids_resume <- function(poids) {
  res <- tryCatch(
    suppressMessages(nemeton::icd_poids_valider(poids)),
    error = function(e) e)
  if (inherits(res, "error")) {
    idx <- intersect(.icd_indices_noms(), names(poids))
    somme <- stats::setNames(
      rowSums(as.matrix(poids[, idx, drop = FALSE]), na.rm = TRUE),
      as.character(poids$essence))
    return(list(valide = NULL,
                erreur = cli::ansi_strip(conditionMessage(res)),
                somme_saisie = somme, renormalise = character(0)))
  }
  list(valide = res, erreur = NA_character_,
       somme_saisie = attr(res, "somme_saisie"),
       renormalise = attr(res, "renormalise"))
}

#' Persist the ICD weights on a project
#'
#' The values are stored as entered (the sum may differ from 1: the core
#' rescales at each run). A table the core refuses (negative, null sum, CRre on
#' a conifer) is not saved.
#'
#' @param project_id Character.
#' @param poids A `data.frame` (see [project_icd_poids()]).
#' @return Invisible `TRUE`.
#' @noRd
set_project_icd_poids <- function(project_id, poids) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }
  r <- icd_poids_resume(poids)
  if (is.null(r$valide)) cli::cli_abort(r$erreur)
  idx <- .icd_indices_noms()
  liste <- lapply(stats::setNames(seq_len(nrow(poids)), as.character(poids$essence)),
                  function(i) lapply(stats::setNames(idx, idx),
                                     function(k) as.numeric(poids[i, k])))
  update_project_metadata(
    project_id,
    list(icd_poids = list(poids = liste,
                          set_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))),
    project_path = project_path)
  cli::cli_alert_success("Suivi sanitaire : poids de l'ICD enregistres")
  invisible(TRUE)
}

#' Forget the ICD weights of a project (back to the core defaults)
#' @noRd
reset_project_icd_poids <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }
  update_project_metadata(project_id, list(icd_poids = NULL),
                          project_path = project_path)
  invisible(TRUE)
}

# --- Temoin du dernier calcul -------------------------------------------------

# Ecrit le temoin apres un calcul abouti. `poids` : la table VALIDEE rendue par
# le coeur dans le resultat du run.
.icd_ecrire_temoin <- function(result_dir, zone_id, poids, annees,
                               annee_provisoire, coupes_retirees = NA) {
  p <- .icd_temoin_path(result_dir, zone_id)
  if (is.null(p)) return(invisible(FALSE))
  idx <- .icd_indices_noms()
  w <- lapply(stats::setNames(seq_len(nrow(poids)), as.character(poids$essence)),
              function(i) lapply(stats::setNames(idx, idx),
                                 function(k) as.numeric(poids[i, k])))
  tryCatch({
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(
      list(poids = w, annees = as.integer(annees),
           annee_provisoire = as.integer(annee_provisoire),
           coupes_retirees = if (is.na(coupes_retirees)) NULL else isTRUE(coupes_retirees),
           calcule_le = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
      p, auto_unbox = TRUE, pretty = TRUE)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

.icd_lire_temoin <- function(result_dir, zone_id) {
  p <- .icd_temoin_path(result_dir, zone_id)
  if (is.null(p) || !file.exists(p)) return(NULL)
  tryCatch(jsonlite::read_json(p), error = function(e) NULL)
}

#' Years offered by the ICD map selector
#'
#' The years the last run computed (from the app's own witness file). Without
#' a witness (results produced elsewhere), every year from the first Sentinel-2
#' season of the method to the current one; the reader answers `NULL` for a
#' year that was not computed.
#'
#' @param result_dir,zone_id Where the ICD results live.
#' @param premiere Integer. First year of the fallback range.
#' @param aujourdhui Date, for tests.
#' @return A list: `annees` (integer, increasing) and `provisoire` (integer or
#'   `NA`).
#' @noRd
icd_annees_disponibles <- function(result_dir, zone_id, premiere = 2017L,
                                   aujourdhui = Sys.Date()) {
  t <- .icd_lire_temoin(result_dir, zone_id)
  if (!is.null(t) && length(t$annees)) {
    return(list(annees = sort(as.integer(unlist(t$annees))),
                provisoire = as.integer(t$annee_provisoire %||% NA_integer_)))
  }
  an <- as.integer(format(aujourdhui, "%Y"))
  list(annees = seq.int(as.integer(premiere), an), provisoire = NA_integer_)
}

#' Have the project weights changed since the last ICD run?
#'
#' Compares the weights the project would use now (after the core's rescaling)
#' with those the last run used. `FALSE` when there is no witness: results
#' produced elsewhere are not declared stale on a guess.
#'
#' @param result_dir,zone_id Where the ICD results live.
#' @param poids Current weights (see [project_icd_poids()]).
#' @return Logical.
#' @noRd
icd_poids_obsoletes <- function(result_dir, zone_id, poids) {
  t <- .icd_lire_temoin(result_dir, zone_id)
  if (is.null(t) || is.null(t$poids)) return(FALSE)
  r <- icd_poids_resume(poids)
  if (is.null(r$valide)) return(FALSE)
  idx <- .icd_indices_noms()
  for (i in seq_len(nrow(r$valide))) {
    ligne <- t$poids[[r$valide$essence[i]]]
    if (is.null(ligne)) return(TRUE)
    for (k in idx) {
      a <- suppressWarnings(as.numeric(ligne[[k]] %||% NA))
      if (length(a) != 1L || is.na(a) || abs(a - r$valide[i, k]) > 1e-6) {
        return(TRUE)
      }
    }
  }
  FALSE
}

# --- Lecture (presentation seule) ---------------------------------------------

# Niveau d'une annee, en raster. `NULL` quand l'annee n'a pas ete calculee.
icd_lire_carte <- function(zone_id, annee, result_dir, a_date = FALSE) {
  tryCatch(
    nemeton::read_icd_raster(as.integer(zone_id), as.integer(annee),
                             result_dir, layer = "niveau", a_date = a_date),
    error = function(e) {
      cli::cli_alert_warning("read_icd_raster failed: {conditionMessage(e)}")
      NULL
    })
}

# Serie d'un pixel (clic). `NULL` hors de la grille ou sans resultat.
icd_lire_pixel <- function(zone_id, lng, lat, result_dir, a_date = FALSE) {
  tryCatch(
    nemeton::read_icd_pixel_series(as.integer(zone_id), xy = c(lng, lat),
                                   crs = 4326, result_dir = result_dir,
                                   a_date = a_date),
    error = function(e) {
      cli::cli_alert_warning("read_icd_pixel_series failed: {conditionMessage(e)}")
      NULL
    })
}

# Comparaison FAST / ICD de la zone et de l'annee. Le statut dit pourquoi elle
# est vide (`fast_absent`, `icd_absent`, `sans_pixel_commun`).
icd_comparer_fast <- function(zone_id, annee, result_dir, con = NULL,
                              cache_dir = NULL, a_date = FALSE) {
  tryCatch(
    nemeton::comparer_fast_icd(as.integer(zone_id), as.integer(annee),
                               result_dir, con = con, cache_dir = cache_dir,
                               a_date = a_date),
    error = function(e) {
      cli::cli_alert_warning("comparer_fast_icd failed: {conditionMessage(e)}")
      list(statut = "icd_absent", annee = as.integer(annee), n_pixels = 0L,
           croisement = NULL, accord_global = NA_real_, kappa = NA_real_,
           surface_egale = NULL)
    })
}

# --- SUFOSAT (coupes rases) ---------------------------------------------------

# Rasters SUFOSAT deja en cache dans le projet (ecrits par le calcul des
# indicateurs, `build_sufosat_layer()`) : `dates_<cle>.tif` et `proba_<cle>.tif`.
# On prend le couple le plus recent ; l'ICD ne telecharge rien. NULL quand la
# source est eteinte dans les parametres du projet ou qu'aucun couple n'existe :
# le coeur dit alors que les coupes ne sont pas retirees, et l'app le repete.
.icd_sufosat_chemins <- function(project_path, metadata = NULL) {
  if (!project_sufosat_enabled(metadata)) return(NULL)
  if (is.null(project_path) || length(project_path) != 1L ||
      is.na(project_path) || !nzchar(project_path)) return(NULL)
  d <- file.path(project_path, "cache", "layers", "sufosat")
  if (!dir.exists(d)) return(NULL)
  dates <- list.files(d, "^dates_.*\\.tif$", full.names = TRUE)
  if (!length(dates)) return(NULL)
  proba <- file.path(d, sub("^dates_", "proba_", basename(dates)))
  ok <- file.exists(proba)
  if (!any(ok)) return(NULL)
  dates <- dates[ok]; proba <- proba[ok]
  i <- which.max(file.mtime(dates))
  list(dates = dates[i], proba = proba[i])
}

# Les coupes ont-elles ete retirees au dernier calcul ? TRUE / FALSE, NA sans
# temoin (resultats produits ailleurs).
icd_coupes_retirees <- function(result_dir, zone_id) {
  t <- .icd_lire_temoin(result_dir, zone_id)
  if (is.null(t) || is.null(t$coupes_retirees)) return(NA)
  isTRUE(t$coupes_retirees)
}

# --- Calcul (enfant plafonne en memoire) --------------------------------------

# Corps du calcul. Execute DANS l'enfant plafonne (`run_memory_capped()` lui
# injecte `con`, ouverte depuis `db_url`, et `progress_callback`, ecrit dans le
# fichier de progression) ; ou directement dans le worker sur un coeur qui ne
# sait pas lancer une fonction d'un autre paquet. Rien ne traverse la frontiere
# de process que des chemins et des tables. Une erreur du coeur (`status =
# "error"`, qu'il attrape et rend en valeur) est relevee ici pour que la tache
# passe en erreur ; une annulation reste une valeur.
.icd_enfant <- function(con, zone_id, cache_dir, result_dir, poids,
                        bdforet_path, sufosat_dates_path = NULL,
                        sufosat_proba_path = NULL, progress_callback = NULL,
                        cancel_path = NULL) {
  bd <- tryCatch(sf::st_read(bdforet_path, quiet = TRUE),
                 error = function(e) NULL)
  if (is.null(bd) || !nrow(bd)) {
    stop("BD Foret V2 illisible ou vide : ", bdforet_path, call. = FALSE)
  }
  sd <- sp <- NULL
  if (!is.null(sufosat_dates_path) && !is.null(sufosat_proba_path)) {
    sd <- tryCatch(terra::rast(sufosat_dates_path), error = function(e) NULL)
    sp <- tryCatch(terra::rast(sufosat_proba_path), error = function(e) NULL)
    if (is.null(sd) || is.null(sp)) sd <- sp <- NULL
  }
  # `write_alerts` reste au defaut du coeur (TRUE) : les alertes de la derniere
  # annee vont dans la table `alert` (`alert_type = "icd_decoloration"`).
  res <- nemeton::run_icd_decoloration(
    con = con, zone_id = as.integer(zone_id), cache_dir = cache_dir,
    result_dir = result_dir, poids = poids, bdforet = bd,
    sufosat_dates = sd, sufosat_proba = sp,
    progress_callback = progress_callback, cancel_path = cancel_path)
  if (identical(res$status, "error")) {
    stop(res$message %||% "ICD run failed", call. = FALSE)
  }
  if (identical(res$status, "success")) {
    .icd_ecrire_temoin(
      result_dir, zone_id, res$poids, res$annees,
      annee_provisoire = as.integer(format(Sys.Date(), "%Y")),
      coupes_retirees = isTRUE(res$coupes_retirees))
  }
  res
}

# Le coeur installe sait-il lancer une fonction d'un autre paquet sous plafond
# (`package=`, nemeton >= 0.158.0) ? Meme garde que la reGeneration.
.icd_plafond_possible <- function() {
  if (!requireNamespace("nemeton", quietly = TRUE)) return(FALSE)
  "package" %in% names(formals(nemeton::run_memory_capped))
}

# Execute le calcul sous plafond memoire : enfant R dans un cgroup, marqueur de
# run (arret dur par `stop_memory_capped()`), journal de l'enfant. Repli : appel
# direct dans le worker, avec sa propre connexion.
.icd_executer <- function(db_url, zone_id, cache_dir, result_dir, poids,
                          bdforet_path, sufosat = NULL, progress_path = NULL,
                          cancel_path = NULL, marker_path = NULL,
                          log_path = NULL, plafonne = .icd_plafond_possible()) {
  args <- list(zone_id = zone_id, cache_dir = cache_dir, result_dir = result_dir,
               poids = poids, bdforet_path = bdforet_path,
               sufosat_dates_path = sufosat$dates,
               sufosat_proba_path = sufosat$proba, cancel_path = cancel_path)
  if (isTRUE(plafonne)) {
    return(.run_capped(
      fun = ".icd_enfant", package = "nemetonshiny", args = args,
      db_url = db_url, progress_path = progress_path, log_path = log_path,
      marker_path = marker_path, quiet = FALSE))
  }
  con <- get_monitoring_db_connection(db_url = db_url)
  if (is.null(con)) {
    stop("Monitoring DB not configured (set NEMETON_DB_URL, NEMETON_DB_HOST/_PORT/_NAME/_USER/_PASSWORD, or open a project to use the local SQLite fallback).")
  }
  on.exit(close_monitoring_db_connection(con), add = TRUE)
  do.call(.icd_enfant, c(list(con = con,
                              progress_callback = .build_progress_writer(progress_path)),
                         args))
}

#' ExtendedTask running the ICD for a monitoring zone
#'
#' Same machinery as the RECONFORT task: a `future_promise` worker that
#' re-loads the package; the heavy part runs in a memory-capped child process
#' (see `.icd_executer()`), which writes its progress events to a file the
#' parent polls.
#'
#' @return A `shiny::ExtendedTask`.
#' @noRd
run_icd_async <- function() {
  .dev_pkg_path <- tryCatch(
    if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
      find.package("nemetonshiny")
    else NULL,
    error = function(e) NULL
  )
  .worker_envvars <- .capture_worker_envvars()

  shiny::ExtendedTask$new(function(zone_id = NULL, cache_dir = NULL,
                                   result_dir = NULL, poids = NULL,
                                   bdforet_path = NULL, sufosat = NULL,
                                   db_url = "", progress_path = NULL,
                                   cancel_path = NULL, marker_path = NULL) {
    if (requireNamespace("future", quietly = TRUE)) {
      plan_classes <- class(future::plan())
      is_parallel <- any(c("multisession", "multicore", "cluster") %in% plan_classes)
      if (!is_parallel) .ensure_async_plan()
    }
    promises::future_promise({
      .apply_worker_envvars(.worker_envvars)
      on.exit(.release_worker_memory(), add = TRUE)
      if (!is.null(.dev_pkg_path) && requireNamespace("pkgload", quietly = TRUE)) {
        pkgload::load_all(.dev_pkg_path, quiet = TRUE)
      } else {
        loadNamespace("nemetonshiny")
      }
      child_db_url <- if (nzchar(db_url %||% "")) db_url else .resolve_monitoring_db_url(NULL)
      .icd_executer(
        db_url = child_db_url, zone_id = zone_id, cache_dir = cache_dir,
        result_dir = result_dir, poids = poids, bdforet_path = bdforet_path,
        sufosat = sufosat, progress_path = progress_path,
        cancel_path = cancel_path, marker_path = marker_path,
        log_path = .child_log_path(dirname(progress_path %||% "."), "icd"))
    }, seed = TRUE)
  })
}

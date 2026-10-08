# Serveur MCP : outils exposes a un assistant (Claude Code via AIGORA, VICTOR).
#
# specs/BRIEF-pilotage-victor-aigora.md, A.2 et A.3. Fines enveloppes de l'API
# hors interface (R/api.R) : aucune logique metier, aucune lecture qui ecrit.
# Lanceur : inst/mcp/server.R. Chaque outil rend une chaine JSON courte, faite
# pour etre lue par un modele (et pour remplir le `display_report` de VICTOR).
# stdout est le canal du protocole MCP (stdio) : rien n'y est ecrit ici.

# ---------------------------------------------------------------- utilitaires

.mcp_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null",
                                na = "null", digits = 4))
}

# Evalue `expr` ; une erreur devient une reponse `ok = false` lisible par le
# modele (message + classe), plutot qu'une exception de protocole.
.mcp_call <- function(expr) {
  res <- tryCatch(expr, error = function(e) {
    cls <- grep("^nemetonshiny_", class(e), value = TRUE)
    structure(list(ok = FALSE, erreur = conditionMessage(e),
                   classe = if (length(cls)) cls[1] else NULL,
                   candidats = e$candidats),
              class = "mcp_erreur")
  })
  if (inherits(res, "mcp_erreur")) res <- unclass(res)
  else res <- c(list(ok = TRUE), res)
  .mcp_json(Filter(Negate(is.null), res))
}

.mcp_norm <- function(x) {
  x <- iconv(as.character(x), from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  tolower(trimws(x))
}

#' Resolve a project from an id or an approximate name
#'
#' Exact id first, then exact name (case and accents ignored), then a single
#' partial name match. Several candidates: error of class
#' `nemetonshiny_projet_ambigu` carrying `candidats`, so the assistant can ask.
#' Never a silent choice.
#'
#' @param projet Character. Project id or name.
#' @return The project id.
#' @noRd
.mcp_resolve_project <- function(projet) {
  if (!is.character(projet) || length(projet) != 1L || !nzchar(trimws(projet))) {
    .api_abort("Projet non pr\u00e9cis\u00e9.", "nemetonshiny_projet_introuvable")
  }
  l <- projets_lister()
  if (projet %in% l$id) return(projet)
  noms <- .mcp_norm(l$nom)
  cible <- .mcp_norm(projet)
  idx <- which(noms == cible)
  if (!length(idx)) idx <- which(grepl(cible, noms, fixed = TRUE))
  if (length(idx) == 1L) return(l$id[idx])
  if (!length(idx)) {
    .api_abort("Aucun projet ne correspond \u00e0 {.val {projet}}.",
               "nemetonshiny_projet_introuvable")
  }
  candidats <- sprintf("%s (%s)", l$nom[idx], l$id[idx])
  .api_abort(c("Plusieurs projets correspondent \u00e0 {.val {projet}}.",
               i = "{candidats}"),
             "nemetonshiny_projet_ambigu", candidats = candidats)
}

# ---------------------------------------------------------------- lecture

#' @noRd
mcp_lister_projets <- function(limite = 20) {
  .mcp_call({
    l <- projets_lister()
    n <- nrow(l)
    l <- utils::head(l, max(1L, as.integer(limite %||% 20)))
    list(total = n, projets = l)
  })
}

#' @noRd
mcp_resume_projet <- function(projet, langue = "fr") {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    s <- projet_lire(id, langue = langue %||% "fr")$synthese
    s$familles <- s$families
    s$families <- NULL
    s
  })
}

# ---------------------------------------------------------------- calcul detache

.mcp_job_path <- function(project_path) {
  file.path(project_path, "data", "compute_job.json")
}

.mcp_job_read <- function(project_path) {
  f <- .mcp_job_path(project_path)
  if (!file.exists(f)) return(NULL)
  tryCatch(jsonlite::read_json(f), error = function(e) NULL)
}

.mcp_job_write <- function(project_path, job) {
  .write_json_atomic(job, .mcp_job_path(project_path), auto_unbox = TRUE,
                     pretty = TRUE, null = "null")
  invisible(job)
}

.mcp_pid_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid %||% NA))
  length(pid) == 1L && !is.na(pid) && pid > 0L && isTRUE(tools::pskill(pid, 0L))
}

.mcp_job_running <- function(job) {
  !is.null(job) && (job$statut %||% "") %in% c("lancement", "en_cours") &&
    (.mcp_pid_alive(job$pid) ||
       # Lance mais pas encore enregistre par l'enfant : laisser 60 s.
       (is.null(job$pid) && .mcp_age_sec(job$lance_a) < 60))
}

.mcp_age_sec <- function(stamp) {
  t <- suppressWarnings(as.POSIXct(stamp %||% NA_character_,
                                   format = "%Y-%m-%dT%H:%M:%S"))
  if (length(t) != 1L || is.na(t)) return(Inf)
  as.numeric(difftime(Sys.time(), t, units = "secs"))
}

.mcp_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

#' Start the detached computation process
#'
#' A fresh `Rscript`, in its own session (`setsid` when available) so it
#' survives the end of the MCP server - one `claude -p` task per request.
#' @noRd
.mcp_spawn <- function(project_id, project_dir, log_path) {
  expr <- sprintf("nemetonshiny:::.mcp_child_compute(%s, %s)",
                  deparse(project_id), deparse(project_dir))
  rscript <- file.path(R.home("bin"), "Rscript")
  if (nzchar(Sys.which("setsid"))) {
    system2("setsid", c("-f", shQuote(rscript), "-e", shQuote(expr)),
            stdout = log_path, stderr = log_path, wait = FALSE)
  } else {
    system2(rscript, c("-e", shQuote(expr)),
            stdout = log_path, stderr = log_path, wait = FALSE)
  }
  invisible(TRUE)
}

#' Body of the detached computation (runs in the child process)
#'
#' Same path as the application: `.compute_run_capped()` (memory ceiling,
#' child log, file progress). Records its pid then its outcome in
#' `data/compute_job.json`.
#' @noRd
.mcp_child_compute <- function(project_id, project_dir) {
  options(nemeton.app_options = list(project_dir = project_dir))
  path <- get_project_path(project_id)
  if (is.null(path)) stop("Projet introuvable : ", project_id)
  job <- .mcp_job_read(path) %||% list(job_id = NA_character_)
  job$pid <- Sys.getpid()
  job$statut <- "en_cours"
  job$demarre_a <- .mcp_now()
  .mcp_job_write(path, job)

  res <- tryCatch(.compute_run_capped(project_id, get_app_options()),
                  error = function(e) list(success = FALSE, error = conditionMessage(e)))
  job <- .mcp_job_read(path) %||% job
  job$statut <- if (isTRUE(res$success)) "termine"
                else if (isTRUE(res$cancelled)) "annule" else "echec"
  if (!isTRUE(res$success) && !isTRUE(res$cancelled)) {
    err <- res$error %||% res$state$errors %||% "erreur inconnue"
    job$erreur <- paste(unlist(err), collapse = " ; ")
  }
  job$fin_a <- .mcp_now()
  .mcp_job_write(path, job)
  invisible(job)
}

#' Refuse to write into a project that is busy
#'
#' Shared by every tool that writes: a computation running (detached or in the
#' application) gives `nemetonshiny_calcul_en_cours`, an edit lock held in the
#' application gives `nemetonshiny_projet_verrouille`.
#'
#' @param id Project id.
#' @return The [projet_etat()] list.
#' @noRd
.mcp_garde_ecriture <- function(id, call = rlang::caller_env()) {
  etat <- projet_etat(id)
  .api_refuser_ancien(etat, call = call)
  if (.mcp_job_running(.mcp_job_read(etat$chemin))) {
    .api_abort("Un calcul est d\u00e9j\u00e0 en cours pour ce projet.",
               "nemetonshiny_calcul_en_cours", call = call)
  }
  if (progress_state_age_sec(id) < 60) {
    .api_abort("L'application calcule ce projet en ce moment.",
               "nemetonshiny_calcul_en_cours", call = call)
  }
  verrou <- tryCatch(lock_status(id), error = function(e) NULL)
  if (!is.null(verrou) && !isTRUE(verrou$stale)) {
    detenteur <- verrou$holder_label %||% verrou$holder_id %||% "?"
    .api_abort("Projet en cours d'\u00e9dition dans l'application ({detenteur}).",
               "nemetonshiny_projet_verrouille", call = call)
  }
  etat
}

#' @noRd
mcp_lancer_calcul <- function(projet) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    etat <- .mcp_garde_ecriture(id)
    path <- etat$chemin
    log_path <- file.path(path, "data", "compute_mcp.log")
    job <- list(job_id = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-",
                                paste0(sample(letters, 4), collapse = "")),
                statut = "lancement", source = "mcp", lance_a = .mcp_now(),
                log = log_path)
    .mcp_job_write(path, job)
    .mcp_spawn(id, get_projects_root(), log_path)
    list(projet = id, job_id = job$job_id, log = log_path,
         message = "Calcul lanc\u00e9 en t\u00e2che de fond ; suivre avec etat_calcul.")
  })
}

.mcp_log_tail <- function(log_path, n = 15L) {
  if (is.null(log_path) || !file.exists(log_path)) return(NULL)
  utils::tail(readLines(log_path, warn = FALSE), n)
}

#' @noRd
mcp_etat_calcul <- function(projet) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    path <- get_project_path(id)
    job <- .mcp_job_read(path)
    statut <- job$statut %||% "aucun"
    if (statut %in% c("lancement", "en_cours") && !.mcp_job_running(job)) {
      statut <- "echec"
      job$erreur <- "Le processus de calcul s'est arr\u00eat\u00e9 sans terminer."
    }
    prog <- read_progress_state(id)
    list(
      projet = id,
      statut = statut,
      job_id = job$job_id,
      lance_a = job$lance_a,
      fin_a = job$fin_a,
      erreur = job$erreur,
      phase = prog$status,
      progression = prog$progress,
      progression_max = prog$progress_max,
      indicateurs_faits = prog$indicators_completed,
      indicateurs_total = prog$indicators_total,
      tache = prog$current_task,
      log = if (identical(statut, "echec")) .mcp_log_tail(job$log)
    )
  })
}

#' @noRd
mcp_annuler_calcul <- function(projet) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    cancel_computation(id)
    list(projet = id, message = "Annulation demand\u00e9e ; le calcul s'arr\u00eate au prochain indicateur.")
  })
}

# ---------------------------------------------------------------- UGF

#' Load a project for writing its management units
#'
#' Parcels, tenements and UGF only - what [save_ug_data()] and
#' [save_parcels()] write back. A project without UGF gets the default layout
#' (one UGF per parcel) in memory.
#' @noRd
.mcp_projet_ugf <- function(id) {
  meta <- load_project_metadata(id) %||% list()
  meta$id <- meta$id %||% id
  projet <- list(id = id, metadata = meta, parcels = load_parcels(id))
  if (is.null(projet$parcels) || nrow(projet$parcels) == 0L) {
    .api_abort("Projet {.val {id}} : aucune parcelle cadastrale.",
               "nemetonshiny_projet_introuvable")
  }
  ug <- load_ug_data(id)
  if (is.null(ug)) {
    projet <- ug_init_default(projet)
  } else {
    projet$tenements <- ug$tenements
    projet$ugs <- ug$ugs
  }
  projet
}

#' Gap between each parcel and the tenements given for it
#'
#' `|parcel - union|` (holes, overflow) plus `sum - union` (overlaps), in m2,
#' in Lambert 93. A parcel passes under `max(1, 0.05 %)` of its area, the
#' tolerance of [validate_tiling()].
#'
#' @param parcels `sf` of the project's parcels, `idu` column.
#' @param imp `sf` of tenements, `idu` column.
#' @return data.frame `idu`, `ecart_m2`, `ok`.
#' @noRd
.mcp_ecart_pavage <- function(parcels, imp) {
  prev <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(prev)), add = TRUE)
  p <- sf::st_make_valid(sf::st_transform(parcels, 2154))
  t <- sf::st_make_valid(sf::st_transform(imp, 2154))
  ids <- unique(as.character(t$idu))
  ecart <- vapply(ids, function(i) {
    gp <- sf::st_geometry(p[as.character(p$idu) == i, ])
    gt <- sf::st_geometry(t[as.character(t$idu) == i, ])
    a_p <- sum(as.numeric(sf::st_area(gp)))
    a_s <- sum(as.numeric(sf::st_area(gt)))
    a_u <- as.numeric(sf::st_area(sf::st_union(gt)))
    a_i <- as.numeric(sf::st_area(suppressWarnings(
      sf::st_intersection(sf::st_union(gp), sf::st_union(gt)))))
    if (!length(a_i)) a_i <- 0
    # Trous (parcelle non couverte), debords (hors parcelle), chevauchements.
    (a_p - a_i) + (a_u - a_i) + (a_s - a_u)
  }, numeric(1))
  aires <- vapply(ids, function(i) {
    sum(as.numeric(sf::st_area(p[as.character(p$idu) == i, ])))
  }, numeric(1))
  data.frame(idu = ids, ecart_m2 = ecart,
             ok = ecart <= pmax(1, 0.0005 * aires), stringsAsFactors = FALSE)
}

#' Persist a re-tiled project and say what it invalidated
#' @noRd
.mcp_sauver_ugf <- function(id, projet, avec_parcelles = FALSE) {
  if (isTRUE(avec_parcelles)) save_parcels(id, projet$parcels)
  # Les ug_id sont neufs : les indicateurs calcules portent sur les anciens.
  # save_ug_data() les met deja de cote quand l'affectation change ; un second
  # invalidate_indicators() ne trouverait plus rien et repondrait FALSE.
  ok <- save_ug_data(id, projet)
  isTRUE(attr(ok, "indicateurs_invalides")) ||
    isTRUE(invalidate_indicators(id, motif = "ugf"))
}

#' Apply management units from a file
#'
#' Replaces the tenement layout of a project with the one of a GeoPackage or
#' GeoJSON file of tenements - typically the output of
#' `nemeton::construire_ugf_onf()` reworked outside the application.
#'
#' The file needs an IDU column (`idu`, `parent_parcelle_id` or `id`) and a
#' UGF label (`label_ugf`, `ugf` or `nom_ugf`); it may carry `onf_foret_id`,
#' `onf_foret_nom`, `onf_parcelle`, `onf_domaniale`, `onf_part` and `groupe`.
#'
#' Nothing is written unless every check passes: an IDU absent from the project
#' is an error (no cadastral parcel is created behind the user's back), and so
#' is a parcel whose tiling is not exact - the error names the parcels. With
#' `remplacer = TRUE` every parcel of the project must be in the file; with
#' `FALSE` only the parcels in the file are re-tiled, the others keep theirs.
#'
#' @param projet Project id or name.
#' @param fichier Path of the file.
#' @param remplacer Logical. Replace the whole layout (default) or only the
#'   parcels present in the file.
#' @return JSON: `ok`, `projet`, `n_ugf`, `n_tenements`, `ecart_pavage_m2`,
#'   `indicateurs_perimes`, `avertissements`.
#' @noRd
mcp_appliquer_ugf <- function(projet, fichier, remplacer = TRUE) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    .mcp_garde_ecriture(id)
    if (!is.character(fichier) || length(fichier) != 1L || !file.exists(fichier)) {
      .api_abort("Fichier introuvable : {.file {fichier}}.",
                 "nemetonshiny_fichier_invalide")
    }
    imp <- tryCatch(sf::st_read(fichier, quiet = TRUE), error = function(e) NULL)
    if (is.null(imp) || !inherits(imp, "sf") || nrow(imp) == 0L) {
      .api_abort("Fichier illisible ou vide : {.file {fichier}}.",
                 "nemetonshiny_fichier_invalide")
    }
    col_idu <- intersect(c("idu", "parent_parcelle_id", "id"), names(imp))[1]
    col_lab <- intersect(c("label_ugf", "ugf", "nom_ugf"), names(imp))[1]
    if (is.na(col_idu) || is.na(col_lab)) {
      .api_abort(c("Colonnes manquantes dans {.file {fichier}}.",
                   i = "Il faut un IDU (idu, parent_parcelle_id ou id) et un libell\u00e9 d'UGF (label_ugf, ugf ou nom_ugf)."),
                 "nemetonshiny_fichier_invalide")
    }
    imp$idu <- as.character(imp[[col_idu]])
    imp$label_ugf <- trimws(as.character(imp[[col_lab]]))
    if (any(is.na(imp$label_ugf) | !nzchar(imp$label_ugf))) {
      .api_abort("Des t\u00e8nements n'ont pas de libell\u00e9 d'UGF.",
                 "nemetonshiny_fichier_invalide")
    }
    if (is.na(sf::st_crs(imp))) sf::st_crs(imp) <- .tenement_guess_crs(imp)
    # Colonne de geometrie au nom fixe (`geom` dans un GeoPackage) : les
    # tenements repris plus bas lui sont empiles.
    if (!identical(attr(imp, "sf_column"), "geometry")) sf::st_geometry(imp) <- "geometry"

    p <- .mcp_projet_ugf(id)
    id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(p$parcels))[1]
    parcels <- p$parcels
    parcels$idu <- as.character(parcels[[id_col]])

    inconnus <- setdiff(unique(imp$idu), parcels$idu)
    if (length(inconnus)) {
      .api_abort(c("{length(inconnus)} IDU absent{?s} du projet : {.val {utils::head(inconnus, 10)}}.",
                   i = "Aucune parcelle n'est cr\u00e9\u00e9e ici ; le projet n'a pas \u00e9t\u00e9 modifi\u00e9."),
                 "nemetonshiny_idu_inconnu", idu = inconnus)
    }
    avert <- character(0)
    if (isTRUE(remplacer)) {
      manquantes <- setdiff(parcels$idu, imp$idu)
      if (length(manquantes)) {
        .api_abort(c("{length(manquantes)} parcelle{?s} du projet sans t\u00e8nement dans le fichier : {.val {utils::head(manquantes, 10)}}.",
                     i = "Les ajouter au fichier, ou appeler avec remplacer = FALSE ; le projet n'a pas \u00e9t\u00e9 modifi\u00e9."),
                   "nemetonshiny_pavage_invalide", parcelles = manquantes)
      }
    }
    pav <- .mcp_ecart_pavage(parcels, imp)
    if (!all(pav$ok)) {
      mauvaises <- pav$idu[!pav$ok]
      .api_abort(c("Pavage inexact pour {length(mauvaises)} parcelle{?s} : {.val {utils::head(mauvaises, 10)}}.",
                   i = "\u00c9cart total {round(sum(pav$ecart_m2[!pav$ok]), 1)} m\u00b2 ; le projet n'a pas \u00e9t\u00e9 modifi\u00e9."),
                   "nemetonshiny_pavage_invalide", parcelles = mauvaises)
    }

    # Parcelles hors du fichier (remplacer = FALSE) : leurs tenements sont
    # repris tels quels, sans libelle, donc rattaches a leur UGF actuelle par
    # recouvrement.
    if (!isTRUE(remplacer)) {
      garder <- p$tenements[!as.character(p$tenements$parent_parcelle_id) %in% imp$idu, ]
      if (nrow(garder)) {
        g <- sf::st_transform(garder, sf::st_crs(imp))
        g <- sf::st_sf(idu = as.character(g$parent_parcelle_id),
                       label_ugf = NA_character_, geometry = sf::st_geometry(g))
        commun <- intersect(names(imp), names(g))
        imp <- rbind(imp[, commun], g[, commun])
      }
    }

    p2 <- withCallingHandlers(
      tenement_import_replace(p, imp),
      warning = function(w) {
        avert <<- c(avert, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    # Groupe d'amenagement du fichier, s'il y en a un.
    if ("groupe" %in% names(imp)) {
      grp <- tapply(as.character(imp$groupe), imp$label_ugf, function(x) {
        x <- unique(stats::na.omit(x)); if (length(x) == 1L) x else NA_character_
      })
      k <- match(p2$ugs$label, names(grp))
      p2$ugs$groupe[!is.na(k)] <- grp[k[!is.na(k)]]
    }
    perimes <- .mcp_sauver_ugf(id, p2)
    list(projet = id, n_ugf = nrow(p2$ugs), n_tenements = nrow(p2$tenements),
         ecart_pavage_m2 = round(sum(pav$ecart_m2), 3),
         indicateurs_perimes = perimes,
         avertissements = if (length(avert)) avert)
  })
}

#' Cross a project with the ONF forest parcels
#'
#' Same crossing as the application's button (`nemeton::construire_ugf_onf()`,
#' with the project's ONF settings), applied to the project. About 15 s per
#' commune; the first call downloads the DGFiP file (376 MB, once).
#'
#' @param projet Project id or name.
#' @param purger Logical or `NULL`. `TRUE` keeps only the parcels under the
#'   forest regime (the others leave the project), `FALSE` keeps them all,
#'   `NULL` follows the project's setting.
#' @return JSON: `ok`, `projet`, `n_ugf`, `n_tenements`, `n_parcelles`,
#'   `n_cad`, `ecartees`, `ecart_median_m`, `indicateurs_perimes`.
#' @noRd
mcp_croiser_onf <- function(projet, purger = NULL) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    .mcp_garde_ecriture(id)
    p <- .mcp_projet_ugf(id)
    cfg <- project_onf_params(p$metadata)
    if (!is.null(purger)) cfg$purger <- isTRUE(purger)
    out <- onf_croise_tache(p, cfg)
    if (!identical(out$status, "ok")) {
      msg <- switch(out$status,
        unavailable = "Parcellaire ONF ou fichier DGFiP injoignable.",
        empty       = "Aucune for\u00eat publique sur l'emprise du projet.",
        no_overlap  = "Aucun recoupement entre le parcellaire ONF et les parcelles du projet.",
        "Croisement ONF impossible.")
      .api_abort(c(msg, i = "Le projet n'a pas \u00e9t\u00e9 modifi\u00e9."),
                 "nemetonshiny_onf_echec", statut = out$status)
    }
    ecartees <- out$ecartees %||% .onf_ecartees_vide()
    perimes <- .mcp_sauver_ugf(id, out$projet, avec_parcelles = nrow(ecartees) > 0L)
    r <- onf_croise_resume(out$tenements)
    list(projet = id, n_ugf = r$n_ugf, n_tenements = nrow(out$projet$tenements),
         n_parcelles = r$n_parcelles, n_cad = r$n_cad,
         ecartees = if (nrow(ecartees)) ecartees,
         ecart_median_m = suppressWarnings(as.numeric(out$calage[["ecart_median_m"]])),
         indicateurs_perimes = perimes)
  })
}

# ---------------------------------------------------------------- exports

#' @noRd
mcp_generer_rapport <- function(projet, langue = "fr", synthese = NULL) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    langue <- langue %||% "fr"
    f <- file.path(get_project_path(id), "exports",
                   sprintf("rapport_%s_%s.pdf", langue, format(Sys.time(), "%Y%m%d-%H%M%S")))
    list(projet = id, fichier = projet_rapport(id, f, langue = langue, synthese = synthese))
  })
}

#' @noRd
mcp_exporter_gpkg <- function(projet) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    f <- file.path(get_project_path(id), "exports",
                   sprintf("resultats_%s.gpkg", format(Sys.time(), "%Y%m%d-%H%M%S")))
    list(projet = id, fichier = projet_gpkg(id, f))
  })
}

#' @noRd
mcp_url_app <- function(projet, onglet = "synthesis") {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    onglet <- onglet %||% "synthesis"
    if (!onglet %in% DEEP_LINK_TABS) {
      cli::cli_abort("Onglet inconnu : {.val {onglet}}. Onglets : {.val {DEEP_LINK_TABS}}.")
    }
    port <- Sys.getenv("NEMETON_APP_PORT", "3838")
    list(projet = id,
         url = sprintf("http://127.0.0.1:%s/?project=%s&tab=%s", port,
                       utils::URLencode(id, reserved = TRUE), onglet))
  })
}

# ---------------------------------------------------------------- declaration

#' MCP tools exposed by nemetonshiny
#'
#' @return A list of `ellmer::tool()` definitions, for
#'   `mcptools::mcp_server(tools = )` (see `inst/mcp/server.R`).
#' @noRd
mcp_tools <- function() {
  t <- ellmer::type_string
  projet_arg <- t("Project id, or its name (case and accents ignored).")
  list(
    ellmer::tool(mcp_lister_projets, name = "lister_projets",
      description = "List the Nemeton forest projects (id, name, status, last update, computed indicators, up to date).",
      arguments = list(limite = ellmer::type_integer("Maximum number of projects (default 20).", required = FALSE))),
    ellmer::tool(mcp_resume_projet, name = "resume_projet",
      description = "Synthesis of a project: global score /100, NDP level, confidence and the 12 family scores. Reads only, never modifies the project.",
      arguments = list(projet = projet_arg,
                       langue = t("'fr' or 'en' (default 'fr').", required = FALSE))),
    ellmer::tool(mcp_lancer_calcul, name = "lancer_calcul",
      description = "Start the full indicator computation of a project in the background (minutes to more than an hour). Returns at once; follow with etat_calcul.",
      arguments = list(projet = projet_arg)),
    ellmer::tool(mcp_etat_calcul, name = "etat_calcul",
      description = "State of a project's background computation: status (en_cours, termine, echec, annule, aucun), progress, current task, error and log tail on failure.",
      arguments = list(projet = projet_arg)),
    ellmer::tool(mcp_annuler_calcul, name = "annuler_calcul",
      description = "Ask a project's running computation to stop.",
      arguments = list(projet = projet_arg)),
    ellmer::tool(mcp_generer_rapport, name = "generer_rapport",
      description = "Write the PDF report of a computed project into its exports folder and return the file path.",
      arguments = list(projet = projet_arg,
                       langue = t("'fr' or 'en' (default 'fr').", required = FALSE),
                       synthese = t("Optional synthesis comment (Markdown).", required = FALSE))),
    ellmer::tool(mcp_exporter_gpkg, name = "exporter_gpkg",
      description = "Write the GeoPackage of a computed project's results (one feature per management unit) and return the file path.",
      arguments = list(projet = projet_arg)),
    ellmer::tool(mcp_appliquer_ugf, name = "appliquer_ugf",
      description = "Replace the management units (UGF) of a project with those of a GeoPackage/GeoJSON file of tenements (columns idu and label_ugf, optional onf_foret_id, onf_parcelle...). Nothing is written if an IDU is unknown or a parcel is not exactly tiled. Computed indicators become outdated.",
      arguments = list(projet = projet_arg,
                       fichier = t("Path of the GeoPackage or GeoJSON file."),
                       remplacer = ellmer::type_boolean("TRUE (default): the file covers every parcel of the project. FALSE: only the parcels in the file are re-tiled.", required = FALSE))),
    ellmer::tool(mcp_croiser_onf, name = "croiser_onf",
      description = "Build the UGF of a project from the ONF forest parcels warped onto the cadastre, each UGF carrying its ONF forest and parcel number. About 15 s per commune. Computed indicators become outdated.",
      arguments = list(projet = projet_arg,
                       purger = ellmer::type_boolean("TRUE: drop the parcels outside the forest regime (private, or little covered by the ONF). FALSE: keep them all. Default: the project's setting.", required = FALSE))),
    ellmer::tool(mcp_url_app, name = "url_app",
      description = "URL opening the Nemeton application on a project and a tab (synthesis, selection, action_plan, terrain, monitoring, regeneration, famille_*).",
      arguments = list(projet = projet_arg,
                       onglet = t("Tab (default 'synthesis').", required = FALSE)))
  )
}

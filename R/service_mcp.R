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

#' @noRd
mcp_lancer_calcul <- function(projet) {
  .mcp_call({
    id <- .mcp_resolve_project(projet)
    etat <- projet_etat(id)
    if (isTRUE(etat$migration_necessaire)) {
      .api_abort(c("Projet {.val {id}} : migration n\u00e9cessaire avant le calcul.",
                   i = "L'ouvrir une fois dans l'application, ou appeler {.fn projet_migrer}."),
                 "nemetonshiny_projet_perime")
    }
    path <- etat$chemin
    if (.mcp_job_running(.mcp_job_read(path))) {
      .api_abort("Un calcul est d\u00e9j\u00e0 en cours pour ce projet.",
                 "nemetonshiny_calcul_en_cours")
    }
    if (progress_state_age_sec(id) < 60) {
      .api_abort("L'application calcule ce projet en ce moment.",
                 "nemetonshiny_calcul_en_cours")
    }
    verrou <- tryCatch(lock_status(id), error = function(e) NULL)
    if (!is.null(verrou) && !isTRUE(verrou$stale)) {
      detenteur <- verrou$holder_label %||% verrou$holder_id %||% "?"
      .api_abort("Projet en cours d'\u00e9dition dans l'application ({detenteur}).",
                 "nemetonshiny_projet_verrouille")
    }
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
    ellmer::tool(mcp_url_app, name = "url_app",
      description = "URL opening the Nemeton application on a project and a tab (synthesis, selection, action_plan, terrain, monitoring, regeneration, famille_*).",
      arguments = list(projet = projet_arg,
                       onglet = t("Tab (default 'synthesis').", required = FALSE)))
  )
}

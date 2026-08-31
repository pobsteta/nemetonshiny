#' Chained-run orchestration ("Tout calculer")
#'
#' @description
#' Application-level state machine that runs every engine of the app one after
#' another, then the AI generations, and reports what happened.
#'
#' Deliberately Shiny-free: the whole decision logic (what runs next, what is
#' skipped, what the final report says) is plain data so it can be tested
#' without a session. `mod_pipeline.R` holds the UI and the signalling; the
#' engines themselves stay where they are, in their own modules.
#'
#' Why signalling rather than calling the engines directly: each engine is an
#' `ExtendedTask` defined inside its module's server, and its arguments are
#' built from that tab's inputs (selected engines, buffer, corrected network,
#' S2 period...). An outside orchestrator would have to duplicate all of it and
#' would drift the day a tab gains an option. Instead the orchestrator posts a
#' request on `app_state` and the owning module launches its own engine through
#' the very code path its button uses.
#'
#' @name service_pipeline
#' @keywords internal
NULL


# =============================================================================
# Registre des etapes
# =============================================================================

#' Ordered registry of the chained run
#'
#' `id` is the contract between the orchestrator and the module that answers
#' it. `module` is informational (reports, debugging). `label` is the i18n key
#' shown in the progress panel and the final report.
#'
#' The order encodes what depends on what: the indicators come first (every
#' other view reads them), the AI generations come last (they summarise
#' everything above). Between the two, the engines are independent - the
#' ordering there is the reading order of the tabs.
#'
#' @noRd
PIPELINE_STEPS <- list(
  list(id = "indicateurs",     module = "home",          label = "pipeline_step_indicateurs"),
  # Correction LiDAR du reseau (~2-3 h) AVANT l'analyse : celle-ci consomme le
  # reseau corrige des qu'il existe sur disque. L'inverse produirait une analyse
  # sur reseau brut, puis une correction dont plus rien ne se servirait du run.
  list(id = "accessibilite_correction", module = "accessibility", label = "pipeline_step_accessibilite_correction"),
  list(id = "accessibilite",   module = "accessibility", label = "pipeline_step_accessibilite"),
  list(id = "desserte",        module = "desserte",      label = "pipeline_step_desserte"),
  # Typage du reseau : calcul metier a part entiere (volume mobilisable,
  # classes de desserte), pas un simple affichage - et il lit le cache que le
  # moteur vient de remplir, d'ou sa place juste apres.
  list(id = "desserte_typage", module = "desserte",      label = "pipeline_step_desserte_typage"),
  list(id = "desserte_integrite", module = "desserte",   label = "pipeline_step_desserte_integrite"),
  # reGeneration en TROIS temps, dans l'ordre que suit l'utilisateur a la main.
  # Ce n'est pas un decoupage cosmetique : `eobs_task` DETERMINE les annees
  # moyenne / canicule et les pousse dans les champs que le moteur lira ensuite.
  # Lancer le moteur seul le ferait tourner sur les valeurs par defaut codees en
  # dur (2018 / 2022), sans que rien ne le signale. Le gel R7 vient ensuite : il
  # enrichit le resultat courant, et `.regen_attach_r7()` reporte sa couche sur
  # le resultat du moteur - c'est justement le cas « analyse relancee apres R7 »
  # que ce helper couvre.
  list(id = "regen_annees",    module = "regeneration",  label = "pipeline_step_regen_annees"),
  # Precipitations et temperature moyenne E-OBS : deux telechargements CDS
  # d'environ 800 Mo chacun. Places APRES la detection des annees, qui les
  # borne, et avant le gel et le moteur, qui les consomment.
  list(id = "regen_eobs_rr",   module = "regeneration",  label = "pipeline_step_regen_eobs_rr"),
  list(id = "regen_eobs_tg",   module = "regeneration",  label = "pipeline_step_regen_eobs_tg"),
  list(id = "regen_gel",       module = "regeneration",  label = "pipeline_step_regen_gel"),
  list(id = "regeneration",    module = "regeneration",  label = "pipeline_step_regeneration"),
  # Creation des zones de suivi AVANT les trois moteurs sante : ils exigent
  # tous un `zone_id`, et sans zone enregistree ils se sautent - ce qu'a montre
  # le premier run reel sur Couchey (« Aucune zone de suivi enregistree »).
  # `build_project_monitoring_zones()` est en upsert : relancer l'etape recree
  # les zones du projet plutot que d'en accumuler.
  list(id = "sante_zone",      module = "monitoring",    label = "pipeline_step_sante_zone"),
  list(id = "sante_fast",      module = "monitoring",    label = "pipeline_step_sante_fast"),
  list(id = "sante_fordead",   module = "monitoring",    label = "pipeline_step_sante_fordead"),
  list(id = "sante_reconfort", module = "monitoring",    label = "pipeline_step_sante_reconfort"),
  list(id = "ia_synthese",     module = "synthesis",     label = "pipeline_step_ia_synthese"),
  list(id = "ia_plan",         module = "action_plan",   label = "pipeline_step_ia_plan")
)

#' Statuts d'etape
#'
#' `skipped` n'est PAS une erreur : c'est un module qui declare, en toute
#' connaissance de cause, qu'il n'a pas de quoi tourner (pas de zone monitoring
#' enregistree, pas de periode Sentinel-2, moteur non installe). Les distinguer
#' est ce qui rend le rapport final lisible - « 3 sautees faute de
#' configuration » ne se lit pas comme « 3 en echec ».
#' @noRd
PIPELINE_STATUSES <- c("pending", "running", "ok", "error", "skipped", "cancelled")

#' All step ids, in run order
#' @noRd
pipeline_all_step_ids <- function() {
  vapply(PIPELINE_STEPS, function(s) s$id, character(1))
}

#' Look a step up by id
#' @noRd
pipeline_step_def <- function(step_id) {
  for (s in PIPELINE_STEPS) if (identical(s$id, step_id)) return(s)
  NULL
}


# =============================================================================
# Etat d'un run
# =============================================================================

#' Start a new chained run
#'
#' @param step_ids Character. Steps to run, in `PIPELINE_STEPS` order. Unknown
#'   ids are dropped rather than aborting the run: a stale bookmark or an old
#'   saved scope must not make the button unusable.
#' @param profil Character. Expert profile applied to every AI generation.
#' @return A run state (plain list).
#' @noRd
pipeline_new_run <- function(step_ids = pipeline_all_step_ids(), profil = NULL) {
  connus <- pipeline_all_step_ids()
  step_ids <- intersect(connus, step_ids)   # intersect() impose l'ordre du registre
  list(
    run_id  = sprintf("run_%s_%s", format(Sys.time(), "%Y%m%d%H%M%S"),
                      paste(sample(letters, 4, replace = TRUE), collapse = "")),
    profil  = profil,
    steps   = step_ids,
    index   = 1L,
    results = stats::setNames(vector("list", length(step_ids)), step_ids),
    started = Sys.time(),
    ended   = NULL
  )
}

#' Id of the step awaiting execution, or NULL when the run is over
#' @noRd
pipeline_current_step <- function(state) {
  if (is.null(state) || length(state$steps) == 0L) return(NULL)
  if (state$index > length(state$steps)) return(NULL)
  state$steps[[state$index]]
}

#' Is the run finished?
#' @noRd
pipeline_is_done <- function(state) {
  is.null(pipeline_current_step(state))
}

#' Record the outcome of a step and move to the next one
#'
#' Records under the step's OWN id rather than at the current index: a late
#' answer from a step already recorded (a module answering twice, a stale
#' reply from a previous run) must not overwrite its successor's result nor
#' shift the cursor. An id outside the run is ignored.
#'
#' @param state Run state.
#' @param step_id Character. Step that just finished.
#' @param status One of `PIPELINE_STATUSES`.
#' @param message Character or NULL. Shown verbatim in the report.
#' @return The updated state.
#' @noRd
pipeline_record <- function(state, step_id, status = "ok", message = NULL) {
  if (is.null(state)) return(state)
  if (!step_id %in% state$steps) return(state)
  if (!status %in% PIPELINE_STATUSES) status <- "error"

  deja <- state$results[[step_id]]
  if (!is.null(deja) && !identical(deja$status, "running")) {
    return(state)   # deja tranchee : une reponse tardive ne rejoue pas
  }

  state$results[[step_id]] <- list(
    status  = status,
    message = message,
    started = if (!is.null(deja)) deja$started else Sys.time(),
    ended   = Sys.time()
  )

  # N'avancer que si c'est bien l'etape courante qui vient de repondre.
  if (identical(pipeline_current_step(state), step_id)) {
    state$index <- state$index + 1L
    if (pipeline_is_done(state)) state$ended <- Sys.time()
  }
  state
}

#' Mark the current step as started
#' @noRd
pipeline_mark_running <- function(state) {
  cur <- pipeline_current_step(state)
  if (is.null(cur)) return(state)
  state$results[[cur]] <- list(status = "running", message = NULL,
                               started = Sys.time(), ended = NULL)
  state
}

#' Cancel the run: everything not yet decided becomes `cancelled`
#' @noRd
pipeline_cancel <- function(state) {
  if (is.null(state)) return(state)
  for (id in state$steps) {
    r <- state$results[[id]]
    if (is.null(r) || identical(r$status, "running")) {
      state$results[[id]] <- list(status = "cancelled", message = NULL,
                                  started = if (is.null(r)) NULL else r$started,
                                  ended = Sys.time())
    }
  }
  state$index <- length(state$steps) + 1L
  state$ended <- Sys.time()
  state
}


# =============================================================================
# Rapport
# =============================================================================

#' Tabular report of a run
#'
#' @param state Run state.
#' @return A data.frame with one row per step: `step_id`, `label` (i18n key),
#'   `status`, `message`, `seconds`.
#' @noRd
pipeline_report <- function(state) {
  vide <- data.frame(step_id = character(0), label = character(0),
                     status = character(0), message = character(0),
                     seconds = numeric(0), stringsAsFactors = FALSE)
  if (is.null(state) || length(state$steps) == 0L) return(vide)

  lignes <- lapply(state$steps, function(id) {
    r   <- state$results[[id]]
    def <- pipeline_step_def(id)
    secs <- if (!is.null(r) && !is.null(r$started) && !is.null(r$ended)) {
      as.numeric(difftime(r$ended, r$started, units = "secs"))
    } else NA_real_
    data.frame(
      step_id = id,
      label   = if (is.null(def)) id else def$label,
      status  = if (is.null(r)) "pending" else r$status,
      message = if (is.null(r) || is.null(r$message)) NA_character_ else as.character(r$message)[1],
      seconds = secs,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, lignes)
}

#' Count of each status in a run
#' @noRd
pipeline_tally <- function(state) {
  rep <- pipeline_report(state)
  vapply(c("ok", "error", "skipped", "cancelled"),
         function(s) sum(rep$status == s), integer(1))
}


# =============================================================================
# Protocole orchestrateur <-> modules
# =============================================================================
#
# Deux champs de `app_state`, et rien d'autre :
#
#   app_state$pipeline_request = list(run_id, step_id, profil, ts)
#       pose par l'orchestrateur : « module proprietaire de `step_id`, lance
#       ton moteur ».
#   app_state$pipeline_answer  = list(run_id, step_id, status, message, ts)
#       pose par le module : « voila ce que ca a donne ».
#
# Regle unique et non negociable cote module : TOUT chemin de code qui a
# reconnu une requete doit finir par une reponse - succes, echec, ou `skipped`.
# Un module qui se tait bloque la chaine sur cette etape, sans rien afficher a
# l'utilisateur, et le seul recours devient le bouton d'arret. C'est le mode de
# defaillance a surveiller quand on branche un nouveau moteur.

#' Does this request target the given step?
#'
#' @param request The value of `app_state$pipeline_request` (or NULL).
#' @param step_id Character. Step owned by the calling module.
#' @return `TRUE` when the module must act on this request.
#' @noRd
pipeline_targets <- function(request, step_id) {
  !is.null(request) &&
    !is.null(request$step_id) &&
    identical(request$step_id, step_id)
}

#' Answer a pipeline request
#'
#' `run_id` travels back with the answer so the orchestrator can drop a reply
#' from a previous run - a module whose engine finishes long after the user
#' cancelled and relaunched would otherwise advance the new run by one step.
#'
#' @param app_state The shared `reactiveValues`.
#' @param request The request being answered.
#' @param status One of `PIPELINE_STATUSES`.
#' @param message Character or NULL. Shown verbatim in the report - prefer an
#'   i18n string, this reaches the user.
#' @return Invisibly `TRUE`.
#' @noRd
pipeline_answer <- function(app_state, request, status = "ok", message = NULL) {
  if (is.null(request)) return(invisible(FALSE))
  app_state$pipeline_answer <- list(
    run_id  = request$run_id,
    step_id = request$step_id,
    status  = status,
    message = message,
    ts      = Sys.time()
  )
  invisible(TRUE)
}

#' Expert profile requested for this run, if any
#'
#' @param request The value of `app_state$pipeline_request`.
#' @return Character scalar, or NULL.
#' @noRd
pipeline_profil <- function(request) {
  if (is.null(request) || is.null(request$profil)) return(NULL)
  p <- as.character(request$profil)[1]
  if (!nzchar(p)) NULL else p
}

#' Message d'erreur reel d'un `ExtendedTask`, pour le rapport de la chaine
#'
#' @description
#' `task$result()` RE-LEVE l'erreur du worker : c'est le seul endroit ou son
#' message existe encore. Sans cette extraction, le rapport du lancement
#' enchaine n'affichait qu'un « Erreur » nu - et sur des moteurs qui tournent
#' des heures (13 h 40 pour l'ingest FAST, 4 h 10 pour FORDEAD sur Couchey),
#' c'est la seule information qui permette de savoir quoi corriger sans tout
#' relancer.
#'
#' @param task An `ExtendedTask`.
#' @param defaut Character. Message de repli quand rien n'est extractible.
#' @return Character scalar.
#' @noRd
pipeline_task_error <- function(task, defaut = "error") {
  msg <- tryCatch({
    res <- task$result()
    # Une tache peut « reussir » en rendant une liste d'echec (contrat
    # `list(status = "error", ...)` de plusieurs moteurs de l'app).
    if (is.list(res) && !is.null(res$detail)) as.character(res$detail)[1]
    else if (is.list(res) && !is.null(res$reason)) as.character(res$reason)[1]
    else NULL
  }, error = function(e) conditionMessage(e))
  if (is.null(msg) || !nzchar(msg)) return(defaut)
  # Tronquer : un traceback Python (FORDEAD via reticulate) tiendrait sur des
  # dizaines de lignes et noierait le rapport.
  msg <- gsub("\\s+", " ", .strip_ansi(msg))
  if (nchar(msg) > 300) paste0(substr(msg, 1, 300), " [...]") else msg
}

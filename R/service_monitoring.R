#' Async ingestion task for the Monitoring tab (E6.b phase 2)
#'
#' @description
#' Wraps `nemeton::ingest_sentinel2_timeseries()` in a
#' `shiny::ExtendedTask` so the call can run in a `future`
#' worker without freezing the Shiny main loop. The fetch + STAC
#' search + per-plot extraction takes minutes; we don't want the
#' UI to lock for that long.
#'
#' Mirrors the pattern used by `mod_home.R::parcels_task` (single
#' source for the async pattern in this app).
#'
#' Usage:
#' ```r
#' fast_task <- run_ingestion_async()
#' fast_task$invoke(zone_id, start, end, bands, max_cloud)
#' shiny::observe({
#'   result <- tryCatch(fast_task$result(), error = function(e) {
#'     if (inherits(e, "shiny.silent.error")) stop(e)
#'     NULL
#'   })
#'   if (!is.null(result)) {  # task completed successfully
#'     ...
#'   }
#' })
#' ```
#'
#' @return A `shiny::ExtendedTask` object.
#' @noRd
# ---------------------------------------------------------------------------
# Log de l'enfant plafonne (v0.143.16, coeur >= 0.195.0)
# ---------------------------------------------------------------------------
# `nemeton::run_memory_capped()` lancait l'enfant avec `stdout = ""`, c'est-a-dire
# « heriter du parent ». Le parent est un worker `future` multisession, que
# `parallelly` demarre avec `OUT=/dev/null` : la sortie de l'enfant - traceback
# python d'IOTA2, message d'erreur R - etait donc jetee a la source. Un run de
# 20 h pouvait echouer sans laisser un mot d'explication (Couchey, 2026-09-03).
#
# Le coeur 0.195.0 accepte `log_path` : les deux flux, fusionnes, dans un
# fichier conserve quel que soit le sort de l'enfant. C'est le pendant du NDJSON
# archive en v0.143.15 - le NDJSON dit JUSQU'OU on est alle, le log dit
# POURQUOI ca s'est arrete.
#
# Le fichier porte un nom stable (`<pipeline>_child.log`) pour que l'utilisateur
# et le support sachent ou regarder sans chercher. La rotation a lieu au
# DEMARRAGE : le coeur conservant le fichier meme en cas de succes, sans
# rotation le run suivant ecraserait la trace du precedent - exactement le
# defaut qu'on vient de corriger sur le NDJSON.
.child_log_path <- function(data_dir, name, keep = 5L) {
  if (is.null(data_dir) || !is.character(data_dir) || length(data_dir) != 1L ||
      !nzchar(data_dir)) {
    return(NULL)
  }
  if (!dir.exists(data_dir)) {
    tryCatch(dir.create(data_dir, recursive = TRUE, showWarnings = FALSE),
             error = function(e) NULL)
    if (!dir.exists(data_dir)) return(NULL)
  }
  p <- file.path(data_dir, paste0(name, "_child.log"))
  if (file.exists(p)) {
    cible <- paste0(p, ".prev-", format(Sys.time(), "%Y%m%d-%H%M%S"))
    ok <- tryCatch(file.rename(p, cible), error = function(e) FALSE)
    # Renommage refuse : on efface plutot que de laisser l'enfant APPEND sur la
    # trace du run precedent - deux runs melanges valent moins qu'un seul.
    if (!isTRUE(ok)) tryCatch(unlink(p), error = function(e) invisible(NULL))
    else utils::getFromNamespace(".prune_run_traces", "nemetonshiny")(
      p, keep = keep, motif = ".prev-*")
  }
  p
}

# Le coeur installe accepte-t-il `log_path` ? Meme motif de garde que
# `package`/`options` ailleurs dans l'app : on ne casse jamais sur un coeur plus
# ancien, on perd seulement la trace.
.capped_accepts_log_path <- function() {
  if (!requireNamespace("nemeton", quietly = TRUE)) return(FALSE)
  "log_path" %in% names(formals(nemeton::run_memory_capped))
}

# Appelle `run_memory_capped()` en laissant tomber `log_path` sur un coeur qui
# ne le connait pas. Sans ce filtre, passer l'argument a un coeur < 0.195.0
# leverait « unused argument » et casserait un chemin qui marchait.
.run_capped <- function(...) {
  a <- list(...)
  if (!.capped_accepts_log_path()) a$log_path <- NULL
  do.call(nemeton::run_memory_capped, a)
}



run_ingestion_async <- function() {
  # Capture the package source path (when running via devtools::load_all)
  # so the future worker can re-load nemetonshiny - workers don't
  # inherit the parent's loaded namespaces.
  .dev_pkg_path <- tryCatch(
    if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
      find.package("nemetonshiny")
    else NULL,
    error = function(e) NULL
  )

  # Capture diagnostic env vars in the parent so we can replay them
  # inside the worker. `future::multisession` workers are separate
  # Rscript.exe processes on Windows that may have been spawned BEFORE
  # the user `Sys.setenv()`d these - they then run blind. We pickle
  # the values via the auto-captured globals mechanism (future scans
  # the expression for symbols defined in the parent) and re-set them
  # at the top of the promise body.
  .worker_envvars <- .capture_worker_envvars()

  shiny::ExtendedTask$new(function(zone_id, start, end, bands,
                                   max_cloud = 20, db_url = "",
                                   progress_path = NULL,
                                   cache_dir = NULL,
                                   skip_cached = TRUE,
                                   log_path = NULL,
                                   lang = "fr",
                                   cancel_path = NULL,
                                   # v0.85.2.9000 - sentinelle de run
                                   # ecrite cote worker (survit a la
                                   # fermeture de session) ; lue au
                                   # relancement par .detect_ingest_state().
                                   sentinel_path = NULL,
                                   # v0.55.0 - pre-calcul des 4 cartes
                                   # FAST deplace du helper app vers
                                   # l'API native `nemeton@v0.61.0` :
                                   # `prewarm_alerts = TRUE` + chemin
                                   # `prewarm_mask_cache_dir`. Le coeur
                                   # enchaine les 4
                                   # `read_fast_alert_raster()` en fin
                                   # d'ingestion, dans le meme process
                                   # worker -> progress events
                                   # `fast_prewarm:*` natifs, cancel
                                   # cooperatif gere cote coeur.
                                   # v0.54.0 (prec.) faisait ce
                                   # pre-calcul via un helper app
                                   # `.prewarm_fast_alerts()` retire
                                   # ici car redondant avec spec 018.
                                   prewarm_alerts = TRUE,
                                   prewarm_mask_cache_dir = NULL,
                                   # Nom d'affichage (projet) resolu cote
                                   # parent depuis app_state - toujours a jour,
                                   # contrairement au nom de zone en DB qui peut
                                   # etre perime (projet renomme). NULL -> repli
                                   # sur .resolve_zone_name (tests / legacy).
                                   project_name = NULL) {
    if (requireNamespace("future", quietly = TRUE)) {
      plan_classes <- class(future::plan())
      is_parallel <- any(c("multisession", "multicore", "cluster") %in% plan_classes)
      if (!is_parallel) .ensure_async_plan()
    }
    promises::future_promise({
      # spec 008 sect.4 - rendre la memoire au systeme : le worker est PERSISTANT.
      on.exit(.release_worker_memory(), add = TRUE)
      # Replay diagnostic env vars captured in the parent so the
      # worker sees the same NEMETON_S2_CACHE_DEBUG / NEMETON_*
      # values as the user set in the main session.
      .apply_worker_envvars(.worker_envvars)

      # Mirror nemeton's cli::cli_* / message() lines into a log file
      # the parent process can tail. Earlier attempt via sink() did NOT
      # work: in non-interactive mode cli writes via `cat(file =
      # stderr())` (a direct connection write), which bypasses
      # sink(type = "message") entirely. We use withCallingHandlers
      # later in the body to catch the `message` / `warning` conditions
      # nemeton signals and rewrite them to disk with explicit flush().
      .ws_log_conn <- NULL
      if (!is.null(log_path) && nzchar(log_path)) {
        .ws_log_conn <- tryCatch(
          file(log_path, open = "wt", encoding = "UTF-8"),
          error = function(e) NULL
        )
        if (!is.null(.ws_log_conn)) {
          # `after = FALSE` pour la meme raison que la fermeture DB plus
          # bas : `.release_worker_memory()` efface `.ws_log_conn` de la
          # frame avant que ce handler ne s'evalue. Ici l'echec etait
          # SILENCIEUX - le `tryCatch` l'avalait - donc la connexion de
          # log n'etait jamais fermee, elle fuyait jusqu'a la mort du
          # worker. Meme defaut, sans le symptome qui l'aurait signale.
          on.exit(
            tryCatch(close(.ws_log_conn), error = function(e) NULL),
            add = TRUE, after = FALSE
          )
        }
      }
      # Helper that writes one line to the log connection and flushes
      # immediately. Buffering on text-mode `file()` connections only
      # spills on close; flushing per line is what makes the parent
      # `reactivePoll` see content during the run, not at the end.
      .ws_log_line <- function(line) {
        if (is.null(.ws_log_conn)) return(invisible(NULL))
        tryCatch({
          writeLines(sub("[\r\n]+$", "", line),
                     .ws_log_conn, useBytes = TRUE)
          flush(.ws_log_conn)
        }, error = function(e) invisible(NULL))
      }

      # Re-load nemetonshiny in the worker so we can use the
      # URL-resolution helper. In dev (load_all), .dev_pkg_path points to
      # the source tree; in prod the installed namespace is used.
      if (!is.null(.dev_pkg_path) && requireNamespace("pkgload", quietly = TRUE)) {
        pkgload::load_all(.dev_pkg_path, quiet = TRUE)
      } else {
        loadNamespace("nemetonshiny")
      }

      # The observer that calls $invoke() pre-resolves the URL from
      # `app_state$current_project` (which workers can't reach) and
      # passes it explicitly. An empty string means no PG env vars
      # AND no project - i.e. no DB at all.
      con <- get_monitoring_db_connection(db_url = db_url)
      if (is.null(con)) {
        stop("Monitoring DB not configured (set NEMETON_DB_URL, NEMETON_DB_HOST/_PORT/_NAME/_USER/_PASSWORD, or open a project to use the local SQLite fallback).")
      }
      # `after = FALSE` : cette fermeture doit s'evaluer AVANT
      # `.release_worker_memory()`, enregistre en tete de corps. Les
      # handlers `on.exit` s'executent dans l'ORDRE D'ENREGISTREMENT, et
      # la liberation fait `rm(list = ls(envir = env), envir = env)` sur
      # la frame du worker - `con` compris. Enregistree apres elle, cette
      # ligne levait « objet 'con' introuvable » APRES que le corps ait
      # fait et persiste son travail (run Couchey du 2026-08-31 : les
      # trois moteurs Sante, `ingest_run.json` a `done`, 183 scenes).
      # Deplacer la liberation en fin de corps serait plus lisible mais
      # la retirerait des chemins d'echec precoce (le `stop()` juste
      # au-dessus), ou rendre la memoire compte le plus.
      on.exit(close_monitoring_db_connection(con), add = TRUE, after = FALSE)

      # ntfy push channel - resolved worker-side (env vars replayed
      # above). NULL when NEMETON_NTFY_TOPIC is unset -> every
      # `.ntfy_send()` call below is a silent no-op. Symetrique avec
      # FORDEAD (cf. run_fordead_async). v0.42.1.
      ntfy <- .ntfy_config()
      i18n <- get_i18n(lang %||% "fr")

      # v0.43.2 - resolve the display name once so the start push reads
      # " (zone villards) " instead of " (zone 1) ". Priorite au nom de
      # projet passe par le parent (a jour) ; repli DB si absent. Silent
      # fallback to the integer id on any error - le resolveur est
      # cosmetique et NE DOIT JAMAIS abort une ingestion longue.
      zone_name <- project_name %||% .resolve_zone_name(con, zone_id)

      # Composite progress callback: the file writer the parent's
      # reactivePoll tails PLUS a worker-side ntfy push when the
      # first per-scene event arrives (one-shot, dedupe via state env).
      # Evite la noise pour les ingestions courtes et le spam pour
      # les ingestions longues (30-100 scenes).
      progress_cb <- .build_ingest_progress_callback(progress_path,
                                                     ntfy, i18n)

      # Timing for the complete-message. Sys.time() in the worker is
      # naturally aligned with the user's perception of the run.
      .ws_t0 <- Sys.time()

      .ntfy_send(
        ntfy,
        sprintf(i18n$t("monitoring_ntfy_ingest_start"),
                zone_name),
        tags  = "satellite",
        title = .ntfy_title("FAST", zone_name)
      )

      # Heartbeat 1/3: worker reached the start of its body. Useful
      # when the worker is silent - if we don't see this in the
      # console after invoke, the worker never spawned (future plan
      # broken).
      .ws_emit(progress_cb, list(current = "s2:worker_started",
                                 cache_dir = cache_dir,
                                 skip_cached = skip_cached))

      # v0.85.2.9000 - Sentinelle " running " (worker-side, survit a la
      # fermeture de session). Porte l'identite du run pour que le
      # bandeau de reprise affiche zone + periode au relancement.
      .write_ingest_sentinel(sentinel_path, "running", list(
        zone_id    = zone_id,
        zone_name  = zone_name,
        date_from  = as.character(start),
        date_to    = as.character(end),
        started_at = as.numeric(.ws_t0),
        pid        = Sys.getpid(),
        host       = unname(Sys.info()[["nodename"]] %||% "")
      ))

      # If a cache_dir is provided, ensure the directory exists before
      # the worker hands it to nemeton. The worker can't reach `dir.create`
      # if the path's parent doesn't exist yet - happens on a brand-new
      # project that hasn't created its `data/` folder yet.
      if (!is.null(cache_dir) && nzchar(cache_dir)) {
        if (!dir.exists(cache_dir)) {
          tryCatch(
            dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE),
            error = function(e) invisible(NULL)
          )
        }
      }

      # Heartbeat 2/3: about to enter the actual nemeton call. If we
      # see this but never see scene events, nemeton is hanging at
      # startup (probably the STAC query).
      .ws_emit(progress_cb, list(current = "s2:nemeton_call_starting",
                                 zone_id = zone_id,
                                 n_bands = length(bands)))

      # Capture STAC / HTTP warnings emitted by nemeton (e.g. when
      # Planetary Computer returns 504 and nemeton falls back to "0
      # scenes found" silently). We propagate them back to the main
      # process so the result observer can surface the real cause
      # instead of a misleading "Telechargement termine : 0 scenes".
      #
      # Also wrap in tryCatch so any R error inside the worker
      # surfaces explicitly via the progress channel BEFORE the
      # future rejects - the standard future rejection message
      # ("MultisessionFuture was interrupted") swallows the actual
      # R error and leaves the user with no diagnostic.
      warns <- character()
      summary <- tryCatch(
        withCallingHandlers(
          nemeton::ingest_sentinel2_timeseries(
            con               = con,
            zone_id           = zone_id,
            start             = start,
            end               = end,
            bands             = bands,
            max_cloud         = max_cloud,
            skip_cached       = skip_cached,
            cache_dir         = cache_dir,
            progress_callback = progress_cb,
            # v0.52.0 - cancel cooperatif (nemeton@v0.53.0). Le worker
            # polled `file.exists(cancel_path)` entre chaque tuile ; si
            # present -> sortie propre avec commit partiel + resume
            # `status = "cancelled"`. Le caller (mod_monitoring) ecrit
            # le fichier sur clic " Annuler le diagnostic ", et le
            # supprime avant chaque lancement pour eviter un cancel
            # fantome persistant.
            cancel_path       = cancel_path,
            # v0.55.0 - pre-calcul natif des 4 cartes FAST (spec 018
            # nemeton@v0.61.0). Quand `prewarm_alerts = TRUE`, le coeur
            # enchaine 4 `read_fast_alert_raster()` (NDVI/NBR x count/
            # rolling) en fin d'ingestion reussie + remplit le cache
            # D6 sous `<prewarm_mask_cache_dir>/zone_<id>/`. Le
            # `progress_callback` emet les events `fast_prewarm:*`
            # consommes par l'observer parent.
            prewarm_alerts         = prewarm_alerts,
            prewarm_mask_cache_dir = prewarm_mask_cache_dir
          ),
          # Catch every `message()` signaled by nemeton - including
          # `.s2_cache_log()` traces (gated on NEMETON_S2_CACHE_DEBUG)
          # AND cli's `cli_alert_*` / `cli_inform` calls (cli inherits
          # from `message` via `rlang::inform`). We rewrite each one to
          # the log file with explicit flush so the parent tail picks
          # it up live, then call `muffleMessage` to suppress the
          # original write to stderr (which `future` discards anyway).
          message = function(m) {
            .ws_log_line(conditionMessage(m))
            invokeRestart("muffleMessage")
          },
          warning = function(w) {
            .ws_log_line(paste0("warning: ", conditionMessage(w)))
            warns <<- c(warns, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          .ws_emit(progress_cb, list(
            current       = "s2:fatal_error",
            error_message = conditionMessage(e),
            error_class   = paste(class(e), collapse = "/")
          ))
          # v0.85.2.9000 - sentinelle terminale " error " cote worker.
          .write_ingest_sentinel(sentinel_path, "error", list(
            error_message = conditionMessage(e),
            finished_at   = as.numeric(Sys.time())
          ))
          .ntfy_send(
            ntfy,
            sprintf(i18n$t("monitoring_ntfy_ingest_error"),
                    conditionMessage(e)),
            priority = "high", tags = "rotating_light",
            title    = "Nemeton FAST"
          )
          stop(e)
        }
      )

      # Heartbeat 3/3: nemeton::ingest_sentinel2_timeseries a rendu la
      # main. Si l'evenement apparait dans la console / le progress file
      # mais que le bouton ne se reactive pas cote UI (et que la
      # notification de completion ne sort jamais), le worker est en
      # train de finaliser sa sortie (typiquement un checkpoint SQLite
      # sur le fichier WAL apres les INSERTs obs_pixel). L'utilisateur
      # peut cliquer " Annuler " pour force-unlock l'UI sans risque
      # (le worker continuera son commit en arriere-plan).
      .ws_emit(progress_cb, list(
        current  = "s2:ingest_done",
        n_scenes = as.integer(summary$n_scenes %||% 0L),
        n_obs    = as.integer(summary$n_obs_inserted %||% 0L)
      ))

      # v0.85.2.9000 - sentinelle terminale cote worker. `cancelled`
      # quand le coeur a honore le cancel cooperatif (commit partiel),
      # `done` sinon. Ecrite AVANT le prewarm/finalisation pour que le
      # relancement ne voie jamais un faux " running " apres coup.
      .write_ingest_sentinel(
        sentinel_path,
        if (identical(as.character(summary$status %||% ""), "cancelled"))
          "cancelled" else "done",
        list(
          n_scenes    = as.integer(summary$n_scenes %||% 0L),
          finished_at = as.numeric(Sys.time())
        )
      )

      # v0.55.0 - Le pre-calcul des 4 cartes FAST est desormais fait
      # PAR LE COEUR (spec 018 nemeton@v0.61.0) via les params
      # `prewarm_alerts = TRUE` et `prewarm_mask_cache_dir = ...`
      # passes a `ingest_sentinel2_timeseries()` ci-dessus. Le helper
      # app `.prewarm_fast_alerts()` (livre en v0.54.0) est retire
      # ici car il faisait double emploi avec l'API native. Les
      # progress events `fast_prewarm:*` sont emis par le coeur via
      # `progress_callback` et captures par l'observer parent en
      # `mod_monitoring.R` qui les transforme en toasts localises.

      duration_sec <- as.numeric(difftime(Sys.time(), .ws_t0,
                                          units = "secs"))
      # v0.70.4 - Le format `monitoring_ntfy_ingest_complete` ne
      # consomme plus `n_obs_inserted` (toujours 0 depuis
      # nemeton@v0.58.0). 2 args : `n_scenes` + `duration`.
      .ntfy_send(
        ntfy,
        sprintf(i18n$t("monitoring_ntfy_ingest_complete"),
                as.integer(summary$n_scenes %||% 0L),
                .format_duration_human(duration_sec)),
        tags  = "white_check_mark",
        title = .ntfy_title("FAST", zone_name)
      )

      list(
        status       = "success",
        summary      = summary,
        warnings     = warns,
        duration_sec = duration_sec,
        timestamp    = Sys.time()
      )
    }, seed = TRUE)
  })
}


# v0.55.0 - helper `.prewarm_fast_alerts()` retire : la logique est
# desormais dans le coeur `nemeton::ingest_sentinel2_timeseries()` via
# les params `prewarm_alerts = TRUE` + `prewarm_mask_cache_dir`
# (spec 018 v0.61.0). Le worker app passe simplement les 2 params,
# le coeur enchaine en interne les 4 `read_fast_alert_raster()` avec
# le meme `con` / `cache_dir` / `cancel_path` qu'il tient deja.


#' Build a worker-side progress writer
#'
#' Returns a function that serialises a single event as JSON to
#' `progress_path` (atomic write via .tmp + rename). Returns NULL when
#' no path is provided so `nemeton::ingest_sentinel2_timeseries()`
#' sees `progress_callback = NULL` and falls back to its silent path.
#'
#' Wrapped in `tryCatch` so a write error never propagates back into
#' the ingestion (we'd rather lose a progress tick than abort the job).
#'
#' @noRd
.build_progress_writer <- function(progress_path) {
  if (is.null(progress_path) || !nzchar(progress_path)) return(NULL)
  # v0.70.0 - Double transport (brief logs FAST propres) :
  # * `progress_path` (.json) : DERNIER event, atomic write +
  #   rename. Sert au toast Shiny coalesce (1 toast actif).
  # * `ndjson_path` (.ndjson) : journal APPEND-ONLY, une ligne
  #   par event. Draine par le mirror console cote lecteur,
  #   garantit l'ordre et l'absence de saut.
  # Le path NDJSON est derive du JSON (meme repertoire, suffixe
  # `.ndjson` au lieu de `.json`). Si l'extension n'est pas `.json`,
  # on append `.ndjson` au path complet.
  ndjson_path <- if (grepl("\\.json$", progress_path)) {
    sub("\\.json$", ".ndjson", progress_path)
  } else {
    paste0(progress_path, ".ndjson")
  }
  function(event) {
    # suppressWarnings as well as tryCatch: writing under a missing
    # directory emits a "cannot open file" *warning* before the error
    # - losing a progress tick must be fully silent (the reader always
    # wraps its read in tryCatch).
    tryCatch(
      suppressWarnings({
        line <- jsonlite::toJSON(event, auto_unbox = TRUE, null = "null",
                                 na = "null", POSIXt = "ISO8601")
        # --- 1. JSON dernier-event (toast Shiny) -------------------
        tmp <- paste0(progress_path, ".tmp")
        writeLines(line, con = tmp, useBytes = TRUE)
        # file.rename is atomic on POSIX, best-effort on Windows where
        # the destination must not exist - fall back to a write+unlink
        # cycle that is good enough for a polling reader.
        ok <- file.rename(tmp, progress_path)
        if (!isTRUE(ok)) {
          file.copy(tmp, progress_path, overwrite = TRUE)
          unlink(tmp)
        }
        # --- 2. NDJSON append-only (mirror console) ----------------
        # v0.70.0 - Une ligne par event, sequentielle, jamais ecrasee.
        # `cat(append = TRUE)` ouvre/ecrit/ferme atomiquement - pas
        # de leak de descripteur si le worker meurt.
        cat(line, "\n", sep = "", file = ndjson_path, append = TRUE)
      }),
      error = function(e) invisible(NULL)
    )
  }
}


#' Write the FAST ingestion run sentinel (worker-side, session-independent)
#'
#' v0.85.2.9000 - A small JSON marker the ingestion worker writes at
#' start (`status = "running"`) and at termination (`"done"` /
#' `"error"` / `"cancelled"`). Because it is written by the worker
#' process (not the Shiny session), it survives a browser disconnect /
#' session end. A freshly-launched Shiny instance reads it (via
#' [.detect_ingest_state()]) to surface an " ingestion en cours " /
#' " ingestion interrompue " banner.
#'
#' Atomic write (.tmp + rename), fully silent on error - a sentinel
#' write must never abort the ingestion.
#'
#' @param path Sentinel file path (`<project>/data/ingest_run.json`),
#'   or NULL (no-op, e.g. PG-only setup with no project on disk).
#' @param status One of `"running"`, `"done"`, `"error"`, `"cancelled"`.
#' @param extra Named list merged into the payload (zone, dates, ...).
#' @return Invisibly the path (or NULL).
#' @noRd
.write_ingest_sentinel <- function(path, status, extra = list()) {
  if (is.null(path) || !nzchar(path)) return(invisible(NULL))
  payload <- c(list(status = as.character(status),
                    updated_at = as.numeric(Sys.time())),
               extra)
  tryCatch(
    suppressWarnings({
      tmp <- paste0(path, ".tmp")
      jsonlite::write_json(payload, tmp, auto_unbox = TRUE, null = "null")
      ok <- file.rename(tmp, path)
      if (!isTRUE(ok)) {
        file.copy(tmp, path, overwrite = TRUE)
        unlink(tmp)
      }
    }),
    error = function(e) invisible(NULL)
  )
  invisible(path)
}


#' Read the FAST ingestion run sentinel
#'
#' v0.85.2.9000 - Companion reader for [.write_ingest_sentinel()].
#'
#' @param path Sentinel file path or NULL.
#' @return A named list (parsed JSON) or NULL when absent / unreadable.
#' @noRd
.read_ingest_sentinel <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(e) NULL
  )
}


#' Async FORDEAD dieback diagnosis (E6.c.5 - spec 008)
#'
#' Wraps `nemeton::run_fordead_dieback()` in a `shiny::ExtendedTask`.
#' Since nemeton@v0.24.0 the pipeline is **6 phases via reticulate**:
#' ingest -> vegetation index -> train -> forest mask -> dieback detection
#' -> export. The full run takes minutes to hours, so it runs in a
#' `future` worker. The worker re-loads `nemetonshiny` and opens a
#' fresh DB connection.
#'
#' Breaking signature change in v0.24.0 (cf. spec 008 sect.13 + ADR-013
#' amendement A2): `aoi` / `scenes_df` / `forest_mask` removed -
#' the core now derives everything from `(con, zone_id, cache_dir)`.
#' The new `ingest` phase 0 fetches the bands FAST didn't cache
#' (B02 / B05 / B8A / B11) and reuses what FAST already downloaded
#' (B04 / B12). It emits `s2:*` events through the same callback,
#' so the existing s2 dispatcher in mod_monitoring picks them up
#' transparently.
#'
#' Inputs are passed at `$invoke()` time:
#' * `dates_training`    - length-2 Date or character (start, end)
#' * `dates_monitoring`  - length-2 Date or character
#' * `threshold_anomaly` - numeric, default 0.16 (ONF/DSF 2024)
#' * `vegetation_index`  - "CRSWIR" (FORDEAD est mono-indice ; cf.
#'                         `nemeton::run_fordead_dieback()`)
#' * `zone_id`           - integer, required for AOI lookup + DB INSERT
#' * `cache_dir`         - path to the project's Sentinel-2 COG cache
#'                         (typically `<project>/cache/layers/sentinel2`)
#' * `db_url`            - pre-resolved DB URL (workers can't reach
#'                         app_state)
#' * `progress_path`     - JSON file the worker writes to for the
#'                         parent's reactivePoll to tail
#'
#' On success, `$result()` returns the list produced by
#' `nemeton::run_fordead_dieback()`.
#'
#' @return A `shiny::ExtendedTask` object.
#' @noRd
run_fordead_async <- function() {
  .dev_pkg_path <- tryCatch(
    if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
      find.package("nemetonshiny")
    else NULL,
    error = function(e) NULL
  )
  .worker_envvars <- .capture_worker_envvars()

  shiny::ExtendedTask$new(function(dates_training, dates_monitoring,
                                   threshold_anomaly = 0.16,
                                   vegetation_index = "CRSWIR",
                                   zone_id = NULL, cache_dir = NULL,
                                   db_url = "", progress_path = NULL,
                                   lang = "fr",
                                   # v0.71.1 - output_dir + keep_output
                                   # forwardes au coeur. Sans cela, le
                                   # coeur utilisait `tempfile("fordead_")`
                                   # (/tmp), supprime en fin de run.
                                   output_dir = NULL,
                                   keep_output = TRUE,
                                   cancel_path = NULL,
                                   # Nom d'affichage (projet) resolu cote parent
                                   # - cf. run_ingestion_async. NULL -> repli DB.
                                   project_name = NULL) {
    if (requireNamespace("future", quietly = TRUE)) {
      plan_classes <- class(future::plan())
      is_parallel <- any(c("multisession", "multicore", "cluster") %in% plan_classes)
      if (!is_parallel) .ensure_async_plan()
    }
    promises::future_promise({
      .apply_worker_envvars(.worker_envvars)
      # spec 008 sect.4 - rendre la memoire au systeme : le worker est PERSISTANT.
      on.exit(.release_worker_memory(), add = TRUE)

      if (!is.null(.dev_pkg_path) && requireNamespace("pkgload", quietly = TRUE)) {
        pkgload::load_all(.dev_pkg_path, quiet = TRUE)
      } else {
        loadNamespace("nemetonshiny")
      }

      con <- get_monitoring_db_connection(db_url = db_url)
      if (is.null(con)) {
        stop("Monitoring DB not configured (set NEMETON_DB_URL, NEMETON_DB_HOST/_PORT/_NAME/_USER/_PASSWORD, or open a project to use the local SQLite fallback).")
      }
      # `after = FALSE` : cette fermeture doit s'evaluer AVANT
      # `.release_worker_memory()`, enregistre en tete de corps. Les
      # handlers `on.exit` s'executent dans l'ORDRE D'ENREGISTREMENT, et
      # la liberation fait `rm(list = ls(envir = env), envir = env)` sur
      # la frame du worker - `con` compris. Enregistree apres elle, cette
      # ligne levait « objet 'con' introuvable » APRES que le corps ait
      # fait et persiste son travail (run Couchey du 2026-08-31 : les
      # trois moteurs Sante, `ingest_run.json` a `done`, 183 scenes).
      # Deplacer la liberation en fin de corps serait plus lisible mais
      # la retirerait des chemins d'echec precoce (le `stop()` juste
      # au-dessus), ou rendre la memoire compte le plus.
      on.exit(close_monitoring_db_connection(con), add = TRUE, after = FALSE)

      # URL passee a l'enfant plafonne, qui ouvre SA propre connexion
      # (une DBIConnection ne franchit pas une frontiere de process).
      # Le parent la resout normalement ; repli sur les variables
      # d'environnement si l'appelant ne l'a pas fournie.
      child_db_url <- if (nzchar(db_url %||% "")) {
        db_url
      } else {
        .resolve_monitoring_db_url(NULL)
      }

      # ntfy push channel - resolved worker-side (env vars replayed
      # above). NULL when NEMETON_NTFY_TOPIC is unset -> every
      # `.ntfy_send()` call below is a silent no-op.
      ntfy  <- .ntfy_config()
      i18n  <- get_i18n(lang %||% "fr")

      # v0.43.2 - resolve the display name once for the start push,
      # symetrique avec FAST : priorite au nom de projet (parent), repli DB.
      zone_name <- project_name %||% .resolve_zone_name(con, zone_id)

      # Composite progress callback: the file writer the parent's
      # reactivePoll tails, PLUS a worker-side ntfy push on each new
      # FORDEAD phase. Phase pushes are de-duplicated (one per phase
      # name, not per progress tick) so a 6-phase run yields 6
      # notifications, not hundreds.
      #
      # v0.106.5.9003 - le run lui-meme tourne dans un process ENFANT
      # plafonne (`nemeton::run_memory_capped()`), et c'est cet enfant
      # qui ecrit les fichiers de progression. Le composite n'est donc
      # plus passe au coeur : il ne sert plus qu'aux heartbeats emis par
      # le worker lui-meme (`.ws_emit()` ci-dessous), qui doivent bien
      # atterrir dans le fichier. Au coeur, on ne rejoue que `ntfy_cb`
      # (sinon chaque evenement serait ecrit deux fois).
      progress_cb <- .build_fordead_progress_callback(progress_path,
                                                      ntfy, i18n)
      ntfy_cb     <- .build_fordead_ntfy_callback(ntfy, i18n)

      .ntfy_send(
        ntfy,
        sprintf(i18n$t("monitoring_ntfy_fordead_start"),
                zone_name),
        tags  = "evergreen_tree",
        title = .ntfy_title("FORDEAD", zone_name)
      )

      # v0.71.1 - Si output_dir est NULL (cas legacy ou worker invoque
      # sans le param), garder le defaut coeur (tempfile). Sinon,
      # creer le dossier cote worker (le mkdir cote main session
      # peut avoir echoue).
      if (!is.null(output_dir) && nzchar(output_dir)) {
        if (!dir.exists(output_dir)) {
          tryCatch(
            dir.create(output_dir, recursive = TRUE, showWarnings = FALSE),
            error = function(e) NULL
          )
        }
      }
      # v0.106.5.9003 (spec 008) - FORDEAD tourne dans un process R
      # ENFANT plafonne en memoire (`nemeton::run_memory_capped()`,
      # coeur >= 0.157.0), pas dans le worker `future`. Raison : le
      # Python de FORDEAD vit dans l'interpreteur EMBARQUE de
      # reticulate, donc sa memoire est celle du worker, donc celle du
      # scope systemd de l'app - et `systemd-oomd` ne tue pas le
      # processus fautif mais le SCOPE entier (app + session R
      # emportees, 2026-07-13 / 2026-07-14). Le coeur pose un cgroup
      # `MemoryMax=` + `MemorySwapMax=0` sur l'enfant : un run qui
      # deborde meurt SEUL, avec une erreur attrapable (le tryCatch
      # ci-dessous, inchange).
      #
      # Frontiere de process -> deux arguments ne traversent pas et sont
      # reconstruits cote enfant :
      #   * `con`               -> `db_url` (l'enfant ouvre sa connexion) ;
      #   * `progress_callback` -> `progress_path` (l'enfant ecrit les
      #     fichiers .json/.ndjson, le parent les tail et rejoue chaque
      #     evenement dans `ntfy_cb` -> les push ntfy sont preserves).
      # `cancel_path` est inchange : l'enfant poll le meme fichier.
      # Log de l'enfant : le NDJSON dit jusqu'ou, celui-ci dit pourquoi.
      child_log <- .child_log_path(dirname(progress_path %||% "."), "fordead")
      result <- tryCatch(
        .run_capped(
          fun = "run_fordead_dieback",
          log_path = child_log,
          args = list(
            zone_id           = zone_id,
            cache_dir         = cache_dir,
            dates_training    = dates_training,
            dates_monitoring  = dates_monitoring,
            threshold_anomaly = threshold_anomaly,
            vegetation_index  = vegetation_index,
            # v0.71.1 - Forward output_dir + keep_output au coeur
            # (`nemeton::run_fordead_dieback`). Par defaut le coeur
            # utilisait `tempfile("fordead_")` -> /tmp. Desormais les
            # outputs intermediaires (training, masks bruts) vivent
            # sous `<projet>/cache/layers/fordead/output_zone_<id>` et
            # sont preserves (`keep_output = TRUE`) - inspectables.
            # NULL = retombe sur le defaut coeur (back-compat).
            output_dir        = output_dir,
            keep_output       = keep_output,
            # v0.52.0 - cancel cooperatif (nemeton@v0.53.0). L'enfant
            # polle `file.exists(cancel_path)` entre phases reticulate
            # (training -> monitoring -> ecriture alertes). Granularite
            # plus grossiere que FAST mais coherente - l'utilisateur
            # sait qu'il abandonne au prochain checkpoint.
            cancel_path       = cancel_path
          ),
          db_url            = child_db_url,
          progress_path     = progress_path,
          progress_callback = ntfy_cb
          # Pas de `memory_max =` : politique du coeur depuis nemeton 0.183.0.
        ),
        error = function(e) {
          .ntfy_send(
            ntfy,
            sprintf(i18n$t("monitoring_ntfy_fordead_error"),
                    conditionMessage(e)),
            priority = "high", tags = "rotating_light",
            title    = "Nemeton FORDEAD"
          )
          stop(e)
        }
      )

      # Heartbeat de fin : nemeton::run_fordead_dieback a rendu la main.
      # Symetrique au heartbeat `s2:ingest_done` cote FAST (cf. l.~240).
      # Si cet evenement apparait dans la console mais que le bouton ne
      # se reactive pas, le worker finalise sa sortie cote SQLite WAL
      # - l'utilisateur peut force-unlock l'UI via " Annuler ".
      .ws_emit(progress_cb, list(
        current           = "fordead:dieback_done",
        n_alerts_inserted = as.integer(result$n_alerts_inserted %||% 0L)
      ))

      .ntfy_send(
        ntfy,
        sprintf(i18n$t("monitoring_ntfy_fordead_complete"),
                as.integer(result$n_alerts_inserted %||% 0L),
                .format_duration_human(result$duration_sec %||% NA_real_)),
        tags  = "white_check_mark",
        title = .ntfy_title("FORDEAD", zone_name)
      )
      result
    }, seed = TRUE)
  })
}


#' Async RECONFORT dieback run (spec 021, L6)
#'
#' Wraps `nemeton::run_reconfort_dieback()` in a `shiny::ExtendedTask`,
#' mirroring [run_fordead_async()] with the RECONFORT parameter set.
#' The run is heavy / opt-in (conda IOTA2 + GEODES + OTB/Shark) and
#' takes minutes to hours, so it runs in a `future` worker that re-loads
#' `nemetonshiny` and opens a fresh DB connection.
#'
#' Inputs passed at `$invoke()` time:
#' * `zone_id`       - integer, required (AOI lookup + DB INSERT)
#' * `cache_dir`     - RECONFORT cache (`<project>/cache/layers/reconfort`);
#'                     the persist phase writes `zone_<id>/run_<run_id>/`
#' * `s2_year`       - integer Sentinel-2 year
#' * `db_url`        - pre-resolved DB URL (workers can't reach app_state)
#' * `progress_path` - JSON file the worker writes to for the parent's
#'                     reactivePoll to tail (events `reconfort:*`)
#' * `output_dir`    - working dir for intermediate outputs (NULL = core
#'                     default tempfile)
#'
#' Unlike FORDEAD, `nemeton::run_reconfort_dieback()` has no `cancel_path`
#' parameter - there is no cooperative cancellation; the UI relies on the
#' force-unlock escape hatch only.
#'
#' On success, `$result()` returns the list produced by the core (carrying
#' `n_alerts` and run metadata).
#'
#' @return A `shiny::ExtendedTask` object.
#' @noRd
run_reconfort_async <- function() {
  .dev_pkg_path <- tryCatch(
    if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
      find.package("nemetonshiny")
    else NULL,
    error = function(e) NULL
  )
  .worker_envvars <- .capture_worker_envvars()

  shiny::ExtendedTask$new(function(zone_id = NULL, cache_dir = NULL,
                                   s2_year = NULL, db_url = "",
                                   progress_path = NULL, lang = "fr",
                                   output_dir = NULL,
                                   # Nom d'affichage (projet) resolu cote parent
                                   # - cf. run_ingestion_async. NULL -> repli DB.
                                   project_name = NULL) {
    if (requireNamespace("future", quietly = TRUE)) {
      plan_classes <- class(future::plan())
      is_parallel <- any(c("multisession", "multicore", "cluster") %in% plan_classes)
      if (!is_parallel) .ensure_async_plan()
    }
    promises::future_promise({
      .apply_worker_envvars(.worker_envvars)
      # spec 008 sect.4 - rendre la memoire au systeme : le worker est PERSISTANT.
      on.exit(.release_worker_memory(), add = TRUE)

      if (!is.null(.dev_pkg_path) && requireNamespace("pkgload", quietly = TRUE)) {
        pkgload::load_all(.dev_pkg_path, quiet = TRUE)
      } else {
        loadNamespace("nemetonshiny")
      }

      con <- get_monitoring_db_connection(db_url = db_url)
      if (is.null(con)) {
        stop("Monitoring DB not configured (set NEMETON_DB_URL, NEMETON_DB_HOST/_PORT/_NAME/_USER/_PASSWORD, or open a project to use the local SQLite fallback).")
      }
      # `after = FALSE` : cette fermeture doit s'evaluer AVANT
      # `.release_worker_memory()`, enregistre en tete de corps. Les
      # handlers `on.exit` s'executent dans l'ORDRE D'ENREGISTREMENT, et
      # la liberation fait `rm(list = ls(envir = env), envir = env)` sur
      # la frame du worker - `con` compris. Enregistree apres elle, cette
      # ligne levait « objet 'con' introuvable » APRES que le corps ait
      # fait et persiste son travail (run Couchey du 2026-08-31 : les
      # trois moteurs Sante, `ingest_run.json` a `done`, 183 scenes).
      # Deplacer la liberation en fin de corps serait plus lisible mais
      # la retirerait des chemins d'echec precoce (le `stop()` juste
      # au-dessus), ou rendre la memoire compte le plus.
      on.exit(close_monitoring_db_connection(con), add = TRUE, after = FALSE)

      # ntfy push channel - resolved worker-side (env vars replayed
      # above). NULL when NEMETON_NTFY_TOPIC is unset -> every
      # `.ntfy_send()` below is a silent no-op. Symetrique avec FORDEAD.
      ntfy      <- .ntfy_config()
      i18n      <- get_i18n(lang %||% "fr")
      # Priorite au nom de projet (parent, a jour), repli DB - cf. FAST.
      zone_name <- project_name %||% .resolve_zone_name(con, zone_id)

      # Moitie « push » SEULE. Sous execution plafonnee, l'enfant ecrit
      # lui-meme les fichiers `.json` / `.ndjson` ; lui passer le
      # callback COMPOSITE dupliquerait chaque ligne NDJSON - le piege
      # deja rencontre sur FORDEAD. Contrairement a FORDEAD, ce worker
      # n'emet aucun heartbeat propre : il n'a donc pas d'usage pour le
      # composite, et ne le construit pas.
      ntfy_cb <- .build_reconfort_ntfy_callback(ntfy, i18n)

      # URL passee a l'enfant plafonne, qui ouvre SA propre connexion
      # (une DBIConnection ne franchit pas une frontiere de process).
      child_db_url <- if (nzchar(db_url %||% "")) {
        db_url
      } else {
        .resolve_monitoring_db_url(NULL)
      }

      .ntfy_send(
        ntfy,
        sprintf(i18n$t("monitoring_ntfy_reconfort_start"), zone_name),
        tags  = "deciduous_tree",
        title = .ntfy_title("RECONFORT", zone_name)
      )

      if (!is.null(output_dir) && nzchar(output_dir) && !dir.exists(output_dir)) {
        tryCatch(
          dir.create(output_dir, recursive = TRUE, showWarnings = FALSE),
          error = function(e) NULL
        )
      }

      # v0.143.10 - RECONFORT passe en ENFANT PLAFONNE, comme FORDEAD.
      #
      # Le 2026-09-01 sur Couchey, `systemd-oomd` a tue le SCOPE ENTIER
      # (9 processus : RStudio, la session R, les workers) pendant
      # l'item 82/203 de l'ingestion - « memory pressure for
      # user@1000.service being 56.87% > 50.00% », scope a 14,5 Go.
      # Aucun evenement d'erreur : le worker n'a pas leve, il a ete tue.
      #
      # Le cœur ne plafonnait QUE le sous-processus Python
      # (`.reconfort_run_py()` -> `.reconfort_cap_memory()`), au motif
      # que c'est lui le gourmand. Mais la boucle d'ingestion des 203
      # scenes est du R PUR (`nemeton:::reconfort_ingest.R`, evenements
      # `reconfort:ingest_item`) et n'etait plafonnee par rien : le run
      # est mort AVANT d'atteindre Python. Le raisonnement du cœur
      # laissait cette phase a decouvert.
      #
      # Sous cgroup, un depassement tue l'enfant SEUL, avec une erreur
      # attrapable - au lieu d'emporter la session de l'utilisateur.
      # `con` et `progress_callback` ne franchissent pas la frontiere de
      # process : l'enfant ouvre sa connexion depuis `db_url` et ecrit
      # lui-meme les fichiers de progression depuis `progress_path`.
      # Log de l'enfant : c'est le chemin qui a manque le 2026-09-03 - IOTA2
      # meurt, son traceback part dans le `/dev/null` du worker, et l'erreur
      # remontee ne dit que « exit 1 ».
      child_log <- .child_log_path(dirname(progress_path %||% "."), "reconfort")
      result <- tryCatch(
        .run_capped(
          fun = "run_reconfort_dieback",
          log_path = child_log,
          args = list(
            zone_id    = zone_id,
            cache_dir  = cache_dir,
            s2_year    = s2_year,
            output_dir = output_dir
          ),
          db_url            = child_db_url,
          progress_path     = progress_path,
          progress_callback = ntfy_cb
        ),
        error = function(e) {
          .ntfy_send(
            ntfy,
            sprintf(i18n$t("monitoring_ntfy_reconfort_error"),
                    conditionMessage(e)),
            priority = "high", tags = "rotating_light",
            title    = "Nemeton RECONFORT"
          )
          stop(e)
        }
      )

      .ntfy_send(
        ntfy,
        sprintf(i18n$t("monitoring_ntfy_reconfort_complete"),
                as.integer(result$n_alerts %||% 0L),
                .format_duration_human(result$elapsed_sec %||% NA_real_)),
        tags  = "white_check_mark",
        title = .ntfy_title("RECONFORT", zone_name)
      )
      result
    }, seed = TRUE)
  })
}


#' Resolve the ntfy push configuration from the environment
#'
#' ntfy (<https://ntfy.sh>) is the out-of-band notification channel for
#' long FORDEAD runs: the `future` worker outlives the Shiny session,
#' so a browser disconnect would otherwise leave the user blind until
#' they reopen the app. ntfy lets the worker push start / per-phase /
#' completion / error messages to a topic the user subscribes to from
#' a phone or browser.
#'
#' Returns `NULL` when `NEMETON_NTFY_TOPIC` is unset - every
#' `.ntfy_send()` then becomes a silent no-op, so ntfy is strictly
#' opt-in and the feature degrades cleanly when not configured.
#'
#' Env vars (cf. CLAUDE.md - no secret in code):
#' * `NEMETON_NTFY_TOPIC` - topic name (required to enable).
#' * `NEMETON_NTFY_URL`   - server, default `https://ntfy.sh`.
#' * `NEMETON_NTFY_TOKEN` - bearer token for a protected topic
#'   (optional, self-hosted ntfy).
#'
#' @return A named list (`url`, `topic`, `token`) or `NULL`.
#' @noRd
.ntfy_config <- function() {
  topic <- Sys.getenv("NEMETON_NTFY_TOPIC", unset = "")
  if (!nzchar(topic)) return(NULL)
  url <- Sys.getenv("NEMETON_NTFY_URL", unset = "https://ntfy.sh")
  list(
    url   = sub("/+$", "", url),
    topic = topic,
    token = Sys.getenv("NEMETON_NTFY_TOKEN", unset = "")
  )
}


#' Send a single ntfy notification (best-effort)
#'
#' POSTs `message` to `<url>/<topic>`. The HTTP body carries the
#' (UTF-8, possibly accented) message; the `Title` header stays a
#' fixed ASCII string because ntfy headers are not UTF-8 safe.
#'
#' Wrapped in `tryCatch` and given a short timeout: a notification is
#' never worth aborting - or even slowing down - a FORDEAD run for.
#' No-op when `cfg` is `NULL`.
#'
#' @param cfg Output of `.ntfy_config()` (or `NULL`).
#' @param message Body text (UTF-8).
#' @param priority ntfy priority (`"min"`/`"low"`/`"default"`/`"high"`/`"max"`).
#' @param tags Character vector of ntfy tags / emoji short-codes.
#' @param title Title HTTP header - ASCII only (ntfy headers are not
#'   UTF-8 safe). Defaults to a neutral `"Nemeton"`; FAST callers pass
#'   `"Nemeton FAST"`, FORDEAD callers `"Nemeton FORDEAD"` so the
#'   device groups notifications by stream (v0.43.1 fix : FAST runs
#'   were mislabelled as `"Nemeton FORDEAD"` because the title was
#'   hard-coded here).
#' @return `TRUE` on a sent request, `FALSE` otherwise (invisibly).
#' @noRd
# Titre ntfy " Nemeton <moteur> - <projet> ", ASCII-safe (les en-tetes HTTP ntfy
# ne sont pas UTF-8 safe -> translitteration + strip des caracteres non-ASCII).
# `project` peut etre NULL/"" (retombe sur le titre moteur seul). Partage par
# FAST/FORDEAD/RECONFORT (service_monitoring) et reGeneration (service_regeneration).
.ntfy_title <- function(engine, project = NULL) {
  base <- paste0("Nemeton ", engine)
  if (is.null(project) || !nzchar(as.character(project))) return(base)
  ascii <- tryCatch(iconv(as.character(project), to = "ASCII//TRANSLIT"),
                    error = function(e) NA_character_)
  if (is.na(ascii)) ascii <- gsub("[^A-Za-z0-9 ._-]", "", as.character(project))
  ascii <- trimws(gsub("[^A-Za-z0-9 ._-]", "", ascii))
  if (nzchar(ascii)) paste0(base, " - ", ascii) else base
}

.ntfy_send <- function(cfg, message, priority = "default",
                       tags = NULL, title = "Nemeton") {
  if (is.null(cfg)) return(invisible(FALSE))
  tryCatch({
    req <- httr2::request(paste0(cfg$url, "/", cfg$topic))
    req <- httr2::req_body_raw(req, enc2utf8(as.character(message)),
                               type = "text/plain; charset=utf-8")
    req <- httr2::req_headers(req,
                              Title    = as.character(title),
                              Priority = priority)
    if (!is.null(tags) && length(tags)) {
      req <- httr2::req_headers(req, Tags = paste(tags, collapse = ","))
    }
    if (nzchar(cfg$token)) {
      req <- httr2::req_auth_bearer_token(req, cfg$token)
    }
    req <- httr2::req_timeout(req, 10)
    httr2::req_perform(req)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}


#' Build the FORDEAD worker progress callback
#'
#' Composes the JSON-file writer (`.build_progress_writer()`, tailed by
#' the parent's `reactivePoll`) with a worker-side ntfy push fired once
#' per new FORDEAD phase. The phase name is tracked in a closure
#' environment so repeated `fordead:phase` ticks within the same phase
#' do not spam the topic.
#'
#' @param progress_path JSON file path (or `NULL`).
#' @param ntfy `.ntfy_config()` output (or `NULL`).
#' @param i18n A `get_i18n()` translator.
#' @return A callback `function(event)`.
#' @noRd
#' Composite progress callback for the FAST ingestion worker
#'
#' Wraps `.build_progress_writer()` (for the parent's reactivePoll
#' tail) with a one-shot ntfy push when the first per-scene event
#' arrives - that's when the worker confirms it knows the total scene
#' count and is starting the actual download. Deduplique via state env
#' (un push max par run, independant du nombre de scenes 30-100).
#'
#' Start / complete / error ntfy pushes are NOT here - they live in the
#' worker body around the nemeton call (cf. `run_ingestion_async`).
#' Putting them in the callback would mean intercepting an arbitrary
#' event boundary; keeping them in the worker keeps the lifecycle
#' explicit. v0.42.1.
#'
#' @param progress_path Path to the JSON progress file.
#' @param ntfy `.ntfy_config()` output (or NULL).
#' @param i18n `get_i18n()` output.
#' @return A function suitable as `progress_callback` for
#'   `nemeton::ingest_sentinel2_timeseries()`.
#' @noRd
.build_ingest_progress_callback <- function(progress_path, ntfy, i18n) {
  file_cb <- .build_progress_writer(progress_path)
  state   <- new.env(parent = emptyenv())
  state$scenes_started <- FALSE
  function(event) {
    if (!is.null(file_cb)) {
      tryCatch(file_cb(event), error = function(e) invisible(NULL))
    }
    current <- as.character(event$current %||% "")
    if (identical(current, "s2:scene") && !isTRUE(state$scenes_started)) {
      total <- as.integer(event$total %||% 0L)
      if (total > 0L) {
        state$scenes_started <- TRUE
        .ntfy_send(
          ntfy,
          sprintf(i18n$t("monitoring_ntfy_ingest_scenes"), total),
          priority = "low", tags = "satellite_orbital",
          title    = "Nemeton FAST"
        )
      }
    }
    invisible(NULL)
  }
}


.build_fordead_progress_callback <- function(progress_path, ntfy, i18n) {
  file_cb <- .build_progress_writer(progress_path)
  ntfy_cb <- .build_fordead_ntfy_callback(ntfy, i18n)
  function(event) {
    if (!is.null(file_cb)) {
      tryCatch(file_cb(event), error = function(e) invisible(NULL))
    }
    ntfy_cb(event)
    invisible(NULL)
  }
}


#' ntfy-only FORDEAD phase callback (no file write)
#'
#' v0.106.5.9003 (spec 008, brief `brief-nemetonshiny-fordead-capped`)
#' - La moitie " push " de [.build_fordead_progress_callback()], isolee
#' pour l'execution plafonnee en process enfant
#' (`nemeton::run_memory_capped()`). Sous isolation, **l'enfant ecrit
#' deja** les fichiers `.json` / `.ndjson` de progression au format de
#' [.build_progress_writer()] ; rejouer le callback composite dans le
#' parent dupliquerait chaque evenement (lignes NDJSON en double,
#' console en double). Le parent ne rejoue donc que le push ntfy.
#'
#' La dedup par phase (une notification par phase, pas par tick) vit
#' dans l'etat `last_phase` de la closure - etat cote parent, donc
#' preserve meme si l'enfant meurt et redemarre.
#'
#' @param ntfy `.ntfy_config()` output (or `NULL` -> no-op).
#' @param i18n A `get_i18n()` translator.
#' @return A callback `function(event)`.
#' @noRd
.build_fordead_ntfy_callback <- function(ntfy, i18n) {
  state <- new.env(parent = emptyenv())
  state$last_phase <- ""
  function(event) {
    current <- as.character(event$current %||% "")
    if (identical(current, "fordead:phase")) {
      phase_name <- as.character(event$phase_name %||% "")
      if (nzchar(phase_name) && !identical(phase_name, state$last_phase)) {
        state$last_phase <- phase_name
        .ntfy_send(
          ntfy,
          sprintf(i18n$t("monitoring_ntfy_fordead_phase"),
                  .fordead_phase_label(phase_name, i18n)),
          priority = "low", tags = "hourglass_flowing_sand",
          title    = "Nemeton FORDEAD"
        )
      }
    }
    invisible(NULL)
  }
}


#' Composite progress callback for RECONFORT (file writer + ntfy phase push)
#'
#' Mirror of [.build_fordead_progress_callback()] for the RECONFORT event
#' stream (`current = "reconfort:phase"` with `phase_name`). Writes the
#' progress file the parent's reactivePoll tails AND pushes one ntfy
#' message per distinct phase (de-duplicated via `last_phase`, so a
#' 10-phase run yields 10 notifications, not hundreds). `ntfy` may be
#' `NULL` (then every `.ntfy_send()` is a silent no-op).
#'
#' @param progress_path JSON file the worker writes to.
#' @param ntfy ntfy config from [.ntfy_config()], or `NULL`.
#' @param i18n An i18n object from [get_i18n()].
#' @return A function of one `event` list.
#' @noRd
.build_reconfort_progress_callback <- function(progress_path, ntfy, i18n) {
  file_cb <- .build_progress_writer(progress_path)
  ntfy_cb <- .build_reconfort_ntfy_callback(ntfy, i18n)
  function(event) {
    if (!is.null(file_cb)) {
      tryCatch(file_cb(event), error = function(e) invisible(NULL))
    }
    ntfy_cb(event)
    invisible(NULL)
  }
}


#' ntfy-only RECONFORT phase callback (no file write)
#'
#' Strict mirror of [.build_fordead_ntfy_callback()], et pour la meme
#' raison : sous execution plafonnee (`nemeton::run_memory_capped()`),
#' **l'enfant ecrit deja** les fichiers `.json` / `.ndjson` de
#' progression. Rejouer le callback COMPOSITE dans le parent
#' dupliquerait chaque evenement - lignes NDJSON en double, console en
#' double. Le parent ne rejoue donc que le push ntfy.
#'
#' La dedup par phase vit dans l'etat `last_phase` de la closure, cote
#' PARENT : elle survit donc a la mort et au redemarrage de l'enfant.
#'
#' @param ntfy `.ntfy_config()` output (or `NULL` -> no-op).
#' @param i18n A `get_i18n()` translator.
#' @return A callback `function(event)`.
#' @noRd
.build_reconfort_ntfy_callback <- function(ntfy, i18n) {
  state <- new.env(parent = emptyenv())
  state$last_phase <- ""
  function(event) {
    current <- as.character(event$current %||% "")
    if (identical(current, "reconfort:phase")) {
      phase_name <- as.character(event$phase_name %||% "")
      if (nzchar(phase_name) && !identical(phase_name, state$last_phase)) {
        state$last_phase <- phase_name
        .ntfy_send(
          ntfy,
          sprintf(i18n$t("monitoring_ntfy_reconfort_phase"),
                  .reconfort_phase_label(phase_name, i18n)),
          priority = "low", tags = "hourglass_flowing_sand",
          title    = "Nemeton RECONFORT"
        )
      }
    }
    invisible(NULL)
  }
}


#' Human-readable duration string (granularite minute)
#'
#' Formats a number of seconds as `"45 s"` / `"12 min"` / `"13 h 47 min"`.
#' Returns `"?"` for `NULL` / non-finite input.
#'
#' v0.106.5 (spec 008 sect.5, consolidation) - mince adaptateur sur
#' `nemeton::format_duration(with_seconds = FALSE)` (coeur >= 0.155.0).
#' L'implementation locale est retiree : une seule source de verite pour le
#' format des durees (regle #2). Cf. `format_elapsed()` pour la granularite
#' seconde.
#'
#' @param sec Number of seconds.
#' @return A length-1 character.
#' @noRd
.format_duration_human <- function(sec) {
  nemeton::format_duration(sec, with_seconds = FALSE)
}


#' Reconcile the FORDEAD UI state from disk after a session reload
#'
#' When a FORDEAD run outlives its Shiny session (long run + browser
#' disconnect), the in-session `fordead_last_result()` reactiveVal is
#' lost on reload - even though the worker completed and persisted its
#' dieback mask. This helper rebuilds a synthetic "success" result by
#' inspecting the on-disk mask cache, so the "Carte FORDEAD" /
#' "Alertes FORDEAD" sub-tabs can show the completed run instead of a
#' stale "not run yet" placeholder.
#'
#' Mask layout (written by the nemeton@>=0.41.0 persist hook):
#' `<project>/cache/layers/fordead/zone_<id>/dieback_mask_<ts>.tif`.
#'
#' @param project The active project (`reactiveValues`-like list with
#'   a `path`).
#' @param zone_id Integer monitoring zone id.
#' @return A named list (`status = "success"`, `reconciled = TRUE`,
#'   `zone_id`, `mask_path`, `mask_timestamp`, `n_alerts_inserted`,
#'   `duration_sec`) when a persisted mask exists, otherwise `NULL`.
#' @noRd
.reconcile_fordead_state <- function(project, zone_id) {
  if (is.null(project) || is.null(project$path)) return(NULL)
  zone_id <- suppressWarnings(as.integer(zone_id)[1])
  if (length(zone_id) != 1L || is.na(zone_id)) return(NULL)
  zdir <- file.path(project$path, "cache", "layers", "fordead",
                    paste0("zone_", zone_id))
  if (!dir.exists(zdir)) return(NULL)
  masks <- list.files(zdir, pattern = "^dieback_mask_.*\\.tif$",
                      full.names = TRUE)
  if (!length(masks)) return(NULL)
  mtimes <- file.info(masks)$mtime
  latest <- masks[which.max(mtimes)]
  list(
    status            = "success",
    reconciled        = TRUE,
    zone_id           = zone_id,
    mask_path         = latest,
    mask_timestamp    = .parse_fordead_mask_timestamp(basename(latest)),
    n_alerts_inserted = NA_integer_,
    duration_sec      = NA_real_
  )
}


#' Parse the timestamp embedded in a FORDEAD mask filename
#'
#' Mask files are named `dieback_mask_<YYYYMMDD>T<HHMMSS>.tif`. Returns
#' a display-formatted `"YYYY-MM-DD HH:MM"` string, or `NA_character_`
#' when no timestamp can be extracted.
#'
#' @param filename A mask file basename.
#' @return A length-1 character.
#' @noRd
.parse_fordead_mask_timestamp <- function(filename) {
  m <- regmatches(filename,
                   regexpr("[0-9]{8}T[0-9]{6}", filename))
  if (!length(m)) return(NA_character_)
  dt <- tryCatch(
    as.POSIXct(m, format = "%Y%m%dT%H%M%S", tz = ""),
    error = function(e) NA
  )
  if (length(dt) != 1L || is.na(dt)) return(NA_character_)
  format(dt, "%Y-%m-%d %H:%M")
}


#' Capture env vars to replay in the worker
#'
#' `future::multisession` workers on Windows are separate Rscript.exe
#' processes spawned (potentially) before the user `Sys.setenv()`d
#' diagnostic flags. The worker then runs blind. We snapshot the
#' relevant `NEMETON_*` env vars at invoke time (parent side) and
#' replay them on the worker side via `.apply_worker_envvars()`.
#'
#' Returns a named character vector - `future` auto-pickles it as a
#' captured global when it appears inside the `future_promise()`
#' expression body.
#'
#' Only forwards env vars that are actually set in the parent
#' (skipping empty/unset values) to avoid clobbering any worker
#' defaults.
#'
#' @noRd
.capture_worker_envvars <- function() {
  keys <- c(
    "NEMETON_S2_CACHE_DEBUG",
    "NEMETON_DB_URL",
    "NEMETON_DB_LOCAL",
    "NEMETON_DB_HOST",
    "NEMETON_DB_PORT",
    "NEMETON_DB_NAME",
    "NEMETON_DB_USER",
    "NEMETON_DB_PASSWORD",
    # Clever Cloud addon vars - consumed by the app-DB resolver
    # (`.resolve_db_config`) for the background project sync worker
    # (`db_sync_project_async`). Harmless for the monitoring workers.
    "POSTGRESQL_ADDON_HOST",
    "POSTGRESQL_ADDON_PORT",
    "POSTGRESQL_ADDON_DB",
    "POSTGRESQL_ADDON_USER",
    "POSTGRESQL_ADDON_PASSWORD",
    # ntfy push channel (E6 - out-of-band FORDEAD progress). The worker
    # outlives the Shiny session on long runs; ntfy is how the user
    # still gets notified. Forwarded so the worker can resolve the
    # topic / server / token without reaching app_state.
    "NEMETON_NTFY_URL",
    "NEMETON_NTFY_TOPIC",
    "NEMETON_NTFY_TOKEN",
    # scratch dir des intermediaires volumineux (nemeton >= 0.156.0). Les
    # pipelines longs y streament leurs stacks au lieu de les tenir en RAM :
    # ~800 Mo pour une petite AOI, de l'ordre de la dizaine de Go a l'echelle
    # d'un departement. Le run tourne DANS le worker, donc c'est le worker qui
    # doit voir la variable - et les workers sont PRE-CHAUFFES au demarrage de
    # la session (warmup_async_workers) : ils figent leur environnement a ce
    # moment-la. Sans ce transfert, un NEMETON_SCRATCH_DIR pose ensuite serait
    # ignore en silence et le coeur retomberait sur tempdir() - qui est parfois
    # un tmpfs, c'est-a-dire de la RAM, ce qui annulerait tout le benefice.
    "NEMETON_SCRATCH_DIR",
    # plafond memoire du process enfant. Resolu par le COEUR (nemeton
    # >= 0.183.0) mais lu DANS le worker, puisque c'est lui qui lance
    # l'enfant : a transferer comme NEMETON_SCRATCH_DIR, les workers etant
    # pre-chauffes, ils figent leur environnement.
    "NEMETON_MEMORY_MAX"
  )
  vals <- vapply(keys, function(k) Sys.getenv(k, unset = ""), character(1))
  vals[nzchar(vals)]
}

#' Replay captured env vars on the worker side
#'
#' Called as the first line of the `future_promise()` body so all
#' downstream `nemeton::*` calls - including the verbose tracing
#' driven by `NEMETON_S2_CACHE_DEBUG` - see the same env as the
#' parent.
#'
#' Robust to NULL / empty inputs (no-op).
#'
#' @noRd
.apply_worker_envvars <- function(envvars) {
  if (is.null(envvars) || length(envvars) == 0L) return(invisible(NULL))
  do.call(Sys.setenv, as.list(envvars))
  invisible(NULL)
}


#' Rendre au systeme la memoire d'une tache worker terminee (spec 008 sect.4)
#'
#' Les workers `future::multisession` sont des processus R **persistants** : ils
#' survivent a la tache et gardent tout ce qu'ils ont alloue. Un run lourd fait
#' gonfler un worker a plusieurs Go ; sans nettoyage il **reste** a ce niveau
#' jusqu'a la fin de la session, et 8 workers dans cet etat suffisent a mettre
#' la session sous le seuil de pression de `systemd-oomd` (incident 2026-07-13).
#'
#' Appele en `on.exit()` a la fin du corps du worker. Les DEUX etapes comptent,
#' et dans cet ordre :
#'   1. `rm(list = ls(envir = env), envir = env)` - `on.exit` s'execute pendant
#'      que la frame du worker est ENCORE VIVANTE : les gros objets y sont
#'      toujours lies, un `gc()` seul ne peut donc rien liberer. Mesure : `gc()`
#'      seul ne fait tomber le worker que de 6,4 Go a 1,6 Go.
#'   2. `gc(full = TRUE)` - une fois les liens coupes, R rend les pages a l'OS.
#'      Mesure : le worker retombe a ~210 Mo (son niveau a vide).
#'
#' La valeur de retour de la tache est deja calculee quand `on.exit` tourne :
#' la vider n'y touche pas (couvert par test).
#'
#' @param env Frame a vider. Par defaut celle de l'appelant (le corps du worker).
#' @return `invisible(NULL)`.
#' @noRd
.release_worker_memory <- function(env = parent.frame()) {
  tryCatch({
    rm(list = ls(envir = env, all.names = TRUE), envir = env)
    gc(full = TRUE)
  }, error = function(e) invisible(NULL))
  invisible(NULL)
}


#' Worker-side progress emit (best-effort, swallows errors)
#'
#' Wrapper around `progress_cb(event)` that is safe to call when
#' `progress_cb` is NULL (no path provided). Used for heartbeats and
#' fatal-error surfacing inside the future worker.
#'
#' @noRd
.ws_emit <- function(progress_cb, event) {
  if (is.null(progress_cb)) return(invisible(NULL))
  tryCatch(progress_cb(event), error = function(e) invisible(NULL))
  invisible(NULL)
}

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
#' ingest_task <- run_ingestion_async()
#' ingest_task$invoke(zone_id, start, end, bands, max_cloud)
#' shiny::observe({
#'   result <- tryCatch(ingest_task$result(), error = function(e) {
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
run_ingestion_async <- function() {
  # Capture the package source path (when running via devtools::load_all)
  # so the future worker can re-load nemetonshiny — workers don't
  # inherit the parent's loaded namespaces.
  .pkg_path <- tryCatch(pkgload::pkg_path(), error = function(e) NULL)

  # Capture diagnostic env vars in the parent so we can replay them
  # inside the worker. `future::multisession` workers are separate
  # Rscript.exe processes on Windows that may have been spawned BEFORE
  # the user `Sys.setenv()`d these — they then run blind. We pickle
  # the values via the auto-captured globals mechanism (future scans
  # the expression for symbols defined in the parent) and re-set them
  # at the top of the promise body.
  .worker_envvars <- .capture_worker_envvars()

  shiny::ExtendedTask$new(function(zone_id, start, end, bands,
                                   max_cloud = 20, db_url = "",
                                   progress_path = NULL,
                                   cache_dir = NULL,
                                   skip_cached = TRUE,
                                   log_path = NULL) {
    if (requireNamespace("future", quietly = TRUE)) {
      plan_classes <- class(future::plan())
      is_parallel <- any(c("multisession", "multicore", "cluster") %in% plan_classes)
      if (!is_parallel) future::plan("multisession")
    }
    promises::future_promise({
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
          on.exit(
            tryCatch(close(.ws_log_conn), error = function(e) NULL),
            add = TRUE
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
      # URL-resolution helper. In dev (load_all), .pkg_path points to
      # the source tree; in prod the installed namespace is used.
      if (!is.null(.pkg_path) && requireNamespace("pkgload", quietly = TRUE)) {
        pkgload::load_all(.pkg_path, quiet = TRUE)
      } else if (requireNamespace("nemetonshiny", quietly = TRUE)) {
        loadNamespace("nemetonshiny")
      }

      # The observer that calls $invoke() pre-resolves the URL from
      # `app_state$current_project` (which workers can't reach) and
      # passes it explicitly. An empty string means no PG env vars
      # AND no project — i.e. no DB at all.
      con <- get_monitoring_db_connection(db_url = db_url)
      if (is.null(con)) {
        stop("Monitoring DB not configured (set NEMETON_DB_URL, NEMETON_DB_HOST/_PORT/_NAME/_USER/_PASSWORD, or open a project to use the local DuckDB fallback).")
      }
      on.exit(close_monitoring_db_connection(con), add = TRUE)

      progress_cb <- .build_progress_writer(progress_path)

      # Heartbeat 1/3: worker reached the start of its body. Useful
      # when the worker is silent — if we don't see this in the
      # console after invoke, the worker never spawned (future plan
      # broken).
      .ws_emit(progress_cb, list(current = "s2:worker_started",
                                 cache_dir = cache_dir,
                                 skip_cached = skip_cached))

      # If a cache_dir is provided, ensure the directory exists before
      # the worker hands it to nemeton. The worker can't reach `dir.create`
      # if the path's parent doesn't exist yet — happens on a brand-new
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
      # instead of a misleading "Téléchargement terminé : 0 scènes".
      #
      # Also wrap in tryCatch so any R error inside the worker
      # surfaces explicitly via the progress channel BEFORE the
      # future rejects — the standard future rejection message
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
            progress_callback = progress_cb
          ),
          # Catch every `message()` signaled by nemeton — including
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
          stop(e)
        }
      )

      list(
        status    = "success",
        summary   = summary,
        warnings  = warns,
        timestamp = Sys.time()
      )
    }, seed = TRUE)
  })
}


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
  function(event) {
    # suppressWarnings as well as tryCatch: writing under a missing
    # directory emits a "cannot open file" *warning* before the error
    # — losing a progress tick must be fully silent (the reader always
    # wraps its read in tryCatch).
    tryCatch(
      suppressWarnings({
        tmp <- paste0(progress_path, ".tmp")
        writeLines(
          jsonlite::toJSON(event, auto_unbox = TRUE, null = "null",
                           na = "null", POSIXt = "ISO8601"),
          con = tmp,
          useBytes = TRUE
        )
        # file.rename is atomic on POSIX, best-effort on Windows where
        # the destination must not exist — fall back to a write+unlink
        # cycle that is good enough for a polling reader.
        ok <- file.rename(tmp, progress_path)
        if (!isTRUE(ok)) {
          file.copy(tmp, progress_path, overwrite = TRUE)
          unlink(tmp)
        }
      }),
      error = function(e) invisible(NULL)
    )
  }
}


#' Async FORDEAD dieback diagnosis (E6.c.5 — spec 008)
#'
#' Wraps `nemeton::run_fordead_dieback()` in a `shiny::ExtendedTask`.
#' Since nemeton@v0.24.0 the pipeline is **6 phases via reticulate**:
#' ingest → vegetation index → train → forest mask → dieback detection
#' → export. The full run takes minutes to hours, so it runs in a
#' `future` worker. The worker re-loads `nemetonshiny` and opens a
#' fresh DB connection.
#'
#' Breaking signature change in v0.24.0 (cf. spec 008 §13 + ADR-013
#' amendement A2): `aoi` / `scenes_df` / `forest_mask` removed —
#' the core now derives everything from `(con, zone_id, cache_dir)`.
#' The new `ingest` phase 0 fetches the bands FAST didn't cache
#' (B02 / B05 / B8A / B11) and reuses what FAST already downloaded
#' (B04 / B12). It emits `s2:*` events through the same callback,
#' so the existing s2 dispatcher in mod_monitoring picks them up
#' transparently.
#'
#' Inputs are passed at `$invoke()` time:
#' * `dates_training`    — length-2 Date or character (start, end)
#' * `dates_monitoring`  — length-2 Date or character
#' * `threshold_anomaly` — numeric, default 0.16 (ONF/DSF 2024)
#' * `vegetation_index`  — "CRSWIR" / "NDVI" / "NDWI"
#' * `zone_id`           — integer, required for AOI lookup + DB INSERT
#' * `cache_dir`         — path to the project's Sentinel-2 COG cache
#'                         (typically `<project>/cache/layers/sentinel2`)
#' * `db_url`            — pre-resolved DB URL (workers can't reach
#'                         app_state)
#' * `progress_path`     — JSON file the worker writes to for the
#'                         parent's reactivePoll to tail
#'
#' On success, `$result()` returns the list produced by
#' `nemeton::run_fordead_dieback()`.
#'
#' @return A `shiny::ExtendedTask` object.
#' @noRd
run_fordead_async <- function() {
  .pkg_path <- tryCatch(pkgload::pkg_path(), error = function(e) NULL)
  .worker_envvars <- .capture_worker_envvars()

  shiny::ExtendedTask$new(function(dates_training, dates_monitoring,
                                   threshold_anomaly = 0.16,
                                   vegetation_index = "CRSWIR",
                                   zone_id = NULL, cache_dir = NULL,
                                   db_url = "", progress_path = NULL,
                                   lang = "fr") {
    if (requireNamespace("future", quietly = TRUE)) {
      plan_classes <- class(future::plan())
      is_parallel <- any(c("multisession", "multicore", "cluster") %in% plan_classes)
      if (!is_parallel) future::plan("multisession")
    }
    promises::future_promise({
      .apply_worker_envvars(.worker_envvars)

      if (!is.null(.pkg_path) && requireNamespace("pkgload", quietly = TRUE)) {
        pkgload::load_all(.pkg_path, quiet = TRUE)
      } else if (requireNamespace("nemetonshiny", quietly = TRUE)) {
        loadNamespace("nemetonshiny")
      }

      con <- get_monitoring_db_connection(db_url = db_url)
      if (is.null(con)) {
        stop("Monitoring DB not configured (set NEMETON_DB_URL, NEMETON_DB_HOST/_PORT/_NAME/_USER/_PASSWORD, or open a project to use the local DuckDB fallback).")
      }
      on.exit(close_monitoring_db_connection(con), add = TRUE)

      # ntfy push channel — resolved worker-side (env vars replayed
      # above). NULL when NEMETON_NTFY_TOPIC is unset → every
      # `.ntfy_send()` call below is a silent no-op.
      ntfy  <- .ntfy_config()
      i18n  <- get_i18n(lang %||% "fr")

      # Composite progress callback: the file writer the parent's
      # reactivePoll tails, PLUS a worker-side ntfy push on each new
      # FORDEAD phase. Phase pushes are de-duplicated (one per phase
      # name, not per progress tick) so a 6-phase run yields 6
      # notifications, not hundreds.
      progress_cb <- .build_fordead_progress_callback(progress_path,
                                                      ntfy, i18n)

      .ntfy_send(
        ntfy,
        sprintf(i18n$t("monitoring_ntfy_fordead_start"),
                as.character(zone_id %||% "?")),
        tags = "evergreen_tree"
      )

      result <- tryCatch(
        nemeton::run_fordead_dieback(
          con               = con,
          zone_id           = zone_id,
          cache_dir         = cache_dir,
          dates_training    = dates_training,
          dates_monitoring  = dates_monitoring,
          threshold_anomaly = threshold_anomaly,
          vegetation_index  = vegetation_index,
          progress_callback = progress_cb
        ),
        error = function(e) {
          .ntfy_send(
            ntfy,
            sprintf(i18n$t("monitoring_ntfy_fordead_error"),
                    conditionMessage(e)),
            priority = "high", tags = "rotating_light"
          )
          stop(e)
        }
      )

      .ntfy_send(
        ntfy,
        sprintf(i18n$t("monitoring_ntfy_fordead_complete"),
                as.integer(result$n_alerts_inserted %||% 0L),
                .format_duration_human(result$duration_sec %||% NA_real_)),
        tags = "white_check_mark"
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
#' Returns `NULL` when `NEMETON_NTFY_TOPIC` is unset — every
#' `.ntfy_send()` then becomes a silent no-op, so ntfy is strictly
#' opt-in and the feature degrades cleanly when not configured.
#'
#' Env vars (cf. CLAUDE.md — no secret in code):
#' * `NEMETON_NTFY_TOPIC` — topic name (required to enable).
#' * `NEMETON_NTFY_URL`   — server, default `https://ntfy.sh`.
#' * `NEMETON_NTFY_TOKEN` — bearer token for a protected topic
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
#' never worth aborting — or even slowing down — a FORDEAD run for.
#' No-op when `cfg` is `NULL`.
#'
#' @param cfg Output of `.ntfy_config()` (or `NULL`).
#' @param message Body text (UTF-8).
#' @param priority ntfy priority (`"min"`/`"low"`/`"default"`/`"high"`/`"max"`).
#' @param tags Character vector of ntfy tags / emoji short-codes.
#' @return `TRUE` on a sent request, `FALSE` otherwise (invisibly).
#' @noRd
.ntfy_send <- function(cfg, message, priority = "default", tags = NULL) {
  if (is.null(cfg)) return(invisible(FALSE))
  tryCatch({
    req <- httr2::request(paste0(cfg$url, "/", cfg$topic))
    req <- httr2::req_body_raw(req, enc2utf8(as.character(message)),
                               type = "text/plain; charset=utf-8")
    req <- httr2::req_headers(req,
                              Title    = "Nemeton FORDEAD",
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
.build_fordead_progress_callback <- function(progress_path, ntfy, i18n) {
  file_cb <- .build_progress_writer(progress_path)
  state   <- new.env(parent = emptyenv())
  state$last_phase <- ""
  function(event) {
    if (!is.null(file_cb)) {
      tryCatch(file_cb(event), error = function(e) invisible(NULL))
    }
    current <- as.character(event$current %||% "")
    if (identical(current, "fordead:phase")) {
      phase_name <- as.character(event$phase_name %||% "")
      if (nzchar(phase_name) && !identical(phase_name, state$last_phase)) {
        state$last_phase <- phase_name
        .ntfy_send(
          ntfy,
          sprintf(i18n$t("monitoring_ntfy_fordead_phase"),
                  .fordead_phase_label(phase_name, i18n)),
          priority = "low", tags = "hourglass_flowing_sand"
        )
      }
    }
    invisible(NULL)
  }
}


#' Human-readable duration string
#'
#' Formats a number of seconds as `"45 s"` / `"12 min"` / `"13 h 47 min"`.
#' Returns `"?"` for `NULL` / non-finite input.
#'
#' @param sec Number of seconds.
#' @return A length-1 character.
#' @noRd
.format_duration_human <- function(sec) {
  if (is.null(sec)) return("?")
  sec <- suppressWarnings(as.numeric(sec)[1])
  if (length(sec) != 1L || !is.finite(sec)) return("?")
  if (sec < 60) return(sprintf("%d s", as.integer(round(sec))))
  total_min <- floor(sec / 60)
  hours <- total_min %/% 60
  mins  <- total_min %% 60
  if (hours > 0) {
    sprintf("%d h %02d min", hours, mins)
  } else {
    sprintf("%d min", mins)
  }
}


#' Reconcile the FORDEAD UI state from disk after a session reload
#'
#' When a FORDEAD run outlives its Shiny session (long run + browser
#' disconnect), the in-session `fordead_last_result()` reactiveVal is
#' lost on reload — even though the worker completed and persisted its
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
#' Returns a named character vector — `future` auto-pickles it as a
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
    # ntfy push channel (E6 — out-of-band FORDEAD progress). The worker
    # outlives the Shiny session on long runs; ntfy is how the user
    # still gets notified. Forwarded so the worker can resolve the
    # topic / server / token without reaching app_state.
    "NEMETON_NTFY_URL",
    "NEMETON_NTFY_TOPIC",
    "NEMETON_NTFY_TOKEN"
  )
  vals <- vapply(keys, function(k) Sys.getenv(k, unset = ""), character(1))
  vals[nzchar(vals)]
}

#' Replay captured env vars on the worker side
#'
#' Called as the first line of the `future_promise()` body so all
#' downstream `nemeton::*` calls — including the verbose tracing
#' driven by `NEMETON_S2_CACHE_DEBUG` — see the same env as the
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

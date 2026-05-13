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

  shiny::ExtendedTask$new(function(zone_id, start, end, bands,
                                   max_cloud = 20, db_url = "",
                                   progress_path = NULL,
                                   cache_dir = NULL,
                                   skip_cached = TRUE) {
    if (requireNamespace("future", quietly = TRUE)) {
      plan_classes <- class(future::plan())
      is_parallel <- any(c("multisession", "multicore", "cluster") %in% plan_classes)
      if (!is_parallel) future::plan("multisession")
    }
    promises::future_promise({
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

      # Capture STAC / HTTP warnings emitted by nemeton (e.g. when
      # Planetary Computer returns 504 and nemeton falls back to "0
      # scenes found" silently). We propagate them back to the main
      # process so the result observer can surface the real cause
      # instead of a misleading "Téléchargement terminé : 0 scènes".
      warns <- character()
      summary <- withCallingHandlers(
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
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
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
    tryCatch({
      tmp <- paste0(progress_path, ".tmp")
      writeLines(
        jsonlite::toJSON(event, auto_unbox = TRUE, null = "null",
                         na = "null", POSIXt = "ISO8601"),
        con = tmp,
        useBytes = TRUE
      )
      # file.rename is atomic on POSIX, best-effort on Windows where
      # the destination must not exist — fall back to a write+unlink
      # cycle that is good enough for a polling reader (the reader
      # always wraps the read in tryCatch).
      ok <- suppressWarnings(file.rename(tmp, progress_path))
      if (!isTRUE(ok)) {
        file.copy(tmp, progress_path, overwrite = TRUE)
        unlink(tmp)
      }
    }, error = function(e) invisible(NULL))
  }
}


#' Async FORDEAD dieback diagnosis (E6.c.5 — spec 008)
#'
#' Wraps `nemeton::run_fordead_dieback()` in a `shiny::ExtendedTask`.
#' The full pipeline (5 phases via reticulate: vegetation index → train →
#' forest mask → dieback detection → export) takes minutes to hours, so
#' it must run in a `future` worker. The worker re-loads `nemetonshiny`
#' and opens a fresh DB connection.
#'
#' Inputs are passed at `$invoke()` time:
#' * `aoi`               — sf POLYGON EPSG:2154
#' * `dates_training`    — length-2 Date or character (start, end)
#' * `dates_monitoring`  — length-2 Date or character
#' * `threshold_anomaly` — numeric, default 0.16 (ONF/DSF 2024)
#' * `vegetation_index`  — "CRSWIR" / "NDVI" / "NDWI"
#' * `zone_id`           — integer, used for the DB INSERT
#'
#' On success, `$result()` returns the list produced by
#' `nemeton::run_fordead_dieback()`.
#'
#' @return A `shiny::ExtendedTask` object.
#' @noRd
run_fordead_async <- function() {
  .pkg_path <- tryCatch(pkgload::pkg_path(), error = function(e) NULL)

  shiny::ExtendedTask$new(function(aoi, dates_training, dates_monitoring,
                                   threshold_anomaly = 0.16,
                                   vegetation_index = "CRSWIR",
                                   zone_id = NULL, db_url = "",
                                   progress_path = NULL) {
    if (requireNamespace("future", quietly = TRUE)) {
      plan_classes <- class(future::plan())
      is_parallel <- any(c("multisession", "multicore", "cluster") %in% plan_classes)
      if (!is_parallel) future::plan("multisession")
    }
    promises::future_promise({
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

      progress_cb <- .build_progress_writer(progress_path)

      nemeton::run_fordead_dieback(
        aoi               = aoi,
        dates_training    = dates_training,
        dates_monitoring  = dates_monitoring,
        threshold_anomaly = threshold_anomaly,
        vegetation_index  = vegetation_index,
        con               = con,
        zone_id           = zone_id,
        progress_callback = progress_cb
      )
    }, seed = TRUE)
  })
}

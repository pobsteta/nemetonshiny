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
                                   max_cloud = 20) {
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

      con <- get_monitoring_db_connection()
      if (is.null(con)) {
        stop("Monitoring DB not configured (set NEMETON_DB_URL or NEMETON_DB_HOST/_PORT/_NAME/_USER/_PASSWORD).")
      }
      on.exit(close_monitoring_db_connection(con), add = TRUE)

      summary <- nemeton::ingest_sentinel2_timeseries(
        con       = con,
        zone_id   = zone_id,
        start     = start,
        end       = end,
        bands     = bands,
        max_cloud = max_cloud
      )

      list(
        status    = "success",
        summary   = summary,
        timestamp = Sys.time()
      )
    }, seed = TRUE)
  })
}

# Tests for the cache_dir + progress_callback wiring of
# run_ingestion_async() (v0.25.0 — feat(monitoring) wiring complete).
#
# We cannot exercise the real worker (future_promise is mocked to a
# synchronous NULL resolver in helper-fixtures.R), so we validate the
# two things we CAN check from the main process:
#   1. invoke() accepts the new cache_dir + progress_path parameters
#      without error.
#   2. The internal .build_progress_writer() helper returns NULL for a
#      NULL path and a working JSON writer otherwise.

test_that("run_ingestion_async invoke() accepts cache_dir + progress_path", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("promises")

  task <- nemetonshiny:::run_ingestion_async()

  tmp_cache <- withr::local_tempdir()
  tmp_pp    <- file.path(tmp_cache, "ingest_progress.json")

  expect_silent(task$invoke(
    zone_id       = 1L,
    start         = as.Date("2025-06-01"),
    end           = as.Date("2025-06-30"),
    bands         = c("NDVI", "NBR"),
    max_cloud     = 20,
    db_url        = "",
    progress_path = tmp_pp,
    cache_dir     = tmp_cache
  ))
})

test_that(".build_progress_writer returns NULL for NULL/empty path", {
  expect_null(nemetonshiny:::.build_progress_writer(NULL))
  expect_null(nemetonshiny:::.build_progress_writer(""))
})

test_that(".build_progress_writer writes a JSON event to disk", {
  skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()
  pp  <- file.path(tmp, "progress.json")

  writer <- nemetonshiny:::.build_progress_writer(pp)
  expect_true(is.function(writer))

  writer(list(current = "s2:scene", completed = 3L, total = 10L,
              scene_id = "S2A_MSIL2A_20240515"))

  expect_true(file.exists(pp))
  ev <- jsonlite::read_json(pp, simplifyVector = TRUE)
  expect_equal(ev$current, "s2:scene")
  expect_equal(ev$completed, 3L)
  expect_equal(ev$total, 10L)
  expect_equal(ev$scene_id, "S2A_MSIL2A_20240515")
})

test_that(".build_progress_writer swallows write errors", {
  # Pointing at a path under a non-existent directory triggers a write
  # failure. The writer must not error — the ingestion would otherwise
  # die on a transient FS hiccup that has nothing to do with STAC.
  pp <- file.path(tempdir(), "no_such_dir__", "progress.json")
  writer <- nemetonshiny:::.build_progress_writer(pp)
  expect_silent(writer(list(current = "s2:scene")))
  expect_false(file.exists(pp))
})

test_that(".resolve_s2_cache_dir uses <project>/cache/layers/sentinel2", {
  tmp_project <- withr::local_tempdir()
  project <- list(id = "test", path = tmp_project)

  cache_dir <- nemetonshiny:::.resolve_s2_cache_dir(project)

  expect_false(is.null(cache_dir))
  # normalizePath collapses slashes — compare via basename chain.
  expect_equal(basename(cache_dir), "sentinel2")
  expect_equal(basename(dirname(cache_dir)), "layers")
  expect_equal(basename(dirname(dirname(cache_dir))), "cache")
  expect_true(dir.exists(cache_dir))
})

test_that(".resolve_s2_cache_dir returns NULL with no project", {
  expect_null(nemetonshiny:::.resolve_s2_cache_dir(NULL))
  expect_null(nemetonshiny:::.resolve_s2_cache_dir(list(id = "x")))
})

test_that(".capture_worker_envvars snapshots only set NEMETON_* vars", {
  withr::with_envvar(
    c(NEMETON_S2_CACHE_DEBUG = "TRUE",
      NEMETON_DB_URL         = "",
      NEMETON_DB_HOST        = "pg.example"),
    {
      snap <- nemetonshiny:::.capture_worker_envvars()
      expect_true("NEMETON_S2_CACHE_DEBUG" %in% names(snap))
      expect_equal(snap[["NEMETON_S2_CACHE_DEBUG"]], "TRUE")
      expect_true("NEMETON_DB_HOST" %in% names(snap))
      expect_false("NEMETON_DB_URL" %in% names(snap))  # empty → skipped
    }
  )
})

test_that(".apply_worker_envvars is a no-op on NULL / empty input", {
  expect_silent(nemetonshiny:::.apply_worker_envvars(NULL))
  expect_silent(nemetonshiny:::.apply_worker_envvars(character()))
})

test_that(".apply_worker_envvars sets env vars from a named vector", {
  withr::with_envvar(c(NEMETON_S2_CACHE_DEBUG = ""), {
    nemetonshiny:::.apply_worker_envvars(c(NEMETON_S2_CACHE_DEBUG = "TRUE"))
    expect_equal(Sys.getenv("NEMETON_S2_CACHE_DEBUG"), "TRUE")
  })
})

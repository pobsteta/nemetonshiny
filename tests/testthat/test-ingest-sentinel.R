# Tests for the FAST ingestion run sentinel + resume detection
# (v0.85.2.9000) — worker-side sentinel survives session end, a
# relaunched Shiny instance reads it to surface a running / interrupted
# banner and offer to resume.

test_that(".write_ingest_sentinel + .read_ingest_sentinel roundtrip", {
  withr::with_tempdir({
    p <- file.path(getwd(), "ingest_run.json")
    out <- nemetonshiny:::.write_ingest_sentinel(
      p, "running",
      list(zone_id = 7L, zone_name = "villards",
           date_from = "2024-01-01", date_to = "2024-12-31")
    )
    expect_equal(out, p)
    expect_true(file.exists(p))
    sent <- nemetonshiny:::.read_ingest_sentinel(p)
    expect_equal(sent$status, "running")
    expect_equal(sent$zone_id, 7L)
    expect_equal(sent$zone_name, "villards")
    expect_true(is.numeric(sent$updated_at))
  })
})

test_that(".write_ingest_sentinel is a silent no-op for NULL/empty path", {
  expect_null(nemetonshiny:::.write_ingest_sentinel(NULL, "running"))
  expect_null(nemetonshiny:::.write_ingest_sentinel("", "running"))
})

test_that(".read_ingest_sentinel returns NULL for missing/empty path", {
  expect_null(nemetonshiny:::.read_ingest_sentinel(NULL))
  expect_null(nemetonshiny:::.read_ingest_sentinel(""))
  expect_null(nemetonshiny:::.read_ingest_sentinel(
    file.path(tempdir(), "does-not-exist-xyz.json")))
})

test_that(".detect_ingest_state returns 'none' without a project or sentinel", {
  expect_equal(nemetonshiny:::.detect_ingest_state(NULL)$state, "none")
  withr::with_tempdir({
    project <- list(path = getwd())
    expect_equal(
      nemetonshiny:::.detect_ingest_state(project)$state, "none")
  })
})

test_that(".detect_ingest_state reports 'finished' for a terminal sentinel", {
  withr::with_tempdir({
    project <- list(path = getwd())
    sp <- nemetonshiny:::.resolve_progress_path(project, "ingest_run.json")
    nemetonshiny:::.write_ingest_sentinel(sp, "done", list(n_scenes = 12L))
    st <- nemetonshiny:::.detect_ingest_state(project)
    expect_equal(st$state, "finished")
  })
})

test_that(".detect_ingest_state reports 'running' with fresh progress + tiles", {
  withr::with_tempdir({
    project <- list(path = getwd())
    sp <- nemetonshiny:::.resolve_progress_path(project, "ingest_run.json")
    pp <- nemetonshiny:::.resolve_progress_path(project, "ingest_progress.json")
    nemetonshiny:::.write_ingest_sentinel(sp, "running", list(zone_id = 1L))
    jsonlite::write_json(
      list(current = "s2:scene", completed = 42L, total = 370L),
      pp, auto_unbox = TRUE)
    st <- nemetonshiny:::.detect_ingest_state(project, freshness_sec = 90)
    expect_equal(st$state, "running")
    expect_equal(st$tile_cur, 42L)
    expect_equal(st$tile_tot, 370L)
  })
})

test_that(".detect_ingest_state reports 'interrupted' when progress is stale", {
  withr::with_tempdir({
    project <- list(path = getwd())
    sp <- nemetonshiny:::.resolve_progress_path(project, "ingest_run.json")
    pp <- nemetonshiny:::.resolve_progress_path(project, "ingest_progress.json")
    nemetonshiny:::.write_ingest_sentinel(sp, "running", list(zone_id = 1L))
    jsonlite::write_json(
      list(current = "s2:scene", completed = 5L, total = 370L),
      pp, auto_unbox = TRUE)
    # freshness_sec = 0 → any positive age is treated as a dead worker.
    st <- nemetonshiny:::.detect_ingest_state(project, freshness_sec = 0)
    expect_equal(st$state, "interrupted")
    expect_equal(st$tile_cur, 5L)
  })
})

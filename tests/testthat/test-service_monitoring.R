# Tests for service_monitoring.R (E6.b phase 2 — async ingestion task).
#
# We never spawn a real future worker in tests (helper-fixtures.R
# globally short-circuits promises::future_promise). The only thing
# we can verify here is that the wrapper returns a well-formed
# ExtendedTask and that its public API matches what the module
# expects (invoke / result / status methods).

test_that("run_ingestion_async returns a shiny::ExtendedTask", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("promises")

  task <- nemetonshiny:::run_ingestion_async()

  expect_true(inherits(task, "ExtendedTask"))
  expect_true(is.function(task$invoke))
  expect_true(is.function(task$result))
  expect_true(is.function(task$status))
})

test_that("run_ingestion_async fresh task starts in 'initial' status", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("promises")

  task <- nemetonshiny:::run_ingestion_async()

  # status() is a reactive expression — must be called inside a
  # reactive context. Use isolate() to read the current value.
  current <- shiny::isolate(task$status())
  expect_equal(current, "initial")
})

test_that("run_ingestion_async accepts the expected invoke signature", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("promises")

  task <- nemetonshiny:::run_ingestion_async()

  # The internal function passed to ExtendedTask$new is wrapped — we
  # can't inspect its formals directly, but we can verify the
  # invoke() call doesn't error on the canonical argument set
  # (future_promise is mocked to a NULL resolver in tests, so no
  # worker is spawned).
  expect_silent(task$invoke(
    zone_id   = 1L,
    start     = as.Date("2025-06-01"),
    end       = as.Date("2025-06-30"),
    bands     = c("NDVI", "NBR"),
    max_cloud = 20
  ))
})


# ---- ntfy push channel (E6 — out-of-band FORDEAD progress) -----------

test_that(".ntfy_config returns NULL when no topic is set", {
  withr::with_envvar(c(NEMETON_NTFY_TOPIC = NA), {
    expect_null(nemetonshiny:::.ntfy_config())
  })
})

test_that(".ntfy_config resolves url / topic / token from the env", {
  withr::with_envvar(
    c(NEMETON_NTFY_TOPIC = "forest-alerts",
      NEMETON_NTFY_URL   = "https://ntfy.example.org/",
      NEMETON_NTFY_TOKEN = "tk_secret"),
    {
      cfg <- nemetonshiny:::.ntfy_config()
      expect_type(cfg, "list")
      expect_equal(cfg$topic, "forest-alerts")
      # trailing slash trimmed
      expect_equal(cfg$url, "https://ntfy.example.org")
      expect_equal(cfg$token, "tk_secret")
    }
  )
})

test_that(".ntfy_config defaults the server to ntfy.sh", {
  withr::with_envvar(
    c(NEMETON_NTFY_TOPIC = "t", NEMETON_NTFY_URL = NA,
      NEMETON_NTFY_TOKEN = NA),
    {
      cfg <- nemetonshiny:::.ntfy_config()
      expect_equal(cfg$url, "https://ntfy.sh")
      expect_equal(cfg$token, "")
    }
  )
})

test_that(".ntfy_send is a silent no-op when config is NULL", {
  expect_false(nemetonshiny:::.ntfy_send(NULL, "message"))
})

test_that(".format_duration_human formats seconds / minutes / hours", {
  expect_equal(nemetonshiny:::.format_duration_human(45), "45 s")
  expect_equal(nemetonshiny:::.format_duration_human(720), "12 min")
  expect_equal(nemetonshiny:::.format_duration_human(49605), "13 h 46 min")
  expect_equal(nemetonshiny:::.format_duration_human(NA), "?")
  expect_equal(nemetonshiny:::.format_duration_human(NULL), "?")
})

test_that(".parse_fordead_mask_timestamp extracts the embedded stamp", {
  expect_equal(
    nemetonshiny:::.parse_fordead_mask_timestamp(
      "dieback_mask_20260520T221320.tif"),
    "2026-05-20 22:13"
  )
  expect_true(is.na(
    nemetonshiny:::.parse_fordead_mask_timestamp("no_timestamp.tif")))
})


# ---- Piste 2 — FORDEAD state reconciliation from disk ----------------

test_that(".reconcile_fordead_state returns NULL without a persisted mask", {
  expect_null(nemetonshiny:::.reconcile_fordead_state(NULL, 2L))
  expect_null(nemetonshiny:::.reconcile_fordead_state(list(path = NULL), 2L))
  withr::with_tempdir({
    proj <- list(path = getwd())
    expect_null(nemetonshiny:::.reconcile_fordead_state(proj, 2L))
    expect_null(nemetonshiny:::.reconcile_fordead_state(proj, NA_integer_))
  })
})

test_that(".reconcile_fordead_state rebuilds a success result from the mask", {
  withr::with_tempdir({
    proj <- list(path = getwd())
    zdir <- file.path(getwd(), "cache", "layers", "fordead", "zone_3")
    dir.create(zdir, recursive = TRUE)
    file.create(file.path(zdir, "dieback_mask_20260101T080000.tif"))
    Sys.sleep(1.1)
    file.create(file.path(zdir, "dieback_mask_20260520T221320.tif"))

    rec <- nemetonshiny:::.reconcile_fordead_state(proj, 3L)
    expect_type(rec, "list")
    expect_equal(rec$status, "success")
    expect_true(isTRUE(rec$reconciled))
    expect_equal(rec$zone_id, 3L)
    # latest mask (by mtime) wins
    expect_match(rec$mask_path, "20260520T221320")
    expect_equal(rec$mask_timestamp, "2026-05-20 22:13")
    expect_true(is.na(rec$n_alerts_inserted))
  })
})


# ---- FORDEAD progress callback (file writer + ntfy compositing) ------

test_that(".build_fordead_progress_callback writes the JSON progress file", {
  withr::with_tempdir({
    ppath <- file.path(getwd(), "fordead_progress.json")
    cb <- nemetonshiny:::.build_fordead_progress_callback(
      ppath, NULL, get_i18n("fr"))
    expect_true(is.function(cb))
    cb(list(current = "fordead:phase", phase_name = "fit",
            completed = 0L, total = 6L))
    expect_true(file.exists(ppath))
    ev <- jsonlite::fromJSON(readLines(ppath, warn = FALSE))
    expect_equal(ev$current, "fordead:phase")
    expect_equal(ev$phase_name, "fit")
  })
})

test_that(".build_fordead_progress_callback tolerates a NULL progress path", {
  cb <- nemetonshiny:::.build_fordead_progress_callback(
    NULL, NULL, get_i18n("fr"))
  expect_silent(cb(list(current = "fordead:phase", phase_name = "predict")))
  expect_silent(cb(list(current = "fordead:complete")))
})

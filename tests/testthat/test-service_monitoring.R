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

test_that(".resolve_zone_name returns the DB name when the zone exists", {
  # v0.43.2 — used by FAST / FORDEAD workers to emit ntfy messages
  # with the zone name instead of the integer id.
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, ...) {
      data.frame(name = "villards", stringsAsFactors = FALSE)
    },
    .package = "DBI"
  )
  expect_identical(
    nemetonshiny:::.resolve_zone_name("fake-con", 1L),
    "villards"
  )
})

test_that(".resolve_zone_name falls back to the id when the zone is missing", {
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, ...) data.frame(),  # 0 rows
    .package = "DBI"
  )
  expect_identical(
    nemetonshiny:::.resolve_zone_name("fake-con", 42L),
    "42"
  )
})

test_that(".resolve_zone_name falls back to the id when DBI errors", {
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, ...) stop("driver hiccup"),
    .package = "DBI"
  )
  expect_identical(
    nemetonshiny:::.resolve_zone_name("fake-con", 7L),
    "7"
  )
})

test_that(".resolve_zone_name handles NULL con or NULL zone_id silently", {
  expect_identical(nemetonshiny:::.resolve_zone_name(NULL, 1L),   "1")
  expect_identical(nemetonshiny:::.resolve_zone_name("c", NULL),  "?")
  expect_identical(nemetonshiny:::.resolve_zone_name(NULL, NULL), "?")
})


test_that(".ntfy_send accepts a title argument (defaults to 'Nemeton')", {
  # v0.43.1 fix : the Title HTTP header was hard-coded to
  # "Nemeton FORDEAD", mislabelling FAST notifications. The function
  # now takes a `title` parameter (default neutral); FAST callers pass
  # "Nemeton FAST", FORDEAD callers "Nemeton FORDEAD".
  fn <- nemetonshiny:::.ntfy_send
  expect_true("title" %in% names(formals(fn)))
  expect_equal(eval(formals(fn)$title), "Nemeton")
  # Smoke : passing custom title with NULL cfg is still a no-op
  expect_false(fn(NULL, "msg", title = "Nemeton FAST"))
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


# ---- FAST ingestion progress callback (v0.42.1) ----------------------

test_that(".build_ingest_progress_callback writes the JSON progress file", {
  withr::with_tempdir({
    ppath <- file.path(getwd(), "ingest_progress.json")
    cb <- nemetonshiny:::.build_ingest_progress_callback(
      ppath, NULL, get_i18n("fr"))
    expect_true(is.function(cb))
    cb(list(current = "s2:scene", scene_id = "ABC", obs_date = "2024-05-01",
            completed = 0L, total = 12L))
    expect_true(file.exists(ppath))
    ev <- jsonlite::fromJSON(readLines(ppath, warn = FALSE))
    expect_equal(ev$current, "s2:scene")
    expect_equal(ev$scene_id, "ABC")
  })
})

test_that(".build_ingest_progress_callback tolerates a NULL progress path", {
  cb <- nemetonshiny:::.build_ingest_progress_callback(
    NULL, NULL, get_i18n("fr"))
  expect_silent(cb(list(current = "s2:scene", completed = 1L, total = 2L)))
  expect_silent(cb(list(current = "s2:worker_started")))
})


# ---- v0.42.1 — i18n parity for the new FAST ntfy keys -----------------

test_that("v0.42.1 — ntfy_ingest_* keys exist and have FR + EN", {
  translations <- nemetonshiny:::TRANSLATIONS
  keys <- c("monitoring_ntfy_ingest_start",
            "monitoring_ntfy_ingest_scenes",
            "monitoring_ntfy_ingest_complete",
            "monitoring_ntfy_ingest_error")
  for (key in keys) {
    expect_true(key %in% names(translations),
                info = paste("Missing key:", key))
    expect_true(nzchar(translations[[key]]$fr),
                info = paste("Empty FR for:", key))
    expect_true(nzchar(translations[[key]]$en),
                info = paste("Empty EN for:", key))
  }
})

test_that("v0.42.1 — ntfy_ingest sprintf placeholders are well-formed", {
  i18n <- nemetonshiny:::get_i18n("fr")
  # start : %s (zone id)
  expect_silent(sprintf(i18n$t("monitoring_ntfy_ingest_start"), "42"))
  # scenes : %d (count)
  expect_silent(sprintf(i18n$t("monitoring_ntfy_ingest_scenes"), 26L))
  # complete : %d %d %s (n_scenes, n_obs, duration)
  expect_silent(sprintf(i18n$t("monitoring_ntfy_ingest_complete"),
                        26L, 5200L, "3 min"))
  # error : %s (message)
  expect_silent(sprintf(i18n$t("monitoring_ntfy_ingest_error"),
                        "STAC 504"))
})

# ============================================================
# v0.54.0 — .prewarm_fast_alerts() : 4 cartes inconditionnelles
# ============================================================

test_that("v0.54.0 — .prewarm_fast_alerts calcule les 4 combinaisons (NDVI/NBR × count/rolling)", {
  skip_if_not_installed("nemeton")
  result_cache <- withr::local_tempdir()
  cache_s2     <- withr::local_tempdir()
  captured <- list()
  testthat::with_mocked_bindings(
    read_fast_alert_raster = function(con, zone_id, index, threshold,
                                       date_from, date_to, mode,
                                       window_days, cache_dir,
                                       cache_result, result_cache_dir,
                                       parallel = FALSE) {
      captured[[length(captured) + 1L]] <<- list(index = index,
                                                  mode = mode,
                                                  threshold = threshold,
                                                  result_cache_dir = result_cache_dir)
      invisible(NULL)
    },
    .package = "nemeton",
    {
      warns <- nemetonshiny:::.prewarm_fast_alerts(
        con              = "fake-con",
        zone_id          = 1L,
        date_from        = as.Date("2025-01-01"),
        date_to          = as.Date("2025-12-31"),
        cache_dir        = cache_s2,
        result_cache_dir = result_cache
      )
      expect_equal(length(captured), 4L)
      expect_equal(length(warns), 0L)
      # Les 4 combinaisons attendues, ordre indifférent.
      combos <- vapply(captured, function(c) paste(c$index, c$mode, sep = "_"),
                       character(1))
      expect_setequal(combos, c("NDVI_count", "NDVI_rolling",
                                "NBR_count",  "NBR_rolling"))
      # Threshold = NULL (défaut cœur 0.40/0.30).
      expect_true(all(vapply(captured, function(c) is.null(c$threshold),
                             logical(1))))
      # result_cache_dir bien forwardé.
      expect_true(all(vapply(captured,
                             function(c) identical(c$result_cache_dir, result_cache),
                             logical(1))))
    }
  )
})

test_that("v0.54.0 — .prewarm_fast_alerts continue sur échec partiel (NBR fail → NDVI OK)", {
  skip_if_not_installed("nemeton")
  result_cache <- withr::local_tempdir()
  cache_s2     <- withr::local_tempdir()
  testthat::with_mocked_bindings(
    read_fast_alert_raster = function(con, zone_id, index, ...) {
      if (identical(index, "NBR")) {
        stop("B12 band missing for many scenes")
      }
      invisible(NULL)
    },
    .package = "nemeton",
    {
      warns <- nemetonshiny:::.prewarm_fast_alerts(
        con              = "fake-con",
        zone_id          = 1L,
        date_from        = as.Date("2025-01-01"),
        date_to          = as.Date("2025-12-31"),
        cache_dir        = cache_s2,
        result_cache_dir = result_cache
      )
      # 2 warnings (NBR_count + NBR_rolling), NDVI tous deux OK.
      expect_equal(length(warns), 2L)
      expect_true(all(grepl("NBR/", warns)))
    }
  )
})

test_that("v0.54.0 — .prewarm_fast_alerts respecte le cancel coopératif", {
  skip_if_not_installed("nemeton")
  result_cache <- withr::local_tempdir()
  cache_s2     <- withr::local_tempdir()
  cancel_flag  <- file.path(withr::local_tempdir(), "cancel.flag")
  # Crée le flag AVANT l'appel — break immédiat.
  file.create(cancel_flag)
  call_count <- 0L
  testthat::with_mocked_bindings(
    read_fast_alert_raster = function(...) {
      call_count <<- call_count + 1L
      invisible(NULL)
    },
    .package = "nemeton",
    {
      nemetonshiny:::.prewarm_fast_alerts(
        con              = "fake-con",
        zone_id          = 1L,
        date_from        = as.Date("2025-01-01"),
        date_to          = as.Date("2025-12-31"),
        cache_dir        = cache_s2,
        result_cache_dir = result_cache,
        cancel_path      = cancel_flag
      )
      # Aucun appel : cancel flag check AVANT le 1er appel break.
      expect_equal(call_count, 0L)
    }
  )
})

test_that("v0.54.0 — .prewarm_fast_alerts no-op quand result_cache_dir est NULL/vide", {
  skip_if_not_installed("nemeton")
  call_count <- 0L
  testthat::with_mocked_bindings(
    read_fast_alert_raster = function(...) {
      call_count <<- call_count + 1L
      invisible(NULL)
    },
    .package = "nemeton",
    {
      # NULL → no-op
      warns_null <- nemetonshiny:::.prewarm_fast_alerts(
        con = "x", zone_id = 1L,
        date_from = as.Date("2025-01-01"), date_to = as.Date("2025-12-31"),
        cache_dir = "/tmp/s2", result_cache_dir = NULL
      )
      expect_equal(length(warns_null), 0L)
      # "" → no-op
      warns_empty <- nemetonshiny:::.prewarm_fast_alerts(
        con = "x", zone_id = 1L,
        date_from = as.Date("2025-01-01"), date_to = as.Date("2025-12-31"),
        cache_dir = "/tmp/s2", result_cache_dir = ""
      )
      expect_equal(length(warns_empty), 0L)
      expect_equal(call_count, 0L)
    }
  )
})

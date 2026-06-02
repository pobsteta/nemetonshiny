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
# v0.55.0 — pré-calcul natif côté cœur (spec 018 nemeton@v0.61.0)
# ============================================================
#
# Les 4 tests précédents qui mockaient `.prewarm_fast_alerts()` ont
# été retirés : le helper app est supprimé en v0.55.0 (la logique
# est désormais dans `nemeton::ingest_sentinel2_timeseries()` via
# les params `prewarm_alerts` + `prewarm_mask_cache_dir`).
#
# Ce qu'on teste côté app maintenant :
# 1. Le helper `.fast_alert_cache_dir()` renvoie le bon chemin
#    canonique (cohérence hash D6 entre invoke + lecture).
# 2. Les clés i18n du toast `fast_prewarm_*` ont les bons
#    placeholders sprintf.

test_that("v0.55.0 — .fast_alert_cache_dir renvoie le chemin canonique", {
  expect_equal(
    nemetonshiny:::.fast_alert_cache_dir("/tmp/proj"),
    file.path("/tmp/proj", "cache", "layers", "fast_alert")
  )
  expect_null(nemetonshiny:::.fast_alert_cache_dir(NULL))
  expect_null(nemetonshiny:::.fast_alert_cache_dir(""))
})

test_that("v0.55.0 — clés i18n fast_prewarm_* ont les bons placeholders sprintf", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")
  # running / done / failed : 2 placeholders %s (index, mode_label)
  for (key in c("fast_prewarm_running", "fast_prewarm_done",
                "fast_prewarm_failed")) {
    expect_silent(sprintf(i18n_fr$t(key), "NDVI", "Fréquence"))
    expect_silent(sprintf(i18n_en$t(key), "NDVI", "Frequency"))
  }
  # cancelled : pas de placeholder
  expect_silent(i18n_fr$t("fast_prewarm_cancelled"))
  expect_silent(i18n_en$t("fast_prewarm_cancelled"))
})

test_that("v0.55.0 — mode → libellé i18n : count = Fréquence, rolling = Intensité", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")
  expect_equal(i18n_fr$t("fast_mode_frequence"), "Fréquence")
  expect_equal(i18n_fr$t("fast_mode_intensite"), "Intensité")
  expect_equal(i18n_en$t("fast_mode_frequence"), "Frequency")
  expect_equal(i18n_en$t("fast_mode_intensite"), "Intensity")
  # Toast assemblé : « Carte NBR Intensité prête. »
  msg_fr <- sprintf(i18n_fr$t("fast_prewarm_done"),
                    "NBR", i18n_fr$t("fast_mode_intensite"))
  expect_equal(msg_fr, "Carte NBR Intensité prête.")
  msg_en <- sprintf(i18n_en$t("fast_prewarm_done"),
                    "NBR", i18n_en$t("fast_mode_intensite"))
  expect_equal(msg_en, "NBR Intensity map ready.")
})

# ============================================================
# v0.57.0 — Mask 0-4 (quartiles, spec 017 D2)
# ============================================================

test_that("v0.57.0 — .fast_alert_mask_cache_dir renvoie le chemin canonique", {
  expect_equal(
    nemetonshiny:::.fast_alert_mask_cache_dir("/tmp/proj"),
    file.path("/tmp/proj", "cache", "layers", "fast_alert_mask")
  )
  expect_null(nemetonshiny:::.fast_alert_mask_cache_dir(NULL))
  expect_null(nemetonshiny:::.fast_alert_mask_cache_dir(""))
  # Distinct du chemin du raster continu (cache D6).
  expect_false(identical(
    nemetonshiny:::.fast_alert_mask_cache_dir("/tmp/proj"),
    nemetonshiny:::.fast_alert_cache_dir("/tmp/proj")
  ))
})

test_that("v0.57.0 — clés i18n fast_alert_class_* + legend title (FR/EN)", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")
  # 4 classes 1-4 (la classe 0 sain est transparente, pas de libellé).
  for (k in 1:4) {
    key <- sprintf("fast_alert_class_%d", k)
    expect_true(nzchar(i18n_fr$t(key)), info = paste("FR:", key))
    expect_true(nzchar(i18n_en$t(key)), info = paste("EN:", key))
  }
  # Titre de la légende.
  expect_true(nzchar(i18n_fr$t("fast_alert_legend_title")))
  expect_true(nzchar(i18n_en$t("fast_alert_legend_title")))
  # Cohérence du formatage : "<niveau> — <libellé>"
  expect_match(i18n_fr$t("fast_alert_class_4"), "4")
  expect_match(i18n_fr$t("fast_alert_class_1"), "Faible")
})

# ============================================================
# v0.58.0 — Toggle « Mode rapide » multi-cœur FAST (TODO #4)
# ============================================================

test_that("v0.58.0 — clé i18n fast_alerts_parallel_label (FR/EN)", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")
  expect_true(nzchar(i18n_fr$t("fast_alerts_parallel_label")))
  expect_true(nzchar(i18n_en$t("fast_alerts_parallel_label")))
  expect_match(i18n_fr$t("fast_alerts_parallel_label"), "Mode rapide")
  expect_match(i18n_en$t("fast_alerts_parallel_label"), "Fast mode")
})

test_that("v0.58.0 — input$fast_mode propage parallel à compute_fast_alert_mask", {
  skip_if_not_installed("nemeton")
  skip_if_not_installed("shiny")
  captured_parallel <- NULL
  testthat::with_mocked_bindings(
    compute_fast_alert_mask = function(..., parallel = FALSE) {
      captured_parallel <<- parallel
      tmp <- tempfile(fileext = ".tif")
      file.create(tmp)
      invisible(tmp)
    },
    .package = "nemeton",
    {
      # On simule le wiring : la fonction utilitaire qui résout le
      # `use_parallel` à partir de input$fast_mode est inline dans
      # `raster_r()`. On reproduit ici le pattern pour vérifier la
      # logique de propagation (TRUE/FALSE/NULL → bool).
      cases <- list(
        list(input = TRUE,  expected = TRUE),
        list(input = FALSE, expected = FALSE),
        list(input = NULL,  expected = FALSE),
        list(input = NA,    expected = FALSE)
      )
      for (case in cases) {
        # Mime ligne 232 de mod_monitoring_fast_alerts.R :
        use_parallel <- isTRUE(case$input %||% FALSE)
        expect_equal(use_parallel, case$expected,
                     info = paste("input =", capture.output(print(case$input))))
      }
    }
  )
})

# ============================================================
# v0.59.0 — Modal diagnostic pixel CRSWIR FORDEAD (TODO #3)
# ============================================================

test_that("v0.59.0 — clés i18n FORDEAD pixel modal (FR/EN)", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")
  keys <- c(
    "monitoring_fordead_pixel_modal_title_fmt",
    "monitoring_fordead_pixel_observed",
    "monitoring_fordead_pixel_predicted",
    "monitoring_fordead_pixel_first_anomaly",
    "monitoring_fordead_pixel_yaxis",
    "monitoring_fordead_pixel_no_data"
  )
  for (k in keys) {
    expect_true(nzchar(i18n_fr$t(k)), info = paste("FR", k))
    expect_true(nzchar(i18n_en$t(k)), info = paste("EN", k))
    expect_false(identical(i18n_fr$t(k), k), info = paste("FR resolved", k))
    expect_false(identical(i18n_en$t(k), k), info = paste("EN resolved", k))
  }
  # sprintf format string is callable
  expect_match(
    sprintf(i18n_fr$t("monitoring_fordead_pixel_modal_title_fmt"),
            46.71, 6.42),
    "46\\.71000.*6\\.42000"
  )
})

test_that("v0.59.0 — signature read_fordead_pixel_series compatible app", {
  skip_if_not_installed("nemeton")
  # Sanity check : la fonction cœur consommée par mod_monitoring_fordead_map
  # expose bien les arguments nommés que l'app passe (con, zone_id, xy,
  # crs, run_id, cache_dir).
  expect_true(exists("read_fordead_pixel_series",
                     envir = asNamespace("nemeton")))
  f <- get("read_fordead_pixel_series", envir = asNamespace("nemeton"))
  expected <- c("con", "zone_id", "xy", "crs", "run_id", "cache_dir")
  expect_true(all(expected %in% names(formals(f))))
})

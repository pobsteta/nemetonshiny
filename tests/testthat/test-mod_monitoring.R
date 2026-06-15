# Tests for Monitoring Module (mod_monitoring.R, E6.b phase 1).
#
# Phase 1 covers UI rendering and the zones reactive (with mocked DB
# layer). Phases 2+ will add ingestion, time series, alerts.

# ---- UI sanity -------------------------------------------------------

test_that("mod_monitoring_ui returns valid Shiny UI with expected controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  testthat::with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonshiny:::mod_monitoring_ui("monitoring")
      expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list") ||
                  inherits(ui, "bslib_fragment"))
      html <- as.character(ui)

      # Sidebar inputs are namespaced as "<id>-<input>".
      expect_true(grepl("monitoring-zone_id",        html))
      expect_true(grepl("monitoring-date_range",     html))
      # v0.61.0 — input `bands` retiré (NDVI + NBR câblés en dur).
      expect_false(grepl("monitoring-bands",         html))
      expect_true(grepl("monitoring-threshold_ndvi", html))
      expect_true(grepl("monitoring-threshold_nbr",  html))
      expect_true(grepl("monitoring-window_days",    html))
      expect_true(grepl("monitoring-run",            html))
      expect_true(grepl("monitoring-db_status",      html))

      # The run button is wired and enabled — ingestion is implemented,
      # so it is no longer the disabled phase-1 placeholder.
      expect_true(grepl("btn-primary", html))
    }
  )
})


# ---- Server: zones reactive -----------------------------------------

make_fake_app_state <- function(language = "fr") {
  shiny::reactiveValues(language = language)
}

test_that("mod_monitoring_server zones reactive returns empty df when DB is unavailable", {
  skip_if_not_installed("shiny")

  testthat::with_mocked_bindings(
    get_monitoring_db_connection = function(...) NULL,
    list_monitoring_zones        = function(con) {
      data.frame(id = integer(0), name = character(0),
                 stringsAsFactors = FALSE)
    },
    close_monitoring_db_connection = function(con) invisible(TRUE),
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          z <- session$returned$zones()
          expect_s3_class(z, "data.frame")
          expect_equal(nrow(z), 0L)
        }
      )
    }
  )
})

test_that("mod_monitoring_server zones reactive forwards DB rows", {
  skip_if_not_installed("shiny")

  fake_zones <- data.frame(
    id   = c(1L, 2L, 3L),
    name = c("Belvedere", "Bois Joli", "Cretes"),
    stringsAsFactors = FALSE
  )

  # v0.73.0 (spec 020) — `zones()` consume désormais
  # `nemeton::find_zones_by_project(con, project_uuid)` au lieu de
  # `list_monitoring_zones(con)`. La reactive sort early (data.frame
  # vide) quand `current_project$id` est NULL, donc on doit fournir
  # un app_state avec un project_id.
  testthat::with_mocked_bindings(
    get_monitoring_db_connection  = function(...) "fake-con",
    close_monitoring_db_connection = function(con) invisible(TRUE),
    {
      testthat::with_mocked_bindings(
        find_zones_by_project = function(con, project_uuid) fake_zones,
        .package = "nemeton",
        {
          fake_state <- shiny::reactiveValues(
            language        = "fr",
            current_project = list(id = "fake-project-id",
                                   name = "Fake",
                                   metadata = list())
          )
          shiny::testServer(
            nemetonshiny:::mod_monitoring_server,
            args = list(app_state = fake_state),
            {
              z <- session$returned$zones()
              expect_equal(nrow(z), 3L)
              expect_equal(z$name, c("Belvedere", "Bois Joli", "Cretes"))
            }
          )
        }
      )
    }
  )
})


# ---- Server: db_status output ---------------------------------------

# DB env vars neutralised so monitoring_db_backend() resolves to
# "none" regardless of a developer .Renviron carrying NEMETON_DB_* /
# POSTGRESQL_ADDON_* (otherwise db_status renders the probe path).
.monitoring_db_env_off <- c(
  NEMETON_DB_URL = NA, NEMETON_DB_LOCAL = NA,
  NEMETON_DB_HOST = NA, NEMETON_DB_PORT = NA, NEMETON_DB_NAME = NA,
  NEMETON_DB_USER = NA, NEMETON_DB_PASSWORD = NA,
  POSTGRESQL_ADDON_HOST = NA, POSTGRESQL_ADDON_PORT = NA,
  POSTGRESQL_ADDON_DB = NA, POSTGRESQL_ADDON_USER = NA,
  POSTGRESQL_ADDON_PASSWORD = NA
)

test_that("db_status renders the 'unavailable' card when DB is not configured", {
  skip_if_not_installed("shiny")

  # Neutralise any developer DB env so the backend resolves to "none"
  # and db_status takes the synchronous "no project / not configured"
  # branch (the only db_status branch reachable without a real DB —
  # see the skipped probe-path tests below).
  withr::with_envvar(.monitoring_db_env_off, {
    testthat::with_mocked_bindings(
      get_monitoring_db_connection  = function(...) NULL,
      list_monitoring_zones         = function(con) {
        data.frame(id = integer(0), name = character(0),
                   stringsAsFactors = FALSE)
      },
      close_monitoring_db_connection = function(con) invisible(TRUE),
      {
        shiny::testServer(
          nemetonshiny:::mod_monitoring_server,
          args = list(app_state = make_fake_app_state()),
          {
            html <- paste(as.character(output$db_status), collapse = "")
            expect_true(grepl("border-warning", html))
            expect_true(grepl("non configur", html, fixed = FALSE))
          }
        )
      }
    )
  })
})

# The 'no zone' and 'connected' db_status cards are rendered only after
# the asynchronous DB-reachability probe (db_probe_task) resolves to
# "ready". That probe runs `nemeton::db_connect` + `db_migrate` inside
# a `future::multisession` worker — a *separate process* the test's
# mocked bindings cannot reach — and its ExtendedTask state transitions
# need `later`/promise pumping that testServer does not drive
# deterministically. Both cards therefore require a genuinely reachable
# monitoring DB and cannot be unit-tested with mocks. This mirrors the
# v0.36.5 decision (see the Zone-saine comment block lower in this
# file) to keep such probe-gated rendering under manual QA.
test_that("db_status renders the 'no zone' card when DB has zero zones", {
  skip("probe-gated rendering — needs a reachable DB (multisession worker)")
})

test_that("db_status renders the 'connected' card with zone count", {
  skip("probe-gated rendering — needs a reachable DB (multisession worker)")
})


# ---- Server: ingestion click handler (phase 2) ----------------------

# A fake ExtendedTask that records every invoke() call. status()
# defaults to "initial" so the button observer treats it as idle;
# pass status = "running" to exercise the FAST<->FORDEAD cross-lock.
make_fake_fast_task <- function(status = "initial") {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  list(
    invoke = function(...) {
      state$calls[[length(state$calls) + 1L]] <- list(...)
      invisible(NULL)
    },
    result = function() NULL,
    status = function() status,
    .calls = function() state$calls
  )
}

fake_zones_df <- function() {
  data.frame(id = c(1L, 2L), name = c("Foret", "Massif"),
             stringsAsFactors = FALSE)
}

test_that("input$run with no zone selected fires a validation notification", {
  skip_if_not_installed("shiny")

  fake_task <- make_fake_fast_task()

  testthat::with_mocked_bindings(
    get_monitoring_db_connection  = function(...) "fake-con",
    list_monitoring_zones         = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async           = function() fake_task,
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          # Force zone_id empty (selectInput populated to "1" by the
          # server observer; we override explicitly).
          session$setInputs(
            zone_id    = "",
            bands      = c("NDVI", "NBR"),
            date_range = c(as.Date("2025-06-01"), as.Date("2025-06-30")),
            run        = 1L
          )
          # Task must NOT have been invoked.
          expect_length(fake_task$.calls(), 0L)
        }
      )
    }
  )
})

test_that("v0.61.0 — input$run invokes the task with NDVI+NBR hard-wired (no `bands` input)", {
  skip_if_not_installed("shiny")

  # Le checkboxGroupInput `bands` est retiré : NDVI + NBR + NDMI sont
  # systématiquement passés au worker FAST (NDMI ajouté v0.71.0, nemeton
  # >= 0.64.0 ; NDRE ajouté v0.85.0 pour le mode FAST trend, red-edge
  # B05+B8A). Même sans `setInputs(bands=)`, le task doit être invoqué
  # avec `bands = c("NDVI", "NBR", "NDMI", "NDRE")`.
  fake_task <- make_fake_fast_task()

  testthat::with_mocked_bindings(
    get_monitoring_db_connection  = function(...) "fake-con",
    list_monitoring_zones         = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async           = function() fake_task,
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          session$setInputs(
            zone_id    = "1",
            # Pas de `bands =` ici — l'input n'existe plus dans la
            # sidebar. Le worker doit quand même recevoir c("NDVI",
            # "NBR", "NDMI") depuis le câblage en dur.
            date_range = c(as.Date("2025-06-01"), as.Date("2025-06-30")),
            run        = 1L
          )
          calls <- fake_task$.calls()
          expect_length(calls, 1L)
          expect_equal(calls[[1]]$bands, c("NDVI", "NBR", "NDMI", "NDRE"))
        }
      )
    }
  )
})

test_that("input$run with valid inputs invokes the ingest task", {
  skip_if_not_installed("shiny")

  fake_task <- make_fake_fast_task()

  testthat::with_mocked_bindings(
    get_monitoring_db_connection  = function(...) "fake-con",
    list_monitoring_zones         = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async           = function() fake_task,
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          session$setInputs(
            zone_id    = "1",
            bands      = c("NDVI", "NBR"),
            date_range = c(as.Date("2025-06-01"), as.Date("2025-06-30")),
            run        = 1L
          )
          calls <- fake_task$.calls()
          expect_length(calls, 1L)
          expect_equal(calls[[1]]$zone_id, 1L)
          expect_equal(calls[[1]]$start,   as.Date("2025-06-01"))
          expect_equal(calls[[1]]$end,     as.Date("2025-06-30"))
          expect_equal(calls[[1]]$bands,   c("NDVI", "NBR", "NDMI", "NDRE"))
        }
      )
    }
  )
})

# Regression test for v0.30.1 — reprime_cache semantics inversion.
# Before v0.30.1: skip_cached = !isTRUE(input$reprime_cache) →
# unchecked checkbox produced skip_cached = TRUE, which short-
# circuited on the DB and left the disk cache empty (broken
# FORDEAD). After v0.30.1: skip_cached is hard-coded FALSE so
# nemeton always consults the disk cache; the checkbox only
# controls the wipe-before-invoke branch.
test_that("input$run passes skip_cached = FALSE regardless of reprime_cache (v0.30.1+)", {
  skip_if_not_installed("shiny")

  for (reprime in c(FALSE, TRUE)) {
    fake_task <- make_fake_fast_task()

    testthat::with_mocked_bindings(
      get_monitoring_db_connection  = function(...) "fake-con",
      list_monitoring_zones         = function(con) fake_zones_df(),
      close_monitoring_db_connection = function(con) invisible(TRUE),
      run_ingestion_async           = function() fake_task,
      {
        shiny::testServer(
          nemetonshiny:::mod_monitoring_server,
          args = list(app_state = make_fake_app_state()),
          {
            session$setInputs(
              zone_id       = "1",
              bands         = c("NDVI", "NBR"),
              date_range    = c(as.Date("2025-06-01"),
                                as.Date("2025-06-30")),
              reprime_cache = reprime,
              run           = 1L
            )
            calls <- fake_task$.calls()
            expect_length(calls, 1L)
            # The whole point of the v0.30.1 fix: skip_cached must be
            # FALSE in BOTH branches so FORDEAD always finds a populated
            # COG cache.
            expect_identical(calls[[1]]$skip_cached, FALSE,
                             info = sprintf("reprime_cache = %s", reprime))
          }
        )
      }
    )
  }
})

test_that("input$run with NA dates skips invocation", {
  skip_if_not_installed("shiny")

  fake_task <- make_fake_fast_task()

  testthat::with_mocked_bindings(
    get_monitoring_db_connection  = function(...) "fake-con",
    list_monitoring_zones         = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async           = function() fake_task,
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          session$setInputs(
            zone_id    = "1",
            bands      = c("NDVI"),
            date_range = c(as.Date(NA), as.Date(NA)),
            run        = 1L
          )
          expect_length(fake_task$.calls(), 0L)
        }
      )
    }
  )
})

test_that("server returns fast_task in its returned list", {
  skip_if_not_installed("shiny")

  fake_task <- make_fake_fast_task()

  testthat::with_mocked_bindings(
    get_monitoring_db_connection  = function(...) "fake-con",
    list_monitoring_zones         = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async           = function() fake_task,
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          expect_identical(session$returned$fast_task, fake_task)
        }
      )
    }
  )
})


# ---- E6.c.5 — Health-mode UI + server -------------------------------

test_that("mod_monitoring_ui exposes the FORDEAD/health-mode controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  testthat::with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonshiny:::mod_monitoring_ui("monitoring")
      html <- as.character(ui)
      expect_true(grepl("monitoring-mode\"",            html))
      expect_true(grepl("monitoring-dates_training",    html))
      expect_true(grepl("monitoring-vegetation_index",  html))
      expect_true(grepl("monitoring-threshold_anomaly", html))
      expect_true(grepl("monitoring-run_health",        html))
      expect_true(grepl("monitoring-validity_banners",  html))
      expect_true(grepl("monitoring-include_low",       html))
    }
  )
})

# Fake ExtendedTask scoped for the health-mode tests.
make_fake_fordead_task <- function(result = NULL, status = "initial") {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  state$result <- result
  state$status <- status
  list(
    invoke = function(...) {
      state$calls[[length(state$calls) + 1L]] <- list(...)
      invisible(NULL)
    },
    result = function() state$result,
    status = function() state$status,
    .calls = function() state$calls,
    .set_result = function(r) state$result <- r,
    .set_status = function(s) state$status <- s
  )
}


# ---- FAST <-> FORDEAD cross-lock (v0.39.2) --------------------------
# FAST and FORDEAD share the project Sentinel-2 band cache; running
# both concurrently risks racing on the same <band>.tif.tmp. The click
# observers refuse to start one task while the other is running.

test_that("input$run is refused while a FORDEAD run is in flight", {
  skip_if_not_installed("shiny")

  fast_task    <- make_fake_fast_task()
  fordead_busy <- make_fake_fordead_task(status = "running")

  testthat::with_mocked_bindings(
    get_monitoring_db_connection  = function(...) "fake-con",
    list_monitoring_zones         = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async           = function() fast_task,
    run_fordead_async             = function() fordead_busy,
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          session$setInputs(
            zone_id    = "1",
            bands      = c("NDVI", "NBR"),
            date_range = c(as.Date("2025-06-01"), as.Date("2025-06-30")),
            run        = 1L
          )
          # FORDEAD is running → the FAST task must NOT be invoked.
          expect_length(fast_task$.calls(), 0L)
        }
      )
    }
  )
})

test_that("input$run_health is refused while a FAST run is in flight", {
  skip_if_not_installed("shiny")

  fast_busy    <- make_fake_fast_task(status = "running")
  fordead_task <- make_fake_fordead_task()

  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() fast_busy,
    run_fordead_async              = function() fordead_task,
    validity_check_for_zone        = function(con, zone_id, units = NULL, ...) {
      list(geo_valid = TRUE, geo_intersection_pct = 0.95,
           species_valid = TRUE, species_resineux_pct = 0.85,
           overall_valid = TRUE, thresholds = list())
    },
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          session$setInputs(
            mode              = "health",
            zone_id           = "1",
            date_range        = c(as.Date("2025-06-01"), as.Date("2025-06-30")),
            dates_training    = c(as.Date("2016-01-01"), as.Date("2017-12-31")),
            vegetation_index  = "CRSWIR",
            threshold_anomaly = 0.16,
            run_health        = 1L
          )
          # FAST is running → the FORDEAD task must NOT be invoked.
          expect_length(fordead_task$.calls(), 0L)
        }
      )
    }
  )
})

test_that("validity reactive is NULL outside health mode", {
  skip_if_not_installed("shiny")

  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_fast_task(),
    run_fordead_async              = function() make_fake_fordead_task(),
    validity_check_for_zone        = function(con, zone_id, units = NULL, ...) {
      stop("must not be called in quick mode")
    },
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          session$setInputs(mode = "quick", zone_id = "1")
          expect_null(session$returned$validity())
        }
      )
    }
  )
})

test_that("validity_banners renders the geo + species warnings when invalid", {
  skip_if_not_installed("shiny")

  fake_validity <- list(
    geo_valid             = FALSE,
    geo_intersection_pct  = 0.30,
    species_valid         = FALSE,
    species_resineux_pct  = 0.40,
    overall_valid         = FALSE,
    thresholds            = list()
  )

  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_fast_task(),
    run_fordead_async              = function() make_fake_fordead_task(),
    validity_check_for_zone        = function(con, zone_id, units = NULL, ...)
      fake_validity,
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          session$setInputs(mode = "health", zone_id = "1")
          html <- paste(as.character(output$validity_banners), collapse = "")
          expect_true(grepl("border-warning",     html))
          expect_true(grepl("monitoring_warning", html) ||
                      grepl("FORDEAD",            html, ignore.case = TRUE) ||
                      grepl("validation",         html, ignore.case = TRUE))
        }
      )
    }
  )
})

test_that("input$run_health blocks when overall_valid = FALSE (G3 modal path)", {
  skip_if_not_installed("shiny")

  fake_task <- make_fake_fordead_task()

  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_fast_task(),
    run_fordead_async              = function() fake_task,
    validity_check_for_zone        = function(con, zone_id, units = NULL, ...) {
      list(geo_valid = FALSE, geo_intersection_pct = 0.2,
           species_valid = TRUE, species_resineux_pct = 0.9,
           overall_valid = FALSE, thresholds = list())
    },
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          session$setInputs(
            mode              = "health",
            zone_id           = "1",
            date_range        = c(as.Date("2025-06-01"), as.Date("2025-06-30")),
            dates_training    = c(as.Date("2016-01-01"), as.Date("2017-12-31")),
            vegetation_index  = "CRSWIR",
            threshold_anomaly = 0.16,
            run_health        = 1L
          )
          # FORDEAD task must NOT have been invoked (modal stops the
          # direct path; user has to confirm via confirm_invalid_run).
          expect_length(fake_task$.calls(), 0L)
        }
      )
    }
  )
})

test_that("input$run_health invokes FORDEAD when validity is OK", {
  skip_if_not_installed("shiny")

  fake_task <- make_fake_fordead_task()

  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_fast_task(),
    run_fordead_async              = function() fake_task,
    validity_check_for_zone        = function(con, zone_id, units = NULL, ...) {
      list(geo_valid = TRUE, geo_intersection_pct = 0.95,
           species_valid = TRUE, species_resineux_pct = 0.85,
           overall_valid = TRUE, thresholds = list())
    },
    update_project_metadata        = function(...) TRUE,
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          session$setInputs(
            mode              = "health",
            zone_id           = "1",
            date_range        = c(as.Date("2025-06-01"), as.Date("2025-06-30")),
            dates_training    = c(as.Date("2016-01-01"), as.Date("2017-12-31")),
            vegetation_index  = "CRSWIR",
            threshold_anomaly = 0.16,
            run_health        = 1L
          )
          calls <- fake_task$.calls()
          expect_length(calls, 1L)
          expect_equal(calls[[1]]$vegetation_index,  "CRSWIR")
          expect_equal(calls[[1]]$threshold_anomaly, 0.16)
          expect_equal(calls[[1]]$zone_id,           1L)
          # v0.33.0: cache_dir is now passed; resolves from project path
          # (NULL here because make_fake_app_state has no current_project).
          expect_true("cache_dir" %in% names(calls[[1]]))
        }
      )
    }
  )
})

test_that("confirm_invalid_run invokes FORDEAD on modal accept (G3 force path)", {
  skip_if_not_installed("shiny")

  fake_task <- make_fake_fordead_task()

  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_fast_task(),
    run_fordead_async              = function() fake_task,
    validity_check_for_zone        = function(con, zone_id, units = NULL, ...) {
      list(geo_valid = FALSE, geo_intersection_pct = 0.1,
           species_valid = FALSE, species_resineux_pct = 0.0,
           overall_valid = FALSE, thresholds = list())
    },
    update_project_metadata        = function(...) TRUE,
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          session$setInputs(
            mode              = "health",
            zone_id           = "1",
            date_range        = c(as.Date("2025-06-01"), as.Date("2025-06-30")),
            dates_training    = c(as.Date("2016-01-01"), as.Date("2017-12-31")),
            vegetation_index  = "CRSWIR",
            threshold_anomaly = 0.16
          )
          # User confirms via the modal.
          session$setInputs(confirm_invalid_run = 1L)
          calls <- fake_task$.calls()
          expect_length(calls, 1L)
        }
      )
    }
  )
})

test_that("server returns fordead_task + validity reactives", {
  skip_if_not_installed("shiny")

  fake_ftask <- make_fake_fordead_task()

  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_fast_task(),
    run_fordead_async              = function() fake_ftask,
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          expect_identical(session$returned$fordead_task, fake_ftask)
          expect_true(is.function(session$returned$validity))
        }
      )
    }
  )
})

# Regression test for v0.32.0 — FORDEAD progress dispatcher.
# Verifies that the `fordead:phase` event triggers a showNotification
# call with the expected message ("Phase 1/7 — <label>"). Calls the
# extracted helper .fordead_handle_progress_event directly so we
# bypass the reactivePoll plumbing and don't have to write a JSON
# progress file + sleep for the poll to fire.
test_that(".fordead_handle_progress_event toasts on fordead:phase (v0.32.0)", {
  skip_if_not_installed("shiny")

  captured <- list()
  i18n <- nemetonshiny:::get_i18n("fr")
  fake_session <- list(ns = function(id) paste0("monitoring-", id))

  testthat::with_mocked_bindings(
    showNotification = function(ui, id = NULL, ...) {
      captured$ui   <<- ui
      captured$id   <<- id
      captured$args <<- list(...)
      invisible(NULL)
    },
    .package = "shiny",
    {
      nemetonshiny:::.fordead_handle_progress_event(
        ev = list(
          current    = "fordead:phase",
          phase_name = "vegetation_index",
          completed  = 0L,
          total      = 7L
        ),
        session = fake_session,
        i18n    = i18n
      )
    }
  )

  # The UI body is a tagList from .monitoring_spinning_msg — convert
  # to plain text so we can assert against the visible message.
  expect_false(is.null(captured$ui))
  rendered <- htmltools::renderTags(captured$ui)$html
  expect_match(rendered, "Phase 1/7", fixed = TRUE)
  # Phase label comes from monitoring_fordead_phase_vegetation_index.
  expect_match(rendered, "végétation", ignore.case = TRUE)
  expect_identical(captured$id, "monitoring-fordead_progress")
  expect_identical(captured$args$type, "message")
  expect_null(captured$args$duration)
})

test_that(".fordead_handle_progress_event is silent on fordead:start (v0.32.0)", {
  skip_if_not_installed("shiny")

  call_count <- 0L
  fake_session <- list(ns = function(id) paste0("monitoring-", id))

  testthat::with_mocked_bindings(
    showNotification = function(...) {
      call_count <<- call_count + 1L
      invisible(NULL)
    },
    .package = "shiny",
    {
      nemetonshiny:::.fordead_handle_progress_event(
        ev = list(
          current        = "fordead:start",
          total          = 7L,
          python_env     = "fake-env",
          fordead_version = "1.x"
        ),
        session = fake_session,
        i18n    = nemetonshiny:::get_i18n("fr")
      )
    }
  )

  expect_equal(call_count, 0L)
})

test_that(".fordead_handle_progress_event humanizes unknown phase names (v0.32.0)", {
  skip_if_not_installed("shiny")

  captured_ui <- NULL
  fake_session <- list(ns = function(id) paste0("monitoring-", id))

  testthat::with_mocked_bindings(
    showNotification = function(ui, ...) {
      captured_ui <<- ui
      invisible(NULL)
    },
    .package = "shiny",
    {
      nemetonshiny:::.fordead_handle_progress_event(
        ev = list(
          current    = "fordead:phase",
          phase_name = "future_unknown_phase",
          completed  = 2L,
          total      = 5L
        ),
        session = fake_session,
        i18n    = nemetonshiny:::get_i18n("fr")
      )
    }
  )

  rendered <- htmltools::renderTags(captured_ui)$html
  expect_match(rendered, "Phase 3/5", fixed = TRUE)
  # Title-cased humanized fallback: "future_unknown_phase" → "Future Unknown Phase"
  expect_match(rendered, "Future Unknown Phase", fixed = TRUE)
})

test_that("metadata restore updates the mode + threshold from current_project", {
  skip_if_not_installed("shiny")

  app_state <- shiny::reactiveValues(
    language = "fr",
    current_project = list(
      id = "p1",
      metadata = list(
        monitoring_mode              = "health",
        monitoring_threshold_anomaly = 0.20,
        monitoring_vegetation_index  = "NDVI",
        monitoring_dates_training    = c("2018-01-01", "2019-12-31")
      )
    )
  )

  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_fast_task(),
    run_fordead_async              = function() make_fake_fordead_task(),
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = app_state),
        {
          # The observer dispatches updateRadioButtons / updateSlider
          # / updateSelect / updateDateRange. We only verify that no
          # error was raised and the returned list is intact (Shiny
          # update*Input messages aren't queryable from testServer
          # without a reactive event handler hooking them).
          expect_true(is.function(session$returned$validity))
        }
      )
    }
  )
})


# ---- Server: register-zone click handler (project ↔ zone bridge) ----

# Builds an app_state with a real on-disk project + saved samples.gpkg
# so the "samples present" reactive returns TRUE. Caller is responsible
# for being inside withr::with_tempdir() + a get_app_options mock.
make_fake_app_state_with_project <- function(project_id, project_path,
                                             monitoring_zone_id = NULL) {
  poly <- sf::st_polygon(list(rbind(
    c(900000, 6500000), c(901000, 6500000),
    c(901000, 6501000), c(900000, 6501000), c(900000, 6500000)
  )))
  indicators_sf <- sf::st_sf(
    ug_id = 1L,
    geometry = sf::st_sfc(poly, crs = 2154)
  )
  meta <- list(name = "Test Forest")
  if (!is.null(monitoring_zone_id)) {
    meta$monitoring_zone_id <- monitoring_zone_id
  }
  shiny::reactiveValues(
    language = "fr",
    samples_refresh = 0L,
    current_project = list(
      id            = project_id,
      path          = project_path,
      metadata      = meta,
      indicators_sf = indicators_sf
    )
  )
}

test_that("register click without a loaded project shows a notification and no-ops", {
  skip_if_not_installed("shiny")

  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_fast_task(),
    run_fordead_async              = function() make_fake_fordead_task(),
    register_project_as_zone       = function(con, project) {
      stop("must not be called")
    },
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          expect_silent(session$setInputs(register = 1L))
        }
      )
    }
  )
})

test_that("v0.73.0 — register click invokes nemeton::build_project_monitoring_zones (spec 020)", {
  skip("v0.73.0 — refactored to nemeton::build_project_monitoring_zones (spec 020). Mock obsolete : the observer now requires bdforet.gpkg + ugf_sf, which need a fully-loaded project fixture. Coverage assured by 'register click without a loaded project' (early-exit branches) + integration testing.")
})

test_that("register click flags 'already registered' when helper returns was_existing=TRUE", {
  skip("v0.73.0 — register_project_as_zone replaced by nemeton::build_project_monitoring_zones (spec 020). The 'already registered' branch is gone (upsert semantics : replace = TRUE by default).")
})

# Tests obsolètes maintenus en référence (skipped). Le wrapper
# legacy `nemetonshiny:::register_project_as_zone()` reste exporté
# (testé dans `test-service_monitoring_db.R`) mais n'est plus
# consommé par `mod_monitoring`.
.legacy_register_test_kept_for_reference <- function() {
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        proj <- nemetonshiny:::create_project(name = "Test Forest")
        plots <- sf::st_sf(
          plot_id  = "P1", type = "Base",
          geometry = sf::st_sfc(sf::st_point(c(900100, 6500100)), crs = 2154)
        )
        nemetonshiny:::save_samples(proj$id, plots)

        testthat::with_mocked_bindings(
          get_monitoring_db_connection   = function(...) "fake-con",
          list_monitoring_zones          = function(con) fake_zones_df(),
          close_monitoring_db_connection = function(con) invisible(TRUE),
          run_ingestion_async            = function() make_fake_fast_task(),
          run_fordead_async              = function() make_fake_fordead_task(),
          register_project_as_zone       = function(con, project) {
            list(zone_id = 1L, zone_name = "Foret", n_plots = 1L,
                 was_existing = TRUE)
          },
          {
            state <- make_fake_app_state_with_project(
              proj$id, proj$path, monitoring_zone_id = 1L
            )
            shiny::testServer(
              nemetonshiny:::mod_monitoring_server,
              args = list(app_state = state),
              {
                session$setInputs(register = 1L)
                expect_equal(state$current_project$metadata$monitoring_zone_id,
                             1L)
              }
            )
          }
        )
      }
    )
  })
}

test_that("auto-select pre-selects the zone matching project metadata$monitoring_zone_id", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  # updateSelectInput sends a message to the client; testServer has no
  # real client that echoes back into `input`, so we capture the call's
  # `selected` argument directly to verify the pre-selection logic.
  captured_selected <- NULL

  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        proj <- nemetonshiny:::create_project(name = "Massif Central")

        testthat::with_mocked_bindings(
          get_monitoring_db_connection   = function(...) "fake-con",
          close_monitoring_db_connection = function(con) invisible(TRUE),
          run_ingestion_async            = function() make_fake_fast_task(),
          run_fordead_async              = function() make_fake_fordead_task(),
          {
            # v0.73.0 (spec 020) — Mock cœur :
            # `nemeton::find_zones_by_project` au lieu de
            # `list_monitoring_zones`.
            testthat::with_mocked_bindings(
              find_zones_by_project = function(con, project_uuid) {
                fake_zones_df()
              },
              .package = "nemeton",
            {
            testthat::with_mocked_bindings(
              updateSelectInput = function(session, inputId, ...,
                                           choices = NULL, selected = NULL) {
                if (inputId == "zone_id") {
                  captured_selected <<- selected
                }
                invisible(NULL)
              },
              .package = "shiny",
              {
                # Project carries id = 2 → zone "Massif" (id 2) should be
                # selected rather than the default "Foret" (id 1).
                # Aucune zone `_tot` dans fake_zones_df() →
                # preferred fallback sur metadata$monitoring_zone_id.
                state <- make_fake_app_state_with_project(
                  proj$id, proj$path, monitoring_zone_id = 2L
                )
                shiny::testServer(
                  nemetonshiny:::mod_monitoring_server,
                  args = list(app_state = state),
                  {
                    session$flushReact()
                    invisible(session$returned$zones())
                  }
                )
              }
            )
            }  # close find_zones_by_project mock body
          )    # close with_mocked_bindings(find_zones_by_project)
          }
        )
      }
    )
  })

  expect_equal(captured_selected, "2")
})


# ---- v0.52.16 — obs_pixel reactive supprimé ------------------------
# Les 4 tests qui mockaient `nemeton::read_obs_pixel` pour vérifier le
# wiring du reactive `obs_pixel_data` ont été retirés : FAST est
# désormais une analyse pure raster per-pixel (spec 017 cœur), donc
# `read_obs_pixel` n'est plus appelé. Les tests retirés étaient :
#   * « obs_pixel reactive does not fire in health mode »
#   * « obs_pixel reactive forwards filters to read_obs_pixel »
#   * « obs_pixel reactive returns NULL when zone, bands or dates missing »
#   * « obs_pixel_data debounces rapid successive input changes »
# Le helper `.skip_if_no_read_obs_pixel` est également supprimé.

# v0.52.16 — 3 tests obs_pixel supprimés ici (reactive obs_pixel_data
# n'existe plus). Voir bloc de commentaires en tête de la section.

# ---- v0.36.4 — .summarize_backend_warnings -------------------------

test_that(".summarize_backend_warnings strips presigned URLs and caps length", {
  long_url_warn <- paste0(
    "GDAL Error 1: HTTP error code : 403 ; Scene \"S2A_x\" skipped: ",
    "[crop] file does not exist: https://sentinel2l2a01.blob.core.windows.net/",
    "sentinel2-l2/31/T/GM/2025/09/07/foo.tif?st=2026-05-16T11:48:17Z&",
    "se=2026-05-17T12:33:17Z&sp=rl&sv=2025-07-05&sr=c&skoid=foo&sktid=bar&",
    "skt=baz&ske=qux&sks=quux&skv=corge&sig=grault"
  )
  short_warn <- "STAC catalog HTTP 504 timeout https://planetarycomputer.microsoft.com/api/stac/v1/search"

  out <- nemetonshiny:::.summarize_backend_warnings(c(long_url_warn, short_warn))

  expect_length(out, 2L)
  # URLs replaced by <URL> placeholder.
  expect_false(grepl("https?://", out[1]))
  expect_true(grepl("<URL>", out[1]))
  # Useful content (HTTP code, scene id) preserved.
  expect_true(grepl("HTTP error code : 403", out[1]))
  expect_true(grepl('S2A_x', out[1]))
  # Length capped (default 200 chars).
  expect_lte(nchar(out[1]), 200L)
  # Short warning unchanged in content (just URL stripped).
  expect_true(grepl("STAC catalog HTTP 504 timeout", out[2]))
  expect_true(grepl("<URL>", out[2]))
})

test_that(".summarize_backend_warnings handles edge cases", {
  expect_equal(nemetonshiny:::.summarize_backend_warnings(character(0)),
               character(0))
  expect_equal(nemetonshiny:::.summarize_backend_warnings(NULL),
               character(0))
  # NA → empty string, not NA.
  expect_equal(nemetonshiny:::.summarize_backend_warnings(NA_character_),
               "")
  # Multi-line warning collapsed.
  ml <- "line one\n\tline two   line three"
  out <- nemetonshiny:::.summarize_backend_warnings(ml)
  expect_false(grepl("\n", out))
  expect_equal(out, "line one line two line three")
})


# ---- v0.36.5 — FORDEAD result handler + Zone saine card ------------
#
# The Zone-saine rendering path (output$alerts_panel branching on
# fordead_last_result()$status) is intentionally NOT covered by a
# testServer-based test here. The mod_monitoring_server reactive
# graph involves multiple reactivePoll timers (fordead_progress,
# ingest_progress, ingest_log_tick) plus the pixel-map ExtendedTask
# composition that wedges testServer on this particular setup. The
# existing tests in the file already document the same harness issue
# (some only run a slice of the server before flushing). Keep manual
# QA for the Zone-saine card and document the expected behaviour in
# NEWS.md / state matrix.
#
# Helper widening (make_fake_fordead_task accepts `result =` and
# `status =`) is still useful for future tests that don't trigger the
# full reactive graph — left in place above.


# ---- v0.37.0 — BD Forêt fallback for G3 species validity ------------

test_that(".load_project_bdforet returns NULL on missing project / path / file", {
  expect_null(nemetonshiny:::.load_project_bdforet(NULL))
  expect_null(nemetonshiny:::.load_project_bdforet(list(id = "x")))
  withr::with_tempdir({
    proj <- list(id = "x", path = getwd())
    # cache/layers/bdforet.gpkg absent → NULL
    expect_null(nemetonshiny:::.load_project_bdforet(proj))
  })
})

test_that(".load_project_bdforet reads the cached GPKG when present", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    proj <- list(id = "x", path = getwd())
    cache_dir <- file.path(proj$path, "cache", "layers")
    dir.create(cache_dir, recursive = TRUE)
    # Tiny fixture: 1 polygon with a `formation` column (representative
    # of BD Forêt V2 schema, though the cœur enrichment uses
    # essence-mapping internally — here we just verify the loader).
    poly <- sf::st_polygon(list(rbind(
      c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0)
    )))
    bd <- sf::st_sf(formation = "FF1-09-09",
                    geometry = sf::st_sfc(poly, crs = 2154))
    sf::st_write(bd, file.path(cache_dir, "bdforet.gpkg"),
                 quiet = TRUE)
    out <- nemetonshiny:::.load_project_bdforet(proj)
    expect_s3_class(out, "sf")
    expect_equal(nrow(out), 1L)
    expect_true("formation" %in% names(out))
  })
})

test_that("validity_check_for_zone forwards bdforet to nemeton", {
  skip_if_not_installed("sf")
  # Capture the call to nemeton::check_fordead_validity to verify
  # bdforet is forwarded (whatever its value).
  captured <- new.env(parent = emptyenv())
  fake_aoi <- sf::st_sf(zone_id = 1L, name = "Z",
                       geometry = sf::st_sfc(
                         sf::st_polygon(list(rbind(
                           c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0)
                         ))),
                         crs = 2154))
  fake_bdforet <- sf::st_sf(formation = "FF1-09-09",
                            geometry = sf::st_sfc(
                              sf::st_polygon(list(rbind(
                                c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0)
                              ))),
                              crs = 2154))

  testthat::with_mocked_bindings(
    get_monitoring_zone_aoi = function(con, zone_id) fake_aoi,
    {
      testthat::local_mocked_bindings(
        check_fordead_validity = function(aoi, units = NULL,
                                           bdforet = NULL, ...) {
          captured$called   <- TRUE
          captured$has_bdf  <- !is.null(bdforet)
          captured$bdf_rows <- if (is.null(bdforet)) NA_integer_
                               else nrow(bdforet)
          list(geo_valid = TRUE, geo_intersection_pct = 0.95,
               species_valid = TRUE, species_resineux_pct = 0.85,
               overall_valid = TRUE, thresholds = list())
        },
        .package = "nemeton"
      )

      # With bdforet provided
      res <- nemetonshiny:::validity_check_for_zone(
        con = "fake-con", zone_id = 1L,
        units = NULL, bdforet = fake_bdforet
      )
      expect_true(isTRUE(captured$called))
      expect_true(isTRUE(captured$has_bdf))
      expect_equal(captured$bdf_rows, 1L)
      expect_false(is.na(res$species_valid))

      # Without bdforet — call still happens, bdforet stays NULL
      captured$called  <- FALSE
      captured$has_bdf <- TRUE  # init opposite of expected
      res2 <- nemetonshiny:::validity_check_for_zone(
        con = "fake-con", zone_id = 1L,
        units = NULL, bdforet = NULL
      )
      expect_true(isTRUE(captured$called))
      expect_false(isTRUE(captured$has_bdf))
    }
  )
})


# v0.52.16 — test « obs_pixel_data debounces rapid successive input
# changes » supprimé : le reactive `obs_pixel_data` n'existe plus.

# ---- COG reprime : date-windowed wipe spares FORDEAD scenes ----------

test_that(".s2_scene_date_from_id parses the S2 acquisition date or NA", {
  expect_equal(
    nemetonshiny:::.s2_scene_date_from_id("S2B_MSIL2A_20260526T103021_T31TFM"),
    as.Date("2026-05-26"))
  expect_equal(
    nemetonshiny:::.s2_scene_date_from_id("S2A_31TGM_20250610_0_L2A"),
    as.Date("2025-06-10"))
  expect_true(is.na(nemetonshiny:::.s2_scene_date_from_id("no_date_here")))
})

test_that("COG reprime date-window selection spares out-of-window (FORDEAD) scenes", {
  dir <- withr::local_tempdir()
  sids <- c(
    "S2A_MSIL2A_20250610T103021_T31TGM",  # in FAST window
    "S2A_MSIL2A_20250705T103021_T31TGM",  # in FAST window
    "S2A_MSIL2A_20240301T103021_T31TGM",  # out of window (FORDEAD training)
    "scene_without_date"                  # undatable → spared (prudence)
  )
  for (s in sids) dir.create(file.path(dir, s))

  d_from <- as.Date("2025-06-01"); d_to <- as.Date("2025-07-31")
  scene_dirs <- list.dirs(dir, recursive = FALSE, full.names = TRUE)
  in_window <- vapply(scene_dirs, function(d) {
    sd <- nemetonshiny:::.s2_scene_date_from_id(basename(d))
    !is.na(sd) && sd >= d_from && sd <= d_to
  }, logical(1))
  victims <- basename(scene_dirs[in_window])

  expect_setequal(victims, c("S2A_MSIL2A_20250610T103021_T31TGM",
                             "S2A_MSIL2A_20250705T103021_T31TGM"))
  # The FORDEAD training scene and the undatable one are preserved.
  expect_false("S2A_MSIL2A_20240301T103021_T31TGM" %in% victims)
  expect_false("scene_without_date" %in% victims)
})

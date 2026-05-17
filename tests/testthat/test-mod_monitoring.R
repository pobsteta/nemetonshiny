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
      expect_true(grepl("monitoring-bands",          html))
      expect_true(grepl("monitoring-threshold_ndvi", html))
      expect_true(grepl("monitoring-threshold_nbr",  html))
      expect_true(grepl("monitoring-window_days",    html))
      expect_true(grepl("monitoring-run",            html))
      expect_true(grepl("monitoring-db_status",      html))

      # Run button is disabled in phase 1.
      expect_true(grepl("disabled", html))
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

  testthat::with_mocked_bindings(
    get_monitoring_db_connection  = function(...) "fake-con",
    list_monitoring_zones         = function(con) fake_zones,
    close_monitoring_db_connection = function(con) invisible(TRUE),
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          z <- session$returned$zones()
          expect_equal(nrow(z), 3L)
          expect_equal(z$name, c("Belvedere", "Bois Joli", "Cretes"))
        }
      )
    }
  )
})


# ---- Server: db_status output ---------------------------------------

test_that("db_status renders the 'unavailable' card when DB is not configured", {
  skip_if_not_installed("shiny")

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

test_that("db_status renders the 'no zone' card when DB has zero zones", {
  skip_if_not_installed("shiny")

  testthat::with_mocked_bindings(
    get_monitoring_db_connection  = function(...) "fake-con",
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
          expect_true(grepl("border-info", html))
          expect_true(grepl("Aucune zone", html))
        }
      )
    }
  )
})

test_that("db_status renders the 'connected' card with zone count", {
  skip_if_not_installed("shiny")

  fake_zones <- data.frame(id = 1L:2L, name = c("A", "B"),
                           stringsAsFactors = FALSE)

  testthat::with_mocked_bindings(
    get_monitoring_db_connection  = function(...) "fake-con",
    list_monitoring_zones         = function(con) fake_zones,
    close_monitoring_db_connection = function(con) invisible(TRUE),
    {
      shiny::testServer(
        nemetonshiny:::mod_monitoring_server,
        args = list(app_state = make_fake_app_state()),
        {
          html <- paste(as.character(output$db_status), collapse = "")
          expect_true(grepl("border-success", html))
          expect_true(grepl("2 zone", html))
        }
      )
    }
  )
})


# ---- Server: ingestion click handler (phase 2) ----------------------

# A fake ExtendedTask that records every invoke() call. status()
# always returns "initial" so the button observer in the module
# treats it as idle.
make_fake_ingest_task <- function() {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  list(
    invoke = function(...) {
      state$calls[[length(state$calls) + 1L]] <- list(...)
      invisible(NULL)
    },
    result = function() NULL,
    status = function() "initial",
    .calls = function() state$calls
  )
}

fake_zones_df <- function() {
  data.frame(id = c(1L, 2L), name = c("Foret", "Massif"),
             stringsAsFactors = FALSE)
}

test_that("input$run with no zone selected fires a validation notification", {
  skip_if_not_installed("shiny")

  fake_task <- make_fake_ingest_task()

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

test_that("input$run with no band selected fires a validation notification", {
  skip_if_not_installed("shiny")

  fake_task <- make_fake_ingest_task()

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
            bands      = character(0),
            date_range = c(as.Date("2025-06-01"), as.Date("2025-06-30")),
            run        = 1L
          )
          expect_length(fake_task$.calls(), 0L)
        }
      )
    }
  )
})

test_that("input$run with valid inputs invokes the ingest task", {
  skip_if_not_installed("shiny")

  fake_task <- make_fake_ingest_task()

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
          expect_equal(calls[[1]]$bands,   c("NDVI", "NBR"))
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
    fake_task <- make_fake_ingest_task()

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

  fake_task <- make_fake_ingest_task()

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

test_that("server returns ingest_task in its returned list", {
  skip_if_not_installed("shiny")

  fake_task <- make_fake_ingest_task()

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
          expect_identical(session$returned$ingest_task, fake_task)
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
make_fake_fordead_task <- function() {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  list(
    invoke = function(...) {
      state$calls[[length(state$calls) + 1L]] <- list(...)
      invisible(NULL)
    },
    result = function() NULL,
    status = function() "initial",
    .calls = function() state$calls
  )
}

test_that("validity reactive is NULL outside health mode", {
  skip_if_not_installed("shiny")

  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function() "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_ingest_task(),
    run_fordead_async              = function() make_fake_fordead_task(),
    validity_check_for_zone        = function(con, zone_id, units = NULL) {
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
    get_monitoring_db_connection   = function() "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_ingest_task(),
    run_fordead_async              = function() make_fake_fordead_task(),
    validity_check_for_zone        = function(con, zone_id, units = NULL)
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
    get_monitoring_db_connection   = function() "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_ingest_task(),
    run_fordead_async              = function() fake_task,
    validity_check_for_zone        = function(con, zone_id, units = NULL) {
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
    get_monitoring_db_connection   = function() "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_ingest_task(),
    run_fordead_async              = function() fake_task,
    validity_check_for_zone        = function(con, zone_id, units = NULL) {
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
    get_monitoring_db_connection   = function() "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_ingest_task(),
    run_fordead_async              = function() fake_task,
    validity_check_for_zone        = function(con, zone_id, units = NULL) {
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
    get_monitoring_db_connection   = function() "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_ingest_task(),
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
    get_monitoring_db_connection   = function() "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_ingest_task(),
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
    get_monitoring_db_connection   = function() "fake-con",
    list_monitoring_zones          = function(con) fake_zones_df(),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() make_fake_ingest_task(),
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

test_that("register click invokes register_project_as_zone and persists zone_id in app_state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        proj <- nemetonshiny:::create_project(name = "Test Forest")
        plots <- sf::st_sf(
          plot_id  = c("P1", "P2", "P3"),
          type     = "Base",
          geometry = sf::st_sfc(
            sf::st_point(c(900100, 6500100)),
            sf::st_point(c(900200, 6500200)),
            sf::st_point(c(900300, 6500300)),
            crs = 2154
          )
        )
        nemetonshiny:::save_samples(proj$id, plots)

        captured_project <- NULL
        testthat::with_mocked_bindings(
          get_monitoring_db_connection   = function() "fake-con",
          list_monitoring_zones          = function(con) fake_zones_df(),
          close_monitoring_db_connection = function(con) invisible(TRUE),
          run_ingestion_async            = function() make_fake_ingest_task(),
          run_fordead_async              = function() make_fake_fordead_task(),
          register_project_as_zone       = function(con, project) {
            captured_project <<- project
            list(zone_id = 99L, zone_name = "Test Forest",
                 n_plots = 3L, was_existing = FALSE)
          },
          {
            state <- make_fake_app_state_with_project(proj$id, proj$path)
            shiny::testServer(
              nemetonshiny:::mod_monitoring_server,
              args = list(app_state = state),
              {
                session$setInputs(register = 1L)
                expect_false(is.null(captured_project))
                expect_equal(captured_project$id, proj$id)
                expect_equal(state$current_project$metadata$monitoring_zone_id,
                             99L)
              }
            )
          }
        )
      }
    )
  })
})

test_that("register click flags 'already registered' when helper returns was_existing=TRUE", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

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
          get_monitoring_db_connection   = function() "fake-con",
          list_monitoring_zones          = function(con) fake_zones_df(),
          close_monitoring_db_connection = function(con) invisible(TRUE),
          run_ingestion_async            = function() make_fake_ingest_task(),
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
})

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
          get_monitoring_db_connection   = function() "fake-con",
          list_monitoring_zones          = function(con) fake_zones_df(),
          close_monitoring_db_connection = function(con) invisible(TRUE),
          run_ingestion_async            = function() make_fake_ingest_task(),
          run_fordead_async              = function() make_fake_fordead_task(),
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
          }
        )
      }
    )
  })

  expect_equal(captured_selected, "2")
})


# ---- Server: phase 3 — obs_pixel reactive --------------------------
# E6.b phase 3 wires the obs_pixel reactive (consumed by the Carte
# pixel sub-tab and its placette marker overlay) to
# nemeton::read_obs_pixel(). We can't reach into a `reactive({...})`
# defined inside moduleServer() from the outside, so the tests below
# assert the *side effects* we care about: how many times
# nemeton::read_obs_pixel was called, and with which arguments.
#
# The plot_filter selectizeInput refresh assertions that used to live
# here were dropped in v0.31.0 along with the "Séries par placette"
# sub-tab — the multi-trace placette view is now reachable spatially
# via the Carte pixel marker click.

.skip_if_no_read_obs_pixel <- function() {
  testthat::skip_if_not_installed("nemeton")
  if (!"read_obs_pixel" %in% getNamespaceExports("nemeton")) {
    testthat::skip("nemeton::read_obs_pixel not exported (need >= v0.21.11)")
  }
}

test_that("obs_pixel reactive does not fire in health mode", {
  skip_if_not_installed("shiny")
  .skip_if_no_read_obs_pixel()

  call_count <- 0L
  testthat::with_mocked_bindings(
    get_monitoring_db_connection  = function(...) "fake-con",
    list_monitoring_zones         = function(con) data.frame(
      id = 1L, name = "Z", stringsAsFactors = FALSE),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    {
      testthat::with_mocked_bindings(
        read_obs_pixel = function(...) {
          call_count <<- call_count + 1L
          data.frame(plot_id = character(0), obs_date = as.Date(character(0)),
                     band = character(0), value = numeric(0),
                     cloud_pct = numeric(0), source = character(0),
                     scene_id = character(0))
        },
        .package = "nemeton",
        {
          shiny::testServer(
            nemetonshiny:::mod_monitoring_server,
            args = list(app_state = make_fake_app_state()),
            {
              session$setInputs(
                mode       = "health",
                zone_id    = "1",
                bands      = c("NDVI", "NBR"),
                date_range = c(as.Date("2025-01-01"),
                               as.Date("2025-12-31"))
              )
              session$flushReact()
              # Health mode short-circuits → reader never invoked.
              expect_equal(call_count, 0L)
            }
          )
        }
      )
    }
  )
})

test_that("obs_pixel reactive forwards filters to read_obs_pixel", {
  skip_if_not_installed("shiny")
  .skip_if_no_read_obs_pixel()

  fake_obs <- data.frame(
    plot_id   = c("P01", "P01", "P02", "P02"),
    obs_date  = as.Date(c("2025-06-10", "2025-06-25",
                          "2025-06-10", "2025-06-25")),
    band      = c("NDVI", "NDVI", "NDVI", "NDVI"),
    value     = c(0.70, 0.65, 0.72, 0.68),
    cloud_pct = c(5, 8, 5, 8),
    source    = "fake",
    scene_id  = c("S1", "S2", "S1", "S2"),
    stringsAsFactors = FALSE
  )

  captured <- list()

  testthat::with_mocked_bindings(
    get_monitoring_db_connection  = function(...) "fake-con",
    list_monitoring_zones         = function(con) data.frame(
      id = 7L, name = "Z7", stringsAsFactors = FALSE),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    {
      testthat::with_mocked_bindings(
        read_obs_pixel = function(con, zone_id, plot_ids = NULL,
                                  bands = NULL,
                                  date_from = NULL, date_to = NULL) {
          captured$zone_id   <<- zone_id
          captured$bands     <<- bands
          captured$date_from <<- date_from
          captured$date_to   <<- date_to
          fake_obs
        },
        .package = "nemeton",
        {
          shiny::testServer(
            nemetonshiny:::mod_monitoring_server,
            args = list(app_state = make_fake_app_state()),
            {
              session$setInputs(
                mode       = "quick",
                zone_id    = "7",
                bands      = c("NDVI", "NBR"),
                date_range = c(as.Date("2025-01-01"),
                               as.Date("2025-12-31"))
              )
              session$flushReact()
            }
          )
        }
      )
    }
  )

  expect_equal(captured$zone_id, 7L)
  expect_equal(captured$bands, c("NDVI", "NBR"))
  expect_equal(captured$date_from, as.Date("2025-01-01"))
  expect_equal(captured$date_to,   as.Date("2025-12-31"))
})

test_that("obs_pixel reactive returns NULL when zone, bands or dates missing", {
  skip_if_not_installed("shiny")
  .skip_if_no_read_obs_pixel()

  call_count <- 0L
  testthat::with_mocked_bindings(
    get_monitoring_db_connection  = function(...) "fake-con",
    list_monitoring_zones         = function(con) data.frame(
      id = 1L, name = "Z", stringsAsFactors = FALSE),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    {
      testthat::with_mocked_bindings(
        read_obs_pixel = function(...) {
          call_count <<- call_count + 1L
          data.frame()
        },
        .package = "nemeton",
        {
          shiny::testServer(
            nemetonshiny:::mod_monitoring_server,
            args = list(app_state = make_fake_app_state()),
            {
              # Quick mode but bands empty → no fire.
              session$setInputs(
                mode       = "quick", zone_id = "1",
                bands      = character(0),
                date_range = c(as.Date("2025-01-01"),
                               as.Date("2025-12-31"))
              )
              session$flushReact()
              expect_equal(call_count, 0L)

              # Quick mode but no zone → no fire.
              session$setInputs(
                mode       = "quick", zone_id = "",
                bands      = "NDVI",
                date_range = c(as.Date("2025-01-01"),
                               as.Date("2025-12-31"))
              )
              session$flushReact()
              expect_equal(call_count, 0L)

              # Quick mode but invalid date_range → no fire.
              session$setInputs(
                mode       = "quick", zone_id = "1",
                bands      = "NDVI",
                date_range = c(as.Date(NA), as.Date(NA))
              )
              session$flushReact()
              expect_equal(call_count, 0L)
            }
          )
        }
      )
    }
  )
})


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

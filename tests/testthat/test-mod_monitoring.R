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
    get_monitoring_db_connection = function() NULL,
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
    get_monitoring_db_connection  = function() "fake-con",
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
    get_monitoring_db_connection  = function() NULL,
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
    get_monitoring_db_connection  = function() "fake-con",
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
    get_monitoring_db_connection  = function() "fake-con",
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

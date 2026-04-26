# Tests for service_monitoring_db.R (E6.b phase 1 — DB connection adapter).
#
# Pure-unit coverage for URL building and graceful-failure paths. Live
# DB integration tests live in nemeton's test-monitoring.R + test-db.R;
# this file only exercises the adapter logic that is specific to the
# Shiny app.

# Convenience: clear every env var that the resolver consults.
clear_monitoring_db_envvars <- function() {
  withr::local_envvar(c(
    NEMETON_DB_URL          = NA,
    NEMETON_DB_HOST         = NA,
    NEMETON_DB_PORT         = NA,
    NEMETON_DB_NAME         = NA,
    NEMETON_DB_USER         = NA,
    NEMETON_DB_PASSWORD     = NA,
    POSTGRESQL_ADDON_HOST     = NA,
    POSTGRESQL_ADDON_PORT     = NA,
    POSTGRESQL_ADDON_DB       = NA,
    POSTGRESQL_ADDON_USER     = NA,
    POSTGRESQL_ADDON_PASSWORD = NA
  ), .local_envir = parent.frame())
}


# ---- .build_monitoring_db_url ---------------------------------------

test_that(".build_monitoring_db_url returns '' when no env vars set", {
  clear_monitoring_db_envvars()
  expect_equal(nemetonshiny:::.build_monitoring_db_url(), "")
})

test_that(".build_monitoring_db_url returns '' when host but creds missing", {
  clear_monitoring_db_envvars()
  withr::local_envvar(c(NEMETON_DB_HOST = "127.0.0.1"))
  expect_equal(nemetonshiny:::.build_monitoring_db_url(), "")
})

test_that(".build_monitoring_db_url builds a complete URL from NEMETON_DB_*", {
  clear_monitoring_db_envvars()
  withr::local_envvar(c(
    NEMETON_DB_HOST     = "127.0.0.1",
    NEMETON_DB_PORT     = "5432",
    NEMETON_DB_NAME     = "nemeton",
    NEMETON_DB_USER     = "neo",
    NEMETON_DB_PASSWORD = "morpheus"
  ))
  url <- nemetonshiny:::.build_monitoring_db_url()
  expect_equal(url, "postgresql://neo:morpheus@127.0.0.1:5432/nemeton")
})

test_that(".build_monitoring_db_url prefers POSTGRESQL_ADDON_* over NEMETON_DB_*", {
  clear_monitoring_db_envvars()
  withr::local_envvar(c(
    NEMETON_DB_HOST           = "fallback.example",
    NEMETON_DB_NAME           = "fallback_db",
    NEMETON_DB_USER           = "fallback_user",
    NEMETON_DB_PASSWORD       = "fallback_pwd",
    POSTGRESQL_ADDON_HOST     = "primary.example",
    POSTGRESQL_ADDON_PORT     = "6543",
    POSTGRESQL_ADDON_DB       = "primary_db",
    POSTGRESQL_ADDON_USER     = "primary_user",
    POSTGRESQL_ADDON_PASSWORD = "primary_pwd"
  ))
  url <- nemetonshiny:::.build_monitoring_db_url()
  expect_match(url, "primary\\.example:6543/primary_db")
  expect_match(url, "primary_user:primary_pwd@")
  expect_false(grepl("fallback", url))
})

test_that(".build_monitoring_db_url URL-encodes special chars in credentials", {
  clear_monitoring_db_envvars()
  withr::local_envvar(c(
    NEMETON_DB_HOST     = "127.0.0.1",
    NEMETON_DB_NAME     = "nemeton",
    NEMETON_DB_USER     = "user@with:colon",
    NEMETON_DB_PASSWORD = "p@ss/w?rd"
  ))
  url <- nemetonshiny:::.build_monitoring_db_url()
  # Reserved chars must be percent-encoded so the URL parser won't
  # mistake them for delimiters.
  expect_false(grepl("@with:colon", url))
  expect_false(grepl("p@ss/w?rd", url))
  expect_match(url, "user%40with%3Acolon")
  expect_match(url, "p%40ss%2Fw%3Frd")
})


# ---- get_monitoring_db_connection -----------------------------------

test_that("get_monitoring_db_connection returns NULL when nothing is configured", {
  skip_if_not_installed("nemeton")
  clear_monitoring_db_envvars()
  expect_null(get_monitoring_db_connection())
})

test_that("get_monitoring_db_connection passes NEMETON_DB_URL straight to nemeton", {
  skip_if_not_installed("nemeton")
  clear_monitoring_db_envvars()
  withr::local_envvar(c(
    NEMETON_DB_URL = "postgresql://u:p@h:5432/d"
  ))
  captured_url <- NULL
  testthat::with_mocked_bindings(
    db_connect = function(url) { captured_url <<- url; "fake-con" },
    .package = "nemeton",
    {
      out <- get_monitoring_db_connection()
      expect_equal(out, "fake-con")
      expect_equal(captured_url, "postgresql://u:p@h:5432/d")
    }
  )
})

test_that("get_monitoring_db_connection swallows nemeton::db_connect errors and warns", {
  skip_if_not_installed("nemeton")
  clear_monitoring_db_envvars()
  withr::local_envvar(c(NEMETON_DB_URL = "postgresql://u:p@h:5432/d"))
  testthat::with_mocked_bindings(
    db_connect = function(url) stop("connection refused"),
    .package = "nemeton",
    {
      expect_warning(out <- get_monitoring_db_connection(),
                     "connection refused")
      expect_null(out)
    }
  )
})


# ---- list_monitoring_zones ------------------------------------------

test_that("list_monitoring_zones returns an empty df when con is NULL", {
  out <- list_monitoring_zones(NULL)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("id", "name"))
})

test_that("list_monitoring_zones returns empty df + warning on query failure", {
  fake_con <- structure(list(), class = "FakeCon")
  testthat::with_mocked_bindings(
    dbGetQuery = function(conn, ...) stop("schema not migrated"),
    .package = "DBI",
    {
      expect_warning(out <- list_monitoring_zones(fake_con),
                     "schema not migrated")
      expect_equal(nrow(out), 0L)
    }
  )
})

test_that("list_monitoring_zones forwards the DB result on success", {
  fake_con <- structure(list(), class = "FakeCon")
  testthat::with_mocked_bindings(
    dbGetQuery = function(conn, ...) {
      data.frame(id = c(1L, 2L), name = c("Aerodrome", "Bois"),
                 stringsAsFactors = FALSE)
    },
    .package = "DBI",
    {
      out <- list_monitoring_zones(fake_con)
      expect_equal(nrow(out), 2L)
      expect_equal(out$name, c("Aerodrome", "Bois"))
    }
  )
})


# ---- close_monitoring_db_connection ---------------------------------

test_that("close_monitoring_db_connection is a no-op for NULL", {
  expect_silent(out <- close_monitoring_db_connection(NULL))
  expect_true(out)
})

test_that("close_monitoring_db_connection delegates to nemeton::db_disconnect", {
  skip_if_not_installed("nemeton")
  closed <- FALSE
  testthat::with_mocked_bindings(
    db_disconnect = function(con) { closed <<- TRUE; invisible(TRUE) },
    .package = "nemeton",
    {
      close_monitoring_db_connection("fake-con")
      expect_true(closed)
    }
  )
})

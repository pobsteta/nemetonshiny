# tests/testthat/test-service_db.R
# Tests pour le module de connexion PostgreSQL/PostGIS

# All three env-var families have to be cleared together — leaving
# NEMETON_DB_URL set (e.g. inherited from the dev .Renviron) would mask
# the "not configured" branch and silently connect to the dev DB.
.unset_db_envs <- c(
  NEMETON_DB_URL = "",
  POSTGRESQL_ADDON_HOST = "",
  NEMETON_DB_HOST = ""
)

test_that("is_db_configured returns FALSE without env vars", {
  withr::with_envvar(.unset_db_envs, {
    expect_false(is_db_configured())
  })
})

test_that("is_db_configured returns TRUE with Clever Cloud vars", {
  withr::with_envvar(
    c(NEMETON_DB_URL = "",
      POSTGRESQL_ADDON_HOST = "test.clever-cloud.com"), {
    expect_true(is_db_configured())
  })
})

test_that("is_db_configured returns TRUE with NEMETON_DB vars", {
  withr::with_envvar(
    c(NEMETON_DB_URL = "",
      POSTGRESQL_ADDON_HOST = "",
      NEMETON_DB_HOST = "localhost"), {
    expect_true(is_db_configured())
  })
})

test_that("is_db_configured returns TRUE with NEMETON_DB_URL", {
  withr::with_envvar(
    c(NEMETON_DB_URL = "postgresql://u:p@127.0.0.1:5432/d",
      POSTGRESQL_ADDON_HOST = "",
      NEMETON_DB_HOST = ""), {
    expect_true(is_db_configured())
  })
})

test_that("NEMETON_DB_URL takes precedence over POSTGRESQL_ADDON_*", {
  withr::with_envvar(
    c(NEMETON_DB_URL = "postgresql://u:p@127.0.0.1:5432/local",
      POSTGRESQL_ADDON_HOST = "remote.clever-cloud.com",
      POSTGRESQL_ADDON_PORT = "50013",
      POSTGRESQL_ADDON_DB   = "remote_db"), {
    expect_equal(db_target_label(), "local@127.0.0.1:5432")
  })
})

test_that("db_target_label uses prefer sslmode for localhost URL", {
  withr::with_envvar(
    c(NEMETON_DB_URL = "postgresql://u:p@127.0.0.1:5432/local",
      POSTGRESQL_ADDON_HOST = "",
      NEMETON_DB_HOST = ""), {
    cfg <- .resolve_db_config()
    expect_equal(cfg$source, "url")
    expect_equal(cfg$sslmode, "prefer")
  })
})

test_that("get_db_connection returns NULL when not configured", {
  withr::with_envvar(.unset_db_envs, {
    expect_null(get_db_connection())
  })
})

test_that("schema.sql file exists", {
  schema_file <- system.file("sql", "schema.sql", package = "nemetonshiny")
  expect_true(nzchar(schema_file))
  sql <- readLines(schema_file)
  expect_true(any(grepl("CREATE TABLE", sql)))
  expect_true(any(grepl("nemeton.projects", sql)))
  expect_true(any(grepl("nemeton.parcels", sql)))
  expect_true(any(grepl("nemeton.indicators", sql)))
  expect_true(any(grepl("nemeton.users", sql)))
  expect_true(any(grepl("postgis", sql)))
})

test_that("db_sync_project returns FALSE when not configured", {
  withr::with_envvar(.unset_db_envs, {
    expect_false(suppressWarnings(db_sync_project("fake_project")))
  })
})

test_that(".resolve_db_connect_timeout defaults to 8 and honours the env override", {
  withr::with_envvar(c(NEMETON_DB_CONNECT_TIMEOUT = NA), {
    expect_equal(nemetonshiny:::.resolve_db_connect_timeout(), 8L)
  })
  withr::with_envvar(c(NEMETON_DB_CONNECT_TIMEOUT = "3"), {
    expect_equal(nemetonshiny:::.resolve_db_connect_timeout(), 3L)
  })
  # Below the libpq minimum (2) or unparseable → fall back to the default.
  withr::with_envvar(c(NEMETON_DB_CONNECT_TIMEOUT = "0"), {
    expect_equal(nemetonshiny:::.resolve_db_connect_timeout(), 8L)
  })
  withr::with_envvar(c(NEMETON_DB_CONNECT_TIMEOUT = "abc"), {
    expect_equal(nemetonshiny:::.resolve_db_connect_timeout(), 8L)
  })
})

# ---------------------------------------------------------------------------
# db_save_action_plan — instantané versionné du plan (miroir db_save_regeneration)
# ---------------------------------------------------------------------------

test_that("db_save_action_plan : état versionné, une ligne par action", {
  plan <- list(horizon_annees = 20L, actions = list(
    list(id = "a1", ug_id = "U1", type = "eclaircie"),
    list(id = "a2", ug_id = "U2", type = "plantation")))
  rec <- new.env(); rec$sqls <- character(0)
  con <- structure(list(), class = "FakeConn")
  testthat::local_mocked_bindings(
    dbExecute = function(conn, statement, ...) { rec$sqls <- c(rec$sqls, statement); 1L },
    dbGetQuery = function(conn, statement, ...) data.frame(v = 2L),
    .package = "DBI")

  ver <- nemetonshiny:::db_save_action_plan(con, "p1", plan)
  expect_equal(ver, 2L)
  expect_true(any(grepl("CREATE TABLE IF NOT EXISTS nemeton.action_plan_states",
                        rec$sqls, fixed = TRUE)))
  expect_equal(sum(grepl("INSERT INTO nemeton.action_plan_states", rec$sqls,
                         fixed = TRUE)), 2L)
})

test_that("db_save_action_plan : version 1 sur historique vide", {
  plan <- list(horizon_annees = 10L, actions = list(list(id = "a1", ug_id = "U1")))
  con <- structure(list(), class = "FakeConn")
  testthat::local_mocked_bindings(
    dbExecute = function(conn, statement, ...) 1L,
    dbGetQuery = function(conn, statement, ...) data.frame(v = NA_integer_),
    .package = "DBI")
  expect_equal(nemetonshiny:::db_save_action_plan(con, "p1", plan), 1L)
})

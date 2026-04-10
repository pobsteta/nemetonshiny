# tests/testthat/test-service_db.R
# Tests pour le module de connexion PostgreSQL/PostGIS

test_that("is_db_configured returns FALSE without env vars", {
  withr::with_envvar(
    c(POSTGRESQL_ADDON_HOST = "", NEMETON_DB_HOST = ""), {
    expect_false(is_db_configured())
  })
})

test_that("is_db_configured returns TRUE with Clever Cloud vars", {
  withr::with_envvar(
    c(POSTGRESQL_ADDON_HOST = "test.clever-cloud.com"), {
    expect_true(is_db_configured())
  })
})

test_that("is_db_configured returns TRUE with NEMETON_DB vars", {
  withr::with_envvar(
    c(POSTGRESQL_ADDON_HOST = "", NEMETON_DB_HOST = "localhost"), {
    expect_true(is_db_configured())
  })
})

test_that("get_db_connection returns NULL when not configured", {
  withr::with_envvar(
    c(POSTGRESQL_ADDON_HOST = "", NEMETON_DB_HOST = ""), {
    expect_null(get_db_connection())
  })
})

test_that("schema.sql file exists", {
  schema_file <- system.file("sql", "schema.sql", package = "nemetonShiny")
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
  withr::with_envvar(
    c(POSTGRESQL_ADDON_HOST = "", NEMETON_DB_HOST = ""), {
    expect_false(suppressWarnings(db_sync_project("fake_project")))
  })
})

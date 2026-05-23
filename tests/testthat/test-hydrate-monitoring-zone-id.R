# Tests for hydrate_monitoring_zone_id() — spec 011 hydration helper.
#
# These tests run fully offline. The DB connection passed to the helper
# is a sentinel string (never touched, since `find_zone_by_project()`
# is mocked) and `update_project_metadata()` is stubbed too so the
# helper does not try to touch the filesystem.

test_that("hydrate_monitoring_zone_id is a no-op when id is already set", {
  project <- list(
    id = "20260520_212017_btfe",
    metadata = list(
      name = "villards",
      monitoring_zone_id = 1L
    )
  )

  called <- FALSE
  testthat::local_mocked_bindings(
    find_zone_by_project = function(con, project_uuid) {
      called <<- TRUE
      42L
    },
    .package = "nemeton"
  )
  testthat::local_mocked_bindings(
    update_project_metadata = function(...) {
      called <<- TRUE
      TRUE
    }
  )

  out <- nemetonshiny:::hydrate_monitoring_zone_id(project, con = "fake-con")

  expect_identical(out$metadata$monitoring_zone_id, 1L)
  expect_false(called)  # neither lookup nor persist was attempted
})


test_that("hydrate_monitoring_zone_id fills the id when DB has a bound zone", {
  project <- list(
    id = "20260520_212017_btfe",
    metadata = list(name = "villards")  # no monitoring_zone_id
  )

  seen_uuid    <- NULL
  persisted_id <- NULL

  testthat::local_mocked_bindings(
    find_zone_by_project = function(con, project_uuid) {
      seen_uuid <<- project_uuid
      7L
    },
    .package = "nemeton"
  )
  testthat::local_mocked_bindings(
    update_project_metadata = function(project_id, updates, project_path = NULL) {
      persisted_id <<- updates$monitoring_zone_id
      TRUE
    }
  )

  out <- nemetonshiny:::hydrate_monitoring_zone_id(project, con = "fake-con")

  expect_identical(seen_uuid, "20260520_212017_btfe")
  expect_identical(out$metadata$monitoring_zone_id, 7L)
  expect_identical(persisted_id, 7L)
})


test_that("hydrate_monitoring_zone_id leaves metadata alone when no zone is bound", {
  project <- list(
    id = "20260520_212017_btfe",
    metadata = list(name = "villards")
  )

  persist_called <- FALSE
  testthat::local_mocked_bindings(
    find_zone_by_project = function(con, project_uuid) integer(0),
    .package = "nemeton"
  )
  testthat::local_mocked_bindings(
    update_project_metadata = function(...) {
      persist_called <<- TRUE
      TRUE
    }
  )

  out <- nemetonshiny:::hydrate_monitoring_zone_id(project, con = "fake-con")

  expect_null(out$metadata$monitoring_zone_id)
  expect_false(persist_called)
})


test_that("hydrate_monitoring_zone_id no-ops when con is NULL", {
  project <- list(
    id = "20260520_212017_btfe",
    metadata = list(name = "villards")
  )

  called <- FALSE
  testthat::local_mocked_bindings(
    find_zone_by_project = function(con, project_uuid) {
      called <<- TRUE
      9L
    },
    .package = "nemeton"
  )

  out <- nemetonshiny:::hydrate_monitoring_zone_id(project, con = NULL)

  expect_null(out$metadata$monitoring_zone_id)
  expect_false(called)
})


test_that("hydrate_monitoring_zone_id swallows lookup errors", {
  project <- list(
    id = "20260520_212017_btfe",
    metadata = list(name = "villards")
  )

  testthat::local_mocked_bindings(
    find_zone_by_project = function(con, project_uuid) {
      stop("connection closed")
    },
    .package = "nemeton"
  )

  expect_no_error(
    out <- nemetonshiny:::hydrate_monitoring_zone_id(project, con = "fake-con")
  )
  expect_null(out$metadata$monitoring_zone_id)
})

# Tests for the project edit lock wrapper (R/service_lock.R).
#
# Three layers:
#  1. No-DB degradation      — get_db_connection() -> NULL, no core call, editable.
#  2. Wrapper coercion       — core result -> app-facing shape (mocked nemeton).
#  3. Real lifecycle         — against a live Postgres, skipped when unreachable.

# --- 1. No database configured: locking is a graceful no-op -----------------

test_that("no-DB path: wrappers degrade to an editable, lock-free project", {
  testthat::local_mocked_bindings(get_db_connection = function(...) NULL)

  # acquire returns the sentinel; the acquisition observer treats is.null()/sentinel
  # as "no DB -> editable", so what matters here is that no core call is attempted
  # and the sentinel is recognisable.
  expect_true(lock_no_db(lock_acquire("P1", "a@x.fr")))
  # heartbeat: we "hold" trivially so the heartbeat observer keeps quiet.
  expect_true(lock_heartbeat("P1", "a@x.fr"))
  # release: nothing to release.
  expect_false(lock_release("P1", "a@x.fr"))
  # status: no holder to report.
  expect_null(lock_status("P1"))
})

test_that("no-DB path never touches the core lock API", {
  testthat::local_mocked_bindings(get_db_connection = function(...) NULL)
  called <- FALSE
  testthat::local_mocked_bindings(
    project_lock_acquire = function(...) { called <<- TRUE; list(ok = TRUE) },
    .package = "nemeton"
  )
  lock_acquire("P1", "a@x.fr")
  expect_false(called)
})

# --- 2. Wrapper coercion with a mocked core + fake connection ---------------

# A fake non-NULL connection so `.with_lock_con` follows the DB branch; both
# get_db_connection and close_db_connection are mocked so no real socket opens.
local_fake_con <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    get_db_connection = function(...) structure(list(), class = "FakeCon"),
    close_db_connection = function(con) invisible(NULL),
    .env = env
  )
}

test_that("lock_heartbeat coerces core boolean to a scalar TRUE/FALSE", {
  local_fake_con()
  testthat::local_mocked_bindings(
    project_lock_heartbeat = function(con, pid, hid) TRUE, .package = "nemeton")
  expect_true(lock_heartbeat("P1", "a@x.fr"))

  testthat::local_mocked_bindings(
    project_lock_heartbeat = function(con, pid, hid) FALSE, .package = "nemeton")
  expect_false(lock_heartbeat("P1", "a@x.fr"))

  # A degenerate core return (NULL) must not propagate as truthy.
  testthat::local_mocked_bindings(
    project_lock_heartbeat = function(con, pid, hid) NULL, .package = "nemeton")
  expect_false(lock_heartbeat("P1", "a@x.fr"))
})

test_that("lock_acquire and lock_status forward the core result verbatim", {
  local_fake_con()
  acq <- list(ok = FALSE, holder_id = "b@x.fr", holder_label = "Bob", stolen = FALSE)
  testthat::local_mocked_bindings(
    project_lock_acquire = function(con, pid, hid, label) acq,
    project_lock_status  = function(con, pid) list(holder_label = "Bob"),
    .package = "nemeton")
  expect_identical(lock_acquire("P1", "a@x.fr"), acq)
  expect_identical(lock_status("P1")$holder_label, "Bob")
})

test_that("the per-call connection is always closed, even on core error", {
  closed <- FALSE
  testthat::local_mocked_bindings(
    get_db_connection = function(...) structure(list(), class = "FakeCon"),
    close_db_connection = function(con) { closed <<- TRUE; invisible(NULL) })
  testthat::local_mocked_bindings(
    project_lock_acquire = function(...) stop("boom"), .package = "nemeton")
  expect_error(lock_acquire("P1", "a@x.fr"), "boom")
  expect_true(closed)   # on.exit ran despite the error
})

# --- project_is_readonly ----------------------------------------------------

test_that("project_is_readonly reads the single readonly flag defensively", {
  expect_true(project_is_readonly(list(readonly = TRUE)))
  expect_false(project_is_readonly(list(readonly = FALSE)))
  expect_false(project_is_readonly(list()))          # field absent -> editable
  expect_false(project_is_readonly(list(readonly = NA)))
})

# --- 3. Real lifecycle against a live Postgres ------------------------------

test_that("full lock lifecycle behaves against a live database", {
  con <- tryCatch(get_db_connection(check_postgis = FALSE), error = function(e) NULL)
  testthat::skip_if(is.null(con), "no reachable database")
  on.exit(try(close_db_connection(con), silent = TRUE), add = TRUE)

  pid <- paste0("LOCKTEST_", as.integer(Sys.time()), "_", sample.int(1e6, 1))
  on.exit(try(lock_release(pid, "alice@x.fr"), silent = TRUE), add = TRUE)

  # Alice takes a free lock.
  a <- lock_acquire(pid, "alice@x.fr", "Alice")
  expect_true(isTRUE(a$ok))

  # Bob is excluded and sees who holds it.
  b <- lock_acquire(pid, "bob@x.fr", "Bob")
  expect_false(isTRUE(b$ok))
  expect_identical(b$holder_label, "Alice")

  # Alice re-acquires (re-entrant), Bob's heartbeat is a no-op.
  expect_true(isTRUE(lock_acquire(pid, "alice@x.fr", "Alice")$ok))
  expect_true(lock_heartbeat(pid, "alice@x.fr"))
  expect_false(lock_heartbeat(pid, "bob@x.fr"))

  # Bob cannot release Alice's lock; Alice can.
  expect_false(isTRUE(lock_release(pid, "bob@x.fr")))
  expect_identical(lock_status(pid)$holder_label, "Alice")
  expect_true(isTRUE(lock_release(pid, "alice@x.fr")))

  # Freed: Bob can now take it.
  expect_null(lock_status(pid))
  expect_true(isTRUE(lock_acquire(pid, "bob@x.fr", "Bob")$ok))
  lock_release(pid, "bob@x.fr")
})

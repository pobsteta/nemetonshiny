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

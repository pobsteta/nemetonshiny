# Battement du verrou hors de la boucle Shiny (audit 1.0)

test_that(".lock_heartbeat_run renews, re-acquires, or reports the loss", {
  local_mocked_bindings(lock_heartbeat = function(pid, hid) TRUE)
  expect_true(.lock_heartbeat_run("P", "h")$ok)
  local_mocked_bindings(lock_heartbeat = function(pid, hid) FALSE,
                        lock_acquire = function(pid, hid, label) list(ok = TRUE))
  expect_true(.lock_heartbeat_run("P", "h")$ok)
  local_mocked_bindings(lock_acquire = function(pid, hid, label)
    list(ok = FALSE, holder_label = "Autre"))
  r <- .lock_heartbeat_run("P", "h")
  expect_false(r$ok)
  expect_identical(r$info$holder_label, "Autre")
  expect_identical(r$pid, "P")
})

test_that(".lock_heartbeat_async resolves after the call (inline mode)", {
  withr::local_options(nemetonshiny.lock_inline = TRUE)
  local_mocked_bindings(lock_heartbeat = function(pid, hid) TRUE)
  vu <- NULL
  promises::then(.lock_heartbeat_async("P", "h"), function(r) vu <<- r)
  expect_null(vu)
  for (i in 1:10) later::run_now(0.05)
  expect_true(vu$ok)
})

test_that(".python_timeout_motif detects inactivity and overall overrun", {
  t0 <- as.POSIXct("2026-10-05 10:00:00", tz = "UTC")
  expect_null(.python_timeout_motif(t0, t0 + 50, t0 + 60, 1800, 3600))
  expect_match(.python_timeout_motif(t0, t0, t0 + 2000, 1800, 3600 * 12), "no output")
  expect_match(.python_timeout_motif(t0, t0 + 4000, t0 + 4001, 1800, 3600), "running")
  expect_null(.python_timeout_motif(t0, t0, t0 + 1e6, Inf, NA))
})

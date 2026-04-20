# Test fixtures and helper functions — automatically loaded before tests.

# Note: the disable-CHM env var for tests is set in setup-chm.R
# (not here), because teardown_env() is only available after the
# test framework has initialised — helper-*.R files are sourced
# before that, so using withr + teardown_env at this point
# would error at load time.

# --- Neutralise async primitives for the whole test session ---
#
# mod_home uses `shiny::ExtendedTask` + `promises::future_promise()`
# which, in a test, spawns real `future::multisession` workers. Those
# workers (and the `later::later` polling loop that waits for their
# result) outlive the testServer session and cause the next test_file
# to hang inside an unrelated reactive output (seen on
# "test-07mod_project.R :: date_display output renders correctly").
#
# Override `promises::future_promise` with a synchronous resolver so
# no background workers are ever started during tests.
suppressWarnings({
  unlockBinding("future_promise", asNamespace("promises"))
  assign("future_promise",
         function(expr, ...) promises::promise_resolve(NULL),
         envir = asNamespace("promises"))
})

# Run each testServer call inside its own `later` loop so any pending
# callbacks (mod_home's poll_fn, shiny's flush ticks) are discarded
# with the loop when the test finishes, instead of leaking onto the
# global queue and firing during an unrelated later test.
#
# shiny::testServer captures `expr` via substitute(), so we have to
# forward the call unevaluated (match.call + eval) instead of going
# through do.call — otherwise the test body sees `expr` as an already-
# evaluated value and `output`/`input`/`session` are not bound.
.original_testServer <- shiny::testServer
.patched_testServer <- function(...) {
  mc <- match.call()
  mc[[1]] <- .original_testServer
  caller <- parent.frame()
  later::with_temp_loop({
    eval(mc, envir = caller)
  })
}
suppressWarnings({
  unlockBinding("testServer", asNamespace("shiny"))
  assign("testServer", .patched_testServer, envir = asNamespace("shiny"))
})

#' Create a test sf object (simple square polygons in Lambert 93)
#'
#' @param crs CRS for the test object (default: EPSG:2154 — Lambert 93).
#' @param n_features Number of features to create.
#' @return sf object with `id`, `area` and polygon geometry.
create_test_units <- function(crs = 2154, n_features = 3) {
  polys <- lapply(seq_len(n_features), function(i) {
    xmin <- 566450 + (i - 1) * 120
    ymin <- 6615150 + (i - 1) * 120
    xmax <- xmin + 100
    ymax <- ymin + 100

    sf::st_polygon(list(matrix(
      c(
        xmin, ymin,
        xmax, ymin,
        xmax, ymax,
        xmin, ymax,
        xmin, ymin
      ),
      ncol = 2, byrow = TRUE
    )))
  })

  sfc <- sf::st_sfc(polys, crs = crs)
  sf::st_sf(
    id = sprintf("unit_%03d", seq_len(n_features)),
    area = rep(10000, n_features),
    geometry = sfc
  )
}

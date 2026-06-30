# test-opencanopy-python.R — Open-Canopy reticulate env resolution
#   (isolated subprocess so open_canopy + FORDEAD reticulate envs coexist)

test_that(".resolve_opencanopy_python honours the explicit option", {
  tf <- withr::local_tempfile(fileext = ".py"); file.create(tf)
  withr::local_options(nemetonshiny.opencanopy_python = tf)
  expect_identical(nemetonshiny:::.resolve_opencanopy_python(), tf)
})

test_that(".resolve_opencanopy_python honours OPENCANOPY_PYTHON", {
  tf <- withr::local_tempfile(fileext = ".py"); file.create(tf)
  withr::local_options(nemetonshiny.opencanopy_python = NULL)
  withr::local_envvar(OPENCANOPY_PYTHON = tf)
  expect_identical(nemetonshiny:::.resolve_opencanopy_python(), tf)
})

test_that(".resolve_opencanopy_python ignores a non-existent override", {
  withr::local_options(nemetonshiny.opencanopy_python = "/no/such/python")
  withr::local_envvar(OPENCANOPY_PYTHON = "")
  expect_false(
    identical(nemetonshiny:::.resolve_opencanopy_python(), "/no/such/python"))
})


# --- subprocess progress forwarding (.chm_forward_line) --------------------

test_that(".chm_forward_line replays tagged CHM events to the callback", {
  seen <- NULL
  cb <- function(ev) seen <<- ev
  nemetonshiny:::.chm_forward_line(
    '__CHM_EV__{"type":"tile","prefix":"ortho","idx":2,"n_tiles":5}', cb)
  expect_identical(seen$type, "tile")
  expect_identical(seen$idx, 2L)
  expect_identical(seen$n_tiles, 5L)
})

test_that(".chm_forward_line ignores plain lines (no callback)", {
  called <- FALSE
  cb <- function(ev) called <<- TRUE
  nemetonshiny:::.chm_forward_line(">>> ÉTAPE 3/5 : Configuration Python", cb)
  nemetonshiny:::.chm_forward_line("", cb)
  expect_false(called)
})

test_that(".chm_forward_line tolerates malformed event JSON", {
  called <- FALSE
  cb <- function(ev) called <<- TRUE
  expect_silent(nemetonshiny:::.chm_forward_line("__CHM_EV__{not json", cb))
  expect_false(called)
})

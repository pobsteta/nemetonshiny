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

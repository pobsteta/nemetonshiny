# Tests for the Theia / DATA TERRA integration service (service_theia.R)
# Pure orchestration: no business logic is tested here, only the
# readiness detection, key persistence and metadata helpers.

test_that("theia_api_key_configured detects env vars", {
  withr::with_envvar(
    c(TLD_ACCESS_KEY = "abc", TLD_SECRET_KEY = "def"),
    expect_true(nemetonshiny:::theia_api_key_configured())
  )
  withr::with_envvar(
    c(TLD_ACCESS_KEY = "", TLD_SECRET_KEY = ""),
    {
      # No env var: result depends only on the (absent) apikey file.
      res <- nemetonshiny:::theia_api_key_configured()
      expect_type(res, "logical")
    }
  )
})


test_that("theia_status aggregates python and key readiness", {
  st <- nemetonshiny:::theia_status()
  expect_named(st, c("python_ok", "python_reason", "key_ok", "ready"))
  expect_type(st$python_ok, "logical")
  expect_type(st$key_ok, "logical")
  expect_identical(st$ready, isTRUE(st$python_ok) && isTRUE(st$key_ok))
})


test_that("theia_status_key maps a status to an i18n key", {
  expect_identical(
    nemetonshiny:::theia_status_key(list(ready = TRUE)),
    "theia_status_ready"
  )
  expect_identical(
    nemetonshiny:::theia_status_key(list(
      ready = FALSE, python_ok = FALSE, python_reason = "reticulate"
    )),
    "theia_error_reticulate"
  )
  expect_identical(
    nemetonshiny:::theia_status_key(list(
      ready = FALSE, python_ok = FALSE, python_reason = "python_modules"
    )),
    "theia_error_python_modules"
  )
  expect_identical(
    nemetonshiny:::theia_status_key(list(ready = FALSE, python_ok = TRUE)),
    "theia_error_no_key"
  )
})


test_that("theia_compute_year honours the option and defaults to N-1", {
  withr::with_options(
    list(nemetonshiny.theia_year = 2023L),
    expect_identical(nemetonshiny:::theia_compute_year(), 2023L)
  )
  withr::with_options(
    list(nemetonshiny.theia_year = NULL),
    {
      yr <- nemetonshiny:::theia_compute_year()
      expect_identical(yr, as.integer(format(Sys.Date(), "%Y")) - 1L)
    }
  )
})


test_that("theia_save_api_key rejects empty keys", {
  expect_false(nemetonshiny:::theia_save_api_key("", "secret"))
  expect_false(nemetonshiny:::theia_save_api_key("access", ""))
})


test_that("theia_save_api_key writes the apikey file and sets env vars", {
  tmp_home <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp_home, TLD_ACCESS_KEY = "", TLD_SECRET_KEY = ""))

  ok <- nemetonshiny:::theia_save_api_key("my-access", "my-secret")
  expect_true(ok)
  expect_identical(Sys.getenv("TLD_ACCESS_KEY"), "my-access")
  expect_identical(Sys.getenv("TLD_SECRET_KEY"), "my-secret")

  apikey_path <- file.path(tmp_home, ".config", "teledetection", ".apikey")
  expect_true(file.exists(apikey_path))
  parsed <- jsonlite::read_json(apikey_path)
  expect_identical(parsed[["access-key"]], "my-access")
  expect_identical(parsed[["secret-key"]], "my-secret")
})


test_that("theia_chm_enabled honours the CHM opt-outs", {
  withr::with_options(
    list(nemetonshiny.chm = "none"),
    expect_false(nemetonshiny:::theia_chm_enabled())
  )
  withr::with_options(
    list(nemetonshiny.chm_source = "opencanopy"),
    expect_false(nemetonshiny:::theia_chm_enabled())
  )
})


test_that("download_chm_theia stops cleanly when Theia is not ready", {
  skip_if_not_installed("sf")
  units <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(900000, 6500000), c(900500, 6500000),
        c(900500, 6500500), c(900000, 6500500), c(900000, 6500000)
      ))),
      crs = 2154
    )
  )
  testthat::local_mocked_bindings(
    theia_status = function() list(ready = FALSE, python_ok = FALSE,
                                   python_reason = "reticulate", key_ok = FALSE)
  )
  err <- tryCatch(
    nemetonshiny:::download_chm_theia(units),
    theia_unavailable = function(e) e
  )
  expect_s3_class(err, "theia_unavailable")
  expect_identical(err$status_key, "theia_error_reticulate")
})


test_that("download_theia_layers returns empty list when Theia not ready", {
  skip_if_not_installed("sf")
  testthat::local_mocked_bindings(
    theia_status = function() list(ready = FALSE)
  )
  units <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_point(c(2.3, 48.8)), crs = 4326)
  )
  expect_identical(nemetonshiny:::download_theia_layers(units), list())
})

test_that("theia_save_api_key locks the .apikey file down to 0600 (POSIX)", {
  skip_on_os("windows")  # POSIX bits don't map to Windows ACLs
  tmp <- withr::local_tempdir()
  withr::local_envvar(c(HOME = tmp,
                        TLD_ACCESS_KEY = NA, TLD_SECRET_KEY = NA))
  ok <- nemetonshiny:::theia_save_api_key("access-xyz", "secret-xyz")
  expect_true(ok)
  apikey_path <- nemetonshiny:::.theia_apikey_path()
  expect_true(file.exists(apikey_path))
  # file.info()$mode returns an octal "octmode" object — compare to 0600.
  expect_equal(as.integer(file.info(apikey_path)$mode), as.integer(as.octmode("0600")))
})

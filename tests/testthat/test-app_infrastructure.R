# Tests for nemetonApp Infrastructure
# Phase 1: Core app setup and configuration

test_that("run_app function exists and is exported", {
  expect_true(exists("run_app", where = asNamespace("nemetonShiny"), inherits = FALSE))
})

test_that("detect_system_language returns valid language code", {
  # Access internal function
  detect_lang <- nemetonShiny:::detect_system_language

  # Should return "fr" or "en"
  result <- detect_lang()
  expect_true(result %in% c("fr", "en"))
  expect_type(result, "character")
  expect_length(result, 1)
})

test_that("get_default_project_dir returns valid path", {
  get_dir <- nemetonShiny:::get_default_project_dir

  result <- get_dir()
  expect_type(result, "character")
  expect_length(result, 1)
  expect_true(grepl("nemeton", result))
  expect_true(grepl("projects", result))
})

test_that("APP_CONFIG contains required keys", {
  config <- nemetonShiny:::APP_CONFIG

  required_keys <- c(
    "app_name",
    "app_version",
    "max_parcels",
    "max_project_name_length",
    "api_timeout",
    "max_retries",
    "project_states",
    "default_crs"
  )

  for (key in required_keys) {
    expect_true(
      key %in% names(config),
      info = paste("Missing config key:", key)
    )
  }
})

test_that("APP_CONFIG max_parcels default is 30", {
  config <- nemetonShiny:::APP_CONFIG
  expect_equal(config$max_parcels, 30L)
})

test_that("run_app exposes a max_parcels argument defaulting to 30", {
  fmls <- formals(nemetonShiny::run_app)
  expect_true("max_parcels" %in% names(fmls))
  expect_equal(eval(fmls$max_parcels), 30L)
})

test_that("run_app max_parcels override is surfaced through get_app_config", {
  # Simulate what run_app() does without actually launching the Shiny app
  withr::local_options(nemeton.app_options = list(
    language = "fr",
    project_dir = tempdir(),
    max_parcels = 75L
  ))
  expect_equal(nemetonShiny:::get_app_config("max_parcels"), 75L)
})

test_that("run_app validates max_parcels", {
  expect_error(nemetonShiny::run_app(max_parcels = 0), "positive integer")
  expect_error(nemetonShiny::run_app(max_parcels = -5), "positive integer")
  expect_error(nemetonShiny::run_app(max_parcels = 1.5), "positive integer")
  expect_error(nemetonShiny::run_app(max_parcels = "20"), "positive integer")
  expect_error(nemetonShiny::run_app(max_parcels = c(10, 20)), "positive integer")
})

test_that("APP_CONFIG project_states are valid", {
  config <- nemetonShiny:::APP_CONFIG
  expected_states <- c("draft", "downloading", "computing", "completed", "error")
  expect_equal(config$project_states, expected_states)
})

test_that("INDICATOR_FAMILIES contains 12 families", {
  families <- nemetonShiny:::INDICATOR_FAMILIES
  expect_length(families, 12)

  expected_codes <- c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")
  expect_equal(names(families), expected_codes)
})

test_that("Each indicator family has required fields", {
  families <- nemetonShiny:::INDICATOR_FAMILIES

  required_fields <- c("code", "name_fr", "name_en", "icon", "color", "indicators")

  for (code in names(families)) {
    family <- families[[code]]
    for (field in required_fields) {
      expect_true(
        field %in% names(family),
        info = paste("Family", code, "missing field:", field)
      )
    }
  }
})

test_that("get_family_codes returns all 12 codes", {
  codes <- nemetonShiny:::get_family_codes()
  expect_length(codes, 12)
  expect_equal(codes, c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N"))
})

test_that("get_family_config returns valid config or NULL", {
  get_config <- nemetonShiny:::get_family_config

  # Valid code
  carbon <- get_config("C")
  expect_type(carbon, "list")
  expect_equal(carbon$code, "C")
  expect_equal(carbon$name_en, "Carbon & Vitality")

  # Invalid code
  invalid <- get_config("X")
  expect_null(invalid)
})

test_that("get_all_indicator_codes returns 31 indicators", {
  codes <- nemetonShiny:::get_all_indicator_codes()
  expect_type(codes, "character")
  expect_length(codes, 31)

  # Check some known indicators
  expect_true("C1" %in% codes)
  expect_true("B3" %in% codes)
  expect_true("R4" %in% codes)
})

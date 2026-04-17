# Tests for nemetonApp Internationalization (i18n)
# R/utils_i18n.R - Translation system for the Shiny application

# ==============================================================================
# Tests for TRANSLATIONS dictionary
# ==============================================================================

test_that("TRANSLATIONS contains required keys", {
  translations <- nemetonshiny:::TRANSLATIONS

  required_keys <- c(
    "app_title",
    "tab_selection",
    "tab_synthesis",
    "tab_families",
    "compute_button",
    "download_pdf",
    "download_gpkg",
    "help"
  )

  for (key in required_keys) {
    expect_true(
      key %in% names(translations),
      info = paste("Missing translation key:", key)
    )
  }
})

test_that("All translations have both fr and en versions", {
  translations <- nemetonshiny:::TRANSLATIONS

  for (key in names(translations)) {
    translation <- translations[[key]]

    expect_true(
      "fr" %in% names(translation),
      info = paste("Missing French translation for:", key)
    )
    expect_true(
      "en" %in% names(translation),
      info = paste("Missing English translation for:", key)
    )
  }
})

test_that("TRANSLATIONS contains navigation keys", {
  translations <- nemetonshiny:::TRANSLATIONS

  nav_keys <- c("tab_selection", "tab_synthesis", "tab_families")

  for (key in nav_keys) {
    expect_true(key %in% names(translations))
    expect_true(is.character(translations[[key]]$fr))
    expect_true(is.character(translations[[key]]$en))
  }
})

test_that("TRANSLATIONS contains all indicator keys", {
  translations <- nemetonshiny:::TRANSLATIONS

  indicator_keys <- c(
    "indicator_C1", "indicator_C2",
    "indicator_B1", "indicator_B2", "indicator_B3",
    "indicator_W1", "indicator_W2", "indicator_W3",
    "indicator_A1", "indicator_A2",
    "indicator_F1", "indicator_F2",
    "indicator_L1", "indicator_L2",
    "indicator_T1", "indicator_T2",
    "indicator_R1", "indicator_R2", "indicator_R3", "indicator_R4",
    "indicator_S1", "indicator_S2", "indicator_S3",
    "indicator_P1", "indicator_P2", "indicator_P3",
    "indicator_E1", "indicator_E2",
    "indicator_N1", "indicator_N2", "indicator_N3"
  )

  for (key in indicator_keys) {
    expect_true(
      key %in% names(translations),
      info = paste("Missing indicator key:", key)
    )
  }
})

test_that("TRANSLATIONS contains error message keys", {
  translations <- nemetonshiny:::TRANSLATIONS

  error_keys <- c(
    "error", "error_api_cadastre", "error_no_parcels",
    "error_invalid_postal", "error_computation",
    "error_no_internet", "error_loading_communes"
  )

  for (key in error_keys) {
    expect_true(
      key %in% names(translations),
      info = paste("Missing error key:", key)
    )
  }
})

test_that("TRANSLATIONS contains project status keys", {
  translations <- nemetonshiny:::TRANSLATIONS

  status_keys <- c(
    "status_draft", "status_downloading", "status_computing",
    "status_completed", "status_error", "status_unknown"
  )

  for (key in status_keys) {
    expect_true(
      key %in% names(translations),
      info = paste("Missing status key:", key)
    )
  }
})

test_that("TRANSLATIONS contains data source keys", {
  translations <- nemetonshiny:::TRANSLATIONS

  source_keys <- c(
    "source_ndvi", "source_dem", "source_forest_cover",
    "source_protected_areas", "source_water_network",
    "source_wetlands", "source_roads", "source_buildings",
    "source_bdforet", "source_lidar_mnh"
  )

  for (key in source_keys) {
    expect_true(
      key %in% names(translations),
      info = paste("Missing source key:", key)
    )
  }
})

# ==============================================================================
# Tests for get_i18n()
# ==============================================================================

test_that("get_i18n returns a translator object", {
  i18n <- nemetonshiny:::get_i18n("fr")

  expect_s3_class(i18n, "nemeton_i18n")
  expect_equal(i18n$language, "fr")
  expect_type(i18n$t, "closure")
  expect_type(i18n$keys, "closure")
  expect_type(i18n$has, "closure")
})

test_that("get_i18n defaults to valid language", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  expect_equal(i18n_fr$language, "fr")
  expect_equal(i18n_en$language, "en")
})

test_that("get_i18n rejects invalid language", {
  expect_error(nemetonshiny:::get_i18n("de"))
  expect_error(nemetonshiny:::get_i18n("es"))
  expect_error(nemetonshiny:::get_i18n("invalid"))
  expect_error(nemetonshiny:::get_i18n(""))
})

test_that("get_i18n creates proper class structure", {
  i18n <- nemetonshiny:::get_i18n("en")

  expect_true("nemeton_i18n" %in% class(i18n))
  expect_true("list" %in% class(i18n))
  expect_true(is.list(i18n))
})

# ==============================================================================
# Tests for translator t() function
# ==============================================================================

test_that("Translator t() function returns correct translations", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  # French

  expect_equal(i18n_fr$t("tab_selection"), "Sélection")
  expect_equal(i18n_fr$t("help"), "Aide")

  # English
  expect_equal(i18n_en$t("tab_selection"), "Selection")
  expect_equal(i18n_en$t("help"), "Help")
})

test_that("Translator t() returns key for missing translations", {
  i18n <- nemetonshiny:::get_i18n("fr")

  # Non-existent key should return the key itself
  expect_warning(
    result <- i18n$t("nonexistent_key"),
    "not found"
  )
  expect_equal(result, "nonexistent_key")
})

test_that("Translator t() supports string interpolation", {
  i18n <- nemetonshiny:::get_i18n("fr")

  # Test with placeholder
  result <- i18n$t("computing_indicator", indicator = "C1")
  expect_true(grepl("C1", result))
})

test_that("Translator t() handles multiple placeholders", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  # Test alert_many_na which has {n} and {total} placeholders
  result_fr <- i18n_fr$t("alert_many_na", n = 5, total = 20)
  expect_true(grepl("5", result_fr))
  expect_true(grepl("20", result_fr))

  result_en <- i18n_en$t("alert_many_na", n = 5, total = 20)
  expect_true(grepl("5", result_en))
  expect_true(grepl("20", result_en))
})

test_that("Translator t() returns character type", {
  i18n <- nemetonshiny:::get_i18n("fr")

  result <- i18n$t("app_title")
  expect_type(result, "character")
  expect_length(result, 1)
})

test_that("Translator t() handles downloading_source with placeholder", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  result_fr <- i18n_fr$t("downloading_source", source = "DEM")
  expect_true(grepl("DEM", result_fr))
  expect_true(grepl("T\u00e9l\u00e9chargement", result_fr))

  result_en <- i18n_en$t("downloading_source", source = "DEM")
  expect_true(grepl("DEM", result_en))
  expect_true(grepl("Downloading", result_en))
})

test_that("Translator t() handles ai_no_api_key with placeholder", {
  i18n <- nemetonshiny:::get_i18n("en")

  result <- i18n$t("ai_no_api_key", key_var = "OPENAI_API_KEY")
  expect_true(grepl("OPENAI_API_KEY", result))
  expect_true(grepl("API key", result))
})

test_that("Translator t() handles missing_indicator with reason placeholder", {
  i18n <- nemetonshiny:::get_i18n("en")

  result <- i18n$t("missing_indicator", reason = "No LiDAR data")
  expect_true(grepl("No LiDAR data", result))
})

# ==============================================================================
# Tests for translator keys() function
# ==============================================================================

test_that("Translator keys() returns all keys", {
  i18n <- nemetonshiny:::get_i18n("fr")
  keys <- i18n$keys()

  expect_type(keys, "character")
  expect_true(length(keys) > 50)  # We have many translations
  expect_true("app_title" %in% keys)
})

test_that("Translator keys() returns same keys for both languages", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  keys_fr <- i18n_fr$keys()
  keys_en <- i18n_en$keys()

  expect_equal(sort(keys_fr), sort(keys_en))
})

# ==============================================================================
# Tests for translator has() function
# ==============================================================================

test_that("Translator has() checks key existence", {
  i18n <- nemetonshiny:::get_i18n("fr")

  expect_true(i18n$has("app_title"))
  expect_true(i18n$has("help"))
  expect_false(i18n$has("nonexistent_key"))
})

test_that("Translator has() returns boolean", {
  i18n <- nemetonshiny:::get_i18n("fr")

  expect_type(i18n$has("app_title"), "logical")
  expect_type(i18n$has("missing"), "logical")
})

# ==============================================================================
# Tests for get_available_languages()
# ==============================================================================

test_that("get_available_languages returns fr and en", {
  langs <- nemetonshiny:::get_available_languages()

  expect_equal(langs, c("fr", "en"))
})

test_that("get_available_languages returns character vector", {
  langs <- nemetonshiny:::get_available_languages()

  expect_type(langs, "character")
  expect_length(langs, 2)
})

# ==============================================================================
# Tests for print.nemeton_i18n()
# ==============================================================================

test_that("print.nemeton_i18n displays translator information", {
  i18n <- nemetonshiny:::get_i18n("fr")

  # Call the print method directly since it's not exported
  output <- capture.output(nemetonshiny:::print.nemeton_i18n(i18n))

  expect_true(any(grepl("nemeton i18n translator", output)))
  expect_true(any(grepl("fr", output)))
  expect_true(any(grepl("translation keys", output)))
})

test_that("print.nemeton_i18n returns object invisibly", {
  i18n <- nemetonshiny:::get_i18n("en")

  # Call the print method directly
  result <- capture.output(returned <- nemetonshiny:::print.nemeton_i18n(i18n))

  expect_identical(returned, i18n)
})

test_that("print.nemeton_i18n works for both languages", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  # Call the print method directly
  output_fr <- capture.output(nemetonshiny:::print.nemeton_i18n(i18n_fr))
  output_en <- capture.output(nemetonshiny:::print.nemeton_i18n(i18n_en))

  expect_true(any(grepl("\\[fr\\]", output_fr)))
  expect_true(any(grepl("\\[en\\]", output_en)))
})

# ==============================================================================
# Tests for translate_task_message()
# ==============================================================================

test_that("translate_task_message handles NULL and empty string", {
  i18n <- nemetonshiny:::get_i18n("fr")

  result_null <- nemetonshiny:::translate_task_message(NULL, i18n)
  expect_equal(result_null, "")

  result_empty <- nemetonshiny:::translate_task_message("", i18n)
  expect_equal(result_empty, "")
})

test_that("translate_task_message handles special task keywords", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  # Test download_start
  result_fr <- nemetonshiny:::translate_task_message("download_start", i18n_fr)
  expect_true(nchar(result_fr) > 0)

  result_en <- nemetonshiny:::translate_task_message("download_start", i18n_en)
  expect_true(nchar(result_en) > 0)

  # Test compute_start
  result <- nemetonshiny:::translate_task_message("compute_start", i18n_en)
  expect_true(nchar(result) > 0)

  # Test complete
  result <- nemetonshiny:::translate_task_message("complete", i18n_en)
  expect_true(nchar(result) > 0)

  # Test error
  result <- nemetonshiny:::translate_task_message("error", i18n_en)
  expect_true(nchar(result) > 0)

  # Test resuming
  result <- nemetonshiny:::translate_task_message("resuming", i18n_en)
  expect_true(nchar(result) > 0)
})

test_that("translate_task_message handles download_complete", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  result_fr <- nemetonshiny:::translate_task_message("download_complete", i18n_fr)
  expect_true(grepl("termin\u00e9", result_fr, ignore.case = TRUE))

  result_en <- nemetonshiny:::translate_task_message("download_complete", i18n_en)
  expect_true(grepl("complete", result_en, ignore.case = TRUE))
})

test_that("translate_task_message handles download:source_key format", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  # Test with source_ndvi
  result_fr <- nemetonshiny:::translate_task_message("download:source_ndvi", i18n_fr)
  expect_true(grepl("T\u00e9l\u00e9chargement", result_fr))

  result_en <- nemetonshiny:::translate_task_message("download:source_ndvi", i18n_en)
  expect_true(grepl("Downloading", result_en))

  # Test with source_dem
  result <- nemetonshiny:::translate_task_message("download:source_dem", i18n_en)
  expect_true(grepl("Downloading", result))
})

test_that("translate_task_message handles compute:indicator_key format", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  # Test with indicator key
  result_fr <- nemetonshiny:::translate_task_message("compute:indicateur_c1_biomasse", i18n_fr)
  expect_true(grepl("Calcul", result_fr))

  result_en <- nemetonshiny:::translate_task_message("compute:indicateur_c1_biomasse", i18n_en)
  expect_true(grepl("Computing", result_en))

  # Test with another indicator
  result <- nemetonshiny:::translate_task_message("compute:indicateur_w3_humidite", i18n_en)
  expect_true(grepl("Computing", result))
})

test_that("translate_task_message returns task as-is for unknown format", {
  i18n <- nemetonshiny:::get_i18n("fr")

  result <- nemetonshiny:::translate_task_message("unknown_task_format", i18n)
  expect_equal(result, "unknown_task_format")

  result2 <- nemetonshiny:::translate_task_message("some_random_message", i18n)
  expect_equal(result2, "some_random_message")
})

test_that("translate_task_message works with both languages", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  # Same task should produce different results for different languages
  result_fr <- nemetonshiny:::translate_task_message("download_start", i18n_fr)
  result_en <- nemetonshiny:::translate_task_message("download_start", i18n_en)

  # Both should have content
  expect_true(nchar(result_fr) > 0)
  expect_true(nchar(result_en) > 0)
})

# ==============================================================================
# Tests for export_translations_json()
# ==============================================================================

test_that("export_translations_json creates output directory if needed", {
  # Use a temporary directory
  temp_dir <- file.path(tempdir(), "test_i18n_export")
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }

  expect_false(dir.exists(temp_dir))

  nemetonshiny:::export_translations_json(temp_dir)

  expect_true(dir.exists(temp_dir))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("export_translations_json creates fr.json and en.json files", {
  temp_dir <- file.path(tempdir(), "test_i18n_export2")
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }

  nemetonshiny:::export_translations_json(temp_dir)

  expect_true(file.exists(file.path(temp_dir, "fr.json")))
  expect_true(file.exists(file.path(temp_dir, "en.json")))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("export_translations_json creates valid JSON files", {
  temp_dir <- file.path(tempdir(), "test_i18n_export3")
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }

  nemetonshiny:::export_translations_json(temp_dir)

  # Read and parse JSON files
  fr_content <- jsonlite::read_json(file.path(temp_dir, "fr.json"))
  en_content <- jsonlite::read_json(file.path(temp_dir, "en.json"))

  expect_type(fr_content, "list")
  expect_type(en_content, "list")

  # Check that they contain expected keys
  expect_true("app_title" %in% names(fr_content))
  expect_true("app_title" %in% names(en_content))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("export_translations_json returns NULL invisibly", {
  temp_dir <- file.path(tempdir(), "test_i18n_export4")
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }

  result <- nemetonshiny:::export_translations_json(temp_dir)

  expect_null(result)

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("export_translations_json works with existing directory", {
  temp_dir <- file.path(tempdir(), "test_i18n_export5")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

  expect_true(dir.exists(temp_dir))

  # Should not error when directory exists
  expect_no_error(nemetonshiny:::export_translations_json(temp_dir))

  expect_true(file.exists(file.path(temp_dir, "fr.json")))
  expect_true(file.exists(file.path(temp_dir, "en.json")))

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

# ==============================================================================
# Tests for family translations
# ==============================================================================

test_that("All 12 family names are translated", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  family_keys <- unname(nemetonshiny:::FAMILLE_NMT_MAP[c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")])

  for (key in family_keys) {
    expect_true(i18n_fr$has(key), info = paste("Missing FR:", key))
    expect_true(i18n_en$has(key), info = paste("Missing EN:", key))

    # Should not be empty
    expect_true(nchar(i18n_fr$t(key)) > 0, info = paste("Empty FR:", key))
    expect_true(nchar(i18n_en$t(key)) > 0, info = paste("Empty EN:", key))
  }
})

test_that("All 12 family descriptions are translated", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  family_desc_keys <- paste0(unname(nemetonshiny:::FAMILLE_NMT_MAP[c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")]), "_desc")

  for (key in family_desc_keys) {
    expect_true(i18n_fr$has(key), info = paste("Missing FR desc:", key))
    expect_true(i18n_en$has(key), info = paste("Missing EN desc:", key))
  }
})

# ==============================================================================
# Tests for tour and help translations
# ==============================================================================

test_that("Tour step translations exist", {
  i18n <- nemetonshiny:::get_i18n("fr")

  tour_keys <- c(
    "tour_search_title", "tour_search_desc",
    "tour_map_title", "tour_map_desc",
    "tour_project_title", "tour_project_desc",
    "tour_compute_title", "tour_compute_desc"
  )

  for (key in tour_keys) {
    expect_true(i18n$has(key), info = paste("Missing tour key:", key))
  }
})

test_that("Help step translations exist", {
  i18n <- nemetonshiny:::get_i18n("fr")

  help_keys <- c(
    "help_title", "help_intro", "help_steps_title",
    "help_step1", "help_step2", "help_step3",
    "help_step4", "help_step5"
  )

  for (key in help_keys) {
    expect_true(i18n$has(key), info = paste("Missing help key:", key))
  }
})

# ==============================================================================
# Tests for progress message translations
# ==============================================================================

test_that("Progress phase translations exist", {
  i18n <- nemetonshiny:::get_i18n("fr")

  phase_keys <- c(
    "phase_init", "phase_downloading",
    "phase_computing", "phase_complete"
  )

  for (key in phase_keys) {
    expect_true(i18n$has(key), info = paste("Missing phase key:", key))
  }
})

test_that("Task message translations exist", {
  i18n <- nemetonshiny:::get_i18n("fr")

  task_keys <- c(
    "task_download_start", "task_compute_start",
    "task_complete", "task_error", "task_resuming"
  )

  for (key in task_keys) {
    expect_true(i18n$has(key), info = paste("Missing task key:", key))
  }
})

# ==============================================================================
# Tests for expert profile translations
# ==============================================================================

test_that("Expert label translation exists", {
  i18n <- nemetonshiny:::get_i18n("fr")
  expect_true(i18n$has("expert_label"), info = "Missing expert_label key")

  i18n_en <- nemetonshiny:::get_i18n("en")
  expect_true(i18n_en$has("expert_label"), info = "Missing expert_label key (en)")
})

# ==============================================================================
# Tests for statistic translations
# ==============================================================================

test_that("Statistic translations exist", {
  i18n <- nemetonshiny:::get_i18n("fr")

  stat_keys <- c(
    "stat_min", "stat_max", "stat_mean",
    "stat_median", "stat_sd", "stat_n", "stat_na"
  )

  for (key in stat_keys) {
    expect_true(i18n$has(key), info = paste("Missing stat key:", key))
  }
})

# ==============================================================================
# Tests for JSON translation files
# ==============================================================================

test_that("JSON translation files exist", {
  fr_path <- system.file("app/i18n/fr.json", package = "nemetonshiny")
  en_path <- system.file("app/i18n/en.json", package = "nemetonshiny")

  # Skip if package not installed (during development)
  skip_if(fr_path == "")

  expect_true(file.exists(fr_path))
  expect_true(file.exists(en_path))
})

# ==============================================================================
# Tests for computation summary translations with sprintf format
# ==============================================================================

test_that("Computation summary translations work with sprintf", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  # Get the raw translation strings
  summary_fr <- i18n_fr$t("computation_summary")
  summary_en <- i18n_en$t("computation_summary")

  # These use sprintf format %d
  result_fr <- sprintf(summary_fr, 10, 15)
  result_en <- sprintf(summary_en, 10, 15)

  expect_true(grepl("10", result_fr))
  expect_true(grepl("15", result_fr))
  expect_true(grepl("10", result_en))
  expect_true(grepl("15", result_en))
})

test_that("Computation failed translations work with sprintf", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  failed_fr <- i18n_fr$t("computation_failed")
  failed_en <- i18n_en$t("computation_failed")

  result_fr <- sprintf(failed_fr, 3)
  result_en <- sprintf(failed_en, 3)

  expect_true(grepl("3", result_fr))
  expect_true(grepl("3", result_en))
})

test_that("and_n_more_errors translations work with sprintf", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")

  errors_fr <- i18n_fr$t("and_n_more_errors")
  errors_en <- i18n_en$t("and_n_more_errors")

  result_fr <- sprintf(errors_fr, 5)
  result_en <- sprintf(errors_en, 5)

  expect_true(grepl("5", result_fr))
  expect_true(grepl("5", result_en))
})

# ==============================================================================
# Tests for special characters in translations
# ==============================================================================

test_that("French translations contain proper accented characters", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")

  # app_title should have accented e
  app_title <- i18n_fr$t("app_title")
  expect_true(grepl("\u00e9", app_title))  # é character

  # department should have accented e
  department <- i18n_fr$t("department")
  expect_true(grepl("\u00e9", department))  # Département

  # synthesis tab should have accented e
  synthesis <- i18n_fr$t("tab_synthesis")
  expect_true(grepl("\u00e8", synthesis))  # Synthèse
})

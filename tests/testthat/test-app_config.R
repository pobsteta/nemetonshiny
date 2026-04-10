# Tests for nemetonApp Configuration
# R/app_config.R - Configuration constants and accessor functions

# ==============================================================================
# Tests for APP_CONFIG structure
# ==============================================================================

test_that("APP_CONFIG has expected keys", {
  config <- nemetonShiny:::APP_CONFIG

  expected_keys <- c(
    "app_name", "app_version",
    "app_title_fr", "app_title_en",
    "max_parcels", "max_project_name_length", "max_description_length",
    "api_timeout", "wfs_timeout", "computation_timeout",
    "max_retries", "retry_delay",
    "parallel_workers",
    "cache_format",
    "project_states",
    "default_crs",
    "llm_provider", "llm_models"
  )

  for (key in expected_keys) {
    expect_true(
      key %in% names(config),
      info = paste("Missing APP_CONFIG key:", key)
    )
  }
})

test_that("APP_CONFIG values have correct types", {
  config <- nemetonShiny:::APP_CONFIG

  expect_type(config$app_name, "character")
  expect_type(config$app_version, "character")
  expect_type(config$max_parcels, "integer")
  expect_type(config$default_crs, "integer")
  expect_type(config$max_retries, "integer")
  expect_type(config$project_states, "character")
  expect_type(config$llm_models, "list")
})

test_that("APP_CONFIG numeric values are reasonable", {
  config <- nemetonShiny:::APP_CONFIG

  expect_equal(config$max_parcels, 20L)
  expect_equal(config$default_crs, 2154L)
  expect_equal(config$max_retries, 3L)
  expect_true(config$api_timeout > 0)
  expect_true(config$wfs_timeout > 0)
  expect_true(config$computation_timeout > 0)
})

# ==============================================================================
# Tests for get_app_config()
# ==============================================================================

test_that("get_app_config returns correct values for known keys", {
  expect_equal(nemetonShiny:::get_app_config("max_parcels"), 20L)
  expect_equal(nemetonShiny:::get_app_config("default_crs"), 2154L)
  expect_equal(nemetonShiny:::get_app_config("max_retries"), 3L)
  expect_equal(nemetonShiny:::get_app_config("cache_format"), "parquet")
})

test_that("get_app_config returns app_name", {
  result <- nemetonShiny:::get_app_config("app_name")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("get_app_config returns default for unknown keys", {
  expect_null(nemetonShiny:::get_app_config("nonexistent_key"))
  expect_equal(nemetonShiny:::get_app_config("nonexistent_key", "fallback"), "fallback")
  expect_equal(nemetonShiny:::get_app_config("missing", 42), 42)
})

test_that("get_app_config returns NULL default when key missing", {
  result <- nemetonShiny:::get_app_config("this_key_does_not_exist")
  expect_null(result)
})

test_that("get_app_config returns project_states correctly", {
  states <- nemetonShiny:::get_app_config("project_states")
  expect_type(states, "character")
  expect_true(length(states) >= 4)
  expect_true("draft" %in% states)
  expect_true("completed" %in% states)
  expect_true("error" %in% states)
})

test_that("get_app_config returns llm_models as a list", {
  models <- nemetonShiny:::get_app_config("llm_models")
  expect_type(models, "list")
  expect_true(length(models) > 0)
  expect_true("mistral" %in% names(models))
})

# ==============================================================================
# Tests for INDICATOR_FAMILIES structure
# ==============================================================================

test_that("INDICATOR_FAMILIES has 12 families", {
  families <- nemetonShiny:::INDICATOR_FAMILIES
  expect_length(families, 12)
})

test_that("INDICATOR_FAMILIES has correct family codes as names", {
  families <- nemetonShiny:::INDICATOR_FAMILIES
  expected_codes <- c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")
  expect_equal(names(families), expected_codes)
})

test_that("Each family has required fields", {
  families <- nemetonShiny:::INDICATOR_FAMILIES
  required_fields <- c(
    "code", "name_fr", "name_en", "icon", "color",
    "indicators", "column_names",
    "indicator_labels", "indicator_tooltips"
  )

  for (fam_code in names(families)) {
    fam <- families[[fam_code]]
    for (field in required_fields) {
      expect_true(
        field %in% names(fam),
        info = paste("Family", fam_code, "missing field:", field)
      )
    }
  }
})

test_that("Each family code field matches its list name", {
  families <- nemetonShiny:::INDICATOR_FAMILIES

  for (fam_code in names(families)) {
    expect_equal(
      families[[fam_code]]$code, fam_code,
      info = paste("Family code mismatch for:", fam_code)
    )
  }
})

test_that("Family indicators and column_names have same length", {
  families <- nemetonShiny:::INDICATOR_FAMILIES

  for (fam_code in names(families)) {
    fam <- families[[fam_code]]
    expect_equal(
      length(fam$indicators), length(fam$column_names),
      info = paste("Indicator/column_names length mismatch for family:", fam_code)
    )
  }
})

test_that("Family indicator_labels keys match indicators", {
  families <- nemetonShiny:::INDICATOR_FAMILIES

  for (fam_code in names(families)) {
    fam <- families[[fam_code]]
    expect_equal(
      sort(names(fam$indicator_labels)), sort(fam$indicators),
      info = paste("indicator_labels keys mismatch for family:", fam_code)
    )
  }
})

test_that("Family indicator_tooltips keys match indicators", {
  families <- nemetonShiny:::INDICATOR_FAMILIES

  for (fam_code in names(families)) {
    fam <- families[[fam_code]]
    expect_equal(
      sort(names(fam$indicator_tooltips)), sort(fam$indicators),
      info = paste("indicator_tooltips keys mismatch for family:", fam_code)
    )
  }
})

test_that("Family labels have fr and en translations", {
  families <- nemetonShiny:::INDICATOR_FAMILIES

  for (fam_code in names(families)) {
    fam <- families[[fam_code]]
    for (ind in fam$indicators) {
      label <- fam$indicator_labels[[ind]]
      expect_true(
        "fr" %in% names(label),
        info = paste("Missing FR label for", ind, "in family", fam_code)
      )
      expect_true(
        "en" %in% names(label),
        info = paste("Missing EN label for", ind, "in family", fam_code)
      )
    }
  }
})

test_that("Family colors are valid hex colors", {
  families <- nemetonShiny:::INDICATOR_FAMILIES

  for (fam_code in names(families)) {
    color <- families[[fam_code]]$color
    expect_match(
      color, "^#[0-9A-Fa-f]{6}$",
      info = paste("Invalid hex color for family:", fam_code)
    )
  }
})

# ==============================================================================
# Tests for get_family_codes()
# ==============================================================================

test_that("get_family_codes returns all 12 family codes", {
  codes <- nemetonShiny:::get_family_codes()

  expect_type(codes, "character")
  expect_length(codes, 12)
  expect_equal(codes, c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N"))
})

# ==============================================================================
# Tests for get_family_config()
# ==============================================================================

test_that("get_family_config returns correct config for valid code", {
  config_c <- nemetonShiny:::get_family_config("C")

  expect_type(config_c, "list")
  expect_equal(config_c$code, "C")
  expect_true("name_fr" %in% names(config_c))
  expect_true("name_en" %in% names(config_c))
  expect_true("indicators" %in% names(config_c))
  expect_equal(config_c$indicators, c("C1", "C2"))
})

test_that("get_family_config returns config for each family", {
  codes <- c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")

  for (code in codes) {
    config <- nemetonShiny:::get_family_config(code)
    expect_true(
      is.list(config),
      info = paste("NULL config for family:", code)
    )
    expect_equal(config$code, code, info = paste("Code mismatch for family:", code))
  }
})

test_that("get_family_config returns NULL for invalid code", {
  expect_null(nemetonShiny:::get_family_config("Z"))
  expect_null(nemetonShiny:::get_family_config("X"))
  expect_null(nemetonShiny:::get_family_config(""))
  expect_null(nemetonShiny:::get_family_config("invalid"))
})

test_that("get_family_config is case-insensitive", {
  config_upper <- nemetonShiny:::get_family_config("C")
  config_lower <- nemetonShiny:::get_family_config("c")

  expect_equal(config_upper, config_lower)
  expect_equal(config_upper$code, "C")

  # Also test with other letters
  config_b <- nemetonShiny:::get_family_config("b")
  expect_equal(config_b$code, "B")

  config_w <- nemetonShiny:::get_family_config("w")
  expect_equal(config_w$code, "W")
})

# ==============================================================================
# Tests for get_all_indicator_codes()
# ==============================================================================

test_that("get_all_indicator_codes returns all 31 indicators", {
  codes <- nemetonShiny:::get_all_indicator_codes()

  expect_type(codes, "character")
  expect_length(codes, 31)
})

test_that("get_all_indicator_codes contains expected indicators", {
  codes <- nemetonShiny:::get_all_indicator_codes()

  # Check a selection of expected indicator codes
  expect_true("C1" %in% codes)
  expect_true("C2" %in% codes)
  expect_true("B1" %in% codes)
  expect_true("B2" %in% codes)
  expect_true("B3" %in% codes)
  expect_true("R1" %in% codes)
  expect_true("R4" %in% codes)
  expect_true("N3" %in% codes)
})

test_that("get_all_indicator_codes returns unnamed vector", {
  codes <- nemetonShiny:::get_all_indicator_codes()
  expect_null(names(codes))
})

test_that("get_all_indicator_codes has no duplicates", {
  codes <- nemetonShiny:::get_all_indicator_codes()
  expect_equal(length(codes), length(unique(codes)))
})

# ==============================================================================
# Tests for get_all_column_names()
# ==============================================================================

test_that("get_all_column_names returns same count as indicator codes", {
  columns <- nemetonShiny:::get_all_column_names()
  indicators <- nemetonShiny:::get_all_indicator_codes()

  expect_equal(length(columns), length(indicators))
})

test_that("get_all_column_names returns character vector", {
  columns <- nemetonShiny:::get_all_column_names()

  expect_type(columns, "character")
  expect_true(length(columns) > 0)
})

test_that("get_all_column_names contains expected column names", {
  columns <- nemetonShiny:::get_all_column_names()

  expect_true("indicateur_c1_biomasse" %in% columns)
  expect_true("indicateur_c2_ndvi" %in% columns)
  expect_true("indicateur_b1_protection" %in% columns)
  expect_true("indicateur_w1_reseau" %in% columns)
  expect_true("indicateur_r1_feu" %in% columns)
})

test_that("get_all_column_names has no duplicates", {
  columns <- nemetonShiny:::get_all_column_names()
  expect_equal(length(columns), length(unique(columns)))
})

# ==============================================================================
# Tests for get_column_family_map()
# ==============================================================================

test_that("get_column_family_map returns named character vector", {
  map <- nemetonShiny:::get_column_family_map()

  expect_type(map, "character")
  expect_true(length(names(map)) > 0)
})

test_that("get_column_family_map maps column names to family codes correctly", {
  map <- nemetonShiny:::get_column_family_map()

  # Long-form column name mappings
  expect_equal(unname(map["indicateur_c1_biomasse"]), "C")
  expect_equal(unname(map["indicateur_c2_ndvi"]), "C")
  expect_equal(unname(map["indicateur_b1_protection"]), "B")
  expect_equal(unname(map["indicateur_w1_reseau"]), "W")
  expect_equal(unname(map["indicateur_r1_feu"]), "R")
  expect_equal(unname(map["indicateur_p1_volume"]), "P")
})

test_that("get_column_family_map maps short indicator codes to family codes", {
  map <- nemetonShiny:::get_column_family_map()

  # Short indicator code mappings
  expect_equal(unname(map["C1"]), "C")
  expect_equal(unname(map["C2"]), "C")
  expect_equal(unname(map["B1"]), "B")
  expect_equal(unname(map["B2"]), "B")
  expect_equal(unname(map["B3"]), "B")
  expect_equal(unname(map["R4"]), "R")
  expect_equal(unname(map["N3"]), "N")
})

test_that("get_column_family_map contains both long and short forms", {
  map <- nemetonShiny:::get_column_family_map()

  # Should have 31 column names + 31 indicator codes = 62 entries
  expect_equal(length(map), 62)
})

test_that("get_column_family_map values are valid family codes", {
  map <- nemetonShiny:::get_column_family_map()
  valid_codes <- c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")

  for (val in unique(map)) {
    expect_true(
      val %in% valid_codes,
      info = paste("Invalid family code in map:", val)
    )
  }
})

# ==============================================================================
# Tests for DATA_SOURCES and get_data_source_config()
# ==============================================================================

test_that("DATA_SOURCES has expected top-level categories", {
  sources <- nemetonShiny:::DATA_SOURCES

  expected_categories <- c("rasters", "vectors", "point_clouds")

  for (cat in expected_categories) {
    expect_true(
      cat %in% names(sources),
      info = paste("Missing data source category:", cat)
    )
  }
})

test_that("DATA_SOURCES rasters contains expected sources", {
  rasters <- nemetonShiny:::DATA_SOURCES$rasters

  expect_type(rasters, "list")
  expect_true(length(rasters) > 0)

  # Each raster source should have name, type, source, required_for
  for (src_name in names(rasters)) {
    src <- rasters[[src_name]]
    expect_true(
      "name" %in% names(src),
      info = paste("Raster source", src_name, "missing field: name")
    )
    expect_true(
      "type" %in% names(src),
      info = paste("Raster source", src_name, "missing field: type")
    )
    expect_true(
      "source" %in% names(src),
      info = paste("Raster source", src_name, "missing field: source")
    )
    expect_true(
      "required_for" %in% names(src),
      info = paste("Raster source", src_name, "missing field: required_for")
    )
  }
})

test_that("DATA_SOURCES vectors contains expected sources", {
  vectors <- nemetonShiny:::DATA_SOURCES$vectors

  expect_type(vectors, "list")
  expect_true(length(vectors) > 0)

  # Each vector source should have name, type, source, required_for
  for (src_name in names(vectors)) {
    src <- vectors[[src_name]]
    expect_true(
      "name" %in% names(src),
      info = paste("Vector source", src_name, "missing field: name")
    )
    expect_true(
      "type" %in% names(src),
      info = paste("Vector source", src_name, "missing field: type")
    )
  }
})

test_that("get_data_source_config returns correct config for rasters", {
  rasters_config <- nemetonShiny:::get_data_source_config("rasters")

  expect_type(rasters_config, "list")
  expect_true(length(rasters_config) > 0)
})

test_that("get_data_source_config returns correct config for vectors", {
  vectors_config <- nemetonShiny:::get_data_source_config("vectors")

  expect_type(vectors_config, "list")
  expect_true(length(vectors_config) > 0)
})

test_that("get_data_source_config returns NULL for unknown source", {
  expect_null(nemetonShiny:::get_data_source_config("nonexistent"))
  expect_null(nemetonShiny:::get_data_source_config(""))
})

test_that("get_data_source_config returns point_clouds config", {
  pc_config <- nemetonShiny:::get_data_source_config("point_clouds")

  expect_type(pc_config, "list")
  expect_true(length(pc_config) > 0)
})

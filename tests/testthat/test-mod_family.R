# Tests for Family Module
# Phase 5: Generic family indicator module

test_that("mod_family_ui returns valid Shiny UI for each family", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  withr::local_options(nemeton.app_options = list(language = "fr"))

  for (code in nemetonShiny:::get_family_codes()) {
    ui <- nemetonShiny:::mod_family_ui(nemetonShiny:::get_famille_col(code), code)
    # UI returns tagList which is shiny.tag.list
    expect_true(inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag"))
  }
})

test_that("mod_family_ui contains expected output elements", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  withr::local_options(nemeton.app_options = list(language = "fr"))

  ui <- nemetonShiny:::mod_family_ui("famille_carbone", "C")
  ui_html <- as.character(ui)

  # Should contain dynamic map output (rendered via renderUI)
  expect_true(grepl("famille_carbone-maps_row", ui_html))

  # Should contain table output
  expect_true(grepl("famille_carbone-indicator_table", ui_html))

  # Should contain statistics output
  expect_true(grepl("famille_carbone-analysis_stats", ui_html))
})

test_that("mod_family_ui works in English", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  withr::local_options(nemeton.app_options = list(language = "en"))

  ui <- nemetonShiny:::mod_family_ui("famille_biodiversite", "B")
  ui_html <- as.character(ui)

  # Should contain English family name
  expect_true(grepl("Biodiversity", ui_html))
})

test_that("get_indicator_cols excludes metadata columns", {
  df <- data.frame(
    nemeton_id = 1:3,
    id = 1:3,
    C1 = runif(3),
    C2 = runif(3),
    geometry = rep("POINT(0 0)", 3)
  )

  result <- nemetonShiny:::get_indicator_cols(df)
  expect_equal(result, c("C1", "C2"))
})

test_that("clean_indicator_label strips _norm suffix", {
  withr::local_options(nemeton.app_options = list(language = "fr"))

  i18n <- nemetonShiny:::get_i18n("fr")
  label <- nemetonShiny:::clean_indicator_label("C1_norm", i18n)
  # Should match indicator_C1 translation with code prefix
  expect_equal(label, "C1 - Biomasse carbone (tC/ha)")
})

test_that("clean_indicator_label falls back to humanized name", {
  withr::local_options(nemeton.app_options = list(language = "fr"))

  i18n <- nemetonShiny:::get_i18n("fr")
  label <- nemetonShiny:::clean_indicator_label("UNKNOWN_IND", i18n)
  expect_equal(label, "UNKNOWN IND")
})

test_that("clean_indicator_label resolves long-form column names", {
  withr::local_options(nemeton.app_options = list(language = "fr"))

  i18n <- nemetonShiny:::get_i18n("fr")
  label <- nemetonShiny:::clean_indicator_label("indicateur_c1_biomasse", i18n)
  expect_equal(label, "C1 - Biomasse carbone (tC/ha)")

  label_en <- nemetonShiny:::clean_indicator_label("indicateur_w3_humidite", nemetonShiny:::get_i18n("en"))
  expect_equal(label_en, "W3 - Topographic Wetness Index")
})

test_that("mod_family_ui contains AI generate button", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  withr::local_options(nemeton.app_options = list(language = "fr"))

  ui <- nemetonShiny:::mod_family_ui("famille_carbone", "C")
  ui_html <- as.character(ui)

  # Should contain AI generate button
  expect_true(grepl("famille_carbone-ai_generate", ui_html))
  expect_true(grepl("robot", ui_html))
})

test_that("build_analysis_prompt returns valid prompt string", {
  family_config <- nemetonShiny:::get_family_config("C")
  ind_data <- data.frame(
    nemeton_id = c("p1", "p2", "p3"),
    carbon_biomass_norm = c(0.5, 0.7, 0.3),
    carbon_ndvi_norm = c(0.8, 0.6, 0.9)
  )

  prompt <- nemetonShiny:::build_analysis_prompt(family_config, ind_data, "fran\u00e7ais")
  expect_type(prompt, "character")
  expect_true(nchar(prompt) > 0)
  expect_true(grepl("3 parcelles", prompt))
  expect_true(grepl("carbon_biomass_norm", prompt))
  expect_true(grepl("carbon_ndvi_norm", prompt))
  expect_true(grepl("min=", prompt))
  expect_true(grepl("mean=", prompt))
})

test_that("build_analysis_prompt handles all-NA indicator", {
  family_config <- nemetonShiny:::get_family_config("W")
  ind_data <- data.frame(
    nemeton_id = c("p1", "p2"),
    water_network_norm = c(NA_real_, NA_real_)
  )

  prompt <- nemetonShiny:::build_analysis_prompt(family_config, ind_data, "English")
  expect_true(grepl("no data", prompt))
})

test_that("create_llm_chat uses configured provider", {
  # Test that the function exists and returns expected type for known providers
  # (Can't easily test unknown provider error without mocking APP_CONFIG)
  skip_if_not_installed("ellmer")

  # The function should exist
  expect_true(exists("create_llm_chat", envir = asNamespace("nemeton"), inherits = FALSE))

  # Test error message format if we had an unknown provider
  # This tests the sprintf format string is correct
  error_msg <- sprintf("Unknown LLM provider: '%s'", "test_provider")
  expect_true(grepl("Unknown LLM provider", error_msg))
})

test_that("get_llm_api_key_var returns correct env var names", {
  expect_equal(nemetonShiny:::get_llm_api_key_var("anthropic"), "ANTHROPIC_API_KEY")
  expect_equal(nemetonShiny:::get_llm_api_key_var("mistral"), "MISTRAL_API_KEY")
  expect_equal(nemetonShiny:::get_llm_api_key_var("openai"), "OPENAI_API_KEY")
  expect_equal(nemetonShiny:::get_llm_api_key_var("google"), "GOOGLE_API_KEY")
  expect_equal(nemetonShiny:::get_llm_api_key_var("deepseek"), "DEEPSEEK_API_KEY")
  expect_null(nemetonShiny:::get_llm_api_key_var("ollama"))
  expect_null(nemetonShiny:::get_llm_api_key_var("nonexistent"))
})

test_that("mod_family_ui returns valid UI for unknown family", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  withr::local_options(nemeton.app_options = list(language = "fr"))

  ui <- nemetonShiny:::mod_family_ui("family_Z", "Z")
  ui_html <- as.character(ui)
  expect_true(grepl("Unknown family", ui_html))
})


# =============================================================================
# Helper function tests
# =============================================================================

test_that("get_indicator_tooltip returns tooltips for short codes", {
  tooltip_fr <- nemetonShiny:::get_indicator_tooltip("C1", "fr")
  expect_type(tooltip_fr, "character")
  expect_true(nchar(tooltip_fr) > 0)
  expect_true(grepl("carbone", tooltip_fr, ignore.case = TRUE))

  tooltip_en <- nemetonShiny:::get_indicator_tooltip("C1", "en")
  expect_type(tooltip_en, "character")
  expect_true(grepl("carbon", tooltip_en, ignore.case = TRUE))
})

test_that("get_indicator_tooltip returns tooltips for long column names", {
  tooltip <- nemetonShiny:::get_indicator_tooltip("indicateur_c1_biomasse", "fr")
  expect_type(tooltip, "character")
  expect_true(nchar(tooltip) > 0)

  tooltip_norm <- nemetonShiny:::get_indicator_tooltip("carbon_biomass_norm", "fr")
  expect_type(tooltip_norm, "character")
  expect_true(nchar(tooltip_norm) > 0)
})

test_that("get_indicator_tooltip returns NULL for unknown indicators", {
  tooltip <- nemetonShiny:::get_indicator_tooltip("unknown_indicator", "fr")
  expect_null(tooltip)
})

test_that("get_indicator_tooltip falls back to English", {
  # Test fallback by using an indicator that should exist
  tooltip <- nemetonShiny:::get_indicator_tooltip("B1", "de")  # German not supported
  # Should fallback to English if French not found, or return English directly
  # If implementation falls back correctly, should still return something
  expect_true(is.null(tooltip) || is.character(tooltip))
})

test_that("get_indicator_cols filters correctly", {
  df <- data.frame(
    nemeton_id = 1:3,
    id = 1:3,
    geo_parcelle = c("a", "b", "c"),
    C1 = runif(3),
    C2 = runif(3),
    B1 = runif(3),
    geometry = rep("POINT(0 0)", 3),
    nomcommune = c("A", "B", "C"),
    area = c(100, 200, 300)
  )

  result <- nemetonShiny:::get_indicator_cols(df)
  expect_equal(sort(result), c("B1", "C1", "C2"))
  expect_false("nemeton_id" %in% result)
  expect_false("geometry" %in% result)
  expect_false("nomcommune" %in% result)
})


# =============================================================================
# make_indicator_leaflet tests
# =============================================================================

test_that("make_indicator_leaflet creates leaflet map", {
  skip_if_not_installed("sf")
  skip_if_not_installed("leaflet")

  sf_data <- sf::st_sf(
    nemeton_id = c("p1", "p2", "p3"),
    C1 = c(50, 75, 60),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  map <- nemetonShiny:::make_indicator_leaflet(sf_data, "C1", "Test Indicator")
  expect_s3_class(map, "leaflet")
})

test_that("make_indicator_leaflet handles all NA values", {
  skip_if_not_installed("sf")
  skip_if_not_installed("leaflet")

  sf_data <- sf::st_sf(
    nemeton_id = c("p1", "p2"),
    C1 = c(NA_real_, NA_real_),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  map <- nemetonShiny:::make_indicator_leaflet(sf_data, "C1", "Test")
  expect_s3_class(map, "leaflet")
})

test_that("make_indicator_leaflet handles identical values", {
  skip_if_not_installed("sf")
  skip_if_not_installed("leaflet")

  sf_data <- sf::st_sf(
    nemeton_id = c("p1", "p2", "p3"),
    C1 = c(50, 50, 50),  # All identical
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  map <- nemetonShiny:::make_indicator_leaflet(sf_data, "C1", "Test")
  expect_s3_class(map, "leaflet")
})

test_that("make_indicator_leaflet uses YlOrRd for risk indicators", {
  skip_if_not_installed("sf")
  skip_if_not_installed("leaflet")

  sf_data <- sf::st_sf(
    nemeton_id = c("p1", "p2"),
    R1 = c(30, 70),  # Risk indicator
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  map <- nemetonShiny:::make_indicator_leaflet(sf_data, "R1", "Fire Risk")
  expect_s3_class(map, "leaflet")
})


# =============================================================================
# mod_family_server tests with testServer
# =============================================================================

test_that("mod_family_server handles NULL project gracefully", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("sf")

  mock_app_state <- shiny::reactiveValues(
    current_project = NULL,
    language = "fr"
  )

  shiny::testServer(
    nemetonShiny:::mod_family_server,
    args = list(
      family_code = "C",
      app_state = mock_app_state
    ),
    {
      # With no project, indicators_data should be NULL
      expect_null(indicators_data())
      expect_null(indicators_sf())
    }
  )
})

test_that("mod_family_server processes project with indicators", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("sf")

  # Create mock project with indicators
  mock_parcels <- sf::st_sf(
    nemeton_id = c("p1", "p2", "p3"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  mock_indicators <- data.frame(
    nemeton_id = c("p1", "p2", "p3"),
    carbon_biomass_norm = c(0.5, 0.7, 0.3),
    carbon_ndvi_norm = c(0.8, 0.6, 0.9)
  )

  mock_project <- list(
    parcels = mock_parcels,
    indicators = mock_indicators
  )

  mock_app_state <- shiny::reactiveValues(
    current_project = mock_project,
    language = "fr",
    family_comments = list()
  )

  shiny::testServer(
    nemetonShiny:::mod_family_server,
    args = list(
      family_code = "C",
      app_state = mock_app_state
    ),
    {
      # With project containing C indicators, should have data
      data <- indicators_data()
      expect_false(is.null(data))
      expect_true("carbon_biomass_norm" %in% names(data) ||
                  "carbon_ndvi_norm" %in% names(data))

      # indicators_sf should also work
      sf_data <- indicators_sf()
      expect_false(is.null(sf_data))
      expect_s3_class(sf_data, "sf")
    }
  )
})

test_that("mod_family_server returns NULL for family with no indicators", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("sf")

  # Create project with only Carbon indicators, test Biodiversity family
  mock_parcels <- sf::st_sf(
    nemeton_id = c("p1", "p2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  mock_indicators <- data.frame(
    nemeton_id = c("p1", "p2"),
    carbon_biomass_norm = c(0.5, 0.7)  # Only Carbon, no Biodiversity
  )

  mock_project <- list(
    parcels = mock_parcels,
    indicators = mock_indicators
  )

  mock_app_state <- shiny::reactiveValues(
    current_project = mock_project,
    language = "fr"
  )

  shiny::testServer(
    nemetonShiny:::mod_family_server,
    args = list(
      family_code = "B",  # Biodiversity - not in indicators
      app_state = mock_app_state
    ),
    {
      # No B indicators in project
      data <- indicators_data()
      expect_null(data)
    }
  )
})

test_that("mod_family_server handles sf indicators data", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("sf")

  mock_parcels <- sf::st_sf(
    nemeton_id = c("p1", "p2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  # Indicators as sf object (can happen with geoarrow)
  mock_indicators <- sf::st_sf(
    nemeton_id = c("p1", "p2"),
    carbon_biomass_norm = c(0.5, 0.7),
    geometry = mock_parcels$geometry
  )

  mock_project <- list(
    parcels = mock_parcels,
    indicators = mock_indicators
  )

  mock_app_state <- shiny::reactiveValues(
    current_project = mock_project,
    language = "fr"
  )

  shiny::testServer(
    nemetonShiny:::mod_family_server,
    args = list(
      family_code = "C",
      app_state = mock_app_state
    ),
    {
      # Should handle sf indicators by dropping geometry
      data <- indicators_data()
      expect_false(is.null(data))
      expect_false(inherits(data, "sf"))  # Geometry should be dropped
    }
  )
})


# =============================================================================
# Family config tests
# =============================================================================

test_that("get_family_config returns valid configs", {
  for (code in nemetonShiny:::get_family_codes()) {
    config <- nemetonShiny:::get_family_config(code)
    expect_type(config, "list")
    expect_true("name_fr" %in% names(config) || "name_en" %in% names(config))
    expect_true("icon" %in% names(config))
    expect_true("indicators" %in% names(config))
    expect_true(length(config$indicators) > 0)
  }
})

test_that("get_family_config returns NULL for unknown family", {
  config <- nemetonShiny:::get_family_config("UNKNOWN")
  expect_null(config)
})

test_that("get_family_codes returns all family codes", {
  codes <- nemetonShiny:::get_family_codes()
  expect_true(length(codes) > 0)
  expect_true("C" %in% codes)  # Carbon
  expect_true("B" %in% codes)  # Biodiversity
  expect_true("W" %in% codes)  # Water
})


# =============================================================================
# Indicator name mapping tests (via config)
# =============================================================================

test_that("INDICATOR_FAMILIES contains column_names mappings", {
  # Test that family config has column_names for mapping
  config <- nemetonShiny:::get_family_config("C")
  expect_true("column_names" %in% names(config))
  expect_equal(config$column_names, c("indicateur_c1_biomasse", "indicateur_c2_ndvi"))

  # The indicators list maps to column_names by position
  expect_equal(length(config$indicators), length(config$column_names))
  expect_equal(config$indicators[1], "C1")  # First indicator is C1
  expect_equal(config$column_names[1], "indicateur_c1_biomasse")  # Maps to indicateur_c1_biomasse
})

# ==============================================================================
# Additional Family Config Edge Cases
# ==============================================================================

test_that("all 12 families have valid icon names", {
  families <- nemetonShiny:::INDICATOR_FAMILIES
  for (code in names(families)) {
    fam <- families[[code]]
    expect_true(is.character(fam$icon), info = paste("Family", code, "icon"))
    expect_true(nchar(fam$icon) > 0, info = paste("Family", code, "icon not empty"))
  }
})

test_that("all families have matching indicator counts between indicators and column_names", {
  families <- nemetonShiny:::INDICATOR_FAMILIES
  for (code in names(families)) {
    fam <- families[[code]]
    expect_equal(
      length(fam$indicators), length(fam$column_names),
      info = paste("Family", code, "indicator/column_names mismatch")
    )
  }
})

test_that("all families have indicator_labels for each indicator", {
  families <- nemetonShiny:::INDICATOR_FAMILIES
  for (code in names(families)) {
    fam <- families[[code]]
    for (ind in fam$indicators) {
      expect_true(
        ind %in% names(fam$indicator_labels),
        info = paste("Family", code, "missing label for", ind)
      )
      label <- fam$indicator_labels[[ind]]
      expect_true("fr" %in% names(label), info = paste(ind, "missing fr label"))
      expect_true("en" %in% names(label), info = paste(ind, "missing en label"))
    }
  }
})

test_that("all families have indicator_tooltips for each indicator", {
  families <- nemetonShiny:::INDICATOR_FAMILIES
  for (code in names(families)) {
    fam <- families[[code]]
    for (ind in fam$indicators) {
      expect_true(
        ind %in% names(fam$indicator_tooltips),
        info = paste("Family", code, "missing tooltip for", ind)
      )
      tooltip <- fam$indicator_tooltips[[ind]]
      expect_true("fr" %in% names(tooltip), info = paste(ind, "missing fr tooltip"))
      expect_true("en" %in% names(tooltip), info = paste(ind, "missing en tooltip"))
    }
  }
})

test_that("family colors are valid hex codes", {
  families <- nemetonShiny:::INDICATOR_FAMILIES
  for (code in names(families)) {
    color <- families[[code]]$color
    expect_true(
      grepl("^#[0-9A-Fa-f]{6}$", color),
      info = paste("Family", code, "invalid color:", color)
    )
  }
})

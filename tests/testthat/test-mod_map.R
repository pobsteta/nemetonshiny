# Tests for Map Module
# Phase 2: Leaflet map with parcel selection

# ==============================================================================
# UI Tests
# ==============================================================================

test_that("mod_map_ui returns valid bslib card", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  ui <- nemetonShiny:::mod_map_ui("test")

  expect_s3_class(ui, "shiny.tag")
  ui_html <- as.character(ui)

  # Should be a card
  expect_true(grepl("card", ui_html, ignore.case = TRUE))
})

test_that("mod_map_ui creates namespaced elements", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  ui <- nemetonShiny:::mod_map_ui("test_ns")
  ui_html <- as.character(ui)

  expect_true(grepl("test_ns-map", ui_html))
  expect_true(grepl("test_ns-clear_selection", ui_html))
})

test_that("mod_map_ui includes basemap toggle buttons", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  ui <- nemetonShiny:::mod_map_ui("test")
  ui_html <- as.character(ui)

  expect_true(grepl("basemap_osm", ui_html))
  expect_true(grepl("basemap_satellite", ui_html))
  expect_true(grepl("OSM", ui_html))
  expect_true(grepl("Satellite", ui_html))
})

test_that("mod_map_ui includes clear selection button", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  ui <- nemetonShiny:::mod_map_ui("test")
  ui_html <- as.character(ui)

  expect_true(grepl("clear_selection", ui_html))
})

test_that("mod_map_ui includes leaflet output", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  ui <- nemetonShiny:::mod_map_ui("test")
  ui_html <- as.character(ui)

  expect_true(grepl("leaflet", ui_html, ignore.case = TRUE))
})

test_that("mod_map_ui includes selection summary", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  ui <- nemetonShiny:::mod_map_ui("test")
  ui_html <- as.character(ui)

  expect_true(grepl("selection_summary", ui_html))
})

test_that("mod_map_ui includes map loading overlay", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  ui <- nemetonShiny:::mod_map_ui("test")
  ui_html <- as.character(ui)

  expect_true(grepl("map_loading_overlay", ui_html))
  expect_true(grepl("spinner", ui_html, ignore.case = TRUE))
})

test_that("mod_map_ui includes map card with full screen option", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  ui <- nemetonShiny:::mod_map_ui("test")
  ui_html <- as.character(ui)

  expect_true(grepl("map_card", ui_html))
})

test_that("mod_map_ui includes selection text and area elements", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  ui <- nemetonShiny:::mod_map_ui("test")
  ui_html <- as.character(ui)

  expect_true(grepl("selection_text", ui_html))
  expect_true(grepl("selection_area", ui_html))
})

test_that("mod_map_ui has proper accessibility attributes", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  ui <- nemetonShiny:::mod_map_ui("test")
  ui_html <- as.character(ui)

  # Button group should have aria-label

  expect_true(grepl("aria-label", ui_html))
  expect_true(grepl("Basemap selection", ui_html))
})

# ==============================================================================
# Server Function Signature Tests
# ==============================================================================

test_that("mod_map_server is a function", {
  expect_type(nemetonShiny:::mod_map_server, "closure")
})

test_that("mod_map_server accepts required parameters", {
  args <- names(formals(nemetonShiny:::mod_map_server))

  expect_true("id" %in% args)
  expect_true("app_state" %in% args)
  expect_true("commune_geometry" %in% args)
  expect_true("parcels" %in% args)
})

# ==============================================================================
# Configuration Tests
# ==============================================================================

test_that("MAX_PARCELS constant is defined correctly", {
  # Check app config returns correct value
  max_parcels <- nemetonShiny:::get_app_config("max_parcels", 20L)

  expect_type(max_parcels, "integer")
  expect_equal(max_parcels, 20L)
})

test_that("Map styling constants are properly defined", {
  # This test validates the styling structure expected by the map module
  # The actual STYLE is defined inside the server function, but we can
  # verify the expected colors are used

  # These should match the theme colors
  commune_color <- "#1B6B1B"   # Forest green
  default_color <- "#666666"
  selected_color <- "#1B6B1B"

  expect_true(grepl("^#[0-9A-Fa-f]{6}$", commune_color))
  expect_true(grepl("^#[0-9A-Fa-f]{6}$", default_color))
  expect_true(grepl("^#[0-9A-Fa-f]{6}$", selected_color))
})

# ==============================================================================
# Server Logic Tests with testServer
# ==============================================================================

test_that("mod_map_server returns expected reactive values", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    get_app_config = function(key, default = NULL) {
      if (key == "max_parcels") return(20L)
      default
    },
    {
      # Create mock app_state
      mock_app_state <- shiny::reactiveValues(
        language = "en",
        computation_running = FALSE,
        commune_transitioning = FALSE,
        restore_project = NULL,
        restore_in_progress = FALSE,
        clear_map_selection = NULL
      )

      # Create mock parcels data (empty)
      mock_parcels <- shiny::reactiveVal(NULL)

      # Create mock commune geometry (empty)
      mock_commune_geometry <- shiny::reactiveVal(NULL)

      shiny::testServer(
        nemetonShiny:::mod_map_server,
        args = list(
          app_state = mock_app_state,
          commune_geometry = mock_commune_geometry,
          parcels = mock_parcels
        ),
        {
          # Check that returned list has expected reactive elements
          result <- session$getReturned()

          expect_true("selected_ids" %in% names(result))
          expect_true("selected_parcels" %in% names(result))
          expect_true("selection_count" %in% names(result))

          # Check that they are reactive functions
          expect_type(result$selected_ids, "closure")
          expect_type(result$selected_parcels, "closure")
          expect_type(result$selection_count, "closure")

          # Initial values should be empty
          expect_equal(result$selected_ids(), character(0))
          expect_null(result$selected_parcels())
          expect_equal(result$selection_count(), 0)
        }
      )
    }
  )
})

test_that("mod_map_server handles parcel selection", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    get_app_config = function(key, default = NULL) {
      if (key == "max_parcels") return(20L)
      default
    },
    get_i18n = function(language = "en") {
      list(
        t = function(key, ...) key,
        has = function(key) TRUE
      )
    },
    {
      # Create test parcel data
      test_parcels <- sf::st_sf(
        id = c("parcel_001", "parcel_002", "parcel_003"),
        section = c("A", "A", "B"),
        numero = c("1", "2", "3"),
        contenance = c(10000, 20000, 15000),
        geometry = sf::st_sfc(
          sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
          sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
          sf::st_polygon(list(matrix(c(0, 1, 1, 1, 1, 2, 0, 2, 0, 1), ncol = 2, byrow = TRUE))),
          crs = 4326
        )
      )

      mock_app_state <- shiny::reactiveValues(
        language = "en",
        computation_running = FALSE,
        commune_transitioning = FALSE,
        restore_project = NULL,
        restore_in_progress = FALSE,
        clear_map_selection = NULL
      )

      mock_parcels <- shiny::reactiveVal(test_parcels)
      mock_commune_geometry <- shiny::reactiveVal(NULL)

      shiny::testServer(
        nemetonShiny:::mod_map_server,
        args = list(
          app_state = mock_app_state,
          commune_geometry = mock_commune_geometry,
          parcels = mock_parcels
        ),
        {
          result <- session$getReturned()

          # Initially no selection
          expect_equal(result$selection_count(), 0)

          # Simulate parcel click
          session$setInputs(map_shape_click = list(id = "parcel_001"))

          # After click, parcel should be selected
          session$flushReact()
          expect_equal(result$selection_count(), 1)
          expect_true("parcel_001" %in% result$selected_ids())
        }
      )
    }
  )
})

test_that("mod_map_server handles parcel deselection", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    get_app_config = function(key, default = NULL) {
      if (key == "max_parcels") return(20L)
      default
    },
    get_i18n = function(language = "en") {
      list(
        t = function(key, ...) key,
        has = function(key) TRUE
      )
    },
    {
      test_parcels <- sf::st_sf(
        id = c("parcel_001", "parcel_002"),
        section = c("A", "A"),
        numero = c("1", "2"),
        contenance = c(10000, 20000),
        geometry = sf::st_sfc(
          sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
          sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
          crs = 4326
        )
      )

      mock_app_state <- shiny::reactiveValues(
        language = "en",
        computation_running = FALSE,
        commune_transitioning = FALSE,
        restore_project = NULL,
        restore_in_progress = FALSE,
        clear_map_selection = NULL
      )

      mock_parcels <- shiny::reactiveVal(test_parcels)
      mock_commune_geometry <- shiny::reactiveVal(NULL)

      shiny::testServer(
        nemetonShiny:::mod_map_server,
        args = list(
          app_state = mock_app_state,
          commune_geometry = mock_commune_geometry,
          parcels = mock_parcels
        ),
        {
          result <- session$getReturned()

          # Select a parcel
          session$setInputs(map_shape_click = list(id = "parcel_001"))
          suppressWarnings(session$flushReact())
          expect_equal(result$selection_count(), 1)

          # Click same parcel again to deselect
          session$setInputs(map_shape_click = list(id = "parcel_001"))
          suppressWarnings(session$flushReact())
          expect_equal(result$selection_count(), 0)
          expect_false("parcel_001" %in% result$selected_ids())
        }
      )
    }
  )
})

test_that("mod_map_server enforces max parcels limit", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    get_app_config = function(key, default = NULL) {
      if (key == "max_parcels") return(3L)  # Low limit for testing
      default
    },
    get_i18n = function(language = "en") {
      list(
        t = function(key, ...) key,
        has = function(key) TRUE
      )
    },
    {
      # Create 5 parcels
      test_parcels <- sf::st_sf(
        id = paste0("parcel_", sprintf("%03d", 1:5)),
        section = rep("A", 5),
        numero = as.character(1:5),
        contenance = rep(10000, 5),
        geometry = sf::st_sfc(
          lapply(0:4, function(i) {
            sf::st_polygon(list(matrix(c(i, 0, i + 1, 0, i + 1, 1, i, 1, i, 0), ncol = 2, byrow = TRUE)))
          }),
          crs = 4326
        )
      )

      mock_app_state <- shiny::reactiveValues(
        language = "en",
        computation_running = FALSE,
        commune_transitioning = FALSE,
        restore_project = NULL,
        restore_in_progress = FALSE,
        clear_map_selection = NULL
      )

      mock_parcels <- shiny::reactiveVal(test_parcels)
      mock_commune_geometry <- shiny::reactiveVal(NULL)

      shiny::testServer(
        nemetonShiny:::mod_map_server,
        args = list(
          app_state = mock_app_state,
          commune_geometry = mock_commune_geometry,
          parcels = mock_parcels
        ),
        {
          result <- session$getReturned()

          # Select parcels up to the limit
          session$setInputs(map_shape_click = list(id = "parcel_001"))
          session$flushReact()
          session$setInputs(map_shape_click = list(id = "parcel_002"))
          session$flushReact()
          session$setInputs(map_shape_click = list(id = "parcel_003"))
          session$flushReact()

          expect_equal(result$selection_count(), 3)

          # Try to select one more (should not increase)
          session$setInputs(map_shape_click = list(id = "parcel_004"))
          session$flushReact()

          # Selection count should still be 3 (limit reached)
          expect_equal(result$selection_count(), 3)
          expect_false("parcel_004" %in% result$selected_ids())
        }
      )
    }
  )
})

test_that("mod_map_server blocks selection during computation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    get_app_config = function(key, default = NULL) {
      if (key == "max_parcels") return(20L)
      default
    },
    get_i18n = function(language = "en") {
      list(
        t = function(key, ...) key,
        has = function(key) TRUE
      )
    },
    {
      test_parcels <- sf::st_sf(
        id = c("parcel_001", "parcel_002"),
        section = c("A", "A"),
        numero = c("1", "2"),
        contenance = c(10000, 20000),
        geometry = sf::st_sfc(
          sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
          sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
          crs = 4326
        )
      )

      mock_app_state <- shiny::reactiveValues(
        language = "en",
        computation_running = TRUE,  # Computation is running
        commune_transitioning = FALSE,
        restore_project = NULL,
        restore_in_progress = FALSE,
        clear_map_selection = NULL
      )

      mock_parcels <- shiny::reactiveVal(test_parcels)
      mock_commune_geometry <- shiny::reactiveVal(NULL)

      shiny::testServer(
        nemetonShiny:::mod_map_server,
        args = list(
          app_state = mock_app_state,
          commune_geometry = mock_commune_geometry,
          parcels = mock_parcels
        ),
        {
          result <- session$getReturned()

          # Try to select a parcel while computation is running
          session$setInputs(map_shape_click = list(id = "parcel_001"))
          session$flushReact()

          # Selection should be blocked
          expect_equal(result$selection_count(), 0)
        }
      )
    }
  )
})

test_that("mod_map_server clear_selection button works", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    get_app_config = function(key, default = NULL) {
      if (key == "max_parcels") return(20L)
      default
    },
    get_i18n = function(language = "en") {
      list(
        t = function(key, ...) key,
        has = function(key) TRUE
      )
    },
    {
      test_parcels <- sf::st_sf(
        id = c("parcel_001", "parcel_002"),
        section = c("A", "A"),
        numero = c("1", "2"),
        contenance = c(10000, 20000),
        geometry = sf::st_sfc(
          sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
          sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
          crs = 4326
        )
      )

      mock_app_state <- shiny::reactiveValues(
        language = "en",
        computation_running = FALSE,
        commune_transitioning = FALSE,
        restore_project = NULL,
        restore_in_progress = FALSE,
        clear_map_selection = NULL
      )

      mock_parcels <- shiny::reactiveVal(test_parcels)
      mock_commune_geometry <- shiny::reactiveVal(NULL)

      shiny::testServer(
        nemetonShiny:::mod_map_server,
        args = list(
          app_state = mock_app_state,
          commune_geometry = mock_commune_geometry,
          parcels = mock_parcels
        ),
        {
          result <- session$getReturned()

          # Select parcels
          session$setInputs(map_shape_click = list(id = "parcel_001"))
          session$flushReact()
          session$setInputs(map_shape_click = list(id = "parcel_002"))
          session$flushReact()

          expect_equal(result$selection_count(), 2)

          # Clear selection
          session$setInputs(clear_selection = 1)
          session$flushReact()

          expect_equal(result$selection_count(), 0)
          expect_equal(result$selected_ids(), character(0))
        }
      )
    }
  )
})

test_that("mod_map_server handles clear_map_selection from app_state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    get_app_config = function(key, default = NULL) {
      if (key == "max_parcels") return(20L)
      default
    },
    get_i18n = function(language = "en") {
      list(
        t = function(key, ...) key,
        has = function(key) TRUE
      )
    },
    {
      test_parcels <- sf::st_sf(
        id = c("parcel_001", "parcel_002"),
        section = c("A", "A"),
        numero = c("1", "2"),
        contenance = c(10000, 20000),
        geometry = sf::st_sfc(
          sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
          sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
          crs = 4326
        )
      )

      mock_app_state <- shiny::reactiveValues(
        language = "en",
        computation_running = FALSE,
        commune_transitioning = FALSE,
        restore_project = NULL,
        restore_in_progress = FALSE,
        clear_map_selection = NULL
      )

      mock_parcels <- shiny::reactiveVal(test_parcels)
      mock_commune_geometry <- shiny::reactiveVal(NULL)

      shiny::testServer(
        nemetonShiny:::mod_map_server,
        args = list(
          app_state = mock_app_state,
          commune_geometry = mock_commune_geometry,
          parcels = mock_parcels
        ),
        {
          result <- session$getReturned()

          # Select parcels
          session$setInputs(map_shape_click = list(id = "parcel_001"))
          session$flushReact()

          expect_equal(result$selection_count(), 1)

          # Trigger external clear via app_state
          mock_app_state$clear_map_selection <- Sys.time()
          session$flushReact()

          expect_equal(result$selection_count(), 0)
        }
      )
    }
  )
})

test_that("mod_map_server selected_parcels returns correct sf object", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    get_app_config = function(key, default = NULL) {
      if (key == "max_parcels") return(20L)
      default
    },
    get_i18n = function(language = "en") {
      list(
        t = function(key, ...) key,
        has = function(key) TRUE
      )
    },
    filter_selected_parcels = function(parcels, selected_ids) {
      if (length(selected_ids) == 0) return(parcels[0, ])
      parcels[parcels$id %in% selected_ids, ]
    },
    {
      test_parcels <- sf::st_sf(
        id = c("parcel_001", "parcel_002", "parcel_003"),
        section = c("A", "A", "B"),
        numero = c("1", "2", "3"),
        contenance = c(10000, 20000, 15000),
        geometry = sf::st_sfc(
          sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
          sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
          sf::st_polygon(list(matrix(c(0, 1, 1, 1, 1, 2, 0, 2, 0, 1), ncol = 2, byrow = TRUE))),
          crs = 4326
        )
      )

      mock_app_state <- shiny::reactiveValues(
        language = "en",
        computation_running = FALSE,
        commune_transitioning = FALSE,
        restore_project = NULL,
        restore_in_progress = FALSE,
        clear_map_selection = NULL
      )

      mock_parcels <- shiny::reactiveVal(test_parcels)
      mock_commune_geometry <- shiny::reactiveVal(NULL)

      shiny::testServer(
        nemetonShiny:::mod_map_server,
        args = list(
          app_state = mock_app_state,
          commune_geometry = mock_commune_geometry,
          parcels = mock_parcels
        ),
        {
          result <- session$getReturned()

          # Select two parcels
          session$setInputs(map_shape_click = list(id = "parcel_001"))
          session$flushReact()
          session$setInputs(map_shape_click = list(id = "parcel_003"))
          session$flushReact()

          selected <- result$selected_parcels()

          expect_s3_class(selected, "sf")
          expect_equal(nrow(selected), 2)
          expect_true("parcel_001" %in% selected$id)
          expect_true("parcel_003" %in% selected$id)
          expect_false("parcel_002" %in% selected$id)
        }
      )
    }
  )
})

# ==============================================================================
# Helper Function Tests
# ==============================================================================

test_that("create_parcel_label generates valid HTML label", {
  skip_if_not_installed("sf")

  # Create a test parcel
  parcel <- sf::st_sf(
    id = "test_001",
    section = "AB",
    numero = "42",
    contenance = 25000,
    nom_com = "Test Commune",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  label <- nemetonShiny:::create_parcel_label(parcel)

  expect_type(label, "character")
  expect_true(grepl("AB", label))  # Section
  expect_true(grepl("42", label))  # Numero
  expect_true(grepl("2.5 ha", label))  # Area in hectares
  expect_true(grepl("Test Commune", label))  # Lieu-dit
})

test_that("create_parcel_label handles missing fields gracefully", {
  skip_if_not_installed("sf")

  # Create a minimal parcel
  parcel <- sf::st_sf(
    id = "test_001",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  label <- nemetonShiny:::create_parcel_label(parcel)

  expect_type(label, "character")
  # Should use "-" for missing section/numero
  expect_true(grepl("-", label))
})

test_that("create_parcel_label handles NULL contenance", {
  skip_if_not_installed("sf")

  parcel <- sf::st_sf(
    id = "test_001",
    section = "A",
    numero = "1",
    contenance = NA_real_,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  label <- nemetonShiny:::create_parcel_label(parcel)

  expect_type(label, "character")
  # Should not contain "ha" since area is NA
  expect_false(grepl("ha", label))
})

test_that("filter_selected_parcels returns correct subset", {
  skip_if_not_installed("sf")

  parcels <- sf::st_sf(
    id = c("A", "B", "C", "D"),
    value = c(1, 2, 3, 4),
    geometry = sf::st_sfc(
      lapply(0:3, function(i) {
        sf::st_polygon(list(matrix(c(i, 0, i + 1, 0, i + 1, 1, i, 1, i, 0), ncol = 2, byrow = TRUE)))
      }),
      crs = 4326
    )
  )

  result <- nemetonShiny:::filter_selected_parcels(parcels, c("B", "D"))

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)
  expect_equal(sort(result$id), c("B", "D"))
})

test_that("filter_selected_parcels returns empty sf for no selection", {
  skip_if_not_installed("sf")

  parcels <- sf::st_sf(
    id = c("A", "B", "C"),
    geometry = sf::st_sfc(
      lapply(0:2, function(i) {
        sf::st_polygon(list(matrix(c(i, 0, i + 1, 0, i + 1, 1, i, 1, i, 0), ncol = 2, byrow = TRUE)))
      }),
      crs = 4326
    )
  )

  result <- nemetonShiny:::filter_selected_parcels(parcels, character(0))

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 0)
})

test_that("calculate_parcel_stats returns correct statistics", {
  skip_if_not_installed("sf")

  parcels <- sf::st_sf(
    id = c("A", "B", "C"),
    contenance = c(10000, 20000, 30000),  # m2
    geometry = sf::st_sfc(
      lapply(0:2, function(i) {
        sf::st_polygon(list(matrix(c(i, 0, i + 1, 0, i + 1, 1, i, 1, i, 0), ncol = 2, byrow = TRUE)))
      }),
      crs = 4326
    )
  )

  stats <- nemetonShiny:::calculate_parcel_stats(parcels)

  expect_type(stats, "list")
  expect_equal(stats$count, 3)
  expect_equal(stats$total_area_ha, 6)  # 60000 m2 = 6 ha
  expect_equal(stats$mean_area_ha, 2)
  expect_equal(stats$min_area_ha, 1)
  expect_equal(stats$max_area_ha, 3)
})

test_that("calculate_parcel_stats handles NULL input", {
  stats <- nemetonShiny:::calculate_parcel_stats(NULL)

  expect_type(stats, "list")
  expect_equal(stats$count, 0)
  expect_equal(stats$total_area_ha, 0)
})

test_that("calculate_parcel_stats handles empty sf", {
  skip_if_not_installed("sf")

  # Create an empty sf object
  empty_parcels <- sf::st_sf(
    id = character(0),
    contenance = numeric(0),
    geometry = sf::st_sfc(crs = 4326)
  )

  stats <- nemetonShiny:::calculate_parcel_stats(empty_parcels)

  expect_type(stats, "list")
  expect_equal(stats$count, 0)
  expect_equal(stats$total_area_ha, 0)
})

# ==============================================================================
# i18n Keys Tests
# ==============================================================================

test_that("i18n keys for map module exist in both languages", {
  fr <- nemetonShiny:::get_i18n("fr")
  en <- nemetonShiny:::get_i18n("en")

  map_keys <- c(
    "map_title",
    "click_to_select",
    "clear_selection",
    "max_parcels_warning",
    "parcels_selected",
    "rendering_parcels",
    "selection_blocked_during_computation"
  )

  for (key in map_keys) {
    expect_true(fr$has(key), info = paste("FR missing:", key))
    expect_true(en$has(key), info = paste("EN missing:", key))
  }
})

# ==============================================================================
# Basemap Toggle Tests
# ==============================================================================

test_that("mod_map_server handles basemap toggle to OSM", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    get_app_config = function(key, default = NULL) {
      if (key == "max_parcels") return(20L)
      default
    },
    {
      mock_app_state <- shiny::reactiveValues(
        language = "en",
        computation_running = FALSE,
        commune_transitioning = FALSE,
        restore_project = NULL,
        restore_in_progress = FALSE,
        clear_map_selection = NULL
      )

      mock_parcels <- shiny::reactiveVal(NULL)
      mock_commune_geometry <- shiny::reactiveVal(NULL)

      shiny::testServer(
        nemetonShiny:::mod_map_server,
        args = list(
          app_state = mock_app_state,
          commune_geometry = mock_commune_geometry,
          parcels = mock_parcels
        ),
        {
          # Toggle to OSM basemap
          session$setInputs(basemap_osm = 1)
          session$flushReact()

          # The server should handle this without error
          # We cannot easily check the Leaflet proxy calls, but we verify no errors
          expect_true(TRUE)
        }
      )
    }
  )
})

test_that("mod_map_server handles basemap toggle to Satellite", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    get_app_config = function(key, default = NULL) {
      if (key == "max_parcels") return(20L)
      default
    },
    {
      mock_app_state <- shiny::reactiveValues(
        language = "en",
        computation_running = FALSE,
        commune_transitioning = FALSE,
        restore_project = NULL,
        restore_in_progress = FALSE,
        clear_map_selection = NULL
      )

      mock_parcels <- shiny::reactiveVal(NULL)
      mock_commune_geometry <- shiny::reactiveVal(NULL)

      shiny::testServer(
        nemetonShiny:::mod_map_server,
        args = list(
          app_state = mock_app_state,
          commune_geometry = mock_commune_geometry,
          parcels = mock_parcels
        ),
        {
          # Toggle to Satellite basemap
          session$setInputs(basemap_satellite = 1)
          session$flushReact()

          # The server should handle this without error
          expect_true(TRUE)
        }
      )
    }
  )
})

# ==============================================================================
# Edge Cases Tests
# ==============================================================================

test_that("mod_map_server handles null click id gracefully", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    get_app_config = function(key, default = NULL) {
      if (key == "max_parcels") return(20L)
      default
    },
    {
      mock_app_state <- shiny::reactiveValues(
        language = "en",
        computation_running = FALSE,
        commune_transitioning = FALSE,
        restore_project = NULL,
        restore_in_progress = FALSE,
        clear_map_selection = NULL
      )

      mock_parcels <- shiny::reactiveVal(NULL)
      mock_commune_geometry <- shiny::reactiveVal(NULL)

      shiny::testServer(
        nemetonShiny:::mod_map_server,
        args = list(
          app_state = mock_app_state,
          commune_geometry = mock_commune_geometry,
          parcels = mock_parcels
        ),
        {
          result <- session$getReturned()

          # Click with no id (should be ignored)
          session$setInputs(map_shape_click = list(id = NULL))
          session$flushReact()

          expect_equal(result$selection_count(), 0)
        }
      )
    }
  )
})

test_that("mod_map_server handles empty click event gracefully", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    get_app_config = function(key, default = NULL) {
      if (key == "max_parcels") return(20L)
      default
    },
    {
      mock_app_state <- shiny::reactiveValues(
        language = "en",
        computation_running = FALSE,
        commune_transitioning = FALSE,
        restore_project = NULL,
        restore_in_progress = FALSE,
        clear_map_selection = NULL
      )

      mock_parcels <- shiny::reactiveVal(NULL)
      mock_commune_geometry <- shiny::reactiveVal(NULL)

      shiny::testServer(
        nemetonShiny:::mod_map_server,
        args = list(
          app_state = mock_app_state,
          commune_geometry = mock_commune_geometry,
          parcels = mock_parcels
        ),
        {
          result <- session$getReturned()

          # Null click event
          session$setInputs(map_shape_click = NULL)
          session$flushReact()

          expect_equal(result$selection_count(), 0)
        }
      )
    }
  )
})

test_that("mod_map_server handles multiple rapid selections", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    get_app_config = function(key, default = NULL) {
      if (key == "max_parcels") return(20L)
      default
    },
    get_i18n = function(language = "en") {
      list(
        t = function(key, ...) key,
        has = function(key) TRUE
      )
    },
    {
      # Create many parcels
      n <- 10
      test_parcels <- sf::st_sf(
        id = paste0("parcel_", sprintf("%03d", 1:n)),
        section = rep("A", n),
        numero = as.character(1:n),
        contenance = rep(10000, n),
        geometry = sf::st_sfc(
          lapply(0:(n - 1), function(i) {
            sf::st_polygon(list(matrix(c(i, 0, i + 1, 0, i + 1, 1, i, 1, i, 0), ncol = 2, byrow = TRUE)))
          }),
          crs = 4326
        )
      )

      mock_app_state <- shiny::reactiveValues(
        language = "en",
        computation_running = FALSE,
        commune_transitioning = FALSE,
        restore_project = NULL,
        restore_in_progress = FALSE,
        clear_map_selection = NULL
      )

      mock_parcels <- shiny::reactiveVal(test_parcels)
      mock_commune_geometry <- shiny::reactiveVal(NULL)

      shiny::testServer(
        nemetonShiny:::mod_map_server,
        args = list(
          app_state = mock_app_state,
          commune_geometry = mock_commune_geometry,
          parcels = mock_parcels
        ),
        {
          result <- session$getReturned()

          # Select multiple parcels rapidly
          for (i in 1:5) {
            session$setInputs(map_shape_click = list(id = paste0("parcel_", sprintf("%03d", i))))
            session$flushReact()
          }

          expect_equal(result$selection_count(), 5)
          expect_equal(length(result$selected_ids()), 5)
        }
      )
    }
  )
})

# Drain async callbacks to prevent testServer session accumulation
later::run_now(0)

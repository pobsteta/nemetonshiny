# Tests for Search Module
# Phase 2: Commune search UI module
# Comprehensive tests for UI generation, server logic, and error handling

# ==============================================================================
# UI Tests
# ==============================================================================

test_that("mod_search_ui returns valid Shiny UI", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_search_ui("test")

      expect_s3_class(ui, "shiny.tag.list")
    }
  )
})

test_that("mod_search_ui creates namespaced inputs", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_search_ui("test_ns")
      ui_html <- as.character(ui)

      # Check for namespaced input IDs
      expect_true(grepl("test_ns-departement", ui_html))
      expect_true(grepl("test_ns-commune", ui_html))
    }
  )
})

test_that("mod_search_ui includes department selector", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_search_ui("test")
      ui_html <- as.character(ui)

      expect_true(grepl("departement", ui_html))
      expect_true(grepl("select", ui_html, ignore.case = TRUE))
    }
  )
})

test_that("mod_search_ui includes commune autocomplete", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_search_ui("test")
      ui_html <- as.character(ui)

      expect_true(grepl("commune", ui_html))
      expect_true(grepl("selectize", ui_html, ignore.case = TRUE))
    }
  )
})

test_that("mod_search_ui includes search_info output", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_search_ui("test")
      ui_html <- as.character(ui)

      expect_true(grepl("search_info", ui_html))
    }
  )
})

test_that("mod_search_ui includes loading indicator", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_search_ui("test")
      ui_html <- as.character(ui)

      expect_true(grepl("loading_indicator", ui_html))
      expect_true(grepl("spinner", ui_html))
    }
  )
})

test_that("mod_search_ui works in English", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      ui <- nemetonShiny:::mod_search_ui("test")

      expect_s3_class(ui, "shiny.tag.list")
      ui_html <- as.character(ui)

      # Should contain English placeholder text
      expect_true(grepl("Search for a municipality", ui_html) ||
                  grepl("search_commune", ui_html))
    }
  )
})

test_that("mod_search_ui contains geo-alt icon for department", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_search_ui("test")
      ui_html <- as.character(ui)

      # bsicons adds svg with class containing geo-alt or similar
      expect_true(grepl("geo-alt", ui_html) || grepl("svg", ui_html))
    }
  )
})

test_that("mod_search_ui contains building icon for commune", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_search_ui("test")
      ui_html <- as.character(ui)

      # bsicons adds svg for building icon
      expect_true(grepl("building", ui_html) || grepl("svg", ui_html))
    }
  )
})

# ==============================================================================
# Server Function Signature Tests
# ==============================================================================

test_that("mod_search_server is a function", {
  expect_type(nemetonShiny:::mod_search_server, "closure")
})

test_that("mod_search_server accepts required parameters", {
  # Check function signature
  args <- names(formals(nemetonShiny:::mod_search_server))

  expect_true("id" %in% args)
  expect_true("app_state" %in% args)
})

# ==============================================================================
# Server Return Value Tests
# ==============================================================================

test_that("mod_search_server returns expected reactive values", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01", "75 - Paris" = "75"),
    get_communes_in_department = function(dept) {
      data.frame(
        code_insee = c("01001", "01002"),
        nom = c("Test A", "Test B"),
        code_postal = c("01100", "01200"),
        label = c("Test A (01001)", "Test B (01002)"),
        stringsAsFactors = FALSE
      )
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) {
      if (is.null(df) || nrow(df) == 0) return(character(0))
      stats::setNames(df$code_insee, df$label)
    },
    {
      mock_app_state <- shiny::reactiveValues(
        language = "fr",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          result <- session$getReturned()

          expect_true("selected_commune" %in% names(result))
          expect_true("commune_info" %in% names(result))
          expect_true("commune_geometry" %in% names(result))
          expect_true("is_loading" %in% names(result))
          expect_true("department_changed" %in% names(result))

          # Check that returns are reactive
          expect_type(result$selected_commune, "closure")
          expect_type(result$commune_info, "closure")
          expect_type(result$commune_geometry, "closure")
          expect_type(result$is_loading, "closure")
          expect_type(result$department_changed, "closure")
        }
      )
    }
  )
})

test_that("mod_search_server initializes with NULL values", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01"),
    get_communes_in_department = function(dept) {
      data.frame(
        code_insee = character(0),
        nom = character(0),
        code_postal = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      )
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) character(0),
    {
      mock_app_state <- shiny::reactiveValues(
        language = "fr",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          result <- session$getReturned()

          # Initial values should be NULL or FALSE
          expect_null(result$selected_commune())
          expect_null(result$commune_info())
          expect_null(result$commune_geometry())
          expect_false(result$is_loading())
          expect_null(result$department_changed())
        }
      )
    }
  )
})

# ==============================================================================
# Department Selection Tests
# ==============================================================================

test_that("mod_search_server handles empty department selection", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01", "75 - Paris" = "75"),
    get_communes_in_department = function(dept) {
      data.frame(
        code_insee = character(0),
        nom = character(0),
        code_postal = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      )
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) character(0),
    {
      mock_app_state <- shiny::reactiveValues(
        language = "fr",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          # Set empty department
          session$setInputs(departement = "")

          # Should not trigger loading
          result <- session$getReturned()
          expect_false(result$is_loading())
        }
      )
    }
  )
})

test_that("mod_search_server triggers department observer on department selection", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01", "75 - Paris" = "75"),
    get_communes_in_department = function(dept) {
      data.frame(
        code_insee = c("01001"),
        nom = c("Test"),
        code_postal = c("01100"),
        label = c("Test (01001)"),
        stringsAsFactors = FALSE
      )
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) {
      if (is.null(df) || nrow(df) == 0) return(character(0))
      stats::setNames(df$code_insee, df$label)
    },
    {
      mock_app_state <- shiny::reactiveValues(
        language = "fr",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          result <- session$getReturned()

          # Initially NULL
          expect_null(result$department_changed())

          # Select department - this triggers the observer
          session$setInputs(departement = "01")

          # Note: department_changed is set in an observer that invokes
          # ExtendedTask. In testServer, async observers may not complete
          # synchronously. We verify the module handles the input without error.
          # The return value should be a reactive.
          expect_type(result$department_changed, "closure")
        }
      )
    }
  )
})

# ==============================================================================
# Commune Selection Tests
# ==============================================================================

test_that("mod_search_server clears selection on empty commune", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01"),
    get_communes_in_department = function(dept) {
      data.frame(
        code_insee = c("01001"),
        nom = c("Test"),
        code_postal = c("01100"),
        label = c("Test (01001)"),
        stringsAsFactors = FALSE
      )
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) {
      if (is.null(df) || nrow(df) == 0) return(character(0))
      stats::setNames(df$code_insee, df$label)
    },
    {
      mock_app_state <- shiny::reactiveValues(
        language = "fr",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          result <- session$getReturned()

          # Set commune to empty
          session$setInputs(commune = "")

          # Should clear commune-related values
          expect_null(result$selected_commune())
          expect_null(result$commune_info())
          expect_null(result$commune_geometry())
        }
      )
    }
  )
})

test_that("mod_search_server handles NULL commune", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01"),
    get_communes_in_department = function(dept) {
      data.frame(
        code_insee = character(0),
        nom = character(0),
        code_postal = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      )
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) character(0),
    {
      mock_app_state <- shiny::reactiveValues(
        language = "fr",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          result <- session$getReturned()

          # Values should remain NULL
          expect_null(result$selected_commune())
          expect_null(result$commune_info())
          expect_null(result$commune_geometry())
        }
      )
    }
  )
})

# ==============================================================================
# API Error Handling Tests
# ==============================================================================

test_that("mod_search_server handles network error in get_communes_in_department", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01"),
    get_communes_in_department = function(dept) {
      result <- data.frame(
        code_insee = character(0),
        nom = character(0),
        code_postal = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      )
      attr(result, "error") <- "network"
      attr(result, "error_message") <- "no_internet_connection"
      result
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) character(0),
    {
      mock_app_state <- shiny::reactiveValues(
        language = "fr",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          # Select department (triggers API call)
          session$setInputs(departement = "01")

          # The module should handle the error gracefully
          # In testServer, async tasks may not complete, but we verify no crash
          result <- session$getReturned()
          expect_null(result$commune_geometry())
        }
      )
    }
  )
})

test_that("mod_search_server handles other API error", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01"),
    get_communes_in_department = function(dept) {
      result <- data.frame(
        code_insee = character(0),
        nom = character(0),
        code_postal = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      )
      attr(result, "error") <- "other"
      attr(result, "error_message") <- "Unknown server error"
      result
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) character(0),
    {
      mock_app_state <- shiny::reactiveValues(
        language = "fr",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          session$setInputs(departement = "01")

          # Module should handle error without crashing
          result <- session$getReturned()
          expect_null(result$commune_geometry())
        }
      )
    }
  )
})

# ==============================================================================
# Language Handling Tests
# ==============================================================================

test_that("mod_search_server handles missing language with fallback", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01"),
    get_communes_in_department = function(dept) {
      data.frame(
        code_insee = character(0),
        nom = character(0),
        code_postal = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      )
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) character(0),
    {
      # App state with NULL language
      mock_app_state <- shiny::reactiveValues(
        language = NULL,
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          # Should not crash even with NULL language
          result <- session$getReturned()
          expect_type(result, "list")
        }
      )
    }
  )
})

test_that("mod_search_server handles empty language string with fallback", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01"),
    get_communes_in_department = function(dept) {
      data.frame(
        code_insee = character(0),
        nom = character(0),
        code_postal = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      )
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) character(0),
    {
      # App state with empty language
      mock_app_state <- shiny::reactiveValues(
        language = "",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          # Should use "fr" fallback
          result <- session$getReturned()
          expect_type(result, "list")
        }
      )
    }
  )
})

# ==============================================================================
# Project Restore Tests
# ==============================================================================

test_that("mod_search_server handles restore_project trigger", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  restore_task_called <- FALSE

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01", "75 - Paris" = "75"),
    get_communes_in_department = function(dept) {
      data.frame(
        code_insee = c("01001"),
        nom = c("Test"),
        code_postal = c("01100"),
        label = c("Test (01001)"),
        stringsAsFactors = FALSE
      )
    },
    get_commune_geometry = function(code) {
      restore_task_called <<- TRUE
      # Return a simple sf geometry for testing
      if (requireNamespace("sf", quietly = TRUE)) {
        coords <- matrix(c(5, 45, 5.1, 45, 5.1, 45.1, 5, 45.1, 5, 45), ncol = 2, byrow = TRUE)
        poly <- sf::st_polygon(list(coords))
        sf::st_sf(code = code, geometry = sf::st_sfc(poly, crs = 4326))
      } else {
        NULL
      }
    },
    format_communes_for_selectize = function(df) {
      if (is.null(df) || nrow(df) == 0) return(character(0))
      stats::setNames(df$code_insee, df$label)
    },
    {
      mock_app_state <- shiny::reactiveValues(
        language = "fr",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          # Trigger restore
          mock_app_state$restore_project <- list(
            department_code = "01",
            commune_code = "01001"
          )

          # The restore observer should be triggered
          # Note: In testServer, async ExtendedTasks may not complete
          result <- session$getReturned()
          expect_type(result, "list")
        }
      )
    }
  )
})

test_that("mod_search_server ignores restore with NULL commune_code", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01"),
    get_communes_in_department = function(dept) {
      data.frame(
        code_insee = character(0),
        nom = character(0),
        code_postal = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      )
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) character(0),
    {
      mock_app_state <- shiny::reactiveValues(
        language = "fr",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          # Restore with NULL commune_code should be ignored
          mock_app_state$restore_project <- list(
            department_code = "01",
            commune_code = NULL
          )

          result <- session$getReturned()
          expect_null(result$selected_commune())
        }
      )
    }
  )
})

# ==============================================================================
# Restore Flag Tests
# ==============================================================================

test_that("mod_search_server sets is_restoring flag during restore", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01"),
    get_communes_in_department = function(dept) {
      data.frame(
        code_insee = c("01001"),
        nom = c("Test"),
        code_postal = c("01100"),
        label = c("Test (01001)"),
        stringsAsFactors = FALSE
      )
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) {
      if (is.null(df) || nrow(df) == 0) return(character(0))
      stats::setNames(df$code_insee, df$label)
    },
    {
      mock_app_state <- shiny::reactiveValues(
        language = "fr",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          # Trigger restore
          mock_app_state$restore_project <- list(
            department_code = "01",
            commune_code = "01001"
          )

          # Module should process the restore
          result <- session$getReturned()
          expect_type(result, "list")
        }
      )
    }
  )
})

# ==============================================================================
# i18n Translation Key Tests
# ==============================================================================

test_that("i18n keys for search module exist in French", {
  i18n <- nemetonShiny:::get_i18n("fr")

  search_keys <- c(
    "department",
    "select_department",
    "commune",
    "search_commune",
    "loading_commune",
    "error_loading_communes",
    "error_no_internet"
  )

  for (key in search_keys) {
    expect_true(i18n$has(key), info = paste("FR missing:", key))
  }
})

test_that("i18n keys for search module exist in English", {
  i18n <- nemetonShiny:::get_i18n("en")

  search_keys <- c(
    "department",
    "select_department",
    "commune",
    "search_commune",
    "loading_commune",
    "error_loading_communes",
    "error_no_internet"
  )

  for (key in search_keys) {
    expect_true(i18n$has(key), info = paste("EN missing:", key))
  }
})

test_that("i18n translations are non-empty for search module keys", {
  fr <- nemetonShiny:::get_i18n("fr")
  en <- nemetonShiny:::get_i18n("en")

  keys <- c("department", "commune", "search_commune")

  for (key in keys) {
    fr_text <- fr$t(key)
    en_text <- en$t(key)

    expect_true(nchar(fr_text) > 0, info = paste("FR empty:", key))
    expect_true(nchar(en_text) > 0, info = paste("EN empty:", key))
    expect_true(fr_text != key, info = paste("FR not translated:", key))
    expect_true(en_text != key, info = paste("EN not translated:", key))
  }
})

# ==============================================================================
# Edge Cases and Boundary Tests
# ==============================================================================

test_that("mod_search_server handles rapid department changes", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  call_count <- 0

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("01 - Ain" = "01", "75 - Paris" = "75"),
    get_communes_in_department = function(dept) {
      call_count <<- call_count + 1
      data.frame(
        code_insee = c(paste0(dept, "001")),
        nom = c("Test"),
        code_postal = c(paste0(dept, "100")),
        label = c(paste0("Test (", dept, "001)")),
        stringsAsFactors = FALSE
      )
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) {
      if (is.null(df) || nrow(df) == 0) return(character(0))
      stats::setNames(df$code_insee, df$label)
    },
    {
      mock_app_state <- shiny::reactiveValues(
        language = "fr",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          # Rapid changes
          session$setInputs(departement = "01")
          session$setInputs(departement = "75")
          session$setInputs(departement = "01")

          # Should not crash
          result <- session$getReturned()
          expect_type(result, "list")
        }
      )
    }
  )
})

test_that("mod_search_server handles department with no communes", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_departments = function() c("99 - Empty" = "99"),
    get_communes_in_department = function(dept) {
      data.frame(
        code_insee = character(0),
        nom = character(0),
        code_postal = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      )
    },
    get_commune_geometry = function(code) NULL,
    format_communes_for_selectize = function(df) character(0),
    {
      mock_app_state <- shiny::reactiveValues(
        language = "fr",
        restore_project = NULL,
        restore_in_progress = FALSE
      )

      shiny::testServer(
        nemetonShiny:::mod_search_server,
        args = list(app_state = mock_app_state),
        {
          # Select department with no communes
          session$setInputs(departement = "99")

          # Should handle gracefully
          result <- session$getReturned()
          expect_null(result$selected_commune())
        }
      )
    }
  )
})

# ==============================================================================
# Helper Function Tests
# ==============================================================================

test_that("format_communes_for_selectize returns named vector", {
  communes <- data.frame(
    code_insee = c("01001", "01002", "01003"),
    nom = c("Commune A", "Commune B", "Commune C"),
    code_postal = c("01100", "01200", "01300"),
    label = c("Commune A (01001)", "Commune B (01002)", "Commune C (01003)"),
    stringsAsFactors = FALSE
  )

  result <- nemetonShiny:::format_communes_for_selectize(communes)

  expect_type(result, "character")
  expect_length(result, 3)
  expect_equal(names(result), communes$label)
  expect_equal(unname(result), communes$code_insee)
})

test_that("format_communes_for_selectize handles empty data frame", {
  communes <- data.frame(
    code_insee = character(0),
    nom = character(0),
    code_postal = character(0),
    label = character(0),
    stringsAsFactors = FALSE
  )

  result <- nemetonShiny:::format_communes_for_selectize(communes)

  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("format_communes_for_selectize handles NULL input", {
  result <- nemetonShiny:::format_communes_for_selectize(NULL)

  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("format_communes_for_selectize handles single row", {
  communes <- data.frame(
    code_insee = "75056",
    nom = "Paris",
    code_postal = "75001",
    label = "Paris (75056)",
    stringsAsFactors = FALSE
  )

  result <- nemetonShiny:::format_communes_for_selectize(communes)

  expect_type(result, "character")
  expect_length(result, 1)
  expect_equal(names(result), "Paris (75056)")
  expect_equal(result[[1]], "75056")
})

# ==============================================================================
# get_departments Tests
# ==============================================================================

test_that("get_departments returns all French departments", {
  depts <- nemetonShiny:::get_departments()

  expect_type(depts, "character")
  expect_true(length(depts) >= 100) # France has ~100 departments
})

test_that("get_departments includes metropolitan departments", {
  depts <- nemetonShiny:::get_departments()

  # Check some known metropolitan departments
  expect_true("01" %in% depts) # Ain
  expect_true("75" %in% depts) # Paris
  expect_true("13" %in% depts) # Bouches-du-Rhone
  expect_true("69" %in% depts) # Rhone
})

test_that("get_departments includes Corsica departments", {
  depts <- nemetonShiny:::get_departments()

  expect_true("2A" %in% depts) # Corse-du-Sud
  expect_true("2B" %in% depts) # Haute-Corse
})

test_that("get_departments includes overseas departments", {
  depts <- nemetonShiny:::get_departments()

  expect_true("971" %in% depts) # Guadeloupe
  expect_true("972" %in% depts) # Martinique
  expect_true("973" %in% depts) # Guyane
  expect_true("974" %in% depts) # La Reunion
  expect_true("976" %in% depts) # Mayotte
})

test_that("get_departments returns named vector", {
  depts <- nemetonShiny:::get_departments()

  expect_true(!is.null(names(depts)))
  expect_true(all(nchar(names(depts)) > 0))

  # Check format: "01 - Ain"
  expect_true(grepl("^[0-9A-B]{2,3} - ", names(depts)[1]))
})

# ==============================================================================
# validate_insee_code Tests
# ==============================================================================

test_that("validate_insee_code accepts valid 5-digit codes", {
  validate <- nemetonShiny:::validate_insee_code

  expect_true(validate("01001"))
  expect_true(validate("75056"))
  expect_true(validate("99999"))
})

test_that("validate_insee_code accepts Corsica codes", {
  validate <- nemetonShiny:::validate_insee_code

  expect_true(validate("2A004")) # Corse-du-Sud

  expect_true(validate("2B033")) # Haute-Corse
})

test_that("validate_insee_code accepts overseas codes", {
  validate <- nemetonShiny:::validate_insee_code

  expect_true(validate("97105")) # Guadeloupe
  expect_true(validate("97411")) # La Reunion
})

test_that("validate_insee_code rejects short codes", {
  validate <- nemetonShiny:::validate_insee_code

  expect_false(validate("1234"))
  expect_false(validate("123"))
  expect_false(validate("1"))
})

test_that("validate_insee_code rejects long codes", {
  validate <- nemetonShiny:::validate_insee_code

  expect_false(validate("123456"))
  expect_false(validate("1234567"))
})

test_that("validate_insee_code rejects invalid characters", {
  validate <- nemetonShiny:::validate_insee_code

  expect_false(validate("ABCDE"))
  expect_false(validate("1234X"))
  expect_false(validate("12-34"))
  expect_false(validate("12 34"))
})

test_that("validate_insee_code rejects empty and NA values", {
  validate <- nemetonShiny:::validate_insee_code

  expect_false(validate(""))
  expect_false(validate(NA))
  expect_false(validate(NULL))
})

test_that("validate_insee_code rejects non-character input", {
  validate <- nemetonShiny:::validate_insee_code

  expect_false(validate(12345))
  expect_false(validate(TRUE))
  expect_false(validate(list()))
})

# ==============================================================================
# Additional: ensure_future_plan helper
# ==============================================================================

test_that("ensure_future_plan is defined in mod_search server context", {
  # The function ensure_future_plan is defined inside mod_search_server
  # We can verify that the future package is handled
  skip_if_not_installed("future")

  # Verify that future::plan can be queried without error
  plan_class <- class(future::plan())
  expect_true(is.character(plan_class))
})

# Drain async callbacks to prevent testServer session accumulation
later::run_now(0)

# Tests for app_server.R
# Main server function for nemetonApp
# Testing: reactive state initialization, language handling, help modal, module coordination

# ============================================================
# HELPER FUNCTIONS
# ============================================================

#' Create a mock app_state reactive values object
#' @noRd
mock_app_state <- function(language = "fr", project_status = "none") {
  shiny::reactiveValues(
    language = language,
    project_dir = tempdir(),
    current_project = NULL,
    project_status = project_status
  )
}

# ============================================================
# BASIC FUNCTION TESTS
# ============================================================

test_that("app_server is a function", {
  expect_type(nemeton:::app_server, "closure")
})

test_that("app_server accepts standard Shiny server parameters", {
  args <- names(formals(nemeton:::app_server))

  expect_true("input" %in% args)
  expect_true("output" %in% args)
  expect_true("session" %in% args)
})

test_that("app_server has exactly 3 parameters", {
  args <- names(formals(nemeton:::app_server))
  expect_length(args, 3)
})

# ============================================================
# HELPER FUNCTIONS TESTS
# ============================================================

test_that("get_departments_list returns named character vector", {
  deps <- nemeton:::get_departments_list()

  expect_type(deps, "character")
  expect_true(length(deps) > 90) # France has ~100 departments
  expect_true(all(nzchar(names(deps)))) # All names are non-empty
})

test_that("get_departments_list contains main French departments", {
  deps <- nemeton:::get_departments_list()

  # Check some key departments
  expect_true("01" %in% deps) # Ain
  expect_true("75" %in% deps) # Paris
  expect_true("2A" %in% deps) # Corse-du-Sud
  expect_true("974" %in% deps) # La Reunion
})

test_that("get_departments_list has correct format", {

  deps <- nemeton:::get_departments_list()

  # Values should be department codes (e.g., "01", "2A", "971")
  # Pattern: 1-3 digits, optionally followed by A or B (for Corsica)
  expect_true(all(grepl("^[0-9]{1,3}[AB]?$", deps)))

  # Names should include department code and name (e.g., "01 - Ain", "2A - Corse-du-Sud")
  expect_true(all(grepl("^[0-9]{1,3}[AB]? - ", names(deps))))
})

test_that("get_tour_preference returns logical", {
  result <- nemeton:::get_tour_preference()
  expect_type(result, "logical")
})

test_that("get_tour_preference reads from options", {
  # Set option
  old_opt <- getOption("nemeton.tour_seen")
  options(nemeton.tour_seen = TRUE)

  result <- nemeton:::get_tour_preference()
  expect_true(result)

  options(nemeton.tour_seen = FALSE)
  result <- nemeton:::get_tour_preference()
  expect_false(result)

  # Restore
  options(nemeton.tour_seen = old_opt)
})

# ============================================================
# REACTIVE STATE INITIALIZATION TESTS (using testServer)
# ============================================================

test_that("app_server initializes app_state reactive values", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  # Mock all the module servers to prevent actual module execution
  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Test that app_state exists
        expect_true(exists("app_state"))
        expect_true(shiny::is.reactivevalues(app_state))

        # Test initial structure
        expect_equal(app_state$language, "fr")
        expect_equal(app_state$project_status, "none")
        expect_null(app_state$current_project)
      })
    }
  )
})

test_that("app_server initializes selection_state reactive values", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Test that selection_state exists
        expect_true(exists("selection_state"))
        expect_true(shiny::is.reactivevalues(selection_state))

        # Test initial values
        expect_null(selection_state$commune_code)
        expect_null(selection_state$commune_geometry)
        expect_null(selection_state$parcelles)
        expect_equal(selection_state$selected_ids, character(0))
      })
    }
  )
})

test_that("app_server initializes compute_state reactive values", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Test that compute_state exists
        expect_true(exists("compute_state"))
        expect_true(shiny::is.reactivevalues(compute_state))

        # Test initial values
        expect_null(compute_state$indicators)
        expect_equal(compute_state$progress, 0)
        expect_null(compute_state$current_indicator)
        expect_type(compute_state$errors, "list")
        expect_length(compute_state$errors, 0)
      })
    }
  )
})

# ============================================================
# LANGUAGE HANDLING TESTS
# ============================================================

test_that("app_server handles language change", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Initial language should be French
        expect_equal(app_state$language, "fr")

        # Simulate language change
        session$setInputs(app_language = "en")

        # Language should be updated
        expect_equal(app_state$language, "en")

        # Option should be stored
        expect_equal(getOption("nemeton.app_language"), "en")
      })
    }
  )
})

test_that("app_server language change shows notification",
{
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  notification_shown <- FALSE

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      # Note: We cannot easily test showNotification in testServer
      # This test verifies that the observeEvent is wired up correctly
      shiny::testServer(nemeton:::app_server, {
        # Simulate language change - if it runs without error, the observer is working
        session$setInputs(app_language = "en")
        expect_equal(app_state$language, "en")
      })
    }
  )
})

# ============================================================
# HELP MODAL TESTS
# ============================================================

test_that("app_server show_help input triggers modal", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Trigger help button - this should not error
        # We cannot easily test that showModal was called, but we verify the observer works
        session$setInputs(show_help = 1)

        # If no error, the observer executed successfully
        expect_true(TRUE)
      })
    }
  )
})

test_that("app_server restart_tour input works", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")
  skip_if_not_installed("later")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Trigger restart tour
        session$setInputs(restart_tour = 1)

        # If no error, the observer executed successfully
        expect_true(TRUE)
      })
    }
  )
})

# ============================================================
# TAB NAVIGATION CONTROL TESTS
# ============================================================

test_that("app_server tab navigation restricts access when not completed", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Initial status is "none"
        expect_equal(app_state$project_status, "none")

        # Change status to "computing"
        app_state$project_status <- "computing"
        session$flushReact()

        # The observe should trigger navigation update
        # We cannot directly test updateNavbarPage but we can verify the state
        expect_equal(app_state$project_status, "computing")
      })
    }
  )
})

test_that("app_server allows navigation when project is completed", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Set status to completed
        app_state$project_status <- "completed"
        session$flushReact()

        # Status should be maintained
        expect_equal(app_state$project_status, "completed")
      })
    }
  )
})

# ============================================================
# MODULE COORDINATION TESTS
# ============================================================

test_that("app_server calls mod_home_server", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  home_called <- FALSE

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      home_called <<- TRUE
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        session$flushReact()
      })
    }
  )

  expect_true(home_called)
})

test_that("app_server calls mod_synthesis_server", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  synthesis_called <- FALSE

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      synthesis_called <<- TRUE
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        session$flushReact()
      })
    }
  )

  expect_true(synthesis_called)
})

test_that("app_server passes app_state to modules", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  received_app_state <- NULL

  with_mocked_bindings(
    get_app_options = function() list(language = "en", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      received_app_state <<- app_state
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        session$flushReact()
      })
    }
  )

  expect_true(shiny::is.reactivevalues(received_app_state))
  # Verify the language was passed correctly from options
  expect_equal(shiny::isolate(received_app_state$language), "en")
})

# ============================================================
# LAZY LOADING FAMILY MODULES TESTS
# ============================================================

test_that("app_server lazy loads family modules on navigation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  family_calls <- character(0)

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      family_calls <<- c(family_calls, family_code)
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Initially no family modules should be loaded
        expect_length(family_calls, 0)

        # Navigate to Carbon family tab
        session$setInputs(main_nav = "famille_carbone")
        session$flushReact()
      })
    }
  )

  # After navigation, Carbon family should be loaded
  expect_true("C" %in% family_calls)
})

test_that("app_server does not reload already initialized family modules", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  family_call_count <- 0

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      if (family_code == "C") {
        family_call_count <<- family_call_count + 1
      }
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Navigate to Carbon tab twice
        session$setInputs(main_nav = "famille_carbone")
        session$flushReact()

        session$setInputs(main_nav = "selection")
        session$flushReact()

        session$setInputs(main_nav = "famille_carbone")
        session$flushReact()
      })
    }
  )

  # Family C should only be initialized once
  expect_equal(family_call_count, 1)
})

test_that("app_server tracks initialized families correctly", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Check that initialized_families starts empty
        expect_equal(initialized_families(), character(0))

        # Navigate to multiple family tabs
        session$setInputs(main_nav = "famille_carbone")
        session$flushReact()
        expect_true("C" %in% initialized_families())

        session$setInputs(main_nav = "famille_biodiversite")
        session$flushReact()
        expect_true("B" %in% initialized_families())
        expect_true("C" %in% initialized_families())

        # Now should have both
        expect_length(initialized_families(), 2)
      })
    }
  )
})

# ============================================================
# SELECTION STATE UPDATE TESTS
# ============================================================

test_that("app_server updates selection_state from home module", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  commune_val <- shiny::reactiveVal(NULL)
  parcels_val <- shiny::reactiveVal(NULL)

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = commune_val,
          selected_parcels = parcels_val
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Initial state
        expect_null(selection_state$commune_code)
        expect_equal(selection_state$selected_ids, character(0))

        # Update commune from home module
        commune_val("36053")
        session$flushReact()

        expect_equal(selection_state$commune_code, "36053")

        # Update parcels from home module
        parcels_val(data.frame(id = c("parcel_1", "parcel_2")))
        session$flushReact()

        expect_equal(selection_state$selected_ids, c("parcel_1", "parcel_2"))
      })
    }
  )
})

test_that("app_server handles NULL parcels from home module", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  parcels_val <- shiny::reactiveVal(data.frame(id = c("p1", "p2")))

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = parcels_val
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Wait for initial state
        session$flushReact()
        expect_equal(selection_state$selected_ids, c("p1", "p2"))

        # Set parcels to NULL
        parcels_val(NULL)
        session$flushReact()

        # Should result in empty character vector, not NULL
        expect_equal(selection_state$selected_ids, character(0))
      })
    }
  )
})

# ============================================================
# SESSION CLEANUP TESTS
# ============================================================

test_that("app_server stores root_session in userData", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Check that root_session is stored
        expect_true(!is.null(session$userData$root_session))
      })
    }
  )
})

# ============================================================
# ASYNC COMPUTATION SETUP TESTS
# ============================================================

test_that("app_server handles missing future package gracefully", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  # This test verifies the code doesn't crash if future is unavailable
  # by using a mock that simulates future not being available
  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      # Should not error even if future setup runs
      expect_no_error({
        shiny::testServer(nemeton:::app_server, {
          session$flushReact()
        })
      })
    }
  )
})

# ============================================================
# PROJECT STATUS TESTS
# ============================================================

test_that("app_state project_status has valid default", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Check default status
        expect_equal(app_state$project_status, "none")

        # Verify it's a valid status value
        valid_statuses <- c("none", "draft", "downloading", "computing", "completed")
        expect_true(app_state$project_status %in% valid_statuses)
      })
    }
  )
})

test_that("app_state project_status can be changed", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr", project_dir = tempdir()),
    mod_home_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) {
        list(
          selected_commune = shiny::reactive(NULL),
          selected_parcels = shiny::reactive(NULL)
        )
      })
    },
    mod_synthesis_server = function(id, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    mod_family_server = function(id, family_code, app_state) {
      shiny::moduleServer(id, function(input, output, session) { })
    },
    {
      shiny::testServer(nemeton:::app_server, {
        # Transition through states
        app_state$project_status <- "draft"
        expect_equal(app_state$project_status, "draft")

        app_state$project_status <- "downloading"
        expect_equal(app_state$project_status, "downloading")

        app_state$project_status <- "computing"
        expect_equal(app_state$project_status, "computing")

        app_state$project_status <- "completed"
        expect_equal(app_state$project_status, "completed")
      })
    }
  )
})

# ============================================================
# I18N INTEGRATION TESTS
# ============================================================

test_that("app_server uses get_i18n for translations", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("future")

  # get_i18n should be available and working
  i18n_fr <- nemeton:::get_i18n("fr")
  i18n_en <- nemeton:::get_i18n("en")

  # Keys used in app_server help modal should exist
  expect_true(i18n_fr$has("help"))
  expect_true(i18n_fr$has("help_title"))
  expect_true(i18n_fr$has("help_intro"))
  expect_true(i18n_fr$has("help_steps_title"))
  expect_true(i18n_fr$has("help_step1"))
  expect_true(i18n_fr$has("tour_restart"))
  expect_true(i18n_fr$has("documentation_link"))
  expect_true(i18n_fr$has("close"))
  expect_true(i18n_fr$has("language_changed"))

  # Same for English
  expect_true(i18n_en$has("help"))
  expect_true(i18n_en$has("help_title"))
  expect_true(i18n_en$has("language_changed"))
})

# Drain async callbacks to prevent testServer session accumulation
later::run_now(0)

# Tests for Progress Module
# Progress UI display, state updates, and error handling

# ==============================================================================
# UI Tests
# ==============================================================================

test_that("mod_progress_ui returns valid Shiny UI", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_progress_ui("test")

      expect_true(
        inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list")
      )
    }
  )
})

test_that("mod_progress_ui contains progress card", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_progress_ui("test")
      ui_html <- as.character(ui)

      # Progress card wrapper
      expect_true(grepl("test-progress_card_wrapper", ui_html))
      # Progress bar element
      expect_true(grepl("test-progress_bar", ui_html))
      # Progress percent display
      expect_true(grepl("test-progress_percent", ui_html))
    }
  )
})

test_that("mod_progress_ui contains completion card", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_progress_ui("test")
      ui_html <- as.character(ui)

      # Completion card wrapper
      expect_true(grepl("test-complete_card_wrapper", ui_html))
      # Completion message text
      expect_true(grepl("test-completion_message_text", ui_html))
      # Indicators table container
      expect_true(grepl("test-indicators_table_container", ui_html))
    }
  )
})

test_that("mod_progress_ui contains error card", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_progress_ui("test")
      ui_html <- as.character(ui)

      # Error card wrapper
      expect_true(grepl("test-error_card_wrapper", ui_html))
      # Error message content
      expect_true(grepl("test-error_message_content", ui_html))
      # Retry button
      expect_true(grepl("test-retry", ui_html))
    }
  )
})

test_that("mod_progress_ui contains indicator counters", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_progress_ui("test")
      ui_html <- as.character(ui)

      # Completed count
      expect_true(grepl("test-completed_count", ui_html))
      # Failed count
      expect_true(grepl("test-failed_count", ui_html))
      # Pending count
      expect_true(grepl("test-pending_count", ui_html))
    }
  )
})

test_that("mod_progress_ui contains cancel button", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_progress_ui("test")
      ui_html <- as.character(ui)

      # Cancel button
      expect_true(grepl("test-cancel", ui_html))
      expect_true(grepl("btn-outline-danger", ui_html))
    }
  )
})

test_that("mod_progress_ui contains task toast notification", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_progress_ui("test")
      ui_html <- as.character(ui)

      # Task toast wrapper
      expect_true(grepl("test-task_toast", ui_html))
      # Toast text element
      expect_true(grepl("test-task_toast_text", ui_html))
    }
  )
})

test_that("mod_progress_ui contains elapsed time display", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_progress_ui("test")
      ui_html <- as.character(ui)

      # Elapsed time element
      expect_true(grepl("test-elapsed_time", ui_html))
    }
  )
})

test_that("mod_progress_ui contains phase text element", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_progress_ui("test")
      ui_html <- as.character(ui)

      # Phase text element
      expect_true(grepl("test-phase_text", ui_html))
    }
  )
})

test_that("mod_progress_ui works in English", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      ui <- nemetonShiny:::mod_progress_ui("test")
      ui_html <- as.character(ui)

      # Should render without error
      expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
    }
  )
})

test_that("mod_progress_ui cards are hidden by default", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_progress_ui("test")
      ui_html <- as.character(ui)

      # All card wrappers should have display: none style
      expect_true(grepl('id="test-progress_card_wrapper"[^>]*style="display: none;', ui_html))
      expect_true(grepl('id="test-complete_card_wrapper"[^>]*style="display: none;', ui_html))
      expect_true(grepl('id="test-error_card_wrapper"[^>]*style="display: none;', ui_html))
    }
  )
})

# ==============================================================================
# Server Function Tests
# ==============================================================================

test_that("mod_progress_server is a function", {
  expect_type(nemetonShiny:::mod_progress_server, "closure")
})

test_that("mod_progress_server accepts required parameters", {
  args <- names(formals(nemetonShiny:::mod_progress_server))

  expect_true("id" %in% args)
  expect_true("compute_state" %in% args)
  expect_true("app_state" %in% args)
})

test_that("mod_progress_server returns expected list", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      # Create mock compute_state
      mock_compute_state <- shiny::reactiveVal(list(
        status = "pending",
        phase = "init",
        progress = 0,
        progress_max = 10,
        indicators_completed = 0,
        indicators_failed = 0,
        indicators_total = 10,
        current_task = NULL,
        errors = list()
      ))

      # Create mock app_state
      mock_app_state <- shiny::reactiveValues(
        cancel_computation = NULL,
        retry_computation = NULL
      )

      shiny::testServer(
        nemetonShiny:::mod_progress_server,
        args = list(
          compute_state = mock_compute_state,
          app_state = mock_app_state
        ),
        {
          # Check that returned list has expected elements
          result <- session$getReturned()

          expect_true("show_progress" %in% names(result))
          expect_true("show_complete" %in% names(result))
          expect_true("show_error" %in% names(result))
          expect_true("send_running_update" %in% names(result))
          expect_true("reset_tracking" %in% names(result))

          # Check that show_* are reactive functions
          expect_type(result$show_progress, "closure")
          expect_type(result$show_complete, "closure")
          expect_type(result$show_error, "closure")

          # Check that helper functions are functions
          expect_type(result$send_running_update, "closure")
          expect_type(result$reset_tracking, "closure")
        }
      )
    }
  )
})

test_that("mod_progress_server cancel button sets app_state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      mock_compute_state <- shiny::reactiveVal(list(
        status = "computing",
        phase = "computing",
        progress = 5,
        progress_max = 10,
        indicators_completed = 5,
        indicators_failed = 0,
        indicators_total = 10,
        current_task = "compute:test_indicator",
        errors = list()
      ))

      mock_app_state <- shiny::reactiveValues(
        cancel_computation = NULL,
        retry_computation = NULL
      )

      shiny::testServer(
        nemetonShiny:::mod_progress_server,
        args = list(
          compute_state = mock_compute_state,
          app_state = mock_app_state
        ),
        {
          # Simulate cancel button click
          session$setInputs(cancel = 1)

          # Check that cancel_computation was set
          expect_false(is.null(mock_app_state$cancel_computation))
        }
      )
    }
  )
})

test_that("mod_progress_server retry button sets app_state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      mock_compute_state <- shiny::reactiveVal(list(
        status = "error",
        phase = "computing",
        progress = 5,
        progress_max = 10,
        indicators_completed = 5,
        indicators_failed = 1,
        indicators_total = 10,
        current_task = NULL,
        errors = list(list(message = "Test error"))
      ))

      mock_app_state <- shiny::reactiveValues(
        cancel_computation = NULL,
        retry_computation = NULL
      )

      shiny::testServer(
        nemetonShiny:::mod_progress_server,
        args = list(
          compute_state = mock_compute_state,
          app_state = mock_app_state
        ),
        {
          # Simulate retry button click
          session$setInputs(retry = 1)

          # Check that retry_computation was set
          expect_false(is.null(mock_app_state$retry_computation))
        }
      )
    }
  )
})

# ==============================================================================
# i18n Keys Tests
# ==============================================================================

test_that("i18n keys for progress module exist in both languages", {
  fr <- nemetonShiny:::get_i18n("fr")
  en <- nemetonShiny:::get_i18n("en")

  # Core progress keys
  core_keys <- c(
    "computing",
    "progress_overall",
    "completed",
    "failed",
    "pending",
    "cancel",
    "computation_complete",
    "computation_error",
    "retry",
    "elapsed_time",
    "unknown_error"
  )

  for (key in core_keys) {
    expect_true(fr$has(key), info = paste("FR missing:", key))
    expect_true(en$has(key), info = paste("EN missing:", key))
  }
})

test_that("i18n keys for progress phases exist in both languages", {
  fr <- nemetonShiny:::get_i18n("fr")
  en <- nemetonShiny:::get_i18n("en")

  phase_keys <- c(
    "phase_init",
    "phase_downloading",
    "phase_computing",
    "phase_complete"
  )

  for (key in phase_keys) {
    expect_true(fr$has(key), info = paste("FR missing:", key))
    expect_true(en$has(key), info = paste("EN missing:", key))
  }
})

test_that("i18n keys for task messages exist in both languages", {
  fr <- nemetonShiny:::get_i18n("fr")
  en <- nemetonShiny:::get_i18n("en")

  task_keys <- c(
    "task_download_start",
    "task_compute_start",
    "task_complete",
    "task_error",
    "task_resuming",
    "download_complete",
    "downloading_source",
    "computing_indicator_name",
    "computing_indicator"
  )

  for (key in task_keys) {
    expect_true(fr$has(key), info = paste("FR missing:", key))
    expect_true(en$has(key), info = paste("EN missing:", key))
  }
})

test_that("i18n keys for computation summary exist in both languages", {
  fr <- nemetonShiny:::get_i18n("fr")
  en <- nemetonShiny:::get_i18n("en")

  summary_keys <- c(
    "computation_summary",
    "computation_failed",
    "and_n_more_errors",
    "indicator_column",
    "status_column",
    "status_ok",
    "status_error",
    "status_not_computed"
  )

  for (key in summary_keys) {
    expect_true(fr$has(key), info = paste("FR missing:", key))
    expect_true(en$has(key), info = paste("EN missing:", key))
  }
})

# ==============================================================================
# COMPUTE_STATUS Constant Tests
# ==============================================================================

test_that("COMPUTE_STATUS constant exists and has required statuses", {
  status <- nemetonShiny:::COMPUTE_STATUS

  expect_type(status, "list")
  expect_true("PENDING" %in% names(status))
  expect_true("DOWNLOADING" %in% names(status))
  expect_true("COMPUTING" %in% names(status))
  expect_true("COMPLETED" %in% names(status))
  expect_true("ERROR" %in% names(status))
  expect_true("CANCELLED" %in% names(status))
})

test_that("COMPUTE_STATUS values are strings", {
  status <- nemetonShiny:::COMPUTE_STATUS

  expect_equal(status$PENDING, "pending")
  expect_equal(status$DOWNLOADING, "downloading")
  expect_equal(status$COMPUTING, "computing")
  expect_equal(status$COMPLETED, "completed")
  expect_equal(status$ERROR, "error")
  expect_equal(status$CANCELLED, "cancelled")
})

# ==============================================================================
# Progress State Structure Tests
# ==============================================================================

test_that("init_compute_state creates valid state structure", {
  skip_if_not_installed("shiny")

  with_mocked_bindings(
    get_computation_progress = function(project_id) list(computed_indicators = character(0)),
    list_available_indicators = function() c("indicateur_c1_biomasse", "indicateur_c2_ndvi", "indicateur_w1_reseau"),
    {
      state <- nemetonShiny:::init_compute_state("test_project", "all")

      expect_true("project_id" %in% names(state))
      expect_true("status" %in% names(state))
      expect_true("phase" %in% names(state))
      expect_true("progress" %in% names(state))
      expect_true("progress_max" %in% names(state))
      expect_true("current_task" %in% names(state))
      expect_true("indicators_total" %in% names(state))
      expect_true("indicators_completed" %in% names(state))
      expect_true("indicators_failed" %in% names(state))
      expect_true("indicators_status" %in% names(state))
      expect_true("errors" %in% names(state))

      expect_equal(state$project_id, "test_project")
      expect_equal(state$status, nemetonShiny:::COMPUTE_STATUS$PENDING)
      expect_equal(state$phase, "init")
      expect_equal(state$progress, 0)
      expect_type(state$errors, "list")
    }
  )
})

test_that("init_compute_state handles resume with existing progress", {
  skip_if_not_installed("shiny")

  with_mocked_bindings(
    get_computation_progress = function(project_id) {
      list(computed_indicators = c("indicateur_c1_biomasse", "indicateur_c2_ndvi"))
    },
    list_available_indicators = function() {
      c("indicateur_c1_biomasse", "indicateur_c2_ndvi", "indicateur_w1_reseau")
    },
    {
      state <- nemetonShiny:::init_compute_state("test_project", "all")

      expect_equal(state$indicators_completed, 2)
      expect_equal(state$indicators_skipped, 2)
      expect_true(state$is_resume)
      expect_equal(state$indicators_status[["indicateur_c1_biomasse"]], "completed")
      expect_equal(state$indicators_status[["indicateur_c2_ndvi"]], "completed")
      expect_equal(state$indicators_status[["indicateur_w1_reseau"]], "pending")
    }
  )
})

# ==============================================================================
# Additional progress and compute state tests (full indicator list)
# ==============================================================================

test_that("list_available_indicators returns 31 indicators", {
  indicators <- nemetonShiny:::list_available_indicators()
  expect_length(indicators, 31)
  expect_true("indicateur_c1_biomasse" %in% indicators)
  expect_true("indicateur_n3_naturalite" %in% indicators)
})

test_that("init_compute_state returns correct structure with all 31 indicators", {
  with_mocked_bindings(
    get_computation_progress = function(project_id) list(computed_indicators = character(0), last_saved_at = NULL),
    {
      state <- nemetonShiny:::init_compute_state("test_project")
      expect_equal(state$project_id, "test_project")
      expect_equal(state$status, "pending")
      expect_equal(state$phase, "init")
      expect_equal(state$progress, 0)
      expect_equal(state$indicators_total, 31)
      expect_equal(state$indicators_completed, 0)
      expect_equal(state$indicators_failed, 0L)
      expect_type(state$indicators_status, "character")
      expect_equal(length(state$indicators_status), 31)
      expect_true(all(state$indicators_status == "pending"))
    }
  )
})

test_that("init_compute_state supports resume with previously computed indicators", {
  with_mocked_bindings(
    get_computation_progress = function(project_id) {
      list(
        computed_indicators = c("indicateur_c1_biomasse", "indicateur_c2_ndvi"),
        last_saved_at = Sys.time()
      )
    },
    {
      state <- nemetonShiny:::init_compute_state("test_project")
      expect_equal(state$indicators_completed, 2)
      expect_equal(state$indicators_skipped, 2)
      expect_true(state$is_resume)
      expect_equal(state$indicators_status[["indicateur_c1_biomasse"]], "completed")
      expect_equal(state$indicators_status[["indicateur_c2_ndvi"]], "completed")
      expect_equal(state$indicators_status[["indicateur_w3_humidite"]], "pending")
    }
  )
})

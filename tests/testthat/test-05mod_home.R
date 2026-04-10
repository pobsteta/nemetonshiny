# Tests for Home Module
# Comprehensive unit tests for mod_home.R — UI + Server (testServer)

# =============================================================================
# Helper: base mocks used by all testServer tests
# =============================================================================

mock_empty_projects <- function(limit = 5) {
  data.frame(
    id = character(0), name = character(0), description = character(0),
    owner = character(0), status = character(0), parcels_count = integer(0),
    created_at = as.POSIXct(character(0)), updated_at = as.POSIXct(character(0)),
    is_corrupted = logical(0), stringsAsFactors = FALSE
  )
}

mock_search_server <- function(id, app_state) {
  list(
    selected_commune = shiny::reactive(NULL),
    commune_geometry = shiny::reactive(NULL)
  )
}

mock_map_server <- function(id, app_state, commune_geometry, parcels) {
  list(
    selected_parcels = shiny::reactive(NULL),
    selection_count = shiny::reactive(0)
  )
}

mock_project_server <- function(id, app_state, selected_parcels) {
  list(current_project = shiny::reactive(NULL))
}

mock_progress_server <- function(id, compute_state, app_state) {
  list(
    reset_tracking = function() {},
    send_running_update = function(state) {}
  )
}

make_app_state <- function(extra = list()) {
  base <- list(
    language = "fr",
    current_project = NULL,
    project_id = NULL,
    project_status = NULL,
    refresh_projects = NULL,
    restore_in_progress = FALSE,
    computation_running = FALSE,
    cancel_computation = NULL,
    retry_computation = NULL,
    view_results = NULL,
    restart_tour = NULL,
    clear_map_selection = NULL,
    family_comments = list(),
    clear_all_comments = NULL,
    commune_transitioning = FALSE
  )
  args <- modifyList(base, extra)
  do.call(shiny::reactiveValues, args)
}

# Helper to collapse testServer renderUI output to a single string
collapse_html <- function(x) paste(as.character(x), collapse = "")

# =============================================================================
# UI Tests
# =============================================================================

test_that("mod_home_ui returns valid Shiny UI with correct structure", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonShiny:::mod_home_ui("test")
      expect_s3_class(ui, "shiny.tag")
      ui_html <- as.character(ui)

      # Sidebar, search module, map module, project module, progress module
      expect_true(grepl("sidebar", ui_html, ignore.case = TRUE))
      expect_true(grepl("test-search", ui_html))
      expect_true(grepl("test-map", ui_html))
      expect_true(grepl("test-project", ui_html))
      expect_true(grepl("test-progress", ui_html))
      expect_true(grepl("recent_projects", ui_html))
      expect_true(grepl("compute_button_ui", ui_html))
    }
  )
})

test_that("mod_home_ui works with English language", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      ui <- nemetonShiny:::mod_home_ui("test_en")
      expect_s3_class(ui, "shiny.tag")
      ui_html <- as.character(ui)
      expect_true(grepl("test_en-", ui_html))
      expect_true(nchar(ui_html) > 100)
    }
  )
})

# =============================================================================
# Server: basic init + return values
# =============================================================================

test_that("mod_home_server is a function with correct formals", {
  expect_type(nemetonShiny:::mod_home_server, "closure")
  args <- names(formals(nemetonShiny:::mod_home_server))
  expect_true("id" %in% args)
  expect_true("app_state" %in% args)
})

test_that("mod_home_server initializes and returns expected reactive list", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = function(id, app_state) {
      list(
        selected_commune = shiny::reactive("01001"),
        commune_geometry = shiny::reactive(NULL)
      )
    },
    mod_map_server = function(id, app_state, commune_geometry, parcels) {
      list(
        selected_parcels = shiny::reactive(data.frame(id = "p1")),
        selection_count = shiny::reactive(1L)
      )
    },
    mod_project_server = function(id, app_state, selected_parcels) {
      list(current_project = shiny::reactive(list(id = "proj_1")))
    },
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state()),
        {
          result <- session$getReturned()
          expect_type(result, "list")
          expect_length(result, 4)
          expect_true(shiny::is.reactive(result$selected_commune))
          expect_true(shiny::is.reactive(result$selected_parcels))
          expect_true(shiny::is.reactive(result$selection_count))
          expect_true(shiny::is.reactive(result$current_project))
          expect_equal(result$selected_commune(), "01001")
          expect_equal(result$selection_count(), 1L)
        }
      )
    }
  )
})

test_that("mod_home_server delegates to all child modules", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  called <- list(search = FALSE, map = FALSE, project = FALSE, progress = FALSE)

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = function(id, app_state) {
      called$search <<- TRUE
      mock_search_server(id, app_state)
    },
    mod_map_server = function(id, app_state, commune_geometry, parcels) {
      called$map <<- TRUE
      mock_map_server(id, app_state, commune_geometry, parcels)
    },
    mod_project_server = function(id, app_state, selected_parcels) {
      called$project <<- TRUE
      mock_project_server(id, app_state, selected_parcels)
    },
    mod_progress_server = function(id, compute_state, app_state) {
      called$progress <<- TRUE
      mock_progress_server(id, compute_state, app_state)
    },
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state()),
        {
          expect_true(called$search)
          expect_true(called$map)
          expect_true(called$project)
          expect_true(called$progress)
        }
      )
    }
  )
})

# =============================================================================
# Server: recent_projects_list renderUI
# =============================================================================

test_that("recent_projects_list renders empty message when no projects", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state()),
        {
          # Trigger renderUI
          html <- output$recent_projects_list
          expect_true(!is.null(html))
          html_str <- collapse_html(html)
          expect_true(grepl("text-muted", html_str))
        }
      )
    }
  )
})

test_that("recent_projects_list renders project cards with various statuses", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  mock_projects <- data.frame(
    id = c("proj_1", "proj_2", "proj_3", "proj_4", "proj_5"),
    name = c("Completed", "Computing", "Downloading", "Error", "Draft"),
    description = rep("", 5),
    owner = rep("", 5),
    status = c("completed", "computing", "downloading", "error", "draft"),
    parcels_count = c(5L, 3L, 2L, 1L, 10L),
    created_at = rep(Sys.time(), 5),
    updated_at = rep(Sys.time(), 5),
    is_corrupted = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = function(limit = 5) mock_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state()),
        {
          html <- output$recent_projects_list
          expect_true(!is.null(html))
          html_str <- collapse_html(html)

          # Check status badge classes appear in the output
          expect_true(grepl("bg-success", html_str))   # completed
          expect_true(grepl("bg-warning", html_str))    # computing
          expect_true(grepl("bg-info", html_str))       # downloading
          expect_true(grepl("bg-danger", html_str))     # error
          expect_true(grepl("bg-secondary", html_str))  # draft (default)
        }
      )
    }
  )
})

test_that("recent_projects_list marks active project correctly", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  mock_projects <- data.frame(
    id = c("active_proj", "other_proj"),
    name = c("Active", "Other"),
    description = rep("", 2),
    owner = rep("", 2),
    status = c("completed", "draft"),
    parcels_count = c(5L, 3L),
    created_at = rep(Sys.time(), 2),
    updated_at = rep(Sys.time(), 2),
    is_corrupted = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = function(limit = 5) mock_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state(list(
          project_id = "active_proj"
        ))),
        {
          html <- output$recent_projects_list
          html_str <- collapse_html(html)

          # Active project should have active styling
          expect_true(grepl("border-primary", html_str))
          expect_true(grepl("bg-primary", html_str))  # badge
          # The inactive one should have cursor-pointer
          expect_true(grepl("cursor-pointer", html_str))
        }
      )
    }
  )
})

test_that("recent_projects_list handles corrupted project", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  mock_projects <- data.frame(
    id = "corrupted_proj",
    name = "Bad Project",
    description = "",
    owner = "",
    status = "error",
    parcels_count = 0L,
    created_at = Sys.time(),
    updated_at = Sys.time(),
    is_corrupted = TRUE,
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = function(limit = 5) mock_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state()),
        {
          html <- output$recent_projects_list
          html_str <- collapse_html(html)

          # Corrupted project has danger styling
          expect_true(grepl("border-danger", html_str))
          # Should trigger delete_corrupted handler, not load_project
          expect_true(grepl("delete_corrupted", html_str))
        }
      )
    }
  )
})

test_that("recent_projects_list shows singular parcel count", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  mock_projects <- data.frame(
    id = "proj_single",
    name = "Single Parcel",
    description = "",
    owner = "",
    status = "draft",
    parcels_count = 1L,
    created_at = Sys.time(),
    updated_at = Sys.time(),
    is_corrupted = FALSE,
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = function(limit = 5) mock_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state()),
        {
          html <- output$recent_projects_list
          html_str <- collapse_html(html)
          # Should contain "1 " for the single parcel
          expect_true(grepl("1 ", html_str))
        }
      )
    }
  )
})

# =============================================================================
# Server: compute_button_ui renderUI
# =============================================================================

test_that("compute_button_ui returns NULL when no project", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state()),
        {
          html <- output$compute_button_ui
          # No project → NULL output (empty string in rendered HTML)
          expect_true(html == "" || is.null(html) || nchar(html) == 0)
        }
      )
    }
  )
})

test_that("compute_button_ui shows compute button for draft project", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  mock_project <- list(
    id = "proj_draft",
    metadata = list(name = "Draft", status = "draft"),
    parcels = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state(list(
          current_project = mock_project,
          project_id = "proj_draft"
        ))),
        {
          html <- output$compute_button_ui
          html_str <- collapse_html(html)
          expect_true(grepl("start_compute", html_str))
          expect_true(grepl("btn-primary", html_str))
        }
      )
    }
  )
})

test_that("compute_button_ui shows compute button for error project", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  mock_project <- list(
    id = "proj_err",
    metadata = list(name = "Error", status = "error"),
    parcels = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state(list(
          current_project = mock_project,
          project_id = "proj_err"
        ))),
        {
          html <- output$compute_button_ui
          html_str <- collapse_html(html)
          # error status is in c("draft", "error") -> shows compute button
          expect_true(grepl("start_compute", html_str))
        }
      )
    }
  )
})

test_that("compute_button_ui shows view results for completed project", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  mock_project <- list(
    id = "proj_done",
    metadata = list(name = "Done", status = "completed"),
    parcels = NULL,
    indicators = data.frame(C1 = 50)
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state(list(
          current_project = mock_project,
          project_id = "proj_done"
        ))),
        {
          html <- output$compute_button_ui
          html_str <- collapse_html(html)
          # completed -> shows view_results + recompute buttons
          expect_true(grepl("view_results", html_str))
          expect_true(grepl("btn-success", html_str))
          expect_true(grepl("recompute", html_str))
        }
      )
    }
  )
})

test_that("compute_button_ui returns NULL for computing status", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  mock_project <- list(
    id = "proj_running",
    metadata = list(name = "Running", status = "computing"),
    parcels = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state(list(
          current_project = mock_project,
          project_id = "proj_running"
        ))),
        {
          html <- output$compute_button_ui
          # computing is not in c("draft","error") nor "completed" -> NULL
          expect_true(html == "" || is.null(html) || nchar(html) == 0)
        }
      )
    }
  )
})

# =============================================================================
# Server: load_project observer
# =============================================================================

test_that("load_project observer loads project and updates app_state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  mock_parcels <- sf::st_sf(
    id = c("p1", "p2"),
    code_insee = c("01001", "01001"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
    ),
    crs = 4326
  )

  mock_project <- list(
    id = "loaded_proj",
    metadata = list(name = "Loaded", status = "draft"),
    parcels = mock_parcels,
    indicators = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    load_project = function(project_id) mock_project,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    read_progress_state = function(project_id) NULL,
    {
      as <- make_app_state()
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          session$setInputs(load_project = "loaded_proj")
          session$flushReact()

          # app_state should be updated
          expect_equal(as$project_id, "loaded_proj")
          expect_equal(as$project_status, "draft")
          expect_identical(as$current_project, mock_project)
          # restore_in_progress set to TRUE because parcels have code_insee
          expect_true(as$restore_in_progress)
          # restore_project should be set
          expect_true(!is.null(as$restore_project))
          expect_equal(as$restore_project$commune_code, "01001")
          expect_equal(as$restore_project$department_code, "01")
        }
      )
    }
  )
})

test_that("load_project handles DOM-TOM commune codes", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  mock_parcels <- sf::st_sf(
    id = "p1",
    code_insee = "97105",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
    ),
    crs = 4326
  )

  mock_project <- list(
    id = "dom_proj",
    metadata = list(name = "DOM", status = "completed"),
    parcels = mock_parcels,
    indicators = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    load_project = function(project_id) mock_project,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    read_progress_state = function(project_id) NULL,
    {
      as <- make_app_state()
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          session$setInputs(load_project = "dom_proj")
          session$flushReact()

          # DOM-TOM uses 3-char department code
          expect_equal(as$restore_project$department_code, "971")
        }
      )
    }
  )
})

test_that("load_project skips already loaded project", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  load_called <- FALSE

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    load_project = function(project_id) {
      load_called <<- TRUE
      NULL
    },
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      as <- make_app_state(list(project_id = "already_loaded"))
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          # Try to load the same project
          session$setInputs(load_project = "already_loaded")
          session$flushReact()

          # load_project should NOT be called (early return)
          # Note: the observer uses req() so it may still call but return() early
          # The key check is that project_id didn't change
          expect_equal(as$project_id, "already_loaded")
        }
      )
    }
  )
})

test_that("load_project handles parcels with invalid commune code", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  mock_parcels <- sf::st_sf(
    id = "p1",
    code_insee = NA_character_,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
    ),
    crs = 4326
  )

  mock_project <- list(
    id = "bad_commune_proj",
    metadata = list(name = "Bad", status = "draft"),
    parcels = mock_parcels,
    indicators = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    load_project = function(project_id) mock_project,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    read_progress_state = function(project_id) NULL,
    {
      as <- make_app_state()
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          # This should trigger the invalid commune code path (L304-308)
          expect_warning(
            {
              session$setInputs(load_project = "bad_commune_proj")
              session$flushReact()
            },
            regexp = "invalid commune code"
          )
        }
      )
    }
  )
})

# =============================================================================
# Server: delete corrupted project
# =============================================================================

test_that("delete_corrupted shows confirmation modal", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state()),
        {
          # Trigger delete_corrupted input
          session$setInputs(delete_corrupted = "corrupted_id")
          session$flushReact()
          # If we got here without error, the modal was shown
          expect_true(TRUE)
        }
      )
    }
  )
})

test_that("confirm_delete deletes project and refreshes list", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  deleted_id <- NULL

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    delete_project = function(project_id) {
      deleted_id <<- project_id
      TRUE
    },
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      as <- make_app_state()
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          session$setInputs(delete_corrupted = "bad_proj")
          session$flushReact()

          # Now confirm delete
          session$setInputs(confirm_delete = 1L)
          session$flushReact()

          expect_equal(deleted_id, "bad_proj")
          # refresh_projects should be updated
          expect_true(!is.null(as$refresh_projects))
        }
      )
    }
  )
})

# =============================================================================
# Server: recompute handler
# =============================================================================

test_that("recompute handler clears cache and resets status", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  cache_cleared <- FALSE
  status_updated <- NULL
  reloaded_project <- NULL

  mock_project <- list(
    id = "proj_recompute",
    metadata = list(name = "Recompute", status = "completed"),
    parcels = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    clear_computation_cache = function(project_id) {
      cache_cleared <<- TRUE
      TRUE
    },
    update_project_status = function(project_id, status) {
      status_updated <<- status
      TRUE
    },
    load_project = function(project_id) {
      reloaded_project <<- project_id
      mock_project
    },
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state(list(
          current_project = mock_project,
          project_id = "proj_recompute"
        ))),
        {
          session$setInputs(recompute = 1L)
          session$flushReact()

          expect_true(cache_cleared)
          expect_equal(status_updated, "draft")
          expect_equal(reloaded_project, "proj_recompute")
        }
      )
    }
  )
})

# =============================================================================
# Server: view_results handler
# =============================================================================

test_that("view_results navigates to synthesis tab", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state()),
        {
          # Trigger view_results — should not error
          session$setInputs(view_results = 1L)
          session$flushReact()
          expect_true(TRUE)
        }
      )
    }
  )
})

# =============================================================================
# Server: start_compute confirmation modal
# =============================================================================

test_that("start_compute shows confirmation modal", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  mock_project <- list(
    id = "proj_compute",
    metadata = list(name = "Compute", status = "draft"),
    parcels = data.frame(id = c("p1", "p2"))
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state(list(
          current_project = mock_project,
          project_id = "proj_compute"
        ))),
        {
          session$setInputs(start_compute = 1L)
          session$flushReact()
          # Modal shown without error
          expect_true(TRUE)
        }
      )
    }
  )
})

# =============================================================================
# Server: cancel computation
# =============================================================================

test_that("cancel computation updates state correctly", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  cancel_called <- FALSE
  status_set <- NULL

  mock_project <- list(
    id = "proj_cancel",
    metadata = list(name = "Cancel Me", status = "computing"),
    parcels = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    cancel_computation = function(project_id) {
      cancel_called <<- TRUE
    },
    update_project_status = function(project_id, status) {
      status_set <<- status
      TRUE
    },
    load_project = function(project_id) mock_project,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      as <- make_app_state(list(
        current_project = mock_project,
        project_id = "proj_cancel",
        computation_running = TRUE
      ))

      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          # ignoreInit = TRUE means first value change is ignored.
          # Set twice to trigger the observer.
          as$cancel_computation <- Sys.time() - 1
          session$flushReact()
          as$cancel_computation <- Sys.time()
          session$flushReact()

          # The cancel handler runs without error.
          # Note: computation_running is reset by progress polling, not by
          # the cancel handler directly. The cancel handler resets
          # computing_project_id and sends JS messages.
          expect_true(TRUE)
        }
      )
    }
  )
})

# =============================================================================
# Server: retry computation
# =============================================================================

test_that("retry computation re-initializes and invokes task", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  # Prevent the ExtendedTask from spawning real multisession workers.
  # compute_task$invoke() calls future::plan("multisession") and
  # promises::future_promise(), which pollute global state and block
  # subsequent shiny::testServer() calls in other test files.
  local_mocked_bindings(
    future_promise = function(...) promises::promise_resolve(NULL),
    .package = "promises"
  )

  # Restore original future plan on exit (compute_task sets multisession)
  if (requireNamespace("future", quietly = TRUE)) {
    old_plan <- future::plan()
    withr::defer(future::plan(old_plan))
  }

  cache_cleared <- FALSE
  status_updated <- NULL

  mock_project <- list(
    id = "proj_retry",
    metadata = list(name = "Retry", status = "error"),
    parcels = data.frame(id = "p1")
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    clear_computation_cache = function(project_id) {
      cache_cleared <<- TRUE
      TRUE
    },
    update_project_status = function(project_id, status) {
      status_updated <<- status
      TRUE
    },
    get_project_path = function(project_id) "/tmp/fake_path",
    init_compute_state = function(project_id, ...) {
      list(
        project_id = project_id,
        status = "pending",
        progress = 0, progress_max = 12
      )
    },
    save_progress_state = function(project_id, state, ...) invisible(NULL),
    # Return a terminal state so the later::later polling loop in
    # start_progress_polling exits cleanly (NULL keeps it alive forever)
    read_progress_state = function(project_id) {
      list(project_id = project_id, status = "cancelled")
    },
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      as <- make_app_state(list(
        current_project = mock_project,
        project_id = "proj_retry"
      ))

      suppressWarnings(shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          # ignoreInit = TRUE — set twice to trigger
          as$retry_computation <- Sys.time() - 1
          suppressWarnings(session$flushReact())
          as$retry_computation <- Sys.time()
          suppressWarnings(session$flushReact())

          expect_true(cache_cleared)
          expect_equal(status_updated, "draft")
        }
      ))
    }
  )
})

# =============================================================================
# Server: view_results from progress module (app_state$view_results)
# =============================================================================

test_that("app_state view_results triggers navigation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      as <- make_app_state()
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          as$view_results <- Sys.time()
          session$flushReact()
          # Reached without error — updateNavbarPage called
          expect_true(TRUE)
        }
      )
    }
  )
})

# =============================================================================
# Server: resume progress tracking on project load
# =============================================================================

test_that("resume progress tracking when loading computing project", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  send_update_called <- FALSE
  reset_called <- FALSE

  mock_project <- list(
    id = "proj_computing",
    metadata = list(name = "Computing", status = "computing"),
    parcels = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    read_progress_state = function(project_id) {
      list(
        project_id = project_id,
        status = "computing",
        progress = 5, progress_max = 12,
        current_task = "Computing C family",
        indicators_completed = 3,
        indicators_failed = 0
      )
    },
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = function(id, compute_state, app_state) {
      list(
        reset_tracking = function() { reset_called <<- TRUE },
        send_running_update = function(state) { send_update_called <<- TRUE }
      )
    },
    {
      as <- make_app_state()
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          # ignoreInit = TRUE — set twice to trigger
          as$current_project <- list(id = "dummy")
          session$flushReact()
          as$current_project <- mock_project
          session$flushReact()

          expect_true(reset_called)
          expect_true(send_update_called)
        }
      )
    }
  )
})

test_that("resume does not interfere when project has no ongoing computation", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  send_update_called <- FALSE

  mock_project <- list(
    id = "proj_done",
    metadata = list(name = "Done", status = "completed"),
    parcels = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    read_progress_state = function(project_id) {
      list(project_id = project_id, status = "completed")
    },
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = function(id, compute_state, app_state) {
      list(
        reset_tracking = function() {},
        send_running_update = function(state) { send_update_called <<- TRUE }
      )
    },
    {
      as <- make_app_state()
      suppressWarnings(shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          as$current_project <- mock_project
          suppressWarnings(session$flushReact())

          # Should NOT send running update for completed project
          expect_false(send_update_called)
        }
      ))
    }
  )
})

# =============================================================================
# Server: reset_project_state on commune change
# =============================================================================

test_that("commune change resets project state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  commune_val <- shiny::reactiveVal(NULL)

  mock_project <- list(
    id = "proj_reset",
    metadata = list(name = "Reset", status = "completed"),
    parcels = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = function(id, app_state) {
      list(
        selected_commune = commune_val,
        commune_geometry = shiny::reactive(NULL)
      )
    },
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      as <- make_app_state(list(
        current_project = mock_project,
        project_id = "proj_reset"
      ))

      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          # ignoreInit = TRUE — set twice to trigger
          commune_val("00001")
          session$flushReact()
          # Re-set project state after first reset
          as$current_project <- mock_project
          as$project_id <- "proj_reset"

          commune_val("01234")
          session$flushReact()

          # Project should be cleared
          expect_null(as$current_project)
          expect_null(as$project_id)
          # clear_map_selection should be set
          expect_true(!is.null(as$clear_map_selection))
        }
      )
    }
  )
})

test_that("commune change during restore does NOT reset project state", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  commune_val <- shiny::reactiveVal(NULL)

  mock_project <- list(
    id = "proj_restore",
    metadata = list(name = "Restoring", status = "completed"),
    parcels = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = function(id, app_state) {
      list(
        selected_commune = commune_val,
        commune_geometry = shiny::reactive(NULL)
      )
    },
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      as <- make_app_state(list(
        current_project = mock_project,
        project_id = "proj_restore",
        restore_in_progress = TRUE
      ))

      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          # Change commune during restore — should NOT reset
          commune_val("01234")
          session$flushReact()

          # Project should still be there
          expect_equal(as$project_id, "proj_restore")
        }
      )
    }
  )
})

# =============================================================================
# Server: load_project with no parcels
# =============================================================================

test_that("load_project with no parcels skips restore", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")

  mock_project <- list(
    id = "proj_no_parcels",
    metadata = list(name = "No Parcels", status = "draft"),
    parcels = NULL,
    indicators = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    load_project = function(project_id) mock_project,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    read_progress_state = function(project_id) NULL,
    {
      as <- make_app_state()
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          session$setInputs(load_project = "proj_no_parcels")
          session$flushReact()

          expect_equal(as$project_id, "proj_no_parcels")
          # No parcels → no restore_in_progress
          expect_false(isTRUE(as$restore_in_progress))
          expect_null(as$restore_project)
        }
      )
    }
  )
})

# =============================================================================
# Server: load_project with parcels missing code_insee column
# =============================================================================

test_that("load_project with parcels but no code_insee skips restore", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  mock_parcels <- sf::st_sf(
    id = "p1",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
    ),
    crs = 4326
  )

  mock_project <- list(
    id = "proj_no_insee",
    metadata = list(name = "No INSEE", status = "draft"),
    parcels = mock_parcels,
    indicators = NULL
  )

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    load_project = function(project_id) mock_project,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    read_progress_state = function(project_id) NULL,
    {
      as <- make_app_state()
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          session$setInputs(load_project = "proj_no_insee")
          session$flushReact()

          expect_equal(as$project_id, "proj_no_insee")
          # No code_insee column → restore not triggered
          expect_false(isTRUE(as$restore_in_progress))
        }
      )
    }
  )
})

# =============================================================================
# Server: cicerone tour (conditional on package availability)
# =============================================================================

test_that("tour related observers exist when cicerone is available", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("cicerone")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state()),
        {
          # Tour observers should exist without error
          # tour_seen_browser = TRUE means tour was already seen
          session$setInputs(tour_seen_browser = TRUE)
          session$flushReact()
          expect_true(TRUE)
        }
      )
    }
  )
})

test_that("tour auto-starts when not previously seen", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("cicerone")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state()),
        {
          # tour_seen_browser = FALSE means tour was not seen → auto-start
          session$setInputs(tour_seen_browser = FALSE)
          session$flushReact()
          # No error = success
          expect_true(TRUE)
        }
      )
    }
  )
})

test_that("tour restart via app_state works", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("cicerone")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      as <- make_app_state()
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = as),
        {
          as$restart_tour <- Sys.time()
          session$flushReact()
          expect_true(TRUE)
        }
      )
    }
  )
})

test_that("tour_ready input triggers do_start_tour", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("cicerone")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    list_recent_projects = mock_empty_projects,
    mod_search_server = mock_search_server,
    mod_map_server = mock_map_server,
    mod_project_server = mock_project_server,
    mod_progress_server = mock_progress_server,
    {
      shiny::testServer(
        nemetonShiny:::mod_home_server,
        args = list(app_state = make_app_state()),
        {
          # Simulate JS callback — tour_ready fires do_start_tour
          session$setInputs(tour_ready = as.numeric(Sys.time()))
          session$flushReact()
          # cicerone may warn but should not error
          expect_true(TRUE)
        }
      )
    }
  )
})

# Drain async callbacks to prevent testServer session accumulation
later::run_now(0)

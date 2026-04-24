# Tests for Field Ingest Module (mod_field_ingest.R)
# UI sanity + validate / attach reactive flow with a real QField GPKG.

test_that("mod_field_ingest_ui returns valid Shiny UI with expected controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonshiny:::mod_field_ingest_ui("field_ingest")
      expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list") ||
                  inherits(ui, "bslib_fragment"))
      html <- as.character(ui)
      expect_true(grepl("field_ingest-gpkg", html))
      expect_true(grepl("field_ingest-validate", html))
      expect_true(grepl("field_ingest-attach", html))
    }
  )
})


# --- Helpers ----------------------------------------------------------

# Unit polygon covering Lambert-93 [900000..901000, 6500000..6501000]
make_units_sf <- function() {
  poly <- sf::st_polygon(list(rbind(
    c(900000, 6500000), c(901000, 6500000),
    c(901000, 6501000), c(900000, 6501000), c(900000, 6500000)
  )))
  sf::st_sf(
    ug_id = 1L,
    geometry = sf::st_sfc(poly, crs = 2154)
  )
}

# Build a tiny valid QField GPKG (placettes + arbres) in a tempdir and
# return a list mimicking Shiny's fileInput (name, datapath).
make_fake_gpkg <- function(tmpdir = tempfile("qfield_")) {
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  placettes <- sf::st_sf(
    plot_id     = c("P001", "P002"),
    type        = c("Base", "Base"),
    visit_order = 1:2,
    geometry = sf::st_sfc(
      sf::st_point(c(900250, 6500250)),
      sf::st_point(c(900750, 6500750)),
      crs = 2154
    )
  )
  arbres <- sf::st_sf(
    plot_id = rep(c("P001", "P002"), each = 3),
    tree_id = sprintf("T%03d", seq_len(6)),
    espece  = rep("QUPE", 6),
    dbh_cm  = c(25, 30, 35, 22, 28, 40),
    h_m     = c(15, 17, 20, 14, 16, 22),
    statut  = rep("vivant", 6),
    geometry = sf::st_sfc(
      sf::st_point(c(900240, 6500245)),
      sf::st_point(c(900260, 6500255)),
      sf::st_point(c(900255, 6500260)),
      sf::st_point(c(900740, 6500745)),
      sf::st_point(c(900760, 6500755)),
      sf::st_point(c(900755, 6500760)),
      crs = 2154
    )
  )
  gpkg <- file.path(tmpdir, "field.gpkg")
  sf::st_write(placettes, gpkg, layer = "placettes", quiet = TRUE,
               delete_dsn = TRUE)
  sf::st_write(arbres, gpkg, layer = "arbres", quiet = TRUE, append = TRUE)

  data.frame(
    name = "field.gpkg",
    size = file.size(gpkg),
    type = "application/octet-stream",
    datapath = gpkg,
    stringsAsFactors = FALSE
  )
}

make_fake_app_state <- function() {
  shiny::reactiveValues(
    language = "fr",
    current_project = list(
      id = "proj-test",
      indicators_sf = make_units_sf(),
      metadata = list(ndp_level = 0L)
    )
  )
}


# --- Tests ------------------------------------------------------------

test_that("mod_field_ingest_server returns NULL reactives before any click", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  shiny::testServer(
    nemetonshiny:::mod_field_ingest_server,
    args = list(app_state = make_fake_app_state()),
    {
      expect_null(session$returned$import())
      expect_null(session$returned$validation())
      expect_null(session$returned$new_ndp())
    }
  )
})


test_that("mod_field_ingest_server warns if validate is clicked without a file", {
  skip_if_not_installed("shiny")

  shiny::testServer(
    nemetonshiny:::mod_field_ingest_server,
    args = list(app_state = make_fake_app_state()),
    {
      session$setInputs(region = "BFC", validate = 1)
      expect_null(session$returned$import())
      expect_null(session$returned$validation())
    }
  )
})


test_that("mod_field_ingest_server validates a real QField GPKG and reports ok", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not_installed("nemeton")

  fake_file <- make_fake_gpkg()

  shiny::testServer(
    nemetonshiny:::mod_field_ingest_server,
    args = list(app_state = make_fake_app_state()),
    {
      session$setInputs(
        gpkg     = fake_file,
        region   = "BFC",
        validate = 1
      )
      imported <- session$returned$import()
      val      <- session$returned$validation()
      agg      <- session$returned$field_agg()

      expect_true(inherits(imported$placettes, "sf"))
      expect_equal(nrow(imported$placettes), 2)
      expect_equal(nrow(imported$arbres), 6)
      expect_true(isTRUE(val$ok))
      expect_true(inherits(agg, "sf"))
      expect_true("field_dbh_mean_cm" %in% names(agg))
    }
  )
})


test_that("mod_field_ingest_server attaches field data and bumps project NDP", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not_installed("nemeton")

  fake_file <- make_fake_gpkg()

  td <- tempfile("proj_")
  dir.create(file.path(td, "data"), recursive = TRUE)

  captured_updates <- list()

  with_mocked_bindings(
    get_project_path      = function(project_id) td,
    update_project_metadata = function(project_id, updates, project_path = NULL) {
      captured_updates[[length(captured_updates) + 1L]] <<- updates
      TRUE
    },
    # load_project is called at the end of attach — return the same
    # project with the bumped NDP level so the reactive update is
    # coherent.
    load_project = function(project_id) {
      list(
        id = project_id,
        indicators_sf = make_units_sf(),
        metadata = list(ndp_level = 3L)
      )
    },
    {
      shiny::testServer(
        nemetonshiny:::mod_field_ingest_server,
        args = list(app_state = make_fake_app_state()),
        {
          session$setInputs(
            gpkg     = fake_file,
            region   = "BFC",
            validate = 1
          )
          expect_true(isTRUE(session$returned$validation()$ok))

          session$setInputs(attach = 1)
          new_ndp  <- session$returned$new_ndp()
          attached <- session$returned$attached()

          expect_true(!is.null(new_ndp))
          expect_gte(new_ndp, 2L)
          expect_true(inherits(attached, "sf"))
          expect_true("field_dbh_mean_cm" %in% names(attached))
        }
      )
    }
  )

  expect_true(file.exists(file.path(td, "data", "field_data.gpkg")))
  expect_true(length(captured_updates) >= 1L)
  expect_true("ndp_level" %in% names(captured_updates[[1]]))
  expect_gte(captured_updates[[1]]$ndp_level, 2L)
})

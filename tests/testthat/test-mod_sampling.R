# Tests for Sampling Module (mod_sampling.R)
# UI sanity + reactive draw of plots + .qgz download.

test_that("mod_sampling_ui returns valid Shiny UI with expected controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonshiny:::mod_sampling_ui("sampling")
      expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list") ||
                  inherits(ui, "bslib_fragment"))
      html <- as.character(ui)
      expect_true(grepl("sampling-n_base", html))
      expect_true(grepl("sampling-n_over", html))
      expect_true(grepl("sampling-generate", html))
      expect_true(grepl("sampling-download_qgz", html))
    }
  )
})


# Helper: build a minimal project-like reactiveValues with indicators_sf
make_fake_app_state <- function() {
  poly <- sf::st_polygon(list(rbind(
    c(900000, 6500000), c(901000, 6500000),
    c(901000, 6501000), c(900000, 6501000), c(900000, 6500000)
  )))
  indicators_sf <- sf::st_sf(
    ug_id = 1L,
    geometry = sf::st_sfc(poly, crs = 2154)
  )
  shiny::reactiveValues(
    language = "fr",
    current_project = list(indicators_sf = indicators_sf)
  )
}


test_that("mod_sampling_server returns NULL plots before generate is clicked", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  shiny::testServer(
    nemetonshiny:::mod_sampling_server,
    args = list(app_state = make_fake_app_state()),
    {
      expect_null(session$returned$sample_plots())
      zone <- session$returned$zone_etude()
      expect_true(inherits(zone, "sf"))
      expect_equal(as.integer(sf::st_crs(zone)$epsg), 2154L)
    }
  )
})


test_that("mod_sampling_server generates POINT plots after clicking generate", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  shiny::testServer(
    nemetonshiny:::mod_sampling_server,
    args = list(app_state = make_fake_app_state()),
    {
      session$setInputs(n_base = 7, n_over = 3, seed = 123,
                        region = "BFC", project_name = "t1",
                        generate = 1)
      plots <- session$returned$sample_plots()
      expect_true(inherits(plots, "sf"))
      expect_equal(nrow(plots), 10)
      expect_true(all(c("plot_id", "type", "visit_order") %in% names(plots)))
      expect_setequal(unique(plots$type), c("Base", "Over"))
      expect_equal(sum(plots$type == "Base"), 7)
      expect_equal(sum(plots$type == "Over"), 3)
      expect_equal(as.integer(sf::st_crs(plots)$epsg), 2154L)
    }
  )
})


test_that("mod_sampling_server defaults project_name to the current project name", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  poly <- sf::st_polygon(list(rbind(
    c(900000, 6500000), c(901000, 6500000),
    c(901000, 6501000), c(900000, 6501000), c(900000, 6500000)
  )))
  indicators_sf <- sf::st_sf(ug_id = 1L,
                             geometry = sf::st_sfc(poly, crs = 2154))
  state <- shiny::reactiveValues(
    language = "fr",
    current_project = list(
      id = "proj-42",
      indicators_sf = indicators_sf,
      metadata = list(name = "Forêt de Gérardmer 2026")
    )
  )

  shiny::testServer(
    nemetonshiny:::mod_sampling_server,
    args = list(app_state = state),
    {
      # Non-ASCII + spaces must be sanitized to match the download
      # filename's [a-zA-Z0-9_-] whitelist.
      expect_equal(session$returned$default_project_name(),
                   "For_t_de_G_rardmer_2026")
    }
  )

  # No active project -> fallback to "echantillon".
  empty_state <- shiny::reactiveValues(language = "fr", current_project = NULL)
  shiny::testServer(
    nemetonshiny:::mod_sampling_server,
    args = list(app_state = empty_state),
    {
      expect_equal(session$returned$default_project_name(), "echantillon")
    }
  )
})


test_that("mod_sampling_server warns when no project is loaded", {
  skip_if_not_installed("shiny")

  empty_state <- shiny::reactiveValues(language = "fr",
                                       current_project = NULL)
  shiny::testServer(
    nemetonshiny:::mod_sampling_server,
    args = list(app_state = empty_state),
    {
      session$setInputs(n_base = 5, n_over = 0, seed = 1,
                        region = "BFC", project_name = "t2",
                        generate = 1)
      expect_null(session$returned$sample_plots())
      expect_null(session$returned$zone_etude())
    }
  )
})


test_that("generated plots feed create_qfield_project() into a valid .qgz", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not_installed("nemeton")

  plots_captured <- NULL
  zone_captured  <- NULL

  shiny::testServer(
    nemetonshiny:::mod_sampling_server,
    args = list(app_state = make_fake_app_state()),
    {
      session$setInputs(n_base = 4, n_over = 1, seed = 7,
                        region = "BFC", project_name = "dl_test",
                        generate = 1)
      plots_captured <<- session$returned$sample_plots()
      zone_captured  <<- session$returned$zone_etude()
    }
  )

  expect_false(is.null(plots_captured))
  expect_equal(nrow(plots_captured), 5)

  withr::with_tempdir({
    qgz <- nemeton::create_qfield_project(
      placettes    = plots_captured,
      zone_etude   = zone_captured,
      output_dir   = ".",
      project_name = "dl_test"
    )
    expect_true(file.exists(qgz))
    expect_gt(file.size(qgz), 1000)  # real ZIP, not the empty fallback

    contents <- utils::unzip(qgz, list = TRUE)
    expect_true(any(grepl("\\.qgs$",  contents$Name)))
    expect_true(any(grepl("\\.gpkg$", contents$Name)))
  })
})

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

# Tiny SpatRaster fake covering the AOI used by make_fake_app_state(),
# tagged with a `nemeton_dem_layer` attribute so the resolved-source
# toast can render its layer name in the new pre-check path.
make_fake_dem <- function(layer = "fake DEM") {
  skip_if_not_installed("terra")
  r <- terra::rast(
    xmin = 899000, xmax = 902000,
    ymin = 6499000, ymax = 6502000,
    resolution = 100, crs = "EPSG:2154"
  )
  terra::values(r) <- stats::runif(terra::ncell(r), 100, 800)
  attr(r, "nemeton_dem_layer") <- layer
  r
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
  skip_if_not_installed("terra")

  fake_dem <- make_fake_dem()
  testthat::local_mocked_bindings(
    resolve_project_dem = function(...) fake_dem,
    resolve_project_chm = function(...) NULL,
    .package = "nemeton"
  )

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


test_that("target_error + manual CV sizes n_base via Cochran", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not_installed("nemeton")
  skip_if_not_installed("terra")

  fake_dem <- make_fake_dem()
  testthat::local_mocked_bindings(
    resolve_project_dem = function(...) fake_dem,
    resolve_project_chm = function(...) NULL,
    .package = "nemeton"
  )

  shiny::testServer(
    nemetonshiny:::mod_sampling_server,
    args = list(app_state = make_fake_app_state()),
    {
      # 35 % CV, ±10 %, 95 % -> n ~= (1.96 * 0.35 / 0.10)^2 ~= 47; with
      # Student correction a few plots more; over_ratio 20 % -> ~10.
      session$setInputs(
        sizing_mode = "target_error",
        cv_source = "manual",
        cv_pct = 35, target_error_pct = 10,
        alpha_pct = 5, over_ratio_pct = 20,
        region = "BFC", project_name = "t_target",
        seed = 1,
        generate = 1
      )
      plots <- session$returned$sample_plots()
      expect_true(inherits(plots, "sf"))
      # Expected n_base between 47 and 55 (normal + Student bump).
      n_base <- sum(plots$type == "Base")
      expect_gte(n_base, 45L)
      expect_lte(n_base, 60L)
      n_over <- sum(plots$type == "Over")
      # Over ratio 20 % -> between 9 and 13.
      expect_gte(n_over, 8L)
      expect_lte(n_over, 14L)
    }
  )
})


test_that("target_error mode bails when the manual CV is missing / zero", {
  skip_if_not_installed("shiny")

  shiny::testServer(
    nemetonshiny:::mod_sampling_server,
    args = list(app_state = make_fake_app_state()),
    {
      session$setInputs(
        sizing_mode = "target_error",
        cv_source = "manual",
        cv_pct = 0,  # invalid
        target_error_pct = 10, alpha_pct = 5, over_ratio_pct = 20,
        region = "BFC", project_name = "t_bad",
        seed = 1,
        generate = 1
      )
      expect_null(session$returned$sample_plots())
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
  skip_if_not_installed("terra")

  fake_dem <- make_fake_dem()
  testthat::local_mocked_bindings(
    resolve_project_dem = function(...) fake_dem,
    resolve_project_chm = function(...) NULL,
    .package = "nemeton"
  )

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


test_that("mod_sampling_server persists samples.gpkg when a real project is loaded", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not_installed("nemeton")
  skip_if_not_installed("terra")

  fake_dem <- make_fake_dem()
  testthat::local_mocked_bindings(
    resolve_project_dem = function(...) fake_dem,
    resolve_project_chm = function(...) NULL,
    .package = "nemeton"
  )

  withr::with_tempdir({
    temp_root <- getwd()
    withr::with_envvar(c(NEMETONSHINY_DISABLE_DB = "1"), {
      with_mocked_bindings(
        get_app_options = function() list(project_dir = temp_root),
        {
          project <- nemetonshiny:::create_project(name = "Persist Test")

          poly <- sf::st_polygon(list(rbind(
            c(900000, 6500000), c(901000, 6500000),
            c(901000, 6501000), c(900000, 6501000), c(900000, 6500000)
          )))
          indicators_sf <- sf::st_sf(
            ug_id = 1L,
            geometry = sf::st_sfc(poly, crs = 2154)
          )
          state <- shiny::reactiveValues(
            language = "fr",
            samples_refresh = 0L,
            current_project = list(
              id = project$id,
              indicators_sf = indicators_sf,
              metadata = list(name = "Persist Test")
            )
          )

          shiny::testServer(
            nemetonshiny:::mod_sampling_server,
            args = list(app_state = state),
            {
              session$setInputs(n_base = 4, n_over = 1, seed = 9,
                                region = "BFC", project_name = "persist_test",
                                generate = 1)
              # Plot reactive resolved -> save_samples() side-effect should
              # have fired.
              plots <- session$returned$sample_plots()
              expect_equal(nrow(plots), 5)
            }
          )

          gpkg_path <- file.path(project$path, "data", "samples.gpkg")
          expect_true(file.exists(gpkg_path))

          loaded <- nemetonshiny:::load_samples(project$id)
          expect_s3_class(loaded, "sf")
          expect_equal(nrow(loaded), 5)
        }
      )
    })
  })
})


# ---------------------------------------------------------------------
# nemeton::resolve_project_dem / _chm wiring (introduced 0.36.6.9001)
# ---------------------------------------------------------------------

test_that("clicking generate forwards resolve_project_dem output to create_sampling_plan as mnt", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  skip_if_not_installed("nemeton")
  skip_if_not_installed("terra")

  fake_dem <- make_fake_dem("LiDAR HD MNT")
  captured <- new.env(parent = emptyenv())
  captured$call <- NULL

  testthat::local_mocked_bindings(
    resolve_project_dem = function(...) fake_dem,
    resolve_project_chm = function(...) NULL,
    create_sampling_plan = function(zone, n_base, n_over, chm = NULL,
                                    mnt = NULL, ...) {
      captured$call <- list(chm = chm, mnt = mnt)
      # Synthetic plan: n_base + n_over POINTs spread on the zone bbox.
      bb <- sf::st_bbox(zone)
      n_total <- n_base + n_over
      x <- seq(bb["xmin"] + 50, bb["xmax"] - 50, length.out = n_total)
      y <- rep(mean(c(bb["ymin"], bb["ymax"])), n_total)
      pts <- sf::st_sf(
        plot_id     = seq_len(n_total),
        type        = c(rep("Base", n_base), rep("Over", n_over)),
        visit_order = seq_len(n_total),
        geometry    = sf::st_sfc(
          lapply(seq_len(n_total),
                 function(i) sf::st_point(c(x[i], y[i]))),
          crs = 2154
        )
      )
      attr(pts, "method") <- "mock"
      pts
    },
    .package = "nemeton"
  )

  shiny::testServer(
    nemetonshiny:::mod_sampling_server,
    args = list(app_state = make_fake_app_state()),
    {
      session$setInputs(n_base = 4, n_over = 1, seed = 1,
                        region = "BFC", project_name = "wire_dem",
                        generate = 1)
      plots <- session$returned$sample_plots()
      expect_true(inherits(plots, "sf"))
      expect_equal(nrow(plots), 5L)
    }
  )

  expect_false(is.null(captured$call))
  expect_true(inherits(captured$call$mnt, "SpatRaster"))
  expect_null(captured$call$chm)
})


test_that("clicking generate with no DEM shows dem_missing toast and skips create_sampling_plan", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("nemeton")

  called <- new.env(parent = emptyenv())
  called$create <- FALSE
  notifications <- list()

  testthat::local_mocked_bindings(
    resolve_project_dem = function(...) NULL,
    resolve_project_chm = function(...) NULL,
    create_sampling_plan = function(...) {
      called$create <- TRUE
      NULL
    },
    .package = "nemeton"
  )

  # Spy on shiny::showNotification to capture the id of the error toast.
  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., id = NULL) {
      notifications[[length(notifications) + 1L]] <<- list(id = id)
      invisible(id)
    },
    .package = "shiny"
  )

  shiny::testServer(
    nemetonshiny:::mod_sampling_server,
    args = list(app_state = make_fake_app_state()),
    {
      session$setInputs(n_base = 4, n_over = 1, seed = 1,
                        region = "BFC", project_name = "no_dem",
                        generate = 1)
      expect_null(session$returned$sample_plots())
    }
  )

  expect_false(called$create)
  ids <- vapply(notifications, function(n) n$id %||% NA_character_,
                character(1))
  expect_true(any(grepl("dem_missing", ids)))
})

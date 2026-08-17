# Tests for the Sentinel-2 NDVI composite (brief C2)
# R/service_compute.R - build_s2_ndvi_layer()

.mk_s2_cache <- function(dates) {
  dir.create("cache/layers/sentinel2", recursive = TRUE, showWarnings = FALSE)
  for (d in dates) {
    # Nom de scene au format lu par .pixel_scene_date_from_id()
    id <- sprintf("SENTINEL2A_%sT105031_L2A_T31UFQ_C_V1-0",
                  format(as.Date(d), "%Y%m%d"))
    dir.create(file.path("cache/layers/sentinel2", id), showWarnings = FALSE)
    writeLines("x", file.path("cache/layers/sentinel2", id, "B04.tif"))
  }
  "cache/layers"
}


test_that("build_s2_ndvi_layer returns NULL without a Sentinel-2 cache", {
  withr::with_tempdir({
    dir.create("cache/layers", recursive = TRUE)
    # Pas de repertoire sentinel2 du tout.
    expect_null(build_s2_ndvi_layer("cache/layers"))

    # Repertoire present mais vide : meme conclusion, le projet retombe sur
    # l'ortho WMS.
    dir.create("cache/layers/sentinel2", recursive = TRUE)
    expect_null(build_s2_ndvi_layer("cache/layers"))
  })
})


test_that("build_s2_ndvi_layer returns NULL on an absent cache_dir", {
  expect_null(build_s2_ndvi_layer(NULL))
  expect_null(build_s2_ndvi_layer(""))
})


test_that("only growing-season scenes are composited", {
  seen <- NULL
  testthat::local_mocked_bindings(
    build_index_stack = function(cache_dir, scenes_df, index, ...) {
      seen <<- scenes_df
      stop("stop apres capture")
    },
    .package = "nemeton"
  )

  withr::with_tempdir({
    cd <- .mk_s2_cache(c("2024-01-15", "2024-07-10", "2024-08-20",
                         "2024-11-30", "2025-06-05"))
    suppressWarnings(build_s2_ndvi_layer(cd))
  })

  expect_false(is.null(seen))
  # Janvier et novembre sont hors saison : un NDVI de houppier nu ne decrit pas
  # la meme grandeur qu'un NDVI de pleine vegetation.
  expect_equal(sort(format(seen$obs_date)),
               c("2024-07-10", "2024-08-20", "2025-06-05"))
})


test_that("all scenes are used when none falls in the growing season", {
  seen <- NULL
  testthat::local_mocked_bindings(
    build_index_stack = function(cache_dir, scenes_df, index, ...) {
      seen <<- scenes_df
      stop("stop apres capture")
    },
    .package = "nemeton"
  )

  withr::with_tempdir({
    cd <- .mk_s2_cache(c("2024-01-15", "2024-11-30"))
    suppressWarnings(build_s2_ndvi_layer(cd))
  })

  # Mieux vaut un composite hors saison que pas de composite du tout : le repli
  # WMS serait pire.
  expect_equal(nrow(seen), 2L)
})


test_that("the number of stacked scenes is capped, keeping the most recent", {
  seen <- NULL
  testthat::local_mocked_bindings(
    build_index_stack = function(cache_dir, scenes_df, index, ...) {
      seen <<- scenes_df
      stop("stop apres capture")
    },
    .package = "nemeton"
  )

  withr::with_tempdir({
    cd <- .mk_s2_cache(c("2020-07-01", "2021-07-01", "2022-07-01",
                         "2023-07-01", "2024-07-01"))
    suppressWarnings(build_s2_ndvi_layer(cd, max_scenes = 2L))
  })

  expect_equal(nrow(seen), 2L)
  expect_equal(sort(format(seen$obs_date)), c("2023-07-01", "2024-07-01"))
})


test_that("the index is always NDVI and comes from the core", {
  seen_index <- NULL
  testthat::local_mocked_bindings(
    build_index_stack = function(cache_dir, scenes_df, index, ...) {
      seen_index <<- index
      stop("stop apres capture")
    },
    .package = "nemeton"
  )

  withr::with_tempdir({
    cd <- .mk_s2_cache("2024-07-10")
    suppressWarnings(build_s2_ndvi_layer(cd))
  })

  # Regle 1 : l'app compose des dates, elle ne calcule pas d'indice.
  expect_equal(seen_index, "NDVI")
})


test_that("a failing core call degrades to NULL rather than throwing", {
  testthat::local_mocked_bindings(
    build_index_stack = function(...) stop("cache corrompu"),
    .package = "nemeton"
  )

  withr::with_tempdir({
    cd <- .mk_s2_cache("2024-07-10")
    expect_null(suppressWarnings(build_s2_ndvi_layer(cd)))
  })
})


test_that("the composite is clamped to [0, 1] and named ndvi", {
  skip_if_not_installed("terra")

  testthat::local_mocked_bindings(
    build_index_stack = function(cache_dir, scenes_df, index, ...) {
      r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2,
                       ymin = 0, ymax = 2, crs = "EPSG:2154")
      a <- r; terra::values(a) <- c(-0.4, 0.2, 0.8, 0.5)
      b <- r; terra::values(b) <- c(-0.2, 0.4, 0.6, 0.5)
      c(a, b)
    },
    .package = "nemeton"
  )

  withr::with_tempdir({
    cd <- .mk_s2_cache(c("2024-07-10", "2024-08-10"))
    out <- build_s2_ndvi_layer(cd)

    expect_s4_class(out, "SpatRaster")
    expect_equal(names(out), "ndvi")

    v <- as.numeric(terra::values(out))
    # Medianes par cellule : -0.3, 0.3, 0.7, 0.5 -> ecretees en bas a 0.
    expect_equal(v, c(0, 0.3, 0.7, 0.5), tolerance = 1e-6)
    expect_true(all(v >= 0 & v <= 1))
  })
})


test_that("the composite is cached and re-read instead of rebuilt", {
  skip_if_not_installed("terra")

  calls <- 0L
  testthat::local_mocked_bindings(
    build_index_stack = function(cache_dir, scenes_df, index, ...) {
      calls <<- calls + 1L
      r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2,
                       ymin = 0, ymax = 2, crs = "EPSG:2154")
      terra::values(r) <- c(0.5, 0.6, 0.7, 0.8)
      r
    },
    .package = "nemeton"
  )

  withr::with_tempdir({
    cd <- .mk_s2_cache("2024-07-10")
    first <- build_s2_ndvi_layer(cd)
    second <- build_s2_ndvi_layer(cd)

    expect_equal(calls, 1L)
    expect_equal(names(second), "ndvi")
    expect_equal(as.numeric(terra::values(first)),
                 as.numeric(terra::values(second)), tolerance = 1e-6)
  })
})

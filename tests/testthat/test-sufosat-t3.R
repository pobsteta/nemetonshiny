# Tests — Coupes rases → T3 continuité (SUFOSAT, spec 030)
#
# Couvre : acquisition + cache des rasters SUFOSAT (build_sufosat_layer),
# injection dans indicateur_t3_coupes_rases (compute_single_indicator), et
# persistance de la config (set_project_sufosat). Aucune logique métier n'est
# testée ici (elle vit dans le cœur) — uniquement le câblage app.

`%||%` <- function(a, b) if (is.null(a)) b else a

# AOI de test : petit polygone en Lambert-93 (EPSG:2154).
.sufosat_aoi <- function() {
  poly <- sf::st_polygon(list(matrix(c(
    899000, 6499000, 901000, 6499000, 901000, 6501000,
    899000, 6501000, 899000, 6499000), ncol = 2, byrow = TRUE)))
  sf::st_sf(id = 1L, geometry = sf::st_sfc(poly, crs = 2154))
}

# Petit SpatRaster couvrant l'AOI (sert de retour mock de load_theia_source).
.sufosat_rast <- function(v = 1) {
  r <- terra::rast(xmin = 898000, xmax = 902000, ymin = 6498000,
                   ymax = 6502000, resolution = 200, crs = "EPSG:2154")
  terra::values(r) <- v
  r
}

# ---------------------------------------------------------------------------
# build_sufosat_layer : acquisition Theia, cache, réutilisation, garde-fous
# ---------------------------------------------------------------------------

test_that("build_sufosat_layer fetches via Theia, caches, and reuses cache", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  # Le test mocke toute l'I/O Theia (load_theia_source), mais build_sufosat_layer
  # garde en amont sur theia_api_key_configured() (clés TLD_*). Sans clés (cas CI),
  # il retourne NULL avant d'atteindre les mocks. On fournit des clés factices pour
  # franchir le garde : le test reste hermétique (aucun réseau, tout est mocké).
  withr::local_envvar(c(TLD_ACCESS_KEY = "test-key", TLD_SECRET_KEY = "test-secret"))

  proj <- withr::local_tempdir()
  aoi  <- .sufosat_aoi()

  calls <- 0L
  testthat::local_mocked_bindings(
    theia_configure_s3 = function(...) invisible(TRUE),
    load_theia_source = function(source_key, aoi, asset = NULL, ...) {
      calls <<- calls + 1L
      .sufosat_rast(if (identical(asset, "dates")) 18001 else 95)
    },
    .package = "nemeton"
  )

  # 1er appel : configure S3 + charge les 2 assets + écrit le cache
  s1 <- nemetonshiny:::build_sufosat_layer(list(enabled = TRUE), proj, aoi = aoi)
  expect_type(s1, "list")
  expect_s4_class(s1$dates, "SpatRaster")
  expect_s4_class(s1$proba, "SpatRaster")
  expect_equal(calls, 2L)  # dates + proba
  cache <- list.files(file.path(proj, "cache", "layers", "sufosat"),
                      pattern = "\\.tif$")
  expect_length(cache, 2L)

  # 2e appel : lit le cache, ne rappelle PAS Theia
  s2 <- nemetonshiny:::build_sufosat_layer(list(enabled = TRUE), proj, aoi = aoi)
  expect_s4_class(s2$dates, "SpatRaster")
  expect_equal(calls, 2L)  # toujours 2
})

test_that("build_sufosat_layer returns NULL when Theia or AOI is unusable", {
  skip_if_not_installed("terra")
  proj <- withr::local_tempdir()

  # AOI absente / vide
  expect_null(nemetonshiny:::build_sufosat_layer(list(enabled = TRUE), proj, aoi = NULL))
  expect_null(nemetonshiny:::build_sufosat_layer(
    list(enabled = TRUE), proj, aoi = .sufosat_aoi()[0, ]))

  # theia_configure_s3 échoue → NULL (T3 restera NA)
  testthat::local_mocked_bindings(
    theia_configure_s3 = function(...) stop("no S3 creds"),
    .package = "nemeton"
  )
  expect_null(nemetonshiny:::build_sufosat_layer(
    list(enabled = TRUE), proj, aoi = .sufosat_aoi()))
})

# ---------------------------------------------------------------------------
# Injection : compute_single_indicator passe les rasters + params à T3
# ---------------------------------------------------------------------------

test_that("compute_single_indicator injects staged SUFOSAT rasters and params", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  captured <- new.env()
  assign("indicateur_test_t3",
         function(units, sufosat_dates = NULL, sufosat_proba = NULL,
                  window_years = 5, min_proba = 0.9) {
           captured$dates  <- sufosat_dates
           captured$proba  <- sufosat_proba
           captured$window <- window_years
           captured$proba_thr <- min_proba
           rep(1, nrow(units))
         },
         envir = globalenv())
  withr::defer(rm("indicateur_test_t3", envir = globalenv()))

  parcels <- .sufosat_aoi()
  layers <- structure(
    list(rasters = list(), vectors = list(),
         sufosat = list(dates = .sufosat_rast(18001), proba = .sufosat_rast(95),
                        window_years = 3L, min_proba = 0.8)),
    class = "nemeton_layers")

  out <- nemetonshiny:::compute_single_indicator("indicateur_test_t3",
                                                 parcels, layers)
  expect_equal(out, rep(1, nrow(parcels)))
  expect_s4_class(captured$dates, "SpatRaster")
  expect_s4_class(captured$proba, "SpatRaster")
  expect_equal(captured$window, 3L)       # UI param propagé
  expect_equal(captured$proba_thr, 0.8)   # UI param propagé
})

test_that("compute_single_indicator leaves T3 args at defaults without SUFOSAT", {
  skip_if_not_installed("sf")

  captured <- new.env()
  assign("indicateur_test_t3b",
         function(units, sufosat_dates = NULL, window_years = 5) {
           captured$dates  <- sufosat_dates
           captured$window <- window_years
           rep(NA_real_, nrow(units))
         },
         envir = globalenv())
  withr::defer(rm("indicateur_test_t3b", envir = globalenv()))

  parcels <- .sufosat_aoi()
  layers <- structure(list(rasters = list(), vectors = list()),
                      class = "nemeton_layers")  # pas de $sufosat

  out <- nemetonshiny:::compute_single_indicator("indicateur_test_t3b",
                                                 parcels, layers)
  expect_true(all(is.na(out)))         # T3 = NA, famille T inchangée
  expect_null(captured$dates)          # aucun raster injecté
  expect_equal(captured$window, 5)     # défaut cœur conservé
})

# ---------------------------------------------------------------------------
# Persistance : set_project_sufosat
# ---------------------------------------------------------------------------

test_that("set_project_sufosat persists the toggle + params and drops cache on disable", {
  skip_if_not_installed("sf")

  temp_dir <- withr::local_tempdir()
  withr::local_options(list(nemeton.project_dir = temp_dir))

  testthat::with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    {
      parcels <- .sufosat_aoi()
      parcels$code_insee <- "01001"
      proj <- nemetonshiny:::create_project(name = "SUFOSAT test", parcels = parcels)
      pid  <- proj$id

      expect_true(nemetonshiny:::set_project_sufosat(
        pid, enabled = TRUE, window_years = 3, min_proba = 0.8))
      meta <- nemetonshiny:::load_project_metadata(pid)
      expect_true(isTRUE(meta$sufosat$enabled))
      expect_equal(as.integer(meta$sufosat$window_years), 3L)
      expect_equal(as.numeric(meta$sufosat$min_proba), 0.8)

      # Simule un cache présent puis désactive → le cache doit disparaître
      ppath <- nemetonshiny:::get_project_path(pid)
      cache_dir <- file.path(ppath, "cache", "layers", "sufosat")
      dir.create(cache_dir, recursive = TRUE)
      writeLines("x", file.path(cache_dir, "dates_abc.tif"))

      expect_true(nemetonshiny:::set_project_sufosat(pid, enabled = FALSE))
      meta2 <- nemetonshiny:::load_project_metadata(pid)
      expect_false(isTRUE(meta2$sufosat$enabled))
      expect_false(dir.exists(cache_dir))
    }
  )
})

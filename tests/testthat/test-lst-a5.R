# Tests — Rafraîchissement urbain → A5 (LST Theia, spec 032)
#
# Couvre le plumbing app : acquisition + cache du raster LST (build_lst_layer),
# injection dans indicateur_a5_rafraichissement (compute_single_indicator),
# gating de A5 dans list_available_indicators() et persistance de la config
# (set_project_lst_urbain). Aucune logique métier ici (elle vit dans le cœur) —
# uniquement le câblage app. Miroir de test-sufosat-t3.R.

`%||%` <- function(a, b) if (is.null(a)) b else a

# AOI de test : petit polygone en Lambert-93 (EPSG:2154).
.lst_aoi <- function() {
  poly <- sf::st_polygon(list(matrix(c(
    899000, 6499000, 901000, 6499000, 901000, 6501000,
    899000, 6501000, 899000, 6499000), ncol = 2, byrow = TRUE)))
  sf::st_sf(id = 1L, geometry = sf::st_sfc(poly, crs = 2154))
}

# Petit SpatRaster LST (kelvin) couvrant l'AOI (retour mock de load_theia_source).
.lst_rast <- function(v = 300) {
  r <- terra::rast(xmin = 898000, xmax = 902000, ymin = 6498000,
                   ymax = 6502000, resolution = 200, crs = "EPSG:2154")
  terra::values(r) <- v
  r
}

# ---------------------------------------------------------------------------
# build_lst_layer : acquisition Theia, cache, réutilisation, garde-fous
# ---------------------------------------------------------------------------

test_that("build_lst_layer fetches via Theia, caches, and reuses cache", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  # Garde amont sur les clés Theia (comme SUFOSAT) : clés factices → hermétique.
  withr::local_envvar(c(TLD_ACCESS_KEY = "test-key", TLD_SECRET_KEY = "test-secret"))

  proj <- withr::local_tempdir()
  aoi  <- .lst_aoi()

  calls <- 0L
  testthat::local_mocked_bindings(
    load_theia_source = function(source_key, aoi, asset = NULL, ...) {
      calls <<- calls + 1L
      .lst_rast(300)
    },
    .package = "nemeton"
  )

  # 1er appel : charge la LST + écrit le cache
  s1 <- nemetonshiny:::build_lst_layer(list(enabled = TRUE), proj, aoi = aoi)
  expect_s4_class(s1, "SpatRaster")
  expect_equal(calls, 1L)
  cache <- list.files(file.path(proj, "cache", "layers", "lst"),
                      pattern = "\\.tif$")
  expect_length(cache, 1L)

  # 2e appel : lit le cache, ne rappelle PAS Theia
  s2 <- nemetonshiny:::build_lst_layer(list(enabled = TRUE), proj, aoi = aoi)
  expect_s4_class(s2, "SpatRaster")
  expect_equal(calls, 1L)  # toujours 1
})

test_that("build_lst_layer returns NULL when Theia key absent, AOI unusable, or fetch fails", {
  skip_if_not_installed("terra")
  proj <- withr::local_tempdir()

  # AOI absente / vide → NULL (avec clés présentes pour isoler la cause AOI)
  withr::with_envvar(c(TLD_ACCESS_KEY = "k", TLD_SECRET_KEY = "s"), {
    expect_null(nemetonshiny:::build_lst_layer(list(enabled = TRUE), proj, aoi = NULL))
    expect_null(nemetonshiny:::build_lst_layer(
      list(enabled = TRUE), proj, aoi = .lst_aoi()[0, ]))
  })

  # Clés Theia absentes → NULL (garde amont, A5 restera NA)
  withr::with_envvar(c(TLD_ACCESS_KEY = "", TLD_SECRET_KEY = ""), {
    testthat::local_mocked_bindings(
      .theia_apikey_path = function() tempfile("nonexistent"),
      .package = "nemetonshiny")
    expect_null(nemetonshiny:::build_lst_layer(
      list(enabled = TRUE), proj, aoi = .lst_aoi()))
  })

  # Fetch Theia échoue (hors couverture urbaine) → NULL (pas de régression)
  withr::with_envvar(c(TLD_ACCESS_KEY = "k", TLD_SECRET_KEY = "s"), {
    testthat::local_mocked_bindings(
      load_theia_source = function(...) stop("out of coverage"),
      .package = "nemeton")
    expect_null(nemetonshiny:::build_lst_layer(
      list(enabled = TRUE), proj, aoi = .lst_aoi()))
  })
})

# ---------------------------------------------------------------------------
# list_available_indicators : A5 gaté sur metadata$lst_urbain$enabled
# ---------------------------------------------------------------------------

test_that("list_available_indicators gates A5 on lst_urbain$enabled", {
  base <- nemetonshiny:::list_available_indicators()
  expect_false("indicateur_a5_rafraichissement" %in% base)   # défaut : pas de A5
  expect_false("indicateur_a5_rafraichissement" %in%
               nemetonshiny:::list_available_indicators(list(lst_urbain = list(enabled = FALSE))))
  enabled <- nemetonshiny:::list_available_indicators(list(lst_urbain = list(enabled = TRUE)))
  expect_true("indicateur_a5_rafraichissement" %in% enabled)
  # les autres indicateurs restent tous présents (A5 ajouté, rien retiré)
  expect_true(all(base %in% enabled))
})

# ---------------------------------------------------------------------------
# Injection : compute_single_indicator passe le raster LST + buffer_m à A5
# ---------------------------------------------------------------------------

test_that("compute_single_indicator injects staged LST raster and buffer_m", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  captured <- new.env()
  assign("indicateur_test_a5",
         function(units, lst = NULL, buffer_m = 500) {
           captured$lst      <- lst
           captured$buffer_m <- buffer_m
           rep(1, nrow(units))
         },
         envir = globalenv())
  withr::defer(rm("indicateur_test_a5", envir = globalenv()))

  parcels <- .lst_aoi()
  layers <- structure(
    list(rasters = list(lst = .lst_rast(295)), vectors = list(),
         lst_buffer_m = 800),
    class = "nemeton_layers")

  out <- nemetonshiny:::compute_single_indicator("indicateur_test_a5",
                                                 parcels, layers)
  expect_equal(out, rep(1, nrow(parcels)))
  expect_s4_class(captured$lst, "SpatRaster")
  expect_equal(captured$buffer_m, 800)   # buffer_m UI propagé
})

test_that("compute_single_indicator leaves A5 args at defaults without LST", {
  skip_if_not_installed("sf")

  captured <- new.env()
  assign("indicateur_test_a5b",
         function(units, lst = NULL, buffer_m = 500) {
           captured$lst      <- lst
           captured$buffer_m <- buffer_m
           rep(NA_real_, nrow(units))
         },
         envir = globalenv())
  withr::defer(rm("indicateur_test_a5b", envir = globalenv()))

  parcels <- .lst_aoi()
  layers <- structure(list(rasters = list(), vectors = list()),
                      class = "nemeton_layers")   # pas de $rasters$lst

  out <- nemetonshiny:::compute_single_indicator("indicateur_test_a5b",
                                                 parcels, layers)
  expect_true(all(is.na(out)))       # A5 = NA, famille A inchangée
  expect_null(captured$lst)          # aucun raster injecté
  expect_equal(captured$buffer_m, 500)   # défaut cœur conservé
})

# ---------------------------------------------------------------------------
# Persistance : set_project_lst_urbain
# ---------------------------------------------------------------------------

test_that("set_project_lst_urbain persists the toggle + buffer and drops cache on disable", {
  skip_if_not_installed("sf")

  temp_dir <- withr::local_tempdir()
  withr::local_options(list(nemeton.project_dir = temp_dir))

  testthat::with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    {
      parcels <- .lst_aoi()
      parcels$code_insee <- "01001"
      proj <- nemetonshiny:::create_project(name = "LST test", parcels = parcels)
      pid  <- proj$id

      expect_true(nemetonshiny:::set_project_lst_urbain(
        pid, enabled = TRUE, buffer_m = 800))
      meta <- nemetonshiny:::load_project_metadata(pid)
      expect_true(isTRUE(meta$lst_urbain$enabled))
      expect_equal(as.numeric(meta$lst_urbain$buffer_m), 800)

      # Simule un cache présent puis désactive → le cache doit disparaître
      ppath <- nemetonshiny:::get_project_path(pid)
      cache_dir <- file.path(ppath, "cache", "layers", "lst")
      dir.create(cache_dir, recursive = TRUE)
      writeLines("x", file.path(cache_dir, "lst_abc.tif"))

      expect_true(nemetonshiny:::set_project_lst_urbain(pid, enabled = FALSE))
      meta2 <- nemetonshiny:::load_project_metadata(pid)
      expect_false(isTRUE(meta2$lst_urbain$enabled))
      expect_false(dir.exists(cache_dir))
    }
  )
})

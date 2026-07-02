# Tests — Forêt ancienne → N2 continuité (spec 031)
#
# Couvre : construction + cache du masque (build_foret_ancienne_layer),
# injection dans indicateur_n2_continuite (compute_single_indicator), et
# persistance de la source sur le projet (set/clear_project_foret_ancienne).

`%||%` <- function(a, b) if (is.null(a)) b else a

# Petit sf vecteur servant de "source historique" (et de retour de masque).
.fa_dummy_sf <- function(val = TRUE) {
  poly <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
  sf::st_sf(foret_ancienne = val, geometry = sf::st_sfc(poly, crs = 2154))
}

# ---------------------------------------------------------------------------
# build_foret_ancienne_layer : build, cache, réutilisation, garde-fous
# ---------------------------------------------------------------------------

test_that("build_foret_ancienne_layer builds via core, caches, and reuses cache", {
  skip_if_not_installed("sf")

  proj <- withr::local_tempdir()
  dir.create(file.path(proj, "data"), recursive = TRUE, showWarnings = FALSE)
  src <- file.path(proj, "data", "foret_ancienne_source.gpkg")
  sf::st_write(.fa_dummy_sf(), src, quiet = TRUE)

  cfg <- list(path = "data/foret_ancienne_source.gpkg",
              kind = "vector", min_area_m2 = 0)

  calls <- 0L
  mask <- .fa_dummy_sf()
  testthat::local_mocked_bindings(
    build_foret_ancienne_mask = function(source, forest_class = NULL,
                                         threshold = NULL, min_area_m2 = 0,
                                         crs = NULL) {
      calls <<- calls + 1L
      mask
    },
    .package = "nemeton"
  )

  # 1er appel : construit via le cœur + écrit le cache
  fa1 <- nemetonshiny:::build_foret_ancienne_layer(cfg, proj, crs = 2154)
  expect_s3_class(fa1, "sf")
  expect_true(all(fa1$foret_ancienne))
  expect_equal(calls, 1L)
  cache_files <- list.files(
    file.path(proj, "cache", "layers", "foret_ancienne"),
    pattern = "\\.gpkg$", full.names = TRUE)
  expect_length(cache_files, 1L)

  # 2e appel : lit le cache, ne rappelle PAS le cœur
  fa2 <- nemetonshiny:::build_foret_ancienne_layer(cfg, proj, crs = 2154)
  expect_s3_class(fa2, "sf")
  expect_equal(calls, 1L)  # toujours 1 : le masque vient du cache
})

test_that("build_foret_ancienne_layer returns NULL without a usable source", {
  proj <- withr::local_tempdir()

  # Pas de config
  expect_null(nemetonshiny:::build_foret_ancienne_layer(NULL, proj))
  # Config sans chemin
  expect_null(nemetonshiny:::build_foret_ancienne_layer(list(kind = "vector"), proj))
  # Chemin pointant sur un fichier absent
  expect_null(nemetonshiny:::build_foret_ancienne_layer(
    list(path = "data/absent.gpkg", kind = "vector"), proj))
})

# ---------------------------------------------------------------------------
# Injection : compute_single_indicator passe foret_ancienne à l'indicateur
# ---------------------------------------------------------------------------

test_that("compute_single_indicator injects a staged foret_ancienne layer", {
  skip_if_not_installed("sf")

  captured <- new.env()
  # Stub global résolu par get(func_name) dans compute_single_indicator.
  assign("indicateur_test_fa",
         function(units, foret_ancienne = NULL) {
           captured$got <- foret_ancienne
           rep(1, nrow(units))
         },
         envir = globalenv())
  withr::defer(rm("indicateur_test_fa", envir = globalenv()))

  parcels <- .fa_dummy_sf()
  fa <- .fa_dummy_sf()
  layers <- structure(list(rasters = list(),
                           vectors = list(foret_ancienne = fa)),
                      class = "nemeton_layers")

  out <- nemetonshiny:::compute_single_indicator("indicateur_test_fa",
                                                 parcels, layers)
  expect_equal(out, rep(1, nrow(parcels)))
  expect_s3_class(captured$got, "sf")
  expect_true(all(captured$got$foret_ancienne))
})

# ---------------------------------------------------------------------------
# Persistance : set/clear_project_foret_ancienne
# ---------------------------------------------------------------------------

test_that("set/clear_project_foret_ancienne persist and remove the source", {
  skip_if_not_installed("sf")

  temp_dir <- withr::local_tempdir()
  withr::local_options(list(nemeton.project_dir = temp_dir))

  testthat::with_mocked_bindings(
    get_app_options = function() list(project_dir = temp_dir),
    {
      parcels <- .fa_dummy_sf()
      parcels$code_insee <- "01001"
      proj <- nemetonshiny:::create_project(name = "FA test", parcels = parcels)
      pid <- proj$id

      # Fichier source uploadé (temp)
      up <- file.path(withr::local_tempdir(), "cassini.gpkg")
      sf::st_write(.fa_dummy_sf(), up, quiet = TRUE)

      ok <- nemetonshiny:::set_project_foret_ancienne(
        pid, up, "cassini.gpkg",
        forest_class = NULL, threshold = NULL, min_area_m2 = 5000)
      expect_true(ok)

      meta <- nemetonshiny:::load_project_metadata(pid)
      expect_false(is.null(meta$foret_ancienne))
      expect_equal(meta$foret_ancienne$kind, "vector")
      expect_equal(as.numeric(meta$foret_ancienne$min_area_m2), 5000)
      expect_equal(meta$foret_ancienne$source_name, "cassini.gpkg")
      # Le fichier a été copié dans le projet
      ppath <- nemetonshiny:::get_project_path(pid)
      expect_true(file.exists(file.path(ppath, meta$foret_ancienne$path)))

      # clear : métadonnée retirée + fichier supprimé
      expect_true(nemetonshiny:::clear_project_foret_ancienne(pid))
      meta2 <- nemetonshiny:::load_project_metadata(pid)
      expect_null(meta2$foret_ancienne)
      expect_length(
        list.files(file.path(ppath, "data"),
                   pattern = "^foret_ancienne_source\\."), 0L)
    }
  )
})

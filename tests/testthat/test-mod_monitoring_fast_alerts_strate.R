# Mission 017 : FAST calcule sur `_tot`, decoupe par strate a l'affichage
# (parite FORDEAD / RECONFORT).
#
# Raster du harnais : 2x2, EPSG:2154, pixels de 10 m, valeurs row-major depuis
# le coin haut-gauche : TL=0, TR=1, BL=2, BR=4. Le coeur n'est pas appele :
# `.compute_fast_mask` est moque (il memorise la zone recue).

.s017_project <- function(env = parent.frame()) {
  proj <- list(path = withr::local_tempdir(.local_envir = env))
  cd <- file.path(proj$path, "cache", "layers", "sentinel2")
  dir.create(cd, recursive = TRUE)
  sid <- "S2A_MSIL2A_20250610T103021_T31TGM"
  dir.create(file.path(cd, sid))
  file.create(file.path(cd, sid, "B05.tif"))
  proj
}

# Strate qui ne couvre que le pixel haut-droite (TR, valeur 1).
.s017_aoi <- function(...) {
  sf::st_sf(zone_id = 6L, geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(950011, 6620011), c(950019, 6620011),
                              c(950019, 6620019), c(950011, 6620019),
                              c(950011, 6620011)))), crs = 2154))
}

.s017_harness <- function(zone_log, env = parent.frame()) {
  mask_tif <- withr::local_tempfile(fileext = ".tif", .local_envir = env)
  r <- terra::rast(nrows = 2, ncols = 2, crs = "EPSG:2154",
                   xmin = 950000, xmax = 950020, ymin = 6620000, ymax = 6620020,
                   vals = c(0, 1, 2, 4))
  terra::writeRaster(r, mask_tif)
  testthat::local_mocked_bindings(
    get_monitoring_db_connection   = function(...) "CON",
    close_monitoring_db_connection = function(...) invisible(NULL),
    get_monitoring_zone_aoi        = .s017_aoi,
    .compute_fast_mask = function(con, zone, index, ...) {
      zone_log$zones <- c(zone_log$zones, zone)
      mask_tif
    },
    .env = env
  )
}

.s017_args <- function(app_state, zone, run_zone) {
  args <- list(
    app_state    = app_state,
    zone_id_r    = zone,
    date_range_r = shiny::reactive(as.Date(c("2017-01-01", "2025-12-31"))),
    thresholds_r = shiny::reactive(list(ndvi = 0.6, nbr = 0.5, ndmi = 0.2,
                                        window_days = 30L))
  )
  if (!is.null(run_zone)) args$run_zone_id_r <- run_zone
  args
}

.s017_state <- function(env = parent.frame()) {
  shiny::reactiveValues(language = "fr", active_main_tab = "monitoring",
                        current_project = .s017_project(env))
}

test_that("FAST computes on the _tot zone, never on the displayed stratum", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  log <- new.env(); log$zones <- character(0)
  .s017_harness(log)
  shiny::testServer(
    nemetonshiny:::mod_monitoring_fast_alerts_server,
    args = .s017_args(.s017_state(), shiny::reactive("6"), shiny::reactive("5")),
    {
      session$setInputs(mode = "count", index = "NDVI")
      session$elapse(500); session$flushReact()
      expect_identical(log$zones, "5")   # id _tot exact, jamais la strate "6"
    }
  )
})

test_that("changing the stratum does not recompute FAST nor raise the compute toast", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  log <- new.env(); log$zones <- character(0)
  .s017_harness(log)
  strate <- shiny::reactiveVal("5")   # d'abord _tot
  shiny::testServer(
    nemetonshiny:::mod_monitoring_fast_alerts_server,
    args = .s017_args(.s017_state(), strate, shiny::reactive("5")),
    {
      session$setInputs(mode = "count", index = "NDVI")
      session$elapse(500); session$flushReact()
      expect_length(log$zones, 1L)       # garde-fou : le calcul initial a eu lieu
      strate("6")                        # _tot -> _res
      session$flushReact(); session$elapse(500); session$flushReact()
      expect_length(log$zones, 1L)       # aucun nouvel appel au coeur
      expect_false(session$getReturned()$computing())
    }
  )
})

test_that("the displayed FAST raster is the _tot mask clipped to the stratum", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  log <- new.env(); log$zones <- character(0)
  .s017_harness(log)
  strate <- shiny::reactiveVal("6")
  shiny::testServer(
    nemetonshiny:::mod_monitoring_fast_alerts_server,
    args = .s017_args(.s017_state(), strate, shiny::reactive("5")),
    {
      session$setInputs(mode = "count", index = "NDVI")
      session$elapse(500); session$flushReact()
      v <- terra::values(session$getReturned()$raster())[, 1]
      # TR (dans la strate) garde sa classe ; TL, BL, BR (hors strate) -> NA.
      expect_identical(unname(v), c(NA_real_, 1, NA_real_, NA_real_))

      # Retour sur _tot : le meme calcul, NON decoupe.
      strate("5"); session$flushReact()
      v2 <- terra::values(session$getReturned()$raster())[, 1]
      expect_identical(unname(v2), c(0, 1, 2, 4))
      expect_length(log$zones, 1L)
    }
  )
})

test_that("FAST without a _tot zone computes on the picked zone (no error)", {
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  log <- new.env(); log$zones <- character(0)
  .s017_harness(log)
  shiny::testServer(
    nemetonshiny:::mod_monitoring_fast_alerts_server,
    # Pas de `run_zone_id_r` : defaut = zone_id_r (repli, projet sans `_tot`).
    args = .s017_args(.s017_state(), shiny::reactive("6"), NULL),
    {
      session$setInputs(mode = "count", index = "NDVI")
      session$elapse(500); session$flushReact()
      expect_identical(log$zones, "6")
      expect_equal(unname(terra::values(session$getReturned()$raster())[, 1]),
                   c(0, 1, 2, 4))   # strate = zone du calcul : pas de decoupe
    }
  )
})

test_that(".fast_stratum_aoi is NULL for the calculation zone and for NA ids", {
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .s017_aoi)
  expect_null(nemetonshiny:::.fast_stratum_aoi("con", "5", "5"))
  expect_null(nemetonshiny:::.fast_stratum_aoi("con", "6", NA_character_))
  expect_null(nemetonshiny:::.fast_stratum_aoi("con", "NA", "5"))
  expect_s3_class(nemetonshiny:::.fast_stratum_aoi("con", "6", "5"), "sf")
})

test_that("a failed stratum clip leaves the _tot raster unclipped", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 2, ncols = 2, crs = "EPSG:2154",
                   xmin = 950000, xmax = 950020, ymin = 6620000, ymax = 6620020,
                   vals = c(0, 1, 2, 4))
  # Pas un sf : st_transform() leve -> repli sur le raster non decoupe.
  out <- suppressMessages(nemetonshiny:::.clip_fast_to_stratum(r, "pas-un-sf"))
  expect_equal(unname(terra::values(out)[, 1]), c(0, 1, 2, 4))
})

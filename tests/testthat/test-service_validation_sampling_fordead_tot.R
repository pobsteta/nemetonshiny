# Mission 019 : Plan de validation FORDEAD lu sur `_tot` (id 5), la strate
# (id 6) ne fait que borner le tirage et l'affichage. 100 % hors ligne : le
# coeur et la base sont moques. NB : aucun expect_* dans un mock (les lecteurs
# sont enveloppes dans tryCatch) ; on capture dans un environnement.

.m19_aoi <- function(con, zone_id) {
  # La strate (6) ne couvre que la maille TR du raster ci-dessous.
  x0 <- if (identical(as.integer(zone_id), 6L)) 950011 else 950000
  x1 <- if (identical(as.integer(zone_id), 6L)) 950019 else 950020
  sf::st_sf(zone_id = as.integer(zone_id), geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(x0, 6620011), c(x1, 6620011), c(x1, 6620019),
                              c(x0, 6620019), c(x0, 6620011)))), crs = 2154))
}

# 2x2, EPSG:2154 : TL=0, TR=1, BL=2, BR=4.
.m19_mask <- function() {
  terra::rast(nrows = 2, ncols = 2, crs = "EPSG:2154",
              xmin = 950000, xmax = 950020, ymin = 6620000, ymax = 6620020,
              vals = c(0, 1, 2, 4))
}

# Projet avec caches FORDEAD sous les zones demandees (fichier de provenance).
.m19_project <- function(zones_with_cache, env = parent.frame()) {
  path <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(path, "cache", "layers", "fordead"), recursive = TRUE)
  for (z in zones_with_cache) {
    d <- file.path(path, "cache", "layers", "fordead", sprintf("zone_%d", z))
    dir.create(d, recursive = TRUE)
    file.create(file.path(d, sprintf("dieback_mask_RUN%d.tif", z)))
  }
  list(id = "p", path = path, metadata = list(monitoring_zone_id = 6L),
       indicators_sf = NULL)
}

.m19_zones <- function(with_tot = TRUE) {
  function(con, project_uuid) {
    if (with_tot) data.frame(id = c(5L, 6L), name = c("p_tot", "p_res"))
    else data.frame(id = 6L, name = "p_res")
  }
}

# Mock du lecteur coeur : `available` = zones ayant un masque.
.m19_reader <- function(seen, available) {
  function(con, zone_id, run_id = NULL, cache_dir) {
    seen$read <- c(seen$read, zone_id)
    if (zone_id %in% available) .m19_mask() else NULL
  }
}

.m19_gen <- function(proj) {
  nemetonshiny:::generate_validation_plan(
    "con", proj, source = "FORDEAD", zone_id = 6L,
    n_validation = 1L, n_control = 0L)
}

.m19_mock_plan <- function(zone, alert_raster, ...) {
  sf::st_sf(plot_id = "V01", type = "Validation", alert_class = 4L,
            source = "FORDEAD",
            geometry = sf::st_sfc(sf::st_point(c(950015, 6620015)),
                                  crs = 2154))
}

# ---- Generation ------------------------------------------------------------

test_that("FORDEAD plan on a stratum reads the mask on _tot, provenance from _tot", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m19_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m19_zones(TRUE),
    read_fordead_dieback_mask = .m19_reader(seen, available = c(5L, 6L)),
    create_validation_sampling_plan = .m19_mock_plan,
    .package = "nemeton"
  )
  plan <- .m19_gen(.m19_project(c(5L, 6L)))
  expect_identical(seen$read, 5L)                       # lu sur _tot seul
  expect_identical(plan$zone_id, 6L)                    # le plan reste a la strate
  expect_identical(plan$source_run_id, "RUN5")          # provenance = _tot
})

test_that("FORDEAD fallback (a): no _tot zone in DB reads the stratum", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m19_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m19_zones(FALSE),
    read_fordead_dieback_mask = .m19_reader(seen, available = 6L),
    create_validation_sampling_plan = .m19_mock_plan,
    .package = "nemeton"
  )
  plan <- .m19_gen(.m19_project(6L))
  expect_identical(seen$read, 6L)
  expect_identical(plan$source_run_id, "RUN6")
})

test_that("FORDEAD fallback (b): _tot without mask falls back to the stratum", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m19_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m19_zones(TRUE),
    read_fordead_dieback_mask = .m19_reader(seen, available = 6L),
    create_validation_sampling_plan = .m19_mock_plan,
    .package = "nemeton"
  )
  plan <- .m19_gen(.m19_project(6L))
  expect_identical(seen$read, c(5L, 6L))               # _tot d'abord, puis strate
  expect_identical(plan$source_run_id, "RUN6")         # provenance = zone lue
})

test_that("FORDEAD plan with no mask anywhere aborts validation_no_mask", {
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m19_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m19_zones(TRUE),
    read_fordead_dieback_mask = function(...) NULL,
    .package = "nemeton"
  )
  expect_error(.m19_gen(.m19_project(integer(0))), class = "validation_no_mask")
})

# ---- Lecture pour l'affichage (histogramme, surcouche) ---------------------

test_that("FORDEAD mask for the stratum counts only the stratum's pixels", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m19_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m19_zones(TRUE),
    read_fordead_dieback_mask = .m19_reader(seen, available = c(5L, 6L)),
    .package = "nemeton"
  )
  r <- nemetonshiny:::.read_fordead_mask_stratum(
    "con", .m19_project(c(5L, 6L)), 6L)
  expect_identical(seen$read, 5L)
  dist <- nemetonshiny:::.validation_class_distribution(r)
  # Seul TR (classe 1) est dans la strate ; TL, BL, BR sont mis a NA.
  expect_identical(unname(dist), c(0L, 1L, 0L, 0L, 0L))
})

test_that("FORDEAD mask: stratum = _tot is not clipped", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m19_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m19_zones(TRUE),
    read_fordead_dieback_mask = .m19_reader(seen, available = 5L),
    .package = "nemeton"
  )
  r <- nemetonshiny:::.read_fordead_mask_stratum(
    "con", .m19_project(5L), 5L)
  expect_identical(
    unname(nemetonshiny:::.validation_class_distribution(r)),
    c(1L, 1L, 1L, 0L, 1L))
})

test_that("FORDEAD mask: fallback to the stratum is not clipped, NULL when absent", {
  skip_if_not_installed("sf"); skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  testthat::local_mocked_bindings(get_monitoring_zone_aoi = .m19_aoi)
  testthat::local_mocked_bindings(
    find_zones_by_project = .m19_zones(TRUE),
    read_fordead_dieback_mask = .m19_reader(seen, available = 6L),
    .package = "nemeton"
  )
  r <- nemetonshiny:::.read_fordead_mask_stratum(
    "con", .m19_project(6L), 6L)
  # Masque ancien (fait sur la strate) : rendu tel quel, sans re-decoupe.
  expect_identical(
    unname(nemetonshiny:::.validation_class_distribution(r)),
    c(1L, 1L, 1L, 0L, 1L))
  testthat::local_mocked_bindings(
    read_fordead_dieback_mask = function(...) NULL, .package = "nemeton")
  expect_null(nemetonshiny:::.read_fordead_mask_stratum(
    "con", .m19_project(6L), 6L))
})

# ---- Cablage du module : l'histogramme lit _tot et decoupe -----------------

test_that("FORDEAD validation sub-tab histogram is read on _tot and clipped", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  seen <- new.env(); seen$read <- integer(0)
  proj <- .m19_project(c(5L, 6L))
  testthat::local_mocked_bindings(
    get_monitoring_db_connection   = function(...) structure(list(), class = "fakecon"),
    close_monitoring_db_connection = function(con) invisible(TRUE),
    get_monitoring_zone_aoi = .m19_aoi
  )
  testthat::local_mocked_bindings(
    find_zones_by_project = .m19_zones(TRUE),
    read_fordead_dieback_mask = .m19_reader(seen, available = c(5L, 6L)),
    .package = "nemeton"
  )
  app_state <- shiny::reactiveValues(
    language = "fr", active_main_tab = "monitoring", current_project = proj)
  res <- new.env()
  shiny::testServer(
    nemetonshiny:::mod_validation_sampling_server,
    args = list(id = "fd", app_state = app_state,
                zone_id_r    = shiny::reactive("6"),
                mode_r       = shiny::reactive("health"),
                thresholds_r = shiny::reactive(list(index = "NDVI")),
                date_range_r = shiny::reactive(NULL),
                source_fixed = "FORDEAD"),
    {
      session$setInputs(classes = c("3", "4"), control_classes = "0")
      session$flushReact()
      res$dist <- class_distribution_r()
    }
  )
  expect_true(5L %in% seen$read)
  expect_false(6L %in% seen$read)
  expect_identical(unname(res$dist), c(0L, 1L, 0L, 0L, 0L))
})

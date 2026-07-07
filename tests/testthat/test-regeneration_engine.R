# Tests — moteurs réels reGénération (option B, spec 027 L1/L2)
#
# Couvre le plumbing app (résolution grille LiDAR HD, garde-fous de prérequis
# par moteur, run des DEUX moteurs microclimf + BILJOU et mise en cache). Les
# vrais runs (LiDAR HD + ERA5 pour microclimf ; SAFRAN/ERA5 pour BILJOU) ne sont
# PAS exécutés ici (non-CI, validés côté cœur) — les fonctions nemeton sont
# mockées.

`%||%` <- function(a, b) if (is.null(a)) b else a

.engine_units <- function(n = 2) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- i * 10
    sf::st_polygon(list(matrix(c(
      x0, 0, x0 + 5, 0, x0 + 5, 5, x0, 5, x0, 0), ncol = 2, byrow = TRUE)))
  })
  sf::st_sf(ug_id = seq_len(n), geometry = sf::st_sfc(polys, crs = 2154))
}

# Crée <project>/cache/layers/{lidar_mnt,lidar_mnh} avec une tuile .tif factice.
# `with_nuage = TRUE` ajoute aussi lidar_nuage/*.laz (structure de végétation
# LiDAR HD → `las` passé au cœur ; sinon repli LAI Sentinel-2/PROSAIL).
.make_lidar_grid <- function(project_path, with_nuage = TRUE) {
  base <- file.path(project_path, "cache", "layers")
  for (d in c("lidar_mnt", "lidar_mnh")) {
    dd <- file.path(base, d)
    dir.create(dd, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(dd, "tile_0000.tif"))
  }
  if (isTRUE(with_nuage)) {
    dd <- file.path(base, "lidar_nuage")
    dir.create(dd, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(dd, "tile_0000.copc.laz"))
  }
  invisible(project_path)
}

test_that("resolve_regen_lidar_grid finds the grid only when both dirs hold .tif", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd()
    expect_null(nemetonshiny:::resolve_regen_lidar_grid(p))       # rien encore
    expect_null(nemetonshiny:::resolve_regen_lidar_grid(NULL))    # pas de projet

    dir.create(file.path(p, "cache", "layers", "lidar_mnt"), recursive = TRUE)
    file.create(file.path(p, "cache", "layers", "lidar_mnt", "a.tif"))
    expect_null(nemetonshiny:::resolve_regen_lidar_grid(p))       # mnh manquant

    # Grille sans nuage : mnt+mnh résolus, las_dir NULL.
    .make_lidar_grid(p, with_nuage = FALSE)
    g <- nemetonshiny:::resolve_regen_lidar_grid(p)
    expect_type(g, "list")
    expect_true(all(c("mnt_dir", "mnh_dir", "las_dir") %in% names(g)))
    expect_true(dir.exists(g$mnt_dir) && dir.exists(g$mnh_dir))
    expect_null(g$las_dir)                                # pas de nuage

    # Ajout d'une dalle nuage (.copc.laz) → las_dir résolu.
    .make_lidar_grid(p, with_nuage = TRUE)
    g2 <- nemetonshiny:::resolve_regen_lidar_grid(p)
    expect_true(grepl("lidar_nuage$", g2$las_dir))
  })
})

test_that("regen_cds_credentials_ready is FALSE without ecmwfr or a key", {
  withr::with_envvar(c(CDSAPI_KEY = "", ECMWFR_CDS_KEY = ""), {
    if (!requireNamespace("ecmwfr", quietly = TRUE)) {
      expect_false(nemetonshiny:::regen_cds_credentials_ready())
    } else {
      expect_type(nemetonshiny:::regen_cds_credentials_ready(), "logical")
    }
  })
})

test_that("regen_cds_credentials_ready is TRUE with an env key (ecmwfr present)", {
  skip_if_not_installed("ecmwfr")
  withr::with_envvar(c(CDSAPI_KEY = "deadbeef-cds-key"), {
    expect_true(nemetonshiny:::regen_cds_credentials_ready())
  })
})

test_that("regen_cds_credentials_ready detects the ecmwfr_<user> env pattern", {
  skip_if_not_installed("ecmwfr")
  withr::with_envvar(
    c(CDSAPI_KEY = "", ECMWFR_CDS_KEY = "", ecmwfr_testuser = "ca1d-key"), {
      expect_true(nemetonshiny:::regen_cds_credentials_ready())
    })
})

test_that("regen_engine_prereqs: SAFRAN needs no CDS, ERA5/microclimf do", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd()
    # SAFRAN, pas de clé, pas de grille → BILJOU prêt (ok=TRUE), microclimf non.
    testthat::local_mocked_bindings(regen_cds_credentials_ready = function() FALSE)
    s <- nemetonshiny:::regen_engine_prereqs(p, "safran")
    expect_true(s$ok)
    expect_false(s$microclimf)
    expect_true(s$biljou)

    # ERA5 sans clé → rien de prêt → raison CDS.
    e <- nemetonshiny:::regen_engine_prereqs(p, "era5")
    expect_false(e$ok)
    expect_equal(e$reason, "regen_engine_prereq_cds")
    expect_false(e$biljou)

    # Grille + clé → microclimf prêt aussi (ERA5 débloqué).
    .make_lidar_grid(p)
    testthat::local_mocked_bindings(regen_cds_credentials_ready = function() TRUE)
    both <- nemetonshiny:::regen_engine_prereqs(p, "era5")
    expect_true(both$ok)
    expect_true(both$microclimf)
    expect_true(both$biljou)
  })
})

test_that("run_regeneration_engine runs both engines and caches their outputs", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd()
    .make_lidar_grid(p)
    seen <- new.env()
    testthat::local_mocked_bindings(regen_cds_credentials_ready = function() TRUE)
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, mnt = NULL, mnh = NULL, las = NULL, ...) {
        seen$mnt <- mnt; seen$mnh <- mnh; seen$las <- las
        units$sensibilite <- 55; units },
      load_biljou_forcing = function(aoi, years, source = NULL, ...) {
        seen$src <- source; data.frame(year = years %||% 2018) },
      build_biljou_soil = function(units = NULL, ewm = 150, ...) {
        seen$ewm <- ewm; list(ewm = ewm) },
      regen_bilan_hydrique = function(units, ...) { units$njstress <- 12; units },
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(.engine_units(3), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran", ewm = 140))

    expect_type(out, "list")
    expect_s3_class(out$units, "sf")
    expect_true(all(c("sensibilite", "njstress") %in% names(out$units)))
    expect_setequal(out$cached, c("sensibilite", "biljou"))
    expect_true(file.exists(file.path(p, "cache", "regeneration", "sensibilite.gpkg")))
    expect_true(file.exists(file.path(p, "cache", "regeneration", "biljou.gpkg")))
    # Entrées bien routées : SAFRAN, ewm UI, répertoires LiDAR + nuage (`las`).
    expect_equal(seen$src, "safran")
    expect_equal(seen$ewm, 140)
    expect_true(grepl("lidar_mnt$", seen$mnt))
    expect_true(grepl("lidar_nuage$", seen$las))   # structure de végétation passée
    expect_equal(out$canopy, "lidar")
    # Sortie consommable en fast-path.
    pc <- nemetonshiny:::load_regeneration_precomputed(p)
    expect_true(!is.null(pc$sensibilite))
    expect_true(!is.null(pc$biljou))
  })
})

test_that("run_regeneration_engine degrades to regen_guard_biljou on BILJOU failure", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd()  # pas de grille → microclimf sauté proprement
    testthat::local_mocked_bindings(regen_cds_credentials_ready = function() TRUE)
    testthat::local_mocked_bindings(
      load_biljou_forcing = function(aoi, years, ...) stop("hors couverture SAFRAN"),
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(forcing = "safran"))

    expect_type(out, "list")
    expect_false("biljou" %in% out$cached)
    expect_false("sensibilite" %in% out$cached)          # pas de grille
    expect_true(any(grepl("BILJOU:", out$warnings)))     # erreur spécifique remontée
    expect_gte(length(out$warnings), 2L)                 # + garde regen_guard_biljou
  })
})

test_that("run_regeneration_engine skips ERA5 BILJOU without CDS credentials", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd()
    called <- new.env(); called$biljou <- FALSE
    testthat::local_mocked_bindings(regen_cds_credentials_ready = function() FALSE)
    testthat::local_mocked_bindings(
      load_biljou_forcing = function(aoi, years, ...) { called$biljou <- TRUE; data.frame() },
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(forcing = "era5"))

    expect_false(called$biljou)                          # BILJOU ERA5 non lancé
    expect_false("biljou" %in% out$cached)
    expect_true(length(out$warnings) >= 1L)
  })
})

# --- Structure de végétation microclimf (brief microclimf-vegetation) --------

test_that("microclimf uses the S2/PROSAIL PAI fallback when no LiDAR point cloud", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd()
    .make_lidar_grid(p, with_nuage = FALSE)   # mnt+mnh mais PAS de nuage
    seen <- new.env()
    fake_pai <- terra::rast(nrows = 2, ncols = 2, vals = 1, crs = "EPSG:2154")
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE,
      .regen_lai_fallback = function(units, out_dir, cfg) fake_pai,   # repli S2 dispo
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, mnt = NULL, mnh = NULL,
                                   las = NULL, pai = NULL, ...) {
        seen$las <- las; seen$pai_ok <- inherits(pai, "SpatRaster")
        units$sensibilite <- 42; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) list(ewm = 150),
      regen_bilan_hydrique = function(units, ...) units,
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    expect_null(seen$las)                     # pas de nuage → pas de las
    expect_true(seen$pai_ok)                  # repli PAI transmis
    expect_equal(out$canopy, "satellite")     # provenance repli S2
    expect_true("sensibilite" %in% out$cached)
  })
})

test_that("microclimf warns (no vegetation structure) when neither las nor pai is available", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd()
    .make_lidar_grid(p, with_nuage = FALSE)
    called <- new.env(); called$sens <- FALSE
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE,
      .regen_lai_fallback = function(units, out_dir, cfg) NULL,   # ni nuage ni S2
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      regen_sensibilite = function(...) { called$sens <- TRUE; stop("ne doit pas être appelé") },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) list(ewm = 150),
      regen_bilan_hydrique = function(units, ...) units,
      .package = "nemeton")

    i18n <- nemetonshiny:::get_i18n("fr")
    out <- nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    expect_false(called$sens)                                     # moteur non lancé
    expect_false("sensibilite" %in% out$cached)
    expect_true(i18n$t("regen_engine_no_vegetation_structure") %in% out$warnings)
    # micro_cache vide retiré (pas de fausse impression de run).
    expect_false(dir.exists(file.path(p, "cache", "regeneration", "microclimf")))
  })
})

test_that("microclimf surfaces a dedicated warning on ERA5 throttling", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd()
    .make_lidar_grid(p, with_nuage = TRUE)    # nuage présent → structure OK
    testthat::local_mocked_bindings(regen_cds_credentials_ready = function() TRUE)
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ...) stop("mcera5: curl_fetch_memory rate limit (429)"),
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) list(ewm = 150),
      regen_bilan_hydrique = function(units, ...) units,
      .package = "nemeton")

    i18n <- nemetonshiny:::get_i18n("fr")
    out <- nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    expect_true(i18n$t("regen_engine_era5_interrupted") %in% out$warnings)
    expect_false(any(grepl("^microclimf:", out$warnings)))        # pas le message générique
    expect_false("sensibilite" %in% out$cached)
  })
})

test_that("run_regeneration_engine validates its input", {
  expect_error(
    nemetonshiny:::run_regeneration_engine(data.frame(x = 1), tempdir()),
    "must be an sf")
})

test_that("run_regeneration_detect_years builds eobs then detects the years", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd(); seen <- new.env()
    testthat::local_mocked_bindings(
      load_eobs_source = function(aoi, var = "tx", cache_dir = NULL, ...) {
        seen$var <- var; seen$cache <- cache_dir; "EOBS_RASTER" },
      microclimate_detect_years = function(eobs = NULL, aoi = NULL, year_window = NULL, ...) {
        seen$eobs <- eobs; list(year_moyenne = 2018, year_canicule = 2022) },
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_detect_years(.engine_units(1), p)

    expect_equal(out$year_moyenne, 2018)
    expect_equal(out$year_canicule, 2022)
    expect_equal(seen$eobs, "EOBS_RASTER")   # eobs construit puis passé à detect_years
    expect_equal(seen$var, "tx")
    expect_true(grepl("eobs$", seen$cache))  # cache disque dédié
  })
})

test_that("run_regeneration_detect_years returns NULL when E-OBS is unavailable", {
  skip_if_not_installed("sf")
  testthat::local_mocked_bindings(load_eobs_source = function(...) NULL, .package = "nemeton")
  expect_null(nemetonshiny:::run_regeneration_detect_years(.engine_units(1), tempdir()))
})

test_that("run_regeneration_detect_years returns NULL on non-sf input", {
  expect_null(nemetonshiny:::run_regeneration_detect_years(data.frame(x = 1), tempdir()))
})

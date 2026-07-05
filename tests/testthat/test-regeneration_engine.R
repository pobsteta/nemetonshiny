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
.make_lidar_grid <- function(project_path) {
  base <- file.path(project_path, "cache", "layers")
  for (d in c("lidar_mnt", "lidar_mnh")) {
    dd <- file.path(base, d)
    dir.create(dd, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(dd, "tile_0000.tif"))
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

    .make_lidar_grid(p)
    g <- nemetonshiny:::resolve_regen_lidar_grid(p)
    expect_type(g, "list")
    expect_true(all(c("mnt_dir", "mnh_dir") %in% names(g)))
    expect_true(dir.exists(g$mnt_dir) && dir.exists(g$mnh_dir))
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
      regen_sensibilite = function(units, mnt = NULL, mnh = NULL, ...) {
        seen$mnt <- mnt; seen$mnh <- mnh; units$sensibilite <- 55; units },
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
    # Entrées bien routées : SAFRAN, ewm UI, répertoires LiDAR.
    expect_equal(seen$src, "safran")
    expect_equal(seen$ewm, 140)
    expect_true(grepl("lidar_mnt$", seen$mnt))
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

test_that("run_regeneration_engine validates its input", {
  expect_error(
    nemetonshiny:::run_regeneration_engine(data.frame(x = 1), tempdir()),
    "must be an sf")
})

# Tests — moteur microclimf réel (option B, spec 027 L1)
#
# Couvre le plumbing app (résolution de la grille LiDAR HD, garde-fous de
# prérequis, run + mise en cache de la sortie). Le vrai run microclimf
# (LiDAR HD + ERA5) n'est PAS testé ici (non exécutable en CI, validé sur
# données réelles côté cœur) — nemeton::regen_sensibilite est mocké.

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
  # Sans clé d'environnement : FALSE si ecmwfr absent ; si présent, dépend du
  # keyring — on neutralise les vars d'env et on tolère les deux issues.
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

test_that("regen_engine_prereqs reports the first missing prerequisite", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd()
    # Pas de grille → raison lidar (le cœur 0.131 exporte regen_sensibilite).
    pre <- nemetonshiny:::regen_engine_prereqs(p)
    expect_false(pre$ok)
    expect_equal(pre$reason, "regen_engine_prereq_lidar")

    .make_lidar_grid(p)
    # Grille présente mais CDS non prêt → raison cds.
    testthat::local_mocked_bindings(regen_cds_credentials_ready = function() FALSE)
    pre2 <- nemetonshiny:::regen_engine_prereqs(p)
    expect_false(pre2$ok)
    expect_equal(pre2$reason, "regen_engine_prereq_cds")
    expect_false(is.null(pre2$grid))

    # Grille + CDS prêts → ok.
    testthat::local_mocked_bindings(regen_cds_credentials_ready = function() TRUE)
    pre3 <- nemetonshiny:::regen_engine_prereqs(p)
    expect_true(pre3$ok)
    expect_null(pre3$reason)
  })
})

test_that("run_regeneration_engine runs the engine and caches sensibilite.gpkg", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd()
    .make_lidar_grid(p)

    seen <- new.env()
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, mnt = NULL, mnh = NULL, ...) {
        seen$mnt <- mnt; seen$mnh <- mnh
        units$sensibilite <- 55
        units$couverture_pct <- 90
        units
      }, .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(
      .engine_units(3), p, cfg = list(year_moyenne = 2018, year_canicule = 2022))

    expect_s3_class(out, "sf")
    expect_true("sensibilite" %in% names(out))
    # Les répertoires (chemins) ont bien été passés au moteur.
    expect_true(grepl("lidar_mnt$", seen$mnt))
    expect_true(grepl("lidar_mnh$", seen$mnh))
    # Sortie mise en cache comme fast-path precomputed.
    cached <- file.path(p, "cache", "regeneration", "sensibilite.gpkg")
    expect_true(file.exists(cached))
    pc <- nemetonshiny:::load_regeneration_precomputed(p)
    expect_true(!is.null(pc$sensibilite))
  })
})

test_that("run_regeneration_engine errors without a LiDAR grid", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    expect_error(
      nemetonshiny:::run_regeneration_engine(.engine_units(1), getwd()),
      "LiDAR HD grid not found")
  })
})

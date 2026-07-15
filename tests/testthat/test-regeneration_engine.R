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
  # Langue figée (le moteur résout i18n via get_app_options()) : robustesse à
  # l'ordre des tests, une suite antérieure pouvant laisser la locale en "en".
  withr::local_options(nemeton.app_options = list(language = "fr"))
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
  withr::local_options(nemeton.app_options = list(language = "fr"))
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

# --- Notifications ntfy du moteur (brief engine-ntfy) ------------------------

test_that("engine pushes ntfy milestones in order when opted in", {
  skip_if_not_installed("sf")
  withr::local_options(nemeton.app_options = list(language = "fr"))
  withr::with_tempdir({
    p <- getwd()
    .make_lidar_grid(p, with_nuage = TRUE)
    rec <- new.env(); rec$msgs <- character(0); seen <- new.env()
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE,
      .ntfy_config = function() list(url = "http://x", topic = "t", token = ""),
      .ntfy_send = function(cfg, message, priority = "default", tags = NULL,
                            title = "Nemeton") {
        rec$msgs <- c(rec$msgs, as.character(message)); invisible(TRUE) },
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ..., progress_callback = NULL) {
        seen$micro_cb <- progress_callback; units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) list(ewm = 150),
      regen_bilan_hydrique = function(units, ..., progress_callback = NULL) {
        seen$biljou_cb <- progress_callback; units$njstress <- 1; units },
      .package = "nemeton")

    i18n <- nemetonshiny:::get_i18n("fr")
    out <- nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    # Séquence attendue des jalons (le résumé porte la liste `cached`).
    expect_equal(rec$msgs, c(
      i18n$t("regen_ntfy_start"),
      i18n$t("regen_ntfy_micro_start"),
      i18n$t("regen_ntfy_micro_done"),
      i18n$t("regen_ntfy_biljou_start"),
      i18n$t("regen_ntfy_biljou_done"),
      sprintf(i18n$t("regen_ntfy_done"), "sensibilite, biljou")))
    # Callback fin transmis aux deux moteurs (opt-in on).
    expect_true(is.function(seen$micro_cb))
    expect_true(is.function(seen$biljou_cb))
    expect_setequal(out$cached, c("sensibilite", "biljou"))
  })
})

test_that("engine still passes a progress_callback with ntfy off (in-app phase channel)", {
  # Depuis le brief engine-phase-status : on_prog est TOUJOURS défini, même sans
  # ntfy, car il alimente engine_status.json (canal de la notif in-app). Sans
  # ntfy il n'émet aucun push réseau, mais écrit bien le fichier de phase.
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd()
    .make_lidar_grid(p, with_nuage = TRUE)
    seen <- new.env()
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE,
      .ntfy_config = function() NULL,                 # NEMETON_NTFY_TOPIC absent
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ..., progress_callback = NULL) {
        seen$micro_cb <- progress_callback
        # Simule un emit cœur → le callback doit écrire la phase sans jamais throw.
        if (is.function(progress_callback))
          progress_callback(list(current = "regen_expo:complete"))
        units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) list(ewm = 150),
      regen_bilan_hydrique = function(units, ..., progress_callback = NULL) {
        seen$biljou_cb <- progress_callback; units$njstress <- 1; units },
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    # Callback désormais toujours fourni au cœur (canal de phase in-app).
    expect_true(is.function(seen$micro_cb))
    expect_true(is.function(seen$biljou_cb))
    # Fin de worker : le fichier d'état existe et porte la phase terminale `done`.
    st <- jsonlite::fromJSON(file.path(p, "cache", "regeneration", "engine_status.json"))
    expect_equal(st$phase, "done")
    expect_setequal(out$cached, c("sensibilite", "biljou"))
  })
})

test_that("engine writes microclimf_skipped with a reason when microclimf is skipped", {
  # Cas RECONFORT : pas de clé CDS → bloc microclimf entièrement sauté, mais
  # BILJOU (SAFRAN) réussit. Le fichier d'état doit porter microclimf_skipped
  # (raison CDS) puis la phase terminale done — jamais bloqué sur `grille`.
  skip_if_not_installed("sf")
  # Langue figée : le moteur résout i18n via get_app_options()$language ; on la
  # force en local pour ne pas dépendre d'une pollution d'ordre de tests (une
  # suite antérieure peut laisser nemeton.app_options en "en").
  withr::local_options(nemeton.app_options = list(language = "fr"))
  withr::with_tempdir({
    p <- getwd()
    .make_lidar_grid(p, with_nuage = TRUE)
    seen <- new.env(); seen$phases <- character(0)
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() FALSE,   # pas de clé CDS
      .ntfy_config = function() NULL,
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) list(ewm = 150),
      regen_bilan_hydrique = function(units, ..., progress_callback = NULL) {
        # Capture la phase d'état juste après le saut microclimf, avant `done`.
        st <- jsonlite::fromJSON(file.path(p, "cache", "regeneration", "engine_status.json"))
        seen$mid <- st
        units$njstress <- 1; units },
      .package = "nemeton")

    nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(forcing = "safran"))

    expect_equal(seen$mid$phase, "microclimf_skipped")
    expect_equal(seen$mid$reason,
                 get_i18n("fr")$t("regen_phase_skip_reason_cds"))
  })
})

test_that("engine passes persistent pai_cache on the LiDAR branch", {
  # brief pai-cache : sur la branche LiDAR (grille + nuage), regen_sensibilite
  # reçoit pai_cache = <project>/cache/regeneration/pai.tif (relecture/écriture
  # du PAI par le cœur → phase éclair au 2e run). Jamais posé côté repli satellite.
  skip_if_not_installed("sf")
  withr::local_options(nemeton.app_options = list(language = "fr"))
  withr::with_tempdir({
    p <- getwd()
    .make_lidar_grid(p, with_nuage = TRUE)   # nuage présent → branche LiDAR
    seen <- new.env()
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE,
      .ntfy_config = function() NULL,
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ..., pai_cache = NULL) {
        seen$pai_cache <- pai_cache
        units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) list(ewm = 150),
      regen_bilan_hydrique = function(units, ...) units,
      .package = "nemeton")

    nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    expect_equal(seen$pai_cache,
                 file.path(p, "cache", "regeneration", "pai.tif"))
  })
})

test_that("engine ntfy on_prog maps core events to messages", {
  skip_if_not_installed("sf")
  withr::local_options(nemeton.app_options = list(language = "fr"))
  withr::with_tempdir({
    p <- getwd()
    .make_lidar_grid(p, with_nuage = TRUE)
    rec <- new.env(); rec$msgs <- character(0)
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE,
      .ntfy_config = function() list(url = "http://x", topic = "t", token = ""),
      .ntfy_send = function(cfg, message, ...) {
        rec$msgs <- c(rec$msgs, as.character(message)); invisible(TRUE) },
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      # Le moteur émet des événements fins via le callback reçu.
      regen_sensibilite = function(units, ..., progress_callback = NULL) {
        if (is.function(progress_callback)) {
          progress_callback(list(current = "regen_expo:microclimf", category = "moyen"))
          progress_callback(list(current = "regen_expo:era5", year = 2018,
                                 category = "moyen", i = 3, n = 12))
        }
        units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) list(ewm = 150),
      regen_bilan_hydrique = function(units, ..., progress_callback = NULL) {
        if (is.function(progress_callback)) {
          progress_callback(list(current = "regen_biljou:start", n = 30))
        }
        units$njstress <- 1; units },
      .package = "nemeton")

    i18n <- nemetonshiny:::get_i18n("fr")
    nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    expect_true(sprintf(i18n$t("regen_ntfy_micro_cat"), "moyen") %in% rec$msgs)
    expect_true(sprintf(i18n$t("regen_ntfy_era5"), 2018L, "moyen", 3L, 12L) %in% rec$msgs)
    expect_true(sprintf(i18n$t("regen_ntfy_biljou_pts"), 30L) %in% rec$msgs)
  })
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

# --- spec 035 B1 : bilan hydrique spatialisé --------------------------------
# Le PAI structural (cache/regeneration/pai.tif) doit alimenter le `lai_max` de
# BILJOU, et le sol doit venir de SoilGrids par UGF quand l'utilisateur n'a pas
# forcé d'`ewm`. Avant spec 035, le PAI n'était consommé que par microclimf et
# BILJOU tournait sur un sol uniforme + un lai_max scalaire → njstress constant.

.stub_pai_cache <- function(project_path) {
  d <- file.path(project_path, "cache", "regeneration")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(d, "pai.tif"))
  invisible(file.path(d, "pai.tif"))
}

test_that("BILJOU consumes the cached LiDAR PAI as per-unit lai_max", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd(); .make_lidar_grid(p); .stub_pai_cache(p)
    seen <- new.env()
    testthat::local_mocked_bindings(regen_cds_credentials_ready = function() TRUE)
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ...) { units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) list(ewm = 150),
      lai_max_depuis_pai = function(units, pai, ...) {
        seen$pai <- pai; seq_len(nrow(units)) + 3 },
      regen_bilan_hydrique = function(units, ..., lai_max = NULL) {
        seen$lai_max <- lai_max; units$njstress <- 12; units },
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(.engine_units(3), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    # Le cœur reçoit un vecteur d'une valeur par UGF (il le convertit en liste
    # nommée par id) — jamais un scalaire.
    expect_equal(seen$lai_max, c(4, 5, 6))
    expect_equal(seen$pai, file.path(p, "cache", "regeneration", "pai.tif"))
    expect_equal(out$lai_source, "pai_lidar")
    expect_equal(out$canopy, "lidar")
    expect_equal(attr(out$units, "lai_source"), "pai_lidar")
  })
})

test_that("an explicit lai_max overrides the cached PAI", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd(); .make_lidar_grid(p); .stub_pai_cache(p)
    seen <- new.env()
    testthat::local_mocked_bindings(regen_cds_credentials_ready = function() TRUE)
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ...) { units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) list(ewm = 150),
      lai_max_depuis_pai = function(units, pai, ...) stop("must not be called"),
      regen_bilan_hydrique = function(units, ..., lai_max = NULL) {
        seen$lai_max <- lai_max; units },
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran",
                 lai_max = 6.2))

    expect_equal(seen$lai_max, 6.2)
    expect_true(is.na(out$lai_source))
  })
})

test_that("soil comes from SoilGrids unless ewm is forced (backward compat)", {
  skip_if_not_installed("sf")
  seen <- new.env()
  mock_soil <- function(units = NULL, ewm = 150, source = "uniform",
                        rooting_depth_cm = 100, progress_callback = NULL, ...) {
    seen$source <- source; seen$ewm <- ewm; seen$depth <- rooting_depth_cm
    seen$cb <- !is.null(progress_callback)
    list(ewm = ewm)
  }
  run <- function(cfg) {
    withr::with_tempdir({
      p <- getwd(); .make_lidar_grid(p)
      testthat::local_mocked_bindings(regen_cds_credentials_ready = function() TRUE)
      testthat::local_mocked_bindings(
        regen_sensibilite = function(units, ...) { units$sensibilite <- 1; units },
        load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
        build_biljou_soil = mock_soil,
        regen_bilan_hydrique = function(units, ...) units,
        .package = "nemeton")
      nemetonshiny:::run_regeneration_engine(.engine_units(2), p, cfg = cfg)
    })
  }

  # ewm absent → SoilGrids par UGF, profondeur transmise, callback de phase câblé.
  run(list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran",
           rooting_depth_cm = 80))
  expect_equal(seen$source, "soilgrids")
  expect_equal(seen$depth, 80)
  expect_true(seen$cb)

  # ewm saisi → sol uniforme, comportement v0.146.x à l'identique.
  run(list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran", ewm = 150))
  expect_equal(seen$source, "uniform")
  expect_equal(seen$ewm, 150)
})

test_that("engine maps SoilGrids progress events to the ewm phase", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd(); .make_lidar_grid(p)
    status <- file.path(p, "cache", "regeneration", "engine_status.json")
    seen <- new.env()
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE,
      .ntfy_config = function() NULL,
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ...) { units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, progress_callback = NULL, ...) {
        progress_callback(list(current = "ewm:layer", i = 2, n = 6))
        # Capturer l'état AU VOL (les phases `biljou` puis `done` l'écrasent),
        # mais NE RIEN asserter ici : le moteur enveloppe cet appel dans un
        # tryCatch(error=) qui avalerait l'échec d'expectation — test vacant.
        seen$st <- jsonlite::fromJSON(status)
        list(ewm = 150)
      },
      regen_bilan_hydrique = function(units, ...) units,
      .package = "nemeton")

    nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    expect_equal(seen$st$phase, "ewm")
    expect_equal(seen$st$i, 2)
    expect_equal(seen$st$n, 6)
  })
})

test_that(".regen_lai_per_unit delegates the plateau to the core (not a mean)", {
  skip_if_not_installed("sf")
  testthat::local_mocked_bindings(
    lai_max_depuis_pai = function(units, pai, ...) rep(7, nrow(units)),
    .package = "nemeton")
  expect_equal(nemetonshiny:::.regen_lai_per_unit(.engine_units(2), "raster"), c(7, 7))
})

# --- spec 035 B3 : observabilité du moteur -----------------------------------
# Le moteur tourne dans un worker `future` : ses cli_warn/message n'atteignent ni
# la console principale ni l'UI. Seul le POST ntfy en sortait. `engine.log`
# (JSONL, en ajout) traverse la frontière de processus par le disque et survit à
# la mort du worker (OOM) — seul post-mortem disponible.

test_that(".regen_log appends JSONL entries and strips ANSI", {
  withr::with_tempdir({
    d <- getwd()
    nemetonshiny:::.regen_log(d, "info", "engine", "demarrage")
    nemetonshiny:::.regen_log(d, "error", "biljou", "\033[31mSAFRAN KO\033[0m")

    lines <- readLines(file.path(d, "engine.log"))
    expect_length(lines, 2L)                      # AJOUT, pas écrasement
    e2 <- jsonlite::fromJSON(lines[2])
    expect_equal(e2$level, "error")
    expect_equal(e2$source, "biljou")
    expect_equal(e2$message, "SAFRAN KO")         # séquences ANSI retirées
    expect_true(is.numeric(e2$ts))
  })
})

test_that(".regen_log never throws on an unwritable directory", {
  expect_silent(nemetonshiny:::.regen_log("/nonexistent/dir", "error", "x", "y"))
})

test_that(".regen_log_reset truncates the log", {
  withr::with_tempdir({
    d <- getwd()
    nemetonshiny:::.regen_log(d, "info", "engine", "a")
    nemetonshiny:::.regen_log_reset(d)
    expect_false(file.exists(file.path(d, "engine.log")))
  })
})

test_that("engine truncates the log at start and records BILJOU errors", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd(); .make_lidar_grid(p)
    out_dir <- file.path(p, "cache", "regeneration")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    # Entrée d'un run précédent : elle doit disparaître.
    nemetonshiny:::.regen_log(out_dir, "info", "engine", "RUN_PRECEDENT")

    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE,
      .ntfy_config = function() NULL,
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ...) { units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) list(ewm = 150),
      regen_bilan_hydrique = function(units, ...) stop("SAFRAN indisponible"),
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    log <- nemetonshiny:::.regen_read_log(p)
    expect_false(any(grepl("RUN_PRECEDENT", log$message)))   # tronqué au départ
    expect_true(any(log$level == "error" & log$source == "biljou"))
    expect_true(any(grepl("SAFRAN indisponible", log$message)))
    # Le journal double le canal : `warnings` reste alimenté comme avant.
    expect_true(any(grepl("BILJOU", out$warnings)))
    # Le journal SURVIT à la fin du run (post-mortem).
    expect_true(file.exists(file.path(out_dir, "engine.log")))
  })
})

test_that("engine logs a warning when ntfy is configured but unreachable", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd(); .make_lidar_grid(p)
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE,
      .ntfy_config = function() list(url = "http://x", topic = "t", token = ""),
      .ntfy_send = function(cfg, message, ...) invisible(FALSE),   # canal tombé
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ...) { units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) list(ewm = 150),
      regen_bilan_hydrique = function(units, ...) units,
      .package = "nemeton")

    nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    log <- nemetonshiny:::.regen_read_log(p)
    expect_true(any(log$source == "ntfy" & log$level == "warning"))
  })
})

test_that("engine logs nothing actionable on a clean run", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd(); .make_lidar_grid(p)
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE,
      .ntfy_config = function() NULL,
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ...) { units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) list(ewm = 150),
      regen_bilan_hydrique = function(units, ...) { units$njstress <- 3; units },
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    log <- nemetonshiny:::.regen_read_log(p)
    expect_true(nrow(log) > 0L)                                  # jalons `info`
    expect_equal(nemetonshiny:::.regen_log_issues(log), character(0))
    expect_length(out$warnings, 0L)
  })
})

test_that(".regen_read_log degrades to an empty frame, never throws", {
  expect_equal(nrow(nemetonshiny:::.regen_read_log(NULL)), 0L)
  expect_equal(nrow(nemetonshiny:::.regen_read_log(tempfile())), 0L)
  withr::with_tempdir({
    p <- getwd()
    dir.create(file.path(p, "cache", "regeneration"), recursive = TRUE)
    writeLines(c("{pas du json", "", '{"ts":1,"level":"error","source":"s","message":"m"}'),
               file.path(p, "cache", "regeneration", "engine.log"))
    log <- nemetonshiny:::.regen_read_log(p)
    expect_equal(nrow(log), 1L)          # ligne corrompue ignorée, pas d'abandon
    expect_equal(log$message, "m")
  })
})

test_that(".regen_relay_log relays errors/warnings, stays silent on info", {
  log <- data.frame(ts = 1L, level = "info", source = "engine", message = "jalon",
                    stringsAsFactors = FALSE)
  expect_silent(nemetonshiny:::.regen_relay_log(log))
  expect_silent(nemetonshiny:::.regen_relay_log(data.frame()))

  bad <- data.frame(ts = c(1L, 2L), level = c("warning", "error"),
                    source = c("ntfy", "biljou"), message = c("w", "e"),
                    stringsAsFactors = FALSE)
  expect_message(nemetonshiny:::.regen_relay_log(bad), "biljou")
})

test_that(".regen_cleanup_status removes the status file but spares engine.log", {
  skip_if_not_installed("shiny")
  withr::with_tempdir({
    p <- getwd()
    d <- file.path(p, "cache", "regeneration")
    dir.create(d, recursive = TRUE)
    nemetonshiny:::.regen_write_phase(d, "biljou")
    nemetonshiny:::.regen_log(d, "error", "biljou", "post-mortem")

    as <- shiny::reactiveValues(current_project = list(path = p))
    shiny::isolate(nemetonshiny:::.regen_cleanup_status(as))

    expect_false(file.exists(file.path(d, "engine_status.json")))
    expect_true(file.exists(file.path(d, "engine.log")))   # post-mortem préservé
  })
})

# --- spec 035 B4 : lisibilité des overrides ----------------------------------
# Le moteur doit remonter ce qu'il a RÉELLEMENT utilisé (lai_max par UGF, réserve
# utile par UGF) : sans ça rien ne distingue à l'écran une valeur spatialisée d'un
# scalaire, et le repli SoilGrids → sol uniforme reste invisible.

.fake_soil <- function(ewm) structure(list(ewm = ewm), class = "biljou_soil")

test_that("engine returns the per-unit lai_max and ewm it actually used", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd(); .make_lidar_grid(p); .stub_pai_cache(p)
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE, .ntfy_config = function() NULL,
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ...) { units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      lai_max_depuis_pai = function(units, pai, ...) c(2.8, 4.1, 6.3),
      # SoilGrids joignable → liste nommée par id d'UGF.
      build_biljou_soil = function(units = NULL, source = "uniform", ...) {
        stats::setNames(lapply(c(90, 137, 210), .fake_soil), as.character(1:3))
      },
      regen_bilan_hydrique = function(units, ...) { units$njstress <- 1; units },
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(.engine_units(3), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    expect_equal(out$lai_max, c(2.8, 4.1, 6.3))
    expect_equal(unname(out$ewm), c(90, 137, 210))
    expect_equal(out$ewm_source, "soilgrids")
    expect_length(out$warnings, 0L)
  })
})

test_that("a silent SoilGrids fallback becomes a warning and a log entry", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd(); .make_lidar_grid(p)
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE, .ntfy_config = function() NULL,
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ...) { units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      # files.isric.org injoignable : le cœur retombe sur UN objet biljou_soil.
      build_biljou_soil = function(units = NULL, ...) .fake_soil(150),
      regen_bilan_hydrique = function(units, ...) units,
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(.engine_units(3), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    expect_equal(out$ewm_source, "soilgrids_fallback")
    expect_true(any(grepl("SoilGrids", out$warnings)))
    log <- nemetonshiny:::.regen_read_log(p)
    expect_true(any(log$source == "soilgrids" & log$level == "warning"))
  })
})

test_that("a forced ewm is reported as uniform, with no fallback warning", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd(); .make_lidar_grid(p)
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE, .ntfy_config = function() NULL,
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ...) { units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ewm = 150, ...) .fake_soil(ewm),
      regen_bilan_hydrique = function(units, ...) units,
      .package = "nemeton")

    out <- nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran",
                 ewm = 150))

    expect_equal(out$ewm_source, "uniform")   # choix explicite, pas un repli
    expect_false(any(grepl("SoilGrids", out$warnings)))
  })
})

test_that("engine does not leak assignments outside its frame (tryCatch scope)", {
  # `<<-` dans le BLOC d'un tryCatch() (évalué dans le frame appelant) écrirait
  # dans l'environnement du package, pas dans la fonction. Garde-fou de régression.
  skip_if_not_installed("sf")
  withr::with_tempdir({
    p <- getwd(); .make_lidar_grid(p)
    testthat::local_mocked_bindings(
      regen_cds_credentials_ready = function() TRUE, .ntfy_config = function() NULL,
      .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      regen_sensibilite = function(units, ...) { units$sensibilite <- 1; units },
      load_biljou_forcing = function(aoi, years, ...) data.frame(year = 2018),
      build_biljou_soil = function(units = NULL, ...) .fake_soil(150),
      regen_bilan_hydrique = function(units, ...) units,
      .package = "nemeton")

    nemetonshiny:::run_regeneration_engine(.engine_units(2), p,
      cfg = list(year_moyenne = 2018, year_canicule = 2022, forcing = "safran"))

    ns <- asNamespace("nemetonshiny")
    expect_false(exists("ewm_values", envir = ns, inherits = FALSE))
    expect_false(exists("ewm_source", envir = ns, inherits = FALSE))
  })
})

test_that(".regen_derived_stats summarises a per-unit vector, NULL on a scalar", {
  st <- nemetonshiny:::.regen_derived_stats(c(2.8, 4.0, 4.2, 6.3))
  expect_equal(st$median, "4.1")
  expect_equal(st$min, "2.8")
  expect_equal(st$max, "6.3")
  expect_equal(st$n, 4L)

  expect_equal(nemetonshiny:::.regen_derived_stats(c(90, 137, 210), digits = 0)$median, "137")

  expect_null(nemetonshiny:::.regen_derived_stats(5))          # scalaire : rien à montrer
  expect_null(nemetonshiny:::.regen_derived_stats(NULL))
  expect_null(nemetonshiny:::.regen_derived_stats(c(NA, NA)))
  expect_null(nemetonshiny:::.regen_derived_stats(c(3, NA)))    # une seule valeur finie
})

test_that(".regen_soil_ewm reads the soil shape without ever throwing", {
  # Liste de biljou_soil de longueur n → réserve utile par UGF (SoilGrids).
  sg <- nemetonshiny:::.regen_soil_ewm(
    stats::setNames(lapply(c(90, 137), .fake_soil), c("1", "2")), n = 2)
  expect_equal(unname(sg$values), c(90, 137))
  expect_equal(sg$source, "soilgrids")

  # Objet unique sans override → repli silencieux de SoilGrids.
  fb <- nemetonshiny:::.regen_soil_ewm(.fake_soil(150), n = 3, forced = FALSE)
  expect_equal(fb$source, "soilgrids_fallback")
  expect_equal(fb$values, 150)

  # Objet unique avec override → choix explicite de l'utilisateur.
  expect_equal(nemetonshiny:::.regen_soil_ewm(.fake_soil(150), n = 3, forced = TRUE)$source,
               "uniform")

  # Formes inattendues : `source` NA, jamais d'erreur. Une lecture purement
  # informative ne doit pas faire tomber le bilan hydrique (régression réelle :
  # `list(ewm = 150)` faisait lever `$` sur un atomique).
  for (weird in list(list(ewm = 150), NULL, 42, list(), list("a", "b"))) {
    got <- expect_no_error(nemetonshiny:::.regen_soil_ewm(weird, n = 2))
    expect_true(is.na(got$source))
    expect_null(got$values)
  }

  # Longueur qui ne correspond pas au nombre d'UGF → refus (pas d'indexation fausse).
  mismatched <- lapply(c(90, 137), .fake_soil)
  expect_true(is.na(nemetonshiny:::.regen_soil_ewm(mismatched, n = 5)$source))
})

# --- Plafond mémoire du moteur reGénération (brief 035 regen-capped) ----------
# `.regen_run_engine_capped()` route vers le chemin capé (nemeton::run_memory_capped
# généralisé, package=/options=) quand le cœur l'expose, sinon repli sur l'appel
# direct dans le worker — sans casser sur un cœur antérieur.

test_that(".regen_run_engine_capped uses the capped path when the core supports it", {
  seen <- new.env(); seen$capped <- NULL; seen$direct <- 0L
  testthat::local_mocked_bindings(
    run_regeneration_engine = function(units, project_path, cfg) {
      seen$direct <- seen$direct + 1L; list(canopy = "lidar", direct = TRUE)
    },
    .package = "nemetonshiny")
  # Stub « cœur généralisé » : formals incluent package/options.
  testthat::local_mocked_bindings(
    run_memory_capped = function(fun, args = list(), package = "nemeton",
                                 options = NULL, quiet = FALSE, ...) {
      seen$capped <- list(fun = fun, package = package, args = args, options = options)
      list(canopy = "lidar", capped = TRUE)
    },
    .package = "nemeton")

  out <- nemetonshiny:::.regen_run_engine_capped(
    units = "U", project_path = "/tmp/p", cfg = list(a = 1), app_opts = list(language = "fr"))

  expect_true(isTRUE(out$capped))                       # chemin capé emprunté
  expect_equal(seen$direct, 0L)                         # pas d'appel direct
  expect_equal(seen$capped$fun, "run_regeneration_engine")
  expect_equal(seen$capped$package, "nemetonshiny")
  expect_equal(seen$capped$args$project_path, "/tmp/p")
  expect_equal(seen$capped$options$nemeton.app_options$language, "fr")
})

test_that(".regen_run_engine_capped falls back to the direct path (flag off)", {
  seen <- new.env(); seen$direct <- 0L; seen$capped <- 0L
  testthat::local_mocked_bindings(
    run_regeneration_engine = function(units, project_path, cfg) {
      seen$direct <- seen$direct + 1L; list(direct = TRUE)
    },
    .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    run_memory_capped = function(fun, args = list(), package = "nemeton",
                                 options = NULL, quiet = FALSE, ...) {
      seen$capped <- seen$capped + 1L; list(capped = TRUE)
    },
    .package = "nemeton")

  withr::local_options(nemetonshiny.regen_capped = FALSE)
  out <- nemetonshiny:::.regen_run_engine_capped("U", "/tmp/p", list(), list())
  expect_true(isTRUE(out$direct))
  expect_equal(seen$capped, 0L)                         # jamais le chemin capé
  expect_equal(seen$direct, 1L)
})

test_that(".regen_run_engine_capped falls back when the core lacks package=/options=", {
  seen <- new.env(); seen$direct <- 0L; seen$capped <- 0L
  testthat::local_mocked_bindings(
    run_regeneration_engine = function(units, project_path, cfg) {
      seen$direct <- seen$direct + 1L; list(direct = TRUE)
    },
    .package = "nemetonshiny")
  # Stub « ancien cœur » : SIGNATURE sans package/options → capacité absente.
  testthat::local_mocked_bindings(
    run_memory_capped = function(fun, args = list(), quiet = FALSE, ...) {
      seen$capped <- seen$capped + 1L; list(capped = TRUE)
    },
    .package = "nemeton")

  out <- nemetonshiny:::.regen_run_engine_capped("U", "/tmp/p", list(), list())
  expect_true(isTRUE(out$direct))                       # repli sur l'appel direct
  expect_equal(seen$capped, 0L)
  expect_equal(seen$direct, 1L)
})

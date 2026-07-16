# Tests — reGénération orchestration service (spec 027, L4)
#
# Couvre run_regeneration : séquence des appels cœur, attache des colonnes,
# mode "bilan hydrique seul", dégradation propre sur erreur moteur, et
# transmission des sorties precomputed. Aucune logique métier testée ici
# (elle vit dans nemeton) — uniquement l'orchestration app, cœur mocké.

`%||%` <- function(a, b) if (is.null(a)) b else a

.regen_units <- function(n = 2) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- i * 10
    sf::st_polygon(list(matrix(c(
      x0, 0, x0 + 5, 0, x0 + 5, 5, x0, 5, x0, 0), ncol = 2, byrow = TRUE)))
  })
  sf::st_sf(ug_id = seq_len(n), geometry = sf::st_sfc(polys, crs = 2154))
}

# Mock qui ajoute une colonne 0-100 à `units` (simule un indicateur cœur).
.add_col <- function(name, value = 50) {
  function(units, ...) { units[[name]] <- rep(value, nrow(units)); units }
}

test_that("run_regeneration sequences core calls and attaches columns", {
  skip_if_not_installed("sf")

  calls <- character(0)
  testthat::local_mocked_bindings(
    microclimate_detect_years = function(eobs = NULL, aoi = NULL, year_window = NULL, ...) {
      calls <<- c(calls, "years"); list(year_moyenne = 2018, year_canicule = 2022)
    },
    regen_sensibilite = function(units, ...) {
      calls <<- c(calls, "sensibilite")
      units$sensibilite <- 60; units$d_tmax <- 2.1; units$couverture_pct <- 90; units
    },
    regen_bilan_hydrique = function(units, ...) {
      calls <<- c(calls, "biljou")
      units$njstress <- 12; units$rew_min <- 0.3; units
    },
    indicateur_a3_microclimat  = function(units, ...) { calls <<- c(calls,"a3"); .add_col("indicateur_a3_microclimat")(units) },
    indicateur_a4_tamponnement = function(units, ...) { calls <<- c(calls,"a4"); .add_col("indicateur_a4_tamponnement")(units) },
    indicateur_w4_vpd          = function(units, ...) { calls <<- c(calls,"w4"); .add_col("indicateur_w4_vpd")(units) },
    indicateur_r6_sensibilite  = function(units, ...) { calls <<- c(calls,"r6"); .add_col("indicateur_r6_sensibilite")(units) },
    indicateur_r3_secheresse   = function(units, ...) { calls <<- c(calls,"r3"); .add_col("indicateur_r3_secheresse")(units) },
    indice_priorite_regen      = function(units, ...) {
      calls <<- c(calls, "priorite")
      units$indice_priorite_regen <- 70; units$priorite <- "haute"; units
    },
    .package = "nemeton"
  )

  units <- .regen_units(3)
  # `eobs_tx` = la série estivale cachée : sans elle, detect_years est (à raison)
  # sauté — cf. le test dédié plus bas.
  out <- nemetonshiny:::run_regeneration(
    units, cfg = list(forest_type = "feuillu"),
    precomputed = list(sensibilite = "S", biljou = "B", eobs_tx = "TX",
                       micro = "MICRO", micro_moyenne = "M", micro_canicule = "C"))

  # Séquence complète appelée dans l'ordre attendu
  expect_equal(calls, c("years", "sensibilite", "biljou", "a3", "a4", "w4", "r6", "r3", "priorite"))
  # Colonnes attachées
  expect_s3_class(out$units, "sf")
  expect_true(all(c("sensibilite", "njstress", "indice_priorite_regen",
                    "indicateur_a3_microclimat", "indicateur_r6_sensibilite",
                    "indicateur_r3_secheresse") %in% names(out$units)))
  expect_equal(unique(out$units$indice_priorite_regen), 70)
  expect_equal(out$years$year_moyenne, 2018)
  expect_length(out$warnings, 0)
})

test_that("hydric_only skips microclimf sensibilite and micro sub-indicators", {
  skip_if_not_installed("sf")

  calls <- character(0)
  testthat::local_mocked_bindings(
    microclimate_detect_years = function(...) { calls <<- c(calls,"years"); list(year_moyenne=2018, year_canicule=2022) },
    regen_sensibilite = function(units, ...) { calls <<- c(calls,"sensibilite"); units },
    regen_bilan_hydrique = function(units, ...) { calls <<- c(calls,"biljou"); units$njstress <- 5; units },
    indicateur_a3_microclimat = function(units, ...) { calls <<- c(calls,"a3"); units },
    indicateur_r3_secheresse = function(units, ...) { calls <<- c(calls,"r3"); units },
    indice_priorite_regen = function(units, ...) { calls <<- c(calls,"priorite"); units$indice_priorite_regen <- 40; units },
    .package = "nemeton"
  )

  out <- nemetonshiny:::run_regeneration(
    .regen_units(2), cfg = list(hydric_only = TRUE),
    precomputed = list(biljou = "B", micro = "MICRO", eobs_tx = "TX"))

  expect_false("sensibilite" %in% calls)  # microclimf sauté
  expect_false("a3" %in% calls)           # sous-indicateurs micro sautés
  expect_true(all(c("years", "biljou", "r3", "priorite") %in% calls))
  expect_true("njstress" %in% names(out$units))
})

test_that("run_regeneration degrades gracefully when an engine errors", {
  skip_if_not_installed("sf")

  testthat::local_mocked_bindings(
    microclimate_detect_years = function(...) list(year_moyenne = 2018, year_canicule = 2022),
    regen_sensibilite = function(units, ...) stop("microclimf non installé et aucun precomputed"),
    regen_bilan_hydrique = function(units, ...) { units$njstress <- 8; units },
    indicateur_r3_secheresse = function(units, ...) units,
    indice_priorite_regen = function(units, ...) { units$indice_priorite_regen <- 30; units },
    .package = "nemeton"
  )

  # precomputed fourni → les moteurs SONT appelés ; le mock sensibilite lève.
  out <- nemetonshiny:::run_regeneration(
    .regen_units(2), cfg = list(),
    precomputed = list(sensibilite = "S", biljou = "B"))

  # Pas de crash : l'erreur moteur est capturée en warning actionnable
  expect_true(any(grepl("microclimf", out$warnings)))
  expect_false("sensibilite" %in% names(out$units))  # colonne absente (dégradation)
  expect_true("njstress" %in% names(out$units))       # les autres étapes tournent
  expect_equal(unique(out$units$indice_priorite_regen), 30)
})

test_that("run_regeneration guards engines when no precomputed is supplied (option A)", {
  skip_if_not_installed("sf")

  called <- character(0)
  testthat::local_mocked_bindings(
    microclimate_detect_years = function(...) list(year_moyenne = 2018, year_canicule = 2022),
    regen_sensibilite    = function(units, ...) { called <<- c(called, "sensibilite"); units },
    regen_bilan_hydrique = function(units, ...) { called <<- c(called, "biljou"); units },
    indicateur_r3_secheresse = function(units, ...) units,
    indice_priorite_regen = function(units, ...) { units$indice_priorite_regen <- 20; units },
    .package = "nemeton"
  )

  # cfg avec années fournies → detect_years non appelé ; aucun precomputed.
  out <- nemetonshiny:::run_regeneration(
    .regen_units(2), cfg = list(year_moyenne = 2018, year_canicule = 2022))

  # Moteurs NON appelés (garde), messages i18n propres au lieu d'erreurs brutes.
  expect_false("sensibilite" %in% called)
  expect_false("biljou" %in% called)
  expect_length(out$warnings, 2L)
  expect_false(any(grepl("\033|Error|stop", out$warnings)))  # messages propres
  # Le pipeline se termine quand même (indice de priorité calculé).
  expect_equal(unique(out$units$indice_priorite_regen), 20)
})

test_that("run_regeneration validates its input", {
  expect_error(nemetonshiny:::run_regeneration(data.frame(x = 1)), "must be an sf")
})

test_that("run_regeneration_detect_years passes an explicit E-OBS years window", {
  skip_if_not_installed("sf")
  # Régression : sans `years`, le cœur (.eobs_cds_fetch) sort en NULL avant tout
  # téléchargement → bouton « Auto (E-OBS) » affichait « indisponible ».
  seen <- new.env()
  testthat::local_mocked_bindings(
    load_eobs_source = function(aoi = NULL, var = NULL, years = NULL,
                                cache_dir = NULL, ...) {
      seen$years <- years; "fake-eobs"
    },
    microclimate_detect_years = function(eobs = NULL, aoi = NULL,
                                         year_window = NULL, ...) {
      list(year_moyenne = 2018, year_canicule = 2022)
    },
    .package = "nemeton")

  out <- nemetonshiny:::run_regeneration_detect_years(
    .regen_units(), project_path = NULL, year_window = 10)

  expect_equal(out, list(year_moyenne = 2018, year_canicule = 2022))
  expect_true(is.numeric(seen$years) && length(seen$years) >= 7L)  # fenêtre non vide
  expect_true(min(seen$years) >= 2011L)                            # un seul bloc CDS
  expect_equal(max(seen$years),
               as.integer(format(Sys.Date(), "%Y")) - 2L)         # latence E-OBS
})

test_that("run_regeneration_detect_years returns NULL when E-OBS is unavailable", {
  skip_if_not_installed("sf")
  testthat::local_mocked_bindings(
    load_eobs_source = function(...) NULL,   # pas de clé / hors couverture / CI
    .package = "nemeton")
  expect_null(nemetonshiny:::run_regeneration_detect_years(
    .regen_units(), project_path = NULL))
})

test_that("regeneration_species_choices delegates to core and auto-detects TFV", {
  skip_if_not_installed("sf")
  seen <- new.env()
  testthat::local_mocked_bindings(
    regen_species_choices = function(units = NULL, tfv_col = NULL, level = NULL, lang = NULL, ...) {
      seen$tfv_col <- tfv_col; seen$lang <- lang
      data.frame(code = c("a", "b"), label = c("A", "B"),
                 present = c(TRUE, FALSE), groupe = c("present", "adaptation"))
    }, .package = "nemeton")

  u <- sf::st_sf(tfv_code = c("FF1", "FF2"),
                 geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 2154))
  out <- nemetonshiny:::regeneration_species_choices(units = u, lang = "en")

  expect_s3_class(out, "data.frame")
  expect_true(all(c("present", "groupe") %in% names(out)))
  expect_equal(seen$tfv_col, "tfv_code")   # colonne TFV BD Forêt v2 auto-détectée
  expect_equal(seen$lang, "en")
})

test_that("regeneration_species_choices falls back to tolerances when core yields none", {
  testthat::local_mocked_bindings(
    regen_species_choices = function(...) NULL,
    regeneration_tolerances = function() data.frame(code = "x", label = "X"),
    .package = "nemeton")
  out <- nemetonshiny:::regeneration_species_choices()
  expect_true(is.data.frame(out) && all(c("code", "label") %in% names(out)))
})

.write_tiny_tif <- function(path, val = 100) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  r <- terra::rast(nrows = 4, ncols = 4, vals = val, crs = "EPSG:2154",
                   extent = terra::ext(0, 40, 0, 40))
  terra::writeRaster(r, path, overwrite = TRUE)
  path
}

test_that(".resolve_regen_dem finds the LiDAR MNT mosaic at the project root", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    proj <- getwd()
    # MNH (canopée) NE doit PAS être pris pour un DEM ; MNT (terrain) oui.
    .write_tiny_tif(file.path(proj, "lidar_mnh_mosaic.tif"), val = 15)
    .write_tiny_tif(file.path(proj, "lidar_mnt_mosaic.tif"), val = 250)
    dem <- nemetonshiny:::.resolve_regen_dem(proj)
    expect_s4_class(dem, "SpatRaster")
    expect_equal(unname(terra::minmax(dem)[1, 1]), 250)   # le MNT, pas le MNH
  })
})

test_that(".resolve_regen_dem prefers the explicit regeneration override", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    proj <- getwd()
    .write_tiny_tif(file.path(proj, "lidar_mnt_mosaic.tif"), val = 250)
    .write_tiny_tif(file.path(proj, "cache", "regeneration", "dem.tif"), val = 111)
    dem <- nemetonshiny:::.resolve_regen_dem(proj)
    expect_equal(unname(terra::minmax(dem)[1, 1]), 111)   # override prioritaire
  })
})

test_that(".resolve_regen_dem falls back to BD ALTI under cache/layers", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    proj <- getwd()
    .write_tiny_tif(file.path(proj, "cache", "layers", "dem.tif"), val = 88)
    dem <- nemetonshiny:::.resolve_regen_dem(proj)
    expect_equal(unname(terra::minmax(dem)[1, 1]), 88)
  })
})

test_that(".resolve_regen_dem returns NULL when no terrain DEM is present", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    proj <- getwd()
    # Seul un MNH (canopée) présent → pas de DEM terrain.
    .write_tiny_tif(file.path(proj, "lidar_mnh_mosaic.tif"), val = 12)
    expect_null(nemetonshiny:::.resolve_regen_dem(proj))
  })
})

test_that("load_regeneration_precomputed exposes the DEM even without a regen cache", {
  skip_if_not_installed("terra")
  withr::with_tempdir({
    proj <- getwd()
    .write_tiny_tif(file.path(proj, "lidar_mnt_mosaic.tif"), val = 200)
    pc <- nemetonshiny:::load_regeneration_precomputed(proj)
    expect_true(!is.null(pc$dem))
    expect_s4_class(pc$dem, "SpatRaster")
  })
})

test_that("add_regen_r_indicators injecte R6/R7 depuis le cache reGénération", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    proj <- getwd()
    dir <- file.path(proj, "cache", "regeneration")
    dir.create(dir, recursive = TRUE)
    units <- .regen_units(2)
    # Persistances reGénération : sensibilite (R6, 0-100 `sensibilite_score` +
    # z-score `sensibilite`) + r7 (R7), clés par ug_id.
    sf::st_write(cbind(units, sensibilite = c(-1.2, 3.4),
                       sensibilite_score = c(72, 41)),
                 file.path(dir, "sensibilite.gpkg"), quiet = TRUE)
    sf::st_write(cbind(units, R7 = c(80, 80)),
                 file.path(dir, "r7.gpkg"), quiet = TRUE)

    base <- units  # ug_id + geometry
    enr <- nemetonshiny:::add_regen_r_indicators(base, list(path = proj))
    expect_true(all(c("indicateur_r6_sensibilite", "indicateur_r7_gel") %in% names(enr)))
    # R6 : la valeur NORMALISÉE 0-100 (sensibilite_score) est préférée au z-score.
    expect_equal(enr$indicateur_r6_sensibilite, c(72, 41))
    expect_equal(enr$indicateur_r7_gel, c(80, 80))
  })
})

test_that("add_regen_r_indicators : repli sur le z-score si sensibilite_score absent", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    proj <- getwd()
    dir <- file.path(proj, "cache", "regeneration")
    dir.create(dir, recursive = TRUE)
    units <- .regen_units(2)
    # Cache antérieur (cœur < 0.161.0) : uniquement le z-score `sensibilite`.
    sf::st_write(cbind(units, sensibilite = c(-1.2, 3.4)),
                 file.path(dir, "sensibilite.gpkg"), quiet = TRUE)
    enr <- nemetonshiny:::add_regen_r_indicators(units, list(path = proj))
    expect_equal(enr$indicateur_r6_sensibilite, c(-1.2, 3.4))
  })
})

test_that("add_regen_r_indicators est un no-op sans cache reGénération / sans ug_id", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    proj <- getwd()
    base <- .regen_units(2)
    # Pas de cache -> base inchangée.
    expect_false("indicateur_r6_sensibilite" %in%
      names(nemetonshiny:::add_regen_r_indicators(base, list(path = proj))))
    # Pas de ug_id -> inchangé.
    no_id <- base; no_id$ug_id <- NULL
    expect_identical(nemetonshiny:::add_regen_r_indicators(no_id, list(path = proj)), no_id)
  })
})

test_that("REGEN_OUTPUT_COLUMNS covers the §7 contract", {
  cols <- nemetonshiny:::REGEN_OUTPUT_COLUMNS
  expect_true(all(c("indice_priorite_regen", "sensibilite", "njstress", "istress",
                    "rew_min", "couverture_pct", "priorite",
                    "indicateur_a3_microclimat", "indicateur_r6_sensibilite") %in% cols))
})

test_that(".regen_step strips ANSI escapes and keeps units on engine error", {
  skip_if_not_installed("sf")
  u <- sf::st_sf(ug_id = 1:2,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 2154))
  env <- new.env(); env$warnings <- character(0)

  out <- nemetonshiny:::.regen_step(
    function() stop("\033[38;5;252mmicroclimf\033[39m not wired"), u, env, "regen_sensibilite")

  expect_identical(out, u)                              # dégradation propre
  expect_length(env$warnings, 1L)
  expect_false(grepl("\033", env$warnings, fixed = TRUE))  # plus d'ANSI
  expect_match(env$warnings, "microclimf not wired")
})

# --- Détection des années de référence : `eobs_tx` est la série E-OBS ---------
# `load_regeneration_precomputed()` peuple `eobs_tx` / `eobs_rr`, jamais `eobs`.
# Lire `pc$eobs` seul passait donc TOUJOURS NULL au cœur, qui abandonne aussitôt :
# la détection d'années échouait systématiquement dès que l'utilisateur n'avait
# pas fixé les deux années à la main.

test_that("run_regeneration feeds the cached eobs_tx series to detect_years", {
  skip_if_not_installed("sf")

  seen <- new.env()
  testthat::local_mocked_bindings(
    microclimate_detect_years = function(eobs = NULL, aoi = NULL, ...) {
      seen$eobs <- eobs; list(year_moyenne = 2019, year_canicule = 2022)
    },
    regen_bilan_hydrique = function(units, ...) { units$njstress <- 5; units },
    indicateur_r3_secheresse = function(units, ...) units,
    indice_priorite_regen = function(units, ...) { units$indice_priorite_regen <- 1; units },
    .package = "nemeton")

  out <- nemetonshiny:::run_regeneration(
    .regen_units(2), cfg = list(hydric_only = TRUE),
    precomputed = list(biljou = "B", eobs_tx = "EOBS_TX_RASTER"))

  expect_equal(seen$eobs, "EOBS_TX_RASTER")   # et non NULL
  expect_equal(out$years$year_moyenne, 2019)
  expect_length(out$warnings, 0)
})

test_that("an explicit pc$eobs still wins over eobs_tx", {
  skip_if_not_installed("sf")

  seen <- new.env()
  testthat::local_mocked_bindings(
    microclimate_detect_years = function(eobs = NULL, ...) {
      seen$eobs <- eobs; list(year_moyenne = 2018, year_canicule = 2022)
    },
    regen_bilan_hydrique = function(units, ...) units,
    indicateur_r3_secheresse = function(units, ...) units,
    indice_priorite_regen = function(units, ...) units,
    .package = "nemeton")

  nemetonshiny:::run_regeneration(
    .regen_units(1), cfg = list(hydric_only = TRUE),
    precomputed = list(biljou = "B", eobs = "EXPLICIT", eobs_tx = "TX"))

  expect_equal(seen$eobs, "EXPLICIT")
})

test_that("no E-OBS series: detect_years is skipped, not failed", {
  skip_if_not_installed("sf")

  ran <- new.env(); ran$hit <- FALSE
  testthat::local_mocked_bindings(
    microclimate_detect_years = function(...) { ran$hit <- TRUE; stop("needs an E-OBS summer series") },
    regen_bilan_hydrique = function(units, ...) units,
    indicateur_r3_secheresse = function(units, ...) units,
    indice_priorite_regen = function(units, ...) units,
    .package = "nemeton")

  out <- nemetonshiny:::run_regeneration(
    .regen_units(1), cfg = list(hydric_only = TRUE),
    precomputed = list(biljou = "B"))

  expect_false(ran$hit)                 # appel inutile évité
  expect_length(out$warnings, 0)        # plus d'avertissement « detect_years: … »
  expect_null(out$years$year_moyenne)
})

# --- restore_regeneration : rattacher, jamais recalculer ----------------------
# Chemin d'ouverture de projet. `run_regeneration()` y était appelé jusqu'à
# v0.101.2 : son étape R3 (`indicateur_r3_secheresse(dem = )`) re-dérive la
# topographie depuis la mosaïque MNT LiDAR — 132 s mesurées sur 30 UGF — et gelait
# la session Shiny (mono-thread) à chaque clic sur un projet récent.

test_that("restore_regeneration attaches cached columns without touching the DEM", {
  skip_if_not_installed("sf")

  called <- character(0)
  testthat::local_mocked_bindings(
    regen_sensibilite = function(units, precomputed = NULL, ...) {
      called <<- c(called, "sensibilite"); units$sensibilite <- 60; units },
    regen_bilan_hydrique = function(units, precomputed = NULL, ...) {
      called <<- c(called, "biljou"); units$njstress <- 12; units },
    indice_priorite_regen = function(units, ...) {
      called <<- c(called, "priorite"); units$indice_priorite_regen <- 70; units },
    # Les étapes coûteuses ne doivent JAMAIS être atteintes.
    indicateur_r3_secheresse = function(units, ...) stop("R3 must not run on restore"),
    indicateur_a3_microclimat = function(units, ...) stop("A3 must not run on restore"),
    indicateur_a4_tamponnement = function(units, ...) stop("A4 must not run on restore"),
    indicateur_w4_vpd = function(units, ...) stop("W4 must not run on restore"),
    indicateur_r6_sensibilite = function(units, ...) stop("R6 must not run on restore"),
    microclimate_detect_years = function(...) stop("detect_years must not run on restore"),
    .package = "nemeton")

  out <- nemetonshiny:::restore_regeneration(
    .regen_units(3),
    precomputed = list(sensibilite = "S", biljou = "B", dem = "DEM", micro = "MICRO"))

  expect_equal(called, c("sensibilite", "biljou", "priorite"))
  expect_true(all(c("sensibilite", "njstress", "indice_priorite_regen") %in% names(out$units)))
  expect_equal(out$warnings, character(0))   # restaurer n'est pas analyser
})

test_that("restore_regeneration restores from biljou alone (no sensibilite cache)", {
  skip_if_not_installed("sf")
  testthat::local_mocked_bindings(
    regen_sensibilite = function(units, ...) stop("no sensibilite cache -> must be skipped"),
    regen_bilan_hydrique = function(units, ...) { units$njstress <- 5; units },
    indice_priorite_regen = function(units, ...) { units$indice_priorite_regen <- 30; units },
    .package = "nemeton")

  out <- nemetonshiny:::restore_regeneration(.regen_units(2), precomputed = list(biljou = "B"))
  expect_true("indice_priorite_regen" %in% names(out$units))
})

test_that("restore_regeneration returns NULL when there is nothing to restore", {
  skip_if_not_installed("sf")
  u <- .regen_units(2)
  expect_null(nemetonshiny:::restore_regeneration(u, precomputed = list()))
  expect_null(nemetonshiny:::restore_regeneration(u, precomputed = list(dem = "DEM")))
  expect_null(nemetonshiny:::restore_regeneration(u, precomputed = list(eobs_tx = "TX")))
  expect_null(nemetonshiny:::restore_regeneration(data.frame(x = 1), precomputed = list(biljou = "B")))
})

test_that("restore_regeneration returns NULL when the priority index cannot be built", {
  skip_if_not_installed("sf")
  # Le cœur échoue → pas d'indice → ni choroplèthe ni table : ne rien afficher
  # plutôt qu'une carte vide.
  testthat::local_mocked_bindings(
    regen_bilan_hydrique = function(units, ...) units,
    indice_priorite_regen = function(units, ...) stop("boom"),
    .package = "nemeton")
  expect_null(nemetonshiny:::restore_regeneration(.regen_units(2), precomputed = list(biljou = "B")))
})

# --- Carte de contexte E-OBS : lire les .nc cachés, ne jamais télécharger -----
# `load_regeneration_precomputed()` lit `eobs_{tx,rr}.tif`, des fichiers qu'AUCUN
# code n'écrit — la vraie donnée est un `.nc` sous `cache/regeneration/eobs/`.
# Le cœur exige les DEUX séries (carte bivariée) et seule `tx` est rapatriée par
# « Auto (E-OBS) » : la carte de contexte était donc vide, et muette.

.stub_eobs_nc <- function(project_path, var) {
  d <- file.path(project_path, "cache", "regeneration", "eobs")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(d, switch(var,
    tx = "eobs_maximum-temperature-2011-2024-v30-0e-0-1deg.nc",
    rr = "eobs_precipitation-amount-2011-2024-v30-0e-0-1deg.nc",
    tg = "eobs_mean-temperature-2011-2024-v30-0e-0-1deg.nc"))
  file.create(f)
  f
}

test_that("regen_eobs_cached_nc finds the cached NetCDF, per variable", {
  withr::with_tempdir({
    p <- getwd()
    expect_null(nemetonshiny:::regen_eobs_cached_nc(p, "tx"))   # rien encore
    expect_null(nemetonshiny:::regen_eobs_cached_nc(NULL, "tx"))

    .stub_eobs_nc(p, "tx")
    expect_match(nemetonshiny:::regen_eobs_cached_nc(p, "tx"), "maximum-temperature")
    expect_null(nemetonshiny:::regen_eobs_cached_nc(p, "rr"))   # rr toujours absent

    .stub_eobs_nc(p, "rr")
    expect_match(nemetonshiny:::regen_eobs_cached_nc(p, "rr"), "precipitation-amount")

    # tg (T° moyenne, spec 036) : nouvelle variable, motif mean-temperature.
    expect_null(nemetonshiny:::regen_eobs_cached_nc(p, "tg"))
    .stub_eobs_nc(p, "tg")
    expect_match(nemetonshiny:::regen_eobs_cached_nc(p, "tg"), "mean-temperature")
  })
})

test_that("regen_fetch_eobs_tg acquiert tg via le cœur (var=tg) et confirme le cache", {
  seen <- new.env(); seen$var <- NA_character_
  testthat::local_mocked_bindings(
    load_eobs_source = function(aoi, var, years = NULL, cache_dir = NULL, ...) {
      seen$var <- var
      # Simule l'écriture du .nc caché que le cœur aurait produit.
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
      file.create(file.path(cache_dir, "eobs_mean-temperature-2011-2024-v30-0e-0-1deg.nc"))
      NULL
    }, .package = "nemeton")

  withr::with_tempdir({
    p <- getwd()
    ugf <- .regen_units(2)
    ok <- nemetonshiny:::regen_fetch_eobs_tg(ugf, p)
    expect_true(ok)
    expect_identical(seen$var, "tg")              # bonne clé CDS demandée au cœur
    expect_match(nemetonshiny:::regen_eobs_cached_nc(p, "tg"), "mean-temperature")
  })
})

test_that("regen_fetch_eobs_tg valide ses entrées (sf + project_path)", {
  expect_false(nemetonshiny:::regen_fetch_eobs_tg(NULL, "/tmp"))
  expect_false(nemetonshiny:::regen_fetch_eobs_tg(.regen_units(1), NULL))
})

test_that("regen_context_availability reports each series separately", {
  withr::with_tempdir({
    p <- getwd()
    expect_equal(nemetonshiny:::regen_context_availability(p), list(tx = FALSE, rr = FALSE))
    .stub_eobs_nc(p, "tx")
    # Le cas réel : « Auto (E-OBS) » n'a rapatrié que la température.
    expect_equal(nemetonshiny:::regen_context_availability(p), list(tx = TRUE, rr = FALSE))
    .stub_eobs_nc(p, "rr")
    expect_equal(nemetonshiny:::regen_context_availability(p), list(tx = TRUE, rr = TRUE))
  })
})

# --- regen_reprioritize : mise à jour live de l'essence cible ----------------

test_that("regen_reprioritize delegates to the core priority index with species", {
  seen <- new.env(); seen$species <- "unset"
  testthat::local_mocked_bindings(
    indice_priorite_regen = function(units, species = NULL, ...) {
      seen$species <- species
      units$indice_priorite_regen <- if (is.null(species)) 10 else 99
      units
    }, .package = "nemeton")

  u <- .regen_units(2)
  # Essence renseignée -> transmise telle quelle au cœur.
  out <- nemetonshiny:::regen_reprioritize(u, "quercus_robur")
  expect_identical(seen$species, "quercus_robur")
  expect_equal(unique(out$indice_priorite_regen), 99)

  # Essence vide/NULL -> index générique (species = NULL).
  seen$species <- "unset"
  out0 <- nemetonshiny:::regen_reprioritize(u, "")
  expect_null(seen$species)
  expect_equal(unique(out0$indice_priorite_regen), 10)
})

test_that("regen_reprioritize is defensive: never blanks the map on failure", {
  testthat::local_mocked_bindings(
    indice_priorite_regen = function(units, species = NULL, ...) stop("boom"),
    .package = "nemeton")
  u <- .regen_units(1)
  # Un cœur qui lève -> résultat d'entrée renvoyé inchangé, pas d'erreur.
  expect_identical(nemetonshiny:::regen_reprioritize(u, "x"), u)
  # NULL en entrée -> NULL (rien à re-prioriser).
  expect_null(nemetonshiny:::regen_reprioritize(NULL, "x"))
})

# ---------------------------------------------------------------------------
# Commentaires de fiche parcelle persistés (spec 039)
# ---------------------------------------------------------------------------

test_that("save/load_regen_comments : round-trip, vides écartés, projet inconnu sûr", {
  d <- withr::local_tempdir()
  testthat::local_mocked_bindings(get_project_path = function(id) d, .package = "nemetonshiny")

  ok <- nemetonshiny:::save_regen_comments("p1",
    list(U1 = "Conseil A", U2 = "", U3 = "Note B"))
  expect_true(ok)

  cm <- nemetonshiny:::load_regen_comments("p1")
  expect_setequal(names(cm), c("U1", "U3"))   # U2 vide écarté
  expect_equal(cm[["U1"]], "Conseil A")
  expect_equal(cm[["U3"]], "Note B")
})

test_that("load_regen_comments : projet sans path / sans fichier -> list() vide", {
  testthat::local_mocked_bindings(get_project_path = function(id) NULL, .package = "nemetonshiny")
  expect_equal(nemetonshiny:::load_regen_comments("nope"), list())

  d <- withr::local_tempdir()
  testthat::local_mocked_bindings(get_project_path = function(id) d, .package = "nemetonshiny")
  expect_equal(nemetonshiny:::load_regen_comments("empty"), list())
})

test_that(".regen_comment_key : token stable et sûr pour un id d'input", {
  expect_equal(nemetonshiny:::.regen_comment_key("U-Nord 12"), "regcom_U_Nord_12")
  expect_equal(nemetonshiny:::.regen_comment_key(3L), "regcom_3")
})

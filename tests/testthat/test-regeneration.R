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
  out <- nemetonshiny:::run_regeneration(
    units, cfg = list(forest_type = "feuillu"),
    precomputed = list(micro = "MICRO", micro_moyenne = "M", micro_canicule = "C"))

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
    precomputed = list(micro = "MICRO"))

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

  out <- nemetonshiny:::run_regeneration(.regen_units(2), cfg = list())

  # Pas de crash : l'erreur moteur est capturée en warning actionnable
  expect_true(any(grepl("microclimf", out$warnings)))
  expect_false("sensibilite" %in% names(out$units))  # colonne absente (dégradation)
  expect_true("njstress" %in% names(out$units))       # les autres étapes tournent
  expect_equal(unique(out$units$indice_priorite_regen), 30)
})

test_that("run_regeneration validates its input", {
  expect_error(nemetonshiny:::run_regeneration(data.frame(x = 1)), "must be an sf")
})

test_that("REGEN_OUTPUT_COLUMNS covers the §7 contract", {
  cols <- nemetonshiny:::REGEN_OUTPUT_COLUMNS
  expect_true(all(c("indice_priorite_regen", "sensibilite", "njstress", "istress",
                    "rew_min", "couverture_pct", "priorite",
                    "indicateur_a3_microclimat", "indicateur_r6_sensibilite") %in% cols))
})

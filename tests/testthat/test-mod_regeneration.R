# Tests — module reGénération (spec 027, L4)
#
# testServer avec service mocké : validation UI, exécution du run, enrichissement
# des UGF, garde "pas de projet", et reactive retournée.

`%||%` <- function(a, b) if (is.null(a)) b else a

test_that(".fmt_elapsed formats the async button chrono", {
  expect_identical(nemetonshiny:::.fmt_elapsed(NULL), "")
  # MM:SS sous une heure ; H:MM:SS au-delà. Bornes de secondes tolérantes
  # (Sys.time() avance entre les deux appels).
  expect_match(nemetonshiny:::.fmt_elapsed(Sys.time() - 65), "^01:0[45]$")
  expect_match(nemetonshiny:::.fmt_elapsed(Sys.time() - 3725), "^1:02:0[45]$")
  expect_match(nemetonshiny:::.fmt_elapsed(Sys.time() - 5), "^00:0[456]$")
})

.regen_mod_units <- function(n = 3) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- i * 10
    sf::st_polygon(list(matrix(c(
      x0, 0, x0 + 5, 0, x0 + 5, 5, x0, 5, x0, 0), ncol = 2, byrow = TRUE)))
  })
  u <- sf::st_sf(ug_id = seq_len(n), indicateur_c1_biomasse = 50,
                 geometry = sf::st_sfc(polys, crs = 2154))
  u
}

test_that("mod_regeneration_ui builds", {
  skip_if_not_installed("bslib")
  ui <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    mod_regeneration_ui("regen")
  )
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list") ||
              inherits(ui, "bslib_fragment"))
})

test_that("run enriches UGF via the service and exposes the result", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  units <- .regen_mod_units(3)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj)

  called <- new.env(); called$cfg <- NULL
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    run_regeneration = function(u, cfg = list(), precomputed = NULL, progress = NULL) {
      called$cfg <- cfg
      u$indice_priorite_regen <- 70
      u$sensibilite <- 60
      u$couverture_pct <- 80
      u$rang_sensibilite <- seq_len(nrow(u))
      list(units = u, years = list(year_moyenne = 2018, year_canicule = 2022),
           warnings = character(0))
    },
    .package = "nemetonshiny"
  )

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(
        map_layer = "indice_priorite_regen", filter_coverage = TRUE,
        hydric_only = FALSE, forest_type = "feuillu",
        year_moyenne = NA, year_canicule = NA, lai_max = NA, species = "")
      session$setInputs(run = 1)

      expect_false(is.null(rv$result))
      expect_true("indice_priorite_regen" %in% names(rv$result))
      expect_equal(unique(rv$result$indice_priorite_regen), 70)
      expect_equal(rv$years$year_moyenne, 2018)
      expect_false(isTRUE(rv$running))
      # forest_type propagé au service
      expect_equal(called$cfg$forest_type, "feuillu")
      # reactive retournée
      expect_equal(session$returned$result(), rv$result)
    }
  )
})

test_that("run without a loaded project is a guarded no-op", {
  skip_if_not_installed("shiny")

  as <- shiny::reactiveValues(current_project = NULL)
  ran <- new.env(); ran$hit <- FALSE
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    regeneration_species_choices = function(...) NULL,
    run_regeneration = function(...) { ran$hit <- TRUE; NULL },
    .package = "nemetonshiny"
  )

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(run = 1)
      expect_false(ran$hit)        # service jamais appelé
      expect_null(rv$result)
    }
  )
})

# --- spec verrou : lecture seule (projet tenu par un autre / anonyme) --------
# En lecture seule (`app_state$readonly = TRUE`), toute action mutante du module
# est court-circuitée par `deny_if_readonly()` AVANT d'atteindre le service.

test_that("a read-only project gates the run action before the service", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")

  units <- .regen_mod_units(3)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  # readonly = TRUE : le projet est ouvert mais tenu par un autre utilisateur.
  as <- shiny::reactiveValues(current_project = proj, readonly = TRUE)

  ran <- new.env(); ran$hit <- FALSE
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    run_regeneration = function(...) { ran$hit <- TRUE; NULL },
    .package = "nemetonshiny"
  )

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(
        map_layer = "indice_priorite_regen", filter_coverage = TRUE,
        hydric_only = FALSE, forest_type = "feuillu",
        year_moyenne = NA, year_canicule = NA, lai_max = NA, species = "")
      session$setInputs(run = 1)
      expect_false(ran$hit)        # service jamais appelé en lecture seule
      expect_null(rv$result)
    }
  )
})

# --- spec 035 B2 : restauration à l'ouverture d'un projet --------------------
# Rouvrir un projet déjà analysé doit réafficher choroplèthe + table SANS clic,
# en relisant le cache disque. La restauration passe par `restore_regeneration()`
# et JAMAIS par `run_regeneration()` : l'étape R3 de ce dernier re-dérive la
# topographie depuis le MNT LiDAR (132 s mesurées sur 30 UGF) et gelait la session
# Shiny, mono-thread, à chaque ouverture de projet.

.regen_restored <- function(u) {
  u$indice_priorite_regen <- 42
  u$sensibilite <- 30
  u$njstress <- seq_len(nrow(u)) * 10
  u
}

test_that("opening a project with a cached biljou restores the result", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")

  units <- .regen_mod_units(3)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj)

  called <- new.env(); called$pc <- NULL; called$heavy <- FALSE
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    regeneration_species_choices = function(...) NULL,
    load_regeneration_precomputed = function(pp) list(biljou = "BILJOU_SF"),
    restore_regeneration = function(u, precomputed = NULL, ...) {
      called$pc <- precomputed
      list(units = .regen_restored(u), warnings = character(0))
    },
    # Garantie de performance : l'analyse complète ne doit PAS être déclenchée.
    run_regeneration = function(...) { called$heavy <- TRUE; NULL },
    .package = "nemetonshiny"
  )

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(
        map_layer = "indice_priorite_regen", filter_coverage = TRUE,
        forest_type = "feuillu", year_moyenne = 2018, year_canicule = 2022,
        lai_max = NA, species = "")

      # Restauré sans aucun clic sur « Lancer l'analyse ».
      expect_false(is.null(rv$result))
      expect_equal(unique(rv$result$indice_priorite_regen), 42)
      # Le cache disque est bien passé en `precomputed`.
      expect_equal(called$pc$biljou, "BILJOU_SF")
      # Aucune analyse complète (donc aucun recalcul de topographie).
      expect_false(called$heavy)
      # Restaurer n'est pas analyser : aucun avertissement remonté.
      expect_equal(rv$warnings, character(0))
      # mod_synthesis consomme ceci pour la perspective IA.
      expect_false(is.null(as$regeneration_result))
    }
  )
})

test_that("opening a project without a regeneration cache runs nothing", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")

  units <- .regen_mod_units(2)
  proj <- list(id = "p2", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj)

  ran <- new.env(); ran$hit <- FALSE
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    regeneration_species_choices = function(...) NULL,
    # `dem` / `eobs_*` seuls : pas de sortie de moteur → ne rien restaurer.
    load_regeneration_precomputed = function(pp) list(dem = "DEM", eobs_tx = "TX"),
    restore_regeneration = function(...) { ran$hit <- TRUE; NULL },
    run_regeneration = function(...) { ran$hit <- TRUE; NULL },
    .package = "nemetonshiny"
  )

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(forest_type = "feuillu", year_moyenne = NA,
                        year_canicule = NA, lai_max = NA, species = "")
      expect_false(ran$hit)          # aucune restauration ni analyse à vide
      expect_null(rv$result)
      expect_equal(rv$warnings, character(0))
    }
  )
})

test_that("switching to a project without cache clears the previous choropleth", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")

  analysed <- list(id = "p1", path = withr::local_tempdir(),
                   indicators_sf = .regen_mod_units(3))
  virgin   <- list(id = "p2", path = withr::local_tempdir(),
                   indicators_sf = .regen_mod_units(2))
  as <- shiny::reactiveValues(current_project = analysed)

  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    regeneration_species_choices = function(...) NULL,
    load_regeneration_precomputed = function(pp) {
      if (identical(pp, analysed$path)) list(biljou = "B") else list()
    },
    restore_regeneration = function(u, ...) {
      list(units = .regen_restored(u), warnings = character(0))
    },
    .package = "nemetonshiny"
  )

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(forest_type = "feuillu", year_moyenne = 2018,
                        year_canicule = 2022, lai_max = NA, species = "")
      expect_equal(nrow(rv$result), 3)      # projet analysé : restauré

      as$current_project <- virgin
      session$flushReact()
      expect_null(rv$result)                # projet vierge : choroplèthe purgé
      expect_null(as$regeneration_result)
    }
  )
})

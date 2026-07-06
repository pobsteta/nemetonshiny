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

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

test_that("changing the target species live-re-prioritises without a full run", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")

  units <- .regen_mod_units(3)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj)

  repriced <- new.env(); repriced$species <- "unset"; repriced$n <- 0L
  runs <- new.env(); runs$n <- 0L
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    run_regeneration = function(u, cfg = list(), precomputed = NULL, progress = NULL) {
      runs$n <- runs$n + 1L
      u$indice_priorite_regen <- 50
      list(units = u, years = list(), warnings = character(0))
    },
    regen_reprioritize = function(units, species) {
      repriced$n <- repriced$n + 1L; repriced$species <- species
      units$indice_priorite_regen <- 77
      units
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
      # Sans résultat, changer l'essence ne déclenche aucune re-priorisation.
      session$setInputs(species = "quercus_robur")
      expect_equal(repriced$n, 0L)

      # Un run produit un résultat (indice = 50).
      session$setInputs(run = 1)
      expect_equal(runs$n, 1L)
      expect_equal(unique(rv$result$indice_priorite_regen), 50)

      # Changer l'essence re-priorise EN PLACE (indice = 77) sans nouveau run.
      session$setInputs(species = "fagus_sylvatica")
      expect_equal(repriced$n, 1L)
      expect_identical(repriced$species, "fagus_sylvatica")
      expect_equal(runs$n, 1L)                       # aucun run complet relancé
      expect_equal(unique(rv$result$indice_priorite_regen), 77)
      expect_equal(unique(as$regeneration_result$indice_priorite_regen), 77)
    }
  )
})

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
  # La restauration est différée à l'entrée dans l'onglet reGénération : simuler
  # l'onglet actif, sinon l'observateur B ne se déclenche pas.
  as <- shiny::reactiveValues(current_project = proj, active_main_tab = "regeneration")

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

      # La restauration est différée d'un tick (`later`) pour peindre d'abord le
      # toast « Chargement des cartes… » : avancer la boucle avant d'asserter.
      later::run_now()

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
  # Onglet reGénération actif : on teste vraiment la branche « pas de cache »,
  # pas seulement le fait d'être hors onglet.
  as <- shiny::reactiveValues(current_project = proj, active_main_tab = "regeneration")

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
      later::run_now()
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
  as <- shiny::reactiveValues(current_project = analysed,
                              active_main_tab = "regeneration")

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
      later::run_now()                      # restauration différée (later)
      expect_equal(nrow(rv$result), 3)      # projet analysé : restauré

      as$current_project <- virgin
      session$flushReact()
      later::run_now()
      expect_null(rv$result)                # projet vierge : choroplèthe purgé
      expect_null(as$regeneration_result)
    }
  )
})

# --- Brief 035 verrou-boutons : exclusion mutuelle des calculs ----------------
# Pendant qu'un calcul lourd tourne (busy() TRUE), tout autre déclencheur est un
# no-op serveur : la tâche visée ne démarre pas et un avertissement est émis. Le
# grisage client (update_task_button / updateActionButton) n'est que cosmétique ;
# la garde serveur `deny_if_busy()` est la protection robuste testée ici.
#
# Plutôt qu'invoquer un vrai `future` lent (non déterministe, worker réel), on
# rebinde le réactif local `busy` dans l'environnement partagé du serveur — celui
# que `deny_if_busy()` interroge — pour simuler « un calcul est déjà en cours ».

test_that("a trigger is a guarded no-op while another computation runs", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")

  units <- .regen_mod_units(2)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj, readonly = FALSE)

  notes <- new.env(); notes$n <- 0L
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    regeneration_species_choices = function(...) NULL,
    load_regeneration_precomputed = function(pp) list(),
    # Sans ce mock, run_frost retournerait tôt (meteoland absent) et le test
    # serait vacant : on veut que SEULE la garde busy bloque l'invocation.
    regen_meteoland_available = function() TRUE,
    .package = "nemetonshiny"
  )
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) { notes$n <- notes$n + 1L; "nid" },
    .package = "shiny"
  )

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(forest_type = "feuillu", year_moyenne = NA,
                        year_canicule = NA, buffer_km = 25, species = "")

      # Baseline : la tâche gel n'a pas encore démarré.
      expect_identical(frost_task$status(), "initial")

      # Simuler un calcul déjà en cours : muter le binding `busy` DANS
      # l'environnement du serveur (testServer évalue cette expression dans un
      # env ENFANT ; un simple `busy <-` créerait un shadow local que
      # `deny_if_busy()` ne verrait pas).
      environment(deny_if_busy)$busy <- function() TRUE
      notes$n <- 0L
      session$setInputs(run_frost = 1)

      # frost_task JAMAIS invoquée (sans la garde, meteoland dispo + projet valide
      # l'auraient lancée) et l'avertissement « déjà en cours » a été émis.
      expect_identical(frost_task$status(), "initial")
      expect_gte(notes$n, 1L)
    }
  )
})

# --- Verrou instantané côté client : re-synchronisation serveur --------------
# Le grisage JS est instantané mais aveugle : si un clic ne lance finalement rien
# (pas de projet, prérequis KO…), le serveur DOIT ré-activer les boutons. Le
# compteur `rv$click_tick`, bumpé à chaque clic de bouton de calcul, force
# l'observer de verrou à recalculer l'état d'autorité. On vérifie ici que tout
# clic de bouton de calcul l'incrémente (donc déclenche la re-synchronisation).

test_that("every calc-button click bumps click_tick (server re-sync)", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")

  # Pas de projet chargé : chaque clic avorte tôt — c'est précisément le cas où
  # la re-synchronisation doit remettre les boutons cliquables.
  as <- shiny::reactiveValues(current_project = NULL)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    regeneration_species_choices = function(...) NULL,
    .package = "nemetonshiny"
  )

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      # Flush de préchauffage : consomme l'init paresseux de l'observeEvent
      # (ignoreInit=TRUE) — en production, c'est le flush de démarrage de session
      # qui joue ce rôle, donc aucun bump au démarrage.
      session$setInputs(species = "")
      expect_equal(rv$click_tick, 0L)
      session$setInputs(run = 1)
      expect_equal(rv$click_tick, 1L)          # clic « Lancer l'analyse »
      session$setInputs(auto_years = 1)
      expect_equal(rv$click_tick, 2L)          # clic « Auto (E-OBS) »
      session$setInputs(run_engine = 1)
      expect_equal(rv$click_tick, 3L)          # clic « Lancer le moteur »
    }
  )
})

# --- Bilan persistant sous le bouton « Risque de gel » -----------------------
# En fin de calcul gel, un message reste affiché sous le bouton — surtout le cas
# « aucun jour de gel tardif », qui sinon donnait l'impression d'un run sans effet.

test_that("frost_status renders the persistent end-of-run summary", {
  skip_if_not_installed("shiny")

  as <- shiny::reactiveValues(current_project = NULL)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    regeneration_species_choices = function(...) NULL,
    .package = "nemetonshiny"
  )

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      # uiOutput → testServer renvoie list(html=, deps=) ; extraire le HTML.
      frost_html <- function() {
        v <- output$frost_status
        if (is.list(v)) as.character(v$html %||% "") else as.character(v %||% "")
      }
      session$setInputs(species = "")
      # Aucun calcul lancé → aucun bilan.
      expect_identical(frost_html(), "")

      # Aucun gel détecté : message vert « rassurant ».
      rv$frost_summary <- list(status = "none")
      session$flushReact()
      expect_match(frost_html(), "aucun jour de gel", ignore.case = TRUE)

      # Gel détecté : médiane + étendue.
      rv$frost_summary <- list(status = "detected",
        stats = list(median = "2.0", min = "0.0", max = "5.0", n = 3L))
      session$flushReact()
      expect_match(frost_html(), "Gel tardif")
      expect_match(frost_html(), "2.0")

      # Pendant un run, pas de bilan (la notif bas-droite suffit).
      rv$frost_running <- TRUE
      session$flushReact()
      expect_identical(frost_html(), "")
    }
  )
})

# --- Brief 035 §8 : R7 (gel) survit à un recalcul dans la même session --------
# run_regeneration()/le moteur ne produisent pas r7_gel_days (pas de tmin) et
# écrasaient rv$result, vidant la couche « Gelées tardives » calculée avant. Le
# report par ug_id (.regen_attach_r7) doit préserver R7 tout en rafraîchissant
# les colonnes d'analyse.

test_that("R7 (gel) survives a re-analysis via input$run", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")
  units <- .regen_mod_units(3)                       # ug_id 1,2,3
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    run_regeneration = function(u, cfg = list(), precomputed = NULL, ...) {
      u$indice_priorite_regen <- 55                  # analyse rafraîchie, PAS de R7
      list(units = u, years = list(), warnings = character(0))
    },
    .package = "nemetonshiny")

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(map_layer = "indice_priorite_regen", filter_coverage = TRUE,
        hydric_only = FALSE, forest_type = "feuillu", year_moyenne = NA,
        year_canicule = NA, lai_max = NA, species = "")
      # Simuler un R7 déjà calculé sur le résultat courant (avant le re-run).
      prior <- units
      prior$r7_gel_days <- c(1, 2, 3); prior$r7_status <- "calculated"
      rv$result <- prior

      session$setInputs(run = 1)

      # R7 conservé (aligné par ug_id) ET analyse bien rafraîchie.
      expect_true("r7_gel_days" %in% names(rv$result))
      expect_equal(rv$result$r7_gel_days, c(1, 2, 3))
      expect_equal(unique(rv$result$indice_priorite_regen), 55)
      # Publié aussi pour mod_synthesis.
      expect_equal(as$regeneration_result$r7_gel_days, c(1, 2, 3))
    }
  )
})

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

# --- Légende bivariée : plage observée + 0 garanti visible --------------------
test_that(".regen_biv_axis uses observed range and keeps 0 in view", {
  # Tendances toutes positives (T°max) : le min est repoussé un peu avant 0 pour
  # que la ligne pointillée du 0 reste visible.
  ys <- nemetonshiny:::.regen_biv_axis(
    list(palette = list(low = 0.3, high = 1.1)), c(0, 0.4, 0.8, 1.2), 0.2)
  expect_lt(ys$range[1], 0)                 # min repoussé sous 0
  expect_equal(ys$range[2], 1.1)            # max = observé
  expect_true(ys$zero > 0 && ys$zero < 1)   # 0 dans la grille

  # Plage qui encadre déjà 0 (précipitations) : bornes = observé.
  xs <- nemetonshiny:::.regen_biv_axis(
    list(palette = list(low = -50, high = 40)), c(-80, -40, 0, 40), 0.6)
  expect_equal(xs$range, c(-50, 40))
  expect_equal(round(xs$zero, 3), 0.556)

  # Repli sur les seuils cœur si la plage observée manque.
  fb <- nemetonshiny:::.regen_biv_axis(list(palette = list()), c(0, 0.4, 0.8, 1.2), 0.2)
  expect_equal(fb$range, c(0, 0.4, 0.8, 1.2))
  expect_equal(fb$zero, 0.2)
})

test_that(".regen_biv_title splits the parenthetical into a subtitle", {
  r <- nemetonshiny:::.regen_biv_title("Tendance bivariée (T°max estivale × précipitations)")
  expect_equal(r$title, "Tendance bivariée")
  expect_equal(r$subtitle, "(T°max estivale × précipitations)")

  # Sans parenthèse : titre seul, pas de sous-titre.
  r2 <- nemetonshiny:::.regen_biv_title("Contexte bivarié")
  expect_equal(r2$title, "Contexte bivarié")
  expect_null(r2$subtitle)

  # Libellé vide / NULL : titre vide, pas de sous-titre.
  expect_null(nemetonshiny:::.regen_biv_title(NULL)$subtitle)
})

# --- spec 036 : clic sur la maille E-OBS -> panneau de 4 graphes -------------

test_that("un clic sur la carte de contexte extrait série + climatologie et ouvre la modale", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")

  units <- .regen_mod_units(3)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj)

  seen <- new.env(); seen$series <- 0L; seen$clim <- 0L; seen$clim_vars <- character(0)
  # Accesseurs cœur mockés : l'app ne fait que clic -> point -> tracé.
  testthat::local_mocked_bindings(
    eobs_summer_series = function(stack, point) {
      seen$series <- seen$series + 1L
      data.frame(year = 2011:2020, value = seq(24, 29, length.out = 10))
    },
    eobs_trend_fit = function(series) list(slope_decade = 1.2, intercept = -400,
      r2 = 0.8, p_value = 0.01, n = 10),
    eobs_monthly_climatology = function(daily, point, var, years = NULL) {
      seen$clim <- seen$clim + 1L; seen$clim_vars <- c(seen$clim_vars, var)
      data.frame(month = 1:12, value = rep(50, 12))
    },
    .package = "nemeton")
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    # Stack estival non-NULL -> la série est extraite.
    .regen_load_eobs_buffered = function(units, project_path, var, buffer_m) "STACK",
    # tg + rr présents dans le cache -> climatologie température (tg) + précip.
    regen_eobs_cached_nc = function(project_path, var = c("tx", "rr", "tg")) {
      var <- match.arg(var); paste0("/fake/", var, ".nc")
    },
    .package = "nemetonshiny")

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(context_view = "tx", buffer_km = 25)
      session$setInputs(context_map_click = list(lng = 6.1, lat = 47.2))

      cs <- rv$click_series
      expect_false(is.null(cs))
      expect_length(cs$entries, 1L)                 # vue tx -> une variable
      expect_equal(cs$entries[[1]]$var, "tx")
      expect_true(seen$series >= 1L)                # eobs_summer_series appelé
      expect_true(seen$clim >= 1L)                  # eobs_monthly_climatology appelé
      expect_true(isTRUE(cs$temp_is_tg))            # tg dispo -> température réelle
      expect_true("tg" %in% seen$clim_vars)         # climatologie température = tg
      # Forcer l'évaluation des 4 rendus = vérifier qu'ils ne plantent pas.
      expect_false(is.null(output$ctx_g1)); expect_false(is.null(output$ctx_g2))
      expect_false(is.null(output$ctx_g3)); expect_false(is.null(output$ctx_g4))
    }
  )
})

test_that("un clic sans projet chargé est un no-op gardé (pas de crash)", {
  skip_if_not_installed("shiny")
  as <- shiny::reactiveValues(current_project = NULL)
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    regeneration_species_choices = function(...) NULL,
    .package = "nemetonshiny")
  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(context_map_click = list(lng = 6.1, lat = 47.2))
      expect_null(rv$click_series)
    }
  )
})

test_that("vue bivariée : les 4 graphes sont peuplés, distribution tx ET rr", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf"); skip_if_not_installed("terra")

  units <- .regen_mod_units(3)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj)

  # Petit raster de pente réel couvrant le point cliqué (extraction distribution).
  slope_rast <- terra::rast(nrows = 5, ncols = 5, xmin = 6, xmax = 6.5,
    ymin = 47, ymax = 47.5, crs = "EPSG:4326", vals = seq_len(25))

  testthat::local_mocked_bindings(
    eobs_summer_series = function(stack, point)
      data.frame(year = 2011:2020, value = seq(24, 29, length.out = 10)),
    eobs_trend_fit = function(series) list(slope_decade = 1.2, intercept = -400,
      r2 = 0.8, p_value = 0.01, n = 10),
    eobs_monthly_climatology = function(daily, point, var, years = NULL)
      data.frame(month = 1:12, value = rep(50, 12)),
    .package = "nemeton")
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    .regen_load_eobs_buffered = function(units, project_path, var, buffer_m) "STACK",
    regen_eobs_cached_nc = function(project_path, var = c("tx", "rr", "tg")) {
      var <- match.arg(var); paste0("/fake/", var, ".nc")
    },
    # Rasters de pente tx & rr présents en cache -> distribution des DEUX.
    regeneration_context_cached = function(project_path, view = "tx")
      list(raster = slope_rast, meta = list(status = "ok")),
    .package = "nemetonshiny")

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(context_view = "bivariate", buffer_km = 25)
      session$setInputs(context_map_click = list(lng = 6.1, lat = 47.2))

      cs <- rv$click_series
      expect_equal(cs$view, "bivariate")
      expect_length(cs$entries, 2L)                 # tx ET rr (graphes 1-2)
      expect_length(cs$distrib, 2L)                 # distribution tx ET rr (graphe 3)
      expect_true(all(is.finite(vapply(cs$distrib, function(e) e$point, numeric(1)))))
      # Les 4 rendus produisent un plotly sans erreur, y compris le graphe 3.
      expect_false(is.null(output$ctx_g1)); expect_false(is.null(output$ctx_g3))
    }
  )
})

test_that("recompute_context purge le cache des 3 vues et re-déclenche le calcul", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")

  units <- .regen_mod_units(2)
  pp <- withr::local_tempdir()
  # Simule des rasters de contexte cachés pour les 3 vues.
  dir.create(file.path(pp, "cache", "regeneration", "eobs"), recursive = TRUE)
  for (v in c("tx", "rr", "bivariate")) {
    paths <- nemetonshiny:::.regen_context_raster_paths(pp, v)
    file.create(paths$tif); file.create(paths$meta)
  }
  proj <- list(id = "p1", path = pp, indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj, active_main_tab = "other")

  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    .package = "nemetonshiny")

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      session$setInputs(context_view = "bivariate")
      before <- rv$context_refresh
      session$setInputs(recompute_context = 1)
      # Les 3 caches sont supprimés.
      for (v in c("tx", "rr", "bivariate")) {
        paths <- nemetonshiny:::.regen_context_raster_paths(pp, v)
        expect_false(file.exists(paths$tif))
        expect_false(file.exists(paths$meta))
      }
      # L'observer lazy est re-déclenché (compteur bumpé, vue chargée réinitialisée).
      expect_gt(rv$context_refresh, before)
      expect_null(rv$context_loaded_view)
    }
  )
})

# ---------------------------------------------------------------------------
# Sélection croisée carte <-> tableau (pattern « Plan d'actions »)
# ---------------------------------------------------------------------------
# Mocks INLINE dans chaque test : local_mocked_bindings est scopé au frame
# appelant, donc un helper qui les pose les défait à son retour (le vrai moteur
# tournerait). run_regeneration mocké -> rv$result à 3 UGF (ug_id 1..3).

.regen_run_mock <- function(u, cfg = list(), precomputed = NULL, progress = NULL) {
  u$indice_priorite_regen <- 70
  u$sensibilite <- 60
  u$couverture_pct <- 80
  u$rang_sensibilite <- seq_len(nrow(u))
  list(units = u, years = list(), warnings = character(0))
}

.regen_sel_inputs <- function(session) {
  session$setInputs(
    map_layer = "indice_priorite_regen", filter_coverage = TRUE,
    hydric_only = FALSE, forest_type = "feuillu",
    year_moyenne = NA, year_canicule = NA, lai_max = NA, species = "")
  session$setInputs(run = 1)
}

test_that("un clic carte bascule la sélection (toggle) et pilote les lignes", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf"); skip_if_not_installed("DT")

  units <- .regen_mod_units(3)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj)

  picked <- new.env(); picked$rows <- "unset"
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    run_regeneration = .regen_run_mock, .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    dataTableProxy = function(...) structure(list(), class = "dataTableProxy"),
    selectRows = function(proxy, rows, ...) { picked$rows <- rows; invisible(proxy) },
    .package = "DT")

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      .regen_sel_inputs(session)
      expect_false(is.null(rv$result))

      # Clic sur l'UGF 2 -> sélectionnée, ligne 2 surlignée dans le tableau.
      session$setInputs(map_shape_click = list(id = "2"))
      expect_identical(selected_ug_rv(), "2")
      expect_equal(picked$rows, 2L)

      # Clic sur l'UGF 3 -> sélection multiple {2,3}, lignes {2,3}.
      session$setInputs(map_shape_click = list(id = "3"))
      expect_setequal(selected_ug_rv(), c("2", "3"))
      expect_setequal(picked$rows, c(2L, 3L))

      # Re-clic sur 2 -> retiré (toggle), reste {3}.
      session$setInputs(map_shape_click = list(id = "2"))
      expect_identical(selected_ug_rv(), "3")
      expect_equal(picked$rows, 3L)
    }
  )
})

test_that("la sélection de lignes alimente la source de vérité (sens table -> carte)", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf"); skip_if_not_installed("DT")

  units <- .regen_mod_units(3)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj)

  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    run_regeneration = .regen_run_mock, .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    dataTableProxy = function(...) structure(list(), class = "dataTableProxy"),
    selectRows = function(proxy, rows, ...) invisible(proxy), .package = "DT")

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      .regen_sel_inputs(session)

      # Sélection des lignes 1 et 3 -> ug_id "1" et "3".
      session$setInputs(table_rows_selected = c(1L, 3L))
      expect_setequal(selected_ug_rv(), c("1", "3"))

      # Désélection totale -> vide (ignoreNULL = FALSE).
      session$setInputs(table_rows_selected = NULL)
      expect_length(selected_ug_rv(), 0L)
    }
  )
})

test_that("« Effacer la sélection » vide carte + tableau", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf"); skip_if_not_installed("DT")

  units <- .regen_mod_units(3)
  proj <- list(id = "p1", path = withr::local_tempdir(), indicators_sf = units)
  as <- shiny::reactiveValues(current_project = proj)

  cleared <- new.env(); cleared$rows <- "unset"
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    load_regeneration_precomputed = function(pp) list(),
    regeneration_species_choices = function(...) NULL,
    run_regeneration = .regen_run_mock, .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    dataTableProxy = function(...) structure(list(), class = "dataTableProxy"),
    selectRows = function(proxy, rows, ...) { cleared$rows <- rows; invisible(proxy) },
    .package = "DT")

  shiny::testServer(
    nemetonshiny:::mod_regeneration_server,
    args = list(app_state = as),
    {
      .regen_sel_inputs(session)
      session$setInputs(map_shape_click = list(id = "2"))
      expect_length(selected_ug_rv(), 1L)

      session$setInputs(clear_selection = 1)
      expect_length(selected_ug_rv(), 0L)
      expect_null(cleared$rows)      # DT::selectRows(proxy, NULL) -> tout désélectionné
    }
  )
})

test_that(".species_bold_render met en gras l'optgroup des essences présentes", {
  js <- nemetonshiny:::.species_bold_render("Essences présentes (BDforêt v2)")
  # Marqueur = libellé d'optgroup ; met <strong> quand item.optgroup == ce libellé.
  expect_match(js, 'item.optgroup === "Essences présentes (BDforêt v2)"', fixed = TRUE)
  expect_match(js, "<strong>", fixed = TRUE)
  # Rend option ET item (dropdown + valeur sélectionnée).
  expect_match(js, "option:", fixed = TRUE)
  expect_match(js, "item:", fixed = TRUE)
  # Guillemets d'un libellé échappés (pas de JS cassé).
  js2 <- nemetonshiny:::.species_bold_render('a"b')
  expect_match(js2, '\\"', fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Top-3 essences par UGF (spec 039) — wrapper service + rendu fiche
# ---------------------------------------------------------------------------

test_that("regeneration_species_ranking : wrapper NA/empty-safe autour du cœur", {
  skip_if_not_installed("sf")
  # Non-sf / vide -> NULL, sans appeler le cœur.
  expect_null(nemetonshiny:::regeneration_species_ranking(NULL))
  expect_null(nemetonshiny:::regeneration_species_ranking(data.frame(a = 1)))

  units <- .regen_mod_units(2)
  long <- data.frame(ug_id = c("1", "1", "2"), rank = c(1L, 2L, 1L),
    species_code = c("a", "b", "a"), label = c("A", "B", "A"),
    suitability = c(95, 90, 88), limiting_factor = c("secheresse", "gel", "chaleur"),
    confidence = "eleve", stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    regen_rank_species = function(units, top_n = 3, region = "BFC", ...) long,
    .package = "nemeton")
  out <- nemetonshiny:::regeneration_species_ranking(units, top_n = 3L)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  # Cœur renvoie 0 ligne -> NULL.
  testthat::local_mocked_bindings(
    regen_rank_species = function(units, top_n = 3, region = "BFC", ...) long[0, ],
    .package = "nemeton")
  expect_null(nemetonshiny:::regeneration_species_ranking(units))
})

test_that(".regen_species_ranking_ui rend le top-N d'une UGF, traduit facteur/confiance", {
  i18n <- nemetonshiny:::get_i18n("fr")
  rk <- data.frame(ug_id = c("U1", "U1", "U2"), rank = c(1L, 2L, 1L),
    label = c("Pin", "Chêne", "Sapin"), suitability = c(97.2, 93.9, 80),
    limiting_factor = c("secheresse", "gel", "ombre"),
    confidence = c("eleve", "moyen", "faible"), stringsAsFactors = FALSE)
  ui <- nemetonshiny:::.regen_species_ranking_ui(rk, "U1", i18n)
  html <- as.character(ui)
  expect_true(grepl("Pin", html) && grepl("Chêne", html))   # les 2 de U1
  expect_false(grepl("Sapin", html))                              # pas ceux de U2
  expect_true(grepl("97/100", html))                             # suitability arrondie
  expect_true(grepl("sécheresse", html))                     # facteur limitant traduit
  expect_true(grepl("confiance élevée", html))          # confiance traduite
  # NA-safe : UGF absente / ranking NULL / colonnes manquantes -> NULL.
  expect_null(nemetonshiny:::.regen_species_ranking_ui(rk, "ZZ", i18n))
  expect_null(nemetonshiny:::.regen_species_ranking_ui(NULL, "U1", i18n))
  expect_null(nemetonshiny:::.regen_species_ranking_ui(data.frame(x = 1), "U1", i18n))
})

# ---------------------------------------------------------------------------
# Conseil de régénération par IA (spec 039, P2)
# ---------------------------------------------------------------------------
# NB : `session$setInputs()` d'amorçage AVANT de poser rv$result — l'observateur
# de reset données (observeEvent(list(current_project, units_sf()))) fait son
# fire d'init au premier flush (réactifs paresseux en testServer) et remettrait
# rv$result à NULL. À l'init réel de l'app, rv$result est déjà NULL (inoffensif).

.regen_ai_ranking <- function() data.frame(
  ug_id = "1", rank = 1L, label = "Pin", suitability = 90,
  limiting_factor = "gel", confidence = "eleve", stringsAsFactors = FALSE)

test_that("conseil IA : clé API absente -> aucun appel LLM, historique vide", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")
  as <- shiny::reactiveValues(current_project = list(path = withr::local_tempdir()))
  chat_calls <- new.env(); chat_calls$n <- 0L
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_app_config = function(...) "anthropic",
    get_llm_api_key_var = function(provider) "NMT_FAKE_UNSET_KEY",
    regeneration_species_ranking = function(units, ...) .regen_ai_ranking(),
    create_llm_chat = function(sp) { chat_calls$n <- chat_calls$n + 1L; list(chat = function(...) "x") },
    .package = "nemetonshiny")
  withr::local_envvar(c(NMT_FAKE_UNSET_KEY = ""))
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(regen_ai_scope = "all", regen_ai_replace = TRUE)
    rv$result <- .regen_mod_units(2)
    session$setInputs(regen_ai_send = 1)
    expect_length(regen_ai_hist(), 0L)
    expect_equal(chat_calls$n, 0L)
  })
})

test_that("conseil IA : sans classement (pas de résultat) -> garde, historique vide", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")
  as <- shiny::reactiveValues(current_project = list(path = withr::local_tempdir()))
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_app_config = function(...) "anthropic",
    get_llm_api_key_var = function(provider) NULL,
    create_llm_chat = function(sp) list(chat = function(...) "x"),
    .package = "nemetonshiny")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(regen_ai_scope = "all", regen_ai_replace = TRUE)
    rv$result <- NULL
    session$setInputs(regen_ai_send = 1)
    expect_length(regen_ai_hist(), 0L)
  })
})

test_that("conseil IA : portée sélection sans UGF sélectionnée -> garde", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")
  as <- shiny::reactiveValues(current_project = list(path = withr::local_tempdir()))
  chat_calls <- new.env(); chat_calls$n <- 0L
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_app_config = function(...) "anthropic",
    get_llm_api_key_var = function(provider) NULL,
    regeneration_species_ranking = function(units, ...) .regen_ai_ranking(),
    build_system_prompt = function(...) "sys",
    create_llm_chat = function(sp) { chat_calls$n <- chat_calls$n + 1L; list(chat = function(...) "x") },
    .package = "nemetonshiny")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(regen_ai_scope = "selected", regen_ai_replace = TRUE)
    rv$result <- .regen_mod_units(2)
    session$setInputs(regen_ai_send = 1)
    expect_length(regen_ai_hist(), 0L)
    expect_equal(chat_calls$n, 0L)
  })
})

test_that("conseil IA : chemin succès -> bloc {q,a} stocké dans l'historique", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")
  as <- shiny::reactiveValues(current_project = list(path = withr::local_tempdir()))
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_app_config = function(...) "anthropic",
    get_llm_api_key_var = function(provider) NULL,
    regeneration_species_ranking = function(units, ...) .regen_ai_ranking(),
    build_system_prompt = function(...) "sys",
    create_llm_chat = function(sp) list(chat = function(prompt, echo = FALSE) "Conseil de test IA"),
    .package = "nemetonshiny")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(regen_ai_scope = "all", regen_ai_replace = TRUE)
    rv$result <- .regen_mod_units(2)
    session$setInputs(regen_ai_input = "Et sur sol calcaire ?")
    session$setInputs(regen_ai_send = 1)
    h <- regen_ai_hist()
    expect_length(h, 1L)
    expect_equal(h[[1]]$a, "Conseil de test IA")
    expect_equal(h[[1]]$q, "Et sur sol calcaire ?")
  })
})

test_that("conseil IA : case décochée -> les conseils s'ajoutent ; Effacer vide", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")
  as <- shiny::reactiveValues(current_project = list(path = withr::local_tempdir()))
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    get_app_config = function(...) "anthropic",
    get_llm_api_key_var = function(provider) NULL,
    regeneration_species_ranking = function(units, ...) .regen_ai_ranking(),
    build_system_prompt = function(...) "sys",
    create_llm_chat = function(sp) list(chat = function(prompt, echo = FALSE) "Conseil"),
    .package = "nemetonshiny")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(regen_ai_scope = "all", regen_ai_replace = FALSE)
    rv$result <- .regen_mod_units(2)
    session$setInputs(regen_ai_send = 1)
    session$setInputs(regen_ai_send = 2)
    expect_length(regen_ai_hist(), 2L)
    session$setInputs(regen_ai_clear = 1)
    expect_length(regen_ai_hist(), 0L)
  })
})

# ---------------------------------------------------------------------------
# Commentaire de fiche parcelle : insertion globale du conseil IA (spec 039)
# ---------------------------------------------------------------------------

test_that("insertion conseil IA : remplit le commentaire des UGF sélectionnées", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")
  as <- shiny::reactiveValues(current_project = list(path = withr::local_tempdir()))
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    .package = "nemetonshiny")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(regen_ai_scope = "all")   # flush init (reset observers)
    selected_ug_rv(c("1", "2"))
    regen_ai_hist(list(list(q = "", a = "Conseil de régénération X")))
    session$setInputs(regen_comment_insert_ai = 1)
    cm <- regen_comments_rv()
    expect_equal(cm[["1"]], "Conseil de régénération X")
    expect_equal(cm[["2"]], "Conseil de régénération X")
  })
})

test_that("insertion conseil IA : sans conseil généré -> aucun commentaire écrit", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")
  as <- shiny::reactiveValues(current_project = list(path = withr::local_tempdir()))
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    .package = "nemetonshiny")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(regen_ai_scope = "all")
    selected_ug_rv(c("1"))
    regen_ai_hist(list())                        # pas de conseil
    session$setInputs(regen_comment_insert_ai = 1)
    expect_length(regen_comments_rv(), 0L)
  })
})

test_that("insertion conseil IA : sans sélection -> aucun commentaire écrit", {
  skip_if_not_installed("shiny"); skip_if_not_installed("sf")
  as <- shiny::reactiveValues(current_project = list(path = withr::local_tempdir()))
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    .package = "nemetonshiny")
  shiny::testServer(nemetonshiny:::mod_regeneration_server, args = list(app_state = as), {
    session$setInputs(regen_ai_scope = "all")
    selected_ug_rv(character())
    regen_ai_hist(list(list(q = "", a = "X")))
    session$setInputs(regen_comment_insert_ai = 1)
    expect_length(regen_comments_rv(), 0L)
  })
})

# Mission 043 - reGeneration : un seul moteur vivant par projet.
# Le coeur (marqueur de run, capped_run_alive, stop_memory_capped) n'est pas
# encore tague : tout est teste via les enveloppes de l'app, mockees.

test_that(".regen_marker_path vise cache/regeneration/engine_run.json", {
  expect_identical(
    nemetonshiny:::.regen_marker_path("/p"),
    file.path("/p", "cache", "regeneration", "engine_run.json"))
})

test_that("marqueur vivant : arret AVANT l'effacement d'engine_status.json, puis notification", {
  proj <- withr::local_tempdir()
  dir.create(file.path(proj, "cache", "regeneration"), recursive = TRUE)
  st <- file.path(proj, "cache", "regeneration", "engine_status.json")
  writeLines("{}", st)
  trace <- character()
  vu <- new.env()
  vu$status_present_a_l_arret <- NA
  testthat::local_mocked_bindings(
    .regen_marker_alive = function(marker) {
      trace <<- c(trace, "alive"); list(pid = 1L)
    },
    .regen_marker_stop = function(marker) {
      trace <<- c(trace, "stop")
      vu$status_present_a_l_arret <- file.exists(st)
      vu$marker <- marker
      TRUE
    },
    .package = "nemetonshiny")

  nemetonshiny:::.regen_preparer_lancement(
    proj, notifier = function() trace <<- c(trace, "notif"))

  expect_identical(trace, c("alive", "stop", "notif"))
  expect_true(vu$status_present_a_l_arret)          # l'arret precede unlink()
  expect_identical(vu$marker, nemetonshiny:::.regen_marker_path(proj))
  expect_false(file.exists(st))                     # puis le statut perime est efface
})

test_that("marqueur absent ou perime : aucun arret, pas de notification, statut efface", {
  proj <- withr::local_tempdir()
  dir.create(file.path(proj, "cache", "regeneration"), recursive = TRUE)
  st <- file.path(proj, "cache", "regeneration", "engine_status.json")
  writeLines("{}", st)
  appels <- 0L
  notifs <- 0L
  testthat::local_mocked_bindings(
    .regen_marker_alive = function(marker) NULL,
    .regen_marker_stop = function(marker) { appels <<- appels + 1L; TRUE },
    .package = "nemetonshiny")

  nemetonshiny:::.regen_preparer_lancement(
    proj, notifier = function() notifs <<- notifs + 1L)

  expect_identical(appels, 0L)
  expect_identical(notifs, 0L)
  expect_false(file.exists(st))
})

test_that("coeur sans capped_run_alive/stop_memory_capped : comportement actuel, sans erreur", {
  testthat::local_mocked_bindings(.regen_core_fn = function(nom) NULL,
                                  .package = "nemetonshiny")
  expect_null(nemetonshiny:::.regen_marker_alive("/m"))
  expect_false(nemetonshiny:::.regen_marker_stop("/m"))
  proj <- withr::local_tempdir()
  expect_no_error(nemetonshiny:::.regen_preparer_lancement(proj))
})

test_that("les enveloppes du coeur ne levent jamais et rendent ce que le coeur rend", {
  testthat::local_mocked_bindings(
    .regen_core_fn = function(nom) function(marker) stop("boom"),
    .package = "nemetonshiny")
  expect_null(nemetonshiny:::.regen_marker_alive("/m"))
  expect_false(nemetonshiny:::.regen_marker_stop("/m"))
  testthat::local_mocked_bindings(
    .regen_core_fn = function(nom) function(marker) list(pid = 2L),
    .package = "nemetonshiny")
  expect_identical(nemetonshiny:::.regen_marker_alive("/m")$pid, 2L)
})

test_that("arret demande : moteur arrete, echec voulu seulement si un moteur tourne ici", {
  vus <- character()
  arreter <- function(marker) { vus <<- c(vus, marker); TRUE }
  expect_true(nemetonshiny:::.regen_arret_demande("/m", TRUE, arreter = arreter))
  expect_identical(vus, "/m")
  # Aucun moteur dans ce module : arret tente, mais pas de drapeau d'arret voulu.
  expect_false(nemetonshiny:::.regen_arret_demande("/m2", FALSE, arreter = arreter))
  expect_identical(vus, c("/m", "/m2"))
  # Rien n'a ete arrete (coeur ancien, marqueur perime) : pas d'arret voulu.
  expect_false(nemetonshiny:::.regen_arret_demande(
    "/m3", TRUE, arreter = function(marker) FALSE))
  # Pas de marqueur : rien a arreter.
  expect_false(nemetonshiny:::.regen_arret_demande(NULL, TRUE, arreter = arreter))
  expect_length(vus, 2L)
})

test_that("fin de session : arret du moteur memorise, rien sans marqueur", {
  vus <- character()
  arreter <- function(marker) { vus <<- c(vus, marker); TRUE }
  expect_true(nemetonshiny:::.regen_arreter_fin_session("/m", arreter = arreter))
  expect_identical(vus, "/m")
  expect_false(nemetonshiny:::.regen_arreter_fin_session(NULL, arreter = arreter))
  expect_false(nemetonshiny:::.regen_arreter_fin_session("", arreter = arreter))
  expect_identical(vus, "/m")
  # Best-effort : une erreur a l'arret ne remonte pas.
  expect_no_error(nemetonshiny:::.regen_arreter_fin_session(
    "/m", arreter = function(marker) stop("boom")))
})

test_that("la cle i18n du moteur precedent existe en FR et EN, <= 30 caracteres", {
  expect_lte(nchar("regen_moteur_precedent_arrete"), 30L)
  expect_match(nemetonshiny:::get_i18n("fr")$t("regen_moteur_precedent_arrete"),
               "arrêté")
  expect_match(nemetonshiny:::get_i18n("en")$t("regen_moteur_precedent_arrete"),
               "stopped")
})

test_that(".regen_run_engine_capped passe marker_path quand le coeur le sait", {
  seen <- new.env()
  proj <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    run_regeneration_engine = function(units, project_path, cfg) list(direct = TRUE),
    .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    run_memory_capped = function(fun, args = list(), package = "nemeton",
                                 options = NULL, quiet = FALSE,
                                 marker_path = NULL, ...) {
      seen$marker_path <- marker_path
      list(capped = TRUE)
    },
    .package = "nemeton")
  nemetonshiny:::.regen_run_engine_capped("U", proj, list(), list())
  expect_identical(seen$marker_path, nemetonshiny:::.regen_marker_path(proj))
  expect_true(dir.exists(dirname(seen$marker_path)))
})

test_that(".regen_run_engine_capped ne passe PAS marker_path si le coeur l'ignore", {
  seen <- new.env()
  testthat::local_mocked_bindings(
    run_regeneration_engine = function(units, project_path, cfg) list(direct = TRUE),
    .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    run_memory_capped = function(fun, args = list(), package = "nemeton",
                                 options = NULL, quiet = FALSE, ...) {
      seen$noms <- names(list(...))
      list(capped = TRUE)
    },
    .package = "nemeton")
  out <- nemetonshiny:::.regen_run_engine_capped(
    "U", withr::local_tempdir(), list(), list())
  expect_true(isTRUE(out$capped))
  expect_false("marker_path" %in% seen$noms)
})

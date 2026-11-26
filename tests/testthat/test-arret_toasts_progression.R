# Mission 072 : l'arret d'un calcul retire les toasts de progression, arrete
# leur chrono, ne les laisse pas reapparaitre, et tue le moteur Python.

.session_factice <- list(ns = function(id) paste0("monitoring-", id))

test_that(".retirer_toasts_progression retire les toasts persistants du mode demande", {
  retires <- character(0)
  testthat::local_mocked_bindings(
    removeNotification = function(id, ...) { retires <<- c(retires, id); invisible(NULL) },
    .package = "shiny")

  nemetonshiny:::.retirer_toasts_progression(.session_factice, "reconfort")
  expect_true("monitoring-reconfort_progress" %in% retires)
  expect_false("monitoring-ingest_progress" %in% retires)

  retires <- character(0)
  nemetonshiny:::.retirer_toasts_progression(.session_factice, "fast")
  expect_true(all(c("monitoring-ingest_progress", "monitoring-fast_prewarm_progress") %in% retires))

  retires <- character(0)
  nemetonshiny:::.retirer_toasts_progression(.session_factice)   # tous les modes
  expect_true(all(c("monitoring-ingest_progress", "monitoring-fordead_progress",
                    "monitoring-reconfort_progress") %in% retires))
})

test_that("un evenement ingest_item tardif, apres l'arret, ne repose ni toast ni chrono", {
  posees <- 0L
  msg <- "intact"
  testthat::local_mocked_bindings(
    showNotification = function(...) { posees <<- posees + 1L; invisible(NULL) },
    removeNotification = function(...) invisible(NULL),
    .package = "shiny")
  i18n <- nemetonshiny:::get_i18n("fr")
  ev <- list(current = "reconfort:ingest_item", completed = 64L, total = 269L,
             step = "download", item_date = "2026-04-01")

  # Run arrete : est_actif() == FALSE.
  nemetonshiny:::.reconfort_handle_progress_event(
    ev, .session_factice, i18n, start = Sys.time(),
    on_msg = function(x) msg <<- x, est_actif = function() FALSE)
  expect_identical(posees, 0L)
  expect_identical(msg, "intact")

  # Run vivant : le toast est pose et le chrono nourri (comportement inchange).
  nemetonshiny:::.reconfort_handle_progress_event(
    ev, .session_factice, i18n, start = Sys.time(),
    on_msg = function(x) msg <<- x, est_actif = function() TRUE)
  expect_identical(posees, 1L)
  expect_match(msg, "64", fixed = TRUE)

  # Sans `est_actif` (appelants anciens) : inchange.
  nemetonshiny:::.reconfort_handle_progress_event(
    ev, .session_factice, i18n, start = Sys.time(), on_msg = function(x) NULL)
  expect_identical(posees, 2L)
})

test_that("reconfort:complete reste traite meme quand le run n'est plus actif", {
  vues <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, id = NULL, ...) { vues <<- c(vues, id); invisible(NULL) },
    removeNotification = function(...) invisible(NULL),
    .package = "shiny")
  nemetonshiny:::.reconfort_handle_progress_event(
    list(current = "reconfort:complete", n_alerts = 0L, duration_sec = 5),
    .session_factice, nemetonshiny:::get_i18n("fr"),
    on_msg = function(x) NULL, est_actif = function() FALSE)
  expect_true("monitoring-reconfort_complete" %in% vues)
})

test_that("le verrou du dossier RECONFORT donne un message clair, sans pile d'appels", {
  i18n <- nemetonshiny:::get_i18n("fr")
  brut <- paste(
    "run_reconfort_dieback failed in its capped child process (exit 1).",
    "> Error in .reconfort_lock_workdir(workdir) :",
    "> The RECONFORT working directory /tmp/x is in use by another process (PID 4242).")
  expect_true(nemetonshiny:::.monitoring_is_verrou_reconfort(brut))
  expect_false(nemetonshiny:::.monitoring_is_verrou_reconfort("boum"))

  rendu <- nemetonshiny:::.monitoring_run_error_msg(brut, i18n, "monitoring_reconfort_error")
  expect_identical(rendu, i18n$t("monitoring_reconfort_verrou"))
  expect_false(grepl("capped child|lock_workdir|PID", rendu))
  expect_match(nemetonshiny:::get_i18n("en")$t("monitoring_reconfort_verrou"),
               "still running", fixed = TRUE)
  # Une autre erreur garde son titre et son message.
  expect_match(nemetonshiny:::.monitoring_run_error_msg("boum", i18n, "monitoring_reconfort_error"),
               "boum", fixed = TRUE)
})

test_that("les cles i18n de la mission respectent 30 caracteres et existent FR/EN", {
  expect_lte(nchar("monitoring_reconfort_verrou"), 30L)
  for (l in c("fr", "en")) {
    expect_true(nemetonshiny:::get_i18n(l)$has("monitoring_reconfort_verrou"))
  }
})

test_that("run_with_python: l'arret de l'utilisateur tue l'enfant en quelques secondes", {
  skip_if_not_installed("callr")
  py <- Sys.which("python3")
  skip_if(!nzchar(py), "python3 absent")
  withr::local_options(nemetonshiny.opencanopy_python = unname(py))

  pid_file <- withr::local_tempfile(fileext = ".pid")
  enfant <- function(pid_file) {
    writeLines(as.character(Sys.getpid()), pid_file)
    Sys.sleep(120)
  }
  t0 <- Sys.time()
  declenche <- FALSE
  err <- tryCatch(
    nemetonshiny:::run_with_python(
      "opencanopy", enfant, args = list(pid_file = pid_file), poll_ms = 100L,
      annule = function() {
        # l'utilisateur arrete une fois l'enfant demarre
        declenche <<- file.exists(pid_file)
        declenche
      }),
    error = function(e) e)
  duree <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_s3_class(err, "nemeton_arret_utilisateur")
  expect_match(conditionMessage(err), "arr\u00eat\u00e9 par l'utilisateur", fixed = TRUE)
  expect_lt(duree, 15)

  pid <- as.integer(readLines(pid_file, n = 1L))
  Sys.sleep(0.5)
  expect_false(isTRUE(suppressWarnings(tools::pskill(pid, 0L))))   # processus mort
})

test_that("le verrou est reconnu meme quand cli replie le message a 80 colonnes", {
  # `cli.condition_width` est a Inf une fois le paquet charge : on le remet a 80,
  # largeur d'un processus R ordinaire (l'enfant plafonne du coeur).
  withr::local_options(cli.width = 80, cli.condition_width = 80, cli.num_colors = 1)
  i18n <- nemetonshiny:::get_i18n("fr")
  replies <- 0L
  for (n in seq(20L, 70L, by = 5L)) {
    workdir <- file.path("/tmp", strrep("d", n), "reconfort_workdir")
    err <- tryCatch(
      cli::cli_abort(
        "The RECONFORT working directory {.path {workdir}} is in use by another process (PID {4242L})."),
      error = function(e) e)
    # Le texte replie, tel que le journal de l'enfant le rapporte.
    brut <- paste("run_reconfort_dieback failed in its capped child process (exit 1).",
                  rlang::cnd_message(err, prefix = FALSE))
    if (!grepl("in use by another process", brut, fixed = TRUE)) replies <- replies + 1L
    expect_true(nemetonshiny:::.monitoring_is_verrou_reconfort(brut), info = n)
    expect_identical(
      nemetonshiny:::.monitoring_run_error_msg(brut, i18n, "monitoring_reconfort_error"),
      i18n$t("monitoring_reconfort_verrou"), info = n)
  }
  # Garde-fou : cli a bien coupe la phrase pour au moins une longueur de chemin.
  expect_gt(replies, 0L)
})

test_that(".run_opencanopy_chm releve l'arret de l'utilisateur, meme si son message dit 'not found'", {
  rejoue <- FALSE
  testthat::local_mocked_bindings(
    run_with_python = function(...) {
      cli::cli_abort("engine not found, is required", class = "nemeton_arret_utilisateur")
    },
    .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    getExportedValue = function(...) { rejoue <<- TRUE; stop("repli en processus") },
    .package = "base")
  expect_error(
    nemetonshiny:::.run_opencanopy_chm("a.gpkg", tempdir(), tempfile(), function(...) NULL,
                                       annule = function() TRUE),
    class = "nemeton_arret_utilisateur")
  expect_false(rejoue)
})

# --- testServer : signal d'arret partage ------------------------------------
.fake_task_072 <- function() {
  list(invoke = function(...) invisible(NULL), result = function() NULL,
       status = function() "initial")
}

.serveur_072 <- function(corps_072) {
  notes <- list(); retirees <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, id = NULL, ...) { notes[[length(notes) + 1L]] <<- id; invisible(id) },
    removeNotification = function(id, ...) { retirees <<- c(retirees, id); invisible(NULL) },
    .package = "shiny")
  app_state <- shiny::reactiveValues(language = "fr", current_project = NULL)
  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) NULL,
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() .fake_task_072(),
    run_fordead_async              = function() .fake_task_072(),
    run_reconfort_async            = function() .fake_task_072(),
    .resolve_progress_path         = function(...) NULL,
    shiny::testServer(mod_monitoring_server, args = list(app_state = app_state), {
      session$flushReact()
      corps_072(list(reconfort_run_start = reconfort_run_start,
                     reconfort_run_msg = reconfort_run_msg,
                     reconfort_progress_path = reconfort_progress_path),
                session, app_state, function() unlist(notes), function() retirees)
    })
  )
  invisible(NULL)
}

test_that("arret d'un run RECONFORT vivant : fichier lache, toasts retires, calcul_arrete pose", {
  skip_if_not_installed("shiny")
  pf <- withr::local_tempfile(fileext = ".json")
  writeLines('{"current":"reconfort:ingest_item"}', pf)
  .serveur_072(function(env, session, app_state, notes, retirees) {
    env$reconfort_run_start(Sys.time()); env$reconfort_run_msg("Scene 64/269")
    env$reconfort_progress_path(pf)
    session$flushReact()

    app_state$cancel_computation <- Sys.time()
    session$flushReact()

    expect_null(env$reconfort_progress_path())
    expect_false(file.exists(pf))
    expect_null(env$reconfort_run_start())
    expect_null(env$reconfort_run_msg())
    expect_true(any(grepl("reconfort_progress$", retirees())))
    expect_true("calcul_arrete" %in% notes())
  })
})

test_that("arret sans run actif : pas de toast calcul_arrete", {
  skip_if_not_installed("shiny")
  .serveur_072(function(env, session, app_state, notes, retirees) {
    app_state$cancel_computation <- Sys.time()
    session$flushReact()
    expect_false("calcul_arrete" %in% notes())
  })
})

test_that("arret puis relance : un evenement ingest_item repose le toast", {
  skip_if_not_installed("shiny")
  pf <- withr::local_tempfile(fileext = ".json")
  .serveur_072(function(env, session, app_state, notes, retirees) {
    env$reconfort_run_start(Sys.time()); env$reconfort_run_msg("x")
    env$reconfort_progress_path(pf)
    app_state$cancel_computation <- Sys.time()
    session$flushReact()
    expect_null(env$reconfort_run_start())

    # Relance : nouveau run, nouveau fichier de progression, nouvel evenement.
    env$reconfort_run_start(Sys.time())
    jsonlite::write_json(list(current = "reconfort:ingest_item", completed = 3L,
                              total = 10L, step = "download", item_date = "2026-04-01"),
                         pf, auto_unbox = TRUE)
    env$reconfort_progress_path(pf)
    session$elapse(1500)
    session$flushReact()
    expect_true(any(grepl("reconfort_progress$", notes())))
  })
})

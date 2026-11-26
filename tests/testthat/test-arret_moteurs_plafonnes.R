# Mission 075 : l'arret tue VRAIMENT les moteurs plafonnes RECONFORT et FORDEAD
# (marqueur de run du coeur : marker_path + stop_memory_capped), comme reGeneration.

# --- Chemin du marqueur -------------------------------------------------------

test_that("le marqueur est propre au moteur et au projet", {
  p <- withr::local_tempdir()
  rc <- nemetonshiny:::.moteur_sante_marker_path(p, "reconfort")
  fd <- nemetonshiny:::.moteur_sante_marker_path(p, "fordead")
  expect_identical(rc, file.path(p, "cache", "layers", "reconfort", "engine_run.json"))
  expect_identical(fd, file.path(p, "cache", "layers", "fordead", "engine_run.json"))
  expect_false(dir.exists(dirname(rc)))                     # sans `creer` : rien n'est cree
  nemetonshiny:::.moteur_sante_marker_path(p, "reconfort", creer = TRUE)
  expect_true(dir.exists(dirname(rc)))
  expect_null(nemetonshiny:::.moteur_sante_marker_path(NULL, "fordead"))
  expect_error(nemetonshiny:::.moteur_sante_marker_path(p, "fast"))
})

# --- Lancement : marker_path transmis (ou retire sur un coeur ancien) ----------

test_that(".run_capped transmet marker_path quand le coeur le connait", {
  vus <- NULL
  testthat::local_mocked_bindings(
    run_memory_capped = function(fun, args = list(), ...) { vus <<- list(...); "ok" },
    .package = "nemeton")
  testthat::local_mocked_bindings(.capped_accepts_marker_path = function() TRUE)
  nemetonshiny:::.run_capped(fun = "f", args = list(), marker_path = "/m/engine_run.json")
  expect_identical(vus$marker_path, "/m/engine_run.json")
})

test_that(".run_capped retire marker_path quand le coeur ne le connait pas", {
  vus <- NULL
  testthat::local_mocked_bindings(
    run_memory_capped = function(fun, args = list(), ...) { vus <<- names(list(...)); "ok" },
    .package = "nemeton")
  testthat::local_mocked_bindings(.capped_accepts_marker_path = function() FALSE)
  expect_identical(
    nemetonshiny:::.run_capped(fun = "f", args = list(), marker_path = "/m.json"), "ok")
  expect_false("marker_path" %in% vus)
})

test_that("les deux moteurs passent marker_path a .run_capped et au worker", {
  src <- paste(readLines(testthat::test_path("../../R/service_monitoring.R"),
                         encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl('fun = "run_reconfort_dieback",\\s*log_path = child_log,\\s*marker_path = marker_path', src))
  expect_true(grepl('fun = "run_fordead_dieback",\\s*log_path = child_log,\\s*marker_path = marker_path', src))
  expect_length(gregexpr("marker_path = NULL,", src, fixed = TRUE)[[1]], 2L)
  mod <- paste(readLines(testthat::test_path("../../R/mod_monitoring.R"),
                         encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl('\\.moteur_sante_marker_path\\(proj\\$path, "reconfort"', mod))
  expect_true(grepl('"fordead",\\s*creer = TRUE', mod))
})

# --- Arret : stop_memory_capped appele avec le bon marqueur --------------------

test_that("l'arret appelle stop_memory_capped avec le marqueur du moteur vivant", {
  p <- withr::local_tempdir()
  rc <- nemetonshiny:::.moteur_sante_marker_path(p, "reconfort")
  fd <- nemetonshiny:::.moteur_sante_marker_path(p, "fordead")
  arretes <- character(0)
  res <- nemetonshiny:::.moteurs_sante_arreter(
    p, "reconfort",
    alive   = function(m) if (identical(m, rc)) list(pid = 1L) else NULL,
    arreter = function(m) { arretes <<- c(arretes, m); TRUE })
  expect_identical(res, "reconfort")
  expect_identical(arretes, rc)

  arretes <- character(0)
  res <- nemetonshiny:::.moteurs_sante_arreter(
    p, "fordead",
    alive   = function(m) list(pid = 2L),
    arreter = function(m) { arretes <<- c(arretes, m); TRUE })
  expect_identical(res, "fordead")
  expect_identical(arretes, fd)
})

test_that("arret sans run vivant : rien n'est appele", {
  p <- withr::local_tempdir()
  appels <- 0L
  res <- nemetonshiny:::.moteurs_sante_arreter(
    p, alive = function(m) NULL,
    arreter = function(m) { appels <<- appels + 1L; TRUE })
  expect_identical(res, character(0))
  expect_identical(appels, 0L)
  expect_identical(
    nemetonshiny:::.moteurs_sante_arreter(NULL, alive = function(m) list(pid = 1L),
                                          arreter = function(m) stop("jamais")),
    character(0))
})

test_that("les enveloppes du coeur ne levent jamais et reGeneration reste inchangee", {
  testthat::local_mocked_bindings(.capped_core_fn = function(nom) NULL)
  expect_null(nemetonshiny:::.capped_marker_alive("/m"))
  expect_false(nemetonshiny:::.capped_marker_stop("/m"))
  expect_null(nemetonshiny:::.regen_marker_alive("/m"))
  expect_false(nemetonshiny:::.regen_marker_stop("/m"))
})

# --- Module : arret et relance --------------------------------------------------

.fake_task_075 <- function(status = "initial") {
  st <- new.env(parent = emptyenv()); st$calls <- list()
  list(invoke = function(...) { st$calls[[length(st$calls) + 1L]] <- list(...); invisible(NULL) },
       result = function() NULL, status = function() status,
       .calls = function() st$calls)
}

.run_monitoring_075 <- function(reconfort = .fake_task_075(), fordead = .fake_task_075(),
                                expr, project, vivants = character(0)) {
  notes <- character(0); stops <- list(); morts <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) { notes <<- c(notes, paste(as.character(ui), collapse = " ")); invisible("id") },
    .package = "shiny")
  testthat::local_mocked_bindings(
    reconfort_year_bounds = function(v_model = "v3", ...) list(min = 2016L, max = 2025L, default = 2025L),
    .package = "nemeton")
  app_state <- shiny::reactiveValues(language = "fr", current_project = project)
  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) NULL,
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_ingestion_async            = function() .fake_task_075(),
    run_fordead_async              = function() fordead,
    run_reconfort_async            = function() reconfort,
    .capped_marker_alive = function(marker, ...) {
      # Un moteur tue ne vit plus (le signal partage rappelle l'arret).
      if (marker %in% morts) return(NULL)
      if (any(vapply(vivants, function(m) grepl(m, marker, fixed = TRUE), TRUE))) list(pid = 9L) else NULL
    },
    .capped_marker_stop = function(marker, ...) { stops[[length(stops) + 1L]] <<- marker; morts <<- c(morts, marker); TRUE },
    shiny::testServer(mod_monitoring_server, args = list(app_state = app_state), {
      session$flushReact()
      expr(session, input, app_state, environment())
    })
  )
  list(notes = notes, stops = stops)
}

test_that("le bouton d'arret RECONFORT tue l'enfant et pose toujours le drapeau cooperatif", {
  p <- withr::local_tempdir()
  r <- .run_monitoring_075(
    project = list(id = "p", path = p, metadata = list(name = "P")),
    vivants = "reconfort",
    expr = function(session, input, app_state, env) session$setInputs(run_reconfort_cancel = 1L))
  expect_length(r$stops, 1L)
  expect_identical(r$stops[[1]], nemetonshiny:::.moteur_sante_marker_path(p, "reconfort"))
  expect_true(file.exists(file.path(p, "data", "reconfort_cancel.flag")))
})

test_that("le bouton d'arret FORDEAD tue l'enfant FORDEAD seul", {
  p <- withr::local_tempdir()
  r <- .run_monitoring_075(
    project = list(id = "p", path = p, metadata = list(name = "P")),
    vivants = "fordead",
    expr = function(session, input, app_state, env) session$setInputs(run_health_cancel = 1L))
  expect_length(r$stops, 1L)
  expect_identical(r$stops[[1]], nemetonshiny:::.moteur_sante_marker_path(p, "fordead"))
  expect_true(file.exists(file.path(p, "data", "fordead_cancel.flag")))
})

test_that("l'arret sans moteur vivant n'appelle pas stop_memory_capped", {
  p <- withr::local_tempdir()
  r <- .run_monitoring_075(
    project = list(id = "p", path = p, metadata = list(name = "P")),
    expr = function(session, input, app_state, env) {
      session$setInputs(run_reconfort_cancel = 1L)
      session$setInputs(run_health_cancel = 1L)
    })
  expect_length(r$stops, 0L)
})

test_that("le signal d'arret partage (Tout calculer) tue les deux moteurs", {
  p <- withr::local_tempdir()
  r <- .run_monitoring_075(
    project = list(id = "p", path = p, metadata = list(name = "P")),
    vivants = c("reconfort", "fordead"),
    expr = function(session, input, app_state, env) {
      app_state$cancel_computation <- Sys.time()
      session$flushReact()
    })
  expect_length(r$stops, 2L)
})

test_that("RECONFORT : pas de second lancement tant qu'un moteur vit", {
  p <- withr::local_tempdir()
  i18n <- get_i18n("fr")
  rec <- .fake_task_075()
  r <- .run_monitoring_075(
    reconfort = rec, vivants = "reconfort",
    project = list(id = "p", path = p, metadata = list(name = "P")),
    expr = function(session, input, app_state, env)
      session$setInputs(zone_id = "1", reconfort_s2_year = 2025L, run_reconfort = 1L))
  expect_length(rec$.calls(), 0L)
  expect_true(any(grepl(i18n$t("monitoring_reconfort_verrou"), r$notes, fixed = TRUE)))
  expect_true(any(grepl("arreter_moteur_reconfort", r$notes, fixed = TRUE)))  # l'option d'arreter
})

test_that("RECONFORT : le lancement transmet le marqueur du moteur", {
  p <- withr::local_tempdir()
  rec <- .fake_task_075()
  .run_monitoring_075(
    reconfort = rec,
    project = list(id = "p", path = p, metadata = list(name = "P")),
    expr = function(session, input, app_state, env) {
      session$setInputs(zone_id = "1", reconfort_s2_year = 2025L)
      get(".invoke_reconfort", envir = env)(zone_id = 1L)
    })
  expect_length(rec$.calls(), 1L)
  expect_identical(rec$.calls()[[1]]$marker_path,
                   nemetonshiny:::.moteur_sante_marker_path(p, "reconfort"))
  expect_true(dir.exists(file.path(p, "cache", "layers", "reconfort")))
})

test_that("FORDEAD : pas de second lancement tant qu'un moteur vit", {
  p <- withr::local_tempdir()
  i18n <- get_i18n("fr")
  fd <- .fake_task_075()
  r <- .run_monitoring_075(
    fordead = fd, vivants = "fordead",
    project = list(id = "p", path = p, metadata = list(name = "P")),
    expr = function(session, input, app_state, env) {
      session$setInputs(zone_id = "1")
      get(".invoke_fordead", envir = env)(zone_id = 1L)
    })
  expect_length(fd$.calls(), 0L)
  expect_true(any(grepl(i18n$t("monitoring_fordead_verrou"), r$notes, fixed = TRUE)))
  expect_true(any(grepl("arreter_moteur_fordead", r$notes, fixed = TRUE)))
})

test_that("le lien \" Arreter ce calcul \" tue le moteur vivant", {
  p <- withr::local_tempdir()
  r <- .run_monitoring_075(
    project = list(id = "p", path = p, metadata = list(name = "P")),
    vivants = "fordead",
    expr = function(session, input, app_state, env)
      session$setInputs(arreter_moteur_fordead = 1L))
  expect_length(r$stops, 1L)
  expect_identical(r$stops[[1]], nemetonshiny:::.moteur_sante_marker_path(p, "fordead"))
})

# --- Verrou perime repris par le coeur ------------------------------------------

test_that("le verrou RECONFORT d'un enfant tue est perime et repris par le coeur", {
  skip_if_not(is.function(tryCatch(getFromNamespace(".reconfort_lock_workdir", "nemeton"),
                                   error = function(e) NULL)),
              ".reconfort_lock_workdir absent du coeur installe")
  skip_if_not(identical(.Platform$OS.type, "unix"))
  skip_if_not_installed("processx")
  wd <- withr::local_tempdir()
  # Un PID certainement mort : un processus lance puis tue.
  px <- processx::process$new("sleep", "60")
  pid <- px$get_pid(); px$kill(); px$wait()
  writeLines(as.character(pid), file.path(wd, ".lock"))
  lock <- getFromNamespace(".reconfort_lock_workdir", "nemeton")(wd)
  expect_identical(readLines(lock, n = 1L), as.character(Sys.getpid()))
  # Contre-epreuve : un verrou tenu par un processus VIVANT bloque encore.
  vivant <- processx::process$new("sleep", "60")
  withr::defer(vivant$kill())
  wd2 <- withr::local_tempdir()
  writeLines(as.character(vivant$get_pid()), file.path(wd2, ".lock"))
  expect_error(getFromNamespace(".reconfort_lock_workdir", "nemeton")(wd2),
               "in use by another process")
})

# --- Vrai processus --------------------------------------------------------------

test_that("un vrai run_memory_capped est tue par le chemin d'arret de l'app", {
  skip_if_not(identical(.Platform$OS.type, "unix"))
  skip_if_not_installed("callr")
  skip_if_not("marker_path" %in% names(formals(nemeton::run_memory_capped)),
              "coeur sans marker_path")
  p <- withr::local_tempdir()
  mp <- nemetonshiny:::.moteur_sante_marker_path(p, "reconfort", creer = TRUE)
  bg <- callr::r_bg(function(m) {
    nemeton::run_memory_capped(fun = "Sys.sleep", args = list(time = 120),
                               package = "base", marker_path = m, quiet = TRUE)
  }, args = list(mp))
  withr::defer(if (bg$is_alive()) bg$kill())
  t0 <- Sys.time()
  while (!file.exists(mp) && difftime(Sys.time(), t0, units = "secs") < 60) Sys.sleep(0.2)
  skip_if_not(file.exists(mp), "l'enfant plafonne n'a pas pose son marqueur")
  expect_false(is.null(nemetonshiny:::.moteur_sante_vivant(p, "reconfort")))

  t1 <- Sys.time()
  arretes <- nemetonshiny:::.moteurs_sante_arreter(p, "reconfort")
  bg$wait(20000)
  expect_identical(arretes, "reconfort")
  expect_false(bg$is_alive())
  expect_lt(as.numeric(difftime(Sys.time(), t1, units = "secs")), 20)
  expect_null(nemetonshiny:::.moteur_sante_vivant(p, "reconfort"))   # plus de marqueur
})

# --- Timeout court de l'arret Sante ----------------------------------------------

test_that("l'arret Sante passe timeout = 3 si le coeur l'accepte, reGeneration non", {
  vu <- NULL
  testthat::local_mocked_bindings(
    .capped_core_fn = function(nom) function(marker, timeout = 10) { vu <<- timeout; TRUE })
  expect_true(nemetonshiny:::.sante_marker_stop("/m"))
  expect_identical(vu, 3)
  expect_true(nemetonshiny:::.regen_marker_stop("/m"))
  expect_identical(vu, 10)                       # defaut du coeur, inchange
})

test_that("l'arret Sante ne passe pas timeout a un coeur qui ne l'a pas", {
  testthat::local_mocked_bindings(
    .capped_core_fn = function(nom) function(marker) TRUE)
  expect_true(nemetonshiny:::.sante_marker_stop("/m"))
})

# --- Erreur de sortie de l'enfant tue : masquee seulement apres arret dur --------

.task_erreur_075 <- function() {
  rv <- shiny::reactiveVal(NULL)
  list(invoke = function(...) invisible(NULL),
       result = function() { e <- rv(); if (!is.null(e)) stop(e); NULL },
       status = function() "initial", erreur = function(m) rv(m),
       .calls = function() list())
}

for (cas in list(
  list(moteur = "reconfort", bouton = "run_reconfort_cancel", arg = "reconfort", cle = "monitoring_reconfort_error"),
  list(moteur = "fordead",   bouton = "run_health_cancel",     arg = "fordead",   cle = "monitoring_health_error"))) {
  local({
    cas <- cas
    mk <- function(tache) if (cas$arg == "reconfort") list(reconfort = tache) else list(fordead = tache)
    killed <- "\"run_x\" was killed (signal 15; systemd's verdict unavailable)."

    test_that(paste(cas$moteur, ": erreur killed apres arret dur effectif -> pas de toast d'erreur"), {
      p <- withr::local_tempdir(); i18n <- get_i18n("fr")
      t <- .task_erreur_075()
      r <- do.call(.run_monitoring_075, c(mk(t), list(
        project = list(id = "p", path = p, metadata = list(name = "P")),
        vivants = cas$moteur,
        expr = function(session, input, app_state, env) {
          session$setInputs(zone_id = "1", reconfort_s2_year = 2025L)
          do.call(session$setInputs, setNames(list(1L), cas$bouton))
          t$erreur(killed)
          session$flushReact()
        })))
      expect_length(r$stops, 1L)
      expect_false(any(grepl(i18n$t("monitoring_error_oom_short"), r$notes, fixed = TRUE)))
      expect_false(any(grepl(i18n$t(cas$cle), r$notes, fixed = TRUE)))
    })

    test_that(paste(cas$moteur, ": erreur OOM sans arret -> toast present"), {
      p <- withr::local_tempdir(); i18n <- get_i18n("fr")
      t <- .task_erreur_075()
      r <- do.call(.run_monitoring_075, c(mk(t), list(
        project = list(id = "p", path = p, metadata = list(name = "P")),
        expr = function(session, input, app_state, env) {
          t$erreur("child failed (exit 137)")
          session$flushReact()
        })))
      expect_length(r$stops, 0L)
      expect_true(any(grepl(i18n$t("monitoring_error_oom_short"), r$notes, fixed = TRUE)))
    })
  })
}

test_that("apres l'arret dur le bouton est de nouveau lancable (RECONFORT, garde FORDEAD levee)", {
  p <- withr::local_tempdir()
  rec <- .fake_task_075(); fd <- .fake_task_075()
  r <- .run_monitoring_075(
    reconfort = rec, fordead = fd, vivants = c("reconfort", "fordead"),
    project = list(id = "p", path = p, metadata = list(name = "P")),
    expr = function(session, input, app_state, env) {
      session$setInputs(zone_id = "1", reconfort_s2_year = 2025L)
      session$setInputs(run_reconfort_cancel = 1L)
      session$setInputs(run_health_cancel = 1L)
      get(".invoke_reconfort", envir = env)(zone_id = 1L)
      get(".invoke_fordead", envir = env)(zone_id = 1L)
    })
  expect_length(r$stops, 2L)
  expect_length(rec$.calls(), 1L)
  # FORDEAD vise la zone `_tot` lue en base (absente ici) : le lancement ne part
  # pas, mais la garde du moteur vivant ne doit plus le refuser.
  expect_false(any(grepl(get_i18n("fr")$t("monitoring_fordead_verrou"), r$notes, fixed = TRUE)))
})

# test-compute_capped.R - le calcul des indicateurs ne doit plus emporter la session.
#
# INCIDENT 2026-08-15 : le calcul des 31 indicateurs sur un projet avec R5 a
# fait tuer RStudio par `systemd-oomd` (17,1 Go dans le scope, pression du user
# slice a 77,22 % pour une limite de 50 %). Le calcul tournait dans un worker
# `future` NU, donc DANS le scope de la session. FORDEAD et la reGeneration
# avaient deja ete isoles en enfant plafonne ; ce chemin-ci ne l'etait pas.

test_that("l'app ne decide plus du plafond : elle ne passe pas memory_max", {
  # Le plafond est une POLITIQUE, et elle appartient au coeur depuis
  # `nemeton 0.183.0` : 50 % de MemTotal, plancher 4 Go, avec
  # NEMETON_MEMORY_MAX et options(nemeton.memory_max=) par-dessus. L'app en
  # portait sa copie -- et trois chemins lourds de la meme session tournaient
  # sous TROIS plafonds differents (indicateurs 50 %, FORDEAD et reGeneration
  # 70 %). C'est la meme classe de defaut que le fork d'INDICATOR_FAMILIES.
  vu <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    run_memory_capped = function(fun, args, package, options, ...) {
      vu$dots <- list(...)
      "resultat"
    },
    .package = "nemeton")

  withr::with_envvar(c(NEMETON_MEMORY_MAX = "10G"), {
    expect_identical(
      nemetonshiny:::.compute_run_capped("p", list(language = "fr")),
      "resultat")
  })
  # Meme avec la variable posee : c'est le COEUR qui la lit, pas l'app.
  expect_false("memory_max" %in% names(vu$dots))
})

test_that("aucune fraction de RAM ne subsiste cote app", {
  # Le remede a trois plafonds n'est pas d'en choisir un meilleur ici : c'est
  # de n'en avoir aucun. Une fraction reintroduite en ferait un quatrieme.
  for (f in c("service_compute.R", "service_monitoring.R", "mod_regeneration.R")) {
    path <- testthat::test_path("..", "..", "R", f)
    testthat::skip_if_not(file.exists(path), "sources R absentes")
    code <- readLines(path, warn = FALSE)
    code <- code[!grepl("^\\s*#", code)]
    expect_false(any(grepl("MemTotal|/proc/meminfo", code)), info = f)
    expect_false(any(grepl("0\\.[57]\\s*\\*\\s*total", code)), info = f)
  }
})

test_that("le calcul part dans un enfant plafonne, pas dans le worker", {
  vu <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    run_memory_capped = function(fun, args, package, options, ...) {
      vu$fun <- fun; vu$package <- package; vu$args <- args
      vu$options <- options
      "resultat"
    },
    .package = "nemeton")

  out <- nemetonshiny:::.compute_run_capped("projet-1", list(language = "fr"))

  # La valeur de retour est RELAYEE : `compute_task$result()` la consomme.
  expect_identical(out, "resultat")
  expect_identical(vu$fun, "start_computation")
  expect_identical(vu$package, "nemetonshiny")
  expect_identical(vu$args$project_id, "projet-1")
  # Le progres passe par le disque, seul canal qui traverse un process enfant.
  expect_true(isTRUE(vu$args$use_file_progress))
  # Les options d'app suivent, sans quoi `get_project_path()` echoue dans l'enfant.
  expect_identical(vu$options$nemeton.app_options$language, "fr")
})

test_that("un coeur trop ancien fait retomber sur l'appel direct, sans casser", {
  appele <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    start_computation = function(...) { appele$oui <- TRUE; "direct" },
    .package = "nemetonshiny")
  # `run_memory_capped()` d'avant nemeton 0.158.0 : ni `package`, ni `options`,
  # donc incapable de lancer une fonction INTERNE de l'app. On perd la
  # protection, on ne casse pas l'application.
  testthat::local_mocked_bindings(
    run_memory_capped = function(fun, args, ...) "plafonne",
    .package = "nemeton")

  out <- nemetonshiny:::.compute_run_capped("projet-1", list(language = "fr"))
  expect_identical(out, "direct")
  expect_true(isTRUE(appele$oui))
})

test_that("l'option de repli force l'appel direct", {
  appele <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    start_computation = function(...) { appele$oui <- TRUE; "direct" },
    .package = "nemetonshiny")
  withr::local_options(nemetonshiny.compute_capped = FALSE)
  expect_identical(
    nemetonshiny:::.compute_run_capped("p", list(language = "fr")), "direct")
  expect_true(isTRUE(appele$oui))
})

test_that("le worker n'appelle plus start_computation en direct", {
  f <- testthat::test_path("..", "..", "R", "mod_home.R")
  testthat::skip_if_not(file.exists(f), "sources R absentes (package installe)")
  code <- readLines(f, warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  # C'est l'appel nu dans le worker qui mettait la memoire dans le scope de la
  # session. Il doit passer par l'enfant plafonne.
  expect_false(any(grepl("^\\s*start_computation\\(", code)))
  expect_true(any(grepl(".compute_run_capped", code, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Le message d'echec
#
# INCIDENT 2026-08-22 : 3 h 20 de CPU, 11 Go de cache, zero indicateur, et pour
# tout diagnostic « failed in its capped child process (exit -15) ». Le journal
# systeme disait la verite au meme instant : `run-r11dc...scope: Failed with
# result 'oom-kill'`. L'ecart vient de ce que processx surveille le CLIENT
# systemd-run, pas le R tue dans le scope : l'OOM (SIGKILL) devient un SIGTERM
# de demontage, et le coeur ne reconnait que -9/137.
# ---------------------------------------------------------------------------

test_that("un enfant tue est traduit en plafond memoire, pas en charabia", {
  i18n <- get_i18n("fr")
  for (st in c("-9", "-15", "137", "143")) {
    msg <- sprintf('"start_computation" failed in its capped child process (exit %s).', st)
    out <- as.character(nemetonshiny:::.compute_error_message(msg, i18n))
    expect_true(grepl("plafond", out), info = st)
    # Le remede est NOMME : sans lui, savoir que c'est la memoire ne sert a rien.
    expect_true(grepl("NEMETON_MEMORY_MAX", out, fixed = TRUE), info = st)
    # Et le code de sortie ne remonte pas a l'ecran : il ne dit rien a personne.
    expect_false(grepl("exit", out, fixed = TRUE), info = st)
  }
  # Le message que le coeur SAIT formuler (-9/137) est reconnu par son texte.
  out <- as.character(nemetonshiny:::.compute_error_message(
    "start_computation ran out of memory and was killed (ceiling: 10G).", i18n))
  expect_true(grepl("plafond", out))
})

test_that("une erreur ordinaire passe telle quelle, echappee et traduite", {
  i18n <- get_i18n("fr")
  out <- as.character(nemetonshiny:::.compute_error_message("objet 'x' introuvable", i18n))
  expect_true(grepl("Erreur de calcul", out, fixed = TRUE))
  expect_true(grepl("introuvable", out, fixed = TRUE))

  # Le message vient d'un moteur tiers : il ne doit pas pouvoir injecter du HTML.
  out <- as.character(nemetonshiny:::.compute_error_message("<script>x</script>", i18n))
  expect_false(grepl("<script>", out, fixed = TRUE))
})

test_that("le message d'echec est bilingue", {
  en <- as.character(nemetonshiny:::.compute_error_message(
    '"f" failed in its capped child process (exit -15).', get_i18n("en")))
  expect_true(grepl("memory ceiling", en, fixed = TRUE))
  expect_false(grepl("plafond", en))
})

test_that("le message francais en dur a quitte mod_home", {
  f <- testthat::test_path("..", "..", "R", "mod_home.R")
  testthat::skip_if_not(file.exists(f), "sources R absentes")
  code <- readLines(f, warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  expect_false(any(grepl('paste("Erreur de calcul:"', code, fixed = TRUE)))
  expect_true(any(grepl(".compute_error_message", code, fixed = TRUE)))
})

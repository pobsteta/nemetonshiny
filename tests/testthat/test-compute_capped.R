# test-compute_capped.R - le calcul des indicateurs ne doit plus emporter la session.
#
# INCIDENT 2026-08-15 : le calcul des 31 indicateurs sur un projet avec R5 a
# fait tuer RStudio par `systemd-oomd` (17,1 Go dans le scope, pression du user
# slice a 77,22 % pour une limite de 50 %). Le calcul tournait dans un worker
# `future` NU, donc DANS le scope de la session. FORDEAD et la reGeneration
# avaient deja ete isoles en enfant plafonne ; ce chemin-ci ne l'etait pas.

test_that("le plafond respecte NEMETON_MEMORY_MAX, dans les deux sens", {
  withr::with_envvar(c(NEMETON_MEMORY_MAX = "10G"), {
    expect_identical(nemetonshiny:::.compute_memory_max(), "10G")
  })
  # Echappatoire documentee : une execution legitime doit pouvoir reclamer
  # toute la place.
  for (off in c("none", "off", "FALSE", "0")) {
    withr::with_envvar(c(NEMETON_MEMORY_MAX = off), {
      expect_false(nemetonshiny:::.compute_memory_max(), info = off)
    })
  }
})

test_that("sans variable, le plafond est SOUS le seuil ou l'OS tue la session", {
  skip_if_not(file.exists("/proc/meminfo"), "hote non Linux")
  withr::with_envvar(c(NEMETON_MEMORY_MAX = ""), {
    mm <- nemetonshiny:::.compute_memory_max()
    skip_if(is.null(mm), "RAM de l'hote illisible")
    go <- as.numeric(sub("G$", "", mm))
    total_go <- nemetonshiny:::.total_memory_bytes() / 1024^3

    # Le defaut du coeur est 70 % de la RAM. Sur la machine de l'incident cela
    # faisait 21,7 Go, tres AU-DESSUS des 17,1 Go auxquels `systemd-oomd` avait
    # deja tue la session : un plafond qui ne se declenche qu'apres l'executeur
    # n'est pas un plafond. Le notre doit rester nettement en dessous.
    expect_lt(go, 0.7 * total_go)
    expect_lte(go, 0.5 * total_go)
    expect_gte(go, 4)          # plancher : sous 4 Go, plus rien ne passe
  })
})

test_that("le plafond ne depend pas de ce qui est libre a l'instant t", {
  skip_if_not(file.exists("/proc/meminfo"), "hote non Linux")
  # `MemTotal` et non `MemAvailable` : sinon la limite changerait selon le
  # nombre d'onglets ouverts, et deux executions du meme calcul n'auraient pas
  # la meme chance d'aboutir.
  a <- nemetonshiny:::.total_memory_bytes()
  b <- nemetonshiny:::.total_memory_bytes()
  expect_identical(a, b)
  expect_gt(a, 0)
})

test_that("le calcul part dans un enfant plafonne, pas dans le worker", {
  vu <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    run_memory_capped = function(fun, args, package, options, memory_max, ...) {
      vu$fun <- fun; vu$package <- package; vu$args <- args
      vu$options <- options; vu$memory_max <- memory_max
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
    run_memory_capped = function(fun, args, memory_max, ...) "plafonne",
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

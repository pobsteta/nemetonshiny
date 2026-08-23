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

test_that("une certitude du coeur n'est PAS re-attenuee", {
  # `nemeton 0.183.1` nomme le scope transitoire et demande son verdict a
  # systemd : quand il ecrit « ran out of memory », l'OOM est CONSTATE, pas
  # infere. Re-couvrir cela d'un « la cause habituelle est... » rendrait a
  # l'utilisateur une incertitude que le coeur venait de lever.
  for (lang in c("fr", "en")) {
    i18n <- get_i18n(lang)
    out <- as.character(nemetonshiny:::.compute_error_message(
      '"start_computation" ran out of memory and was killed (ceiling: 96M).', i18n))
    attenue <- if (lang == "fr") "habituelle" else "usual"
    expect_false(grepl(attenue, out), info = lang)
    # Le plafond REMONTE : c'est lui qu'il faut relever, le connaitre est utile.
    expect_true(grepl("96M", out, fixed = TRUE), info = lang)
    expect_true(grepl("NEMETON_MEMORY_MAX", out, fixed = TRUE), info = lang)
  }
})

test_that("un verdict indisponible reste au conditionnel", {
  # Sans cgroup ni systemctl, un scope arrete et un `kill` exterieur ont
  # exactement le meme visage qu'un OOM. C'est le seul cas ou la prudence a
  # encore un objet - et le coeur la formule lui-meme.
  i18n <- get_i18n("fr")
  out <- as.character(nemetonshiny:::.compute_error_message(
    '"f" was killed (signal 9; systemd\u2019s verdict unavailable). The memory ceiling (10G) is the usual cause',
    i18n))
  expect_true(grepl("habituelle", out))
  expect_true(grepl("10G", out, fixed = TRUE))
})

test_that("un coeur anterieur a 0.183.1 reste traduit", {
  # L'incident du 2026-08-22 : un `exit -15` nu, qui etait deja un OOM sans
  # pouvoir le dire. Le plancher n'ayant pas bouge, ce cas doit survivre.
  i18n <- get_i18n("fr")
  for (st in c("-9", "-15", "137", "143")) {
    out <- as.character(nemetonshiny:::.compute_error_message(
      sprintf('"start_computation" failed in its capped child process (exit %s).', st), i18n))
    expect_true(grepl("plafond", out), info = st)
    # Le code de sortie ne remonte pas a l'ecran : il ne dit rien a personne.
    expect_false(grepl("exit", out, fixed = TRUE), info = st)
  }
})

test_that("quand systemd dit que ce N'EST PAS la memoire, on ne le contredit pas", {
  # Le faux positif que le coeur a pris soin d'eviter : elargir a -15 aurait
  # fait passer un `systemctl stop` pour un depassement. L'app ne doit pas le
  # reintroduire par le bas.
  i18n <- get_i18n("fr")
  out <- as.character(nemetonshiny:::.compute_error_message(
    '"f" failed in its capped child process (systemd: "signal"). This is not the memory ceiling: systemd would have said "oom-kill".',
    i18n))
  expect_false(grepl("plafond m", out))
  expect_true(grepl("not the memory ceiling", out, fixed = TRUE))
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

test_that("le plafond est extrait des DEUX formulations du coeur", {
  f <- nemetonshiny:::.compute_error_ceiling
  expect_identical(f("ran out of memory and was killed (ceiling: 15G)."), "15G")
  expect_identical(f("The memory ceiling (10G) is the usual cause"), "10G")
  # « none » est une vraie valeur : le run n'etait pas plafonne, et le dire aide.
  expect_identical(f("ran out of memory and was killed (ceiling: none)."), "none")
  expect_null(f("objet introuvable"))
})

test_that("le message francais en dur a quitte mod_home", {
  f <- testthat::test_path("..", "..", "R", "mod_home.R")
  testthat::skip_if_not(file.exists(f), "sources R absentes")
  code <- readLines(f, warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  expect_false(any(grepl('paste("Erreur de calcul:"', code, fixed = TRUE)))
  expect_true(any(grepl(".compute_error_message", code, fixed = TRUE)))
})

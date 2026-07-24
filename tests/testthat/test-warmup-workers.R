# Tests — pré-chauffage des workers future (perf ouverture projet).
#
# warmup_async_workers charge le namespace dans les process worker en arrière-plan
# pour retirer les ~5-6 s du 1er future d'une session (db_sync / calcul / moteur).
# On mocke future::plan / promises::future_promise : aucun vrai worker spawné.

# Réinitialise le drapeau d'idempotence (env interne, muté par référence : lire
# via ::: puis mettre à jour — une assignation `ns:::env$x <- v` échouerait au
# writeback).
.reset_async_warm <- function() {
  w <- nemetonshiny:::.async_workers_warmed
  w$done <- FALSE
}

test_that("warmup_async_workers est un no-op sous un plan séquentiel", {
  skip_if_not_installed("future"); skip_if_not_installed("promises")
  .reset_async_warm()

  fired <- new.env(); fired$n <- 0L
  testthat::local_mocked_bindings(
    future_promise = function(...) { fired$n <- fired$n + 1L; NULL },
    .package = "promises")
  testthat::local_mocked_bindings(
    plan = function(...) structure(list(), class = c("sequential", "future", "function")),
    .package = "future")

  expect_false(warmup_async_workers())   # séquentiel → rien à chauffer
  expect_identical(fired$n, 0L)          # aucun future lancé (sinon blocage main-thread)
})

test_that("warmup_async_workers chauffe les workers une seule fois sous plan parallèle", {
  skip_if_not_installed("future"); skip_if_not_installed("promises")
  .reset_async_warm()

  fired <- new.env(); fired$n <- 0L
  testthat::local_mocked_bindings(
    future_promise = function(expr, ...) { fired$n <- fired$n + 1L; NULL },
    .package = "promises")
  testthat::local_mocked_bindings(
    plan = function(...) structure(list(), class = c("multisession", "cluster", "future")),
    nbrOfWorkers = function(...) 3L,
    .package = "future")

  expect_true(warmup_async_workers())
  # Contrat : min(nbrOfWorkers - 1, 4). Le `- 1` laisse TOUJOURS un worker
  # libre. Depuis que le pool est borné (.resolve_parallel_workers : 4 par
  # défaut), chauffer min(n, 4) occupait la TOTALITÉ du pool pendant les ~5-6 s
  # de chargement du namespace, et toute tâche async déclenchée dans cet
  # intervalle attendait la fin du préchauffage — boucle Shiny figée. Avant le
  # bornage, 4 workers sur 8 restaient libres et masquaient le problème.
  expect_identical(fired$n, 2L)          # min(3 - 1, 4) = 2 futures

  # Idempotent : un 2e appel ne relance aucun future.
  expect_false(warmup_async_workers())
  expect_identical(fired$n, 2L)
})

test_that("warmup_async_workers laisse toujours un worker libre", {
  skip_if_not_installed("future"); skip_if_not_installed("promises")
  # Sur un pool de 2 (le plancher de .resolve_parallel_workers), un seul worker
  # est chauffé : l'autre doit rester disponible.
  for (pool in list(c(n = 2L, warmed = 1L), c(n = 4L, warmed = 3L),
                    c(n = 8L, warmed = 4L))) {
    .reset_async_warm()
    fired <- new.env(); fired$n <- 0L
    local({
      testthat::local_mocked_bindings(
        future_promise = function(expr, ...) { fired$n <- fired$n + 1L; NULL },
        .package = "promises")
      testthat::local_mocked_bindings(
        plan = function(...) structure(list(), class = c("multisession", "cluster", "future")),
        nbrOfWorkers = function(...) pool[["n"]],
        .package = "future")
      expect_true(warmup_async_workers())
    })
    expect_identical(fired$n, pool[["warmed"]],
                     info = sprintf("pool de %d workers", pool[["n"]]))
  }
})

test_that("warmup_async_workers chauffe au moins un worker sur un pool de 1", {
  skip_if_not_installed("future"); skip_if_not_installed("promises")
  # Plancher : `max(1L, ...)`. Sur un hôte 1 cœur on ne peut pas laisser de
  # worker libre — on en chauffe quand même un plutôt que zéro (sinon la
  # première tâche async paierait les ~5-6 s de chargement du namespace).
  .reset_async_warm()
  fired <- new.env(); fired$n <- 0L
  testthat::local_mocked_bindings(
    future_promise = function(expr, ...) { fired$n <- fired$n + 1L; NULL },
    .package = "promises")
  testthat::local_mocked_bindings(
    plan = function(...) structure(list(), class = c("multisession", "cluster", "future")),
    nbrOfWorkers = function(...) 1L,
    .package = "future")

  expect_true(warmup_async_workers())
  expect_identical(fired$n, 1L)
})

test_that("warmup_async_workers plafonne le nombre de workers chauffés à 4", {
  skip_if_not_installed("future"); skip_if_not_installed("promises")
  .reset_async_warm()

  fired <- new.env(); fired$n <- 0L
  testthat::local_mocked_bindings(
    future_promise = function(expr, ...) { fired$n <- fired$n + 1L; NULL },
    .package = "promises")
  testthat::local_mocked_bindings(
    plan = function(...) structure(list(), class = c("multisession", "cluster", "future")),
    nbrOfWorkers = function(...) 16L,
    .package = "future")

  expect_true(warmup_async_workers())
  expect_identical(fired$n, 4L)          # plafond à 4 même avec 16 cœurs
})

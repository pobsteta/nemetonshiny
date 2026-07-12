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
  expect_identical(fired$n, 3L)          # min(nbrOfWorkers=3, 4) = 3 futures

  # Idempotent : un 2e appel ne relance aucun future.
  expect_false(warmup_async_workers())
  expect_identical(fired$n, 3L)
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

# Onglet principal " Atlas " et ses sous-onglets Selection / Synthese
# (R/service_navigation.R).

test_that(".onglet_parent renvoie l'Atlas pour ses sous-onglets", {
  expect_identical(nemetonshiny:::.onglet_parent("selection"), "atlas")
  expect_identical(nemetonshiny:::.onglet_parent("synthesis"), "atlas")
  expect_identical(nemetonshiny:::.onglet_parent("monitoring"), "monitoring")
  expect_identical(nemetonshiny:::.onglet_parent("famille_carbone"), "famille_carbone")
  expect_null(nemetonshiny:::.onglet_parent(NULL))
})

test_that(".onglet_effectif expose le sous-onglet actif de l'Atlas", {
  f <- nemetonshiny:::.onglet_effectif
  expect_identical(f("atlas", "synthesis"), "synthesis")
  expect_identical(f("atlas", "selection"), "selection")
  # Avant que le client n'ait remonte `atlas_nav` : premier sous-onglet.
  expect_identical(f("atlas", NULL), "selection")
  expect_identical(f("atlas", "inconnu"), "selection")
  # Hors Atlas, le sous-onglet memorise ne compte pas.
  expect_identical(f("monitoring", "synthesis"), "monitoring")
  expect_null(f(NULL, "synthesis"))
})

test_that(".aller_onglet selectionne l'onglet parent puis le sous-onglet", {
  appels <- list()
  note <- function(id, value) appels[[length(appels) + 1L]] <<- list(id = id, value = value)
  testthat::local_mocked_bindings(
    updateNavbarPage = function(session, inputId, selected) note(inputId, selected),
    .package = "shiny")
  testthat::local_mocked_bindings(
    nav_select = function(id, selected = NULL, session) note(id, selected),
    .package = "bslib")

  nemetonshiny:::.aller_onglet(NULL, "synthesis")
  expect_equal(appels, list(list(id = "main_nav", value = "atlas"),
                            list(id = "atlas_nav", value = "synthesis")))

  appels <- list()
  nemetonshiny:::.aller_onglet(NULL, "monitoring")
  expect_equal(appels, list(list(id = "main_nav", value = "monitoring")))
})

test_that("les sous-onglets de l'Atlas existent dans l'UI", {
  html <- as.character(htmltools::renderTags(nemetonshiny:::app_ui(NULL))$html)
  expect_true(grepl('id="atlas_nav"', html, fixed = TRUE))
  expect_true(grepl('data-value="atlas"', html, fixed = TRUE))
  for (v in nemetonshiny:::ATLAS_SUBTABS) {
    expect_true(grepl(sprintf('data-value="%s"', v), html, fixed = TRUE),
                info = v)
  }
})

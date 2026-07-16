# Tests for mod_action_plan helpers (the reactive UI logic is covered
# end-to-end in the Shiny app; here we focus on the pure helpers).

test_that("coerce_table_value parses ints, numerics, strings and blanks", {
  expect_identical(nemetonshiny:::coerce_table_value("annee_cible", "5"), 5L)
  expect_identical(nemetonshiny:::coerce_table_value("nb_tiges",   "120"), 120L)
  expect_identical(nemetonshiny:::coerce_table_value("volume_m3",  "12.5"), 12.5)
  expect_identical(nemetonshiny:::coerce_table_value("rdi",        "0.7"), 0.7)
  expect_identical(nemetonshiny:::coerce_table_value("commentaire", "abc"), "abc")
  # blanks become NA (caller decides what to do)
  expect_true(is.na(nemetonshiny:::coerce_table_value("annee_cible", "")))
  expect_true(is.na(nemetonshiny:::coerce_table_value("commentaire", NULL)))
})

test_that("module UI renders without error", {
  ui <- nemetonshiny:::mod_action_plan_ui("ap")
  # bslib widgets may be tag, tagList or htmlwidget shells — we just want
  # the call to succeed and produce HTML when rendered.
  expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list", "shiny.tag.env"),
                       which = FALSE) ||
              !is.null(htmltools::renderTags(ui)$html))
})

test_that("Kanban drag-and-drop assets are vendored in inst/app/www/js", {
  sortable <- system.file("app/www/js/Sortable-1.15.6.min.js",
                          package = "nemetonshiny")
  init_js  <- system.file("app/www/js/action_plan_kanban.js",
                          package = "nemetonshiny")
  expect_true(nzchar(sortable))
  expect_true(nzchar(init_js))
  # The init script must expose initKanbanSortable so the inline
  # <script> emitted by renderUI can call it.
  expect_match(paste(readLines(init_js, warn = FALSE), collapse = "\n"),
               "window\\.initKanbanSortable", fixed = FALSE)
  # Sortable.create is the API we rely on inside initKanbanSortable.
  expect_match(paste(readLines(sortable, warn = FALSE), collapse = "\n"),
               "Sortable", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Archive du PDF du plan d'action dans exports/ (spec 037, §5)
# ---------------------------------------------------------------------------
# La logique d'archivage vit dans le helper `.archive_action_plan_pdf`, appelé
# sur le SEUL chemin succès de output$download_pdf (le downloadHandler lui-même
# n'est pas invocable proprement en testServer sous cette version de shiny :
# registerDownload n'est pas câblé via le session_proxy). On teste donc le
# helper directement — il porte tout le comportement décrit au §5.

`%||%` <- function(a, b) if (is.null(a)) b else a

test_that("le PDF rendu est archivé sous exports/<slug>_action_plan.pdf", {
  pdir <- withr::local_tempdir()
  rendered <- tempfile(fileext = ".pdf")
  writeLines("%PDF-1.4 fake", rendered)
  project <- list(path = pdir, metadata = list(name = "Forêt de Test"))

  res <- nemetonshiny:::.archive_action_plan_pdf(rendered, project)
  expect_true(res)
  # Slug = caractères non [A-Za-z0-9_-] remplacés par « _ ».
  archived <- file.path(pdir, "exports", "For_t_de_Test_action_plan.pdf")
  expect_true(file.exists(archived))
  expect_match(readLines(archived, n = 1), "PDF")
})

test_that("exports/ est créé au besoin et l'archive écrase la précédente", {
  pdir <- withr::local_tempdir()
  project <- list(path = pdir, metadata = list(name = "Projet"))
  expect_false(dir.exists(file.path(pdir, "exports")))

  r1 <- tempfile(fileext = ".pdf"); writeLines("v1", r1)
  nemetonshiny:::.archive_action_plan_pdf(r1, project)
  archived <- file.path(pdir, "exports", "Projet_action_plan.pdf")
  expect_identical(readLines(archived, n = 1), "v1")

  # Un second export écrase l'archive (un seul PDF courant par projet).
  r2 <- tempfile(fileext = ".pdf"); writeLines("v2", r2)
  nemetonshiny:::.archive_action_plan_pdf(r2, project)
  expect_identical(readLines(archived, n = 1), "v2")
})

test_that("projet sans chemin disque -> pas d'archive, pas d'erreur", {
  rendered <- tempfile(fileext = ".pdf"); writeLines("%PDF fake", rendered)
  # path NULL
  expect_false(nemetonshiny:::.archive_action_plan_pdf(rendered,
    list(path = NULL, metadata = list(name = "X"))))
  # path inexistant
  expect_false(nemetonshiny:::.archive_action_plan_pdf(rendered,
    list(path = file.path(tempdir(), "nope-does-not-exist"),
         metadata = list(name = "X"))))
})

test_that("fichier rendu absent -> best-effort, aucune exception", {
  pdir <- withr::local_tempdir()
  project <- list(path = pdir, metadata = list(name = "Projet"))
  # Source inexistante : file.copy renvoie FALSE (avec un warning), pas d'erreur.
  expect_no_error(
    res <- suppressWarnings(
      nemetonshiny:::.archive_action_plan_pdf(
        file.path(tempdir(), "absent.pdf"), project)))
  expect_false(res)
})

test_that("nom de projet manquant -> slug de repli nemeton_action_plan.pdf", {
  pdir <- withr::local_tempdir()
  rendered <- tempfile(fileext = ".pdf"); writeLines("%PDF fake", rendered)
  project <- list(path = pdir, metadata = list())   # ni name ni id
  expect_true(nemetonshiny:::.archive_action_plan_pdf(rendered, project))
  expect_true(file.exists(file.path(pdir, "exports", "nemeton_action_plan.pdf")))
})

# Racine du corpus RAG transmise au worker (brief coeur 0.210.0 §1.1)

test_that(".rag_corpus_root follows the core precedence", {
  f <- nemetonshiny:::.rag_corpus_root
  withr::local_options(nemeton.corpus_root = NULL)
  withr::local_envvar(NEMETON_CORPUS_ROOT = "")
  expect_identical(f(), "")
  withr::local_envvar(NEMETON_CORPUS_ROOT = "/srv/corpus")
  expect_identical(f(), "/srv/corpus")
  withr::local_options(nemeton.corpus_root = "/opt/corpus")
  expect_identical(f(), "/opt/corpus")
  withr::local_options(nemeton.corpus_root = "")
  expect_identical(f(), "/srv/corpus")
})

test_that("the import task hands the corpus root to the worker", {
  src <- system.file("R", package = "nemetonshiny")
  fichier <- if (nzchar(src) && file.exists(file.path(src, "mod_rag_admin.R"))) {
    file.path(src, "mod_rag_admin.R")
  } else {
    testthat::test_path("..", "..", "R", "mod_rag_admin.R")
  }
  skip_if_not(file.exists(fichier), "sources R absentes (covr / paquet installe)")
  code <- paste(readLines(fichier, warn = FALSE), collapse = "\n")
  expect_match(code, "options(nemeton.corpus_root = root)", fixed = TRUE)
  expect_match(code, ".rag_corpus_root())", fixed = TRUE)
})

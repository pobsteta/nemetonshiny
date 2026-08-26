# Les gardes qui empechent R-CMD-check de rougir sur des tests d'arbre source.
#
# Ecrits d'abord avec `expect_error(..., class = "skip")` : le skip se PROPAGE
# et sautait le test lui-meme (SKIP 1, aucune assertion evaluee). Un test
# vacant, exactement ce qu'on cherchait a eviter. La condition doit donc etre
# attrapee, pas attendue.
attrape_skip <- function(expr) {
  tryCatch({ force(expr); NULL }, skip = function(c) conditionMessage(c))
}

test_that("skip_sans_sources saute quand l'arbre source manque", {
  msg <- attrape_skip(skip_sans_sources("/n/existe/pas.R"))
  expect_type(msg, "character")
  expect_match(msg, "pas.R", fixed = TRUE)
})

test_that("skip_sans_sources laisse passer un fichier present", {
  f <- withr::local_tempfile()
  writeLines("x", f)
  expect_null(attrape_skip(skip_sans_sources(f)))
  expect_identical(skip_sans_sources(f), f)
})

test_that("un seul chemin manquant sur plusieurs suffit a sauter", {
  f <- withr::local_tempfile()
  writeLines("x", f)
  msg <- attrape_skip(skip_sans_sources(f, "/n/existe/pas.R"))
  expect_match(msg, "pas.R", fixed = TRUE)
})

test_that("chemin_inst resout une ressource qui survit a l'installation", {
  # C'est tout l'interet de la distinguer de `R/` : sous R CMD check ce test
  # s'EXECUTE au lieu d'etre saute.
  p <- chemin_inst("app", "www", "css", "custom.css")
  expect_true(file.exists(p))
  expect_match(paste(readLines(p, warn = FALSE), collapse = "\n"),
               ".btn-ia {", fixed = TRUE)
})

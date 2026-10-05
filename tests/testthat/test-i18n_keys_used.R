# Toute cle i18n appelee en dur par le code existe dans TRANSLATIONS
# (audit 1.0 : `dess_typage_no_parcelles` affichee brute a l'utilisateur).

test_that("every literal i18n$t() key used in R/ exists in TRANSLATIONS", {
  src <- system.file("R", package = "nemetonshiny")
  dir_r <- testthat::test_path("..", "..", "R")
  skip_if(!dir.exists(dir_r), "sources R absentes (covr / paquet installe)")
  fichiers <- list.files(dir_r, "\\.R$", full.names = TRUE)
  skip_if(!length(fichiers), "sources R absentes")
  code <- unlist(lapply(fichiers, readLines, warn = FALSE))
  m <- regmatches(code, gregexpr("\\$t\\(\\s*\"([a-z0-9_]+)\"", code, perl = TRUE))
  cles <- unique(sub("^\\$t\\(\\s*\"", "", sub("\"$", "", unlist(m))))
  manquantes <- setdiff(cles, names(TRANSLATIONS))
  expect_identical(manquantes, character(0))
})

test_that("TRANSLATIONS has no duplicated key (audit 1.0)", {
  # En R, une liste a cles dupliquees rend la PREMIERE : la seconde definition
  # est du texte mort qui peut contredire la premiere (c2_wms_irc).
  expect_identical(names(TRANSLATIONS)[duplicated(names(TRANSLATIONS))], character(0))
})

test_that("no translation shows raw cli markup (audit 1.0)", {
  txt <- unlist(TRANSLATIONS)
  expect_false(any(grepl("\\{\\.(pkg|code|val|file|fn|arg)\\b", txt)))
})

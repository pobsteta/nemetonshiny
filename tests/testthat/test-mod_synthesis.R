
# ---- Accent IA (v0.130.8.9001) ---------------------------------------------

test_that("le bouton IA de la Synthese porte l'accent ambre et les trois etoiles", {
  skip_if_not_installed("bslib")
  h <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_synthesis_ui("syn"))
  )
  expect_true(grepl("btn-ia", h, fixed = TRUE))
  expect_true(grepl("stars", h, fixed = TRUE))
  # L'ancienne icône « robot » a bien disparu de ce bouton.
  expect_false(grepl("fa-robot", h, fixed = TRUE))
})

test_that("la classe btn-ia existe dans le CSS, avec un texte lisible", {
  css <- readLines(
    testthat::test_path("..", "..", "inst", "app", "www", "css", "custom.css"),
    warn = FALSE)
  txt <- paste(css, collapse = "\n")
  expect_match(txt, "--nemeton-ia: #E8A33D", fixed = TRUE)
  expect_match(txt, ".btn-ia {", fixed = TRUE)

  # Le texte doit rester SOMBRE sur cet ambre. Mesuré : #2C3E50 donne 5,09:1
  # (AA exige 4,5 pour du texte normal), le blanc tombe à 2,16:1. Un bouton
  # d'accent illisible n'accentue rien.
  bloc <- sub(".*\\.btn-ia \\{", "", txt)
  bloc <- sub("\\}.*", "", bloc)
  expect_match(bloc, "color: var(--nemeton-fg)", fixed = TRUE)
  expect_false(grepl("color:\\s*white", bloc))
})

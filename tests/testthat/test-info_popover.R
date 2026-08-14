# `info_popover()` porte le pattern UNIQUE du « i » d'information : celui des
# titres de l'onglet Synthèse (icône bleue `circle-info` + popover au clic).
# Tout nouveau « i » passe par ce helper — d'où ces tests, qui figent le rendu
# attendu et vérifient que reGénération l'utilise vraiment.

test_that("info_popover rend l'icone bleue circle-info du pattern Synthese", {
  skip_if_not_installed("bslib")
  h <- as.character(nemetonshiny:::info_popover("Explication"))
  expect_match(h, "text-info", fixed = TRUE)      # bleu sémantique Bootstrap
  expect_match(h, "cursor: help", fixed = TRUE)   # curseur d'aide au survol
  expect_match(h, "fa-circle-info", fixed = TRUE) # icône FontAwesome, pas bsicons
  expect_match(h, "fa-sm", fixed = TRUE)
  expect_match(h, "popover-lg", fixed = TRUE)     # largeur des popovers Synthèse
  expect_match(h, "Explication", fixed = TRUE)
  # Popover (clic) et NON tooltip (survol) : le contenu est de la prose, il doit
  # rester à l'écran pendant la lecture.
  expect_match(h, "<bslib-popover", fixed = TRUE)
  expect_false(grepl("<bslib-tooltip", h, fixed = TRUE))
})

test_that("reGeneration n'utilise plus l'ancien « i » bsicons", {
  skip_if_not_installed("bslib")
  ui <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    nemetonshiny:::mod_regeneration_ui("regen")
  )
  h <- as.character(ui)
  expect_false(grepl("bi-info-circle", h, fixed = TRUE))
  # 6 labels de paramètres + 8 entrées de couches/vues raster.
  expect_equal(lengths(regmatches(h, gregexpr("fa-circle-info", h))), 14L)
  # Le « i » d'une couche ne doit PAS sélectionner le radio qui le contient :
  # s'informer n'est pas choisir (la vue « rr » déclencherait 800 Mo de E-OBS).
  expect_match(h, "event.preventDefault()", fixed = TRUE)
})

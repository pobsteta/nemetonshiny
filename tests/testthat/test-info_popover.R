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

test_that("info_popover_in_label neutralise l'activation du label", {
  skip_if_not_installed("bslib")
  h <- as.character(nemetonshiny:::info_popover_in_label("Explication"))
  # Même icône que le pattern de base...
  expect_match(h, "fa-circle-info", fixed = TRUE)
  expect_match(h, "<bslib-popover", fixed = TRUE)
  # ...plus l'annulation de l'action par défaut du <label> : un clic sur le « i »
  # d'un choix de radio le sélectionnerait, et chaque sélection de couche coûte
  # une lecture de raster (jusqu'à ~800 Mo de E-OBS en contexte reGénération).
  expect_match(h, "event.preventDefault()", fixed = TRUE)
  # Mais PAS de stopPropagation : le document doit continuer de voir le clic,
  # sinon les popovers déjà ouverts ne se refermeraient plus.
  expect_false(grepl("stopPropagation", h, fixed = TRUE))
})

test_that("les « i » de couche du suivi sanitaire passent par le helper", {
  skip_if_not_installed("bslib")
  # FORDEAD et RECONFORT construisaient chacun leur icône ad hoc (tooltip
  # bsicons gris) dans le <label> de leurs radios de couche.
  for (f in list(nemetonshiny:::.fordead_layer_choice,
                 nemetonshiny:::.reconfort_layer_choice)) {
    h <- as.character(f("Sévérité", "Ce que la couche affiche"))
    expect_match(h, "fa-circle-info", fixed = TRUE)
    expect_match(h, "event.preventDefault()", fixed = TRUE)
    expect_false(grepl("bi-info-circle", h, fixed = TRUE))
    expect_false(grepl("<bslib-tooltip", h, fixed = TRUE))
  }
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

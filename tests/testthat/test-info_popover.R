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
  # 8 entrées de couches/vues raster + les 2 paramètres restés dans le sidebar.
  # Le compte était de 14 : quatre « i » de calibrage ont suivi leurs réglages
  # dans Paramètres › Sources & paramètres en v0.128.0. Ils ne sont pas perdus,
  # le test suivant les y retrouve — c'est tout l'objet de le vérifier là-bas
  # plutôt que de baisser un chiffre.
  expect_equal(lengths(regmatches(h, gregexpr("fa-circle-info", h))), 10L)
  # Le « i » d'une couche ne doit PAS sélectionner le radio qui le contient :
  # s'informer n'est pas choisir (la vue « rr » déclencherait 800 Mo de E-OBS).
  expect_match(h, "event.preventDefault()", fixed = TRUE)
})


test_that("les « i » des calibrages reGeneration ont suivi dans les parametres", {
  # Contrepartie du compte ci-dessus. Les quatre reglages deplaces en v0.128.0
  # sont rendus cote serveur (`output$regen_block`), donc invisibles a
  # `mod_sources_config_ui()` : sans ce test, leur « i » pourrait disparaitre
  # sans qu'aucune assertion ne bouge.
  skip_if_not_installed("bslib")
  shiny::testServer(nemetonshiny:::mod_sources_config_server, args = list(
    app_state = shiny::reactiveValues(
      language = "fr", project_id = "p1",
      current_project = list(id = "p1", metadata = list()))
  ), {
    session$setInputs(x = 1)
    h <- paste(as.character(output$regen_block), collapse = " ")
    expect_false(grepl("bi-info-circle", h, fixed = TRUE))
    expect_equal(lengths(regmatches(h, gregexpr("fa-circle-info", h))), 4L)
  })
})

# Tests for mod_pipeline.R
# UI du lancement enchaine (« Tout calculer »).

test_that("mod_pipeline_ui rend une UI Shiny valide", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  ui <- nemetonshiny:::mod_pipeline_ui("pipeline")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("la section « Tout calculer » est retractable comme les autres", {
  # Le panneau de progression liste dix-sept etapes : sans repli, il pousse le
  # reste de la sidebar hors de l'ecran des que la chaine est lancee. On
  # verifie le meme mecanisme Bootstrap que les autres blocs de cette sidebar
  # (projets recents, recherche), et non un simple `div` : c'est l'attribut
  # `data-bs-toggle` qui rend l'entete cliquable.
  withr::local_options(nemeton.app_options = list(language = "fr"))
  html <- as.character(nemetonshiny:::mod_pipeline_ui("home-pipeline"))

  expect_true(grepl('data-bs-toggle="collapse"', html, fixed = TRUE))
  expect_true(grepl('data-bs-target="#home-pipeline-pipeline_collapse"',
                    html, fixed = TRUE))
  expect_true(grepl('id="home-pipeline-pipeline_collapse"', html, fixed = TRUE))
  # Deplie par defaut : le bouton doit rester visible sans clic prealable.
  expect_true(grepl('class="collapse show"', html, fixed = TRUE))
  # Le chevron, marqueur visuel commun aux sections repliables de la sidebar.
  expect_true(grepl("collapse-icon", html, fixed = TRUE))
})

test_that("le bouton de lancement reste dans la section repliable", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  html <- as.character(nemetonshiny:::mod_pipeline_ui("home-pipeline"))
  # Le bouton et le panneau doivent etre APRES l'ouverture du bloc collapse :
  # les laisser en dehors les rendrait insensibles au repli.
  pos_collapse <- regexpr('id="home-pipeline-pipeline_collapse"', html, fixed = TRUE)
  pos_bouton   <- regexpr('id="home-pipeline-open"', html, fixed = TRUE)
  pos_panneau  <- regexpr('id="home-pipeline-panel"', html, fixed = TRUE)
  expect_gt(pos_bouton, pos_collapse)
  expect_gt(pos_panneau, pos_collapse)
})

test_that("le bouton de lancement porte la couleur d'action principale", {
  # Regle normative des couleurs : vert = action principale. PAS d'ambre - qui
  # signale une provenance (contenu genere), pas un niveau d'action - meme si
  # la chaine se termine par deux generations IA.
  withr::local_options(nemeton.app_options = list(language = "fr"))
  html <- as.character(nemetonshiny:::mod_pipeline_ui("pipeline"))
  expect_true(grepl("btn-primary", html, fixed = TRUE))
  expect_false(grepl("btn-ia", html, fixed = TRUE))
})


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

test_that("les trois surfaces IA partagent le meme accent", {
  skip_if_not_installed("bslib")
  # Une charte d'accent qui ne vaut que pour un écran n'est pas une charte.
  # Ces trois surfaces produisent toutes du contenu généré : le bouton de la
  # Synthèse, le panneau de dialogue du Plan d'actions, celui de reGénération.
  opts <- function(expr) with_mocked_bindings(
    get_app_options = function() list(language = "fr"), expr)

  syn <- opts(as.character(nemetonshiny:::mod_synthesis_ui("syn")))
  ap  <- opts(as.character(nemetonshiny:::mod_action_plan_ui("ap")))
  rg  <- opts(as.character(nemetonshiny:::mod_regeneration_ui("rg")))

  expect_true(grepl("btn-ia", syn, fixed = TRUE))
  expect_true(grepl("bg-ia", ap, fixed = TRUE))
  expect_true(grepl("bg-ia", rg, fixed = TRUE))
  # Toutes portent les trois étoiles.
  for (h in list(syn, ap, rg)) expect_true(grepl("stars", h, fixed = TRUE))

  # Les couleurs qu'elles portaient avant ont disparu de CES panneaux : le bleu
  # « information » du Plan d'actions et le vert « succès » de reGénération.
  # Un panneau de dialogue avec un modèle n'est ni l'un ni l'autre.
  expect_false(grepl("card-header bg-info", ap, fixed = TRUE))
  expect_false(grepl("bi-chat-dots", ap, fixed = TRUE))
  expect_false(grepl("bi-robot", rg, fixed = TRUE))

  # La vue Famille a rejoint la charte (v0.130.9.9001).
  fam <- opts(as.character(nemetonshiny:::mod_family_ui("fam", "W")))
  expect_true(grepl("btn-ia", fam, fixed = TRUE))
  expect_true(grepl("stars", fam, fixed = TRUE))
})

test_that("plus aucune icone robot ne subsiste dans les sources", {
  # L'accent ne tient que si le robot disparaît PARTOUT — y compris des
  # `updateActionButton()` qui restaurent l'icône après une génération. Ceux-là
  # défaisaient l'accent dès la première utilisation du bouton, sans qu'aucun
  # test d'UI ne le voie : ils ne s'exécutent qu'en session.
  src <- list.files(testthat::test_path("..", "..", "R"), pattern = "\\.R$",
                    full.names = TRUE)
  restants <- unlist(lapply(src, function(f) {
    l <- readLines(f, warn = FALSE)
    hit <- grep('icon\\("robot"\\)', l)
    if (length(hit)) sprintf("%s:%d", basename(f), hit) else NULL
  }))
  expect_equal(restants, NULL)
})

test_that("le CLAUDE.md documente l'accent ambre", {
  # Sans cette ligne, la règle normative des couleurs de bouton décrit une
  # hiérarchie où l'ambre n'existe pas : une prochaine relecture y verrait une
  # entorse esthétique — ce que la règle interdit explicitement — et la
  # « corrigerait » en repassant le bouton en vert.
  md <- readLines(testthat::test_path("..", "..", "CLAUDE.md"), warn = FALSE)
  txt <- paste(md, collapse = "\n")
  expect_match(txt, "btn-ia", fixed = TRUE)
  expect_match(txt, "#E8A33D", fixed = TRUE)
  # Et il dit POURQUOI elle échappe à la hiérarchie, sinon la ligne se lit
  # comme un sixième niveau d'action.
  expect_match(txt, "provenance", fixed = TRUE)
})

test_that("le vert du bloc Tableau des actions n'est PAS touche", {
  skip_if_not_installed("bslib")
  # `action_table_card()` reste vert : c'est un bloc d'actions de l'utilisateur,
  # pas une surface IA. Confondre les deux viderait l'accent de son sens.
  css <- readLines(testthat::test_path("..", "..", "R", "utils_ui.R"), warn = FALSE)
  expect_true(any(grepl("card-header bg-success", css, fixed = TRUE)))
})

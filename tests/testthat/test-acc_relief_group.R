# test-acc_relief_group.R — un seul relief, une seule case
#
# La carte d'accessibilité peint le relief par DEUX chemins (le rendu de la
# carte, en fond semi-transparent ; le comparateur de desserte, en fond opaque
# sous les tronçons) mais affiche le MÊME fichier : les deux passent par
# `generate_rvt()`.
#
# Ils écrivaient dans deux groupes leaflet distincts, dont un seul était déclaré
# dans les `overlayGroups` du LayersControl. Décocher « Relief CVAT » pendant que
# le comparateur était actif masquait donc un raster invisible et laissait à
# l'écran celui que l'utilisateur voyait, sans case pour l'éteindre.
#
# Ces tests figent l'invariant : UN nom de groupe, porté par une constante, et
# une case déclarée en toutes circonstances.

test_that("le groupe de relief est une constante unique", {
  g <- nemetonshiny:::ACC_RELIEF_GROUP
  expect_type(g, "character")
  expect_length(g, 1L)
  expect_true(nzchar(g))
})

test_that("aucun second groupe de relief ne subsiste dans les sources", {
  src <- readLines(test_path("..", "..", "R", "mod_accessibility.R"),
                   warn = FALSE)
  code <- src[!grepl("^\\s*#", src)]   # hors commentaires et roxygen

  # L'ancien groupe orphelin du comparateur.
  expect_false(any(grepl("Relief RVT", code, fixed = TRUE)))

  # Plus aucun littéral « Relief… » dans un `group =` : tous les chemins de
  # peinture passent par la constante, sinon ils divergeraient à nouveau.
  expect_false(any(grepl("group\\s*=\\s*\"Relief", code)))
})

test_that("la case du relief est declaree meme sans CVAT pret", {
  # Le comparateur peut peindre un relief plus tard (worker async) : si la case
  # n'est déclarée que lorsqu'un CVAT existe déjà au rendu, ce relief-là devient
  # inextinguible — c'est exactement le bug d'origine.
  src <- readLines(test_path("..", "..", "R", "mod_accessibility.R"),
                   warn = FALSE)
  code <- src[!grepl("^\\s*#", src)]

  overlays <- grep("^\\s*overlays\\s*<-", code)
  expect_length(overlays, 1L)
  bloc <- paste(code[overlays:(overlays + 4L)], collapse = " ")
  expect_match(bloc, "ACC_RELIEF_GROUP", fixed = TRUE)
  # Pas de condition sur la disponibilité du CVAT autour du relief.
  expect_false(grepl("is.null(cvat_bg)) \"Relief", bloc, fixed = TRUE))
})

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

test_that("les couches du comparateur sont declarees dans le controle", {
  # Même classe de bug que le relief : une couche peinte mais absente des
  # `overlayGroups` n'a pas de case, donc rien ne peut l'éteindre. Décocher la
  # couche corrigée est précisément la façon de lire ce qu'elle change par
  # rapport à la BD TOPO qu'elle recouvre.
  expect_type(nemetonshiny:::ACC_DESSERTE_CORR_GROUP, "character")
  expect_type(nemetonshiny:::ACC_DESSERTE_ORIG_GROUP, "character")

  src <- readLines(test_path("..", "..", "R", "mod_accessibility.R"),
                   warn = FALSE)
  code <- src[!grepl("^\\s*#", src)]

  ov <- grep("^\\s*overlays\\s*<-", code)
  expect_length(ov, 1L)
  bloc <- paste(code[ov:(ov + 6L)], collapse = " ")
  expect_match(bloc, "ACC_DESSERTE_ORIG_GROUP", fixed = TRUE)
  expect_match(bloc, "ACC_DESSERTE_CORR_GROUP", fixed = TRUE)

  # Et plus aucun littéral pour CES deux groupes : ils passent par les
  # constantes, sinon la déclaration et la peinture peuvent diverger sans bruit.
  # (`group = "Desserte"` reste littéral : c'est une AUTRE couche, celle qui a
  # servi au calcul, déclarée telle quelle dans les overlays.)
  expect_false(any(grepl("group\\s*=\\s*\"Desserte (origine|corrigee)\"", code)))
})

test_that("la desserte du RUN n'est pas peinte sous le comparateur", {
  # Trois couches de tronçons peuvent coexister sur cette carte. Celle du run
  # utilise une palette différente de celle du comparateur : superposées, elles
  # mettent à l'écran des tronçons de même couleur qui ne disent pas la même
  # chose, sans légende pour l'expliquer. L'observe de la desserte du run doit
  # donc sortir tôt quand la couche comparateur est sélectionnée.
  src <- readLines(test_path("..", "..", "R", "mod_accessibility.R"),
                   warn = FALSE)
  i <- grep('clearGroup\\("Desserte"\\)', src)
  expect_length(i, 1L)

  bloc <- paste(src[i:min(length(src), i + 14L)], collapse = " ")
  expect_match(bloc, 'identical\\(input\\$layer, "desserte_comparee"\\)')
  expect_match(bloc, "return\\(\\)", fixed = FALSE)
})

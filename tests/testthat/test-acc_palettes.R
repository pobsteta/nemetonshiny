# test-acc_palettes.R — séparation des deux légendes du comparateur de desserte
#
# Le comparateur affiche DEUX légendes côte à côte : la classe du tronçon BD
# TOPO (couche de fond) et la source du tronçon corrigé (par-dessus). Une
# couleur choisie « jolie » pour une palette peut se retrouver à l'identique
# dans l'autre — c'est exactement ce qui était arrivé à `route` (#37474F) et
# `bdtopo` (#455A64), séparés de 8 unités Lab, soit rien.
#
# Ces tests mesurent la séparation plutôt que de la supposer. Ils échouent si
# une future retouche rapproche deux modalités, dans l'une ou l'autre légende.

# Distance euclidienne dans CIE Lab entre deux couleurs hexadécimales.
# grDevices suffit — pas de dépendance nouvelle pour une mesure de 3 lignes.
.lab_distance <- function(hex1, hex2) {
  lab <- grDevices::convertColor(
    t(grDevices::col2rgb(c(hex1, hex2))) / 255, from = "sRGB", to = "Lab")
  sqrt(sum((lab[1, ] - lab[2, ])^2))
}

# Ratio de contraste WCAG entre une couleur et le blanc.
.contrast_on_white <- function(hex) {
  v <- grDevices::col2rgb(hex) / 255
  f <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  lum <- 0.2126 * f[1, ] + 0.7152 * f[2, ] + 0.0722 * f[3, ]
  (1 + 0.05) / (lum + 0.05)
}

test_that("les classes BD TOPO sont franchement distinctes entre elles", {
  cols <- nemetonshiny:::DESS_CLASSE_COLS
  expect_setequal(names(cols),
                  c("route", "piste", "reseau_public", "hors_desserte"))

  pairs <- utils::combn(names(cols), 2)
  d <- apply(pairs, 2, function(p) .lab_distance(cols[[p[1]]], cols[[p[2]]]))
  worst <- pairs[, which.min(d)]

  # Seuil 20 : très au-dessus du seuil de perception (~2.3) et au-dessus de la
  # valeur mesurée de l'ancienne palette (30 entre classes, mais 8 contre la
  # palette de source — cf. test suivant).
  expect_gt(min(d), 20)
  expect_true(all(d > 20),
              info = sprintf("paire la plus proche : %s / %s (Lab %.1f)",
                             worst[1], worst[2], min(d)))
})

test_that("aucune classe ne se confond avec une source de la legende voisine", {
  classe <- nemetonshiny:::DESS_CLASSE_COLS
  source <- nemetonshiny:::DESS_SOURCE_COLS

  grid <- expand.grid(c = names(classe), s = names(source),
                      stringsAsFactors = FALSE)
  d <- mapply(function(c, s) .lab_distance(classe[[c]], source[[s]]),
              grid$c, grid$s)
  worst <- grid[which.min(d), ]

  # C'est CE test qui aurait attrapé la régression d'origine : route/bdtopo
  # était à 8. Il n'y a pas de raison d'accepter moins de 20 ici non plus.
  expect_gt(min(d), 20)
  expect_true(all(d > 20),
              info = sprintf("paire la plus proche : classe %s / source %s (Lab %.1f)",
                             worst$c, worst$s, min(d)))
})

test_that("les traits restent lisibles sur le relief RVT clair", {
  # Les tronçons sont peints sur un fond de relief qui va du gris clair au
  # blanc, et les pastilles de légende sont sur fond blanc : une teinte trop
  # claire est délavée quel que soit son écart aux autres. Seuil WCAG 1.4.11
  # (objets graphiques) = 3:1. `hors_desserte` est exclu : il DOIT s'effacer,
  # il est peint en tireté à 0.6 d'opacité pour se lire comme non utilisable.
  cols <- nemetonshiny:::DESS_CLASSE_COLS
  actives <- cols[setdiff(names(cols), "hors_desserte")]

  ratios <- vapply(actives, .contrast_on_white, numeric(1))
  expect_true(all(ratios >= 3),
              info = paste(sprintf("%s=%.1f:1", names(ratios), ratios),
                           collapse = " "))

  # Et le gris de `hors_desserte` reste, lui, volontairement en retrait.
  expect_lt(.contrast_on_white(cols[["hors_desserte"]]), 3)
})

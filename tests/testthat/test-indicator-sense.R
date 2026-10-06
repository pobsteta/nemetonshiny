# Tests — sens des indicateurs de risque (spec 048, nemeton >= 0.181.0)
#
# `nemeton 0.181.0` inverse R1 (feu), R2 (tempête), R3 (sécheresse) et R4
# (abroutissement) à la normalisation, comme R5 depuis 0.99.1. Leur grandeur
# brute est « haut = mauvais » et passait telle quelle sur le radar : une UGF
# très exposée obtenait un `famille_risque` ÉLEVÉ, donc flatteur.
#
# Côté app il n'y a aucun calcul à écrire — mais deux pièges à désamorcer :
# les indicateurs déjà calculés sont faux et doivent être invalidés, et la
# palette de la carte se retourne toute seule.





test_that("famille_risque n'est plus peinte avec la palette de risque", {
  # Le piège que le brief 048 ne couvre pas. `famille_risque` est désormais
  # orienté « haut = bon » : une palette YlOrRd (jaune -> rouge) colorerait en
  # ROUGE les UGF les MOINS à risque. L'inversion des valeurs retourne le sens
  # de la palette sans que personne y touche.
  est_risque <- function(x) grepl("^R[1-4]|^risk_", x)

  # Les grandeurs BRUTES gardent la palette : leur sens n'a pas bougé.
  expect_true(est_risque("R1"))
  expect_true(est_risque("R4"))
  expect_true(est_risque("risk_erosion"))
  # L'agrégat de famille en sort.
  expect_false(est_risque("famille_risque"))

  # Et le code source ne doit plus le mentionner dans ce motif.
  f <- chemin_source("R", "mod_family.R"); skip_sans_sources(f)
  src <- readLines(f,
                   warn = FALSE)
  pal <- grep("is_risk <- grepl", src, value = TRUE)
  expect_length(pal, 1L)
  expect_false(grepl("famille_risque", pal, fixed = TRUE))
})

test_that("l'app n'inverse aucun indicateur de risque elle-meme", {
  # Consigne n°1 du brief : le cœur rend déjà la valeur dans le bon sens. Toute
  # inversion côté app annulerait la correction EN SILENCE — le radar
  # remonterait sur les massifs exposés sans qu'aucun test ne tombe.
  r_dir <- chemin_source("R"); skip_sans_sources(r_dir)
  src <- list.files(r_dir, pattern = "\\.R$",
                    full.names = TRUE)
  suspects <- unlist(lapply(src, function(f) {
    l <- readLines(f, warn = FALSE)
    hit <- grep("(1|100)\\s*-\\s*.*(indicateur_r[1-5]|famille_risque)", l)
    if (length(hit)) sprintf("%s:%d", basename(f), hit) else NULL
  }))
  expect_equal(suspects, NULL)
})

# Garde-fou : l'app n'inverse aucun indicateur elle-meme. Le sens (haut = bon)
# est porte par le coeur ; une inversion cote app l'annulerait en silence.

test_that("l'app n'inverse RIEN cote client (piege du brief, §1)", {
  # Le cœur rend deja L1 dans le bon sens. Une inversion cote app
  # annulerait la correction EN SILENCE - meme consigne que pour R5 en
  # 0.94.0 et R1-R4 en 0.181.0. Et le piege des slugs croises (spec 045)
  # ferait retourner le MORCELLEMENT sur les jeux non migres.
  src <- unlist(lapply(
    list.files(testthat::test_path("..", "..", "R"), pattern = "\\.R$",
               full.names = TRUE),
    function(f) readLines(f, warn = FALSE)))
  src <- src[!grepl("^\\s*#", src)]   # les commentaires n'inversent rien

  motifs <- c("100\\s*-\\s*[a-zA-Z_.$]*l1", "100\\s*-\\s*[a-zA-Z_.$]*paysage",
              "100\\s*-\\s*[a-zA-Z_.$]*lisiere", "100\\s*-\\s*[a-zA-Z_.$]*t1",
              "100\\s*-\\s*[a-zA-Z_.$]*e1", "100\\s*-\\s*[a-zA-Z_.$]*e2")
  for (m in motifs) expect_false(any(grepl(m, src)), label = m)
})



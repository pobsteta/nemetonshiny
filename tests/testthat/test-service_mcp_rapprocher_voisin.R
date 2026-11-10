# Mission 007, aller-retour 3/3 : l'egalite exacte au mot-cle ne l'emporte que si
# aucun autre projet n'est a distance <= 1 du mot dicte.

test_that("Ecouche trouve Ecouche : Couchey est a distance 2", {
  noms <- c("Forêt communale de Écouché", "Forêt communale de Couchey (ONF)")
  expect_equal(.mcp_rapprocher("Écouché", noms), 1L)
})

test_that("Couchey face a Couchay : les deux candidats", {
  noms <- c("Forêt communale de Couchey (ONF)", "Forêt communale de Couchay")
  expect_equal(.mcp_rapprocher("Couchey", noms), c(1L, 2L))
})

test_that("Couchey face a Couchay : projet_ambigu avec les candidats", {
  l <- data.frame(id = c("a1", "b2"),
                  nom = c("Forêt communale de Couchey (ONF)", "Forêt communale de Couchay"),
                  stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(projets_lister = function(...) l)
  err <- tryCatch(.mcp_resolve_project("Couchey"), error = function(e) e)
  expect_s3_class(err, "nemetonshiny_projet_ambigu")
  expect_length(err$candidats, 2L)
})

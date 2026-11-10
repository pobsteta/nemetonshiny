# Mission 007 (aller-retour 1/3) : locale C, symetrie du prefixe, plancher de 4 lettres.
# Complete test-service_mcp_noms_dictes.R (006, non modifie).

.nl_projets <- function(noms, env = parent.frame()) {
  local_mocked_bindings(
    projets_lister = function(...) data.frame(id = paste0("p", seq_along(noms)),
                                              nom = noms, stringsAsFactors = FALSE),
    .env = env
  )
}

# Les accents sont ecrits en \u (pas de caracteres non ASCII dans la source).
.nl_couche <- "Couché"
.nl_foret_couche <- "Forêt communale de Couché"
.nl_projet_couchey <- "Forêt communale de Couchey (ONF)"

test_that("la normalisation ne depend pas de la locale (LC_ALL=C)", {
  withr::local_locale(c(LC_CTYPE = "C"))
  expect_identical(.mcp_norm_dicte(.nl_couche), "couche")
  expect_identical(.mcp_norm_dicte("Forêt communale de Marsannay-la-côte"),
                   "foret communale de marsannay la cote")
  expect_identical(.mcp_norm_dicte("ÉTÉ Ça"), "ete ca")
  expect_identical(.mcp_norm_dicte(NA_character_), "")
})

test_that("sous LC_ALL=C, Couche et Foret communale de Couche resolvent sans erreur", {
  .nl_projets(c(.nl_projet_couchey, "Forêt communale de Sombernon"))
  withr::local_envvar(LC_ALL = "C")
  withr::local_locale(c(LC_CTYPE = "C"))
  expect_identical(.mcp_resolve_project(.nl_couche), "p1")
  expect_identical(.mcp_resolve_project(.nl_foret_couche), "p1")
})

test_that("le prefixe Foret de/du/des est symetrique", {
  .nl_projets(c("Forêt de Dabo", "Forêt communale de Sombernon"))
  expect_identical(.mcp_resolve_project("Forêt de Dabot"), "p1")
  expect_identical(.mcp_resolve_project("Dabot"), "p1")
  expect_identical(.mcp_resolve_project("Forêt du Dabo"), "p1")
})

test_that("plancher : de et du ne rapprochent jamais Foret de Be", {
  .nl_projets(c("Forêt de Bé", "Forêt communale de Sombernon"))
  expect_error(.mcp_resolve_project("de"), class = "nemetonshiny_projet_introuvable")
  expect_error(.mcp_resolve_project("du"), class = "nemetonshiny_projet_introuvable")
  # L'egalite exacte reste permise sous le plancher.
  expect_identical(.mcp_resolve_project("Be"), "p1")
  expect_identical(.mcp_resolve_project("Bé"), "p1")
})

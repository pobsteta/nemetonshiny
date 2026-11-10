# Mission 007 (allers-retours 1/3 et 2/3) : locale C, symetrie du prefixe, plancher de 4 lettres.
# Complete test-service_mcp_noms_dictes.R (006, non modifie).

.nl_projets <- function(noms, env = parent.frame()) {
  local_mocked_bindings(
    projets_lister = function(...) data.frame(id = paste0("p", seq_along(noms)),
                                              nom = noms, stringsAsFactors = FALSE),
    .env = env
  )
}

# Accents en clair dans la source.
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

test_that("ligatures et majuscules : Cœuvres / Coeuvres / Coeuvre, pas de ß -> y", {
  expect_identical(.mcp_norm_dicte("Cœuvres"), "coeuvres")
  expect_identical(.mcp_norm_dicte("ŒUVRE Æ"), "oeuvre ae")
  expect_identical(.mcp_norm_dicte("Ÿ ÿ ß"), "y y")
  .nl_projets(c("Forêt communale de Cœuvres", "Forêt communale de Sombernon"))
  expect_identical(.mcp_resolve_project("Coeuvres"), "p1")
  expect_identical(.mcp_resolve_project("Cœuvres"), "p1")
  expect_identical(.mcp_resolve_project("Coeuvre"), "p1")
})

test_that("élision : Forêt communale d'Écouché ne se confond pas avec Couchey", {
  .nl_projets(c("Forêt communale d'Écouché", .nl_projet_couchey))
  expect_identical(.mcp_resolve_project("Écouché"), "p1")
  expect_identical(.mcp_resolve_project("Ecouche"), "p1")
  # Couchet est a distance <= 2 des deux mots-cles : candidats (regle 006, ligne 210).
  expect_error(.mcp_resolve_project("Couchet"), class = "nemetonshiny_projet_ambigu")
  expect_identical(.mcp_resolve_project("Forêt d’Écouché"), "p1")
  expect_identical(.mcp_resolve_project("Forêt communale d'Écouché"), "p1")
})

test_that("plancher côté dicté : la face à Laix, les face à Lens, introuvables", {
  .nl_projets(c("Laix", "Lens"))
  expect_error(.mcp_resolve_project("la"), class = "nemetonshiny_projet_introuvable")
  expect_error(.mcp_resolve_project("les"), class = "nemetonshiny_projet_introuvable")
})

test_that("vrai sous-processus LC_ALL=C : Couché (chaîne non marquée) résolu sans erreur", {
  skip_on_cran()
  racine <- normalizePath(test_path("..", ".."))
  if (length(list.files(file.path(racine, "R"), pattern = "\\.R$")) == 0L) {
    skip("sources R absentes (paquet installe, covr)")
  }
  script <- tempfile(fileext = ".R")
  writeLines(c(
    sprintf('pkgload::load_all("%s", quiet = TRUE)', racine),
    'x <- rawToChar(as.raw(c(0x43, 0x6f, 0x75, 0x63, 0x68, 0xc3, 0xa9)))',
    'r <- nemetonshiny:::.mcp_rapprocher(x, "Forêt communale de Couchey (ONF)")',
    'cat("RES", nemetonshiny:::.mcp_norm_dicte(x), r, "\\n")'
  ), script)
  sortie <- suppressWarnings(system2("Rscript", script,
    env = "LC_ALL=C", stdout = TRUE, stderr = TRUE))
  expect_null(attr(sortie, "status"))
  expect_true(any(grepl("^RES couche 1 *$", sortie)), info = paste(sortie, collapse = "\n"))
})

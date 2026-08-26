# Lecture de l'ARBRE SOURCE depuis les tests
#
# Une partie des tests de ce paquet n'inspecte pas un comportement mais du
# TEXTE : la classe d'un bouton dans `R/mod_synthesis.R`, une variable CSS dans
# `custom.css`, une regle du CLAUDE.md. C'est legitime - un test qui verrouille
# une convention doit lire la convention la ou elle est ecrite.
#
# Mais `R/*.R` et `CLAUDE.md` N'EXISTENT PAS dans le paquet installe. Sous
# `R CMD check`, les tests tournent depuis `<pkg>/tests/`, ou `../../R/` ne
# mene nulle part : `readLines()` echoue sur « cannot open the connection ».
# C'est ce qui a rendu R-CMD-check rouge sur `main` du 2026-08-23 au 2026-08-26,
# douze runs de suite, pendant que la suite locale annoncait 12 510 PASS.
#
# Deux situations, deux reponses - et elles ne se valent pas :
#
#   * `inst/` SURVIT a l'installation, sous un autre chemin. `chemin_inst()`
#     interroge `system.file()` d'abord : le test s'EXECUTE en CI, il ne se
#     contente pas d'y survivre.
#   * `R/` et la racine du depot ne survivent pas. `skip_sans_sources()` saute,
#     faute de mieux : un test saute vaut mieux qu'un test rouge, mais il ne
#     verifie plus rien la-bas. C'est le prix d'un test d'arbre source, et il
#     est paye sciemment.

# Chemin d'un fichier a la racine du depot (hors paquet installe).
chemin_source <- function(...) testthat::test_path("..", "..", ...)

# Saute le test quand l'arbre source est hors de portee (paquet installe).
skip_sans_sources <- function(...) {
  chemins <- c(...)
  manquants <- chemins[!file.exists(chemins)]
  testthat::skip_if(
    length(manquants) > 0L,
    paste0("arbre source hors de portee : ",
           paste(basename(manquants), collapse = ", "))
  )
  invisible(chemins)
}

# Chemin d'une ressource `inst/`, resolue dans le paquet installe quand il y en
# a un. Le test tourne alors pour de bon sous `R CMD check`.
chemin_inst <- function(...) {
  rel <- file.path(...)
  installe <- system.file(rel, package = "nemetonshiny")
  if (nzchar(installe) && file.exists(installe)) return(installe)
  testthat::test_path("..", "..", "inst", rel)
}

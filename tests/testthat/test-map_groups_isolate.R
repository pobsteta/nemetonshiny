# Garde de source : aucune lecture RÉACTIVE de `input$<id>_groups`.
#
# Leaflet renvoie `input$<id>_groups` à CHAQUE ajout ou retrait de groupe sur la
# carte (coalescé à 100 ms, cf. le binding leaflet.js). Un observe qui peint la
# carte ajoute et retire des groupes : s'il lit cet input réactivement, il se
# re-déclenche lui-même — et déclenche aussi les autres observes de la même
# carte, qui partagent l'input. La boucle converge, mais après plusieurs
# relectures de fichiers et plusieurs ré-encodages de raster : l'utilisateur voit
# la couche se peindre deux à quatre fois avant de se stabiliser (v0.122.3).
#
# La valeur reste nécessaire (respecter la décoche d'un groupe après re-dessin) :
# elle doit être lue sous `isolate()`. Le test complète la régression
# comportementale de `test-mod_accessibility.R` en couvrant TOUS les modules,
# y compris ceux à venir.

test_that("aucun module ne lit input$*_groups de façon réactive", {
  r_dir <- chemin_source("R"); skip_sans_sources(r_dir)
  skip_if_not(dir.exists(r_dir), "sources R absentes (package installé)")

  offenders <- character()
  for (f in list.files(r_dir, pattern = "[.]R$", full.names = TRUE)) {
    lines <- readLines(f, warn = FALSE)
    # Les commentaires PARLENT de cet input (dont ceux qui expliquent la règle) :
    # les retirer avant de chercher, sinon le garde se déclenche sur sa propre
    # documentation. Approximation assumée : aucun `#` littéral dans ces lignes.
    lines <- sub("#.*$", "", lines)
    hits <- grep("input\\$[A-Za-z0-9_]*groups", lines)
    for (i in hits) {
      # Seule forme admise : la lecture est enveloppée dans un isolate().
      if (grepl("isolate\\(\\s*input\\$[A-Za-z0-9_]*groups", lines[i])) next
      offenders <- c(offenders, sprintf("%s:%d", basename(f), i))
    }
  }
  expect_equal(offenders, character(),
               info = paste("lecture reactive de input$*_groups :",
                            paste(offenders, collapse = ", ")))
})

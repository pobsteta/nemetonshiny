# Mission 002 - tests d'acceptation du critere 5 (spec pilotage) :
# chaque outil de `mcp_tools()` porte `read_only_hint`, `destructive_hint` et
# `idempotent_hint` justes, visibles dans `tools/list`.
#
# Perimetre de la confirmation (Q1, Pascal, 2026-11-09) : toute modification
# d'un projet existant ; lecture, calcul et exports libres.
# Rouges tant que la mission 004 n'est pas fusionnee.

.an_outils <- function() {
  skip_if_not_installed("ellmer")
  o <- mcp_tools()
  stats::setNames(o, vapply(o, function(t) t@name, character(1)))
}
.an_hint <- function(outil, h) outil@annotations[[h]]

OUTILS_ATTENDUS <- c("lister_projets", "resume_projet", "lancer_calcul",
                     "etat_calcul", "annuler_calcul", "generer_rapport",
                     "exporter_gpkg", "appliquer_ugf", "croiser_onf", "url_app")
LECTURE <- c("lister_projets", "resume_projet", "etat_calcul", "url_app")
ECRITURE <- setdiff(OUTILS_ATTENDUS, LECTURE)
DESTRUCTIFS <- c("appliquer_ugf", "croiser_onf")

test_that("mcp_tools() expose exactement les dix outils attendus", {
  expect_setequal(names(.an_outils()), OUTILS_ATTENDUS)
  expect_length(.an_outils(), 10L)
})

test_that("les trois hints sont poses (logiques, non NA) sur chaque outil", {
  for (nom in OUTILS_ATTENDUS) {
    o <- .an_outils()[[nom]]
    for (h in c("read_only_hint", "destructive_hint", "idempotent_hint")) {
      v <- .an_hint(o, h)
      expect_true(is.logical(v) && length(v) == 1L && !is.na(v),
                  info = paste(nom, h))
    }
  }
})

test_that("read_only_hint : TRUE pour les 4 outils de lecture, FALSE pour les 6 autres", {
  for (nom in LECTURE) expect_true(.an_hint(.an_outils()[[nom]], "read_only_hint"), info = nom)
  for (nom in ECRITURE) expect_false(.an_hint(.an_outils()[[nom]], "read_only_hint"), info = nom)
})

# Arbitrages de Pascal (2026-11-10) : assertions exactes pour les dix outils.
# idempotent_hint FALSE pour lancer_calcul seul, TRUE pour les 9 autres.
IDEMPOTENTS <- c(LECTURE, "appliquer_ugf", "croiser_onf", "generer_rapport",
                 "exporter_gpkg", "annuler_calcul")

test_that("destructive_hint : TRUE pour appliquer_ugf et croiser_onf seulement", {
  for (nom in OUTILS_ATTENDUS)
    expect_identical(.an_hint(.an_outils()[[nom]], "destructive_hint"),
                     nom %in% DESTRUCTIFS, info = nom)
})

test_that("idempotent_hint : FALSE pour lancer_calcul seul, TRUE pour les 9 autres", {
  expect_length(IDEMPOTENTS, 9L)
  for (nom in OUTILS_ATTENDUS)
    expect_identical(.an_hint(.an_outils()[[nom]], "idempotent_hint"),
                     nom %in% IDEMPOTENTS, info = nom)
})

test_that("generer_rapport et exporter_gpkg : ecrivent, non destructifs, idempotents", {
  for (nom in c("generer_rapport", "exporter_gpkg")) {
    o <- .an_outils()[[nom]]
    expect_false(.an_hint(o, "read_only_hint"), info = nom)
    expect_false(.an_hint(o, "destructive_hint"), info = nom)
    expect_true(.an_hint(o, "idempotent_hint"), info = nom)
  }
})

test_that("les annotations sont transmises dans tools/list sous les noms MCP (mcptools 1.0.3)", {
  skip_if_not_installed("mcptools")
  conv <- getFromNamespace("tool_annotations_as_json", "mcptools")
  for (nom in OUTILS_ATTENDUS) {
    j <- conv(.an_outils()[[nom]]@annotations)
    expect_true(all(c("readOnlyHint", "destructiveHint", "idempotentHint") %in% names(j)),
                info = nom)
  }
  expect_true(conv(.an_outils()[["croiser_onf"]]@annotations)$destructiveHint)
  expect_true(conv(.an_outils()[["etat_calcul"]]@annotations)$readOnlyHint)
  expect_false(conv(.an_outils()[["lancer_calcul"]]@annotations)$idempotentHint)
})

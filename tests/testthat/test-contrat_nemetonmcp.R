# Contrat nemetonshiny -> nemetonmcp (pilotage/adr/0001, mission 001).
#
# `nemetonmcp` lit 32 objets internes par getFromNamespace(). CONTRAT.md,
# section « API pour nemetonmcp », les declare ; ce test les verrouille : un
# objet renomme, retire, ou dont les arguments changent fait echouer la CI
# ici, pas VICTOR en production. Changer cette table = changer le contrat :
# mettre CONTRAT.md a jour et prevenir nemetonmcp (brief).

# Fonctions : noms des arguments, dans l'ordre (formals()).
.contrat_fonctions <- list(
  # plan d'actions
  load_action_plan        = c("project_id"),
  save_action_plan        = c("project_id", "plan"),
  add_action_to_plan      = c("plan", "action", "ug_ids", "user"),
  update_action_in_plan   = c("plan", "action_id", "updates", "ug_ids", "user"),
  delete_action_from_plan = c("plan", "action_id", "user"),
  get_action_by_id        = c("plan", "action_id"),
  find_action_index       = c("plan", "action_id"),
  action_plan_annee_base  = c("plan"),
  load_ug_comments        = c("project_id"),
  # projet
  load_project            = c("project_id", "build_indicators_sf"),
  load_parcels            = c("project_id"),
  get_project_path        = c("project_id"),
  get_projects_root       = character(),
  ug_build_sf             = c("projet"),
  lock_status             = c("pid"),
  # communes
  get_commune_geometry    = c("code_insee"),
  save_commune_geometry   = c("project_id", "commune_geom"),
  get_departments         = character(),
  # MCP
  mcp_tools               = character(),
  mcp_etat_calcul         = c("projet"),
  mcp_exporter_gpkg       = c("projet"),
  .mcp_resolve_project    = c("projet"),
  .mcp_vers_outil         = c("x"),
  # divers
  get_expert_profiles     = character(),
  marculus_export_bundle  = c("project_id", "file", "essences")
)

# Constantes : type attendu (mode) et longueur minimale.
.contrat_constantes <- list(
  ACTION_PLAN_TYPES             = list(type = "character", n = 1L),
  ACTION_PLAN_STATUTS           = list(type = "character", n = 1L),
  ACTION_PLAN_PRIORITES         = list(type = "character", n = 1L),
  ACTION_PLAN_FAMILY_CODES      = list(type = "character", n = 12L),
  ACTION_PLAN_TRANSITIONS       = list(type = "list", n = 1L),
  MARCULUS_CONTEXT_ACTION_TYPES = list(type = "character", n = 1L),
  INDICATOR_FAMILIES            = list(type = "list", n = 12L)
)

.contrat_noms <- function() c(names(.contrat_fonctions), names(.contrat_constantes))

.contrat_ns <- function() asNamespace("nemetonshiny")

test_that("le contrat compte 32 objets, sans doublon", {
  expect_length(.contrat_noms(), 32L)
  expect_false(anyDuplicated(.contrat_noms()) > 0L)
})

test_that("chaque fonction du contrat existe avec les memes arguments", {
  ns <- .contrat_ns()
  for (nom in names(.contrat_fonctions)) {
    expect_true(exists(nom, envir = ns, inherits = FALSE),
                info = paste("objet absent :", nom))
    obj <- get(nom, envir = ns, inherits = FALSE)
    expect_true(is.function(obj), info = paste("pas une fonction :", nom))
    expect_identical(names(formals(obj)) %||% character(),
                     .contrat_fonctions[[nom]],
                     info = paste("signature changee :", nom))
  }
})

test_that("chaque constante du contrat existe, du bon type et non vide", {
  ns <- .contrat_ns()
  for (nom in names(.contrat_constantes)) {
    attendu <- .contrat_constantes[[nom]]
    expect_true(exists(nom, envir = ns, inherits = FALSE),
                info = paste("objet absent :", nom))
    obj <- get(nom, envir = ns, inherits = FALSE)
    expect_identical(typeof(obj), attendu$type, info = nom)
    expect_gte(length(obj), attendu$n)
  }
})

test_that("les familles du contrat portent les 12 codes de la convention NMT", {
  codes <- c("B", "C", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")
  ns <- .contrat_ns()
  expect_setequal(get("ACTION_PLAN_FAMILY_CODES", envir = ns), codes)
  expect_setequal(names(get("INDICATOR_FAMILIES", envir = ns)), codes)
})

test_that("CONTRAT.md declare exactement les objets verrouilles", {
  f <- testthat::test_path("..", "..", "CONTRAT.md")
  skip_if_not(file.exists(f), "CONTRAT.md absent (paquet installe)")
  txt <- readLines(f, encoding = "UTF-8", warn = FALSE)
  debut <- grep("^## .*API pour nemetonmcp", txt)
  expect_length(debut, 1L)
  reste <- txt[seq.int(debut + 1L, length(txt))]
  fin <- grep("^## ", reste)
  section <- if (length(fin)) reste[seq_len(fin[1] - 1L)] else reste
  # Premiere cellule de chaque ligne de tableau : `nom` ou `nom()`.
  lignes <- grep("^\\| *`", section, value = TRUE)
  declares <- sub("^\\| *`([^`(]+)(\\(\\))?.*$", "\\1", lignes)
  expect_setequal(declares, .contrat_noms())
  expect_false(anyDuplicated(declares) > 0L)
})

test_that("inst/mcp/README.md decrit les 10 outils servis par mcp_tools()", {
  f <- testthat::test_path("..", "..", "inst", "mcp", "README.md")
  skip_if_not(file.exists(f), "README MCP absent (paquet installe)")
  outils <- vapply(mcp_tools(), function(o) o@name, character(1))
  expect_length(outils, 10L)
  txt <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  for (o in outils) {
    expect_true(grepl(sprintf("`%s`", o), txt, fixed = TRUE),
                info = paste("outil absent du README :", o))
  }
  # La purge de croiser_onf est declaree.
  expect_match(txt, "purger", fixed = TRUE)
})

test_that("nemetonmcp n'appelle aucun objet hors contrat (si son depot est la)", {
  dir <- Sys.getenv("NEMETONMCP_R_DIR", "/home/pascal/dev/nemetonmcp/r/R")
  skip_if_not(dir.exists(dir), "depot nemetonmcp absent")
  appelles <- unique(unlist(lapply(
    list.files(dir, pattern = "\\.R$", full.names = TRUE), function(f) {
      x <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
      m <- regmatches(x, gregexpr('\\.ns\\("[^"]+"\\)', x))[[1]]
      sub('^\\.ns\\("([^"]+)"\\)$', "\\1", m)
    })))
  # Tout ce que nemetonmcp appelle est au contrat. L'inverse peut etre
  # transitoirement faux : .mcp_vers_outil est au contrat avant que
  # nemetonmcp l'appelle (brief mission 013).
  expect_length(setdiff(appelles, .contrat_noms()), 0L)
})

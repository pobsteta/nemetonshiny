# Tests de l'API hors interface (R/api.R) et de l'invalidation reversible.
# Brief : ~/dev/briefs/vers-nemetonshiny/2026-10-04-api-hors-interface-sans-effet-de-bord.md

.api_sq <- function(x) {
  sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0), c(x + 100, 100),
                            c(x, 100), c(x, 0))))
}

.api_parcelles <- function() {
  sf::st_sf(
    id = c("21001000AA0001", "21001000AA0002"), section = "AA",
    numero = c("0001", "0002"), contenance = c(10000, 10000),
    commune = "Test", code_insee = "21001",
    geometry = sf::st_sfc(.api_sq(800000), .api_sq(800100), crs = 2154)
  )
}

# Racine de projets temporaire + enrichissements R5/R6/R7 neutralises (R5 ouvre
# la base de suivi) + pas de synchronisation PostGIS.
.api_env <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_options(nemeton.app_options = list(project_dir = root),
                       .local_envir = env)
  local_mocked_bindings(
    add_r5_to_indicators = function(base_sf, project) base_sf,
    add_regen_r_indicators = function(base_sf, project) base_sf,
    is_db_configured = function() FALSE,
    .env = env
  )
  root
}

# Projet complet : parcelles, UGF, indicateurs calcules.
.api_projet_calcule <- function() {
  id <- suppressMessages(projet_creer("Foret API", .api_parcelles()))
  ugs <- load_ug_data(id)$ugs
  suppressMessages(save_indicators(id, data.frame(
    ug_id = ugs$ug_id,
    indicateur_b1_protection = c(40, 60),
    indicateur_c1_biomasse = c(70, 90)
  )))
  id
}

# Empreinte du dossier projet : chemin relatif -> md5 + mtime.
.api_empreinte <- function(id) {
  path <- get_project_path(id)
  f <- sort(list.files(path, recursive = TRUE, all.files = TRUE, full.names = TRUE))
  data.frame(fichier = sub(path, "", f, fixed = TRUE),
             md5 = unname(tools::md5sum(f)),
             mtime = as.numeric(file.mtime(f)),
             stringsAsFactors = FALSE)
}

.api_vieillir <- function(id, sens = 2L) {
  suppressMessages(update_project_metadata(id, list(indicator_sense_version = sens)))
}

# ==============================================================================
# Invalidation reversible
# ==============================================================================

test_that("invalidate_indicators sets the file aside instead of deleting it", {
  skip_if_not_installed("arrow")
  .api_env()
  id <- .api_projet_calcule()
  path <- get_project_path(id)
  .api_vieillir(id, 2L)
  original <- unname(tools::md5sum(file.path(path, "data", "indicators.parquet")))

  expect_true(suppressMessages(invalidate_indicators(id, motif = "sens")))

  expect_false(file.exists(file.path(path, "data", "indicators.parquet")))
  arch <- list.files(file.path(path, "data"), "^indicators\\.perime-")
  expect_length(arch, 1L)
  expect_match(arch, "^indicators\\.perime-v2-\\d{8}-\\d{6}\\.parquet$")
  # Le contenu est intact : c'est un renommage
  expect_identical(unname(tools::md5sum(file.path(path, "data", arch))), original)

  meta <- load_project_metadata(id)
  expect_false(meta$indicators_computed)
  expect_identical(meta$status, "draft")
  expect_length(meta$indicateurs_perimes, 1L)
  expect_identical(meta$indicateurs_perimes[[1]]$fichier, arch)
  expect_identical(meta$indicateurs_perimes[[1]]$sens, 2L)
  expect_identical(meta$indicateurs_perimes[[1]]$motif, "sens")
})

test_that("only the most recent stale generations are kept", {
  skip_if_not_installed("arrow")
  .api_env()
  id <- .api_projet_calcule()
  path <- get_project_path(id)
  ugs <- load_ug_data(id)$ugs
  for (k in 1:3) {
    suppressMessages(save_indicators(id, data.frame(
      ug_id = ugs$ug_id, indicateur_b1_protection = c(k, k))))
    suppressMessages(invalidate_indicators(id, motif = paste0("g", k)))
  }
  arch <- list.files(file.path(path, "data"), "^indicators\\.perime-")
  expect_length(arch, INDICATORS_STALE_KEEP)
  meta <- load_project_metadata(id)
  expect_identical(vapply(meta$indicateurs_perimes, `[[`, "", "motif"), c("g3", "g2"))
  expect_setequal(vapply(meta$indicateurs_perimes, `[[`, "", "fichier"), arch)
})

test_that("a failed rename keeps the indicators in place", {
  skip_if_not_installed("arrow")
  .api_env()
  id <- .api_projet_calcule()
  path <- get_project_path(id)
  vrai_rename <- base::file.rename
  local_mocked_bindings(file.rename = function(from, to) {
    if (all(basename(from) == "indicators.parquet")) FALSE else vrai_rename(from, to)
  }, .package = "base")
  expect_warning(res <- invalidate_indicators(id), "impossibles")
  expect_false(res)
  expect_true(file.exists(file.path(path, "data", "indicators.parquet")))
})

test_that("a project created then computed headless is not invalidated at load", {
  skip_if_not_installed("arrow")
  .api_env()
  id <- .api_projet_calcule()
  expect_identical(load_project_metadata(id)$indicator_sense_version,
                   INDICATOR_SENSE_VERSION)
  p <- suppressMessages(load_project(id))
  expect_false(isTRUE(p$indicators_invalidated))
  expect_true(file.exists(file.path(get_project_path(id), "data", "indicators.parquet")))
})

# ==============================================================================
# projet_etat / projets_lister
# ==============================================================================

test_that("projet_etat describes a computed, current project", {
  skip_if_not_installed("arrow")
  .api_env()
  id <- .api_projet_calcule()
  e <- projet_etat(id)
  expect_identical(e$id, id)
  expect_identical(e$nom, "Foret API")
  expect_identical(e$sens_vu, INDICATOR_SENSE_VERSION)
  expect_true(e$indicateurs)
  expect_true(e$ugf)
  expect_false(e$migration_necessaire)
})

test_that("projet_etat flags stale indicators and missing UGF, without writing", {
  skip_if_not_installed("arrow")
  .api_env()
  id <- .api_projet_calcule()
  .api_vieillir(id, 2L)
  file.remove(file.path(get_project_path(id), "data", "ugs.json"))
  avant <- .api_empreinte(id)
  e <- projet_etat(id)
  expect_true(e$indicateurs_perimes)
  expect_true(e$migration_ugf)
  expect_true(e$migration_necessaire)
  expect_identical(.api_empreinte(id), avant)
})

test_that("stale sense without indicators does not require a migration", {
  .api_env()
  id <- suppressMessages(projet_creer("Sans calcul", .api_parcelles()))
  .api_vieillir(id, 1L)
  e <- projet_etat(id)
  expect_false(e$indicateurs_perimes)
  expect_false(e$migration_necessaire)
})

test_that("unknown or unsafe ids raise a classed error", {
  .api_env()
  expect_error(projet_etat("inexistant"), class = "nemetonshiny_projet_introuvable")
  expect_error(projet_etat("../x"), class = "nemetonshiny_projet_introuvable")
  expect_error(projet_lire(NA_character_), class = "nemetonshiny_erreur")
})

test_that("projets_lister lists projects with their sense status", {
  skip_if_not_installed("arrow")
  .api_env()
  expect_identical(nrow(projets_lister()), 0L)
  a <- .api_projet_calcule()
  b <- suppressMessages(projet_creer("Autre", .api_parcelles()))
  .api_vieillir(a, 2L)
  l <- projets_lister()
  expect_setequal(l$id, c(a, b))
  expect_false(l$sens_a_jour[l$id == a])
  expect_true(l$sens_a_jour[l$id == b])
  expect_true(l$indicateurs[l$id == a])
  expect_false(l$indicateurs[l$id == b])
})

# ==============================================================================
# projet_lire : aucun effet de bord
# ==============================================================================

test_that("projet_lire on a stale project changes nothing and raises the classed error", {
  skip_if_not_installed("arrow")
  .api_env()
  id <- .api_projet_calcule()
  .api_vieillir(id, 2L)
  avant <- .api_empreinte(id)

  err <- expect_error(projet_lire(id), class = "nemetonshiny_projet_perime")
  expect_true(err$etat$indicateurs_perimes)
  expect_identical(err$etat$sens_vu, 2L)

  expect_identical(.api_empreinte(id), avant)
})

test_that("projet_lire on a project without UGF changes nothing", {
  skip_if_not_installed("arrow")
  .api_env()
  id <- .api_projet_calcule()
  file.remove(file.path(get_project_path(id), "data", "ugs.json"))
  avant <- .api_empreinte(id)
  err <- expect_error(projet_lire(id), class = "nemetonshiny_projet_perime")
  expect_true(err$etat$migration_ugf)
  expect_identical(.api_empreinte(id), avant)
})

test_that("projet_lire returns the Synthesis figures without writing", {
  skip_if_not_installed("arrow")
  .api_env()
  id <- .api_projet_calcule()
  avant <- .api_empreinte(id)

  lu <- suppressMessages(projet_lire(id, langue = "en"))

  expect_identical(.api_empreinte(id), avant)
  expect_s3_class(lu$indicateurs, "sf")
  expect_identical(nrow(lu$indicateurs), 2L)
  expect_s3_class(lu$familles, "sf")
  expect_true(all(c("famille_biodiversite", "famille_carbone") %in% names(lu$familles)))
  # Memes chiffres que le chemin de l'application (load_project + service)
  ref <- suppressMessages(project_synthesis_summary(load_project(id), "en"))
  expect_identical(lu$synthese$global_score, ref$global_score)
  expect_identical(lu$synthese$families, ref$families)
  expect_false(is.na(lu$synthese$global_score))
})

test_that("projet_lire on a project never computed returns empty results", {
  .api_env()
  id <- suppressMessages(projet_creer("Vide", .api_parcelles()))
  lu <- projet_lire(id)
  expect_null(lu$indicateurs)
  expect_null(lu$familles)
  expect_true(is.na(lu$synthese$global_score))
})

# ==============================================================================
# projet_migrer / projet_creer
# ==============================================================================

test_that("projet_migrer sets stale indicators aside and makes the project readable", {
  skip_if_not_installed("arrow")
  .api_env()
  id <- .api_projet_calcule()
  .api_vieillir(id, 2L)
  e <- suppressWarnings(suppressMessages(projet_migrer(id)))
  expect_false(e$migration_necessaire)
  expect_false(e$indicateurs)
  expect_identical(e$sens_vu, INDICATOR_SENSE_VERSION)
  expect_length(e$archives, 1L)
  expect_true(file.exists(file.path(e$chemin, "data", e$archives[[1]]$fichier)))
  expect_no_error(projet_lire(id))
})

test_that("projet_creer initialises UGF and the current sense", {
  .api_env()
  id <- suppressMessages(projet_creer("Neuf", .api_parcelles(),
                                      description = "d", proprietaire = "o"))
  e <- projet_etat(id)
  expect_true(e$ugf)
  expect_identical(e$sens_vu, INDICATOR_SENSE_VERSION)
  expect_false(e$migration_necessaire)
  meta <- load_project_metadata(id)
  expect_identical(meta$owner, "o")
  expect_error(projet_creer("x", data.frame()), "sf")
})

# ==============================================================================
# parcelles_commune
# ==============================================================================

test_that("parcelles_commune filters and reports missing ids", {
  local_mocked_bindings(get_cadastral_parcels = function(code_insee, ...) .api_parcelles())
  expect_identical(nrow(parcelles_commune("21001")), 2L)
  expect_identical(parcelles_commune("21001", "21001000AA0002")$id, "21001000AA0002")
  err <- expect_error(parcelles_commune("21001", c("21001000AA0002", "nope")),
                      class = "nemetonshiny_parcelles_introuvables")
  expect_identical(err$manquants, "nope")
})

# ==============================================================================
# projet_calculer
# ==============================================================================

test_that("projet_calculer refuses a project to migrate", {
  skip_if_not_installed("arrow")
  .api_env()
  id <- .api_projet_calcule()
  .api_vieillir(id, 2L)
  appele <- FALSE
  local_mocked_bindings(start_computation = function(...) { appele <<- TRUE; list(success = TRUE) })
  expect_error(projet_calculer(id), class = "nemetonshiny_projet_perime")
  expect_false(appele)
})

test_that("projet_calculer forwards to start_computation and classes failures", {
  .api_env()
  id <- suppressMessages(projet_creer("Calcul", .api_parcelles()))
  recu <- NULL
  local_mocked_bindings(start_computation = function(project_id, indicators,
                                                     progress_callback, use_file_progress) {
    recu <<- list(id = project_id, ind = indicators, cb = progress_callback,
                  file = use_file_progress)
    list(success = TRUE)
  })
  cb <- function(s) NULL
  res <- projet_calculer(id, indicateurs = c("B1", "C1"), progression = cb)
  expect_true(res$success)
  expect_identical(recu$id, id)
  expect_identical(recu$ind, c("B1", "C1"))
  expect_identical(recu$cb, cb)
  expect_true(recu$file)

  expect_error(projet_calculer(id, progression = "x"), "fonction")

  local_mocked_bindings(start_computation = function(...) list(success = FALSE, error = "boom"))
  err <- expect_error(projet_calculer(id), class = "nemetonshiny_calcul_echec")
  expect_match(conditionMessage(err), "boom")
})

# ==============================================================================
# projet_rapport / projet_gpkg
# ==============================================================================

test_that("projet_rapport validates comments and passes them to the report", {
  skip_if_not_installed("arrow")
  .api_env()
  id <- .api_projet_calcule()
  out <- file.path(withr::local_tempdir(), "sous", "rapport.pdf")
  recu <- NULL
  local_mocked_bindings(generate_report_pdf = function(project, family_scores, output_file,
                                                       language, synthesis_comments,
                                                       family_comments, ...) {
    recu <<- list(project = project, fs = family_scores, lang = language,
                  syn = synthesis_comments, fam = family_comments)
    writeLines("pdf", output_file)
    output_file
  })

  expect_error(projet_rapport(id, out, familles = list(ZZ = "x")), "code de famille")
  expect_error(projet_rapport(id, out, familles = list("x")), "code de famille")
  expect_error(projet_rapport(id, out, synthese = c("a", "b")), "cha")

  avant <- .api_empreinte(id)
  sources <- "[^1]: Auteur, Titre, p. 3. <https://exemple.org>"
  f <- suppressMessages(projet_rapport(
    id, out, langue = "en", synthese = "Texte[^1].",
    familles = list(B = "Biodiv[^1].", C = "  "), sources = sources))
  expect_identical(f, normalizePath(out))
  expect_true(file.exists(out))
  expect_identical(.api_empreinte(id), avant)
  expect_identical(recu$lang, "en")
  expect_identical(recu$project$id, id)
  expect_s3_class(recu$fs, "sf")
  # Commentaires vides retires ; notes resolues comme dans l'application
  expect_identical(names(recu$fam), "B")
  expect_identical(recu$syn, .prepare_footnotes("Texte[^1].", sources))
  expect_identical(recu$fam$B, .prepare_family_footnotes("Biodiv[^1].", sources, "B"))
})

test_that("exports need computed indicators", {
  .api_env()
  id <- suppressMessages(projet_creer("Sans", .api_parcelles()))
  out <- file.path(withr::local_tempdir(), "x.gpkg")
  expect_error(projet_gpkg(id, out), class = "nemetonshiny_sans_indicateurs")
  expect_error(projet_rapport(id, sub("gpkg$", "pdf", out)),
               class = "nemetonshiny_sans_indicateurs")
})

test_that("projet_gpkg writes one feature per management unit", {
  skip_if_not_installed("arrow")
  .api_env()
  id <- .api_projet_calcule()
  out <- file.path(withr::local_tempdir(), "res", "x.gpkg")
  avant <- .api_empreinte(id)
  f <- suppressMessages(projet_gpkg(id, out))
  expect_identical(.api_empreinte(id), avant)
  g <- sf::st_read(f, quiet = TRUE)
  expect_identical(nrow(g), 2L)
  expect_true("famille_biodiversite" %in% names(g))
})

# ==============================================================================
# Dossier des projets hors interface
# ==============================================================================

test_that("NEMETON_PROJECT_DIR sets the default projects directory", {
  d <- withr::local_tempdir()
  withr::local_envvar(NEMETON_PROJECT_DIR = d)
  withr::local_options(nemeton.app_options = NULL)
  expect_identical(get_default_project_dir(), d)
  expect_identical(get_projects_root(), normalizePath(d))
  withr::local_envvar(NEMETON_PROJECT_DIR = "")
  expect_false(identical(get_default_project_dir(), d))
})

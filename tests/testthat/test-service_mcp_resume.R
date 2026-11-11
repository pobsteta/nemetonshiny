# Mission 006 - critere 2 de pilotage/spec.md : `resume_projet` rend tout ce
# qu'affiche l'onglet Synthese (R/mod_synthesis.R : nom, description, creation,
# nb UGF / parcelles, statut, avertissement CHM, score global, tableau des 12
# familles : score, NDP, nb d'indicateurs), a ecart 0, plus les heures ISO 8601
# avec fuseau. Ecrit sans lire l'implementation de mcp_resume_projet.
#
# Noms de champs valides par Pascal (2026-11-10) :
#   description, created_at, updated_at, calcule_le, chm_suspect,
#   chm_suspect_max, familles$n_indicateurs.
# `updated_at` existe DEJA dans la synthese actuelle (project_synthesis_summary) ;
#   les autres champs sont nouveaux. Conserves : project_id, name, status,
#   ndp_level, global_score, n_ugf, n_parcels, familles{code,famille,score}.
# Heures : ISO 8601 avec fuseau Europe/Paris FIXE (meme resultat sur un serveur
#   en UTC). `calcule_le` = vraie fin de calcul : `completed_at` de
#   data/progress_state.json ou `fin_a` de data/compute_job.json quand presents,
#   repli sur la date de data/indicators.parquet, NA si non calcule.
#   Priorite (arbitrage 2026-11-10, suite) : completed_at > fin_a SEULEMENT si
#   statut == "termine" > date du parquet > NA.
# Les heures naives des fichiers projet (created_at, updated_at, completed_at,
#   fin_a) se lisent dans le fuseau du process lecteur, puis sont rendues en
#   Europe/Paris.

.rs_sq <- function(x) {
  sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0), c(x + 100, 100),
                            c(x, 100), c(x, 0))))
}
.rs_projet <- function(env = parent.frame(), calcule = TRUE) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_options(nemeton.app_options = list(project_dir = root),
                       .local_envir = env)
  withr::local_timezone("Europe/Paris", .local_envir = env)
  local_mocked_bindings(
    add_r5_to_indicators = function(base_sf, project) base_sf,
    add_regen_r_indicators = function(base_sf, project) base_sf,
    is_db_configured = function() FALSE,
    lock_status = function(pid) NULL,
    .env = env
  )
  parcelles <- sf::st_sf(
    id = c("21001000AA0001", "21001000AA0002"), section = "AA",
    numero = c("0001", "0002"), contenance = c(1e4, 1e4),
    commune = "Test", code_insee = "21001",
    geometry = sf::st_sfc(.rs_sq(800000), .rs_sq(800100), crs = 2154))
  id <- suppressMessages(projet_creer("Forêt de Dabo", parcelles))
  if (calcule) {
    ugs <- load_ug_data(id)$ugs
    suppressMessages(save_indicators(id, data.frame(
      ug_id = ugs$ug_id, indicateur_b1_protection = c(40, 60),
      indicateur_c1_biomasse = c(70, 90))))
  }
  id
}
# Impose des metadonnees connues (heures locales Europe/Paris, hiver et ete).
.rs_meta <- function(id, ...) {
  f <- file.path(get_project_path(id), "metadata.json")
  m <- jsonlite::read_json(f)
  m$created_at <- "2026-01-15 08:30:00"
  m$updated_at <- "2026-07-15 09:45:10"
  m$description <- "Description du projet de test"
  for (k in names(list(...))) m[[k]] <- list(...)[[k]]
  jsonlite::write_json(m, f, auto_unbox = TRUE, pretty = TRUE)
}
.rs_lit <- function(json) jsonlite::fromJSON(json, simplifyVector = TRUE)
.rs_iso <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$"
.rs_data <- function(id, f) file.path(get_project_path(id), "data", f)
# Reference = ce que calcule l'onglet Synthese (service_synthesis.R, extrait du module).
.rs_ref <- function(id) {
  p <- suppressMessages(load_project(id))
  fam <- project_family_scores(p)
  moy <- project_family_means(fam)
  list(moy = moy,
       global = project_global_index(moy, project_ndp_level(p))$score,
       ndp = project_ndp_level(p))
}
.rs_codes <- c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")

test_that("resume_projet : ecart 0 avec la Synthese (score global, NDP, UGF, parcelles, 12 scores arrondis a 2 decimales)", {
  skip_if_not_installed("arrow")
  id <- .rs_projet()
  r <- .rs_lit(mcp_resume_projet(id))
  ref <- .rs_ref(id)

  expect_true(r$ok)
  expect_identical(r$name, "Forêt de Dabo")
  expect_true("famille" %in% names(r$familles))
  expect_equal(r$global_score, ref$global, tolerance = 1e-8)
  expect_identical(r$ndp_level, ref$ndp)
  expect_identical(r$n_ugf, 2L)
  expect_identical(r$n_parcels, 2L)
  expect_identical(nrow(r$familles), 12L)
  expect_setequal(r$familles$code, .rs_codes)
  for (code in .rs_codes) {
    col <- get_famille_col(code)
    attendu <- if (col %in% names(ref$moy)) round(unname(ref$moy[[col]]), 2) else NA_real_
    obtenu <- r$familles$score[r$familles$code == code]
    if (is.na(attendu)) expect_true(is.na(obtenu), label = paste("score", code, "NA"))
    else expect_equal(obtenu, attendu, tolerance = 1e-8, label = paste("score", code))
  }
  # Nom de famille affiche par la Synthese (name_fr, langue par defaut fr).
  for (code in .rs_codes) {
    expect_identical(r$familles$famille[r$familles$code == code],
                     INDICATOR_FAMILIES[[code]]$name_fr, label = paste("nom", code))
  }
  # Au moins un score present : le test ne doit pas passer sur 12 NA.
  expect_true(any(!is.na(r$familles$score)))
})

test_that("resume_projet : NDP par famille = metadata$ndp_level %||% 0, comme le tableau de la Synthese", {
  skip_if_not_installed("arrow")
  # Sans ndp_level dans les metadonnees : 0 partout.
  id0 <- .rs_projet()
  r0 <- .rs_lit(mcp_resume_projet(id0))
  expect_true("ndp" %in% names(r0$familles))
  expect_identical(as.integer(r0$familles$ndp), rep(0L, 12L))
  expect_identical(r0$ndp_level, .rs_ref(id0)$ndp)

  # Avec ndp_level = 2 : la valeur sort au niveau projet ET sur chaque ligne de famille.
  id <- .rs_projet()
  .rs_meta(id, ndp_level = 2L)
  expect_identical(.rs_ref(id)$ndp, 2L)
  r <- .rs_lit(mcp_resume_projet(id))
  expect_identical(r$ndp_level, 2L)
  expect_true("ndp" %in% names(r$familles))
  expect_identical(as.integer(r$familles$ndp), rep(2L, 12L))
})

test_that("resume_projet : description, statut, nb d'indicateurs des 12 familles (present ou, a defaut, prevu)", {
  skip_if_not_installed("arrow")
  id <- .rs_projet()
  .rs_meta(id)
  r <- .rs_lit(mcp_resume_projet(id))
  expect_identical(r$description, "Description du projet de test")
  expect_identical(r$status, "completed")
  expect_true("n_indicateurs" %in% names(r$familles))
  expect_true(is.integer(r$familles$n_indicateurs))
  expect_identical(length(r$familles$n_indicateurs), 12L)
  n <- stats::setNames(r$familles$n_indicateurs, r$familles$code)
  # Familles avec un indicateur calcule : le compte des presents (b1, c1).
  expect_identical(n[["B"]], 1L)
  expect_identical(n[["C"]], 1L)
  # Les 10 autres n'ont aucun indicateur calcule : la Synthese (present_count,
  # R/mod_synthesis.R) remplace alors 0 par le nombre PREVU par la config.
  for (code in setdiff(.rs_codes, c("B", "C"))) {
    prevu <- length(INDICATOR_FAMILIES[[code]]$indicators)
    expect_gt(prevu, 0L)
    expect_identical(n[[code]], as.integer(prevu), label = paste("n_indicateurs", code))
  }
  # Garde : le repli est distinguable du present (la famille B prevoit plus d'un indicateur).
  expect_gt(length(INDICATOR_FAMILIES[["B"]]$indicators), 1L)
})

test_that("resume_projet : heures en ISO 8601 avec fuseau Europe/Paris, valeurs exactes (hiver +01:00, ete +02:00)", {
  skip_if_not_installed("arrow")
  id <- .rs_projet()
  .rs_meta(id)
  r <- .rs_lit(mcp_resume_projet(id))
  expect_identical(r$created_at, "2026-01-15T08:30:00+01:00")
  expect_identical(r$updated_at, "2026-07-15T09:45:10+02:00")
})

test_that("resume_projet : date de calcul = date du fichier d'indicateurs en repli, Paris meme sur un serveur en UTC", {
  skip_if_not_installed("arrow")
  id <- .rs_projet()
  .rs_meta(id)
  f <- .rs_data(id, "indicators.parquet")
  # Pas d'etat de calcul : le repli sur le fichier s'applique.
  unlink(c(.rs_data(id, "progress_state.json"), .rs_data(id, "compute_job.json")))
  Sys.setFileTime(f, as.POSIXct("2026-03-20 14:05:30", tz = "Europe/Paris"))
  r <- .rs_lit(mcp_resume_projet(id))
  expect_identical(r$calcule_le, "2026-03-20T14:05:30+01:00")

  # Meme instant, serveur en UTC (13:05:30 UTC) : toujours l'heure de Paris.
  withr::local_timezone("UTC")
  r_utc <- .rs_lit(mcp_resume_projet(id))
  expect_identical(r_utc$calcule_le, "2026-03-20T14:05:30+01:00")

  # Ete : +02:00.
  Sys.setFileTime(f, as.POSIXct("2026-07-20 14:05:30", tz = "Europe/Paris"))
  r_ete <- .rs_lit(mcp_resume_projet(id))
  expect_identical(r_ete$calcule_le, "2026-07-20T14:05:30+02:00")
})

test_that("resume_projet : calcule_le = completed_at de progress_state.json quand present (pas la date du fichier)", {
  skip_if_not_installed("arrow")
  id <- .rs_projet()
  .rs_meta(id)
  unlink(.rs_data(id, "compute_job.json"))
  Sys.setFileTime(.rs_data(id, "indicators.parquet"),
                  as.POSIXct("2026-03-20 14:05:30", tz = "Europe/Paris"))
  jsonlite::write_json(list(project_id = id, status = "completed",
                            started_at = "2026-03-21 09:00:00",
                            completed_at = "2026-03-21 10:00:00"),
                       .rs_data(id, "progress_state.json"), auto_unbox = TRUE)
  r <- .rs_lit(mcp_resume_projet(id))
  expect_identical(r$calcule_le, "2026-03-21T10:00:00+01:00")
})

test_that("resume_projet : calcule_le = fin_a de compute_job.json quand present et sans progress_state", {
  skip_if_not_installed("arrow")
  id <- .rs_projet()
  .rs_meta(id)
  unlink(.rs_data(id, "progress_state.json"))
  Sys.setFileTime(.rs_data(id, "indicators.parquet"),
                  as.POSIXct("2026-03-20 14:05:30", tz = "Europe/Paris"))
  jsonlite::write_json(list(job_id = "j1", statut = "termine", source = "mcp",
                            lance_a = "2026-07-22T10:00:00",
                            fin_a = "2026-07-22T11:15:00"),
                       .rs_data(id, "compute_job.json"), auto_unbox = TRUE)
  r <- .rs_lit(mcp_resume_projet(id))
  expect_identical(r$calcule_le, "2026-07-22T11:15:00+02:00")
})

test_that("resume_projet : calcule_le, completed_at et fin_a coexistent : completed_at gagne", {
  skip_if_not_installed("arrow")
  id <- .rs_projet()
  .rs_meta(id)
  Sys.setFileTime(.rs_data(id, "indicators.parquet"),
                  as.POSIXct("2026-03-20 14:05:30", tz = "Europe/Paris"))
  jsonlite::write_json(list(project_id = id, status = "completed",
                            completed_at = "2026-03-21 10:00:00"),
                       .rs_data(id, "progress_state.json"), auto_unbox = TRUE)
  jsonlite::write_json(list(job_id = "j1", statut = "termine", source = "mcp",
                            fin_a = "2026-07-22T11:15:00"),
                       .rs_data(id, "compute_job.json"), auto_unbox = TRUE)
  r <- .rs_lit(mcp_resume_projet(id))
  expect_identical(r$calcule_le, "2026-03-21T10:00:00+01:00")
})

test_that("resume_projet : calcule_le, fin_a d'un job en echec est ignore, repli sur le parquet", {
  skip_if_not_installed("arrow")
  id <- .rs_projet()
  .rs_meta(id)
  unlink(.rs_data(id, "progress_state.json"))
  Sys.setFileTime(.rs_data(id, "indicators.parquet"),
                  as.POSIXct("2026-03-20 14:05:30", tz = "Europe/Paris"))
  jsonlite::write_json(list(job_id = "j2", statut = "echec", source = "mcp",
                            lance_a = "2026-07-22T10:00:00",
                            fin_a = "2026-07-22T11:15:00"),
                       .rs_data(id, "compute_job.json"), auto_unbox = TRUE)
  r <- .rs_lit(mcp_resume_projet(id))
  expect_identical(r$calcule_le, "2026-03-20T14:05:30+01:00")
})

test_that("resume_projet : fichiers ecrits sous UTC et lus sous UTC, heures rendues en Europe/Paris", {
  skip_if_not_installed("arrow")
  id <- .rs_projet()
  withr::with_timezone("UTC", {
    .rs_meta(id)   # created_at 2026-01-15 08:30:00 UTC, updated_at 2026-07-15 09:45:10 UTC
    unlink(.rs_data(id, "compute_job.json"))
    jsonlite::write_json(list(project_id = id, status = "completed",
                              completed_at = "2026-03-21 10:00:00"),
                         .rs_data(id, "progress_state.json"), auto_unbox = TRUE)
    r <- .rs_lit(mcp_resume_projet(id))
    # 08:30 UTC = 09:30 Paris (hiver) ; 09:45:10 UTC = 11:45:10 Paris (ete) ;
    # 10:00 UTC le 21 mars = 11:00 Paris (+01:00, avant le passage a l'heure d'ete).
    expect_identical(r$created_at, "2026-01-15T09:30:00+01:00")
    expect_identical(r$updated_at, "2026-07-15T11:45:10+02:00")
    expect_identical(r$calcule_le, "2026-03-21T11:00:00+01:00")
  })
})

test_that("resume_projet : projet jamais calcule, cles presentes a null dans le JSON brut", {
  skip_if_not_installed("arrow")
  id <- .rs_projet(calcule = FALSE)
  json <- as.character(mcp_resume_projet(id))
  # Sur la chaine brute : la cle existe ET vaut null (ni absente, ni "NA", ni inventee).
  expect_match(json, '"calcule_le"\\s*:\\s*null')
  expect_match(json, '"global_score"\\s*:\\s*null')
  r <- .rs_lit(json)
  expect_true(r$ok)
  expect_type(r$created_at, "character")
  expect_match(r$created_at, .rs_iso)
})

test_that("resume_projet : avertissement CHM suspect rendu comme la Synthese", {
  skip_if_not_installed("arrow")
  id <- .rs_projet()
  .rs_meta(id, chm_suspect = TRUE, chm_suspect_max = 3.2)
  r <- .rs_lit(mcp_resume_projet(id))
  expect_true(isTRUE(r$chm_suspect))
  expect_equal(r$chm_suspect_max, 3.2)

  id2 <- .rs_projet()
  r2 <- .rs_lit(mcp_resume_projet(id2))
  expect_identical(r2$chm_suspect, FALSE)
})

test_that("resume_projet : lecture seule, aucun fichier du projet modifie", {
  skip_if_not_installed("arrow")
  id <- .rs_projet()
  .rs_meta(id)
  fs <- list.files(get_project_path(id), recursive = TRUE, full.names = TRUE)
  avant <- tools::md5sum(fs)
  invisible(mcp_resume_projet(id))
  expect_identical(tools::md5sum(fs), avant)
  expect_identical(list.files(get_project_path(id), recursive = TRUE, full.names = TRUE), fs)
})

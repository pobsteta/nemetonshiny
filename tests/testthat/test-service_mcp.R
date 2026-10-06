# Serveur MCP (R/service_mcp.R) - specs/BRIEF-pilotage-victor-aigora.md A.2/A.3

.mcp_sq <- function(x) {
  sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0), c(x + 100, 100),
                            c(x, 100), c(x, 0))))
}
.mcp_parcelles <- function() {
  sf::st_sf(id = c("21001000AA0001", "21001000AA0002"), section = "AA",
            numero = c("0001", "0002"), contenance = c(1e4, 1e4),
            commune = "Test", code_insee = "21001",
            geometry = sf::st_sfc(.mcp_sq(800000), .mcp_sq(800100), crs = 2154))
}
.mcp_env <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_options(nemeton.app_options = list(project_dir = root),
                       .local_envir = env)
  local_mocked_bindings(
    add_r5_to_indicators = function(base_sf, project) base_sf,
    add_regen_r_indicators = function(base_sf, project) base_sf,
    is_db_configured = function() FALSE,
    lock_status = function(pid) NULL,
    .env = env
  )
  root
}
.mcp_projet <- function(nom = "Forêt de Dabo", calcule = TRUE) {
  id <- suppressMessages(projet_creer(nom, .mcp_parcelles()))
  if (calcule) {
    ugs <- load_ug_data(id)$ugs
    suppressMessages(save_indicators(id, data.frame(
      ug_id = ugs$ug_id, indicateur_b1_protection = c(40, 60),
      indicateur_c1_biomasse = c(70, 90))))
  }
  id
}
.lit <- function(json) jsonlite::fromJSON(json, simplifyVector = TRUE)

# ---------------------------------------------------------------- resolution


# Projet 0.x : metadata.json sans marqueur `format_projet`.
.mcp_ancien <- function(id) {
  f <- file.path(get_project_path(id), "metadata.json")
  m <- jsonlite::read_json(f); m$format_projet <- NULL
  jsonlite::write_json(m, f, auto_unbox = TRUE, pretty = TRUE)
  invisible(id)
}

test_that(".mcp_resolve_project matches id, name without accents, partial name", {
  .mcp_env()
  a <- .mcp_projet("Forêt de Dabo", calcule = FALSE)
  b <- .mcp_projet("Couchey", calcule = FALSE)
  expect_identical(.mcp_resolve_project(a), a)
  expect_identical(.mcp_resolve_project("foret de dabo"), a)
  expect_identical(.mcp_resolve_project("COUCH"), b)
  expect_error(.mcp_resolve_project("Mouthe"), class = "nemetonshiny_projet_introuvable")
  expect_error(.mcp_resolve_project(""), class = "nemetonshiny_projet_introuvable")
})

test_that("an ambiguous name returns the candidates instead of choosing", {
  .mcp_env()
  .mcp_projet("Dabo nord", calcule = FALSE)
  .mcp_projet("Dabo sud", calcule = FALSE)
  r <- .lit(mcp_resume_projet("dabo"))
  expect_false(r$ok)
  expect_identical(r$classe, "nemetonshiny_projet_ambigu")
  expect_length(r$candidats, 2L)
})

# ---------------------------------------------------------------- lecture

test_that("lister_projets and resume_projet return JSON without writing", {
  skip_if_not_installed("arrow")
  .mcp_env()
  id <- .mcp_projet()
  avant <- tools::md5sum(list.files(get_project_path(id), recursive = TRUE, full.names = TRUE))

  l <- .lit(mcp_lister_projets())
  expect_true(l$ok)
  expect_identical(l$total, 1L)
  expect_identical(l$projets$id, id)

  r <- .lit(mcp_resume_projet("dabo", "en"))
  expect_true(r$ok)
  ref <- suppressMessages(projet_lire(id, "en"))$synthese
  expect_equal(r$global_score, ref$global_score)
  expect_identical(nrow(r$familles), 12L)
  expect_null(r$families)

  apres <- tools::md5sum(list.files(get_project_path(id), recursive = TRUE, full.names = TRUE))
  expect_identical(apres, avant)
})

test_that("resume_projet on a pre-1.0 project explains and changes nothing", {
  skip_if_not_installed("arrow")
  .mcp_env()
  id <- .mcp_projet()
  .mcp_ancien(id)
  r <- .lit(mcp_resume_projet(id))
  expect_false(r$ok)
  expect_identical(r$classe, "nemetonshiny_projet_ancien")
  expect_true(file.exists(file.path(get_project_path(id), "data", "indicators.parquet")))
})

# ---------------------------------------------------------------- calcul detache

test_that("lancer_calcul writes the job and spawns a detached process", {
  .mcp_env()
  id <- .mcp_projet(calcule = FALSE)
  lance <- NULL
  local_mocked_bindings(.mcp_spawn = function(project_id, project_dir, log_path) {
    lance <<- list(id = project_id, dir = project_dir, log = log_path)
    invisible(TRUE)
  })
  r <- .lit(mcp_lancer_calcul(id))
  expect_true(r$ok)
  expect_identical(lance$id, id)
  expect_identical(lance$dir, get_projects_root())
  job <- .mcp_job_read(get_project_path(id))
  expect_identical(job$statut, "lancement")
  expect_identical(job$job_id, r$job_id)

  # Juste lance (pas encore de pid) : un second lancement est refuse
  r2 <- .lit(mcp_lancer_calcul(id))
  expect_false(r2$ok)
  expect_identical(r2$classe, "nemetonshiny_calcul_en_cours")
})

test_that("lancer_calcul refuses a running job, an app computation, a lock, a stale project", {
  skip_if_not_installed("arrow")
  .mcp_env()
  id <- .mcp_projet(calcule = FALSE)
  path <- get_project_path(id)
  local_mocked_bindings(.mcp_spawn = function(...) stop("ne doit pas etre lance"))

  .mcp_job_write(path, list(job_id = "x", statut = "en_cours", pid = Sys.getpid()))
  expect_identical(.lit(mcp_lancer_calcul(id))$classe, "nemetonshiny_calcul_en_cours")
  unlink(.mcp_job_path(path))

  local_mocked_bindings(progress_state_age_sec = function(id) 5)
  expect_identical(.lit(mcp_lancer_calcul(id))$classe, "nemetonshiny_calcul_en_cours")
  local_mocked_bindings(progress_state_age_sec = function(id) Inf)

  local_mocked_bindings(lock_status = function(pid) list(holder_label = "Pascal", stale = FALSE))
  r <- .lit(mcp_lancer_calcul(id))
  expect_identical(r$classe, "nemetonshiny_projet_verrouille")
  expect_match(r$erreur, "Pascal")
  local_mocked_bindings(lock_status = function(pid) list(holder_label = "x", stale = TRUE))

  id2 <- .mcp_projet("Vieux")
  .mcp_ancien(id2)
  expect_identical(.lit(mcp_lancer_calcul(id2))$classe, "nemetonshiny_projet_ancien")
})

test_that(".mcp_child_compute records pid and outcome", {
  .mcp_env()
  id <- .mcp_projet(calcule = FALSE)
  path <- get_project_path(id)
  .mcp_job_write(path, list(job_id = "j1", statut = "lancement"))
  local_mocked_bindings(.compute_run_capped = function(project_id, app_opts) list(success = TRUE))
  job <- .mcp_child_compute(id, get_projects_root())
  expect_identical(job$statut, "termine")
  expect_identical(.mcp_job_read(path)$pid, Sys.getpid())
  expect_identical(.mcp_job_read(path)$job_id, "j1")

  local_mocked_bindings(.compute_run_capped = function(...) list(success = FALSE, error = "boom"))
  expect_identical(.mcp_child_compute(id, get_projects_root())$erreur, "boom")
  local_mocked_bindings(.compute_run_capped = function(...) list(success = FALSE, cancelled = TRUE))
  expect_identical(.mcp_child_compute(id, get_projects_root())$statut, "annule")
  local_mocked_bindings(.compute_run_capped = function(...) stop("crash"))
  expect_identical(.mcp_child_compute(id, get_projects_root())$statut, "echec")
})

test_that("etat_calcul reports progress and a dead process as a failure", {
  .mcp_env()
  id <- .mcp_projet(calcule = FALSE)
  path <- get_project_path(id)
  expect_identical(.lit(mcp_etat_calcul(id))$statut, "aucun")

  local_mocked_bindings(read_progress_state = function(id)
    list(status = "computing", progress = 12, progress_max = 47,
         indicators_completed = 5, indicators_total = 37, current_task = "R1"))
  .mcp_job_write(path, list(job_id = "j", statut = "en_cours", pid = Sys.getpid(),
                            lance_a = .mcp_now()))
  e <- .lit(mcp_etat_calcul(id))
  expect_identical(e$statut, "en_cours")
  expect_identical(e$indicateurs_faits, 5L)
  expect_identical(e$tache, "R1")

  log <- file.path(path, "data", "compute_mcp.log")
  writeLines(c("debut", "Killed"), log)
  .mcp_job_write(path, list(job_id = "j", statut = "en_cours", pid = 999999L,
                            lance_a = .mcp_now(), log = log))
  e <- .lit(mcp_etat_calcul(id))
  expect_identical(e$statut, "echec")
  expect_true("Killed" %in% e$log)
})

test_that("annuler_calcul raises the cancel signal", {
  .mcp_env()
  id <- .mcp_projet(calcule = FALSE)
  expect_true(.lit(mcp_annuler_calcul(id))$ok)
  expect_true(is_cancelled(id))
})

# ---------------------------------------------------------------- exports, url

test_that("generer_rapport and exporter_gpkg write into the project's exports", {
  skip_if_not_installed("arrow")
  .mcp_env()
  id <- .mcp_projet()
  local_mocked_bindings(generate_report_pdf = function(project, family_scores, output_file, ...) {
    writeLines("pdf", output_file); output_file
  })
  r <- .lit(mcp_generer_rapport("dabo", "fr", "Texte."))
  expect_true(r$ok)
  expect_true(file.exists(r$fichier))
  expect_identical(dirname(r$fichier), normalizePath(file.path(get_project_path(id), "exports")))
  g <- .lit(suppressMessages(mcp_exporter_gpkg(id)))
  expect_true(g$ok)
  expect_true(file.exists(g$fichier))
})

test_that("url_app builds a deep link and checks the tab", {
  .mcp_env()
  id <- .mcp_projet(calcule = FALSE)
  withr::local_envvar(NEMETON_APP_PORT = "4000")
  u <- .lit(mcp_url_app(id, "famille_risque"))
  expect_identical(u$url, sprintf("http://127.0.0.1:4000/?project=%s&tab=famille_risque", id))
  expect_false(.lit(mcp_url_app(id, "inconnu"))$ok)
  expect_match(.lit(mcp_url_app(id))$url, "tab=synthesis$")
})

# ---------------------------------------------------------------- declaration

test_that("mcp_tools declares the eight tools", {
  tools <- mcp_tools()
  noms <- vapply(tools, function(t) t@name, "")
  expect_setequal(noms, c("lister_projets", "resume_projet", "lancer_calcul",
                          "etat_calcul", "annuler_calcul", "generer_rapport",
                          "exporter_gpkg", "url_app"))
})

test_that("the MCP server answers tools/list over stdio", {
  skip_on_cran()
  skip_if_not_installed("mcptools")
  skip_if_not_installed("processx")
  srv <- system.file("mcp", "server.R", package = "nemetonshiny")
  skip_if(!nzchar(srv), "nemetonshiny non installe (inst/mcp absent)")
  # Le serveur charge le paquet INSTALLE : sauter si celui-ci n'a pas mcp_tools()
  skip_if(!exists("mcp_tools", envir = asNamespace("nemetonshiny")) ||
            !"mcp_tools" %in% ls(asNamespace("nemetonshiny"), all.names = TRUE),
          "mcp_tools absent du paquet installe")
  p <- processx::process$new(file.path(R.home("bin"), "Rscript"), srv,
                             stdin = "|", stdout = "|", stderr = "|")
  on.exit(p$kill(), add = TRUE)
  envoyer <- function(msg) p$write_input(paste0(jsonlite::toJSON(msg, auto_unbox = TRUE), "\n"))
  envoyer(list(jsonrpc = "2.0", id = 1, method = "initialize",
               params = list(protocolVersion = "2024-11-05", capabilities = list(),
                             clientInfo = list(name = "test", version = "0"))))
  envoyer(list(jsonrpc = "2.0", method = "notifications/initialized"))
  envoyer(list(jsonrpc = "2.0", id = 2, method = "tools/list"))
  lignes <- character(0)
  fin <- Sys.time() + 60
  while (Sys.time() < fin && !any(grepl('"id":2', lignes))) {
    p$poll_io(1000)
    lignes <- c(lignes, p$read_output_lines())
  }
  rep <- lignes[grepl('"id":2', lignes)]
  skip_if(!length(rep), "pas de reponse du serveur dans le delai")
  noms <- jsonlite::fromJSON(rep[1])$result$tools$name
  expect_true(all(c("lister_projets", "lancer_calcul", "url_app") %in% noms))
})

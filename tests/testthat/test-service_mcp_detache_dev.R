# Mission 012, relecture du 2026-11-11 : rapport refuse pendant un calcul de
# l'application, etat_calcul en echec sans isError, annuler_calcul honnete,
# job_id verifie par l'enfant.

.dd_lit <- function(json) jsonlite::fromJSON(as.character(json), simplifyVector = TRUE)

.dd_env <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_options(nemeton.app_options = list(project_dir = root),
                       .local_envir = env)
  local_mocked_bindings(
    add_r5_to_indicators = function(base_sf, project) base_sf,
    add_regen_r_indicators = function(base_sf, project) base_sf,
    is_db_configured = function() FALSE,
    lock_status = function(pid) NULL,
    .env = env)
  root
}
.dd_projet <- function() {
  sq <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0),
                                              c(x + 100, 100), c(x, 100), c(x, 0))))
  p <- sf::st_sf(id = "21001000AA0001", section = "AA", numero = "0001",
                 contenance = 1e4, commune = "Test", code_insee = "21001",
                 geometry = sf::st_sfc(sq(800000), crs = 2154))
  suppressMessages(projet_creer("Dabo", p))
}

test_that("generer_rapport est refuse en calcul_en_cours pendant un calcul de l'application", {
  skip_if_not_installed("arrow")
  .dd_env()
  id <- .dd_projet()
  local_mocked_bindings(progress_state_age_sec = function(id) 5,
                        .mcp_spawn = function(...) stop("ne doit pas etre lance"))
  r <- .dd_lit(mcp_generer_rapport(id))
  expect_false(isTRUE(r$ok))
  expect_identical(r$code, "calcul_en_cours")
  expect_null(r$job_id)
  expect_null(.mcp_job_read(get_project_path(id)))
})

test_that("generer_rapport est refuse en projet_verrouille quand l'application edite le projet", {
  skip_if_not_installed("arrow")
  .dd_env()
  id <- .dd_projet()
  local_mocked_bindings(
    lock_status = function(pid) list(holder_label = "Pascal", stale = FALSE),
    .mcp_spawn = function(...) stop("ne doit pas etre lance"))
  expect_identical(.dd_lit(mcp_generer_rapport(id))$code, "projet_verrouille")
})

test_that("etat_calcul en echec : champs d'erreur au premier niveau, pas de marquage isError", {
  skip_if_not_installed("arrow")
  .dd_env()
  id <- .dd_projet()
  path <- get_project_path(id)
  .mcp_job_write(path, list(job_id = "j", type = "onf", statut = "echec",
                            erreur = "boum", erreur_code = "onf_echec",
                            lance_a = .mcp_now(), fin_a = .mcp_now()))
  r <- mcp_etat_calcul(id)
  expect_false(isTRUE(attr(r, "mcp_erreur")))
  j <- .dd_lit(r)
  expect_false(isTRUE(j$ok))
  expect_identical(j$statut, "echec")
  expect_identical(j$code, "onf_echec")
  expect_identical(j$type, "onf")
  # et l'outil MCP rend un resultat normal, pas une erreur d'outil
  out <- .mcp_vers_outil(r)
  expect_false(inherits(out, "ellmer::ContentToolResult") && !is.null(out@error))
})

test_that("annuler_calcul pendant un rapport ou un croisement ONF : non annulable, dit honnetement", {
  skip_if_not_installed("arrow")
  .dd_env()
  id <- .dd_projet()
  path <- get_project_path(id)
  for (type in c("rapport", "onf")) {
    .mcp_job_write(path, list(job_id = "j", type = type, statut = "en_cours",
                              pid = Sys.getpid(), lance_a = .mcp_now()))
    r <- .dd_lit(mcp_annuler_calcul(id))
    expect_true(r$ok)
    expect_false(r$annule, info = type)
    expect_false(file.exists(file.path(path, "data", "cancel_requested")), info = type)
  }
  # un calcul garde l'annulation reelle
  .mcp_job_write(path, list(job_id = "j", type = "calcul", statut = "en_cours",
                            pid = Sys.getpid(), lance_a = .mcp_now()))
  r <- .dd_lit(mcp_annuler_calcul(id))
  expect_true(r$ok)
  expect_null(r$annule)
  expect_true(file.exists(file.path(path, "data", "cancel_requested")))
})

test_that(".mcp_child_job sort sans rien faire si le job_id ne correspond pas", {
  skip_if_not_installed("arrow")
  .dd_env()
  id <- .dd_projet()
  path <- get_project_path(id)
  .mcp_job_write(path, list(job_id = "autre", type = "rapport", statut = "lancement",
                            lance_a = .mcp_now()))
  local_mocked_bindings(projet_rapport = function(...) stop("ne doit pas tourner"))
  expect_null(.mcp_child_job(id, get_projects_root(), job_id = "perime"))
  job <- .mcp_job_read(path)
  expect_identical(job$statut, "lancement")   # intact : ni pid ni issue
  expect_null(job$pid)
})

test_that(".mcp_spawn passe le job_id du fichier job a l'enfant", {
  skip_if_not_installed("arrow")
  .dd_env()
  id <- .dd_projet()
  path <- get_project_path(id)
  log <- file.path(path, "data", "compute_mcp.log")
  .mcp_job_write(path, list(job_id = "j42", type = "rapport", statut = "lancement"))
  vu <- NULL
  local_mocked_bindings(system2 = function(command, args, ...) {
    vu <<- args
    invisible(0L)
  }, .package = "base")
  .mcp_spawn(id, get_projects_root(), log)
  expect_true(any(grepl("j42", vu, fixed = TRUE)))
})

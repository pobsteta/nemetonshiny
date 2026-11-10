# Extracted from test-service_mcp.R:159

# prequel ----------------------------------------------------------------------
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
.mcp_ancien <- function(id) {
  f <- file.path(get_project_path(id), "metadata.json")
  m <- jsonlite::read_json(f); m$format_projet <- NULL
  jsonlite::write_json(m, f, auto_unbox = TRUE, pretty = TRUE)
  invisible(id)
}

# test -------------------------------------------------------------------------
.mcp_env()
id <- .mcp_projet(calcule = FALSE)
path <- get_project_path(id)
f <- file.path(path, "data", "progress_state.json")
local_mocked_bindings(.mcp_spawn = function(...) invisible(TRUE))
ecrire <- function(x) {
    if (is.character(x)) writeLines(x, f) else jsonlite::write_json(x, f, auto_unbox = TRUE)
    Sys.setFileTime(f, Sys.time() - 5)
  }
ecrire(list(status = "completed", phase = "complete"))
expect_lt(progress_state_age_sec(id), 60)
expect_true(.lit(mcp_lancer_calcul(id))$ok)
unlink(.mcp_job_path(path))
for (st in c("error", "cancelled", "failed")) {
    ecrire(list(status = st))
    expect_true(.lit(mcp_lancer_calcul(id))$ok, label = st)
    unlink(.mcp_job_path(path))
  }
ecrire(list(status = "?", phase = "complete"))
expect_true(.lit(mcp_lancer_calcul(id))$ok)
unlink(.mcp_job_path(path))
for (st in c("computing", "downloading", "pending")) {
    ecrire(list(status = st, phase = "indicators"))
    expect_identical(.lit(mcp_lancer_calcul(id))$classe,
                     "nemetonshiny_calcul_en_cours", label = st)
  }

# Extracted from test-service_mcp.R:415

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
.mcp_rect <- function(x0, x1) {
  sf::st_polygon(list(rbind(c(x0, 0), c(x1, 0), c(x1, 100), c(x0, 100), c(x0, 0))))
}
.mcp_ugf_fichier <- function(geoms = list(.mcp_rect(800000, 800050),
                                          .mcp_rect(800050, 800100),
                                          .mcp_sq(800100)),
                             idu = c("21001000AA0001", "21001000AA0001",
                                     "21001000AA0002")) {
  f <- tempfile(fileext = ".gpkg")
  n <- length(idu)
  x <- sf::st_sf(idu = idu,
                 label_ugf = rep(c("F1-1", "F1-2", "F1-2"), length.out = n),
                 onf_foret_id = "F1", onf_foret_nom = "Forêt test",
                 onf_parcelle = rep(c("1", "2", "2"), length.out = n),
                 onf_domaniale = FALSE, onf_part = 0.9,
                 geometry = sf::st_sfc(geoms, crs = 2154))
  sf::st_write(x, f, quiet = TRUE)
  f
}
.mcp_ugf_etat <- function(id) {
  ug <- load_ug_data(id)
  sort(paste(ug$tenements$parent_parcelle_id, ug$ugs$label[match(ug$tenements$ug_id, ug$ugs$ug_id)]))
}

# test -------------------------------------------------------------------------
skip_if_not_installed("arrow")
.mcp_env()
id <- .mcp_projet()
vu <- NULL
local_mocked_bindings(
    .onf_construire = function(cad, onf, selection, cfg) {
      vu <<- selection
      x <- sf::st_sf(idu = "21001000AA0001", ugf_id = "F1-1",
                     nom_ugf = "Forêt test - parcelle 1", foret_id = "F1",
                     foret_nom = "Forêt test", parcelle = "1",
                     domaniale = FALSE, part_onf = 0.9, surface_m2 = 1e4,
                     geometry = sf::st_sfc(.mcp_sq(800000), crs = 2154))
      attr(x, "parcelles") <- data.frame(
        idu = c("21001000AA0001", "21001000AA0002"), retenue = c(TRUE, FALSE),
        raison = c(NA, "hors ONF"), proprietaire = c("COMMUNE", "X"),
        couverture_onf = c(1, 0))
      x
    },
    onf_load_parcelles = function(...) list(status = "ok", parcelles = NULL))
r <- .lit(mcp_croiser_onf(id, purger = TRUE))
expect_identical(vu, "foret")
expect_true(r$ok)
expect_identical(r$n_ugf, 1L)
expect_identical(r$ecartees$idu, "21001000AA0002")
expect_identical(r$ecartees$raison, "hors_onf")
expect_identical(as.character(load_parcels(id)$id), "21001000AA0001")
expect_identical(load_ug_data(id)$ugs$onf_parcelle, "1")
avant <- .mcp_ugf_etat(id)
local_mocked_bindings(.onf_construire = function(...) NULL)
r <- .lit(mcp_croiser_onf(id, purger = FALSE))

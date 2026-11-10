# Mission 010, complement de relecture (2026-11-10) :
#  1. l'apercu d'une ecriture nomme le projet (id ET nom resolu) ;
#  2. un jeton presente avec un projet introuvable / ambigu, ou des metadonnees
#     ONF illisibles (lues avant le jeton), n'est PAS consomme.
# Ne modifie pas test-service_mcp_confirmation.R (ses helpers sont recopies).

.pn_sq <- function(x) {
  sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0), c(x + 100, 100),
                            c(x, 100), c(x, 0))))
}
.pn_rect <- function(x0, x1) {
  sf::st_polygon(list(rbind(c(x0, 0), c(x1, 0), c(x1, 100), c(x0, 100), c(x0, 0))))
}
.pn_env <- function(env = parent.frame()) {
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
.pn_projet <- function(nom) {
  parcelles <- sf::st_sf(
    id = c("21001000AA0001", "21001000AA0002"), section = "AA",
    numero = c("0001", "0002"), contenance = c(1e4, 1e4),
    commune = "Test", code_insee = "21001",
    geometry = sf::st_sfc(.pn_sq(800000), .pn_sq(800100), crs = 2154))
  suppressMessages(projet_creer(nom, parcelles))
}
.pn_fichier <- function() {
  f <- tempfile(fileext = ".gpkg")
  x <- sf::st_sf(idu = c("21001000AA0001", "21001000AA0001", "21001000AA0002"),
                 label_ugf = c("F1-1", "F1-2", "F1-2"),
                 onf_foret_id = "F1", onf_foret_nom = "Foret test",
                 onf_parcelle = c("1", "2", "2"), onf_domaniale = FALSE,
                 onf_part = 0.9,
                 geometry = sf::st_sfc(list(.pn_rect(800000, 800050),
                                            .pn_rect(800050, 800100),
                                            .pn_sq(800100)), crs = 2154))
  sf::st_write(x, f, quiet = TRUE)
  f
}
.pn_mock_onf <- function(env = parent.frame()) {
  local_mocked_bindings(
    .onf_construire = function(cad, onf, selection, cfg) {
      x <- sf::st_sf(idu = "21001000AA0001", ugf_id = "F1-1",
                     nom_ugf = "Foret test - parcelle 1", foret_id = "F1",
                     foret_nom = "Foret test", parcelle = "1",
                     domaniale = FALSE, part_onf = 0.9, surface_m2 = 1e4,
                     geometry = sf::st_sfc(.pn_sq(800000), crs = 2154))
      attr(x, "parcelles") <- data.frame(
        idu = c("21001000AA0001", "21001000AA0002"), retenue = c(TRUE, FALSE),
        raison = c(NA, "hors ONF"), proprietaire = c("COMMUNE", "X"),
        couverture_onf = c(1, 0))
      x
    },
    onf_load_parcelles = function(...) list(status = "ok", parcelles = NULL),
    .env = env)
}
.pn_json <- function(x) jsonlite::fromJSON(as.character(x), simplifyVector = TRUE)
.pn_nom <- function(id) {
  l <- projets_lister()
  l$nom[l$id == id]
}

test_that("appliquer_ugf : l'apercu nomme le projet (id et nom exacts)", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")
  .pn_env()
  id <- .pn_projet("Foret de Dabo")
  nom <- .pn_nom(id)
  expect_identical(nom, "Foret de Dabo")
  j <- .pn_json(mcp_appliquer_ugf(id, .pn_fichier()))
  expect_identical(j$code, "confirmation_requise")
  expect_identical(j$projet, id)
  expect_identical(j$projet_nom, nom)
  expect_match(j$erreur, id, fixed = TRUE)
  expect_match(j$erreur, nom, fixed = TRUE)
})

test_that("croiser_onf : l'apercu nomme le projet (id et nom exacts), en anglais aussi", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")
  .pn_env()
  .pn_mock_onf()
  id <- .pn_projet("Foret de Dabo")
  nom <- .pn_nom(id)
  j <- .pn_json(mcp_croiser_onf(id))
  expect_identical(j$code, "confirmation_requise")
  expect_identical(j$projet, id)
  expect_identical(j$projet_nom, nom)
  expect_match(j$erreur, id, fixed = TRUE)
  expect_match(j$erreur, nom, fixed = TRUE)
  e <- structure(class = c("nemetonshiny_confirmation_requise",
                           "nemetonshiny_erreur", "error", "condition"),
                 list(message = "x", call = NULL, projet = id, projet_nom = nom))
  en <- .pn_json(.mcp_erreur(e, "en"))
  expect_match(en$erreur, nom, fixed = TRUE)
  expect_match(en$erreur, "Confirmation required", fixed = TRUE)
})

test_that("projet donne par son nom : l'apercu cite l'id resolu et le nom", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")
  .pn_env()
  id <- .pn_projet("Foret unique zzz")
  j <- .pn_json(mcp_appliquer_ugf("foret unique ZZZ", .pn_fichier()))
  expect_identical(j$projet, id)
  expect_match(j$erreur, "Foret unique zzz", fixed = TRUE)
})

test_that("jeton + projet introuvable : erreur de projet, jeton NON consomme", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")
  .pn_env()
  id <- .pn_projet("Dabo")
  f <- .pn_fichier()
  jeton <- .pn_json(mcp_appliquer_ugf(id, f))$jeton
  r <- .pn_json(mcp_appliquer_ugf("projet-qui-n-existe-pas", f, jeton = jeton))
  expect_identical(r$code, "projet_introuvable")
  ok <- .pn_json(mcp_appliquer_ugf(id, f, jeton = jeton))
  expect_true(isTRUE(ok$ok), info = "le jeton a survecu au refus de projet")
})

test_that("jeton + projet ambigu : erreur de projet, jeton NON consomme", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")
  .pn_env()
  id <- .pn_projet("Dabo nord")
  .pn_projet("Dabo sud")
  f <- .pn_fichier()
  jeton <- .pn_json(mcp_appliquer_ugf(id, f))$jeton
  r <- .pn_json(mcp_appliquer_ugf("dabo", f, jeton = jeton))
  expect_identical(r$code, "projet_ambigu")
  ok <- .pn_json(mcp_appliquer_ugf(id, f, jeton = jeton))
  expect_true(isTRUE(ok$ok), info = "le jeton a survecu au refus de projet")
})

test_that("croiser_onf + metadonnees illisibles : erreur, jeton NON consomme", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")
  .pn_env()
  .pn_mock_onf()
  id <- .pn_projet("Dabo")
  jeton <- .pn_json(mcp_croiser_onf(id))$jeton
  local({
    local_mocked_bindings(
      load_project_metadata = function(project_id) stop("metadata.json illisible"))
    r <- .pn_json(mcp_croiser_onf(id, jeton = jeton))
    expect_false(isTRUE(r$ok))
    expect_false(identical(r$code, "jeton_invalide"))
  })
  ok <- .pn_json(mcp_croiser_onf(id, jeton = jeton))
  expect_true(isTRUE(ok$ok), info = "le jeton a survecu aux metadonnees illisibles")
})

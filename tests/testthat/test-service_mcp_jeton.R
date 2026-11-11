# Mission 010 (ADR 0002) : tests unitaires de la confirmation par jeton
# (.mcp_jeton_emettre, .mcp_jeton_consommer, .mcp_empreinte) et cas
# « parcelles modifiees entre l'apercu et la confirmation ». Les tests
# d'acceptation sont ceux de la mission 009 (test-service_mcp_confirmation.R).

.jt_cond <- function(expr) tryCatch(expr, error = function(e) e)

# Mission 016 : le registre est un fichier par jeton (NEMETON_MCP_JETONS_DIR,
# pose par setup-service_mcp_jetons.R) ; ces deux aides remplacent
# l'inspection de l'environnement en memoire, les attentes sont inchangees.
.jt_au_registre <- function(jeton) {
  file.exists(.mcp_jeton_fichier(jeton))
}
.jt_retirer <- function(jeton) unlink(.mcp_jeton_fichier(jeton))

.jt_emettre <- function(outil = "outil_x", projet = "p1", lien = list(a = 1),
                        apercu = list(n = 1L)) {
  .jt_cond(.mcp_jeton_emettre(outil, projet, lien, apercu))
}

.jt_sans_garde <- function(jeton, outil = "outil_x", projet = "p1",
                           lien = list(a = 1)) {
  .jt_cond(.mcp_jeton_consommer(jeton, outil, projet, lien, garde = NULL))
}

test_that("emettre : n'ecrit rien, leve confirmation_requise avec jeton, apercu, expire_a", {
  e <- .jt_emettre()
  expect_s3_class(e, "nemetonshiny_confirmation_requise")
  expect_s3_class(e, "nemetonshiny_erreur")
  expect_match(e$jeton, "^[0-9a-z]{24}$")
  expect_identical(e$apercu, list(n = 1L))
  expect_match(e$expire_a, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
  t <- as.POSIXct(e$expire_a, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  dt <- as.numeric(difftime(t, Sys.time(), units = "secs"))
  expect_true(dt > .MCP_JETON_DUREE - 5 && dt <= .MCP_JETON_DUREE)
  expect_true(.jt_au_registre(e$jeton))
  .jt_retirer(e$jeton)
})

test_that("la duree est de 10 minutes (provisoire, une seule constante)", {
  expect_identical(.MCP_JETON_DUREE, 600)
})

test_that("consommer : un jeton valide passe une fois, puis il est mort", {
  j <- .jt_emettre()$jeton
  expect_true(.mcp_jeton_consommer(j, "outil_x", "p1", list(a = 1), garde = NULL))
  expect_false(.jt_au_registre(j))
  expect_s3_class(.jt_sans_garde(j), "nemetonshiny_jeton_invalide")
})

test_that("consommer : outil, projet ou lien differents -> refuse ET consomme", {
  for (cas in list(list(outil = "autre"), list(projet = "p2"),
                   list(lien = list(a = 2)))) {
    j <- .jt_emettre()$jeton
    e <- do.call(.jt_sans_garde, c(list(j), cas))
    expect_s3_class(e, "nemetonshiny_jeton_invalide")
    # les bons parametres ne le ressuscitent pas
    expect_s3_class(.jt_sans_garde(j), "nemetonshiny_jeton_invalide")
  }
})

test_that("consommer : jeton absent, vide, NA, non chaine -> jeton_invalide", {
  for (j in list(NULL, "", NA_character_, 42, c("a", "b"), "inconnu")) {
    expect_s3_class(.jt_sans_garde(j), "nemetonshiny_jeton_invalide")
  }
})

test_that("consommer : jeton perime refuse (horloge simulee)", {
  j <- .jt_emettre()$jeton
  reel <- base::Sys.time
  local_mocked_bindings(Sys.time = function() reel() + .MCP_JETON_DUREE + 1,
                        .package = "base")
  expect_s3_class(.jt_sans_garde(j), "nemetonshiny_jeton_invalide")
  expect_false(.jt_au_registre(j))
})

test_that("emettre purge les jetons perimes du registre", {
  vieux <- .jt_emettre()$jeton
  reel <- base::Sys.time
  local_mocked_bindings(Sys.time = function() reel() + .MCP_JETON_DUREE + 1,
                        .package = "base")
  neuf <- .jt_emettre()$jeton
  expect_false(.jt_au_registre(vieux))
  expect_true(.jt_au_registre(neuf))
  .jt_retirer(neuf)
})

test_that("consommer : la garde est rejouee, son erreur passe, le jeton est consomme, le lien n'est pas evalue", {
  j <- .jt_emettre()$jeton
  vu <- NULL
  evalue <- FALSE
  garde <- function(id) {
    vu <<- id
    .api_abort("occupe", "nemetonshiny_projet_verrouille", detenteur = "Pascal")
  }
  e <- .jt_cond(.mcp_jeton_consommer(j, "outil_x", "p1",
                                     { evalue <- TRUE; list(a = 1) },
                                     garde = garde))
  expect_s3_class(e, "nemetonshiny_projet_verrouille")
  expect_identical(vu, "p1")
  expect_false(evalue)
  expect_s3_class(.jt_sans_garde(j), "nemetonshiny_jeton_invalide")
})

test_that(".mcp_erreur porte jeton, apercu, expire_a pour confirmation_requise seule", {
  e <- .jt_emettre(apercu = list(ecartees = I("21001000AA0002"), n_ecartees = 1L))
  j <- jsonlite::fromJSON(as.character(.mcp_erreur(e)), simplifyVector = TRUE)
  expect_identical(j$code, "confirmation_requise")
  expect_identical(j$jeton, e$jeton)
  expect_identical(j$apercu$ecartees, "21001000AA0002")
  expect_identical(j$expire_a, e$expire_a)
  .jt_retirer(e$jeton)
  autre <- .jt_cond(.api_abort("x", "nemetonshiny_calcul_en_cours"))
  j <- jsonlite::fromJSON(as.character(.mcp_erreur(autre)), simplifyVector = TRUE)
  expect_false(any(c("jeton", "apercu", "expire_a") %in% names(j)))
})

test_that(".mcp_lien_fichier : chemin normalise et md5, NA si absent ou invalide", {
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("a", f)
  l <- .mcp_lien_fichier(f)
  expect_identical(l$chemin, normalizePath(f))
  expect_identical(l$md5, unname(tools::md5sum(f)))
  writeLines("b", f)
  expect_false(identical(.mcp_lien_fichier(f)$md5, l$md5))
  expect_identical(.mcp_lien_fichier(file.path(tempdir(), "absent-010.gpkg"))$md5,
                   NA_character_)
  expect_identical(.mcp_lien_fichier(NULL)$chemin, NA_character_)
})

# ------------------------------------------------------- integration (projet)

.jt_sq <- function(x) {
  sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0), c(x + 100, 100),
                            c(x, 100), c(x, 0))))
}
.jt_env <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_options(nemeton.app_options = list(project_dir = root),
                       .local_envir = env)
  local_mocked_bindings(
    add_r5_to_indicators = function(base_sf, project) base_sf,
    add_regen_r_indicators = function(base_sf, project) base_sf,
    is_db_configured = function() FALSE,
    lock_status = function(pid) NULL,
    .onf_construire = function(cad, onf, selection, cfg) {
      x <- sf::st_sf(idu = "21001000AA0001", ugf_id = "F1-1",
                     nom_ugf = "Foret test - parcelle 1", foret_id = "F1",
                     foret_nom = "Foret test", parcelle = "1",
                     domaniale = FALSE, part_onf = 0.9, surface_m2 = 1e4,
                     geometry = sf::st_sfc(.jt_sq(800000), crs = 2154))
      attr(x, "parcelles") <- data.frame(
        idu = c("21001000AA0001", "21001000AA0002"), retenue = c(TRUE, FALSE),
        raison = c(NA, "hors ONF"), proprietaire = c("COMMUNE", "X"),
        couverture_onf = c(1, 0))
      x
    },
    onf_load_parcelles = function(...) list(status = "ok", parcelles = NULL),
    .env = env
  )
  root
}
.jt_projet <- function() {
  parcelles <- sf::st_sf(
    id = c("21001000AA0001", "21001000AA0002"), section = "AA",
    numero = c("0001", "0002"), contenance = c(1e4, 1e4),
    commune = "Test", code_insee = "21001",
    geometry = sf::st_sfc(.jt_sq(800000), .jt_sq(800100), crs = 2154))
  suppressMessages(projet_creer("Projet jeton", parcelles))
}
.jt_fichier <- function() {
  f <- tempfile(fileext = ".gpkg")
  sf::st_write(sf::st_sf(idu = c("21001000AA0001", "21001000AA0002"),
                         label_ugf = c("F1-1", "F1-2"),
                         geometry = sf::st_sfc(.jt_sq(800000), .jt_sq(800100),
                                               crs = 2154)),
               f, quiet = TRUE)
  f
}
.jt_lit <- function(x) jsonlite::fromJSON(as.character(x), simplifyVector = TRUE)
# Les parcelles du projet changent (contenance), sans changer d'IDU.
.jt_modifier_parcelles <- function(id) {
  p <- load_parcels(id)
  p$contenance <- p$contenance + 1
  suppressMessages(save_parcels(id, p))
}
.jt_dossier <- function(id) {
  d <- get_project_path(id)
  f <- sort(list.files(d, recursive = TRUE, all.files = TRUE, full.names = TRUE))
  unname(tools::md5sum(f))
}

test_that(".mcp_empreinte change quand les parcelles du projet changent", {
  skip_if_not_installed("arrow")
  .jt_env()
  id <- .jt_projet()
  a <- .mcp_empreinte(id, x = 1)
  expect_true(length(a$parcelles) >= 1L)
  expect_identical(a$x, 1)
  expect_identical(.mcp_empreinte(id, x = 1), a)
  .jt_modifier_parcelles(id)
  expect_false(identical(.mcp_empreinte(id, x = 1), a))
})

test_that("appliquer_ugf : parcelles modifiees entre apercu et confirmation -> jeton_invalide, rien d'ecrit", {
  skip_if_not_installed("arrow")
  .jt_env()
  id <- .jt_projet()
  f <- .jt_fichier()
  j <- .jt_lit(mcp_appliquer_ugf(id, f))
  expect_identical(j$code, "confirmation_requise")
  .jt_modifier_parcelles(id)
  avant <- .jt_dossier(id)
  r <- .jt_lit(mcp_appliquer_ugf(id, f, jeton = j$jeton))
  expect_identical(r$code, "jeton_invalide")
  expect_identical(.jt_dossier(id), avant)
})

test_that("croiser_onf : parcelles modifiees entre apercu et confirmation -> jeton_invalide, rien d'ecrit", {
  skip_if_not_installed("arrow")
  .jt_env()
  id <- .jt_projet()
  j <- .jt_lit(mcp_croiser_onf(id, purger = TRUE))
  expect_identical(j$code, "confirmation_requise")
  .jt_modifier_parcelles(id)
  avant <- .jt_dossier(id)
  r <- .jt_lit(mcp_croiser_onf(id, purger = TRUE, jeton = j$jeton))
  expect_identical(r$code, "jeton_invalide")
  expect_identical(.jt_dossier(id), avant)
  expect_identical(as.character(load_parcels(id)$id),
                   c("21001000AA0001", "21001000AA0002"))
})

test_that("appliquer_ugf remplacer = FALSE : apercu = UGF actuelles des parcelles du fichier", {
  skip_if_not_installed("arrow")
  .jt_env()
  id <- .jt_projet()
  f <- tempfile(fileext = ".gpkg")
  sf::st_write(sf::st_sf(idu = "21001000AA0001", label_ugf = "F1-1",
                         geometry = sf::st_sfc(.jt_sq(800000), crs = 2154)),
               f, quiet = TRUE)
  ug <- load_ug_data(id)
  attendu <- length(unique(ug$tenements$ug_id[
    ug$tenements$parent_parcelle_id == "21001000AA0001"]))
  j <- .jt_lit(mcp_appliquer_ugf(id, f, remplacer = FALSE))
  expect_identical(j$code, "confirmation_requise")
  expect_identical(j$apercu$n_ugf_remplacees, attendu)
  expect_identical(j$apercu$n_ugf_nouvelles, 1L)
})

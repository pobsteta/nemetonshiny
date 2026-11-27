# Service ICD : poids du projet, temoin du dernier calcul, appel du coeur.
# Le coeur est simule pour le calcul ; `icd_poids_*` et les lecteurs absents
# tournent avec le vrai coeur.

.projet_icd <- function(meta = list(), env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  jsonlite::write_json(meta, file.path(dir, "metadata.json"), auto_unbox = TRUE)
  dir
}

test_that("project_icd_poids rend les valeurs du coeur sans metadonnees", {
  p <- nemetonshiny:::project_icd_poids(NULL)
  d <- nemeton::icd_poids_defaut()
  expect_identical(p$essence, d$essence)
  expect_equal(p$GRVI, d$GRVI)
  expect_equal(p$CRre, d$CRre)
})

test_that("project_icd_poids lit les valeurs stockees et ignore les aberrantes", {
  meta <- list(icd_poids = list(poids = list(
    chene = list(GRVI = 0.4, CIre = "abc", CRSWIR = -1, CRre = 0.1, NDVI = 0.2))))
  p <- nemetonshiny:::project_icd_poids(meta)
  d <- nemeton::icd_poids_defaut()
  expect_equal(p$GRVI[p$essence == "chene"], 0.4)
  expect_equal(p$CIre[p$essence == "chene"], d$CIre[1])      # non numerique
  expect_equal(p$CRSWIR[p$essence == "chene"], d$CRSWIR[1])  # negatif
  expect_equal(p$GRVI[p$essence == "hetre"], d$GRVI[2])      # essence absente
})

test_that("icd_poids_resume dit la somme saisie et renormalise a 1 (coeur reel)", {
  p <- nemetonshiny:::project_icd_poids(NULL)
  p$GRVI[1] <- 0.30                       # chene : somme 1.15
  r <- nemetonshiny:::icd_poids_resume(p)
  expect_true(is.na(r$erreur))
  expect_equal(unname(r$somme_saisie["chene"]), 1.15)
  expect_identical(r$renormalise, "chene")
  expect_equal(sum(r$valide[1, c("GRVI", "CIre", "CRSWIR", "CRre", "NDVI")]), 1)
})

test_that("icd_poids_resume rend l'erreur du coeur au lieu de la lever", {
  p <- nemetonshiny:::project_icd_poids(NULL)
  p$CRre[p$essence == "douglas"] <- 0.1    # interdit pour un resineux
  r <- nemetonshiny:::icd_poids_resume(p)
  expect_null(r$valide)
  expect_match(r$erreur, "CRre")
})

test_that("les poids se sauvent puis se relisent, et le coeur les recoit", {
  dir <- .projet_icd()
  testthat::local_mocked_bindings(get_project_path = function(id) dir)
  p <- nemetonshiny:::project_icd_poids(NULL)
  p$GRVI[2] <- 0.5                         # hetre : somme 1.2, acceptee
  expect_true(nemetonshiny:::set_project_icd_poids("p1", p))
  meta <- jsonlite::read_json(file.path(dir, "metadata.json"))
  relu <- nemetonshiny:::project_icd_poids(meta)
  expect_equal(relu$GRVI[2], 0.5)          # stocke tel que saisi
  expect_equal(relu$essence, p$essence)
})

test_that("un tableau que le coeur refuse n'est pas enregistre", {
  dir <- .projet_icd()
  testthat::local_mocked_bindings(get_project_path = function(id) dir)
  p <- nemetonshiny:::project_icd_poids(NULL)
  p$CRre[3] <- 0.2
  expect_error(nemetonshiny:::set_project_icd_poids("p1", p), "CRre")
  meta <- jsonlite::read_json(file.path(dir, "metadata.json"))
  expect_null(meta$icd_poids)
})

test_that("les poids sont obsoletes seulement apres un changement", {
  rd <- withr::local_tempdir()
  p <- nemetonshiny:::project_icd_poids(NULL)
  # Sans temoin : pas de verdict a la legere.
  expect_false(nemetonshiny:::icd_poids_obsoletes(rd, 7L, p))
  v <- nemetonshiny:::icd_poids_resume(p)$valide
  nemetonshiny:::.icd_ecrire_temoin(rd, 7L, v, 2017:2026, 2026L)
  expect_false(nemetonshiny:::icd_poids_obsoletes(rd, 7L, p))
  p$GRVI[1] <- 0.25                         # chene : 0.25 renormalise differe
  expect_true(nemetonshiny:::icd_poids_obsoletes(rd, 7L, p))
  # Un changement qui se renormalise a l'identique n'obsolete rien.
  q <- nemetonshiny:::project_icd_poids(NULL)
  q[1, c("GRVI", "CIre", "CRSWIR", "CRre", "NDVI")] <-
    2 * q[1, c("GRVI", "CIre", "CRSWIR", "CRre", "NDVI")]
  expect_false(nemetonshiny:::icd_poids_obsoletes(rd, 7L, q))
})

test_that("icd_annees_disponibles lit le temoin, sinon une plage", {
  rd <- withr::local_tempdir()
  a <- nemetonshiny:::icd_annees_disponibles(rd, 3L, aujourdhui = as.Date("2026-11-27"))
  expect_equal(a$annees, 2017:2026)
  expect_true(is.na(a$provisoire))
  v <- nemetonshiny:::icd_poids_resume(nemetonshiny:::project_icd_poids(NULL))$valide
  nemetonshiny:::.icd_ecrire_temoin(rd, 3L, v, 2019:2021, 2021L)
  a <- nemetonshiny:::icd_annees_disponibles(rd, 3L)
  expect_equal(a$annees, 2019:2021)
  expect_equal(a$provisoire, 2021L)
})

test_that("lecteurs et comparaison sans resultat : vides, avec le vrai coeur", {
  rd <- withr::local_tempdir()
  expect_null(nemetonshiny:::icd_lire_carte(1L, 2024L, rd))
  expect_null(nemetonshiny:::icd_lire_pixel(1L, 5, 45, rd))
  expect_identical(nemetonshiny:::icd_comparer_fast(1L, 2024L, rd)$statut,
                   "icd_absent")
})

test_that(".icd_executer passe poids et BD Foret au coeur et ecrit le temoin", {
  rd <- withr::local_tempdir()
  gpkg <- file.path(withr::local_tempdir(), "bd.gpkg")
  sf::st_write(sf::st_sf(code_tfv = "FF1G01-01",
                         geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)),
               gpkg, quiet = TRUE)
  p <- nemetonshiny:::project_icd_poids(NULL)
  v <- nemetonshiny:::icd_poids_resume(p)$valide
  vu <- NULL
  testthat::local_mocked_bindings(
    run_icd_decoloration = function(con, zone_id, cache_dir, result_dir, poids,
                                    bdforet, progress_callback, cancel_path, ...) {
      vu <<- list(zone = zone_id, poids = poids, bd = bdforet, cancel = cancel_path)
      list(status = "success", poids = v, annees = 2017:2026, n_pixels_icd = 12L)
    },
    .package = "nemeton")
  res <- nemetonshiny:::.icd_executer(NULL, 5, "cache", rd, p, gpkg,
                                      cancel_path = "flag")
  expect_equal(vu$zone, 5L)
  expect_equal(vu$poids, p)
  expect_s3_class(vu$bd, "sf")
  expect_identical(vu$cancel, "flag")
  expect_equal(res$n_pixels_icd, 12L)
  expect_equal(nemetonshiny:::icd_annees_disponibles(rd, 5L)$annees, 2017:2026)
})

test_that(".icd_executer leve l'erreur du coeur et laisse passer l'annulation", {
  gpkg <- file.path(withr::local_tempdir(), "bd.gpkg")
  sf::st_write(sf::st_sf(code_tfv = "FF1G01-01",
                         geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)),
               gpkg, quiet = TRUE)
  p <- nemetonshiny:::project_icd_poids(NULL)
  testthat::local_mocked_bindings(
    run_icd_decoloration = function(...) list(status = "error", message = "panne S2"),
    .package = "nemeton")
  expect_error(nemetonshiny:::.icd_executer(NULL, 5, "c", tempdir(), p, gpkg), "panne S2")
  testthat::local_mocked_bindings(
    run_icd_decoloration = function(...) list(status = "cancelled", phase = "ingest"),
    .package = "nemeton")
  expect_identical(
    nemetonshiny:::.icd_executer(NULL, 5, "c", tempdir(), p, gpkg)$status, "cancelled")
})

test_that(".icd_executer refuse une BD Foret illisible", {
  expect_error(nemetonshiny:::.icd_executer(NULL, 5, "c", tempdir(), NULL,
                                            "/nulle/part.gpkg"), "BD Foret")
})

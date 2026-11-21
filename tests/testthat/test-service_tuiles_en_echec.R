# Mission 038 : foretaccess >= 2.6.2 perd une tuile WFS apres 3 tentatives et
# l'ANNONCE par un avertissement (la couche rendue est incomplete). L'app doit
# capturer cet avertissement dans le service, le rendre dans le resultat
# (`avertissements`) et l'afficher dans le rapport du pipeline (etape `ok`).

# Avertissement tel que foretaccess (>= 2.6.3) l'emet : cli_warn, 3 lignes, classe
# `foretaccess_tuiles_perdues` (c'est elle que l'app reconnait, plus le texte).
.avert_tuile <- function(n = 1L, total = 4L,
                         couche = "BDTOPO_V3:troncon_de_route") {
  cli::cli_warn(c(
    "{n} tuile{?s} WFS sur {total} en echec pour {.val {couche}}.",
    "!" = "La couche rendue est incomplete sur une partie de l'emprise.",
    "x" = "Derniere erreur : Unexpected content type \"text/plain\"."),
    class = "foretaccess_tuiles_perdues")
}

.desserte_toy <- function() {
  sf::st_sf(classe = "route", largeur = 4, geometry = sf::st_sfc(
    sf::st_linestring(rbind(c(0, 10), c(50, 10))), crs = 2154))
}

test_that(".capturer_alertes_tuiles garde et etouffe l'avertissement de tuile", {
  fuites <- character(0)
  out <- withCallingHandlers(
    nemetonshiny:::.capturer_alertes_tuiles({ .avert_tuile(); 42L }),
    warning = function(w) {
      fuites <<- c(fuites, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  expect_identical(out$valeur, 42L)
  expect_length(out$alertes, 1L)
  expect_match(out$alertes, "1 tuile WFS sur 4 en echec", fixed = TRUE)
  # Message complet : la derniere ligne porte l'erreur de l'IGN, sans ANSI.
  expect_match(out$alertes, "Derniere erreur", fixed = TRUE)
  expect_false(grepl("\033", out$alertes, fixed = TRUE))
  # Etouffe : rien n'a fui vers l'appelant.
  expect_length(fuites, 0L)
})

test_that(".capturer_alertes_tuiles laisse passer un avertissement sans rapport", {
  fuites <- character(0)
  out <- withCallingHandlers(
    nemetonshiny:::.capturer_alertes_tuiles({
      warning("autre chose, sans rapport"); 1L }),
    warning = function(w) {
      fuites <<- c(fuites, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  expect_length(out$alertes, 0L)
  expect_identical(fuites, "autre chose, sans rapport")
})

test_that(".capturer_alertes_tuiles : meme texte SANS la classe n'est pas capture", {
  fuites <- character(0)
  out <- withCallingHandlers(
    nemetonshiny:::.capturer_alertes_tuiles({
      warning("2 tuiles WFS sur 4 en echec pour X."); 1L }),
    warning = function(w) {
      fuites <<- c(fuites, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  expect_length(out$alertes, 0L)
  expect_identical(fuites, "2 tuiles WFS sur 4 en echec pour X.")
})

test_that(".capturer_alertes_tuiles : classee avec un autre texte, capturee et etouffee", {
  fuites <- character(0)
  out <- withCallingHandlers(
    nemetonshiny:::.capturer_alertes_tuiles({
      warning(warningCondition("texte reformule par foretaccess",
                               class = "foretaccess_tuiles_perdues"))
      1L }),
    warning = function(w) {
      fuites <<- c(fuites, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  expect_identical(out$alertes, "texte reformule par foretaccess")
  expect_length(fuites, 0L)
})

test_that(".capturer_alertes_tuiles : un echec dans un tryCatch interne rend quand meme la valeur", {
  out <- nemetonshiny:::.capturer_alertes_tuiles(
    tryCatch({ .avert_tuile(); stop("boum") }, error = function(e) "repli"))
  expect_identical(out$valeur, "repli")
  expect_length(out$alertes, 1L)
})

test_that("run_desserte (IGN mocke) : succes, alerte rendue dans le resultat", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("sf")
  toy <- system.file("extdata", "toy", package = "foretaccess")
  skip_if(!nzchar(toy) || !file.exists(file.path(toy, "mnt.tif")))
  loadNamespace("foretaccess")
  aoi_path <- file.path(toy, "foret.gpkg")
  foret_toy <- sf::st_transform(sf::st_read(aoi_path, quiet = TRUE), 2154)
  desserte <- sf::st_read(file.path(toy, "desserte.gpkg"), quiet = TRUE)
  mnt_toy <- file.path(toy, "mnt.tif")
  project_path <- withr::local_tempdir()
  cache <- nemetonshiny:::.desserte_cache_dir(project_path)

  testthat::local_mocked_bindings(
    .acquire_mnt_highres = function(...) mnt_toy, .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    acquire_desserte = function(...) { .avert_tuile(1L, 4L); desserte },
    acquire_foret = function(...) {
      .avert_tuile(2L, 3L, "BDFORET_V2:formation_vegetale"); foret_toy },
    .package = "foretaccess")

  fuites <- character(0)
  res <- withCallingHandlers(
    nemetonshiny:::run_desserte(aoi_path, "glouton", cache),
    warning = function(w) {
      if (grepl("tuile", conditionMessage(w))) fuites <<- c(fuites, "fuite")
      if (grepl("tuile", conditionMessage(w))) invokeRestart("muffleWarning")
    })
  expect_identical(res$status, "success")        # l'etape REUSSIT
  expect_length(fuites, 0L)                       # rien n'a fui
  expect_length(res$avertissements, 2L)           # desserte + foret
  expect_true(any(grepl("troncon_de_route", res$avertissements)))
  expect_true(any(grepl("formation_vegetale", res$avertissements)))
})

test_that("run_desserte (IGN mocke) : sans perte de tuile, aucune alerte", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("sf")
  toy <- system.file("extdata", "toy", package = "foretaccess")
  skip_if(!nzchar(toy) || !file.exists(file.path(toy, "mnt.tif")))
  loadNamespace("foretaccess")
  aoi_path <- file.path(toy, "foret.gpkg")
  foret_toy <- sf::st_transform(sf::st_read(aoi_path, quiet = TRUE), 2154)
  desserte <- sf::st_read(file.path(toy, "desserte.gpkg"), quiet = TRUE)
  mnt_toy <- file.path(toy, "mnt.tif")
  cache <- nemetonshiny:::.desserte_cache_dir(withr::local_tempdir())
  testthat::local_mocked_bindings(
    .acquire_mnt_highres = function(...) mnt_toy, .package = "nemetonshiny")
  testthat::local_mocked_bindings(
    acquire_desserte = function(...) desserte,
    acquire_foret = function(...) foret_toy, .package = "foretaccess")
  res <- nemetonshiny:::run_desserte(aoi_path, "glouton", cache)
  expect_identical(res$status, "success")
  expect_length(res$avertissements, 0L)
  expect_null(nemetonshiny:::pipeline_engine_outcome(
    res, nemetonshiny:::get_i18n("fr"), "desserte_engine_failed")$message)
})

test_that("run_desserte_osm : l'alerte de tuile de la BD TOPO est rendue", {
  skip_if_not_installed("foretaccess")
  withr::with_tempdir({
    cd <- getwd()
    aoi <- sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(1000, 0), c(1000, 1000), c(0, 1000), c(0, 0)))), crs = 2154))
    ap <- file.path(cd, "aoi.gpkg")
    sf::st_write(aoi, ap, quiet = TRUE)
    ligne <- function(y) sf::st_linestring(rbind(c(0, y), c(500, y)))
    faux <- sf::st_sf(highway = "track",
                      geometry = sf::st_sfc(list(ligne(100)), crs = 2154))
    bd <- sf::st_sf(classe = "route",
                    geometry = sf::st_sfc(list(ligne(900)), crs = 2154))
    res <- testthat::with_mocked_bindings(
      nemetonshiny:::run_desserte_osm(cd, ap, buffer_m = 0),
      acquire_desserte_osm = function(...) faux,
      acquire_desserte = function(...) { .avert_tuile(); bd },
      comparer_desserte_osm = function(...) list(resume = list(a = 1), corridor_m = 15),
      .package = "foretaccess")
    expect_identical(res$status, "success")
    expect_length(res$avertissements, 1L)
    expect_match(res$avertissements, "tuile WFS", fixed = TRUE)
  })
})

test_that(".acquire_mnt_desserte (accessibilite, correction LiDAR) rend l'alerte", {
  skip_if_not_installed("foretaccess")
  skip_if_not_installed("terra")
  withr::with_tempdir({
    cd <- getwd()
    aoi <- sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(100, 0), c(100, 100), c(0, 100), c(0, 0)))), crs = 2154))
    ap <- file.path(cd, "aoi.gpkg")
    sf::st_write(aoi, ap, quiet = TRUE)
    mnt <- file.path(cd, "mnt.tif")
    terra::writeRaster(terra::rast(nrows = 2, ncols = 2, vals = 1:4,
                                   crs = "EPSG:2154"), mnt)
    testthat::local_mocked_bindings(
      .acquire_mnt_highres = function(...) mnt, .package = "nemetonshiny")
    testthat::local_mocked_bindings(
      acquire_desserte = function(...) { .avert_tuile(); .desserte_toy() },
      .package = "foretaccess")
    acq <- nemetonshiny:::.acquire_mnt_desserte(ap, file.path(cd, "cache"))
    expect_identical(acq$status, "ok")
    expect_length(acq$avertissements, 1L)
    expect_match(acq$avertissements, "1 tuile WFS sur 4", fixed = TRUE)
  })
})

# --- Rapport du pipeline : etape `ok` + message ------------------------------

test_that("pipeline_alertes_message : intitule + message complet sur une ligne, sans ANSI", {
  i18n <- nemetonshiny:::get_i18n("fr")
  a <- "1 tuile WFS sur 4 en echec.\n! Incomplete.\n\033[31mx Derniere erreur : boum\033[39m"
  m <- nemetonshiny:::pipeline_alertes_message(a, i18n)
  expect_match(m, i18n$t("tuiles_en_echec"), fixed = TRUE)
  expect_match(m, "1 tuile WFS sur 4 en echec. ! Incomplete. x Derniere erreur : boum",
               fixed = TRUE)
  expect_false(grepl("\n|\033", m))
  expect_null(nemetonshiny:::pipeline_alertes_message(character(0), i18n))
  expect_null(nemetonshiny:::pipeline_alertes_message(NULL, i18n))
  expect_null(nemetonshiny:::pipeline_alertes_message("", i18n))
})

test_that("pipeline_engine_outcome : succes avec alerte = ok + message + alerte", {
  i18n <- nemetonshiny:::get_i18n("fr")
  res <- list(status = "success",
              avertissements = "1 tuile WFS sur 4 en echec pour X.")
  o <- nemetonshiny:::pipeline_engine_outcome(
    res, i18n, "desserte_engine_failed", step_id = "desserte", run_id = "r1")
  expect_true(o$ok)
  expect_identical(o$status, "ok")
  expect_true(o$alerte)
  expect_match(o$message, "1 tuile WFS sur 4", fixed = TRUE)
  expect_null(o$ko_run)
  # Sans alerte : comportement inchange (message NULL, alerte FALSE).
  o0 <- nemetonshiny:::pipeline_engine_outcome(
    list(status = "success", avertissements = character(0)), i18n, "x")
  expect_null(o0$message)
  expect_false(o0$alerte)
  # Un echec n'est jamais une alerte.
  oe <- nemetonshiny:::pipeline_engine_outcome(
    list(status = "error", reason = "desserte_desserte_failed",
         avertissements = "1 tuile WFS"), i18n, "x")
  expect_false(oe$alerte)
  expect_identical(oe$status, "error")
})

test_that("l'alerte arrive dans le rapport d'une etape ok (pipeline_answer -> pipeline_record)", {
  i18n <- nemetonshiny:::get_i18n("fr")
  res <- list(status = "success", avertissements = "1 tuile WFS sur 4 en echec.")
  issue <- nemetonshiny:::pipeline_engine_outcome(res, i18n, "desserte_engine_failed")
  etat <- nemetonshiny:::pipeline_new_run(c("desserte"))
  etat <- nemetonshiny:::pipeline_record(etat, "desserte", issue$status, issue$message)
  rep <- nemetonshiny:::pipeline_report(etat)
  expect_identical(rep$status[rep$step_id == "desserte"], "ok")
  expect_match(rep$message[rep$step_id == "desserte"], "1 tuile WFS sur 4", fixed = TRUE)
})

test_that("la cle tuiles_en_echec respecte la convention (<= 30 car., FR/EN)", {
  expect_lte(nchar("tuiles_en_echec"), 30L)
  expect_true(all(c("fr", "en") %in% names(nemetonshiny:::TRANSLATIONS$tuiles_en_echec)))
})

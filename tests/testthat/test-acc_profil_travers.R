# test-acc_profil_travers.R - profil en travers au clic (spec 030 cote coeur).
#
# L'app ne calcule RIEN ici : elle resout les chemins du projet, convertit le
# clic WGS84 vers le CRS de travail, appelle `foretaccess::profil_travers()` et
# dessine. Ces tests couvrent donc la RESOLUTION et la traduction des echecs en
# raisons lisibles, pas la geometrie - qui est testee cote `foretaccess`.

.profil_projet <- function(avec_desserte = TRUE, avec_laz = TRUE,
                           avec_mnt = TRUE) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(d, "cache", "accessibility"), recursive = TRUE,
             showWarnings = FALSE)
  dir.create(file.path(d, "cache", "layers"), recursive = TRUE,
             showWarnings = FALSE)
  if (avec_desserte) {
    file.create(file.path(d, "cache", "accessibility", "desserte_corrigee.gpkg"))
  }
  if (avec_laz) {
    dir.create(file.path(d, "cache", "layers", "lidar_nuage"),
               showWarnings = FALSE)
    file.create(file.path(d, "cache", "layers", "lidar_nuage", "dalle.laz"))
  }
  if (avec_mnt) file.create(file.path(d, "cache", "layers", "lidar_mnt_mosaic.tif"))
  d
}

test_that("chaque ingredient manquant a sa propre raison", {
  skip_if_not_installed("sf")

  expect_identical(
    nemetonshiny:::acc_profil_travers(NULL, 6, 46)$reason,
    "acc_profil_no_project")

  # Projet sans desserte corrigee : c'est la correction LiDAR qu'il faut lancer,
  # pas un nuage qu'il faut acquerir. Les deux messages different.
  p1 <- .profil_projet(avec_desserte = FALSE)
  expect_identical(nemetonshiny:::acc_profil_travers(p1, 6, 46)$reason,
                   "acc_profil_no_desserte")

  p2 <- .profil_projet(avec_laz = FALSE)
  expect_identical(nemetonshiny:::acc_profil_travers(p2, 6, 46)$reason,
                   "acc_profil_no_lidar")

  p3 <- .profil_projet(avec_mnt = FALSE)
  expect_identical(nemetonshiny:::acc_profil_travers(p3, 6, 46)$reason,
                   "acc_profil_no_mnt")
})

test_that("le clic WGS84 est transmis au coeur dans le CRS de travail", {
  skip_if_not_installed("sf")
  proj <- .profil_projet()
  vu <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    st_read = function(dsn, layer, ...) {
      sf::st_sf(id = 1L,
                geometry = sf::st_sfc(sf::st_linestring(
                  rbind(c(900000, 6500000), c(900100, 6500000))), crs = 2154))
    },
    .package = "sf")
  testthat::local_mocked_bindings(
    profil_travers = function(desserte, xy, las_source, mnt, crs, ...) {
      vu$xy <- xy; vu$crs <- crs; vu$las <- las_source
      list(points = data.frame(x_travers = 0, z = 0), bords = data.frame())
    },
    .package = "foretaccess")

  out <- nemetonshiny:::acc_profil_travers(proj, lng = 6.1, lat = 46.2)
  expect_identical(out$status, "success")
  # Le coeur doit recevoir un POINT projete, pas des degres.
  expect_s3_class(vu$xy, "sfc")
  expect_equal(sf::st_crs(vu$xy)$epsg, 2154L)
  expect_equal(vu$crs, 2154)
  # Et le nuage du projet, pas un chemin devine.
  expect_true(grepl("lidar_nuage$", vu$las))
})

test_that("aucun troncon sous le clic est une REPONSE, pas une panne", {
  skip_if_not_installed("sf")
  proj <- .profil_projet()
  testthat::local_mocked_bindings(
    st_read = function(dsn, layer, ...) {
      sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_linestring(
        rbind(c(900000, 6500000), c(900100, 6500000))), crs = 2154))
    }, .package = "sf")
  testthat::local_mocked_bindings(
    profil_travers = function(...) NULL, .package = "foretaccess")

  out <- nemetonshiny:::acc_profil_travers(proj, 6.1, 46.2)
  # `empty` et non `error` : le module en tire un avertissement, pas une alerte.
  expect_identical(out$status, "empty")
  expect_identical(out$reason, "acc_profil_no_segment")
})

test_that("une erreur du coeur est rapportee sans faire tomber l'app", {
  skip_if_not_installed("sf")
  proj <- .profil_projet()
  testthat::local_mocked_bindings(
    st_read = function(dsn, layer, ...) {
      sf::st_sf(id = 1L, geometry = sf::st_sfc(sf::st_linestring(
        rbind(c(900000, 6500000), c(900100, 6500000))), crs = 2154))
    }, .package = "sf")
  testthat::local_mocked_bindings(
    profil_travers = function(...) stop("dalle illisible"), .package = "foretaccess")

  out <- nemetonshiny:::acc_profil_travers(proj, 6.1, 46.2)
  expect_identical(out$status, "error")
  expect_identical(out$reason, "acc_profil_failed")
  expect_match(out$detail, "dalle illisible", fixed = TRUE)
})

# --- Planche -----------------------------------------------------------------

.profil_factice <- function(n = 40) {
  x <- seq(-15, 15, length.out = n)
  list(
    status = "success",
    points = data.frame(x_travers = x, z = abs(x) / 5,
                        intensite = 100, sol = abs(x) < 3,
                        classification = 2L),
    sol = data.frame(x_travers = x, z = abs(x) / 8),
    ajustement = list(a = 0.02, b = 0, c = 0, rmse = 0.03, n = 20,
                      source = "points_sol"),
    bords = data.frame(
      type = c("drivable", "road", "rescue", "right_of_way", "shoulder"),
      cote = "les_deux", x_gauche = c(-1.6, -2.5, -4, -10, -3),
      x_droite = c(1.6, 2.5, 4, 10, 3),
      largeur_m = c(3.2, 5, 8, 20, 6), stringsAsFactors = FALSE),
    station = list(chainage_m = 65),
    meta = list(n_points = n))
}

test_that("la planche se construit sur le contrat du coeur", {
  skip_if_not_installed("plotly")
  i18n <- nemetonshiny:::get_i18n("fr")
  p <- nemetonshiny:::plot_desserte_profil(.profil_factice(), i18n)
  expect_s3_class(p, "plotly")

  b <- plotly::plotly_build(p)
  noms <- vapply(b$x$data, function(d) d$name %||% "", character(1))
  # Une trace par famille de bords, libellee ET cotee : la largeur se lit sur la
  # legende, sans survol.
  for (lab in c("Chaussée roulable", "Plateforme", "Emprise")) {
    expect_true(any(grepl(lab, noms, fixed = TRUE)), info = lab)
  }
  expect_true(any(grepl("20.0 m", noms, fixed = TRUE)))
})

test_that("la planche ne s'invente pas de donnees quand il n'y en a pas", {
  skip_if_not_installed("plotly")
  i18n <- nemetonshiny:::get_i18n("fr")
  vide <- .profil_factice(); vide$points <- vide$points[0, ]
  expect_null(nemetonshiny:::plot_desserte_profil(vide, i18n))
  expect_null(nemetonshiny:::plot_desserte_profil(list(), i18n))
})

test_that("la parabole n'est tracee que sur la largeur roulable", {
  skip_if_not_installed("plotly")
  i18n <- nemetonshiny:::get_i18n("fr")
  b <- plotly::plotly_build(
    nemetonshiny:::plot_desserte_profil(.profil_factice(), i18n))
  tr <- Filter(function(d) grepl("Chaussée ajustée", d$name %||% ""),
               b$x$data)
  expect_length(tr, 1L)
  # Extrapoler au-dela de la chaussee ferait lire une mesure la ou il n'y en a
  # pas : les bornes doivent rester celles du bord `drivable` (-1.6 / +1.6).
  expect_gte(min(tr[[1]]$x), -1.7)
  expect_lte(max(tr[[1]]$x), 1.7)
})

test_that("la modale du profil offre le plein ecran, comme le suivi sanitaire", {
  # La modale est montee dans un observeEvent : on compare le CODE des deux
  # modules. Ce qui est verifie ici, c'est que le patron est REPRIS et non
  # reecrit — le jour ou l'un des deux evolue, l'ecart doit se voir.
  f <- testthat::test_path("..", "..", "R", "mod_accessibility.R")
  g <- testthat::test_path("..", "..", "R", "mod_monitoring_pixel_map.R")
  testthat::skip_if_not(file.exists(f) && file.exists(g),
                        "sources R absentes (package installe)")
  acc <- readLines(f, warn = FALSE)
  mon <- readLines(g, warn = FALSE)

  bascule <- "modal-dialog').classList.toggle('modal-fullscreen')"
  expect_true(any(grepl(bascule, acc, fixed = TRUE)))
  expect_true(any(grepl(bascule, mon, fixed = TRUE)))

  # `resize` differe : plotly en mode responsive n'ecoute que window.resize.
  # Sans lui, le graphe reste a sa taille initiale dans la modale agrandie —
  # le bouton semblerait ne rien faire.
  resize <- "window.dispatchEvent(new Event('resize'))"
  expect_true(any(grepl(resize, acc, fixed = TRUE)))

  # La hauteur doit suivre : un conteneur nomme + sa regle en plein ecran.
  expect_true(any(grepl('class = "profil-wrap"', acc, fixed = TRUE)))
  expect_true(any(grepl("modal-fullscreen .profil-wrap", acc, fixed = TRUE)))

  # Libelle partage avec le suivi sanitaire plutot qu'une cle jumelle.
  expect_true(any(grepl("monitoring_pixel_map_fullscreen", acc, fixed = TRUE)))
})

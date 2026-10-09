# Nuage de points de drone (spec 059) : l'app range, choisit les references et
# appelle `nemeton::traiter_nuage_points()`, simule ici.

skip_if_not_installed("terra")

.projet_nuage <- function() {
  p <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(p, "cache", "layers"), recursive = TRUE)
  p
}

.raster <- function(f, valeur) {
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 846000, xmax = 846010,
                   ymin = 6687000, ymax = 6687010, crs = "EPSG:2154", vals = valeur)
  terra::writeRaster(r, f, overwrite = TRUE, NAflag = -9999)
  f
}

test_that("le depot range les .las/.laz et refuse le reste", {
  p <- .projet_nuage()
  src <- withr::local_tempdir()
  f <- file.path(src, c("0.laz", "1.las", "2.txt"))
  for (x in f) writeLines("x", x)
  dep <- nemetonshiny:::nuage_deposer(p, f, c("vol.copc.laz", "vol2.LAS", "notes.txt"))
  expect_equal(basename(dep$deposes), c("vol.copc.laz", "vol2.LAS"))
  expect_equal(dep$refuses, "notes.txt")
  expect_length(nemetonshiny:::nuage_fichiers(p), 2L)
  # Un nom avec chemin ne sort pas du dossier du nuage.
  dep <- nemetonshiny:::nuage_deposer(p, f[1], "../../evade.laz")
  expect_equal(dirname(dep$deposes), nemetonshiny:::nuage_dossier(p))
})

test_that("les references LiDAR ignorent une mosaique vide", {
  p <- .projet_nuage()
  L <- file.path(p, "cache", "layers")
  .raster(file.path(L, "lidar_mnt_mosaic.tif"), -9999)
  .raster(file.path(L, "lidar_mnt", "dtm.tif"), 480)
  .raster(file.path(L, "lidar_mnh_mosaic.tif"), 12)
  refs <- nemetonshiny:::nuage_references_lidar(p)
  expect_equal(refs$mnt, file.path(L, "lidar_mnt", "dtm.tif"))
  expect_equal(refs$mnh, file.path(L, "lidar_mnh_mosaic.tif"))
  # Sans LiDAR : repli BD ALTI pour le MNT, pas de MNH.
  p2 <- .projet_nuage()
  .raster(file.path(p2, "cache", "layers", "dem.tif"), 470)
  refs <- nemetonshiny:::nuage_references_lidar(p2)
  expect_match(refs$mnt, "dem\\.tif$")
  expect_null(refs$mnh)
})

test_that("traiter : sans nuage, sans MNT pour la photogrammetrie", {
  p <- .projet_nuage()
  expect_equal(nemetonshiny:::nuage_traiter(p, "lidar_drone")$status, "sans_nuage")
  dir.create(nemetonshiny:::nuage_dossier(p))
  writeLines("x", file.path(nemetonshiny:::nuage_dossier(p), "vol.laz"))
  expect_equal(nemetonshiny:::nuage_traiter(p, "photogrammetrie")$status, "sans_mnt")
})

test_that("traiter appelle le coeur avec les references et garde ses avertissements", {
  p <- .projet_nuage()
  L <- file.path(p, "cache", "layers")
  .raster(file.path(L, "lidar_mnt_mosaic.tif"), 480)
  .raster(file.path(L, "lidar_mnh_mosaic.tif"), 12)
  dir.create(nemetonshiny:::nuage_dossier(p))
  writeLines("x", file.path(nemetonshiny:::nuage_dossier(p), "vol.laz"))
  vu <- NULL
  local_mocked_bindings(
    traiter_nuage_points = function(nuage, type, mnt_externe, mnh_reference,
                                    dossier, ncores, ...) {
      vu <<- list(nuage = nuage, type = type, mnt = mnt_externe,
                  mnh = mnh_reference, dossier = dossier)
      warning("Only 3 % of the points were classified as ground.")
      list(mnt = .raster(file.path(L, "drone_mnt", "mnt.tif"), 480),
           mns = .raster(file.path(L, "drone_mns", "mns.tif"), 495),
           mnh = .raster(file.path(L, "drone_mnh", "mnh.tif"), 15),
           classes = data.frame(), elapsed = 1,
           qualite = list(n_points = 1e6, densite = 120, part_sol = NA,
                          part_bruit = 0.001, part_mnh_negatif = 0.01,
                          decalage_vertical = 2.3, decalage_iqr = 0.02,
                          n_sol_nu = 5000L))
    }, .package = "nemeton")
  res <- nemetonshiny:::nuage_traiter(p, "photogrammetrie", ncores = 1L)
  expect_equal(res$status, "ok")
  expect_equal(vu$type, "photogrammetrie")
  expect_equal(vu$mnt, file.path(L, "lidar_mnt_mosaic.tif"))
  expect_equal(vu$mnh, file.path(L, "lidar_mnh_mosaic.tif"))
  expect_equal(vu$dossier, L)
  expect_match(res$avertissements, "ground")
  # Relu a la reouverture du projet.
  der <- nemetonshiny:::nuage_dernier_traitement(p)
  expect_equal(der$status, "ok")
  expect_equal(der$qualite$decalage_vertical, 2.3)
  # Bilan : decalage affiche pour la photogrammetrie, avertissement visible.
  html <- as.character(nemetonshiny:::nuage_bilan_ui(der, nemetonshiny:::get_i18n("fr")))
  expect_match(html, "2,30", fixed = TRUE)
  expect_match(html, "ground", fixed = TRUE)
  # Rasters disparus : plus de bilan.
  unlink(file.path(L, "drone_mnh"), recursive = TRUE)
  expect_null(nemetonshiny:::nuage_dernier_traitement(p))
})

test_that("une erreur du coeur est rendue, pas levee", {
  p <- .projet_nuage()
  dir.create(nemetonshiny:::nuage_dossier(p))
  writeLines("x", file.path(nemetonshiny:::nuage_dossier(p), "vol.laz"))
  local_mocked_bindings(traiter_nuage_points = function(...) stop("lasR absent"),
                        .package = "nemeton")
  res <- nemetonshiny:::nuage_traiter(p, "lidar_drone", ncores = 1L)
  expect_equal(res$status, "error")
  expect_match(res$message, "lasR absent")
})

test_that("le raster d'affichage est sous-echantillonne", {
  f <- withr::local_tempfile(fileext = ".tif")
  r <- terra::rast(nrows = 400, ncols = 400, xmin = 0, xmax = 400, ymin = 0,
                   ymax = 400, crs = "EPSG:2154", vals = 1)
  terra::writeRaster(r, f)
  a <- nemetonshiny:::.nuage_raster_affichage(f, max_cellules = 1e4)
  expect_lte(terra::ncell(a), 1e4)
  expect_null(nemetonshiny:::.nuage_raster_affichage(NULL))
})

test_that("le module relit le dernier traitement du projet ouvert", {
  skip_if_not_installed("shiny")
  p <- .projet_nuage()
  local_mocked_bindings(nuage_dernier_traitement = function(path)
    list(status = "ok", type = "lidar_drone", qualite = list(n_points = 10)),
    .package = "nemetonshiny")
  app_state <- shiny::reactiveValues(current_project = list(path = p),
                                     language = "fr")
  shiny::testServer(nemetonshiny:::mod_nuage_points_server,
                    args = list(app_state = app_state), {
    session$setInputs(couche = "mnh")
    expect_equal(session$returned()$type, "lidar_drone")
    expect_match(as.character(output$bilan$html), "NDP 2", fixed = TRUE)
  })
})

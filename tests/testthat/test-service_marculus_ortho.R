# Fond ortho 20 cm des GeoPackages Marculus (R/service_marculus_ortho.R)

# Nombre de matrices de tuiles declarees sans aucune tuile.
.matrices_vides <- function(gpkg) {
  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg)
  on.exit(DBI::dbDisconnect(con))
  as.integer(DBI::dbGetQuery(con, paste(
    "SELECT COUNT(*) AS n FROM gpkg_tile_matrix WHERE table_name = 'ortho'",
    "AND zoom_level NOT IN (SELECT DISTINCT zoom_level FROM ortho)"))$n)
}

.parcelle_orleans <- function(dx = 0) {
  sf::st_sf(proprietaire = "Commune", geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(1.900 + dx, 47.900), c(1.905 + dx, 47.900), c(1.905 + dx, 47.903),
      c(1.900 + dx, 47.903), c(1.900 + dx, 47.900)))), crs = 4326))
}

test_that("l'emprise du fond est alignee sur la grille du zoom 19 et couvre +50 m", {
  par <- .parcelle_orleans()
  bb <- nemetonshiny:::.marculus_ortho_bbox(par)
  res <- nemetonshiny:::.marculus_ortho_res()
  o <- -pi * 6378137
  expect_length(bb, 4L)
  # Chaque bord tombe sur un bord de pixel du zoom 19.
  expect_true(all(abs(((bb - o) / res) - round((bb - o) / res)) < 1e-6))
  # L'emprise contient les parcelles elargies de 50 m.
  z <- sf::st_transform(sf::st_buffer(sf::st_transform(par, 2154), 50), 3857)
  zb <- as.numeric(sf::st_bbox(z))
  expect_true(bb[1] <= zb[1] && bb[2] <= zb[2] && bb[3] >= zb[3] && bb[4] >= zb[4])
  expect_null(nemetonshiny:::.marculus_ortho_bbox(NULL))
})

test_that("le chemin en cache depend de l'emprise, et d'elle seule", {
  bb1 <- nemetonshiny:::.marculus_ortho_bbox(.parcelle_orleans())
  bb2 <- nemetonshiny:::.marculus_ortho_bbox(.parcelle_orleans(0.01))
  c1 <- nemetonshiny:::.marculus_ortho_chemin("/p", "ug 1", bb1)
  expect_identical(c1, nemetonshiny:::.marculus_ortho_chemin("/p", "ug 1", bb1))
  expect_false(identical(c1, nemetonshiny:::.marculus_ortho_chemin("/p", "ug 1", bb2)))
  expect_match(c1, "^/p/cache/layers/ortho_marculus/ug_1_[0-9a-f]{12}\\.gpkg$")
})

test_that("un fond par UGF, et seuls les manquants sont a preparer", {
  proj <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .marculus_parcelles = function(project, ug_id) {
      .parcelle_orleans(if (ug_id == "u1") 0 else 0.01)
    })
  actions <- list(list(ug_id = "u1"), list(ug_id = "u1"), list(ug_id = "u2"))
  tr <- nemetonshiny:::marculus_ortho_travaux(list(path = proj), actions)
  expect_equal(vapply(tr, `[[`, "", "ug_id"), c("u1", "u2"))
  dir.create(dirname(tr[[1]]$chemin), recursive = TRUE)
  file.create(tr[[1]]$chemin)
  m <- nemetonshiny:::marculus_ortho_manquants(list(path = proj), actions)
  expect_equal(vapply(m, `[[`, "", "ug_id"), "u2")
})

test_that("les dalles WMS couvrent l'emprise sans depasser 4 000 px", {
  res <- nemetonshiny:::.marculus_ortho_res()
  bb <- c(0, 0, 9000 * res, 5000 * res)
  d <- nemetonshiny:::.marculus_ortho_dalles(bb)
  expect_length(d, 6L)                                   # 3 x 2 dalles
  expect_true(all(vapply(d, function(x) x$w <= 4000 && x$h <= 4000, TRUE)))
  expect_equal(sum(vapply(d, function(x) x$w * x$h, 1)), 9000 * 5000)
  expect_true(all(grepl("LAYERS=HR.ORTHOIMAGERY.ORTHOPHOTOS", vapply(d, `[[`, "", "url"),
                        fixed = TRUE)))
  expect_true(all(grepl("CRS=EPSG:3857", vapply(d, `[[`, "", "url"), fixed = TRUE)))
})

test_that("le fond est construit en table de tuiles Web Mercator, zoom 19", {
  skip_if_not_installed("httr2")
  res <- nemetonshiny:::.marculus_ortho_res()
  o <- -pi * 6378137
  x0 <- o + round((211500 - o) / res) * res
  y0 <- o + round((6097000 - o) / res) * res
  bb <- c(x0, y0, x0 + 600 * res, y0 + 400 * res)
  testthat::local_mocked_bindings(
    req_perform_parallel = function(reqs, paths, ...) {
      lapply(seq_along(reqs), function(i) {
        r <- terra::rast(nrows = 400, ncols = 600, nlyrs = 3,
                         vals = sample(0:255, 600 * 400 * 3, TRUE))
        terra::writeRaster(r, paths[i], filetype = "JPEG", datatype = "INT1U",
                           overwrite = TRUE)
        # Un JPEG du WMS ne porte AUCUN georeferencement : c'est le world
        # file ecrit par le service qui le place. Retirer celui de terra.
        unlink(paste0(paths[i], ".aux.xml"))
        structure(list(status_code = 200L), class = "httr2_response")
      })
    },
    resp_status = function(resp) resp$status_code,
    .package = "httr2"
  )
  chemin <- file.path(withr::local_tempdir(), "cache", "u1_abc.gpkg")
  out <- suppressWarnings(nemetonshiny:::marculus_ortho_construire(bb, chemin))
  expect_equal(out, chemin)
  expect_true(file.exists(chemin))
  expect_false(file.exists(sub("\\.gpkg$", ".part.gpkg", chemin)))
  r <- terra::rast(paste0("GPKG:", chemin, ":ortho"))
  expect_equal(terra::crs(r, describe = TRUE)$code, "3857")
  expect_equal(terra::res(r)[1], res, tolerance = 1e-6)
  # Aucune matrice sans tuile : Marculus (NGA) plante sur une matrice vide.
  skip_if_not_installed("RSQLite")
  expect_equal(.matrices_vides(chemin), 0L)
})

test_that("un echec WMS ne laisse aucun fond a moitie ecrit", {
  skip_if_not_installed("httr2")
  testthat::local_mocked_bindings(
    req_perform_parallel = function(reqs, paths, ...) {
      lapply(reqs, function(r) structure(list(), class = "httr2_failure"))
    },
    .package = "httr2"
  )
  chemin <- file.path(withr::local_tempdir(), "u1_abc.gpkg")
  res <- nemetonshiny:::.marculus_ortho_res()
  expect_warning(out <- nemetonshiny:::marculus_ortho_construire(
    c(0, 0, 100 * res, 100 * res), chemin), "Fond ortho")
  expect_null(out)
  expect_false(file.exists(chemin))
})

test_that("le fond en cache devient la base du GeoPackage du chantier", {
  res <- nemetonshiny:::.marculus_ortho_res()
  base <- file.path(withr::local_tempdir(), "fond.gpkg")
  r <- terra::rast(nrows = 256, ncols = 256, nlyrs = 3, vals = 128,
                   xmin = 0, xmax = 256 * res, ymin = 0, ymax = 256 * res,
                   crs = "EPSG:3857")
  tif <- tempfile(fileext = ".tif")
  terra::writeRaster(r, tif, datatype = "INT1U")
  sf::gdal_utils("translate", tif, base, quiet = TRUE, options = c(
    "-of", "GPKG", "-co", "RASTER_TABLE=ortho",
    "-co", "TILING_SCHEME=GoogleMapsCompatible", "-co", "TILE_FORMAT=JPEG"))
  testthat::local_mocked_bindings(
    .marculus_parcelles = function(project, ug_id) .parcelle_orleans())
  f <- file.path(withr::local_tempdir(), "ctx.gpkg")
  empreinte <- tools::md5sum(base)
  nemetonshiny:::marculus_write_action_gpkg(list(), list(ug_id = "u1"), f,
                                            ortho = base)
  expect_true("parcelle" %in% sf::st_layers(f)$name)
  expect_silent(terra::rast(paste0("GPKG:", f, ":ortho")))
  # Le fond en cache n'est pas modifie par l'ajout des couches vecteur.
  expect_identical(unname(tools::md5sum(base)), unname(empreinte))
})

test_that("un fond de la v0.146.0 est repare avant d'etre expedie", {
  # GoogleMapsCompatible declare les zooms 0 a 19 ; sans tuile aux bas zooms,
  # NGA leve une NullPointerException et Marculus n'affiche jamais l'ortho.
  skip_if_not_installed("RSQLite")
  res <- nemetonshiny:::.marculus_ortho_res()
  base <- file.path(withr::local_tempdir(), "ancien.gpkg")
  r <- terra::rast(nrows = 256, ncols = 256, nlyrs = 3, vals = 128,
                   xmin = 0, xmax = 256 * res, ymin = 0, ymax = 256 * res,
                   crs = "EPSG:3857")
  tif <- tempfile(fileext = ".tif")
  terra::writeRaster(r, tif, datatype = "INT1U")
  sf::gdal_utils("translate", tif, base, quiet = TRUE, options = c(
    "-of", "GPKG", "-co", "RASTER_TABLE=ortho",
    "-co", "TILING_SCHEME=GoogleMapsCompatible", "-co", "TILE_FORMAT=JPEG"))
  expect_gt(.matrices_vides(base), 0L)                 # le defaut, reproduit
  expect_equal(nemetonshiny:::.marculus_ortho_reparer(base), base)
  expect_equal(.matrices_vides(base), 0L)
  expect_silent(terra::rast(paste0("GPKG:", base, ":ortho")))
  expect_null(nemetonshiny:::.marculus_ortho_reparer(file.path(tempdir(), "absent.gpkg")))
})

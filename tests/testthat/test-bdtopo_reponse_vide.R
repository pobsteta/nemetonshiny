# Mission 032 : une reponse BD TOPO vide est une mesure (W1 = 0, pas NA)

.bdtopo_bbox <- c(6.0, 47.0, 6.01, 47.01)

.bdtopo_ligne <- function() {
  sf::st_sf(id = 1L, geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(6.0, 47.0, 6.005, 47.005), ncol = 2, byrow = TRUE)),
    crs = 4326))
}

test_that("reponse WFS vide : couche vide Lambert-93 ecrite, relue et rendue", {
  skip_if_not_installed("sf")
  skip_if_not_installed("happign")
  d <- withr::local_tempdir()
  cache_file <- file.path(d, "water_network.gpkg")
  n <- 0L
  local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      n <<- n + 1L
      .bdtopo_ligne()[0, ]
    },
    .package = "happign"
  )
  res <- suppressMessages(nemetonshiny:::download_ign_bdtopo(
    "water_network", .bdtopo_bbox, cache_file))
  expect_s3_class(res, "sf")
  expect_identical(nrow(res), 0L)
  expect_equal(sf::st_crs(res)$epsg, 2154L)
  expect_true(file.exists(cache_file))

  # Relecture par le chemin de cache : sf a 0 ligne, jamais NULL.
  relu <- sf::st_read(cache_file, quiet = TRUE)
  expect_s3_class(relu, "sf")
  expect_identical(nrow(relu), 0L)

  # Par download_vector_source : cache + .bbox.json, pas de 2e interrogation.
  cfg <- list(source = "ign_bd_topo")
  r1 <- suppressMessages(nemetonshiny:::download_vector_source(
    "water_network", cfg, .bdtopo_bbox, d))
  expect_s3_class(r1, "sf")
  expect_identical(nrow(r1), 0L)
  expect_true(file.exists(paste0(cache_file, ".bbox.json")))
  n_apres <- n
  r2 <- suppressMessages(nemetonshiny:::download_vector_source(
    "water_network", cfg, .bdtopo_bbox, d))
  expect_s3_class(r2, "sf")
  expect_identical(nrow(r2), 0L)
  expect_identical(n, n_apres)
})

test_that("les quatre couches BD TOPO vides donnent un sf vide", {
  skip_if_not_installed("happign")
  local_mocked_bindings(
    get_wfs = function(x, layer, ...) .bdtopo_ligne()[0, ],
    .package = "happign"
  )
  for (couche in c("water_network", "water_surfaces", "roads", "buildings")) {
    d <- withr::local_tempdir()
    res <- suppressMessages(nemetonshiny:::download_ign_bdtopo(
      couche, .bdtopo_bbox, file.path(d, paste0(couche, ".gpkg"))))
    expect_s3_class(res, "sf")
    expect_identical(nrow(res), 0L)
  }
})

test_that("echec (erreur ou NULL de happign) : NULL, aucun fichier", {
  skip_if_not_installed("happign")
  d <- withr::local_tempdir()
  cache_file <- file.path(d, "water_network.gpkg")
  local_mocked_bindings(get_wfs = function(...) NULL, .package = "happign")
  expect_null(suppressMessages(nemetonshiny:::download_ign_bdtopo(
    "water_network", .bdtopo_bbox, cache_file)))
  expect_false(file.exists(cache_file))

  local_mocked_bindings(get_wfs = function(...) stop("reseau"), .package = "happign")
  expect_null(suppressWarnings(suppressMessages(nemetonshiny:::download_ign_bdtopo(
    "water_network", .bdtopo_bbox, cache_file))))
  expect_false(file.exists(cache_file))
})

test_that("couche non vide : comportement inchange", {
  skip_if_not_installed("happign")
  d <- withr::local_tempdir()
  cache_file <- file.path(d, "roads.gpkg")
  local_mocked_bindings(get_wfs = function(...) .bdtopo_ligne(), .package = "happign")
  res <- suppressMessages(nemetonshiny:::download_ign_bdtopo(
    "roads", .bdtopo_bbox, cache_file))
  expect_identical(nrow(res), 1L)
  expect_equal(sf::st_crs(res)$epsg, 2154L)
})

test_that(".bbox_elargie elargit d'au moins la distance, en metres", {
  b <- c(6.0, 47.0, 6.01, 47.01)
  e <- nemetonshiny:::.bbox_elargie(b, 500)
  expect_true(nemetonshiny:::.bbox_covers(e, b))
  # Ordre explicite (xmin, ymin, xmax, ymax), WGS84 : 500 m = ~0,0045 deg en
  # latitude, ~0,0066 deg en longitude a 47 N.
  expect_gte(b[2] - e[2], 500 / 111320 * 0.99)
  expect_gte(e[4] - b[4], 500 / 111320 * 0.99)
  expect_gte((b[1] - e[1]) * 111320 * cos(47 * pi / 180), 500 * 0.99)
  # Point du terrain : un point a 450 m au nord de l'emprise y tombe.
  pt <- sf::st_transform(sf::st_sfc(
    sf::st_point(c(6.005, 47.01)), crs = 4326), 2154)
  pt450 <- sf::st_transform(sf::st_sfc(
    sf::st_point(sf::st_coordinates(pt) + c(0, 450)), crs = 2154), 4326)
  xy <- sf::st_coordinates(pt450)
  expect_true(xy[1, "Y"] <= e[4] && xy[1, "Y"] >= e[2])
})

test_that("water_network : emprise telechargee elargie, cache plus etroit non resservi", {
  skip_if_not_installed("happign")
  d <- withr::local_tempdir()
  vus <- list()
  local_mocked_bindings(
    download_ign_bdtopo = function(layer_name, bbox, cache_file) {
      vus[[length(vus) + 1L]] <<- list(layer = layer_name, bbox = as.numeric(bbox))
      sf::st_write(.bdtopo_ligne(), cache_file, quiet = TRUE, delete_dsn = TRUE)
      .bdtopo_ligne()
    }
  )
  cfg <- list(source = "ign_bd_topo")
  suppressMessages(nemetonshiny:::download_vector_source(
    "water_network", cfg, .bdtopo_bbox, d))
  suppressMessages(nemetonshiny:::download_vector_source(
    "roads", cfg, .bdtopo_bbox, d))
  expect_equal(vus[[2]]$bbox, .bdtopo_bbox)  # roads : emprise inchangee
  eau <- vus[[1]]$bbox
  expect_true(nemetonshiny:::.bbox_covers(eau, .bdtopo_bbox))
  expect_gt(.bdtopo_bbox[2] - eau[2], 0.0044)

  # Cache ecrit avec l'ancienne emprise etroite (sans marge de 500 m) : refait.
  nemetonshiny:::.layer_cache_write_bbox(file.path(d, "water_network.gpkg"), .bdtopo_bbox)
  n0 <- length(vus)
  suppressMessages(nemetonshiny:::download_vector_source(
    "water_network", cfg, .bdtopo_bbox, d))
  expect_gt(length(vus), n0)
  # Cache a l'emprise elargie : reutilise.
  n1 <- length(vus)
  suppressMessages(nemetonshiny:::download_vector_source(
    "water_network", cfg, .bdtopo_bbox, d))
  expect_identical(length(vus), n1)
})

test_that(".masque_ou_null neutralise une couche vide, pas une couche pleine", {
  expect_null(nemetonshiny:::.masque_ou_null(nemetonshiny:::.couche_bdtopo_vide()))
  expect_null(nemetonshiny:::.masque_ou_null(NULL))
  plein <- sf::st_transform(.bdtopo_ligne(), 2154)
  expect_identical(nemetonshiny:::.masque_ou_null(plein), plein)
})

test_that("W1 vaut 0 (et non NA) sur la couche vide ecrite puis relue", {
  skip_if_not_installed("happign")
  skip_if(!exists("indicateur_w1_reseau", asNamespace("nemeton")),
          "indicateur_w1_reseau absent du nemeton installe")
  d <- withr::local_tempdir()
  local_mocked_bindings(
    get_wfs = function(x, layer, ...) .bdtopo_ligne()[0, ],
    .package = "happign"
  )
  cfg <- list(source = "ign_bd_topo")
  eau <- suppressMessages(nemetonshiny:::download_vector_source(
    "water_network", cfg, .bdtopo_bbox, d))
  expect_s3_class(eau, "sf")

  pol <- sf::st_transform(sf::st_sf(id = 1L, geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(c(6.0, 47.0, 6.005, 47.0, 6.005, 47.005,
                                 6.0, 47.005, 6.0, 47.0), ncol = 2, byrow = TRUE))),
    crs = 4326)), 2154)
  layers <- nemeton::nemeton_layers(vectors = list(
    water_network = file.path(d, "water_network.gpkg")), validate = FALSE)
  res <- suppressWarnings(suppressMessages(
    nemeton::indicateur_w1_reseau(pol, layers)))
  expect_false(anyNA(res$W1))
  expect_equal(res$W1, 0)
})

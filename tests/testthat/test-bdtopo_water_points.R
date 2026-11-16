# Mission 035 : W1 lit les sources - couche BD TOPO `water_points`
# Coordonnees : (x = longitude, y = latitude) en WGS84 (4326) puis Lambert-93 (2154).

.wp_bbox <- c(6.0, 47.0, 6.01, 47.01)  # xmin, ymin, xmax, ymax

.wp_parcelles <- function() {
  sf::st_sf(id = "p1", geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(c(2.0, 48.0, 2.001, 48.0, 2.001, 48.001,
                                 2.0, 48.001, 2.0, 48.0),
                               ncol = 2, byrow = TRUE))),
    crs = 4326))
}

.wp_points <- function(natures = "Source", etats = "En service",
                       xy = cbind(2.0005, 48.0005)) {
  sf::st_transform(sf::st_sf(
    nature = natures, etat_de_l_objet = etats,
    geometry = sf::st_sfc(lapply(seq_len(nrow(xy)),
                                 function(i) sf::st_point(xy[i, ])),
                          crs = 4326)), 2154)
}

test_that("water_points est declare, BD TOPO, requis par W1", {
  v <- nemetonshiny:::DATA_SOURCES$vectors
  expect_true("water_points" %in% names(v))
  expect_equal(v$water_points$type, "vector")
  expect_equal(v$water_points$source, "ign_bd_topo")
  expect_true("indicateur_w1_reseau" %in% v$water_points$required_for)
  expect_true("source_water_points" %in% names(nemetonshiny:::TRANSLATIONS))
})

test_that("water_points : typename detail_hydrographique interroge, aucun filtre", {
  skip_if_not_installed("happign")
  d <- withr::local_tempdir()
  vu <- list()
  local_mocked_bindings(
    get_wfs = function(x, layer, ...) {
      vu[[length(vu) + 1L]] <<- list(layer = layer, args = names(list(...)))
      .wp_points()
    },
    .package = "happign"
  )
  res <- suppressMessages(nemetonshiny:::download_ign_bdtopo(
    "water_points", .wp_bbox, file.path(d, "water_points.gpkg")))
  expect_s3_class(res, "sf")
  expect_identical(vu[[1]]$layer, "BDTOPO_V3:detail_hydrographique")
  # Aucun filtre CQL cote app : le coeur filtre natures et etat.
  expect_false(any(c("cql_filter", "filter") %in% vu[[1]]$args))
})

test_that("water_points : emprise elargie de 500 m, comme water_network", {
  skip_if_not_installed("happign")
  d <- withr::local_tempdir()
  vus <- list()
  local_mocked_bindings(
    download_ign_bdtopo = function(layer_name, bbox, cache_file) {
      vus[[length(vus) + 1L]] <<- list(layer = layer_name, bbox = as.numeric(bbox))
      sf::st_write(.wp_points(), cache_file, quiet = TRUE, delete_dsn = TRUE)
      .wp_points()
    }
  )
  cfg <- list(source = "ign_bd_topo")
  suppressMessages(nemetonshiny:::download_vector_source(
    "water_points", cfg, .wp_bbox, d))
  suppressMessages(nemetonshiny:::download_vector_source(
    "water_network", cfg, .wp_bbox, d))
  expect_equal(vus[[1]]$layer, "water_points")
  expect_true(nemetonshiny:::.bbox_covers(vus[[1]]$bbox, .wp_bbox))
  expect_gt(.wp_bbox[2] - vus[[1]]$bbox[2], 0.0044)  # 500 m ~ 0,0045 deg
  expect_equal(vus[[1]]$bbox, vus[[2]]$bbox)
})

.wp_telecharger <- function(water_points) {
  d <- withr::local_tempdir()
  local_mocked_bindings(
    download_raster_source = function(...) NULL,
    download_ign_lidar_hd = function(...) NULL,
    download_vector_source = function(source_name, ...) {
      if (identical(source_name, "water_points")) return(water_points)
      NULL
    }
  )
  suppressWarnings(suppressMessages(nemetonshiny:::download_layers_for_parcels(
    parcels = .wp_parcelles(), project_path = d)))
}

test_that("couche water_points : passee au coeur ; vide = mesure ; echec = absente", {
  skip_if_not_installed("sf")
  ok <- .wp_telecharger(.wp_points())
  expect_s3_class(ok$vectors$water_points, "sf")
  expect_identical(nrow(ok$vectors$water_points), 1L)

  vide <- .wp_telecharger(nemetonshiny:::.couche_bdtopo_vide())
  expect_true("water_points" %in% names(vide$vectors))
  expect_identical(nrow(vide$vectors$water_points), 0L)

  ko <- .wp_telecharger(NULL)
  expect_false("water_points" %in% names(ko$vectors))
})

# --- Appels reels du coeur (>= 2026.11.34) -----------------------------------

.wp_w1 <- function(couche_sources) {
  skip_if(utils::packageVersion("nemeton") < "2026.11.34",
          "nemeton < 2026.11.34 : pas de spring_layer")
  # UGF (~70 m x 111 m) sans cours d'eau : le seul troncon est tres loin (> 500 m).
  ugf <- sf::st_transform(.wp_parcelles(), 2154)
  eau <- sf::st_transform(sf::st_sf(id = 1L, geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(2.5, 48.5, 2.501, 48.501), ncol = 2, byrow = TRUE)),
    crs = 4326)), 2154)
  d <- withr::local_tempdir()
  sf::st_write(eau, file.path(d, "water_network.gpkg"), quiet = TRUE)
  vect <- list(water_network = file.path(d, "water_network.gpkg"))
  if (!is.null(couche_sources)) {
    sf::st_write(couche_sources, file.path(d, "water_points.gpkg"), quiet = TRUE)
    vect$water_points <- file.path(d, "water_points.gpkg")
  }
  layers <- nemeton::nemeton_layers(vectors = vect, validate = FALSE)
  res <- suppressWarnings(suppressMessages(
    nemeton::indicateur_w1_reseau(ugf, layers)))
  res$W1
}

test_that("coeur reel : une Source En service a ~100 m donne W1 > 0", {
  # 0,0009 deg de latitude ~ 100 m au nord du bord de l'UGF (y = 48.001).
  pt <- .wp_points("Source", "En service", cbind(2.0005, 48.001 + 0.0009))
  expect_gt(.wp_w1(pt), 0)
})

test_that("coeur reel : une Fontaine seule donne 0 (ecartee par le coeur)", {
  pt <- .wp_points("Fontaine", "En service", cbind(2.0005, 48.001 + 0.0009))
  expect_equal(.wp_w1(pt), 0)
})

test_that("coeur reel : water_points absente, W1 calcule (non NA)", {
  expect_false(is.na(.wp_w1(NULL)))
})

test_that("echec water_points : ligne info dediee, pas le message commun", {
  ko <- .wp_telecharger(NULL)
  cles <- vapply(ko$warnings, function(w) w$key %||% "", character(1))
  expect_true("dl_w1_sources_absentes" %in% cles)
  expect_false(any(vapply(ko$warnings, function(w) {
    identical(w$key, "dl_source_vide") && identical(w$source, "Water Points")
  }, logical(1))))
  expect_true("dl_w1_sources_absentes" %in% names(nemetonshiny:::TRANSLATIONS))
  expect_lte(nchar("dl_w1_sources_absentes"), 30L)
  w <- ko$warnings[[which(cles == "dl_w1_sources_absentes")]]
  expect_equal(w$type, "info")
})

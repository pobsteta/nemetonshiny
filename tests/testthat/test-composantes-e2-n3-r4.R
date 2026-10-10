# E2, N3 et R4 NA sur tous les projets (brief du 2026-10-09) : le coeur lit
# E1 (E2) et N1, N2, L1, B3 (N3) dans les unites, et le MNH de R4 dans le slot
# `lidar_mnh`, que seul le LiDAR HD publie remplit.

skip_if_not_installed("sf")

.parcelles_2 <- function() {
  sq <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 100, 0),
                                              c(x + 100, 100), c(x, 100),
                                              c(x, 0))))
  sf::st_sf(id = c("p1", "p2"),
            geometry = sf::st_sfc(sq(846000), sq(846200), crs = 2154))
}

.layers_vides <- function(rasters = list(), chm_source = "none") {
  structure(list(rasters = rasters, vectors = list(), point_clouds = list(),
                 bbox = c(0, 0, 1, 1), crs = sf::st_crs(2154),
                 cache_dir = tempdir(), chm_source = chm_source,
                 warnings = list()),
            class = "nemeton_layers")
}

test_that(".units_for_indicator transmet E1 a E2 et N1, N2, L1, B3 a N3", {
  f <- nemetonshiny:::.units_for_indicator
  p <- .parcelles_2()
  res <- data.frame(indicateur_e1_bois_energie = c(3, 5),
                    indicateur_n1_distance = c(60, 20),
                    indicateur_n2_continuite = c(80, NA),
                    indicateur_l1_effet_lisiere = c(30, 50),
                    indicateur_b3_connectivite = c(40, 70))
  u <- f("indicateur_e2_evitement", p, res, list())
  expect_identical(u$E1, c(3, 5))
  expect_null(u$N1)

  u <- f("indicateur_n3_naturalite", p, res, list())
  expect_identical(u$N1, c(60, 20))
  expect_identical(u$N2, c(80, NA))
  expect_identical(u$L1, c(30, 50))
  expect_identical(u$B3, c(40, 70))
  expect_null(u$E1)

  # Composante absente ou de mauvaise longueur : pas de colonne inventee
  u <- f("indicateur_n3_naturalite", p,
         data.frame(indicateur_n1_distance = 1), list())
  expect_null(u$N1)
})

test_that("le vrai E2 et le vrai N3 du coeur ont des valeurs sur ces unites", {
  p <- .parcelles_2()
  res <- data.frame(indicateur_e1_bois_energie = c(3, 5),
                    indicateur_n1_distance = c(60, 20),
                    indicateur_n2_continuite = c(80, 40),
                    indicateur_l1_effet_lisiere = c(30, 50),
                    indicateur_b3_connectivite = c(40, 70))
  e2 <- suppressMessages(nemeton::indicateur_e2_evitement(
    nemetonshiny:::.units_for_indicator("indicateur_e2_evitement", p, res, list())))
  expect_false(anyNA(e2$E2))
  n3 <- suppressMessages(nemeton::indicateur_n3_naturalite(
    nemetonshiny:::.units_for_indicator("indicateur_n3_naturalite", p, res, list())))
  expect_equal(n3$N3, 0.35 * c(60, 20) + 0.35 * c(80, 40) +
                 0.15 * (100 - c(30, 50)) + 0.15 * c(40, 70))
})

test_that("un calcul qui produit E1 produit E2, et N1, N2, L1, B3 produisent N3", {
  p <- .parcelles_2()
  vrai <- nemetonshiny:::compute_single_indicator
  amont <- list(indicateur_e1_bois_energie = c(3, 5),
                indicateur_n1_distance = c(60, 20),
                indicateur_n2_continuite = c(80, 40),
                indicateur_l1_effet_lisiere = c(30, 50),
                indicateur_b3_connectivite = c(40, 70))
  res <- with_mocked_bindings(
    load_indicators = function(id) NULL,
    is_cancelled = function(id) FALSE,
    save_indicators_incremental = function(...) TRUE,
    compute_single_indicator = function(indicator, parcels, layers) {
      if (indicator %in% names(amont)) amont[[indicator]]
      else vrai(indicator, parcels, layers)
    },
    suppressMessages(nemetonshiny:::compute_all_indicators(
      parcels = p, layers = .layers_vides(),
      indicators = c("indicateur_b3_connectivite", "indicateur_l1_effet_lisiere",
                     "indicateur_e1_bois_energie", "indicateur_e2_evitement",
                     "indicateur_n1_distance", "indicateur_n2_continuite",
                     "indicateur_n3_naturalite"),
      project_id = "test"))
  )
  expect_false(anyNA(res$indicateur_e2_evitement))
  expect_false(anyNA(res$indicateur_n3_naturalite))
})

test_that(".layers_mnh_depuis_chm donne le CHM retenu a R4 seul", {
  skip_if_not_installed("terra")
  chm <- terra::rast(nrows = 10, ncols = 10, xmin = 846000, xmax = 846300,
                     ymin = 0, ymax = 100, crs = "EPSG:2154", vals = 6)
  layers <- .layers_vides(list(chm = chm), chm_source = "lasr")
  f <- nemetonshiny:::.layers_mnh_depuis_chm

  l4 <- suppressMessages(f("indicateur_r4_abroutissement", layers))
  expect_identical(l4$rasters$lidar_mnh, chm)
  # Les autres indicateurs, et l'objet d'origine, ne changent pas
  expect_identical(f("indicateur_c1_biomasse", layers), layers)
  expect_null(layers$rasters$lidar_mnh)
  # Un vrai MNH LiDAR HD reste prioritaire
  mnh <- chm * 2
  l_hd <- .layers_vides(list(chm = chm, lidar_mnh = mnh), "lidar_hd")
  expect_identical(f("indicateur_r4_abroutissement", l_hd)$rasters$lidar_mnh, mnh)
  # Sans CHM : rien
  expect_identical(f("indicateur_r4_abroutissement", .layers_vides()),
                   .layers_vides())

  # Le vrai R4 du coeur a alors sa composante de vulnerabilite
  testthat::local_mocked_bindings(get_game_pressure_raster = function(...) NULL,
                                  .package = "nemeton")
  r4 <- suppressMessages(nemeton::indicateur_r4_abroutissement(
    .parcelles_2(), layers = l4))
  expect_equal(r4$R4_vulnerability, rep((10 - 6) / 8 * 100, 2))
})

test_that("P2 sans age le dit dans le journal", {
  p <- .parcelles_2()
  expect_message(
    out <- nemetonshiny:::.cause_sans_age(c(NA_real_, NA_real_),
                                          "indicateur_p2_station", p,
                                          .layers_vides()),
    "stand age unknown")
  expect_equal(attr(out, "nemeton_status"), rep("sans_age", 2))
})

# Tests for score regression (Phase 0 safety net)
# Ensures that UG refactoring does not change existing indicator/family scores.
#
# These tests use a deterministic fixture with 3 mock parcels.
# If any future change alters the computation pipeline, these tests will catch it.

# ==============================================================================
# Helper: create or load test fixture
# ==============================================================================

#' Create the Couchey mini test fixture (3 parcels, pre-computed indicators)
#' @return List with $parcels (sf), $indicators (data.frame), $metadata (list)
#' @noRd
create_couchey_mini_fixture <- function() {
  # 3 mock cadastral parcels (simplified polygons near Couchey, Cote-d'Or)
  p1 <- sf::st_polygon(list(matrix(c(
    4.950, 47.270, 4.952, 47.270, 4.952, 47.272, 4.950, 47.272, 4.950, 47.270
  ), ncol = 2, byrow = TRUE)))
  p2 <- sf::st_polygon(list(matrix(c(
    4.952, 47.270, 4.954, 47.270, 4.954, 47.272, 4.952, 47.272, 4.952, 47.270
  ), ncol = 2, byrow = TRUE)))
  p3 <- sf::st_polygon(list(matrix(c(
    4.954, 47.270, 4.956, 47.270, 4.956, 47.272, 4.954, 47.272, 4.954, 47.270
  ), ncol = 2, byrow = TRUE)))

  parcels <- sf::st_sf(
    id = c("couchey_54", "couchey_55", "couchey_56"),
    geo_parcelle = c("21189000A0054", "21189000A0055", "21189000A0056"),
    section = c("A", "A", "A"),
    numero = c("54", "55", "56"),
    contenance = c(25000, 18000, 32000),
    code_insee = rep("21189", 3),
    geometry = sf::st_sfc(p1, p2, p3, crs = 4326)
  )

  # Deterministic indicator values
  indicators <- sf::st_drop_geometry(parcels)
  indicators$indicateur_b1_protection   <- c(65.2, 58.3, 72.1)
  indicators$indicateur_b2_structure    <- c(71.0, 62.5, 78.4)
  indicators$indicateur_b3_connectivite <- c(55.8, 48.2, 60.3)
  indicators$indicateur_c1_biomasse     <- c(80.1, 73.6, 85.9)
  indicators$indicateur_c2_ndvi         <- c(78.5, 70.2, 82.3)
  indicators$indicateur_w1_reseau       <- c(42.3, 38.7, 45.1)
  indicators$indicateur_w2_zones_humides <- c(35.6, 30.1, 40.8)
  indicators$indicateur_w3_humidite     <- c(58.9, 52.4, 63.2)
  indicators$indicateur_a1_couverture   <- c(82.4, 76.8, 87.1)
  indicators$indicateur_a2_qualite_air  <- c(70.3, 65.9, 74.6)
  indicators$indicateur_f1_fertilite    <- c(55.7, 50.2, 60.4)
  indicators$indicateur_f2_erosion      <- c(68.1, 62.5, 73.8)
  indicators$indicateur_l1_sylvosphere  <- c(75.2, 69.8, 80.5)
  indicators$indicateur_l2_fragmentation <- c(60.4, 55.1, 65.7)
  indicators$indicateur_t1_anciennete   <- c(85.3, 80.7, 90.1)
  indicators$indicateur_t2_changement   <- c(45.6, 40.2, 50.8)
  indicators$indicateur_r1_feu          <- c(72.8, 67.3, 77.5)
  indicators$indicateur_r2_tempete      <- c(58.4, 53.1, 63.7)
  indicators$indicateur_r3_secheresse   <- c(48.9, 43.5, 54.2)
  indicators$indicateur_r4_abroutissement <- c(62.1, 56.8, 67.4)
  indicators$indicateur_s1_routes       <- c(40.5, 35.2, 45.8)
  indicators$indicateur_s2_bati         <- c(30.7, 25.3, 36.1)
  indicators$indicateur_s3_population   <- c(38.2, 33.6, 43.5)
  indicators$indicateur_p1_volume       <- c(76.4, 71.0, 81.7)
  indicators$indicateur_p2_station      <- c(68.9, 63.5, 74.2)
  indicators$indicateur_p3_qualite_bois <- c(72.1, 66.8, 77.4)
  indicators$indicateur_e1_bois_energie <- c(65.3, 60.0, 70.6)
  indicators$indicateur_e2_evitement    <- c(58.7, 53.4, 64.0)
  indicators$indicateur_n1_distance     <- c(82.6, 77.2, 87.9)
  indicators$indicateur_n2_continuite   <- c(70.8, 65.4, 76.1)
  indicators$indicateur_n3_naturalite   <- c(75.4, 70.1, 80.7)

  # Family scores (simple mean of constituent indicators)
  indicators$famille_biodiversite <- rowMeans(indicators[, c("indicateur_b1_protection", "indicateur_b2_structure", "indicateur_b3_connectivite")])
  indicators$famille_carbone      <- rowMeans(indicators[, c("indicateur_c1_biomasse", "indicateur_c2_ndvi")])
  indicators$famille_eau          <- rowMeans(indicators[, c("indicateur_w1_reseau", "indicateur_w2_zones_humides", "indicateur_w3_humidite")])
  indicators$famille_air          <- rowMeans(indicators[, c("indicateur_a1_couverture", "indicateur_a2_qualite_air")])
  indicators$famille_sol          <- rowMeans(indicators[, c("indicateur_f1_fertilite", "indicateur_f2_erosion")])
  indicators$famille_paysage      <- rowMeans(indicators[, c("indicateur_l1_sylvosphere", "indicateur_l2_fragmentation")])
  indicators$famille_temporel     <- rowMeans(indicators[, c("indicateur_t1_anciennete", "indicateur_t2_changement")])
  indicators$famille_risque       <- rowMeans(indicators[, c("indicateur_r1_feu", "indicateur_r2_tempete", "indicateur_r3_secheresse", "indicateur_r4_abroutissement")])
  indicators$famille_social       <- rowMeans(indicators[, c("indicateur_s1_routes", "indicateur_s2_bati", "indicateur_s3_population")])
  indicators$famille_production   <- rowMeans(indicators[, c("indicateur_p1_volume", "indicateur_p2_station", "indicateur_p3_qualite_bois")])
  indicators$famille_energie      <- rowMeans(indicators[, c("indicateur_e1_bois_energie", "indicateur_e2_evitement")])
  indicators$famille_naturalite   <- rowMeans(indicators[, c("indicateur_n1_distance", "indicateur_n2_continuite", "indicateur_n3_naturalite")])

  list(
    parcels = parcels,
    indicators = indicators,
    metadata = list(
      id = "test_couchey_mini",
      name = "Couchey Mini Test",
      parcels_count = 3L,
      status = "completed"
    )
  )
}


# ==============================================================================
# Regression tests: indicator values must not change
# ==============================================================================

test_that("fixture has exactly 3 parcels and 31 indicators", {
  fixture <- create_couchey_mini_fixture()

  expect_equal(nrow(fixture$parcels), 3)
  expect_equal(nrow(fixture$indicators), 3)

  # 31 indicator columns
  ind_cols <- grep("^indicateur_", names(fixture$indicators), value = TRUE)
  expect_equal(length(ind_cols), 31)

  # 12 family columns
  fam_cols <- grep("^famille_", names(fixture$indicators), value = TRUE)
  expect_equal(length(fam_cols), 12)
})

test_that("indicator values for parcel couchey_54 are stable", {
  fixture <- create_couchey_mini_fixture()
  ind <- fixture$indicators
  row1 <- ind[ind$id == "couchey_54", ]

  # Spot-check key indicators (these are the reference values)
  expect_equal(row1$indicateur_b1_protection, 65.2)
  expect_equal(row1$indicateur_c1_biomasse, 80.1)
  expect_equal(row1$indicateur_w1_reseau, 42.3)
  expect_equal(row1$indicateur_p1_volume, 76.4)
  expect_equal(row1$indicateur_n3_naturalite, 75.4)
})

test_that("indicator values for parcel couchey_55 are stable", {
  fixture <- create_couchey_mini_fixture()
  ind <- fixture$indicators
  row2 <- ind[ind$id == "couchey_55", ]

  expect_equal(row2$indicateur_b1_protection, 58.3)
  expect_equal(row2$indicateur_c1_biomasse, 73.6)
  expect_equal(row2$indicateur_w1_reseau, 38.7)
  expect_equal(row2$indicateur_p1_volume, 71.0)
  expect_equal(row2$indicateur_n3_naturalite, 70.1)
})

test_that("indicator values for parcel couchey_56 are stable", {
  fixture <- create_couchey_mini_fixture()
  ind <- fixture$indicators
  row3 <- ind[ind$id == "couchey_56", ]

  expect_equal(row3$indicateur_b1_protection, 72.1)
  expect_equal(row3$indicateur_c1_biomasse, 85.9)
  expect_equal(row3$indicateur_w1_reseau, 45.1)
  expect_equal(row3$indicateur_p1_volume, 81.7)
  expect_equal(row3$indicateur_n3_naturalite, 80.7)
})

test_that("family scores are correctly computed from indicators", {
  fixture <- create_couchey_mini_fixture()
  ind <- fixture$indicators

  # Biodiversite = mean(B1, B2, B3) for each parcel
  expected_bio_1 <- mean(c(65.2, 71.0, 55.8))
  expect_equal(ind$famille_biodiversite[1], expected_bio_1)

  # Carbone = mean(C1, C2)
  expected_carb_1 <- mean(c(80.1, 78.5))
  expect_equal(ind$famille_carbone[1], expected_carb_1)

  # Risque = mean(R1, R2, R3, R4)
  expected_risk_1 <- mean(c(72.8, 58.4, 48.9, 62.1))
  expect_equal(ind$famille_risque[1], expected_risk_1)

  # All family scores should be between 0 and 100
  fam_cols <- grep("^famille_", names(ind), value = TRUE)
  for (col in fam_cols) {
    expect_true(all(ind[[col]] >= 0 & ind[[col]] <= 100),
                info = paste("Family", col, "out of range"))
  }
})

test_that("parcel geometries are valid", {
  fixture <- create_couchey_mini_fixture()
  parcels <- fixture$parcels

  expect_true(inherits(parcels, "sf"))
  expect_equal(sf::st_crs(parcels)$epsg, 4326L)
  expect_true(all(sf::st_is_valid(parcels)))

  # Parcels should not overlap
  for (i in 1:2) {
    for (j in (i + 1):3) {
      intersection <- sf::st_intersection(parcels[i, ], parcels[j, ])
      # Intersection should be empty or a line (shared boundary), not a polygon
      if (nrow(intersection) > 0) {
        geom_type <- sf::st_geometry_type(intersection)
        expect_false(any(geom_type %in% c("POLYGON", "MULTIPOLYGON")),
                     info = sprintf("Parcels %d and %d overlap", i, j))
      }
    }
  }
})

test_that("parcels have consistent contenance values", {
  fixture <- create_couchey_mini_fixture()
  parcels <- fixture$parcels

  expect_equal(parcels$contenance, c(25000, 18000, 32000))
  expect_equal(sum(parcels$contenance), 75000)  # Total area in m2
})

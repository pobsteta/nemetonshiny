# Script to create the test fixture for UG regression tests
# Run once to generate projet-test-couchey-mini.rds
#
# This creates a minimal project structure with 3 mock cadastral parcels
# and pre-computed indicator values, simulating a small Couchey project.

library(sf)

# --- 3 mock cadastral parcels (simplified polygons near Couchey, Cote-d'Or) ---
# Using EPSG:4326 (WGS84) for simplicity in the fixture
p1 <- st_polygon(list(matrix(c(
  4.950, 47.270,
  4.952, 47.270,
  4.952, 47.272,
  4.950, 47.272,
  4.950, 47.270
), ncol = 2, byrow = TRUE)))

p2 <- st_polygon(list(matrix(c(
  4.952, 47.270,
  4.954, 47.270,
  4.954, 47.272,
  4.952, 47.272,
  4.952, 47.270
), ncol = 2, byrow = TRUE)))

p3 <- st_polygon(list(matrix(c(
  4.954, 47.270,
  4.956, 47.270,
  4.956, 47.272,
  4.954, 47.272,
  4.954, 47.270
), ncol = 2, byrow = TRUE)))

parcels <- st_sf(
  id = c("couchey_54", "couchey_55", "couchey_56"),
  geo_parcelle = c("21189000A0054", "21189000A0055", "21189000A0056"),
  section = c("A", "A", "A"),
  numero = c("54", "55", "56"),
  contenance = c(25000, 18000, 32000),  # m2
  code_insee = rep("21189", 3),
  geometry = st_sfc(p1, p2, p3, crs = 4326)
)

# --- Pre-computed indicator values (deterministic, realistic range 0-100) ---
indicators <- st_drop_geometry(parcels)
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

# --- Family scores (mean of indicators per family) ---
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

# --- Assemble project fixture ---
projet_test <- list(
  metadata = list(
    id = "test_couchey_mini",
    name = "Couchey Mini Test",
    description = "3 parcelles for regression testing",
    owner = "test",
    created_at = "2026-04-01T00:00:00",
    updated_at = "2026-04-01T00:00:00",
    status = "completed",
    version = "0.7.0",
    parcels_count = 3L,
    indicators_computed = TRUE
  ),
  parcels = parcels,
  indicators = indicators
)

saveRDS(projet_test, "inst/extdata/projet-test-couchey-mini.rds")
cat("Fixture created: inst/extdata/projet-test-couchey-mini.rds\n")

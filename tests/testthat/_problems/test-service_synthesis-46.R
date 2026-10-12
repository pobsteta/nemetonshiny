# Extracted from test-service_synthesis.R:46

# prequel ----------------------------------------------------------------------
.synth_project <- function(ndp_level = 0L) {
  ind <- sf::st_sf(
    ug_id = 1:3,
    indicateur_b1_protection = c(40, 60, 80),
    indicateur_c1_biomasse = c(70, 90, NA),
    indicateur_w1_reseau = c(10, 20, 30),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2)),
      crs = 2154
    )
  )
  list(
    id = "p-test",
    metadata = list(name = "Foret test", status = "completed",
                    ndp_level = ndp_level, updated_at = "2026-10-04T10:00:00"),
    parcels = data.frame(id = 1:5),
    indicators_sf = ind
  )
}
.identity_enrichers <- function(env = parent.frame()) {
  local_mocked_bindings(
    add_r5_to_indicators = function(base_sf, project) base_sf,
    add_regen_r_indicators = function(base_sf, project) base_sf,
    .env = env
  )
}

# test -------------------------------------------------------------------------
.identity_enrichers()
p <- .synth_project()
got <- suppressMessages(project_family_scores(p))
want <- suppressMessages(
    create_family_index(p$indicators_sf, method = "mean", na.rm = TRUE)
  )
expect_s3_class(got, "sf")
expect_identical(sf::st_drop_geometry(got), sf::st_drop_geometry(want))

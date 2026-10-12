# Extracted from test-mod_family.R:450

# test -------------------------------------------------------------------------
skip_if_not_installed("shiny")
skip_if_not_installed("sf")
geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(1,0,2,0,2,1,1,1,1,0), ncol = 2, byrow = TRUE))),
    crs = 2154
  )
ind_sf <- sf::st_sf(
    ug_id = c("p1", "p2"),
    indicateur_r1_feu_norm = c(0.4, 0.6),
    geometry = geom
  )
mock_project <- list(
    indicators = sf::st_drop_geometry(ind_sf),
    indicators_sf = ind_sf
  )
app_state <- shiny::reactiveValues(current_project = mock_project,
                                     language = "fr", family_comments = list())
testthat::local_mocked_bindings(
    add_r5_to_indicators = function(base_sf, project) {
      base_sf$indicateur_r5_deperissement <- c(20, 80)
      base_sf
    }
  )
shiny::testServer(
    nemetonshiny:::mod_family_server,
    args = list(family_code = "R", app_state = app_state),
    {
      data <- indicators_data()
      expect_false(is.null(data))
      expect_true("indicateur_r5_deperissement" %in% names(data))

      sf_data <- indicators_sf()
      expect_true("indicateur_r5_deperissement" %in% names(sf_data))
    }
  )

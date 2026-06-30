# test-service_r5.R — live R5 dieback column for the synthesis radar
#   * graceful no-op when no bound zone / no alert
#   * R5 column injected + routing by alert intersection
#   * temp routing columns dropped, R5 renamed to the canonical long name

make_ugfs <- function() {
  mk <- function(x0, y0) sf::st_polygon(list(rbind(
    c(x0, y0), c(x0 + 100, y0), c(x0 + 100, y0 + 100),
    c(x0, y0 + 100), c(x0, y0))))
  sf::st_sf(
    ug_id = c("u1", "u2"),
    indicateur_r1_feu = c(70, 70),
    geometry = sf::st_sfc(mk(700000, 6800000), mk(701000, 6800000),
                          crs = 2154))
}

# One RECONFORT alert inside UGF u1, none in u2.
make_alerts <- function() {
  sf::st_sf(
    alert_type       = "reconfort_dieback",
    confidence_class = "3-tres-deperissant",
    area_m2          = 500,
    geometry         = sf::st_sfc(sf::st_point(c(700050, 6800050)), crs = 2154))
}

test_that("no monitoring_zone_id leaves base_sf untouched (no DB)", {
  skip_if_not_installed("sf")
  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) stop("should not connect"))
  out <- add_r5_to_indicators(make_ugfs(), list(metadata = list()))
  expect_false("indicateur_r5_deperissement" %in% names(out))
  expect_identical(nrow(out), 2L)
})

test_that("no alerts leaves base_sf untouched", {
  skip_if_not_installed("sf")
  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) structure(list(), class = "DBIConnection"),
    close_monitoring_db_connection = function(con) invisible(NULL))
  testthat::local_mocked_bindings(
    list_alerts = function(...) make_alerts()[0, ], .package = "nemeton")
  out <- add_r5_to_indicators(make_ugfs(), list(metadata = list(monitoring_zone_id = 5L)))
  expect_false("indicateur_r5_deperissement" %in% names(out))
})

test_that("alerts present inject R5 and route by intersection", {
  skip_if_not_installed("sf")
  captured <- NULL
  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) structure(list(), class = "DBIConnection"),
    close_monitoring_db_connection = function(con) invisible(NULL))
  testthat::local_mocked_bindings(
    list_alerts = function(con, zone_id, ...) make_alerts(),
    indicateur_r5_deperissement = function(units, ...) {
      captured <<- units
      units$R5 <- ifelse(units$.r5_feuillus > 0 | units$.r5_resineux > 0,
                         50, NA_real_)
      units
    },
    .package = "nemeton")

  out <- add_r5_to_indicators(make_ugfs(),
                              list(metadata = list(monitoring_zone_id = 5L)))

  # canonical long-name column added, temp/diagnostic columns gone
  expect_true("indicateur_r5_deperissement" %in% names(out))
  expect_false(any(c("R5", ".r5_feuillus", ".r5_resineux", "r5_status") %in%
                     names(out)))
  # routing: u1 intersects the reconfort alert (feuillus), u2 doesn't
  expect_equal(captured$.r5_feuillus, c(1, 0))
  expect_equal(captured$.r5_resineux, c(0, 0))
  # R5 scored for u1, NA for u2 (no method routed)
  expect_equal(out$indicateur_r5_deperissement, c(50, NA))
})

# Tests for .compute_zone_surfaces() — FAST zone surfaces banner
# (v0.77.0). Surfaces (ha) + part (%) of the 4 project strata
# `_tot/_feu/_res/_mix`. `get_monitoring_zone_aoi()` is mocked so the
# helper is exercised without a live monitoring DB.

# Build a square sf polygon (EPSG:2154) of the given side length (m),
# i.e. surface = side^2 m^2. side=100 → 1 ha, side=200 → 4 ha.
.square_aoi <- function(side) {
  m <- matrix(c(0, 0, side, 0, side, side, 0, side, 0, 0),
              ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(m))
  sf::st_sf(geometry = sf::st_sfc(poly, crs = 2154))
}

test_that(".compute_zone_surfaces returns NULL on empty / NULL input", {
  expect_null(.compute_zone_surfaces(NULL, NULL))
  expect_null(.compute_zone_surfaces(
    "con", data.frame(id = integer(0), name = character(0))))
})

test_that(".compute_zone_surfaces computes ha + pct relative to _tot", {
  zones_df <- data.frame(
    id   = c(1L, 2L, 3L, 4L),
    name = c("villards_tot", "villards_feu", "villards_res", "villards_mix"),
    stringsAsFactors = FALSE
  )
  # tot = 4 ha (200 m square), feu = 1 ha, res = 2 ha, mix = 1 ha.
  areas <- list(`1` = 200, `2` = 100, `3` = sqrt(2e4), `4` = 100)

  testthat::local_mocked_bindings(
    get_monitoring_zone_aoi = function(con, zone_id) {
      .square_aoi(areas[[as.character(zone_id)]])
    },
    .package = "nemetonshiny"
  )

  df <- .compute_zone_surfaces("con", zones_df)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 4L)
  # Ordered tot → feu → res → mix.
  expect_equal(df$strata, c("tot", "feu", "res", "mix"))
  expect_equal(df$label_key,
               c("zone_tot", "zone_feu", "zone_res", "zone_mix"))
  # Surfaces.
  expect_equal(round(df$ha, 1), c(4, 1, 2, 1))
  # tot has no pct; others relative to 4 ha.
  expect_true(is.na(df$pct[df$strata == "tot"]))
  expect_equal(round(df$pct[df$strata == "feu"]), 25)
  expect_equal(round(df$pct[df$strata == "res"]), 50)
  expect_equal(round(df$pct[df$strata == "mix"]), 25)
})

test_that(".compute_zone_surfaces skips strata whose AOI is unresolved", {
  zones_df <- data.frame(
    id   = c(1L, 2L),
    name = c("p_tot", "p_feu"),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    get_monitoring_zone_aoi = function(con, zone_id) {
      if (identical(as.integer(zone_id), 2L)) return(NULL)  # feu unresolved
      .square_aoi(100)                                       # tot = 1 ha
    },
    .package = "nemetonshiny"
  )
  df <- .compute_zone_surfaces("con", zones_df)
  expect_equal(nrow(df), 1L)
  expect_equal(df$strata, "tot")
  expect_equal(round(df$ha, 1), 1)
})

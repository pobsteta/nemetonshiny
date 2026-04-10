# Tests for atome_split.R
# Parcel subdivision via imported geometries

# ==============================================================================
# Helper: create a project with a square parcel
# ==============================================================================

create_split_test_projet <- function() {
  # One 2x2 square parcel
  p1 <- sf::st_polygon(list(matrix(c(
    0, 0, 2, 0, 2, 2, 0, 2, 0, 0
  ), ncol = 2, byrow = TRUE)))

  parcels <- sf::st_sf(
    id = "p1",
    geo_parcelle = "REF001",
    section = "A",
    numero = "1",
    contenance = 40000,  # m2 (approximate)
    geometry = sf::st_sfc(p1, crs = 4326)
  )

  projet <- list(parcels = parcels)
  nemetonShiny:::ug_init_default(projet)
}


# ==============================================================================
# atome_split_by_import tests
# ==============================================================================

test_that("split creates correct number of atoms", {
  projet <- create_split_test_projet()

  # Split with a polygon covering the left half
  left_half <- sf::st_sf(
    id = "left",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,0, 1,2, 0,2, 0,0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  result <- nemetonShiny:::atome_split_by_import(projet, "p1", left_half)

  # Should have 2 atoms: left half + remainder (right half)
  p1_atoms <- result$atomes[result$atomes$parent_parcelle_id == "p1", ]
  expect_equal(nrow(p1_atoms), 2)

  # All atoms should trace back to p1
  expect_true(all(p1_atoms$parent_parcelle_id == "p1"))

  # Validation should pass
  expect_true(nemetonShiny:::projet_validate(result))
})

test_that("split with two polygons creates 3 atoms (2 + remainder)", {
  projet <- create_split_test_projet()

  # Two polygons: bottom-left and top-left quarters
  quarters <- sf::st_sf(
    id = c("bl", "tl"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(0,1, 1,1, 1,2, 0,2, 0,1), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  result <- nemetonShiny:::atome_split_by_import(projet, "p1", quarters)

  p1_atoms <- result$atomes[result$atomes$parent_parcelle_id == "p1", ]
  # 2 quarters + right half remainder = 3 atoms
  expect_equal(nrow(p1_atoms), 3)
  expect_true(nemetonShiny:::projet_validate(result))
})

test_that("split with full coverage creates no remainder", {
  projet <- create_split_test_projet()

  # Two polygons that exactly tile the parcel
  halves <- sf::st_sf(
    id = c("left", "right"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,0, 1,2, 0,2, 0,0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1,0, 2,0, 2,2, 1,2, 1,0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  result <- nemetonShiny:::atome_split_by_import(projet, "p1", halves)

  p1_atoms <- result$atomes[result$atomes$parent_parcelle_id == "p1", ]
  # Exactly 2 atoms, no remainder
  expect_equal(nrow(p1_atoms), 2)
  expect_true(nemetonShiny:::projet_validate(result))
})

test_that("split preserves UG assignment", {
  projet <- create_split_test_projet()

  # Set a groupe before splitting
  uid <- projet$ugs$ug_id[1]
  projet <- nemetonShiny:::ug_set_groupe(projet, uid, "TSF")

  left <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,0, 1,2, 0,2, 0,0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  result <- nemetonShiny:::atome_split_by_import(projet, "p1", left)

  # All new atoms should be assigned to the same UG
  p1_atoms <- result$atomes[result$atomes$parent_parcelle_id == "p1", ]
  expect_true(all(p1_atoms$ug_id == uid))
})

test_that("split rejects non-overlapping polygons", {
  projet <- create_split_test_projet()

  # Polygon completely outside the parcel
  outside <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(10,10, 11,10, 11,11, 10,11, 10,10), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  expect_error(
    nemetonShiny:::atome_split_by_import(projet, "p1", outside),
    "No valid polygon"
  )
})

test_that("split rejects unknown parcel ID", {
  projet <- create_split_test_projet()

  poly <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  expect_error(
    nemetonShiny:::atome_split_by_import(projet, "nonexistent", poly),
    "not found"
  )
})


# ==============================================================================
# atome_undo_split tests
# ==============================================================================

test_that("undo_split restores single atom", {
  projet <- create_split_test_projet()

  # Split first
  left <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,0, 1,2, 0,2, 0,0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )
  projet <- nemetonShiny:::atome_split_by_import(projet, "p1", left)
  expect_equal(nrow(projet$atomes[projet$atomes$parent_parcelle_id == "p1", ]), 2)

  # Undo
  result <- nemetonShiny:::atome_undo_split(projet, "p1")
  expect_equal(nrow(result$atomes[result$atomes$parent_parcelle_id == "p1", ]), 1)
  expect_true(nemetonShiny:::projet_validate(result))
})

test_that("undo_split on unsplit parcel is a no-op", {
  projet <- create_split_test_projet()

  result <- nemetonShiny:::atome_undo_split(projet, "p1")
  expect_equal(nrow(result$atomes), nrow(projet$atomes))
})


# ==============================================================================
# validate_tiling tests
# ==============================================================================

test_that("validate_tiling passes for valid tiling", {
  projet <- create_split_test_projet()

  left <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,0, 1,2, 0,2, 0,0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )
  projet <- nemetonShiny:::atome_split_by_import(projet, "p1", left)

  expect_true(nemetonShiny:::validate_tiling(projet, "p1"))
})

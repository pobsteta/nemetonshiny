# Tests for tenement_split.R
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
  nemetonshiny:::ug_init_default(projet)
}


# ==============================================================================
# tenement_split_by_import tests
# ==============================================================================

test_that("split creates correct number of tenements", {
  projet <- create_split_test_projet()

  # Split with a polygon covering the left half
  left_half <- sf::st_sf(
    id = "left",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,0, 1,2, 0,2, 0,0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  result <- nemetonshiny:::tenement_split_by_import(projet, "p1", left_half)

  # Should have 2 tenements: left half + remainder (right half)
  p1_tenements <- result$tenements[result$tenements$parent_parcelle_id == "p1", ]
  expect_equal(nrow(p1_tenements), 2)

  # All tenements should trace back to p1
  expect_true(all(p1_tenements$parent_parcelle_id == "p1"))

  # Validation should pass
  expect_true(nemetonshiny:::projet_validate(result))
})

test_that("split with two polygons creates 3 tenements (2 + remainder)", {
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

  result <- nemetonshiny:::tenement_split_by_import(projet, "p1", quarters)

  p1_tenements <- result$tenements[result$tenements$parent_parcelle_id == "p1", ]
  # 2 quarters + right half remainder = 3 tenements
  expect_equal(nrow(p1_tenements), 3)
  expect_true(nemetonshiny:::projet_validate(result))
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

  result <- nemetonshiny:::tenement_split_by_import(projet, "p1", halves)

  p1_tenements <- result$tenements[result$tenements$parent_parcelle_id == "p1", ]
  # Exactly 2 tenements, no remainder
  expect_equal(nrow(p1_tenements), 2)
  expect_true(nemetonshiny:::projet_validate(result))
})

test_that("split preserves UG assignment", {
  projet <- create_split_test_projet()

  # Set a groupe before splitting
  uid <- projet$ugs$ug_id[1]
  projet <- nemetonshiny:::ug_set_groupe(projet, uid, "TSF")

  left <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,0, 1,2, 0,2, 0,0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  result <- nemetonshiny:::tenement_split_by_import(projet, "p1", left)

  # All new tenements should be assigned to the same UG
  p1_tenements <- result$tenements[result$tenements$parent_parcelle_id == "p1", ]
  expect_true(all(p1_tenements$ug_id == uid))
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
    nemetonshiny:::tenement_split_by_import(projet, "p1", outside),
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
    nemetonshiny:::tenement_split_by_import(projet, "nonexistent", poly),
    "not found"
  )
})


# ==============================================================================
# tenement_undo_split tests
# ==============================================================================

test_that("undo_split restores single tenement", {
  projet <- create_split_test_projet()

  # Split first
  left <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0,0, 1,0, 1,2, 0,2, 0,0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )
  projet <- nemetonshiny:::tenement_split_by_import(projet, "p1", left)
  expect_equal(nrow(projet$tenements[projet$tenements$parent_parcelle_id == "p1", ]), 2)

  # Undo
  result <- nemetonshiny:::tenement_undo_split(projet, "p1")
  expect_equal(nrow(result$tenements[result$tenements$parent_parcelle_id == "p1", ]), 1)
  expect_true(nemetonshiny:::projet_validate(result))
})

test_that("undo_split on unsplit parcel is a no-op", {
  projet <- create_split_test_projet()

  result <- nemetonshiny:::tenement_undo_split(projet, "p1")
  expect_equal(nrow(result$tenements), nrow(projet$tenements))
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
  projet <- nemetonshiny:::tenement_split_by_import(projet, "p1", left)

  expect_true(nemetonshiny:::validate_tiling(projet, "p1"))
})


# ==============================================================================
# tenement_import_replace tests — label_ugf handling
# ==============================================================================

# Helper: two contiguous halves covering the parent parcel
make_two_halves <- function(labels = NULL) {
  left <- sf::st_polygon(list(matrix(
    c(0,0, 1,0, 1,2, 0,2, 0,0), ncol = 2, byrow = TRUE
  )))
  right <- sf::st_polygon(list(matrix(
    c(1,0, 2,0, 2,2, 1,2, 1,0), ncol = 2, byrow = TRUE
  )))
  sf <- sf::st_sf(
    geometry = sf::st_sfc(left, right, crs = 4326)
  )
  if (!is.null(labels)) sf$label_ugf <- labels
  sf
}


test_that("tenement_import_replace without label_ugf falls back to overlap-inheritance", {
  projet <- create_split_test_projet()
  # Two halves, no label_ugf column
  imported <- make_two_halves()

  result <- nemetonshiny:::tenement_import_replace(projet, imported)

  # Two tenements, both sharing the single pre-existing UG
  expect_equal(nrow(result$tenements), 2)
  expect_equal(length(unique(result$tenements$ug_id)), 1)
  expect_equal(nrow(result$ugs), 1)
  expect_true(nemetonshiny:::projet_validate(result))
})


test_that("tenement_import_replace creates UGF from distinct label_ugf values", {
  projet <- create_split_test_projet()
  imported <- make_two_halves(labels = c("UGF-Ouest", "UGF-Est"))

  result <- nemetonshiny:::tenement_import_replace(projet, imported)

  expect_equal(nrow(result$tenements), 2)
  expect_equal(sort(result$ugs$label), c("UGF-Est", "UGF-Ouest"))
  # Each tenement maps to its own UGF
  expect_equal(length(unique(result$tenements$ug_id)), 2)
  expect_true(nemetonshiny:::projet_validate(result))
})


test_that("tenement_import_replace groups tenements sharing the same label_ugf", {
  projet <- create_split_test_projet()
  imported <- make_two_halves(labels = c("UGF-Unique", "UGF-Unique"))

  result <- nemetonshiny:::tenement_import_replace(projet, imported)

  expect_equal(nrow(result$tenements), 2)
  expect_equal(length(unique(result$tenements$ug_id)), 1)
  expect_equal(result$ugs$label, "UGF-Unique")
  expect_true(nemetonshiny:::projet_validate(result))
})


test_that("tenement_import_replace reuses existing UGF when label matches", {
  projet <- create_split_test_projet()

  # Assign a groupe to the existing UG so we can tell if it's reused
  original_ug_id <- projet$ugs$ug_id[1]
  original_label <- projet$ugs$label[1]
  projet$ugs$groupe[projet$ugs$ug_id == original_ug_id] <- "TSF"

  # Re-import with one half using the existing UG label and another new label
  imported <- make_two_halves(labels = c(original_label, "UGF-Nouveau"))

  result <- nemetonshiny:::tenement_import_replace(projet, imported)

  # Existing UG preserved (same id + groupe) and a new one appended
  expect_true(original_ug_id %in% result$ugs$ug_id)
  expect_equal(
    result$ugs$groupe[result$ugs$ug_id == original_ug_id], "TSF"
  )
  expect_true("UGF-Nouveau" %in% result$ugs$label)
  expect_true(nemetonshiny:::projet_validate(result))
})


test_that("tenement_import_replace treats empty/NA label_ugf as fallback", {
  projet <- create_split_test_projet()

  # First half has a label, second half has a blank one
  imported <- make_two_halves(labels = c("UGF-Unique", "  "))

  result <- nemetonshiny:::tenement_import_replace(projet, imported)

  # UGF-Unique created; blank-label row falls back to existing UG via overlap
  expect_true("UGF-Unique" %in% result$ugs$label)
  expect_true(nrow(result$ugs) >= 1)
  expect_true(nemetonshiny:::projet_validate(result))
})

# ---- Unicite des tenement_id (regression) ----------------------------------

test_that("deux decoupes dans la meme seconde ne collisionnent pas", {
  skip_if_not_installed("sf")
  # Régression : les ids étaient `tnm_<horodatage a la seconde>_<index>`, donc
  # deux parcelles découpées dans la même seconde produisaient les MÊMES ids
  # des deux côtés. `projet_validate()` ne contrôle pas l'unicité, la
  # corruption passait donc en silence — et toute opération désignant un
  # tènement par son id (affectation à une UGF, sélection carte, undo) devenait
  # ambiguë. Mesuré avant correctif : 4 tènements, 2 ids distincts.
  prev <- sf::sf_use_s2(); sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(prev), add = TRUE)

  carre <- function(i) sf::st_polygon(list(rbind(
    c(i * 100, 0), c(i * 100 + 100, 0), c(i * 100 + 100, 100),
    c(i * 100, 100), c(i * 100, 0))))
  parcels <- sf::st_sf(
    id = c("P1", "P2"), contenance = c(10000, 10000),
    geometry = sf::st_sfc(carre(0), carre(1), crs = 2154))
  projet <- nemetonshiny:::ug_init_default(list(parcels = parcels))

  moities <- function(i) sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(i * 100, 0), c(i * 100 + 50, 0),
                              c(i * 100 + 50, 100), c(i * 100, 100), c(i * 100, 0)))),
    sf::st_polygon(list(rbind(c(i * 100 + 50, 0), c(i * 100 + 100, 0),
                              c(i * 100 + 100, 100), c(i * 100 + 50, 100),
                              c(i * 100 + 50, 0)))),
    crs = 2154))

  projet <- nemetonshiny:::tenement_split_by_import(projet, "P1", moities(0))
  projet <- nemetonshiny:::tenement_split_by_import(projet, "P2", moities(1))

  ids <- projet$tenements$tenement_id
  expect_equal(length(ids), 4L)
  expect_equal(length(unique(ids)), 4L)
})

test_that(".tenement_id_slug rend un fragment sur d'identifiant", {
  # Les identifiants cadastraux réels portent des caractères que l'on ne veut
  # pas voir apparaître tels quels dans un id de tènement.
  expect_equal(nemetonshiny:::.tenement_id_slug("250010000A0001"), "250010000A0001")
  expect_equal(nemetonshiny:::.tenement_id_slug("A-12 / B"), "A_12_B")
  # Un id vide ou uniquement séparateurs ne doit pas produire un id vide.
  expect_true(nzchar(nemetonshiny:::.tenement_id_slug("///")))
  expect_true(nzchar(nemetonshiny:::.tenement_id_slug("")))
})

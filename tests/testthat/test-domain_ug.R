# Tests for domain_ug.R
# Domain layer: Unités de Gestion (UG) constructors, commands, queries, validation

# ==============================================================================
# Helper: create a minimal project with 3 parcels
# ==============================================================================

create_test_projet <- function() {
  p1 <- sf::st_polygon(list(matrix(c(
    0, 0, 1, 0, 1, 1, 0, 1, 0, 0
  ), ncol = 2, byrow = TRUE)))
  p2 <- sf::st_polygon(list(matrix(c(
    1, 0, 2, 0, 2, 1, 1, 1, 1, 0
  ), ncol = 2, byrow = TRUE)))
  p3 <- sf::st_polygon(list(matrix(c(
    2, 0, 3, 0, 3, 1, 2, 1, 2, 0
  ), ncol = 2, byrow = TRUE)))

  parcels <- sf::st_sf(
    id = c("p1", "p2", "p3"),
    geo_parcelle = c("REF001", "REF002", "REF003"),
    section = c("A", "A", "B"),
    numero = c("1", "2", "3"),
    contenance = c(10000, 10000, 10000),
    geometry = sf::st_sfc(p1, p2, p3, crs = 4326)
  )

  list(parcels = parcels, metadata = list(id = "test_project"))
}


# ==============================================================================
# Constructor tests
# ==============================================================================

test_that("new_atome creates valid atom", {
  geom <- sf::st_sfc(sf::st_polygon(list(matrix(c(
    0, 0, 1, 0, 1, 1, 0, 1, 0, 0
  ), ncol = 2, byrow = TRUE))), crs = 4326)

  atome <- nemetonShiny:::new_atome("a1", "p1", geom, 10000)

  expect_true(inherits(atome, "sf"))
  expect_equal(nrow(atome), 1)
  expect_equal(atome$atome_id, "a1")
  expect_equal(atome$parent_parcelle_id, "p1")
  expect_true(is.na(atome$ug_id))
  expect_equal(atome$surface_m2, 10000)
})

test_that("new_atome rejects invalid input", {
  geom <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)

  expect_error(nemetonShiny:::new_atome("", "p1", geom, 100), "id is required")
  expect_error(nemetonShiny:::new_atome("a1", "", geom, 100), "parent_parcelle_id")
  expect_error(nemetonShiny:::new_atome("a1", "p1", geom, -1), "positive number")
})

test_that("new_ug creates valid UG", {
  ug <- nemetonShiny:::new_ug("ug1", "TSF-Sud", "TSF")

  expect_true(is.data.frame(ug))
  expect_equal(nrow(ug), 1)
  expect_equal(ug$ug_id, "ug1")
  expect_equal(ug$label, "TSF-Sud")
  expect_equal(ug$groupe, "TSF")
})

test_that("new_ug rejects empty label", {
  expect_error(nemetonShiny:::new_ug("ug1", ""), "label is required")
  expect_error(nemetonShiny:::new_ug("ug1", "  "), "label is required")
})


# ==============================================================================
# ug_init_default tests
# ==============================================================================

test_that("ug_init_default creates 1 atome and 1 UG per parcel", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  expect_equal(nrow(projet$atomes), 3)
  expect_equal(nrow(projet$ugs), 3)

  # Each atom has a unique UG
  expect_equal(length(unique(projet$atomes$ug_id)), 3)

  # Each atom traces to a parcel
  expect_equal(sort(projet$atomes$parent_parcelle_id), c("p1", "p2", "p3"))
})

test_that("ug_init_default uses geo_parcelle as label", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  expect_equal(sort(projet$ugs$label), c("REF001", "REF002", "REF003"))
})

test_that("ug_init_default passes validation", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  expect_true(nemetonShiny:::projet_validate(projet))
})

test_that("ug_init_default rejects empty parcels", {
  expect_error(
    nemetonShiny:::ug_init_default(list(parcels = NULL)),
    "non-empty parcels"
  )
})


# ==============================================================================
# ug_merge tests
# ==============================================================================

test_that("ug_merge combines two UGs into one", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  # Get first two UG ids
  ug_ids_to_merge <- projet$ugs$ug_id[1:2]

  projet <- nemetonShiny:::ug_merge(projet, ug_ids_to_merge, "Merged-UG")

  # Should now have 2 UGs (3 - 2 merged + 1 new)
  expect_equal(nrow(projet$ugs), 2)

  # The merged UG should have 2 atoms
  merged_ug <- projet$ugs[projet$ugs$label == "Merged-UG", ]
  expect_equal(nrow(merged_ug), 1)

  merged_atom_count <- sum(projet$atomes$ug_id == merged_ug$ug_id)
  expect_equal(merged_atom_count, 2)

  # Still 3 atoms total
  expect_equal(nrow(projet$atomes), 3)

  # Passes validation
  expect_true(nemetonShiny:::projet_validate(projet))
})

test_that("ug_merge rejects fewer than 2 UGs", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  expect_error(
    nemetonShiny:::ug_merge(projet, projet$ugs$ug_id[1], "Single"),
    "At least two"
  )
})

test_that("ug_merge preserves groupe when uniform", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  # Set same groupe on first two UGs
  projet <- nemetonShiny:::ug_set_groupe(projet, projet$ugs$ug_id[1], "TSF")
  projet <- nemetonShiny:::ug_set_groupe(projet, projet$ugs$ug_id[2], "TSF")

  merged <- nemetonShiny:::ug_merge(
    projet, projet$ugs$ug_id[1:2], "TSF-Merged"
  )

  tsf_ug <- merged$ugs[merged$ugs$label == "TSF-Merged", ]
  expect_equal(tsf_ug$groupe, "TSF")
})


# ==============================================================================
# ug_create tests
# ==============================================================================

test_that("ug_create makes new UG from selected atoms", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  # Get atom IDs for first two parcels
  atome_ids <- projet$atomes$atome_id[1:2]

  projet <- nemetonShiny:::ug_create(projet, atome_ids, "New-UG", "REGT")

  # The new UG should exist
  new_ug <- projet$ugs[projet$ugs$label == "New-UG", ]
  expect_equal(nrow(new_ug), 1)
  expect_equal(new_ug$groupe, "REGT")

  # Its atoms should be reassigned
  expect_equal(sum(projet$atomes$ug_id == new_ug$ug_id), 2)

  # Old empty UGs removed, so total = 2 (new + remaining)
  expect_equal(nrow(projet$ugs), 2)

  expect_true(nemetonShiny:::projet_validate(projet))
})

test_that("ug_create rejects empty atom list", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  expect_error(
    nemetonShiny:::ug_create(projet, character(0), "Empty"),
    "At least one"
  )
})

test_that("ug_create rejects unknown atom IDs", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  expect_error(
    nemetonShiny:::ug_create(projet, "nonexistent", "Bad"),
    "Unknown atom"
  )
})


# ==============================================================================
# ug_split tests
# ==============================================================================

test_that("ug_split with default creates 1 UG per atom", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  # First merge 2 UGs
  ug_ids <- projet$ugs$ug_id[1:2]
  projet <- nemetonShiny:::ug_merge(projet, ug_ids, "ToSplit")

  merged_ug <- projet$ugs[projet$ugs$label == "ToSplit", ]
  projet <- nemetonShiny:::ug_split(projet, merged_ug$ug_id)

  # Should now have 3 UGs again (2 from split + 1 unchanged)
  expect_equal(nrow(projet$ugs), 3)
  expect_equal(nrow(projet$atomes), 3)
  expect_true(nemetonShiny:::projet_validate(projet))
})

test_that("ug_split rejects single-atom UG", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  expect_error(
    nemetonShiny:::ug_split(projet, projet$ugs$ug_id[1]),
    "fewer than 2"
  )
})


# ==============================================================================
# ug_assign_atom tests
# ==============================================================================

test_that("ug_assign_atom moves atom and removes empty UG", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  # Move atom 1 to UG of atom 2
  atome_to_move <- projet$atomes$atome_id[1]
  target_ug <- projet$atomes$ug_id[2]

  projet <- nemetonShiny:::ug_assign_atom(projet, atome_to_move, target_ug)

  # UG count should decrease by 1 (source UG became empty)
  expect_equal(nrow(projet$ugs), 2)

  # Target UG should now have 2 atoms
  expect_equal(sum(projet$atomes$ug_id == target_ug), 2)

  expect_true(nemetonShiny:::projet_validate(projet))
})


# ==============================================================================
# ug_set_groupe tests
# ==============================================================================

test_that("ug_set_groupe updates groupe", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  uid <- projet$ugs$ug_id[1]
  projet <- nemetonShiny:::ug_set_groupe(projet, uid, "TSF")

  expect_equal(projet$ugs$groupe[projet$ugs$ug_id == uid], "TSF")
})

test_that("ug_set_groupe warns on unknown groupe", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  uid <- projet$ugs$ug_id[1]
  expect_warning(
    nemetonShiny:::ug_set_groupe(projet, uid, "UNKNOWN_GROUP"),
    "Unknown management group"
  )
})


# ==============================================================================
# Query tests
# ==============================================================================

test_that("ug_geometry returns valid union geometry", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  geom <- nemetonShiny:::ug_geometry(projet, projet$ugs$ug_id[1])

  expect_true(inherits(geom, "sfc"))
  expect_true(sf::st_is_valid(geom))
})

test_that("ug_surface returns correct value", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  surf <- nemetonShiny:::ug_surface(projet, projet$ugs$ug_id[1])
  expect_equal(surf, 10000)
})

test_that("ug_cadastral_refs returns parcel references", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  refs <- nemetonShiny:::ug_cadastral_refs(projet, projet$ugs$ug_id[1])

  expect_true(is.data.frame(refs))
  expect_equal(nrow(refs), 1)
  expect_true("geo_parcelle" %in% names(refs))
  expect_true("section" %in% names(refs))
  expect_true("numero" %in% names(refs))
  expect_true("surface_m2" %in% names(refs))
})

test_that("ug_cadastral_refs returns multiple refs after merge", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  # Merge first two UGs
  projet <- nemetonShiny:::ug_merge(
    projet, projet$ugs$ug_id[1:2], "Merged"
  )

  merged_ug <- projet$ugs[projet$ugs$label == "Merged", ]
  refs <- nemetonShiny:::ug_cadastral_refs(projet, merged_ug$ug_id)

  expect_equal(nrow(refs), 2)
})

test_that("ug_list returns all UGs with stats", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  listing <- nemetonShiny:::ug_list(projet)

  expect_true(is.data.frame(listing))
  expect_equal(nrow(listing), 3)
  expect_true(all(c("ug_id", "label", "groupe", "n_atomes", "surface_m2") %in% names(listing)))
  expect_true(all(listing$n_atomes == 1))
  expect_true(all(listing$surface_m2 == 10000))
})

test_that("ug_list filters by groupe", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  projet <- nemetonShiny:::ug_set_groupe(projet, projet$ugs$ug_id[1], "TSF")

  filtered <- nemetonShiny:::ug_list(projet, filter_groupe = "TSF")
  expect_equal(nrow(filtered), 1)

  filtered_none <- nemetonShiny:::ug_list(projet, filter_groupe = "HSN")
  expect_equal(nrow(filtered_none), 0)
})


# ==============================================================================
# ug_build_sf tests
# ==============================================================================

test_that("ug_build_sf creates valid sf with one row per UG", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  ug_sf <- nemetonShiny:::ug_build_sf(projet)

  expect_true(inherits(ug_sf, "sf"))
  expect_equal(nrow(ug_sf), 3)
  expect_true(all(c("ug_id", "label", "groupe", "surface_m2", "n_atomes", "cadastral_refs") %in% names(ug_sf)))
  expect_true(all(sf::st_is_valid(ug_sf)))
})

test_that("ug_build_sf after merge has fewer rows", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)
  projet <- nemetonShiny:::ug_merge(projet, projet$ugs$ug_id[1:2], "Merged")

  ug_sf <- nemetonShiny:::ug_build_sf(projet)
  expect_equal(nrow(ug_sf), 2)

  merged_row <- ug_sf[ug_sf$label == "Merged", ]
  expect_equal(merged_row$n_atomes, 2L)
  expect_equal(merged_row$surface_m2, 20000)
})


# ==============================================================================
# has_ug_data tests
# ==============================================================================

test_that("has_ug_data returns FALSE for bare project", {
  projet <- create_test_projet()
  expect_false(nemetonShiny:::has_ug_data(projet))
})

test_that("has_ug_data returns TRUE after init", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)
  expect_true(nemetonShiny:::has_ug_data(projet))
})


# ==============================================================================
# Validation tests
# ==============================================================================

test_that("projet_validate catches orphan atoms (no UG)", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  # Manually break invariant
  projet$atomes$ug_id[1] <- NA_character_

  expect_error(
    nemetonShiny:::projet_validate(projet),
    "Invariant 2"
  )
})

test_that("projet_validate catches empty UG", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  # Add an empty UG manually
  projet$ugs <- rbind(projet$ugs, data.frame(
    ug_id = "ghost_ug", label = "Ghost", groupe = NA_character_,
    stringsAsFactors = FALSE
  ))

  expect_error(
    nemetonShiny:::projet_validate(projet),
    "Invariant 3"
  )
})

test_that("projet_validate catches atoms without parent parcel", {
  projet <- create_test_projet()
  projet <- nemetonShiny:::ug_init_default(projet)

  projet$atomes$parent_parcelle_id[1] <- NA_character_

  expect_error(
    nemetonShiny:::projet_validate(projet),
    "Invariant 4"
  )
})

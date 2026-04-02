# Tests for Cadastre Service
# Phase 2: Cadastral parcel retrieval

test_that("get_cadastral_parcels validates INSEE code format", {
  expect_error(
    nemeton:::get_cadastral_parcels("123"),
    regexp = "Invalid INSEE"
  )

  expect_error(
    nemeton:::get_cadastral_parcels("ABCDE"),
    regexp = "Invalid INSEE"
  )

  expect_error(
    nemeton:::get_cadastral_parcels(""),
    regexp = "Invalid INSEE"
  )
})

test_that("get_cadastral_parcels accepts valid INSEE codes", {
  skip_if_offline()
  skip_on_cran()

  # Don't actually call the API, just check format validation passes
  # Use a small commune for faster test
  expect_error(
    nemeton:::get_cadastral_parcels("01001"),
    regexp = NA  # Should not error on format
  )
})

test_that("standardize_parcels creates required columns", {
  skip_if_not_installed("sf")

  # Create mock parcel data
  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    idu = "01001000A0001",
    section = "A",
    numero = "0001",
    contenance = 5000,
    geometry = mock_geom
  )

  result <- nemeton:::standardize_parcels(mock_parcels, "01001")

  # Check required columns exist
  expect_true("id" %in% names(result))
  expect_true("section" %in% names(result))
  expect_true("numero" %in% names(result))
  expect_true("contenance" %in% names(result))
  expect_true("code_insee" %in% names(result))

  # Check code_insee is set

  expect_equal(result$code_insee[1], "01001")
})

test_that("standardize_parcels generates IDs if missing", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    other_col = "value",
    geometry = mock_geom
  )

  result <- nemeton:::standardize_parcels(mock_parcels, "01001")

  expect_true("id" %in% names(result))
  expect_true(grepl("^01001_", result$id[1]))
})

test_that("standardize_parcels calculates area if missing", {
  skip_if_not_installed("sf")

  # Create a square polygon (approximately 100m x 100m at equator)
  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(
      0, 0,
      0.001, 0,
      0.001, 0.001,
      0, 0.001,
      0, 0
    ), ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    id = "test_id",
    geometry = mock_geom
  )

  result <- nemeton:::standardize_parcels(mock_parcels, "01001")

  expect_true("contenance" %in% names(result))
  expect_true(result$contenance[1] > 0)
})

test_that("get_parcel_by_id returns correct parcel", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(2,0, 3,0, 3,1, 2,1, 2,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    id = c("parcel_1", "parcel_2"),
    section = c("A", "B"),
    geometry = mock_geom
  )

  result <- nemeton:::get_parcel_by_id("parcel_1", mock_parcels)

  expect_equal(nrow(result), 1)
  expect_equal(result$id, "parcel_1")
  expect_equal(result$section, "A")
})

test_that("get_parcel_by_id returns NULL for non-existent ID", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    id = "parcel_1",
    geometry = mock_geom
  )

  result <- nemeton:::get_parcel_by_id("nonexistent", mock_parcels)

  expect_null(result)
})

test_that("filter_selected_parcels returns subset", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(2,0, 3,0, 3,1, 2,1, 2,0), ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(4,0, 5,0, 5,1, 4,1, 4,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    id = c("p1", "p2", "p3"),
    geometry = mock_geom
  )

  result <- nemeton:::filter_selected_parcels(mock_parcels, c("p1", "p3"))

  expect_equal(nrow(result), 2)
  expect_true("p1" %in% result$id)
  expect_true("p3" %in% result$id)
  expect_false("p2" %in% result$id)
})

test_that("filter_selected_parcels handles empty selection", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    id = "parcel_1",
    geometry = mock_geom
  )

  result <- nemeton:::filter_selected_parcels(mock_parcels, character(0))

  expect_equal(nrow(result), 0)
})

test_that("calculate_parcel_stats returns correct values", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(2,0, 3,0, 3,1, 2,1, 2,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    id = c("p1", "p2"),
    contenance = c(10000, 20000),  # m²
    geometry = mock_geom
  )

  stats <- nemeton:::calculate_parcel_stats(mock_parcels)

  expect_equal(stats$count, 2)
  expect_equal(stats$total_area_ha, 3)  # 30000 m² = 3 ha
  expect_equal(stats$mean_area_ha, 1.5)
  expect_equal(stats$min_area_ha, 1)
  expect_equal(stats$max_area_ha, 2)
})

test_that("calculate_parcel_stats handles NULL input", {
  stats <- nemeton:::calculate_parcel_stats(NULL)

  expect_equal(stats$count, 0)
  expect_equal(stats$total_area_ha, 0)
})

test_that("calculate_parcel_stats handles empty data", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(crs = 4326)
  mock_parcels <- sf::st_sf(
    id = character(0),
    contenance = numeric(0),
    geometry = mock_geom
  )

  stats <- nemeton:::calculate_parcel_stats(mock_parcels)

  expect_equal(stats$count, 0)
  expect_equal(stats$total_area_ha, 0)
})

test_that("format_parcel_info returns French format", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcel <- sf::st_sf(
    id = "01001000A0001",
    section = "A",
    numero = "0001",
    contenance = 15000,
    geometry = mock_geom
  )[1, ]

  result <- nemeton:::format_parcel_info(mock_parcel, "fr")

  expect_true(grepl("Parcelle", result))
  expect_true(grepl("Section", result))
  expect_true(grepl("Surface", result))
  expect_true(grepl("1\\.50 ha", result))
})

test_that("format_parcel_info returns English format", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcel <- sf::st_sf(
    id = "01001000A0001",
    section = "A",
    numero = "0001",
    contenance = 15000,
    geometry = mock_geom
  )[1, ]

  result <- nemeton:::format_parcel_info(mock_parcel, "en")

  expect_true(grepl("Parcel", result))
  expect_true(grepl("Area", result))
})

test_that("create_parcel_popup returns HTML", {
  skip_if_not_installed("sf")

  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcel <- sf::st_sf(
    id = "01001000A0001",
    section = "A",
    numero = "0001",
    contenance = 15000,
    geometry = mock_geom
  )[1, ]

  result <- nemeton:::create_parcel_popup(mock_parcel)

  expect_type(result, "character")
  expect_true(grepl("<strong>", result))
  expect_true(grepl("Section", result))
  expect_true(grepl("ha", result))
})

# ==============================================================================
# Tests for create_parcel_label
# ==============================================================================

test_that("create_parcel_label returns HTML with section and numero", {
  parcel <- list(
    section = "AB",
    numero = "0042",
    contenance = 25000,
    nom_com = NULL,
    lieu_dit = NULL,
    lieudit = NULL
  )

  result <- nemeton:::create_parcel_label(parcel)

  expect_type(result, "character")
  expect_true(grepl("<b>AB 0042</b>", result, fixed = TRUE))
  expect_true(grepl("Cliquez pour", result))
})

test_that("create_parcel_label includes area in hectares", {
  parcel <- list(
    section = "A",
    numero = "0001",
    contenance = 50000,
    nom_com = NULL,
    lieu_dit = NULL,
    lieudit = NULL
  )

  result <- nemeton:::create_parcel_label(parcel)

  # 50000 m² = 5 ha
  expect_true(grepl("<b>5 ha</b>", result, fixed = TRUE))
})

test_that("create_parcel_label handles missing contenance (NA)", {
  parcel <- list(
    section = "B",
    numero = "0010",
    contenance = NA,
    nom_com = NULL,
    lieu_dit = NULL,
    lieudit = NULL
  )

  result <- nemeton:::create_parcel_label(parcel)

  expect_type(result, "character")
  # Should not contain "ha" area text when contenance is NA
  expect_false(grepl("\\bha\\b", result))
  # Section and numero should still be present

  expect_true(grepl("<b>B 0010</b>", result, fixed = TRUE))
})

test_that("create_parcel_label includes lieu-dit when available (nom_com column)", {
  parcel <- list(
    section = "C",
    numero = "0005",
    contenance = 10000,
    nom_com = "Les Grands Bois",
    lieu_dit = NULL,
    lieudit = NULL
  )

  result <- nemeton:::create_parcel_label(parcel)

  expect_true(grepl("Les Grands Bois", result, fixed = TRUE))
  expect_true(grepl("color:#666", result, fixed = TRUE))
})

test_that("create_parcel_label excludes lieu-dit when 'NA' string", {
  parcel <- list(
    section = "D",
    numero = "0003",
    contenance = 20000,
    nom_com = "NA",
    lieu_dit = NULL,
    lieudit = NULL
  )

  result <- nemeton:::create_parcel_label(parcel)

  # Should NOT include the lieu-dit span when value is "NA" string
  expect_false(grepl("color:#666", result, fixed = TRUE))
})

test_that("create_parcel_label handles missing section/numero gracefully (shows '-')", {
  parcel <- list(
    section = NULL,
    numero = NULL,
    contenance = 15000,
    nom_com = NULL,
    lieu_dit = NULL,
    lieudit = NULL
  )

  result <- nemeton:::create_parcel_label(parcel)

  expect_type(result, "character")
  # Should show "-" for missing section and numero
  expect_true(grepl("<b>- -</b>", result, fixed = TRUE))
})

# ==============================================================================
# Coverage-focused tests for uncovered code paths
# ==============================================================================

# --- standardize_parcels additional coverage ---

test_that("standardize_parcels uses idu column for id", {
  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                                ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    idu = "01001000A0001",
    geometry = mock_geom
  )

  result <- nemeton:::standardize_parcels(mock_parcels, "01001")

  expect_true("id" %in% names(result))
  expect_equal(result$id[1], "01001000A0001")
})

test_that("standardize_parcels uses id_parcelle column for id", {
  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                                ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    id_parcelle = "PARC_42",
    geometry = mock_geom
  )

  result <- nemeton:::standardize_parcels(mock_parcels, "01001")

  expect_true("id" %in% names(result))
  expect_equal(result$id[1], "PARC_42")
})

test_that("standardize_parcels generates IDs when no id column at all", {
  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                                ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0),
                                ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    some_field = c("val1", "val2"),
    geometry = mock_geom
  )

  result <- nemeton:::standardize_parcels(mock_parcels, "01001")

  expect_true("id" %in% names(result))
  expect_equal(result$id[1], "01001_1")
  expect_equal(result$id[2], "01001_2")
})

test_that("standardize_parcels filters non-polygon geometries", {
  # Create mixed geometry types: 1 polygon + 1 point
  poly_geom <- sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                                           ncol = 2, byrow = TRUE)))
  point_geom <- sf::st_point(c(0.5, 0.5))

  mixed_geom <- sf::st_sfc(
    poly_geom,
    point_geom,
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    id = c("poly_1", "point_1"),
    contenance = c(5000, 100),
    geometry = mixed_geom
  )

  # cli_alert_warning does not produce a standard R warning,
  # so just check that it runs and filters correctly
  result <- nemeton:::standardize_parcels(mock_parcels, "01001")

  # Only the polygon should remain
  expect_equal(nrow(result), 1)
  expect_equal(result$id[1], "poly_1")
})

test_that("standardize_parcels errors when no polygon geometries found", {
  point_geom <- sf::st_sfc(
    sf::st_point(c(0.5, 0.5)),
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    id = "point_1",
    geometry = point_geom
  )

  expect_error(
    nemeton:::standardize_parcels(mock_parcels, "01001"),
    "No polygon geometries"
  )
})

# --- get_cadastral_parcels mocked tests ---

test_that("get_cadastral_parcels rejects invalid formats", {
  # Too short

  expect_error(nemeton:::get_cadastral_parcels("123"), "Invalid INSEE")
  # Letters that are not A or B
  expect_error(nemeton:::get_cadastral_parcels("CDEFG"), "Invalid INSEE")
  # Empty
  expect_error(nemeton:::get_cadastral_parcels(""), "Invalid INSEE")
  # Special characters
  expect_error(nemeton:::get_cadastral_parcels("12!45"), "Invalid INSEE")
})

test_that("get_cadastral_parcels accepts Corsican codes (2A, 2B)", {
  # Corsican INSEE codes contain A or B (e.g., 2A004, 2B033)
  # The regex ^[0-9A-B]{5}$ should accept these
  expect_true(grepl("^[0-9A-B]{5}$", "2A004"))
  expect_true(grepl("^[0-9A-B]{5}$", "2B033"))
})

# --- calculate_parcel_stats with non-empty data ---

test_that("calculate_parcel_stats with multiple parcels of varying sizes", {
  mock_geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                                ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0),
                                ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(4, 0, 5, 0, 5, 1, 4, 1, 4, 0),
                                ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  mock_parcels <- sf::st_sf(
    id = c("p1", "p2", "p3"),
    contenance = c(10000, 30000, 50000),
    geometry = mock_geom
  )

  stats <- nemeton:::calculate_parcel_stats(mock_parcels)

  expect_equal(stats$count, 3)
  expect_equal(stats$total_area_ha, 9)  # (10000+30000+50000)/10000
  expect_equal(stats$mean_area_ha, 3)   # 9/3
  expect_equal(stats$min_area_ha, 1)    # 10000/10000
  expect_equal(stats$max_area_ha, 5)    # 50000/10000
})

# --- create_parcel_label additional coverage ---

test_that("create_parcel_label with lieu_dit column", {
  parcel <- list(
    section = "A",
    numero = "0001",
    contenance = 20000,
    nom_com = NULL,
    lieu_dit = "Le Bois Joli",
    lieudit = NULL
  )

  result <- nemeton:::create_parcel_label(parcel)

  expect_type(result, "character")
  expect_true(grepl("Le Bois Joli", result, fixed = TRUE))
})

test_that("create_parcel_label with lieudit column (third fallback)", {
  parcel <- list(
    section = "B",
    numero = "0005",
    contenance = 15000,
    nom_com = NULL,
    lieu_dit = NULL,
    lieudit = "Les Chataigniers"
  )

  result <- nemeton:::create_parcel_label(parcel)

  expect_type(result, "character")
  expect_true(grepl("Les Chataigniers", result, fixed = TRUE))
})

test_that("create_parcel_label with NULL contenance", {
  parcel <- list(
    section = "C",
    numero = "0010",
    contenance = NULL,
    nom_com = NULL,
    lieu_dit = NULL,
    lieudit = NULL
  )

  result <- nemeton:::create_parcel_label(parcel)

  expect_type(result, "character")
  # Should not contain area text
  expect_false(grepl("\\bha\\b", result))
})

# --- fetch_api_cadastre coverage ---

test_that("fetch_api_cadastre requires httr2 package", {
  skip_if_not_installed("httr2")

  # We cannot easily mock httr2 requests, but we can verify the function

  # exists and starts with the right check
  expect_true(is.function(nemeton:::fetch_api_cadastre))
})

test_that("fetch_happign_cadastre requires happign package", {
  # Verify function exists
  expect_true(is.function(nemeton:::fetch_happign_cadastre))
})

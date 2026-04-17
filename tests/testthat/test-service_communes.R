# Tests for Commune Service
# Phase 2: Commune search and autocomplete
# Comprehensive tests for service_communes.R (target: 70%+ coverage)

# ==============================================================================
# get_departments tests
# ==============================================================================

test_that("get_departments returns all French departments", {
  depts <- nemetonshiny:::get_departments()

  expect_type(depts, "character")
  expect_true(length(depts) >= 100)  # France has ~100 departments

  # Check some known departments
  expect_true("01" %in% depts)   # Ain
  expect_true("75" %in% depts)   # Paris
  expect_true("2A" %in% depts)   # Corse-du-Sud
  expect_true("974" %in% depts)  # La Reunion
})

test_that("get_departments returns named vector", {
  depts <- nemetonshiny:::get_departments()

  expect_true(!is.null(names(depts)))
  expect_true(all(nchar(names(depts)) > 0))

  # Check format: "01 - Ain"
  expect_true(grepl("^[0-9A-B]{2,3} - ", names(depts)[1]))
})

test_that("get_departments includes overseas departments", {
  depts <- nemetonshiny:::get_departments()

  # Overseas departments
  expect_true("971" %in% depts)  # Guadeloupe
  expect_true("972" %in% depts)  # Martinique
  expect_true("973" %in% depts)  # Guyane
  expect_true("974" %in% depts)  # La Reunion
  expect_true("976" %in% depts)
})

test_that("get_departments has correct Corsican codes", {
  depts <- nemetonshiny:::get_departments()

  # Corsican departments use alphanumeric codes

  expect_true("2A" %in% depts)   # Corse-du-Sud
  expect_true("2B" %in% depts)   # Haute-Corse
})

# ==============================================================================
# validate_insee_code tests
# ==============================================================================

test_that("validate_insee_code accepts valid codes", {
  validate <- nemetonshiny:::validate_insee_code

  expect_true(validate("75056"))   # Paris
  expect_true(validate("01001"))   # Ain
  expect_true(validate("2A004"))   # Corsica
  expect_true(validate("97411"))   # La Reunion
  expect_true(validate("2B033"))   # Haute-Corse
})

test_that("validate_insee_code rejects invalid codes", {
  validate <- nemetonshiny:::validate_insee_code

  expect_false(validate("1234"))      # Too short
  expect_false(validate("123456"))    # Too long
  expect_false(validate("ABCDE"))     # Invalid chars (only A, B allowed)
  expect_false(validate(""))          # Empty
  expect_false(validate(NA))          # NA
  expect_false(validate(NULL))        # NULL
  expect_false(validate(12345))       # Numeric (not character)
})

test_that("validate_insee_code rejects invalid alphanumeric codes", {
  validate <- nemetonshiny:::validate_insee_code

  expect_false(validate("2C001"))     # C not allowed (only 0-9, A, B)
  expect_false(validate("2a001"))     # lowercase not allowed
  expect_false(validate("XYZAB"))     # Invalid letters other than A, B
  expect_false(validate("2c001"))     # lowercase c not allowed
})

test_that("validate_insee_code accepts codes with A and B anywhere", {
  validate <- nemetonshiny:::validate_insee_code

  # The regex allows A and B anywhere in the 5-character code
  expect_true(validate("A0001"))      # Leading letter A valid
  expect_true(validate("0000A"))      # Trailing letter A valid
  expect_true(validate("AABBB"))      # All letters valid
  expect_true(validate("12AB3"))      # Mixed valid
})

# ==============================================================================
# format_communes_for_selectize tests
# ==============================================================================

test_that("format_communes_for_selectize returns correct structure", {
  # Create mock commune data
  communes <- data.frame(
    code_insee = c("01001", "01002"),
    nom = c("Commune A", "Commune B"),
    code_postal = c("01100", "01200"),
    label = c("Commune A (01001)", "Commune B (01002)"),
    stringsAsFactors = FALSE
  )

  result <- nemetonshiny:::format_communes_for_selectize(communes)

  expect_type(result, "character")
  expect_length(result, 2)
  expect_equal(names(result)[1], "Commune A (01001)")
  expect_equal(result[[1]], "01001")
})

test_that("format_communes_for_selectize handles empty data", {
  communes <- data.frame(
    code_insee = character(0),
    nom = character(0),
    code_postal = character(0),
    label = character(0)
  )

  result <- nemetonshiny:::format_communes_for_selectize(communes)

  expect_length(result, 0)
  expect_type(result, "character")
})

test_that("format_communes_for_selectize handles NULL input", {
  result <- nemetonshiny:::format_communes_for_selectize(NULL)
  expect_length(result, 0)
  expect_type(result, "character")
})

test_that("format_communes_for_selectize handles single commune", {
  communes <- data.frame(
    code_insee = "75056",
    nom = "Paris",
    code_postal = "75001",
    label = "Paris (75001)",
    stringsAsFactors = FALSE
  )

  result <- nemetonshiny:::format_communes_for_selectize(communes)

  expect_length(result, 1)
  expect_equal(result[["Paris (75001)"]], "75056")
})

# ==============================================================================
# search_communes tests (short query handling)
# ==============================================================================

test_that("search_communes returns empty for short query", {
  # Query too short (< 3 chars), no API call needed
  result <- nemetonshiny:::search_communes("Pa")
  expect_equal(nrow(result), 0)
  expect_true("code_insee" %in% names(result))
  expect_true("nom" %in% names(result))
  expect_true("departement" %in% names(result))
  expect_true("label" %in% names(result))
})
test_that("search_communes returns empty for empty query", {
  result <- nemetonshiny:::search_communes("")
  expect_equal(nrow(result), 0)
})

test_that("search_communes returns empty for single character query", {
  result <- nemetonshiny:::search_communes("P")
  expect_equal(nrow(result), 0)
})

test_that("search_communes returns empty for two character query", {
  result <- nemetonshiny:::search_communes("Pa")
  expect_equal(nrow(result), 0)
})

# ==============================================================================
# search_communes tests (with mocking)
# ==============================================================================

test_that("search_communes parses API response correctly", {
  skip_if_not_installed("httr2")

  # Mock API response
  mock_response <- list(
    list(
      code = "75056",
      nom = "Paris",
      codesPostaux = c("75001", "75002", "75003"),
      codeDepartement = "75"
    ),
    list(
      code = "75057",
      nom = "Paris 1er",
      codesPostaux = c("75001"),
      codeDepartement = "75"
    )
  )

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) mock_response,
    .package = "httr2"
  )

  result <- nemetonshiny:::search_communes("Paris", limit = 5)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$code_insee[1], "75056")
  expect_equal(result$nom[1], "Paris")
  expect_equal(result$departement[1], "75")
  # Multiple postal codes should be joined
  expect_true(grepl("75001", result$code_postal[1]))
  expect_true(grepl("75002", result$code_postal[1]))
})

test_that("search_communes handles empty API response", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) list(),
    .package = "httr2"
  )

  result <- nemetonshiny:::search_communes("NonexistentCommune", limit = 5)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("search_communes handles API errors gracefully", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    req_perform = function(...) {
      stop("Connection timeout")
    },
    .package = "httr2"
  )

  # Should not throw, should return empty data.frame with warning
  expect_warning(
    result <- nemetonshiny:::search_communes("Paris"),
    regexp = "Error searching communes"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("search_communes with department filter returns data", {
  skip_if_not_installed("httr2")

  mock_response <- list(
    list(
      code = "73001",
      nom = "Saint-Alban-Leysse",
      codesPostaux = c("73230"),
      codeDepartement = "73"
    )
  )

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) mock_response,
    .package = "httr2"
  )

  result <- nemetonshiny:::search_communes("Saint", department = "73")

  # Function should work with department parameter
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$departement[1], "73")
})

test_that("search_communes without department filter returns data", {
  skip_if_not_installed("httr2")

  mock_response <- list(
    list(
      code = "73001",
      nom = "Saint-Alban-Leysse",
      codesPostaux = c("73230"),
      codeDepartement = "73"
    ),
    list(
      code = "01001",
      nom = "Saint-Andre",
      codesPostaux = c("01001"),
      codeDepartement = "01"
    )
  )

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) mock_response,
    .package = "httr2"
  )

  # Test with empty string department
  result <- nemetonshiny:::search_communes("Saint", department = "")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)

  # Test with NULL department
  result <- nemetonshiny:::search_communes("Saint", department = NULL)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})

# ==============================================================================
# search_by_postal_code tests
# ==============================================================================

test_that("search_by_postal_code validates input format", {
  # Invalid postal code formats
  result <- nemetonshiny:::search_by_postal_code("123")
  expect_equal(nrow(result), 0)

  result <- nemetonshiny:::search_by_postal_code("ABCDE")
  expect_equal(nrow(result), 0)

  result <- nemetonshiny:::search_by_postal_code("123456")
  expect_equal(nrow(result), 0)

  result <- nemetonshiny:::search_by_postal_code("7500")
  expect_equal(nrow(result), 0)
})

test_that("search_by_postal_code returns correct structure for invalid input", {
  result <- nemetonshiny:::search_by_postal_code("ABC")

  expect_s3_class(result, "data.frame")
  expect_true("code_insee" %in% names(result))
  expect_true("nom" %in% names(result))
  expect_true("code_postal" %in% names(result))
  expect_true("departement" %in% names(result))
  expect_true("label" %in% names(result))
})

test_that("search_by_postal_code parses API response correctly", {
  skip_if_not_installed("httr2")

  mock_response <- list(
    list(
      code = "75101",
      nom = "Paris 1er Arrondissement",
      codesPostaux = c("75001"),
      codeDepartement = "75"
    )
  )

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) mock_response,
    .package = "httr2"
  )

  result <- nemetonshiny:::search_by_postal_code("75001")

  expect_equal(nrow(result), 1)
  expect_equal(result$code_insee[1], "75101")
  expect_equal(result$nom[1], "Paris 1er Arrondissement")
})

test_that("search_by_postal_code handles multiple communes for same postal code", {
  skip_if_not_installed("httr2")

  # Some postal codes cover multiple communes
  mock_response <- list(
    list(
      code = "01001",
      nom = "Commune A",
      codesPostaux = c("01100"),
      codeDepartement = "01"
    ),
    list(
      code = "01002",
      nom = "Commune B",
      codesPostaux = c("01100"),
      codeDepartement = "01"
    )
  )

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) mock_response,
    .package = "httr2"
  )

  result <- nemetonshiny:::search_by_postal_code("01100")

  expect_equal(nrow(result), 2)
})

test_that("search_by_postal_code handles API errors gracefully", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    req_perform = function(...) {
      stop("Network error")
    },
    .package = "httr2"
  )

  expect_warning(
    result <- nemetonshiny:::search_by_postal_code("75001"),
    regexp = "Error searching by postal code"
  )

  expect_equal(nrow(result), 0)
})

# ==============================================================================
# get_commune_geometry tests
# ==============================================================================

test_that("get_commune_geometry validates INSEE code format", {
  # Invalid code returns NULL with warning
  expect_warning(
    result <- nemetonshiny:::get_commune_geometry("invalid"),
    regexp = "Invalid"
  )
  expect_null(result)
})

test_that("get_commune_geometry validates short codes", {
  expect_warning(
    result <- nemetonshiny:::get_commune_geometry("1234"),
    regexp = "Invalid"
  )
  expect_null(result)
})

test_that("get_commune_geometry validates long codes", {
  expect_warning(
    result <- nemetonshiny:::get_commune_geometry("123456"),
    regexp = "Invalid"
  )
  expect_null(result)
})

test_that("get_commune_geometry handles missing contour", {
  skip_if_not_installed("httr2")

  mock_response <- list(
    code = "75056",
    nom = "Paris",
    contour = NULL
  )

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) mock_response,
    .package = "httr2"
  )

  expect_warning(
    result <- nemetonshiny:::get_commune_geometry("75056"),
    regexp = "No contour"
  )
  expect_null(result)
})

test_that("get_commune_geometry handles API errors gracefully", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    req_perform = function(...) {
      stop("Server error 500")
    },
    .package = "httr2"
  )

  expect_warning(
    result <- nemetonshiny:::get_commune_geometry("75056"),
    regexp = "Error getting commune geometry"
  )
  expect_null(result)
})

# ==============================================================================
# get_commune_centroid tests
# ==============================================================================

test_that("get_commune_centroid parses response correctly", {
  skip_if_not_installed("httr2")

  mock_response <- list(
    centre = list(
      type = "Point",
      coordinates = c(2.3522, 48.8566)
    )
  )

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) mock_response,
    .package = "httr2"
  )

  result <- nemetonshiny:::get_commune_centroid("75056")

  expect_type(result, "double")
  expect_length(result, 2)
  expect_equal(result["lng"], c(lng = 2.3522))
  expect_equal(result["lat"], c(lat = 48.8566))
})

test_that("get_commune_centroid handles missing centre", {
  skip_if_not_installed("httr2")

  mock_response <- list(
    code = "75056",
    nom = "Paris"
    # No centre field
  )

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) mock_response,
    .package = "httr2"
  )

  result <- nemetonshiny:::get_commune_centroid("75056")
  expect_null(result)
})

test_that("get_commune_centroid handles missing coordinates", {
  skip_if_not_installed("httr2")

  mock_response <- list(
    centre = list(
      type = "Point"
      # No coordinates
    )
  )

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) mock_response,
    .package = "httr2"
  )

  result <- nemetonshiny:::get_commune_centroid("75056")
  expect_null(result)
})

test_that("get_commune_centroid handles API errors gracefully", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    req_perform = function(...) {
      stop("Connection refused")
    },
    .package = "httr2"
  )

  expect_warning(
    result <- nemetonshiny:::get_commune_centroid("75056"),
    regexp = "Error getting commune centroid"
  )
  expect_null(result)
})

# ==============================================================================
# get_communes_in_department tests
# ==============================================================================

test_that("get_communes_in_department returns empty for NULL department", {
  result <- nemetonshiny:::get_communes_in_department(NULL)
  expect_equal(nrow(result), 0)
})

test_that("get_communes_in_department returns empty for empty department", {
  result <- nemetonshiny:::get_communes_in_department("")
  expect_equal(nrow(result), 0)
})

test_that("get_communes_in_department parses response correctly", {
  skip_if_not_installed("httr2")

  mock_response <- list(
    list(
      code = "75101",
      nom = "Paris 1er Arrondissement",
      codesPostaux = c("75001")
    ),
    list(
      code = "75102",
      nom = "Paris 2eme Arrondissement",
      codesPostaux = c("75002")
    )
  )

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) mock_response,
    .package = "httr2"
  )

  result <- nemetonshiny:::get_communes_in_department("75")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("code_insee" %in% names(result))
  expect_true("nom" %in% names(result))
  expect_true("code_postal" %in% names(result))
  expect_true("label" %in% names(result))
})

test_that("get_communes_in_department sorts results by name", {
  skip_if_not_installed("httr2")

  mock_response <- list(
    list(
      code = "01003",
      nom = "Zebourg",
      codesPostaux = c("01300")
    ),
    list(
      code = "01001",
      nom = "Aville",
      codesPostaux = c("01100")
    ),
    list(
      code = "01002",
      nom = "Middleton",
      codesPostaux = c("01200")
    )
  )

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) mock_response,
    .package = "httr2"
  )

  result <- nemetonshiny:::get_communes_in_department("01")

  # Should be sorted: Aville, Middleton, Zebourg
  expect_equal(result$nom[1], "Aville")
  expect_equal(result$nom[2], "Middleton")
  expect_equal(result$nom[3], "Zebourg")
})

test_that("get_communes_in_department handles empty postal codes", {
  skip_if_not_installed("httr2")

  mock_response <- list(
    list(
      code = "97501",
      nom = "Saint-Pierre",
      codesPostaux = list()  # Empty postal codes
    )
  )

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) mock_response,
    .package = "httr2"
  )

  result <- nemetonshiny:::get_communes_in_department("975")

  expect_equal(nrow(result), 1)
  expect_equal(result$code_postal[1], "")
})

test_that("get_communes_in_department handles empty API response", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    req_perform = function(...) {
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) list(),
    .package = "httr2"
  )

  result <- nemetonshiny:::get_communes_in_department("99")

  expect_equal(nrow(result), 0)
})

test_that("get_communes_in_department handles network errors", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    req_perform = function(...) {
      stop("Could not resolve host: geo.api.gouv.fr")
    },
    .package = "httr2"
  )

  expect_warning(
    result <- nemetonshiny:::get_communes_in_department("75"),
    regexp = "Error getting communes"
  )

  expect_equal(nrow(result), 0)
  expect_equal(attr(result, "error"), "network")
  expect_equal(attr(result, "error_message"), "no_internet_connection")
})

test_that("get_communes_in_department handles timeout errors", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    req_perform = function(...) {
      stop("HTTP request timeout after 30 seconds")
    },
    .package = "httr2"
  )

  expect_warning(
    result <- nemetonshiny:::get_communes_in_department("75"),
    regexp = "Error getting communes"
  )

  expect_equal(nrow(result), 0)
  expect_equal(attr(result, "error"), "network")
})

test_that("get_communes_in_department handles curl errors", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    req_perform = function(...) {
      stop("curl error: connection refused")
    },
    .package = "httr2"
  )

  expect_warning(
    result <- nemetonshiny:::get_communes_in_department("75"),
    regexp = "Error getting communes"
  )

  expect_equal(attr(result, "error"), "network")
})

test_that("get_communes_in_department handles non-network errors", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    req_perform = function(...) {
      stop("Invalid JSON response")
    },
    .package = "httr2"
  )

  expect_warning(
    result <- nemetonshiny:::get_communes_in_department("75"),
    regexp = "Error getting communes"
  )

  expect_equal(nrow(result), 0)
  expect_equal(attr(result, "error"), "other")
})

# ==============================================================================
# Integration tests (require network)
# ==============================================================================

test_that("search_communes returns correct structure with real API", {
  skip_if_offline()
  skip_on_cran()

  result <- nemetonshiny:::search_communes("Paris", limit = 5)

  if (nrow(result) > 0) {
    expect_true("code_insee" %in% names(result))
    expect_true("nom" %in% names(result))
    expect_true("departement" %in% names(result))
    expect_true("code_postal" %in% names(result))
    expect_true("label" %in% names(result))
  }
})

test_that("search_communes finds Paris with real API", {
  skip_if_offline()
  skip_on_cran()

  result <- nemetonshiny:::search_communes("Paris", limit = 20)

  if (nrow(result) > 0) {
    # Paris should be in results
    paris <- result[result$code_insee == "75056", ]
    if (nrow(paris) > 0) {
      expect_equal(paris$departement, "75")
    }
  }
})

test_that("get_commune_geometry returns sf object with real API", {
  skip_if_offline()
  skip_on_cran()
  skip_if_not_installed("sf")

  # Use Paris as test case (stable)
  geom <- nemetonshiny:::get_commune_geometry("75056")

  if (!is.null(geom)) {
    expect_s3_class(geom, "sf")
    expect_true(sf::st_crs(geom)$epsg == 4326)
  }
})

test_that("get_communes_in_department filters correctly with real API", {
  skip_if_offline()
  skip_on_cran()

  result <- nemetonshiny:::get_communes_in_department("75")

  if (nrow(result) > 0) {
    # All should be Paris arrondissements
    expect_true(all(grepl("^75", result$code_insee)))
  }
})

test_that("search_by_postal_code works with real API", {
  skip_if_offline()
  skip_on_cran()

  result <- nemetonshiny:::search_by_postal_code("75001")

  if (nrow(result) > 0) {
    expect_true(any(grepl("Paris", result$nom)))
  }
})

test_that("get_commune_centroid returns coordinates with real API", {
  skip_if_offline()
  skip_on_cran()

  result <- nemetonshiny:::get_commune_centroid("75056")

  if (!is.null(result)) {
    expect_length(result, 2)
    expect_true(result["lng"] > 2 && result["lng"] < 3)  # Paris longitude
    expect_true(result["lat"] > 48 && result["lat"] < 49)  # Paris latitude
  }
})

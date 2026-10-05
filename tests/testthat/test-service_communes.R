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




# ==============================================================================
# search_communes tests (with mocking)
# ==============================================================================






# ==============================================================================
# search_by_postal_code tests
# ==============================================================================






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
  nemetonshiny:::.reset_communes_dept_cache()

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
  nemetonshiny:::.reset_communes_dept_cache()

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
  nemetonshiny:::.reset_communes_dept_cache()

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
  nemetonshiny:::.reset_communes_dept_cache()

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

test_that("get_communes_in_department caches per department (no refetch)", {
  skip_if_not_installed("httr2")
  nemetonshiny:::.reset_communes_dept_cache()

  calls <- 0L
  local_mocked_bindings(
    req_perform = function(...) {
      calls <<- calls + 1L
      structure(list(), class = "httr2_response")
    },
    resp_body_json = function(...) list(
      list(code = "01001", nom = "Bourg", codesPostaux = list("01000"))
    ),
    .package = "httr2"
  )

  r1 <- nemetonshiny:::get_communes_in_department("01")
  r2 <- nemetonshiny:::get_communes_in_department("01")  # cache hit
  expect_equal(nrow(r1), 1L)
  expect_identical(r1, r2)
  expect_equal(calls, 1L)  # un seul appel réseau malgré 2 demandes

  # reset → refetch
  nemetonshiny:::.reset_communes_dept_cache()
  nemetonshiny:::get_communes_in_department("01")
  expect_equal(calls, 2L)
})

# ==============================================================================
# Integration tests (require network)
# ==============================================================================



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



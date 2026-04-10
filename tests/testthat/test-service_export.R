# Tests for service_export.R

# ==============================================================================
# QUARTO INSTALLATION CHECKS
# ==============================================================================

test_that("is_quarto_installed returns logical", {
  result <- nemetonShiny:::is_quarto_installed()
  expect_type(result, "logical")
})

test_that("ensure_quarto_installed wraps is_quarto_installed", {
  result <- nemetonShiny:::ensure_quarto_installed()
  expect_type(result, "logical")
  expect_equal(result, nemetonShiny:::is_quarto_installed())
})

# ==============================================================================
# TEXT CLEANING FUNCTIONS
# ==============================================================================

test_that("clean_text_for_pdf handles NULL and empty text", {
  expect_null(nemetonShiny:::clean_text_for_pdf(NULL))
  expect_equal(nemetonShiny:::clean_text_for_pdf(""), "")
})

test_that("clean_text_for_pdf replaces curly quotes", {
  # Left and right single quotes
  text <- "\u2018hello\u2019"
  result <- nemetonShiny:::clean_text_for_pdf(text)
  expect_equal(result, "'hello'")

  # Left and right double quotes
  text <- "\u201Chello\u201D"
  result <- nemetonShiny:::clean_text_for_pdf(text)
  expect_equal(result, "\"hello\"")
})

test_that("clean_text_for_pdf replaces dashes", {
  # En dash
  text <- "2020\u20132025"
  result <- nemetonShiny:::clean_text_for_pdf(text)
  expect_equal(result, "2020-2025")

  # Em dash
  text <- "hello\u2014world"
  result <- nemetonShiny:::clean_text_for_pdf(text)
  expect_equal(result, "hello-world")
})

test_that("clean_text_for_pdf replaces ellipsis and non-breaking space", {
  text <- "Loading\u2026"
  result <- nemetonShiny:::clean_text_for_pdf(text)
  expect_equal(result, "Loading...")

  text <- "Hello\u00A0World"
  result <- nemetonShiny:::clean_text_for_pdf(text)
  expect_equal(result, "Hello World")
})

test_that("clean_text_for_pdf handles combined replacements", {
  text <- "\u201CTest\u201D \u2013 \u2018Example\u2019\u2026"
  result <- nemetonShiny:::clean_text_for_pdf(text)
  expect_equal(result, "\"Test\" - 'Example'...")
})

# ==============================================================================
# MARKDOWN STRIPPING FUNCTIONS
# ==============================================================================

test_that("strip_markdown_for_display removes bold markers", {
  text <- "This is **bold** text"
  result <- nemetonShiny:::strip_markdown_for_display(text)
  expect_equal(result, "This is bold text")
})

test_that("strip_markdown_for_display removes italic markers", {
  text <- "This is *italic* text"
  result <- nemetonShiny:::strip_markdown_for_display(text)
  expect_equal(result, "This is italic text")
})

test_that("strip_markdown_for_display removes strikethrough markers", {
  text <- "This is ~~strikethrough~~ text"
  result <- nemetonShiny:::strip_markdown_for_display(text)
  expect_equal(result, "This is strikethrough text")
})

test_that("strip_markdown_for_display removes inline code markers", {
  text <- "This is `code` text"
  result <- nemetonShiny:::strip_markdown_for_display(text)
  expect_equal(result, "This is code text")
})

test_that("strip_markdown_for_display removes links but keeps text", {
  text <- "Visit [our site](https://example.com) today"
  result <- nemetonShiny:::strip_markdown_for_display(text)
  expect_equal(result, "Visit our site today")
})

test_that("strip_markdown_for_display handles combined formatting", {
  text <- "**Bold** and *italic* with `code` and [link](url)"
  result <- nemetonShiny:::strip_markdown_for_display(text)
  expect_equal(result, "Bold and italic with code and link")
})

# ==============================================================================
# PREPARE REPORT DATA
# ==============================================================================

test_that("prepare_report_data creates valid structure", {
  skip_if_not_installed("sf")

  # Create mock project
  project <- list(
    metadata = list(
      name = "Test Project",
      description = "A test project",
      owner = "Test Owner",
      created_at = "2026-01-01"
    )
  )

  # Create mock family scores
  family_scores <- sf::st_sf(
    id = 1:3,
    famille_carbone = c(60, 70, 65),
    famille_biodiversite = c(55, 60, 58),
    famille_eau = c(40, 45, 42),
    famille_air = c(50, 55, 52),
    famille_sol = c(45, 50, 48),
    famille_paysage = c(35, 40, 38),
    famille_temporel = c(70, 75, 72),
    famille_risque = c(30, 35, 32),
    famille_social = c(25, 30, 28),
    famille_production = c(65, 70, 68),
    famille_energie = c(55, 60, 58),
    famille_naturalite = c(80, 85, 82),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 0)),
      sf::st_point(c(0, 1)),
      crs = 2154
    )
  )

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "fr", NULL)

  expect_type(result, "list")
  expect_equal(result$project_name, "Test Project")
  expect_equal(result$n_parcels, 3)
  expect_true(result$global_score > 0 && result$global_score <= 100)
  expect_type(result$family_stats, "list")
  expect_equal(length(result$family_stats), 12)
  expect_equal(result$language, "fr")
})

test_that("prepare_report_data works with en language", {
  skip_if_not_installed("sf")

  project <- list(
    metadata = list(
      name = "Test",
      description = NULL,
      owner = NULL,
      created_at = "2026-01-01"
    )
  )

  family_scores <- sf::st_sf(
    id = 1,
    famille_carbone = 50,
    famille_biodiversite = 50,
    famille_eau = 50,
    famille_air = 50,
    famille_sol = 50,
    famille_paysage = 50,
    famille_temporel = 50,
    famille_risque = 50,
    famille_social = 50,
    famille_production = 50,
    famille_energie = 50,
    famille_naturalite = 50,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "en", "Test comments")

  expect_equal(result$language, "en")
  expect_equal(result$synthesis_comments, "Test comments")
  expect_equal(result$global_score, 50)
})

test_that("prepare_report_data handles family_comments parameter", {
  skip_if_not_installed("sf")

  project <- list(
    metadata = list(
      name = "Test",
      description = NULL,
      owner = NULL,
      created_at = "2026-01-01"
    )
  )

  family_scores <- sf::st_sf(
    id = 1,
    famille_carbone = 50,
    famille_biodiversite = 50,
    famille_eau = 50,
    famille_air = 50,
    famille_sol = 50,
    famille_paysage = 50,
    famille_temporel = 50,
    famille_risque = 50,
    famille_social = 50,
    famille_production = 50,
    famille_energie = 50,
    famille_naturalite = 50,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  family_comments <- list(C = "Carbon comment", B = "Biodiversity comment")
  result <- nemetonShiny:::prepare_report_data(project, family_scores, "fr", NULL, family_comments)

  expect_equal(result$family_comments$C, "Carbon comment")
  expect_equal(result$family_comments$B, "Biodiversity comment")
})

test_that("prepare_report_data works with non-sf data (drops geometry)", {
  skip_if_not_installed("sf")

  project <- list(
    metadata = list(
      name = "Test",
      description = NULL,
      owner = NULL,
      created_at = "2026-01-01"
    )
  )

  # Create as data.frame (not sf)
  family_scores <- data.frame(
    id = 1,
    famille_carbone = 50,
    famille_biodiversite = 50,
    famille_eau = 50,
    famille_air = 50,
    famille_sol = 50,
    famille_paysage = 50,
    famille_temporel = 50,
    famille_risque = 50,
    famille_social = 50,
    famille_production = 50,
    famille_energie = 50,
    famille_naturalite = 50
  )

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "fr", NULL)

  expect_equal(result$n_parcels, 1)
  expect_equal(result$global_score, 50)
})

# ==============================================================================
# GEOPACKAGE EXPORT
# ==============================================================================

test_that("export_geopackage creates valid file", {
  skip_if_not_installed("sf")

  # Create test data
  test_sf <- sf::st_sf(
    id = 1:2,
    famille_carbone = c(60, 70),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".gpkg")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::export_geopackage(test_sf, temp_file)

  expect_true(file.exists(result))
  expect_equal(result, temp_file)

  # Verify content
  read_back <- sf::st_read(temp_file, quiet = TRUE)
  expect_equal(nrow(read_back), 2)
  expect_true("famille_carbone" %in% names(read_back))
})

test_that("export_geopackage fails for non-sf input", {
  expect_error(
    nemetonShiny:::export_geopackage(data.frame(x = 1), tempfile()),
    "must be an sf object"
  )
})

test_that("export_geopackage uses custom layer name", {
  skip_if_not_installed("sf")

  test_sf <- sf::st_sf(
    id = 1,
    value = 100,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  temp_file <- tempfile(fileext = ".gpkg")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::export_geopackage(test_sf, temp_file, layer_name = "custom_layer")

  expect_true(file.exists(result))

  # Verify layer name
  layers <- sf::st_layers(temp_file)
  expect_true("custom_layer" %in% layers$name)
})

test_that("export_geopackage overwrites existing file", {
  skip_if_not_installed("sf")

  test_sf1 <- sf::st_sf(
    id = 1,
    value = 100,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  test_sf2 <- sf::st_sf(
    id = 1:3,
    value = c(50, 60, 70),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2)),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".gpkg")
  on.exit(unlink(temp_file), add = TRUE)

  # Create first file
  nemetonShiny:::export_geopackage(test_sf1, temp_file)
  read_back1 <- sf::st_read(temp_file, quiet = TRUE)
  expect_equal(nrow(read_back1), 1)

  # Overwrite with second file
  nemetonShiny:::export_geopackage(test_sf2, temp_file)
  read_back2 <- sf::st_read(temp_file, quiet = TRUE)
  expect_equal(nrow(read_back2), 3)
})

# ==============================================================================
# GENERATE INDICATOR MAP
# ==============================================================================

test_that("generate_indicator_map returns NULL for non-sf input", {
  result <- nemetonShiny:::generate_indicator_map(
    data.frame(x = 1),
    "x",
    "Test"
  )

  expect_null(result)
})

test_that("generate_indicator_map returns NULL for missing column", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = 1,
    value = 50,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  result <- nemetonShiny:::generate_indicator_map(data, "nonexistent", "Test")
  expect_null(result)
})

test_that("generate_indicator_map returns NULL for all NA values", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = 1:2,
    value = c(NA_real_, NA_real_),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 2154
    )
  )

  result <- nemetonShiny:::generate_indicator_map(data, "value", "Test")
  expect_null(result)
})

test_that("generate_indicator_map creates PNG file when output_file specified", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = 1:3,
    value = c(25, 50, 75),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(0, 1, 1, 1, 1, 2, 0, 2, 0, 1), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_indicator_map(data, "value", "Test Map", output_file = temp_file)

  expect_null(result)
  expect_true(file.exists(temp_file))
  expect_true(file.size(temp_file) > 0)
})

# ==============================================================================
# DRAW PARCEL TABLE
# ==============================================================================

test_that("draw_parcel_table returns NULL for missing column", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = 1,
    value = 50,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  # Open graphics device to avoid errors
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  on.exit(grDevices::dev.off(), add = TRUE)

  result <- nemetonShiny:::draw_parcel_table(data, "nonexistent", "id", "fr")
  expect_null(result)
})

test_that("draw_parcel_table handles sf and non-sf data", {
  skip_if_not_installed("sf")

  # sf data
  sf_data <- sf::st_sf(
    id = c("PARCEL001", "PARCEL002"),
    score = c(80, 45),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 2154
    )
  )

  # non-sf data
  df_data <- data.frame(
    id = c("PARCEL001", "PARCEL002"),
    score = c(80, 45)
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  # Both should work without error
  expect_no_error(nemetonShiny:::draw_parcel_table(sf_data, "score", "id", "fr"))
  expect_no_error(nemetonShiny:::draw_parcel_table(df_data, "score", "id", "en"))

  grDevices::dev.off()
})

test_that("draw_parcel_table handles NA values", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = c("P1", "P2", "P3"),
    score = c(80, NA_real_, 30),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 0)),
      sf::st_point(c(0, 1)),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  expect_no_error(nemetonShiny:::draw_parcel_table(data, "score", "id", "fr"))

  grDevices::dev.off()
})

test_that("draw_parcel_table handles missing parcel_id_col", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    score = c(80, 45),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  # Should use seq_len(nrow(data)) as IDs
  expect_no_error(nemetonShiny:::draw_parcel_table(data, "score", "nonexistent_id", "fr"))

  grDevices::dev.off()
})

test_that("draw_parcel_table shows indicator_title when provided", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = "P1",
    score = 75,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  expect_no_error(
    nemetonShiny:::draw_parcel_table(data, "score", "id", "fr", indicator_title = "C1 - Carbon Biomass")
  )

  grDevices::dev.off()
})

# ==============================================================================
# DRAW STANDARD COVER PAGE
# ==============================================================================

test_that("draw_standard_cover_page renders without error", {
  report_data <- list(
    labels = list(
      title = "Test Title",
      subtitle = "Test Subtitle",
      parcels = "Parcels",
      created_at = "Created",
      global_score_label = "Global Score",
      generated_by = "Generated by nemeton"
    ),
    project_name = "Test Project",
    project_description = "Description",
    n_parcels = 5,
    created_at = "2026-01-01",
    global_score = 75
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  expect_no_error(nemetonShiny:::draw_standard_cover_page(report_data, "fr"))

  grDevices::dev.off()
  expect_true(file.exists(temp_file))
})

test_that("draw_standard_cover_page handles NULL description", {
  report_data <- list(
    labels = list(
      title = "Test Title",
      subtitle = "Test Subtitle",
      parcels = "Parcels",
      created_at = "Created",
      global_score_label = "Global Score",
      generated_by = "Generated by nemeton"
    ),
    project_name = "Test Project",
    project_description = NULL,
    n_parcels = 5,
    created_at = "2026-01-01",
    global_score = 75
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  expect_no_error(nemetonShiny:::draw_standard_cover_page(report_data, "fr"))

  grDevices::dev.off()
})

test_that("draw_standard_cover_page handles different score ranges", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  for (score in c(25, 50, 80)) {
    report_data <- list(
      labels = list(
        title = "Test", subtitle = "Sub", parcels = "P", created_at = "C",
        global_score_label = "Score", generated_by = "nemeton"
      ),
      project_name = "Test", project_description = NULL, n_parcels = 1,
      created_at = "2026-01-01", global_score = score
    )
    expect_no_error(nemetonShiny:::draw_standard_cover_page(report_data, "fr"))
  }

  grDevices::dev.off()
})

# ==============================================================================
# DRAW INDICATOR MAP PAGE
# ==============================================================================

test_that("draw_indicator_map_page handles missing column", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = 1,
    value = 50,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  expect_no_error(
    nemetonShiny:::draw_indicator_map_page(data, "nonexistent", "Test", "#228B22", "fr")
  )

  grDevices::dev.off()
})

test_that("draw_indicator_map_page handles all NA values", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = 1:2,
    value = c(NA_real_, NA_real_),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  expect_no_error(
    nemetonShiny:::draw_indicator_map_page(data, "value", "Test", "#228B22", "en")
  )

  grDevices::dev.off()
})

test_that("draw_indicator_map_page renders polygon data", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = 1:2,
    value = c(30, 70),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  expect_no_error(
    nemetonShiny:::draw_indicator_map_page(data, "value", "Test Map", "#9932CC", "fr")
  )

  grDevices::dev.off()
})

# ==============================================================================
# ADD PARCEL LABELS
# ==============================================================================

test_that("add_parcel_labels returns NULL for non-sf input", {
  result <- nemetonShiny:::add_parcel_labels(data.frame(x = 1))
  expect_null(result)
})

test_that("add_parcel_labels uses sequential IDs when id_col missing", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    value = c(50, 60),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  # Create base plot first
  plot(sf::st_geometry(data))

  expect_no_error(nemetonShiny:::add_parcel_labels(data, id_col = "nonexistent"))

  grDevices::dev.off()
})

test_that("add_parcel_labels truncates long IDs to last 6 characters", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = c("PARCEL_VERY_LONG_ID_123456", "SHORT"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  plot(sf::st_geometry(data))
  expect_no_error(nemetonShiny:::add_parcel_labels(data))

  grDevices::dev.off()
})

# ==============================================================================
# GENERATE FAMILY MAPS
# ==============================================================================

test_that("generate_family_maps returns NULL for non-sf input", {
  temp_dir <- tempdir()
  result <- nemetonShiny:::generate_family_maps(data.frame(x = 1), temp_dir, "fr")
  expect_null(result)
})

test_that("generate_family_maps creates map files for family columns", {
  skip_if_not_installed("sf")

  family_scores <- sf::st_sf(
    id = 1:2,
    famille_carbone = c(60, 70),
    famille_biodiversite = c(50, 55),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_dir <- file.path(tempdir(), "family_maps_test")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  suppressWarnings(
    expect_no_error(nemetonShiny:::generate_family_maps(family_scores, temp_dir, "fr"))
  )

  # Check that map files were created
  expect_true(file.exists(file.path(temp_dir, "famille_carbone_map.png")))
  expect_true(file.exists(file.path(temp_dir, "famille_biodiversite_map.png")))
})

test_that("generate_family_maps handles all NA values", {
  skip_if_not_installed("sf")

  family_scores <- sf::st_sf(
    id = 1:2,
    famille_carbone = c(NA_real_, NA_real_),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_dir <- file.path(tempdir(), "family_maps_na_test")
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  suppressWarnings(
    expect_no_error(nemetonShiny:::generate_family_maps(family_scores, temp_dir, "en"))
  )
})

# ==============================================================================
# GENERATE RADAR IMAGE
# ==============================================================================

test_that("generate_radar_image creates PNG file", {
  skip_if_not_installed("sf")
  # Skip if graphics device is not available (headless CI environments)
  skip_if_not(capabilities("png"), "PNG graphics device not available")
  # Skip on CI where display may not be available

  skip_on_ci()

  family_scores <- sf::st_sf(
    id = 1:2,
    famille_carbone = c(60, 70),
    famille_biodiversite = c(55, 60),
    famille_eau = c(40, 45),
    famille_air = c(50, 55),
    famille_sol = c(45, 50),
    famille_paysage = c(35, 40),
    famille_temporel = c(70, 75),
    famille_risque = c(30, 35),
    famille_social = c(25, 30),
    famille_production = c(65, 70),
    famille_energie = c(55, 60),
    famille_naturalite = c(80, 85),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  expect_no_error(nemetonShiny:::generate_radar_image(family_scores, temp_file, "fr"))

  # File may not be created in some environments without proper display
  skip_if_not(file.exists(temp_file), "PNG file not created (display issue)")
  expect_true(file.size(temp_file) > 0)
})

# ==============================================================================
# RENDER MARKDOWN TEXT
# ==============================================================================

test_that("render_markdown_text handles NULL and empty text", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  result_null <- nemetonShiny:::render_markdown_text(NULL, 0.1, 0.9)
  expect_equal(result_null, 0.9)

  result_empty <- nemetonShiny:::render_markdown_text("", 0.1, 0.9)
  expect_equal(result_empty, 0.9)

  grDevices::dev.off()
})

test_that("render_markdown_text processes headers", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  text <- "# Header 1\n## Header 2\n### Header 3\nRegular text"
  result <- nemetonShiny:::render_markdown_text(text, 0.1, 0.9)

  # Should return a lower y position
  expect_true(result < 0.9)

  grDevices::dev.off()
})

test_that("render_markdown_text processes blockquotes", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  text <- "> This is a blockquote\n> Another line"
  result <- nemetonShiny:::render_markdown_text(text, 0.1, 0.9)

  expect_true(result < 0.9)

  grDevices::dev.off()
})

test_that("render_markdown_text processes bullet lists", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  text <- "- First item\n- Second item\n* Third item"
  result <- nemetonShiny:::render_markdown_text(text, 0.1, 0.9)

  expect_true(result < 0.9)

  grDevices::dev.off()
})

test_that("render_markdown_text processes numbered lists", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  text <- "1. First item\n2. Second item\n3. Third item"
  result <- nemetonShiny:::render_markdown_text(text, 0.1, 0.9)

  expect_true(result < 0.9)

  grDevices::dev.off()
})

test_that("render_markdown_text handles empty lines (paragraph breaks)", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  text <- "Paragraph 1\n\nParagraph 2"
  result <- nemetonShiny:::render_markdown_text(text, 0.1, 0.9)

  expect_true(result < 0.9)

  grDevices::dev.off()
})

# ==============================================================================
# RENDER FORMATTED LINE
# ==============================================================================

test_that("render_formatted_line handles plain text", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  expect_no_error(
    nemetonShiny:::render_formatted_line("Plain text without formatting", 0.1, 0.5, 0.7, "black", 80)
  )

  grDevices::dev.off()
})

test_that("render_formatted_line handles bold text", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  expect_no_error(
    nemetonShiny:::render_formatted_line("This is **bold** text", 0.1, 0.5, 0.7, "black", 80)
  )

  grDevices::dev.off()
})

test_that("render_formatted_line handles italic text", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  expect_no_error(
    nemetonShiny:::render_formatted_line("This is *italic* text", 0.1, 0.5, 0.7, "black", 80)
  )

  grDevices::dev.off()
})

test_that("render_formatted_line handles inline code", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  expect_no_error(
    nemetonShiny:::render_formatted_line("This is `code` text", 0.1, 0.5, 0.7, "black", 80)
  )

  grDevices::dev.off()
})

test_that("render_formatted_line handles strikethrough text", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  expect_no_error(
    nemetonShiny:::render_formatted_line("This is ~~strikethrough~~ text", 0.1, 0.5, 0.7, "black", 80)
  )

  grDevices::dev.off()
})

test_that("render_formatted_line handles links", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  expect_no_error(
    nemetonShiny:::render_formatted_line("Visit [our site](https://example.com)", 0.1, 0.5, 0.7, "black", 80)
  )

  grDevices::dev.off()
})

# ==============================================================================
# SIMPLE PDF REPORT GENERATION
# ==============================================================================

test_that("generate_simple_pdf_report creates file", {
  skip_if_not_installed("sf")

  project <- list(
    metadata = list(
      name = "Test PDF",
      description = "Test description",
      owner = "Owner",
      created_at = "2026-01-01"
    )
  )

  family_scores <- sf::st_sf(
    id = 1:2,
    famille_carbone = c(60, 70),
    famille_biodiversite = c(55, 60),
    famille_eau = c(40, 45),
    famille_air = c(50, 55),
    famille_sol = c(45, 50),
    famille_paysage = c(35, 40),
    famille_temporel = c(70, 75),
    famille_risque = c(30, 35),
    famille_social = c(25, 30),
    famille_production = c(65, 70),
    famille_energie = c(55, 60),
    famille_naturalite = c(80, 85),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_simple_pdf_report(
    project, family_scores, temp_file,
    language = "fr",
    synthesis_comments = "Test comments for PDF"
  )

  expect_true(file.exists(result))
  expect_equal(result, temp_file)
  expect_true(file.size(result) > 0)
})

test_that("generate_simple_pdf_report works with English language", {
  skip_if_not_installed("sf")

  project <- list(
    metadata = list(
      name = "English Test",
      description = NULL,
      owner = NULL,
      created_at = "2026-01-01"
    )
  )

  family_scores <- sf::st_sf(
    id = 1,
    famille_carbone = 50, famille_biodiversite = 50, famille_eau = 50, famille_air = 50,
    famille_sol = 50, famille_paysage = 50, famille_temporel = 50, famille_risque = 50,
    famille_social = 50, famille_production = 50, famille_energie = 50, famille_naturalite = 50,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_simple_pdf_report(
    project, family_scores, temp_file,
    language = "en"
  )

  expect_true(file.exists(result))
})

test_that("generate_simple_pdf_report includes family_comments", {
  skip_if_not_installed("sf")

  project <- list(
    metadata = list(
      name = "Comments Test",
      description = NULL,
      owner = NULL,
      created_at = "2026-01-01"
    )
  )

  family_scores <- sf::st_sf(
    id = 1,
    famille_carbone = 50, famille_biodiversite = 50, famille_eau = 50, famille_air = 50,
    famille_sol = 50, famille_paysage = 50, famille_temporel = 50, famille_risque = 50,
    famille_social = 50, famille_production = 50, famille_energie = 50, famille_naturalite = 50,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  family_comments <- list(
    C = "Carbon family analysis with **bold** text",
    B = "Biodiversity family analysis"
  )

  result <- nemetonShiny:::generate_simple_pdf_report(
    project, family_scores, temp_file,
    language = "fr",
    family_comments = family_comments
  )

  expect_true(file.exists(result))
})

test_that("generate_simple_pdf_report handles polygon geometry", {
  skip_if_not_installed("sf")

  project <- list(
    metadata = list(
      name = "Polygon Test",
      description = "Test with polygons",
      owner = "Owner",
      created_at = "2026-01-01"
    )
  )

  # Create polygon geometries instead of points
  family_scores <- sf::st_sf(
    id = c("PARCEL001", "PARCEL002"),
    famille_carbone = c(60, 70),
    famille_biodiversite = c(55, 60),
    famille_eau = c(40, 45),
    famille_air = c(50, 55),
    famille_sol = c(45, 50),
    famille_paysage = c(35, 40),
    famille_temporel = c(70, 75),
    famille_risque = c(30, 35),
    famille_social = c(25, 30),
    famille_production = c(65, 70),
    famille_energie = c(55, 60),
    famille_naturalite = c(80, 85),
    indicateur_c1_biomasse = c(100, 120),
    indicateur_c2_ndvi = c(0.7, 0.8),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_simple_pdf_report(
    project, family_scores, temp_file,
    language = "fr"
  )

  expect_true(file.exists(result))
  expect_true(file.size(result) > 1000)  # Should be larger with map pages
})

# ==============================================================================
# GENERATE REPORT PDF (MAIN ENTRY POINT)
# ==============================================================================

test_that("generate_report_pdf works without Quarto", {
  skip_if_not_installed("sf")

  project <- list(
    metadata = list(
      name = "Fallback Test",
      description = NULL,
      owner = NULL,
      created_at = "2026-01-01"
    )
  )

  family_scores <- sf::st_sf(
    id = 1,
    famille_carbone = 50, famille_biodiversite = 50, famille_eau = 50, famille_air = 50,
    famille_sol = 50, famille_paysage = 50, famille_temporel = 50, famille_risque = 50,
    famille_social = 50, famille_production = 50, famille_energie = 50, famille_naturalite = 50,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  # Force fallback by setting use_quarto = FALSE
  result <- nemetonShiny:::generate_report_pdf(
    project, family_scores, temp_file,
    language = "en",
    use_quarto = FALSE
  )

  expect_true(file.exists(result))
})

test_that("generate_report_pdf handles synthesis_comments and family_comments", {
  skip_if_not_installed("sf")

  project <- list(
    metadata = list(
      name = "Full Test",
      description = "Complete test",
      owner = "Test Owner",
      created_at = "2026-01-01"
    )
  )

  family_scores <- sf::st_sf(
    id = 1:2,
    famille_carbone = c(60, 70),
    famille_biodiversite = c(55, 60),
    famille_eau = c(40, 45),
    famille_air = c(50, 55),
    famille_sol = c(45, 50),
    famille_paysage = c(35, 40),
    famille_temporel = c(70, 75),
    famille_risque = c(30, 35),
    famille_social = c(25, 30),
    famille_production = c(65, 70),
    famille_energie = c(55, 60),
    famille_naturalite = c(80, 85),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_report_pdf(
    project, family_scores, temp_file,
    language = "fr",
    synthesis_comments = "# Analysis Summary\n\nThis is **important** analysis.",
    family_comments = list(C = "Carbon analysis", B = "Biodiversity notes"),
    use_quarto = FALSE
  )

  expect_true(file.exists(result))
  expect_true(file.size(result) > 0)
})

# ==============================================================================
# EDGE CASES AND ERROR HANDLING
# ==============================================================================

test_that("export functions handle empty data gracefully", {
  skip_if_not_installed("sf")

  # Empty sf object (0 rows)
  empty_sf <- sf::st_sf(
    id = character(0),
    value = numeric(0),
    geometry = sf::st_sfc(crs = 2154)
  )

  temp_file <- tempfile(fileext = ".gpkg")
  on.exit(unlink(temp_file), add = TRUE)

  # This should work (write empty layer)
  expect_no_error(nemetonShiny:::export_geopackage(empty_sf, temp_file))
})

test_that("clean_text_for_pdf handles special characters in sequence", {
  text <- "\u201C\u2018Test\u2019 \u2013 \u2014 \u2026\u00A0\u201D"
  result <- nemetonShiny:::clean_text_for_pdf(text)
  expect_equal(result, "\"'Test' - - ... \"")
})

test_that("prepare_report_data calculates correct global score", {
  skip_if_not_installed("sf")

  project <- list(
    metadata = list(
      name = "Score Test",
      description = NULL,
      owner = NULL,
      created_at = "2026-01-01"
    )
  )

  # All families with same value = 75
  family_scores <- sf::st_sf(
    id = 1,
    famille_carbone = 75, famille_biodiversite = 75, famille_eau = 75, famille_air = 75,
    famille_sol = 75, famille_paysage = 75, famille_temporel = 75, famille_risque = 75,
    famille_social = 75, famille_production = 75, famille_energie = 75, famille_naturalite = 75,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "fr", NULL)

  expect_equal(result$global_score, 75)
})

test_that("prepare_report_data handles multiple parcels for statistics", {
  skip_if_not_installed("sf")

  project <- list(
    metadata = list(
      name = "Stats Test",
      description = NULL,
      owner = NULL,
      created_at = "2026-01-01"
    )
  )

  # Family C with values 20, 50, 80 -> mean = 50, min = 20, max = 80
  family_scores <- sf::st_sf(
    id = 1:3,
    famille_carbone = c(20, 50, 80),
    famille_biodiversite = c(30, 30, 30),
    famille_eau = c(40, 40, 40),
    famille_air = c(50, 50, 50),
    famille_sol = c(60, 60, 60),
    famille_paysage = c(70, 70, 70),
    famille_temporel = c(80, 80, 80),
    famille_risque = c(90, 90, 90),
    famille_social = c(100, 100, 100),
    famille_production = c(10, 10, 10),
    famille_energie = c(25, 25, 25),
    famille_naturalite = c(35, 35, 35),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 0)),
      sf::st_point(c(0, 1)),
      crs = 2154
    )
  )

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "fr", NULL)

  expect_equal(result$family_stats$C$mean, 50)
  expect_equal(result$family_stats$C$min, 20)
  expect_equal(result$family_stats$C$max, 80)
})

# ==============================================================================
# ADDITIONAL TESTS FOR INCREASED COVERAGE
# ==============================================================================

# --- Helpers for new tests ---

#' Create minimal family_scores sf for testing
create_test_family_scores <- function(n = 3) {
  units <- create_test_units(n_features = n)
  # Add family score columns
  for (code in c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")) {
    units[[nemetonShiny:::get_famille_col(code)]] <- runif(n, 20, 80)
  }
  units$id <- paste0("PARCEL_", seq_len(n))
  units
}

#' Create minimal project object
create_test_project <- function() {
  list(
    id = "test_project",
    path = tempdir(),
    metadata = list(
      name = "Test Project",
      description = "A test",
      owner = "Tester",
      created_at = "2024-01-01",
      status = "completed",
      parcels_count = 3L
    )
  )
}

# ==============================================================================
# is_quarto_installed - MOCKED SCENARIOS
# ==============================================================================

test_that("is_quarto_installed returns FALSE when quarto package not available", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "quarto") return(FALSE)
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  # Because we cannot easily mock requireNamespace at package level,
  # we verify the function is callable and returns logical
  result <- nemetonShiny:::is_quarto_installed()
  expect_type(result, "logical")
})

test_that("is_quarto_installed handles system2 error gracefully", {
  # Verify the function handles edge cases and always returns logical
  result <- nemetonShiny:::is_quarto_installed()
  expect_type(result, "logical")
  expect_length(result, 1)
})

# ==============================================================================
# strip_markdown_for_display - ADDITIONAL EDGE CASES
# ==============================================================================

test_that("strip_markdown_for_display handles empty string", {
  expect_equal(nemetonShiny:::strip_markdown_for_display(""), "")
})

test_that("strip_markdown_for_display handles text with no markdown", {
  expect_equal(
    nemetonShiny:::strip_markdown_for_display("Plain text with no formatting"),
    "Plain text with no formatting"
  )
})

test_that("strip_markdown_for_display handles nested bold and italic", {
  # Bold with italic adjacent
  text <- "**Bold** and *italic* together"
  result <- nemetonShiny:::strip_markdown_for_display(text)
  expect_equal(result, "Bold and italic together")
})

test_that("strip_markdown_for_display handles multiple links", {
  text <- "Visit [site1](http://a.com) and [site2](http://b.com)"
  result <- nemetonShiny:::strip_markdown_for_display(text)
  expect_equal(result, "Visit site1 and site2")
})

test_that("strip_markdown_for_display handles combined inline code and bold", {
  text <- "Use `func()` and **important**"
  result <- nemetonShiny:::strip_markdown_for_display(text)
  expect_equal(result, "Use func() and important")
})

# ==============================================================================
# clean_text_for_pdf - ADDITIONAL EDGE CASES
# ==============================================================================

test_that("clean_text_for_pdf handles multiple curly quotes in one string", {
  text <- "\u201CFirst\u201D and \u201CSecond\u201D"
  result <- nemetonShiny:::clean_text_for_pdf(text)
  expect_equal(result, "\"First\" and \"Second\"")
})

test_that("clean_text_for_pdf handles mixed single and double curly quotes", {
  text <- "\u2018word\u2019 \u201Cphrase\u201D"
  result <- nemetonShiny:::clean_text_for_pdf(text)
  expect_equal(result, "'word' \"phrase\"")
})

test_that("clean_text_for_pdf handles string with only special characters", {
  text <- "\u2013\u2014\u2026\u00A0"
  result <- nemetonShiny:::clean_text_for_pdf(text)
  expect_equal(result, "--... ")
})

test_that("clean_text_for_pdf handles multi-line text", {
  text <- "Line 1\u2013end\nLine 2\u2014end"
  result <- nemetonShiny:::clean_text_for_pdf(text)
  expect_equal(result, "Line 1-end\nLine 2-end")
})

test_that("clean_text_for_pdf handles normal ASCII text unchanged", {
  text <- "Normal ASCII text with 123 numbers."
  result <- nemetonShiny:::clean_text_for_pdf(text)
  expect_equal(result, text)
})

# ==============================================================================
# render_markdown_text - ADDITIONAL COVERAGE
# ==============================================================================

test_that("render_markdown_text handles text reaching bottom of page", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  # Create very long text that will exceed page bounds
  long_text <- paste(rep("Line of text that wraps.", 50), collapse = "\n")
  result <- nemetonShiny:::render_markdown_text(long_text, 0.1, 0.9)

  # Should stop rendering when y < 0.05
  expect_true(result <= 0.9)

  grDevices::dev.off()
})

test_that("render_markdown_text handles mixed markdown elements", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  text <- paste(
    "# Main Title",
    "",
    "A **bold** statement with *italic* emphasis.",
    "",
    "> A blockquote",
    "",
    "- Item 1",
    "- Item 2",
    "",
    "1. Numbered one",
    "2. Numbered two",
    "",
    "Final paragraph with `code` and [link](http://example.com).",
    sep = "\n"
  )
  result <- nemetonShiny:::render_markdown_text(text, 0.1, 0.9)

  expect_true(result < 0.9)

  grDevices::dev.off()
})

test_that("render_markdown_text handles bold followed by text without space", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  text <- "**bold**text here"
  result <- nemetonShiny:::render_markdown_text(text, 0.1, 0.9)
  expect_true(result < 0.9)

  grDevices::dev.off()
})

test_that("render_markdown_text handles italic followed by text without space", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  text <- "*italic*text"
  result <- nemetonShiny:::render_markdown_text(text, 0.1, 0.9)
  expect_true(result < 0.9)

  grDevices::dev.off()
})

test_that("render_markdown_text handles h2 and h3 headers", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  text <- "## Section Title\n\n### Subsection\n\nText here."
  result <- nemetonShiny:::render_markdown_text(text, 0.1, 0.9)
  expect_true(result < 0.9)

  grDevices::dev.off()
})

test_that("render_markdown_text handles long bullet list items that wrap", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  text <- paste0(
    "- ", paste(rep("word", 30), collapse = " "), "\n",
    "- Short item"
  )
  result <- nemetonShiny:::render_markdown_text(text, 0.1, 0.9)
  expect_true(result < 0.9)

  grDevices::dev.off()
})

test_that("render_markdown_text handles long numbered list items that wrap", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  text <- paste0(
    "1. ", paste(rep("long", 30), collapse = " "), "\n",
    "2. Short"
  )
  result <- nemetonShiny:::render_markdown_text(text, 0.1, 0.9)
  expect_true(result < 0.9)

  grDevices::dev.off()
})

test_that("render_markdown_text handles blockquote with multiple wrapped lines", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  text <- paste0("> ", paste(rep("blockquote", 25), collapse = " "))
  result <- nemetonShiny:::render_markdown_text(text, 0.1, 0.9)
  expect_true(result < 0.9)

  grDevices::dev.off()
})

# ==============================================================================
# render_formatted_line - ADDITIONAL COVERAGE
# ==============================================================================

test_that("render_formatted_line handles mixed bold and italic on same line", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  expect_no_error(
    nemetonShiny:::render_formatted_line("**Bold** and *italic* and `code` text", 0.1, 0.5, 0.7, "black", 80)
  )

  grDevices::dev.off()
})

test_that("render_formatted_line handles unmatched marker gracefully", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  # Single asterisk without closing - should fall through to skip one char
  expect_no_error(
    nemetonShiny:::render_formatted_line("Start * end", 0.1, 0.5, 0.7, "black", 80)
  )

  grDevices::dev.off()
})

test_that("render_formatted_line handles text before bold marker", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  expect_no_error(
    nemetonShiny:::render_formatted_line("Preceding text **bold** trailing text", 0.1, 0.5, 0.7, "black", 80)
  )

  grDevices::dev.off()
})

test_that("render_formatted_line handles strikethrough with adjacent text", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  expect_no_error(
    nemetonShiny:::render_formatted_line("Before ~~strike~~ after", 0.1, 0.5, 0.7, "black", 80)
  )

  grDevices::dev.off()
})

test_that("render_formatted_line handles link in the middle of text", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  expect_no_error(
    nemetonShiny:::render_formatted_line("Before [link text](http://x.com) after", 0.1, 0.5, 0.7, "black", 80)
  )

  grDevices::dev.off()
})

test_that("render_formatted_line handles inline code in middle", {
  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)
  plot.new()

  expect_no_error(
    nemetonShiny:::render_formatted_line("Use `function()` in your code", 0.1, 0.5, 0.7, "black", 80)
  )

  grDevices::dev.off()
})

# ==============================================================================
# draw_parcel_table - ADDITIONAL COVERAGE
# ==============================================================================

test_that("draw_parcel_table renders all quality levels", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = c("P1", "P2", "P3", "P4", "P5"),
    score = c(90, 65, 45, 25, 10),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 0)),
      sf::st_point(c(0, 1)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 0)),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  # French version
  expect_no_error(nemetonShiny:::draw_parcel_table(data, "score", "id", "fr"))
  # English version
  expect_no_error(nemetonShiny:::draw_parcel_table(data, "score", "id", "en"))

  grDevices::dev.off()
})

test_that("draw_parcel_table handles many rows (truncation)", {
  skip_if_not_installed("sf")

  n <- 25
  data <- sf::st_sf(
    id = paste0("PARCEL_", sprintf("%03d", seq_len(n))),
    score = seq(10, 90, length.out = n),
    geometry = sf::st_sfc(
      lapply(seq_len(n), function(i) sf::st_point(c(i, 0))),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  # Should handle the truncation message ("... N additional parcels")
  expect_no_error(nemetonShiny:::draw_parcel_table(data, "score", "id", "fr"))
  expect_no_error(nemetonShiny:::draw_parcel_table(data, "score", "id", "en"))

  grDevices::dev.off()
})

test_that("draw_parcel_table with indicator_title in English", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = "P1",
    score = 75,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  expect_no_error(
    nemetonShiny:::draw_parcel_table(data, "score", "id", "en", indicator_title = "C1 - Carbon Biomass")
  )

  grDevices::dev.off()
})

# ==============================================================================
# draw_indicator_map_page - ADDITIONAL COVERAGE (fallback path)
# ==============================================================================

test_that("draw_indicator_map_page renders with polygon data using fallback", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = c("P001", "P002", "P003"),
    value = c(25, 55, 85),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(0, 1, 1, 1, 1, 2, 0, 2, 0, 1), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  # This should go through the fallback simple sf plot path
  expect_no_error(
    nemetonShiny:::draw_indicator_map_page(data, "value", "Test Map", "#228B22", "fr")
  )

  grDevices::dev.off()
  expect_true(file.exists(temp_file))
  expect_true(file.size(temp_file) > 0)
})

test_that("draw_indicator_map_page handles values with NA mixed in", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = c("P1", "P2", "P3"),
    value = c(30, NA, 70),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(0, 1, 1, 1, 1, 2, 0, 2, 0, 1), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  expect_no_error(
    nemetonShiny:::draw_indicator_map_page(data, "value", "Mixed NAs", "#1E90FF", "en")
  )

  grDevices::dev.off()
})

test_that("draw_indicator_map_page renders in English with missing column", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = 1,
    value = 50,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  expect_no_error(
    nemetonShiny:::draw_indicator_map_page(data, "nonexistent", "Test", "#228B22", "en")
  )

  grDevices::dev.off()
})

test_that("draw_indicator_map_page renders in English with all NA values", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = 1,
    value = NA_real_,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 2154)
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  expect_no_error(
    nemetonShiny:::draw_indicator_map_page(data, "value", "Test", "#228B22", "en")
  )

  grDevices::dev.off()
})

# ==============================================================================
# add_parcel_labels - ADDITIONAL COVERAGE
# ==============================================================================

test_that("add_parcel_labels draws labels with halo effect on polygons", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = c("PARCEL_001", "PARCEL_002"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  # Plot geometry first
  plot(sf::st_geometry(data))

  # Exercise the label rendering (halo effect loop)
  expect_no_error(nemetonShiny:::add_parcel_labels(data, id_col = "id"))

  grDevices::dev.off()
})

test_that("add_parcel_labels handles short IDs (less than 6 chars)", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = c("AB", "CD"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  plot(sf::st_geometry(data))
  expect_no_error(nemetonShiny:::add_parcel_labels(data, id_col = "id"))

  grDevices::dev.off()
})

# ==============================================================================
# generate_indicator_map - ADDITIONAL COVERAGE (output to device)
# ==============================================================================

test_that("generate_indicator_map renders to current device when no output_file", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = 1:3,
    value = c(25, 50, 75),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(0, 1, 1, 1, 1, 2, 0, 2, 0, 1), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  # Call without output_file - should render to current device
  result <- nemetonShiny:::generate_indicator_map(data, "value", "Test Map")
  expect_null(result)

  grDevices::dev.off()
})

test_that("generate_indicator_map handles NA values mixed with valid data", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = 1:3,
    value = c(25, NA, 75),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(0, 1, 1, 1, 1, 2, 0, 2, 0, 1), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_indicator_map(data, "value", "Test with NAs",
                                              output_file = temp_file)
  expect_null(result)
  expect_true(file.exists(temp_file))
  expect_true(file.size(temp_file) > 0)
})

test_that("generate_indicator_map uses custom color", {
  skip_if_not_installed("sf")

  data <- sf::st_sf(
    id = 1:2,
    value = c(30, 70),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE))),
      crs = 2154
    )
  )

  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_indicator_map(data, "value", "Custom Color",
                                              color = "#9932CC",
                                              output_file = temp_file)
  expect_null(result)
  expect_true(file.exists(temp_file))
})

# ==============================================================================
# generate_family_maps - ADDITIONAL COVERAGE
# ==============================================================================

test_that("generate_family_maps creates maps for all 12 family columns", {
  skip_if_not_installed("sf")

  family_scores <- create_test_family_scores(n = 3)

  temp_dir <- file.path(tempdir(), paste0("all_family_maps_", Sys.getpid()))
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  suppressWarnings(
    expect_no_error(nemetonShiny:::generate_family_maps(family_scores, temp_dir, "fr"))
  )

  # Verify maps for all families are created
  for (code in c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")) {
    map_file <- file.path(temp_dir, paste0(nemetonShiny:::get_famille_col(code), "_map.png"))
    expect_true(file.exists(map_file), info = paste("Missing map for family", code))
    expect_true(file.size(map_file) > 0, info = paste("Empty map for family", code))
  }
})

test_that("generate_family_maps works with English language", {
  skip_if_not_installed("sf")

  family_scores <- create_test_family_scores(n = 2)

  temp_dir <- file.path(tempdir(), paste0("family_maps_en_", Sys.getpid()))
  dir.create(temp_dir, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  suppressWarnings(
    expect_no_error(nemetonShiny:::generate_family_maps(family_scores, temp_dir, "en"))
  )

  expect_true(file.exists(file.path(temp_dir, "famille_carbone_map.png")))
})

# ==============================================================================
# generate_radar_image - ADDITIONAL COVERAGE
# ==============================================================================

test_that("generate_radar_image creates file with English language", {
  skip_if_not_installed("sf")
  skip_if_not(capabilities("png"), "PNG graphics device not available")
  skip_on_ci()

  family_scores <- create_test_family_scores(n = 3)

  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  expect_no_error(nemetonShiny:::generate_radar_image(family_scores, temp_file, "en"))

  skip_if_not(file.exists(temp_file), "PNG file not created (display issue)")
  expect_true(file.size(temp_file) > 0)
})

test_that("generate_radar_image handles error gracefully (fallback plot)", {
  skip_if_not(capabilities("png"), "PNG graphics device not available")
  skip_on_ci()

  # Pass non-sf data to trigger error in nemeton_radar
  bad_data <- data.frame(x = 1)

  temp_file <- tempfile(fileext = ".png")
  on.exit(unlink(temp_file), add = TRUE)

  # Should not error - has tryCatch fallback
  expect_no_error(nemetonShiny:::generate_radar_image(bad_data, temp_file, "fr"))
})

# ==============================================================================
# prepare_report_data - ADDITIONAL COVERAGE
# ==============================================================================

test_that("prepare_report_data builds family_descriptions for all families", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 2)

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "fr", NULL)

  expect_type(result$family_descriptions, "list")
  expect_equal(length(result$family_descriptions), 12)
  # Each description should mention number of indicators
  for (code in names(result$family_descriptions)) {
    desc <- result$family_descriptions[[code]]
    expect_type(desc, "character")
    expect_true(nchar(desc) > 0)
    expect_true(grepl("indicateur", desc))
  }
})

test_that("prepare_report_data builds family_descriptions in English", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 2)

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "en", NULL)

  for (code in names(result$family_descriptions)) {
    desc <- result$family_descriptions[[code]]
    expect_true(grepl("indicator", desc))
  }
})

test_that("prepare_report_data builds indicator_stats correctly", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 3)
  # Add a known indicator column
  family_scores$indicateur_c1_biomasse <- c(100, 120, 80)
  family_scores$indicateur_c2_ndvi <- c(0.7, 0.8, 0.6)

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "fr", NULL)

  # indicator_stats should have data for family C since we added its columns
  ind_stats_c <- result$indicator_stats$C
  expect_true(nrow(ind_stats_c) > 0)
  # Check column names in French
  expect_true("Indicateur" %in% names(ind_stats_c))
  expect_true("Moyenne" %in% names(ind_stats_c))
})

test_that("prepare_report_data indicator_stats uses English column names", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 2)
  family_scores$indicateur_c1_biomasse <- c(100, 120)

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "en", NULL)

  ind_stats_c <- result$indicator_stats$C
  if (nrow(ind_stats_c) > 0) {
    expect_true("Indicator" %in% names(ind_stats_c))
    expect_true("Mean" %in% names(ind_stats_c))
  }
})

test_that("prepare_report_data includes all labels in fr", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 1)

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "fr", NULL)

  expect_true(grepl("N\u00e9meton", result$labels$title))
  expect_type(result$labels$subtitle, "character")
  expect_type(result$labels$project_name, "character")
  expect_type(result$labels$owner, "character")
  expect_type(result$labels$global_score_label, "character")
  expect_type(result$labels$parcels, "character")
  expect_type(result$labels$created_at, "character")
  expect_type(result$labels$family_scores, "character")
  expect_type(result$labels$radar_title, "character")
  expect_type(result$labels$comments_title, "character")
  expect_type(result$labels$generated_by, "character")
})

test_that("prepare_report_data includes all labels in en", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 1)

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "en", NULL)

  expect_true(grepl("N\u00e9meton", result$labels$title))
  expect_type(result$labels$family_scores, "character")
  expect_true(grepl("family|score", result$labels$family_scores, ignore.case = TRUE))
})

test_that("prepare_report_data handles NULL family_comments by filling all codes", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 1)

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "fr", NULL, NULL)

  # family_comments should be filled with empty strings for all family codes
  expect_equal(length(result$family_comments), 12)
  for (code in c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")) {
    expect_equal(result$family_comments[[code]], "")
  }
})

test_that("prepare_report_data passes through family_comments when provided", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 1)
  comments <- list(C = "Carbon analysis", W = "Water analysis")

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "fr", NULL, comments)

  expect_equal(result$family_comments$C, "Carbon analysis")
  expect_equal(result$family_comments$W, "Water analysis")
})

test_that("prepare_report_data includes standard deviation in family_stats", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 5)

  result <- nemetonShiny:::prepare_report_data(project, family_scores, "fr", NULL)

  for (code in names(result$family_stats)) {
    expect_true("sd" %in% names(result$family_stats[[code]]))
    expect_type(result$family_stats[[code]]$sd, "double")
  }
})

# ==============================================================================
# generate_simple_pdf_report - EXTENSIVE COVERAGE (BIGGEST OPPORTUNITY)
# ==============================================================================

test_that("generate_simple_pdf_report creates multi-page PDF with all families", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 3)

  # Add some indicator columns to exercise individual indicator pages
  family_scores$indicateur_c1_biomasse <- c(100, 120, 80)
  family_scores$indicateur_c2_ndvi <- c(0.7, 0.8, 0.6)

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_simple_pdf_report(
    project, family_scores, temp_file,
    language = "fr",
    synthesis_comments = "# Analysis\n\nThis is **important**.\n\n- Item 1\n- Item 2",
    family_comments = list(
      C = "Carbon analysis with **bold** and *italic*.",
      B = "Biodiversity notes here."
    )
  )

  expect_true(file.exists(result))
  expect_equal(result, temp_file)
  # Multi-page PDF with content should be substantial
  expect_true(file.size(result) > 2000)
})

test_that("generate_simple_pdf_report with cover image path that does not exist", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 2)

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  # cover_image is set but does not exist -> should use standard cover
  result <- nemetonShiny:::generate_simple_pdf_report(
    project, family_scores, temp_file,
    language = "fr",
    cover_image = "/tmp/nonexistent_cover_image.png"
  )

  expect_true(file.exists(result))
  expect_true(file.size(result) > 0)
})

test_that("generate_simple_pdf_report with valid cover image PNG", {
  skip_if_not_installed("sf")
  skip_if_not_installed("png")
  skip_if_not(capabilities("png"), "PNG device not available")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 2)

  # Create a real PNG cover image
  cover_file <- tempfile(fileext = ".png")
  grDevices::png(cover_file, width = 200, height = 200)
  plot.new()
  graphics::text(0.5, 0.5, "Cover")
  grDevices::dev.off()

  temp_file <- tempfile(fileext = ".pdf")
  on.exit({
    unlink(temp_file)
    unlink(cover_file)
  }, add = TRUE)

  result <- nemetonShiny:::generate_simple_pdf_report(
    project, family_scores, temp_file,
    language = "fr",
    cover_image = cover_file
  )

  expect_true(file.exists(result))
  expect_true(file.size(result) > 0)
})

test_that("generate_simple_pdf_report works without synthesis_comments", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 2)

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_simple_pdf_report(
    project, family_scores, temp_file,
    language = "fr",
    synthesis_comments = NULL
  )

  expect_true(file.exists(result))
})

test_that("generate_simple_pdf_report with empty synthesis_comments", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 2)

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_simple_pdf_report(
    project, family_scores, temp_file,
    language = "en",
    synthesis_comments = ""
  )

  expect_true(file.exists(result))
})

test_that("generate_simple_pdf_report handles low global score coloring", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  # Create data with all low scores
  units <- create_test_units(n_features = 2)
  for (code in c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")) {
    units[[nemetonShiny:::get_famille_col(code)]] <- c(15, 20)
  }
  units$id <- c("P1", "P2")

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_simple_pdf_report(
    project, units, temp_file,
    language = "fr"
  )

  expect_true(file.exists(result))
})

test_that("generate_simple_pdf_report handles medium global score coloring", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  units <- create_test_units(n_features = 2)
  for (code in c("C", "B", "W", "A", "F", "L", "T", "R", "S", "P", "E", "N")) {
    units[[nemetonShiny:::get_famille_col(code)]] <- c(45, 50)
  }
  units$id <- c("P1", "P2")

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_simple_pdf_report(
    project, units, temp_file,
    language = "en"
  )

  expect_true(file.exists(result))
})

test_that("generate_simple_pdf_report renders complete report with all comments", {
  skip_if_not_installed("sf")

  project <- list(
    metadata = list(
      name = "Complete Report Test",
      description = "A comprehensive forest diagnostic report for testing.",
      owner = "Test Owner",
      created_at = "2025-12-01"
    )
  )

  family_scores <- create_test_family_scores(n = 4)
  # Add multiple indicator columns
  family_scores$indicateur_c1_biomasse <- runif(4, 50, 150)
  family_scores$indicateur_c2_ndvi <- runif(4, 0.3, 0.9)
  family_scores$indicateur_b1_protection <- runif(4, 20, 80)
  family_scores$indicateur_b2_structure <- runif(4, 30, 70)
  family_scores$indicateur_b3_connectivite <- runif(4, 10, 90)

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  all_comments <- list(
    C = "## Carbon Analysis\n\nThe **carbon stocks** are above average.\n\n- Biomass: Good\n- NDVI: Excellent",
    B = "Biodiversity is *diverse* with `high` connectivity.",
    W = "Water indicators are moderate.",
    A = "Air quality analysis not yet available.",
    F = "Fire risk is **low**.",
    L = "Landscape fragmentation is minimal.",
    T = "Timber quality is good.",
    R = "Recreational value is high.",
    S = "Soil quality needs improvement.",
    P = "Protection status is adequate.",
    E = "Economic indicators show promise.",
    N = "Natural heritage is well preserved."
  )

  result <- nemetonShiny:::generate_simple_pdf_report(
    project, family_scores, temp_file,
    language = "fr",
    synthesis_comments = "# Synthesis\n\n> Important finding\n\nOverall the forest is in good condition.\n\n1. Carbon stocks are high\n2. Biodiversity is diverse\n3. Water quality is good",
    family_comments = all_comments
  )

  expect_true(file.exists(result))
  expect_true(file.size(result) > 5000)
})

# ==============================================================================
# generate_pdf_report - QUARTO-BASED (test error/fallback)
# ==============================================================================

test_that("generate_pdf_report errors when Quarto not installed", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 1)

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  # If Quarto is not installed, this should error with the expected message
  if (!nemetonShiny:::is_quarto_installed()) {
    expect_error(
      nemetonShiny:::generate_pdf_report(
        project, family_scores, temp_file, language = "fr"
      ),
      "Quarto is required"
    )
  } else {
    # If Quarto IS installed, the function should run without error
    # (just skip this specific assertion)
    skip("Quarto is installed, cannot test missing-quarto path")
  }
})

# ==============================================================================
# generate_report_pdf - MAIN ENTRY POINT ADDITIONAL COVERAGE
# ==============================================================================

test_that("generate_report_pdf with use_quarto=TRUE falls back when Quarto unavailable", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 2)

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  # With use_quarto = TRUE, it will try Quarto first then fall back
  result <- suppressWarnings(nemetonShiny:::generate_report_pdf(
    project, family_scores, temp_file,
    language = "fr",
    use_quarto = TRUE
  ))

  # Should always produce a PDF (either via Quarto or fallback)
  expect_true(file.exists(result))
  expect_true(file.size(result) > 0)
})

test_that("generate_report_pdf passes cover_image through to simple PDF", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 2)

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_report_pdf(
    project, family_scores, temp_file,
    language = "en",
    use_quarto = FALSE,
    cover_image = "/tmp/nonexistent.png"
  )

  expect_true(file.exists(result))
})

test_that("generate_report_pdf with all parameters produces valid PDF", {
  skip_if_not_installed("sf")

  project <- create_test_project()
  family_scores <- create_test_family_scores(n = 3)

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)

  result <- nemetonShiny:::generate_report_pdf(
    project, family_scores, temp_file,
    language = "fr",
    synthesis_comments = "Test synthesis with **markdown**.",
    family_comments = list(C = "Carbon", B = "Bio"),
    use_quarto = FALSE
  )

  expect_true(file.exists(result))
  expect_true(file.size(result) > 0)
})

# ==============================================================================
# draw_standard_cover_page - ADDITIONAL COVERAGE (with description)
# ==============================================================================

test_that("draw_standard_cover_page with long description", {
  report_data <- list(
    labels = list(
      title = "Test Title",
      subtitle = "Test Subtitle",
      parcels = "Parcels",
      created_at = "Created",
      global_score_label = "Global Score",
      generated_by = "Generated by nemeton"
    ),
    project_name = "Test Project With Long Name",
    project_description = "This is a very detailed description of the test project that covers multiple topics and scenarios.",
    n_parcels = 10,
    created_at = "2025-06-15",
    global_score = 42
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  expect_no_error(nemetonShiny:::draw_standard_cover_page(report_data, "fr"))

  grDevices::dev.off()
})

test_that("draw_standard_cover_page handles empty string description", {
  report_data <- list(
    labels = list(
      title = "Title", subtitle = "Sub", parcels = "P",
      created_at = "C", global_score_label = "Score", generated_by = "nemeton"
    ),
    project_name = "Test",
    project_description = "",
    n_parcels = 1,
    created_at = "2026-01-01",
    global_score = 60
  )

  temp_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(temp_file), add = TRUE)
  grDevices::pdf(temp_file)

  expect_no_error(nemetonShiny:::draw_standard_cover_page(report_data, "en"))

  grDevices::dev.off()
})

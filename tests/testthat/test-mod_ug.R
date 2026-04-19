# Tests for mod_ug.R
# UG (Management Units) Shiny module

# ==============================================================================
# UI tests
# ==============================================================================

test_that("mod_ug_ui returns valid Shiny UI", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  ui <- nemetonshiny:::mod_ug_ui("ug")
  expect_true(
    inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag")
  )
})

test_that("mod_ug_ui contains expected elements", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  ui_html <- as.character(nemetonshiny:::mod_ug_ui("ug"))

  # Check for key UI elements
  expect_true(grepl("ug-btn_merge", ui_html))
  expect_true(grepl("ug-btn_split", ui_html))
  expect_true(grepl("ug-btn_rename", ui_html))
  expect_true(grepl("ug-sel_groupe", ui_html))
  expect_true(grepl("ug-ug_table", ui_html))

  # Boucle 1: leaflet map and create-from-map button
  expect_true(grepl("ug-ug_map", ui_html))
  expect_true(grepl("ug-btn_create_from_map", ui_html))
})

test_that("mod_ug_ui works in English", {
  withr::local_options(nemeton.app_options = list(language = "en"))
  ui <- nemetonshiny:::mod_ug_ui("ug")
  ui_html <- as.character(ui)

  expect_true(grepl("Management Units|Merge|Split", ui_html))
})


# ==============================================================================
# Integration with domain_ug tests
# ==============================================================================

test_that("GROUPES_AMENAGEMENT is defined", {
  groupes <- nemetonshiny:::GROUPES_AMENAGEMENT
  expect_type(groupes, "character")
  expect_true(length(groupes) >= 8)
  expect_true("TSF" %in% groupes)
  expect_true("HSN" %in% groupes)
  expect_true("REGT" %in% groupes)
})

test_that("GROUPE_COLORS has a color for each known groupe", {
  colors <- nemetonshiny:::GROUPE_COLORS
  groupes <- nemetonshiny:::GROUPES_AMENAGEMENT
  expect_true(all(groupes %in% names(colors)))
  # All values should be valid hex colors
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
})

test_that("ug_color returns groupe color when available", {
  color_tsf <- nemetonshiny:::ug_color("TSF", 1L)
  expect_equal(color_tsf, nemetonshiny:::GROUPE_COLORS[["TSF"]])

  color_hsn <- nemetonshiny:::ug_color("HSN", 1L)
  expect_equal(color_hsn, nemetonshiny:::GROUPE_COLORS[["HSN"]])
})

test_that("ug_color returns palette color when no groupe", {
  color_na <- nemetonshiny:::ug_color(NA_character_, 1L)
  expect_equal(color_na, nemetonshiny:::UG_PALETTE[1])

  color_empty <- nemetonshiny:::ug_color("", 3L)
  expect_equal(color_empty, nemetonshiny:::UG_PALETTE[3])
})

test_that("ug_color wraps palette index correctly", {
  n <- length(nemetonshiny:::UG_PALETTE)
  # Index beyond palette length should wrap
  color <- nemetonshiny:::ug_color(NA_character_, n + 1L)
  expect_equal(color, nemetonshiny:::UG_PALETTE[1])
})

test_that("has_ug_data works correctly", {
  # No UG data
  projet <- list(parcels = data.frame(id = "p1"))
  expect_false(nemetonshiny:::has_ug_data(projet))

  # With UG data
  p1 <- sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)))
  projet <- list(
    parcels = sf::st_sf(
      id = "p1", contenance = 10000,
      geometry = sf::st_sfc(p1, crs = 4326)
    )
  )
  projet <- nemetonshiny:::ug_init_default(projet)
  expect_true(nemetonshiny:::has_ug_data(projet))
})


# ==============================================================================
# Translation tests for UG keys
# ==============================================================================

test_that("UG translation keys exist in French", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  i18n <- nemetonshiny:::get_i18n("fr")

  ug_keys <- c(
    "tab_ug", "ug_title", "ug_merge", "ug_split", "ug_rename",
    "ug_group", "ug_composition", "ug_no_data",
    "ug_select_hint", "ug_select_one",
    "ug_map_tab", "ug_table_tab", "ug_map_click_hint",
    "ug_create_from_map", "ug_create_btn", "ug_clear_selection"
  )

  for (key in ug_keys) {
    translated <- i18n$t(key)
    expect_true(
      nchar(translated) > 0 && translated != key,
      info = paste("Missing FR translation for:", key)
    )
  }
})

test_that("UG translation keys exist in English", {
  withr::local_options(nemeton.app_options = list(language = "en"))
  i18n <- nemetonshiny:::get_i18n("en")

  ug_keys <- c(
    "tab_ug", "ug_title", "ug_merge", "ug_split", "ug_rename",
    "ug_group", "ug_composition", "ug_no_data"
  )

  for (key in ug_keys) {
    translated <- i18n$t(key)
    expect_true(
      nchar(translated) > 0 && translated != key,
      info = paste("Missing EN translation for:", key)
    )
  }
})

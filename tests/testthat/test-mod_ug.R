# Tests for mod_ug.R
# UG (Management Units) Shiny module

# ==============================================================================
# UI tests
# ==============================================================================

test_that("mod_ug_ui returns valid Shiny UI", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  ui <- nemetonShiny:::mod_ug_ui("ug")
  expect_true(
    inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag")
  )
})

test_that("mod_ug_ui contains expected elements", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  ui_html <- as.character(nemetonShiny:::mod_ug_ui("ug"))

  # Check for key UI elements
  expect_true(grepl("ug-btn_merge", ui_html))
  expect_true(grepl("ug-btn_split", ui_html))
  expect_true(grepl("ug-btn_rename", ui_html))
  expect_true(grepl("ug-btn_recompute", ui_html))
  expect_true(grepl("ug-sel_groupe", ui_html))
  expect_true(grepl("ug-ug_table", ui_html))
})

test_that("mod_ug_ui works in English", {
  withr::local_options(nemeton.app_options = list(language = "en"))
  ui <- nemetonShiny:::mod_ug_ui("ug")
  ui_html <- as.character(ui)

  expect_true(grepl("Management Units|Merge|Split", ui_html))
})


# ==============================================================================
# Integration with domain_ug tests
# ==============================================================================

test_that("GROUPES_AMENAGEMENT is defined", {
  groupes <- nemetonShiny:::GROUPES_AMENAGEMENT
  expect_type(groupes, "character")
  expect_true(length(groupes) >= 8)
  expect_true("TSF" %in% groupes)
  expect_true("HSN" %in% groupes)
  expect_true("REGT" %in% groupes)
})

test_that("has_ug_data works correctly", {
  # No UG data
  projet <- list(parcels = data.frame(id = "p1"))
  expect_false(nemetonShiny:::has_ug_data(projet))

  # With UG data
  p1 <- sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)))
  projet <- list(
    parcels = sf::st_sf(
      id = "p1", contenance = 10000,
      geometry = sf::st_sfc(p1, crs = 4326)
    )
  )
  projet <- nemetonShiny:::ug_init_default(projet)
  expect_true(nemetonShiny:::has_ug_data(projet))
})


# ==============================================================================
# Translation tests for UG keys
# ==============================================================================

test_that("UG translation keys exist in French", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  i18n <- nemetonShiny:::get_i18n("fr")

  ug_keys <- c(
    "tab_ug", "ug_title", "ug_merge", "ug_split", "ug_rename",
    "ug_group", "ug_composition", "ug_recompute", "ug_no_data",
    "ug_select_hint", "ug_select_one"
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
  i18n <- nemetonShiny:::get_i18n("en")

  ug_keys <- c(
    "tab_ug", "ug_title", "ug_merge", "ug_split", "ug_rename",
    "ug_group", "ug_composition", "ug_recompute", "ug_no_data"
  )

  for (key in ug_keys) {
    translated <- i18n$t(key)
    expect_true(
      nchar(translated) > 0 && translated != key,
      info = paste("Missing EN translation for:", key)
    )
  }
})

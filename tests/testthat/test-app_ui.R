# Tests for nemetonApp UI
# R/app_ui.R - Main UI function and external resource loading

# ==============================================================================
# Tests for app_add_external_resources()
# ==============================================================================

test_that("app_add_external_resources returns a valid Shiny tagList", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemeton:::app_add_external_resources()

      # Should return a tagList (which is a list of shiny tags)
      expect_true(inherits(result, "shiny.tag.list") || is.list(result))
    }
  )
})

test_that("app_add_external_resources contains CSS link tag", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemeton:::app_add_external_resources()

      # Use renderTags to get full HTML including head content
      rendered <- htmltools::renderTags(result)
      full_html <- paste(rendered$head, rendered$html)

      # Should contain a link tag for CSS
      expect_true(
        grepl("custom\\.min\\.css", full_html),
        info = "Expected CSS link tag for custom.min.css"
      )
    }
  )
})

test_that("app_add_external_resources contains JS script tag", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemeton:::app_add_external_resources()

      # Use renderTags to get full HTML including head content
      rendered <- htmltools::renderTags(result)
      full_html <- paste(rendered$head, rendered$html)

      # Should contain a script tag for JS
      expect_true(
        grepl("custom\\.min\\.js", full_html),
        info = "Expected JS script tag for custom.min.js"
      )
    }
  )
})

test_that("app_add_external_resources contains viewport meta tag", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemeton:::app_add_external_resources()

      rendered <- htmltools::renderTags(result)
      full_html <- paste(rendered$head, rendered$html)

      # Should contain viewport meta tag
      expect_true(
        grepl("viewport", full_html),
        info = "Expected viewport meta tag"
      )
    }
  )
})

test_that("app_add_external_resources contains favicon link", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemeton:::app_add_external_resources()

      rendered <- htmltools::renderTags(result)
      full_html <- paste(rendered$head, rendered$html)

      # Should contain favicon link
      expect_true(
        grepl("logo\\.svg", full_html),
        info = "Expected favicon link tag"
      )
    }
  )
})

test_that("app_add_external_resources contains theme-color meta tag", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemeton:::app_add_external_resources()

      rendered <- htmltools::renderTags(result)
      full_html <- paste(rendered$head, rendered$html)

      # Should contain theme-color meta tag
      expect_true(
        grepl("theme-color", full_html),
        info = "Expected theme-color meta tag"
      )
    }
  )
})

test_that("app_add_external_resources contains inline critical CSS", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemeton:::app_add_external_resources()

      rendered <- htmltools::renderTags(result)
      full_html <- paste(rendered$head, rendered$html)

      # Should contain inline style tag with background-color
      expect_true(
        grepl("background-color", full_html),
        info = "Expected inline critical CSS with background-color"
      )
    }
  )
})

test_that("app_add_external_resources head content includes link and meta tags", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemeton:::app_add_external_resources()

      rendered <- htmltools::renderTags(result)
      head_html <- rendered$head

      # Head section should contain link and meta tags
      expect_true(
        grepl("<link", head_html) && grepl("<meta", head_html),
        info = "Expected head content with link and meta tags"
      )
    }
  )
})

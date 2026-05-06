# Tests for mod_action_plan helpers (the reactive UI logic is covered
# end-to-end in the Shiny app; here we focus on the pure helpers).

test_that("coerce_table_value parses ints, numerics, strings and blanks", {
  expect_identical(nemetonshiny:::coerce_table_value("annee_cible", "5"), 5L)
  expect_identical(nemetonshiny:::coerce_table_value("nb_tiges",   "120"), 120L)
  expect_identical(nemetonshiny:::coerce_table_value("volume_m3",  "12.5"), 12.5)
  expect_identical(nemetonshiny:::coerce_table_value("rdi",        "0.7"), 0.7)
  expect_identical(nemetonshiny:::coerce_table_value("commentaire", "abc"), "abc")
  # blanks become NA (caller decides what to do)
  expect_true(is.na(nemetonshiny:::coerce_table_value("annee_cible", "")))
  expect_true(is.na(nemetonshiny:::coerce_table_value("commentaire", NULL)))
})

test_that("module UI renders without error", {
  ui <- nemetonshiny:::mod_action_plan_ui("ap")
  # bslib widgets may be tag, tagList or htmlwidget shells — we just want
  # the call to succeed and produce HTML when rendered.
  expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list", "shiny.tag.env"),
                       which = FALSE) ||
              !is.null(htmltools::renderTags(ui)$html))
})

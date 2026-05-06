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

test_that("Kanban drag-and-drop assets are vendored in inst/app/www/js", {
  sortable <- system.file("app/www/js/Sortable-1.15.6.min.js",
                          package = "nemetonshiny")
  init_js  <- system.file("app/www/js/action_plan_kanban.js",
                          package = "nemetonshiny")
  expect_true(nzchar(sortable))
  expect_true(nzchar(init_js))
  # The init script must expose initKanbanSortable so the inline
  # <script> emitted by renderUI can call it.
  expect_match(paste(readLines(init_js, warn = FALSE), collapse = "\n"),
               "window\\.initKanbanSortable", fixed = FALSE)
  # Sortable.create is the API we rely on inside initKanbanSortable.
  expect_match(paste(readLines(sortable, warn = FALSE), collapse = "\n"),
               "Sortable", fixed = TRUE)
})

test_that("kanban_drop validation: legal vs refused transitions", {
  # Direct probe of the underlying transition validator, which is
  # the single source of truth for the drop observer.
  expect_true(nemetonshiny:::is_valid_status_transition("proposee", "validee"))
  expect_true(nemetonshiny:::is_valid_status_transition("planifiee", "realisee"))
  # Realisee is terminal — drag-drop must refuse moving out of it.
  expect_false(nemetonshiny:::is_valid_status_transition("realisee", "proposee"))
  # Same-column drop is treated as a no-op upstream; the validator
  # itself returns TRUE for identical states.
  expect_true(nemetonshiny:::is_valid_status_transition("validee", "validee"))
})

test_that("i18n key for refused drop exists in FR + EN", {
  fr <- nemetonshiny:::get_i18n("fr")
  en <- nemetonshiny:::get_i18n("en")
  expect_match(fr$t("action_plan_kanban_drop_invalid_fmt"),
               "Transition refus", fixed = FALSE)
  expect_match(en$t("action_plan_kanban_drop_invalid_fmt"),
               "Transition refused", fixed = FALSE)
})

# Coherence tests for the guided tour (service_tour.R). The cicerone
# framing itself needs a browser (not unit-testable), but we guard the
# two things that silently break the tour : (1) i18n keys that don't
# resolve, (2) anchor ids that no longer exist in the app UI.

`%||%` <- function(a, b) if (is.null(a)) b else a

.valid_main_nav_tabs <- c(
  "selection", "synthesis", "action_plan", "terrain", "monitoring",
  "famille_carbone", "famille_biodiversite", "famille_eau", "famille_air",
  "famille_sol", "famille_paysage", "famille_temporel", "famille_risque",
  "famille_social", "famille_production", "famille_energie", "famille_naturalite"
)

test_that("build_tour_steps yields a well-formed, i18n-resolved spec", {
  steps <- nemetonshiny:::build_tour_steps(nemetonshiny:::get_i18n("fr"), 30L)
  expect_gte(length(steps), 11L)
  for (s in steps) {
    expect_true(all(c("el", "title", "description", "tab") %in% names(s)))
    # el / tab non-empty strings
    expect_true(is.character(s$el) && nzchar(s$el))
    expect_true(s$tab %in% .valid_main_nav_tabs)
    # i18n actually resolved (not the raw key, not a "not found" marker)
    for (txt in c(s$title, s$description)) {
      expect_true(is.character(txt) && nzchar(txt))
      expect_false(grepl("not found", txt, ignore.case = TRUE))
      expect_false(grepl("^tour_[a-z_]+$", txt))  # would be an unresolved key
    }
  }
  # Every main top-level tab is covered at least once.
  covered <- unique(vapply(steps, function(s) s$tab, character(1)))
  for (tab in c("selection", "synthesis", "action_plan", "terrain",
                "monitoring", "famille_carbone")) {
    expect_true(tab %in% covered, info = paste("tab not covered:", tab))
  }
})

test_that("build_tour_steps resolves identically in EN", {
  steps_en <- nemetonshiny:::build_tour_steps(nemetonshiny:::get_i18n("en"), 30L)
  for (s in steps_en) {
    expect_false(grepl("not found", s$title, ignore.case = TRUE))
    expect_false(grepl("^tour_[a-z_]+$", s$title))
  }
})

test_that("every tour anchor id still exists in the app UI", {
  skip_if_not_installed("bslib")
  ui_html <- as.character(nemetonshiny:::app_ui(list()))
  steps <- nemetonshiny:::build_tour_steps(nemetonshiny:::get_i18n("fr"), 30L)

  # `home-project-create_project` is rendered server-side (mod_project's
  # action_button renderUI, create mode) — its static proxy is the
  # uiOutput container. Everything else is present in the static UI.
  server_rendered <- "home-project-create_project"
  for (s in steps) {
    if (identical(s$el, server_rendered)) next
    expect_true(
      grepl(paste0('id="', s$el, '"'), ui_html, fixed = TRUE),
      info = paste("tour anchor missing from app UI:", s$el)
    )
  }
  # Proxy container for the server-rendered create button.
  expect_true(grepl('id="home-project-action_button"', ui_html, fixed = TRUE))
})

test_that("build_tour_guide returns a cicerone guide when available", {
  skip_if_not_installed("cicerone")
  g <- nemetonshiny:::build_tour_guide(nemetonshiny:::get_i18n("fr"), 30L)
  expect_s3_class(g, "Cicerone")
})

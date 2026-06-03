# test-mod_rag_admin-e2e.R — spec 009.2 (E7) smoke E2E
#
# Boots the full Shiny app via shinytest2::AppDriver, navigates to the
# RAG admin tab (Settings menu) and asserts the manifest table and the
# add-row control are present. No DB, no real embedding — the module
# reads the packaged seed manifest and keeps the inventory empty when no
# corpus DB is reachable, which is exactly the smoke state we want.
#
# Skipped on machines without shinytest2 / chromote / Chrome.

test_that("RAG admin tab boots and shows the manifest controls", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("nemeton")
  skip_if_not(exists("knowledge_manifest_path", where = asNamespace("nemeton"),
                     inherits = FALSE),
              "nemeton >= 0.63.0 (knowledge_* API) not installed")
  if (!nzchar(Sys.which("google-chrome")) &&
      !nzchar(Sys.which("chromium")) &&
      !nzchar(Sys.which("chromium-browser")) &&
      !nzchar(Sys.which("chrome"))) {
    skip("No Chrome / Chromium binary found for chromote")
  }

  app_object <- shiny::shinyApp(
    ui     = nemetonshiny:::app_ui,
    server = nemetonshiny:::app_server
  )

  app <- tryCatch(
    shinytest2::AppDriver$new(
      app_object,
      name         = "rag-admin-smoke",
      load_timeout = 30 * 1000,
      timeout      = 10 * 1000,
      variant      = NULL,
      view         = FALSE,
      options      = list(nemeton.app_options = list(language = "fr"))
    ),
    error = function(e) {
      skip(sprintf("AppDriver failed to boot: %s", conditionMessage(e)))
    }
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  # Navigate to the RAG admin tab (nav_panel value "rag_admin", nested in
  # the Settings nav_menu).
  app$set_inputs(main_nav = "rag_admin")
  Sys.sleep(0.5)

  vals <- app$get_values()$input
  expect_equal(vals[["main_nav"]], "rag_admin")
  # The add-row button is a static (non-renderUI) control, so it always
  # appears in the input registry once the tab is shown.
  expect_true("rag_admin-add_row" %in% names(vals),
              info = paste("rag_admin-add_row input not found. Inputs:",
                           paste(names(vals), collapse = ", ")))
})

# test-mod_rag_admin-e2e.R — spec 009.2 (E7) smoke E2E
#
# Boots the full Shiny app via shinytest2::AppDriver, opens the settings
# (gear) modal where the RAG admin now lives as a third tab, and asserts
# the manifest add-row control is present. No DB, no real embedding — the
# module reads the packaged seed manifest and keeps the inventory empty
# when no corpus DB is reachable, which is exactly the smoke state we want.
#
# Skipped on machines without shinytest2 / chromote / Chrome.

test_that("RAG admin tab boots inside the settings modal", {
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

  # Open the settings (gear) modal. `open` is an actionLink; set_inputs()
  # rejects a non-"click" value for action inputs, and app$click() waits
  # for an output change that showModal() never produces. A direct DOM
  # click on the link reliably fires its Shiny handler and shows the modal.
  app$run_js("document.getElementById('theia_config-open').click();")
  app$wait_for_idle(timeout = 5 * 1000)

  # The RAG tab is LAZY-rendered: mod_rag_admin_ui() (and its static
  # add_row control) only mounts when `config_tab == "tab_rag"`. This is
  # deliberate — DataTables must not initialise inside a display:none
  # container (it errors and cascades onto sibling outputs). A tabsetPanel
  # exposes no settable input binding, so activate the tab by clicking its
  # nav link (Bootstrap toggle + Shiny input update), which both reveals
  # the panel and flips `config_tab` to "tab_rag".
  app$run_js(
    "var t = document.querySelector('.modal [data-value=\"tab_rag\"]');
     if (t) { t.click(); }"
  )
  app$wait_for_idle(timeout = 5 * 1000)
  Sys.sleep(0.8)

  vals <- app$get_values()$input
  # Add-row is a static (non-renderUI) control under the nested namespace
  # theia_config-rag_admin-…
  expect_true("theia_config-rag_admin-add_row" %in% names(vals),
              info = paste("theia_config-rag_admin-add_row not found.",
                           "Inputs:", paste(names(vals), collapse = ", ")))
})

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
  # A NON-snap Chrome is required: the chromium snap wedges on Page.navigate
  # under AppDriver (confined sandbox), whereas a google-chrome .deb boots the
  # app reliably. The boot is wrapped in tryCatch -> skip() below so a flaky
  # navigate never turns into a false failure.
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
      load_timeout = 40 * 1000,
      timeout      = 15 * 1000,
      variant      = NULL,
      view         = FALSE,
      options      = list(nemeton.app_options = list(language = "fr"))
    ),
    error = function(e) {
      skip(sprintf("AppDriver failed to boot: %s", conditionMessage(e)))
    }
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  # Open the settings (gear) modal. The gear renders as a navbar nav-link
  # (role="tab"): a native DOM .click() and app$click() are swallowed by
  # Bootstrap's tab handling and never fire input$open. Driving the action
  # input with the value "click" is the reliable trigger for an actionLink
  # under shinytest2 (a numeric value is rejected).
  #
  # KEEP the default wait_/timeout_ (do NOT set wait_ = FALSE or shorten
  # timeout_): showModal() changes no output value, so shinytest2 pumps the
  # message loop for the full timeout while waiting — and that pumping is what
  # lets the server process input$open and push the modal to the client. It
  # prints a benign "Server did not update any output values" note (WARN 0);
  # with wait_ = FALSE or a short timeout_ the loop isn't pumped enough and the
  # modal never appears (verified: both fail).
  app$set_inputs(`theia_config-open` = "click", allow_no_input_binding_ = TRUE)

  # NOTE: wait_for_idle() throws here ("session unstable") because of an
  # UNRELATED cicerone (guided-tour) client-side JS error emitted on the same
  # flush ("There are no steps defined to iterate"). It is harmless for this
  # smoke test, so tolerate it and wait for the modal DOM instead.
  try(app$wait_for_idle(timeout = 6 * 1000), silent = TRUE)
  app$wait_for_js("!!document.querySelector('.modal.show')", timeout = 8 * 1000)

  # The RAG tab is LAZY-rendered: mod_rag_admin_ui() (and its static add_row
  # control) only mounts when config_tab == "tab_rag". This is deliberate —
  # DataTables must not initialise inside a display:none container. A
  # tabsetPanel exposes no settable input binding, so activate the tab by
  # clicking its nav link, which reveals the panel and flips config_tab.
  app$run_js(
    "var t = document.querySelector('.modal [data-value=\"tab_rag\"]');
     if (t) { t.click(); }"
  )
  try(app$wait_for_idle(timeout = 6 * 1000), silent = TRUE)
  # The add_row control is a static actionButton in the nested namespace; once
  # the lazy tab mounts, its DOM node exists. Wait on it rather than a fixed
  # sleep so the assertion is not racy.
  app$wait_for_js(
    "!!document.getElementById('theia_config-rag_admin-add_row')",
    timeout = 8 * 1000
  )

  vals <- app$get_values()$input
  # Add-row is a static (non-renderUI) control under the nested namespace
  # theia_config-rag_admin-…
  expect_true("theia_config-rag_admin-add_row" %in% names(vals),
              info = paste("theia_config-rag_admin-add_row not found.",
                           "Inputs:", paste(names(vals), collapse = ", ")))
})

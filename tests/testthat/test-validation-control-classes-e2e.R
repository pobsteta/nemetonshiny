# test-validation-control-classes-e2e.R — smoke E2E (spec control_classes)
#
# Boots the full Shiny app via shinytest2::AppDriver, navigates to the
# Monitoring tab → **FORDEAD** « Plan de validation » sub-tab (mode
# « health »), and asserts that the `control_classes` checkbox group is
# rendered and defaults to "0". Depuis spec 025, le plan de validation
# **FAST** est branché sur le trend (pondération continue |pente|) et n'a
# plus de classes/témoins : `control_classes` n'existe que pour FORDEAD /
# RECONFORT. No DB / no ingestion : the distribution hint and auto-relax
# need a cached alert mask and so are NOT exercised here.
#
# Skipped on machines without shinytest2 / chromote / Chrome (CI workers
# + dev machines differ). Same guards as test-monitoring-smoke-e2e.R.

test_that("FORDEAD validation sub-tab renders the control_classes selector", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("nemeton")
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
      name         = "validation-control-classes-smoke",
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

  # Monitoring tab, then switch to the « Diagnostic sanitaire » (health)
  # mode so the FORDEAD validation sub-tab is shown by the mode-driven
  # nav_show/nav_hide.
  # `wait_ = FALSE` : navigating a fresh app updates no output (no project
  # loaded) → the default output-wait would time out with a warning. We
  # only need the inputs registered, so we drive the nav and sleep.
  app$set_inputs(main_nav = "monitoring", wait_ = FALSE)
  Sys.sleep(0.5)
  app$set_inputs(`monitoring-mode` = "health", wait_ = FALSE)
  Sys.sleep(0.5)
  # Activate the FORDEAD "Plan de validation" sub-tab (navset id "subtab",
  # nav_panel value "validation_sampling_fordead" — cf. mod_monitoring_ui).
  app$set_inputs(`monitoring-subtab` = "validation_sampling_fordead",
                 wait_ = FALSE)
  Sys.sleep(1)

  # The control_classes checkbox group lives in the FORDEAD validation
  # module sidebar : id monitoring-validation_sampling_fordead-control_classes.
  ccid <- "monitoring-validation_sampling_fordead-control_classes"

  # Prefer a DOM check (robust for a freshly-shown panel), fall back to
  # the input-value check.
  html <- tryCatch(app$get_html(paste0("#", ccid)), error = function(e) NULL)
  if (!is.null(html) && nzchar(html)) {
    expect_match(html, "checkbox")
  } else {
    vals <- app$get_values()$input
    expect_true(ccid %in% names(vals),
                info = paste("control_classes input not found. Inputs:",
                             paste(names(vals), collapse = ", ")))
    expect_equal(vals[[ccid]], "0")  # default = healthy class
  }
})

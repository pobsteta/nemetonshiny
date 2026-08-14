# test-monitoring-smoke-e2e.R — E6.b phase 6 smoke E2E
#
# Boots the full Shiny app via shinytest2::AppDriver, navigates to the
# Monitoring tab, and asserts that the two monitoring modes (quick /
# health) are present and switchable. No DB, no real ingestion — the
# monitoring server keeps zones empty when no DB is reachable, which
# is exactly the state we want for a smoke test.
#
# Skipped on machines without shinytest2 / chromote / Chrome installed
# (CI workers + dev machines may differ).

test_that("monitoring tab boots and the two modes are reachable", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("nemeton")
  # v0.52.17 — précondition `read_obs_pixel exported` retirée :
  # cette fonction a été supprimée du cœur en `nemeton@v0.60.0`
  # (suite spec 017 / refactor obs_pixel). Le smoke test ne dépend
  # plus de cette API.
  skip_if_not(e2e_has_chrome(), "No Chrome / Chromium binary found for chromote")

  # Démarrage mutualisé (helper-e2e_app.R) : il construit l'app depuis
  # `app_ui`/`app_server` (pas `run_app()`, dont les options et vérifications de
  # dépendances ne servent pas à un smoke), impose la base hors-jeu — « No DB »
  # est la prémisse de ce test, cf. en-tête — et réessaye `Page.navigate`.
  app <- e2e_boot_app("monitoring-smoke",
                      load_timeout = 30 * 1000, timeout = 10 * 1000)
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  # Navigate to the Monitoring tab. Uses the navbar's id (`main_nav`)
  # and the nav_panel value ("monitoring") declared in app_ui.R.
  app$set_inputs(main_nav = "monitoring")
  Sys.sleep(0.5)

  vals <- app$get_values()$input
  expect_true(!is.null(vals[["main_nav"]]),
              info = "main_nav input not exposed by the app")
  expect_equal(vals[["main_nav"]], "monitoring")

  # The mode radio is namespaced "monitoring-mode" and defaults to
  # "quick" (cf. mod_monitoring_ui).
  expect_true("monitoring-mode" %in% names(vals),
              info = paste("monitoring-mode input not found.",
                           "Available inputs:",
                           paste(names(vals), collapse = ", ")))
  expect_equal(vals[["monitoring-mode"]], "quick")

  # Switch to health mode and confirm the radio updates.
  app$set_inputs(`monitoring-mode` = "health")
  Sys.sleep(0.3)
  expect_equal(app$get_value(input = "monitoring-mode"), "health")

  # And back to quick — bidirectional toggle.
  app$set_inputs(`monitoring-mode` = "quick")
  Sys.sleep(0.3)
  expect_equal(app$get_value(input = "monitoring-mode"), "quick")
})

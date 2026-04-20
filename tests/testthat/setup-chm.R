# Disable Open-Canopy CHM auto-detection for the test suite.
#
# download_layers_for_parcels() attempts CHM when the `opencanopy`
# package is installed (which it is on dev boxes), which would run
# a multi-minute Python ML pipeline inside every test that exercises
# downloads. Tests already mock the IGN WFS / S2 downloads; CHM is
# simply out of scope. Forcing the opt-out keeps test runs fast and
# deterministic regardless of the host's Python/conda setup.
#
# This MUST live in a setup-*.R file (not helper-*.R): testthat's
# teardown_env() is only available after the test framework has
# finished its own initialisation, which happens between helper
# sourcing and setup sourcing.
withr::local_envvar(
  NEMETONSHINY_DISABLE_CHM = "1",
  .local_envir = testthat::teardown_env()
)

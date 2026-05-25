# Tests for persist_validation_plan() — spec 014 phase B.

.make_validation_plan <- function(n = 3L,
                                  generated_at = Sys.time(),
                                  prefix = "V") {
  ts <- rep(generated_at, n)
  sf::st_sf(
    plot_id      = sprintf("%s%02d", prefix, seq_len(n)),
    type         = "Validation",
    alert_class  = 3L,
    visit_order  = seq_len(n),
    zone_id      = 7L,
    source       = "FORDEAD",
    classes      = "3,4",
    seed         = 42L,
    source_run_id = "20260520T100000",
    generated_at = ts,
    geometry     = sf::st_sfc(
      lapply(seq_len(n), function(i) sf::st_point(c(i, i))),
      crs = 2154
    )
  )
}


test_that("persist_validation_plan creates the layer when absent", {
  withr::with_tempdir({
    proj <- getwd()
    plan <- .make_validation_plan(n = 4L)
    n <- nemetonshiny:::persist_validation_plan(plan, proj)
    expect_equal(n, 4L)
    gpkg <- file.path(proj, "data", "samples.gpkg")
    expect_true(file.exists(gpkg))
    layers <- sf::st_layers(gpkg)$name
    expect_true("validation_plots" %in% layers)
    reread <- sf::st_read(gpkg, layer = "validation_plots",
                          quiet = TRUE)
    expect_equal(nrow(reread), 4L)
  })
})

test_that("persist_validation_plan appends a new run with a new generated_at", {
  withr::with_tempdir({
    proj <- getwd()
    t1 <- Sys.time()
    nemetonshiny:::persist_validation_plan(
      .make_validation_plan(n = 3L, generated_at = t1, prefix = "V"),
      proj
    )
    # Second run with a different timestamp.
    t2 <- t1 + 60
    n <- nemetonshiny:::persist_validation_plan(
      .make_validation_plan(n = 2L, generated_at = t2, prefix = "W"),
      proj
    )
    expect_equal(n, 5L)  # 3 + 2, no dedup since timestamps differ.
  })
})

test_that("persist_validation_plan is idempotent on (plot_id, generated_at)", {
  withr::with_tempdir({
    proj <- getwd()
    t1 <- Sys.time()
    plan <- .make_validation_plan(n = 3L, generated_at = t1)
    nemetonshiny:::persist_validation_plan(plan, proj)
    # Same plan, persisted twice → still 3 rows.
    n <- nemetonshiny:::persist_validation_plan(plan, proj)
    expect_equal(n, 3L)
  })
})

test_that("persist_validation_plan overwrites the layer when append = FALSE", {
  withr::with_tempdir({
    proj <- getwd()
    nemetonshiny:::persist_validation_plan(
      .make_validation_plan(n = 5L), proj
    )
    n <- nemetonshiny:::persist_validation_plan(
      .make_validation_plan(n = 2L, generated_at = Sys.time() + 10,
                            prefix = "W"),
      proj,
      append = FALSE
    )
    expect_equal(n, 2L)  # original 5 rows replaced
  })
})

test_that("persist_validation_plan coexists with the systemic 'plots' layer", {
  withr::with_tempdir({
    proj <- getwd()
    # Pre-existing systemic plan in the "plots" layer.
    data_dir <- file.path(proj, "data")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    syst <- sf::st_sf(
      plot_id  = c("P01", "P02"),
      type     = "Base",
      geometry = sf::st_sfc(sf::st_point(c(0, 0)),
                             sf::st_point(c(1, 1)),
                             crs = 2154)
    )
    sf::st_write(syst, file.path(data_dir, "samples.gpkg"),
                 layer = "plots", quiet = TRUE)
    # Now persist a validation plan ; the "plots" layer must remain.
    nemetonshiny:::persist_validation_plan(
      .make_validation_plan(n = 3L), proj
    )
    layers <- sf::st_layers(file.path(data_dir, "samples.gpkg"))$name
    expect_true(all(c("plots", "validation_plots") %in% layers))
  })
})

test_that("load_validation_plan returns NULL when layer is absent", {
  withr::with_tempdir({
    expect_null(nemetonshiny:::load_validation_plan(getwd()))
  })
})

test_that("load_validation_plan round-trips a persisted plan", {
  withr::with_tempdir({
    proj <- getwd()
    plan <- .make_validation_plan(n = 2L)
    nemetonshiny:::persist_validation_plan(plan, proj)
    out <- nemetonshiny:::load_validation_plan(proj)
    expect_s3_class(out, "sf")
    expect_equal(nrow(out), 2L)
    expect_setequal(out$plot_id, plan$plot_id)
  })
})

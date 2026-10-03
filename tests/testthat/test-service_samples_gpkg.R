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


test_that("un plan FAST persiste apres un plan FORDEAD ne lui retire aucune colonne", {
  # Jusqu'en v0.152.4, l'accumulation ne gardait que les colonnes COMMUNES et
  # reecrivait la couche : `alert_class` et `visit_order` des placettes
  # FORDEAD deja sur disque etaient perdus definitivement.
  withr::with_tempdir({
    fordead <- .make_validation_plan(3L, as.POSIXct("2026-05-20 10:00:00", tz = "UTC"))
    nemetonshiny:::persist_validation_plan(fordead, getwd())

    fast <- sf::st_sf(
      plot_id = c("F01", "F02"), type = "Validation", source = "FAST",
      alert_value = c(0.31, 0.47), index = "NDVI", zone_id = 7L, seed = 1L,
      generated_at = rep(as.POSIXct("2026-05-21 10:00:00", tz = "UTC"), 2),
      geometry = sf::st_sfc(sf::st_point(c(5, 5)), sf::st_point(c(6, 6)), crs = 2154))
    n <- nemetonshiny:::persist_validation_plan(fast, getwd())
    expect_equal(n, 5L)

    lu <- sf::st_read(file.path("data", "samples.gpkg"), layer = "validation_plots",
                      quiet = TRUE)
    expect_true(all(c("alert_class", "visit_order", "classes", "alert_value",
                      "index") %in% names(lu)))
    fd <- lu[lu$source == "FORDEAD", ]
    expect_equal(sort(fd$visit_order), 1:3)
    expect_true(all(fd$alert_class == 3L))
    expect_true(all(is.na(fd$alert_value)))
    fa <- lu[lu$source == "FAST", ]
    expect_equal(sort(fa$alert_value), c(0.31, 0.47))
    expect_true(all(is.na(fa$visit_order)))
    expect_false(file.exists(file.path("data", "samples.gpkg.tmp")))
  })
})

# Tests — persistance & export reGénération (spec 027 §7, L6)

.regen_persist_units <- function(n = 3) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- i * 10
    sf::st_polygon(list(matrix(c(
      x0, 0, x0 + 5, 0, x0 + 5, 5, x0, 5, x0, 0), ncol = 2, byrow = TRUE)))
  })
  sf::st_sf(
    ug_id = as.character(seq_len(n)),
    indice_priorite_regen = seq(70, by = 5, length.out = n),
    sensibilite = seq(60, by = -5, length.out = n),
    njstress = seq(10, by = 2, length.out = n),
    couverture_pct = 80,
    geometry = sf::st_sfc(polys, crs = 2154)
  )
}

test_that("export_regeneration_geopackage writes the §7 columns and reads back", {
  skip_if_not_installed("sf")
  units <- .regen_persist_units(3)
  f <- withr::local_tempfile(fileext = ".gpkg")

  out <- nemetonshiny:::export_regeneration_geopackage(units, f)
  expect_true(file.exists(out))

  back <- sf::st_read(out, quiet = TRUE)
  expect_true(all(c("ug_id", "indice_priorite_regen", "sensibilite", "njstress")
                  %in% names(back)))
  expect_equal(nrow(back), 3L)
})

test_that("export_regeneration_geopackage rejects non-sf input", {
  expect_error(
    nemetonshiny:::export_regeneration_geopackage(data.frame(a = 1), tempfile()),
    "sf object")
})

test_that("db_save_regeneration appends a new versioned state (§6A)", {
  units <- data.frame(
    ug_id = c("a", "b"),
    indice_priorite_regen = c(70, 80),
    sensibilite = c(60, 50),
    njstress = c(12, 18))

  rec <- new.env(); rec$sqls <- character(0)
  con <- structure(list(), class = "FakeConn")

  testthat::local_mocked_bindings(
    dbExecute = function(conn, statement, ...) {
      rec$sqls <- c(rec$sqls, statement); 1L
    },
    dbGetQuery = function(conn, statement, ...) data.frame(v = 3L),
    .package = "DBI"
  )

  ver <- nemetonshiny:::db_save_regeneration(con, "p1", units)

  expect_equal(ver, 3L)                                   # MAX(version)+1
  expect_true(any(grepl("CREATE TABLE IF NOT EXISTS nemeton.regeneration_states",
                        rec$sqls, fixed = TRUE)))         # table idempotente
  expect_equal(sum(grepl("INSERT INTO nemeton.regeneration_states", rec$sqls,
                         fixed = TRUE)), 2L)              # une ligne par UG
})

test_that("db_save_regeneration defaults to version 1 on empty history", {
  units <- data.frame(ug_id = "x", indice_priorite_regen = 55)
  con <- structure(list(), class = "FakeConn")
  testthat::local_mocked_bindings(
    dbExecute = function(conn, statement, ...) 1L,
    dbGetQuery = function(conn, statement, ...) data.frame(v = NA_integer_),
    .package = "DBI"
  )
  expect_equal(nemetonshiny:::db_save_regeneration(con, "p1", units), 1L)
})

test_that("regeneration_report_summary ranks and truncates for the Quarto section", {
  units <- .regen_persist_units(4)
  units$rang_sensibilite <- c(3, 1, 4, 2)

  s <- nemetonshiny:::regeneration_report_summary(units, top_n = 2)
  expect_s3_class(s, "data.frame")
  expect_equal(nrow(s), 2L)
  expect_equal(s$rang_sensibilite, c(1, 2))          # trié par rang croissant
  expect_true("indice_priorite_regen" %in% names(s))

  expect_null(nemetonshiny:::regeneration_report_summary(NULL))
})

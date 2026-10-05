# Suivi sanitaire : petits correctifs de l'audit 1.0

test_that(".pixel_yrange keeps [-0.2, 1] and widens to the data", {
  expect_equal(.pixel_yrange(data.frame(value = c(0.3, 0.8))), c(-0.2, 1))
  r <- .pixel_yrange(data.frame(value = c(-0.6, 0.5), smoothed = c(-0.5, NA)))
  expect_lt(r[1], -0.6)
  expect_equal(r[2], 1)
  # Codes aberrants (> 2 en valeur absolue) ignores
  expect_equal(.pixel_yrange(data.frame(value = c(-9999, 0.4))), c(-0.2, 1))
  expect_equal(.pixel_yrange(NULL), c(-0.2, 1))
})

test_that(".validation_classe_saine is 1 for RECONFORT, 0 otherwise", {
  expect_identical(.validation_classe_saine("RECONFORT"), "1")
  expect_identical(.validation_classe_saine("FORDEAD"), "0")
  expect_identical(.validation_classe_saine("FAST"), "0")
})

test_that("the real disable handler exists in custom.js", {
  js <- system.file("app", "www", "js", "custom.js", package = "nemetonshiny")
  if (!nzchar(js)) js <- testthat::test_path("..", "..", "inst", "app", "www", "js", "custom.js")
  skip_if(!file.exists(js))
  expect_true(any(grepl("addCustomMessageHandler('nemetonSetDisabled'",
                        readLines(js, warn = FALSE), fixed = TRUE)))
})

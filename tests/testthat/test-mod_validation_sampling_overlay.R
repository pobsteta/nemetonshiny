# Mission 023 - surcouche du Plan de validation : classe saine transparente,
# couleurs de la carte de sante de chaque moteur. Aucun Shiny : helper pur.

.ov_raster <- function(vals) {
  r <- terra::rast(nrows = 1, ncols = length(vals), xmin = 0,
                   xmax = length(vals), ymin = 0, ymax = 1, crs = "EPSG:2154")
  terra::values(r) <- vals
  r
}
.ov_vals <- function(x) as.numeric(terra::values(x$r_show))
.ov_colors <- function(ov, k) {
  toupper(vapply(k, function(v) ov$pal(v), character(1)))
}

test_that("FORDEAD : classe 0 transparente, couleurs = carte FORDEAD reelle", {
  i18n <- get_i18n("fr")
  r  <- .ov_raster(c(0, 1, 2, 3, 4, NA))
  ov <- .validation_overlay_style(r, "FORDEAD", i18n)
  expect_equal(.ov_vals(ov), c(NA, 1, 2, 3, 4, NA))
  carte <- .fordead_layer_spec(r, "severity", i18n)
  expect_equal(toupper(ov$pal(1:4)), toupper(carte$pal(1:4)))
  expect_equal(toupper(ov$pal(2)), "#FF9933")
  expect_equal(toupper(ov$pal(4)), "#222222")
  expect_equal(toupper(unname(.FORDEAD_SEVERITY_COLORS)),
               toupper(carte$legend$colors))
  expect_length(ov$legend$colors, 4L)
  expect_equal(ov$legend$title, i18n$t("monitoring_fordead_class_title"))
})

test_that("FAST : classe 0 transparente, palette FAST (4 = rouge severe)", {
  i18n <- get_i18n("fr")
  r  <- .ov_raster(c(0, 1, 2, 3, 4, NA))
  ov <- .validation_overlay_style(r, "FAST", i18n, index = "NDVI")
  expect_equal(.ov_vals(ov), c(NA, 1, 2, 3, 4, NA))
  expect_equal(.ov_colors(ov, 1:4), toupper(unname(.FAST_ALERT_CLASS_COLORS)))
  expect_equal(toupper(ov$pal(4)), "#D73027")
  expect_false(identical(toupper(ov$pal(4)), "#222222"))
  expect_equal(toupper(ov$pal(1)), "#FEE08B")
  expect_equal(ov$legend$labels,
               vapply(1:4, function(k) i18n$t(paste0("fast_alert_class_", k)),
                      character(1)))
  expect_match(ov$legend$title, "NDVI")
})

test_that("RECONFORT : 0, 1 et NA transparents, 2 et 3 aux couleurs de la carte", {
  i18n <- get_i18n("fr")
  r  <- .ov_raster(c(0, 1, 2, 3, NA))
  ov <- .validation_overlay_style(r, "RECONFORT", i18n)
  expect_equal(.ov_vals(ov), c(NA, NA, 2, 3, NA))
  expect_equal(.ov_colors(ov, 2:3),
               toupper(unname(.RECONFORT_CLASSIF_COLORS[c("2", "3")])))
  expect_equal(toupper(ov$pal(2)), "#FF9933")
  expect_equal(toupper(ov$pal(3)), "#D62728")
  expect_equal(toupper(ov$legend$colors), c("#FF9933", "#D62728"))
  expect_equal(ov$legend$labels,
               c(i18n$t("reconfort_class_label_2"),
                 i18n$t("reconfort_class_label_3")))
})

test_that("une source inconnue retombe sur FORDEAD", {
  ov <- .validation_overlay_style(.ov_raster(c(0, 3)), "AUTRE", get_i18n("en"))
  expect_equal(.ov_vals(ov), c(NA, 3))
  expect_equal(toupper(ov$pal(3)), "#D62728")
})

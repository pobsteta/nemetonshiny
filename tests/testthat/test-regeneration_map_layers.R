# Couches composites de la carte reGeneration : bivariee DeltaTmax x DeltaVPD
# (brief 027 onglet sect.4.3) et meilleure essence (spec 039 sect.7).

.ml_units <- function(n = 3) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- 900000 + i * 100
    sf::st_polygon(list(matrix(c(x0, 6500000, x0 + 50, 6500000, x0 + 50, 6500050,
                                 x0, 6500050, x0, 6500000), ncol = 2, byrow = TRUE)))
  })
  sf::st_sf(ug_id = as.character(seq_len(n)), geometry = sf::st_sfc(polys, crs = 2154))
}

test_that("les terciles d'affichage gardent l'ordre et les NA", {
  t <- nemetonshiny:::.regen_tercile(c(1, 2, 3, NA, 10, 20))
  expect_identical(t, c(1L, 2L, 2L, NA, 3L, 3L))
  expect_true(all(is.na(nemetonshiny:::.regen_tercile(c(NA, NA)))))
})

test_that("la classe bivariee met l'UGF chaude ET seche dans le coin 9", {
  cls <- nemetonshiny:::.regen_bivariate_class(d_tmax = c(1, 2, 3), d_vpd = c(0.1, 0.2, 0.3))
  expect_identical(cls, c(1L, 5L, 9L))
  # Chaude mais humide : ligne haute, colonne basse -> 7.
  expect_identical(nemetonshiny:::.regen_bivariate_class(c(1, 2, 3), c(0.3, 0.2, 0.1))[3], 7L)
  expect_true(is.na(nemetonshiny:::.regen_bivariate_class(c(1, NA, 3), c(1, 2, 3))[2]))
})

test_that("la couche bivariee peint les UGF et pose sa legende 3x3", {
  skip_if_not_installed("sf")
  i18n <- get_i18n("fr")
  res <- .ml_units(); res$d_tmax <- c(1, 2, 3); res$d_vpd <- c(0.1, 0.2, 0.3)
  m <- nemetonshiny:::.regen_map_bivariee(leaflet::leaflet(), res, i18n)
  expect_true(m)
})

test_that("la meilleure essence lit le rang 1 du classement, sinon rend la main", {
  skip_if_not_installed("sf")
  i18n <- get_i18n("fr")
  res <- .ml_units()
  rk <- data.frame(ug_id = c("1", "1", "2", "3"), rank = c(1L, 2L, 1L, NA),
                   label = c("Chêne sessile", "Hêtre", "Pin sylvestre", NA),
                   suitability = c(80, 60, 70, NA))
  expect_true(nemetonshiny:::.regen_map_meilleure_essence(leaflet::leaflet(), res, rk, i18n))
  expect_false(nemetonshiny:::.regen_map_meilleure_essence(leaflet::leaflet(), res, NULL, i18n))
  expect_false(nemetonshiny:::.regen_map_meilleure_essence(
    leaflet::leaflet(), res, rk[rk$rank %in% 2L, ], i18n))
})

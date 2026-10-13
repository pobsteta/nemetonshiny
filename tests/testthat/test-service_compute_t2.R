# T2 (spec 060) : mesure FORMS-T seule. Depuis nemeton 2026.10.5 le coeur ne
# recopie plus N2 ni l'age T1 (brief du 2026-10-13) : l'app ne lui passe plus
# ces colonnes et ne force plus N2 avant T2.

.t2_parcels <- function() {
  sq <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 1, 0), c(x + 1, 1),
                                              c(x, 1), c(x, 0))))
  sf::st_sf(id = c("p1", "p2"), geometry = sf::st_sfc(sq(0), sq(2), crs = 2154))
}

test_that(".units_for_indicator ne passe plus N2 ni T1 a T2", {
  f <- nemetonshiny:::.units_for_indicator
  p <- .t2_parcels()
  res <- data.frame(indicateur_n2_continuite = c(70, 40),
                    indicateur_t1_anciennete = c(120, 30))
  u <- f("indicateur_t2_changement", p, res, list())
  expect_identical(u, p)
})

test_that("sans serie FORMS-T, T2 vaut NA (nemeton >= 2026.10.5)", {
  skip_if_not_installed("sf")
  skip_if(utils::packageVersion("nemeton") < "2026.10.5")
  p <- .t2_parcels()
  p$N2 <- c(70, 40)
  p$T1 <- c(120, 30)
  t2 <- suppressMessages(suppressWarnings(nemeton::indicateur_t2_changement(p)))$T2
  expect_true(all(is.na(t2)))
})

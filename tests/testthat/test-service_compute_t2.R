# T2 (stabilite) : sources N2 / T1 transmises par l'app (brief coeur 0.212.0 §4).
# Depuis nemeton 0.212.0, T2 rend NA sans source ; l'app ne lui passait ni N2
# ni T1, d'ou un T2 NA sur tous les projets.

.t2_parcels <- function() {
  sq <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 1, 0), c(x + 1, 1),
                                              c(x, 1), c(x, 0))))
  sf::st_sf(id = c("p1", "p2"), geometry = sf::st_sfc(sq(0), sq(2), crs = 2154))
}

test_that(".order_indicators_for_dependencies puts N2 just before T2", {
  ord <- nemetonshiny:::.order_indicators_for_dependencies
  canon <- c("indicateur_t1_anciennete", "indicateur_t2_changement",
             "indicateur_t3_coupes_rases", "indicateur_r1_feu",
             "indicateur_n1_distance", "indicateur_n2_continuite",
             "indicateur_n3_naturalite")
  got <- ord(canon)
  expect_identical(got, c("indicateur_t1_anciennete", "indicateur_n2_continuite",
                          "indicateur_t2_changement", "indicateur_t3_coupes_rases",
                          "indicateur_r1_feu", "indicateur_n1_distance",
                          "indicateur_n3_naturalite"))
  expect_setequal(got, canon)
  # Sans l'un des deux, ou deja dans le bon ordre : inchange
  expect_identical(ord(setdiff(canon, "indicateur_n2_continuite")),
                   setdiff(canon, "indicateur_n2_continuite"))
  expect_identical(ord(got), got)
  expect_identical(ord(character(0)), character(0))
})

test_that(".units_for_indicator hands N2 and T1 to T2", {
  f <- nemetonshiny:::.units_for_indicator
  p <- .t2_parcels()
  res <- data.frame(indicateur_n2_continuite = c(70, 40),
                    indicateur_t1_anciennete = c(120, 30))
  u <- f("indicateur_t2_changement", p, res, list())
  expect_identical(u$N2, c(70, 40))
  expect_identical(u$T1, c(120, 30))

  # N2 tout NA : pas transmis (le coeur le prendrait et rendrait NA partout)
  res$indicateur_n2_continuite <- c(NA_real_, NA_real_)
  u <- f("indicateur_t2_changement", p, res, list())
  expect_null(u$N2)
  expect_identical(u$T1, c(120, 30))

  # Longueur incoherente ou absent : rien
  u <- f("indicateur_t2_changement", p, data.frame(indicateur_t1_anciennete = 1), list())
  expect_null(u$T1)
  expect_null(u$N2)

  # Les autres indicateurs recoivent les parcelles telles quelles
  expect_identical(f("indicateur_t1_anciennete", p, res, list()), p)
})

test_that("compute_all_indicators computes N2 before T2 and T2 is no longer NA", {
  skip_if_not_installed("sf")
  p <- .t2_parcels()
  layers <- structure(list(rasters = list(), vectors = list(), point_clouds = list(),
                           bbox = c(0, 0, 3, 1), crs = sf::st_crs(2154),
                           cache_dir = tempdir(), warnings = list()),
                      class = "nemeton_layers")
  ordre <- character(0)
  with_mocked_bindings(
    load_indicators = function(id) NULL,
    is_cancelled = function(id) FALSE,
    save_indicators_incremental = function(...) TRUE,
    compute_single_indicator = function(indicator, parcels, layers) {
      ordre <<- c(ordre, indicator)
      switch(indicator,
        indicateur_t1_anciennete = c(150, 30),
        indicateur_n2_continuite = c(80, 45),
        # Le vrai T2 du coeur, sur les unites que l'app lui passe
        indicateur_t2_changement = suppressMessages(
          nemeton::indicateur_t2_changement(parcels)),
        rep(50, nrow(parcels)))
    },
    {
      res <- suppressMessages(nemetonshiny:::compute_all_indicators(
        parcels = p, layers = layers,
        indicators = c("indicateur_t1_anciennete", "indicateur_t2_changement",
                       "indicateur_n2_continuite"),
        project_id = "test"))
    }
  )
  expect_lt(match("indicateur_n2_continuite", ordre),
            match("indicateur_t2_changement", ordre))
  # N2 est la source prioritaire de T2
  expect_equal(res$indicateur_t2_changement, c(80, 45))
})

test_that("T2 falls back to T1 when N2 is unavailable", {
  skip_if_not_installed("sf")
  p <- .t2_parcels()
  u <- nemetonshiny:::.units_for_indicator(
    "indicateur_t2_changement", p,
    data.frame(indicateur_n2_continuite = c(NA, NA),
               indicateur_t1_anciennete = c(150, 30)), list())
  expect_equal(suppressMessages(nemeton::indicateur_t2_changement(u)), c(100, 30))
})

test_that("T2 falls back to T1 unit by unit when N2 is partly missing (nemeton >= 0.212.1)", {
  skip_if_not_installed("sf")
  skip_if(utils::packageVersion("nemeton") < "0.212.1")
  p <- .t2_parcels()
  u <- nemetonshiny:::.units_for_indicator(
    "indicateur_t2_changement", p,
    data.frame(indicateur_n2_continuite = c(70, NA),
               indicateur_t1_anciennete = c(150, 30)), list())
  expect_equal(suppressMessages(nemeton::indicateur_t2_changement(u)), c(70, 30))
})

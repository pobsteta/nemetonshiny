# Cablage de la grille INSEE, la seule entree de S3.
#
# Depuis `nemeton 0.187.0` le coeur ne fabrique plus aucune valeur de
# population : sans grille, S3 vaut NA - et c'est la reponse, pas une panne.
# L'ancien chemin « proxy » rendait `surface_du_tampon x 100 hab/km2`, un nombre
# qui variait plausiblement avec la taille de l'unite et passait donc pour une
# mesure.

test_that("la couche population est declaree comme les autres sources vectorielles", {
  v <- nemetonshiny:::DATA_SOURCES$vectors
  expect_true("population" %in% names(v))
  expect_identical(v$population$type, "vector")
  expect_identical(v$population$source, "insee_filosofi")
  expect_identical(v$population$required_for, "indicateur_s3_population")
})

test_that("la grille est INJECTEE dans l'appel de l'indicateur", {
  # LE point du cablage, et le seul qui decide. Le dispatcher filtre les
  # arguments sur les formals de la fonction cible ; `indicateur_s3_population()`
  # declare `population_grid` mais ni `layers` ni `...`. Sans cette injection
  # nommee, la grille peut etre telechargee, mise en cache et resolue : elle
  # n'atteint jamais l'indicateur, et S3 reste NA en silence.
  f <- testthat::test_path("..", "..", "R", "service_compute.R")
  testthat::skip_if_not(file.exists(f), "sources R absentes")
  code <- readLines(f, warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]

  expect_true(any(grepl('args$population_grid <- layers$population',
                        code, fixed = TRUE)))
  # Conditionnee a la presence de la couche : sans grille, on ne passe pas NULL
  # explicitement, on ne passe rien - la valeur par defaut du coeur.
  expect_true(any(grepl('"population_grid" %in% func_args', code, fixed = TRUE)))
  # Et le telechargement est branche sur le nom de source declare.
  expect_true(any(grepl('"insee_filosofi" = download_insee_population',
                        code, fixed = TRUE)))
})

test_that("download_insee_population degrade sans jamais fabriquer de valeur", {
  # Un coeur trop ancien, une source injoignable : NULL, un avertissement, et
  # S3 restera NA. Jamais un nombre invente.
  d <- withr::local_tempdir()
  cible <- file.path(d, "population.gpkg")

  # Entree inexploitable : ni sf, ni bbox.
  expect_null(nemetonshiny:::download_insee_population("pas une emprise", cible))
  expect_false(file.exists(cible))

  # Le coeur rend NULL (source injoignable) : on n'ecrit rien.
  testthat::local_mocked_bindings(
    load_insee_population_source = function(...) NULL, .package = "nemeton")
  aoi <- sf::st_as_sfc(sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 1, ymax = 1),
                                   crs = 2154))
  expect_null(nemetonshiny:::download_insee_population(aoi, cible))
  expect_false(file.exists(cible))
})

test_that("download_insee_population met en cache ce qu'il rend", {
  # Le coeur cache la grille NATIONALE (~52 Mo) ; ce cache-ci porte l'extrait
  # decoupe, pour qu'un recalcul du projet ne relise pas 52 Mo afin d'y
  # retailler les memes cellules.
  skip_if_not_installed("sf")
  d <- withr::local_tempdir()
  cible <- file.path(d, "population.gpkg")
  carre <- sf::st_sf(
    ind = 42,
    geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(0, 0), c(1000, 0), c(1000, 1000), c(0, 1000), c(0, 0)))), crs = 2154))

  testthat::local_mocked_bindings(
    load_insee_population_source = function(...) carre, .package = "nemeton")
  aoi <- sf::st_as_sfc(sf::st_bbox(carre))
  out <- nemetonshiny:::download_insee_population(aoi, cible)

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 1L)
  expect_true(file.exists(cible))
  expect_equal(sf::st_read(cible, quiet = TRUE)$ind, 42)
})

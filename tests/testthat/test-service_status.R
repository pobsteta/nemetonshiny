# Tests for the source status layer (spec 032, brief A5)
# R/service_status.R

# ==============================================================================
# Degradation when the core cannot answer
# ==============================================================================

test_that("theia_source_status_safe returns NULL when the core lacks the accessor", {
  testthat::local_mocked_bindings(.theia_status_available = function() FALSE)

  aoi <- sf::st_as_sf(data.frame(x = 1, y = 1), coords = c("x", "y"), crs = 4326)

  # NULL veut dire « je ne sais pas », jamais « tout va bien » : l'appelant doit
  # garder son comportement precedent plutot qu'afficher une cause inventee.
  expect_null(theia_source_status_safe("theia_lst", aoi))
})


test_that("theia_source_status_safe returns NULL on an empty or absent AOI", {
  testthat::local_mocked_bindings(.theia_status_available = function() TRUE)

  expect_null(theia_source_status_safe("theia_lst", NULL))
  expect_null(theia_source_status_safe("theia_lst", data.frame(a = 1)))
})


test_that("a core that throws is reported as an error, not as coverage", {
  testthat::local_mocked_bindings(
    .theia_status_available = function() TRUE,
    .theia_status_call = function(source_key, aoi) stop("catalogue injoignable")
  )

  aoi <- sf::st_as_sf(data.frame(x = 1, y = 1), coords = c("x", "y"), crs = 4326)
  st <- suppressWarnings(theia_source_status_safe("theia_lst", aoi))

  expect_equal(st$reason, "error")
  expect_false(st$available)
})


test_that("an unknown reason degrades to error rather than to a blank label", {
  testthat::local_mocked_bindings(
    .theia_status_available = function() TRUE,
    .theia_status_call = function(source_key, aoi) {
      list(available = FALSE, reason = "quelque_chose_de_neuf", n_assets = 0L)
    }
  )

  aoi <- sf::st_as_sf(data.frame(x = 1, y = 1), coords = c("x", "y"), crs = 4326)
  expect_equal(theia_source_status_safe("theia_lst", aoi)$reason, "error")
})


test_that("a well-formed answer is passed through and coerced", {
  testthat::local_mocked_bindings(
    .theia_status_available = function() TRUE,
    .theia_status_call = function(source_key, aoi) {
      list(available = TRUE, reason = "ok", n_assets = "8",
           collection = "thermocity-lst")
    }
  )

  aoi <- sf::st_as_sf(data.frame(x = 1, y = 1), coords = c("x", "y"), crs = 4326)
  st <- theia_source_status_safe("theia_lst", aoi)

  expect_true(st$available)
  expect_equal(st$reason, "ok")
  expect_identical(st$n_assets, 8L)
})


# ==============================================================================
# Persistence
# ==============================================================================

test_that("a status survives a write/read round-trip", {
  withr::with_tempdir({
    dir.create("data")
    save_source_status(".", "theia_lst",
                       list(reason = "no_asset_over_aoi", available = FALSE,
                            n_assets = 0L))

    st <- load_source_status(".", "theia_lst")
    expect_equal(st$reason, "no_asset_over_aoi")
    expect_false(st$available)
    expect_true(nzchar(st$checked_at))
  })
})


test_that("two sources coexist in the same file", {
  withr::with_tempdir({
    save_source_status(".", "theia_lst",
                       list(reason = "no_asset_over_aoi", available = FALSE))
    save_source_status(".", "sufosat",
                       list(reason = "ok", available = TRUE, n_assets = 3L))

    expect_equal(load_source_status(".", "theia_lst")$reason, "no_asset_over_aoi")
    expect_equal(load_source_status(".", "sufosat")$reason, "ok")
    expect_equal(load_source_status(".", "sufosat")$n_assets, 3L)
  })
})


test_that("load_source_status returns NULL when nothing was recorded", {
  withr::with_tempdir({
    expect_null(load_source_status(".", "theia_lst"))
    save_source_status(".", "sufosat", list(reason = "ok", available = TRUE))
    expect_null(load_source_status(".", "theia_lst"))
  })
})


# ==============================================================================
# Translation into a user-facing state
# ==============================================================================

test_that("source_status_message distinguishes the three states", {
  i18n <- get_i18n("fr")

  ok <- source_status_message(
    list(reason = "ok", available = TRUE, n_assets = 8L), i18n)
  expect_equal(ok$level, "ok")
  expect_true(grepl("8", ok$text, fixed = TRUE))

  # Hors couverture : information, PAS avertissement - c'est le cas normal
  # d'une commune forestiere.
  none <- source_status_message(
    list(reason = "no_asset_over_aoi", available = FALSE, n_assets = 0L), i18n)
  expect_equal(none$level, "info")
  expect_true(nzchar(none$text))

  err <- source_status_message(
    list(reason = "no_stac_collection", available = FALSE), i18n)
  expect_equal(err$level, "error")
  expect_true(grepl("no_stac_collection", err$text, fixed = TRUE))

  # Les deux ne doivent surtout pas se confondre : c'est tout l'objet du brief.
  expect_false(identical(none$level, err$level))
  expect_false(identical(none$text, err$text))
})


test_that("an unknown scene count drops the number instead of printing NA", {
  i18n <- get_i18n("fr")
  msg <- source_status_message(
    list(reason = "ok", available = TRUE, n_assets = NA_integer_), i18n)

  expect_equal(msg$level, "ok")
  expect_false(grepl("NA", msg$text, fixed = TRUE))
})


test_that("missing credentials keep their own upstream message", {
  i18n <- get_i18n("fr")
  # `lst_need_theia` est actionnable et s'affiche en amont ; le repeter ici en
  # ferait une panne alors que c'est une etape de configuration.
  expect_null(source_status_message(
    list(reason = "no_credentials", available = FALSE), i18n))
})


test_that("source_status_message returns NULL on an unknown status", {
  i18n <- get_i18n("fr")
  expect_null(source_status_message(NULL, i18n))
  expect_null(source_status_message(list(available = FALSE), i18n))
})


test_that("every reason has a translation in both languages", {
  for (lang in c("fr", "en")) {
    i18n <- get_i18n(lang)
    for (r in SOURCE_STATUS_REASONS) {
      msg <- source_status_message(list(reason = r, n_assets = 1L), i18n)
      if (is.null(msg)) next  # no_credentials, traite en amont
      expect_true(nzchar(msg$text), info = paste(lang, r))
      # Une cle non traduite se renvoie elle-meme : le test tomberait ici.
      expect_false(msg$text %in% names(nemetonshiny:::TRANSLATIONS),
                   info = paste(lang, r))
    }
  }
})


# ==============================================================================
# build_lst_layer: short-circuit and recorded cause (§4.2)
# ==============================================================================

.lst_aoi <- function() {
  sf::st_as_sf(
    data.frame(id = 1,
               geom = sf::st_sfc(sf::st_polygon(list(rbind(
                 c(850000, 6900000), c(851000, 6900000),
                 c(851000, 6901000), c(850000, 6901000),
                 c(850000, 6900000)))), crs = 2154)))
}

test_that("build_lst_layer records the cause and skips the download", {
  skip_if_not_installed("sf")

  called <- FALSE
  testthat::local_mocked_bindings(
    theia_api_key_configured = function() TRUE,
    theia_source_status_safe = function(source_key, aoi) {
      list(available = FALSE, reason = "no_asset_over_aoi", n_assets = 0L)
    }
  )
  testthat::local_mocked_bindings(
    load_theia_source = function(...) {
      called <<- TRUE
      stop("ne doit pas etre appele")
    },
    .package = "nemeton"
  )

  withr::with_tempdir({
    dir.create("data", recursive = TRUE)
    out <- build_lst_layer(list(enabled = TRUE), ".", .lst_aoi())

    expect_null(out)
    # Le court-circuit est l'objet du §4.2 : on ne paie plus une requete STAC
    # pour un resultat connu d'avance.
    expect_false(called)
    expect_equal(load_source_status(".", "theia_lst")$reason, "no_asset_over_aoi")
  })
})


test_that("build_lst_layer records missing credentials as such", {
  skip_if_not_installed("sf")

  testthat::local_mocked_bindings(theia_api_key_configured = function() FALSE)

  withr::with_tempdir({
    dir.create("data", recursive = TRUE)
    out <- suppressWarnings(build_lst_layer(list(enabled = TRUE), ".", .lst_aoi()))

    expect_null(out)
    expect_equal(load_source_status(".", "theia_lst")$reason, "no_credentials")
  })
})


test_that("an old core still reaches the download and records an error on failure", {
  skip_if_not_installed("sf")

  called <- FALSE
  testthat::local_mocked_bindings(
    theia_api_key_configured = function() TRUE,
    # NULL = le coeur ne sait pas repondre : pas de court-circuit.
    theia_source_status_safe = function(source_key, aoi) NULL
  )
  testthat::local_mocked_bindings(
    load_theia_source = function(...) {
      called <<- TRUE
      stop("reseau indisponible")
    },
    .package = "nemeton"
  )

  withr::with_tempdir({
    dir.create("data", recursive = TRUE)
    out <- suppressWarnings(build_lst_layer(list(enabled = TRUE), ".", .lst_aoi()))

    expect_null(out)
    expect_true(called)
    # La cause reste indistincte, mais elle est enregistree comme panne plutot
    # que laissee muette.
    expect_equal(load_source_status(".", "theia_lst")$reason, "error")
  })
})

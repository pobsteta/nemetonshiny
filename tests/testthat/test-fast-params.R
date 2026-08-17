# Tests for the FAST calibration parameters moved to the settings modal
# R/service_project.R - project_fast_params() / set_project_fast_params()

# ==============================================================================
# Reading: absent metadata must behave exactly as the old sliders did
# ==============================================================================

test_that("absent metadata yields the former slider defaults", {
  for (meta in list(NULL, list(), list(fast_params = NULL))) {
    fp <- project_fast_params(meta)
    # Ce sont les valeurs qui etaient codees dans les sliders du sidebar : un
    # projet qui n'a jamais ouvert la modale doit se comporter comme avant.
    expect_equal(fp$threshold_ndvi, 0.40)
    expect_equal(fp$threshold_nbr, 0.30)
    expect_equal(fp$threshold_ndmi, 0.20)
    expect_identical(fp$window_days, 30L)
  }
})


test_that("stored values are read back", {
  fp <- project_fast_params(list(fast_params = list(
    threshold_ndvi = 0.55, threshold_nbr = 0.45,
    threshold_ndmi = 0.25, window_days = 45)))

  expect_equal(fp$threshold_ndvi, 0.55)
  expect_equal(fp$threshold_nbr, 0.45)
  expect_equal(fp$threshold_ndmi, 0.25)
  expect_identical(fp$window_days, 45L)
})


test_that("a partial record falls back per field, not wholesale", {
  # Une seule valeur enregistree ne doit pas faire perdre les trois autres.
  fp <- project_fast_params(list(fast_params = list(threshold_ndvi = 0.7)))

  expect_equal(fp$threshold_ndvi, 0.7)
  expect_equal(fp$threshold_nbr, 0.30)
  expect_equal(fp$threshold_ndmi, 0.20)
  expect_identical(fp$window_days, 30L)
})


test_that("unusable values fall back rather than propagating NA", {
  # Un NA dans les seuils rendrait le raster d'alerte entierement vide, sans
  # que rien ne le signale : mieux vaut le defaut.
  fp <- project_fast_params(list(fast_params = list(
    threshold_ndvi = NA, threshold_nbr = "abc",
    threshold_ndmi = character(0), window_days = NULL)))

  expect_equal(fp$threshold_ndvi, 0.40)
  expect_equal(fp$threshold_nbr, 0.30)
  expect_equal(fp$threshold_ndmi, 0.20)
  expect_identical(fp$window_days, 30L)
})


test_that("window_days is always an integer", {
  fp <- project_fast_params(list(fast_params = list(window_days = 44.6)))
  expect_identical(fp$window_days, 45L)
  expect_type(fp$window_days, "integer")
})


# ==============================================================================
# Writing
# ==============================================================================

test_that("set_project_fast_params persists and round-trips", {
  skip_if_not_installed("withr")

  withr::with_tempdir({
    withr::local_options(list(nemeton.app_options = list(project_dir = ".")))

    proj <- tryCatch(create_project(name = "seuils", commune = "X"),
                     error = function(e) NULL)
    skip_if(is.null(proj$id), "create_project indisponible dans ce contexte")

    set_project_fast_params(proj$id, threshold_ndvi = 0.52,
                            threshold_nbr = 0.41, threshold_ndmi = 0.22,
                            window_days = 21)

    # Le projet de test n'a pas de parcelles : la migration v1->v2 le signale,
    # ce qui n'a rien a voir avec ce qu'on verifie ici.
    reread <- suppressWarnings(load_project(proj$id))
    fp <- project_fast_params(reread$metadata)

    expect_equal(fp$threshold_ndvi, 0.52)
    expect_equal(fp$threshold_nbr, 0.41)
    expect_equal(fp$threshold_ndmi, 0.22)
    expect_identical(fp$window_days, 21L)
    # Horodatage conserve, comme pour sufosat / lst_urbain.
    expect_true(nzchar(reread$metadata$fast_params$set_at))
  })
})


test_that("set_project_fast_params rejects an unknown project", {
  expect_error(set_project_fast_params("pas-un-projet", threshold_ndvi = 0.5))
})


# ==============================================================================
# The sidebar no longer owns these widgets
# ==============================================================================

test_that("the Suivi sanitaire sidebar no longer carries the four widgets", {
  html <- as.character(mod_monitoring_ui("mon"))

  # Deplaces dans la modale : leur presence ici signifierait un doublon, donc
  # deux sources de verite pour le meme seuil.
  for (id in c("mon-threshold_ndvi", "mon-threshold_nbr",
               "mon-threshold_ndmi", "mon-window_days")) {
    expect_false(grepl(id, html, fixed = TRUE), info = id)
  }

  # La periode d'observation, elle, est RESTEE : c'est le geste courant.
  expect_true(grepl("mon-date_range", html, fixed = TRUE))
  # Et le rappel des seuils en vigueur prend leur place.
  expect_true(grepl("mon-fast_params_recap", html, fixed = TRUE))
})


test_that("the settings tab carries the four widgets and a save button", {
  html <- as.character(mod_sources_config_ui("src"))

  expect_true(grepl("src-fast_block", html, fixed = TRUE))
})


test_that("the settings tab is no longer named after sources alone", {
  # Il accueille desormais des calibrages, qui ne sont pas des sources.
  for (lang in c("fr", "en")) {
    label <- get_i18n(lang)$t("api_keys_tab_sources")
    expect_true(nzchar(label))
    expect_false(identical(label, "Sources optionnelles"))
    expect_false(identical(label, "Optional sources"))
  }
})


test_that("every new key is translated in both languages", {
  keys <- c("fast_params_section", "fast_params_hint", "fast_params_save",
            "fast_params_saved", "fast_params_where", "api_keys_tab_sources")
  for (lang in c("fr", "en")) {
    i18n <- get_i18n(lang)
    for (k in keys) {
      expect_true(i18n$has(k), info = paste(lang, k))
      # Une cle non traduite se renvoie elle-meme.
      expect_false(identical(i18n$t(k), k), info = paste(lang, k))
    }
  }
})

# Tests for the family labels sourced from the core package
# R/utils_i18n.R - .core_family_translations() / .translations()

# ==============================================================================
# The core overlay reaches i18n$t()
# ==============================================================================

test_that("family labels come from the core, not from the static dictionary", {
  fake <- data.frame(
    family_column = c("famille_risque", "famille_social"),
    name_fr = c("Risques (coeur)", "Social (coeur)"),
    name_en = c("Risks (core)", "Social (core)"),
    description_fr = c("Description R du coeur", "Description S du coeur"),
    description_en = c("Core R description", "Core S description"),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    indicator_families = function(...) fake,
    .package = "nemeton"
  )
  nemetonshiny:::.reset_i18n_cache()
  withr::defer(nemetonshiny:::.reset_i18n_cache())

  i18n <- nemetonshiny:::get_i18n("fr")
  expect_equal(i18n$t("famille_risque"), "Risques (coeur)")
  expect_equal(i18n$t("famille_risque_desc"), "Description R du coeur")

  i18n_en <- nemetonshiny:::get_i18n("en")
  expect_equal(i18n_en$t("famille_social"), "Social (core)")
  expect_equal(i18n_en$t("famille_social_desc"), "Core S description")

  # Une famille absente du coeur mocke garde son entree statique : l'overlay
  # complete, il ne remplace pas le dictionnaire.
  expect_equal(i18n$t("famille_carbone"), nemetonshiny:::TRANSLATIONS$famille_carbone$fr)
})


# ==============================================================================
# Graceful degradation
# ==============================================================================

test_that("a core that errors leaves the static dictionary untouched", {
  testthat::local_mocked_bindings(
    indicator_families = function(...) stop("coeur trop ancien"),
    .package = "nemeton"
  )
  nemetonshiny:::.reset_i18n_cache()
  withr::defer(nemetonshiny:::.reset_i18n_cache())

  expect_equal(nemetonshiny:::.core_family_translations(), list())

  i18n <- nemetonshiny:::get_i18n("fr")
  expect_equal(
    i18n$t("famille_risque"),
    nemetonshiny:::TRANSLATIONS$famille_risque$fr
  )
  expect_equal(
    i18n$t("famille_risque_desc"),
    nemetonshiny:::TRANSLATIONS$famille_risque_desc$fr
  )
})


test_that("a core missing the expected columns is ignored", {
  testthat::local_mocked_bindings(
    indicator_families = function(...) {
      data.frame(code = c("R", "S"), name = c("Risques", "Social"),
                 stringsAsFactors = FALSE)
    },
    .package = "nemeton"
  )
  nemetonshiny:::.reset_i18n_cache()
  withr::defer(nemetonshiny:::.reset_i18n_cache())

  expect_equal(nemetonshiny:::.core_family_translations(), list())
})


test_that("empty or NA core labels never blank out the fallback", {
  fake <- data.frame(
    family_column = c("famille_risque", "famille_social"),
    name_fr = c(NA_character_, "Social (coeur)"),
    name_en = c("Risks (core)", "Social (core)"),
    description_fr = c("Description R du coeur", ""),
    description_en = c("Core R description", "Core S description"),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    indicator_families = function(...) fake,
    .package = "nemeton"
  )
  nemetonshiny:::.reset_i18n_cache()
  withr::defer(nemetonshiny:::.reset_i18n_cache())

  overlay <- nemetonshiny:::.core_family_translations()

  # name_fr est NA -> le nom n'est pas ecrase, mais sa description l'est.
  expect_false("famille_risque" %in% names(overlay))
  expect_equal(overlay$famille_risque_desc$fr, "Description R du coeur")

  # description_fr est vide -> la description n'est pas ecrasee, le nom l'est.
  expect_equal(overlay$famille_social$fr, "Social (coeur)")
  expect_false("famille_social_desc" %in% names(overlay))

  i18n <- nemetonshiny:::get_i18n("fr")
  expect_equal(
    i18n$t("famille_risque"),
    nemetonshiny:::TRANSLATIONS$famille_risque$fr
  )
})


# ==============================================================================
# Drift guard: the static fallback must still match the core
# ==============================================================================

test_that("the static famille_* entries still match the core", {
  skip_if_not_installed("nemeton")

  fams <- tryCatch(nemeton::indicator_families(), error = function(e) NULL)
  skip_if(
    !is.data.frame(fams) || nrow(fams) == 0,
    "nemeton::indicator_families() unavailable"
  )

  # Ce test est la raison d'etre du repli statique : il n'a de valeur que s'il
  # echoue quand le coeur bouge. Sans lui, une famille renommee cote coeur
  # s'afficherait avec l'ancien libelle sur un poste ou l'accesseur echoue.
  for (i in seq_len(nrow(fams))) {
    key <- fams$family_column[i]
    static_name <- nemetonshiny:::TRANSLATIONS[[key]]
    static_desc <- nemetonshiny:::TRANSLATIONS[[paste0(key, "_desc")]]

    expect_false(is.null(static_name), info = paste("Missing fallback key:", key))
    expect_false(
      is.null(static_desc),
      info = paste("Missing fallback key:", paste0(key, "_desc"))
    )

    expect_equal(static_name$fr, fams$name_fr[i], info = paste(key, "fr"))
    expect_equal(static_name$en, fams$name_en[i], info = paste(key, "en"))
    expect_equal(static_desc$fr, fams$description_fr[i], info = paste(key, "desc fr"))
    expect_equal(static_desc$en, fams$description_en[i], info = paste(key, "desc en"))
  }
})


test_that("the merged dictionary keeps every static key", {
  nemetonshiny:::.reset_i18n_cache()
  withr::defer(nemetonshiny:::.reset_i18n_cache())

  dict <- nemetonshiny:::.translations()
  expect_true(all(names(nemetonshiny:::TRANSLATIONS) %in% names(dict)))

  i18n <- nemetonshiny:::get_i18n("fr")
  expect_true(i18n$has("famille_naturalite_desc"))
  expect_true(i18n$has("app_title"))
})

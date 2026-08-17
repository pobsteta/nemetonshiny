# Tests for the applicability verdicts (brief « trois derniers points », point 1)
# R/service_applicabilite.R

test_that("applicabilite_safe returns NULL when the core cannot judge", {
  testthat::local_mocked_bindings(.applicabilite_available = function(fn) FALSE)
  # NULL veut dire « je ne sais pas », jamais « non applicable ».
  expect_null(applicabilite_safe("r5_applicabilite", units = NULL))
})


test_that("a core that throws becomes an error verdict, not a skip", {
  testthat::local_mocked_bindings(
    .applicabilite_available = function(fn) TRUE,
    .applicabilite_call = function(fn, ...) stop("boom")
  )
  v <- suppressWarnings(applicabilite_safe("a5_applicabilite", units = NULL))
  expect_equal(v$status, "error")
  # Une panne ne doit pas faire sauter le calcul.
  expect_false(applicabilite_skip(v))
})


test_that("a malformed answer is treated as unknown", {
  for (bad in list(list(), list(status = ""), "pas une liste", NULL)) {
    testthat::local_mocked_bindings(
      .applicabilite_available = function(fn) TRUE,
      .applicabilite_call = function(fn, ...) bad
    )
    expect_null(applicabilite_safe("r5_applicabilite", units = NULL))
  }
})


test_that("only three verdicts short-circuit the computation", {
  for (s in c("not_applicable", "no_species", "no_coverage")) {
    expect_true(applicabilite_skip(list(status = s)), info = s)
  }
  for (s in c("eligible_fordead", "eligible_fordead_out_of_calibration",
              "eligible_reconfort", "eligible", "eligible_partial",
              "no_reference", "no_credentials", "error")) {
    expect_false(applicabilite_skip(list(status = s)), info = s)
  }
  # Verdict inconnu : on calcule. Ne pas savoir n'est jamais une raison de
  # sauter, le coût d'un calcul inutile étant une colonne de NA quand celui
  # d'un saut erroné est un indicateur perdu.
  expect_false(applicabilite_skip(NULL))
})


test_that("out_of_calibration is information, never a refusal", {
  i18n <- get_i18n("fr")
  msg <- applicabilite_message(
    "r5", list(status = "eligible_fordead_out_of_calibration",
               n_units = 30, n_fordead = 30), i18n)

  # Le point le plus important du brief : hors zone ONF/DSF, le calcul tourne.
  expect_equal(msg$level, "info")
  expect_false(applicabilite_skip(list(status = msg$status)))
  expect_true(grepl("ONF/DSF", msg$text, fixed = TRUE))
  # Et le message doit dire pourquoi c'est utilisable, pas « non calculable ».
  expect_true(grepl("extrapol", msg$text, fixed = TRUE))
})


test_that("the three levels are distinguished", {
  i18n <- get_i18n("fr")
  expect_equal(applicabilite_message("r5", list(status = "eligible_fordead"), i18n)$level, "ok")
  expect_equal(applicabilite_message("a5", list(status = "no_coverage"), i18n)$level, "info")
  expect_equal(applicabilite_message("a5", list(status = "error"), i18n)$level, "error")
})


test_that("counts are appended when the core supplies them, not invented", {
  i18n <- get_i18n("fr")

  with_counts <- applicabilite_message(
    "a5", list(status = "eligible_partial", n_units = 30, n_eligible = 7), i18n)
  expect_true(grepl("7", with_counts$text, fixed = TRUE))
  expect_true(grepl("30", with_counts$text, fixed = TRUE))

  # Sans compte : pas de « 0 sur 0 » inventé.
  without <- applicabilite_message("a5", list(status = "eligible_partial"), i18n)
  expect_false(grepl(" 0 ", without$text, fixed = TRUE))
})


test_that("an unknown status falls back on the error wording", {
  i18n <- get_i18n("fr")
  msg <- applicabilite_message("r5", list(status = "quelque_chose_de_neuf"), i18n)
  expect_equal(msg$text, i18n$t("r5_appl_error"))
  # Une cause inconnue ne doit pas afficher sa clé brute.
  expect_false(grepl("quelque_chose", msg$text, fixed = TRUE))
})


test_that("applicabilite_message returns NULL on an unknown verdict", {
  i18n <- get_i18n("fr")
  expect_null(applicabilite_message("r5", NULL, i18n))
  expect_null(applicabilite_message("r5", list(n_units = 3), i18n))
})


test_that("every documented verdict is translated in both languages", {
  vocab <- list(
    r5 = c("eligible_fordead", "eligible_fordead_out_of_calibration",
           "eligible_reconfort", "no_species", "not_applicable", "error"),
    a5 = c("eligible", "eligible_partial", "no_coverage", "no_reference",
           "no_credentials", "error")
  )
  for (lang in c("fr", "en")) {
    i18n <- get_i18n(lang)
    for (kind in names(vocab)) {
      for (s in vocab[[kind]]) {
        key <- paste0(kind, "_appl_", s)
        expect_true(i18n$has(key), info = paste(lang, key))
        expect_false(identical(i18n$t(key), key), info = paste(lang, key))
      }
    }
  }
})


test_that("the badge renders the level it is given", {
  i18n <- get_i18n("fr")
  ok <- as.character(nemetonshiny:::.applicabilite_badge(
    applicabilite_message("r5", list(status = "eligible_fordead"), i18n)))
  info <- as.character(nemetonshiny:::.applicabilite_badge(
    applicabilite_message("a5", list(status = "no_coverage"), i18n)))
  err <- as.character(nemetonshiny:::.applicabilite_badge(
    applicabilite_message("a5", list(status = "error"), i18n)))

  expect_true(grepl("text-success", ok, fixed = TRUE))
  # Hors couverture : information neutre, PAS un avertissement.
  expect_true(grepl("text-info", info, fixed = TRUE))
  expect_false(grepl("text-warning", info, fixed = TRUE))
  expect_true(grepl("text-warning", err, fixed = TRUE))

  expect_null(nemetonshiny:::.applicabilite_badge(NULL))
})


.r5_base <- function() {
  sf::st_as_sf(data.frame(ug_id = c("a", "b"), x = 1:2, y = 1:2),
               coords = c("x", "y"), crs = 4326)
}

test_that("add_r5_to_indicators skips on not_applicable and records the cause", {
  skip_if_not_installed("sf")

  testthat::local_mocked_bindings(
    applicabilite_safe = function(fn, ...) list(status = "not_applicable")
  )

  out <- add_r5_to_indicators(.r5_base(),
                              list(metadata = list(monitoring_zone_id = 5)))

  # Aucune colonne de valeur : le calcul n'a pas tourné.
  expect_false("indicateur_r5_deperissement" %in% names(out))
  # Mais la cause est là, et sa clé tombe sur une traduction existante.
  expect_equal(unique(out$.r5_status), "appl_not_applicable")
  expect_true(get_i18n("fr")$has("r5_appl_not_applicable"))
})


test_that("no_species does NOT skip R5, because routing is by alert type", {
  skip_if_not_installed("sf")

  # LA regression rattrapée par les tests. `add_r5_to_indicators()` route chaque
  # UGF par le TYPE D'ALERTE qui l'intersecte, jamais par une colonne d'essence :
  # il fonctionne donc sur des unités d'essence inconnue, ce qui est le cas par
  # défaut. Or `r5_applicabilite(units)` sans BD Forêt rend précisément
  # `no_species` — sauter là-dessus désactivait en silence un chemin qui marche.
  reached <- FALSE
  testthat::local_mocked_bindings(
    applicabilite_safe = function(fn, ...) list(status = "no_species"),
    get_monitoring_db_connection = function(...) { reached <<- TRUE; NULL }
  )

  out <- add_r5_to_indicators(.r5_base(),
                              list(metadata = list(monitoring_zone_id = 5)))

  expect_true(reached)
  expect_false(".r5_status" %in% names(out))
})


test_that("out_of_calibration does not skip R5 either", {
  skip_if_not_installed("sf")

  reached <- FALSE
  testthat::local_mocked_bindings(
    applicabilite_safe = function(fn, ...) {
      list(status = "eligible_fordead_out_of_calibration")
    },
    get_monitoring_db_connection = function(...) { reached <<- TRUE; NULL }
  )

  add_r5_to_indicators(.r5_base(), list(metadata = list(monitoring_zone_id = 5)))
  expect_true(reached)
})


test_that("applicabilite_skip honours the caller's skip set", {
  # Le jeu generique reste celui du brief ; R5 en restreint volontairement la
  # portee, parce que le saut depend de COMMENT l'appelant calcule.
  expect_true(applicabilite_skip(list(status = "no_species")))
  expect_false(applicabilite_skip(list(status = "no_species"),
                                  skip = "not_applicable"))
  expect_true(applicabilite_skip(list(status = "not_applicable"),
                                 skip = "not_applicable"))
})


test_that("an unknown verdict does not skip R5", {
  skip_if_not_installed("sf")

  base <- sf::st_as_sf(data.frame(ug_id = "a", x = 1, y = 1),
                       coords = c("x", "y"), crs = 4326)
  reached <- FALSE

  testthat::local_mocked_bindings(
    applicabilite_safe = function(fn, ...) NULL,
    get_monitoring_db_connection = function(...) { reached <<- TRUE; NULL }
  )

  out <- add_r5_to_indicators(base, list(metadata = list(monitoring_zone_id = 5)))

  expect_true(reached)
  expect_false(".r5_status" %in% names(out))
})

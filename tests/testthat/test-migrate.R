

# ---- Marqueur de sens v3 : L1, T1, E1/E2 (spec 048 §9-§11) -----------
#
# Brief `2026-09-18-l1-sens-inverse.md`. Trois corrections du cœur
# (nemeton >= 0.197.0) changent l'echelle ou le sens de L1, T1, E1 et E2.
# Un parquet calcule avant reste LISIBLE : `compute_all_indicators()` le
# relirait et sauterait le recalcul en propageant des familles fausses.

test_that("INDICATOR_SENSE_VERSION est passe a 3", {
  # C'est ce seul entier qui declenche l'invalidation unique a la premiere
  # ouverture apres la montee de version.
  expect_identical(nemetonshiny:::INDICATOR_SENSE_VERSION, 3L)
})


test_that("un projet en v2 est invalide, un projet en v3 ne l'est plus", {
  withr::with_tempdir({
    dir.create(file.path("p1", "data"), recursive = TRUE)
    writeLines("x", file.path("p1", "data", "indicators.parquet"))
    vus <- list()

    testthat::with_mocked_bindings(
      load_project_metadata = function(id) {
        list(id = id, indicators_computed = TRUE,
             indicator_sense_version = vus$vue %||% 2L)
      },
      get_project_path = function(id) file.path(getwd(), "p1"),
      invalidate_indicators = function(id) { vus$invalide <<- TRUE; TRUE },
      update_project_metadata = function(id, updates, ...) {
        vus$ecrit <<- updates$indicator_sense_version; TRUE
      },
      {
        # v2 -> invalidation, et le marqueur passe a 3.
        vus$vue <- 2L
        expect_true(nemetonshiny:::ensure_indicator_sense_current("p1"))
        expect_true(isTRUE(vus$invalide))
        expect_identical(vus$ecrit, 3L)

        # v3 -> plus rien : sinon le test se rejouerait a chaque ouverture.
        vus$invalide <- NULL; vus$ecrit <- NULL; vus$vue <- 3L
        expect_false(nemetonshiny:::ensure_indicator_sense_current("p1"))
        expect_null(vus$invalide)
      }
    )
  })
})


test_that("la cle du message d'invalidation existe en FR et EN", {
  for (lg in c("fr", "en")) {
    i18n <- nemetonshiny:::get_i18n(lg)
    expect_true(i18n$has("indicateurs_invalides"))
    txt <- i18n$t("indicateurs_invalides")
    # Le message doit NOMMER les familles qui bougent : « recalculez » seul
    # ne dit pas a l'utilisateur que ses scores ne sont plus comparables.
    expect_true(nchar(txt) > 120L)
  }
  expect_match(nemetonshiny:::get_i18n("fr")$t("indicateurs_invalides"),
               "Paysage", fixed = TRUE)
  expect_match(nemetonshiny:::get_i18n("en")$t("indicateurs_invalides"),
               "Landscape", fixed = TRUE)
})


test_that("l'app n'inverse RIEN cote client (piege du brief, §1)", {
  # Le cœur rend deja L1 dans le bon sens. Une inversion cote app
  # annulerait la correction EN SILENCE - meme consigne que pour R5 en
  # 0.94.0 et R1-R4 en 0.181.0. Et le piege des slugs croises (spec 045)
  # ferait retourner le MORCELLEMENT sur les jeux non migres.
  src <- unlist(lapply(
    list.files(testthat::test_path("..", "..", "R"), pattern = "\\.R$",
               full.names = TRUE),
    function(f) readLines(f, warn = FALSE)))
  src <- src[!grepl("^\\s*#", src)]   # les commentaires n'inversent rien

  motifs <- c("100\\s*-\\s*[a-zA-Z_.$]*l1", "100\\s*-\\s*[a-zA-Z_.$]*paysage",
              "100\\s*-\\s*[a-zA-Z_.$]*lisiere", "100\\s*-\\s*[a-zA-Z_.$]*t1",
              "100\\s*-\\s*[a-zA-Z_.$]*e1", "100\\s*-\\s*[a-zA-Z_.$]*e2")
  for (m in motifs) expect_false(any(grepl(m, src)), label = m)
})

# Tests — canal de phase du moteur reGénération (engine_status.json)
#
# Couvre le plumbing app du message de phase en cours (notif bas-droite,
# spec 027 / brief engine-phase-status) : écriture atomique côté worker
# (.regen_write_phase), lecture/péremption côté module (.regen_read_phase) et
# libellés i18n des 6 phases + états terminaux (.regen_phase_label). Aucun run
# réel : on manipule directement le fichier d'état.

`%||%` <- function(a, b) if (is.null(a)) b else a

test_that(".regen_write_phase writes an atomic, well-formed JSON payload", {
  d <- withr::local_tempdir()
  nemetonshiny:::.regen_write_phase(d, "microclimf_moyenne",
                                    list(year = 2019, i = 2L, n = 5L))
  f <- file.path(d, "engine_status.json")
  expect_true(file.exists(f))
  # pas de fichier temporaire laissé traîner (rename atomique effectué)
  expect_false(file.exists(file.path(d, ".engine_status.json.tmp")))
  st <- jsonlite::fromJSON(f)
  expect_equal(st$phase, "microclimf_moyenne")
  expect_equal(st$year, 2019)
  expect_equal(st$i, 2)
  expect_equal(st$n, 5)
  expect_true(is.numeric(st$ts))
})

test_that(".regen_write_phase serialises absent source as JSON null", {
  d <- withr::local_tempdir()
  nemetonshiny:::.regen_write_phase(d, "pai", list(source = NA))
  # `null = \"null\"` + auto_unbox → NA sérialisé en null (cf. format du brief §1)
  raw <- readLines(file.path(d, "engine_status.json"))
  expect_match(raw, '"source":null', fixed = TRUE)
})

test_that(".regen_write_phase never throws on an unwritable directory", {
  # out_dir inexistant : tryCatch avale l'erreur (jamais fatal pour le moteur).
  # Un warning « cannot open » peut fuiter avant l'erreur — non bloquant : la
  # garantie du brief est « jamais fatal », pas « jamais de warning ».
  expect_no_error(suppressWarnings(
    nemetonshiny:::.regen_write_phase(file.path(tempdir(), "no", "such", "dir"),
                                      "grille")))
})

test_that(".regen_read_phase returns NULL when absent, and reads a fresh file", {
  expect_null(nemetonshiny:::.regen_read_phase(NULL))
  d <- withr::local_tempdir()
  # cache/regeneration/ n'existe pas encore → NULL
  expect_null(nemetonshiny:::.regen_read_phase(d))
  reg <- file.path(d, "cache", "regeneration")
  dir.create(reg, recursive = TRUE)
  nemetonshiny:::.regen_write_phase(reg, "exposition")
  st <- nemetonshiny:::.regen_read_phase(d)
  expect_equal(st$phase, "exposition")
})

test_that(".regen_read_phase drops a stale file (> 2 min without update)", {
  d <- withr::local_tempdir()
  reg <- file.path(d, "cache", "regeneration")
  dir.create(reg, recursive = TRUE)
  # ts vieux de > 120 s → périmé → NULL (worker mort, phase obsolète)
  writeLines(
    jsonlite::toJSON(list(phase = "biljou", ts = as.integer(Sys.time()) - 300L),
                     auto_unbox = TRUE),
    file.path(reg, "engine_status.json"))
  expect_null(nemetonshiny:::.regen_read_phase(d))
})

test_that(".regen_phase_label renders the 6 phases + terminal states (FR)", {
  i18n <- get_i18n("fr")
  lbl <- function(st) nemetonshiny:::.regen_phase_label(i18n, st)

  expect_equal(lbl(list(phase = "grille")), i18n$t("regen_phase_grille"))
  # PAI : source lidar vs satellite injectée dans le libellé
  expect_match(lbl(list(phase = "pai", source = "lidar")), "LiDAR")
  expect_match(lbl(list(phase = "pai", source = "raster")), "satellite")
  # microclimf avec year/i/n → suffixe « year (i/n) »
  expect_match(lbl(list(phase = "microclimf_moyenne", year = 2019, i = 2, n = 5)),
               "2019 (2/5)", fixed = TRUE)
  # microclimf sans compteur → libellé de base, pas de suffixe
  expect_equal(lbl(list(phase = "microclimf_canicule")),
               i18n$t("regen_phase_micro_can"))
  expect_equal(lbl(list(phase = "exposition")), i18n$t("regen_phase_exposition"))
  expect_equal(lbl(list(phase = "biljou")), i18n$t("regen_phase_biljou"))
  # phase sautée : la raison est injectée
  expect_match(lbl(list(phase = "microclimf_skipped",
                        reason = i18n$t("regen_phase_skip_reason_cds"))),
               i18n$t("regen_phase_skip_reason_cds"), fixed = TRUE)
  # done / phase inconnue → chaîne vide (le module retombe sur regen_engine_running)
  expect_equal(lbl(list(phase = "done")), "")
  expect_equal(lbl(list(phase = "wat")), "")
})

test_that(".regen_phase_label mirrors labels in EN", {
  i18n <- get_i18n("en")
  expect_match(nemetonshiny:::.regen_phase_label(
    i18n, list(phase = "pai", source = "raster")), "satellite")
  expect_equal(nemetonshiny:::.regen_phase_label(i18n, list(phase = "biljou")),
               i18n$t("regen_phase_biljou"))
})

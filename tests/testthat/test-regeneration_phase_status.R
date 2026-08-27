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

# Un fichier vieux n'est PLUS jeté : il l'était, et la notif retombait alors sur
# « Moteur en cours… ». Le cœur n'émet qu'un événement par année ERA5, puis
# télécharge douze mois sans un mot — la phase réelle disparaissait pendant
# l'heure et demie la plus longue du run. On la garde, et on date son silence.
.ecrire_phase_agee <- function(reg, phase, age_s) {
  writeLines(
    jsonlite::toJSON(list(phase = phase, ts = as.integer(Sys.time()) - age_s),
                     auto_unbox = TRUE),
    file.path(reg, "engine_status.json"))
}

test_that(".regen_read_phase keeps a stale file and dates its silence", {
  d <- withr::local_tempdir()
  reg <- file.path(d, "cache", "regeneration")
  dir.create(reg, recursive = TRUE)
  .ecrire_phase_agee(reg, "biljou", 300L)

  st <- nemetonshiny:::.regen_read_phase(d)
  expect_equal(st$phase, "biljou")
  # ~300 s : la lecture porte l'âge, elle ne le juge pas.
  expect_true(abs(st$stale_s - 300L) <= 2L)
})

test_that(".regen_read_phase dates a fresh file at ~0 s, and NA without ts", {
  d <- withr::local_tempdir()
  reg <- file.path(d, "cache", "regeneration")
  dir.create(reg, recursive = TRUE)

  nemetonshiny:::.regen_write_phase(reg, "grille")
  expect_lt(nemetonshiny:::.regen_read_phase(d)$stale_s, 5L)

  # Un worker qui n'horodate pas ne doit pas faire inventer un âge de 1970.
  writeLines(jsonlite::toJSON(list(phase = "grille"), auto_unbox = TRUE),
             file.path(reg, "engine_status.json"))
  expect_true(is.na(nemetonshiny:::.regen_read_phase(d)$stale_s))
})

test_that(".regen_silence_suffix ne parle qu'au-dela de deux minutes", {
  i18n <- get_i18n("fr")
  sfx <- function(s) nemetonshiny:::.regen_silence_suffix(i18n, list(stale_s = s))

  # Tant que le worker parle, la phase se suffit : pas de suffixe.
  expect_equal(sfx(0L), "")
  expect_equal(sfx(119L), "")
  expect_equal(sfx(NA_integer_), "")
  expect_equal(nemetonshiny:::.regen_silence_suffix(i18n, list()), "")

  expect_match(sfx(300L), "dernier signe de vie il y a 5 min")
  expect_match(sfx(6000L), "il y a 100 min")
  expect_match(nemetonshiny:::.regen_silence_suffix(get_i18n("en"),
                                                    list(stale_s = 300L)),
               "last sign of life 5 min ago")
})

test_that(".regen_micro_lbl compte les MOIS quand le coeur les emet", {
  i18n <- get_i18n("fr")
  lbl <- function(st) nemetonshiny:::.regen_phase_label(i18n, st)

  # Le mois seul, sans compteur d'années : c'est la forme que produira
  # `regen_expo:era5_mois` (le cœur ne connaît pas le rang de l'année là-bas).
  expect_match(lbl(list(phase = "microclimf_canicule", year = 2022,
                        mois_i = 3, mois_n = 12)),
               "2022 \u2014 mois 3/12", fixed = TRUE)
  # L'année seule reste rendue même sans (i/n) — l'ancienne version exigeait
  # les trois et retombait sur le libellé nu.
  expect_match(lbl(list(phase = "microclimf_moyenne", year = 2020)),
               "2020", fixed = TRUE)
  # Les deux compteurs cohabitent si le cœur les fournit ensemble.
  expect_match(lbl(list(phase = "microclimf_moyenne", year = 2020, i = 1, n = 2,
                        mois_i = 7, mois_n = 12)),
               "2020 (1/2) \u2014 mois 7/12", fixed = TRUE)
})

test_that("la notif garde la phase ERA5 nommee et y accroche le silence", {
  i18n <- get_i18n("fr")
  # L'état exact observé sur le projet Fordead : microclimf année canicule,
  # (1/1) parce que le cœur ne compte que les ANNÉES, silencieux depuis 27 min
  # parce qu'il télécharge les mois sans rien émettre.
  st <- list(phase = "microclimf_canicule", year = 2022, i = 1, n = 1,
             stale_s = 1620L)
  lbl <- paste0(nemetonshiny:::.regen_phase_label(i18n, st),
                nemetonshiny:::.regen_silence_suffix(i18n, st))
  expect_match(lbl, "2022 \\(1/1\\)")
  expect_match(lbl, "il y a 27 min")
})

test_that(".regen_phase_label renders the 6 phases + terminal states (FR)", {
  i18n <- get_i18n("fr")
  lbl <- function(st) nemetonshiny:::.regen_phase_label(i18n, st)

  expect_equal(lbl(list(phase = "grille")), i18n$t("regen_phase_grille"))
  # PAI : source lidar / satellite / cache injectée dans le libellé
  expect_match(lbl(list(phase = "pai", source = "lidar")), "LiDAR")
  expect_match(lbl(list(phase = "pai", source = "raster")), "satellite")
  # hit cache disque (nemeton >= 0.146.2) → « PAI (cache) », phase éclair
  expect_match(lbl(list(phase = "pai", source = "cache")),
               i18n$t("regen_phase_pai_cache"), fixed = TRUE)
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

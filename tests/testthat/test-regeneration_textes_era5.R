# Mission 045 : le forcage microclimf / BILJOU vient d'ERA5 (0,25 deg,
# reanalysis-era5-single-levels), pas d'ERA5-Land ; la duree annoncee est
# celle de la file du CDS.

test_that("aucune traduction ne nomme ERA5-Land", {
  tout <- unlist(TRANSLATIONS, use.names = FALSE)
  expect_false(any(grepl("ERA5-Land", tout, fixed = TRUE)))
  for (l in c("fr", "en")) {
    expect_false(grepl("ERA5-Land", TRANSLATIONS$regen_forcing_era5[[l]], fixed = TRUE))
    expect_false(grepl("ERA5-Land", TRANSLATIONS$regen_engine_tip[[l]], fixed = TRUE))
  }
})

test_that("l'infobulle du moteur annonce la durée du jeu timeseries, FR et EN", {
  fr <- TRANSLATIONS$regen_engine_tip$fr
  en <- TRANSLATIONS$regen_engine_tip$en
  expect_match(fr, "quelques minutes par année et par point", fixed = TRUE)
  expect_match(fr, "jusqu'à 4 requêtes", fixed = TRUE)
  # Aucune durée non mesurée promise pour les calculs suivants.
  expect_false(grepl("quelques secondes", fr, fixed = TRUE))
  expect_false(grepl("deux jours|plusieurs heures|~12", fr))
  expect_match(en, "a few minutes per year and per point", fixed = TRUE)
  expect_match(en, "up to 4 requests", fixed = TRUE)
  expect_false(grepl("a few seconds", en, fixed = TRUE))
  expect_false(grepl("two days|several hours|~12", en))
})

test_that("le message d'interruption ne parle plus de throttle", {
  for (l in c("fr", "en")) {
    expect_false(grepl("throttl", TRANSLATIONS$regen_engine_era5_interrupted[[l]],
                       ignore.case = TRUE))
  }
})

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

test_that("l'infobulle du moteur annonce la file du CDS, FR et EN", {
  expect_match(TRANSLATIONS$regen_engine_tip$fr, "file du CDS", fixed = TRUE)
  expect_match(TRANSLATIONS$regen_engine_tip$fr, "deux jours", fixed = TRUE)
  expect_false(grepl("minutes à heures", TRANSLATIONS$regen_engine_tip$fr, fixed = TRUE))
  expect_match(TRANSLATIONS$regen_engine_tip$en, "CDS queue", fixed = TRUE)
  expect_match(TRANSLATIONS$regen_engine_tip$en, "two days", fixed = TRUE)
})

test_that("le message d'interruption ne parle plus de throttle", {
  for (l in c("fr", "en")) {
    expect_false(grepl("throttl", TRANSLATIONS$regen_engine_era5_interrupted[[l]],
                       ignore.case = TRUE))
  }
})

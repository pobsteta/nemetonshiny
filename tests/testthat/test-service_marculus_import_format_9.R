# Import Marculus : FormatCsv;9, bois fort renomme et volume aerien EMERGE
# (mission 097 ; brief brief-nemetonshiny-marculus-format-csv-9)

.marsync <- function(dir, contextes, tiges = NULL, nom = "t.marsync") {
  f <- file.path(dir, nom)
  jsonlite::write_json(list(version = 3, contextes = contextes,
                            tiges = tiges %||% list(), configs = list()),
                       f, auto_unbox = TRUE, na = "null", digits = NA)
  f
}

# Journal : memes colonnes qu'au format 8 ; au format 9 `VolumeTotalM3` devient
# `VolumeBoisFortTotalM3` (meme place, 18e) et l'aerien est la 23e colonne.
.COLS8 <- "Horodatage;Essence;Classe;Action;Quantite;Hauteur;QualiteArbre;Latitude;Longitude;Operateur;QualiteFix;Precision_m;Uuid;Parcelle;Modifie;VolumeTigeM3;VolumeHouppierM3;VolumeTotalM3;SurfaceTerriereM2;Cubage;Lot;AffouageLotM3"
.COLS9 <- sub("VolumeTotalM3", "VolumeBoisFortTotalM3", paste0(.COLS8, ";VolumeAerienTotalM3"))

# Contexte mixte : une tige EMERGE (aerien 1.5) et une tige Schaeffer (vide).
.csv8 <- function() {
  c("Contexte;Mixte", "FormatCsv;8", "ContexteId;a1", "Statut;REALISEE",
    "DateMartelage;2027-10-15", "Modifie;1790000000000",
    "VolumeTigeTotalM3;2.0", "VolumeTotalM3;2.4",
    "SurfaceTerriereTotaleM2;0.3", "NbTigesNonCubees;0", "Mode;DIAMETRE",
    "Affouage;false", "Journal;COMPLET", "", "JOURNAL", .COLS8,
    "2027-10-15T08:00:00Z;Hetre;40;PLUS;1;;;;;PO;;;u1;;1;1.2;0.2;1.4;0.1;EMERGE;;",
    "2027-10-15T08:01:00Z;Chene;40;PLUS;1;;;;;PO;;;u2;;1;0.8;0.2;1.0;0.1;S:8;;")
}
.csv9 <- function() {
  c("Contexte;Mixte", "FormatCsv;9", "ContexteId;a1", "Statut;REALISEE",
    "DateMartelage;2027-10-15", "Modifie;1790000000000",
    "VolumeTigeTotalM3;2.0", "VolumeBoisFortTotalM3;2.4",
    "VolumeAerienTotalM3;1.5", "NbTigesVolumeAerien;1",
    "SurfaceTerriereTotaleM2;0.3", "NbTigesNonCubees;0", "Mode;DIAMETRE",
    "Affouage;false", "Journal;COMPLET", "", "JOURNAL", .COLS9,
    "2027-10-15T08:00:00Z;Hetre;40;PLUS;1;;;;;PO;;;u1;;1;1.2;0.2;1.4;0.1;EMERGE;;;1.5",
    "2027-10-15T08:01:00Z;Chene;40;PLUS;1;;;;;PO;;;u2;;1;0.8;0.2;1.0;0.1;S:8;;;")
}
.ecrire <- function(lignes, env = parent.frame()) {
  f <- file.path(withr::local_tempdir(.local_envir = env), "f.csv")
  writeLines(lignes, f)
  f
}
.lire <- function(f) nemetonshiny:::marculus_lire_exports(f)
.plan9 <- function() {
  nemetonshiny:::add_action_to_plan(
    nemetonshiny:::init_empty_action_plan("p"),
    list(id = "a1", ug_id = "ug_1", type = "eclaircie", annee_cible = 3L,
         priorite = "moyenne", statut = "proposee"))
}

test_that("CSV 8 (ancien nom) et CSV 9 donnent le meme bois fort, tige et contexte", {
  l8 <- .lire(.ecrire(.csv8())); l9 <- .lire(.ecrire(.csv9()))
  expect_equal(l8$tiges$volumeBoisFortTotalM3, c(1.4, 1.0))
  expect_equal(l9$tiges$volumeBoisFortTotalM3, l8$tiges$volumeBoisFortTotalM3)
  expect_equal(l9$contextes$volumeBoisFortTotalM3, 2.4)
  expect_equal(l9$contextes$volumeBoisFortTotalM3,
               l8$contextes$volumeBoisFortTotalM3)
  expect_equal(l9$contextes$formatCsv, 9L)
  expect_false("volumeTotalM3" %in% names(l9$tiges))
})

test_that("l'aerien du format 9 est lu au contexte et par tige, NA avant", {
  l9 <- .lire(.ecrire(.csv9())); l8 <- .lire(.ecrire(.csv8()))
  expect_equal(l9$contextes$volumeAerienTotalM3, 1.5)
  expect_equal(l9$contextes$nbTigesVolumeAerien, 1)
  expect_equal(l9$tiges$volumeAerienTotalM3, c(1.5, NA))
  expect_true(is.na(l8$contextes$volumeAerienTotalM3))
  expect_true(is.na(l8$contextes$nbTigesVolumeAerien))
  expect_true(all(is.na(l8$tiges$volumeAerienTotalM3)))
})

test_that("l'aerien en 23e colonne ne decale aucune autre colonne", {
  l8 <- .lire(.ecrire(.csv8())); l9 <- .lire(.ecrire(.csv9()))
  cols <- setdiff(names(l9$tiges), c("volumeAerienTotalM3", "formatCsv"))
  expect_equal(l9$tiges[, cols], l8$tiges[, cols])
  expect_equal(l9$tiges$cubage, c("EMERGE", "S:8"))
  expect_true(is.na(l9$tiges$lot[1]) && is.na(l9$tiges$affouageLotM3[1]))
  expect_equal(l9$tiges$volumeTigeM3, c(1.2, 0.8))
})

test_that("un contexte sans EMERGE laisse l'aerien et le nombre de tiges a NA", {
  lignes <- .csv9()
  lignes <- sub("^VolumeAerienTotalM3;1.5$", "VolumeAerienTotalM3;", lignes)
  lignes <- sub("^NbTigesVolumeAerien;1$", "NbTigesVolumeAerien;", lignes)
  lignes <- sub(";EMERGE;;;1.5$", ";S:8;;;", lignes)
  l <- .lire(.ecrire(lignes))
  expect_true(is.na(l$contextes$volumeAerienTotalM3))
  expect_true(is.na(l$contextes$nbTigesVolumeAerien))
  expect_true(all(is.na(l$tiges$volumeAerienTotalM3)))
  expect_equal(l$contextes$volumeBoisFortTotalM3, 2.4)
})

test_that(".marsync ancien et nouveau donnent le meme bois fort", {
  d <- withr::local_tempdir()
  t <- data.frame(uuid = c("u1", "u2"), contexteId = "a1", essence = "Hetre",
                  classe = 40L, action = "PLUS", horodatage = 1:2, quantite = 1L,
                  latitude = 47.9, longitude = 1.9, modifie = 1,
                  volumeTigeM3 = c(1.2, 0.8), stringsAsFactors = FALSE)
  ctx <- data.frame(id = "a1", nom = "A", statut = "REALISEE",
                    dateMartelage = NA, modifie = 1, volumeTigeTotalM3 = 2.0,
                    surfaceTerriereTotaleM2 = 0.3, nbTigesNonCubees = 0)
  t_old <- t; t_old$volumeTotalM3 <- c(1.4, 1.0)
  c_old <- ctx; c_old$volumeTotalM3 <- 2.4
  t_new <- t; t_new$volumeBoisFortTotalM3 <- c(1.4, 1.0)
  t_new$volumeAerienTotalM3 <- c(1.5, NA)
  c_new <- ctx; c_new$volumeBoisFortTotalM3 <- 2.4
  c_new$volumeAerienTotalM3 <- 1.5; c_new$nbTigesVolumeAerien <- 1
  lo <- .lire(.marsync(d, c_old, t_old, "ancien.marsync"))
  ln <- .lire(.marsync(d, c_new, t_new, "nouveau.marsync"))
  expect_equal(lo$tiges$volumeBoisFortTotalM3, c(1.4, 1.0))
  expect_equal(ln$tiges$volumeBoisFortTotalM3, lo$tiges$volumeBoisFortTotalM3)
  expect_equal(ln$contextes$volumeBoisFortTotalM3, 2.4)
  expect_equal(lo$contextes$volumeBoisFortTotalM3, 2.4)
  expect_true(is.na(lo$contextes$volumeAerienTotalM3))
  expect_equal(ln$contextes$volumeAerienTotalM3, 1.5)
  expect_equal(ln$contextes$nbTigesVolumeAerien, 1)
  expect_equal(ln$tiges$volumeAerienTotalM3, c(1.5, NA))
})

test_that("l'action recoit le bois fort, et l'aerien seulement s'il est present", {
  r9 <- nemetonshiny:::marculus_appliquer_retour(
    .plan9(), .lire(.ecrire(.csv9()))$contextes,
    .lire(.ecrire(.csv9()))$tiges, annee_base = 2026L)
  q <- r9$plan$actions[[1]]$quantite
  expect_equal(q$volume_total_m3, 2.4)
  expect_equal(q$volume_aerien_total_m3, 1.5)
  expect_equal(q$nb_tiges_volume_aerien, 1L)
  l8 <- .lire(.ecrire(.csv8()))
  r8 <- nemetonshiny:::marculus_appliquer_retour(.plan9(), l8$contextes,
                                                 l8$tiges, annee_base = 2026L)
  q8 <- r8$plan$actions[[1]]$quantite
  expect_equal(q8$volume_total_m3, 2.4)
  expect_null(q8$volume_aerien_total_m3)
  expect_null(q8$nb_tiges_volume_aerien)
})

test_that("des tiges deja enregistrees (ancien nom) gardent leur bois fort", {
  anciennes <- data.frame(uuid = "u1", contexteId = "a1", essence = "Hetre",
                          classe = 40L, action = "PLUS", horodatage = 1,
                          quantite = 1L, modifie = 1, volumeTotalM3 = 1.4,
                          stringsAsFactors = FALSE)
  n <- nemetonshiny:::.marculus_tiges_normaliser(anciennes)
  expect_equal(n$volumeBoisFortTotalM3, 1.4)
  expect_false("volumeTotalM3" %in% names(n))
})

test_that("le texte d'aide de l'import annonce les formats 2 a 9", {
  tous <- unlist(nemetonshiny:::TRANSLATIONS)
  expect_true(any(grepl("FormatCsv;2 [^;]{1,6} 9)", tous)))
  expect_false(any(grepl("FormatCsv;2 [^;]{1,6} 8)", tous)))
})

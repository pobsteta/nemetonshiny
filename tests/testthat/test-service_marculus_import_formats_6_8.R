# Import Marculus : CSV FormatCsv;6 a ;8 et cles du .marsync (mission 027)

.marsync <- function(dir, contextes, tiges = NULL) {
  f <- file.path(dir, "t.marsync")
  jsonlite::write_json(list(version = 3, contextes = contextes,
                            tiges = tiges %||% list(), configs = list()),
                       f, auto_unbox = TRUE, na = "null", digits = NA)
  f
}

.tiges <- function(uuids, ctx = "a1") {
  data.frame(uuid = uuids, contexteId = ctx, essence = "Chene", classe = 40L,
             action = "PLUS", horodatage = seq_along(uuids), quantite = 1L,
             latitude = 47.9, longitude = 1.9, modifie = 1,
             stringsAsFactors = FALSE)
}

.ent_fmt <- function(fmt, extra = character(), uuid = "u1") {
  c("Contexte;Affouage 2027", paste0("FormatCsv;", fmt), "ContexteId;a1",
    "Statut;REALISEE", "DateMartelage;2027-10-15", "Modifie;1790000000000",
    "Tarif;SCHAEFFER_RAPIDE", "TarifNumero;9", "CoefficientForme;0.5",
    "VolumeTigeTotalM3;1.0", "VolumeTotalM3;1.6",
    "SurfaceTerriereTotaleM2;0.2", "NbTigesNonCubees;0",
    "Mode;DIAMETRE", "Increment;1", "Affouage;true", "VolumeMaxLotM3;1.0",
    "VolumeHouppierTotalM3;0.6", "PartHouppier;", "AffouageLotTotalM3;1.0",
    extra, "Journal;COMPLET", "", "JOURNAL",
    "Horodatage;Essence;Classe;Action;Quantite;Hauteur;QualiteArbre;Latitude;Longitude;Operateur;QualiteFix;Precision_m;Uuid;Parcelle;Modifie;VolumeTigeM3;VolumeHouppierM3;VolumeTotalM3;SurfaceTerriereM2;Cubage;Lot;AffouageLotM3",
    paste0("2027-10-15T08:00:00Z;Hetre;40;PLUS;1;;;;;PO;;;", uuid,
           ";;1;0.6;0.6;1.2;0.1;S:8;1;1.200000"))
}

.csv_fmt <- function(fmt, extra, env = parent.frame(), uuid = "u1") {
  f <- file.path(withr::local_tempdir(.local_envir = env), "f.csv")
  writeLines(.ent_fmt(fmt, extra, uuid), f)
  f
}

.EXTRA6 <- c("TarifsEssence;Chêne=SCHAEFFER_RAPIDE:9|Hêtre=SCHAEFFER_INTERMEDIAIRE:11|Charme=EMERGE:9|Pin=AUCUN:0",
             "SteresParM3;1.4")
.EXTRA7 <- c(.EXTRA6, "CommuneInsee;21295", "Ser;C20", "MillesimeTarifs;2011-2025")
.EXTRA8 <- c(.EXTRA7, "PartsLot;Chêne=tige:0,houppier:1|Hêtre=tige:1,houppier:1")

.appliquer <- function(lu, env = parent.frame()) {
  plan <- list(horizon_annees = 20L, actions = list(list(
    id = "a1", ug_id = "ug1", type = "coupe", statut = "proposee",
    annee_cible = 1L)))
  testthat::local_mocked_bindings(validate_action = function(...) list(valid = TRUE),
                                  .package = "nemetonshiny", .env = env)
  nemetonshiny:::marculus_appliquer_retour(plan, lu$contextes, lu$tiges,
                                           annee_base = 2026L)$plan$actions[[1]]
}

test_that("le format 6 apporte le tarif par essence et les steres", {
  lu <- nemetonshiny:::marculus_lire_exports(.csv_fmt(6, .EXTRA6))
  rt <- .appliquer(lu)$reglages_terrain
  expect_equal(rt$format_csv, 6L)
  expect_equal(rt$steres_par_m3, 1.4)
  expect_equal(rt$tarifs_essence[["Hêtre"]],
               list(tarif = "SCHAEFFER_INTERMEDIAIRE", numero = 11L))
  # Numero ignore pour AUCUN et EMERGE, meme avec un reste de saisie.
  expect_true(is.na(rt$tarifs_essence$Charme$numero))
  expect_equal(rt$tarifs_essence$Charme$tarif, "EMERGE")
  expect_true(is.na(rt$tarifs_essence$Pin$numero))
  # Pas de cles du format 7 ni 8.
  expect_null(rt$commune_insee); expect_null(rt$ser); expect_null(rt$parts_lot)
})

test_that("un TarifsEssence entre guillemets avec ; et \"\" se lit", {
  extra <- c("TarifsEssence;\"Chêne (a;b)=SCHAEFFER_LENT:9|Pin \"\"noir\"\"=SCHAEFFER_TRES_LENT:7\"",
             "SteresParM3;1.5")
  rt <- .appliquer(nemetonshiny:::marculus_lire_exports(.csv_fmt(6, extra)))$reglages_terrain
  te <- rt$tarifs_essence
  expect_equal(names(te), c("Chêne (a;b)", "Pin \"noir\""))
  expect_equal(te[["Pin \"noir\""]]$tarif, "SCHAEFFER_TRES_LENT")
  expect_equal(te[["Pin \"noir\""]]$numero, 7L)
})

test_that("le format 7 apporte commune, SER et millesime; vides = absents", {
  rt <- .appliquer(nemetonshiny:::marculus_lire_exports(.csv_fmt(7, .EXTRA7)))$reglages_terrain
  expect_equal(rt$commune_insee, "21295")
  expect_equal(rt$ser, "C20")
  expect_equal(rt$millesime_tarifs, "2011-2025")
  vide <- c(.EXTRA6, "CommuneInsee;", "Ser;", "MillesimeTarifs;")
  rt0 <- .appliquer(nemetonshiny:::marculus_lire_exports(.csv_fmt(7, vide)))$reglages_terrain
  expect_null(rt0$commune_insee); expect_null(rt0$ser); expect_null(rt0$millesime_tarifs)
})

test_that("le format 8 apporte les parties comptees au lot", {
  rt <- .appliquer(nemetonshiny:::marculus_lire_exports(.csv_fmt(8, .EXTRA8)))$reglages_terrain
  expect_equal(rt$format_csv, 8L)
  expect_equal(rt$parts_lot[["Chêne"]], list(tige = FALSE, houppier = TRUE))
  expect_equal(rt$parts_lot[["Hêtre"]], list(tige = TRUE, houppier = TRUE))
})

test_that("les formats 2 a 5 sont inchanges: aucune cle des formats 6 a 8", {
  lu <- nemetonshiny:::marculus_lire_exports(.csv_fmt(5, character()))
  expect_true(is.na(lu$contextes$tarifsEssence) && is.na(lu$contextes$partsLot))
  rt <- .appliquer(lu)$reglages_terrain
  expect_equal(rt$format_csv, 5L)
  expect_null(rt$tarifs_essence); expect_null(rt$steres_par_m3)
  expect_null(rt$parts_lot); expect_null(rt$ser)
  expect_true(rt$affouage)
})

test_that("le .marsync lit les cles des formats 6 a 8, partsLot partiel", {
  d <- withr::local_tempdir()
  f <- .marsync(d, contextes = list(
    list(id = "a1", nom = "A", statut = "REALISEE", modifie = 2,
         affouage = TRUE, volumeMaxLotM3 = 2,
         tarifsEssence = list("Hêtre" = list(tarif = "SCHAEFFER_TRES_LENT", numero = 12),
                              "Pin" = list(tarif = "AUCUN", numero = 3)),
         steresParM3 = 1.4, communeInsee = "21295", ser = "C20",
         millesimeTarifs = "2011-2025",
         partsLot = list("Chêne" = list(tige = FALSE, houppier = TRUE))),
    list(id = "b2", nom = "B", statut = "PREVU", modifie = 1, affouage = FALSE)),
    tiges = .tiges("u1"))
  lu <- nemetonshiny:::marculus_lire_exports(f)
  ctx <- lu$contextes[order(lu$contextes$id), ]
  expect_equal(ctx$partsLot, c("Chêne=tige:0,houppier:1", ""))  # jsonlite : cle absente = objet vide
  expect_equal(ctx$steresParM3, c(1.4, NA))
  rt <- .appliquer(lu)$reglages_terrain
  expect_true(is.null(rt$format_csv))
  expect_equal(rt$parts_lot[["Chêne"]], list(tige = FALSE, houppier = TRUE))
  expect_equal(rt$tarifs_essence$Pin$tarif, "AUCUN")
  expect_true(is.na(rt$tarifs_essence$Pin$numero))
  expect_equal(rt$tarifs_essence[["Hêtre"]]$numero, 12L)
  expect_equal(rt$ser, "C20")
  expect_equal(rt$commune_insee, "21295")
})

test_that("communeInsee, tarifsEssence, steresParM3 et partsLot ne sont jamais reemis", {
  a <- .appliquer(nemetonshiny:::marculus_lire_exports(.csv_fmt(8, .EXTRA8)))
  projet <- list(id = "p", metadata = list(name = "P"))
  interdites <- c("communeInsee", "tarifsEssence", "steresParM3", "partsLot",
                  "commune_insee", "tarifs_essence", "steres_par_m3", "parts_lot",
                  "tarif", "tarifNumero", "partHouppier")
  ctx <- nemetonshiny:::marculus_context_from_action(a, projet)
  expect_false(any(interdites %in% names(ctx)))
  # `ser` et `millesimeTarifs` restent emis comme avant, depuis le cache.
  tarifs <- list(ser = "C20", millesime = "IFN 2011-2025",
                 essences = list("Hêtre" = list(conseille = "SCHL")))
  ctx2 <- nemetonshiny:::marculus_context_from_action(
    a, projet, essences = "Hêtre", tarifs = tarifs)
  expect_false(any(interdites %in% names(ctx2)))
  expect_equal(ctx2$ser, "C20")
  expect_equal(ctx2$millesimeTarifs, "IFN 2011-2025")
})

test_that("le bloc des lots dit quelles parties comptent (format 8 vs 7)", {
  i18n <- nemetonshiny:::get_i18n("fr")
  lu8 <- nemetonshiny:::marculus_lire_exports(.csv_fmt(8, .EXTRA8))
  rt8 <- .appliquer(lu8)$reglages_terrain
  h8 <- as.character(nemetonshiny:::.marculus_lots_bloc(
    lu8$tiges, list(id = "a1", reglages_terrain = rt8), i18n))
  expect_match(h8, "Parties comptées au lot : Chêne houppier, Hêtre tige + houppier", fixed = TRUE)
  expect_false(grepl("avant le format 8", h8, fixed = TRUE))
  lu7 <- nemetonshiny:::marculus_lire_exports(.csv_fmt(7, .EXTRA7))
  rt7 <- .appliquer(lu7)$reglages_terrain
  h7 <- as.character(nemetonshiny:::.marculus_lots_bloc(
    lu7$tiges, list(id = "a1", reglages_terrain = rt7), i18n))
  expect_match(h7, "avant le format 8", fixed = TRUE)
  expect_false(grepl("Parties compt", h7, fixed = TRUE))
  # .marsync (sans numero de format) : les autres essences comptent tout.
  hm <- as.character(nemetonshiny:::.marculus_lots_bloc(
    lu8$tiges, list(id = "a1", reglages_terrain = list(
      affouage = TRUE, volume_max_lot_m3 = 1,
      parts_lot = list("Chêne" = list(tige = FALSE, houppier = TRUE)))), i18n))
  expect_match(hm, "autres essences : tige + houppier", fixed = TRUE)
})

test_that("le controle croise signale une SER ou un millesime different", {
  i18n <- nemetonshiny:::get_i18n("fr")
  rt <- list(ser = "C20", millesime_tarifs = "2011-2025")
  # Identiques (prefixe IFN tolere) : rien.
  expect_null(nemetonshiny:::.marculus_controle_tarifs(
    rt, list(ser = "C20", millesime = "IFN 2011-2025"), i18n))
  expect_null(nemetonshiny:::.marculus_controle_tarifs(rt, NULL, i18n))
  h <- as.character(nemetonshiny:::.marculus_controle_tarifs(
    rt, list(ser = "B11", millesime = "IFN 2010-2024"), i18n))
  expect_match(h, "SER relue du téléphone : C20, émise par l'application : B11", fixed = TRUE)
  expect_match(h, "2011-2025", fixed = TRUE)
  # Rien relu (champ vide au telephone) : pas de fausse alerte.
  expect_null(nemetonshiny:::.marculus_controle_tarifs(
    list(), list(ser = "B11", millesime = "IFN 2010-2024"), i18n))
})

test_that("des tiges de formats 7 et 8 dans un meme contexte : avertissement", {
  i18n <- nemetonshiny:::get_i18n("fr")
  l7 <- nemetonshiny:::marculus_lire_exports(.csv_fmt(7, .EXTRA7, uuid = "u1"))
  l8 <- nemetonshiny:::marculus_lire_exports(.csv_fmt(8, .EXTRA8, uuid = "u2"))
  expect_equal(l7$tiges$formatCsv, 7L)
  expect_equal(l8$tiges$formatCsv, 8L)
  # Comme `marculus_importer()` : union par uuid des tiges lues et gardees.
  tiges <- nemetonshiny:::.marculus_tiges_union(rbind(l8$tiges, l7$tiges))
  rt <- .appliquer(l8)$reglages_terrain
  h <- as.character(nemetonshiny:::.marculus_lots_bloc(
    tiges, list(id = "a1", reglages_terrain = rt), i18n))
  expect_match(h, "formats différents", fixed = TRUE)
  expect_match(h, "text-warning", fixed = TRUE)
  expect_false(grepl("Parties compt", h, fixed = TRUE))
  # Contexte homogene : pas d'avertissement, la ligne des parties reste.
  h8 <- as.character(nemetonshiny:::.marculus_lots_bloc(
    l8$tiges, list(id = "a1", reglages_terrain = rt), i18n))
  expect_false(grepl("formats diff", h8, fixed = TRUE))
  expect_match(h8, "Parties compt", fixed = TRUE)
  # Tiges stockees avant la colonne : format inconnu, pas d'avertissement.
  anc <- nemetonshiny:::.marculus_tiges_normaliser(
    tiges[, setdiff(names(tiges), "formatCsv")])
  expect_true(all(is.na(anc$formatCsv)))
})

test_that("un .marsync avec partsLot date ses tiges au format 8", {
  d <- withr::local_tempdir()
  f <- .marsync(d, contextes = list(
    list(id = "a1", nom = "A", statut = "REALISEE", modifie = 2, affouage = TRUE,
         partsLot = list("Chêne" = list(tige = FALSE, houppier = TRUE)))),
    tiges = .tiges("u1"))
  expect_equal(nemetonshiny:::marculus_lire_exports(f)$tiges$formatCsv, 8L)
  g <- .marsync(d, contextes = list(
    list(id = "a1", nom = "A", statut = "REALISEE", modifie = 2, affouage = TRUE)),
    tiges = .tiges("u1"))
  expect_true(is.na(nemetonshiny:::marculus_lire_exports(g)$tiges$formatCsv))
})

test_that("le texte d'aide dit FormatCsv;2 a 8", {
  expect_match(nemetonshiny:::get_i18n("fr")$t("marculus_import_help"), "2 à 8", fixed = TRUE)
  expect_match(nemetonshiny:::get_i18n("en")$t("marculus_import_help"), "2 to 8", fixed = TRUE)
})

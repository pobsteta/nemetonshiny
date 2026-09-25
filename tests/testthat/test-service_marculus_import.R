# Retour du martelage Marculus (R/service_marculus_import.R)

.ms <- function(x) as.numeric(as.POSIXct(x, tz = "UTC")) * 1000

.marsync <- function(dir, nom = "t.marsync", contextes, tiges = NULL) {
  f <- file.path(dir, nom)
  jsonlite::write_json(list(version = 3, contextes = contextes,
                            tiges = tiges %||% list(), configs = list()),
                       f, auto_unbox = TRUE, na = "null", digits = NA)
  f
}

.tiges <- function(uuids, ctx = "a1", essence = "Chene", classe = 40L,
                   action = "PLUS", modifie = 1) {
  data.frame(uuid = uuids, contexteId = ctx, essence = essence,
             classe = classe, action = action, horodatage = seq_along(uuids),
             quantite = 1L, latitude = 47.9, longitude = 1.9,
             modifie = modifie, stringsAsFactors = FALSE)
}

.plan_a1 <- function() {
  plan <- nemetonshiny:::init_empty_action_plan("p")
  nemetonshiny:::add_action_to_plan(plan, list(
    id = "a1", ug_id = "ug_1", type = "eclaircie", annee_cible = 3L,
    priorite = "moyenne", statut = "proposee"))
}

test_that("les totaux nets suivent la regle du telephone (PLUS - ANNULATION)", {
  t <- rbind(.tiges(c("u1", "u2", "u3")),
             .tiges("u4", action = "ANNULATION"),
             .tiges("u5", essence = "Hetre", classe = 35L))
  tot <- nemetonshiny:::marculus_totaux(t)
  expect_equal(tot$tiges[tot$essence == "Chene"], 2L)
  expect_equal(tot$tiges[tot$essence == "Hetre"], 1L)
  # Une cellule revenue a zero disparait.
  t0 <- rbind(.tiges("u1"), .tiges("u2", action = "ANNULATION"))
  expect_equal(nrow(nemetonshiny:::marculus_totaux(t0)), 0L)
})

test_that("les tiges sont unies par uuid, version la plus recente gardee", {
  t <- rbind(.tiges("u1", classe = 40L, modifie = 1),
             .tiges("u1", classe = 45L, modifie = 9),
             .tiges("u2"))
  u <- nemetonshiny:::.marculus_tiges_union(t)
  expect_equal(nrow(u), 2L)
  expect_equal(u$classe[u$uuid == "u1"], 45L)
})

test_that("le retour met l'action a jour, le terrain fait foi", {
  d <- withr::local_tempdir()
  f <- .marsync(d, contextes = data.frame(
    id = c("a1", "etranger"), nom = c("A", "X"),
    statut = c("REALISEE", "PROPOSEE"),
    dateMartelage = c(.ms("2027-10-15"), NA), modifie = 5),
    tiges = rbind(.tiges(c("u1", "u2")), .tiges("u3", action = "ANNULATION")))
  lu <- nemetonshiny:::marculus_lire_exports(f)
  r <- nemetonshiny:::marculus_appliquer_retour(.plan_a1(), lu$contextes,
                                                lu$tiges, user = "terrain",
                                                annee_base = 2026L)
  a <- r$plan$actions[[1]]
  expect_equal(r$n_actions, 1L)
  expect_equal(r$n_orphelins, 1L)
  expect_equal(a$statut, "realisee")
  expect_equal(a$date_martelage, "2027-10-15")
  expect_equal(a$annee_cible, 1L)
  expect_equal(a$quantite$nb_tiges, 1L)
  # Chaque changement est trace, ancienne valeur comprise.
  statut <- Filter(function(e) identical(e$champ, "statut"), r$plan$audit)
  expect_equal(statut[[1]]$ancien, "proposee")

  # Reimporter la meme chose ne touche a rien.
  r2 <- nemetonshiny:::marculus_appliquer_retour(r$plan, lu$contextes,
                                                 lu$tiges, annee_base = 2026L)
  expect_equal(r2$n_actions, 0L)
})

test_that("une date de martelage de l'annee en cours ne casse pas annee_cible", {
  lu <- list(contextes = data.frame(id = "a1", nom = "A", statut = "REALISEE",
                                    dateMartelage = .ms("2026-03-02"),
                                    modifie = 1, stringsAsFactors = FALSE),
             tiges = nemetonshiny:::.marculus_tiges_vides())
  r <- nemetonshiny:::marculus_appliquer_retour(.plan_a1(), lu$contextes,
                                                lu$tiges, annee_base = 2026L)
  a <- r$plan$actions[[1]]
  expect_equal(a$date_martelage, "2026-03-02")
  expect_equal(a$annee_cible, 3L)                    # hors horizon : inchange
})

test_that("l'import stocke les tiges et n'en double aucune", {
  proj <- withr::local_tempdir()
  dir.create(file.path(proj, "data"))
  sauve <- NULL
  testthat::local_mocked_bindings(
    get_project_path = function(id) proj,
    load_action_plan = function(id) .plan_a1(),
    save_action_plan = function(id, plan) { sauve <<- plan; TRUE }
  )
  ctx <- data.frame(id = "a1", nom = "A", statut = "PLANIFIEE",
                    dateMartelage = NA, modifie = 1)
  f1 <- .marsync(proj, "1.marsync", ctx, .tiges(c("u1", "u2")))
  r1 <- nemetonshiny:::marculus_importer("p", f1, user = "t")
  expect_equal(r1$n_tiges_nouvelles, 2L)
  expect_equal(sauve$actions[[1]]$quantite$nb_tiges, 2L)

  f2 <- .marsync(proj, "2.marsync", ctx, .tiges(c("u2", "u3")))
  r2 <- nemetonshiny:::marculus_importer("p", f2)
  expect_equal(r2$n_tiges_nouvelles, 1L)
  expect_equal(r2$n_tiges, 3L)
  expect_equal(nrow(nemetonshiny:::marculus_charger_tiges("p")), 3L)
})

test_that("un fichier qui n'est pas un export Marculus est signale", {
  d <- withr::local_tempdir()
  f <- file.path(d, "autre.json"); writeLines('{"a": 1}', f)
  lu <- nemetonshiny:::marculus_lire_exports(f)
  expect_equal(lu$illisibles, "autre.json")
  expect_equal(nrow(lu$contextes), 0L)
})

test_that("l'export reutilise la date revenue du terrain", {
  a <- list(id = "a1", ug_id = "ug_1", type = "eclaircie", annee_cible = 3L,
            statut = "realisee", date_martelage = "2027-10-15")
  ctx <- nemetonshiny:::marculus_context_from_action(
    a, list(metadata = list(name = "P")))
  expect_equal(ctx$dateMartelage, .ms("2027-10-15"))
})


# ---- CSV de contexte, FormatCsv;2 (ExportCsv.kt) --------------------------

.csv_v2 <- function(dir, nom = "ctx.csv", date = "2027-10-15", uuids = c("u1", "u2", "u3")) {
  f <- file.path(dir, nom)
  writeLines(c(
    '"Reconfort - parcelle 1116; obs" - observation',
    "FormatCsv;2",
    "ContexteId;a1",
    "Statut;REALISEE",
    paste0("DateMartelage;", date),
    "Modifie;1790000000000",
    "Mode;CIRCONFERENCE",
    "Increment;1",
    "",
    "TOTAUX",
    "Essence;Classe;Total",
    "Chene sessile;40;1",
    "",
    "JOURNAL",
    "Horodatage;Essence;Classe;Action;Quantite;Hauteur;QualiteArbre;Latitude;Longitude;Operateur;QualiteFix;Precision_m;Uuid;Parcelle;Modifie",
    paste0("2027-10-15T08:12:03Z;Chene sessile;40;PLUS;1;27-6AB;B;47.912345;1.905432;PO;RTK fixe;0.02;", uuids[1], ";B 7;1790000000001"),
    paste0("2027-10-15T08:13:04.250Z;Chene sessile;40;PLUS;1;;;;;PO;;;", uuids[2], ";;1790000000002"),
    paste0("2027-10-15T08:14:00Z;Chene sessile;40;ANNULATION;1;;;;;PO;;;", uuids[3], ";;1790000000003")
  ), f, useBytes = TRUE)
  # Le nom porte un « ; » : `champ()` l'entoure de guillemets, comme le telephone.
  l <- readLines(f); l[1] <- paste0("Contexte;", l[1]); writeLines(l, f)
  f
}

test_that("un CSV FormatCsv;2 se lit comme un .marsync", {
  d <- withr::local_tempdir()
  lu <- nemetonshiny:::marculus_lire_exports(.csv_v2(d))
  expect_equal(lu$contextes$id, "a1")
  expect_equal(lu$contextes$statut, "REALISEE")
  expect_equal(lu$contextes$nom, "Reconfort - parcelle 1116; obs - observation")
  expect_equal(lu$contextes$dateMartelage, .ms("2027-10-15"))
  expect_equal(nrow(lu$tiges), 3L)
  expect_equal(lu$tiges$latitude[1], 47.912345)
  expect_equal(lu$tiges$parcelle[1], "B 7")
  expect_equal(lu$tiges$horodatage[2], .ms("2027-10-15 08:13:04.25"), tolerance = 1)
  expect_equal(nemetonshiny:::marculus_totaux(lu$tiges)$tiges, 1L)

  r <- nemetonshiny:::marculus_appliquer_retour(.plan_a1(), lu$contextes,
                                                lu$tiges, annee_base = 2026L)
  a <- r$plan$actions[[1]]
  expect_equal(a$statut, "realisee")
  expect_equal(a$date_martelage, "2027-10-15")
  expect_equal(a$quantite$nb_tiges, 1L)
})

test_that("CSV et .marsync d'un meme contexte ne doublent pas les tiges", {
  d <- withr::local_tempdir()
  csv <- .csv_v2(d)
  ms <- .marsync(d, contextes = data.frame(id = "a1", nom = "A", statut = "REALISEE",
                                           dateMartelage = NA, modifie = 1),
                 tiges = .tiges(c("u1", "u2", "u9")))
  lu <- nemetonshiny:::marculus_lire_exports(c(csv, ms))
  expect_setequal(lu$tiges$uuid, c("u1", "u2", "u3", "u9"))
  expect_equal(nrow(lu$contextes), 1L)
})

test_that("un CSV sans FormatCsv (format 1) est refuse et signale a part", {
  d <- withr::local_tempdir()
  f <- file.path(d, "ancien.csv")
  writeLines(c("Contexte;Vieux", "Mode;CIRCONFERENCE", "Increment;1", "",
               "TOTAUX", "Essence;Classe;Total", "", "JOURNAL",
               "Horodatage;Essence;Classe;Action;Quantite;Hauteur;QualiteArbre;Latitude;Longitude;Operateur;QualiteFix;Precision_m"), f)
  lu <- nemetonshiny:::marculus_lire_exports(f)
  expect_equal(lu$csv_anciens, "ancien.csv")
  expect_length(lu$illisibles, 0L)
  expect_equal(nrow(lu$contextes), 0L)
})

test_that("une date de martelage vide dans le CSV laisse la date du plan", {
  d <- withr::local_tempdir()
  lu <- nemetonshiny:::marculus_lire_exports(.csv_v2(d, date = ""))
  expect_true(is.na(lu$contextes$dateMartelage))
  r <- nemetonshiny:::marculus_appliquer_retour(.plan_a1(), lu$contextes,
                                                lu$tiges, annee_base = 2026L)
  expect_null(r$plan$actions[[1]]$date_martelage)
})

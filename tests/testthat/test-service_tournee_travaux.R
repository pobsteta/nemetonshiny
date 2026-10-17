# Tournee de travaux (brief surculus du 2026-10-15) : programme exporte vers
# sommieR, realise relu depuis son CSV.

.plan_tournee <- function() {
  plan <- init_empty_action_plan("p1")
  ajout <- function(plan, id, type, statut) {
    add_action_to_plan(plan, list(id = id, ug_id = "ug1", type = type,
                                  annee_cible = 2L, statut = statut),
                       ug_ids = "ug1")
  }
  plan <- ajout(plan, "plant_planif", "plantation", "planifiee")
  plan <- ajout(plan, "prot_valid", "protection", "validee")
  plan <- ajout(plan, "depr_propose", "depressage", "proposee")
  plan <- ajout(plan, "entr_abandon", "entretien", "abandonnee")
  plan <- ajout(plan, "coupe_planif", "coupe_rase", "planifiee")
  plan <- ajout(plan, "ecl_valid", "eclaircie", "validee")
  plan <- ajout(plan, "obs_planif", "observation", "planifiee")
  plan
}

.csv_realise <- function(lignes) {
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  utils::write.csv(lignes, f, row.names = FALSE)
  f
}

test_that("le programme ne garde que les travaux valides ou planifies", {
  ids <- tournee_travaux_action_ids(.plan_tournee())
  expect_setequal(ids, c("plant_planif", "prot_valid"))
  expect_length(tournee_travaux_action_ids(init_empty_action_plan("p1")), 0L)
})

test_that("le GeoPackage du programme n'a ni proposee, ni abandonnee, ni coupe", {
  skip_if_not_installed("sf")
  poly <- sf::st_polygon(list(rbind(c(5, 46), c(5.01, 46), c(5.01, 46.01),
                                    c(5, 46.01), c(5, 46))))
  ug <- sf::st_sf(ug_id = "ug1", label = "UGF 1",
                  geometry = sf::st_sfc(poly, crs = 4326))
  f <- withr::local_tempfile(fileext = ".gpkg")
  expect_true(export_programme_travaux_gpkg(.plan_tournee(), ug, f))
  act <- sf::st_read(f, layer = "actions", quiet = TRUE)
  expect_setequal(act$id, c("plant_planif", "prot_valid"))
  expect_false(any(act$statut %in% c("proposee", "abandonnee")))
  expect_false(any(act$type %in% TOURNEE_TRAVAUX_TYPES_EXCLUS))
  expect_true("ugf" %in% sf::st_layers(f)$name)
  # Rien a exporter : FALSE, pas de fichier vide.
  f2 <- withr::local_tempfile(fileext = ".gpkg")
  expect_false(export_programme_travaux_gpkg(init_empty_action_plan("p1"), ug, f2))
})

test_that("le CSV de sommieR est lu une ligne par action, quantites sommees", {
  f <- .csv_realise(data.frame(
    action_id = c("plant_planif", "plant_planif", "prot_valid", "x", ""),
    travaux_id = c("t1", "t2", "t3", "t4", "t5"),
    code_travaux = c("PL", "PL", "PI", "PL", "PL"),
    date_reception = c("2026-04-02", "2026-05-10", "2026-04-20", "", ""),
    quantite = c("0.8", "1.2", "300", "2", "1"),
    unite = c("ha", "ha", "u", "ha", "ha")))
  r <- lire_actions_realisees(f)
  expect_identical(r$action_id, c("plant_planif", "prot_valid", "x"))
  expect_identical(r$n_travaux, c(2L, 1L, 1L))
  expect_equal(r$quantite, c(2, 300, 2))
  expect_identical(r$date_reception[1], "2026-05-10")
  expect_true(is.na(r$date_reception[3]))
})

test_that("des unites differentes ne sont pas sommees", {
  f <- .csv_realise(data.frame(
    action_id = c("a", "a"), travaux_id = c("t1", "t2"),
    code_travaux = "PL", date_reception = "2026-04-02",
    quantite = c("1", "50"), unite = c("ha", "u")))
  r <- lire_actions_realisees(f)
  expect_true(is.na(r$quantite))
  expect_identical(r$unite, "ha, u")
})

test_that("un CSV sans les colonnes de sommieR est refuse", {
  f <- .csv_realise(data.frame(action_id = "a", quantite = 1))
  expect_error(lire_actions_realisees(f), class = "tournee_csv_invalide")
})

test_that("le CSV d'exemple passe deux actions en realisee, avec l'audit", {
  plan <- .plan_tournee()
  f <- .csv_realise(data.frame(
    action_id = c("plant_planif", "prot_valid", "inconnue", "entr_abandon",
                  "depr_propose"),
    travaux_id = paste0("t", 1:5), code_travaux = "PL",
    date_reception = "2026-04-02", quantite = "1", unite = "ha"))
  prop <- proposer_actions_realisees(plan, lire_actions_realisees(f))
  etats <- stats::setNames(prop$etat, prop$action_id)
  expect_identical(unname(etats[c("plant_planif", "prot_valid", "inconnue",
                                  "entr_abandon", "depr_propose")]),
                   c("a_passer", "a_passer", "inconnue",
                     "transition_invalide", "transition_invalide"))

  n_audit <- length(plan$audit)
  res <- appliquer_actions_realisees(plan, prop$action_id, user = "agent")
  expect_identical(res$n, 2L)
  statut <- function(id) get_action_by_id(res$plan, id)$statut
  expect_identical(statut("plant_planif"), "realisee")
  # Validee : par planifiee, chaque pas autorise par le Kanban.
  expect_identical(statut("prot_valid"), "realisee")
  expect_identical(statut("entr_abandon"), "abandonnee")
  expect_identical(statut("depr_propose"), "proposee")
  nouveaux <- res$plan$audit[-seq_len(n_audit)]
  expect_length(nouveaux, 3L)
  expect_true(all(vapply(nouveaux, `[[`, "", "op") == "update"))
  expect_true(all(vapply(nouveaux, `[[`, "", "user") == "agent"))
  expect_identical(vapply(nouveaux, `[[`, "", "nouveau"),
                   c("realisee", "planifiee", "realisee"))

  # Rejouer le fichier : deja realisees, rien ne bouge.
  prop2 <- proposer_actions_realisees(res$plan, lire_actions_realisees(f))
  expect_identical(prop2$etat[prop2$action_id == "plant_planif"], "deja_realisee")
  expect_identical(appliquer_actions_realisees(res$plan, "plant_planif")$n, 0L)
})

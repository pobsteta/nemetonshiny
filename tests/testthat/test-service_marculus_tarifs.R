# Numeros de tarif Schaeffer proposes a Marculus (R/service_marculus_tarifs.R,
# spec 062 du coeur, brief nemetonshiny du 2026-10-16).

# Sortie type de `nemeton::tarif_schaeffer_local()` : chene sessile au niveau
# foret (4 types), hetre en repli SER (un seul type qualifie ici).
.tab_tarifs <- function() {
  data.frame(
    espar = c(rep("03", 4), "09"),
    essence_libelle = c(rep("Chêne sessile", 4), "Hêtre"),
    type = c("SCHR", "SCHI", "SCHL", "SCHTL", "SCHL"),
    numero = c(7.44, 8.0, 9.1, 10.2, 11.0),
    cv = c(14, 12, 11, 13, 12),
    n_arbres = c(rep(212L, 4), 1240L),
    niveau_utilise = c(rep("foret", 4), "SER"),
    code = c(rep(NA, 4), "C30"),
    type_conseille = c(rep("SCHL", 4), "SCHL"),
    n_placettes = c(rep(41L, 4), NA),
    stringsAsFactors = FALSE)
}

.unites <- function() {
  sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(5, 47), c(5.01, 47), c(5.01, 47.01), c(5, 47.01), c(5, 47)))), crs = 4326))
}

.travail <- function(dir, essences = c("Chêne sessile", "Hêtre", "Frêne")) {
  u <- .unites()
  list(unites = u, essences = essences,
       signature = nemetonshiny:::.marculus_tarifs_signature(u, essences),
       chemin = file.path(dir, "cache", "marculus_tarifs.json"))
}

test_that("une essence est mise en forme pour le .marsync", {
  tab <- .tab_tarifs()
  p <- nemetonshiny:::.marculus_tarifs_essence(tab[tab$espar == "03", ])
  expect_setequal(names(p), c("SCHR", "SCHI", "SCHL", "SCHTL", "conseille"))
  expect_identical(p$conseille, "SCHL")
  expect_equal(p$SCHR$numero, 7.4)              # a 0,1
  expect_identical(p$SCHL$niveau, "foret")
  expect_equal(p$SCHL$cv, 0.11)                 # % du coeur -> fraction
  expect_identical(p$SCHL$nArbres, 212L)
  expect_identical(p$SCHL$nPlacettes, 41L)
  expect_true("code" %in% names(p$SCHL))        # present, a null
  expect_null(p$SCHL$code)

  h <- nemetonshiny:::.marculus_tarifs_essence(tab[tab$espar == "09", ])
  expect_identical(h$SCHL$niveau, "ser")        # SER -> ser
  expect_identical(h$SCHL$code, "C30")
  expect_null(h$SCHL$nPlacettes)

  # Repli France : niveau " national ".
  f <- tab[5, ]; f$niveau_utilise <- "FRANCE"; f$code <- "FR"
  expect_identical(nemetonshiny:::.marculus_tarifs_essence(f)$SCHL$niveau, "national")
  # Aucun echelon qualifie : pas de proposition.
  v <- tab[5, ]; v$numero <- NA
  expect_null(nemetonshiny:::.marculus_tarifs_essence(v))
})

test_that("la preparation calcule une fois pour la foret et met en cache", {
  skip_if_not_installed("sf")
  d <- withr::local_tempdir()
  local_mocked_bindings(localiser_ser = function(units, ...) {
    units$ser <- "C30"; units
  }, .package = "nemeton")
  appels <- 0L
  calc <- function(units, especes, ser, ...) {
    appels <<- appels + 1L
    expect_identical(ser, "C30")
    expect_setequal(especes, c("03", "09"))     # Frene (genre) non resolu
    .tab_tarifs()
  }
  res <- nemetonshiny:::marculus_tarifs_preparer(.travail(d), calculer = calc)
  expect_identical(appels, 1L)
  expect_identical(res$statut, "ok")
  expect_identical(res$ser, "C30")
  expect_setequal(names(res$essences), c("Chêne sessile", "Hêtre"))
  expect_true(file.exists(file.path(d, "cache", "marculus_tarifs.json")))
  lu <- nemetonshiny:::marculus_tarifs_lire(file.path(d, "cache", "marculus_tarifs.json"))
  expect_equal(lu$essences[["Hêtre"]]$SCHL$numero, 11)
})

test_that("un echec du calcul est ecrit, et se retente au clic suivant", {
  skip_if_not_installed("sf")
  d <- withr::local_tempdir()
  local_mocked_bindings(localiser_ser = function(...) stop("WFS injoignable"),
                        .package = "nemeton")
  res <- nemetonshiny:::marculus_tarifs_preparer(
    .travail(d), calculer = function(...) stop("IFN injoignable"))
  expect_identical(res$statut, "echec")
  expect_match(res$erreur, "IFN injoignable")
  project <- list(path = d, parcels = .unites(),
                  metadata = list(groupes_profile = NULL))
  ess <- c("Chêne sessile", "Hêtre", "Frêne")
  # Cache en echec : un nouveau travail est propose.
  expect_false(is.null(nemetonshiny:::marculus_tarifs_travail(project, ess)))
  ex <- nemetonshiny:::marculus_tarifs_pour_export(project, ess)
  expect_identical(ex$statut, "echec")
  expect_null(ex$tarifs)
})

test_that("un cache a jour n'est pas recalcule ; un perimetre change l'invalide", {
  skip_if_not_installed("sf")
  d <- withr::local_tempdir()
  local_mocked_bindings(localiser_ser = function(units, ...) {
    units$ser <- "C30"; units
  }, .package = "nemeton")
  ess <- c("Chêne sessile", "Hêtre", "Frêne")
  nemetonshiny:::marculus_tarifs_preparer(.travail(d, ess),
                                          calculer = function(...) .tab_tarifs())
  project <- list(path = d, parcels = .unites(), metadata = list())
  expect_null(nemetonshiny:::marculus_tarifs_travail(project, ess))
  expect_identical(nemetonshiny:::marculus_tarifs_pour_export(project, ess)$statut, "ok")
  # Autres essences : le cache ne vaut plus.
  expect_identical(nemetonshiny:::marculus_tarifs_pour_export(
    project, c("Douglas"))$statut, "non_calcule")
  # Autre perimetre : idem.
  project$parcels <- sf::st_buffer(sf::st_transform(.unites(), 2154), 500)
  expect_false(is.null(nemetonshiny:::marculus_tarifs_travail(project, ess)))
})

test_that("sans tarifs, le contexte est identique a celui d'avant", {
  p <- list(id = "p", metadata = list(name = "F"))
  a <- list(id = "x", ug_id = "u", type = "eclaircie", annee_cible = 2L)
  ess <- c("Chêne sessile", "Hêtre")
  avant <- nemetonshiny:::marculus_context_from_action(a, p, essences = ess)
  apres <- nemetonshiny:::marculus_context_from_action(a, p, essences = ess, tarifs = NULL)
  avant$dateCreation <- avant$modifie <- apres$dateCreation <- apres$modifie <- 0
  expect_identical(jsonlite::toJSON(apres, auto_unbox = TRUE),
                   jsonlite::toJSON(avant, auto_unbox = TRUE))
  expect_false(any(c("ser", "millesimeTarifs", "tarifsProposes") %in% names(avant)))
})

test_that("avec tarifs, le contexte porte ser, millesime et propositions filtrees", {
  tab <- .tab_tarifs()
  tarifs <- list(ser = "C30", millesime = "IFN 2011-2025", essences = list(
    "Chêne sessile" = nemetonshiny:::.marculus_tarifs_essence(tab[tab$espar == "03", ]),
    "Hêtre" = nemetonshiny:::.marculus_tarifs_essence(tab[tab$espar == "09", ])))
  p <- list(id = "p", metadata = list(name = "F"))
  a <- list(id = "x", ug_id = "u", type = "eclaircie")
  ctx <- nemetonshiny:::marculus_context_from_action(
    a, p, essences = c("Hêtre", "Douglas"), tarifs = tarifs)
  expect_identical(ctx$ser, "C30")
  expect_identical(ctx$millesimeTarifs, "IFN 2011-2025")
  # Essences du contexte seulement ; Douglas sans proposition : absente.
  expect_identical(names(ctx$tarifsProposes), "Hêtre")
  # Jamais de reglage du terrain.
  expect_false(any(c("tarif", "tarifNumero", "tarifNumeroParEssence") %in% names(ctx)))
  j <- jsonlite::fromJSON(nemetonshiny:::marculus_sync_json(list(ctx)),
                          simplifyVector = FALSE)
  h <- j$contextes[[1]]$tarifsProposes[["Hêtre"]]
  expect_equal(h$SCHL$numero, 11)
  expect_identical(h$SCHL$niveau, "ser")
  expect_equal(h$SCHL$cv, 0.12)
  expect_identical(h$conseille, "SCHL")
  # Aucune essence couverte : objet vide, pas un tableau.
  ctx2 <- nemetonshiny:::marculus_context_from_action(
    a, p, essences = "Douglas", tarifs = tarifs)
  expect_match(nemetonshiny:::marculus_sync_json(list(ctx2)),
               "\"tarifsProposes\": \\{\\}")
})

test_that("le message de fin d'export compte les essences par niveau", {
  i18n <- nemetonshiny:::get_i18n("fr")
  tab <- .tab_tarifs()
  tarifs <- list(essences = list(
    "Chêne sessile" = nemetonshiny:::.marculus_tarifs_essence(tab[tab$espar == "03", ]),
    "Hêtre" = nemetonshiny:::.marculus_tarifs_essence(tab[tab$espar == "09", ])))
  cpt <- nemetonshiny:::.marculus_tarifs_comptes(
    tarifs, c("Chêne sessile", "Hêtre", "Frêne"))
  expect_identical(cpt, list(n = 2L, foret = 1L, ser = 1L, repli = 0L))
  base <- list(n_contexts = 1L, n_gpkg = 1L, n_essences = 3L, n_houppiers = 1L,
               has_desserte = TRUE)
  m <- nemetonshiny:::.marculus_export_notice(
    c(base, list(tarifs_statut = "ok", tarifs_comptes = cpt)), i18n)
  expect_match(m$message, "2 essences sur 3 (niveau forêt : 1, SER : 1, repli : 0)",
               fixed = TRUE)
  expect_identical(m$type, "message")
  m <- nemetonshiny:::.marculus_export_notice(
    c(base, list(tarifs_statut = "echec", tarifs_erreur = "IFN injoignable")), i18n)
  expect_match(m$message, "IFN injoignable", fixed = TRUE)
  expect_identical(m$type, "warning")
})

test_that("l'export sans cache de tarifs reussit, sans les trois cles", {
  skip_if_not_installed("sf")
  poly <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  withr::with_tempdir({
    racine <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = racine),
      {
        parcels <- sf::st_sf(id = "p1", section = "A", numero = "1", commune = "1",
                             contenance = 1e4, geometry = sf::st_sfc(poly, crs = 4326))
        pid <- nemetonshiny:::create_project(name = "Lot", parcels = parcels)$id
        jsonlite::write_json(list(
          version = 1L, project_id = pid, horizon_annees = 20L,
          actions = list(list(id = "a1", ug_id = "ug_1", type = "eclaircie",
                              annee_cible = 2L, priorite = "haute",
                              statut = "validee")), audit = list()),
          file.path(nemetonshiny:::get_project_path(pid), "data", "action_plan.json"),
          auto_unbox = TRUE)
        z <- file.path(racine, "lot.zip")
        res <- nemetonshiny:::marculus_export_bundle(pid, z, essences = "Hêtre")
        expect_identical(res$tarifs_statut, "non_calcule")
        d <- file.path(racine, "ouvert"); dir.create(d)
        utils::unzip(z, exdir = d)
        j <- jsonlite::fromJSON(list.files(d, "[.]marsync$", full.names = TRUE),
                                simplifyVector = FALSE)
        expect_false(any(c("ser", "millesimeTarifs", "tarifsProposes") %in%
                           names(j$contextes[[1]])))
      })
  })
})

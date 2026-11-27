# Onglet ICD du Suivi sanitaire : presence, carte, clic -> deux graphiques,
# comparaison avec FAST, calcul asynchrone et nettoyage des notifications.
# Le coeur est simule (`read_icd_*`, `comparer_fast_icd`, tache asynchrone).

# --- Serie de pixel factice, au format de `read_icd_pixel_series()` ----------
.serie_icd <- function(essence = "chene", annee_prov = 2026L) {
  idx <- c("GRVI", "CIre", "CRSWIR", "CRre", "NDVI")
  w <- nemeton::icd_poids_defaut()
  w <- unlist(w[w$essence == essence, idx])
  actifs <- names(w)[w > 0]
  serie <- expand.grid(annee = 2024:2026, mois = 6:8, indice = idx,
                       stringsAsFactors = FALSE)
  serie$brut <- 0.5; serie$z <- -1
  icd <- expand.grid(annee = 2024:2026, mois = 6:8)
  icd$icd <- -2.5; icd$sensible <- TRUE
  list(essence = essence, motif = NA_character_, serie = serie, icd = icd,
       annuel = data.frame(annee = 2024:2026, classe = c(0, 1, 2),
                           niveau = c(0, 1, 2), icd_min = c(-1, -1.6, -2.5),
                           mois_alerte = c(NA, NA, 7L),
                           date_alerte = as.Date(c(NA, NA, "2026-07-01")),
                           provisoire = c(FALSE, FALSE, TRUE)),
       poids = w, indices_actifs = actifs, mois_sensibles = 6:8,
       seuils = c(surveillance = -1.5, alerte = -2, alerte_forte = -3),
       annee_provisoire = annee_prov)
}

.niveau_raster <- function() {
  r <- terra::rast(nrows = 4, ncols = 4, xmin = 800000, xmax = 800080,
                   ymin = 6500000, ymax = 6500080, crs = "EPSG:2154")
  terra::values(r) <- c(0:4, NA)[(seq_len(16) %% 6) + 1]
  names(r) <- "niveau"
  attr(r, "n_pixels_zone") <- 16L; attr(r, "n_pixels_icd") <- 13L
  attr(r, "n_pixels_sans_icd") <- 3L
  r
}

# --- Graphiques ----------------------------------------------------------------

test_that("le graphique de l'ICD trace les trois seuils et marque l'annee provisoire", {
  i18n <- get_i18n("fr")
  p <- plotly::plotly_build(nemetonshiny:::plot_icd_composite(.serie_icd(), i18n))
  formes <- Filter(function(s) identical(s$type, "line"), p$x$layout$shapes)
  expect_setequal(vapply(formes, function(s) s$y0, numeric(1)), c(-1.5, -2, -3))
  traces <- p$x$data
  nom_prov <- sprintf("2026 (%s)", i18n$t("icd_provisoire"))
  prov <- Filter(function(t) identical(t$name, nom_prov), traces)[[1]]
  expect_identical(prov$line$dash, "dash")
  expect_identical(prov$marker$symbol, "circle-open")
  ancien <- Filter(function(t) identical(t$name, "2025"), traces)[[1]]
  expect_identical(ancien$line$dash, "solid")
  # Mois d'alerte (juillet 2026) mis en evidence.
  alerte <- Filter(function(t) identical(t$name, i18n$t("icd_mois_alerte")), traces)
  expect_length(alerte, 1L)
  expect_equal(as.character(as.Date(alerte[[1]]$x)), "2026-07-15")
})

test_that("le graphique des indices omet CRre pour un resineux, pas pour un feuillu", {
  i18n <- get_i18n("fr")
  noms <- function(ess) unique(vapply(
    plotly::plotly_build(nemetonshiny:::plot_icd_indices(.serie_icd(ess), "z", i18n))$x$data,
    function(t) t$name, character(1)))
  expect_false("CRre" %in% noms("sapin_epicea"))
  expect_false("CRre" %in% noms("douglas"))
  expect_true("CRre" %in% noms("hetre"))
  expect_setequal(noms("chene"), c("GRVI", "CIre", "CRSWIR", "CRre", "NDVI"))
})

test_that("le graphique des indices montre les valeurs brutes sur demande", {
  i18n <- get_i18n("fr")
  s <- .serie_icd(); s$serie$z <- -1; s$serie$brut <- 0.5
  y <- function(v) plotly::plotly_build(
    nemetonshiny:::plot_icd_indices(s, v, i18n))$x$data[[1]]$y
  expect_true(all(y("z") == -1))
  expect_true(all(y("brut") == 0.5))
})

test_that("les mois sensibles sont surlignes", {
  p <- plotly::plotly_build(nemetonshiny:::plot_icd_indices(
    .serie_icd(), "z", get_i18n("fr")))
  rect <- Filter(function(s) identical(s$type, "rect"), p$x$layout$shapes)
  expect_length(rect, 3L)    # une bande par annee
})

test_that("l'en-tete dit essence, poids, classe, niveau et date d'alerte", {
  i18n <- get_i18n("fr")
  e <- nemetonshiny:::icd_entete_pixel(.serie_icd(), 2026L, i18n)
  txt <- paste(e, collapse = " | ")
  expect_match(txt, i18n$t("icd_ess_chene"), fixed = TRUE)
  expect_match(txt, "GRVI", fixed = TRUE)
  expect_match(txt, i18n$t("icd_cls_2"), fixed = TRUE)
  expect_match(txt, "2026-07", fixed = TRUE)
})

# --- Interface : l'onglet vient apres RECONFORT ---------------------------------

test_that("l'onglet ICD et son sous-onglet suivent RECONFORT", {
  html <- paste(as.character(nemetonshiny:::mod_monitoring_ui("m")), collapse = " ")
  pos <- function(v) regexpr(sprintf('data-value="%s"', v), html, fixed = TRUE)[1]
  expect_true(pos("icd") > pos("reconfort"))
  expect_true(pos("pixel_map_icd") > pos("pixel_map_reconfort"))
  expect_true(pos("pixel_map_icd") < pos("validation_sampling_fast"))
  expect_match(html, "run_icd", fixed = TRUE)
  # Le calcul est l'action principale de l'onglet : vert, jamais ambre.
  expect_match(html, "btn-primary", fixed = TRUE)
  expect_false(grepl("btn-ia", html, fixed = TRUE))
})

# --- Sous-module : carte, clic, comparaison ------------------------------------

.etat_icd <- function(poids_obsoletes = FALSE) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  rd <- file.path(dir, "cache", "layers", "icd")
  dir.create(rd, recursive = TRUE)
  p <- nemetonshiny:::project_icd_poids(NULL)
  v <- nemetonshiny:::icd_poids_resume(p)$valide
  nemetonshiny:::.icd_ecrire_temoin(rd, 9L, v, 2024:2026, 2026L)
  if (poids_obsoletes) p$GRVI[1] <- 0.9
  list(path = dir, metadata = list(icd_poids = list(poids = lapply(
    stats::setNames(seq_len(nrow(p)), p$essence), function(i)
      as.list(p[i, c("GRVI", "CIre", "CRSWIR", "CRre", "NDVI")])))))
}

test_that("le clic ouvre une modale avec les deux graphiques et appelle le coeur", {
  modale <- NULL
  appel <- NULL
  testthat::local_mocked_bindings(
    showModal = function(ui, ...) { modale <<- paste(as.character(ui), collapse = " ") },
    .package = "shiny")
  testthat::local_mocked_bindings(
    icd_lire_carte = function(...) .niveau_raster(),
    icd_lire_pixel = function(zone, lng, lat, rd, ...) { appel <<- c(zone, lng, lat); .serie_icd() })
  app_state <- shiny::reactiveValues(language = "fr", current_project = .etat_icd())
  shiny::testServer(nemetonshiny:::mod_monitoring_icd_server,
    args = list(app_state = app_state, zone_id_r = shiny::reactive(9L)), {
      session$setInputs(annee = "2026")
      session$setInputs(map_click = list(lat = 45.1, lng = 5.2))
      expect_equal(appel, c(9, 5.2, 45.1))
  })
  expect_match(modale, "icd_g1", fixed = TRUE)
  expect_match(modale, "icd_g2", fixed = TRUE)
  expect_match(modale, get_i18n("fr")$t("icd_ess_chene"), fixed = TRUE)
})

test_that("un pixel hors des quatre peuplements ouvre une modale qui le dit", {
  modale <- NULL
  testthat::local_mocked_bindings(
    showModal = function(ui, ...) { modale <<- paste(as.character(ui), collapse = " ") },
    .package = "shiny")
  testthat::local_mocked_bindings(
    icd_lire_carte = function(...) .niveau_raster(),
    icd_lire_pixel = function(...) list(essence = NA_character_, motif = "x"))
  app_state <- shiny::reactiveValues(language = "fr", current_project = .etat_icd())
  shiny::testServer(nemetonshiny:::mod_monitoring_icd_server,
    args = list(app_state = app_state, zone_id_r = shiny::reactive(9L)), {
      session$setInputs(annee = "2026")
      session$setInputs(map_click = list(lat = 45.1, lng = 5.2))
  })
  expect_match(modale, get_i18n("fr")$t("icd_pas_icd"), fixed = TRUE)
  expect_false(grepl("icd_g1", modale, fixed = TRUE))
})

test_that("des poids modifies gardent la carte et affichent le bandeau", {
  testthat::local_mocked_bindings(icd_lire_carte = function(...) .niveau_raster())
  app_state <- shiny::reactiveValues(language = "fr",
                                     current_project = .etat_icd(poids_obsoletes = TRUE))
  shiny::testServer(nemetonshiny:::mod_monitoring_icd_server,
    args = list(app_state = app_state, zone_id_r = shiny::reactive(9L)), {
      session$setInputs(annee = "2026")
      expect_true(obsolete_r())
      expect_false(is.null(raster_r()))             # la carte reste
      html <- paste(as.character(output$bandeau), collapse = " ")
      expect_match(html, get_i18n("fr")$t("icd_poids_modifies"), fixed = TRUE)
  })
})

test_that("sans bandeau quand les poids n'ont pas change", {
  testthat::local_mocked_bindings(icd_lire_carte = function(...) .niveau_raster())
  app_state <- shiny::reactiveValues(language = "fr", current_project = .etat_icd())
  shiny::testServer(nemetonshiny:::mod_monitoring_icd_server,
    args = list(app_state = app_state, zone_id_r = shiny::reactive(9L)), {
      session$setInputs(annee = "2026")
      expect_false(grepl("alert-warning",
                         paste(as.character(output$bandeau), collapse = " ")))
  })
})

test_that("carte et modale disent que les coupes rases ne sont pas retirees", {
  modale <- NULL
  testthat::local_mocked_bindings(
    showModal = function(ui, ...) { modale <<- paste(as.character(ui), collapse = " ") },
    .package = "shiny")
  testthat::local_mocked_bindings(
    icd_lire_carte = function(...) .niveau_raster(),
    icd_lire_pixel = function(...) .serie_icd(),
    icd_coupes_retirees = function(...) FALSE)
  app_state <- shiny::reactiveValues(language = "fr", current_project = .etat_icd())
  msg <- get_i18n("fr")$t("icd_coupes_non_retirees")
  shiny::testServer(nemetonshiny:::mod_monitoring_icd_server,
    args = list(app_state = app_state, zone_id_r = shiny::reactive(9L)), {
      session$setInputs(annee = "2026")
      expect_match(paste(as.character(output$sans_icd), collapse = " "), msg, fixed = TRUE)
      session$setInputs(map_click = list(lat = 45.1, lng = 5.2))
  })
  expect_match(modale, msg, fixed = TRUE)
})

test_that("coupes retirees : aucune mention", {
  testthat::local_mocked_bindings(
    icd_lire_carte = function(...) .niveau_raster(),
    icd_coupes_retirees = function(...) TRUE)
  app_state <- shiny::reactiveValues(language = "fr", current_project = .etat_icd())
  shiny::testServer(nemetonshiny:::mod_monitoring_icd_server,
    args = list(app_state = app_state, zone_id_r = shiny::reactive(9L)), {
      session$setInputs(annee = "2026")
      expect_false(grepl(get_i18n("fr")$t("icd_coupes_non_retirees"),
                         paste(as.character(output$sans_icd), collapse = " "), fixed = TRUE))
  })
})

test_that("une annee sans classe ne cherche pas de cle icd_cls_NA", {
  s <- .serie_icd(); s$annuel$classe[3] <- NA; s$annuel$niveau[3] <- NA
  i18n <- get_i18n("fr")
  expect_silent(e <- nemetonshiny:::icd_entete_pixel(s, 2026L, i18n))
  expect_match(paste(e, collapse = " "), i18n$t("icd_cls_na"), fixed = TRUE)
})

test_that("les en-tetes du tableau croise passent par i18n", {
  cr <- expand.grid(fast = 0:1, icd = 0:1); cr$surface_ha <- 1
  html <- as.character(nemetonshiny:::.icd_table_croisement(cr, get_i18n("en")))
  expect_match(html, "FAST 0", fixed = TRUE)
  expect_match(html, "ICD 1", fixed = TRUE)
  expect_true(all(c("icd_cmp_ent_fast", "icd_cmp_ent_icd") %in% names(nemetonshiny:::TRANSLATIONS)))
})

test_that("des poids inchanges laissent la carte et disent combien de pixels sans ICD", {
  testthat::local_mocked_bindings(icd_lire_carte = function(...) .niveau_raster())
  app_state <- shiny::reactiveValues(language = "fr", current_project = .etat_icd())
  shiny::testServer(nemetonshiny:::mod_monitoring_icd_server,
    args = list(app_state = app_state, zone_id_r = shiny::reactive(9L)), {
      session$setInputs(annee = "2026")
      expect_false(obsolete_r())
      expect_false(is.null(raster_r()))
      html <- paste(as.character(output$sans_icd), collapse = " ")
      expect_match(html, "3 pixels sans ICD sur 16", fixed = TRUE)
  })
})

test_that("la comparaison est vide et le dit quand FAST n'a pas tourne", {
  testthat::local_mocked_bindings(
    icd_lire_carte = function(...) .niveau_raster(),
    icd_comparer_fast = function(...) list(statut = "fast_absent", annee = 2026L,
                                           n_pixels = 0L, croisement = NULL))
  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) NULL,
    close_monitoring_db_connection = function(...) invisible(TRUE))
  app_state <- shiny::reactiveValues(language = "fr", current_project = .etat_icd())
  shiny::testServer(nemetonshiny:::mod_monitoring_icd_server,
    args = list(app_state = app_state, zone_id_r = shiny::reactive(9L)), {
      session$setInputs(annee = "2026")
      expect_null(comparaison <- NULL)
      session$setInputs(cmp_run = 1)
      html <- paste(as.character(output$comparaison), collapse = " ")
      expect_match(html, get_i18n("fr")$t("icd_cmp_fast_absent"), fixed = TRUE)
      expect_false(grepl("<table", html, fixed = TRUE))
  })
})

test_that("la comparaison rend le croisement en ha, l'accord et le kappa comme indicateur", {
  cr <- expand.grid(fast = 0:4, icd = 0:4)
  cr$n_pixels <- 10L; cr$surface_ha <- 0.4
  cmp <- list(statut = "ok", annee = 2026L, n_pixels = 250L, croisement = cr,
              accord_global = 0.4, kappa = 0.123,
              surface_egale = list(part_alertee = 0.1, n_alertes = 25L,
                                   accord = 0.8, kappa = 0.3, les_deux = 1,
                                   fast_seul = 2, icd_seul = 3, aucune = 4))
  testthat::local_mocked_bindings(
    icd_lire_carte = function(...) .niveau_raster(),
    icd_comparer_fast = function(...) cmp)
  testthat::local_mocked_bindings(
    get_monitoring_db_connection = function(...) NULL,
    close_monitoring_db_connection = function(...) invisible(TRUE))
  app_state <- shiny::reactiveValues(language = "fr", current_project = .etat_icd())
  shiny::testServer(nemetonshiny:::mod_monitoring_icd_server,
    args = list(app_state = app_state, zone_id_r = shiny::reactive(9L)), {
      session$setInputs(annee = "2026")
      session$setInputs(cmp_run = 1)
      html <- paste(as.character(output$comparaison), collapse = " ")
      expect_match(html, "<table", fixed = TRUE)
      expect_match(html, "40.0 %", fixed = TRUE)
      expect_match(html, "0.123", fixed = TRUE)
      expect_match(html, get_i18n("fr")$t("icd_cmp_kappa_note"), fixed = TRUE)
      expect_match(html, get_i18n("fr")$t("icd_cmp_egale_titre"), fixed = TRUE)
  })
})

# --- Progression : le toast disparait aux evenements terminaux ------------------

test_that("les evenements terminaux retirent le toast, les tardifs sont ignores", {
  poses <- character(0); retires <- character(0); msgs <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, id = NULL, ...) { poses <<- c(poses, id); invisible(id) },
    removeNotification = function(id, ...) { retires <<- c(retires, id); invisible(id) },
    .package = "shiny")
  sess <- list(ns = function(x) paste0("mon-", x))
  i18n <- get_i18n("fr")
  on_msg <- function(m) msgs <<- c(msgs, if (is.null(m)) "<fin>" else as.character(m))
  h <- nemetonshiny:::.icd_handle_progress_event
  h(list(current = "s2:scene", completed = 3, total = 40), sess, i18n,
    on_msg = on_msg, est_actif = function() TRUE)
  h(list(current = "icd:composite", completed = 5, total = 70), sess, i18n,
    on_msg = on_msg, est_actif = function() TRUE)
  expect_length(poses, 2L)
  expect_match(msgs[1], "3/40")
  expect_match(msgs[2], "5/70")
  for (fin in c("icd:complete", "icd:cancelled", "icd:error")) {
    retires <- character(0)
    h(list(current = fin), sess, i18n, on_msg = on_msg, est_actif = function() TRUE)
    expect_identical(retires, "mon-icd_progress")
  }
  # Evenement tardif d'un run arrete : ni toast ni chrono.
  poses <- character(0)
  h(list(current = "icd:composite", completed = 6, total = 70), sess, i18n,
    on_msg = on_msg, est_actif = function() FALSE)
  expect_length(poses, 0L)
})

# --- Calcul asynchrone depuis le module Suivi sanitaire ------------------------

.fake_task_icd <- function(result = NULL, status = "initial") {
  st <- new.env(parent = emptyenv()); st$calls <- list()
  list(invoke = function(...) { st$calls[[length(st$calls) + 1L]] <- list(...); invisible(NULL) },
       result = function() result, status = function() status,
       .calls = function() st$calls)
}

.run_monitoring_icd <- function(icd = .fake_task_icd(), project = NULL, expr,
                                con = NULL) {
  notes <- character(0); retires <- character(0)
  testthat::local_mocked_bindings(
    showNotification = function(ui, id = NULL, ...) {
      notes <<- c(notes, paste(as.character(ui), collapse = " ")); invisible(id) },
    removeNotification = function(id, ...) { retires <<- c(retires, id); invisible(id) },
    .package = "shiny")
  app_state <- shiny::reactiveValues(language = "fr", current_project = project)
  testthat::with_mocked_bindings(
    get_monitoring_db_connection   = function(...) con,
    close_monitoring_db_connection = function(con) invisible(TRUE),
    run_icd_async                  = function() icd,
    .resolve_s2_cache_dir          = function(...) "cache-s2",
    .resolve_monitoring_db_url     = function(...) "db://x",
    shiny::testServer(nemetonshiny:::mod_monitoring_server,
                      args = list(app_state = app_state), {
      session$flushReact()
      expr(session, input, app_state)
    })
  )
  list(notes = notes, retires = retires, task = icd)
}

# Une base factice qui porte la zone `_tot` : `fordead_zone_id()` la resout.
.avec_zone_tot <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    find_zones_by_project = function(con, ...) data.frame(
      id = 4L, name = "projet_tot", stringsAsFactors = FALSE),
    .package = "nemeton", .env = env)
  testthat::local_mocked_bindings(
    get_monitoring_zone_aoi = function(...) NULL, .env = env)
}

test_that("sans BD Foret le calcul ne part pas et le dit", {
  proj <- list(path = withr::local_tempdir(), id = "p", metadata = list())
  testthat::local_mocked_bindings(.icd_bdforet_path = function(...) NULL)
  .avec_zone_tot()
  r <- .run_monitoring_icd(project = proj, con = structure(list(), class = "fausse"), expr = function(session, input, st) {
    session$flushReact()
    session$setInputs(run_icd = 1)
  })
  expect_length(r$task$.calls(), 0L)
  expect_true(any(grepl(get_i18n("fr")$t("icd_bdforet_absente"), r$notes, fixed = TRUE)))
})

test_that("un calcul valide part avec les poids du projet, la BD Foret et la zone _tot", {
  proj <- list(path = withr::local_tempdir(), id = "p",
               metadata = list(icd_poids = list(poids = list(
                 chene = list(GRVI = 0.5, CIre = 0.2, CRSWIR = 0.1, CRre = 0.1, NDVI = 0.1)))))
  bd <- file.path(proj$path, "bd.gpkg")
  testthat::local_mocked_bindings(
    .icd_bdforet_path = function(...) bd,
    .icd_result_dir = function(...) "res")
  .avec_zone_tot()
  r <- .run_monitoring_icd(project = proj, con = structure(list(), class = "fausse"), expr = function(session, input, st) {
    session$flushReact()
    session$setInputs(run_icd = 1)
  })
  appels <- r$task$.calls()
  expect_length(appels, 1L)
  expect_equal(appels[[1]]$zone_id, 4L)
  expect_identical(appels[[1]]$bdforet_path, bd)
  expect_equal(appels[[1]]$poids$GRVI[1], 0.5)       # poids du projet
  expect_identical(appels[[1]]$cache_dir, "cache-s2")
})

test_that("une annulation du coeur est dite et retire le toast", {
  r <- .run_monitoring_icd(
    icd = .fake_task_icd(result = list(status = "cancelled"), status = "success"),
    expr = function(session, input, st) NULL)
  expect_true(any(grepl(get_i18n("fr")$t("icd_cancelled"), r$notes, fixed = TRUE)))
  expect_true(any(grepl("icd_progress", r$retires)))
  expect_false(any(grepl("ICD calcul", r$notes)))
})

test_that("un calcul abouti est annonce et retire le toast a chrono", {
  r <- .run_monitoring_icd(
    icd = .fake_task_icd(result = list(status = "success", n_pixels_icd = 321L),
                         status = "success"),
    expr = function(session, input, st) NULL)
  expect_true(any(grepl("321", r$notes)))
  expect_true(any(grepl("icd_progress", r$retires)))
})

test_that("une erreur du calcul est dite lisiblement et retire le toast", {
  tache <- .fake_task_icd(status = "error")
  tache$result <- function() stop("panne reseau")
  r <- .run_monitoring_icd(icd = tache, expr = function(session, input, st) NULL)
  expect_true(any(grepl("panne reseau", r$notes)))
  expect_true(any(grepl("icd_progress", r$retires)))
})

test_that("l'arret d'un calcul actif retire le toast, pose le drapeau et le signal d'arret", {
  proj <- list(path = withr::local_tempdir(), id = "p", metadata = list())
  bd <- file.path(proj$path, "bd.gpkg")
  testthat::local_mocked_bindings(.icd_bdforet_path = function(...) bd)
  .avec_zone_tot()
  r <- .run_monitoring_icd(
    project = proj, con = structure(list(), class = "fausse"),
    expr = function(session, input, st) {
      session$flushReact()
      session$setInputs(run_icd = 1)             # calcul lance : le chrono demarre
      session$setInputs(run_icd_cancel = 1)
      expect_false(is.null(shiny::isolate(st$cancel_computation)))
    })
  expect_true(any(grepl("icd_progress", r$retires)))
  expect_true(any(grepl(get_i18n("fr")$t("icd_arret_demande"), r$notes, fixed = TRUE)))
  expect_true(file.exists(file.path(proj$path, "data", "icd_cancel.flag")))
})

test_that("l'etape sante_icd existe, tient son ecouteur et saute sans zone", {
  expect_true("sante_icd" %in% nemetonshiny:::pipeline_all_step_ids())
  src <- paste(readLines(testthat::test_path("..", "..", "R", "mod_monitoring.R"),
                         warn = FALSE), collapse = "\n")
  expect_true(grepl('pipeline_targets(req, "sante_icd")', src, fixed = TRUE))
})

test_that("sans calcul ICD actif, l'arret ne pose pas de drapeau", {
  proj <- list(path = withr::local_tempdir(), id = "p", metadata = list())
  .run_monitoring_icd(
    icd = .fake_task_icd(status = "initial"), project = proj,
    expr = function(session, input, st) session$setInputs(run_icd_cancel = 1))
  expect_false(file.exists(file.path(proj$path, "data", "icd_cancel.flag")))
})

test_that("l'arret tue l'enfant plafonne par son marqueur", {
  proj <- list(path = withr::local_tempdir(), id = "p", metadata = list())
  tues <- NULL
  testthat::local_mocked_bindings(
    .moteurs_sante_arreter = function(path, moteurs, ...) { tues <<- c(tues, moteurs); moteurs })
  .run_monitoring_icd(
    icd = .fake_task_icd(status = "running"), project = proj,
    expr = function(session, input, st) session$setInputs(run_icd_cancel = 1))
  expect_true("icd" %in% tues)
})

test_that("un enfant ICD encore vivant empeche un second lancement", {
  proj <- list(path = withr::local_tempdir(), id = "p", metadata = list())
  bd <- file.path(proj$path, "bd.gpkg")
  testthat::local_mocked_bindings(
    .icd_bdforet_path = function(...) bd,
    .moteur_sante_vivant = function(path, moteur, ...) if (moteur == "icd") list(pid = 1L) else NULL)
  .avec_zone_tot()
  r <- .run_monitoring_icd(project = proj, con = structure(list(), class = "fausse"),
    expr = function(session, input, st) { session$flushReact(); session$setInputs(run_icd = 1) })
  expect_length(r$task$.calls(), 0L)
  expect_true(any(grepl(get_i18n("fr")$t("monitoring_icd_verrou"), r$notes, fixed = TRUE)))
})

test_that("le couple SUFOSAT du cache part avec le calcul", {
  proj <- list(path = withr::local_tempdir(), id = "p", metadata = list())
  bd <- file.path(proj$path, "bd.gpkg")
  suf <- list(dates = "d.tif", proba = "p.tif")
  testthat::local_mocked_bindings(
    .icd_bdforet_path = function(...) bd, .icd_sufosat_chemins = function(...) suf)
  .avec_zone_tot()
  r <- .run_monitoring_icd(project = proj, con = structure(list(), class = "fausse"),
    expr = function(session, input, st) { session$flushReact(); session$setInputs(run_icd = 1) })
  a <- r$task$.calls()[[1]]
  expect_identical(a$sufosat, suf)
  expect_match(a$marker_path, "layers/icd/engine_run.json", fixed = TRUE)
})

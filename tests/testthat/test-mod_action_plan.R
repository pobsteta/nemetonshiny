# Tests for mod_action_plan helpers (the reactive UI logic is covered
# end-to-end in the Shiny app; here we focus on the pure helpers).

test_that("coerce_table_value parses ints, numerics, strings and blanks", {
  expect_identical(nemetonshiny:::coerce_table_value("annee_cible", "5"), 5L)
  expect_identical(nemetonshiny:::coerce_table_value("nb_tiges",   "120"), 120L)
  expect_identical(nemetonshiny:::coerce_table_value("volume_m3",  "12.5"), 12.5)
  expect_identical(nemetonshiny:::coerce_table_value("rdi",        "0.7"), 0.7)
  expect_identical(nemetonshiny:::coerce_table_value("commentaire", "abc"), "abc")
  # blanks become NA (caller decides what to do)
  expect_true(is.na(nemetonshiny:::coerce_table_value("annee_cible", "")))
  expect_true(is.na(nemetonshiny:::coerce_table_value("commentaire", NULL)))
})

test_that("module UI renders without error", {
  ui <- nemetonshiny:::mod_action_plan_ui("ap")
  # bslib widgets may be tag, tagList or htmlwidget shells — we just want
  # the call to succeed and produce HTML when rendered.
  expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list", "shiny.tag.env"),
                       which = FALSE) ||
              !is.null(htmltools::renderTags(ui)$html))
})

test_that("Kanban drag-and-drop assets are vendored in inst/app/www/js", {
  sortable <- system.file("app/www/js/Sortable-1.15.6.min.js",
                          package = "nemetonshiny")
  init_js  <- system.file("app/www/js/action_plan_kanban.js",
                          package = "nemetonshiny")
  expect_true(nzchar(sortable))
  expect_true(nzchar(init_js))
  # The init script must expose initKanbanSortable so the inline
  # <script> emitted by renderUI can call it.
  expect_match(paste(readLines(init_js, warn = FALSE), collapse = "\n"),
               "window\\.initKanbanSortable", fixed = FALSE)
  # Sortable.create is the API we rely on inside initKanbanSortable.
  expect_match(paste(readLines(sortable, warn = FALSE), collapse = "\n"),
               "Sortable", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Archive du PDF du plan d'action dans exports/ (spec 037, §5)
# ---------------------------------------------------------------------------
# La logique d'archivage vit dans le helper `.archive_action_plan_pdf`, appelé
# sur le SEUL chemin succès de output$download_pdf (le downloadHandler lui-même
# n'est pas invocable proprement en testServer sous cette version de shiny :
# registerDownload n'est pas câblé via le session_proxy). On teste donc le
# helper directement — il porte tout le comportement décrit au §5.

`%||%` <- function(a, b) if (is.null(a)) b else a

test_that("le PDF rendu est archivé sous exports/<slug>_action_plan.pdf", {
  pdir <- withr::local_tempdir()
  rendered <- tempfile(fileext = ".pdf")
  writeLines("%PDF-1.4 fake", rendered)
  project <- list(path = pdir, metadata = list(name = "Forêt de Test"))

  res <- nemetonshiny:::.archive_action_plan_pdf(rendered, project)
  expect_true(res)
  # Slug = caractères non [A-Za-z0-9_-] remplacés par « _ ».
  archived <- file.path(pdir, "exports", "For_t_de_Test_action_plan.pdf")
  expect_true(file.exists(archived))
  expect_match(readLines(archived, n = 1), "PDF")
})

test_that("exports/ est créé au besoin et l'archive écrase la précédente", {
  pdir <- withr::local_tempdir()
  project <- list(path = pdir, metadata = list(name = "Projet"))
  expect_false(dir.exists(file.path(pdir, "exports")))

  r1 <- tempfile(fileext = ".pdf"); writeLines("v1", r1)
  nemetonshiny:::.archive_action_plan_pdf(r1, project)
  archived <- file.path(pdir, "exports", "Projet_action_plan.pdf")
  expect_identical(readLines(archived, n = 1), "v1")

  # Un second export écrase l'archive (un seul PDF courant par projet).
  r2 <- tempfile(fileext = ".pdf"); writeLines("v2", r2)
  nemetonshiny:::.archive_action_plan_pdf(r2, project)
  expect_identical(readLines(archived, n = 1), "v2")
})

test_that("projet sans chemin disque -> pas d'archive, pas d'erreur", {
  rendered <- tempfile(fileext = ".pdf"); writeLines("%PDF fake", rendered)
  # path NULL
  expect_false(nemetonshiny:::.archive_action_plan_pdf(rendered,
    list(path = NULL, metadata = list(name = "X"))))
  # path inexistant
  expect_false(nemetonshiny:::.archive_action_plan_pdf(rendered,
    list(path = file.path(tempdir(), "nope-does-not-exist"),
         metadata = list(name = "X"))))
})

test_that("fichier rendu absent -> best-effort, aucune exception", {
  pdir <- withr::local_tempdir()
  project <- list(path = pdir, metadata = list(name = "Projet"))
  # Source inexistante : file.copy renvoie FALSE (avec un warning), pas d'erreur.
  expect_no_error(
    res <- suppressWarnings(
      nemetonshiny:::.archive_action_plan_pdf(
        file.path(tempdir(), "absent.pdf"), project)))
  expect_false(res)
})

test_that("nom de projet manquant -> slug de repli nemeton_action_plan.pdf", {
  pdir <- withr::local_tempdir()
  rendered <- tempfile(fileext = ".pdf"); writeLines("%PDF fake", rendered)
  project <- list(path = pdir, metadata = list())   # ni name ni id
  expect_true(nemetonshiny:::.archive_action_plan_pdf(rendered, project))
  expect_true(file.exists(file.path(pdir, "exports", "nemeton_action_plan.pdf")))
})


test_that("le bouton IA du Plan d'actions porte l'accent ambre", {
  # v0.130.10 annoncait que l'accent IA couvrait « toute l'app » et nommait
  # quatre surfaces : Synthese, Plan d'actions, reGeneration, Famille. Trois
  # avaient un test ; le Plan d'actions n'en avait pas, et son bouton est reste
  # vert avec une baguette magique jusqu'a ce qu'un utilisateur le signale.
  skip_if_not_installed("bslib")
  h <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_action_plan_ui("ap")))

  expect_true(grepl("btn-ia", h, fixed = TRUE))
  expect_true(grepl("bi-stars", h, fixed = TRUE))
  # L'ancienne signaletique ne doit pas revenir par megarde.
  expect_false(grepl("wand-magic", h, fixed = TRUE))
})

test_that("aucune surface generatrice ne reste au vert", {
  # Garde-fou de SOURCE, celui qui manquait : il enumere les boutons qui
  # produisent du contenu genere et verifie que chacun porte `btn-ia`. Un test
  # par module aurait laisse passer le suivant - c'est exactement ce qui s'est
  # produit.
  boutons <- list(
    list(f = "mod_action_plan.R", id = "generate_all"),
    list(f = "mod_action_plan.R", id = "gen_run"),
    list(f = "mod_regeneration.R", id = NULL),
    list(f = "app_ui.R", id = "ai_generate"))

  for (b in boutons) {
    path <- testthat::test_path("..", "..", "R", b$f)
    testthat::skip_if_not(file.exists(path), "sources R absentes")
    code <- readLines(path, warn = FALSE)
    code <- code[!grepl("^\\s*#", code)]
    # La baguette magique et le robot sont les deux signaletiques que l'ambre a
    # remplacees : aucune ne doit subsister dans un fichier qui genere.
    expect_false(any(grepl("wand-magic|icon\\(\"robot\"\\)", code)), info = b$f)
    expect_true(any(grepl("btn-ia", code, fixed = TRUE)), info = b$f)
  }
})

# ---------------------------------------------------------------------------
# Cadrage de la carte des actions quand elle est masquee
# ---------------------------------------------------------------------------
# Changer de projet depuis un autre onglet envoyait `fitBounds` a une carte
# masquee (dimensions nulles) : elle cadrait le monde entier, et la signature
# de bbox deja posee empechait tout recadrage au retour sur l'onglet.

test_that(".action_plan_map_visible exige l'onglet ET le sous-onglet carte", {
  vis <- nemetonshiny:::.action_plan_map_visible
  expect_true(vis("action_plan", "map_table"))
  expect_true(vis("action_plan", NULL))   # sous-onglet pas encore rapporte
  expect_false(vis("action_plan", "kanban"))
  expect_false(vis("selection", "map_table"))
  expect_false(vis(NULL, NULL))
})

.action_plan_ug_sf <- function(x0) {
  poly <- sf::st_polygon(list(rbind(c(x0, 46), c(x0 + 0.01, 46),
                                    c(x0 + 0.01, 46.01), c(x0, 46.01),
                                    c(x0, 46))))
  sf::st_sf(ug_id = "ug1", label = "UGF 1",
            geometry = sf::st_sfc(poly, crs = 4326))
}

test_that("un projet charge carte masquee est cadre a l'arrivee sur l'onglet", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  testthat::local_mocked_bindings(
    ug_build_sf      = function(projet) .action_plan_ug_sf(projet$x0),
    load_action_plan = function(project_id) init_empty_action_plan(project_id)
  )
  app_state <- shiny::reactiveValues(
    language = "fr", active_main_tab = "selection",
    current_project = list(id = "p1", x0 = 5)
  )
  shiny::testServer(
    nemetonshiny:::mod_action_plan_server,
    args = list(app_state = app_state),
    {
      session$setInputs(map_color_by = "priorite")
      session$flushReact()
      # Carte masquee : le cadrage est differe, pas perdu.
      expect_false(is.null(rv_state$pending_fit_bbox))
      expect_equal(as.numeric(rv_state$pending_fit_bbox[["xmin"]]), 5)

      # Changement de projet, toujours depuis un autre onglet.
      app_state$current_project <- list(id = "p2", x0 = 6)
      session$flushReact()
      expect_equal(as.numeric(rv_state$pending_fit_bbox[["xmin"]]), 6)

      # Arrivee sur le Plan d'actions : le cadrage en attente est consomme.
      app_state$active_main_tab <- "action_plan"
      session$flushReact()
      expect_null(rv_state$pending_fit_bbox)
    }
  )
})

test_that("carte visible : cadrage immediat, rien en attente", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  testthat::local_mocked_bindings(
    ug_build_sf      = function(projet) .action_plan_ug_sf(projet$x0),
    load_action_plan = function(project_id) init_empty_action_plan(project_id)
  )
  app_state <- shiny::reactiveValues(
    language = "fr", active_main_tab = "action_plan",
    current_project = list(id = "p1", x0 = 5)
  )
  shiny::testServer(
    nemetonshiny:::mod_action_plan_server,
    args = list(app_state = app_state),
    {
      session$setInputs(map_color_by = "priorite", inner_nav = "map_table")
      session$flushReact()
      expect_null(rv_state$pending_fit_bbox)
      expect_false(is.null(rv_state$last_bbox_sig))
    }
  )
})


# ---- Suppression des actions selectionnees -----------------------------

.plan_trois_actions <- function(project_id) {
  plan <- init_empty_action_plan(project_id)
  for (id in c("a1", "a2", "a3")) {
    plan <- add_action_to_plan(plan, list(
      id = id, ug_id = "ug1", type = "eclaircie", annee_cible = 2L,
      priorite = "moyenne", statut = "proposee"), ug_ids = "ug1")
  }
  plan
}

test_that("le bouton Supprimer la selection est sous Ajouter, en outline-danger", {
  skip_if_not_installed("bslib")
  h <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_action_plan_ui("ap")))
  pos_add <- regexpr('id="ap-add_action"', h, fixed = TRUE)
  pos_del <- regexpr('id="ap-delete_selected"', h, fixed = TRUE)
  expect_true(pos_add > 0 && pos_del > pos_add)
  bouton <- regmatches(h, regexpr('<button[^>]*id="ap-delete_selected"[^>]*>', h))
  expect_match(bouton, "btn-outline-danger", fixed = TRUE)
})

test_that("supprimer la selection retire les lignes choisies et sauvegarde", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  sauve <- NULL
  testthat::local_mocked_bindings(
    ug_build_sf      = function(projet) .action_plan_ug_sf(5),
    load_action_plan = function(project_id) .plan_trois_actions(project_id),
    save_action_plan = function(project_id, plan) { sauve <<- plan; TRUE }
  )
  app_state <- shiny::reactiveValues(
    language = "fr", active_main_tab = "action_plan",
    current_project = list(id = "p1", x0 = 5),
    auth = list(authenticated = TRUE, user_roles = character())
  )
  shiny::testServer(
    nemetonshiny:::mod_action_plan_server,
    args = list(app_state = app_state),
    {
      session$flushReact()
      expect_length(plan_rv()$actions, 3L)
      ids <- actions_df_all()$id
      # Lignes 1 et 3 selectionnees, puis confirmation dans la modale.
      session$setInputs(action_table_rows_selected = c(1L, 3L))
      session$setInputs(delete_selected = 1)
      expect_setequal(delete_pending_rv(), ids[c(1L, 3L)])
      session$setInputs(delete_run = 1)
      expect_equal(vapply(plan_rv()$actions, `[[`, "", "id"), ids[2L])
      expect_equal(length(sauve$actions), 1L)
      expect_length(delete_pending_rv(), 0L)
    }
  )
})

test_that("sans selection ou en lecture seule, rien n'est supprime", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")

  sauve <- NULL
  testthat::local_mocked_bindings(
    ug_build_sf      = function(projet) .action_plan_ug_sf(5),
    load_action_plan = function(project_id) .plan_trois_actions(project_id),
    save_action_plan = function(project_id, plan) { sauve <<- plan; TRUE }
  )
  app_state <- shiny::reactiveValues(
    language = "fr", active_main_tab = "action_plan",
    current_project = list(id = "p1", x0 = 5),
    auth = list(authenticated = TRUE, user_roles = character())
  )
  shiny::testServer(
    nemetonshiny:::mod_action_plan_server,
    args = list(app_state = app_state),
    {
      session$flushReact()
      # Aucune ligne selectionnee : pas de modale, rien en attente.
      session$setInputs(delete_selected = 1)
      expect_length(delete_pending_rv(), 0L)

      # Selection faite, puis le projet passe en lecture seule avant la
      # confirmation : la garde est rejouee au moment de supprimer.
      session$setInputs(action_table_rows_selected = 2L)
      session$setInputs(delete_selected = 2)
      expect_length(delete_pending_rv(), 1L)
      app_state$readonly <- TRUE
      session$setInputs(delete_run = 1)
      expect_length(plan_rv()$actions, 3L)
      expect_null(sauve)
    }
  )
})


# ---- Marculus : fond ortho prepare avant le telechargement -------------

test_that("le bouton Marculus visible prepare, le telechargement est masque", {
  skip_if_not_installed("bslib")
  h <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_action_plan_ui("ap")))
  bouton <- regmatches(h, regexpr('<button[^>]*id="ap-prepare_marculus"[^>]*>', h))
  expect_length(bouton, 1L)
  telech <- regmatches(h, regexpr('<a[^>]*id="ap-download_marculus"[^>]*>', h))
  expect_match(telech, "d-none", fixed = TRUE)
})

test_that("fonds deja en cache : aucune tache de fond n'est lancee", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  testthat::local_mocked_bindings(
    ug_build_sf      = function(projet) .action_plan_ug_sf(5),
    load_action_plan = function(project_id) .plan_trois_actions(project_id),
    marculus_ortho_manquants = function(project, actions) list()
  )
  app_state <- shiny::reactiveValues(
    language = "fr", active_main_tab = "action_plan",
    current_project = list(id = "p1", x0 = 5)
  )
  shiny::testServer(
    nemetonshiny:::mod_action_plan_server,
    args = list(app_state = app_state),
    {
      session$flushReact()
      session$setInputs(prepare_marculus = 1)
      expect_identical(marculus_ortho_task$status(), "initial")
    }
  )
})

test_that("le lien Marculus masque est rendu malgre son masquage", {
  # Sans `suspendWhenHidden = FALSE`, Shiny ne rendait pas le lien cache : il
  # restait `disabled` sans href, et le clic du serveur ne telechargeait rien
  # (v0.146.0, constate sous Chrome headless).
  f <- testthat::test_path("..", "..", "R", "mod_action_plan.R")
  testthat::skip_if_not(file.exists(f), "sources R absentes")
  code <- readLines(f, warn = FALSE)
  expect_true(any(grepl('outputOptions(output, "download_marculus", suspendWhenHidden = FALSE)',
                        code, fixed = TRUE)))
})


# ---- Import du retour Marculus ------------------------------------------

test_that("le bouton d'import Marculus suit l'export, en outline-primary", {
  skip_if_not_installed("bslib")
  h <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_action_plan_ui("ap")))
  bouton <- regmatches(h, regexpr('<button[^>]*id="ap-import_marculus"[^>]*>', h))
  expect_match(bouton, "btn-outline-primary", fixed = TRUE)
  expect_true(regexpr('id="ap-prepare_marculus"', h) < regexpr('id="ap-import_marculus"', h))
})

test_that("importer un .marsync met le plan et les tiges a jour", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  proj <- withr::local_tempdir(); dir.create(file.path(proj, "data"))
  plan0 <- .plan_trois_actions("p1")
  sauve <- NULL
  testthat::local_mocked_bindings(
    ug_build_sf      = function(projet) .action_plan_ug_sf(5),
    load_action_plan = function(project_id) sauve %||% plan0,
    save_action_plan = function(project_id, plan) { sauve <<- plan; TRUE },
    get_project_path = function(id) proj
  )
  f <- file.path(proj, "retour.marsync")
  jsonlite::write_json(list(
    version = 3,
    contextes = data.frame(id = "a2", nom = "A2", statut = "REALISEE",
                           dateMartelage = NA, modifie = 1),
    tiges = data.frame(uuid = c("u1", "u2"), contexteId = "a2",
                       essence = "Chene", classe = 40L, action = "PLUS",
                       horodatage = 1:2, quantite = 1L, latitude = 47.9,
                       longitude = 1.9, modifie = 1)),
    f, auto_unbox = TRUE, na = "null")
  app_state <- shiny::reactiveValues(
    language = "fr", active_main_tab = "action_plan",
    current_project = list(id = "p1", x0 = 5),
    auth = list(authenticated = TRUE, user_roles = character())
  )
  shiny::testServer(
    nemetonshiny:::mod_action_plan_server,
    args = list(app_state = app_state),
    {
      session$flushReact()
      session$setInputs(marculus_fichiers = data.frame(
        name = "retour.marsync", size = file.size(f), type = "",
        datapath = f, stringsAsFactors = FALSE))
      session$setInputs(marculus_import_run = 1)
      a2 <- Filter(function(a) a$id == "a2", plan_rv()$actions)[[1]]
      expect_equal(a2$statut, "realisee")
      expect_equal(a2$quantite$nb_tiges, 2L)
      expect_equal(nrow(marculus_tiges_rv()), 2L)
      expect_false(is.null(sauve))
    }
  )
})

test_that("la synthese Marculus est une feuille : une ligne par essence", {
  # Une ligne par couple essence x classe faisait deborder la fenetre
  # (25 lignes pour une seule eclaircie de « Reconfort »).
  sub <- data.frame(essence = c("Hêtre", "Hêtre", "Sapin", "Hêtre"),
                    classe = c(20L, 40L, 20L, 80L), tiges = c(1L, 4L, 2L, 1L))
  h <- as.character(nemetonshiny:::.marculus_synthese_table(
    sub, nemetonshiny:::get_i18n("fr")))
  expect_equal(lengths(regmatches(h, gregexpr("<th scope=\"row\">", h))), 2L)
  expect_match(h, "table-responsive", fixed = TRUE)
  # Totaux : 6 pour le hetre, 2 pour le sapin, 8 au total.
  expect_match(h, ">6</td>", fixed = TRUE)
  expect_match(h, ">8</td>", fixed = TRUE)
})

test_that("la fiche Kanban affiche le martelage sous le commentaire", {
  i18n <- nemetonshiny:::get_i18n("fr")
  row <- list(date_martelage = "2027-10-15", nb_tiges = 42L,
              nb_tiges_biodiversite = 3L)
  h <- as.character(nemetonshiny:::.kanban_ligne_martelage(row, i18n))
  expect_match(h, "Martelage du 15/10/2027", fixed = TRUE)
  expect_match(h, "42 tige(s) désignée(s)", fixed = TRUE)
  expect_match(h, "dont 3 Biodiversité", fixed = TRUE)
  # Rien sans martelage : un nombre de tiges seul peut venir du plan IA.
  expect_null(nemetonshiny:::.kanban_ligne_martelage(
    list(date_martelage = NA, nb_tiges = 12L, nb_tiges_biodiversite = NA), i18n))
})

test_that("fiche et synthese affichent le volume martele", {
  i18n <- nemetonshiny:::get_i18n("fr")
  row <- list(date_martelage = "2027-10-15", nb_tiges = 42L,
              nb_tiges_biodiversite = 3L, volume_martele_m3 = 38.64)
  h <- as.character(nemetonshiny:::.kanban_ligne_martelage(row, i18n))
  expect_match(h, "dont 3 Biodiversité · 38,6 m³ bois fort", fixed = TRUE)

  sub <- data.frame(essence = c("Hêtre", "Sapin"), classe = c(40L, 20L),
                    tiges = c(2L, 1L), volume_m3 = c(1.74, 0.2))
  t <- as.character(nemetonshiny:::.marculus_synthese_table(sub, i18n))
  expect_match(t, "Volume (m³)", fixed = TRUE)
  expect_match(t, ">1,74</td>", fixed = TRUE)
  expect_match(t, ">1,94</td>", fixed = TRUE)
  # Sans volume exporte, pas de colonne Volume.
  sub$volume_m3 <- NA_real_
  t2 <- as.character(nemetonshiny:::.marculus_synthese_table(sub, i18n))
  expect_false(grepl("Volume (m³)", t2, fixed = TRUE))
})

test_that("la fiche Martelage porte carte, diagramme et tableau", {
  skip_if_not_installed("plotly")
  t <- data.frame(uuid = paste0("u", 1:3), contexteId = "a1",
                  essence = c("Hêtre", "Hêtre", "Sapin"),
                  classe = c(25L, 50L, 70L), action = "PLUS", horodatage = 1:3,
                  quantite = 1L, latitude = c(47.9, 47.901, NA),
                  longitude = c(1.9, 1.901, NA), modifie = 1,
                  volumeTigeM3 = c(0.3, 1.5, 2.8), mode = "DIAMETRE")
  t <- nemetonshiny:::.marculus_tiges_normaliser(t)
  ug <- sf::st_sf(ug_id = "ug_1", geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(1.899, 47.899), c(1.902, 47.899), c(1.902, 47.902), c(1.899, 47.902),
    c(1.899, 47.899)))), crs = 4326))
  f <- nemetonshiny:::.marculus_fiche_martelage(t, ug, nemetonshiny:::get_i18n("fr"))
  h <- as.character(htmltools::renderTags(f)$html)
  expect_match(h, "leaflet", fixed = TRUE)
  expect_match(h, "plotly", fixed = TRUE)
  expect_match(h, "datatables", fixed = TRUE)
  # Trois classes, trois categories distinctes (PB, GB, TGB) dans le diagramme.
  expect_match(h, "(PB)", fixed = TRUE)
  expect_match(h, "(TGB)", fixed = TRUE)
})

test_that("le choix de coloration vit dans une barre laterale droite", {
  skip_if_not_installed("bslib")
  h <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_action_plan_ui("ap")))
  # Comme la carte de reGeneration : sidebar a droite, toujours ouverte, carte
  # dans la zone principale et radio dans le panneau.
  expect_true(grepl(paste0('(?s)sidebar-right[^>]*data-open-desktop="always"',
                           '.*?id="ap-map".*?<aside class="sidebar"',
                           '.*?id="ap-map_color_by"'), h, perl = TRUE))
  expect_match(h, "Couche affich\u00e9e", fixed = TRUE)
})

test_that("Annee, Type et Priorite portent chacun leur « i » explicatif", {
  skip_if_not_installed("bslib")
  h <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_action_plan_ui("ap")))
  debut <- regexpr('id="ap-map_color_by"', h, fixed = TRUE)
  fin <- regexpr("</aside>", substring(h, debut), fixed = TRUE)
  bloc <- substr(h, debut, debut + fin)
  # Un « i » par couche, dans le libelle (un clic ne selectionne pas la couche).
  expect_equal(lengths(regmatches(bloc, gregexpr("<bslib-popover", bloc))), 3L)
  expect_match(h, "prochaine \u00e9ch\u00e9ance", fixed = TRUE)
})

test_that("le graphique du bilan est sous le tableau des actions", {
  skip_if_not_installed("bslib")
  h <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_action_plan_ui("ap")))
  expect_true(regexpr('id="ap-action_table"', h, fixed = TRUE) <
              regexpr('id="ap-balance_summary"', h, fixed = TRUE))
})

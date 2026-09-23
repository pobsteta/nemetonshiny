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

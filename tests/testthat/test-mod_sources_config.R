# ===========================================================================
# mod_sources_config — onglet « Sources optionnelles » du modal Paramètres
# ===========================================================================
#
# Les blocs SUFOSAT (T3) et LST (A5) ont quitté la carte Projet pour l'onglet
# Paramètres (roue dentée). Ce fichier couvre : les défauts « activé », le
# rendu des deux blocs selon l'état (pas de projet / pas de Theia / nominal),
# et la persistance déclenchée par les deux boutons Enregistrer.

# --- Défauts : sources actives sans métadonnées -----------------------------

test_that("project_sufosat_enabled / project_lst_enabled default to TRUE", {
  # Métadonnées absentes ou vides -> activé (nouveau défaut).
  expect_true(nemetonshiny:::project_sufosat_enabled(NULL))
  expect_true(nemetonshiny:::project_sufosat_enabled(list()))
  expect_true(nemetonshiny:::project_lst_enabled(NULL))
  expect_true(nemetonshiny:::project_lst_enabled(list()))
  # Seul un FALSE explicite désactive.
  expect_false(nemetonshiny:::project_sufosat_enabled(
    list(sufosat = list(enabled = FALSE))))
  expect_false(nemetonshiny:::project_lst_enabled(
    list(lst_urbain = list(enabled = FALSE))))
  # TRUE explicite reste TRUE.
  expect_true(nemetonshiny:::project_sufosat_enabled(
    list(sufosat = list(enabled = TRUE))))
  expect_true(nemetonshiny:::project_lst_enabled(
    list(lst_urbain = list(enabled = TRUE))))
})

# --- Rendu des blocs --------------------------------------------------------

# `output$<uiOutput>` en testServer rend une LISTE (html + dépendances) : un
# as.character() nu renvoie un vecteur de longueur 2 et grepl() y répond par un
# vecteur, ce qui rend expect_true() ambigu. On aplatit systématiquement.
.render_html <- function(x) paste(as.character(x), collapse = " ")

test_that("both blocks ask for a project when none is loaded", {
  shiny::testServer(nemetonshiny:::mod_sources_config_server, args = list(
    app_state = shiny::reactiveValues(language = "fr", current_project = NULL,
                                      project_id = NULL)
  ), {
    session$setInputs(x = 1)   # amorce le flush réactif
    html_s <- .render_html(output$sufosat_block)
    html_l <- .render_html(output$lst_block)
    need <- get_i18n("fr")$t("sources_need_project")
    expect_true(grepl(need, html_s, fixed = TRUE))
    expect_true(grepl(need, html_l, fixed = TRUE))
    # Aucun contrôle de saisie tant qu'il n'y a pas de projet.
    expect_false(grepl("sufosat_window", html_s, fixed = TRUE))
    expect_false(grepl("lst_buffer", html_l, fixed = TRUE))
  })
})

test_that("both blocks fall back to the Theia warning without credentials", {
  testthat::local_mocked_bindings(theia_api_key_configured = function() FALSE)
  shiny::testServer(nemetonshiny:::mod_sources_config_server, args = list(
    app_state = shiny::reactiveValues(
      language = "fr", project_id = "p1",
      current_project = list(id = "p1", metadata = list()))
  ), {
    session$setInputs(x = 1)
    i18n <- get_i18n("fr")
    expect_true(grepl(i18n$t("sufosat_need_theia"),
                      .render_html(output$sufosat_block), fixed = TRUE))
    expect_true(grepl(i18n$t("lst_need_theia"),
                      .render_html(output$lst_block), fixed = TRUE))
  })
})

test_that("blocks render the controls pre-checked when metadata is absent", {
  testthat::local_mocked_bindings(theia_api_key_configured = function() TRUE)
  shiny::testServer(nemetonshiny:::mod_sources_config_server, args = list(
    app_state = shiny::reactiveValues(
      language = "fr", project_id = "p1",
      current_project = list(id = "p1", metadata = list()))
  ), {
    session$setInputs(x = 1)
    html_s <- .render_html(output$sufosat_block)
    html_l <- .render_html(output$lst_block)
    # Défaut activé : la case est cochée ET le bandeau « Activé » est affiché.
    expect_true(grepl("checked", html_s, fixed = TRUE))
    expect_true(grepl("checked", html_l, fixed = TRUE))
    expect_true(grepl(get_i18n("fr")$t("sufosat_active"), html_s, fixed = TRUE))
    expect_true(grepl(get_i18n("fr")$t("lst_active"), html_l, fixed = TRUE))
    # Les paramètres sont bien là (sliders + bouton d'enregistrement).
    expect_true(grepl("sufosat_min_proba", html_s, fixed = TRUE))
    expect_true(grepl("lst_buffer", html_l, fixed = TRUE))
  })
})

test_that("an explicit enabled = FALSE renders the blocks unchecked", {
  testthat::local_mocked_bindings(theia_api_key_configured = function() TRUE)
  shiny::testServer(nemetonshiny:::mod_sources_config_server, args = list(
    app_state = shiny::reactiveValues(
      language = "fr", project_id = "p1",
      current_project = list(id = "p1", metadata = list(
        sufosat = list(enabled = FALSE),
        lst_urbain = list(enabled = FALSE))))
  ), {
    session$setInputs(x = 1)
    i18n <- get_i18n("fr")
    expect_true(grepl(i18n$t("sufosat_none"),
                      .render_html(output$sufosat_block), fixed = TRUE))
    expect_true(grepl(i18n$t("lst_none"),
                      .render_html(output$lst_block), fixed = TRUE))
  })
})

# --- Persistance ------------------------------------------------------------

test_that("the Save buttons persist both configs and refresh the project", {
  testthat::local_mocked_bindings(theia_api_key_configured = function() TRUE)
  seen_sufosat <- NULL
  seen_lst <- NULL
  testthat::local_mocked_bindings(
    set_project_sufosat = function(project_id, enabled, window_years, min_proba) {
      seen_sufosat <<- list(id = project_id, enabled = enabled,
                            window = window_years, proba = min_proba)
      TRUE
    },
    set_project_lst_urbain = function(project_id, enabled, buffer_m = 500) {
      seen_lst <<- list(id = project_id, enabled = enabled, buffer = buffer_m)
      TRUE
    },
    load_project = function(project_id) {
      list(id = project_id, metadata = list(sufosat = list(enabled = TRUE)))
    })

  app_state <- shiny::reactiveValues(
    language = "fr", project_id = "p1",
    current_project = list(id = "p1", metadata = list()))

  shiny::testServer(nemetonshiny:::mod_sources_config_server,
                    args = list(app_state = app_state), {
    session$setInputs(sufosat_enabled = TRUE, sufosat_window = 6,
                      sufosat_min_proba = 0.85)
    session$setInputs(sufosat_save = 1)
    expect_equal(seen_sufosat$id, "p1")
    expect_true(seen_sufosat$enabled)
    expect_equal(seen_sufosat$window, 6)
    expect_equal(seen_sufosat$proba, 0.85)

    session$setInputs(lst_enabled = FALSE, lst_buffer = 800)
    session$setInputs(lst_save = 1)
    expect_equal(seen_lst$id, "p1")
    expect_false(seen_lst$enabled)
    expect_equal(seen_lst$buffer, 800)

    # Le projet rechargé est republié dans app_state (le calcul aval le lit).
    expect_true(isTRUE(app_state$current_project$metadata$sufosat$enabled))
  })
})

test_that("saving without a project warns instead of writing", {
  testthat::local_mocked_bindings(theia_api_key_configured = function() TRUE)
  called <- FALSE
  testthat::local_mocked_bindings(
    set_project_sufosat = function(...) { called <<- TRUE; TRUE })
  shiny::testServer(nemetonshiny:::mod_sources_config_server, args = list(
    app_state = shiny::reactiveValues(language = "fr", current_project = NULL,
                                      project_id = NULL)
  ), {
    session$setInputs(sufosat_enabled = TRUE, sufosat_save = 1)
    expect_false(called)
  })
})


test_that("le bloc ONF des parametres est complet et coche par defaut", {
  # Trouve en LANCANT l'app : le bouton portait `i18n$t("save")`, une cle qui
  # n'existe pas - il affichait donc sa cle brute. Les blocs voisins ont chacun
  # la leur (`dess_params_save`, `acc_params_save`...), et rien dans les tests
  # unitaires ne regardait le libelle.
  skip_if_not_installed("bslib")
  i18n <- get_i18n("fr")
  expect_true(i18n$has("onf_params_save"))

  projet <- list(id = "p1", metadata = list(name = "F"))
  shiny::testServer(nemetonshiny:::mod_sources_config_server, args = list(
    app_state = shiny::reactiveValues(language = "fr", project_id = "p1",
                                      current_project = projet)), {
    session$setInputs(x = 1)
    h <- paste(as.character(output$onf_block), collapse = " ")

    for (id in c("onf_domanialite_cfg", "onf_purge_cfg", "onf_seuil_cfg",
                 "onf_clip_cfg", "onf_save")) {
      expect_true(grepl(id, h, fixed = TRUE), info = id)
    }
    # Le libelle, pas la cle.
    expect_true(grepl("Enregistrer les param", h, fixed = TRUE))

    # Defauts demandes : purge cochee, decoupe cochee, seuil a 0 %.
    expect_true(grepl('value="0"', h, fixed = TRUE))
    # Quatre controles coches : purge, decoupe, et les deux domanialites.
    expect_equal(length(gregexpr("checked", h)[[1]]), 8L)
  })
})

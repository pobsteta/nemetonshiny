# Tests for mod_ug.R
# UG (Management Units) Shiny module

# ==============================================================================
# UI tests
# ==============================================================================

test_that("mod_ug_ui returns valid Shiny UI", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  ui <- nemetonshiny:::mod_ug_ui("ug")
  expect_true(
    inherits(ui, "shiny.tag.list") || inherits(ui, "shiny.tag")
  )
})

test_that("mod_ug_ui contains expected elements", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  ui_html <- as.character(nemetonshiny:::mod_ug_ui("ug"))

  # Check for key UI elements
  expect_true(grepl("ug-btn_merge", ui_html))
  expect_true(grepl("ug-btn_split", ui_html))
  expect_true(grepl("ug-btn_rename", ui_html))
  expect_true(grepl("ug-sel_groupe", ui_html))
  expect_true(grepl("ug-ug_table", ui_html))

  # Boucle 1: leaflet map and create-from-map button
  expect_true(grepl("ug-ug_map", ui_html))
  expect_true(grepl("ug-btn_create_from_map", ui_html))
})

test_that("mod_ug_ui works in English", {
  withr::local_options(nemeton.app_options = list(language = "en"))
  ui <- nemetonshiny:::mod_ug_ui("ug")
  ui_html <- as.character(ui)

  expect_true(grepl("Management Units|Merge|Split", ui_html))
})


# ==============================================================================
# Integration with domain_ug tests
# ==============================================================================

test_that("GROUPES_AMENAGEMENT is defined", {
  groupes <- nemetonshiny:::GROUPES_AMENAGEMENT
  expect_type(groupes, "character")
  expect_true(length(groupes) >= 8)
  expect_true("TSF" %in% groupes)
  expect_true("HSN" %in% groupes)
  expect_true("REGT" %in% groupes)
})

test_that("GROUPE_COLORS has a color for each known groupe", {
  colors <- nemetonshiny:::GROUPE_COLORS
  groupes <- nemetonshiny:::GROUPES_AMENAGEMENT
  expect_true(all(groupes %in% names(colors)))
  # All values should be valid hex colors
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
})

test_that("ug_color returns groupe color when available", {
  color_tsf <- nemetonshiny:::ug_color("TSF", 1L)
  expect_equal(color_tsf, nemetonshiny:::GROUPE_COLORS[["TSF"]])

  color_hsn <- nemetonshiny:::ug_color("HSN", 1L)
  expect_equal(color_hsn, nemetonshiny:::GROUPE_COLORS[["HSN"]])
})

test_that("ug_color returns palette color when no groupe", {
  color_na <- nemetonshiny:::ug_color(NA_character_, 1L)
  expect_equal(color_na, nemetonshiny:::UG_PALETTE[1])

  color_empty <- nemetonshiny:::ug_color("", 3L)
  expect_equal(color_empty, nemetonshiny:::UG_PALETTE[3])
})

test_that("ug_color wraps palette index correctly", {
  n <- length(nemetonshiny:::UG_PALETTE)
  # Index beyond palette length should wrap
  color <- nemetonshiny:::ug_color(NA_character_, n + 1L)
  expect_equal(color, nemetonshiny:::UG_PALETTE[1])
})

test_that("has_ug_data works correctly", {
  # No UG data
  projet <- list(parcels = data.frame(id = "p1"))
  expect_false(nemetonshiny:::has_ug_data(projet))

  # With UG data
  p1 <- sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)))
  projet <- list(
    parcels = sf::st_sf(
      id = "p1", contenance = 10000,
      geometry = sf::st_sfc(p1, crs = 4326)
    )
  )
  projet <- nemetonshiny:::ug_init_default(projet)
  expect_true(nemetonshiny:::has_ug_data(projet))
})


# ==============================================================================
# Translation tests for UG keys
# ==============================================================================

test_that("UG translation keys exist in French", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  i18n <- nemetonshiny:::get_i18n("fr")

  ug_keys <- c(
    "tab_ug", "ug_title", "ug_merge", "ug_split", "ug_rename",
    "ug_group", "ug_composition", "ug_no_data",
    "ug_select_hint", "ug_select_one",
    "ug_map_tab", "ug_table_tab", "ug_map_click_hint",
    "ug_create_from_map", "ug_create_btn", "ug_clear_selection"
  )

  for (key in ug_keys) {
    translated <- i18n$t(key)
    expect_true(
      nchar(translated) > 0 && translated != key,
      info = paste("Missing FR translation for:", key)
    )
  }
})

test_that("UG translation keys exist in English", {
  withr::local_options(nemeton.app_options = list(language = "en"))
  i18n <- nemetonshiny:::get_i18n("en")

  ug_keys <- c(
    "tab_ug", "ug_title", "ug_merge", "ug_split", "ug_rename",
    "ug_group", "ug_composition", "ug_no_data"
  )

  for (key in ug_keys) {
    translated <- i18n$t(key)
    expect_true(
      nchar(translated) > 0 && translated != key,
      info = paste("Missing EN translation for:", key)
    )
  }
})

# ---- Parcellaire forestier ONF (spec 046) ----------------------------------

test_that("la barre d'actions carte porte l'action ONF, et une seule", {
  skip_if_not_installed("bslib")
  h <- with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    as.character(nemetonshiny:::mod_ug_map_actions_bar("ug"))
  )
  expect_true(grepl("ug-btn_onf_croise", h, fixed = TRUE))
  # v0.130.0.9001 — un « Importer le parcellaire ONF » a existé, qui REMPLAÇAIT
  # les parcelles du projet. Retiré : même emprise, mêmes UGF, mais la
  # composition cadastrale était perdue — un cas dégradé du croisement, et
  # destructif. Le test le verrouille pour qu'il ne revienne pas par mégarde.
  expect_false(grepl("ug-btn_onf_import", h, fixed = TRUE))
  # La domanialité, la purge et son seuil ont rejoint « Paramètres › Sources &
  # paramètres » : ce sont des calibrages, réglés une fois par massif, alors que
  # le bouton ci-dessus est un geste qu'on répète. Ce qui reste ici est le
  # RAPPEL des valeurs en vigueur — une sidebar qui perd ses réglages sans dire
  # où ils sont partis oblige à les chercher.
  expect_false(grepl("ug-onf_domanialite\"", h))
  expect_true(grepl("ug-onf_params_rappel", h, fixed = TRUE))
  # v0.130.1.9001 — le calage sur les limites cadastrales est SYSTÉMATIQUE, la
  # coche est retirée. Il reste annoncé en clair : une UGF dont le bord suit le
  # cadastre plutôt que le tracé ONF serait incompréhensible sans cette phrase.
  expect_false(grepl("ug-onf_caler", h, fixed = TRUE))
  expect_true(grepl(i18n_note <- nemetonshiny:::get_i18n("fr")$t("onf_caler_note"),
                    h, fixed = TRUE))

  i18n <- nemetonshiny:::get_i18n("fr")
  # La note de grain est permanente, pas repliée : lire une UGF comme un
  # peuplement homogène est l'erreur que la spec veut éviter.
  expect_true(grepl(i18n$t("onf_grain_parcelle"), h, fixed = TRUE))
  # Producteur mentionné (ONF, diffusion publique).
  expect_true(grepl(i18n$t("onf_source_note"), h, fixed = TRUE))
})

test_that("le calage cadastral est systematique cote service", {
  # Le réglage n'est plus exposé à l'utilisateur : il doit donc être actif par
  # DÉFAUT dans la signature, sinon un appel sans argument produirait des UGF
  # aux bords ONF bruts, en contradiction avec ce que la note annonce à l'écran.
  expect_true(isTRUE(
    formals(nemetonshiny:::onf_projet_croise)$caler_sur_cadastre))
  # Le paramètre SUBSISTE : le comportement brut reste joignable et testable.
  expect_true("caler_sur_cadastre" %in%
                names(formals(nemetonshiny:::onf_projet_croise)))
})

test_that("les actions ONF refusent un projet sans donnees UGF", {
  skip_if_not_installed("sf")
  testthat::local_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    .package = "nemetonshiny")

  app_state <- shiny::reactiveValues(language = "fr", current_project = NULL)
  shiny::testServer(
    nemetonshiny:::mod_ug_server,
    args = list(app_state = app_state),
    {
      # Sans projet chargé, aucune des deux actions ne doit appeler le WFS :
      # le garde est en amont de l'appel réseau, pas après.
      appele <- FALSE
      testthat::with_mocked_bindings(
        onf_load_parcelles = function(...) { appele <<- TRUE; list(status = "ok") },
        .package = "nemetonshiny",
        session$setInputs(btn_onf_croise = 1L))
      expect_false(appele)
    })
})

test_that("la couche Parcellaire ONF a une case dans TOUS les controles", {
  skip_if_not_installed("bslib")
  # Régression v0.130.7.9001 : la surcouche était ajoutée par `addPolygons(group
  # = "Parcellaire ONF")` mais absente des `overlayGroups`. Sans case dans le
  # contrôle de couches, elle ne pouvait pas être décochée — elle restait
  # affichée quoi qu'on fasse, et donnait l'impression que la couche « Dessin »
  # peignait des parcelles.
  #
  # Il y a DEUX contrôles (rendu initial + re-création après `clearControls()`
  # à chaque redessin) : en oublier un suffit à faire réapparaître le bug.
  f <- chemin_source("R", "mod_ug.R"); skip_sans_sources(f)
  src <- readLines(f, warn = FALSE)
  ctrl <- grep("overlayGroups = ", src, value = TRUE)
  expect_length(ctrl, 2L)
  expect_true(all(grepl("Parcellaire ONF", ctrl, fixed = TRUE)))
})

test_that("la previsualisation ONF est effacee apres le croisement", {
  # Régression v0.130.7.9001 : `rv$onf_preview` était posé avant le calcul et
  # jamais remis à NULL. La surcouche restait superposée au résultat — d'autant
  # plus trompeuse après une purge, puisqu'elle continuait d'afficher un
  # parcellaire que le projet ne contenait plus.
  f <- chemin_source("R", "mod_ug.R"); skip_sans_sources(f)
  src <- readLines(f, warn = FALSE)
  pose <- grep("rv\\$onf_preview <- res\\$parcelles", src)
  efface <- grep("rv\\$onf_preview <- NULL", src)
  expect_length(pose, 1L)
  expect_length(efface, 1L)
  # L'effacement vient APRÈS la pose, et après le commit du projet.
  expect_gt(efface, pose)
  # Depuis que l'import CSV purge lui aussi (brief du 2026-08-25), il y a DEUX
  # appels a `.onf_commit(projet_final, ...)`. Ce test ne parle que du chemin du
  # BOUTON : la previsualisation orange n'existe que la. On compare donc
  # l'effacement au commit qui le precede, pas a un appel suppose unique.
  commit <- grep("\\.onf_commit\\(projet_final", src)
  expect_gte(length(commit), 1L)
  avant <- commit[commit < efface]
  expect_gte(length(avant), 1L)
  expect_gt(efface, max(avant))
})


# ==============================================================================
# PERF : la carte UGF ne se dessine pas onglet ferme
# ==============================================================================
# Le rendu des tenements passe par `leafletProxy()` dans un `observe()`, donc
# rien ne le suspend quand l'onglet est cache - et leaflet jette silencieusement
# les polygones envoyes a une carte absente du DOM. Ce travail etait paye sur le
# thread unique, dans le flush qui doit afficher les parcelles cadastrales
# (2 x 370 ms au chargement d'un projet recent). Le module redessine deja a
# l'ouverture de l'onglet via `rv$redraw_counter` : ces tests verrouillent les
# deux moities de la garde - muet quand cache, dessine quand visible.

# Session racine simulee : `mod_ug` lit `input$main_nav` et
# `input[["home-main_tabs"]]` sur `session$userData$root_session`.
.fausse_session_racine <- function(main_nav, sous_onglet) {
  list(input = list(main_nav = main_nav, `home-main_tabs` = sous_onglet))
}

# Projet minimal PORTANT DEJA ses UGF : `has_ug_data()` doit etre vrai, sinon
# l'observer d'init tenterait une migration disque et le test ne testerait plus
# la garde de visibilite.
.projet_ugf_minimal <- function() {
  carre <- function(x) sf::st_polygon(list(matrix(c(
    x, 0, x + 1, 0, x + 1, 1, x, 1, x, 0), ncol = 2, byrow = TRUE)))
  parcels <- sf::st_sf(
    id = c("p1", "p2"),
    geo_parcelle = c("REF001", "REF002"),
    section = c("A", "A"), numero = c("1", "2"),
    contenance = c(10000, 10000),
    geometry = sf::st_sfc(carre(0), carre(1), crs = 4326))
  projet <- nemetonshiny:::ug_init_default(
    list(parcels = parcels, metadata = list(id = "test_ug_visibilite")))
  projet$metadata <- list(id = "test_ug_visibilite", groupes_profile = "onf")
  projet
}

.compte_dessins_carte_ugf <- function(main_nav, sous_onglet) {
  projet <- .projet_ugf_minimal()
  app_state <- shiny::reactiveValues(language = "fr", current_project = projet)

  dessins <- 0L
  shiny::testServer(
    nemetonshiny:::mod_ug_server,
    args = list(app_state = app_state),
    {
      session$userData$root_session <-
        .fausse_session_racine(main_nav, sous_onglet)
      # On compte le SEUL marqueur propre au dessin des tenements - le groupe
      # "Tenements" - et non les appels a `leafletProxy()` : d'autres
      # observers du module en emettent, un compteur global ne distinguerait
      # pas le dessin de la selection ou du zoom.
      testthat::with_mocked_bindings(
        leafletProxy = function(...) structure(list(), class = c("leaflet_proxy", "leaflet")),
        clearGroup = function(map, ...) map,
        addPolygons = function(map, ...) {
          if (identical(list(...)$group, "Tenements")) dessins <<- dessins + 1L
          map
        },
        addLegend = function(map, ...) map,
        clearControls = function(map, ...) map,
        addLayersControl = function(map, ...) map,
        .package = "leaflet",
        {
          session$flushReact()
          rv$redraw_counter <- rv$redraw_counter + 1L
          session$flushReact()
        }
      )
    })
  dessins
}

test_that("la carte UGF ne se dessine pas quand son sous-onglet est cache", {
  skip_if_not_installed("bslib")
  expect_equal(.compte_dessins_carte_ugf("synthesis", "tenements"), 0L)
  expect_equal(.compte_dessins_carte_ugf("selection", "cadastre"), 0L)
})

test_that("la carte UGF se dessine quand son sous-onglet est visible", {
  skip_if_not_installed("bslib")
  expect_gt(.compte_dessins_carte_ugf("selection", "tenements"), 0L)
})

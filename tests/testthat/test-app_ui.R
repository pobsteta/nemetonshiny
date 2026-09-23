# Tests for nemetonApp UI
# R/app_ui.R - Main UI function and external resource loading

# ==============================================================================
# Tests for app_add_external_resources()
# ==============================================================================

test_that("app_add_external_resources returns a valid Shiny tagList", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemetonshiny:::app_add_external_resources()

      # Should return a tagList (which is a list of shiny tags)
      expect_true(inherits(result, "shiny.tag.list") || is.list(result))
    }
  )
})

test_that("app_add_external_resources contains CSS link tag", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemetonshiny:::app_add_external_resources()

      # Use renderTags to get full HTML including head content
      rendered <- htmltools::renderTags(result)
      full_html <- paste(rendered$head, rendered$html)

      # Le lien doit viser le fichier SOURCE. On servait une copie « min » que
      # rien ne régénérait, et qui a fini par avaler deux règles ; il n'y a plus
      # qu'un fichier, c'est lui qu'on sert.
      expect_true(
        grepl("www/css/custom\\.css", full_html),
        info = "Expected CSS link tag for custom.css"
      )
      expect_false(
        grepl("custom\\.min\\.css", full_html),
        info = "custom.min.css a été supprimé : ne pas le servir"
      )
    }
  )
})

test_that("app_add_external_resources contains JS script tag", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemetonshiny:::app_add_external_resources()

      # Use renderTags to get full HTML including head content
      rendered <- htmltools::renderTags(result)
      full_html <- paste(rendered$head, rendered$html)

      # Le fichier SOURCE, comme pour le CSS : la copie « min » n'était pas
      # minifiée et rien ne la régénérait.
      expect_true(
        grepl("www/js/custom\\.js", full_html),
        info = "Expected JS script tag for custom.js"
      )
      expect_false(
        grepl("custom\\.min\\.js", full_html),
        info = "custom.min.js a été supprimé : ne pas le servir"
      )
    }
  )
})

test_that("app_add_external_resources contains viewport meta tag", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemetonshiny:::app_add_external_resources()

      rendered <- htmltools::renderTags(result)
      full_html <- paste(rendered$head, rendered$html)

      # Should contain viewport meta tag
      expect_true(
        grepl("viewport", full_html),
        info = "Expected viewport meta tag"
      )
    }
  )
})

test_that("app_add_external_resources contains favicon link", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemetonshiny:::app_add_external_resources()

      rendered <- htmltools::renderTags(result)
      full_html <- paste(rendered$head, rendered$html)

      # Should contain favicon link
      expect_true(
        grepl("logo\\.svg", full_html),
        info = "Expected favicon link tag"
      )
    }
  )
})

test_that("app_add_external_resources contains theme-color meta tag", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemetonshiny:::app_add_external_resources()

      rendered <- htmltools::renderTags(result)
      full_html <- paste(rendered$head, rendered$html)

      # Should contain theme-color meta tag
      expect_true(
        grepl("theme-color", full_html),
        info = "Expected theme-color meta tag"
      )
    }
  )
})

test_that("app_add_external_resources contains inline critical CSS", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemetonshiny:::app_add_external_resources()

      rendered <- htmltools::renderTags(result)
      full_html <- paste(rendered$head, rendered$html)

      # Should contain inline style tag with background-color
      expect_true(
        grepl("background-color", full_html),
        info = "Expected inline critical CSS with background-color"
      )
    }
  )
})

test_that("app_add_external_resources head content includes link and meta tags", {
  with_mocked_bindings(
    get_app_options = function() list(language = "en"),
    {
      result <- nemetonshiny:::app_add_external_resources()

      rendered <- htmltools::renderTags(result)
      head_html <- rendered$head

      # Head section should contain link and meta tags
      expect_true(
        grepl("<link", head_html) && grepl("<meta", head_html),
        info = "Expected head content with link and meta tags"
      )
    }
  )
})

# ==============================================================================
# Menu « Familles d'indicateurs » — le libellé porte le code de la famille
# ==============================================================================

test_that("chaque famille du menu affiche son code entre parentheses", {
  # Le code (B, C, W, …) est la clé qui circule partout ailleurs : radar,
  # exports, profils experts, indicateurs B1/C2/… Le menu était le seul endroit
  # qui ne le donnait pas.
  attendus <- c(famille_carbone = "C", famille_biodiversite = "B",
                famille_eau = "W", famille_air = "A", famille_sol = "F",
                famille_paysage = "L", famille_temporel = "T",
                famille_risque = "R", famille_social = "S",
                famille_production = "P", famille_energie = "E",
                famille_naturalite = "N")

  for (lang in c("fr", "en")) {
    html <- with_mocked_bindings(
      get_app_options = function() list(language = lang),
      as.character(htmltools::renderTags(nemetonshiny:::app_ui(NULL))$html)
    )
    i18n <- nemetonshiny:::get_i18n(lang)

    for (key in names(attendus)) {
      # `renderTags` échappe `&` en `&amp;` — on compare sur du texte échappé.
      libelle <- htmltools::htmlEscape(
        sprintf("%s (%s)", i18n$t(key), attendus[[key]]))
      expect_true(grepl(libelle, html, fixed = TRUE),
                  info = sprintf("[%s] libellé attendu absent : %s", lang, libelle))
    }

    # Les valeurs d'onglet ne doivent PAS bouger : `input$main_nav` est lu
    # ailleurs (mod_ug, tours guidés, navigation programmatique).
    for (key in names(attendus)) {
      expect_true(grepl(sprintf("data-value=\"%s\"", key), html, fixed = TRUE),
                  info = sprintf("[%s] valeur d'onglet perdue : %s", lang, key))
    }
  }
})


# ---- Pas de definition en double (2026-09-14) ------------------------

test_that("mod_home_ui n'est defini qu'une fois", {
  # `app_ui.R` portait une copie « Placeholder » de `mod_home_ui`, masquee
  # par celle de `mod_home.R` (collation alphabetique : le dernier gagne).
  # Elle n'atteignait donc jamais l'ecran, tout en restant lisible comme si
  # elle etait la vraie - avec ses propres boutons de fond de carte, oublies
  # lors du passage au LayersControl.
  #
  # Le piege inverse existe dans le MEME fichier : `mod_synthesis_ui` et
  # `mod_family_ui` portent le meme titre « Placeholder » mais sont, elles,
  # les implementations VIVANTES. Le titre ne dit rien de l'etat reel :
  # seule la collation le dit.
  racine <- testthat::test_path("..", "..", "R")
  fichiers <- list.files(racine, pattern = "\\.R$", full.names = TRUE)
  # Sous covr, `../../R` existe mais c'est celui du paquet installe (.rdb) :
  # tester le dossier ne suffit pas, il faut des sources.
  testthat::skip_if(length(fichiers) == 0L, "sources R absentes (package installe)")

  compte <- function(nom) {
    sum(vapply(fichiers, function(f) {
      any(grepl(paste0("^", nom, " <- function"), readLines(f, warn = FALSE)))
    }, logical(1)))
  }

  expect_equal(compte("mod_home_ui"), 1L)
  expect_equal(compte("mod_synthesis_ui"), 1L)
  expect_equal(compte("mod_family_ui"), 1L)
})


test_that("les trois UI de modules se construisent", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  testthat::with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      expect_gt(nchar(as.character(nemetonshiny:::mod_home_ui("t"))), 1000L)
      expect_gt(nchar(as.character(nemetonshiny:::mod_synthesis_ui("t"))), 1000L)
      expect_gt(nchar(as.character(nemetonshiny:::mod_family_ui("t", "B"))), 1000L)
    }
  )
})

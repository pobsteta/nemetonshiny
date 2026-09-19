# Coherence tests for the guided tour (service_tour.R). The cicerone
# framing itself needs a browser (not unit-testable), but we guard the
# two things that silently break the tour : (1) i18n keys that don't
# resolve, (2) anchor ids that no longer exist in the app UI.

`%||%` <- function(a, b) if (is.null(a)) b else a

.valid_main_nav_tabs <- c(
  "selection", "synthesis", "action_plan", "terrain", "monitoring",
  "famille_carbone", "famille_biodiversite", "famille_eau", "famille_air",
  "famille_sol", "famille_paysage", "famille_temporel", "famille_risque",
  "famille_social", "famille_production", "famille_energie", "famille_naturalite"
)

test_that("build_tour_steps yields a well-formed, i18n-resolved spec", {
  # Catalogue COMPLET : `project_status = "completed"` est le seul etat ou
  # toutes les etapes sont proposees (cf. onglets restreints plus bas).
  steps <- nemetonshiny:::build_tour_steps(
    nemetonshiny:::get_i18n("fr"), 30L, project_status = "completed")
  expect_gte(length(steps), 11L)
  for (s in steps) {
    expect_true(all(c("el", "title", "description", "tab") %in% names(s)))
    # el / tab non-empty strings
    expect_true(is.character(s$el) && nzchar(s$el))
    expect_true(s$tab %in% .valid_main_nav_tabs)
    # i18n actually resolved (not the raw key, not a "not found" marker)
    for (txt in c(s$title, s$description)) {
      expect_true(is.character(txt) && nzchar(txt))
      expect_false(grepl("not found", txt, ignore.case = TRUE))
      expect_false(grepl("^tour_[a-z_]+$", txt))  # would be an unresolved key
    }
  }
  # Every main top-level tab is covered at least once.
  covered <- unique(vapply(steps, function(s) s$tab, character(1)))
  for (tab in c("selection", "synthesis", "action_plan", "terrain",
                "monitoring", "famille_carbone")) {
    expect_true(tab %in% covered, info = paste("tab not covered:", tab))
  }
})

test_that("build_tour_steps resolves identically in EN", {
  steps_en <- nemetonshiny:::build_tour_steps(
    nemetonshiny:::get_i18n("en"), 30L, project_status = "completed")
  for (s in steps_en) {
    expect_false(grepl("not found", s$title, ignore.case = TRUE))
    expect_false(grepl("^tour_[a-z_]+$", s$title))
  }
})

test_that("every tour anchor id still exists in the app UI", {
  skip_if_not_installed("bslib")
  ui_html <- as.character(nemetonshiny:::app_ui(list()))
  steps <- nemetonshiny:::build_tour_steps(
    nemetonshiny:::get_i18n("fr"), 30L, project_status = "completed")

  # `home-project-create_project` is rendered server-side (mod_project's
  # action_button renderUI, create mode) — its static proxy is the
  # uiOutput container. Everything else is present in the static UI.
  server_rendered <- "home-project-create_project"
  for (s in steps) {
    if (identical(s$el, server_rendered)) next
    expect_true(
      grepl(paste0('id="', s$el, '"'), ui_html, fixed = TRUE),
      info = paste("tour anchor missing from app UI:", s$el)
    )
  }
  # Proxy container for the server-rendered create button.
  expect_true(grepl('id="home-project-action_button"', ui_html, fixed = TRUE))
})


test_that("les ancres du tour sont STATIQUES, pas des sorties serveur", {
  skip_if_not_installed("bslib")
  # Mesure du 2026-09-19 : un `uiOutput` porte par un onglet masque est
  # SUSPENDU. Il ne rend rien avant un aller-retour serveur, donc il mesure
  # 0 de haut a l'instant ou driver.js cadre l'etape (`synthesis-project_summary`
  # = 447x0, `famille_carbone-maps_row` = 1408x0) : `canHighlight()` est faux
  # et l'etape est sautee EN SILENCE. Une ancre doit donc etre un conteneur
  # statique. Seule exception : `home-project-create_project`, rendu cote
  # serveur mais sur l'onglet DEJA visible (output non suspendu).
  ui_html <- as.character(nemetonshiny:::app_ui(list()))
  steps <- nemetonshiny:::build_tour_steps(
    nemetonshiny:::get_i18n("fr"), 30L, project_status = "completed")
  for (s in steps) {
    if (identical(s$el, "home-project-create_project")) next
    i <- regexpr(paste0('id="', s$el, '"'), ui_html, fixed = TRUE)
    expect_gt(i, 0)
    # la balise porteuse de l'id : de son '<' jusqu'au '>' suivant
    debut <- max(gregexpr("<", substr(ui_html, 1, i), fixed = TRUE)[[1]])
    balise <- substr(ui_html, debut, debut + regexpr(">", substr(ui_html, debut, nchar(ui_html)), fixed = TRUE) - 1L)
    expect_false(
      grepl("shiny-html-output", balise, fixed = TRUE),
      info = paste("ancre de tour rendue cote serveur (mesure 0 a froid):", s$el)
    )
  }
})


# --- Onglets restreints : le tour ne doit pas y aller sans projet ----------

test_that(".tab_requires_completed_project reconnait Synthese et les familles", {
  f <- nemetonshiny:::.tab_requires_completed_project
  expect_true(f("synthesis"))
  expect_true(f("famille_carbone"))
  expect_true(f("famille_naturalite"))
  expect_false(f("selection"))
  expect_false(f("action_plan"))
  expect_false(f("terrain"))
  expect_false(f("monitoring"))
  expect_false(f("regeneration"))
  # entrees degenerees : ne jamais restreindre par accident
  expect_false(f(NULL))
  expect_false(f(character(0)))
  expect_false(f(NA_character_))
})

test_that("les etapes d'onglets restreints sautent tant que le projet n'est pas termine", {
  i18n <- nemetonshiny:::get_i18n("fr")
  onglets <- function(statut) {
    vapply(nemetonshiny:::build_tour_steps(i18n, 30L, project_status = statut),
           function(s) s$tab, character(1))
  }
  # Sans projet (cas de l'auto-demarrage) : ni Synthese ni famille. Sinon
  # l'app renvoie sur l'Accueil (app_server) pendant que driver.js cadre.
  for (statut in list(NULL, "none", "draft", "computing")) {
    tabs <- onglets(statut)
    expect_false("synthesis" %in% tabs, info = paste("statut:", statut %||% "NULL"))
    expect_false(any(grepl("^famille_", tabs)), info = paste("statut:", statut %||% "NULL"))
    # les onglets libres, eux, restent couverts
    for (t in c("selection", "action_plan", "terrain", "monitoring")) {
      expect_true(t %in% tabs)
    }
  }
  # Projet termine : les deux etapes reviennent.
  tabs <- onglets("completed")
  expect_true("synthesis" %in% tabs)
  expect_true("famille_carbone" %in% tabs)
})

test_that("app_server partage le predicat du filtre (pas de liste dupliquee)", {
  # Les deux listes DOIVENT coincider : c'est leur divergence qui envoyait le
  # tour sur un onglet d'ou l'app le renvoyait aussitot.
  src <- readLines(testthat::test_path("..", "..", "R", "app_server.R"),
                   warn = FALSE, encoding = "UTF-8")
  expect_true(any(grepl(".tab_requires_completed_project", src, fixed = TRUE)))
  # et plus aucune liste en dur cote app_server
  expect_false(any(grepl('grep("^famille_"', src, fixed = TRUE)))
})

test_that("build_tour_guide returns a cicerone guide when available", {
  skip_if_not_installed("cicerone")
  g <- nemetonshiny:::build_tour_guide(nemetonshiny:::get_i18n("fr"), 30L)
  expect_s3_class(g, "Cicerone")
})

test_that(".tour_switch_tab_js is a JS function expression (cicerone new Function)", {
  # Regression : cicerone 1.0.4 injecte on_highlight_started brut dans
  # `new Function("return " + js)()`. Le JS DOIT donc etre une expression de
  # fonction (`function(){...}`) ; une chaine commencant par `var` produit
  # `return var ...` -> SyntaxError, cassant les steps du tour (« no steps to
  # iterate ») et la bascule d'onglet.
  js <- nemetonshiny:::.tour_switch_tab_js("synthesis")
  expect_match(js, "^function\\(\\)\\{", perl = TRUE)
  expect_false(grepl("^\\s*var", js))
  # cible le bon onglet
  expect_match(js, 'data-value=\"synthesis\"', fixed = TRUE)
  # la chaine, prefixee de "return " (ce que fait cicerone.js), doit parser
  # comme du JS valide -> on verifie au moins qu'elle est bien equilibree.
  expect_equal(lengths(regmatches(js, gregexpr("{", js, fixed = TRUE))),
               lengths(regmatches(js, gregexpr("}", js, fixed = TRUE))))
})

# --- Auto-demarrage du tour (run_app(tour =) / NEMETON_TOUR) ----------------

test_that(".tour_autostart_enabled defaults to TRUE", {
  withr::with_envvar(c(NEMETON_TOUR = ""), {
    withr::with_options(list(nemeton.app_options = list(language = "fr")), {
      expect_true(nemetonshiny:::.tour_autostart_enabled())
    })
  })
})

test_that(".tour_autostart_enabled honours the run_app(tour =) option", {
  withr::with_envvar(c(NEMETON_TOUR = ""), {
    withr::with_options(list(nemeton.app_options = list(tour = FALSE)), {
      expect_false(nemetonshiny:::.tour_autostart_enabled())
    })
    withr::with_options(list(nemeton.app_options = list(tour = TRUE)), {
      expect_true(nemetonshiny:::.tour_autostart_enabled())
    })
  })
})

test_that("NEMETON_TOUR overrides the option, in both directions", {
  withr::with_options(list(nemeton.app_options = list(tour = TRUE)), {
    for (v in c("0", "false", "no", "non", "off", "FALSE")) {
      withr::with_envvar(c(NEMETON_TOUR = v),
                         expect_false(nemetonshiny:::.tour_autostart_enabled()))
    }
  })
  withr::with_options(list(nemeton.app_options = list(tour = FALSE)), {
    for (v in c("1", "true", "yes", "oui", "on")) {
      withr::with_envvar(c(NEMETON_TOUR = v),
                         expect_true(nemetonshiny:::.tour_autostart_enabled()))
    }
  })
})

test_that(".tour_autostart_enabled falls back to TRUE on junk input", {
  withr::with_options(list(nemeton.app_options = list(tour = "peut-etre")), {
    withr::with_envvar(c(NEMETON_TOUR = "bruit"), {
      expect_true(nemetonshiny:::.tour_autostart_enabled())
    })
  })
})

test_that("run_app rejects a non-logical tour argument", {
  expect_error(run_app(tour = "oui"), "tour")
  expect_error(run_app(tour = NA), "tour")
  expect_error(run_app(tour = c(TRUE, FALSE)), "tour")
})

# --- Régression : app_ui robuste à une option app partielle ------------------
# Un test qui laisse fuir options(nemeton.app_options=...) SANS `language`
# faisait planter app_ui() via get_expert_choices (p$label[[NULL]]) en contexte
# complet (R CMD check), erreur « attempt to select less than one element ».

test_that("get_app_options merges defaults over a partial option", {
  withr::local_options(nemeton.app_options = list(project_dir = tempdir()))
  opts <- nemetonshiny:::get_app_options()
  expect_identical(opts$language, "fr")          # champ absent -> défaut
  expect_identical(opts$project_dir, tempdir())   # champ fourni -> conservé
  expect_true(!is.null(opts$max_parcels))
  # language explicitement NULL/vide -> retombe sur "fr"
  withr::local_options(nemeton.app_options = list(language = NULL))
  expect_identical(nemetonshiny:::get_app_options()$language, "fr")
  withr::local_options(nemeton.app_options = list(language = ""))
  expect_identical(nemetonshiny:::get_app_options()$language, "fr")
})

test_that("get_expert_choices tolerates a NULL/empty lang", {
  expect_gt(length(nemetonshiny:::get_expert_choices(NULL)), 0L)
  expect_gt(length(nemetonshiny:::get_expert_choices(character(0))), 0L)
  expect_gt(length(nemetonshiny:::get_expert_choices("")), 0L)
})

test_that("app_ui does not error when the app option lacks a language", {
  skip_if_not_installed("bslib")
  withr::local_options(nemeton.app_options = list(project_dir = tempdir()))
  expect_no_error(as.character(nemetonshiny:::app_ui(list())))
})


# --- Cadrage : re-mesure apres la transition d'onglet (2026-09-18) ---------
#
# Cliquer ACTIVE l'onglet, ne le MESURE pas. Un `.tab-pane` masque est en
# `display: none` : ses elements ont des dimensions nulles jusqu'a ce que le
# navigateur l'ait pose, et Bootstrap 5 ajoute une transition `.fade`.
# driver.js cadrait donc une geometrie qui n'existait pas encore.

test_that(".tour_switch_tab_js attend shown.bs.tab avant de re-mesurer", {
  js <- nemetonshiny:::.tour_switch_tab_js("synthesis")

  # L'attente porte sur l'evenement Bootstrap, pas sur un delai suppose.
  # Viser `addEventListener` et non la simple chaine « shown.bs.tab » :
  # celle-ci apparait aussi dans le `removeEventListener`, et l'assertion
  # passait donc meme en retirant l'ecoute (constate par mutation).
  expect_match(js, "addEventListener('shown.bs.tab'", fixed = TRUE)
  # La re-mesure passe par `resize`, que driver.js ecoute deja
  # (bind -> onResize -> refresh) : aucun interne de cicerone n'est touche.
  expect_match(js, "dispatchEvent", fixed = TRUE)
  expect_match(js, "resize", fixed = TRUE)
  # Le clic reste la, evidemment.
  expect_match(js, ".click()", fixed = TRUE)
})


test_that(".tour_switch_tab_js garde un repli si l'onglet est deja actif", {
  # Piege : sur un onglet DEJA actif, le clic n'emet aucun `shown.bs.tab`.
  # Sans repli, l'attente ne se resoudrait jamais et l'etape resterait mal
  # cadree - pire qu'avant le correctif.
  js <- nemetonshiny:::.tour_switch_tab_js("selection")
  expect_match(js, "setTimeout", fixed = TRUE)
  # et une garde d'idempotence, pour que le repli et l'evenement ne
  # declenchent pas la re-mesure deux fois.
  expect_match(js, "__done", fixed = TRUE)
})


test_that(".tour_switch_tab_js reste une expression de fonction equilibree", {
  # Le contrat de cicerone n'a pas change : `new Function("return " + js)()`.
  for (tab in c("selection", "synthesis", "monitoring")) {
    js <- nemetonshiny:::.tour_switch_tab_js(tab)
    expect_match(js, "^function\\(\\)\\{", perl = TRUE)
    expect_equal(lengths(regmatches(js, gregexpr("{", js, fixed = TRUE))),
                 lengths(regmatches(js, gregexpr("}", js, fixed = TRUE))))
    expect_equal(lengths(regmatches(js, gregexpr("(", js, fixed = TRUE))),
                 lengths(regmatches(js, gregexpr(")", js, fixed = TRUE))))
    expect_match(js, sprintf('data-value=\"%s\"', tab), fixed = TRUE)
  }
})


# --- Clic de bascule : ne doit pas fermer le tour (2026-09-19) -------------

test_that(".tour_switch_tab_js etouffe le clic avant window", {
  js <- nemetonshiny:::.tour_switch_tab_js("synthesis")
  # Regression : driver.js ecoute les clics sur `window` et appelle reset()
  # pour tout clic hors popover / hors element mis en avant (allowClose vaut
  # TRUE). Notre clic de bascule partant PENDANT on_highlight_started - alors
  # que le step precedent est encore l'element courant - il fermait le tour :
  # l'etape suivante s'affichait mais plus rien n'y etait cliquable.
  expect_match(js, "document.addEventListener('click'", fixed = TRUE)
  expect_match(js, "stopPropagation", fixed = TRUE)
  # L'ecoute est retiree dans la foulee, sinon elle etoufferait aussi les
  # clics suivants de l'utilisateur.
  expect_match(js, "finally{document.removeEventListener('click'", fixed = TRUE)
})


test_that("l'etape Recherche cadre la carte entiere, en-tete compris", {
  skip_if_not_installed("bslib")
  steps <- nemetonshiny:::build_tour_steps(nemetonshiny:::get_i18n("fr"), 30L)
  # Ancre sur `home-search_collapse` (le corps repliable seul), le cadre
  # laissait le titre « Rechercher une commune... » sous le voile sombre.
  expect_identical(steps[[1]]$el, "home-search_card")

  ui_html <- as.character(nemetonshiny:::app_ui(list()))
  # Le titre doit se trouver ENTRE l'ouverture de la carte et l'ouverture du
  # corps repliable : c'est ce qui prouve qu'il est bien dans l'element ancre.
  i_card <- regexpr('id="home-search_card"', ui_html, fixed = TRUE)
  i_body <- regexpr('id="home-search_collapse"', ui_html, fixed = TRUE)
  expect_gt(i_card, 0)
  expect_gt(i_body, i_card)
  entete <- substr(ui_html, i_card, i_body)
  expect_true(grepl("card-header", entete, fixed = TRUE))
  expect_true(grepl(
    nemetonshiny:::get_i18n("fr")$t("search_commune"), entete, fixed = TRUE))
})

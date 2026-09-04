# Tests for mod_pipeline.R
# UI du lancement enchaine (« Tout calculer »).

test_that("mod_pipeline_ui rend une UI Shiny valide", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  ui <- nemetonshiny:::mod_pipeline_ui("pipeline")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})

test_that("la section « Tout calculer » est retractable comme les autres", {
  # Le panneau de progression liste dix-sept etapes : sans repli, il pousse le
  # reste de la sidebar hors de l'ecran des que la chaine est lancee. On
  # verifie le meme mecanisme Bootstrap que les autres blocs de cette sidebar
  # (projets recents, recherche), et non un simple `div` : c'est l'attribut
  # `data-bs-toggle` qui rend l'entete cliquable.
  withr::local_options(nemeton.app_options = list(language = "fr"))
  html <- as.character(nemetonshiny:::mod_pipeline_ui("home-pipeline"))

  expect_true(grepl('data-bs-toggle="collapse"', html, fixed = TRUE))
  expect_true(grepl('data-bs-target="#home-pipeline-pipeline_collapse"',
                    html, fixed = TRUE))
  expect_true(grepl('id="home-pipeline-pipeline_collapse"', html, fixed = TRUE))
  # Deplie par defaut : le bouton doit rester visible sans clic prealable.
  expect_true(grepl('class="collapse show"', html, fixed = TRUE))
  # Le chevron, marqueur visuel commun aux sections repliables de la sidebar.
  expect_true(grepl("collapse-icon", html, fixed = TRUE))
})

test_that("le bouton de lancement reste dans la section repliable", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  html <- as.character(nemetonshiny:::mod_pipeline_ui("home-pipeline"))
  # Le bouton et le panneau doivent etre APRES l'ouverture du bloc collapse :
  # les laisser en dehors les rendrait insensibles au repli.
  pos_collapse <- regexpr('id="home-pipeline-pipeline_collapse"', html, fixed = TRUE)
  pos_bouton   <- regexpr('id="home-pipeline-open"', html, fixed = TRUE)
  pos_panneau  <- regexpr('id="home-pipeline-panel"', html, fixed = TRUE)
  expect_gt(pos_bouton, pos_collapse)
  expect_gt(pos_panneau, pos_collapse)
})

test_that("le bouton de lancement cede l'emphase pleine, sans devenir ambre", {
  # v0.143.17 : depuis que les actions de projet ont rejoint ce bloc, deux
  # boutons verts s'y touchaient. La regle normative dit une seule action
  # principale par vue, et c'est celui-ci qui cede - a l'etat `completed`,
  # consulter les resultats est le geste attendu.
  withr::local_options(nemeton.app_options = list(language = "fr"))
  html <- as.character(nemetonshiny:::mod_pipeline_ui("pipeline"))

  expect_true(grepl("btn-outline-primary", html, fixed = TRUE))
  # Le vert PLEIN a bien disparu du bouton (la sous-chaine "btn-primary" n'est
  # pas contenue dans "btn-outline-primary" : le test n'est pas vacant).
  expect_false(grepl('class="btn-primary', html, fixed = TRUE))
  # L'intention reste POSITIVE : la bordure porte le sens, ce n'est ni du
  # neutre (`outline-secondary`) ni de la prudence (`outline-danger`).
  expect_false(grepl("btn-outline-secondary", html, fixed = TRUE))
  # Toujours pas d'ambre : elle signale une provenance, pas un niveau d'action.
  expect_false(grepl("btn-ia", html, fixed = TRUE))
})

test_that("l'entete du bloc et le libelle du bouton sont deux textes distincts", {
  # L'entete repetait le libelle du bouton mot pour mot. Le bloc nomme ce
  # qu'il REGROUPE, le bouton nomme l'ACTION - c'est ce decouplage que ce
  # test verrouille, pas la valeur exacte des deux chaines.
  withr::local_options(nemeton.app_options = list(language = "fr"))
  html <- as.character(nemetonshiny:::mod_pipeline_ui("home-pipeline"))
  i18n <- nemetonshiny:::get_i18n("fr")

  expect_true(grepl(i18n$t("pipeline_section_title"), html, fixed = TRUE))
  expect_true(grepl(i18n$t("pipeline_button"), html, fixed = TRUE))
  expect_false(identical(i18n$t("pipeline_section_title"),
                         i18n$t("pipeline_button")))

  # L'entete porte le titre du bloc, pas celui du bouton : on decoupe avant
  # l'ouverture du corps repliable pour ne regarder QUE la barre de titre.
  entete <- substr(html, 1L,
                   regexpr('id="home-pipeline-pipeline_collapse"', html,
                           fixed = TRUE))
  expect_true(grepl(i18n$t("pipeline_section_title"), entete, fixed = TRUE))
  expect_false(grepl(i18n$t("pipeline_button"), entete, fixed = TRUE))
})

test_that("les deux nouvelles cles i18n existent en FR et EN", {
  for (lang in c("fr", "en")) {
    i18n <- nemetonshiny:::get_i18n(lang)
    for (cle in c("pipeline_section_title", "pipeline_running_toast")) {
      valeur <- i18n$t(cle)
      # `t()` renvoie la cle elle-meme quand la traduction manque : comparer a
      # la cle est ce qui distingue « traduit » de « absent ».
      expect_false(identical(valeur, cle),
                   info = paste(lang, cle))
      expect_true(nzchar(valeur), info = paste(lang, cle))
    }
  }
})

test_that("le lancement grise le bouton et pose le toast, la cloture les leve", {
  # Regle stricte #9 : retour immediat + bouton indisponible le temps de
  # l'operation. `MockShinySession` n'expose NI `sendInputMessage` NI
  # `lastInputMessage` (verifie sur shiny 1.14) : passer par la session ne
  # testerait rien. On intercepte donc les deux appels a la source.
  appels <- list()
  testthat::local_mocked_bindings(
    updateActionButton = function(session, inputId, ..., disabled = NULL) {
      appels[[length(appels) + 1L]] <<-
        list(quoi = "bouton", id = inputId, disabled = disabled)
      invisible(NULL)
    },
    showNotification = function(ui, ..., id = NULL) {
      appels[[length(appels) + 1L]] <<-
        list(quoi = "toast", id = id, texte = paste(format(ui), collapse = " "))
      id %||% ""
    },
    removeNotification = function(id, ...) {
      appels[[length(appels) + 1L]] <<- list(quoi = "retrait", id = id)
      invisible(NULL)
    },
    .package = "shiny"
  )

  app_state <- shiny::reactiveValues(
    language = "fr", current_project = list(id = "p1"),
    pipeline_request = NULL, pipeline_answer = NULL
  )
  i18n <- nemetonshiny:::get_i18n("fr")

  shiny::testServer(
    nemetonshiny:::mod_pipeline_server,
    args = list(app_state = app_state),
    {
      session$setInputs(scope = c("indicateurs"), profil = "generalist")
      session$setInputs(start = 1)

      boutons <- Filter(function(a) identical(a$quoi, "bouton"), appels)
      expect_length(boutons, 1L)
      expect_identical(boutons[[1]]$id, "open")
      expect_true(isTRUE(boutons[[1]]$disabled))

      toasts <- Filter(function(a) identical(a$quoi, "toast"), appels)
      expect_length(toasts, 1L)
      expect_true(grepl(i18n$t("pipeline_running_toast"),
                        toasts[[1]]$texte, fixed = TRUE))

      run_id <- rv$state$run_id
      expect_false(is.null(run_id))

      # Un clic reste en vol pendant la chaine : la garde serveur double le
      # grisage de l'UI et ne doit pas ouvrir un second run.
      session$setInputs(open = 1)
      expect_identical(rv$state$run_id, run_id)

      # Arret manuel : c'est l'une des DEUX sorties. Rendre le bouton
      # seulement en fin naturelle laisserait ce chemin avec un bouton mort.
      session$setInputs(cancel = 1)

      boutons <- Filter(function(a) identical(a$quoi, "bouton"), appels)
      expect_length(boutons, 2L)
      expect_identical(boutons[[2]]$id, "open")
      expect_false(isTRUE(boutons[[2]]$disabled))

      retraits <- Filter(function(a) identical(a$quoi, "retrait"), appels)
      expect_length(retraits, 1L)
      expect_identical(retraits[[1]]$id, toasts[[1]]$id)
    }
  )
})

test_that("le bloc accueille les actions de l'appelant, avant le bouton de chaine", {
  # v0.143.17 : « Voir les resultats » / « Reessayer » / « Lancer le calcul »
  # flottaient AU-DESSUS du bloc. Un bloc nomme « Tableau des actions » qui n'en
  # contenait qu'une seule ne tenait pas sa promesse.
  withr::local_options(nemeton.app_options = list(language = "fr"))
  marqueur <- htmltools::div(id = "sentinelle-actions", "actions de l'appelant")
  html <- as.character(
    nemetonshiny:::mod_pipeline_ui("home-pipeline", actions_ui = marqueur))

  pos_collapse <- regexpr('id="home-pipeline-pipeline_collapse"', html, fixed = TRUE)
  pos_actions  <- regexpr('id="sentinelle-actions"', html, fixed = TRUE)
  pos_chaine   <- regexpr('id="home-pipeline-open"', html, fixed = TRUE)

  # DANS le corps repliable : hors de lui, les actions resteraient visibles
  # bloc replie, ce qui est exactement le defaut qu'on corrige.
  expect_gt(pos_actions, pos_collapse)
  # AVANT le bouton de chaine : elles dependent de l'etat du projet, il est
  # toujours la et ferme la liste.
  expect_lt(pos_actions, pos_chaine)
})

test_that("sans actions_ui le bloc est inchange", {
  # Retro-compatibilite : le defaut NULL doit rendre exactement l'ancien bloc,
  # sinon tout appelant qui ne passe rien verrait sa mise en page bouger.
  withr::local_options(nemeton.app_options = list(language = "fr"))
  html <- as.character(nemetonshiny:::mod_pipeline_ui("pipeline"))
  expect_true(grepl('id="pipeline-open"', html, fixed = TRUE))
  expect_false(grepl("sentinelle-actions", html, fixed = TRUE))
})

test_that("mod_home place ses actions projet dans le bloc, plus au-dessus", {
  withr::local_options(nemeton.app_options = list(language = "fr"))
  html <- as.character(nemetonshiny:::mod_home_ui("home"))

  pos_collapse <- regexpr('id="home-pipeline-pipeline_collapse"', html, fixed = TRUE)
  pos_compute  <- regexpr('id="home-compute_section"', html, fixed = TRUE)
  pos_chaine   <- regexpr('id="home-pipeline-open"', html, fixed = TRUE)

  expect_gt(pos_compute, pos_collapse)   # dans le bloc
  expect_lt(pos_compute, pos_chaine)     # avant « Tout calculer »
})

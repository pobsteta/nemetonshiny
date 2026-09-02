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

test_that("le bouton de lancement porte la couleur d'action principale", {
  # Regle normative des couleurs : vert = action principale. PAS d'ambre - qui
  # signale une provenance (contenu genere), pas un niveau d'action - meme si
  # la chaine se termine par deux generations IA.
  withr::local_options(nemeton.app_options = list(language = "fr"))
  html <- as.character(nemetonshiny:::mod_pipeline_ui("pipeline"))
  expect_true(grepl("btn-primary", html, fixed = TRUE))
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

# Liens profonds ?project=&tab= (specs/BRIEF-pilotage-victor-aigora.md A.4)

test_that(".parse_deep_link keeps a known project and an allowed tab", {
  root <- withr::local_tempdir()
  withr::local_options(nemeton.app_options = list(project_dir = root))
  dir.create(file.path(root, "20261004_101500_abcd"))

  l <- .parse_deep_link("?project=20261004_101500_abcd&tab=synthesis")
  expect_identical(l$project, "20261004_101500_abcd")
  expect_identical(l$tab, "synthesis")
  expect_length(l$invalid, 0L)

  l <- .parse_deep_link("?tab=famille_risque")
  expect_null(l$project)
  expect_identical(l$tab, "famille_risque")
})

test_that(".parse_deep_link rejects unknown or unsafe values", {
  root <- withr::local_tempdir()
  withr::local_options(nemeton.app_options = list(project_dir = root))
  l <- .parse_deep_link("?project=..%2F..%2Fetc&tab=%3Cscript%3E")
  expect_null(l$project)
  expect_null(l$tab)
  expect_setequal(l$invalid, c("project", "tab"))
  expect_identical(.parse_deep_link("")$invalid, character(0))
  expect_identical(.parse_deep_link(NULL)$invalid, character(0))
})

test_that("every deep-link tab exists in the UI", {
  ui <- as.character(withr::with_options(
    list(nemeton.app_options = list(language = "fr")), app_ui(NULL)))
  for (tab in DEEP_LINK_TABS) {
    expect_true(grepl(sprintf('data-value="%s"', tab), ui, fixed = TRUE), label = tab)
  }
})

test_that("the load script sets the home module input, escaped", {
  s <- as.character(.deep_link_load_script("20261004_101500_abcd"))
  expect_match(s, "Shiny.setInputValue(\"home-load_project\", \"20261004_101500_abcd\"",
               fixed = TRUE)
  s2 <- as.character(.deep_link_load_script("a\"b"))
  expect_false(grepl('"a"b"', s2, fixed = TRUE))
})

test_that(".setup_deep_link opens the project, then the tab once it is loaded", {
  root <- withr::local_tempdir()
  withr::local_options(nemeton.app_options = list(project_dir = root, language = "fr"))
  dir.create(file.path(root, "20261004_101500_abcd"))
  inseres <- list()
  onglets <- character(0)
  local_mocked_bindings(
    insertUI = function(selector, where, ui, ...) inseres[[length(inseres) + 1L]] <<- ui,
    updateNavbarPage = function(session, inputId, selected = NULL) onglets <<- c(onglets, selected),
    .package = "shiny"
  )
  serveur <- function(input, output, session) {
    app_state <- shiny::reactiveValues(current_project = NULL, language = "fr")
    .setup_deep_link(session, app_state,
                     url_search = function() "?project=20261004_101500_abcd&tab=synthesis")
    session$userData$app_state <- app_state
  }
  shiny::testServer(serveur, {
    session$flushReact()
    expect_length(inseres, 1L)
    expect_match(as.character(inseres[[1]]), "20261004_101500_abcd", fixed = TRUE)
    expect_false("synthesis" %in% onglets)
    st <- session$userData$app_state
    st$current_project <- list(id = "autre", metadata = list())
    session$flushReact()
    expect_false("synthesis" %in% onglets)
    st$current_project <- list(id = "20261004_101500_abcd", metadata = list())
    session$flushReact()
    expect_identical(onglets, "synthesis")
    # Une seule fois : un rechargement ulterieur ne re-selectionne pas l'onglet
    st$current_project <- list(id = "20261004_101500_abcd", metadata = list(x = 1))
    session$flushReact()
    expect_identical(onglets, "synthesis")
  })
})

test_that(".setup_deep_link with a tab only selects it directly", {
  onglets <- character(0)
  local_mocked_bindings(
    updateNavbarPage = function(session, inputId, selected = NULL) onglets <<- c(onglets, selected),
    .package = "shiny"
  )
  serveur <- function(input, output, session) {
    .setup_deep_link(session, shiny::reactiveValues(current_project = NULL),
                     url_search = function() "?tab=monitoring")
  }
  shiny::testServer(serveur, {
    session$flushReact()
    expect_identical(onglets, "monitoring")
  })
})

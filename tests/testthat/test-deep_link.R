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
    .package = "shiny"
  )
  # Onglet LOGIQUE demande (l'Atlas et ses sous-onglets : service_navigation.R).
  local_mocked_bindings(.aller_onglet = function(session, tab) onglets <<- c(onglets, tab))
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
  # Onglet LOGIQUE demande (l'Atlas et ses sous-onglets : service_navigation.R).
  local_mocked_bindings(.aller_onglet = function(session, tab) onglets <<- c(onglets, tab))
  serveur <- function(input, output, session) {
    .setup_deep_link(session, shiny::reactiveValues(current_project = NULL),
                     url_search = function() "?tab=monitoring")
  }
  shiny::testServer(serveur, {
    session$flushReact()
    expect_identical(onglets, "monitoring")
  })
})

# ---- Signal " page prete " pour VICTOR ----------------------------------------

test_that(".victor_origin defaults, overrides, disables and rejects bad values", {
  withr::with_envvar(c(NEMETON_VICTOR_ORIGIN = NA), {
    expect_identical(.victor_origin(), "http://127.0.0.1:8788")
  })
  withr::with_envvar(c(NEMETON_VICTOR_ORIGIN = "https://victor.example.org:8443/"), {
    expect_identical(.victor_origin(), "https://victor.example.org:8443")
  })
  withr::with_envvar(c(NEMETON_VICTOR_ORIGIN = ""), expect_null(.victor_origin()))
  withr::with_envvar(c(NEMETON_VICTOR_ORIGIN = "*"), expect_null(.victor_origin()))
  withr::with_envvar(c(NEMETON_VICTOR_ORIGIN = "http://h/chemin"), expect_null(.victor_origin()))
})

test_that(".signal_pret sends the target origin, and nothing when disabled", {
  recu <- list()
  fausse <- list(sendCustomMessage = function(type, message) recu[[type]] <<- message)
  withr::with_envvar(c(NEMETON_VICTOR_ORIGIN = NA), {
    expect_true(.signal_pret(fausse, "ready", "p1", "synthesis"))
  })
  expect_identical(recu$nemeton_pret,
                   list(type = "ready", project = "p1", tab = "synthesis",
                        origin = "http://127.0.0.1:8788"))
  recu <- list()
  withr::with_envvar(c(NEMETON_VICTOR_ORIGIN = ""), {
    expect_false(.signal_pret(fausse, "ready"))
  })
  expect_length(recu, 0L)
})

test_that("ready is signalled once the project is loaded and the tab selected", {
  root <- withr::local_tempdir()
  withr::local_options(nemeton.app_options = list(project_dir = root, language = "fr"))
  dir.create(file.path(root, "20261004_101500_abcd"))
  signaux <- list()
  local_mocked_bindings(
    .signal_pret = function(session, type, project = NULL, tab = NULL)
      signaux[[length(signaux) + 1L]] <<- list(type = type, project = project, tab = tab))
  local_mocked_bindings(insertUI = function(...) NULL, updateNavbarPage = function(...) NULL,
                        .package = "shiny")
  serveur <- function(input, output, session) {
    app_state <- shiny::reactiveValues(current_project = NULL, language = "fr")
    .setup_deep_link(session, app_state,
                     url_search = function() "?project=20261004_101500_abcd&tab=synthesis")
    session$userData$app_state <- app_state
  }
  shiny::testServer(serveur, {
    session$flushReact()
    expect_length(signaux, 0L)   # projet pas encore charge
    st <- session$userData$app_state
    st$current_project <- list(id = "20261004_101500_abcd", metadata = list())
    session$flushReact()
    expect_identical(signaux, list(list(type = "ready", project = "20261004_101500_abcd",
                                        tab = "synthesis")))
    st$current_project <- list(id = "20261004_101500_abcd", metadata = list(x = 1))
    session$flushReact()
    expect_length(signaux, 1L)   # une seule fois
  })
})

test_that("tab-only, bare and refused links signal ready / invalid exactly once", {
  cas <- list(
    list(url = "?tab=monitoring", attendu = list(type = "ready", project = NULL, tab = "monitoring")),
    list(url = "", attendu = list(type = "ready", project = NULL, tab = NULL)),
    list(url = "?project=inexistant", attendu = list(type = "invalid", project = NULL, tab = NULL)),
    list(url = "?project=inexistant&tab=synthesis",
         attendu = list(type = "invalid", project = NULL, tab = "synthesis")))
  root <- withr::local_tempdir()
  withr::local_options(nemeton.app_options = list(project_dir = root, language = "fr"))
  local_mocked_bindings(updateNavbarPage = function(...) NULL,
                        showNotification = function(...) NULL, .package = "shiny")
  for (k in cas) {
    signaux <- list()
    local_mocked_bindings(
      .signal_pret = function(session, type, project = NULL, tab = NULL)
        signaux[[length(signaux) + 1L]] <<- list(type = type, project = project, tab = tab))
    serveur <- function(input, output, session) {
      .setup_deep_link(session, shiny::reactiveValues(current_project = NULL, language = "fr"),
                       url_search = function() k$url)
    }
    shiny::testServer(serveur, session$flushReact())
    expect_identical(signaux, list(k$attendu), info = k$url)
  }
})

# ---- Sessions fermees et patches de test ---------------------------------------

test_that(".later_sur swallows the destroyed-session error, not the others", {
  ok <- FALSE
  later::with_temp_loop({
    .later_sur(function() stop(structure(class = c("shiny.destroyed.error", "error", "condition"),
                                         list(message = "detruite", call = NULL))))
    .later_sur(function() ok <<- TRUE)
    expect_no_error(later::run_now(0.1))
  })
  expect_true(ok)
  later::with_temp_loop({
    .later_sur(function() stop("autre"))
    expect_error(later::run_now(0.1), "autre")
  })
})

test_that("no raw later::later() call is left in R/ (audit 2026-10-05)", {
  f <- chemin_source("R"); skip_sans_sources(file.path(f, "utils_io.R"))
  src <- unlist(lapply(setdiff(list.files(f, "[.]R$", full.names = TRUE),
                               file.path(f, "utils_io.R")), readLines, warn = FALSE))
  code <- src[!grepl("^\\s*#", src)]
  expect_false(any(grepl("later::later(", code, fixed = TRUE)))
})

test_that("the test-only patches are guarded by TESTTHAT", {
  f <- chemin_source("tests", "testthat", "helper-fixtures.R"); skip_sans_sources(f)
  src <- readLines(f, warn = FALSE)
  patch <- grep("unlockBinding(", src, fixed = TRUE)
  expect_true(length(patch) >= 2L)
  expect_true(all(grepl("if (.en_test)", src[patch - 1L], fixed = TRUE)))
})

# Plan d'actions : un echec d'ecriture n'est plus affiche comme un succes (audit 1.0)

test_that(".sauver_plan only displays the plan when it was written", {
  skip_if_not_installed("bslib")
  st <- shiny::reactiveValues(current_project = NULL, project_id = NULL, language = "fr")
  notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) notes <<- c(notes, as.character(ui)),
                        .package = "shiny")
  local_mocked_bindings(save_action_plan = function(project_id, plan) FALSE)
  shiny::testServer(mod_action_plan_server, args = list(app_state = st), {
    expect_false(.sauver_plan("p", list(actions = list("x"))))
    expect_null(plan_rv())
    expect_true(any(grepl("enregistr", notes)))
  })
  local_mocked_bindings(save_action_plan = function(project_id, plan) TRUE)
  shiny::testServer(mod_action_plan_server, args = list(app_state = st), {
    expect_true(.sauver_plan("p", list(actions = list("y"))))
    expect_identical(plan_rv()$actions[[1]], "y")
  })
})

test_that("every plan write in the module goes through the checked helper", {
  f <- testthat::test_path("..", "..", "R", "mod_action_plan.R")
  skip_if(!file.exists(f), "sources R absentes")
  code <- readLines(f, warn = FALSE)
  appels <- grep("(?<![A-Za-z_.])save_action_plan\\(", code, perl = TRUE, value = TRUE)
  expect_true(all(grepl("isTRUE\\(save_action_plan\\(pid, plan\\)\\)", appels)))
})

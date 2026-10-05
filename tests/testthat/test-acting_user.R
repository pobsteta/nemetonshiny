# Audit signe par l'utilisateur connecte, pas par le compte systeme (audit 1.0)

test_that(".acting_user prefers the authenticated user's e-mail, then name", {
  auth <- shiny::reactiveValues(authenticated = TRUE, anonymous = FALSE,
                                user_email = "a@b.fr", user_name = "Alice")
  expect_identical(.acting_user(shiny::reactiveValues(auth = auth)), "a@b.fr")
  auth$user_email <- NULL
  expect_identical(.acting_user(shiny::reactiveValues(auth = auth)), "Alice")
})

test_that(".acting_user falls back to the system account only when anonymous", {
  sys_user <- Sys.info()[["user"]]
  auth <- shiny::reactiveValues(authenticated = TRUE, anonymous = TRUE, user_name = "Anonyme")
  expect_identical(.acting_user(shiny::reactiveValues(auth = auth)), sys_user)
  expect_identical(.acting_user(shiny::reactiveValues()), sys_user)
  expect_identical(.acting_user(NULL), sys_user)
})

test_that("no audit signature reads the system account directly", {
  dir_r <- testthat::test_path("..", "..", "R")
  skip_if(!file.exists(file.path(dir_r, "mod_action_plan.R")), "sources R absentes")
  code <- readLines(file.path(dir_r, "mod_action_plan.R"), warn = FALSE)
  expect_false(any(grepl('Sys.info()[["user"]]', code, fixed = TRUE)))
})

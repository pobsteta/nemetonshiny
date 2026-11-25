# Planchers de DESCRIPTION : la version minimale déclarée doit fournir les
# fonctions que l'app appelle (ExtendedTask : shiny 1.8.1 ; input_task_button,
# bind_task_button : bslib 0.7.0 — NEWS des deux paquets).

.plancher <- function(pkg) {
  desc <- system.file("DESCRIPTION", package = "nemetonshiny")
  if (!nzchar(desc)) desc <- testthat::test_path("..", "..", "DESCRIPTION")
  imports <- read.dcf(desc, fields = "Imports")[1, 1]
  m <- regmatches(imports, regexpr(paste0(pkg, "\\s*\\(>=\\s*[0-9.]+\\)"), imports))
  if (!length(m)) return(NA_character_)
  sub(".*>=\\s*([0-9.]+).*", "\\1", m)
}

test_that("le plancher shiny couvre ExtendedTask (shiny 1.8.1)", {
  expect_true(utils::compareVersion(.plancher("shiny"), "1.8.1") >= 0,
              info = .plancher("shiny"))
})

test_that("le plancher bslib couvre input_task_button et bind_task_button (bslib 0.7.0)", {
  expect_true(utils::compareVersion(.plancher("bslib"), "0.7.0") >= 0,
              info = .plancher("bslib"))
})

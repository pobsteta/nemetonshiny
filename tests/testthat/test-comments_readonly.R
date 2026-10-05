# Commentaires : rien d'ecrit en lecture seule, sauvegarde differee (audit 1.0)

.commentaire_state <- function(readonly) {
  shiny::reactiveValues(current_project = NULL, language = "fr",
                        project_id = "p1", readonly = readonly,
                        family_comments = NULL)
}

test_that("a family comment is saved after the debounce, never in read-only", {
  skip_if_not_installed("bslib")
  ecrits <- list()
  local_mocked_bindings(save_comments = function(project_id, synthesis = NULL, families = NULL) {
    ecrits[[length(ecrits) + 1L]] <<- families
    TRUE
  })
  shiny::testServer(mod_family_server,
    args = list(family_code = "C", app_state = .commentaire_state(FALSE)), {
      # Le textarea existe des le demarrage avec une valeur vide
      session$setInputs(analysis_comments = "")
      session$elapse(1100)
      session$setInputs(analysis_comments = "a")
      session$setInputs(analysis_comments = "ab")
      session$elapse(1100)
      expect_identical(ecrits[[length(ecrits)]]$C, "ab")
      expect_false(any(vapply(ecrits, function(e) identical(e$C, "a"), logical(1))))
    })

  ecrits <- list()
  shiny::testServer(mod_family_server,
    args = list(family_code = "C", app_state = .commentaire_state(TRUE)), {
      session$setInputs(analysis_comments = "")
      session$elapse(1100)
      session$setInputs(analysis_comments = "texte")
      session$elapse(1100)
      expect_length(ecrits, 0L)
      expect_true(isTRUE(session$userData$.comments_readonly_warned))
    })
})

test_that(".comments_readonly warns once per session", {
  st <- shiny::reactiveValues(readonly = TRUE, language = "fr")
  s <- shiny::MockShinySession$new()
  notes <- 0L
  local_mocked_bindings(showNotification = function(...) notes <<- notes + 1L, .package = "shiny")
  expect_true(shiny::isolate(.comments_readonly(st, s)))
  expect_true(shiny::isolate(.comments_readonly(st, s)))
  expect_identical(notes, 1L)
  st2 <- shiny::reactiveValues(readonly = FALSE)
  expect_false(shiny::isolate(.comments_readonly(st2, s)))
})

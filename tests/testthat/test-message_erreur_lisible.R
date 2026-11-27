# Mission 090 : message d'erreur lisible des calculs longs.

.esc <- "\033"
.msg_pascal <- function(journal) {
  paste0(
    "! \"", .esc, "[36mrun_reconfort_dieback", .esc, "[39m\" failed in its capped child process (exit 1).\n",
    "ℹ The child's output was kept in\n  '", .esc, "[34m", journal, .esc, "[39m'.\n",
    "→  15.  └─nemeton:::.stac_search_paginate(...)\n",
    "→  16.  └─httr2::req_perform(req)\n",
    "→  17.  └─httr2:::handle_resp(req, resp, error_call = error_call)\n",
    "→  18.  └─rlang::cnd_signal(resp)\n",
    "→ Execution halted")
}

test_that("message reel : sans ANSI ni pile, cause racine lue dans le journal, chemin du journal", {
  withr::with_tempdir({
    journal <- file.path(getwd(), "reconfort_child.log")
    writeLines(c("debut", "✖ Failed to perform HTTP request.",
                 "Caused by error in `curl::curl_fetch_memory()`:",
                 "! Timeout was reached [api.stac.teledetection.fr]: Connection timed out after 60006 milliseconds",
                 "Execution halted"), journal)
    i18n <- nemetonshiny:::get_i18n("fr")
    out <- nemetonshiny:::.message_erreur_lisible(.msg_pascal(journal), i18n)
    expect_false(grepl("\033", out, fixed = TRUE))
    expect_false(grepl("stac_search_paginate|req_perform|Execution halted", out))
    expect_match(out, "Timeout was reached", fixed = TRUE)
    expect_match(out, "api.stac.teledetection.fr", fixed = TRUE)
    expect_match(out, "failed in its capped child process", fixed = TRUE)
    expect_match(out, journal, fixed = TRUE)
    expect_match(out, i18n$t("erreur_journal_prefixe"), fixed = TRUE)
    # Passe aussi par le rendu de la notification.
    rendu <- nemetonshiny:::.monitoring_run_error_msg(
      .msg_pascal(journal), i18n, "monitoring_reconfort_error")
    expect_match(rendu, "Timeout was reached", fixed = TRUE)
    expect_false(grepl("\033|req_perform", rendu))
  })
})

test_that("journal absent : pas d'erreur, le reste du message est garde", {
  out <- nemetonshiny:::.message_erreur_lisible(.msg_pascal("/inexistant/x.log"))
  expect_match(out, "capped child process", fixed = TRUE)
  expect_match(out, "/inexistant/x.log", fixed = TRUE)
  expect_false(grepl("req_perform", out))
})

test_that("message simple : inchange ; ANSI seul : retire ; longueur bornee", {
  expect_identical(nemetonshiny:::.message_erreur_lisible("boum"), "boum")
  expect_identical(nemetonshiny:::.message_erreur_lisible("\033[1mboum\033[22m"), "boum")
  long <- paste(rep("mot", 1000), collapse = " ")
  expect_lte(nchar(nemetonshiny:::.message_erreur_lisible(long, max_chars = 200L)), 201L)
})

test_that("cle i18n FR/EN, 30 caracteres au plus", {
  expect_lte(nchar("erreur_journal_prefixe"), 30L)
  for (l in c("fr", "en")) expect_true(nemetonshiny:::get_i18n(l)$has("erreur_journal_prefixe"))
})

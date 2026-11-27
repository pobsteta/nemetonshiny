# Mission 090 : message d'erreur lisible des calculs longs.

.esc <- "\033"
.msg_pascal <- function(journal) {
  paste0(
    "! \"", .esc, "[36mrun_reconfort_dieback", .esc, "[39m\" failed in its capped child process (exit 1).\n",
    "\u2139 The child's output was kept in\n  '", .esc, "[34m", journal, .esc, "[39m'.\n",
    "\u2192  15.  \u2514\u2500nemeton:::.stac_search_paginate(...)\n",
    "\u2192  16.  \u2514\u2500httr2::req_perform(req)\n",
    "\u2192  17.  \u2514\u2500httr2:::handle_resp(req, resp, error_call = error_call)\n",
    "\u2192  18.  \u2514\u2500rlang::cnd_signal(resp)\n",
    "\u2192 Execution halted")
}

test_that("message reel : sans ANSI ni pile, cause racine lue dans le journal, chemin du journal", {
  withr::with_tempdir({
    journal <- file.path(getwd(), "reconfort_child.log")
    writeLines(c("debut", "\u2716 Failed to perform HTTP request.",
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

test_that("journal en octets invalides ou binaire : pas d'erreur, message rendu", {
  withr::with_tempdir({
    journal <- file.path(getwd(), "x.log")
    set.seed(1)
    writeBin(as.raw(sample(0:255, 20000, replace = TRUE)), journal)
    i18n <- nemetonshiny:::get_i18n("fr")
    out <- expect_no_error(nemetonshiny:::.message_erreur_lisible(.msg_pascal(journal), i18n))
    expect_match(out, "capped child process", fixed = TRUE)
    # Coupe au milieu d'un caractere multioctet.
    brut <- c(charToRaw(strrep("a", 16383)), as.raw(c(0xc3)), charToRaw("\nCaused by x\n! Timeout\n"))
    brut <- c(as.raw(c(0xe2, 0x9c)), brut)
    writeBin(brut, journal)
    out <- expect_no_error(nemetonshiny:::.message_erreur_lisible(.msg_pascal(journal), i18n))
    expect_match(out, "Timeout", fixed = TRUE)
    expect_no_error(nemetonshiny:::.monitoring_run_error_msg(.msg_pascal(journal), i18n, "monitoring_reconfort_error"))
  })
})

test_that("rien d'actif dans la notification : HTML echappe", {
  withr::with_tempdir({
    journal <- file.path(getwd(), "h.log")
    writeLines(c("Caused by error:", "! <b>boom</b> <img src=x onerror=alert(1)> <script>x()</script>"), journal)
    i18n <- nemetonshiny:::get_i18n("fr")
    out <- nemetonshiny:::.monitoring_run_error_msg(.msg_pascal(journal), i18n, "monitoring_reconfort_error")
    expect_false(grepl("<script|<img|<b>", out))
    expect_match(out, "&lt;script&gt;", fixed = TRUE)
  })
})

test_that("chemin du journal avec apostrophe et espace ; lecture limitee a .log", {
  withr::with_tempdir({
    dir.create("l'a b")
    journal <- file.path(getwd(), "l'a b", "c.log")
    writeLines(c("Caused by error:", "! Timeout was reached"), journal)
    out <- nemetonshiny:::.message_erreur_lisible(.msg_pascal(journal))
    expect_match(out, journal, fixed = TRUE)
    expect_match(out, "Timeout was reached", fixed = TRUE)
    autre <- file.path(getwd(), "l'a b", "c.txt")
    writeLines(c("Caused by error:", "! Secret"), autre)
    out2 <- nemetonshiny:::.message_erreur_lisible(.msg_pascal(autre))
    expect_match(out2, autre, fixed = TRUE)
    expect_false(grepl("Secret", out2))
  })
})

test_that("journal + troncature : la ligne Journal reste entiere", {
  withr::with_tempdir({
    journal <- file.path(getwd(), "t.log")
    writeLines(c("Caused by error:", paste("!", strrep("mot ", 400))), journal)
    out <- nemetonshiny:::.message_erreur_lisible(.msg_pascal(journal), max_chars = 300L)
    expect_match(out, paste0(journal, "$"))
    expect_lte(nchar(out), 301L + nchar(journal))
  })
})

# Tests — helpers de notification unifiés (R/utils_notif.R + .ntfy_title)

test_that(".fmt_elapsed formats MM:SS and H:MM:SS", {
  expect_equal(nemetonshiny:::.fmt_elapsed(NULL), "")
  now <- Sys.time()
  expect_equal(nemetonshiny:::.fmt_elapsed(now - 65), "01:05")
  expect_equal(nemetonshiny:::.fmt_elapsed(now - 3661), "1:01:01")
  expect_equal(nemetonshiny:::.fmt_elapsed(now + 10), "00:00")  # future clampé
})

test_that(".running_notif_content renders spinning gear + label, chrono only with start", {
  no_chrono <- as.character(nemetonshiny:::.running_notif_content("Ingestion 3/10"))
  expect_true(grepl("nmt-spin", no_chrono))          # engrenage qui tourne
  expect_true(grepl("Ingestion 3/10", no_chrono))
  expect_false(grepl("font-monospace", no_chrono))   # pas de chrono sans start

  with_chrono <- as.character(
    nemetonshiny:::.running_notif_content("Ingestion 3/10", Sys.time() - 65))
  expect_true(grepl("nmt-spin", with_chrono))
  expect_true(grepl("font-monospace", with_chrono))  # chrono présent
  expect_true(grepl("01:05", with_chrono))
})

test_that(".monitoring_spinning_msg reuses the unified frame (spinning gear)", {
  html <- as.character(nemetonshiny:::.monitoring_spinning_msg("Tuile 2/8"))
  expect_true(grepl("nmt-spin", html))
  expect_true(grepl("Tuile 2/8", html))
})

test_that(".ntfy_title is ASCII-safe and appends the transliterated project", {
  # Base seul quand pas de projet
  expect_equal(nemetonshiny:::.ntfy_title("FAST"), "Nemeton FAST")
  expect_equal(nemetonshiny:::.ntfy_title("Regen", NULL), "Nemeton Regen")
  expect_equal(nemetonshiny:::.ntfy_title("RECONFORT", ""), "Nemeton RECONFORT")

  # Projet accentué → translittéré, sortie strictement ASCII imprimable
  t1 <- nemetonshiny:::.ntfy_title("FAST", "Forêt de Mouthé")
  expect_match(t1, "^Nemeton FAST - ")
  expect_false(grepl("[^ -~]", t1))          # aucun caractère non-ASCII
  expect_match(t1, "Foret de Mouthe")

  t2 <- nemetonshiny:::.ntfy_title("FORDEAD", "Zone Éée çà")
  expect_false(grepl("[^ -~]", t2))
})

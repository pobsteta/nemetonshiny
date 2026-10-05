# Notifications ntfy sans chemins ni identifiants (audit 1.0, constat mineur securite)

test_that(".ntfy_safe_message strips paths and credentials", {
  m <- .ntfy_safe_message(
    "cannot open /home/pascal/projets/abc/cache/layers/dem.tif ; postgresql://u:secret@h/db")
  expect_false(grepl("/home/pascal", m, fixed = TRUE))
  expect_match(m, "dem.tif", fixed = TRUE)
  expect_false(grepl("secret", m, fixed = TRUE))
  w <- .ntfy_safe_message("C:\\Users\\x\\data\\f.tif failed")
  expect_false(grepl("Users", w, fixed = TRUE))
  expect_match(w, "f.tif failed", fixed = TRUE)
  long <- .ntfy_safe_message(strrep("a", 1000))
  expect_lte(nchar(long), 300L)
  expect_identical(.ntfy_safe_message("Calcul termin\u00e9 (12 alertes)"),
                   "Calcul termin\u00e9 (12 alertes)")
})

test_that(".ntfy_send sends the cleaned message and warns once on a public topic", {
  corps <- NULL
  local_mocked_bindings(
    req_perform = function(req) { corps <<- req$body$data; invisible(NULL) },
    .package = "httr2")
  .nemeton_env$.ntfy_public_warned <- NULL
  cfg <- list(url = "https://ntfy.sh", topic = "t", token = "")
  expect_warning(.ntfy_send(cfg, "echec /srv/data/projets/p1/x.gpkg"), "ntfy.sh")
  expect_identical(corps, "echec x.gpkg")
  expect_no_warning(.ntfy_send(cfg, "ok"))
  .nemeton_env$.ntfy_public_warned <- NULL
  expect_no_warning(.ntfy_send(list(url = "https://ntfy.example.org", topic = "t", token = ""), "ok"))
})

test_that(".mask_db_credentials masks URL and key-value passwords", {
  expect_identical(.mask_db_credentials("postgresql://user:p%40ss@host:5432/db"),
                   "postgresql://user:***@host:5432/db")
  expect_identical(.mask_db_credentials("host=h password=abc dbname=d"),
                   "host=h password=*** dbname=d")
})

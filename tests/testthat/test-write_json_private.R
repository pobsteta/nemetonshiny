# Fichiers de cles owner-only des la creation (audit 1.0, constat mineur securite)

test_that(".write_json_private creates an owner-only file and restores the umask", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  f <- file.path(d, "sous", "cles.json")
  dir.create(dirname(f))
  avant <- Sys.umask()
  .write_json_private(list(a = "secret"), f, auto_unbox = TRUE)
  expect_identical(Sys.umask(), avant)
  expect_identical(format(file.mode(f)), "600")
  expect_identical(jsonlite::read_json(f)$a, "secret")
  # Aucun fichier temporaire laisse
  expect_identical(list.files(dirname(f)), "cles.json")
})

test_that("Theia and LLM key writers go through the private writer", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  local_mocked_bindings(.theia_apikey_path = function() file.path(d, "theia.json"))
  withr::local_envvar(TLD_ACCESS_KEY = "", TLD_SECRET_KEY = "")
  expect_true(suppressMessages(theia_save_api_key("ak", "sk")))
  expect_identical(format(file.mode(file.path(d, "theia.json"))), "600")
  local_mocked_bindings(.llm_apikey_path = function() file.path(d, "llm.json"))
  .llm_write_file(list(MISTRAL_API_KEY = "x"))
  expect_identical(format(file.mode(file.path(d, "llm.json"))), "600")
})

# Base de suivi : TLS, erreur par session, zones par projet (audit 1.0)

test_that(".monitoring_sslmode requires TLS for a remote host only", {
  expect_identical(.monitoring_sslmode("postgresql://u:p@db.example.org:5432/x"), "require")
  expect_identical(.monitoring_sslmode("postgres://u:p@localhost/x"), "prefer")
  expect_identical(.monitoring_sslmode("postgresql://u:p@127.0.0.1/x"), "prefer")
  expect_null(.monitoring_sslmode("sqlite:///tmp/m.sqlite"))
})

test_that(".nemeton_db_connect sets PGSSLMODE for the call, then restores it", {
  withr::local_envvar(PGSSLMODE = "")
  vu <- NULL
  local_mocked_bindings(db_connect = function(url, ...) { vu <<- Sys.getenv("PGSSLMODE"); "CON" },
                        .package = "nemeton")
  expect_identical(.nemeton_db_connect("postgresql://u:p@db.example.org/x"), "CON")
  expect_identical(vu, "require")
  expect_identical(Sys.getenv("PGSSLMODE"), "")
  # Valeur de l'exploitant respectee
  withr::local_envvar(PGSSLMODE = "verify-full")
  .nemeton_db_connect("postgresql://u:p@db.example.org/x")
  expect_identical(vu, "verify-full")
  .nemeton_db_connect("sqlite:///tmp/m.sqlite")
  expect_identical(Sys.getenv("PGSSLMODE"), "verify-full")
})

test_that("monitoring DB errors are masked and kept per session", {
  .nemeton_env$.last_monitoring_db_error <- NULL
  s1 <- shiny::MockShinySession$new()
  s2 <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(s1, .set_monitoring_db_error(
    "could not connect postgresql://u:secret@h/db"))
  expect_identical(shiny::withReactiveDomain(s1, last_monitoring_db_error()),
                   "could not connect postgresql://u:***@h/db")
  expect_null(shiny::withReactiveDomain(s2, last_monitoring_db_error()))
  expect_null(last_monitoring_db_error())
  .set_monitoring_db_error("hors session")
  expect_identical(last_monitoring_db_error(), "hors session")
  .nemeton_env$.last_monitoring_db_error <- NULL
})

test_that("list_monitoring_zones filters by project when given", {
  appel <- NULL
  local_mocked_bindings(find_zones_by_project = function(con, project_uuid) {
    appel <<- project_uuid
    data.frame(id = 3L, name = "p_tot")
  }, .package = "nemeton")
  z <- list_monitoring_zones("CON", project_uuid = "p-uuid")
  expect_identical(appel, "p-uuid")
  expect_identical(z$name, "p_tot")
  expect_identical(nrow(list_monitoring_zones(NULL, "x")), 0L)
})

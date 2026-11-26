# Decoupage du schema SQL en instructions (mission 083)

test_that(".sql_decouper ne coupe pas sur un point-virgule de commentaire", {
  sql <- paste("-- entete ; avec point-virgule",
               "CREATE TABLE a (x INT); -- fin ; de ligne",
               "-- (une ligne ; deux)",
               "CREATE INDEX i ON a (x);", sep = "\n")
  st <- .sql_decouper(sql)
  expect_equal(st, c("CREATE TABLE a (x INT)", "CREATE INDEX i ON a (x)"))
})

test_that("schema.sql : chaque instruction commence par un mot SQL attendu", {
  f <- system.file("sql", "schema.sql", package = "nemetonshiny")
  skip_if_not(nzchar(f) && file.exists(f))
  sql_text <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  st <- .sql_decouper(sql_text)
  expect_gt(length(st), 20)
  expect_true(all(grepl("^(CREATE|ALTER|INSERT|DO|COMMENT|DROP|GRANT|SET|UPDATE)\\b",
                        st)))
  # Les tables d'archive, jadis sautees, y sont
  expect_true(any(grepl("nemeton\\.regeneration_states", st)))
  expect_true(any(grepl("nemeton\\.action_plan_states", st)))
  # Hypothese du decoupeur : aucune chaine SQL ne contient `--` ni `;`
  lignes <- strsplit(sql_text, "\n", fixed = TRUE)[[1]]
  lignes <- lignes[!grepl("^\\s*--", lignes)]
  chaines <- unlist(regmatches(lignes, gregexpr("'[^']*'", lignes)))
  expect_false(any(grepl("--|;", chaines)))
})

test_that("db_init_schema cree les tables d'archive sans avertissement", {
  url <- Sys.getenv("NEMETON_DB_URL_TEST", "")
  skip_if(!nzchar(url), "NEMETON_DB_URL_TEST non defini")
  skip_if_not_installed("RPostgres")
  # postgresql://user@host/base?host=/chemin/socket
  m <- regmatches(url, regexec("^postgres(?:ql)?://([^@/]*)@([^/?]*)/([^?]+)(?:\\?host=(.*))?$",
                               url, perl = TRUE))[[1]]
  skip_if(length(m) < 4, "NEMETON_DB_URL_TEST illisible")
  skip_if_not(grepl("test", m[4]), "la base doit s'appeler *test*")
  hote <- if (length(m) >= 5 && nzchar(m[5])) m[5] else m[3]
  con <- tryCatch(DBI::dbConnect(RPostgres::Postgres(), host = hote,
                                 dbname = m[4], user = m[2]),
                  error = function(e) NULL)
  skip_if(is.null(con), "base de test injoignable")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "DROP TABLE IF EXISTS nemeton.regeneration_states, nemeton.action_plan_states")
  expect_no_warning(ok <- db_init_schema(con))
  expect_true(ok)
  for (t in c("regeneration_states", "action_plan_states")) {
    expect_true(DBI::dbExistsTable(con, DBI::Id(schema = "nemeton", table = t)))
  }
})

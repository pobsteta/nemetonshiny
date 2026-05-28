# Tests for service_monitoring_db.R (E6.b phase 1 — DB connection adapter).
#
# Pure-unit coverage for URL building and graceful-failure paths. Live
# DB integration tests live in nemeton's test-monitoring.R + test-db.R;
# this file only exercises the adapter logic that is specific to the
# Shiny app.

# Convenience: clear every env var that the resolver consults.
clear_monitoring_db_envvars <- function() {
  withr::local_envvar(c(
    NEMETON_DB_URL          = NA,
    NEMETON_DB_LOCAL        = NA,
    NEMETON_DB_HOST         = NA,
    NEMETON_DB_PORT         = NA,
    NEMETON_DB_NAME         = NA,
    NEMETON_DB_USER         = NA,
    NEMETON_DB_PASSWORD     = NA,
    POSTGRESQL_ADDON_HOST     = NA,
    POSTGRESQL_ADDON_PORT     = NA,
    POSTGRESQL_ADDON_DB       = NA,
    POSTGRESQL_ADDON_USER     = NA,
    POSTGRESQL_ADDON_PASSWORD = NA
  ), .local_envir = parent.frame())
}


# ---- .build_monitoring_db_url ---------------------------------------

test_that(".build_monitoring_db_url returns '' when no env vars set", {
  clear_monitoring_db_envvars()
  expect_equal(nemetonshiny:::.build_monitoring_db_url(), "")
})

test_that(".build_monitoring_db_url returns '' when host but creds missing", {
  clear_monitoring_db_envvars()
  withr::local_envvar(c(NEMETON_DB_HOST = "127.0.0.1"))
  expect_equal(nemetonshiny:::.build_monitoring_db_url(), "")
})

test_that(".build_monitoring_db_url builds a complete URL from NEMETON_DB_*", {
  clear_monitoring_db_envvars()
  withr::local_envvar(c(
    NEMETON_DB_HOST     = "127.0.0.1",
    NEMETON_DB_PORT     = "5432",
    NEMETON_DB_NAME     = "nemeton",
    NEMETON_DB_USER     = "neo",
    NEMETON_DB_PASSWORD = "morpheus"
  ))
  url <- nemetonshiny:::.build_monitoring_db_url()
  expect_equal(url, "postgresql://neo:morpheus@127.0.0.1:5432/nemeton")
})

test_that(".build_monitoring_db_url prefers POSTGRESQL_ADDON_* over NEMETON_DB_*", {
  clear_monitoring_db_envvars()
  withr::local_envvar(c(
    NEMETON_DB_HOST           = "fallback.example",
    NEMETON_DB_NAME           = "fallback_db",
    NEMETON_DB_USER           = "fallback_user",
    NEMETON_DB_PASSWORD       = "fallback_pwd",
    POSTGRESQL_ADDON_HOST     = "primary.example",
    POSTGRESQL_ADDON_PORT     = "6543",
    POSTGRESQL_ADDON_DB       = "primary_db",
    POSTGRESQL_ADDON_USER     = "primary_user",
    POSTGRESQL_ADDON_PASSWORD = "primary_pwd"
  ))
  url <- nemetonshiny:::.build_monitoring_db_url()
  expect_match(url, "primary\\.example:6543/primary_db")
  expect_match(url, "primary_user:primary_pwd@")
  expect_false(grepl("fallback", url))
})

test_that(".build_monitoring_db_url URL-encodes special chars in credentials", {
  clear_monitoring_db_envvars()
  withr::local_envvar(c(
    NEMETON_DB_HOST     = "127.0.0.1",
    NEMETON_DB_NAME     = "nemeton",
    NEMETON_DB_USER     = "user@with:colon",
    NEMETON_DB_PASSWORD = "p@ss/w?rd"
  ))
  url <- nemetonshiny:::.build_monitoring_db_url()
  # Reserved chars must be percent-encoded so the URL parser won't
  # mistake them for delimiters.
  expect_false(grepl("@with:colon", url))
  expect_false(grepl("p@ss/w?rd", url))
  expect_match(url, "user%40with%3Acolon")
  expect_match(url, "p%40ss%2Fw%3Frd")
})


# ---- get_monitoring_db_connection -----------------------------------

test_that("get_monitoring_db_connection returns NULL when nothing is configured", {
  skip_if_not_installed("nemeton")
  clear_monitoring_db_envvars()
  expect_null(get_monitoring_db_connection())
})

test_that("get_monitoring_db_connection passes NEMETON_DB_URL straight to nemeton", {
  skip_if_not_installed("nemeton")
  clear_monitoring_db_envvars()
  withr::local_envvar(c(
    NEMETON_DB_URL = "postgresql://u:p@h:5432/d"
  ))
  captured_url <- NULL
  testthat::with_mocked_bindings(
    db_connect = function(url) { captured_url <<- url; "fake-con" },
    .package = "nemeton",
    {
      out <- get_monitoring_db_connection()
      expect_equal(out, "fake-con")
      expect_equal(captured_url, "postgresql://u:p@h:5432/d")
    }
  )
})

test_that("get_monitoring_db_connection swallows nemeton::db_connect errors and warns", {
  skip_if_not_installed("nemeton")
  clear_monitoring_db_envvars()
  withr::local_envvar(c(NEMETON_DB_URL = "postgresql://u:p@h:5432/d"))
  testthat::with_mocked_bindings(
    db_connect = function(url) stop("connection refused"),
    .package = "nemeton",
    {
      expect_warning(out <- get_monitoring_db_connection(),
                     "connection refused")
      expect_null(out)
    }
  )
})

# ---- read-only lifecycle (SQLite/WAL 1 writer + N readers) ----------

test_that(".is_file_db_url detects sqlite URLs and bare local paths", {
  expect_true(nemetonshiny:::.is_file_db_url("/x/monitoring.sqlite"))
  expect_true(nemetonshiny:::.is_file_db_url("sqlite:///x/m.sqlite"))
  expect_true(nemetonshiny:::.is_file_db_url("sqlite://C:/x/m.sqlite"))
  expect_true(nemetonshiny:::.is_file_db_url("/x/m.db"))
  expect_false(nemetonshiny:::.is_file_db_url("postgresql://u:p@h:5432/d"))
  # DuckDB is no longer a supported local backend (cut net, v0.50.0).
  expect_false(nemetonshiny:::.is_file_db_url("/x/monitoring.duckdb"))
  expect_false(nemetonshiny:::.is_file_db_url("duckdb:///x/m.duckdb"))
})

test_that(".file_db_path_from_url strips the sqlite scheme", {
  expect_equal(nemetonshiny:::.file_db_path_from_url("/x/m.sqlite"),
               "/x/m.sqlite")
  expect_equal(nemetonshiny:::.file_db_path_from_url("sqlite:///x/m.sqlite"),
               "/x/m.sqlite")
  expect_equal(nemetonshiny:::.file_db_path_from_url("sqlite://C:/x/m.sqlite"),
               "C:/x/m.sqlite")
})

test_that("read_only path opens RO and skips migration (PostgreSQL)", {
  skip_if_not_installed("nemeton")
  clear_monitoring_db_envvars()
  withr::local_envvar(c(NEMETON_DB_URL = "postgresql://u:p@h:5432/d"))
  seen_ro <- NULL
  migrated <- FALSE
  testthat::with_mocked_bindings(
    db_connect = function(url, read_only = FALSE) {
      seen_ro <<- read_only
      structure(list(), class = "fake_con")
    },
    db_migrate = function(con) { migrated <<- TRUE; invisible() },
    .package = "nemeton",
    {
      out <- get_monitoring_db_connection(read_only = TRUE)
      expect_s3_class(out, "fake_con")
      expect_true(seen_ro)         # read_only forwarded to db_connect
      expect_false(migrated)       # migration skipped on RO path
    }
  )
})

test_that("read_only path returns NULL when the local SQLite file is absent", {
  skip_if_not_installed("nemeton")
  clear_monitoring_db_envvars()
  missing <- file.path(tempfile("nodb-"), "monitoring.sqlite")
  withr::local_envvar(c(NEMETON_DB_URL = paste0("sqlite://", missing)))
  called <- FALSE
  testthat::with_mocked_bindings(
    db_connect = function(url, read_only = FALSE) {
      called <<- TRUE
      structure(list(), class = "fake_con")
    },
    .package = "nemeton",
    {
      out <- get_monitoring_db_connection(read_only = TRUE)
      expect_null(out)             # file absent → graceful NULL
      expect_false(called)         # never even attempts to connect
    }
  )
})


# ---- local backend : SQLite/WAL only (DuckDB removed) --------------

test_that(".resolve_monitoring_db_url emits a sqlite:// URL for a local project", {
  skip_if_not_installed("RSQLite")
  clear_monitoring_db_envvars()
  withr::local_envvar(c(NEMETON_DB_LOCAL = "1"))
  withr::with_tempdir({
    proj <- list(path = getwd())
    url <- nemetonshiny:::.resolve_monitoring_db_url(proj)
    expect_match(url, "^sqlite://")
    expect_match(url, "monitoring\\.sqlite$")
    expect_equal(nemetonshiny:::monitoring_db_backend(proj), "local")
  })
})

test_that(".resolve_monitoring_db_url ignores a lingering monitoring.duckdb", {
  # Cut net : even when a legacy DuckDB file is still on disk, the local
  # backend always resolves to SQLite — no back-compat branch.
  skip_if_not_installed("RSQLite")
  clear_monitoring_db_envvars()
  withr::local_envvar(c(NEMETON_DB_LOCAL = "1"))
  withr::with_tempdir({
    proj <- list(path = getwd())
    dir.create("data")
    file.create(file.path("data", "monitoring.duckdb"))
    url <- nemetonshiny:::.resolve_monitoring_db_url(proj)
    expect_match(url, "^sqlite://")
    expect_match(url, "monitoring\\.sqlite$")
  })
})

test_that(".resolve_monitoring_db_url leaves a PostgreSQL URL untouched", {
  clear_monitoring_db_envvars()
  withr::local_envvar(c(NEMETON_DB_URL = "postgresql://u:p@h:5432/d"))
  url <- nemetonshiny:::.resolve_monitoring_db_url(NULL)
  expect_equal(url, "postgresql://u:p@h:5432/d")
  expect_equal(nemetonshiny:::monitoring_db_backend(NULL), "postgres")
})


# ---- list_monitoring_zones ------------------------------------------

test_that("list_monitoring_zones returns an empty df when con is NULL", {
  out <- list_monitoring_zones(NULL)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("id", "name"))
})

test_that("list_monitoring_zones returns empty df + warning on query failure", {
  fake_con <- structure(list(), class = "FakeCon")
  testthat::with_mocked_bindings(
    dbGetQuery = function(conn, ...) stop("schema not migrated"),
    .package = "DBI",
    {
      expect_warning(out <- list_monitoring_zones(fake_con),
                     "schema not migrated")
      expect_equal(nrow(out), 0L)
    }
  )
})

test_that("list_monitoring_zones forwards the DB result on success", {
  fake_con <- structure(list(), class = "FakeCon")
  testthat::with_mocked_bindings(
    dbGetQuery = function(conn, ...) {
      data.frame(id = c(1L, 2L), name = c("Aerodrome", "Bois"),
                 stringsAsFactors = FALSE)
    },
    .package = "DBI",
    {
      out <- list_monitoring_zones(fake_con)
      expect_equal(nrow(out), 2L)
      expect_equal(out$name, c("Aerodrome", "Bois"))
    }
  )
})


# ---- close_monitoring_db_connection ---------------------------------

test_that("close_monitoring_db_connection is a no-op for NULL", {
  expect_silent(out <- close_monitoring_db_connection(NULL))
  expect_true(out)
})

test_that("close_monitoring_db_connection delegates to nemeton::db_disconnect", {
  skip_if_not_installed("nemeton")
  closed <- FALSE
  testthat::with_mocked_bindings(
    db_disconnect = function(con) { closed <<- TRUE; invisible(TRUE) },
    .package = "nemeton",
    {
      close_monitoring_db_connection("fake-con")
      expect_true(closed)
    }
  )
})


# ---- register_project_as_zone ---------------------------------------

# Helper: a project on disk with a saved samples.gpkg and a tiny
# UGF polygon, ready to feed register_project_as_zone(). Returns the
# project list in the same shape as `app_state$current_project`.
.test_make_project_with_samples <- function(name = "Zone Test",
                                            n_plots = 4L,
                                            extra_meta = list()) {
  poly <- sf::st_polygon(list(rbind(
    c(900000, 6500000), c(901000, 6500000),
    c(901000, 6501000), c(900000, 6501000), c(900000, 6500000)
  )))
  indicators_sf <- sf::st_sf(
    ug_id = 1L,
    geometry = sf::st_sfc(poly, crs = 2154)
  )

  proj <- nemetonshiny:::create_project(name = name)

  pts <- lapply(seq_len(n_plots), function(i) {
    sf::st_point(c(900200 + i * 50, 6500200 + i * 50))
  })
  plots <- sf::st_sf(
    plot_id  = sprintf("P%03d", seq_len(n_plots)),
    type     = "Base",
    geometry = sf::st_sfc(pts, crs = 2154)
  )
  nemetonshiny:::save_samples(proj$id, plots)

  if (length(extra_meta)) {
    nemetonshiny:::update_project_metadata(proj$id, extra_meta)
  }

  list(
    id            = proj$id,
    path          = proj$path,
    metadata      = nemetonshiny:::load_project_metadata(proj$id),
    indicators_sf = indicators_sf
  )
}

test_that("register_project_as_zone aborts when con is NULL", {
  expect_error(
    nemetonshiny:::register_project_as_zone(NULL, list(id = "x")),
    regexp = "DB is not connected"
  )
})

test_that("register_project_as_zone aborts when project is missing or has no id", {
  expect_error(
    nemetonshiny:::register_project_as_zone("fake-con", NULL),
    regexp = "No project loaded"
  )
  expect_error(
    nemetonshiny:::register_project_as_zone("fake-con", list(metadata = list())),
    regexp = "No project loaded"
  )
})

test_that("register_project_as_zone aborts when project has no sampling plan", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        proj <- nemetonshiny:::create_project(name = "No Plan")
        project <- list(
          id = proj$id, path = proj$path,
          metadata = nemetonshiny:::load_project_metadata(proj$id),
          indicators_sf = NULL
        )
        expect_error(
          nemetonshiny:::register_project_as_zone("fake-con", project),
          regexp = "no sampling plan"
        )
      }
    )
  })
})

test_that("register_project_as_zone calls nemeton + persists monitoring_zone_id", {
  skip_if_not_installed("sf")
  skip_if_not_installed("nemeton")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- .test_make_project_with_samples(name = "Forest A",
                                                   n_plots = 6L)

        captured_args <- NULL
        testthat::with_mocked_bindings(
          dbGetQuery = function(conn, ...) data.frame(),
          .package = "DBI",
          {
            testthat::with_mocked_bindings(
              register_monitoring_zone = function(con, zone_name,
                                                  zone_polygon, placettes,
                                                  radius_m = 15,
                                                  project_uuid = NULL) {
                captured_args <<- list(zone_name    = zone_name,
                                       n_plots      = nrow(placettes),
                                       project_uuid = project_uuid)
                42L
              },
              .package = "nemeton",
              {
                result <- nemetonshiny:::register_project_as_zone(
                  con = "fake-con", project = project
                )
                expect_equal(result$zone_id, 42L)
                expect_equal(result$zone_name, "Forest A")
                expect_equal(result$n_plots, 6L)
                expect_false(result$was_existing)
              }
            )
          }
        )
        expect_equal(captured_args$zone_name, "Forest A")
        expect_equal(captured_args$n_plots,   6L)
        # Spec 011 — project_uuid is forwarded from project$id.
        expect_identical(captured_args$project_uuid, project$id)

        # Persistence: metadata.json now carries monitoring_zone_id.
        meta <- nemetonshiny:::load_project_metadata(project$id)
        expect_equal(as.integer(meta$monitoring_zone_id), 42L)
      }
    )
  })
})

test_that("register_project_as_zone is idempotent when metadata id still exists in DB", {
  skip_if_not_installed("sf")
  skip_if_not_installed("nemeton")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- .test_make_project_with_samples(
          name = "Forest B",
          extra_meta = list(monitoring_zone_id = 7L)
        )

        register_call_count <- 0L
        update_calls <- list()
        testthat::with_mocked_bindings(
          dbGetQuery = function(conn, ...) {
            # Spec 011 — the SELECT now includes project_uuid.
            # NA simulates a pre-spec-011 zone that needs backfill.
            data.frame(id = 7L, name = "Forest B (DB)",
                       project_uuid = NA_character_,
                       stringsAsFactors = FALSE)
          },
          .package = "DBI",
          {
            testthat::with_mocked_bindings(
              dbExecute = function(conn, statement, params = NULL, ...) {
                update_calls[[length(update_calls) + 1L]] <<- list(
                  statement = statement, params = params
                )
                1L
              },
              .package = "DBI",
              {
                testthat::with_mocked_bindings(
                  register_monitoring_zone = function(...) {
                    register_call_count <<- register_call_count + 1L
                    999L
                  },
                  .package = "nemeton",
                  {
                    result <- nemetonshiny:::register_project_as_zone(
                      con = "fake-con", project = project
                    )
                    expect_equal(result$zone_id, 7L)
                    expect_equal(result$zone_name, "Forest B (DB)")
                    expect_true(result$was_existing)
                  }
                )
              }
            )
          }
        )
        # No insert happened — zone was already there.
        expect_equal(register_call_count, 0L)
        # Backfill UPDATE was issued for the missing project_uuid.
        expect_equal(length(update_calls), 1L)
        expect_match(update_calls[[1]]$statement,
                     "UPDATE monitoring_zone SET project_uuid")
        expect_identical(update_calls[[1]]$params[[1]],
                         as.character(project$id))
        expect_identical(update_calls[[1]]$params[[2]], 7L)
      }
    )
  })
})

test_that("register_project_as_zone skips backfill when project_uuid already set", {
  skip_if_not_installed("sf")
  skip_if_not_installed("nemeton")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- .test_make_project_with_samples(
          name = "Forest B2",
          extra_meta = list(monitoring_zone_id = 7L)
        )

        update_call_count <- 0L
        testthat::with_mocked_bindings(
          dbGetQuery = function(conn, ...) {
            data.frame(id = 7L, name = "Forest B2 (DB)",
                       project_uuid = project$id,
                       stringsAsFactors = FALSE)
          },
          .package = "DBI",
          {
            testthat::with_mocked_bindings(
              dbExecute = function(...) {
                update_call_count <<- update_call_count + 1L
                1L
              },
              .package = "DBI",
              {
                testthat::with_mocked_bindings(
                  register_monitoring_zone = function(...) 999L,
                  .package = "nemeton",
                  {
                    result <- nemetonshiny:::register_project_as_zone(
                      con = "fake-con", project = project
                    )
                    expect_equal(result$zone_id, 7L)
                    expect_true(result$was_existing)
                  }
                )
              }
            )
          }
        )
        expect_equal(update_call_count, 0L)
      }
    )
  })
})

test_that("register_project_as_zone re-creates zone when stored id has been deleted", {
  skip_if_not_installed("sf")
  skip_if_not_installed("nemeton")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- .test_make_project_with_samples(
          name = "Forest C",
          extra_meta = list(monitoring_zone_id = 13L)
        )

        testthat::with_mocked_bindings(
          dbGetQuery = function(conn, ...) data.frame(),  # not in DB anymore
          .package = "DBI",
          {
            testthat::with_mocked_bindings(
              register_monitoring_zone = function(...) 88L,
              .package = "nemeton",
              {
                result <- nemetonshiny:::register_project_as_zone(
                  con = "fake-con", project = project
                )
                expect_equal(result$zone_id, 88L)
                expect_false(result$was_existing)
              }
            )
          }
        )
        meta <- nemetonshiny:::load_project_metadata(project$id)
        expect_equal(as.integer(meta$monitoring_zone_id), 88L)
      }
    )
  })
})

test_that("register_project_as_zone aborts when project has no UGF geometry", {
  skip_if_not_installed("sf")
  withr::with_tempdir({
    temp_root <- getwd()
    with_mocked_bindings(
      get_app_options = function() list(project_dir = temp_root),
      {
        project <- .test_make_project_with_samples(name = "No Geom")
        project$indicators_sf <- NULL

        testthat::with_mocked_bindings(
          dbGetQuery = function(conn, ...) data.frame(),
          .package = "DBI",
          {
            expect_error(
              nemetonshiny:::register_project_as_zone(
                con = "fake-con", project = project
              ),
              regexp = "UGF geometry"
            )
          }
        )
      }
    )
  })
})


# ============================================================================
# .resolve_monitoring_db_url — SQLite/WAL local fallback
# ============================================================================

test_that(".resolve_monitoring_db_url passes through NEMETON_DB_URL", {
  withr::local_envvar(c(NEMETON_DB_URL = "postgresql://u:p@host/db"))
  expect_equal(
    nemetonshiny:::.resolve_monitoring_db_url(NULL),
    "postgresql://u:p@host/db"
  )
})

test_that(".resolve_monitoring_db_url builds PG URL from env parts", {
  clear_monitoring_db_envvars()
  withr::local_envvar(c(
    NEMETON_DB_HOST     = "127.0.0.1",
    NEMETON_DB_NAME     = "nemeton",
    NEMETON_DB_USER     = "nemeton",
    NEMETON_DB_PASSWORD = "secret"
  ))
  url <- nemetonshiny:::.resolve_monitoring_db_url(NULL)
  expect_match(url, "^postgresql://nemeton:secret@127\\.0\\.0\\.1:5432/nemeton$")
})

test_that(".resolve_monitoring_db_url returns a sqlite:// URL when project given and no PG env", {
  skip_if_not_installed("RSQLite")
  clear_monitoring_db_envvars()
  withr::with_tempdir({
    project <- list(id = "p1",
                    path = file.path(getwd(), "my_project"))
    dir.create(project$path, recursive = TRUE)

    url <- nemetonshiny:::.resolve_monitoring_db_url(project)
    expect_match(url, "^sqlite://")
    expect_match(url, "monitoring\\.sqlite$")
    # The parent <project>/data/ directory should have been created
    # as a side effect (so SQLite can later open the file).
    expect_true(dir.exists(file.path(project$path, "data")))
  })
})

test_that("NEMETON_DB_LOCAL=1 bypasses PG env vars and uses local SQLite", {
  # Regression: a user with Clever Cloud POSTGRESQL_ADDON_* sitting
  # in ~/.Renviron for production saw the app dial the prod Postgres
  # (and time out) even when running locally and wanting local mode.
  # NEMETON_DB_LOCAL=1 lets them keep the prod creds in env and
  # force local mode at runtime.
  skip_if_not_installed("RSQLite")
  clear_monitoring_db_envvars()
  withr::local_envvar(c(
    POSTGRESQL_ADDON_HOST     = "prod.example.com",
    POSTGRESQL_ADDON_DB       = "nemeton",
    POSTGRESQL_ADDON_USER     = "u",
    POSTGRESQL_ADDON_PASSWORD = "p",
    NEMETON_DB_LOCAL          = "1"
  ))
  withr::with_tempdir({
    project <- list(id = "p1", path = file.path(getwd(), "p"))
    dir.create(project$path)
    url <- nemetonshiny:::.resolve_monitoring_db_url(project)
    expect_match(url, "^sqlite://")
    expect_match(url, "monitoring\\.sqlite$")
    expect_false(grepl("^postgresql:", url))
  })
})

test_that(".nemeton_truthy recognises common truthy spellings", {
  for (v in c("1", "TRUE", "true", "YES", "yes", "on", "Y", "t")) {
    expect_true(nemetonshiny:::.nemeton_truthy(v), info = v)
  }
  for (v in c("", "0", "false", "no", "off", "n")) {
    expect_false(nemetonshiny:::.nemeton_truthy(v), info = paste0("'", v, "'"))
  }
})


test_that(".resolve_monitoring_db_url returns empty string when nothing usable", {
  clear_monitoring_db_envvars()
  expect_equal(
    nemetonshiny:::.resolve_monitoring_db_url(NULL),
    ""
  )
  # Project without path also yields empty
  expect_equal(
    nemetonshiny:::.resolve_monitoring_db_url(list(id = "no-path")),
    ""
  )
})

test_that("monitoring_db_backend classifies URLs correctly", {
  clear_monitoring_db_envvars()

  # Postgres URL
  withr::with_envvar(c(NEMETON_DB_URL = "postgresql://u:p@h/db"), {
    expect_equal(nemetonshiny:::monitoring_db_backend(NULL), "postgres")
  })

  # Local SQLite fallback
  if (requireNamespace("RSQLite", quietly = TRUE)) {
    withr::with_tempdir({
      project <- list(id = "p1", path = file.path(getwd(), "p"))
      dir.create(project$path)
      expect_equal(
        nemetonshiny:::monitoring_db_backend(project),
        "local"
      )
    })
  }

  # Nothing
  expect_equal(nemetonshiny:::monitoring_db_backend(NULL), "none")
})

test_that("get_monitoring_db_connection opens a local SQLite file when no PG env is set", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("nemeton")
  clear_monitoring_db_envvars()
  withr::with_tempdir({
    project <- list(id = "p1", path = file.path(getwd(), "proj"))
    dir.create(project$path)

    con <- nemetonshiny:::get_monitoring_db_connection(project = project)
    on.exit(nemetonshiny:::close_monitoring_db_connection(con), add = TRUE)

    expect_s4_class(con, "SQLiteConnection")
    expect_true(DBI::dbIsValid(con))
    expect_true(file.exists(file.path(project$path, "data",
                                      "monitoring.sqlite")))
  })
})

test_that("get_monitoring_db_connection prefers explicit db_url over project", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("nemeton")
  clear_monitoring_db_envvars()
  withr::with_tempdir({
    explicit <- file.path(getwd(), "explicit.sqlite")
    # Pass a project too, but db_url should win.
    project  <- list(id = "p1", path = file.path(getwd(), "proj"))
    dir.create(project$path)

    con <- nemetonshiny:::get_monitoring_db_connection(
      project = project,
      db_url  = paste0("sqlite://", normalizePath(explicit, winslash = "/",
                                                  mustWork = FALSE))
    )
    on.exit(nemetonshiny:::close_monitoring_db_connection(con), add = TRUE)

    expect_true(file.exists(explicit))
    # The project-derived path must NOT have been created (db_url won).
    expect_false(file.exists(file.path(project$path, "data",
                                       "monitoring.sqlite")))
  })
})

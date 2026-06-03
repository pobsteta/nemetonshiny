# Tests for the RAG corpus admin module (mod_rag_admin.R, spec 009.2).
#
# The business logic (manifest read/validate/write, corpus build,
# inventory) lives in `nemeton` and is mocked here so the tests exercise
# only the app-side wiring : manifest reactive, inline editing, live
# validation, save-blocking on errors, and the pure helpers.

# --- Pure helpers ----------------------------------------------------------

test_that("can_admin_rag gates on authentication and roles", {
  expect_false(nemetonshiny:::can_admin_rag(NULL))
  expect_false(nemetonshiny:::can_admin_rag(
    list(authenticated = FALSE, user_roles = "admin")))
  # Anonymous (authenticated, no roles) -> editor/admin fallback.
  expect_true(nemetonshiny:::can_admin_rag(
    list(authenticated = TRUE, user_roles = character(0))))
  expect_true(nemetonshiny:::can_admin_rag(
    list(authenticated = TRUE, user_roles = c("admin"))))
  expect_true(nemetonshiny:::can_admin_rag(
    list(authenticated = TRUE, user_roles = c("Proprietaire"))))
  # A non-privileged role must NOT grant admin.
  expect_false(nemetonshiny:::can_admin_rag(
    list(authenticated = TRUE, user_roles = c("lecteur"))))
})

test_that("rag_enum_values maps manifest columns to controlled vocab", {
  vocab <- list(
    langs      = c("fr", "en"),
    statuses   = c("active", "to_confirm"),
    strategies = c("pdf", "url"),
    doc_types  = c("guide", "report"),
    licenses   = c("CC-BY-4.0", "proprietary")
  )
  expect_identical(nemetonshiny:::rag_enum_values("lang", vocab), c("fr", "en"))
  expect_identical(nemetonshiny:::rag_enum_values("status", vocab),
                   c("active", "to_confirm"))
  expect_identical(nemetonshiny:::rag_enum_values("license", vocab),
                   c("CC-BY-4.0", "proprietary"))
  # Free-text columns and a NULL vocab both yield NULL.
  expect_null(nemetonshiny:::rag_enum_values("title", vocab))
  expect_null(nemetonshiny:::rag_enum_values("lang", NULL))
})

test_that("rag_translate_cols falls back to raw names for unknown cols", {
  i18n <- nemetonshiny:::get_i18n("fr")
  out <- nemetonshiny:::rag_translate_cols(c("doc_id", "title", "zzz_unknown"),
                                           i18n)
  expect_length(out, 3L)
  # Known keys are translated; the unknown one is returned verbatim.
  expect_identical(out[[3]], "zzz_unknown")
  expect_false(identical(out[[1]], "doc_id"))  # rag_col_doc_id exists
})

test_that("rag_color_actions wraps the action column in coloured badges", {
  df <- data.frame(
    doc_id = c("a", "b", "c", "d"),
    action = c("ingested", "skipped", "error", "planned"),
    stringsAsFactors = FALSE
  )
  out <- nemetonshiny:::rag_color_actions(df)
  expect_true(all(grepl("<span class=\"badge\"", out$action)))
  expect_true(any(grepl("#1B6B1B", out$action)))  # ingested -> green
  # No action column -> returned unchanged.
  df2 <- data.frame(x = 1:2)
  expect_identical(nemetonshiny:::rag_color_actions(df2), df2)
})

# --- UI --------------------------------------------------------------------

test_that("mod_rag_admin_ui renders the expected controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("bsicons")

  with_mocked_bindings(
    get_app_options = function() list(language = "fr"),
    {
      ui <- nemetonshiny:::mod_rag_admin_ui("rag_admin")
      html <- as.character(ui)
      expect_true(grepl("rag_admin-manifest", html))
      expect_true(grepl("rag_admin-issues", html))
      expect_true(grepl("rag_admin-report", html))
      expect_true(grepl("rag_admin-inventory", html))
      expect_true(grepl("rag_admin-add_row", html))
      expect_true(grepl("rag_admin-delete_doc", html))
      expect_true(grepl("rag_admin-import_csv", html))
      expect_true(grepl("rag_admin-export_csv", html))
    }
  )
})

# --- Server (testServer with nemeton mocked) -------------------------------

# A tiny manifest fixture (subset of the 16 canonical columns is fine for
# the wiring tests — the module never hard-codes the column set).
.rag_fake_manifest <- function() {
  data.frame(
    doc_id = c("doc_a", "doc_b"),
    title  = c("Doc A", "Doc B"),
    lang   = c("fr", "en"),
    status = c("active", "active"),
    stringsAsFactors = FALSE
  )
}

test_that("server exposes manifest, reacts to edits, blocks save on errors", {
  skip_if_not_installed("shiny")
  # local_mocked_bindings needs the bindings to exist in the nemeton NS.
  skip_if_not(exists("knowledge_manifest_path", where = asNamespace("nemeton"),
                     inherits = FALSE),
              "nemeton >= 0.63.0 (knowledge_* API) not installed")

  # `issues` is a mutable closure so a test step can flip validation from
  # "clean" to "one error row" and assert has_errors() reacts.
  issue_state <- new.env()
  issue_state$df <- data.frame(
    row = integer(0), doc_id = character(0), severity = character(0),
    field = character(0), message = character(0), stringsAsFactors = FALSE
  )
  saved <- new.env(); saved$called <- FALSE

  testthat::local_mocked_bindings(
    knowledge_manifest_path  = function(writable = TRUE) tempfile(fileext = ".csv"),
    read_knowledge_manifest  = function(path) .rag_fake_manifest(),
    knowledge_manifest_vocab = function() list(
      langs = c("fr", "en"), statuses = c("active", "to_confirm"),
      strategies = c("pdf", "url"), doc_types = c("guide"),
      licenses = c("CC-BY-4.0")
    ),
    validate_knowledge_manifest = function(manifest) issue_state$df,
    write_knowledge_manifest = function(manifest, path = NULL, validate = TRUE) {
      saved$called <- TRUE
      invisible(path)
    },
    .package = "nemeton"
  )

  mock_app_state <- shiny::reactiveValues(
    language = "fr",
    auth = list(authenticated = TRUE, user_roles = character(0))  # admin fallback
  )

  shiny::testServer(
    nemetonshiny:::mod_rag_admin_server,
    args = list(id = "rag_admin", app_state = mock_app_state,
                con = NULL, con_url = function() ""),
    {
      # Manifest loaded from the (mocked) writable path.
      expect_equal(nrow(session$returned$manifest()), 2L)
      expect_false(session$returned$has_errors())

      # Inline cell edit on the title column (col index 1 -> col=1 with
      # rownames=FALSE means names()[2] == "title").
      session$setInputs(manifest_cell_edit = list(
        row = 1L, col = 1L, value = "Doc A edited"
      ))
      expect_identical(session$returned$manifest()$title[[1]], "Doc A edited")

      # An invalid enum value is rejected (lang must be fr/en) -> manifest
      # unchanged for that cell.
      session$setInputs(manifest_cell_edit = list(
        row = 1L, col = 2L, value = "zz"
      ))
      expect_identical(session$returned$manifest()$lang[[1]], "fr")

      # Flip validation to an error -> has_errors() becomes TRUE and save
      # is refused (write_knowledge_manifest never called).
      issue_state$df <- data.frame(
        row = 1L, doc_id = "doc_a", severity = "error",
        field = "license", message = "missing license",
        stringsAsFactors = FALSE
      )
      # Force re-eval of the issues reactive by touching the manifest.
      session$setInputs(manifest_cell_edit = list(
        row = 2L, col = 0L, value = "doc_b2"
      ))
      expect_true(session$returned$has_errors())

      session$setInputs(save = 1L)
      expect_false(saved$called)
    }
  )
})

test_that("preview runs a dry-run build and stores the report", {
  skip_if_not_installed("shiny")
  skip_if_not(exists("build_knowledge_corpus", where = asNamespace("nemeton"),
                     inherits = FALSE),
              "nemeton >= 0.63.0 (knowledge_* API) not installed")

  fake_report <- data.frame(
    doc_id = c("doc_a"), action = "planned", reason = "dry_run",
    mode = "pdf", n_chunks = 0L, document_id = NA_character_,
    duration_sec = 0, stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    knowledge_manifest_path  = function(writable = TRUE) tempfile(fileext = ".csv"),
    read_knowledge_manifest  = function(path) .rag_fake_manifest(),
    knowledge_manifest_vocab = function() list(langs = c("fr", "en")),
    validate_knowledge_manifest = function(manifest) data.frame(
      row = integer(0), severity = character(0)),
    build_knowledge_corpus = function(con = NULL, manifest = NULL,
                                      dry_run = FALSE, ...) {
      stopifnot(is.null(con), isTRUE(dry_run))
      fake_report
    },
    .package = "nemeton"
  )

  mock_app_state <- shiny::reactiveValues(
    language = "fr",
    auth = list(authenticated = TRUE, user_roles = character(0))
  )

  shiny::testServer(
    nemetonshiny:::mod_rag_admin_server,
    args = list(id = "rag_admin", app_state = mock_app_state,
                con = NULL, con_url = function() ""),
    {
      session$setInputs(incl_tbc = FALSE)
      session$setInputs(preview = 1L)
      expect_equal(nrow(session$returned$report()), 1L)
      expect_identical(session$returned$report()$action[[1]], "planned")
    }
  )
})

test_that("importing a manifest CSV replaces the editable table", {
  skip_if_not_installed("shiny")
  skip_if_not(exists("read_knowledge_manifest", where = asNamespace("nemeton"),
                     inherits = FALSE),
              "nemeton >= 0.63.0 (knowledge_* API) not installed")

  imported <- data.frame(
    doc_id = c("x1", "x2", "x3"),
    title  = c("X1", "X2", "X3"),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    knowledge_manifest_path  = function(writable = TRUE) tempfile(fileext = ".csv"),
    # Seed with 2 rows; the uploaded file parses to 3 rows.
    read_knowledge_manifest  = function(path) {
      if (grepl("uploaded", path)) imported else .rag_fake_manifest()
    },
    knowledge_manifest_vocab = function() list(langs = c("fr", "en")),
    validate_knowledge_manifest = function(manifest) data.frame(
      row = integer(0), severity = character(0)),
    .package = "nemeton"
  )

  mock_app_state <- shiny::reactiveValues(
    language = "fr",
    auth = list(authenticated = TRUE, user_roles = character(0))
  )

  shiny::testServer(
    nemetonshiny:::mod_rag_admin_server,
    args = list(id = "rag_admin", app_state = mock_app_state,
                con = NULL, con_url = function() ""),
    {
      expect_equal(nrow(session$returned$manifest()), 2L)   # seeded
      session$setInputs(import_csv = list(
        name = "uploaded.csv", size = 10L, type = "text/csv",
        datapath = file.path(tempdir(), "uploaded.csv")
      ))
      expect_equal(nrow(session$returned$manifest()), 3L)   # imported
      expect_identical(session$returned$manifest()$doc_id, c("x1", "x2", "x3"))
    }
  )
})

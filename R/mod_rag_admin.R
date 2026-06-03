#' RAG corpus administration module (spec 009.2 — consumes nemeton >= 0.63.0)
#'
#' @description
#' Admin tab to curate the knowledge corpus that feeds the sourced AI
#' perspectives (E7). Lets an administrator :
#'   1. edit the writable manifest (CSV) — add / edit / delete documents ;
#'   2. validate the manifest live (D5 licence guardrails) ;
#'   3. import the corpus into the database (embeddings) asynchronously,
#'      with progress + report ;
#'   4. inspect the database inventory and delete a document.
#'
#' All business logic lives in `nemeton` (CLAUDE.md §1) ; this module only
#' wires the exported functions to the UI. Every string goes through i18n
#' (CLAUDE.md §3).
#'
#' @name mod_rag_admin
#' @keywords internal
NULL


# ---------------------------------------------------------------------------
# Helpers (file-private)
# ---------------------------------------------------------------------------

#' Can the current auth state administer the RAG corpus?
#'
#' Mirrors `can_edit_action_plan()` but restricts the privileged role
#' set : only `admin` / `owner` / `proprietaire` may curate the corpus.
#' Anonymous installs (no OAuth provider configured → empty
#' `user_roles`) fall back to TRUE so single-user / demo setups keep
#' working without an identity provider.
#'
#' @param auth_state reactiveValues / list from `mod_auth_server`, or NULL.
#' @return Logical (length 1).
#' @noRd
can_admin_rag <- function(auth_state) {
  if (is.null(auth_state)) return(FALSE)
  authenticated <- tryCatch(isTRUE(auth_state[["authenticated"]]),
                            error = function(e) FALSE)
  if (!authenticated) return(FALSE)
  roles <- tryCatch(auth_state[["user_roles"]] %||% character(),
                    error = function(e) character())
  # Anonymous fallback: no provider → no roles → editor/admin by default.
  if (length(roles) == 0L) return(TRUE)
  any(c("proprietaire", "owner", "admin") %in% tolower(roles))
}


#' Allowed enum values for a manifest column (or NULL for free text)
#'
#' @param col Character. Manifest column name.
#' @param vocab List returned by `nemeton::knowledge_manifest_vocab()`.
#' @return Character vector of allowed values, or NULL.
#' @noRd
rag_enum_values <- function(col, vocab) {
  if (is.null(vocab)) return(NULL)
  switch(col,
    lang            = vocab$langs,
    status          = vocab$statuses,
    ingest_strategy = vocab$strategies,
    doc_type        = vocab$doc_types,
    license         = vocab$licenses,
    NULL
  )
}


#' Translate a vector of column names through the i18n dictionary
#'
#' Falls back to the raw name when no `rag_col_<name>` key exists.
#'
#' @param cols Character vector of column names.
#' @param i18n i18n translator object.
#' @return Character vector of (translated) labels.
#' @noRd
rag_translate_cols <- function(cols, i18n) {
  vapply(cols, function(cn) {
    key <- paste0("rag_col_", cn)
    if (isTRUE(i18n$has(key))) i18n$t(key) else cn
  }, character(1), USE.NAMES = FALSE)
}


#' Render the `action` column of an import report as coloured badges
#'
#' @param df data.frame returned by `build_knowledge_corpus()`.
#' @return The same data.frame with an HTML `action` column (escape=FALSE).
#' @noRd
rag_color_actions <- function(df) {
  if (is.null(df) || !"action" %in% names(df)) return(df)
  pal <- c(ingested = "#1B6B1B", planned = "#0d6efd",
           skipped = "#6c757d", error = "#b00020")
  df$action <- vapply(as.character(df$action), function(a) {
    col <- pal[[a]] %||% "#6c757d"
    sprintf(
      '<span class="badge" style="background-color:%s;color:#fff;">%s</span>',
      col, a
    )
  }, character(1))
  df
}


# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

#' @param id Module id.
#' @noRd
mod_rag_admin_ui <- function(id) {
  ns <- shiny::NS(id)
  opts <- get_app_options()
  i18n <- get_i18n(opts$language)

  bslib::card(
    bslib::card_header(
      class = "d-flex align-items-center gap-2",
      bsicons::bs_icon("book"),
      i18n$t("rag_tab_title")
    ),
    bslib::card_body(
      # Admin gate + API-key notices (rendered server-side).
      shiny::uiOutput(ns("auth_notice")),
      shiny::uiOutput(ns("key_notice")),

      # ---- Manifest ----------------------------------------------------
      shiny::h5(i18n$t("rag_manifest_section")),
      shiny::div(
        class = "d-flex gap-2 mb-2",
        shiny::actionButton(ns("add_row"), i18n$t("rag_btn_add_row"),
                            icon = shiny::icon("plus"),
                            class = "btn-outline-secondary btn-sm"),
        shiny::actionButton(ns("delete_row"), i18n$t("rag_btn_delete_row"),
                            icon = shiny::icon("trash"),
                            class = "btn-outline-secondary btn-sm")
      ),
      DT::DTOutput(ns("manifest")),

      shiny::div(
        class = "d-flex gap-2 my-3 flex-wrap align-items-center",
        shiny::uiOutput(ns("actions"), inline = TRUE),
        shiny::checkboxInput(ns("incl_tbc"), i18n$t("rag_opt_include_tbc"),
                             value = FALSE, width = "auto"),
        shiny::checkboxInput(ns("fresh"), i18n$t("rag_opt_fresh"),
                             value = FALSE, width = "auto")
      ),

      # ---- Validation --------------------------------------------------
      shiny::h5(i18n$t("rag_validation_section")),
      DT::DTOutput(ns("issues")),

      # ---- Progress ----------------------------------------------------
      shiny::uiOutput(ns("progress")),

      # ---- Report ------------------------------------------------------
      shiny::h5(i18n$t("rag_report_section")),
      DT::DTOutput(ns("report")),

      # ---- Inventory ---------------------------------------------------
      shiny::div(
        class = "d-flex align-items-center justify-content-between mt-3",
        shiny::h5(i18n$t("rag_inventory_section"), class = "mb-0"),
        shiny::div(
          class = "d-flex gap-2",
          shiny::actionButton(ns("refresh_inventory"), i18n$t("rag_btn_refresh"),
                              icon = shiny::icon("rotate"),
                              class = "btn-outline-secondary btn-sm"),
          shiny::actionButton(ns("delete_doc"), i18n$t("rag_btn_delete"),
                              icon = shiny::icon("trash"),
                              class = "btn-outline-danger btn-sm")
        )
      ),
      DT::DTOutput(ns("inventory"))
    )
  )
}


# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

#' @param id Module id.
#' @param app_state Shared `reactiveValues` (reads `language`, `auth`).
#'   May be NULL (defaults : French, anonymous-admin).
#' @param con Live `DBIConnection` (or a `reactive()`/function returning
#'   one) used for main-session inventory reads / deletes. NULL → the
#'   module opens its own connection from `con_url`.
#' @param con_url DB URL string (or a `reactive()`/function returning one).
#'   NOT a live connection : the async worker opens its own connection.
#'   NULL → resolved from `NEMETON_KNOWLEDGE_DB_URL` / `NEMETON_DB_URL`.
#' @noRd
mod_rag_admin_server <- function(id, app_state = NULL, con = NULL,
                                 con_url = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- small resolvers --------------------------------------------------
    lang <- function() {
      l <- tryCatch(app_state[["language"]], error = function(e) NULL)
      get_i18n(l %||% "fr")
    }
    auth <- function() tryCatch(app_state[["auth"]], error = function(e) NULL)
    is_admin <- function() can_admin_rag(auth())

    resolve_con <- function() {
      if (is.null(con)) return(NULL)
      if (is.function(con)) return(tryCatch(con(), error = function(e) NULL))
      con
    }
    resolve_con_url <- function() {
      if (is.null(con_url)) {
        return(Sys.getenv("NEMETON_KNOWLEDGE_DB_URL",
                          Sys.getenv("NEMETON_DB_URL", "")))
      }
      val <- if (is.function(con_url)) {
        tryCatch(con_url(), error = function(e) "")
      } else {
        con_url
      }
      val %||% ""
    }
    resolve_api_key <- function() {
      k <- Sys.getenv("NEMETON_MISTRAL_API_KEY", "")
      if (!nzchar(k)) k <- Sys.getenv("MISTRAL_API_KEY", "")
      k
    }
    have_key <- function() nzchar(resolve_api_key())

    # ---- manifest state ---------------------------------------------------
    path <- tryCatch(nemeton::knowledge_manifest_path(writable = TRUE),
                     error = function(e) NA_character_)
    man <- shiny::reactiveVal(
      tryCatch(
        if (!is.na(path)) nemeton::read_knowledge_manifest(path)
        else data.frame(),
        error = function(e) data.frame()
      )
    )
    vocab <- tryCatch(nemeton::knowledge_manifest_vocab(),
                      error = function(e) NULL)

    issues <- shiny::reactive(
      tryCatch(nemeton::validate_knowledge_manifest(man()),
               error = function(e) data.frame())
    )
    has_errors <- shiny::reactive({
      df <- issues()
      !is.null(df) && nrow(df) > 0L &&
        "severity" %in% names(df) && any(df$severity == "error")
    })

    report <- shiny::reactiveVal(NULL)

    # ---- notices ----------------------------------------------------------
    output$auth_notice <- shiny::renderUI({
      if (is_admin()) return(NULL)
      htmltools::div(class = "alert alert-danger", role = "alert",
                     lang()$t("rag_unauthorized"))
    })
    output$key_notice <- shiny::renderUI({
      if (have_key()) return(NULL)
      i18n <- lang()
      htmltools::div(
        class = "alert alert-warning", role = "alert",
        i18n$t("rag_no_api_key"), " ",
        htmltools::tags$a(
          href = "https://console.mistral.ai/api-keys",
          target = "_blank", rel = "noopener",
          i18n$t("rag_api_key_console")
        )
      )
    })

    # ---- action buttons (disabled while errors / no key / not admin) ------
    output$actions <- shiny::renderUI({
      i18n  <- lang()
      admin <- is_admin()
      errs  <- has_errors()
      key   <- have_key()
      shiny::tagList(
        shiny::actionButton(ns("save"), i18n$t("rag_btn_save"),
                            icon = shiny::icon("floppy-disk"),
                            class = "btn-outline-primary",
                            disabled = (!admin || errs)),
        shiny::actionButton(ns("preview"), i18n$t("rag_btn_preview"),
                            icon = shiny::icon("eye"),
                            class = "btn-outline-secondary",
                            disabled = (!admin || errs)),
        shiny::actionButton(ns("import"), i18n$t("rag_btn_import"),
                            icon = shiny::icon("upload"),
                            class = "btn-primary",
                            disabled = (!admin || errs || !key))
      )
    })

    # ---- manifest table (render once; mutate via proxy) -------------------
    proxy <- DT::dataTableProxy("manifest")
    output$manifest <- DT::renderDT({
      i18n <- shiny::isolate(lang())
      df   <- shiny::isolate(man())
      DT::datatable(
        df,
        editable  = "cell",
        rownames  = FALSE,
        selection = "single",
        colnames  = rag_translate_cols(names(df), i18n),
        options   = list(scrollX = TRUE, pageLength = 10, dom = "tp")
      )
    })

    # Apply inline cell edits back into the reactive manifest, enforcing
    # the controlled vocabulary on enum columns (revert + notify on a
    # bad value). validate_knowledge_manifest() also flags these, but
    # catching them here gives instant feedback.
    shiny::observeEvent(input$manifest_cell_edit, {
      info    <- input$manifest_cell_edit
      df      <- man()
      col_idx <- info$col + 1L            # rownames = FALSE -> +1
      if (col_idx < 1L || col_idx > ncol(df)) return()
      col_nm  <- names(df)[col_idx]
      new_val <- as.character(info$value)

      allowed <- rag_enum_values(col_nm, vocab)
      if (!is.null(allowed) && nzchar(new_val) && !(new_val %in% allowed)) {
        shiny::showNotification(
          sprintf(lang()$t("rag_enum_invalid"), col_nm,
                  paste(allowed, collapse = ", ")),
          type = "error"
        )
        DT::replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE)
        return()
      }
      df[info$row, col_idx] <- new_val
      man(df)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$add_row, {
      df <- man()
      if (ncol(df) == 0L) return()
      empty <- as.data.frame(
        as.list(stats::setNames(rep("", ncol(df)), names(df))),
        stringsAsFactors = FALSE
      )
      df2 <- rbind(df, empty)
      man(df2)
      DT::replaceData(proxy, df2, resetPaging = FALSE, rownames = FALSE)
    })

    shiny::observeEvent(input$delete_row, {
      sel <- input$manifest_rows_selected
      if (is.null(sel) || !length(sel)) {
        shiny::showNotification(lang()$t("rag_delete_select_first"),
                                type = "warning")
        return()
      }
      df <- man()[-sel, , drop = FALSE]
      man(df)
      DT::replaceData(proxy, df, resetPaging = FALSE, rownames = FALSE)
    })

    # ---- validation panel -------------------------------------------------
    output$issues <- DT::renderDT({
      i18n <- lang()
      df   <- issues()
      if (is.null(df) || !nrow(df)) df <- data.frame()
      dt <- DT::datatable(
        df, rownames = FALSE,
        colnames = rag_translate_cols(names(df), i18n),
        options  = list(pageLength = 5, dom = "tp")
      )
      if ("severity" %in% names(df) && nrow(df)) {
        dt <- DT::formatStyle(
          dt, "severity",
          color = DT::styleEqual(c("error", "warning"),
                                 c("#b00020", "#b8860b")),
          fontWeight = "bold"
        )
      }
      dt
    })

    # ---- save -------------------------------------------------------------
    shiny::observeEvent(input$save, {
      i18n <- lang()
      if (!is_admin()) {
        shiny::showNotification(i18n$t("rag_unauthorized"), type = "error")
        return()
      }
      if (is.na(path)) {
        shiny::showNotification(i18n$t("rag_no_manifest"), type = "error")
        return()
      }
      # Guard server-side too (the button is also disabled in the UI):
      # never persist a manifest that still has blocking `error` rows.
      if (has_errors()) {
        shiny::showNotification(i18n$t("rag_save_blocked"), type = "error")
        return()
      }
      tryCatch({
        nemeton::write_knowledge_manifest(man(), path = path)
        shiny::showNotification(i18n$t("rag_saved_ok"), type = "message")
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n$t("rag_save_blocked"), conditionMessage(e)),
          type = "error"
        )
      })
    })

    # ---- preview (dry-run : no DB, no API) --------------------------------
    shiny::observeEvent(input$preview, {
      i18n <- lang()
      out <- tryCatch(
        nemeton::build_knowledge_corpus(
          con = NULL, manifest = man(), dry_run = TRUE,
          include_to_confirm = isTRUE(input$incl_tbc)
        ),
        error = function(e) {
          shiny::showNotification(
            paste(i18n$t("rag_import_error"), conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )
      if (!is.null(out)) {
        report(out)
        shiny::showNotification(i18n$t("rag_preview_done"), type = "message")
      }
    })

    # ---- async import -----------------------------------------------------
    # Heartbeat file : the worker writes its progress here, the main
    # session polls it via invalidateLater (a `future` worker cannot
    # write a main-process reactiveVal — pièges techniques §4).
    hb_file <- tempfile(fileext = ".rds")

    task <- shiny::ExtendedTask$new(
      function(url, df, provider, key, incl, fr, hb) {
        if (requireNamespace("future", quietly = TRUE)) {
          plan_classes <- class(future::plan())
          is_parallel <- any(c("multisession", "multicore", "cluster") %in%
                               plan_classes)
          if (!is_parallel) future::plan("multisession")
        }
        promises::future_promise({
          if (!nzchar(url)) stop("no_db_url")
          # OPEN A NEW CONNECTION INSIDE THE WORKER — a DBI connection
          # is not shareable across processes (pièges techniques §1).
          con_w <- nemeton::db_connect(url)
          on.exit(try(nemeton::db_disconnect(con_w), silent = TRUE),
                  add = TRUE)
          nemeton::build_knowledge_corpus(
            con_w, manifest = df, provider = provider, api_key = key,
            include_to_confirm = incl, fresh = fr,
            progress = function(i, n, row, rr) {
              try(saveRDS(list(i = i, n = n), hb), silent = TRUE)
            }
          )
        }, seed = TRUE)
      }
    )

    do_import <- function() {
      i18n <- lang()
      url <- resolve_con_url()
      if (!nzchar(url)) {
        shiny::showNotification(i18n$t("rag_no_db"), type = "error")
        return()
      }
      key <- resolve_api_key()
      if (!nzchar(key)) {
        shiny::showNotification(i18n$t("rag_no_api_key"), type = "error")
        return()
      }
      if (file.exists(hb_file)) try(unlink(hb_file), silent = TRUE)
      task$invoke(url, man(), "mistral", key,
                  isTRUE(input$incl_tbc), isTRUE(input$fresh), hb_file)
    }

    shiny::observeEvent(input$import, {
      if (!is_admin()) {
        shiny::showNotification(lang()$t("rag_unauthorized"), type = "error")
        return()
      }
      if (has_errors()) {
        shiny::showNotification(lang()$t("rag_save_blocked"), type = "error")
        return()
      }
      if (isTRUE(input$fresh)) {
        # Destructive rebuild → confirmation modal first.
        i18n <- lang()
        shiny::showModal(shiny::modalDialog(
          title = i18n$t("rag_fresh_confirm_title"),
          i18n$t("rag_fresh_confirm"),
          easyClose = TRUE,
          footer = shiny::tagList(
            shiny::modalButton(i18n$t("rag_fresh_confirm_cancel")),
            shiny::actionButton(ns("fresh_confirm_yes"),
                                i18n$t("rag_fresh_confirm_yes"),
                                class = "btn-danger")
          )
        ))
      } else {
        do_import()
      }
    })

    shiny::observeEvent(input$fresh_confirm_yes, {
      shiny::removeModal()
      do_import()
    })

    # Poll the heartbeat for progress while the task runs.
    output$progress <- shiny::renderUI({
      i18n <- lang()
      st <- task$status()
      if (!identical(st, "running")) {
        return(htmltools::tags$em(class = "text-muted",
                                  i18n$t("rag_progress_idle")))
      }
      shiny::invalidateLater(500, session)
      hb <- tryCatch(readRDS(hb_file), error = function(e) NULL)
      if (is.null(hb)) {
        return(htmltools::tags$em(i18n$t("rag_progress")))
      }
      htmltools::tagList(
        htmltools::tags$span(
          sprintf("%s %d / %d", i18n$t("rag_progress"), hb$i, hb$n)
        ),
        htmltools::tags$progress(value = hb$i, max = hb$n,
                                 class = "w-100 d-block")
      )
    })

    shiny::observeEvent(task$status(), {
      st <- task$status()
      i18n <- lang()
      if (identical(st, "success")) {
        res <- tryCatch(task$result(), error = function(e) NULL)
        if (!is.null(res)) report(res)
        if (file.exists(hb_file)) try(unlink(hb_file), silent = TRUE)
        shiny::showNotification(i18n$t("rag_import_done"), type = "message")
        inv_refresh(inv_refresh() + 1L)
      } else if (identical(st, "error")) {
        msg <- tryCatch(task$result(), error = function(e) conditionMessage(e))
        shiny::showNotification(
          paste(i18n$t("rag_import_error"), msg), type = "error"
        )
      }
    }, ignoreInit = TRUE)

    output$report <- DT::renderDT({
      i18n <- lang()
      rep <- report()
      shiny::req(rep)
      DT::datatable(
        rag_color_actions(rep), rownames = FALSE, escape = FALSE,
        colnames = rag_translate_cols(names(rep), i18n),
        options  = list(pageLength = 10, dom = "tp")
      )
    })

    # ---- inventory + delete (main-session reads) --------------------------
    # The module owns at most one main-session knowledge connection. When
    # the caller supplies a live `con`, we reuse it (and never close it) ;
    # otherwise we open our own from the URL and close it on session end.
    kb <- new.env(parent = emptyenv())
    kb$con <- NULL
    kb$owned <- FALSE
    get_kb_con <- function() {
      valid <- !is.null(kb$con) &&
        tryCatch(DBI::dbIsValid(kb$con), error = function(e) FALSE)
      if (valid) return(kb$con)
      passed <- resolve_con()
      if (!is.null(passed)) {
        kb$con <- passed; kb$owned <- FALSE
        return(passed)
      }
      url <- resolve_con_url()
      if (!nzchar(url)) return(NULL)
      opened <- tryCatch(nemeton::db_connect(url), error = function(e) NULL)
      kb$con <- opened; kb$owned <- !is.null(opened)
      opened
    }

    inv_refresh <- shiny::reactiveVal(0L)
    inventory_docs <- shiny::reactive({
      inv_refresh()
      con_live <- get_kb_con()
      if (is.null(con_live)) return(NULL)
      tryCatch(nemeton::list_knowledge_documents(con_live),
               error = function(e) NULL)
    })

    output$inventory <- DT::renderDT({
      i18n <- lang()
      docs <- inventory_docs()
      if (is.null(docs) || !nrow(docs)) docs <- data.frame()
      DT::datatable(
        docs, rownames = FALSE, selection = "single",
        colnames = rag_translate_cols(names(docs), i18n),
        options  = list(pageLength = 10, dom = "tp",
                        language = list(emptyTable = i18n$t("rag_inventory_empty")))
      )
    })

    shiny::observeEvent(input$refresh_inventory, {
      inv_refresh(inv_refresh() + 1L)
    })

    shiny::observeEvent(input$delete_doc, {
      i18n <- lang()
      if (!is_admin()) {
        shiny::showNotification(i18n$t("rag_unauthorized"), type = "error")
        return()
      }
      docs <- inventory_docs()
      sel  <- input$inventory_rows_selected
      if (is.null(docs) || is.null(sel) || !length(sel)) {
        shiny::showNotification(i18n$t("rag_delete_select_first"),
                                type = "warning")
        return()
      }
      doc_id <- docs[["document_id"]][sel] %||% docs[["doc_id"]][sel]
      con_live <- get_kb_con()
      if (is.null(con_live)) {
        shiny::showNotification(i18n$t("rag_no_db"), type = "error")
        return()
      }
      ok <- tryCatch({
        nemeton::delete_knowledge_document(con_live, doc_id)
        TRUE
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n$t("rag_import_error"), conditionMessage(e)),
          type = "error"
        )
        FALSE
      })
      if (isTRUE(ok)) {
        shiny::showNotification(i18n$t("rag_delete_done"), type = "message")
        inv_refresh(inv_refresh() + 1L)
      }
    })

    session$onSessionEnded(function() {
      if (isTRUE(kb$owned) && !is.null(kb$con)) {
        try(nemeton::db_disconnect(kb$con), silent = TRUE)
      }
      if (file.exists(hb_file)) try(unlink(hb_file), silent = TRUE)
    })

    # Expose a few reactives for testServer().
    list(
      manifest   = man,
      issues     = issues,
      has_errors = has_errors,
      report     = report
    )
  })
}

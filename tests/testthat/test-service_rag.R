# ============================================================
# v0.56.0 — RAG « perspectives IA sourcées »
# ============================================================
# Brief 2026-06-02 (BRIEF-nemetonshiny-rag.md côté nemeton). On
# teste le service d'orchestration mince `R/service_rag.R` et le
# wiring dans `mod_synthesis_server()` côté UI.

# ---- Helpers : data.frame chunks fixture (shape nemeton::retrieve_knowledge) ----

.fake_chunks_df <- function(rows) {
  data.frame(
    chunk_id    = sprintf("c%d", seq_len(rows)),
    document_id = sprintf("d%d", seq_len(rows)),
    title       = sprintf("Doc %d", seq_len(rows)),
    author      = sprintf("Auteur %d", seq_len(rows)),
    pub_date    = as.Date("2024-01-01") + seq_len(rows),
    source_url  = sprintf("https://example.org/doc%d", seq_len(rows)),
    lang        = "fr",
    chunk_index = seq_len(rows),
    page_number = seq_len(rows),
    text        = sprintf("Extrait pertinent numéro %d.", seq_len(rows)),
    similarity  = seq(0.95, by = -0.05, length.out = rows),
    stringsAsFactors = FALSE
  )
}


# ---- rag_profile_code : mapping app key → corpus short code -----------

test_that("rag_profile_code strips the profil_ prefix", {
  expect_equal(nemetonshiny:::rag_profile_code("profil_naturaliste"),
               "naturaliste")
  expect_equal(nemetonshiny:::rag_profile_code("profil_gestionnaire_onf"),
               "gestionnaire_onf")
})

test_that("rag_profile_code returns short codes unchanged", {
  expect_equal(nemetonshiny:::rag_profile_code("naturaliste"),
               "naturaliste")
  expect_equal(nemetonshiny:::rag_profile_code("generalist"),
               "generalist")
})

test_that("rag_profile_code handles NULL/empty input gracefully", {
  expect_null(nemetonshiny:::rag_profile_code(NULL))
  expect_null(nemetonshiny:::rag_profile_code(""))
  expect_null(nemetonshiny:::rag_profile_code(NA_character_))
  expect_null(nemetonshiny:::rag_profile_code(character(0)))
})


# ---- build_situation_summary : phrase courte non-vide --------------------

test_that("build_situation_summary returns a non-empty FR/EN sentence", {
  fr <- nemetonshiny:::build_situation_summary(
    units = NULL, profile_key = "profil_naturaliste", lang = "fr")
  expect_true(nzchar(fr))
  expect_match(fr, "naturaliste")
  en <- nemetonshiny:::build_situation_summary(
    units = NULL, profile_key = "profil_naturaliste", lang = "en")
  expect_true(nzchar(en))
  expect_match(en, "naturaliste")
})

test_that("build_situation_summary falls back to 'generaliste' when key is NULL", {
  txt <- nemetonshiny:::build_situation_summary(units = NULL,
                                                profile_key = NULL,
                                                lang = "fr")
  expect_match(txt, "generaliste")
})


# ---- rag_context : nominal — retrieve_knowledge renvoie 3 chunks -----------

test_that("rag_context returns chunks + numbered prompt block + sources_md", {
  skip_if_not_installed("nemeton")
  fake_chunks <- .fake_chunks_df(3)

  testthat::with_mocked_bindings(
    retrieve_knowledge = function(con, query, ...) {
      fake_chunks
    },
    format_citations = function(retrieved_chunks, format = "markdown",
                                lang = "fr") {
      sprintf("## Sources\n- %s\n",
              paste(retrieved_chunks$title, collapse = "\n- "))
    },
    .package = "nemeton",
    {
      ctx <- nemetonshiny:::rag_context(
        app_con        = "fake-con",
        profile_code   = "naturaliste",
        family_codes   = NULL,
        situation_text = "Forêt d'épicéas avec dépérissement."
      )
      expect_equal(nrow(ctx$chunks), 3L)
      expect_true(nzchar(ctx$prompt_block))
      # Numérotation [^1] [^2] [^3] présente.
      expect_true(grepl("\\[\\^1\\]", ctx$prompt_block))
      expect_true(grepl("\\[\\^2\\]", ctx$prompt_block))
      expect_true(grepl("\\[\\^3\\]", ctx$prompt_block))
      # Titre du bloc en FR par défaut.
      expect_true(grepl("Documents de référence", ctx$prompt_block))
      # Bloc sources non-vide.
      expect_true(nzchar(ctx$sources_md))
      # 3 documents distincts (fake_chunks_df utilise document_id unique).
      expect_equal(ctx$n_sources, 3L)
    }
  )
})


# ---- rag_context : dédup par document_id (cas du brief §4.2) -----------

test_that("rag_context déduplique le bloc Sources par document_id", {
  skip_if_not_installed("nemeton")
  # 3 chunks issus du MÊME document → 1 seule citation attendue
  # dans le bloc Sources, mais 3 chunks dans le prompt_block.
  dup_chunks <- .fake_chunks_df(3)
  dup_chunks$document_id <- "spec_008"
  dup_chunks$title       <- "Spec 008 — Suivi sanitaire"

  captured <- list()
  testthat::with_mocked_bindings(
    retrieve_knowledge = function(...) dup_chunks,
    format_citations = function(retrieved_chunks, ...) {
      captured$n_rows <<- nrow(retrieved_chunks)
      "## Sources\n- Spec 008\n"
    },
    .package = "nemeton",
    {
      ctx <- nemetonshiny:::rag_context(
        app_con        = "fake-con",
        profile_code   = "naturaliste",
        family_codes   = NULL,
        situation_text = "Dépérissement"
      )
      # format_citations a reçu UNE seule ligne (dédupliquée).
      expect_equal(captured$n_rows, 1L)
      expect_equal(ctx$n_sources, 1L)
      # v0.84.6 — numérotation par DOCUMENT, cohérente prompt ↔ sources :
      # 3 chunks d'1 seul document → le prompt les regroupe sous [^1]
      # (PAS de [^2]/[^3] orphelins que le LLM citerait sans définition).
      expect_true(grepl("\\[\\^1\\]", ctx$prompt_block))
      expect_false(grepl("\\[\\^2\\]", ctx$prompt_block))
      expect_false(grepl("\\[\\^3\\]", ctx$prompt_block))
    }
  )
})


# ---- rag_context : numérotation cohérente prompt ↔ sources (v0.84.6) ----

test_that("rag_context numérote prompt et sources à l'identique (par document)", {
  skip_if_not_installed("nemeton")
  # 5 chunks issus de 3 documents (A:3, B:1, C:1).
  chunks <- data.frame(
    document_id = c("A", "A", "A", "B", "C"),
    text        = c("a1", "a2", "a3", "b1", "c1"),
    title       = c("Doc A", "Doc A", "Doc A", "Doc B", "Doc C"),
    stringsAsFactors = FALSE
  )
  testthat::with_mocked_bindings(
    retrieve_knowledge = function(...) chunks,
    format_citations = function(retrieved_chunks, ...) {
      paste0("## Sources

", paste(
        sprintf("[^%d] %s.", seq_len(nrow(retrieved_chunks)),
                retrieved_chunks$title), collapse = "

"))
    },
    .package = "nemeton",
    {
      ctx <- nemetonshiny:::rag_context(
        app_con = "fake", profile_code = "x", family_codes = NULL,
        situation_text = "abc")
      max_num <- function(s) max(as.integer(gsub("\\D", "",
        regmatches(s, gregexpr("\\[\\^[0-9]+\\]", s))[[1]])))
      # 3 documents uniques → prompt ET sources s'arrêtent à [^3].
      expect_equal(ctx$n_sources, 3L)
      expect_equal(max_num(ctx$prompt_block), 3L)
      expect_equal(max_num(ctx$sources_md), 3L)
      # Les 3 chunks du doc A sont regroupés sous [^1] (un seul).
      expect_equal(lengths(regmatches(ctx$prompt_block,
        gregexpr("\\[\\^1\\]", ctx$prompt_block)))[1], 1L)
    }
  )
})


# ---- rag_context : dégradation gracieuse ---------------------------------

test_that("rag_context returns empty payload when retrieve_knowledge throws", {
  skip_if_not_installed("nemeton")
  testthat::with_mocked_bindings(
    retrieve_knowledge = function(...) {
      stop("network unreachable")
    },
    .package = "nemeton",
    {
      ctx <- nemetonshiny:::rag_context(
        app_con        = "fake-con",
        profile_code   = "naturaliste",
        family_codes   = NULL,
        situation_text = "anything"
      )
      expect_null(ctx$chunks)
      expect_equal(ctx$prompt_block, "")
      expect_equal(ctx$sources_md, "")
      expect_equal(ctx$n_sources, 0L)
    }
  )
})

test_that("rag_context returns empty when retrieve_knowledge yields 0 rows", {
  skip_if_not_installed("nemeton")
  testthat::with_mocked_bindings(
    retrieve_knowledge = function(...) {
      data.frame(chunk_id = character(0), document_id = character(0),
                 title = character(0), text = character(0),
                 similarity = numeric(0),
                 stringsAsFactors = FALSE)
    },
    .package = "nemeton",
    {
      ctx <- nemetonshiny:::rag_context(
        app_con        = "fake-con",
        profile_code   = "naturaliste",
        family_codes   = NULL,
        situation_text = "anything"
      )
      expect_null(ctx$chunks)
      expect_equal(ctx$n_sources, 0L)
    }
  )
})

test_that("rag_context honors options(nemeton.rag_enabled = FALSE)", {
  skip_if_not_installed("nemeton")
  withr::local_options(list(nemeton.rag_enabled = FALSE))
  # Mock pour vérifier que `retrieve_knowledge` n'est PAS appelé.
  call_count <- 0L
  testthat::with_mocked_bindings(
    retrieve_knowledge = function(...) {
      call_count <<- call_count + 1L
      .fake_chunks_df(1)
    },
    .package = "nemeton",
    {
      ctx <- nemetonshiny:::rag_context(
        app_con        = "fake-con",
        profile_code   = "naturaliste",
        family_codes   = NULL,
        situation_text = "anything"
      )
      expect_equal(call_count, 0L)  # opt-out → retrieve_knowledge JAMAIS appelé
      expect_equal(ctx$n_sources, 0L)
    }
  )
})

test_that("rag_context returns empty when situation_text is empty/NULL", {
  skip_if_not_installed("nemeton")
  call_count <- 0L
  testthat::with_mocked_bindings(
    retrieve_knowledge = function(...) {
      call_count <<- call_count + 1L
      .fake_chunks_df(1)
    },
    .package = "nemeton",
    {
      ctx_null <- nemetonshiny:::rag_context(
        app_con = "fake-con", profile_code = "naturaliste",
        family_codes = NULL, situation_text = NULL
      )
      ctx_empty <- nemetonshiny:::rag_context(
        app_con = "fake-con", profile_code = "naturaliste",
        family_codes = NULL, situation_text = ""
      )
      expect_equal(call_count, 0L)
      expect_equal(ctx_null$n_sources, 0L)
      expect_equal(ctx_empty$n_sources, 0L)
    }
  )
})

test_that("rag_context returns empty when app_con is NULL and no env URL", {
  skip_if_not_installed("nemeton")
  withr::with_envvar(c(NEMETON_KNOWLEDGE_DB_URL = ""), {
    call_count <- 0L
    testthat::with_mocked_bindings(
      retrieve_knowledge = function(...) {
        call_count <<- call_count + 1L
        .fake_chunks_df(1)
      },
      .package = "nemeton",
      {
        ctx <- nemetonshiny:::rag_context(
          app_con        = NULL,
          profile_code   = "naturaliste",
          family_codes   = NULL,
          situation_text = "anything"
        )
        expect_equal(call_count, 0L)
        expect_equal(ctx$n_sources, 0L)
      }
    )
  })
})


# ---- i18n : clés rag_* ----------------------------------------------------

test_that("i18n rag_sourced_badge accepts an integer argument (FR/EN)", {
  i18n_fr <- nemetonshiny:::get_i18n("fr")
  i18n_en <- nemetonshiny:::get_i18n("en")
  expect_silent(sprintf(i18n_fr$t("rag_sourced_badge"), 3L))
  expect_silent(sprintf(i18n_en$t("rag_sourced_badge"), 3L))
  expect_true(nzchar(i18n_fr$t("rag_toggle_label")))
  expect_true(nzchar(i18n_en$t("rag_toggle_label")))
})

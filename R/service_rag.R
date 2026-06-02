# RAG orchestration — récupération de passages corpus pour les
# perspectives IA sourcées (spec brief 2026-06-02, nemeton@v0.62.0).
#
# Règles de séparation app/cœur (CLAUDE.md §1, §3) : aucune logique
# métier ici — embedding, similarité pgvector et formatage des
# citations vivent côté `nemeton`. Ce service se contente :
#   1. d'ouvrir/réutiliser une connexion DB (corpus dédié ou partagé),
#   2. d'appeler `nemeton::retrieve_knowledge()` avec les bons filtres,
#   3. de dédupliquer par document pour le bloc Sources,
#   4. de produire un payload structuré consommé par `llm_prompts.R`
#      (prompt) et `mod_synthesis.R` (UI).
#
# Le service est **non-bloquant par construction** : toute erreur,
# corpus vide, clé manquante ou option désactivée renvoie un payload
# « empty » et la perspective est générée sans sources.

#' Resolve a knowledge-base DB connection (RAG corpus)
#'
#' Priority order :
#'   1. `NEMETON_KNOWLEDGE_DB_URL` — dedicated corpus DB.
#'   2. `app_con` (passed in) — corpus co-located with the app's
#'      monitoring DB (typical prod setup: same `NEMETON_DB_URL`,
#'      `knowledge_*` tables alongside `monitoring_zone`).
#'
#' Read-only by intent — the caller never writes to the corpus from
#' the app side.
#'
#' @param app_con An existing app DB connection (or NULL).
#' @return `list(con, owned)` where `owned = TRUE` means the caller
#'   must close the connection (case (1) where we opened our own).
#' @noRd
rag_knowledge_con <- function(app_con = NULL) {
  url <- Sys.getenv("NEMETON_KNOWLEDGE_DB_URL", "")
  if (nzchar(url)) {
    con <- tryCatch(nemeton::db_connect(url), error = function(e) NULL)
    return(list(con = con, owned = !is.null(con)))
  }
  list(con = app_con, owned = FALSE)
}


#' Map an app profile key to its corpus profile code (manifest tag)
#'
#' The app stores profile keys with a `profil_` prefix
#' (e.g. `profil_naturaliste`), but the knowledge document manifests
#' tag chunks with the short code only (`naturaliste`). This helper
#' strips the prefix so `retrieve_knowledge(profile_codes = ...)` can
#' match the metadata.
#'
#' If the input doesn't have the `profil_` prefix (already a short
#' code), it's returned unchanged.
#'
#' @param profile_key Character. App-side profile key (typically
#'   `profil_<short>`).
#' @return Character. The corpus-side short code, or `NULL` if input
#'   is empty/NA.
#' @noRd
rag_profile_code <- function(profile_key) {
  if (is.null(profile_key) || length(profile_key) == 0L) return(NULL)
  k <- as.character(profile_key)
  if (is.na(k) || !nzchar(k)) return(NULL)
  sub("^profil_", "", k)
}


#' Build a short, semantically-rich situation summary for embedding
#'
#' The query passed to `retrieve_knowledge()` is embedded by Mistral
#' (→ 1024-dim vector → pgvector cosine search). Better embeddings
#' come from a concrete French sentence than from a JSON dump. We
#' compose : `« Perspective pour un <profil>. <famille saillante 1>,
#' <fait marquant 2>… »`.
#'
#' Strategy (V1, kept simple) :
#'   * Translate the profile key to a human-readable French label.
#'   * Mention up to 3 family codes with the highest weights in the
#'     project (or NULL if not available).
#'   * Bail out to a generic sentence if the unit data is empty —
#'     never produce an empty string (an empty query would fail the
#'     embedding call upstream).
#'
#' @param units Optional named list / data.frame with the project's
#'   units data (parcelles, scores). May be NULL (V1 fallback).
#' @param profile_key Character. App-side profile key.
#' @param lang Character `"fr"` or `"en"` for the language of the
#'   summary. The embedding model handles both interchangeably, but
#'   matching the corpus language helps recall.
#' @return Character scalar. Always non-empty.
#' @noRd
build_situation_summary <- function(units = NULL, profile_key = NULL,
                                    lang = "fr") {
  prof_short <- rag_profile_code(profile_key) %||% "generaliste"
  if (identical(lang, "en")) {
    sprintf("Perspective for a %s on this forest unit.", prof_short)
  } else {
    sprintf("Perspective pour un acteur %s sur cette unité forestière.",
            prof_short)
  }
}


#' Build the RAG context payload for one perspective request
#'
#' Orchestrates the corpus lookup. Returns a structured list that
#' both `llm_prompts.R` (for prompt assembly) and `mod_synthesis.R`
#' (for the UI Sources block) consume.
#'
#' **Non-blocking by construction** : any error (no connection,
#' missing schema, empty corpus, missing API key, network failure,
#' 0 chunks above threshold) returns the canonical `empty` payload —
#' the caller MUST generate the perspective without sources rather
#' than failing.
#'
#' @param app_con App DB connection (or NULL when the app DB is not
#'   configured ; the corpus lookup is then skipped entirely).
#' @param profile_code Character. Short corpus profile code (see
#'   `rag_profile_code()`). `NULL` is accepted (no profile filter).
#' @param family_codes Character vector. Family codes to filter by.
#'   V1 : `NULL` recommended (small 19-doc corpus, exact-array-
#'   intersection filtering hits 0 results too easily).
#' @param situation_text Character. Semantic query (see
#'   `build_situation_summary()`).
#' @param lang Character `"fr"` or `"en"`.
#' @param top_k Integer. Max chunks to retrieve.
#' @param min_similarity Numeric in [0, 1]. Cosine similarity
#'   threshold. The real corpus test gives ~0.87 for a clearly
#'   relevant doc ; 0.55 keeps a margin without much noise.
#' @return A list with members :
#'   * `chunks` — full data.frame (NULL when empty)
#'   * `prompt_block` — text block injected before the user prompt,
#'     numbered `[^n]` for citation
#'   * `sources_md` — markdown block « Sources documentaires » for UI
#'   * `n_sources` — count of UNIQUE documents (after dedup)
#' @noRd
rag_context <- function(app_con, profile_code, family_codes,
                        situation_text, lang = "fr", top_k = 8L,
                        min_similarity = 0.55) {
  empty <- list(chunks = NULL, prompt_block = "", sources_md = "",
                n_sources = 0L)

  # Opt-out global via option — useful for tests, CI, dev environments
  # where the corpus isn't available.
  if (!isTRUE(getOption("nemeton.rag_enabled", TRUE))) return(empty)

  # Empty situation_text would crash the Mistral embedding call.
  if (is.null(situation_text) || !nzchar(situation_text)) return(empty)

  kc <- rag_knowledge_con(app_con)
  if (is.null(kc$con)) return(empty)
  on.exit({
    if (isTRUE(kc$owned)) {
      try(nemeton::db_disconnect(kc$con), silent = TRUE)
    }
  }, add = TRUE)

  chunks <- tryCatch(
    nemeton::retrieve_knowledge(
      kc$con,
      query          = situation_text,
      top_k          = top_k,
      family_codes   = family_codes,
      profile_codes  = profile_code,
      min_similarity = min_similarity,
      lang           = NULL,
      embed_provider = "mistral"
    ),
    error = function(e) {
      cli::cli_alert_warning(
        "RAG retrieve_knowledge failed (graceful degradation): {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(chunks) || !nrow(chunks)) return(empty)

  # v0.61.1 — observabilité : une ligne par perspective dans le log
  # de l'app. Visible dans la console R (dev) ou la log container
  # (prod). Aide à diagnostiquer un corpus muet : si on voit
  # systématiquement « 0 chunk(s) », c'est un problème de filtre /
  # seuil / embed ; si on ne voit jamais la ligne, c'est que
  # `rag_context()` a court-circuité avant le retrieve (corpus
  # indisponible, opt-out, situation_text vide, etc.).
  cli::cli_inform(
    "RAG: {nrow(chunks)} chunk(s) récupéré(s) au-dessus de {min_similarity}"
  )

  # Prompt block : ALL chunks numbered [^n] so the LLM can cite
  # them with the same markers `format_citations` uses for the UI
  # block. Title is i18n-aware (FR/EN).
  lines <- vapply(seq_len(nrow(chunks)), function(i) {
    sprintf("[^%d] %s", i, chunks$text[i])
  }, character(1))
  block_title <- if (identical(lang, "en")) {
    "## Reference documents"
  } else {
    "## Documents de référence"
  }
  prompt_block <- paste0(block_title, "\n", paste(lines, collapse = "\n\n"))

  # Sources block : ONE citation per UNIQUE document (top-k chunks
  # often come from the same doc → without dedup we'd render the
  # same citation 3-5 times). The data.frame is sorted by similarity
  # desc, so `!duplicated()` keeps the BEST chunk per doc.
  best_per_doc <- chunks[!duplicated(chunks$document_id), , drop = FALSE]
  sources_md <- tryCatch(
    nemeton::format_citations(best_per_doc, format = "markdown",
                              lang = lang),
    error = function(e) {
      cli::cli_alert_warning(
        "RAG format_citations failed: {conditionMessage(e)}")
      ""
    }
  )

  list(
    chunks       = chunks,
    prompt_block = prompt_block,
    sources_md   = sources_md,
    n_sources    = nrow(best_per_doc)
  )
}

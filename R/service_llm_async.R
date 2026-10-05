# Appels LLM hors de la boucle Shiny (audit 1.0, phase 4).
#
# Le serveur Shiny est mono-thread : un appel LLM (10 a 60 s, 13 pour la
# synthese complete) fait dans un observateur gelait TOUTES les sessions. Ces
# helpers renvoient une promesse : l'appel tourne dans un worker `future`.
#
# Les cles API peuvent etre posees en cours de session (modale de
# configuration, `Sys.setenv`) : un worker deja demarre ne les verrait pas. Elles
# sont donc capturees au lancement et reposees dans le worker, avec les options
# de l'application (fournisseur, modeles).

#' Environment variables an LLM call may need, captured for a worker
#' @return A named character vector of the non-empty ones.
#' @noRd
.llm_env_snapshot <- function() {
  vars <- unique(c(
    "MISTRAL_API_KEY", "NEMETON_MISTRAL_API_KEY", "ANTHROPIC_API_KEY",
    "OPENAI_API_KEY", "GOOGLE_API_KEY", "GEMINI_API_KEY", "DEEPSEEK_API_KEY",
    "OLLAMA_BASE_URL", "OLLAMA_HOST"))
  v <- Sys.getenv(vars, unset = "")
  v[nzchar(v)]
}

#' Path of the development source tree, when the package is loaded with pkgload
#' @noRd
.dev_pkg_path_courant <- function() {
  tryCatch(
    if (requireNamespace("pkgload", quietly = TRUE) &&
        isTRUE(pkgload::is_dev_package("nemetonshiny")))
      find.package("nemetonshiny") else NULL,
    error = function(e) NULL)
}

#' Run LLM prompts in sequence, with the same system prompt
#'
#' @param system_prompt Character.
#' @param prompts Named list of user prompts. The first one is mandatory: if it
#'   fails, the others are not attempted (a synthesis without which the
#'   family comments make no sense).
#' @param retries Attempts per prompt after the first one (an empty response
#'   counts as a failure: free tiers throttle bursts of requests).
#' @return `list(results = <named list of character>, errors = <named
#'   character>)`.
#' @noRd
.llm_batch_run <- function(system_prompt, prompts, retries = 1L) {
  results <- list()
  errors <- character(0)
  for (i in seq_along(prompts)) {
    nom <- names(prompts)[i]
    rep <- NULL
    derniere <- NULL
    for (essai in seq_len(1L + if (i == 1L) 0L else retries)) {
      rep <- tryCatch({
        chat <- create_llm_chat(system_prompt)
        out <- as.character(chat$chat(prompts[[i]], echo = FALSE))
        if (!length(out) || !any(nzchar(trimws(out)))) {
          stop("Empty response from LLM", call. = FALSE)
        }
        paste(out, collapse = "\n")
      }, error = function(e) {
        derniere <<- strip_ansi(conditionMessage(e))
        NULL
      })
      if (!is.null(rep)) break
      if (essai <= retries) Sys.sleep(1)
    }
    if (is.null(rep)) {
      errors[[nom]] <- derniere %||% "LLM error"
      if (i == 1L) break
    } else {
      results[[nom]] <- rep
    }
  }
  list(results = results, errors = errors)
}

#' Run LLM prompts off the Shiny loop
#'
#' @inheritParams .llm_batch_run
#' @return A promise resolving to the value of [.llm_batch_run()].
#' @noRd
llm_batch_async <- function(system_prompt, prompts, retries = 1L) {
  env <- .llm_env_snapshot()
  app_opts <- getOption("nemeton.app_options")
  dev_path <- .dev_pkg_path_courant()
  if (isTRUE(getOption("nemetonshiny.llm_inline"))) {
    # Tests : dans le processus (doublures visibles), derriere une promesse.
    return(promises::promise(function(resolve, reject) {
      later::later(function() {
        tryCatch(resolve(.llm_batch_run(system_prompt, prompts, retries)),
                 error = function(e) reject(e))
      })
    }))
  }
  if (requireNamespace("future", quietly = TRUE)) {
    plan_classes <- class(future::plan())
    if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
      .ensure_async_plan()
    }
  }
  promises::future_promise({
    if (length(env)) do.call(Sys.setenv, as.list(env))
    if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
      pkgload::load_all(dev_path, quiet = TRUE)
    } else {
      loadNamespace("nemetonshiny")
    }
    options(nemeton.app_options = app_opts)
    utils::getFromNamespace(".llm_batch_run", "nemetonshiny")(system_prompt, prompts, retries)
  }, seed = TRUE)
}

#' One LLM call off the Shiny loop
#'
#' @param system_prompt,prompt Character.
#' @return A promise resolving to the response text, or rejected with the
#'   error message.
#' @noRd
llm_chat_async <- function(system_prompt, prompt) {
  promises::then(llm_batch_async(system_prompt, list(reponse = prompt), retries = 0L),
                 function(r) {
                   if (!is.null(r$results$reponse)) return(r$results$reponse)
                   stop(r$errors[["reponse"]] %||% "LLM error", call. = FALSE)
                 })
}

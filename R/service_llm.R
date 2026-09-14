# LLM API key configuration (Mistral / Anthropic / OpenAI).
#
# Mirror of the Theia API key persistence pattern (R/service_theia.R) :
#
#   * env vars take precedence (MISTRAL_API_KEY / ANTHROPIC_API_KEY /
#     OPENAI_API_KEY - the well-established names ellmer + the public
#     SDKs already look up) ;
#   * fallback to a single JSON file `~/.config/nemetonshiny/llm.json`
#     of shape `{"mistral": "...", "anthropic": "...", "openai": "..."}`
#     chmod-0600 at write so the secret stays owner-only ;
#   * a save also Sys.setenv() so the running R session picks the key
#     up immediately (no need to restart R) ;
#   * a clear unlinks the entry (and the file when it becomes empty) and
#     Sys.unsetenv() in the running session.


# Provider registry. Adding a provider here is enough to expose it in
# the modal - the helpers iterate over `names(.LLM_PROVIDERS)`.
.LLM_PROVIDERS <- list(
  mistral   = list(label = "Mistral",   env = "MISTRAL_API_KEY"),
  anthropic = list(label = "Anthropic", env = "ANTHROPIC_API_KEY"),
  openai    = list(label = "OpenAI",    env = "OPENAI_API_KEY")
)


#' Read-only view of the supported LLM providers
#'
#' @return The `.LLM_PROVIDERS` registry. Module code uses this to
#'   populate the provider `selectInput()` and to translate a provider
#'   id to its display label / env var name.
#' @noRd
llm_providers <- function() {
  .LLM_PROVIDERS
}


#' Path to the persisted LLM keys file
#'
#' @return Absolute path to `~/.config/nemetonshiny/llm.json`.
#' @noRd
.llm_apikey_path <- function() {
  file.path(path.expand("~"), ".config", "nemetonshiny", "llm.json")
}


# Read the persisted keys file as a named list. Returns an empty list
# when the file does not exist or fails to parse. Internal - callers
# read through llm_status() / llm_save_api_key().
.llm_read_file <- function() {
  path <- .llm_apikey_path()
  if (!file.exists(path)) return(list())
  tryCatch(
    {
      raw <- jsonlite::read_json(path, simplifyVector = TRUE)
      if (is.null(raw) || !is.list(raw)) return(list())
      # Drop empty entries - defensive in case of manual editing.
      Filter(function(v) is.character(v) && nzchar(v), raw)
    },
    error = function(e) {
      cli::cli_warn(
        "Failed to read LLM keys file ({.path {path}}): {e$message}")
      list()
    }
  )
}


# Write the named keys list back to disk with chmod 0600. When the
# list is empty, unlink the file (and its parent dir if it's our own).
.llm_write_file <- function(keys) {
  path <- .llm_apikey_path()
  if (!length(keys)) {
    if (file.exists(path)) {
      tryCatch(unlink(path, force = TRUE), error = function(e) NULL)
    }
    return(invisible(TRUE))
  }
  ok <- tryCatch({
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(keys, path, auto_unbox = TRUE)
    tryCatch(Sys.chmod(path, mode = "0600"), error = function(e) NULL)
    TRUE
  }, error = function(e) {
    cli::cli_warn("Failed to write LLM keys file: {e$message}")
    FALSE
  })
  invisible(ok)
}


#' Check whether an LLM provider's API key is configured
#'
#' Considered configured when EITHER the matching env var (e.g.
#' `MISTRAL_API_KEY`) is non-empty, OR the persisted JSON file holds an
#' entry for that provider.
#'
#' @param provider One of `names(llm_providers())`.
#' @return Logical.
#' @noRd
llm_api_key_configured <- function(provider) {
  spec <- .LLM_PROVIDERS[[provider]]
  if (is.null(spec)) return(FALSE)
  if (nzchar(Sys.getenv(spec$env))) return(TRUE)
  keys <- .llm_read_file()
  isTRUE(nzchar(keys[[provider]] %||% ""))
}


#' Aggregate LLM readiness status
#'
#' @return Named list (one entry per provider) of
#'   `list(env_ok, file_ok, configured, label, env_name)`. `configured`
#'   is `env_ok || file_ok`. Used by the modal to render the per-provider
#'   status alert and to gate the edit/delete buttons.
#' @noRd
llm_status_all <- function() {
  keys <- .llm_read_file()
  stats::setNames(
    lapply(names(.LLM_PROVIDERS), function(p) {
      spec <- .LLM_PROVIDERS[[p]]
      env_ok  <- nzchar(Sys.getenv(spec$env))
      file_ok <- isTRUE(nzchar(keys[[p]] %||% ""))
      list(
        env_ok     = env_ok,
        file_ok    = file_ok,
        configured = env_ok || file_ok,
        label      = spec$label,
        env_name   = spec$env
      )
    }),
    names(.LLM_PROVIDERS)
  )
}


#' Persist an LLM API key for a given provider
#'
#' Writes the key to `~/.config/nemetonshiny/llm.json` (chmod 0600,
#' merging with any keys already stored for other providers), and
#' sets the matching env var (`MISTRAL_API_KEY` / `ANTHROPIC_API_KEY` /
#' `OPENAI_API_KEY`) for the running session.
#'
#' @param provider One of `names(llm_providers())`.
#' @param key Character. The API key to persist.
#' @return Logical. TRUE on success.
#' @noRd
llm_save_api_key <- function(provider, key) {
  spec <- .LLM_PROVIDERS[[provider]]
  if (is.null(spec)) return(FALSE)
  key <- trimws(key %||% "")
  if (!nzchar(key)) return(FALSE)

  keys <- .llm_read_file()
  keys[[provider]] <- key
  ok <- .llm_write_file(keys)
  # Always set the env var, even if the file write failed - that way
  # the running session has the key (Best effort, durability unknown).
  do.call(Sys.setenv, stats::setNames(list(key), spec$env))
  isTRUE(ok)
}


#' Clear an LLM API key for a given provider
#'
#' Removes the entry from the persisted JSON file (and unlinks the
#' file when it becomes empty) and `Sys.unsetenv()`s the matching env
#' var for the running session.
#'
#' @param provider One of `names(llm_providers())`.
#' @return Logical. TRUE when at least one source was cleared
#'   (file entry or env var), FALSE when nothing was set.
#' @noRd
llm_clear_api_key <- function(provider) {
  spec <- .LLM_PROVIDERS[[provider]]
  if (is.null(spec)) return(FALSE)

  cleared <- FALSE
  keys <- .llm_read_file()
  if (!is.null(keys[[provider]])) {
    keys[[provider]] <- NULL
    .llm_write_file(keys)
    cleared <- TRUE
  }
  if (nzchar(Sys.getenv(spec$env, ""))) {
    Sys.unsetenv(spec$env)
    cleared <- TRUE
  }
  cleared
}


# ---------------------------------------------------------------------------
# Repli de modele (403 palier / 429 quota)
# ---------------------------------------------------------------------------

# Modeles de repli par provider, essayes DANS L'ORDRE quand le modele
# principal est refuse pour une raison de palier ou de quota.
#
# Mesure le 2026-09-14 sur une cle Mistral sans plan actif : toute la gamme
# " premier " (large, medium, small, magistral) repond 403 ou 429 avec
# x-ratelimit-limit-req-minute a 0, tandis que la famille ministral garde du
# quota - 750 / 188 / 30 req/min pour 3b / 8b / 14b. Le repli va donc du plus
# capable au moins capable parmi ceux qui restent joignables.
#
# Un repli reste un PIS-ALLER : ces modeles " edge " sont nettement moins fins
# que la gamme premier pour une analyse par profil expert. D'ou la notification
# de .llm_notify_fallback() - une analyse degradee ne doit jamais passer
# silencieusement pour une analyse nominale.
.LLM_FALLBACK_MODELS <- list(
  mistral = c("ministral-14b-latest", "ministral-8b-latest", "ministral-3b-latest")
)


#' Fallback model ids for a provider
#'
#' @param provider Character. Provider id.
#' @return Character vector of model ids, empty when the provider has none.
#' @noRd
.llm_fallback_models <- function(provider) {
  if (is.null(provider) || length(provider) != 1L ||
      !provider %in% names(.LLM_FALLBACK_MODELS)) {
    return(character(0))
  }
  .LLM_FALLBACK_MODELS[[provider]]
}


#' Is this error a tier or quota refusal?
#'
#' 403 means the plan does not include the model; 429 means no quota is left,
#' or none was ever granted (a key with no active plan reports
#' `x-ratelimit-limit-req-minute: 0`). Both are refusals that another model of
#' the same provider may escape, so both are worth a retry.
#'
#' Every other failure - bad key, network outage, malformed prompt, provider
#' incident - is NOT, and must surface unchanged. Retrying those would replace
#' the real diagnosis with the error of a second, equally doomed call.
#'
#' @param e A condition.
#' @return TRUE when trying a fallback model is worthwhile.
#' @noRd
.llm_is_tier_error <- function(e) {
  msg <- tryCatch(conditionMessage(e), error = function(...) NULL)
  if (!is.character(msg) || !length(msg)) return(FALSE)
  msg <- paste(msg, collapse = " ")
  grepl("HTTP[[:space:]]*(403|429)", msg) ||
    grepl("subscription tier", msg, ignore.case = TRUE) ||
    grepl("rate limit", msg, ignore.case = TRUE)
}


# Prevenir l'utilisateur qu'une reponse vient d'un modele de repli. Hors
# session Shiny (tests, appel console), un message cli tient lieu de canal -
# la regle stricte #9 interdit message()/cat() en code de prod.
.llm_notify_fallback <- function(model) {
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) {
    cli::cli_alert_warning("LLM fallback model in use: {.val {model}}.")
    return(invisible(FALSE))
  }
  i18n <- get_i18n(get_app_config("language", "fr"))
  shiny::showNotification(
    sprintf(i18n$t("ia_modele_repli"), model),
    type = "warning",
    duration = 10
  )
  invisible(TRUE)
}


#' Wrap a chat builder with automatic model fallback
#'
#' Returns an object exposing a single `$chat()` method - the only method the
#' call sites use (verified across the six `create_llm_chat()` consumers). That
#' keeps the fallback free for every caller: no call site changes.
#'
#' `$chat()` tries `model` first. On a tier/quota refusal it retries with each
#' fallback model in turn and notifies the user on the first success. When no
#' fallback succeeds it raises the ORIGINAL error, not the last one: the user
#' needs to read why the configured model was refused, not why a stand-in they
#' never chose also failed.
#'
#' @param build_chat Function of one argument (a model id) returning an ellmer
#'   chat object.
#' @param provider Character. Provider id, used to look up the fallback chain.
#' @param model Character. The configured (primary) model id.
#' @return A list with a single `chat` element.
#' @noRd
llm_chat_with_fallback <- function(build_chat, provider, model) {
  list(
    chat = function(...) {
      premier <- tryCatch(build_chat(model)$chat(...), error = function(e) e)
      if (!inherits(premier, "error")) return(premier)
      if (!.llm_is_tier_error(premier)) stop(premier)

      for (repli in .llm_fallback_models(provider)) {
        # Ne pas rejouer a l'identique le modele qui vient d'echouer.
        if (identical(repli, model)) next
        out <- tryCatch(build_chat(repli)$chat(...), error = function(e) e)
        if (!inherits(out, "error")) {
          .llm_notify_fallback(repli)
          return(out)
        }
      }
      stop(premier)
    }
  )
}

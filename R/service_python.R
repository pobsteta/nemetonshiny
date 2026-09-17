# =============================================================================
# Interpreteurs Python de l'application
# =============================================================================
#
# LA REGLE, et elle n'est pas negociable :
#
#   Tout moteur qui a besoin de Python s'execute dans SON PROPRE PROCESSUS,
#   avec son interpreteur epingle a la CREATION du processus.
#
# Pourquoi une regle et pas un reglage : `reticulate` lie un interpreteur
# UNE FOIS par processus R et ne le relie jamais. Un second moteur qui en
# demande un autre obtient :
#
#   Error: reticulate is already bound to a different Python
#
# Et aucun reglage global ne peut les servir tous, parce que leurs exigences
# se CONTREDISENT :
#
#   * opencanopy  exige `RETICULATE_PYTHON` epinglee sur l'env conda
#     `open_canopy` ;
#   * FORDEAD     exige qu'elle soit ABSENTE - le cœur le documente
#     (`nemeton/R/fordead_python.R:336-343`) : une variable definie « ecrase
#     silencieusement use_python() / use_virtualenv() meme avec
#     required = TRUE », et le remede imprime a l'utilisateur est de la
#     retirer puis de redemarrer R ;
#   * RECONFORT   utilise l'env conda IOTA2/GEODES ;
#   * rvt-py      n'a pas d'env dedie et prend le Python ambiant.
#
# Ce que opencanopy impose, FORDEAD l'interdit. D'ou l'isolation par
# processus, seule reponse qui les concilie.
#
# Consequence pratique pour qui ajoute un moteur : passer par
# `run_with_python()`. Un `reticulate::import()` ecrit directement dans la
# session Shiny la lie DEFINITIVEMENT au mauvais interpreteur, et seul un
# redemarrage de l'app la delie. Le test
# `test-service_python.R` refuse toute nouvelle occurrence hors liste connue.


# Registre moteur -> interpreteur. Ajouter un moteur = ajouter une entree.
#
# `option_python` / `envvar` : chemin complet, prioritaires (echappatoire pour
# un poste non standard). `option_env` / `condaenv` : nom de l'env conda,
# resolu ensuite. Les quatre champs sont facultatifs, l'ordre ne l'est pas.
.PYTHON_ENGINES <- list(
  opencanopy = list(
    condaenv      = "open_canopy",
    option_python = "nemetonshiny.opencanopy_python",
    option_env    = "nemetonshiny.opencanopy_condaenv",
    envvar        = "OPENCANOPY_PYTHON"
  )
)

# Racines d'installation conda balayees quand `reticulate::conda_python()`
# ne repond pas (conda absent du PATH, installation non enregistree).
.CONDA_ROOTS <- c("miniforge3", "mambaforge", "miniconda3", "anaconda3")


#' Resolve the Python interpreter of a declared engine
#'
#' Order: explicit option, then environment variable, then the conda env
#' (via reticulate, then by scanning the usual install roots). The explicit
#' forms come first so a non-standard machine can always be unblocked without
#' touching the code.
#'
#' @param engine Character. A name of [.PYTHON_ENGINES].
#' @return Absolute path to a `python` binary, or `NA_character_` when the
#'   engine is unknown or its environment is not installed. **Never raises**:
#'   a missing engine is a degraded mode (the caller falls back), not a crash.
#' @noRd
engine_python <- function(engine) {
  spec <- .PYTHON_ENGINES[[engine]]
  if (is.null(spec)) return(NA_character_)

  if (!is.null(spec$option_python)) {
    cand <- getOption(spec$option_python,
                      Sys.getenv(spec$envvar %||% "", unset = ""))
    if (is.character(cand) && length(cand) == 1L &&
        nzchar(cand) && file.exists(cand)) {
      return(cand)
    }
  }

  env <- if (!is.null(spec$option_env)) {
    getOption(spec$option_env, spec$condaenv)
  } else spec$condaenv
  if (is.null(env) || !nzchar(env)) return(NA_character_)

  p <- tryCatch(reticulate::conda_python(env), error = function(e) NA_character_)
  if (length(p) == 1L && !is.na(p) && file.exists(p)) return(p)

  for (root in .CONDA_ROOTS) {
    fp <- file.path(path.expand("~"), root, "envs", env, "bin", "python")
    if (file.exists(fp)) return(fp)
  }
  NA_character_
}


#' Run a function in an isolated R subprocess with a pinned interpreter
#'
#' The generic form of what the Open-Canopy CHM path has been doing since
#' spec 005. `func` runs in a brand-new R process whose `RETICULATE_PYTHON`
#' is the engine's interpreter, so it binds cleanly whatever the calling
#' session already bound.
#'
#' `R_ENVIRON_USER = ""` is not decoration: without it a `RETICULATE_PYTHON`
#' left in the user's `.Renviron` would override the pin we just set, and the
#' child would bind the wrong interpreter while reporting the right one.
#'
#' Child stdout is streamed line by line to `on_line` as it arrives, so a run
#' of several minutes keeps talking instead of dumping everything at the end.
#'
#' @param engine Character. Key of [.PYTHON_ENGINES].
#' @param func Function executed in the child. Must be self-contained: it
#'   crosses a process boundary, so it closes over NOTHING from here.
#' @param args List of arguments for `func`.
#' @param on_line Optional function called with each stdout line.
#' @param poll_ms Polling interval, milliseconds.
#' @return The child's return value. A child error is re-raised here.
#' @noRd
run_with_python <- function(engine, func, args = list(),
                            on_line = NULL, poll_ms = 250L) {
  py <- engine_python(engine)
  if (is.na(py)) {
    cli::cli_abort(c(
      "Python environment for engine {.val {engine}} not found.",
      i = "Install its conda env, or point {.code options({(.PYTHON_ENGINES[[engine]]$option_python %||% '?')}=)} at an interpreter."
    ))
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg callr} is required to isolate engine {.val {engine}}.",
      i = "Running in-process would bind this session to {.path {py}} for good."
    ))
  }

  cli::cli_alert_info(
    "Engine {.val {engine}} in an isolated R session (RETICULATE_PYTHON = {.path {py}})...")

  px <- callr::r_bg(
    func = func, args = args,
    env  = c(callr::rcmd_safe_env(), RETICULATE_PYTHON = py, R_ENVIRON_USER = ""),
    stdout = "|", stderr = "2>&1", supervise = TRUE
  )
  drain <- function() {
    for (ln in px$read_output_lines()) {
      if (is.function(on_line)) on_line(ln) else cat(ln, "\n", sep = "")
    }
  }
  repeat {
    px$poll_io(poll_ms)
    drain()
    if (!px$is_alive()) break
  }
  drain()
  px$get_result()   # re-leve l'erreur de l'enfant, s'il y en a une
}

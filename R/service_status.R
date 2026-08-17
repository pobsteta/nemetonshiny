#' Source availability status (spec 032, brief A5)
#'
#' @description
#' Why an indicator is empty, kept as a **named cause** rather than a sentence.
#'
#' Four situations used to leave `build_lst_layer()` through the same `NULL`:
#' the source is off, the credentials are missing, the AOI is outside coverage,
#' or the fetch failed. The last two are the ones that matter and the ones the
#' interface could not tell apart - yet "no Thermocity scene over a forest
#' commune" is the normal case, and "the catalogue is unreachable" is a fault.
#' Showing the same blank for both makes a fault look normal and a normality
#' look like a fault.
#'
#' The cause comes from the core (`nemeton::theia_source_status()`), which owns
#' the catalogue knowledge; the app only stores it and translates it.
#'
#' @name service_status
#' @keywords internal
NULL


#' Reasons a source may be unavailable
#'
#' @description
#' Mirror of the `reason` vocabulary of `nemeton::theia_source_status()`. Kept
#' here so the app can be tested - and can degrade - without the core function.
#'
#' @noRd
SOURCE_STATUS_REASONS <- c(
  "ok", "unknown_source", "no_stac_collection",
  "no_credentials", "no_asset_over_aoi", "error"
)


#' Is the core able to answer about a source?
#'
#' @description
#' `theia_source_status()` is not published in every core version the app
#' supports. Absent, the app keeps its previous behaviour: it tries the
#' download and reports nothing more than before - never a wrong cause.
#'
#' @return Logical.
#'
#' @noRd
.theia_status_available <- function() {
  isTRUE(tryCatch(
    "theia_source_status" %in% getNamespaceExports("nemeton"),
    error = function(e) FALSE
  ))
}


#' Call the core accessor
#'
#' @description
#' Resolved at call time rather than written as `nemeton::theia_source_status()`
#' so the app still parses and installs against a core that does not export it
#' yet - and so tests can substitute it.
#'
#' @param source_key Character. Datasource key.
#' @param aoi An sf area of interest.
#'
#' @return Whatever the core returns.
#'
#' @noRd
.theia_status_call <- function(source_key, aoi) {
  fn <- getExportedValue("nemeton", "theia_source_status")
  fn(source_key, aoi)
}


#' Ask the core why a Theia source is (un)available
#'
#' @param source_key Character. Datasource key, e.g. `"theia_lst"`.
#' @param aoi An sf area of interest.
#'
#' @return A list with `available`, `reason`, `n_assets`, `collection`, or
#'   `NULL` when the core cannot answer - `NULL` means "unknown", never "fine".
#'
#' @noRd
theia_source_status_safe <- function(source_key, aoi) {
  if (!.theia_status_available()) return(NULL)
  if (is.null(aoi) || !inherits(aoi, "sf") || nrow(aoi) == 0) return(NULL)

  st <- tryCatch(
    .theia_status_call(source_key, aoi),
    error = function(e) {
      cli::cli_warn("theia_source_status({source_key}) a \u00e9chou\u00e9 : {conditionMessage(e)}")
      list(available = FALSE, reason = "error", n_assets = 0L)
    }
  )

  if (!is.list(st) || is.null(st$reason)) return(NULL)

  # Une cause inconnue de cette version de l'app ne doit pas devenir un libelle
  # vide : on la degrade en "error", qui a une traduction.
  if (!st$reason %in% SOURCE_STATUS_REASONS) st$reason <- "error"

  st$available <- isTRUE(st$available)
  st$n_assets <- suppressWarnings(as.integer(st$n_assets %||% 0L))
  if (is.na(st$n_assets)) st$n_assets <- 0L

  # `detail` (v0.174.0) porte le message technique de l'echec : "no STAC API
  # endpoint configured", le `conditionMessage` de la recherche... C'est ce
  # qu'un exploitant veut lire, la cause seule ne suffit pas a diagnostiquer.
  st$detail <- .clean_detail(st$detail)
  st
}


#' Normalise the optional `detail` field
#'
#' @description
#' `detail` arrives as `NA_character_` from the core, as JSON `null` from the
#' status file, or absent altogether. All three mean "nothing to add" and must
#' collapse to `NULL` - the caller then falls back to the bare reason. Written
#' defensively because the three shapes do not compare alike: `is.na()` on a
#' list returns `NA`, which is not a condition.
#'
#' @param detail Whatever was read.
#'
#' @return A non-empty character scalar, or `NULL`.
#'
#' @noRd
.clean_detail <- function(detail) {
  if (is.null(detail)) return(NULL)
  d <- suppressWarnings(as.character(unlist(detail, use.names = FALSE)))
  d <- d[!is.na(d) & nzchar(d) & d != "NA"]
  if (length(d) == 0L) NULL else d[1]
}


#' Path of the per-project source status file
#'
#' @param project_path Character. Project root.
#'
#' @return Character path.
#'
#' @noRd
.source_status_path <- function(project_path) {
  file.path(project_path, "data", "source_status.json")
}


#' Persist the status of one source
#'
#' @description
#' Written at acquisition time so the sources panel can explain an empty
#' indicator **without** re-running a computation - the question "why is A5
#' empty?" is asked long after the run that answered it.
#'
#' @param project_path Character. Project root.
#' @param source_key Character. Datasource key.
#' @param status List from [theia_source_status_safe()], or `NULL` to erase.
#'
#' @return Invisible `TRUE` on success, `FALSE` otherwise.
#'
#' @noRd
save_source_status <- function(project_path, source_key, status) {
  if (is.null(project_path) || !nzchar(project_path)) return(invisible(FALSE))

  path <- .source_status_path(project_path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  all <- tryCatch(
    if (file.exists(path)) jsonlite::read_json(path, simplifyVector = TRUE) else list(),
    error = function(e) list()
  )
  if (!is.list(all)) all <- list()

  all[[source_key]] <- if (is.null(status)) NULL else list(
    reason = status$reason,
    available = isTRUE(status$available),
    n_assets = suppressWarnings(as.integer(status$n_assets %||% NA_integer_)),
    detail = status$detail %||% NA_character_,
    checked_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  isTRUE(tryCatch({
    jsonlite::write_json(all, path, auto_unbox = TRUE, pretty = TRUE)
    TRUE
  }, error = function(e) {
    cli::cli_warn("Statut de source non enregistr\u00e9 : {conditionMessage(e)}")
    FALSE
  }))
}


#' Read back the stored status of one source
#'
#' @param project_path Character. Project root.
#' @param source_key Character. Datasource key.
#'
#' @return A list with `reason`, `available`, `n_assets`, `checked_at`, or
#'   `NULL` when nothing was ever recorded.
#'
#' @noRd
load_source_status <- function(project_path, source_key) {
  if (is.null(project_path) || !nzchar(project_path)) return(NULL)

  path <- .source_status_path(project_path)
  if (!file.exists(path)) return(NULL)

  all <- tryCatch(jsonlite::read_json(path, simplifyVector = TRUE),
                  error = function(e) NULL)
  if (!is.list(all) || is.null(all[[source_key]])) return(NULL)

  st <- all[[source_key]]
  if (is.null(st$reason)) return(NULL)
  st$available <- isTRUE(st$available)
  st$detail <- .clean_detail(st$detail)
  st
}


#' Translate a source status into a user-facing sentence
#'
#' @description
#' Three states, deliberately: available, legitimately empty, broken. The
#' middle one is phrased as information and not as a warning - a forest commune
#' outside Thermocity coverage is not a problem to fix.
#'
#' @param status List from [theia_source_status_safe()] or [load_source_status()].
#' @param i18n Translator object.
#'
#' @return A list with `level` (`"ok"`, `"info"`, `"error"`) and `text`, or
#'   `NULL` when the status is unknown - the caller then keeps its previous
#'   rendering rather than inventing a cause.
#'
#' @noRd
source_status_message <- function(status, i18n) {
  if (is.null(status) || is.null(status$reason)) return(NULL)

  n <- suppressWarnings(as.integer(status$n_assets %||% NA_integer_))

  switch(
    status$reason,
    # Le compte de scenes vient de `theia_source_status()`. Quand le coeur ne
    # sait pas repondre, on sait que la source a fonctionne mais pas combien de
    # scenes : mieux vaut une phrase sans chiffre qu'un "NA scenes".
    ok = list(
      level = "ok",
      text = if (is.na(n)) i18n$t("lst_status_ok_nocount") else
        sprintf(i18n$t("lst_status_ok"), n)
    ),
    no_asset_over_aoi = list(
      level = "info",
      text = i18n$t("lst_status_no_coverage")
    ),
    # `no_credentials` garde son message dedie amont (`lst_need_theia`), qui est
    # actionnable : l'utilisateur peut configurer ses cles. Le renvoyer ici en
    # ferait une panne alors que c'est une etape de configuration.
    no_credentials = NULL,
    list(
      level = "error",
      # Le `detail` du coeur quand il existe : "no STAC API endpoint
      # configured" se diagnostique, "error" ne se diagnostique pas.
      text = sprintf(i18n$t("lst_status_error"),
                     status$detail %||% status$reason)
    )
  )
}

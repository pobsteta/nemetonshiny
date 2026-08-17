#' Applicability verdicts for the source-conditioned indicators (core v0.175.0)
#'
#' @description
#' Whether R5 and A5 can say anything about *this* forest, asked **before** the
#' computation rather than discovered after it. The core owns the judgement
#' (`nemeton::r5_applicabilite()`, `nemeton::a5_applicabilite()`) and returns a
#' **stable key**; the app translates it and decides what to skip.
#'
#' Two nuances this file exists to protect:
#'
#'   * **`eligible_fordead_out_of_calibration` is not a refusal.** R5's
#'     validation area is the ONF/DSF 2024 one - Vosges, Jura, Ain, Savoie,
#'     Haute-Savoie, 27 565 km2. Outside it a silver fir is still a silver fir:
#'     the computation runs, only its confidence classes are extrapolated. None
#'     of the local projects sits inside that area, and Fordead and Dabo are
#'     nonetheless 100 % fir. Blocking R5 on that criterion would withhold a
#'     usable signal, so this verdict is rendered as information and **never
#'     short-circuits**.
#'   * **`a5_applicabilite()` without `lst` answers at the scale of the AOI.** A
#'     STAC query knows bounding boxes, not pixels: `eligible` there means "the
#'     coverage exists", not "every unit is scoreable". Passing the cached raster
#'     switches to a per-unit verdict, the only one able to return
#'     `eligible_partial`.
#'
#' @name service_applicabilite
#' @keywords internal
NULL


#' Verdicts that mean "do not bother computing"
#'
#' @description
#' Deliberately short. A verdict absent from this list lets the computation run,
#' because the cost of a needless run is a column of `NA` while the cost of a
#' wrong skip is a missing indicator the user cannot get back.
#' `eligible_fordead_out_of_calibration` is **not** here, on purpose.
#'
#' @noRd
APPLICABILITE_SKIP <- c("not_applicable", "no_species", "no_coverage")


#' Is the core able to judge applicability?
#'
#' @param fn Character. `"r5_applicabilite"` or `"a5_applicabilite"`.
#'
#' @return Logical.
#'
#' @noRd
.applicabilite_available <- function(fn) {
  isTRUE(tryCatch(fn %in% getNamespaceExports("nemeton"),
                  error = function(e) FALSE))
}


#' Call a core applicability accessor
#'
#' @description
#' Resolved at call time so the app installs and parses against a core that does
#' not export these yet, and so tests can substitute it.
#'
#' @param fn Character. Accessor name.
#' @param ... Passed through.
#'
#' @return Whatever the core returns.
#'
#' @noRd
.applicabilite_call <- function(fn, ...) {
  getExportedValue("nemeton", fn)(...)
}


#' Ask the core whether an indicator applies
#'
#' @param fn Character. `"r5_applicabilite"` or `"a5_applicabilite"`.
#' @param ... Passed to the accessor (`units`, `bdforet`, `lst`, `buffer_m`).
#'
#' @return A list carrying at least `status`, or `NULL` when the core cannot
#'   answer - `NULL` means "unknown", never "not applicable".
#'
#' @noRd
applicabilite_safe <- function(fn, ...) {
  if (!.applicabilite_available(fn)) return(NULL)

  out <- tryCatch(
    .applicabilite_call(fn, ...),
    error = function(e) {
      cli::cli_warn("{fn}() a \u00e9chou\u00e9 : {conditionMessage(e)}")
      list(status = "error")
    }
  )

  if (!is.list(out) || is.null(out$status) || !nzchar(out$status)) return(NULL)
  out
}


#' Should the computation be skipped for this verdict?
#'
#' @description
#' `skip` is a parameter and not a constant because the answer depends on **how
#' the caller computes**, not on the verdict alone.
#'
#' `add_r5_to_indicators()` is the case that proved it: it routes each unit by
#' the **alert type intersecting it**, never by a species column, so it works
#' perfectly on units whose species is unknown. Skipping R5 on `no_species` -
#' which the generic set does - silently disabled a path that worked. R5 is
#' therefore called with `skip = "not_applicable"` alone.
#'
#' @param verdict List from [applicabilite_safe()], or `NULL`.
#' @param skip Character vector of statuses that warrant skipping.
#'
#' @return Logical. `FALSE` when the verdict is unknown: not knowing is never a
#'   reason to skip.
#'
#' @noRd
applicabilite_skip <- function(verdict, skip = APPLICABILITE_SKIP) {
  !is.null(verdict) && isTRUE(verdict$status %in% skip)
}


#' Translate an applicability verdict
#'
#' @description
#' Three levels, as for the source statuses: `ok` when the indicator applies,
#' `info` when it legitimately will not produce a value (or will produce one
#' whose confidence is extrapolated), `error` when the question could not be
#' answered.
#'
#' The counts matter as much as the verdict - "3 UGF sur 30" is actionable where
#' "partiel" is not - so they are interpolated into the sentence.
#'
#' @param kind Character. `"r5"` or `"a5"`.
#' @param verdict List from [applicabilite_safe()], or `NULL`.
#' @param i18n Translator object.
#'
#' @return List with `level` and `text`, or `NULL` when the verdict is unknown.
#'
#' @noRd
applicabilite_message <- function(kind, verdict, i18n) {
  if (is.null(verdict) || is.null(verdict$status)) return(NULL)

  key <- paste0(kind, "_appl_", verdict$status)
  if (!isTRUE(i18n$has(key))) key <- paste0(kind, "_appl_error")
  if (!isTRUE(i18n$has(key))) return(NULL)

  txt <- i18n$t(key)

  # Les comptes ne sont pas decoratifs : ils disent l'ampleur. On ne les ajoute
  # que quand le coeur les a fournis, pour ne pas afficher "0 sur 0".
  n_units <- suppressWarnings(as.integer(verdict$n_units %||% NA_integer_))
  n_elig <- suppressWarnings(as.integer(
    verdict$n_eligible %||% verdict$n_fordead %||% NA_integer_))
  if (!is.na(n_units) && !is.na(n_elig) && n_units > 0L) {
    txt <- paste0(txt, " ", sprintf(i18n$t("appl_count_fmt"), n_elig, n_units))
  }

  level <- if (identical(verdict$status, "error")) {
    "error"
  } else if (verdict$status %in% APPLICABILITE_SKIP) {
    "info"
  } else if (isTRUE(grepl("out_of_calibration", verdict$status, fixed = TRUE))) {
    # Information, pas avertissement : le calcul tourne et le signal est
    # exploitable, seules les classes de confiance sont extrapolees.
    "info"
  } else {
    "ok"
  }

  list(level = level, text = txt, status = verdict$status)
}

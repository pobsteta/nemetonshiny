#' T4 dynamic vitality (spec 068) - application glue
#'
#' @description
#' `nemeton::indicateur_t4_vitalite()` (nemeton >= 2026.11.28) compares the
#' observed dominant-height growth (FORMS-T) with the growth expected for IFN
#' plots of the same species and SER. It reads two columns of the units, which
#' the app has to supply:
#'
#'   * `essence`: dominant species as an IFN `ESPAR` code, derived here from the
#'     BD Foret label (`species`) with `nemeton::resoudre_espar()`;
#'   * `ser`: sylvoecoregion, localised once per UGF by `ensure_ugf_ser()`.
#'
#' No figure is computed here.
#'
#' @name service_t4
#' @keywords internal
NULL


#' Add the `essence` (ESPAR) column T4 reads
#'
#' @description
#' A label `nemeton::resoudre_espar()` cannot resolve gets `NA`, which T4 reports
#' as `skipped_no_essence`: no code is guessed. That is the case of the BD Foret
#' label "Sapin, epicea", which names two species (ESPAR 61 and 62).
#'
#' An `essence` column already present on the units (user input) is kept.
#'
#' @param parcels sf / data.frame. Compute units, with a `species` label column
#'   when BD Foret enrichment succeeded.
#'
#' @return `parcels`, with a character `essence` column.
#'
#' @noRd
.t4_ajouter_essence <- function(parcels) {
  if ("essence" %in% names(parcels)) return(parcels)
  n <- nrow(parcels)
  sp <- if ("species" %in% names(parcels)) as.character(parcels$species) else NULL
  essence <- rep(NA_character_, n)
  if (length(sp) == n && any(!is.na(sp) & nzchar(sp))) {
    espar <- tryCatch(as.character(nemeton::resoudre_espar(sp)),
                      error = function(e) NULL)
    if (length(espar) == n) essence <- espar
  }
  parcels$essence <- essence
  parcels
}

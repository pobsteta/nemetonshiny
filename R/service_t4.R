#' T4 dynamic vitality (spec 068) - application glue
#'
#' @description
#' `nemeton::indicateur_t4_vitalite()` (nemeton >= 2026.11.28) compares the
#' observed dominant-height growth (FORMS-T) with the growth expected for IFN
#' plots of the same species and SER. It reads two columns of the units, which
#' the app has to supply:
#'
#'   * `essence`: dominant species as an IFN `ESPAR` code, derived here from the
#'     BD Foret label (`species`) and the UGF SER with
#'     `nemeton::resoudre_espar_bdforet()` (nemeton >= 2026.11.35);
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
#' The core resolves the BD Foret label together with the UGF SER:
#' `nemeton::resoudre_espar_bdforet(label, ser)`. A group label ("Chenes
#' decidus", "Pin laricio, pin noir", "Sapin, epicea") takes the species with
#' the largest IFN volume per hectare in the SER; a generic label ("Feuillus",
#' "Coniferes", "Mixte") gets `NA`, which T4 reports as `skipped_no_essence`.
#' No species convention lives in the app.
#'
#' An `essence` column already present on the units (user input) is kept.
#'
#' @param parcels sf / data.frame. Compute units, with a `species` label column
#'   when BD Foret enrichment succeeded, and a `ser` column when localised.
#'
#' @return `parcels`, with a character `essence` column.
#'
#' @noRd
.t4_ajouter_essence <- function(parcels) {
  if ("essence" %in% names(parcels)) return(parcels)
  n <- nrow(parcels)
  sp <- if ("species" %in% names(parcels)) as.character(parcels$species) else NULL
  ser <- if ("ser" %in% names(parcels)) as.character(parcels$ser) else NULL
  essence <- rep(NA_character_, n)
  if (length(sp) == n && any(!is.na(sp) & nzchar(sp))) {
    espar <- tryCatch(
      as.character(nemeton::resoudre_espar_bdforet(sp, ser = ser)),
      error = function(e) {
        cli::cli_warn("T4: species resolution to ESPAR failed: {conditionMessage(e)}")
        NULL
      })
    if (length(espar) == n) essence <- espar
  }
  parcels$essence <- essence
  parcels
}

# Mission 022 - " Aucune alerte dans cette strate " (FAST, FORDEAD, RECONFORT).
#
# Quand la strate affichee n'est pas la zone `_tot` et ne compte aucune alerte,
# afficher " zone saine " laisserait croire que toute la foret est saine alors
# que le calcul porte sur `_tot`. Ce fichier ne contient que de la PRESENTATION :
# on compte des pixels DEJA classes par le coeur (aucune regle metier nouvelle).

#' Alert rule of a monitoring engine, as a boolean raster
#'
#' Classes come from the core: FAST and FORDEAD masks use 0-4 (class >= 1 is an
#' alert, bare soil included), RECONFORT classification uses 1-3 (classes 2-3
#' are dieback). `NA` stays `NA`.
#'
#' @param r A `SpatRaster`.
#' @param moteur One of `"fast"`, `"fordead"`, `"reconfort"`.
#' @return A boolean `SpatRaster`.
#' @noRd
.sante_regle_alerte <- function(r, moteur = c("fast", "fordead", "reconfort")) {
  moteur <- match.arg(moteur)
  if (identical(moteur, "reconfort")) (r >= 2) & (r <= 3) else r >= 1
}

#' Number of cells and surface (ha) in alert, and number of non-NA cells
#' @noRd
.sante_compte <- function(r, moteur) {
  vide <- list(n_alerte = 0, ha_alerte = 0, n_donnees = 0)
  if (is.null(r) || !inherits(r, "SpatRaster")) return(vide)
  if (terra::ncell(r) == 0L) return(vide)
  if (terra::nlyr(r) > 1L) r <- r[[1L]]
  tryCatch({
    al <- .sante_regle_alerte(r, moteur)
    n  <- as.numeric(terra::global(al, "sum", na.rm = TRUE)[[1]])
    # Surface approximative (presentation) : pixels x surface d'une cellule.
    # CRS projete -> resolution planaire ; lon/lat -> cellSize() (ellipsoide).
    ha <- if (isTRUE(terra::is.lonlat(r))) {
      as.numeric(terra::global(
        terra::cellSize(r, unit = "ha") * al, "sum", na.rm = TRUE)[[1]])
    } else {
      n * prod(terra::res(r)) / 1e4
    }
    nd <- as.numeric(terra::global(!is.na(r), "sum", na.rm = TRUE)[[1]])
    list(n_alerte  = if (is.finite(n)) n else 0,
         ha_alerte = if (is.finite(ha)) ha else 0,
         n_donnees = if (is.finite(nd)) nd else 0)
  }, error = function(e) vide)
}

#' State of a stratum display for the health maps
#'
#' @param r_strate Raster already clipped to the displayed stratum.
#' @param r_tot Unclipped `_tot` raster, or `NULL` when the displayed stratum
#'   IS `_tot` (or there is no `_tot`): the legacy behaviour is then unchanged.
#' @param moteur `"fast"`, `"fordead"` or `"reconfort"`.
#' @return A list: `etat` (`"alertes_strate"` = nothing new to show,
#'   `"strate_vide_tot_alerte"` = no alert in the stratum but some in `_tot`,
#'   `"tout_sain"` = legacy "healthy" behaviour), `n_tot` and `ha_tot` (alert
#'   pixels / hectares of `_tot`, 0 unless the second state).
#' @noRd
.etat_strate_sante <- function(r_strate, r_tot = NULL,
                               moteur = c("fast", "fordead", "reconfort")) {
  moteur <- match.arg(moteur)
  res <- function(etat, n = 0, ha = 0)
    list(etat = etat, n_tot = n, ha_tot = ha)
  s <- .sante_compte(r_strate, moteur)
  if (s$n_alerte > 0) return(res("alertes_strate"))
  # Strate sans aucune donnee (hors emprise) : comportement actuel.
  if (s$n_donnees <= 0) return(res("tout_sain"))
  if (is.null(r_tot)) return(res("tout_sain"))
  t <- .sante_compte(r_tot, moteur)
  if (t$n_alerte > 0) {
    return(res("strate_vide_tot_alerte", t$n_alerte, t$ha_alerte))
  }
  res("tout_sain")
}

#' Reminder sentence " the whole zone counts N pixels in alert (approx. H ha) "
#' @param i18n i18n object.
#' @param etat Result of `.etat_strate_sante()`.
#' @noRd
.sante_rappel_tot <- function(i18n, etat) {
  fr <- identical(i18n$language, "fr")
  big <- if (fr) "\u00a0" else ","
  dec <- if (fr) "," else "."
  sprintf(
    i18n$t("sante_strate_rappel_tot"),
    format(round(etat$n_tot), big.mark = big, scientific = FALSE, trim = TRUE),
    format(round(etat$ha_tot, 1), big.mark = big, decimal.mark = dec,
           nsmall = 1, scientific = FALSE, trim = TRUE)
  )
}

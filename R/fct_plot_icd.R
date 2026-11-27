# fct_plot_icd.R - Graphiques plotly du clic sur la carte ICD.
#
# Presentation pure : toutes les valeurs (series, seuils, poids, mois sensibles,
# classe et niveau) viennent de `nemeton::read_icd_pixel_series()`. Rien n'est
# recalcule ici.

# Couleur par indice (palette qualitative, daltonisme-compatible).
.ICD_COULEURS_INDICES <- c(GRVI = "#1b9e77", CIre = "#7570b3",
                           CRSWIR = "#d95f02", CRre = "#e7298a",
                           NDVI = "#66a61e")

# Date du milieu du mois, pour l'axe des abscisses.
.icd_date_mois <- function(annee, mois) {
  as.Date(sprintf("%04d-%02d-15", as.integer(annee), as.integer(mois)))
}

# Rectangles (shapes plotly) des mois sensibles de l'essence, un par annee.
.icd_formes_sensibles <- function(annees, mois_sensibles) {
  if (!length(mois_sensibles)) return(list())
  deb <- min(mois_sensibles); fin <- max(mois_sensibles)
  lapply(annees, function(a) list(
    type = "rect", xref = "x", yref = "paper", y0 = 0, y1 = 1,
    x0 = as.character(as.Date(sprintf("%04d-%02d-01", a, deb))),
    x1 = as.character(as.Date(sprintf("%04d-%02d-28", a, fin))),
    fillcolor = "rgba(150,150,150,0.14)", line = list(width = 0),
    layer = "below"))
}

#' Chart 1: the five indices of a pixel
#'
#' One line per index and per year (anomaly `z` or raw value), sensitive months
#' of the species shaded. Only the indices that enter the species' formula are
#' drawn (CRre is absent for conifers).
#'
#' @param s List from `nemeton::read_icd_pixel_series()` (with an essence).
#' @param valeur `"z"` or `"brut"`.
#' @param i18n Translator.
#' @return A plotly object.
#' @noRd
plot_icd_indices <- function(s, valeur = c("z", "brut"), i18n) {
  valeur <- match.arg(valeur)
  d <- s$serie
  d <- d[d$indice %in% s$indices_actifs, , drop = FALSE]
  d$val <- d[[valeur]]
  d$date <- .icd_date_mois(d$annee, d$mois)
  p <- plotly::plot_ly()
  vus <- character(0)
  for (ind in unique(d$indice)) {
    for (a in sort(unique(d$annee[d$indice == ind]))) {
      di <- d[d$indice == ind & d$annee == a, , drop = FALSE]
      di <- di[order(di$date), , drop = FALSE]
      premier <- !(ind %in% vus)
      vus <- c(vus, ind)
      p <- plotly::add_trace(
        p, data = di, x = ~date, y = ~val, type = "scatter",
        mode = "lines+markers", name = ind, legendgroup = ind,
        showlegend = premier, connectgaps = FALSE,
        line = list(color = unname(.ICD_COULEURS_INDICES[ind]), width = 1.5),
        marker = list(color = unname(.ICD_COULEURS_INDICES[ind]), size = 5),
        hovertemplate = paste0(ind, " %{x|%Y-%m} : %{y:.3f}<extra></extra>"))
    }
  }
  titre_y <- if (identical(valeur, "z")) i18n$t("icd_axe_z") else i18n$t("icd_axe_brut")
  plotly::layout(
    p,
    title = list(text = i18n$t("icd_g1_titre"), font = list(size = 14)),
    xaxis = list(title = "", type = "date"),
    yaxis = list(title = titre_y, zeroline = TRUE),
    shapes = .icd_formes_sensibles(sort(unique(d$annee)), s$mois_sensibles),
    legend = list(orientation = "h", y = -0.15),
    margin = list(t = 40))
}

#' Chart 2: the ICD of a pixel
#'
#' Monthly ICD per year with the three thresholds as horizontal lines, the
#' current (provisional) year dashed with hollow points, and the alert months
#' emphasised.
#'
#' @param s List from `nemeton::read_icd_pixel_series()`.
#' @param i18n Translator.
#' @return A plotly object.
#' @noRd
plot_icd_composite <- function(s, i18n) {
  d <- s$icd
  d$date <- .icd_date_mois(d$annee, d$mois)
  d <- d[!is.na(d$icd), , drop = FALSE]
  prov <- s$annee_provisoire
  p <- plotly::plot_ly()
  for (a in sort(unique(d$annee))) {
    da <- d[d$annee == a, , drop = FALSE]
    da <- da[order(da$date), , drop = FALSE]
    provisoire <- isTRUE(a == prov)
    nom <- if (provisoire) sprintf("%d (%s)", a, i18n$t("icd_provisoire")) else as.character(a)
    p <- plotly::add_trace(
      p, data = da, x = ~date, y = ~icd, type = "scatter",
      mode = "lines+markers", name = nom, legendgroup = as.character(a),
      line = list(width = if (provisoire) 2 else 1.5,
                  dash = if (provisoire) "dash" else "solid"),
      marker = list(size = 7,
                    symbol = if (provisoire) "circle-open" else "circle"),
      hovertemplate = paste0(nom, " %{x|%Y-%m} : %{y:.2f}<extra></extra>"))
  }
  # Mois d'alerte : marqueur rouge a l'ICD du mois d'alerte de chaque annee.
  an <- s$annuel
  if (!is.null(an) && nrow(an)) {
    an <- an[!is.na(an$mois_alerte), , drop = FALSE]
    if (nrow(an)) {
      pts <- merge(
        data.frame(annee = an$annee, mois = as.integer(an$mois_alerte)),
        d[, c("annee", "mois", "icd", "date")], by = c("annee", "mois"))
      if (nrow(pts)) {
        p <- plotly::add_trace(
          p, data = pts, x = ~date, y = ~icd, type = "scatter", mode = "markers",
          name = i18n$t("icd_mois_alerte"), showlegend = TRUE,
          marker = list(color = "#d7191c", size = 13, symbol = "diamond",
                        line = list(color = "#000000", width = 1)),
          hovertemplate = paste0(i18n$t("icd_mois_alerte"),
                                 " %{x|%Y-%m} : %{y:.2f}<extra></extra>"))
      }
    }
  }
  seuils <- s$seuils
  etiq <- c(surveillance = "icd_seuil_surv", alerte = "icd_seuil_alerte",
            alerte_forte = "icd_seuil_forte")
  formes <- lapply(seq_along(seuils), function(i) list(
    type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y",
    y0 = unname(seuils[i]), y1 = unname(seuils[i]),
    line = list(color = c("#fdae61", "#f46d43", "#a50026")[i],
                width = 1.5, dash = "dot")))
  notes <- lapply(seq_along(seuils), function(i) list(
    xref = "paper", x = 1, xanchor = "right", yref = "y",
    y = unname(seuils[i]), yanchor = "bottom", showarrow = FALSE,
    text = sprintf("%s (%s)", i18n$t(etiq[[names(seuils)[i]]]),
                   format(unname(seuils[i]), decimal.mark = ".")),
    font = list(size = 10)))
  plotly::layout(
    p,
    title = list(text = i18n$t("icd_g2_titre"), font = list(size = 14)),
    xaxis = list(title = "", type = "date"),
    yaxis = list(title = i18n$t("icd_axe_icd"), zeroline = TRUE),
    shapes = c(.icd_formes_sensibles(sort(unique(d$annee)), s$mois_sensibles),
               formes),
    annotations = notes,
    legend = list(orientation = "h", y = -0.15),
    margin = list(t = 40))
}

# Resume d'en-tete de la modale : essence, poids utilises, classe et niveau de
# l'annee choisie, date d'alerte. Renvoie des lignes de texte deja traduites.
icd_entete_pixel <- function(s, annee, i18n) {
  ess <- sprintf("%s : %s", i18n$t("icd_entete_essence"),
                 i18n$t(paste0("icd_ess_", s$essence)))
  w <- s$poids
  poids <- sprintf("%s : %s", i18n$t("icd_entete_poids"),
                   paste(sprintf("%s %s", names(w),
                                 format(round(unname(w), 3), nsmall = 2)),
                         collapse = " \u00b7 "))
  an <- s$annuel
  ligne <- if (!is.null(an) && nrow(an)) an[an$annee == annee, , drop = FALSE] else NULL
  classe <- NULL; alerte <- NULL
  if (!is.null(ligne) && nrow(ligne)) {
    # Classe absente (annee sans mois valide) : libelle dedie, pas de cle `NA`.
    lib <- if (is.na(ligne$classe)) i18n$t("icd_cls_na")
           else i18n$t(paste0("icd_cls_", as.integer(ligne$classe)))
    niv <- if (is.na(ligne$niveau)) "" else sprintf(" (%s %d)", i18n$t("icd_entete_niveau"),
                                                    as.integer(ligne$niveau))
    classe <- sprintf("%s %d : %s%s", i18n$t("icd_entete_annee"),
                      as.integer(annee), lib, niv)
    if (!is.na(ligne$date_alerte)) {
      alerte <- sprintf("%s : %s", i18n$t("icd_entete_alerte"),
                        format(ligne$date_alerte, "%Y-%m"))
    }
  }
  c(ess, poids, classe, alerte)
}

# Légende bivariée 2D (matrice 3×3) — pure visualisation.
#
# Squelette prêt à câbler sur la carte « Contexte régional (E-OBS) » dès que le
# cœur livrera un raster de classe bivariée (`classe_bivariee` 1..9, terciles
# T°max × terciles précipitations) + `meta$palette_matrix` / `meta$axis_*`.
# AUCUN classement ici (règle 1) : le classement/les bornes viennent du cœur ;
# l'app ne fait que dessiner la grille et coder l'axe.
#
# Convention d'indexation (alignée sur `classe_bivariee` 1..9 du cœur) :
#   classe = (rang_tx - 1) * 3 + rang_rr, rang ∈ {1,2,3} (1 = bas tercile).
#   -> colonnes = précipitations (rr) croissantes vers la DROITE,
#      lignes    = température (tx) croissante vers le HAUT.
#   Coin critique « chaud + sec » = tx haut (3) × rr bas (1) = classe 7.

# Palette bivariée par défaut (repli tant que `meta$palette_matrix` est absent).
# Schéma « chaud+sec = rouge foncé » (critique), « frais+humide = bleu-vert ».
# 9 couleurs indexées par classe 1..9 (cf. convention ci-dessus).
bivariate_palette_default <- function() {
  c(
    # tx bas (frais)
    "1" = "#c8e8c8", "2" = "#9bd3c0", "3" = "#5ab4bd",
    # tx moyen
    "4" = "#e6c9a8", "5" = "#b7a99a", "6" = "#6f8ba8",
    # tx haut (chaud)
    "7" = "#c0463b", "8" = "#9a5a6a", "9" = "#3b4994"
  )
}

#' Bivariate 2D legend (3×3 grid) as an htmltools tag
#'
#' Pure viz: renders a 3×3 colour matrix with two axis labels/arrows. Designed to
#' sit in a Leaflet `addControl()` or an overlay `div` on the context map.
#'
#' @param palette Named character(9) of hex colours indexed by class `"1".."9"`
#'   (core `meta$palette_matrix`), or `NULL` for the built-in default.
#' @param axis_x Label of the horizontal axis (precipitations, low->high right).
#' @param axis_y Label of the vertical axis (temperature, low->high up).
#' @param title Optional legend title.
#' @return An `htmltools` tag.
#' @noRd
bivariate_legend_html <- function(palette = NULL,
                                  axis_x = "Précipitations",
                                  axis_y = "T°max",
                                  title = NULL) {
  pal <- palette %||% bivariate_palette_default()
  cell <- function(cls) {
    col <- pal[[as.character(cls)]] %||% "#cccccc"
    htmltools::tags$div(
      style = sprintf(
        "width:20px;height:20px;background:%s;border:1px solid rgba(0,0,0,.15);", col),
      title = paste0("classe ", cls))
  }
  # Lignes de haut (tx fort) en bas (tx faible) ; colonnes rr croissantes à droite.
  # Ordre d'affichage : ligne du haut = tx=3 (chaud), du bas = tx=1 (frais).
  grid_rows <- lapply(c(3L, 2L, 1L), function(rt) {
    htmltools::tags$div(
      style = "display:flex;",
      lapply(c(1L, 2L, 3L), function(rr) cell((rt - 1L) * 3L + rr)))
  })

  htmltools::tags$div(
    class = "nmt-bivariate-legend",
    style = paste0("background:rgba(255,255,255,.85);padding:6px 8px;",
                   "border-radius:4px;font-size:11px;line-height:1;",
                   "box-shadow:0 1px 4px rgba(0,0,0,.3);"),
    if (!is.null(title)) htmltools::tags$div(
      style = "font-weight:600;margin-bottom:4px;", title),
    htmltools::tags$div(
      style = "display:flex;align-items:center;",
      # Axe Y (température) : flèche verticale + label pivoté.
      htmltools::tags$div(
        style = paste0("writing-mode:vertical-rl;transform:rotate(180deg);",
                       "text-align:center;margin-right:3px;white-space:nowrap;"),
        paste0("↑ ", axis_y)),
      htmltools::tags$div(
        style = "display:flex;flex-direction:column;", grid_rows)),
    # Axe X (précipitations) sous la grille.
    htmltools::tags$div(
      style = "text-align:center;margin-top:3px;margin-left:18px;white-space:nowrap;",
      paste0(axis_x, " →"))
  )
}

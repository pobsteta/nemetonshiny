# Légende bivariée 2D (matrice N×N) — pure visualisation.
#
# Câblée sur la carte « Contexte régional (E-OBS) ». Le cœur
# `nemeton::eobs_downscale_bivariate()` livre le raster `classe_bivariee`
# (1..N*N) + `meta$palette` (`colors`, `classes`, `ncol`). Il classe en 5 par axe
# (25 classes, `ncol = 5`), comme l'IF IGN. AUCUN classement ici (règle 1) : le
# classement/les bornes viennent du cœur ; l'app ne fait que dessiner la grille.
#
# Convention d'indexation (alignée sur `classe_bivariee` du cœur, N = ncol) :
#   classe = (rang_tx - 1) * N + rang_rr, rang ∈ {1..N} (1 = classe basse).
#   -> colonnes = précipitations (rr) croissantes vers la DROITE,
#      lignes    = température (tx) croissante vers le HAUT.

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

#' Bivariate 2D legend (N×N grid) as an htmltools tag
#'
#' Pure viz: renders an N×N colour matrix with two axis labels/arrows. Designed to
#' sit in a Leaflet `addControl()` or an overlay `div` on the context map. The
#' grid side N follows the core palette: `nemeton::eobs_downscale_bivariate()`
#' returns 5 classes per axis (25 classes, `meta$palette$ncol = 5`). Pass that
#' `ncol`; otherwise N is inferred from the palette length (sqrt), falling back to
#' 3×3 for the built-in default.
#'
#' @param palette Named character of hex colours indexed by class `"1".."N*N"`
#'   (core `meta$palette$colors`), or `NULL` for the built-in 3×3 default.
#' @param axis_x Label of the horizontal axis (precipitations, low->high right).
#' @param axis_y Label of the vertical axis (temperature, low->high up).
#' @param title Optional legend title.
#' @param ncol Grid side N (core `meta$palette$ncol`); inferred from the palette
#'   length when `NULL`.
#' @return An `htmltools` tag.
#' @noRd
bivariate_legend_html <- function(palette = NULL,
                                  axis_x = "Précipitations",
                                  axis_y = "T°max",
                                  title = NULL,
                                  ncol = NULL) {
  pal <- palette %||% bivariate_palette_default()
  # N = côté de la grille. Priorité au `ncol` fourni par le cœur ; sinon racine
  # carrée du nombre de couleurs (25 -> 5, 9 -> 3). Repli 3 si indéterminable.
  n <- ncol %||% round(sqrt(length(pal)))
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L) n <- 3L
  # Cellules plus petites au-delà de 3×3 pour garder une légende compacte.
  px <- if (n > 3L) 16L else 20L
  cell <- function(cls) {
    col <- pal[[as.character(cls)]] %||% "#cccccc"
    htmltools::tags$div(
      style = sprintf(
        "width:%dpx;height:%dpx;background:%s;border:1px solid rgba(0,0,0,.15);",
        px, px, col),
      title = paste0("classe ", cls))
  }
  # Lignes tx de haut (fort) en bas (faible) ; colonnes rr croissantes à droite.
  # class = (rang_tx - 1) * n + rang_rr, aligné sur `classe_bivariee` du cœur.
  grid_rows <- lapply(rev(seq_len(n)), function(rt) {
    htmltools::tags$div(
      style = "display:flex;",
      lapply(seq_len(n), function(rr) cell((rt - 1L) * n + rr)))
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

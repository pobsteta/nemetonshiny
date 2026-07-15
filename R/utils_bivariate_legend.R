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
#' @param zero Optional `list(tmax=, precip=)` — fractional position (in `[0,1]`,
#'   mesured from the LOW end) of the no-change (0) line on each axis, from core
#'   `meta$palette$zero`. Drawn as a white dashed line across the grid.
#' @param x_range,y_range Optional numeric of the horizontal (precip) / vertical
#'   (temperature) axis values, e.g. core `meta$breaks$precip` / `meta$breaks$tmax`.
#'   Their min/max annotate the axis ends.
bivariate_legend_html <- function(palette = NULL,
                                  axis_x = "Précipitations",
                                  axis_y = "T°max",
                                  title = NULL,
                                  ncol = NULL,
                                  zero = NULL,
                                  x_range = NULL,
                                  y_range = NULL) {
  pal <- palette %||% bivariate_palette_default()
  # N = côté de la grille. Priorité au `ncol` fourni par le cœur ; sinon racine
  # carrée du nombre de couleurs (25 -> 5, 9 -> 3). Repli 3 si indéterminable.
  n <- ncol %||% round(sqrt(length(pal)))
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L) n <- 3L
  # Cellules plus petites au-delà de 3×3 pour garder une légende compacte.
  px <- if (n > 3L) 16L else 20L
  W <- n * px; H <- n * px
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

  # Format court des valeurs d'axe (3 chiffres significatifs).
  fmtv <- function(v) { v <- suppressWarnings(as.numeric(v)); v <- v[is.finite(v)]
    if (!length(v)) "" else format(signif(v, 3), trim = TRUE) }
  frac <- function(z, k) { if (is.null(z)) return(NA_real_)
    v <- suppressWarnings(as.numeric(z[[k]])); if (length(v) != 1L) NA_real_ else v }

  # Lignes « zéro » (pas de changement) en pointillé blanc, position fractionnaire
  # fournie par le cœur (meta$palette$zero), mesurée depuis l'extrémité BASSE :
  # rr croît vers la droite (gauche = bas), tx croît vers le haut (bas = bas).
  zp <- frac(zero, "precip"); zt <- frac(zero, "tmax")
  overlay <- list()
  if (is.finite(zp)) overlay <- c(overlay, list(htmltools::tags$div(
    style = sprintf(paste0("position:absolute;top:0;height:%dpx;left:%.1fpx;",
      "border-left:1.5px dashed #fff;pointer-events:none;"), H, zp * W))))
  if (is.finite(zt)) overlay <- c(overlay, list(htmltools::tags$div(
    style = sprintf(paste0("position:absolute;left:0;width:%dpx;bottom:%.1fpx;",
      "border-top:1.5px dashed #fff;pointer-events:none;"), W, zt * H))))

  grid_box <- htmltools::tags$div(
    style = sprintf("position:relative;width:%dpx;height:%dpx;", W, H),
    htmltools::tags$div(style = "display:flex;flex-direction:column;", grid_rows),
    overlay)

  # Axe Y (température) : max en haut / label pivoté / min en bas, hauteur = grille.
  has_y <- any(is.finite(suppressWarnings(as.numeric(y_range))))
  y_col <- htmltools::tags$div(
    style = sprintf(paste0("display:flex;flex-direction:column;height:%dpx;",
      "align-items:flex-end;margin-right:3px;font-size:9px;%s"),
      H, if (has_y) "justify-content:space-between;" else "justify-content:center;"),
    if (has_y) htmltools::tags$div(fmtv(max(as.numeric(y_range), na.rm = TRUE))),
    htmltools::tags$div(
      style = paste0("writing-mode:vertical-rl;transform:rotate(180deg);",
                     "white-space:nowrap;font-size:11px;"),
      paste0("↑ ", axis_y)),
    if (has_y) htmltools::tags$div(fmtv(min(as.numeric(y_range), na.rm = TRUE))))

  # Axe X (précipitations) sous la grille (largeur = grille) : min / label / max.
  has_x <- any(is.finite(suppressWarnings(as.numeric(x_range))))
  x_row <- htmltools::tags$div(
    style = sprintf(paste0("display:flex;align-items:center;width:%dpx;margin-top:3px;",
      "font-size:9px;%s"), W,
      if (has_x) "justify-content:space-between;" else "justify-content:center;"),
    if (has_x) htmltools::tags$span(fmtv(min(as.numeric(x_range), na.rm = TRUE))),
    htmltools::tags$span(style = "font-size:11px;white-space:nowrap;",
                         paste0(axis_x, " →")),
    if (has_x) htmltools::tags$span(fmtv(max(as.numeric(x_range), na.rm = TRUE))))

  htmltools::tags$div(
    class = "nmt-bivariate-legend",
    style = paste0("background:rgba(255,255,255,.85);padding:6px 8px;",
                   "border-radius:4px;font-size:11px;line-height:1;",
                   "box-shadow:0 1px 4px rgba(0,0,0,.3);"),
    if (!is.null(title)) htmltools::tags$div(
      style = "font-weight:600;margin-bottom:4px;", title),
    htmltools::tags$div(
      style = "display:flex;align-items:stretch;",
      y_col,
      htmltools::tags$div(
        style = "display:flex;flex-direction:column;", grid_box, x_row)))
}

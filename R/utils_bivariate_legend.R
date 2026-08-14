# Legende bivariee 2D (matrice NxN) - pure visualisation.
#
# Cablee sur la carte " Contexte regional (E-OBS) ". Le coeur
# `nemeton::eobs_downscale_bivariate()` livre le raster `classe_bivariee`
# (1..N*N) + `meta$palette` (`colors`, `classes`, `ncol`). Il classe en 5 par axe
# (25 classes, `ncol = 5`), comme l'IF IGN. AUCUN classement ici (regle 1) : le
# classement/les bornes viennent du coeur ; l'app ne fait que dessiner la grille.
#
# Convention d'indexation (alignee sur `classe_bivariee` du coeur, N = ncol) :
#   classe = (rang_tx - 1) * N + rang_rr, rang dans {1..N} (1 = classe basse).
#   -> colonnes = precipitations (rr) croissantes vers la DROITE,
#      lignes    = temperature (tx) croissante vers le HAUT.

# Palette bivariee par defaut (repli tant que `meta$palette_matrix` est absent).
# Schema " chaud+sec = rouge fonce " (critique), " frais+humide = bleu-vert ".
# 9 couleurs indexees par classe 1..9 (cf. convention ci-dessus).
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

#' Bivariate 2D legend (NxN grid) as an htmltools tag
#'
#' Pure viz: renders an NxN colour matrix with two axis labels/arrows. Designed to
#' sit in a Leaflet `addControl()` or an overlay `div` on the context map. The
#' grid side N follows the core palette: `nemeton::eobs_downscale_bivariate()`
#' returns 5 classes per axis (25 classes, `meta$palette$ncol = 5`). Pass that
#' `ncol`; otherwise N is inferred from the palette length (sqrt), falling back to
#' 3x3 for the built-in default.
#'
#' @param palette Named character of hex colours indexed by class `"1".."N*N"`
#'   (core `meta$palette$colors`), or `NULL` for the built-in 3x3 default.
#' @param axis_x Label of the horizontal axis (precipitations, low->high right).
#' @param axis_y Label of the vertical axis (temperature, low->high up).
#' @param title Optional legend title (main line, bold).
#' @param subtitle Optional secondary line under the title (smaller, regular),
#'   e.g. the parenthetical variable pair "(Tdegmax estivale x precipitations)".
#' @param ncol Grid side N (core `meta$palette$ncol`); inferred from the palette
#'   length when `NULL`.
#' @return An `htmltools` tag.
#' @noRd
#' @param zero Optional `list(tmax=, precip=)` - fractional position (in `[0,1]`,
#'   mesured from the LOW end) of the no-change (0) line on each axis, from core
#'   `meta$palette$zero`. Drawn as a white dashed line across the grid.
#' @param x_range,y_range Optional numeric of the horizontal (precip) / vertical
#'   (temperature) axis values, e.g. core `meta$breaks$precip` / `meta$breaks$tmax`.
#'   Their min/max annotate the axis ends.
bivariate_legend_html <- function(palette = NULL,
                                  axis_x = "Pr\u00e9cipitations",
                                  axis_y = "T\u00b0max",
                                  title = NULL,
                                  subtitle = NULL,
                                  ncol = NULL,
                                  zero = NULL,
                                  x_range = NULL,
                                  y_range = NULL) {
  pal <- palette %||% bivariate_palette_default()
  # N = cote de la grille. Priorite au `ncol` fourni par le coeur ; sinon racine
  # carree du nombre de couleurs (25 -> 5, 9 -> 3). Repli 3 si indeterminable.
  n <- ncol %||% round(sqrt(length(pal)))
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L) n <- 3L
  # Cellules un peu plus petites au-dela de 3x3, mais assez grandes pour rester
  # lisibles sans ascenseur (la classe `nmt-bivariate-control` retire le plafond
  # de hauteur du controle leaflet, cf. custom.css).
  px <- if (n > 3L) 22L else 20L
  W <- n * px; H <- n * px
  cell <- function(cls) {
    col <- pal[[as.character(cls)]] %||% "#cccccc"
    htmltools::tags$div(
      style = sprintf(
        "width:%dpx;height:%dpx;background:%s;border:1px solid rgba(0,0,0,.15);",
        px, px, col),
      title = paste0("classe ", cls))
  }
  # Lignes tx de haut (fort) en bas (faible) ; colonnes rr croissantes a droite.
  # class = (rang_tx - 1) * n + rang_rr, aligne sur `classe_bivariee` du coeur.
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

  # Lignes " zero " (pas de changement) en pointille blanc, position fractionnaire
  # fournie par le coeur (meta$palette$zero), mesuree depuis l'extremite BASSE :
  # rr croit vers la droite (gauche = bas), tx croit vers le haut (bas = bas).
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

  # Axe Y (temperature) : max en haut / label pivote / min en bas, hauteur = grille.
  has_y <- any(is.finite(suppressWarnings(as.numeric(y_range))))
  y_col <- htmltools::tags$div(
    style = sprintf(paste0("display:flex;flex-direction:column;height:%dpx;",
      "align-items:flex-end;margin-right:3px;font-size:9px;%s"),
      H, if (has_y) "justify-content:space-between;" else "justify-content:center;"),
    if (has_y) htmltools::tags$div(fmtv(max(as.numeric(y_range), na.rm = TRUE))),
    htmltools::tags$div(
      style = paste0("writing-mode:vertical-rl;transform:rotate(180deg);",
                     "white-space:nowrap;font-size:11px;"),
      paste0("\u2191 ", axis_y)),
    if (has_y) htmltools::tags$div(fmtv(min(as.numeric(y_range), na.rm = TRUE))))

  # Axe X (precipitations) sous la grille (largeur = grille) : min / label / max.
  has_x <- any(is.finite(suppressWarnings(as.numeric(x_range))))
  x_row <- htmltools::tags$div(
    style = sprintf(paste0("display:flex;align-items:center;width:%dpx;margin-top:3px;",
      "font-size:9px;%s"), W,
      if (has_x) "justify-content:space-between;" else "justify-content:center;"),
    if (has_x) htmltools::tags$span(fmtv(min(as.numeric(x_range), na.rm = TRUE))),
    htmltools::tags$span(style = "font-size:11px;white-space:nowrap;",
                         paste0(axis_x, " \u2192")),
    if (has_x) htmltools::tags$span(fmtv(max(as.numeric(x_range), na.rm = TRUE))))

  htmltools::tags$div(
    class = "nmt-bivariate-legend",
    style = paste0("background:rgba(255,255,255,.85);padding:6px 8px;",
                   "border-radius:4px;font-size:11px;line-height:1;",
                   "box-shadow:0 1px 4px rgba(0,0,0,.3);"),
    if (!is.null(title)) htmltools::tags$div(
      style = if (is.null(subtitle)) "font-weight:600;margin-bottom:4px;" else "font-weight:600;",
      title),
    if (!is.null(subtitle)) htmltools::tags$div(
      style = "font-size:9px;font-weight:400;color:#333;margin-bottom:4px;", subtitle),
    htmltools::tags$div(
      style = "display:flex;align-items:stretch;",
      y_col,
      htmltools::tags$div(
        style = "display:flex;flex-direction:column;", grid_box, x_row)))
}

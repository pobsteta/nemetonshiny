# fct_plot_desserte_profil.R - planche du profil en travers d'un troncon.
#
# Presentation PURE (CLAUDE.md regles 1-3) : consomme la sortie deja derivee de
# `foretaccess::profil_travers()` et n'y recalcule rien. Meme contrat que
# `fct_plot_pixel_dieback.R`. Toute mesure - largeurs, bords, parabole - vient
# du coeur ; ici on ne fait que la dessiner.

#' Couleurs des cinq familles de bords
#'
#' Emboitees par construction cote coeur (`drivable` inclus dans `road`, inclus
#' dans `rescue`, inclus dans `right_of_way`) : la palette suit cet ordre, du
#' plus etroit au plus large, pour que la lecture de la planche soit monotone.
#' Teintes distinctes de celles des CLASSES de desserte (`DESS_CLASSE_COLS`),
#' qui parlent d'autre chose sur la meme carte.
#' @noRd
PROFIL_BORD_COLS <- c(
  drivable     = "#1B7837",   # vert   - chaussee roulable
  road         = "#762A83",   # violet - plateforme
  rescue       = "#E08214",   # orange - bande de secours
  right_of_way = "#2166AC",   # bleu   - emprise
  shoulder     = "#B2182B"    # rouge  - accotements
)

#' Cross-section plate of a road segment (plotly)
#'
#' @description
#' Draws what [foretaccess::profil_travers()] returned: the LiDAR slice, the
#' ground profile, the fitted crown, and the edge families with their widths.
#'
#' The point cloud is coloured by **height above ground** and not by intensity:
#' the section is read vertically (what stands on either side of the platform),
#' and height answers that question directly. Ground points are drawn apart, in
#' a neutral tone, because they carry the profile rather than the vegetation.
#'
#' @param profil The `profil_travers()` list (`status == "success"`).
#' @param i18n Translator.
#' @return A `plotly` object, or `NULL` when `profil` carries no points.
#' @noRd
plot_desserte_profil <- function(profil, i18n) {
  if (!is.list(profil) || is.null(profil$points) || !nrow(profil$points)) {
    return(NULL)
  }
  pts <- profil$points
  sol <- profil$sol
  bords <- profil$bords

  sol_pts <- pts[isTRUE(pts$sol) | pts$sol %in% TRUE, , drop = FALSE]
  haut_pts <- pts[!(pts$sol %in% TRUE), , drop = FALSE]

  p <- plotly::plot_ly()

  # 1. Nuage hors sol, colore par hauteur : c'est ce qui borde la plateforme.
  if (nrow(haut_pts)) {
    p <- plotly::add_markers(
      p, data = haut_pts, x = ~x_travers, y = ~z,
      marker = list(size = 3, color = ~z, colorscale = "Viridis",
                    showscale = TRUE,
                    colorbar = list(title = i18n$t("profil_legend_hauteur"),
                                    len = 0.4, y = 0.85)),
      name = i18n$t("profil_serie_vegetation"),
      hovertemplate = paste0("%{x:.1f} m | %{y:.1f} m<extra></extra>"))
  }
  # 2. Points sol, a part : ils portent le profil, pas le couvert.
  if (nrow(sol_pts)) {
    p <- plotly::add_markers(
      p, data = sol_pts, x = ~x_travers, y = ~z,
      marker = list(size = 3, color = "#4D4D4D"),
      name = i18n$t("profil_serie_sol"),
      hovertemplate = paste0("%{x:.1f} m | %{y:.1f} m<extra></extra>"))
  }
  # 3. Profil du terrain, en trait continu.
  if (is.data.frame(sol) && nrow(sol)) {
    p <- plotly::add_lines(
      p, data = sol[order(sol$x_travers), , drop = FALSE],
      x = ~x_travers, y = ~z,
      line = list(color = "#000000", width = 2),
      name = i18n$t("profil_serie_terrain"))
  }
  # 4. Parabole de chaussee, tracee sur la seule largeur roulable : au-dela elle
  #    n'a plus de sens physique, et une extrapolation se lirait comme une mesure.
  aj <- profil$ajustement
  if (is.list(aj) && all(c("a", "b", "c") %in% names(aj))) {
    dr <- bords[bords$type %in% "drivable", , drop = FALSE]
    if (nrow(dr)) {
      xs <- seq(min(dr$x_gauche), max(dr$x_droite), length.out = 60)
      p <- plotly::add_lines(
        p, x = xs, y = aj$a * xs^2 + aj$b * xs + aj$c,
        line = list(color = PROFIL_BORD_COLS[["drivable"]], width = 3),
        name = sprintf("%s (RMSE %.2f m)", i18n$t("profil_serie_chaussee"),
                       aj$rmse %||% NA_real_))
    }
  }
  # 5. Bords : un segment horizontal cote par famille, empiles vers le haut pour
  #    qu'ils ne se recouvrent pas. L'ordre suit l'emboitement.
  if (is.data.frame(bords) && nrow(bords)) {
    ordre <- c("drivable", "road", "rescue", "right_of_way", "shoulder")
    bords <- bords[order(match(bords$type, ordre)), , drop = FALSE]
    y_max <- max(c(pts$z, 0), na.rm = TRUE)
    for (i in seq_len(nrow(bords))) {
      b <- bords[i, ]
      y <- y_max * (0.55 + 0.09 * i)
      col <- unname(PROFIL_BORD_COLS[b$type])
      if (is.na(col)) col <- "#666666"
      p <- plotly::add_lines(
        p, x = c(b$x_gauche, b$x_droite), y = c(y, y),
        line = list(color = col, width = 2, dash = "dot"),
        name = sprintf("%s - %.1f m", i18n$t(paste0("profil_bord_", b$type)),
                       b$largeur_m),
        hoverinfo = "name")
    }
  }
  # 6. L'axe du troncon : origine des abscisses, donc repere de lecture.
  p <- plotly::add_lines(
    p, x = c(0, 0),
    y = c(min(c(pts$z, 0), na.rm = TRUE), max(c(pts$z, 0), na.rm = TRUE)),
    line = list(color = "#999999", width = 1, dash = "dash"),
    name = i18n$t("profil_serie_axe"), hoverinfo = "name")

  plotly::layout(
    p,
    xaxis = list(title = i18n$t("profil_axe_x"), zeroline = FALSE),
    yaxis = list(title = i18n$t("profil_axe_y"), zeroline = TRUE,
                 scaleanchor = "x", scaleratio = 1),
    legend = list(orientation = "v", x = 1.02, y = 0.4),
    margin = list(t = 30))
}

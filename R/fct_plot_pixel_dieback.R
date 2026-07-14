#' Police des annotations pédagogiques de la planche pixel.
#'
#' 14 px (et non 10) + gris foncé : lisible en plein écran, où plotly
#' agrandit la planche mais PAS la police (taille fixe en px).
#' @noRd
.PIXEL_ANNOT_FONT <- list(size = 14, color = "#333333")

#' Multi-year pixel dieback plate (CRswir + CRre), plotly
#'
#' Pure presentation helper (CLAUDE.md rules 1-3: NO business logic here).
#' Consumes the ALREADY-derived output of
#' [nemeton::prepare_pixel_dieback_series()] and assembles a 4-panel plotly
#' plate. Nothing is (re)computed: every series, trough/peak, centroid and gap
#' comes straight from `prepared`.
#'
#' Panels:
#' * A — dual native Y axis (CRswir left `y`, CRre right `y2`): raw points
#'   (optional), smoothed curves, summer bands, interpolation-gap shading, and
#'   the summer trough (CRswir) / peak (CRre) trajectories distinguished by
#'   dash + symbol (non-colour redundancy).
#' * B / C — folded annual cycles (x = day-of-year), one line per year, cividis.
#' * D — CRswir x CRre state space, coloured by year (single colorbar) with the
#'   annual-centroid trajectory.
#'
#' @param prepared List returned by [nemeton::prepare_pixel_dieback_series()]:
#'   `grid_swir`, `grid_re`, `obs_swir`, `obs_re` (`date`, `val`, `year`,
#'   `doy`); `trough_swir`, `peak_re` (`year`, `date`, `val`); `state`
#'   (`date`, `year`, `val_sw`, `val_re`); `centroids` (`year`, `val_sw`,
#'   `val_re`); `gaps` (`from`, `to`).
#' @param opts List of display options: `show_points` (logical, default TRUE)
#'   toggles the raw-observation markers; `summer` (length-2 integer DOY, default
#'   `c(152L, 273L)`) drives the summer band shading only (cosmetic — must match
#'   the `summer` passed to the core preparer).
#' @param i18n App translator (`get_i18n()` result) or NULL. All labels go
#'   through it (rule 4).
#' @return A `plotly` htmlwidget (subplot of the 4 panels).
#' @noRd
plot_pixel_dieback <- function(prepared, opts = list(), i18n = NULL) {
  tr <- function(x) if (is.null(i18n)) x else i18n$t(x)
  show_points <- isTRUE(opts$show_points %||% TRUE)
  summer      <- opts$summer %||% c(152L, 273L)

  # Métadonnées du pixel (titre) : essence + lon/lat + modèle. Fournies par le
  # module au clic (attr(series, "species") / "v_model" + coordonnées du clic
  # carte) ; toutes facultatives. Le titre reprend la maquette (« Pixel de
  # <essence> — … ») et ajoute une 2ᵉ ligne lat/lon (parité graphique FORDEAD).
  .ok1 <- function(x) !is.null(x) && length(x) == 1L && !is.na(x)
  .sp  <- if (.ok1(opts$species) && nzchar(as.character(opts$species)))
    as.character(opts$species) else NULL
  .vm  <- if (.ok1(opts$v_model) && nzchar(as.character(opts$v_model)))
    as.character(opts$v_model) else NULL
  .lat <- if (.ok1(opts$lat) && is.numeric(opts$lat)) opts$lat else NULL
  .lng <- if (.ok1(opts$lng) && is.numeric(opts$lng)) opts$lng else NULL

  title_main <- paste0(
    if (!is.null(.sp)) sprintf(tr("pixel_title_of_species"), .sp)
    else tr("pixel_title_bare"),
    tr("pixel_title_suffix")
  )
  sub_bits <- c(
    if (!is.null(.lat) && !is.null(.lng))
      sprintf(tr("pixel_title_coords_fmt"), .lat, .lng),
    if (!is.null(.vm))
      sprintf("%s : %s", tr("monitoring_reconfort_model"), .vm)
  )
  title_txt <- if (length(sub_bits))
    paste0(title_main, "<br><span style='font-size:11px;color:#666'>",
           paste(sub_bits, collapse = " — "), "</span>")
  else title_main

  # Colours: CRswir / CRre fixed hues on panel A; cividis ramp for the
  # per-year traces (folded cycles + state space). Les deux trajectoires ont
  # une couleur DÉDIÉE (rouge = creux CRswir, violet = pic CRre) — cf. image
  # de référence — chacune reprise sur ses points, sa ligne et ses libellés.
  col_swir <- "#1F77B4"  # bleu (CRswir)
  col_re   <- "#2CA02C"  # vert (CRre)
  col_trough <- "#D62728"  # rouge — trajectoire du creux estival CRswir
  col_peak   <- "#7E3F9E"  # violet — trajectoire du pic estival CRre

  .df <- function(x) if (is.data.frame(x)) x else NULL
  grid_swir <- .df(prepared$grid_swir); grid_re <- .df(prepared$grid_re)
  obs_swir  <- .df(prepared$obs_swir);  obs_re  <- .df(prepared$obs_re)
  trough    <- .df(prepared$trough_swir); peak   <- .df(prepared$peak_re)
  state     <- .df(prepared$state);     centroids <- .df(prepared$centroids)
  gaps      <- .df(prepared$gaps)

  # Palette année partagée (discrète) : cividis mappé aux années présentes.
  yrs <- sort(unique(stats::na.omit(c(
    grid_swir$year, grid_re$year, state$year
  ))))
  if (!length(yrs)) yrs <- integer(0)
  year_pal <- stats::setNames(
    if (length(yrs)) viridisLite::cividis(length(yrs)) else character(0),
    as.character(yrs)
  )

  # ---------------------------------------------------------------
  # Shapes panneau A : bandes estivales (par année) + lacunes ombrées.
  # ---------------------------------------------------------------
  shapes <- list()
  for (y in yrs) {
    from <- as.Date(sprintf("%d-01-01", y)) + (as.integer(summer[1]) - 1L)
    to   <- as.Date(sprintf("%d-01-01", y)) + (as.integer(summer[2]) - 1L)
    shapes[[length(shapes) + 1L]] <- list(
      type = "rect", xref = "x", yref = "paper",
      x0 = format(from, "%Y-%m-%d"), x1 = format(to, "%Y-%m-%d"),
      y0 = 0, y1 = 1, fillcolor = "rgba(255, 214, 102, 0.16)",
      line = list(width = 0), layer = "below"
    )
  }
  if (!is.null(gaps) && nrow(gaps)) {
    for (k in seq_len(nrow(gaps))) {
      shapes[[length(shapes) + 1L]] <- list(
        type = "rect", xref = "x", yref = "paper",
        x0 = format(as.Date(gaps$from[k]), "%Y-%m-%d"),
        x1 = format(as.Date(gaps$to[k]),   "%Y-%m-%d"),
        y0 = 0, y1 = 1, fillcolor = "rgba(120, 120, 120, 0.14)",
        line = list(width = 0), layer = "below"
      )
    }
  }

  # ---------------------------------------------------------------
  # Panneau A — double axe Y natif.
  # ---------------------------------------------------------------
  pA <- plotly::plot_ly()
  if (show_points && !is.null(obs_swir) && nrow(obs_swir)) {
    pA <- plotly::add_markers(
      pA, data = obs_swir, x = ~date, y = ~val, yaxis = "y",
      name = tr("monitoring_reconfort_crswir"),
      legendgroup = "swir", showlegend = FALSE,
      marker = list(color = col_swir, size = 4, opacity = 0.3),
      hovertemplate = paste0("CRswir<br>%{x|%Y-%m-%d}<br>%{y:.3f}<extra></extra>")
    )
  }
  if (show_points && !is.null(obs_re) && nrow(obs_re)) {
    pA <- plotly::add_markers(
      pA, data = obs_re, x = ~date, y = ~val, yaxis = "y2",
      name = tr("monitoring_reconfort_crre"),
      legendgroup = "re", showlegend = FALSE,
      marker = list(color = col_re, size = 4, opacity = 0.3),
      hovertemplate = paste0("CRre<br>%{x|%Y-%m-%d}<br>%{y:.3f}<extra></extra>")
    )
  }
  if (!is.null(grid_swir) && nrow(grid_swir)) {
    pA <- plotly::add_lines(
      pA, data = grid_swir, x = ~date, y = ~val, yaxis = "y",
      name = tr("pixel_legend_crswir"), legendgroup = "swir",
      line = list(color = col_swir, width = 2.2),
      hovertemplate = paste0("CRswir<br>%{x|%Y-%m-%d}<br>%{y:.3f}<extra></extra>")
    )
  }
  # Trajectoire creux estival CRswir (rouge, pointillés, cercles, labels rouges
  # sous les points) — axe y.
  if (!is.null(trough) && nrow(trough)) {
    pA <- plotly::add_trace(
      pA, data = trough, x = ~date, y = ~val, yaxis = "y",
      type = "scatter", mode = "lines+markers+text",
      name = tr("pixel_trough_swir"),
      text = ~sprintf("%.2f", val), textposition = "bottom center",
      textfont = list(color = col_trough, size = 11),
      line = list(color = col_trough, width = 1.5, dash = "dash"),
      marker = list(color = col_trough, size = 8, symbol = "circle"),
      hovertemplate = paste0("<b>", tr("pixel_trough_swir"),
                             "</b><br>%{x|%Y-%m-%d}<br>%{y:.3f}<extra></extra>")
    )
  }
  if (!is.null(grid_re) && nrow(grid_re)) {
    pA <- plotly::add_lines(
      pA, data = grid_re, x = ~date, y = ~val, yaxis = "y2",
      name = tr("pixel_legend_crre"), legendgroup = "re",
      line = list(color = col_re, width = 2.2),
      hovertemplate = paste0("CRre<br>%{x|%Y-%m-%d}<br>%{y:.3f}<extra></extra>")
    )
  }
  # Trajectoire pic estival CRre (violet, pointillés, cercles, labels violets
  # au-dessus des points) — axe y2.
  if (!is.null(peak) && nrow(peak)) {
    pA <- plotly::add_trace(
      pA, data = peak, x = ~date, y = ~val, yaxis = "y2",
      type = "scatter", mode = "lines+markers+text",
      name = tr("pixel_peak_re"),
      text = ~sprintf("%.2f", val), textposition = "top center",
      textfont = list(color = col_peak, size = 11),
      line = list(color = col_peak, width = 1.5, dash = "dash"),
      marker = list(color = col_peak, size = 8, symbol = "circle"),
      hovertemplate = paste0("<b>", tr("pixel_peak_re"),
                             "</b><br>%{x|%Y-%m-%d}<br>%{y:.3f}<extra></extra>")
    )
  }
  pA <- plotly::layout(
    pA,
    shapes = shapes,
    xaxis  = list(title = "", type = "date"),
    yaxis  = list(title = list(text = tr("pixel_axis_crswir"),
                               font = list(color = col_swir)),
                  tickfont = list(color = col_swir)),
    yaxis2 = list(title = list(text = tr("pixel_axis_crre"),
                               font = list(color = col_re)),
                  tickfont = list(color = col_re),
                  overlaying = "y", side = "right", showgrid = FALSE),
    legend = list(x = 0.02, y = 0.60, xanchor = "left", yanchor = "top",
                  bgcolor = "rgba(255,255,255,0.7)",
                  bordercolor = "rgba(0,0,0,0.15)", borderwidth = 1,
                  font = list(size = 11))
  )

  # ---------------------------------------------------------------
  # Panneaux B & C — cycles annuels repliés (x = doy), 1 ligne/année.
  # ---------------------------------------------------------------
  # Bande estivale (orange) commune aux cycles repliés (x = doy).
  summer_band <- list(
    type = "rect", xref = "x", yref = "paper",
    x0 = as.integer(summer[1]), x1 = as.integer(summer[2]),
    y0 = 0, y1 = 1, fillcolor = "rgba(255, 214, 102, 0.20)",
    line = list(width = 0), layer = "below"
  )
  folded <- function(grid, y_title, annot) {
    p <- plotly::plot_ly()
    if (!is.null(grid) && nrow(grid)) {
      for (y in yrs) {
        sub <- grid[grid$year == y, , drop = FALSE]
        sub <- sub[order(sub$doy), , drop = FALSE]
        if (!nrow(sub)) next
        p <- plotly::add_lines(
          p, data = sub, x = ~doy, y = ~val,
          name = as.character(y), showlegend = FALSE,
          line = list(color = unname(year_pal[as.character(y)]), width = 1.5),
          hovertemplate = paste0(as.character(y),
                                 "<br>doy %{x}<br>%{y:.3f}<extra></extra>")
        )
      }
    }
    plotly::layout(
      p,
      shapes = list(summer_band),
      annotations = list(annot),
      xaxis = list(title = tr("pixel_doy_axis")),
      yaxis = list(title = y_title)
    )
  }
  # Annotations « pédagogiques » ancrées en coordonnées données (doy, valeur).
  # v0.106.4 — police 10 → 14 px, gris #666 → #333 : ces annotations portent la
  # LECTURE du diagnostic ; en plein écran plotly agrandit la planche mais garde
  # une police FIXE en px, elles devenaient donc illisibles.
  annot_trough <- list(
    x = 235, y = 0.72, xref = "x", yref = "y", showarrow = TRUE,
    ax = 20, ay = -30, arrowhead = 2, arrowsize = 1, arrowwidth = 1,
    arrowcolor = "#888888", text = tr("pixel_annot_trough"),
    font = .PIXEL_ANNOT_FONT, align = "left"
  )
  annot_peak <- list(
    x = 300, y = 0.72, xref = "x", yref = "y", showarrow = FALSE,
    text = tr("pixel_annot_peak"),
    font = .PIXEL_ANNOT_FONT, align = "left"
  )
  pB <- folded(grid_swir, "CRswir", annot_trough)
  pC <- folded(grid_re,   "CRre",   annot_peak)

  # ---------------------------------------------------------------
  # Panneau D — espace d'état CRswir x CRre, couleur = année (1 seule colorbar).
  # ---------------------------------------------------------------
  pD <- plotly::plot_ly()
  if (!is.null(state) && nrow(state)) {
    pD <- plotly::add_markers(
      pD, data = state, x = ~val_sw, y = ~val_re,
      color = ~year, colors = viridisLite::cividis(256),
      marker = list(size = 6, colorbar = list(
        title = list(text = tr("pixel_year")),
        orientation = "h", x = 0.5, xanchor = "center",
        y = -0.14, yanchor = "top", len = 0.42, thickness = 12,
        tickvals = if (length(yrs)) yrs else NULL,
        ticktext = if (length(yrs)) as.character(yrs) else NULL
      )),
      showlegend = FALSE,
      hovertemplate = paste0("%{marker.color}<br>CRswir %{x:.3f}",
                             "<br>CRre %{y:.3f}<extra></extra>")
    )
  }
  # Trajectoire des centroïdes annuels (losanges reliés en pointillés).
  if (!is.null(centroids) && nrow(centroids)) {
    cc <- centroids[order(centroids$year), , drop = FALSE]
    pD <- plotly::add_trace(
      pD, data = cc, x = ~val_sw, y = ~val_re,
      type = "scatter", mode = "lines+markers",
      name = tr("pixel_centroids"),
      line = list(color = "#333333", width = 1.5, dash = "dash"),
      marker = list(color = "#333333", size = 10, symbol = "diamond"),
      showlegend = FALSE,
      hovertemplate = paste0("<b>", tr("pixel_centroids"),
                             " %{text}</b><br>CRswir %{x:.3f}",
                             "<br>CRre %{y:.3f}<extra></extra>"),
      text = ~as.character(year)
    )
  }
  pD <- plotly::layout(
    pD,
    annotations = list(list(
      x = 0.85, y = 0.72, xref = "x", yref = "y", showarrow = FALSE,
      text = tr("pixel_annot_drift"),
      font = .PIXEL_ANNOT_FONT, align = "left"
    )),
    xaxis = list(title = tr("pixel_axis_crswir_water")),
    yaxis = list(title = tr("pixel_axis_crre_chloro"))
  )

  # ---------------------------------------------------------------
  # Assemblage : ligne du bas (B, C, D) puis empilement avec A.
  # Titres de sous-graphes via annotations (subplot n'en porte pas).
  # ---------------------------------------------------------------
  bottom <- plotly::subplot(pB, pC, pD, nrows = 1,
                            titleX = TRUE, titleY = TRUE, margin = 0.04)
  fig <- plotly::subplot(pA, bottom, nrows = 2,
                         heights = c(0.55, 0.45), margin = 0.06,
                         titleX = TRUE, titleY = TRUE)

  panel_title <- function(x, y, key) list(
    x = x, y = y, xref = "paper", yref = "paper",
    xanchor = "center", yanchor = "bottom", showarrow = FALSE,
    text = paste0("<b>", tr(key), "</b>"), font = list(size = 13)
  )
  fig <- plotly::layout(
    fig,
    title = list(text = title_txt, x = 0.02, y = 0.98,
                 font = list(size = 15)),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font = list(size = 13),
    margin = list(t = 78, b = 95, l = 55, r = 20),
    annotations = list(
      panel_title(0.14, 0.40, "pixel_cycles_swir"),
      panel_title(0.5,  0.40, "pixel_cycles_re"),
      panel_title(0.86, 0.40, "pixel_state_space")
    )
  )
  fig
}

# v0.106.4 — `.pixel_export_engine()` / `save_plotly_png()` supprimées avec
# le bouton « Exporter PNG » de la modale pixel RECONFORT (leur unique
# consommateur). La planche reste exportable depuis la barre d'outils
# native de plotly (icône appareil photo).

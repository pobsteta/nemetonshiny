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

  # Colours: CRswir / CRre fixed hues on panel A; cividis ramp for the
  # per-year traces (folded cycles + state space).
  col_swir <- "#2C7FB8"  # bleu
  col_re   <- "#2CA02C"  # vert
  col_tr   <- "#B15928"  # trajectoires creux/pic (brun, distinct des 2 indices)

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
      name = tr("monitoring_reconfort_crswir"), legendgroup = "swir",
      line = list(color = col_swir, width = 2),
      hovertemplate = paste0("CRswir<br>%{x|%Y-%m-%d}<br>%{y:.3f}<extra></extra>")
    )
  }
  if (!is.null(grid_re) && nrow(grid_re)) {
    pA <- plotly::add_lines(
      pA, data = grid_re, x = ~date, y = ~val, yaxis = "y2",
      name = tr("monitoring_reconfort_crre"), legendgroup = "re",
      line = list(color = col_re, width = 2),
      hovertemplate = paste0("CRre<br>%{x|%Y-%m-%d}<br>%{y:.3f}<extra></extra>")
    )
  }
  # Trajectoire creux estival CRswir (dash + cercle + label) — axe y.
  if (!is.null(trough) && nrow(trough)) {
    pA <- plotly::add_trace(
      pA, data = trough, x = ~date, y = ~val, yaxis = "y",
      type = "scatter", mode = "lines+markers+text",
      name = tr("pixel_trough_swir"),
      text = ~sprintf("%.2f", val), textposition = "top center",
      line = list(color = col_tr, width = 1.5, dash = "dash"),
      marker = list(color = col_tr, size = 9, symbol = "circle"),
      hovertemplate = paste0("<b>", tr("pixel_trough_swir"),
                             "</b><br>%{x|%Y-%m-%d}<br>%{y:.3f}<extra></extra>")
    )
  }
  # Trajectoire pic estival CRre (dashdot + triangle) — axe y2.
  if (!is.null(peak) && nrow(peak)) {
    pA <- plotly::add_trace(
      pA, data = peak, x = ~date, y = ~val, yaxis = "y2",
      type = "scatter", mode = "lines+markers+text",
      name = tr("pixel_peak_re"),
      text = ~sprintf("%.2f", val), textposition = "bottom center",
      line = list(color = col_tr, width = 1.5, dash = "dashdot"),
      marker = list(color = col_tr, size = 9, symbol = "triangle-up"),
      hovertemplate = paste0("<b>", tr("pixel_peak_re"),
                             "</b><br>%{x|%Y-%m-%d}<br>%{y:.3f}<extra></extra>")
    )
  }
  pA <- plotly::layout(
    pA,
    shapes = shapes,
    xaxis  = list(title = tr("monitoring_timeseries_xaxis"), type = "date"),
    yaxis  = list(title = tr("monitoring_reconfort_crswir")),
    yaxis2 = list(title = tr("monitoring_reconfort_crre"),
                  overlaying = "y", side = "right", showgrid = FALSE),
    legend = list(orientation = "h", y = -0.2)
  )

  # ---------------------------------------------------------------
  # Panneaux B & C — cycles annuels repliés (x = doy), 1 ligne/année.
  # ---------------------------------------------------------------
  folded <- function(grid, y_title) {
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
      xaxis = list(title = tr("pixel_doy_axis")),
      yaxis = list(title = y_title)
    )
  }
  pB <- folded(grid_swir, tr("monitoring_reconfort_crswir"))
  pC <- folded(grid_re,   tr("monitoring_reconfort_crre"))

  # ---------------------------------------------------------------
  # Panneau D — espace d'état CRswir x CRre, couleur = année (1 seule colorbar).
  # ---------------------------------------------------------------
  pD <- plotly::plot_ly()
  if (!is.null(state) && nrow(state)) {
    pD <- plotly::add_markers(
      pD, data = state, x = ~val_sw, y = ~val_re,
      color = ~year, colors = viridisLite::cividis(256),
      marker = list(size = 6, colorbar = list(title = tr("pixel_year"))),
      showlegend = FALSE,
      hovertemplate = paste0("%{marker.color}<br>CRswir %{x:.3f}",
                             "<br>CRre %{y:.3f}<extra></extra>")
    )
  }
  if (!is.null(centroids) && nrow(centroids)) {
    cc <- centroids[order(centroids$year), , drop = FALSE]
    pD <- plotly::add_trace(
      pD, data = cc, x = ~val_sw, y = ~val_re,
      type = "scatter", mode = "lines+markers+text",
      name = tr("pixel_centroids"),
      text = ~as.character(year), textposition = "top center",
      line = list(color = "#333333", width = 1.5),
      marker = list(color = "#333333", size = 10, symbol = "diamond"),
      showlegend = FALSE,
      hovertemplate = paste0("<b>", tr("pixel_centroids"),
                             " %{text}</b><br>CRswir %{x:.3f}",
                             "<br>CRre %{y:.3f}<extra></extra>")
    )
  }
  pD <- plotly::layout(
    pD,
    xaxis = list(title = tr("monitoring_reconfort_crswir")),
    yaxis = list(title = tr("monitoring_reconfort_crre"))
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
    title = list(text = tr("pixel_planche_title"), x = 0.02, y = 0.99,
                 font = list(size = 15)),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font = list(size = 13),
    margin = list(t = 60, b = 40, l = 55, r = 20),
    annotations = list(
      panel_title(0.14, 0.40, "pixel_cycles_swir"),
      panel_title(0.5,  0.40, "pixel_cycles_re"),
      panel_title(0.86, 0.40, "pixel_state_space")
    )
  )
  fig
}

#' Static-image export engine available for a plotly figure, or NA
#'
#' Prefers `kaleido` (via reticulate, already a dependency — no browser, no
#' server) then falls back to `webshot2` (headless Chrome). Returns NA when
#' neither is installed so callers can hide the export affordance.
#' @return `"kaleido"`, `"webshot2"`, or `NA_character_`.
#' @noRd
.pixel_export_engine <- function() {
  if (requireNamespace("reticulate", quietly = TRUE) &&
      isTRUE(tryCatch(reticulate::py_module_available("kaleido"),
                      error = function(e) FALSE))) {
    return("kaleido")
  }
  if (requireNamespace("webshot2", quietly = TRUE)) return("webshot2")
  NA_character_
}

#' Export a plotly figure to a static PNG (kaleido, webshot2 fallback)
#'
#' Pure IO helper — reusable by any static-export path (download button,
#' future Quarto report section). No plotting logic here: it renders an
#' already-built plotly figure. Degrades gracefully: returns FALSE (and warns)
#' when no export engine is available or the render fails.
#'
#' @param fig A `plotly` figure (e.g. from [plot_pixel_dieback()]).
#' @param path Output PNG path.
#' @param width,height Pixel dimensions.
#' @return `TRUE` on success (file written), `FALSE` otherwise.
#' @noRd
save_plotly_png <- function(fig, path, width = 1600L, height = 1000L) {
  engine <- .pixel_export_engine()
  if (is.na(engine)) {
    cli::cli_warn("No static-image engine (kaleido/webshot2) for PNG export.")
    return(FALSE)
  }
  ok <- tryCatch({
    if (identical(engine, "kaleido")) {
      plotly::save_image(fig, file = path, width = width, height = height)
    } else {
      tmp_html <- tempfile(fileext = ".html")
      on.exit(unlink(tmp_html), add = TRUE)
      htmlwidgets::saveWidget(plotly::as_widget(fig), tmp_html,
                              selfcontained = TRUE)
      webshot2::webshot(tmp_html, file = path,
                        vwidth = width, vheight = height)
    }
    TRUE
  }, error = function(e) {
    cli::cli_warn("Pixel dieback PNG export failed ({engine}): {conditionMessage(e)}")
    FALSE
  })
  isTRUE(ok) && file.exists(path)
}

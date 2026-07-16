# Graphiques au clic sur la carte « Contexte régional (E-OBS) » — spec 036.
#
# Pure visualisation (plotly). AUCUN calcul métier ici (règle 1) : la série
# estivale, la climatologie mensuelle et l'ajustement de tendance viennent des
# accesseurs cœur (`nemeton::eobs_summer_series` / `eobs_monthly_climatology` /
# `eobs_trend_fit`). Ces helpers ne font que METTRE EN FORME des `data.frame`
# déjà extraits à la maille cliquée. Tous NA-safe : hors emprise E-OBS, la série
# est intégralement `NA` → on rend un panneau « hors couverture » plutôt qu'un
# graphe vide.

# Panneau plotly minimal affichant un message centré (hors couverture, données
# manquantes). Sert de repli commun aux 4 tracés.
.regen_ctx_empty_plot <- function(msg) {
  plotly::layout(
    plotly::plot_ly(type = "scatter", mode = "none"),
    xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
    annotations = list(list(text = msg, showarrow = FALSE, xref = "paper",
      yref = "paper", x = 0.5, y = 0.5, font = list(size = 14, color = "#666"))))
}

# Une série (data.frame year/value) est exploitable si elle a >= 2 valeurs finies.
.regen_ctx_series_ok <- function(ser) {
  is.data.frame(ser) && nrow(ser) > 0L &&
    sum(is.finite(suppressWarnings(as.numeric(ser$value)))) >= 2L
}

# Statistiques ombrothermiques (Gaussen-Bagnouls + De Martonne) à partir des deux
# climatologies mensuelles (précip mm/mois, température °C/mois). PURE viz-support :
#   - mois secs de Gaussen : P < 2T (précip mensuelle sous deux fois la T°) ;
#   - indice de De Martonne annuel : sum(P) / (mean(T) + 10).
# NA-safe : un mois dont P ou T manque n'est pas compté sec ; indices `NA` si la
# jointure est vide. Renvoie `list(dry_idx, dry_months, demartonne)`.
.regen_ctx_ombro_stats <- function(clim_rr, clim_t) {
  empty <- list(dry_idx = integer(0), dry_months = NA_integer_, demartonne = NA_real_)
  if (!is.data.frame(clim_rr) || !is.data.frame(clim_t)) return(empty)
  m <- merge(
    data.frame(month = clim_rr$month, P = suppressWarnings(as.numeric(clim_rr$value))),
    data.frame(month = clim_t$month,  Tm = suppressWarnings(as.numeric(clim_t$value))),
    by = "month", all = FALSE)
  if (!nrow(m)) return(empty)
  m <- m[order(m$month), , drop = FALSE]
  dry <- is.finite(m$P) & is.finite(m$Tm) & (m$P < 2 * m$Tm)
  tot_p  <- sum(m$P,  na.rm = TRUE)
  mean_t <- mean(m$Tm[is.finite(m$Tm)])
  list(
    dry_idx    = m$month[dry],
    dry_months = sum(dry, na.rm = TRUE),
    demartonne = if (is.finite(mean_t)) tot_p / (mean_t + 10) else NA_real_)
}

# Graphe 1 — série estivale + droite de tendance. La pente AFFICHÉE est
# `fit$slope_decade` (= la couleur de la maille), jamais un `lm` ré-estimé.
# `unit` : suffixe d'annotation de pente ("°C/déc" ou "mm/déc").
# Titre d'un graphe rendu comme ANNOTATION `paper` (haut-centre) plutôt que comme
# `layout(title=)`. Raison : `plotly::subplot()` ne conserve qu'UN titre global
# (le dernier), écrasant les autres ; il remappe en revanche les annotations
# `paper` sur le domaine de chaque panneau. Un titre-annotation donne donc un
# titre PAR panneau (bivariée : tx à gauche, rr à droite) tout en restant correct
# pour un graphe unique.
.regen_ctx_title_ann <- function(title) {
  list(text = title, x = 0.5, y = 1.0, xref = "paper", yref = "paper",
    xanchor = "center", yanchor = "bottom", showarrow = FALSE,
    font = list(size = 13))
}

.regen_ctx_series_plot <- function(ser, fit, title, unit, i18n) {
  if (!.regen_ctx_series_ok(ser)) return(.regen_ctx_empty_plot(i18n$t("regen_ctx_out_of_coverage")))
  yr <- suppressWarnings(as.numeric(ser$year)); val <- suppressWarnings(as.numeric(ser$value))
  p <- plotly::plot_ly()
  p <- plotly::add_markers(p, x = yr, y = val, name = title,
    marker = list(size = 8, color = "#1f6feb"))
  anns <- list(.regen_ctx_title_ann(title))
  # Droite de tendance depuis le cœur : y = intercept + year * slope_decade/10.
  if (is.list(fit) && all(c("slope_decade", "intercept") %in% names(fit)) &&
      is.finite(fit$slope_decade) && is.finite(fit$intercept)) {
    line_y <- fit$intercept + yr * (fit$slope_decade / 10)
    p <- plotly::add_lines(p, x = yr, y = line_y, name = i18n$t("regen_ctx_trend"),
      line = list(color = "#d62728", width = 2))
    ann <- sprintf("%s %s%s", format(signif(fit$slope_decade, 3), trim = TRUE), unit,
      if (is.finite(fit$r2 %||% NA)) sprintf("  |  R²=%.2f", fit$r2) else "")
    if (is.finite(fit$p_value %||% NA)) ann <- sprintf("%s  |  p=%.3g", ann, fit$p_value)
    anns <- c(anns, list(list(text = ann, showarrow = FALSE,
      xref = "paper", yref = "paper", x = 0.02, y = 0.98, xanchor = "left",
      bgcolor = "rgba(255,255,255,.7)", font = list(size = 12))))
  }
  plotly::layout(p, annotations = anns,
    xaxis = list(title = i18n$t("regen_ctx_year")), yaxis = list(title = unit),
    showlegend = FALSE, margin = list(t = 40))
}

# Graphe 2 — anomalies annuelles (valeur - moyenne), colorées +/-. `sense` pilote
# la couleur défavorable : "hot_red" (T°max, chaud = rouge) ou "dry_red"
# (précip, sec = rouge, donc anomalie négative en rouge).
.regen_ctx_anomaly_plot <- function(ser, title, sense, i18n) {
  if (!.regen_ctx_series_ok(ser)) return(.regen_ctx_empty_plot(i18n$t("regen_ctx_out_of_coverage")))
  yr <- suppressWarnings(as.numeric(ser$year)); val <- suppressWarnings(as.numeric(ser$value))
  keep <- is.finite(yr) & is.finite(val); yr <- yr[keep]; val <- val[keep]
  anom <- val - mean(val)
  unfav <- if (identical(sense, "dry_red")) anom < 0 else anom > 0
  cols <- ifelse(unfav, "#d62728", "#1f6feb")
  plotly::layout(
    plotly::add_bars(plotly::plot_ly(), x = yr, y = anom,
      marker = list(color = cols)),
    annotations = list(.regen_ctx_title_ann(title)),
    xaxis = list(title = i18n$t("regen_ctx_year")),
    yaxis = list(title = i18n$t("regen_ctx_anomaly")),
    showlegend = FALSE, margin = list(t = 40))
}

# Graphe 3 — distribution des pentes du buffer (valeurs du raster de contexte
# affiché, déjà en mémoire) + trait vertical à la pente de la maille cliquée.
# `values` : numeric des pentes ; `point_value` : pente au point. AUCUN appel cœur.
.regen_ctx_distrib_plot <- function(values, point_value, title, unit, i18n) {
  v <- suppressWarnings(as.numeric(values)); v <- v[is.finite(v)]
  if (length(v) < 5L) return(.regen_ctx_empty_plot(i18n$t("regen_ctx_distrib_na")))
  anns <- list(.regen_ctx_title_ann(title))
  p <- plotly::layout(
    plotly::add_histogram(plotly::plot_ly(), x = v, nbinsx = 30,
      marker = list(color = "#9ecae1", line = list(color = "#6baed6", width = 0.5))),
    annotations = anns,
    xaxis = list(title = unit), yaxis = list(title = i18n$t("regen_ctx_count")),
    bargap = 0.02, showlegend = FALSE, margin = list(t = 40))
  pv <- suppressWarnings(as.numeric(point_value))
  if (length(pv) == 1L && is.finite(pv)) {
    # Percentile régional du point (spec 036 §5.3) : « ce point est au P{xx} du
    # massif » — situe la maille dans la distribution des pentes du buffer.
    pct <- round(100 * mean(v <= pv))
    p <- plotly::layout(p,
      shapes = list(list(type = "line", x0 = pv, x1 = pv, y0 = 0, y1 = 1,
        yref = "paper", line = list(color = "#d62728", width = 2, dash = "dash"))),
      annotations = c(anns, list(
        list(text = i18n$t("regen_ctx_this_cell"), x = pv, y = 1, yref = "paper",
          showarrow = FALSE, xanchor = "left", yanchor = "top",
          font = list(color = "#d62728", size = 11)),
        list(text = sprintf(i18n$t("regen_ctx_percentile"), pct), xref = "paper",
          yref = "paper", x = 0.98, y = 0.98, xanchor = "right", showarrow = FALSE,
          bgcolor = "rgba(255,255,255,.7)", font = list(size = 12)))))
  }
  p
}

# Graphe 4 — diagramme ombrothermique (Gaussen-Bagnouls) : barres précip (bleu) +
# courbe température (rouge), deux axes Y couplés P = 2T, mois secs (P < 2T)
# ombrés, annotation « n mois secs / De Martonne ». `temp_is_tg` : titre honnête
# (T° moyenne réelle vs repli T°max majorant la sécheresse).
.regen_ctx_ombro_plot <- function(clim_rr, clim_t, temp_is_tg, i18n) {
  ok_rr <- is.data.frame(clim_rr) && any(is.finite(suppressWarnings(as.numeric(clim_rr$value))))
  ok_t  <- is.data.frame(clim_t)  && any(is.finite(suppressWarnings(as.numeric(clim_t$value))))
  if (!ok_rr || !ok_t) return(.regen_ctx_empty_plot(i18n$t("regen_ctx_out_of_coverage")))
  P  <- suppressWarnings(as.numeric(clim_rr$value))
  Tm <- suppressWarnings(as.numeric(clim_t$value))
  months <- suppressWarnings(as.integer(clim_t$month %||% seq_len(length(Tm))))
  labs <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
  mlab <- labs[months]
  st <- .regen_ctx_ombro_stats(clim_rr, clim_t)

  # Axes couplés P = 2T : l'axe précip (droite) est gradué 2× l'axe °C (gauche).
  # L'axe température (gauche) doit contenir À LA FOIS max(T) ET max(P)/2 — sinon,
  # dès que la précipitation dépasse 2×Tmax (cas fréquent), les barres débordaient
  # en haut et étaient tronquées (« mur » de barres au plafond, diagramme faux).
  t_hi <- max(max(Tm, na.rm = TRUE), max(P, na.rm = TRUE) / 2)
  t_lo <- min(c(Tm, 0), na.rm = TRUE)
  pad  <- 0.1 * max(t_hi - t_lo, 1)
  t_rng <- c(t_lo - pad, t_hi + pad)

  p <- plotly::plot_ly()
  p <- plotly::add_bars(p, x = mlab, y = P, name = i18n$t("regen_ctx_axis_precip"),
    yaxis = "y2", marker = list(color = "#4292c6"))
  p <- plotly::add_trace(p, x = mlab, y = Tm, name = i18n$t("regen_ctx_axis_temp"),
    type = "scatter", mode = "lines+markers",
    line = list(color = "#d62728", width = 2), marker = list(color = "#d62728"))
  # Mois secs (P < 2T) : bande verticale ombrée + annotation récap.
  shapes <- lapply(st$dry_idx, function(mo) {
    xi <- match(mo, months) - 1L
    list(type = "rect", xref = "x", yref = "paper", x0 = xi - 0.5, x1 = xi + 0.5,
      y0 = 0, y1 = 1, fillcolor = "rgba(214,39,40,.10)", line = list(width = 0))
  })
  recap <- sprintf("%s: %d  |  %s: %s",
    i18n$t("regen_ctx_dry_months"),
    if (is.finite(st$dry_months)) st$dry_months else 0L,
    i18n$t("regen_ctx_demartonne"),
    if (is.finite(st$demartonne)) format(signif(st$demartonne, 3), trim = TRUE) else "—")
  title <- if (isTRUE(temp_is_tg)) i18n$t("regen_ctx_ombro_tg") else i18n$t("regen_ctx_ombro_proxy_tx")

  plotly::layout(p,
    title = list(text = title, font = list(size = 13)),
    shapes = shapes,
    xaxis = list(title = "", categoryorder = "array", categoryarray = mlab),
    yaxis  = list(title = i18n$t("regen_ctx_axis_temp"), range = t_rng,
      titlefont = list(color = "#d62728"), tickfont = list(color = "#d62728")),
    yaxis2 = list(title = i18n$t("regen_ctx_axis_precip"), overlaying = "y",
      side = "right", range = 2 * t_rng, titlefont = list(color = "#4292c6"),
      tickfont = list(color = "#4292c6")),
    legend = list(orientation = "h", y = -0.15),
    margin = list(t = 34, r = 50),
    annotations = list(list(text = recap, showarrow = FALSE, xref = "paper",
      yref = "paper", x = 0.5, y = 1.06, font = list(size = 11))))
}

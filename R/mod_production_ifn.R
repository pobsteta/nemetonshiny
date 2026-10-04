#' IFN production display helpers (spec 054)
#'
#' @description
#' What the family views say about P2 and E1 when they run in an IFN mode, and
#' what they must **not** say:
#'
#'   * P2 in IFN mode is the production of the **sylvoecoregion**, not the
#'     productivity of the UGF's site: every UGF of one SER gets the same value.
#'     Its RSE and its level (SER / GRECO / national) are shown with it.
#'   * E1 in flux mode follows the production. Combined with the IFN harvest
#'     ratio of the SER, it is simply the SER's **observed harvest**
#'     (`E1_mode = "recolte_observee"`) and is never presented as a potential.
#'   * The massif panel shows `poids_direct` and `part_bordure`, which say how
#'     much of the figure is really the massif's own.
#'
#' Pure rendering: the values come from the annex columns written by
#' `compute_all_indicators()` and from `read_production_ifn_summary()`.
#'
#' @name mod_production_ifn
#' @keywords internal
NULL


#' Annex columns that follow an indicator into the family view
#'
#' @param ind_col Character. Indicator column (raw or `_norm`).
#'
#' @return Character vector of dot-prefixed column names (possibly empty).
#'
#' @noRd
.production_annex_cols <- function(ind_col) {
  base <- sub("_norm$", "", ind_col)
  cols <- unname(PRODUCTION_ANNEX_COLS[[base]] %||% character(0))
  if (identical(base, "indicateur_e1_bois_energie")) cols <- c(cols, ".e1_taux")
  cols
}


#' First non-missing values of a column, as character
#' @noRd
.annex_values <- function(data, col) {
  v <- data[[col]]
  if (is.null(v)) return(character(0))
  v <- as.character(v)
  unique(v[!is.na(v) & nzchar(v)])
}


#' Number formatted for the current language
#' @noRd
.fmt_num <- function(x, i18n, digits = 2) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return("NA")
  format(round(as.numeric(x), digits), nsmall = 0, trim = TRUE,
         decimal.mark = if (identical(i18n$language, "fr")) "," else ".")
}


#' Production mode of a family-view column
#'
#' @param data data.frame / sf. Family indicator table.
#' @param ind_col Character. Indicator column.
#'
#' @return `"p2_ifn"`, `"e1_flux"`, `"e1_recolte_observee"`, or `NULL`.
#'
#' @noRd
.production_display_mode <- function(data, ind_col) {
  base <- sub("_norm$", "", ind_col)
  if (identical(base, "indicateur_p2_station") &&
      length(.annex_values(data, ".p2_provenance")) > 0L) {
    return("p2_ifn")
  }
  if (identical(base, "indicateur_e1_bois_energie")) {
    modes <- .annex_values(data, ".e1_mode")
    if ("recolte_observee" %in% modes) return("e1_recolte_observee")
    if ("ressource_flux" %in% modes) return("e1_flux")
  }
  NULL
}


#' Display label of an indicator, aware of the IFN modes
#'
#' @description
#' The core labels P2 "site productivity index" and E1 "fuelwood potential". In
#' IFN mode both would be wrong, so the label follows the mode the values were
#' computed under. Any other column keeps [clean_indicator_label()].
#'
#' @param data data.frame / sf. Family indicator table.
#' @param ind_col Character. Indicator column.
#' @param i18n Translator object.
#'
#' @return Character label.
#'
#' @noRd
indicator_display_label <- function(data, ind_col, i18n) {
  key <- switch(.production_display_mode(data, ind_col) %||% "",
    p2_ifn              = "p2_ifn_label",
    e1_flux             = "e1_flux_label",
    e1_recolte_observee = "e1_mode_recolte_observee",
    NULL
  )
  if (is.null(key)) return(clean_indicator_label(ind_col, i18n))
  code <- if (grepl("p2", ind_col)) "P2" else "E1"
  paste0(code, " - ", i18n$t(key))
}


#' Banner qualifying P2 / E1 computed in an IFN mode
#'
#' @description
#' Shown under the indicator map. For P2: one line per SER with the value, its
#' RSE, its level and its nature, then the reminder that this is not the site
#' productivity. For E1: the share used in flux mode, or - in the degenerate
#' case - a warning that the value is the SER's current harvest, not a
#' potential.
#'
#' @param data data.frame / sf. Family indicator table (raw annex columns).
#' @param ind_col Character. Indicator column.
#' @param i18n Translator object.
#'
#' @return A [htmltools::div] or `NULL` outside the IFN modes.
#'
#' @noRd
production_ifn_banner <- function(data, ind_col, i18n) {
  mode <- .production_display_mode(data, ind_col)
  if (is.null(mode)) return(NULL)

  if (identical(mode, "p2_ifn")) {
    df <- if (inherits(data, "sf")) sf::st_drop_geometry(data) else data
    raw <- df[["indicateur_p2_station"]]
    keys <- data.frame(
      ser  = as.character(df[[".p2_ser"]] %||% rep(NA, nrow(df))),
      val  = if (is.null(raw)) rep(NA_real_, nrow(df)) else as.numeric(raw),
      rse  = as.numeric(df[[".p2_rse"]] %||% rep(NA, nrow(df))),
      prov = as.character(df[[".p2_provenance"]]),
      nat  = as.character(df[[".p2_nature"]] %||% rep(NA, nrow(df))),
      stringsAsFactors = FALSE
    )
    keys <- unique(keys[!is.na(keys$prov), , drop = FALSE])

    lines <- lapply(seq_len(nrow(keys)), function(i) {
      k <- keys[i, ]
      echelon <- sub("^ifn_prod_", "", k$prov)
      echelon_key <- paste0("p2_echelon_", echelon)
      nature_key <- paste0("p2_nature_", k$nat)
      parts <- c(
        if (!is.na(k$val)) paste(.fmt_num(k$val, i18n), i18n$t("p2_ifn_unit")),
        if (!is.na(k$rse)) paste0(i18n$t("p2_ifn_rse"), " ",
                                  .fmt_num(k$rse, i18n, 1), " %"),
        paste0(i18n$t("p2_ifn_echelon"), " : ",
               if (isTRUE(i18n$has(echelon_key))) i18n$t(echelon_key) else echelon),
        if (isTRUE(i18n$has(nature_key))) i18n$t(nature_key)
      )
      htmltools::div(
        if (!is.na(k$ser)) htmltools::tags$strong(paste0(k$ser, " : ")),
        paste(parts, collapse = " \u00b7 ")
      )
    })

    return(htmltools::div(
      class = "small text-muted px-1 pb-1 production-ifn-banner",
      htmltools::div(
        bsicons::bs_icon("info-circle", class = "me-1"),
        htmltools::tags$strong(i18n$t("p2_ifn_label"))
      ),
      lines,
      htmltools::div(class = "fst-italic", i18n$t("p2_ifn_not_station"))
    ))
  }

  if (identical(mode, "e1_recolte_observee")) {
    return(htmltools::div(
      class = "small text-warning px-1 pb-1 production-ifn-banner",
      bsicons::bs_icon("exclamation-triangle-fill", class = "me-1"),
      htmltools::tags$strong(i18n$t("e1_mode_recolte_observee")),
      htmltools::div(i18n$t("e1_recolte_observee_avert"))
    ))
  }

  taux <- .annex_values(data, ".e1_taux")
  taux_txt <- if (identical(taux[1], "ifn_ser")) {
    i18n$t("prod_ifn_taux_ifn_ser")
  } else if (length(taux)) {
    .fmt_num(suppressWarnings(as.numeric(taux[1])), i18n)
  } else {
    "NA"
  }
  htmltools::div(
    class = "small text-muted fst-italic px-1 pb-1 production-ifn-banner",
    bsicons::bs_icon("info-circle", class = "me-1"),
    i18n$t("e1_mode_ressource_flux", taux = taux_txt)
  )
}


#' Panel "production of the massif" (spec 054 S3 bis)
#'
#' @description
#' The production of the union of the project's UGF, with the two figures that
#' say how much of it is really the massif's own:
#'
#'   * `poids_direct` below 0.2 - the value is essentially the SER's;
#'   * `part_bordure` above one half - the massif is small next to the 700 m
#'     blurring of the public IFN coordinates;
#'   * under 3 000 ha the SER value (P2) is preferable;
#'   * `hors_calibrage` - the area lies outside the range the error of the
#'     prediction was calibrated on (22 500 to 1 000 000 ha);
#'   * `nature = "prediction"` - no IFN plot in the massif at all.
#'
#' The prediction the plots are shrunk towards is named: the SER's, or the
#' SER's corrected by the massif's FORMS-T height and altitude (`"hybride"`).
#'
#' Followed by the harvest / production ratio of each SER, with its RSE and the
#' known bias, so that a ratio slightly above 1 is not read as decapitalisation.
#'
#' @param summary List from [read_production_ifn_summary()], or `NULL`.
#' @param i18n Translator object.
#'
#' @return A [bslib::card] or `NULL` when there is nothing to show.
#'
#' @noRd
production_ifn_panel <- function(summary, i18n) {
  if (is.null(summary)) return(NULL)
  m <- summary$massif
  r <- summary$ratios
  has_m <- is.data.frame(m) && nrow(m) > 0L
  has_r <- is.data.frame(r) && nrow(r) > 0L
  if (!has_m && !has_r) return(NULL)

  massif_ui <- if (has_m) {
    m <- m[1, ]
    num <- function(col) if (col %in% names(m)) suppressWarnings(as.numeric(m[[col]])) else NA_real_
    valeur <- num("valeur"); rse <- num("rse"); poids <- num("poids_direct")
    bordure <- num("part_bordure"); surface <- num("surface_ha")
    n_pl <- num("n_placettes")
    txt <- function(col) if (col %in% names(m)) as.character(m[[col]]) else NA_character_
    hybride <- identical(txt("predicteur"), "hybride")
    predicteur <- if (hybride) {
      sprintf(i18n$t("prod_predicteur_hybride"),
              as.character(summary$forms_t_year %||% "?"))
    } else if (!is.na(txt("predicteur"))) {
      i18n$t("prod_predicteur_ser")
    }

    alerts <- list(
      if (identical(txt("nature"), "prediction")) i18n$t("prod_massif_sans_placette"),
      if (!is.na(poids) && poids < 0.2) i18n$t("prod_massif_poids_faible"),
      if (!is.na(bordure) && bordure > 0.5) i18n$t("prod_massif_bordure_elevee"),
      if (!is.na(surface) && surface < 3000) i18n$t("prod_massif_petit"),
      if (isTRUE(as.logical(txt("hors_calibrage")))) i18n$t("prod_massif_hors_calibrage")
    )
    alerts <- Filter(Negate(is.null), alerts)

    row <- function(label, value) {
      htmltools::tags$tr(htmltools::tags$th(class = "fw-normal text-muted", label),
                         htmltools::tags$td(value))
    }
    htmltools::tagList(
      htmltools::tags$table(
        class = "table table-sm mb-2",
        htmltools::tags$tbody(
          row(i18n$t("prod_massif_valeur"),
              paste(.fmt_num(valeur, i18n), i18n$t("p2_ifn_unit"),
                    if (!is.na(rse)) paste0("(", i18n$t("p2_ifn_rse"), " ",
                                            .fmt_num(rse, i18n, 1), " %)"))),
          row(i18n$t("prod_massif_surface"),
              paste(.fmt_num(surface, i18n, 0), "ha")),
          row(i18n$t("prod_massif_placettes"), .fmt_num(n_pl, i18n, 0)),
          row(i18n$t("prod_massif_poids_direct"), .fmt_num(poids, i18n)),
          row(i18n$t("prod_massif_bordure"),
              if (is.na(bordure)) "NA" else paste0(.fmt_num(100 * bordure, i18n, 0), " %")),
          if (!is.null(predicteur)) row(i18n$t("prod_massif_predicteur"), predicteur)
        )
      ),
      lapply(alerts, function(a) htmltools::div(
        class = "small text-muted fst-italic mb-1",
        bsicons::bs_icon("info-circle", class = "me-1"), a))
    )
  }

  ratio_ui <- if (has_r) {
    def_label <- function(d) {
      k <- paste0("prod_ratio_def_", d)
      if (isTRUE(i18n$has(k))) i18n$t(k) else d
    }
    htmltools::tagList(
      htmltools::tags$h6(class = "mt-3", i18n$t("prod_ratio_title")),
      htmltools::tags$table(
        class = "table table-sm mb-2",
        htmltools::tags$thead(htmltools::tags$tr(
          htmltools::tags$th(i18n$t("prod_ratio_col_ser")),
          htmltools::tags$th(i18n$t("prod_ratio_col_def")),
          htmltools::tags$th(i18n$t("prod_ratio_col_ratio")),
          htmltools::tags$th(i18n$t("p2_ifn_rse")))),
        htmltools::tags$tbody(lapply(seq_len(nrow(r)), function(i) {
          htmltools::tags$tr(
            htmltools::tags$td(as.character(r$ser[i])),
            htmltools::tags$td(def_label(as.character(r$definition[i]))),
            htmltools::tags$td(.fmt_num(r$ratio[i], i18n)),
            htmltools::tags$td(if (is.na(r$rse[i])) "NA"
                               else paste0(.fmt_num(r$rse[i], i18n, 1), " %")))
        }))
      ),
      htmltools::div(class = "small text-muted fst-italic",
                     bsicons::bs_icon("info-circle", class = "me-1"),
                     i18n$t("prod_ratio_avert"))
    )
  }

  bslib::card(
    class = "mb-3",
    bslib::card_header(i18n$t("prod_massif_title")),
    bslib::card_body(massif_ui, ratio_ui)
  )
}

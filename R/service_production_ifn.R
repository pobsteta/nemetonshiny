#' IFN production by sylvoecoregion (spec 054) - application services
#'
#' @description
#' Wiring of the core's opt-in IFN production modes (`nemeton >= 0.204.0`):
#'
#'   * `ensure_ugf_ser()` - the sylvoecoregion (SER) code of each UGF, which
#'     every IFN mode needs. Without it the core falls back on the national
#'     figure - honestly flagged, but of no use.
#'   * `build_production_ifn_summary()` / `read_production_ifn_summary()` - the
#'     production of the whole massif (`nemeton::ifn_production_domaines()`) and
#'     the harvest / production ratio of each SER
#'     (`nemeton::ifn_taux_prelevement_production()`), computed in the compute
#'     worker and persisted next to the parcels.
#'
#' No figure is computed here: every number comes from `nemeton`.
#'
#' @name service_production_ifn
#' @keywords internal
NULL


#' Annex columns of the IFN-capable indicators
#'
#' @description
#' The core returns, next to the value, the columns that qualify it. They are
#' carried into the results under a **dot-prefixed** name - same convention as
#' `.a5_status` - so they cross the parquet without being taken for indicators.
#' Names are the core's column; values the app's.
#'
#' `ser` is not an annex of the core: it is the input column, read back from
#' P2's result so the display can name the SER without a second lookup.
#'
#' @noRd
PRODUCTION_ANNEX_COLS <- list(
  indicateur_p2_station = c(
    P2_rse        = ".p2_rse",
    P2_provenance = ".p2_provenance",
    P2_nature     = ".p2_nature",
    ser           = ".p2_ser"
  ),
  indicateur_e1_bois_energie = c(
    E1_mode = ".e1_mode"
  )
)


#' Does this indicator run in an IFN mode for this configuration?
#'
#' @description
#' P2 with `source = "ifn_fh"` and E1 in flux mode need no canopy height model:
#' they are precisely the answer to "NDP 0 without CHM" that the CHM guard of
#' `compute_single_indicator()` would otherwise block.
#'
#' @param indicator Character. Indicator column name.
#' @param cfg List from [project_production_ifn_params()], or `NULL`.
#'
#' @return Logical scalar.
#'
#' @noRd
.production_ifn_mode <- function(indicator, cfg) {
  if (is.null(cfg)) return(FALSE)
  switch(indicator,
    indicateur_p2_station      = identical(cfg$p2_source, "ifn_fh"),
    indicateur_e1_bois_energie = identical(cfg$e1_mode, "flux"),
    FALSE
  )
}


#' Copy the annex columns of a core result onto the value vector
#'
#' @description
#' `nemeton::extract_indicator_value()` keeps only the value column. The annex
#' columns travel as an attribute up to `compute_all_indicators()`, exactly like
#' the status column (`.capture_status_attr()`).
#'
#' @param vals Numeric vector.
#' @param result The sf / data.frame returned by the core.
#' @param indicator Character. Indicator column name.
#'
#' @return `vals`, possibly carrying a `nemeton_annex` named list.
#'
#' @noRd
.capture_annex_attr <- function(vals, result, indicator) {
  map <- PRODUCTION_ANNEX_COLS[[indicator]]
  if (is.null(map)) return(vals)
  present <- intersect(names(map), names(result))
  annex <- lapply(present, function(col) result[[col]])
  names(annex) <- unname(map[present])
  annex <- annex[vapply(annex, length, integer(1)) == length(vals)]
  if (length(annex)) attr(vals, "nemeton_annex") <- annex
  vals
}


#' Write the annex columns of an indicator into the results
#'
#' @description
#' Drops every annex column the indicator may own (a P2 back in CHM mode must
#' not keep an IFN RSE), then writes those carried by `values`. For E1 in flux
#' mode, `.e1_taux` records the share used - the only trace that lets a resume
#' tell a 0.6 run from an `"ifn_ser"` one (see [.production_mode_stale()]).
#'
#' @param results sf / data.frame. Accumulated results.
#' @param indicator Character.
#' @param values Vector returned by `compute_single_indicator()`, or `NULL`
#'   when the indicator failed.
#' @param cfg List from [project_production_ifn_params()].
#'
#' @return `results`.
#'
#' @noRd
.write_production_annex <- function(results, indicator, values, cfg) {
  map <- PRODUCTION_ANNEX_COLS[[indicator]]
  if (is.null(map)) return(results)
  owned <- unname(map)
  if (identical(indicator, "indicateur_e1_bois_energie")) {
    owned <- c(owned, ".e1_taux")
  }
  for (col in intersect(owned, names(results))) results[[col]] <- NULL
  if (is.null(values)) return(results)

  annex <- attr(values, "nemeton_annex")
  for (col in names(annex)) {
    if (length(annex[[col]]) == nrow(results)) results[[col]] <- annex[[col]]
  }
  if (identical(indicator, "indicateur_e1_bois_energie") &&
      .production_ifn_mode(indicator, cfg)) {
    results[[".e1_taux"]] <- rep(as.character(cfg$taux_mobilisation),
                                 nrow(results))
  }
  results
}


#' Units handed to an indicator, with what it reads from earlier indicators
#'
#' @description
#' E1 in flux mode reads P2 in **its own** units (`production_field = "P2"`) and
#' detects the degenerate case through `P2_provenance`. Each indicator is
#' otherwise handed the original parcels, so both columns are added here from
#' the results of P2 (computed before E1).
#'
#' @param indicator Character.
#' @param parcels sf. Compute units.
#' @param results sf / data.frame. Results accumulated so far.
#' @param cfg List from [project_production_ifn_params()].
#'
#' @return `parcels`, possibly with `P2` / `P2_provenance`.
#'
#' @noRd
.units_for_indicator <- function(indicator, parcels, results, cfg) {
  if (!identical(indicator, "indicateur_e1_bois_energie") ||
      !.production_ifn_mode(indicator, cfg)) {
    return(parcels)
  }
  n <- nrow(parcels)
  p2 <- results[["indicateur_p2_station"]]
  parcels$P2 <- if (length(p2) == n) as.numeric(p2) else rep(NA_real_, n)
  prov <- results[[".p2_provenance"]]
  parcels$P2_provenance <- if (length(prov) == n) as.character(prov)
                           else rep(NA_character_, n)
  parcels
}


#' Indicators already computed under another production mode
#'
#' @description
#' On resume, a P2 computed as a CHM site index must not be kept when the
#' project has since opted into the IFN mode, and conversely - same for E1 and
#' its share. The mode of a stored result is read from its annex columns:
#' `.p2_provenance` exists only in IFN mode, `.e1_taux` only in flux mode. In
#' flux mode, a recomputed P2 drags E1 along, since E1 reads it.
#'
#' @param existing data.frame. Stored results.
#' @param computed Character. Indicators considered already computed.
#' @param cfg List from [project_production_ifn_params()].
#'
#' @return Character. The subset of `computed` to recompute.
#'
#' @noRd
.production_mode_stale <- function(existing, computed, cfg) {
  stale <- character(0)
  p2 <- "indicateur_p2_station"
  e1 <- "indicateur_e1_bois_energie"

  if (p2 %in% computed) {
    prov <- existing[[".p2_provenance"]]
    has_ifn <- !is.null(prov) && any(!is.na(prov))
    if (!identical(has_ifn, .production_ifn_mode(p2, cfg))) stale <- c(stale, p2)
  }

  if (e1 %in% computed) {
    want <- if (.production_ifn_mode(e1, cfg)) as.character(cfg$taux_mobilisation)
            else NA_character_
    have <- existing[[".e1_taux"]]
    have <- if (is.null(have)) NA_character_ else {
      h <- unique(as.character(have[!is.na(have)]))
      if (length(h)) h[1] else NA_character_
    }
    p2_redone <- p2 %in% stale || !p2 %in% computed
    if (!identical(want, have) ||
        (.production_ifn_mode(e1, cfg) && p2_redone)) {
      stale <- c(stale, e1)
    }
  }

  stale
}


#' Sylvoecoregion of each UGF, localised once and cached
#'
#' @description
#' Calls `nemeton::localiser_ser()` (WFS INRAE bounded to the extent of the
#' UGF, about one second) and caches the result in `data/ugf_ser.rds`, keyed on
#' the UGF id **and** a hash of its geometry: a redrawn UGF is localised again,
#' an unchanged one never. A failed lookup (all `NA`) is not cached, so the next
#' run retries rather than freezing the national fallback.
#'
#' Never fails: on error the `ser` column is `NA`, which the core turns into the
#' national figure, flagged `ifn_prod_national`.
#'
#' @param units sf. Compute units with an `ug_id` column.
#' @param project_path Character. Project directory.
#'
#' @return `units` with a character `ser` column.
#'
#' @noRd
ensure_ugf_ser <- function(units, project_path) {
  n <- nrow(units)
  ids <- as.character(units$ug_id %||% seq_len(n))
  hashes <- vapply(sf::st_as_binary(sf::st_geometry(units)),
                   rlang::hash, character(1))
  cache_file <- file.path(project_path, "data", "ugf_ser.rds")

  cached <- tryCatch(
    if (file.exists(cache_file)) readRDS(cache_file) else NULL,
    error = function(e) NULL)
  if (is.data.frame(cached) &&
      all(c("ug_id", "geom_hash", "ser") %in% names(cached))) {
    key <- paste(ids, hashes)
    idx <- match(key, paste(cached$ug_id, cached$geom_hash))
    if (!anyNA(idx)) {
      units$ser <- as.character(cached$ser[idx])
      return(units)
    }
  }

  ser <- tryCatch(
    as.character(nemeton::localiser_ser(units)$ser),
    error = function(e) {
      cli::cli_warn("SER localisation failed: {conditionMessage(e)}")
      rep(NA_character_, n)
    })
  if (length(ser) != n) ser <- rep(NA_character_, n)
  units$ser <- ser

  if (any(!is.na(ser))) {
    tryCatch({
      dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
      saveRDS(data.frame(ug_id = ids, geom_hash = hashes, ser = ser,
                         stringsAsFactors = FALSE), cache_file)
    }, error = function(e) {
      cli::cli_warn("Could not cache SER codes: {conditionMessage(e)}")
    })
  }
  units
}


#' Production of the massif and harvest ratios of its SER
#'
#' @description
#' Spec 054 S3 bis. The union of the project's UGF is passed as a single domain
#' to `nemeton::ifn_production_domaines()`; the ratio harvest / production is
#' read for every SER the UGF fall in, under both definitions (`"ign"`: all
#' felled trees; `"vidange"`: felled and taken out). About five seconds, hence
#' its place in the compute worker rather than in an observer.
#'
#' With a DEM and FORMS-T height (`nemeton >= 0.206.0`), the massif's own
#' covariates are passed too: the core then corrects the SER prediction for the
#' gap between the massif and its SER (`predicteur = "hybride"`, volume
#' production only). Without them the prediction stays the SER's.
#'
#' Best-effort: each part fails on its own into `NULL`.
#'
#' @param units sf. Compute units, with a `ser` column (see [ensure_ugf_ser()]).
#' @param project_path Character. Project directory; the summary is written to
#'   `data/production_ifn.rds`.
#' @param dem `SpatRaster` (or path) of the project DEM, or `NULL`.
#'
#' @return The summary list (invisibly): `massif` (one-row data.frame or
#'   `NULL`), `ratios` (data.frame or `NULL`), `forms_t_year` (year of the
#'   FORMS-T height behind the covariates, or `NULL`), `computed_at`.
#'
#' @noRd
build_production_ifn_summary <- function(units, project_path, dem = NULL) {
  forms_t_year <- NULL
  massif <- tryCatch({
    dom <- sf::st_sf(id = "massif",
                     geometry = sf::st_union(sf::st_make_valid(
                       sf::st_geometry(units))))
    cov <- .massif_covariables(dom, dem)
    forms_t_year <- attr(cov, "forms_t_year")
    nemeton::ifn_production_domaines(dom, id_col = "id", covariables = cov)
  }, error = function(e) {
    cli::cli_warn("Massif IFN production failed: {conditionMessage(e)}")
    NULL
  })

  sers <- unique(as.character(units$ser %||% character(0)))
  sers <- sers[!is.na(sers) & nzchar(sers)]
  ratios <- tryCatch({
    rows <- unlist(lapply(sers, function(s) {
      lapply(c("ign", "vidange"), function(def) {
        nemeton::ifn_taux_prelevement_production(s, definition = def)
      })
    }), recursive = FALSE)
    if (length(rows)) do.call(rbind, rows) else NULL
  }, error = function(e) {
    cli::cli_warn("IFN harvest ratio failed: {conditionMessage(e)}")
    NULL
  })

  hybride <- is.data.frame(massif) &&
    identical(as.character(massif$predicteur[1]), "hybride")
  out <- list(massif = massif, ratios = ratios,
              forms_t_year = if (hybride) forms_t_year,
              computed_at = Sys.time())
  tryCatch({
    f <- file.path(project_path, "data", "production_ifn.rds")
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, f)
  }, error = function(e) {
    cli::cli_warn("Could not save the IFN production summary: {conditionMessage(e)}")
  })
  invisible(out)
}


#' Covariates of the massif for the hybrid IFN prediction
#'
#' @description
#' `nemeton::ifn_covariables_domaines()` on FORMS-T height and the project DEM.
#' Only FORMS-T is used, whatever CHM the indicators run on: the model was
#' fitted on it. A covariate left `NA` (no forest pixel, DEM off the massif)
#' returns `NULL` - the core would otherwise drop the correction silently while
#' still labelling the prediction hybrid.
#'
#' @param dom sf. One-row domain with an `id` column.
#' @param dem `SpatRaster`, path to one, or `NULL`.
#'
#' @return The covariates data.frame with a `forms_t_year` attribute, or `NULL`.
#'
#' @noRd
.massif_covariables <- function(dom, dem) {
  if (is.character(dem) && length(dem) == 1L && file.exists(dem)) {
    dem <- tryCatch(terra::rast(dem), error = function(e) NULL)
  }
  if (!inherits(dem, "SpatRaster")) return(NULL)
  h <- tryCatch(download_forms_t_height(dom), error = function(e) {
    cli::cli_warn("FORMS-T height failed: {conditionMessage(e)}")
    NULL
  })
  if (is.null(h)) return(NULL)
  cov <- tryCatch(
    nemeton::ifn_covariables_domaines(dom, h$height, dem, id_col = "id",
                                      unite_hauteur = "cm"),
    error = function(e) {
      cli::cli_warn("Massif covariates failed: {conditionMessage(e)}")
      NULL
    })
  vars <- c("h_mean", "h_sd", "alt_mean", "alt_sd")
  if (!is.data.frame(cov) || !nrow(cov) || !all(vars %in% names(cov)) ||
      !all(is.finite(as.matrix(cov[vars])))) {
    return(NULL)
  }
  attr(cov, "forms_t_year") <- h$year
  cov
}


#' Read the persisted IFN production summary of a project
#'
#' @param project_path Character. Project directory.
#'
#' @return The list written by [build_production_ifn_summary()], or `NULL`.
#'
#' @noRd
read_production_ifn_summary <- function(project_path) {
  if (is.null(project_path) || !nzchar(project_path)) return(NULL)
  f <- file.path(project_path, "data", "production_ifn.rds")
  if (!file.exists(f)) return(NULL)
  tryCatch(readRDS(f), error = function(e) NULL)
}

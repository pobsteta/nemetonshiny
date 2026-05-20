#' Theia / DATA TERRA data source integration
#'
#' @description
#' Application-level orchestration for the Theia / DATA TERRA data
#' sources exposed by the nemeton core package (>= 0.40.0). This
#' service only *loads* and *forwards* rasters to the nemeton
#' indicator functions; it never computes business logic itself.
#'
#' The Theia STAC API is reached through the Python `teledetection`
#' SDK (via reticulate), which signs asset URLs that nemeton then
#' reads with /vsicurl/. Access requires a registered API key.
#'
#' @name service_theia
#' @keywords internal
NULL


# Theia source keys declared by nemeton v0.40.0 (inst/datasources/FR.json).
THEIA_SOURCE_KEYS <- c(
  "forms_t", "formspot", "formspot_delta", "s2_biophysical",
  "theia_soil", "theia_snow", "theia_water", "theia_soil_moisture",
  "s2_l2a_muscate", "theia_species", "theia_lst"
)

# CHM source used to unblock the Production family (P1/P2/P3) and E1.
# FORMSpoT stores canopy height (at 1.5 m) in DECIMETRES — see
# `download_chm_theia()` for the metres conversion.
THEIA_CHM_SOURCE <- "formspot"

# Secondary Theia rasters loaded for the other indicator families.
# Each entry maps a `layers$rasters` slot to a single Theia source.
# `asset = NULL` lets nemeton pick the source default asset.
THEIA_INDICATOR_SOURCES <- list(
  fapar         = list(source = "s2_biophysical",     asset = NULL),
  snow          = list(source = "theia_snow",         asset = NULL),
  soil_moisture = list(source = "theia_soil_moisture", asset = NULL)
)


#' Path to the teledetection API key file
#'
#' @return Character. Absolute path to `~/.config/teledetection/.apikey`.
#' @noRd
.theia_apikey_path <- function() {
  file.path(path.expand("~"), ".config", "teledetection", ".apikey")
}


#' Check the Python prerequisites for Theia access
#'
#' `load_theia_source()` in `year` mode signs STAC asset URLs through
#' the Python `teledetection` SDK. The only hard prerequisite the app
#' can check up-front is the `reticulate` R package.
#'
#' The Python modules themselves (`teledetection`, `pystac_client`)
#' are declared by `nemeton` via `reticulate::py_require()` and
#' provisioned lazily by reticulate on first use. We deliberately do
#' NOT probe them with `py_module_available()` here: that call
#' initialises Python *before* `nemeton` has declared its
#' requirements, which both gives a false negative and locks the
#' interpreter without the needed packages. The real availability is
#' established when `download_chm_theia()` actually calls
#' `load_theia_source()`, and any genuine failure is caught there.
#'
#' @return A list with `ok` (logical) and `reason` (character or NULL).
#'   `reason` is `"reticulate"` when the R package is missing.
#' @noRd
theia_python_ready <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    return(list(ok = FALSE, reason = "reticulate"))
  }
  list(ok = TRUE, reason = NULL)
}


#' Check whether a Theia API key is configured
#'
#' A key is considered configured when both `TLD_ACCESS_KEY` and
#' `TLD_SECRET_KEY` environment variables are set, or when the
#' teledetection `.apikey` file exists.
#'
#' @return Logical.
#' @noRd
theia_api_key_configured <- function() {
  if (nzchar(Sys.getenv("TLD_ACCESS_KEY")) &&
      nzchar(Sys.getenv("TLD_SECRET_KEY"))) {
    return(TRUE)
  }
  file.exists(.theia_apikey_path())
}


#' Persist a Theia API key
#'
#' Writes the key to the teledetection `.apikey` file (durable) and
#' sets the `TLD_ACCESS_KEY` / `TLD_SECRET_KEY` environment variables
#' for the running session (immediate effect).
#'
#' @param access_key Character. Theia access key.
#' @param secret_key Character. Theia secret key.
#'
#' @return Logical. TRUE on success.
#' @noRd
theia_save_api_key <- function(access_key, secret_key) {
  access_key <- trimws(access_key %||% "")
  secret_key <- trimws(secret_key %||% "")
  if (!nzchar(access_key) || !nzchar(secret_key)) {
    return(FALSE)
  }
  ok <- tryCatch({
    apikey_path <- .theia_apikey_path()
    dir.create(dirname(apikey_path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(
      list(`access-key` = access_key, `secret-key` = secret_key),
      apikey_path,
      auto_unbox = TRUE
    )
    TRUE
  }, error = function(e) {
    cli::cli_warn("Failed to write Theia API key file: {e$message}")
    FALSE
  })
  Sys.setenv(TLD_ACCESS_KEY = access_key, TLD_SECRET_KEY = secret_key)
  ok
}


#' Aggregate Theia readiness status
#'
#' @return A list with `python_ok`, `python_reason`, `key_ok` and
#'   `ready` (TRUE only when both prerequisites are met).
#' @noRd
theia_status <- function() {
  py <- theia_python_ready()
  key <- theia_api_key_configured()
  list(
    python_ok     = isTRUE(py$ok),
    python_reason = py$reason,
    key_ok        = isTRUE(key),
    ready         = isTRUE(py$ok) && isTRUE(key)
  )
}


#' Translate a Theia readiness status into an i18n message key
#'
#' @param status A list returned by `theia_status()`.
#'
#' @return Character. An i18n key describing the missing prerequisite,
#'   or `"theia_status_ready"` when everything is in place.
#' @noRd
theia_status_key <- function(status) {
  if (isTRUE(status$ready)) return("theia_status_ready")
  if (!isTRUE(status$python_ok)) return("theia_error_reticulate")
  "theia_error_no_key"
}


#' Resolve the target year for FORMSpoT canopy height
#'
#' FORMSpoT is published per year. Defaults to the previous calendar
#' year (the most recent fully consolidated product) unless overridden
#' by `options(nemetonshiny.theia_year = ...)`.
#'
#' @return Integer year.
#' @noRd
theia_compute_year <- function() {
  opt <- getOption("nemetonshiny.theia_year")
  if (!is.null(opt)) {
    yr <- suppressWarnings(as.integer(opt))
    if (!is.na(yr)) return(yr)
  }
  as.integer(format(Sys.Date(), "%Y")) - 1L
}


#' Whether the Theia CHM path should be attempted
#'
#' Opt-in by nature: it only runs when Theia is fully ready (Python
#' prerequisites + API key). Honours the shared CHM opt-outs
#' (`options(nemetonshiny.chm = "none")`,
#' `options(nemetonshiny.chm_source = "opencanopy")`).
#'
#' @return Logical.
#' @noRd
theia_chm_enabled <- function() {
  if (identical(getOption("nemetonshiny.chm"), "none")) return(FALSE)
  if (identical(getOption("nemetonshiny.chm_source"), "opencanopy")) {
    return(FALSE)
  }
  isTRUE(theia_status()$ready)
}


#' Build an AOI sf for Theia loading
#'
#' nemeton crops the loaded raster to the AOI. We pass the project
#' parcels reprojected to WGS84 with a small metric buffer.
#'
#' @param parcels sf object (any CRS).
#' @param buffer_m Numeric. Buffer in metres.
#'
#' @return An sf object in EPSG:4326.
#' @noRd
.theia_aoi <- function(parcels, buffer_m = 200) {
  proj <- sf::st_transform(parcels, 2154)
  buffered <- sf::st_buffer(proj, buffer_m)
  sf::st_transform(buffered, 4326)
}


#' Fetch a Canopy Height Model from Theia FORMSpoT
#'
#' Loads the FORMSpoT canopy height product for the project AOI via
#' `nemeton::load_theia_source()`, converts decimetres to metres
#' (FORMSpoT stores height at 1.5 m in DECIMETRES), and cleans it with
#' `nemeton::sanitize_chm()` using whatever mask layers are available.
#'
#' This is the primary path that unblocks the Production family
#' (P1/P2/P3) and E1 in NDP 0 from public Theia data.
#'
#' @param parcels sf of project parcels (any CRS).
#' @param year Integer. FORMSpoT product year (default: `theia_compute_year()`).
#' @param rasters List of already-downloaded raster layers (may include `ndvi`).
#' @param vectors List of already-downloaded vector layers (may include
#'   `bdforet`, `buildings`, `water_surfaces`).
#'
#' @return list(chm = SpatRaster, pct_masked = numeric, source =
#'   "theia_formspot", year = integer), or NULL when no data is returned.
#'   Stops with an explicit condition when Theia is not ready.
#' @noRd
download_chm_theia <- function(parcels, year = NULL,
                               rasters = list(), vectors = list()) {
  status <- theia_status()
  if (!isTRUE(status$ready)) {
    stop(structure(
      class = c("theia_unavailable", "error", "condition"),
      list(message = paste0("Theia unavailable: ", theia_status_key(status)),
           call = NULL,
           status_key = theia_status_key(status))
    ))
  }

  if (is.null(year)) year <- theia_compute_year()
  aoi <- .theia_aoi(parcels)

  cli::cli_alert_info(
    "Fetching FORMSpoT canopy height from Theia (year {year})..."
  )
  chm_dm <- nemeton::load_theia_source(
    THEIA_CHM_SOURCE, aoi = aoi, year = year, country = "FR"
  )
  if (is.null(chm_dm)) return(NULL)

  # FORMSpoT stores canopy height in DECIMETRES -> convert to metres,
  # the unit expected by the nemeton indicator `chm` argument.
  chm_m <- chm_dm / 10

  cleaned <- tryCatch(
    nemeton::sanitize_chm(
      chm_m,
      forest_mask = vectors$bdforet,
      buildings   = vectors$buildings,
      water       = vectors$water_surfaces,
      ndvi        = rasters$ndvi
    ),
    error = function(e) {
      cli::cli_warn("sanitize_chm failed on Theia CHM: {e$message}")
      list(chm_clean = chm_m, pct_masked = NA_real_,
           steps_applied = character(0))
    }
  )

  cli::cli_alert_success("Theia FORMSpoT CHM ready (year {year}).")
  list(
    chm        = cleaned$chm_clean,
    pct_masked = cleaned$pct_masked,
    source     = "theia_formspot",
    year       = year
  )
}


#' Load secondary Theia rasters for non-Production indicators
#'
#' Loads FAPAR (C2), snow days (R3) and soil moisture (R3) from their
#' Theia sources. Every load is independent and fault-tolerant: a
#' failure on one source logs a warning and leaves that slot NULL, so
#' the affected indicator simply falls back to its legacy path.
#'
#' @param parcels sf of project parcels (any CRS).
#' @param year Integer. Target product year.
#'
#' @return A named list of SpatRaster objects (slots `fapar`, `snow`,
#'   `soil_moisture`); missing slots are simply absent.
#' @noRd
download_theia_layers <- function(parcels, year = NULL) {
  if (!isTRUE(theia_status()$ready)) return(list())
  if (is.null(year)) year <- theia_compute_year()
  aoi <- .theia_aoi(parcels)

  out <- list()
  for (slot in names(THEIA_INDICATOR_SOURCES)) {
    cfg <- THEIA_INDICATOR_SOURCES[[slot]]
    raster <- tryCatch(
      nemeton::load_theia_source(
        cfg$source, aoi = aoi, asset = cfg$asset,
        year = year, country = "FR"
      ),
      error = function(e) {
        cli::cli_warn(
          "Theia source {.val {cfg$source}} ({slot}) failed: {e$message}"
        )
        NULL
      }
    )
    if (!is.null(raster)) {
      out[[slot]] <- raster
      cli::cli_alert_success("Theia layer loaded: {slot} ({cfg$source}).")
    }
  }
  out
}


#' Provenance / licence metadata for the Theia sources
#'
#' Reads the `provenance`, `consumed_by` and `license` fields from
#' `nemeton::get_data_source()` for every declared Theia source, for
#' display in the configuration screen.
#'
#' @return A data.frame with columns `key`, `name`, `provenance`,
#'   `consumed_by`, `license`. Sources that cannot be resolved are
#'   skipped.
#' @noRd
theia_source_provenance <- function() {
  rows <- lapply(THEIA_SOURCE_KEYS, function(key) {
    meta <- tryCatch(
      nemeton::get_data_source(key, "FR"),
      error = function(e) NULL
    )
    if (is.null(meta)) return(NULL)
    flatten <- function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      paste(unlist(x), collapse = ", ")
    }
    data.frame(
      key         = key,
      name        = flatten(meta$name %||% key),
      provenance  = flatten(meta$provenance),
      consumed_by = flatten(meta$consumed_by),
      license     = flatten(meta$license),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(
      key = character(0), name = character(0),
      provenance = character(0), consumed_by = character(0),
      license = character(0), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

#' Cadastre Service for nemetonApp
#'
#' @description
#' Service for retrieving cadastral parcel data from French cadastre.
#' Uses API Cadastre (data.gouv.fr) as primary source with happign as fallback.
#'
#' @name service_cadastre
#' @keywords internal
NULL


#' Get cadastral parcels for a commune
#'
#' @description
#' Retrieves all cadastral parcels within a commune.
#' Uses API Cadastre as primary source, with happign WFS as fallback.
#'
#' @param code_insee Character. The 5-character INSEE code of the commune.
#' @param commune_geometry Optional sf object. Pre-fetched commune boundary.
#'
#' @return An sf object with parcel geometries and attributes:
#'   - id: Unique parcel identifier
#'   - section: Cadastral section
#'   - numero: Parcel number
#'   - contenance: Area in square meters
#'   - commune: Commune name
#'   - code_insee: INSEE code
#'
#' @details
#' The function attempts to fetch parcels from API Cadastre first.
#' If that fails (network error, API unavailable), it falls back to
#' using the happign package to query the IGN WFS service.
#'
#' @noRd
get_cadastral_parcels <- function(code_insee, commune_geometry = NULL) {
  if (!grepl("^[0-9A-B]{5}$", code_insee)) {
    cli::cli_abort("Invalid INSEE code format: {code_insee}")
  }

  cli::cli_alert_info("Fetching cadastral parcels for commune {code_insee}...")

 # Try API Cadastre first
  parcels <- tryCatch({
    fetch_api_cadastre(code_insee)
  }, error = function(e) {
    cli::cli_warn("API Cadastre failed: {e$message}")
    NULL
  })

  # Fallback to happign if primary fails
  if (is.null(parcels) || nrow(parcels) == 0) {
    cli::cli_alert_info("Falling back to happign WFS...")

    parcels <- tryCatch({
      fetch_happign_cadastre(code_insee, commune_geometry)
    }, error = function(e) {
      cli::cli_abort(c(
        "Unable to fetch cadastral parcels",
        "x" = "API Cadastre: unavailable",
        "x" = "happign WFS: {e$message}",
        "i" = "Check your internet connection"
      ))
    })
  }

  if (is.null(parcels) || nrow(parcels) == 0) {
    cli::cli_abort("No parcels found for commune {code_insee}")
  }

  cli::cli_alert_success("Retrieved {nrow(parcels)} parcels")

  # Ensure consistent structure
  parcels <- standardize_parcels(parcels, code_insee)

  parcels
}


#' Fetch parcels from API Cadastre
#'
#' @description
#' Fetches parcels from the official cadastre.data.gouv.fr API.
#'
#' @param code_insee Character. INSEE code of the commune.
#'
#' @return An sf object with parcel geometries.
#'
#' @noRd
fetch_api_cadastre <- function(code_insee) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    cli::cli_abort("Package 'httr2' is required for API calls")
  }

  # API Cadastre URL for GeoJSON parcels (source: inst/datasources/FR.json)
  cadastre_cfg <- get_data_source("cadastre_api", "FR")
  url_template <- cadastre_cfg$url %||%
    "https://cadastre.data.gouv.fr/bundler/cadastre-etalab/communes/{code_commune}/geojson/parcelles"
  url <- gsub("{code_commune}", code_insee, url_template, fixed = TRUE)

  cli::cli_alert("Requesting: {url}")

  resp <- httr2::request(url) |>
    httr2::req_timeout(get_app_config("api_timeout", 30000) / 1000) |>
    httr2::req_retry(
      max_tries = get_app_config("max_retries", 3),
      backoff = ~ 2
    ) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  # Check for errors
  if (httr2::resp_status(resp) >= 400) {
    status <- httr2::resp_status(resp)
    cli::cli_abort("API Cadastre returned status {status}")
  }

  # Parse GeoJSON response
  geojson_text <- httr2::resp_body_string(resp)

  parcels <- sf::st_read(geojson_text, quiet = TRUE)

  # Ensure CRS is WGS84
  if (is.na(sf::st_crs(parcels))) {
    sf::st_crs(parcels) <- 4326
  }

  parcels
}


#' Fetch parcels from happign WFS
#'
#' @description
#' Fetches parcels using the happign package's WFS interface to IGN.
#'
#' @param code_insee Character. INSEE code of the commune.
#' @param commune_geometry Optional sf object. Commune boundary for spatial filter.
#'
#' @return An sf object with parcel geometries.
#'
#' @noRd
fetch_happign_cadastre <- function(code_insee, commune_geometry = NULL) {
  if (!requireNamespace("happign", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package 'happign' is required for WFS fallback",
      "i" = "Install with: install.packages('happign')"
    ))
  }

  # Get commune geometry if not provided
  if (is.null(commune_geometry)) {
    commune_geometry <- get_commune_geometry(code_insee)
    if (is.null(commune_geometry)) {
      cli::cli_abort("Unable to get commune geometry for spatial query")
    }
  }

  # Transform to Lambert 93 for WFS query
  commune_l93 <- sf::st_transform(commune_geometry, 2154)

  # Query WFS
  parcels <- happign::get_wfs(
    x = commune_l93,
    layer = "CADASTRALPARCELS.PARCELLAIRE_EXPRESS:parcelle",
    spatial_filter = "intersects"
  )

  # Transform back to WGS84 for consistency
  parcels <- sf::st_transform(parcels, 4326)

  parcels
}


#' Standardize parcel data structure
#'
#' @description
#' Ensures consistent column names and data types across different sources.
#' Filters to only keep POLYGON/MULTIPOLYGON geometries.
#'
#' @param parcels sf object from any source.
#' @param code_insee Character. INSEE code for reference.
#'
#' @return sf object with standardized columns.
#'
#' @noRd
standardize_parcels <- function(parcels, code_insee) {
  # Filter to keep only polygon geometries (exclude points, lines)
  geom_types <- sf::st_geometry_type(parcels)
  polygon_idx <- geom_types %in% c("POLYGON", "MULTIPOLYGON")

  if (sum(polygon_idx) == 0) {
    cli::cli_abort("No polygon geometries found in parcels data")
  }

  if (sum(!polygon_idx) > 0) {
    cli::cli_alert_warning(
      "Filtered out {sum(!polygon_idx)} non-polygon geometries (POINT/LINE)"
    )
    parcels <- parcels[polygon_idx, ]
  }

  # Get column names (case insensitive matching)
  cols <- tolower(names(parcels))

  # Create standardized data frame
  std_parcels <- parcels

  # Ensure required columns exist
  if (!"id" %in% cols) {
    # Try to construct ID from available columns
    if ("idu" %in% cols) {
      std_parcels$id <- parcels$idu
    } else if ("id_parcelle" %in% tolower(cols)) {
      idx <- which(tolower(cols) == "id_parcelle")
      std_parcels$id <- parcels[[names(parcels)[idx]]]
    } else {
      # Generate IDs
      std_parcels$id <- paste0(code_insee, "_", seq_len(nrow(parcels)))
    }
  }

  # Section
  if (!"section" %in% cols) {
    if ("section" %in% tolower(cols)) {
      idx <- which(tolower(cols) == "section")
      std_parcels$section <- parcels[[names(parcels)[idx]]]
    } else {
      std_parcels$section <- NA_character_
    }
  }

  # Numero
  if (!"numero" %in% cols) {
    if ("numero" %in% tolower(cols)) {
      idx <- which(tolower(cols) == "numero")
      std_parcels$numero <- parcels[[names(parcels)[idx]]]
    } else {
      std_parcels$numero <- NA_character_
    }
  }

  # Contenance (area)
  if (!"contenance" %in% cols) {
    if ("contenance" %in% tolower(cols)) {
      idx <- which(tolower(cols) == "contenance")
      std_parcels$contenance <- as.numeric(parcels[[names(parcels)[idx]]])
    } else {
      # Calculate from geometry
      area_m2 <- as.numeric(sf::st_area(sf::st_transform(parcels, 2154)))
      std_parcels$contenance <- area_m2
    }
  }

  # Code INSEE
  std_parcels$code_insee <- code_insee

  # Select and order columns
  required_cols <- c("id", "section", "numero", "contenance", "code_insee")
  other_cols <- setdiff(names(std_parcels), c(required_cols, "geometry"))
  geom_col <- attr(std_parcels, "sf_column")

  std_parcels <- std_parcels[, c(required_cols, other_cols, geom_col)]

  # Ensure valid geometries
  std_parcels <- sf::st_make_valid(std_parcels)

  std_parcels
}


#' Get parcel by ID
#'
#' @description
#' Retrieve a single parcel by its unique identifier.
#'
#' @param parcel_id Character. The parcel ID.
#' @param parcels sf object. The full set of parcels to search.
#'
#' @return sf object with single parcel, or NULL if not found.
#'
#' @noRd
get_parcel_by_id <- function(parcel_id, parcels) {
  idx <- which(parcels$id == parcel_id)
  if (length(idx) == 0) {
    return(NULL)
  }
  parcels[idx, ]
}


#' Filter parcels by selection
#'
#' @description
#' Filter parcels to only those that are selected.
#'
#' @param parcels sf object. All parcels.
#' @param selected_ids Character vector. IDs of selected parcels.
#'
#' @return sf object with selected parcels only.
#'
#' @noRd
filter_selected_parcels <- function(parcels, selected_ids) {
  if (length(selected_ids) == 0) {
    return(parcels[0, ])
  }

  parcels[parcels$id %in% selected_ids, ]
}


#' Calculate parcel statistics
#'
#' @description
#' Calculate summary statistics for a set of parcels.
#'
#' @param parcels sf object.
#'
#' @return List with statistics.
#'
#' @noRd
calculate_parcel_stats <- function(parcels) {
  if (is.null(parcels) || nrow(parcels) == 0) {
    return(list(
      count = 0,
      total_area_ha = 0,
      mean_area_ha = 0,
      min_area_ha = 0,
      max_area_ha = 0
    ))
  }

  areas_ha <- parcels$contenance / 10000  # Convert m² to ha

  list(
    count = nrow(parcels),
    total_area_ha = sum(areas_ha, na.rm = TRUE),
    mean_area_ha = mean(areas_ha, na.rm = TRUE),
    min_area_ha = min(areas_ha, na.rm = TRUE),
    max_area_ha = max(areas_ha, na.rm = TRUE)
  )
}


#' Format parcel for display
#'
#' @description
#' Format parcel information for display in UI.
#'
#' @param parcel sf row. Single parcel.
#' @param language Character. "fr" or "en".
#'
#' @return Character string with formatted info.
#'
#' @noRd
format_parcel_info <- function(parcel, language = "fr") {
  area_ha <- parcel$contenance / 10000

  if (language == "fr") {
    sprintf(
      "Parcelle %s\nSection: %s | N\u00b0: %s\nSurface: %.2f ha",
      parcel$id,
      parcel$section %||% "?",
      parcel$numero %||% "?",
      area_ha
    )
  } else {
    sprintf(
      "Parcel %s\nSection: %s | No: %s\nArea: %.2f ha",
      parcel$id,
      parcel$section %||% "?",
      parcel$numero %||% "?",
      area_ha
    )
  }
}


#' Create parcel popup content
#'
#' @description
#' Create HTML content for Leaflet popup.
#'
#' @param parcel sf row.
#'
#' @return Character string with HTML.
#'
#' @noRd
create_parcel_popup <- function(parcel) {
  area_ha <- round(parcel$contenance / 10000, 3)

  sprintf(
    "<strong>%s</strong><br/>
    Section: %s<br/>
    N&deg;: %s<br/>
    Surface: %s ha<br/>
    <em style='color: #666;'>Cliquez pour s&eacute;lectionner</em>",
    parcel$id,
    parcel$section %||% "-",
    parcel$numero %||% "-",
    area_ha
  )
}


#' Create hover label for parcel
#'
#' @description
#' Creates a label shown on hover for a parcel, with detailed information
#' since popups are no longer used (hover-based display only).
#'
#' @param parcel sf row with parcel data.
#'
#' @return Character string with HTML.
#'
#' @noRd
create_parcel_label <- function(parcel) {
  # Section and number
  section <- parcel$section %||% "-"
  numero <- parcel$numero %||% "-"

  # Area in hectares
  area_text <- ""
  if (!is.null(parcel$contenance) && !is.na(parcel$contenance)) {
    area_ha <- round(parcel$contenance / 10000, 2)
    area_text <- sprintf(" | <b>%s ha</b>", area_ha)
  }

  # Lieu-dit if available
  lieu_dit_text <- ""
  lieu_dit <- parcel$nom_com %||% parcel$lieu_dit %||% parcel$lieudit %||% NULL
  if (!is.null(lieu_dit) && nchar(lieu_dit) > 0 && lieu_dit != "NA") {
    lieu_dit_text <- sprintf("<br/><span style='color:#666;'>%s</span>", lieu_dit)
  }

  sprintf(
    "<b>%s %s</b>%s%s<br/><span style='color:#1B6B1B;font-size:11px;'>Cliquez pour s\u00e9lectionner</span>",
    section,
    numero,
    area_text,
    lieu_dit_text
  )
}

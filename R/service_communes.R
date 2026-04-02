#' Communes Service for nemetonApp
#'
#' @description
#' Service for searching and retrieving French commune (municipality) data.
#' Provides autocomplete functionality and geometry retrieval.
#'
#' @name service_communes
#' @keywords internal
NULL


#' Get list of French departments
#'
#' @description
#' Returns a named vector of French departments with their codes.
#'
#' @return Named character vector (name = display label, value = code)
#'
#' @examples
#' \dontrun{
#' depts <- get_departments()
#' head(depts)
#' }
#'
#' @noRd
get_departments <- function() {
  c(
    "01 - Ain" = "01",
    "02 - Aisne" = "02",
    "03 - Allier" = "03",
    "04 - Alpes-de-Haute-Provence" = "04",
    "05 - Hautes-Alpes" = "05",
    "06 - Alpes-Maritimes" = "06",
    "07 - Ard\u00e8che" = "07",
    "08 - Ardennes" = "08",
    "09 - Ari\u00e8ge" = "09",
    "10 - Aube" = "10",
    "11 - Aude" = "11",
    "12 - Aveyron" = "12",
    "13 - Bouches-du-Rh\u00f4ne" = "13",
    "14 - Calvados" = "14",
    "15 - Cantal" = "15",
    "16 - Charente" = "16",
    "17 - Charente-Maritime" = "17",
    "18 - Cher" = "18",
    "19 - Corr\u00e8ze" = "19",
    "21 - C\u00f4te-d'Or" = "21",
    "22 - C\u00f4tes-d'Armor" = "22",
    "23 - Creuse" = "23",
    "24 - Dordogne" = "24",
    "25 - Doubs" = "25",
    "26 - Dr\u00f4me" = "26",
    "27 - Eure" = "27",
    "28 - Eure-et-Loir" = "28",
    "29 - Finist\u00e8re" = "29",
    "2A - Corse-du-Sud" = "2A",
    "2B - Haute-Corse" = "2B",
    "30 - Gard" = "30",
    "31 - Haute-Garonne" = "31",
    "32 - Gers" = "32",
    "33 - Gironde" = "33",
    "34 - H\u00e9rault" = "34",
    "35 - Ille-et-Vilaine" = "35",
    "36 - Indre" = "36",
    "37 - Indre-et-Loire" = "37",
    "38 - Is\u00e8re" = "38",
    "39 - Jura" = "39",
    "40 - Landes" = "40",
    "41 - Loir-et-Cher" = "41",
    "42 - Loire" = "42",
    "43 - Haute-Loire" = "43",
    "44 - Loire-Atlantique" = "44",
    "45 - Loiret" = "45",
    "46 - Lot" = "46",
    "47 - Lot-et-Garonne" = "47",
    "48 - Loz\u00e8re" = "48",
    "49 - Maine-et-Loire" = "49",
    "50 - Manche" = "50",
    "51 - Marne" = "51",
    "52 - Haute-Marne" = "52",
    "53 - Mayenne" = "53",
    "54 - Meurthe-et-Moselle" = "54",
    "55 - Meuse" = "55",
    "56 - Morbihan" = "56",
    "57 - Moselle" = "57",
    "58 - Ni\u00e8vre" = "58",
    "59 - Nord" = "59",
    "60 - Oise" = "60",
    "61 - Orne" = "61",
    "62 - Pas-de-Calais" = "62",
    "63 - Puy-de-D\u00f4me" = "63",
    "64 - Pyr\u00e9n\u00e9es-Atlantiques" = "64",
    "65 - Hautes-Pyr\u00e9n\u00e9es" = "65",
    "66 - Pyr\u00e9n\u00e9es-Orientales" = "66",
    "67 - Bas-Rhin" = "67",
    "68 - Haut-Rhin" = "68",
    "69 - Rh\u00f4ne" = "69",
    "70 - Haute-Sa\u00f4ne" = "70",
    "71 - Sa\u00f4ne-et-Loire" = "71",
    "72 - Sarthe" = "72",
    "73 - Savoie" = "73",
    "74 - Haute-Savoie" = "74",
    "75 - Paris" = "75",
    "76 - Seine-Maritime" = "76",
    "77 - Seine-et-Marne" = "77",
    "78 - Yvelines" = "78",
    "79 - Deux-S\u00e8vres" = "79",
    "80 - Somme" = "80",
    "81 - Tarn" = "81",
    "82 - Tarn-et-Garonne" = "82",
    "83 - Var" = "83",
    "84 - Vaucluse" = "84",
    "85 - Vend\u00e9e" = "85",
    "86 - Vienne" = "86",
    "87 - Haute-Vienne" = "87",
    "88 - Vosges" = "88",
    "89 - Yonne" = "89",
    "90 - Territoire de Belfort" = "90",
    "91 - Essonne" = "91",
    "92 - Hauts-de-Seine" = "92",
    "93 - Seine-Saint-Denis" = "93",
    "94 - Val-de-Marne" = "94",
    "95 - Val-d'Oise" = "95",
    "971 - Guadeloupe" = "971",
    "972 - Martinique" = "972",
    "973 - Guyane" = "973",
    "974 - La R\u00e9union" = "974",
    "976 - Mayotte" = "976"
  )
}


#' Search communes by name
#'
#' @description
#' Search for communes by name with autocomplete support.
#' Uses the geo.api.gouv.fr API.
#'
#' @param query Character. Search query (minimum 3 characters).
#' @param department Character. Optional department code to filter results.
#' @param limit Integer. Maximum number of results (default: 20).
#'
#' @return A data.frame with columns: code_insee, nom, code_postal, departement, label
#'
#' @examples
#' \dontrun{
#' # Search for communes starting with "Lyon"
#' search_communes("Lyon")
#'
#' # Search within a specific department
#' search_communes("Saint", department = "73")
#' }
#'
#' @noRd
search_communes <- function(query, department = NULL, limit = 20L) {
  if (nchar(query) < 3) {
    return(data.frame(
      code_insee = character(0),
      nom = character(0),
      code_postal = character(0),
      departement = character(0),
      label = character(0)
    ))
  }

  # Build API URL (source: inst/datasources/FR.json)
  communes_cfg <- get_data_source("communes_api", "FR")
  base_url <- paste0(communes_cfg$url %||% "https://geo.api.gouv.fr", "/communes")

  params <- list(
    nom = query,
    fields = "nom,code,codesPostaux,codeDepartement",
    boost = "population",
    limit = limit
  )

  if (!is.null(department) && department != "") {
    params$codeDepartement <- department
  }

  # Make request
  result <- tryCatch({
    if (!requireNamespace("httr2", quietly = TRUE)) {
      cli::cli_abort("Package 'httr2' is required for API calls")
    }

    resp <- httr2::request(base_url) |>
      httr2::req_url_query(!!!params) |>
      httr2::req_timeout(10) |>
      httr2::req_retry(max_tries = 3, backoff = ~ 2) |>
      httr2::req_perform()

    data <- httr2::resp_body_json(resp)

    if (length(data) == 0) {
      return(data.frame(
        code_insee = character(0),
        nom = character(0),
        code_postal = character(0),
        departement = character(0),
        label = character(0)
      ))
    }

    # Parse results
    do.call(rbind, lapply(data, function(commune) {
      codes_postaux <- paste(commune$codesPostaux, collapse = ", ")
      data.frame(
        code_insee = commune$code,
        nom = commune$nom,
        code_postal = codes_postaux,
        departement = commune$codeDepartement,
        label = sprintf("%s (%s)", commune$nom, codes_postaux),
        stringsAsFactors = FALSE
      )
    }))

  }, error = function(e) {
    cli::cli_warn("Error searching communes: {e$message}")
    data.frame(
      code_insee = character(0),
      nom = character(0),
      code_postal = character(0),
      departement = character(0),
      label = character(0)
    )
  })

  result
}


#' Search communes by postal code
#'
#' @description
#' Search for communes by postal code.
#'
#' @param postal_code Character. 5-digit French postal code.
#'
#' @return A data.frame with commune information.
#'
#' @noRd
search_by_postal_code <- function(postal_code) {
  if (!grepl("^[0-9]{5}$", postal_code)) {
    return(data.frame(
      code_insee = character(0),
      nom = character(0),
      code_postal = character(0),
      departement = character(0),
      label = character(0)
    ))
  }

  communes_cfg <- get_data_source("communes_api", "FR")
  base_url <- paste0(communes_cfg$url %||% "https://geo.api.gouv.fr", "/communes")

  result <- tryCatch({
    if (!requireNamespace("httr2", quietly = TRUE)) {
      cli::cli_abort("Package 'httr2' is required for API calls")
    }

    resp <- httr2::request(base_url) |>
      httr2::req_url_query(
        codePostal = postal_code,
        fields = "nom,code,codesPostaux,codeDepartement"
      ) |>
      httr2::req_timeout(10) |>
      httr2::req_retry(max_tries = 3, backoff = ~ 2) |>
      httr2::req_perform()

    data <- httr2::resp_body_json(resp)

    if (length(data) == 0) {
      return(data.frame(
        code_insee = character(0),
        nom = character(0),
        code_postal = character(0),
        departement = character(0),
        label = character(0)
      ))
    }

    do.call(rbind, lapply(data, function(commune) {
      codes_postaux <- paste(commune$codesPostaux, collapse = ", ")
      data.frame(
        code_insee = commune$code,
        nom = commune$nom,
        code_postal = codes_postaux,
        departement = commune$codeDepartement,
        label = sprintf("%s (%s)", commune$nom, codes_postaux),
        stringsAsFactors = FALSE
      )
    }))

  }, error = function(e) {
    cli::cli_warn("Error searching by postal code: {e$message}")
    data.frame(
      code_insee = character(0),
      nom = character(0),
      code_postal = character(0),
      departement = character(0),
      label = character(0)
    )
  })

  result
}


#' Get commune geometry
#'
#' @description
#' Retrieve the geometry (boundary polygon) of a commune by its INSEE code.
#'
#' @param code_insee Character. The 5-character INSEE code of the commune.
#'
#' @return An sf object with the commune boundary, or NULL if not found.
#'
#' @noRd
get_commune_geometry <- function(code_insee) {
  if (!grepl("^[0-9A-B]{5}$", code_insee)) {
    cli::cli_warn("Invalid INSEE code format: {code_insee}")
    return(NULL)
  }

  result <- tryCatch({
    if (!requireNamespace("httr2", quietly = TRUE)) {
      cli::cli_abort("Package 'httr2' is required for API calls")
    }

    # Request contour field (source: inst/datasources/FR.json)
    communes_cfg <- get_data_source("communes_api", "FR")
    communes_base <- communes_cfg$url %||% "https://geo.api.gouv.fr"
    url <- sprintf("%s/communes/%s?fields=nom,code,contour", communes_base, code_insee)

    resp <- httr2::request(url) |>
      httr2::req_timeout(15) |>
      httr2::req_retry(max_tries = 3, backoff = ~ 2) |>
      httr2::req_perform()

    # Parse JSON response
    data <- httr2::resp_body_json(resp)

    # Check if contour exists
    if (is.null(data$contour)) {
      cli::cli_warn("No contour geometry available for commune {code_insee}")
      return(NULL)
    }

    # Build a proper GeoJSON FeatureCollection
    feature_collection <- list(
      type = "FeatureCollection",
      features = list(
        list(
          type = "Feature",
          properties = list(
            code = data$code,
            nom = data$nom
          ),
          geometry = data$contour
        )
      )
    )

    # Write to temp file and read with sf (more robust than string parsing)
    tmp_file <- tempfile(fileext = ".geojson")
    on.exit(unlink(tmp_file), add = TRUE)

    writeLines(
      jsonlite::toJSON(feature_collection, auto_unbox = TRUE, pretty = FALSE),
      tmp_file
    )

    commune_sf <- sf::st_read(tmp_file, quiet = TRUE)

    # Ensure CRS is set (WGS84)
    if (is.na(sf::st_crs(commune_sf))) {
      sf::st_crs(commune_sf) <- 4326
    }

    # Handle GEOMETRYCOLLECTION by extracting polygons
    geom_type <- sf::st_geometry_type(commune_sf, by_geometry = FALSE)
    if (geom_type == "GEOMETRYCOLLECTION") {
      commune_sf <- sf::st_collection_extract(commune_sf, type = "POLYGON")
      geom_type <- sf::st_geometry_type(commune_sf, by_geometry = FALSE)
    }

    if (!geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
      cli::cli_warn("Commune geometry is not a polygon: {geom_type}")
      return(NULL)
    }

    # Make valid geometries
    commune_sf <- sf::st_make_valid(commune_sf)

    commune_sf

  }, error = function(e) {
    cli::cli_warn("Error getting commune geometry: {e$message}")
    NULL
  })

  result
}


#' Get commune centroid
#'
#' @description
#' Get the centroid coordinates of a commune.
#'
#' @param code_insee Character. The INSEE code of the commune.
#'
#' @return A numeric vector c(lng, lat) or NULL if not found.
#'
#' @noRd
get_commune_centroid <- function(code_insee) {
  result <- tryCatch({
    if (!requireNamespace("httr2", quietly = TRUE)) {
      cli::cli_abort("Package 'httr2' is required")
    }

    communes_cfg <- get_data_source("communes_api", "FR")
    communes_base <- communes_cfg$url %||% "https://geo.api.gouv.fr"
    url <- sprintf("%s/communes/%s?fields=centre", communes_base, code_insee)

    resp <- httr2::request(url) |>
      httr2::req_timeout(10) |>
      httr2::req_perform()

    data <- httr2::resp_body_json(resp)

    if (!is.null(data$centre) && !is.null(data$centre$coordinates)) {
      c(
        lng = data$centre$coordinates[[1]],
        lat = data$centre$coordinates[[2]]
      )
    } else {
      NULL
    }

  }, error = function(e) {
    cli::cli_warn("Error getting commune centroid: {e$message}")
    NULL
  })

  result
}


#' Get communes in department
#'
#' @description
#' Get all communes in a department for dropdown population.
#'
#' @param department_code Character. Department code.
#'
#' @return A data.frame with commune information.
#'
#' @noRd
get_communes_in_department <- function(department_code) {
  empty_result <- data.frame(
    code_insee = character(0),
    nom = character(0),
    code_postal = character(0),
    label = character(0)
  )

  if (is.null(department_code) || department_code == "") {
    return(empty_result)
  }

  result <- tryCatch({
    if (!requireNamespace("httr2", quietly = TRUE)) {
      cli::cli_abort("Package 'httr2' is required")
    }

    communes_cfg <- get_data_source("communes_api", "FR")
    url <- paste0(communes_cfg$url %||% "https://geo.api.gouv.fr", "/departements")

    resp <- httr2::request(url) |>
      httr2::req_url_path_append(department_code, "communes") |>
      httr2::req_url_query(
        fields = "nom,code,codesPostaux",
        limit = 1000
      ) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()

    data <- httr2::resp_body_json(resp)

    if (length(data) == 0) {
      return(empty_result)
    }

    communes_df <- do.call(rbind, lapply(data, function(commune) {
      codes_postaux <- if (length(commune$codesPostaux) > 0) {
        paste(commune$codesPostaux, collapse = ", ")
      } else {
        ""
      }
      data.frame(
        code_insee = commune$code,
        nom = commune$nom,
        code_postal = codes_postaux,
        label = sprintf("%s (%s)", commune$nom, commune$code),
        stringsAsFactors = FALSE
      )
    }))

    # Sort by name
    communes_df[order(communes_df$nom), ]

  }, error = function(e) {
    # Detect network/connection errors
    error_msg <- e$message
    is_network_error <- grepl(
      "HTTP request|connection|timeout|resolve|network|internet|curl",
      error_msg,
      ignore.case = TRUE
    )

    if (is_network_error) {
      attr(empty_result, "error") <- "network"
      attr(empty_result, "error_message") <- "no_internet_connection"
    } else {
      attr(empty_result, "error") <- "other"
      attr(empty_result, "error_message") <- error_msg
    }

    cli::cli_warn("Error getting communes in department: {error_msg}")
    empty_result
  })

  result
}



#' Validate INSEE code format
#'
#' @description
#' Check if a string is a valid French INSEE commune code.
#'
#' @param code Character. The code to validate.
#'
#' @return TRUE if valid, FALSE otherwise.
#'
#' @noRd
validate_insee_code <- function(code) {
  if (is.null(code) || is.na(code) || !is.character(code)) {
    return(FALSE)
  }
  grepl("^[0-9A-B]{5}$", code)
}


#' Format communes for selectize input
#'
#' @description
#' Format communes data for use in a selectize dropdown.
#'
#' @param communes_df Data.frame from search_communes or get_communes_in_department.
#'
#' @return Named character vector suitable for selectize choices.
#'
#' @noRd
format_communes_for_selectize <- function(communes_df) {
  if (is.null(communes_df) || nrow(communes_df) == 0) {
    return(character(0))
  }

  # Return named character vector (name = label displayed, value = code_insee)
  stats::setNames(
    communes_df$code_insee,
    communes_df$label
  )
}

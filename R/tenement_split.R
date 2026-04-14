#' Tenement Splitting Functions
#'
#' @description
#' Functions for splitting cadastral parcels into sub-tenements via imported
#' geometries (GeoJSON/Shapefile). Maintains the cadastral tiling invariant.
#'
#' Boucle 2: Import-based splitting only. Interactive splitting (leaflet draw)
#' is planned for boucle 3.
#'
#' @name tenement_split
#' @keywords internal
NULL


#' Split a parcel into tenements using imported polygons
#'
#' @description
#' Intersects the imported polygons with the target parcel geometry to create
#' sub-tenements. Any area of the parcel not covered by the imported polygons
#' becomes a "reste" (remainder) tenement, ensuring the tiling invariant holds.
#'
#' @param projet List. Project with $parcels, $tenements, $ugs.
#' @param parcelle_id Character. ID of the parcel to split.
#' @param sf_polygones sf object. Polygons defining the subdivision.
#'   Must overlap with the target parcel. CRS will be transformed to match.
#' @param labels Character vector. Optional labels for each fragment.
#'   If NULL, auto-generates labels (parcelle_id + letter suffix: a, b, c...).
#' @param tolerance_m2 Numeric. Area tolerance for tiling check (default 0.01 m2).
#'
#' @return Updated projet with new tenements replacing the original parcel tenement.
#' @noRd
tenement_split_by_import <- function(projet,
                                  parcelle_id,
                                  sf_polygones,
                                  labels = NULL,
                                  tolerance_m2 = 0.01) {
  if (!has_ug_data(projet)) {
    cli::cli_abort("Project must have UG data. Run ug_init_default() first.")
  }

  parcels <- projet$parcels
  tenements <- projet$tenements

  # Find the parcel
  id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(parcels))
  if (length(id_col) == 0) {
    cli::cli_abort("Parcels must have an id column")
  }
  id_col <- id_col[1]

  parcel_mask <- as.character(parcels[[id_col]]) == parcelle_id
  if (!any(parcel_mask)) {
    cli::cli_abort("Parcel not found: {parcelle_id}")
  }
  parcel_geom <- sf::st_geometry(parcels[parcel_mask, ])

  # Validate imported polygons
  if (!inherits(sf_polygones, "sf")) {
    cli::cli_abort("sf_polygones must be an sf object")
  }
  if (nrow(sf_polygones) == 0) {
    cli::cli_abort("sf_polygones is empty")
  }

  # Transform CRS to match parcels
  if (!is.na(sf::st_crs(sf_polygones)) && !is.na(sf::st_crs(parcels))) {
    if (sf::st_crs(sf_polygones) != sf::st_crs(parcels)) {
      sf_polygones <- sf::st_transform(sf_polygones, sf::st_crs(parcels))
    }
  } else if (is.na(sf::st_crs(sf_polygones))) {
    sf::st_crs(sf_polygones) <- sf::st_crs(parcels)
  }

  # Make geometries valid
  sf_polygones <- sf::st_make_valid(sf_polygones)
  parcel_geom <- sf::st_make_valid(parcel_geom)

  # Intersect imported polygons with the parcel
  fragments <- sf::st_intersection(sf::st_geometry(sf_polygones), parcel_geom)

  # Filter out non-polygon results (lines, points from boundary intersections)
  geom_types <- sf::st_geometry_type(fragments)
  polygon_mask <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
  fragments <- fragments[polygon_mask]

  if (length(fragments) == 0) {
    cli::cli_abort("No valid polygon intersections found between imported geometries and parcel {parcelle_id}")
  }

  # Cast all to MULTIPOLYGON for consistency
  fragments <- sf::st_cast(fragments, "MULTIPOLYGON")

  # Compute remainder (parcel area not covered by any fragment)
  fragments_union <- sf::st_union(fragments)
  remainder <- sf::st_difference(parcel_geom, fragments_union)

  # Filter remainder for polygons
  if (length(remainder) > 0) {
    remainder <- sf::st_make_valid(remainder)
    rem_types <- sf::st_geometry_type(remainder)
    if (any(rem_types %in% c("POLYGON", "MULTIPOLYGON"))) {
      remainder <- sf::st_cast(remainder[rem_types %in% c("POLYGON", "MULTIPOLYGON")], "MULTIPOLYGON")
      # Only keep if area is significant
      rem_area <- as.numeric(sf::st_area(remainder))
      if (any(rem_area > tolerance_m2)) {
        remainder <- remainder[rem_area > tolerance_m2]
      } else {
        remainder <- NULL
      }
    } else {
      remainder <- NULL
    }
  } else {
    remainder <- NULL
  }

  # Build list of all fragment geometries
  all_geoms <- fragments
  if (!is.null(remainder)) {
    all_geoms <- c(all_geoms, remainder)
  }
  n_fragments <- length(all_geoms)

  # Generate labels
  if (is.null(labels)) {
    if (n_fragments <= 26) {
      suffixes <- letters[seq_len(n_fragments)]
    } else {
      suffixes <- seq_len(n_fragments)
    }
    labels <- paste0(parcelle_id, "_", suffixes)
    # Last one is "reste" if remainder exists
    if (!is.null(remainder)) {
      labels[n_fragments] <- paste0(parcelle_id, "_reste")
    }
  } else {
    # Pad labels if needed
    if (length(labels) < n_fragments) {
      extra <- n_fragments - length(labels)
      labels <- c(labels, paste0(parcelle_id, "_", seq_len(extra)))
    }
  }

  # Find the current tenement(s) for this parcel
  current_tenements <- tenements[tenements$parent_parcelle_id == parcelle_id, ]
  if (nrow(current_tenements) == 0) {
    cli::cli_abort("No tenements found for parcel {parcelle_id}")
  }

  # Get the UG assignment of the original tenement(s)
  # If multiple tenements already exist for this parcel, this is a re-split
  original_ug_id <- current_tenements$ug_id[1]

  # Remove old tenements for this parcel
  tenements <- tenements[tenements$parent_parcelle_id != parcelle_id, , drop = FALSE]

  # Create new tenements
  new_tenement_ids <- paste0("tnm_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", seq_len(n_fragments))

  # Match the geometry column name of the existing tenements (could be "geom"
  # if loaded from GPKG or "geometry" by default).
  existing_geom_col <- attr(projet$tenements, "sf_column") %||% "geometry"

  new_df <- data.frame(
    tenement_id = new_tenement_ids,
    parent_parcelle_id = rep(parcelle_id, n_fragments),
    ug_id = rep(original_ug_id, n_fragments),
    surface_m2 = as.numeric(sf::st_area(all_geoms)),
    stringsAsFactors = FALSE
  )
  new_df[[existing_geom_col]] <- all_geoms
  new_tenements <- sf::st_sf(new_df, sf_column_name = existing_geom_col)
  sf::st_crs(new_tenements) <- sf::st_crs(parcels)

  # Append new tenements
  tenements <- rbind(tenements, new_tenements)

  projet$tenements <- tenements

  # Validate tiling invariant
  validate_tiling(projet, parcelle_id, tolerance_m2)

  # Validate all invariants
  projet_validate(projet)

  cli::cli_alert_success(
    "Split parcel {parcelle_id} into {n_fragments} tenement(s){if (!is.null(remainder)) ' (+ remainder)' else ''}"
  )

  projet
}


#' Split a parcel into tenements using drawn geometries
#'
#' @description
#' Same as \code{tenement_split_by_import} but accepts raw GeoJSON from
#' leaflet.extras draw events. Parses the drawn features into an sf object
#' and delegates to the import-based split.
#'
#' @param projet List. Project with $parcels, $tenements, $ugs.
#' @param parcelle_id Character. ID of the parcel to split.
#' @param geojson Character. GeoJSON string from leaflet draw event.
#' @param tolerance_m2 Numeric. Area tolerance (default 0.01 m2).
#'
#' @return Updated projet with new tenements.
#' @noRd
tenement_split_by_geometry <- function(projet,
                                    parcelle_id,
                                    geojson,
                                    tolerance_m2 = 0.01) {
  if (is.null(geojson) || nchar(geojson) == 0) {
    cli::cli_abort("No geometry provided")
  }

  # Parse GeoJSON to sf
  sf_polygones <- tryCatch({
    sf::st_read(geojson, quiet = TRUE)
  }, error = function(e) {
    cli::cli_abort("Invalid GeoJSON: {e$message}")
  })

  if (nrow(sf_polygones) == 0) {
    cli::cli_abort("No features found in GeoJSON")
  }

  # Filter to polygons only
  geom_types <- sf::st_geometry_type(sf_polygones)
  polygon_mask <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
  if (!any(polygon_mask)) {
    cli::cli_abort("No polygon features found. Draw polygons, not lines or points.")
  }
  sf_polygones <- sf_polygones[polygon_mask, ]

  # Delegate to import-based split
  tenement_split_by_import(
    projet,
    parcelle_id,
    sf_polygones,
    tolerance_m2 = tolerance_m2
  )
}


#' Split all tenements crossed by a drawn polygon
#'
#' @description
#' Auto-detects which tenements the drawn polygon(s) intersect and splits
#' each one into "inside polygon" and "outside polygon" sub-tenements.
#' Each new sub-tenement keeps the UG assignment of its parent tenement.
#'
#' Workflow: user draws a polygon spanning multiple tenements, every
#' affected tenement is automatically split — no need to pick one parcel.
#'
#' @param projet List. Project with $tenements and $ugs.
#' @param geojson Character. GeoJSON of the cutting polygon(s) from leaflet draw.
#' @param tolerance_m2 Numeric. Area tolerance to discard slivers (default 0.01).
#'
#' @return Updated projet with all crossed tenements replaced by sub-tenements.
#' @noRd
tenement_split_by_drawn_polygon <- function(projet, geojson, tolerance_m2 = 0.01) {
  if (!has_ug_data(projet)) {
    cli::cli_abort("Project must have UG data.")
  }

  tenements <- projet$tenements

  # Parse drawn polygon(s)
  polys_sf <- tryCatch({
    sf::st_read(geojson, quiet = TRUE)
  }, error = function(e) {
    cli::cli_abort("Invalid GeoJSON: {e$message}")
  })
  geom_types <- sf::st_geometry_type(polys_sf)
  poly_mask <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
  if (!any(poly_mask)) {
    cli::cli_abort("No polygon features found in the drawn shape.")
  }
  polys_sf <- polys_sf[poly_mask, ]

  # Match CRS
  if (!is.na(sf::st_crs(polys_sf)) && !is.na(sf::st_crs(tenements))) {
    if (sf::st_crs(polys_sf) != sf::st_crs(tenements)) {
      polys_sf <- sf::st_transform(polys_sf, sf::st_crs(tenements))
    }
  } else if (is.na(sf::st_crs(polys_sf))) {
    sf::st_crs(polys_sf) <- sf::st_crs(tenements)
  }

  cutter <- sf::st_make_valid(sf::st_union(sf::st_geometry(polys_sf)))

  # Find tenements that intersect the cutter
  hits <- tryCatch(
    sf::st_intersects(sf::st_geometry(tenements), cutter, sparse = FALSE)[, 1],
    error = function(e) rep(FALSE, nrow(tenements))
  )
  if (!any(hits)) {
    cli::cli_abort("The drawn polygon does not intersect any tenement.")
  }

  affected_idx <- which(hits)
  cli::cli_alert_info(
    "Splitting {length(affected_idx)} tenement(s) crossed by the drawn polygon"
  )

  # Detect existing geometry column name
  existing_geom_col <- attr(tenements, "sf_column") %||% "geometry"

  # Process each affected tenement
  all_new <- list()
  removed_ids <- character(0)
  ts <- format(Sys.time(), "%Y%m%d%H%M%S")
  counter <- 0L

  for (i in affected_idx) {
    tn <- tenements[i, ]
    tn_geom <- sf::st_make_valid(sf::st_geometry(tn))
    parent_id <- tn$parent_parcelle_id
    ug_id <- tn$ug_id

    inside <- tryCatch(
      sf::st_make_valid(sf::st_intersection(tn_geom, cutter)),
      error = function(e) NULL
    )
    outside <- tryCatch(
      sf::st_make_valid(sf::st_difference(tn_geom, cutter)),
      error = function(e) NULL
    )

    # Filter to polygons + drop slivers
    pieces <- list()
    for (g in list(inside, outside)) {
      if (is.null(g) || length(g) == 0) next
      g_types <- sf::st_geometry_type(g)
      g <- g[g_types %in% c("POLYGON", "MULTIPOLYGON")]
      if (length(g) == 0) next
      g <- sf::st_cast(g, "MULTIPOLYGON")
      areas <- as.numeric(sf::st_area(g))
      g <- g[areas > tolerance_m2]
      if (length(g) > 0) pieces <- c(pieces, list(g))
    }

    if (length(pieces) < 2) next  # No real split for this tenement

    geoms <- do.call(c, pieces)
    n <- length(geoms)
    counter <- counter + 1L
    new_ids <- paste0("tnm_", ts, "_", counter, "_", seq_len(n))

    new_df <- data.frame(
      tenement_id = new_ids,
      parent_parcelle_id = rep(parent_id, n),
      ug_id = rep(ug_id, n),
      surface_m2 = as.numeric(sf::st_area(geoms)),
      stringsAsFactors = FALSE
    )
    new_df[[existing_geom_col]] <- geoms
    new_sf <- sf::st_sf(new_df, sf_column_name = existing_geom_col)
    sf::st_crs(new_sf) <- sf::st_crs(tenements)

    all_new <- c(all_new, list(new_sf))
    removed_ids <- c(removed_ids, tn$tenement_id)
  }

  if (length(all_new) == 0) {
    cli::cli_abort("The drawn polygon did not split any tenement (only edge touches).")
  }

  # Remove original tenements + append new sub-tenements
  tenements <- tenements[!tenements$tenement_id %in% removed_ids, , drop = FALSE]
  combined <- do.call(rbind, all_new)
  tenements <- rbind(tenements, combined)
  projet$tenements <- tenements

  projet_validate(projet)

  cli::cli_alert_success(
    "Split {length(removed_ids)} tenement(s) into {nrow(combined)} sub-tenements"
  )

  projet
}


#' Split an tenement by drawing a cutting line
#'
#' @description
#' Splits an tenement (or the whole parcel if unsplit) along a drawn polyline.
#' Uses \code{lwgeom::st_split()} if available, otherwise falls back to
#' a buffer-based approach.
#'
#' Workflow: user selects an tenement on the map, draws a line across it,
#' the tenement is cut into 2+ sub-tenements along that line.
#'
#' @param projet List. Project with $parcels, $tenements, $ugs.
#' @param tenement_id Character. ID of the tenement to split.
#' @param line_geojson Character. GeoJSON of the cutting line(s).
#' @param tolerance_m2 Numeric. Area tolerance (default 0.01 m2).
#'
#' @return Updated projet with the tenement replaced by sub-tenements.
#' @noRd
tenement_split_by_line <- function(projet,
                                tenement_id,
                                line_geojson,
                                tolerance_m2 = 0.01) {
  if (!has_ug_data(projet)) {
    cli::cli_abort("Project must have UG data.")
  }

  tenements <- projet$tenements
  ugs <- projet$ugs

  # Find the target tenement
  tenement_mask <- tenements$tenement_id == tenement_id
  if (!any(tenement_mask)) {
    cli::cli_abort("Tenement not found: {tenement_id}")
  }

  target_tenement <- tenements[tenement_mask, ]
  parent_id <- target_tenement$parent_parcelle_id
  original_ug_id <- target_tenement$ug_id

  # Parse the cutting line from GeoJSON
  line_sf <- tryCatch({
    sf::st_read(line_geojson, quiet = TRUE)
  }, error = function(e) {
    cli::cli_abort("Invalid GeoJSON line: {e$message}")
  })

  if (nrow(line_sf) == 0) {
    cli::cli_abort("No features in the cutting line GeoJSON")
  }

  # Keep only lines
  geom_types <- sf::st_geometry_type(line_sf)
  line_mask <- geom_types %in% c("LINESTRING", "MULTILINESTRING")
  if (!any(line_mask)) {
    cli::cli_abort("No line features found. Draw a line, not a polygon.")
  }
  line_sf <- line_sf[line_mask, ]

  # Transform CRS to match tenements
  if (!is.na(sf::st_crs(line_sf)) && !is.na(sf::st_crs(tenements))) {
    if (sf::st_crs(line_sf) != sf::st_crs(tenements)) {
      line_sf <- sf::st_transform(line_sf, sf::st_crs(tenements))
    }
  } else if (is.na(sf::st_crs(line_sf))) {
    sf::st_crs(line_sf) <- sf::st_crs(tenements)
  }

  # Union all lines into one cutting geometry
  cutting_line <- sf::st_union(sf::st_geometry(line_sf))

  # Split the tenement geometry using the line
  tenement_geom <- sf::st_geometry(target_tenement)

  fragments <- NULL

  # Try lwgeom::st_split first (best method)
  if (requireNamespace("lwgeom", quietly = TRUE)) {
    tryCatch({
      split_result <- lwgeom::st_split(tenement_geom, cutting_line)
      # st_split returns a GEOMETRYCOLLECTION — extract polygons
      split_collected <- sf::st_collection_extract(split_result, "POLYGON")
      if (length(split_collected) > 1) {
        fragments <- split_collected
      }
    }, error = function(e) {
      cli::cli_warn("lwgeom::st_split failed: {e$message}. Trying buffer fallback.")
    })
  }

  # Fallback: buffer the line slightly and use st_difference/st_intersection
  if (is.null(fragments)) {
    # Buffer the line to create a thin "blade"
    blade <- sf::st_buffer(cutting_line, dist = 0.00001)  # ~1m in WGS84
    blade <- sf::st_make_valid(blade)

    part_a <- sf::st_difference(tenement_geom, blade)
    part_a <- sf::st_make_valid(part_a)

    # Extract individual polygons from potentially multi-part result
    parts <- sf::st_cast(part_a, "POLYGON")
    # Filter tiny slivers
    areas <- as.numeric(sf::st_area(parts))
    parts <- parts[areas > tolerance_m2]

    if (length(parts) > 1) {
      fragments <- parts
    }
  }

  if (is.null(fragments) || length(fragments) < 2) {
    cli::cli_abort("The cutting line does not split the tenement into multiple parts. Ensure the line crosses the tenement completely.")
  }

  # Cast to MULTIPOLYGON for consistency
  fragments <- sf::st_cast(fragments, "MULTIPOLYGON")
  n_fragments <- length(fragments)

  # Generate IDs and labels
  new_ids <- paste0("tnm_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", seq_len(n_fragments))
  if (n_fragments <= 26) {
    suffixes <- letters[seq_len(n_fragments)]
  } else {
    suffixes <- seq_len(n_fragments)
  }

  # Remove old tenement
  tenements <- tenements[!tenement_mask, , drop = FALSE]

  # Detect existing geometry column name (could be "geom" if loaded from GPKG)
  existing_geom_col <- attr(projet$tenements, "sf_column") %||% "geometry"

  # Create new sub-tenements with the SAME geometry column name as existing
  new_df <- data.frame(
    tenement_id = new_ids,
    parent_parcelle_id = rep(parent_id, n_fragments),
    ug_id = rep(original_ug_id, n_fragments),
    surface_m2 = as.numeric(sf::st_area(fragments)),
    stringsAsFactors = FALSE
  )
  new_df[[existing_geom_col]] <- fragments
  new_tenements <- sf::st_sf(new_df, sf_column_name = existing_geom_col)
  sf::st_crs(new_tenements) <- sf::st_crs(projet$tenements)

  tenements <- rbind(tenements, new_tenements)
  projet$tenements <- tenements

  # Validate
  projet_validate(projet)

  cli::cli_alert_success(
    "Split tenement {tenement_id} into {n_fragments} sub-tenements using cutting line"
  )

  projet
}


#' Undo a parcel split (restore single tenement)
#'
#' @description
#' Removes all tenements for a given parcel and replaces them with a single
#' tenement covering the entire parcel geometry.
#'
#' @param projet List. Project with $parcels, $tenements, $ugs.
#' @param parcelle_id Character. ID of the parcel to unsplit.
#'
#' @return Updated projet with a single tenement for the specified parcel.
#' @noRd
tenement_undo_split <- function(projet, parcelle_id) {
  if (!has_ug_data(projet)) {
    cli::cli_abort("Project must have UG data.")
  }

  parcels <- projet$parcels
  tenements <- projet$tenements
  ugs <- projet$ugs

  # Find the parcel
  id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(parcels))
  if (length(id_col) == 0) {
    cli::cli_abort("Parcels must have an id column")
  }
  id_col <- id_col[1]

  parcel_mask <- as.character(parcels[[id_col]]) == parcelle_id
  if (!any(parcel_mask)) {
    cli::cli_abort("Parcel not found: {parcelle_id}")
  }

  # Get current tenements for this parcel
  parcel_tenements <- tenements[tenements$parent_parcelle_id == parcelle_id, ]
  if (nrow(parcel_tenements) <= 1) {
    cli::cli_alert_info("Parcel {parcelle_id} is not split (has {nrow(parcel_tenements)} tenement)")
    return(projet)
  }

  # Determine which UG to assign the restored tenement to
  # Use the UG of the first tenement (or create a new one if mixed)
  ug_ids <- unique(parcel_tenements$ug_id)
  target_ug_id <- ug_ids[1]

  # Remove all tenements for this parcel
  tenements <- tenements[tenements$parent_parcelle_id != parcelle_id, , drop = FALSE]

  # Create a single tenement from the parcel geometry
  parcel_sf <- parcels[parcel_mask, ]
  new_tenement_id <- paste0("tnm_", format(Sys.time(), "%Y%m%d%H%M%S"), "_1")

  # Match the geometry column name of the existing tenements
  existing_geom_col <- attr(projet$tenements, "sf_column") %||% "geometry"

  new_df <- data.frame(
    tenement_id = new_tenement_id,
    parent_parcelle_id = parcelle_id,
    ug_id = target_ug_id,
    surface_m2 = if ("contenance" %in% names(parcel_sf)) {
      as.numeric(parcel_sf$contenance)
    } else {
      as.numeric(sf::st_area(parcel_sf))
    },
    stringsAsFactors = FALSE
  )
  new_df[[existing_geom_col]] <- sf::st_geometry(parcel_sf)
  new_tenement <- sf::st_sf(new_df, sf_column_name = existing_geom_col)

  tenements <- rbind(tenements, new_tenement)

  # Remove UGs that became empty
  active_ug_ids <- unique(tenements$ug_id)
  ugs <- ugs[ugs$ug_id %in% active_ug_ids, , drop = FALSE]

  projet$tenements <- tenements
  projet$ugs <- ugs

  projet_validate(projet)

  cli::cli_alert_success(
    "Restored parcel {parcelle_id} to a single tenement (was {nrow(parcel_tenements)} fragments)"
  )

  projet
}


#' Validate the tiling invariant for a parcel
#'
#' @description
#' Checks that the union of all tenements for a parcel covers the entire
#' parcel geometry (within tolerance).
#'
#' @param projet List. Project.
#' @param parcelle_id Character. Parcel ID to check.
#' @param tolerance_m2 Numeric. Area tolerance.
#'
#' @return Invisible TRUE if valid.
#' @noRd
validate_tiling <- function(projet, parcelle_id, tolerance_m2 = 0.01) {
  parcels <- projet$parcels
  tenements <- projet$tenements

  id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(parcels))
  id_col <- id_col[1]

  parcel_geom <- sf::st_geometry(parcels[as.character(parcels[[id_col]]) == parcelle_id, ])
  parcel_area <- as.numeric(sf::st_area(parcel_geom))

  parcel_tenements <- tenements[tenements$parent_parcelle_id == parcelle_id, ]
  tenements_union <- sf::st_union(sf::st_geometry(parcel_tenements))
  tenements_area <- as.numeric(sf::st_area(tenements_union))

  area_diff <- abs(parcel_area - tenements_area)

  if (area_diff > tolerance_m2) {
    cli::cli_abort(
      "Tiling invariant violated for parcel {parcelle_id}: area difference = {round(area_diff, 4)} m\u00b2 (tolerance: {tolerance_m2} m\u00b2)"
    )
  }

  invisible(TRUE)
}

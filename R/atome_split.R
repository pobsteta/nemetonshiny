#' Atome Splitting Functions
#'
#' @description
#' Functions for splitting cadastral parcels into sub-atoms via imported
#' geometries (GeoJSON/Shapefile). Maintains the cadastral tiling invariant.
#'
#' Boucle 2: Import-based splitting only. Interactive splitting (leaflet draw)
#' is planned for boucle 3.
#'
#' @name atome_split
#' @keywords internal
NULL


#' Split a parcel into atoms using imported polygons
#'
#' @description
#' Intersects the imported polygons with the target parcel geometry to create
#' sub-atoms. Any area of the parcel not covered by the imported polygons
#' becomes a "reste" (remainder) atom, ensuring the tiling invariant holds.
#'
#' @param projet List. Project with $parcels, $atomes, $ugs.
#' @param parcelle_id Character. ID of the parcel to split.
#' @param sf_polygones sf object. Polygons defining the subdivision.
#'   Must overlap with the target parcel. CRS will be transformed to match.
#' @param labels Character vector. Optional labels for each fragment.
#'   If NULL, auto-generates labels (parcelle_id + letter suffix: a, b, c...).
#' @param tolerance_m2 Numeric. Area tolerance for tiling check (default 0.01 m2).
#'
#' @return Updated projet with new atoms replacing the original parcel atom.
#' @noRd
atome_split_by_import <- function(projet,
                                  parcelle_id,
                                  sf_polygones,
                                  labels = NULL,
                                  tolerance_m2 = 0.01) {
  if (!has_ug_data(projet)) {
    cli::cli_abort("Project must have UG data. Run ug_init_default() first.")
  }

  parcels <- projet$parcels
  atomes <- projet$atomes

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

  # Find the current atom(s) for this parcel
  current_atoms <- atomes[atomes$parent_parcelle_id == parcelle_id, ]
  if (nrow(current_atoms) == 0) {
    cli::cli_abort("No atoms found for parcel {parcelle_id}")
  }

  # Get the UG assignment of the original atom(s)
  # If multiple atoms already exist for this parcel, this is a re-split
  original_ug_id <- current_atoms$ug_id[1]

  # Remove old atoms for this parcel
  atomes <- atomes[atomes$parent_parcelle_id != parcelle_id, , drop = FALSE]

  # Create new atoms
  new_atome_ids <- paste0("atm_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", seq_len(n_fragments))

  new_atomes <- sf::st_sf(
    atome_id = new_atome_ids,
    parent_parcelle_id = rep(parcelle_id, n_fragments),
    ug_id = rep(original_ug_id, n_fragments),
    surface_m2 = as.numeric(sf::st_area(all_geoms)),
    geometry = all_geoms
  )
  sf::st_crs(new_atomes) <- sf::st_crs(parcels)

  # Append new atoms
  atomes <- rbind(atomes, new_atomes)

  projet$atomes <- atomes

  # Validate tiling invariant
  validate_tiling(projet, parcelle_id, tolerance_m2)

  # Validate all invariants
  projet_validate(projet)

  cli::cli_alert_success(
    "Split parcel {parcelle_id} into {n_fragments} atom(s){if (!is.null(remainder)) ' (+ remainder)' else ''}"
  )

  projet
}


#' Undo a parcel split (restore single atom)
#'
#' @description
#' Removes all atoms for a given parcel and replaces them with a single
#' atom covering the entire parcel geometry.
#'
#' @param projet List. Project with $parcels, $atomes, $ugs.
#' @param parcelle_id Character. ID of the parcel to unsplit.
#'
#' @return Updated projet with a single atom for the specified parcel.
#' @noRd
atome_undo_split <- function(projet, parcelle_id) {
  if (!has_ug_data(projet)) {
    cli::cli_abort("Project must have UG data.")
  }

  parcels <- projet$parcels
  atomes <- projet$atomes
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

  # Get current atoms for this parcel
  parcel_atoms <- atomes[atomes$parent_parcelle_id == parcelle_id, ]
  if (nrow(parcel_atoms) <= 1) {
    cli::cli_alert_info("Parcel {parcelle_id} is not split (has {nrow(parcel_atoms)} atom)")
    return(projet)
  }

  # Determine which UG to assign the restored atom to
  # Use the UG of the first atom (or create a new one if mixed)
  ug_ids <- unique(parcel_atoms$ug_id)
  target_ug_id <- ug_ids[1]

  # Remove all atoms for this parcel
  atomes <- atomes[atomes$parent_parcelle_id != parcelle_id, , drop = FALSE]

  # Create a single atom from the parcel geometry
  parcel_sf <- parcels[parcel_mask, ]
  new_atome_id <- paste0("atm_", format(Sys.time(), "%Y%m%d%H%M%S"), "_1")

  new_atome <- sf::st_sf(
    atome_id = new_atome_id,
    parent_parcelle_id = parcelle_id,
    ug_id = target_ug_id,
    surface_m2 = if ("contenance" %in% names(parcel_sf)) {
      as.numeric(parcel_sf$contenance)
    } else {
      as.numeric(sf::st_area(parcel_sf))
    },
    geometry = sf::st_geometry(parcel_sf)
  )

  atomes <- rbind(atomes, new_atome)

  # Remove UGs that became empty
  active_ug_ids <- unique(atomes$ug_id)
  ugs <- ugs[ugs$ug_id %in% active_ug_ids, , drop = FALSE]

  projet$atomes <- atomes
  projet$ugs <- ugs

  projet_validate(projet)

  cli::cli_alert_success(
    "Restored parcel {parcelle_id} to a single atom (was {nrow(parcel_atoms)} fragments)"
  )

  projet
}


#' Validate the tiling invariant for a parcel
#'
#' @description
#' Checks that the union of all atoms for a parcel covers the entire
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
  atomes <- projet$atomes

  id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(parcels))
  id_col <- id_col[1]

  parcel_geom <- sf::st_geometry(parcels[as.character(parcels[[id_col]]) == parcelle_id, ])
  parcel_area <- as.numeric(sf::st_area(parcel_geom))

  parcel_atoms <- atomes[atomes$parent_parcelle_id == parcelle_id, ]
  atoms_union <- sf::st_union(sf::st_geometry(parcel_atoms))
  atoms_area <- as.numeric(sf::st_area(atoms_union))

  area_diff <- abs(parcel_area - atoms_area)

  if (area_diff > tolerance_m2) {
    cli::cli_abort(
      "Tiling invariant violated for parcel {parcelle_id}: area difference = {round(area_diff, 4)} m\u00b2 (tolerance: {tolerance_m2} m\u00b2)"
    )
  }

  invisible(TRUE)
}

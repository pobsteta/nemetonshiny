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

  # Use planar GEOS operations (S2 rejects self-touching imported polygons)
  prev_s2 <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(prev_s2), add = TRUE)

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

  # Keep BOTH the cadastral and SIG surfaces for each sub-tenement:
  # - surface_m2: cadastral surface proportionally distributed from the
  #   parent parcel's "contenance" (preserves the official total)
  # - surface_sig_m2: raw geometric area from st_area()
  parcel_row <- parcels[parcel_mask, ]
  parcel_total <- if ("contenance" %in% names(parcel_row)) {
    as.numeric(parcel_row$contenance)[1]
  } else {
    as.numeric(sf::st_area(parcel_row))
  }
  geom_areas <- as.numeric(sf::st_area(all_geoms))
  total_geom <- sum(geom_areas)
  sub_cadastral <- if (!is.na(parcel_total) && total_geom > 0) {
    geom_areas * (parcel_total / total_geom)
  } else {
    geom_areas
  }

  new_df <- data.frame(
    tenement_id = new_tenement_ids,
    parent_parcelle_id = rep(parcelle_id, n_fragments),
    ug_id = rep(original_ug_id, n_fragments),
    surface_m2 = sub_cadastral,
    surface_sig_m2 = geom_areas,
    stringsAsFactors = FALSE
  )
  new_df[[existing_geom_col]] <- all_geoms
  new_tenements <- sf::st_sf(new_df, sf_column_name = existing_geom_col)
  sf::st_crs(new_tenements) <- sf::st_crs(parcels)

  # Append new tenements
  tenements <- rbind(tenements, new_tenements)

  projet$tenements <- tenements

  # Validate tiling invariant using the adaptive default tolerance
  # (max(1 m², 0.05% of parcel area)) when the caller didn't override it.
  validate_tiling(projet, parcelle_id,
                  tolerance_m2 = if (identical(tolerance_m2, 0.01)) NULL else tolerance_m2)

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

  # Match CRS. Drawn GeoJSON coming from Leaflet is always in EPSG:4326,
  # but sf::st_read on an in-memory GeoJSON string sometimes returns NA —
  # force WGS84 then reproject to the tenements CRS. Never silently
  # re-tag coordinates without a transform: that would misalign geometry.
  if (is.na(sf::st_crs(polys_sf))) {
    sf::st_crs(polys_sf) <- 4326
  }
  if (!is.na(sf::st_crs(tenements)) &&
      sf::st_crs(polys_sf) != sf::st_crs(tenements)) {
    polys_sf <- sf::st_transform(polys_sf, sf::st_crs(tenements))
  }

  # For polygon cuts we use S2 spherical geometry directly on the
  # 4326 lng/lat coordinates. This is the ONLY way to preserve the
  # drawn polygon vertices exactly as the user traced them — any
  # 4326 ↔ 2154 round-trip introduces a small but visible offset at
  # high zoom. S2 can be strict about self-touching vertices in
  # imported GeoJSON; we run st_make_valid on the cutter and on each
  # tenement before the op. If S2 still throws, we fall back to GEOS
  # operations in Lambert 93 per-tenement.
  orig_crs <- sf::st_crs(tenements)
  prev_s2 <- sf::sf_use_s2()
  sf::sf_use_s2(TRUE)
  on.exit(sf::sf_use_s2(prev_s2), add = TRUE)

  # Normalize valid geometries (S2-compatible)
  polys_sf <- tryCatch(sf::st_make_valid(polys_sf), error = function(e) polys_sf)
  cutter <- tryCatch(
    sf::st_make_valid(sf::st_union(sf::st_geometry(polys_sf))),
    error = function(e) sf::st_union(sf::st_geometry(polys_sf))
  )

  # Find tenements that intersect the cutter (S2, native 4326)
  hits <- tryCatch(
    sf::st_intersects(sf::st_geometry(tenements), cutter, sparse = FALSE)[, 1],
    error = function(e) {
      # S2 rejected one of the geometries — fall back to planar on 2154
      sf::sf_use_s2(FALSE)
      tw <- sf::st_transform(tenements, 2154)
      cw <- sf::st_transform(cutter, 2154)
      sf::st_intersects(sf::st_geometry(tw), cw, sparse = FALSE)[, 1]
    }
  )
  if (!any(hits)) {
    cli::cli_abort("The drawn polygon does not intersect any tenement.")
  }

  affected_idx <- which(hits)
  cli::cli_alert_info(
    "[BACKEND POLYGONE] d\u00e9coupe de {length(affected_idx)} t\u00e8nement{?s}"
  )

  # Detect existing geometry column name
  existing_geom_col <- attr(tenements, "sf_column") %||% "geometry"

  # Process each affected tenement
  all_new <- list()
  removed_ids <- character(0)
  ts <- format(Sys.time(), "%Y%m%d%H%M%S")
  counter <- 0L

  n_fully_contained <- 0L   # polygon fully covers a tenement (nothing to split)

  for (i in affected_idx) {
    tn <- tenements[i, ]
    tn_geom <- sf::st_make_valid(sf::st_geometry(tn))
    parent_id <- tn$parent_parcelle_id
    ug_id <- tn$ug_id

    # S2 intersection/difference on the 4326 geometries — keeps the
    # drawn polygon vertices exactly. If S2 rejects a geometry we
    # fall back to GEOS on Lambert 93 for this tenement only.
    run_cut <- function() {
      list(
        inside  = sf::st_make_valid(sf::st_intersection(tn_geom, cutter)),
        outside = sf::st_make_valid(sf::st_difference(tn_geom, cutter))
      )
    }
    res <- tryCatch(run_cut(), error = function(e) NULL)
    if (is.null(res)) {
      prev_inner <- sf::sf_use_s2()
      sf::sf_use_s2(FALSE)
      res <- tryCatch({
        tn_m  <- sf::st_transform(tn_geom, 2154)
        cut_m <- sf::st_transform(cutter, 2154)
        list(
          inside  = sf::st_transform(
            sf::st_make_valid(sf::st_intersection(tn_m, cut_m)), orig_crs
          ),
          outside = sf::st_transform(
            sf::st_make_valid(sf::st_difference(tn_m, cut_m)), orig_crs
          )
        )
      }, error = function(e) list(inside = NULL, outside = NULL))
      sf::sf_use_s2(prev_inner)
    }
    inside  <- res$inside
    outside <- res$outside

    # Filter to polygons + drop slivers (areas always in m² — st_area
    # uses great-circle distance when the CRS is lng/lat)
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

    if (length(pieces) < 2) {
      # Polygon fully contained this tenement (only "inside" = full tenement)
      # or only edge-touched it (no real overlap). Skip but count.
      n_fully_contained <- n_fully_contained + 1L
      next
    }

    geoms <- do.call(c, pieces)
    n <- length(geoms)
    counter <- counter + 1L
    new_ids <- paste0("tnm_", ts, "_", counter, "_", seq_len(n))

    # Keep BOTH cadastral and SIG surfaces for each sub-tenement.
    geom_areas <- as.numeric(sf::st_area(geoms))
    total_geom <- sum(geom_areas)
    original_cadastral <- as.numeric(tn$surface_m2)
    sub_cadastral <- if (total_geom > 0 && !is.na(original_cadastral)) {
      geom_areas * (original_cadastral / total_geom)
    } else {
      geom_areas
    }

    new_df <- data.frame(
      tenement_id = new_ids,
      parent_parcelle_id = rep(parent_id, n),
      ug_id = rep(ug_id, n),
      surface_m2 = sub_cadastral,
      surface_sig_m2 = geom_areas,
      stringsAsFactors = FALSE
    )
    new_df[[existing_geom_col]] <- geoms
    new_sf <- sf::st_sf(new_df, sf_column_name = existing_geom_col)
    sf::st_crs(new_sf) <- sf::st_crs(tenements)

    all_new <- c(all_new, list(new_sf))
    removed_ids <- c(removed_ids, tn$tenement_id)
  }

  if (length(all_new) == 0) {
    if (n_fully_contained > 0) {
      # Polygon fully wraps existing tenements — no cut needed.
      # Return the project unchanged rather than erroring.
      cli::cli_alert_info(
        "The drawn polygon fully covers {n_fully_contained} tenement(s); no cut was needed (polygon boundaries do not cross the tenement geometry)."
      )
      return(projet)
    }
    cli::cli_abort(c(
      "The drawn polygon did not split any tenement.",
      i = "The polygon only touches edges or the overlap is below tolerance ({tolerance_m2} m\u00b2).",
      i = "Draw a polygon whose boundary actually crosses the tenement geometry."
    ))
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


#' Split all tenements crossed by a drawn polyline
#'
#' @description
#' Auto-detects which tenements the drawn line(s) cross and splits each
#' one along the line(s). Each new sub-tenement keeps the UG and parent
#' parcel of its source tenement.
#'
#' Uses \code{lwgeom::st_split()} when available; falls back to a thin
#' buffer + st_difference approach.
#'
#' @param projet List. Project with $tenements and $ugs.
#' @param geojson Character. GeoJSON of the cutting polyline(s).
#' @param tolerance_m2 Numeric. Sliver area threshold (default 0.01).
#'
#' @return Updated projet with crossed tenements replaced by sub-tenements.
#' @noRd
tenement_split_by_drawn_line <- function(projet, geojson, tolerance_m2 = 0.01) {
  if (!has_ug_data(projet)) {
    cli::cli_abort("Project must have UG data.")
  }

  tenements <- projet$tenements

  # Parse drawn line(s)
  line_sf <- tryCatch({
    sf::st_read(geojson, quiet = TRUE)
  }, error = function(e) {
    cli::cli_abort("Invalid GeoJSON: {e$message}")
  })
  geom_types <- sf::st_geometry_type(line_sf)
  line_mask <- geom_types %in% c("LINESTRING", "MULTILINESTRING")
  if (!any(line_mask)) {
    cli::cli_abort("No line features found. Draw a polyline.")
  }
  line_sf <- line_sf[line_mask, ]

  # Match CRS. Drawn GeoJSON coming from Leaflet is always in EPSG:4326,
  # but sf::st_read on an in-memory GeoJSON string sometimes returns NA —
  # force WGS84 then reproject to the tenements CRS. Never silently
  # re-tag coordinates without a transform: that would misalign geometry.
  if (is.na(sf::st_crs(line_sf))) {
    sf::st_crs(line_sf) <- 4326
  }
  if (!is.na(sf::st_crs(tenements)) &&
      sf::st_crs(line_sf) != sf::st_crs(tenements)) {
    line_sf <- sf::st_transform(line_sf, sf::st_crs(tenements))
  }

  # Planar GEOS on Lambert 93. lwgeom::st_split (step 2 below) requires
  # GEOS, and detecting hits in the same CRS guarantees preview and
  # actual split agree on which tenements are affected. S2 is too
  # error-prone on imported GeoJSON linestrings.
  orig_crs <- sf::st_crs(tenements)
  use_projection <- !is.na(orig_crs) && isTRUE(sf::st_is_longlat(tenements))

  prev_s2 <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(prev_s2), add = TRUE)

  if (use_projection) {
    work_crs <- sf::st_crs(2154)
    tenements_work <- sf::st_transform(tenements, work_crs)
    line_sf        <- sf::st_transform(line_sf, work_crs)
  } else {
    tenements_work <- tenements
  }

  cutting_line <- sf::st_union(sf::st_geometry(line_sf))

  # Find tenements that intersect the line (planar 2154)
  hits <- tryCatch(
    sf::st_intersects(sf::st_geometry(tenements_work), cutting_line,
                      sparse = FALSE)[, 1],
    error = function(e) rep(FALSE, nrow(tenements_work))
  )
  cli::cli_alert_info(
    "[BACKEND POLYLIGNE] d\u00e9tection : {sum(hits)} t\u00e8nement{?s} crois\u00e9{?s} (CRS={if (use_projection) 'EPSG:2154' else as.character(orig_crs$input)})"
  )
  if (!any(hits)) {
    cli::cli_abort("The drawn line does not cross any tenement.")
  }

  affected_idx <- which(hits)
  cli::cli_alert_info(
    "[BACKEND POLYLIGNE] d\u00e9coupe de {length(affected_idx)} t\u00e8nement{?s}"
  )

  existing_geom_col <- attr(tenements, "sf_column") %||% "geometry"

  # Helper: split one polygon geometry by the cutting line.
  # Requires lwgeom (listed in DESCRIPTION::Imports) for exact splits
  # that produce adjacent polygons with no visible gap.
  split_one <- function(poly_geom) {
    if (!requireNamespace("lwgeom", quietly = TRUE)) {
      cli::cli_abort(c(
        "Package 'lwgeom' is required to split tenements along a line.",
        i = "Install it with: install.packages('lwgeom')"
      ))
    }
    sp <- lwgeom::st_split(poly_geom, cutting_line)
    polys <- sf::st_collection_extract(sp, "POLYGON")
    if (length(polys) < 2) return(NULL)
    polys
  }

  all_new <- list()
  removed_ids <- character(0)
  ts <- format(Sys.time(), "%Y%m%d%H%M%S")
  counter <- 0L

  for (i in affected_idx) {
    tn <- tenements_work[i, ]
    tn_geom <- sf::st_make_valid(sf::st_geometry(tn))
    parent_id <- tn$parent_parcelle_id
    ug_id <- tn$ug_id

    fragments <- split_one(tn_geom)
    if (is.null(fragments) || length(fragments) < 2) next

    fragments <- sf::st_cast(fragments, "MULTIPOLYGON")
    n <- length(fragments)
    counter <- counter + 1L
    new_ids <- paste0("tnm_", ts, "_", counter, "_", seq_len(n))

    # Keep BOTH cadastral and SIG surfaces for each sub-tenement.
    # Areas are computed in the metric work CRS.
    geom_areas <- as.numeric(sf::st_area(fragments))
    total_geom <- sum(geom_areas)
    original_cadastral <- as.numeric(tn$surface_m2)
    sub_cadastral <- if (total_geom > 0 && !is.na(original_cadastral)) {
      geom_areas * (original_cadastral / total_geom)
    } else {
      geom_areas
    }

    # Transform fragments back to the original CRS before appending
    if (use_projection) {
      fragments <- sf::st_transform(fragments, orig_crs)
    }

    new_df <- data.frame(
      tenement_id = new_ids,
      parent_parcelle_id = rep(parent_id, n),
      ug_id = rep(ug_id, n),
      surface_m2 = sub_cadastral,
      surface_sig_m2 = geom_areas,
      stringsAsFactors = FALSE
    )
    new_df[[existing_geom_col]] <- fragments
    new_sf <- sf::st_sf(new_df, sf_column_name = existing_geom_col)
    sf::st_crs(new_sf) <- sf::st_crs(tenements)

    all_new <- c(all_new, list(new_sf))
    removed_ids <- c(removed_ids, tn$tenement_id)
  }

  if (length(all_new) == 0) {
    cli::cli_abort("The drawn line did not split any tenement (only edge touches).")
  }

  tenements <- tenements[!tenements$tenement_id %in% removed_ids, , drop = FALSE]
  combined <- do.call(rbind, all_new)
  tenements <- rbind(tenements, combined)
  projet$tenements <- tenements

  projet_validate(projet)

  cli::cli_alert_success(
    "Split {length(removed_ids)} tenement(s) into {nrow(combined)} sub-tenements via line"
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

  # lwgeom::st_split is required for exact (gap-free) splits.
  if (!requireNamespace("lwgeom", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package 'lwgeom' is required to split tenements along a line.",
      i = "Install it with: install.packages('lwgeom')"
    ))
  }

  split_result <- lwgeom::st_split(tenement_geom, cutting_line)
  fragments <- sf::st_collection_extract(split_result, "POLYGON")

  if (length(fragments) < 2) {
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
    surface_sig_m2 = as.numeric(sf::st_area(parcel_sf)),
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
validate_tiling <- function(projet, parcelle_id, tolerance_m2 = NULL) {
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

  # Adaptive tolerance: 0.05 % of parcel area, min 1 m² (generous enough for
  # typical WGS84 precision on GeoJSON/Shapefile round-trips).
  if (is.null(tolerance_m2)) {
    tolerance_m2 <- max(1, 0.0005 * parcel_area)
  }

  if (area_diff > tolerance_m2) {
    cli::cli_abort(
      "Tiling invariant violated for parcel {parcelle_id}: area difference = {round(area_diff, 4)} m\u00b2 (tolerance: {round(tolerance_m2, 2)} m\u00b2)"
    )
  }

  invisible(TRUE)
}


#' Replace the tenement layout with an edited sf object
#'
#' @description
#' Used when the user exports the tenements to a GPKG/GeoJSON, edits
#' it in an external GIS (QGIS: split feature, reshape, merge…), and
#' reimports the file. The imported file is treated as the **new
#' tenement layout**, not as a cutting geometry.
#'
#' Pipeline:
#' \enumerate{
#'   \item Multipart geometries are cast to singleparts. This handles
#'     the common QGIS workflow where the user uses "Séparer les
#'     parties" which only splits parts inside a single feature row.
#'   \item Each new feature is assigned to its parent cadastral parcel
#'     via largest-overlap spatial join with \code{projet$parcels}.
#'   \item Each new feature inherits the UGF of the existing tenement
#'     it most overlaps (preserves the user's UGF grouping whenever
#'     possible).
#'   \item \code{surface_sig_m2} is recomputed in Lambert 93;
#'     \code{surface_m2} is rescaled so that the total per-parent-
#'     parcel remains equal to the cadastral contenance
#'     (invariant 5 of the domain).
#'   \item A fresh \code{tenement_id} is generated for every feature.
#' }
#'
#' @param projet List. Project with $parcels, $tenements, $ugs.
#' @param imported_sf sf. User-edited layout from the external GIS.
#'
#' @return Updated projet with a new $tenements sf.
#' @noRd
tenement_import_replace <- function(projet, imported_sf) {
  if (!inherits(imported_sf, "sf") || nrow(imported_sf) == 0) {
    cli::cli_abort("Imported file is empty or not an sf object.")
  }

  parcels    <- projet$parcels
  tenements  <- projet$tenements
  ugs        <- projet$ugs
  if (is.null(parcels) || is.null(tenements) || is.null(ugs)) {
    cli::cli_abort("Project must have parcels / tenements / ugs to import a layout.")
  }

  prev_s2 <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(prev_s2), add = TRUE)

  # Match CRS to the tenements CRS
  if (is.na(sf::st_crs(imported_sf))) {
    sf::st_crs(imported_sf) <- 4326
  }
  if (!is.na(sf::st_crs(tenements)) &&
      sf::st_crs(imported_sf) != sf::st_crs(tenements)) {
    imported_sf <- sf::st_transform(imported_sf, sf::st_crs(tenements))
  }

  # Normalise to singleparts: QGIS "Séparer les parties" splits parts
  # inside a single row — casting to single polygons turns those into
  # proper separate features.
  imported_sf <- tryCatch(sf::st_make_valid(imported_sf),
                          error = function(e) imported_sf)
  imported_sf <- suppressWarnings(sf::st_cast(imported_sf, "MULTIPOLYGON"))
  singleparts <- suppressWarnings(sf::st_cast(imported_sf, "POLYGON"))
  if (nrow(singleparts) >= nrow(imported_sf)) {
    imported_sf <- singleparts
  }
  imported_sf <- suppressWarnings(
    sf::st_cast(sf::st_make_valid(imported_sf), "MULTIPOLYGON")
  )

  n_new <- nrow(imported_sf)
  cli::cli_alert_info(
    "Import: {n_new} feature{?s} apr\u00e8s \u00e9clatement en singleparts"
  )

  # Work in metric CRS for accurate overlap + area calculations
  orig_crs <- sf::st_crs(tenements)
  use_proj <- !is.na(orig_crs) && isTRUE(sf::st_is_longlat(tenements))
  if (use_proj) {
    parcels_m   <- sf::st_transform(parcels,   2154)
    tenements_m <- sf::st_transform(tenements, 2154)
    imported_m  <- sf::st_transform(imported_sf, 2154)
  } else {
    parcels_m   <- parcels
    tenements_m <- tenements
    imported_m  <- imported_sf
  }

  # Area in m² (metric CRS)
  geom_areas <- as.numeric(sf::st_area(imported_m))

  # Parent parcel: pick the parcel whose intersection area with the
  # imported feature is the largest.
  parcel_id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"),
                             names(parcels_m))[1]
  if (is.na(parcel_id_col) || is.null(parcel_id_col)) {
    cli::cli_abort("Parcels layer has no id / nemeton_id / geo_parcelle column.")
  }

  parent_ids <- vapply(seq_len(nrow(imported_m)), function(i) {
    f <- sf::st_geometry(imported_m)[i]
    inter <- tryCatch(
      sf::st_intersection(sf::st_geometry(parcels_m), f),
      error = function(e) NULL
    )
    if (is.null(inter) || length(inter) == 0) return(NA_character_)
    a <- as.numeric(sf::st_area(inter))
    hit_rows <- which(lengths(sf::st_intersects(
      sf::st_geometry(parcels_m), f)) > 0)
    if (length(hit_rows) == 0) return(NA_character_)
    # Use the parcel with the largest intersection area
    areas <- vapply(hit_rows, function(k) {
      ii <- tryCatch(
        sf::st_intersection(sf::st_geometry(parcels_m)[k], f),
        error = function(e) NULL
      )
      if (is.null(ii) || length(ii) == 0) 0 else as.numeric(sum(sf::st_area(ii)))
    }, numeric(1))
    as.character(parcels_m[[parcel_id_col]][hit_rows[which.max(areas)]])
  }, character(1))

  if (any(is.na(parent_ids))) {
    cli::cli_warn(
      "{sum(is.na(parent_ids))} feature{?s} sans parcelle cadastrale parent \u2014 mises sur la premi\u00e8re parcelle du projet."
    )
    parent_ids[is.na(parent_ids)] <-
      as.character(parcels_m[[parcel_id_col]][1])
  }

  # Inherit UGF from the existing tenement with the largest overlap;
  # if a new feature has zero overlap with any existing tenement
  # (shouldn't happen if tiling is preserved), fall back to the first
  # UGF of the parent parcel.
  ug_ids <- vapply(seq_len(nrow(imported_m)), function(i) {
    f <- sf::st_geometry(imported_m)[i]
    hit_rows <- which(lengths(sf::st_intersects(
      sf::st_geometry(tenements_m), f)) > 0)
    if (length(hit_rows) == 0) {
      # Fallback: any existing tenement of the parent parcel
      cand <- which(tenements_m$parent_parcelle_id == parent_ids[i])
      if (length(cand) == 0) return(NA_character_)
      return(as.character(tenements_m$ug_id[cand[1]]))
    }
    areas <- vapply(hit_rows, function(k) {
      ii <- tryCatch(
        sf::st_intersection(sf::st_geometry(tenements_m)[k], f),
        error = function(e) NULL
      )
      if (is.null(ii) || length(ii) == 0) 0 else as.numeric(sum(sf::st_area(ii)))
    }, numeric(1))
    as.character(tenements_m$ug_id[hit_rows[which.max(areas)]])
  }, character(1))

  # Rescale surface_m2 per parent parcel so the total matches the
  # cadastral contenance (surface_m2 of the original parcel).
  parent_cad_total <- vapply(split(seq_along(parent_ids), parent_ids),
                             function(ix) {
    rows <- which(tenements$parent_parcelle_id == parent_ids[ix[1]])
    sum(tenements$surface_m2[rows], na.rm = TRUE)
  }, numeric(1))
  parent_geom_total <- vapply(split(seq_along(parent_ids), parent_ids),
                              function(ix) sum(geom_areas[ix]), numeric(1))
  scale <- stats::setNames(
    ifelse(parent_geom_total > 0,
           parent_cad_total / parent_geom_total, 1),
    names(parent_cad_total)
  )
  surface_m2_new <- geom_areas * as.numeric(scale[parent_ids])

  # Build new tenements sf
  ts <- format(Sys.time(), "%Y%m%d%H%M%S")
  new_ids <- paste0("tnm_", ts, "_", seq_len(n_new))

  # Geometry in original CRS for storage
  geoms_out <- if (use_proj) {
    sf::st_transform(sf::st_geometry(imported_m), orig_crs)
  } else {
    sf::st_geometry(imported_m)
  }

  existing_geom_col <- attr(tenements, "sf_column") %||% "geometry"
  df <- data.frame(
    tenement_id        = new_ids,
    parent_parcelle_id = parent_ids,
    ug_id              = ug_ids,
    surface_m2         = surface_m2_new,
    surface_sig_m2     = geom_areas,
    stringsAsFactors   = FALSE
  )
  df[[existing_geom_col]] <- geoms_out
  new_tenements <- sf::st_sf(df, sf_column_name = existing_geom_col)
  sf::st_crs(new_tenements) <- sf::st_crs(tenements)

  projet$tenements <- new_tenements

  # Remove UGs that became empty after the new layout
  active <- unique(new_tenements$ug_id)
  projet$ugs <- projet$ugs[projet$ugs$ug_id %in% active, , drop = FALSE]

  # Validate the 5 invariants
  projet_validate(projet)

  cli::cli_alert_success(
    "Import appliqu\u00e9 : {n_new} t\u00e8nement{?s} (au lieu de {nrow(tenements)})"
  )
  projet
}

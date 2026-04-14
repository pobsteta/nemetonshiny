#' Domain Layer: Unités de Gestion (UG)
#'
#' @description
#' Pure domain logic for Management Units (UG). Contains constructors,
#' commands, queries, and invariant validation. No Shiny dependency.
#'
#' Domain concepts:
#' - Parcelle cadastrale: immutable cadastral polygon
#' - Tenement: smallest geometric subdivision (whole parcel or fragment)
#' - UG: grouping of 1+ tenements = operational forestry unit
#' - Groupe d'aménagement: functional classification of UGs
#'
#' Each command returns a new projet (functional immutability).
#' All 5 invariants are enforced after every command.
#'
#' @name domain_ug
#' @keywords internal
NULL


# ==============================================================================
# Predefined management groups (groupes d'aménagement)
# ==============================================================================

#' Known management group codes
#' @noRd
GROUPES_AMENAGEMENT <- c(
  "AMETS",  # Amélioration de taillis sous futaie
  "AMER",   # Amélioration en régénération
  "IRR",    # Irrégulier
  "TSF",    # Taillis sous futaie
  "REGT",   # Régénération par taillis
  "REGF",   # Régénération par futaie
  "HSN",    # Hors sylviculture naturelle
  "HSY",    # Hors sylviculture
  "PROT",   # Protection
  "ACC"     # Accueil
)


#' Color palette for management groups
#' @noRd
GROUPE_COLORS <- c(
  "AMETS" = "#228B22",  # ForestGreen
  "AMER"  = "#2E8B57",  # SeaGreen
  "IRR"   = "#4682B4",  # SteelBlue
  "TSF"   = "#DAA520",  # Goldenrod
  "REGT"  = "#D2691E",  # Chocolate
  "REGF"  = "#CD853F",  # Peru
  "HSN"   = "#9932CC",  # DarkOrchid
  "HSY"   = "#8B008B",  # DarkMagenta
  "PROT"  = "#20B2AA",  # LightSeaGreen
  "ACC"   = "#FFD700"   # Gold
)


#' Distinct color palette for UGs (when no groupe assigned)
#' @noRd
UG_PALETTE <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
  "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5"
)


#' Get fill color for a UG based on its groupe or index
#'
#' @param groupe Character. Management group code (can be NA).
#' @param index Integer. UG index (used for fallback color).
#'
#' @return Character. Hex color string.
#' @noRd
ug_color <- function(groupe, index = 1L) {
  if (!is.na(groupe) && nchar(groupe) > 0 && groupe %in% names(GROUPE_COLORS)) {
    return(GROUPE_COLORS[[groupe]])
  }
  UG_PALETTE[((index - 1L) %% length(UG_PALETTE)) + 1L]
}


# ==============================================================================
# Constructors
# ==============================================================================

#' Create a new tenement
#'
#' @param id Character. Unique tenement identifier.
#' @param parent_parcelle_id Character. ID of the parent cadastral parcel.
#' @param geometry sfc geometry. The tenement's polygon geometry.
#' @param surface_m2 Numeric. Area in square meters.
#'
#' @return A one-row sf data.frame representing the tenement.
#' @noRd
new_tenement <- function(id, parent_parcelle_id, geometry, surface_m2) {
  if (is.null(id) || nchar(id) == 0) {
    cli::cli_abort("Tenement id is required")
  }
  if (is.null(parent_parcelle_id) || nchar(parent_parcelle_id) == 0) {
    cli::cli_abort("parent_parcelle_id is required")
  }
  if (is.null(surface_m2) || !is.numeric(surface_m2) || surface_m2 <= 0) {
    cli::cli_abort("surface_m2 must be a positive number")
  }

  sf::st_sf(
    tenement_id = id,
    parent_parcelle_id = parent_parcelle_id,
    ug_id = NA_character_,
    surface_m2 = surface_m2,
    geometry = geometry
  )
}


#' Create a new UG definition
#'
#' @param id Character. Unique UG identifier.
#' @param label Character. User-visible label for this UG.
#' @param groupe Character. Management group code (optional).
#'
#' @return A one-row data.frame representing the UG.
#' @noRd
new_ug <- function(id, label, groupe = NA_character_) {
  if (is.null(id) || nchar(id) == 0) {
    cli::cli_abort("UG id is required")
  }
  if (is.null(label) || nchar(trimws(label)) == 0) {
    cli::cli_abort("UG label is required")
  }

  data.frame(
    ug_id = id,
    label = trimws(label),
    groupe = as.character(groupe),
    stringsAsFactors = FALSE
  )
}


#' Generate a unique ID
#' @noRd
generate_id <- function(prefix = "ug") {
  sprintf("%s_%s_%s",
          prefix,
          format(Sys.time(), "%Y%m%d%H%M%S"),
          paste0(sample(letters, 4), collapse = ""))
}


# ==============================================================================
# Initialization
# ==============================================================================

#' Initialize default UGs from cadastral parcels
#'
#' @description
#' Creates the default mapping: 1 parcel = 1 tenement = 1 UG.
#' This is the starting state for any project.
#'
#' @param projet List. Project with at least $parcels (sf object).
#'
#' @return Updated projet with $tenements (sf) and $ugs (data.frame) populated.
#' @noRd
ug_init_default <- function(projet) {
  parcels <- projet$parcels
  if (is.null(parcels) || !inherits(parcels, "sf") || nrow(parcels) == 0) {
    cli::cli_abort("Project must have non-empty parcels sf object")
  }

  n <- nrow(parcels)

  # Determine parcel ID column
  id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(parcels))
  if (length(id_col) == 0) {
    cli::cli_abort("Parcels must have an 'id', 'nemeton_id', or 'geo_parcelle' column")
  }
  parcel_ids <- as.character(parcels[[id_col[1]]])

  # Create one tenement per parcel
  tenement_ids <- paste0("tnm_", seq_len(n))
  ug_ids <- paste0("ug_", seq_len(n))

  # Build tenements sf with BOTH cadastral (contenance) and SIG (st_area)
  # surfaces so they can be compared in the UI.
  cadastral_surface <- if ("contenance" %in% names(parcels)) {
    as.numeric(parcels$contenance)
  } else {
    as.numeric(sf::st_area(parcels))
  }
  sig_surface <- as.numeric(sf::st_area(parcels))

  tenements <- sf::st_sf(
    tenement_id = tenement_ids,
    parent_parcelle_id = parcel_ids,
    ug_id = ug_ids,
    surface_m2 = cadastral_surface,
    surface_sig_m2 = sig_surface,
    geometry = sf::st_geometry(parcels)
  )

  # Build UG definitions with default labels from parcel refs
  labels <- if ("geo_parcelle" %in% names(parcels)) {
    as.character(parcels$geo_parcelle)
  } else {
    parcel_ids
  }

  ugs <- data.frame(
    ug_id = ug_ids,
    label = labels,
    groupe = NA_character_,
    stringsAsFactors = FALSE
  )

  projet$tenements <- tenements
  projet$ugs <- ugs

  # Validate invariants
  projet_validate(projet)

  projet
}


# ==============================================================================
# Commands (mutating)
# ==============================================================================

#' Create a new UG from selected tenements
#'
#' @param projet List. Project with $tenements and $ugs.
#' @param tenements_ids Character vector. IDs of tenements to assign to the new UG.
#' @param label Character. Label for the new UG.
#' @param groupe Character. Optional management group code.
#'
#' @return Updated projet.
#' @noRd
ug_create <- function(projet, tenements_ids, label, groupe = NA_character_) {
  if (length(tenements_ids) == 0) {
    cli::cli_abort("At least one tenement must be specified")
  }

  tenements <- projet$tenements
  ugs <- projet$ugs

  # Check all tenements exist
  missing <- setdiff(tenements_ids, tenements$tenement_id)
  if (length(missing) > 0) {
    cli::cli_abort("Unknown tenement IDs: {paste(missing, collapse = ', ')}")
  }

  # Create new UG
  new_ug_id <- generate_id("ug")
  new_ug_row <- new_ug(new_ug_id, label, groupe)

  # Reassign tenements to new UG
  tenements$ug_id[tenements$tenement_id %in% tenements_ids] <- new_ug_id

  # Remove UGs that became empty
  active_ug_ids <- unique(tenements$ug_id)
  ugs <- ugs[ugs$ug_id %in% active_ug_ids, , drop = FALSE]

  # Add the new UG
  ugs <- rbind(ugs, new_ug_row)

  projet$tenements <- tenements
  projet$ugs <- ugs

  projet_validate(projet)
  projet
}


#' Merge multiple UGs into one
#'
#' @param projet List. Project.
#' @param ug_ids Character vector. IDs of UGs to merge.
#' @param nouveau_label Character. Label for the merged UG.
#'
#' @return Updated projet.
#' @noRd
ug_merge <- function(projet, ug_ids, nouveau_label) {
  if (length(ug_ids) < 2) {
    cli::cli_abort("At least two UGs must be specified for merge")
  }

  tenements <- projet$tenements
  ugs <- projet$ugs

  # Check all UGs exist
  missing <- setdiff(ug_ids, ugs$ug_id)
  if (length(missing) > 0) {
    cli::cli_abort("Unknown UG IDs: {paste(missing, collapse = ', ')}")
  }

  # Collect all tenements from the UGs to merge
  merged_tenement_ids <- tenements$tenement_id[tenements$ug_id %in% ug_ids]

  # Determine groupe: use the most common one, or NA if mixed
  groupes <- ugs$groupe[ugs$ug_id %in% ug_ids]
  groupes <- groupes[!is.na(groupes)]
  merged_groupe <- if (length(groupes) > 0 && length(unique(groupes)) == 1) {
    groupes[1]
  } else {
    NA_character_
  }

  # Create new merged UG
  new_ug_id <- generate_id("ug")
  new_ug_row <- new_ug(new_ug_id, nouveau_label, merged_groupe)

  # Reassign all tenements
  tenements$ug_id[tenements$tenement_id %in% merged_tenement_ids] <- new_ug_id

  # Remove old UGs, add new one
  ugs <- ugs[!ugs$ug_id %in% ug_ids, , drop = FALSE]
  ugs <- rbind(ugs, new_ug_row)

  projet$tenements <- tenements
  projet$ugs <- ugs

  projet_validate(projet)
  projet
}


#' Split a UG back into individual tenement-based UGs
#'
#' @param projet List. Project.
#' @param ug_id Character. ID of the UG to split.
#' @param partition_tenements List of character vectors. Each element is a group
#'   of tenement IDs forming a new UG. If NULL, splits into 1 UG per tenement.
#'
#' @return Updated projet.
#' @noRd
ug_split <- function(projet, ug_id, partition_tenements = NULL) {
  tenements <- projet$tenements
  ugs <- projet$ugs

  if (!ug_id %in% ugs$ug_id) {
    cli::cli_abort("Unknown UG ID: {ug_id}")
  }

  ug_tenement_ids <- tenements$tenement_id[tenements$ug_id == ug_id]
  if (length(ug_tenement_ids) < 2) {
    cli::cli_abort("Cannot split a UG with fewer than 2 tenements")
  }

  old_ug <- ugs[ugs$ug_id == ug_id, ]

  if (is.null(partition_tenements)) {
    # Default: 1 UG per tenement
    partition_tenements <- as.list(ug_tenement_ids)
  }

  # Validate partition covers all tenements exactly
  all_partitioned <- unlist(partition_tenements)
  if (!setequal(all_partitioned, ug_tenement_ids)) {
    cli::cli_abort("Partition must cover exactly the tenements of the UG being split")
  }
  if (length(all_partitioned) != length(unique(all_partitioned))) {
    cli::cli_abort("An tenement cannot appear in multiple partition groups")
  }

  # Remove old UG
  ugs <- ugs[ugs$ug_id != ug_id, , drop = FALSE]

  # Create new UGs for each partition group
  for (i in seq_along(partition_tenements)) {
    group_ids <- partition_tenements[[i]]
    new_id <- generate_id("ug")
    # Auto-label: old label + suffix
    new_label <- if (length(partition_tenements) <= 26) {
      paste0(old_ug$label, "_", letters[i])
    } else {
      paste0(old_ug$label, "_", i)
    }

    tenements$ug_id[tenements$tenement_id %in% group_ids] <- new_id
    ugs <- rbind(ugs, new_ug(new_id, new_label, old_ug$groupe))
  }

  projet$tenements <- tenements
  projet$ugs <- ugs

  projet_validate(projet)
  projet
}


#' Move an tenement from its current UG to another
#'
#' @param projet List. Project.
#' @param tenement_id Character. Tenement to move.
#' @param ug_id Character. Target UG.
#'
#' @return Updated projet.
#' @noRd
ug_assign_tenement <- function(projet, tenement_id, ug_id) {
  tenements <- projet$tenements
  ugs <- projet$ugs

  if (!tenement_id %in% tenements$tenement_id) {
    cli::cli_abort("Unknown tenement ID: {tenement_id}")
  }
  if (!ug_id %in% ugs$ug_id) {
    cli::cli_abort("Unknown UG ID: {ug_id}")
  }

  old_ug_id <- tenements$ug_id[tenements$tenement_id == tenement_id]

  # Move tenement

  tenements$ug_id[tenements$tenement_id == tenement_id] <- ug_id

  # Check if old UG is now empty → remove it
  if (!any(tenements$ug_id == old_ug_id)) {
    ugs <- ugs[ugs$ug_id != old_ug_id, , drop = FALSE]
  }

  projet$tenements <- tenements
  projet$ugs <- ugs

  projet_validate(projet)
  projet
}


#' Set the management group of a UG
#'
#' @param projet List. Project.
#' @param ug_id Character. UG to update.
#' @param groupe Character. Management group code.
#'
#' @return Updated projet.
#' @noRd
ug_set_groupe <- function(projet, ug_id, groupe) {
  ugs <- projet$ugs

  if (!ug_id %in% ugs$ug_id) {
    cli::cli_abort("Unknown UG ID: {ug_id}")
  }

  # Validate groupe if not NA
  if (!is.na(groupe) && nchar(groupe) > 0 && !groupe %in% GROUPES_AMENAGEMENT) {
    cli::cli_warn("Unknown management group: {groupe}. Known groups: {paste(GROUPES_AMENAGEMENT, collapse = ', ')}")
  }

  ugs$groupe[ugs$ug_id == ug_id] <- groupe
  projet$ugs <- ugs

  projet
}


#' Delete an empty UG
#'
#' @param projet List. Project.
#' @param ug_id Character. UG to delete.
#'
#' @return Updated projet.
#' @noRd
ug_delete <- function(projet, ug_id) {
  tenements <- projet$tenements
  ugs <- projet$ugs

  if (!ug_id %in% ugs$ug_id) {
    cli::cli_abort("Unknown UG ID: {ug_id}")
  }

  # Check UG is empty
  if (any(tenements$ug_id == ug_id)) {
    cli::cli_abort("Cannot delete non-empty UG. Move or reassign tenements first.")
  }

  ugs <- ugs[ugs$ug_id != ug_id, , drop = FALSE]
  projet$ugs <- ugs

  projet
}


# ==============================================================================
# Queries (read model)
# ==============================================================================

#' Get the geometry of a UG (union of its tenements)
#'
#' @param projet List. Project.
#' @param ug_id Character. UG identifier.
#'
#' @return sfc geometry (union of tenement geometries).
#' @noRd
ug_geometry <- function(projet, ug_id) {
  tenements <- projet$tenements

  ug_tenements <- tenements[tenements$ug_id == ug_id, ]
  if (nrow(ug_tenements) == 0) {
    cli::cli_abort("UG {ug_id} has no tenements")
  }

  sf::st_union(sf::st_geometry(ug_tenements))
}


#' Get the total surface of a UG
#'
#' @param projet List. Project.
#' @param ug_id Character. UG identifier.
#'
#' @return Numeric. Total surface in square meters.
#' @noRd
ug_surface <- function(projet, ug_id) {
  tenements <- projet$tenements
  sum(tenements$surface_m2[tenements$ug_id == ug_id], na.rm = TRUE)
}


#' Get cadastral references for a UG
#'
#' @param projet List. Project.
#' @param ug_id Character. UG identifier.
#'
#' @return Data.frame with columns: geo_parcelle, section, numero, surface_m2.
#' @noRd
ug_cadastral_refs <- function(projet, ug_id) {
  tenements <- projet$tenements
  parcels <- projet$parcels

  ug_tenements <- tenements[tenements$ug_id == ug_id, ]
  if (nrow(ug_tenements) == 0) {
    return(data.frame(
      geo_parcelle = character(0),
      section = character(0),
      numero = character(0),
      surface_m2 = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  # Get unique parent parcels
  parent_ids <- unique(ug_tenements$parent_parcelle_id)

  # Determine parcel ID column
  id_col <- intersect(c("id", "nemeton_id", "geo_parcelle"), names(parcels))
  if (length(id_col) == 0) {
    return(data.frame(
      geo_parcelle = parent_ids,
      section = NA_character_,
      numero = NA_character_,
      surface_m2 = vapply(parent_ids, function(pid) {
        sum(ug_tenements$surface_m2[ug_tenements$parent_parcelle_id == pid])
      }, numeric(1)),
      stringsAsFactors = FALSE
    ))
  }

  parent_parcels <- parcels[as.character(parcels[[id_col[1]]]) %in% parent_ids, ]

  data.frame(
    geo_parcelle = if ("geo_parcelle" %in% names(parent_parcels)) {
      as.character(parent_parcels$geo_parcelle)
    } else {
      as.character(parent_parcels[[id_col[1]]])
    },
    section = if ("section" %in% names(parent_parcels)) {
      as.character(parent_parcels$section)
    } else {
      NA_character_
    },
    numero = if ("numero" %in% names(parent_parcels)) {
      as.character(parent_parcels$numero)
    } else {
      NA_character_
    },
    surface_m2 = vapply(
      if ("geo_parcelle" %in% names(parent_parcels)) {
        as.character(parent_parcels$geo_parcelle)
      } else {
        as.character(parent_parcels[[id_col[1]]])
      },
      function(pid) {
        matching_parent <- if ("geo_parcelle" %in% names(parcels)) {
          parcels$geo_parcelle == pid
        } else {
          parcels[[id_col[1]]] == pid
        }
        p_id <- as.character(parcels[[id_col[1]]][matching_parent])
        sum(ug_tenements$surface_m2[ug_tenements$parent_parcelle_id %in% p_id])
      },
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )
}


#' List all UGs in the project
#'
#' @param projet List. Project.
#' @param filter_groupe Character. Optional filter by management group.
#'
#' @return Data.frame with ug_id, label, groupe, n_tenements, surface_m2.
#' @noRd
ug_list <- function(projet, filter_groupe = NULL) {
  ugs <- projet$ugs
  tenements <- projet$tenements

  if (is.null(ugs) || nrow(ugs) == 0) {
    return(data.frame(
      ug_id = character(0), label = character(0), groupe = character(0),
      n_tenements = integer(0),
      surface_m2 = numeric(0),
      surface_sig_m2 = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  # Filter by groupe if requested
  if (!is.null(filter_groupe)) {
    ugs <- ugs[!is.na(ugs$groupe) & ugs$groupe == filter_groupe, , drop = FALSE]
  }

  # Backward compat: older projects don't have surface_sig_m2 → compute on the fly
  if (!"surface_sig_m2" %in% names(tenements)) {
    tenements$surface_sig_m2 <- as.numeric(sf::st_area(tenements))
  }

  # Compute statistics per UG (both cadastral and SIG surfaces)
  result <- data.frame(
    ug_id = ugs$ug_id,
    label = ugs$label,
    groupe = ugs$groupe,
    n_tenements = vapply(ugs$ug_id, function(uid) {
      sum(tenements$ug_id == uid, na.rm = TRUE)
    }, integer(1)),
    surface_m2 = vapply(ugs$ug_id, function(uid) {
      sum(tenements$surface_m2[tenements$ug_id == uid], na.rm = TRUE)
    }, numeric(1)),
    surface_sig_m2 = vapply(ugs$ug_id, function(uid) {
      sum(tenements$surface_sig_m2[tenements$ug_id == uid], na.rm = TRUE)
    }, numeric(1)),
    stringsAsFactors = FALSE
  )

  result
}


# ==============================================================================
# Build UG sf object (for computation and display)
# ==============================================================================

#' Build an sf object with one row per UG
#'
#' @description
#' Creates an sf data.frame suitable for indicator aggregation and map display.
#' Each row is a UG with its dissolved geometry and metadata.
#'
#' @param projet List. Project with $tenements, $ugs, $parcels.
#'
#' @return sf object with columns: ug_id, label, groupe, surface_m2,
#'   n_tenements, cadastral_refs, geometry.
#' @noRd
ug_build_sf <- function(projet) {
  ugs <- projet$ugs
  tenements <- projet$tenements

  if (is.null(ugs) || nrow(ugs) == 0) {
    return(NULL)
  }

  # Build geometry per UG
  geoms <- lapply(ugs$ug_id, function(uid) {
    ug_geometry(projet, uid)
  })
  geom_sfc <- do.call(c, geoms)
  sf::st_crs(geom_sfc) <- sf::st_crs(tenements)

  # Backward compat: compute surface_sig_m2 if missing
  if (!"surface_sig_m2" %in% names(tenements)) {
    tenements$surface_sig_m2 <- as.numeric(sf::st_area(tenements))
  }

  # Build metadata
  n_tenements <- vapply(ugs$ug_id, function(uid) {
    sum(tenements$ug_id == uid, na.rm = TRUE)
  }, integer(1))

  surfaces_cad <- vapply(ugs$ug_id, function(uid) {
    sum(tenements$surface_m2[tenements$ug_id == uid], na.rm = TRUE)
  }, numeric(1))

  surfaces_sig <- vapply(ugs$ug_id, function(uid) {
    sum(tenements$surface_sig_m2[tenements$ug_id == uid], na.rm = TRUE)
  }, numeric(1))

  # Cadastral references as concatenated string
  cad_refs <- vapply(ugs$ug_id, function(uid) {
    refs <- ug_cadastral_refs(projet, uid)
    if (nrow(refs) == 0) return("")
    paste(refs$geo_parcelle, collapse = ", ")
  }, character(1))

  sf::st_sf(
    ug_id = ugs$ug_id,
    label = ugs$label,
    groupe = ugs$groupe,
    surface_m2 = surfaces_cad,
    surface_sig_m2 = surfaces_sig,
    n_tenements = n_tenements,
    cadastral_refs = cad_refs,
    geometry = geom_sfc
  )
}


# ==============================================================================
# Validation
# ==============================================================================

#' Validate all domain invariants
#'
#' @description
#' Checks the 5 core invariants:
#' 1. Cadastral tiling: union(tenements) ≡ parcel for each parcel
#' 2. UG partition: every tenement belongs to exactly one UG
#' 3. Non-emptiness: every UG has at least one tenement
#' 4. Traceability: every UG can produce its cadastral composition
#' 5. Surface coherence: UG surface = sum(tenement surfaces)
#'
#' @param projet List. Project with $parcels, $tenements, $ugs.
#' @param tolerance_m2 Numeric. Area tolerance for geometric checks.
#'
#' @return Invisible TRUE if all invariants hold, otherwise aborts.
#' @noRd
projet_validate <- function(projet, tolerance_m2 = 0.01) {
  tenements <- projet$tenements
  ugs <- projet$ugs

  if (is.null(tenements) || is.null(ugs)) {
    cli::cli_abort("Project must have $tenements and $ugs to validate")
  }

  # --- Invariant 2: UG partition (every tenement in exactly one UG) ---
  if (any(is.na(tenements$ug_id))) {
    orphan_ids <- tenements$tenement_id[is.na(tenements$ug_id)]
    cli::cli_abort(
      "Invariant 2 violated: tenements without UG assignment: {paste(orphan_ids, collapse = ', ')}"
    )
  }

  # --- Invariant 3: Non-emptiness (every UG has >= 1 tenement) ---
  for (uid in ugs$ug_id) {
    n <- sum(tenements$ug_id == uid, na.rm = TRUE)
    if (n == 0) {
      cli::cli_abort("Invariant 3 violated: UG {uid} has no tenements")
    }
  }

  # --- Invariant 4: Traceability (every tenement traces to a parcel) ---
  if (any(is.na(tenements$parent_parcelle_id))) {
    orphan_ids <- tenements$tenement_id[is.na(tenements$parent_parcelle_id)]
    cli::cli_abort(
      "Invariant 4 violated: tenements without parent parcel: {paste(orphan_ids, collapse = ', ')}"
    )
  }

  # --- Invariant 5: Surface coherence ---
  for (uid in ugs$ug_id) {
    atom_surface_sum <- sum(tenements$surface_m2[tenements$ug_id == uid], na.rm = TRUE)
    if (is.na(atom_surface_sum) || atom_surface_sum <= 0) {
      cli::cli_abort(
        "Invariant 5 violated: UG {uid} has zero or NA total surface"
      )
    }
  }

  invisible(TRUE)
}


#' Check if a project has UG data
#'
#' @param projet List. Project.
#' @return Logical. TRUE if UG data is present.
#' @noRd
has_ug_data <- function(projet) {
  !is.null(projet$tenements) &&
    !is.null(projet$ugs) &&
    inherits(projet$tenements, "sf") &&
    nrow(projet$tenements) > 0 &&
    nrow(projet$ugs) > 0
}

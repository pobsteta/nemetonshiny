#' Project Service for nemetonApp
#'
#' @description
#' Service for managing nemeton projects - creation, saving, loading.
#' Projects are stored in GeoParquet format for efficient spatial data handling.
#'
#' @name service_project
#' @keywords internal
NULL


#' Get projects root directory
#'
#' @description
#' Returns the root directory for nemeton projects.
#' Uses the project_dir from app options if available.
#'
#' @return Character. Path to projects root directory.
#'
#' @noRd
get_projects_root <- function() {
  opts <- get_app_options()

  if (!is.null(opts$project_dir)) {
    root <- opts$project_dir
  } else {
    root <- file.path(
      Sys.getenv("HOME", rappdirs::user_data_dir()),
      "nemeton_projects"
    )
  }

  # Create if doesn't exist

if (!dir.exists(root)) {
    dir.create(root, recursive = TRUE)
  }

  normalizePath(root, mustWork = FALSE)
}


#' Project format of version 1.0.0
#'
#' Written in `metadata$format_projet` by [create_project()]. Version 1.0.0
#' starts from scratch: a project without this marker predates it and is not
#' taken over (no migration, decision of 2026-10-06). Bump it, with a
#' migration, only for a format change AFTER 1.0.0.
#' @noRd
PROJET_FORMAT <- 1L

#' Is a project in the current format?
#' @param metadata Parsed `metadata.json`.
#' @return Logical scalar.
#' @noRd
.projet_format_ok <- function(metadata) {
  f <- suppressWarnings(as.integer(metadata$format_projet %||% NA_integer_))
  length(f) == 1L && !is.na(f) && f == PROJET_FORMAT
}


#' Create a new project
#'
#' @description
#' Creates a new project directory with metadata and initial structure.
#'
#' @param name Character. Project name (required, max 100 chars).
#' @param description Character. Project description (optional, max 500 chars).
#' @param owner Character. Project owner (optional, max 100 chars).
#' @param parcels sf object. Selected parcels (optional, can be added later).
#'
#' @return List with project info (id, path, metadata).
#'
#' @noRd
create_project <- function(name, description = "", owner = "", parcels = NULL,
                           groupes_profile = NULL, commune_geometry = NULL) {
  # Validate name
  if (missing(name) || is.null(name) || nchar(trimws(name)) == 0) {
    cli::cli_abort("Project name is required")
  }

  name <- trimws(name)
  if (nchar(name) > 100) {
    cli::cli_abort("Project name must be 100 characters or less")
  }

  # Validate description
  if (nchar(description) > 500) {
    cli::cli_abort("Description must be 500 characters or less")
  }

  # Validate owner
  if (nchar(owner) > 100) {
    cli::cli_abort("Owner must be 100 characters or less")
  }

  # Generate project ID (timestamp + random)
  project_id <- sprintf(
    "%s_%s",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    paste0(sample(letters, 4), collapse = "")
  )

  # Create project directory
  root <- get_projects_root()
  project_path <- file.path(root, project_id)
  dir.create(project_path, recursive = TRUE)

  # Create subdirectories
  dir.create(file.path(project_path, "data"), showWarnings = FALSE)
  dir.create(file.path(project_path, "cache"), showWarnings = FALSE)
  dir.create(file.path(project_path, "exports"), showWarnings = FALSE)

  # Resolve UGF groupes profile (default from YAML config)
  if (is.null(groupes_profile) || !nzchar(groupes_profile)) {
    groupes_profile <- tryCatch(get_default_groupes_profile(),
                                error = function(e) "onf")
  }

  # Create metadata
  metadata <- list(
    id = project_id,
    name = name,
    description = description,
    owner = owner,
    created_at = Sys.time(),
    updated_at = Sys.time(),
    status = "draft",
    version = "0.7.0",
    parcels_count = 0L,
    indicators_computed = FALSE,
    groupes_profile = groupes_profile,
    # Format des projets de la 1.0.0. Un projet sans ce marqueur est anterieur
    # et n'est pas repris (pas de migration, decision du 2026-10-06).
    format_projet = PROJET_FORMAT
 )

  # Save metadata
  metadata_path <- file.path(project_path, "metadata.json")
  .write_json_atomic(metadata, metadata_path, auto_unbox = TRUE, pretty = TRUE)

  # Save parcels if provided
  if (!is.null(parcels) && inherits(parcels, "sf") && nrow(parcels) > 0) {
    save_parcels(project_id, parcels)
    metadata$parcels_count <- nrow(parcels)
    .write_json_atomic(metadata, metadata_path, auto_unbox = TRUE, pretty = TRUE)
  }

  # Cache the commune boundary so it restores instantly on load (best-effort)
  if (!is.null(commune_geometry)) {
    save_commune_geometry(project_id, commune_geometry)
  }

  cli::cli_alert_success("Project created: {.val {name}}")
  .invalidate_recent_projects_cache()

  list(
    id = project_id,
    path = project_path,
    metadata = metadata
  )
}


#' Update an existing project
#'
#' @description
#' Updates project metadata (name, description, owner) and optionally parcels.
#'
#' @param project_id Character. Project ID.
#' @param name Character. New project name.
#' @param description Character. New description.
#' @param owner Character. New owner.
#' @param parcels sf object. New parcels (optional).
#'
#' @return List with project info (id, path, metadata, parcels).
#'
#' @noRd
update_project <- function(project_id, name, description = "", owner = "",
                           parcels = NULL, groupes_profile = NULL,
                           commune_geometry = NULL) {
  # Check project exists
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  # Validate name
  if (missing(name) || is.null(name) || nchar(trimws(name)) == 0) {
    cli::cli_abort("Project name is required")
  }

  name <- trimws(name)
  if (nchar(name) > 100) {
    cli::cli_abort("Project name must be 100 characters or less")
  }

  # Validate description
  if (nchar(description) > 500) {
    cli::cli_abort("Description must be 500 characters or less")
  }

  # Validate owner
  if (nchar(owner) > 100) {
    cli::cli_abort("Owner must be 100 characters or less")
  }

  # Load existing metadata
  metadata <- load_project_metadata(project_id)
  if (is.null(metadata)) {
    cli::cli_abort("Could not load project metadata")
  }

  # Update metadata fields
  metadata$name <- name
  metadata$description <- description
  metadata$owner <- owner
  metadata$updated_at <- Sys.time()
  if (!is.null(groupes_profile) && nzchar(groupes_profile)) {
    metadata$groupes_profile <- groupes_profile
  }

  # Update parcels if provided
  parcelles_changees <- FALSE
  if (!is.null(parcels) && inherits(parcels, "sf") && nrow(parcels) > 0) {
    # Ajouter ou retirer une parcelle rend le decoupage UGF et les indicateurs
    # incoherents (parcelle sans tenement, ou tenement d'une parcelle retiree).
    avant <- .ids_parcelles(tryCatch(load_parcels(project_id), error = function(e) NULL))
    save_parcels(project_id, parcels)
    metadata$parcels_count <- nrow(parcels)
    parcelles_changees <- !is.null(avant) && !identical(avant, .ids_parcelles(parcels))
  }

  # Refresh the cached commune boundary (best-effort, instant restore on load)
  if (!is.null(commune_geometry)) {
    save_commune_geometry(project_id, commune_geometry)
  }

  # Save updated metadata
  metadata_path <- file.path(project_path, "metadata.json")
  .write_json_atomic(metadata, metadata_path, auto_unbox = TRUE, pretty = TRUE)

  # APRES l'ecriture des metadonnees, qui sinon effacerait les marqueurs : le
  # decoupage UGF est mis de cote (recuperable a la main) et les indicateurs
  # invalides ; le rechargement repart d'un decoupage par defaut coherent avec
  # les nouvelles parcelles.
  if (parcelles_changees) {
    .mettre_de_cote_ug(project_id)
    metadata <- load_project_metadata(project_id) %||% metadata
  }

  cli::cli_alert_success("Project updated: {.val {name}}")
  .invalidate_recent_projects_cache()

  list(
    id = project_id,
    path = project_path,
    metadata = metadata,
    parcels = if (!is.null(parcels)) parcels else load_parcels(project_id),
    indicators = load_indicators(project_id),
    ugf_reinitialisees = parcelles_changees
  )
}


#' Identifiers of a parcel set, sorted
#'
#' @param parcels sf / data.frame with `id` or `geo_parcelle`, or NULL.
#' @return Sorted unique character vector, or NULL.
#' @noRd
.ids_parcelles <- function(parcels) {
  if (is.null(parcels) || !nrow(parcels)) return(NULL)
  col <- intersect(c("id", "geo_parcelle"), names(parcels))[1]
  if (is.na(col)) return(NULL)
  sort(unique(as.character(parcels[[col]])))
}


#' Save parcels to project
#'
#' @description
#' Saves selected parcels to project. Primary format is GeoPackage (.gpkg)
#' which is universally compatible with QGIS and other GIS tools.
#' Also saves as Parquet for fast loading within the app.
#'
#' @param project_id Character. Project ID.
#' @param parcels sf object. Parcels to save.
#'
#' @return Logical. TRUE if successful.
#'
#' @noRd
save_parcels <- function(project_id, parcels) {
  if (!inherits(parcels, "sf")) {
    cli::cli_abort("parcels must be an sf object")
  }

  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  # File paths
  gpkg_path <- file.path(project_path, "data", "parcels.gpkg")
  parquet_path <- file.path(project_path, "data", "parcels.parquet")

  tryCatch({
    # Ensure valid CRS before saving (default to WGS84)
    if (is.na(sf::st_crs(parcels))) {
      parcels <- sf::st_set_crs(parcels, 4326)
    }

    cli::cli_alert_info("Saving {nrow(parcels)} parcels")

    # 1. Save as GeoPackage (primary format, QGIS-compatible). Ecrit a cote
    # puis renomme : l'ancien `unlink()` prealable laissait le projet sans
    # parcelles quand l'ecriture echouait.
    .st_write_atomic(parcels, gpkg_path)
    cli::cli_alert_success("Saved GeoPackage: {basename(gpkg_path)}")

    # 2. Also save as Parquet for fast internal loading
    if (requireNamespace("geoarrow", quietly = TRUE) &&
        requireNamespace("arrow", quietly = TRUE)) {
      # geoarrow enables arrow to handle sf geometry
      tmp_pq <- .tmp_sibling(parquet_path)
      arrow::write_parquet(parcels, tmp_pq)
      .replace_file(tmp_pq, parquet_path)
      cli::cli_alert_success("Saved Parquet: {basename(parquet_path)}")
    }

    # Update metadata
    update_project_metadata(project_id, list(
      parcels_count = nrow(parcels),
      updated_at = Sys.time()
    ))

    cli::cli_alert_success("Saved {nrow(parcels)} parcels")
    TRUE

  }, error = function(e) {
    cli::cli_abort("Failed to save parcels: {e$message}")
  })
}


#' Load parcels from project
#'
#' @description
#' Loads parcels from project. Tries Parquet first (faster), falls back to
#' GeoPackage if Parquet is unavailable or fails.
#'
#' @param project_id Character. Project ID.
#'
#' @return sf object with parcels, or NULL if not found.
#'
#' @noRd
load_parcels <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  # File paths
  parquet_path <- file.path(project_path, "data", "parcels.parquet")
  gpkg_path <- file.path(project_path, "data", "parcels.gpkg")

  parcels_sf <- NULL

 # Try Parquet first (faster loading)
  if (file.exists(parquet_path) &&
      requireNamespace("geoarrow", quietly = TRUE) &&
      requireNamespace("arrow", quietly = TRUE)) {
    parcels_sf <- tryCatch({
      # geoarrow enables arrow to read sf geometry
      parcels_arrow <- arrow::read_parquet(parquet_path, as_data_frame = FALSE)
      sf::st_as_sf(parcels_arrow)
    }, error = function(e) {
      cli::cli_warn("Failed to load Parquet, trying GeoPackage: {e$message}")
      NULL
    })
  }

  # Fall back to GeoPackage
  if (is.null(parcels_sf) && file.exists(gpkg_path)) {
    parcels_sf <- tryCatch({
      sf::st_read(gpkg_path, quiet = TRUE)
    }, error = function(e) {
      cli::cli_warn("Failed to load GeoPackage: {e$message}")
      NULL
    })
  }

  # No parcels found
  if (is.null(parcels_sf)) {
    return(NULL)
  }

  # Verify it's an sf object with geometry
  if (!inherits(parcels_sf, "sf")) {
    cli::cli_warn("Failed to load parcels as sf object")
    return(NULL)
  }

  geom <- sf::st_geometry(parcels_sf)
  if (is.null(geom) || length(geom) == 0) {
    cli::cli_warn("Parcels file has no valid geometry")
    return(NULL)
  }

  # Get CRS for logging
  crs_info <- sf::st_crs(parcels_sf)$epsg
  if (is.null(crs_info) || is.na(crs_info)) crs_info <- "unknown"

  cli::cli_alert_success("Loaded {nrow(parcels_sf)} parcels (CRS: {crs_info})")
  parcels_sf
}


#' Save the commune boundary geometry to project
#'
#' @description
#' Persists the commune contour (single MULTIPOLYGON, EPSG:4326) alongside
#' the parcels so it can be restored instantly on project load - without a
#' network round-trip to geo.api.gouv.fr nor a background worker. The map's
#' render observer needs BOTH parcels AND commune geometry; caching the
#' latter on disk removes it from the critical path of "Projet charge ->
#' parcelles affichees".
#'
#' @param project_id Character. Project ID.
#' @param commune_geom sf object. Commune boundary geometry.
#'
#' @return Logical. TRUE if saved, FALSE if skipped (invalid/empty input).
#'
#' @noRd
save_commune_geometry <- function(project_id, commune_geom) {
  if (!inherits(commune_geom, "sf") || nrow(commune_geom) == 0) {
    return(FALSE)
  }

  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  data_dir <- file.path(project_path, "data")
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  gpkg_path <- file.path(data_dir, "commune.gpkg")

  tryCatch({
    # Ensure a valid CRS before saving (default to WGS84, as served by
    # geo.api.gouv.fr and expected by Leaflet).
    if (is.na(sf::st_crs(commune_geom))) {
      commune_geom <- sf::st_set_crs(commune_geom, 4326)
    }

    if (file.exists(gpkg_path)) {
      unlink(gpkg_path)
    }
    sf::st_write(commune_geom, gpkg_path, driver = "GPKG", quiet = TRUE)
    cli::cli_alert_success("Saved commune geometry: {basename(gpkg_path)}")
    TRUE
  }, error = function(e) {
    # Best-effort: a failed commune-geometry save must never block the
    # project save. The legacy async refetch path remains as fallback.
    cli::cli_warn("Failed to save commune geometry (non-blocking): {e$message}")
    FALSE
  })
}


#' Load the commune boundary geometry from project
#'
#' @description
#' Reads back the commune contour persisted by [save_commune_geometry()].
#' Returns NULL for legacy projects saved before this cache existed - the
#' caller then falls back to the async refetch path.
#'
#' @param project_id Character. Project ID.
#'
#' @return sf object with the commune boundary, or NULL if not found.
#'
#' @noRd
load_commune_geometry <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  gpkg_path <- file.path(project_path, "data", "commune.gpkg")
  if (!file.exists(gpkg_path)) {
    return(NULL)
  }

  commune_sf <- tryCatch(
    sf::st_read(gpkg_path, quiet = TRUE),
    error = function(e) {
      cli::cli_warn("Failed to load commune geometry: {e$message}")
      NULL
    }
  )

  if (is.null(commune_sf) || !inherits(commune_sf, "sf") ||
      nrow(commune_sf) == 0) {
    return(NULL)
  }

  # Guard against a CRS-less file (older sf writes); Leaflet needs WGS84.
  if (is.na(sf::st_crs(commune_sf))) {
    sf::st_crs(commune_sf) <- 4326
  }

  commune_sf
}


#' Backfill the commune-geometry cache for all legacy projects
#'
#' @description
#' One-shot migration. For every project that has parcels but no cached
#' commune boundary (`data/commune.gpkg`, introduced v0.74.0), fetch the
#' commune contour from geo.api.gouv.fr and persist it, so subsequent
#' loads inject it synchronously (instant map) instead of refetching it
#' asynchronously on every open. Idempotent: projects that already carry a
#' cache are skipped. Network-bound - one API call per uncached commune.
#'
#' @return Invisibly, a data.frame with one row per project (columns
#'   `id`, `name`, `status`). `status` is one of: `"backfilled"`,
#'   `"cached"` (already had it), `"no_parcels"`, `"no_commune_code"`,
#'   `"fetch_failed"`.
#'
#' @noRd
backfill_all_commune_geometries <- function() {
  root <- get_projects_root()
  if (is.null(root) || !dir.exists(root)) {
    cli::cli_warn("Projects root not found")
    return(invisible(data.frame()))
  }

  ids <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  if (length(ids) == 0) {
    cli::cli_alert_info("No project found under {.path {root}}")
    return(invisible(data.frame()))
  }

  rows <- lapply(ids, function(id) {
    status <- tryCatch({
      if (!is.null(load_commune_geometry(id))) {
        "cached"
      } else {
        parcels <- load_parcels(id)
        if (is.null(parcels) || nrow(parcels) == 0 ||
            !"code_insee" %in% names(parcels)) {
          "no_parcels"
        } else {
          codes <- unique(parcels$code_insee)
          codes <- codes[!is.na(codes) & nzchar(codes)]
          if (length(codes) == 0) {
            "no_commune_code"
          } else {
            geom <- get_commune_geometry(codes[1])
            if (!is.null(geom) && isTRUE(save_commune_geometry(id, geom))) {
              "backfilled"
            } else {
              "fetch_failed"
            }
          }
        }
      }
    }, error = function(e) {
      cli::cli_warn("Backfill failed for {id}: {conditionMessage(e)}")
      "fetch_failed"
    })

    meta <- tryCatch(load_project_metadata(id), error = function(e) NULL)
    data.frame(
      id = id,
      name = meta$name %||% NA_character_,
      status = status,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  n_bf <- sum(out$status == "backfilled")
  n_cached <- sum(out$status == "cached")
  cli::cli_alert_success(
    "Backfill termine : {n_bf} projet(s) rechauffe(s), {n_cached} deja en cache, sur {nrow(out)}")
  invisible(out)
}


#' Add the normalised twin of every indicator column
#'
#' @description
#' An indicator is stored **twice**: its raw value in its own unit, and its
#' normalised value on 0-100. The two answer different questions - "how many
#' m3/ha?" and "where does that place this UGF?" - and keeping only one cost us
#' both ways. Only the raw was persisted, so every consumer wanting a comparable
#' score had to re-derive it, and the screen showed an NDVI of 0.227 beside 75 %
#' of ancientness as if the two scales were one.
#'
#' **The normalisation is the core's, per indicator** (`normalize_indicator()`),
#' not a min-max over whatever rows this project happens to hold. Absolute
#' bounds make two projects comparable; a min-max would make each project its
#' own yardstick, and the same stand would score differently depending on its
#' neighbours.
#'
#' It is also the function `create_family_index()` uses - and that one **prefers
#' `_norm` columns when they exist**. Persisting them therefore makes the stored
#' number and the displayed number the same, instead of two truths that drift.
#'
#' **Only indicators the core declares** get a twin. `normalize_indicator()`
#' returns an unknown indicator's values **unchanged** rather than failing - a
#' silent pass-through. Writing that as `_norm` would be a lie by omission: the
#' column would claim a 0-100 scale while holding raw metres or inhabitants,
#' and the family view *prefers* `_norm` when it exists, so the raw value would
#' then be displayed as if normalised. Better no twin than a false one.
#'
#' The indicator's status column (`.<code>_status`), when present, is passed as
#' `statut`: for P2 it tells a site index in metres from a production in
#' m3/ha/yr, which do not share a ceiling.
#'
#' @param df A data.frame of indicators.
#' @return The same data.frame with `<indicateur>_norm` columns added.
#' @noRd
.add_normalized_indicators <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0L) return(df)
  cols <- grep("^indicateur_", names(df), value = TRUE)
  cols <- cols[!grepl("_norm$", cols)]
  if (length(cols) == 0L) return(df)

  connus <- tryCatch(as.character(nemeton::list_indicators()),
                     error = function(e) character(0))
  inconnus <- setdiff(cols, connus)
  cols <- intersect(cols, connus)

  for (cc in cols) {
    v <- suppressWarnings(as.numeric(df[[cc]]))
    # Le statut qualifie parfois l'unite de la valeur : P2 en mode CHM est un
    # indice de station en metres (`.p2_status = "indice_station_m"`), plafonne
    # a 40 et non a 15 m3/ha/an (nemeton >= 0.207.0). NULL si absent.
    st_col <- .indicator_status_col(cc)
    st <- if (!is.null(st_col)) df[[st_col]]
    n <- tryCatch(nemeton::normalize_indicator(cc, v, statut = st),
                  error = function(e) NULL)
    if (is.null(n) || length(n) != nrow(df)) next
    df[[paste0(cc, "_norm")]] <- as.numeric(n)
  }
  if (length(inconnus)) {
    cli::cli_warn(c(
      "Normalisation impossible pour {length(inconnus)} indicateur{?s} : {.field {inconnus}}.",
      i = "Leur valeur brute est conserv\u00e9e ; la colonne {.code _norm} manque."))
  }
  df
}


#' Save indicators results to project
#'
#' @description
#' Saves computed indicators to project.
#'
#' @param project_id Character. Project ID.
#' @param indicators List or data.frame. Computed indicators.
#'
#' @return Logical. TRUE if successful.
#'
#' @noRd
save_indicators <- function(project_id, indicators) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  indicators_path <- file.path(project_path, "data", "indicators.parquet")

  tryCatch({
    if (!requireNamespace("arrow", quietly = TRUE)) {
      cli::cli_abort("Package 'arrow' is required")
    }

    # Convert to data.frame (drop geometry if sf - UGF geometry is
    # rebuilt from tenements.gpkg + ugs.json at load time via
    # ug_build_sf(), keeping indicators.parquet geometry-free).
    if (inherits(indicators, "sf")) {
      indicators_df <- sf::st_drop_geometry(indicators)
    } else if (is.list(indicators) && !is.data.frame(indicators)) {
      indicators_df <- as.data.frame(indicators)
    } else {
      indicators_df <- indicators
    }

    # Write via temp file to avoid Windows memory-mapped file lock
    # (renommage atomique ; la cible n'est supprimee avant que si le renommage
    # echoue, cas Windows)
    tmp_path <- paste0(indicators_path, ".tmp")
    arrow::write_parquet(indicators_df, tmp_path)
    .replace_file(tmp_path, indicators_path)

    # Update metadata (avec NDP detecte)
    # Essayer d'abord via les attributs, puis fallback sur le cache disque.
    # Depuis nemeton v0.16.0, detect_ndp() retourne un ndp_result (S3 list) ;
    # as.integer() extrait le niveau pour persistance en base.
    ndp_level <- as.integer(nemeton::detect_ndp(indicators))
    if (ndp_level == 0L) {
      ndp_level <- detect_ndp_from_cache(project_path)
    }
    update_project_metadata(project_id, list(
      indicators_computed = TRUE,
      ndp_level = ndp_level,
      updated_at = Sys.time(),
      status = "completed"
    ))

    cli::cli_alert_success("Indicators saved")
    TRUE

  }, error = function(e) {
    cli::cli_abort("Failed to save indicators: {e$message}")
  })
}


#' Load indicators from project
#'
#' @description
#' Loads computed indicators from project.
#'
#' @param project_id Character. Project ID.
#'
#' @return data.frame with indicators, or NULL if not found.
#'
#' @noRd
load_indicators <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  indicators_path <- file.path(project_path, "data", "indicators.parquet")

  if (!file.exists(indicators_path)) {
    return(NULL)
  }

  tryCatch({
    results <- arrow::read_parquet(indicators_path)

    # Migration des slugs L (coeur v0.176.0, spec 045). Tout projet calcule
    # avant porte `indicateur_l1_sylvosphere` / `indicateur_l2_fragmentation` ;
    # sans ce passage, ses deux cartes Paysage disparaissent de l'onglet, la
    # table des familles ne connaissant plus ces noms. Le renommage est sans
    # perte, prend les variantes `_norm`, et laisse intact un jeu deja migre -
    # il se pose donc une fois pour toutes dans le chemin de lecture.
    results <- nemeton::migrer_colonnes_l(results, quiet = TRUE)

    # Restaurer les attributs NDP depuis les metadonnees projet
    meta <- load_project_metadata(project_id)
    if (!is.null(meta$ndp_level)) {
      results <- restore_ndp_attributes(results, meta$ndp_level)
    }

    results
  }, error = function(e) {
    cli::cli_warn("Failed to load indicators: {e$message}")
    NULL
  })
}


#' Save sampling plots to project
#'
#' @description
#' Persists the sampling placettes generated by `mod_sampling` to
#' `<project>/data/samples.gpkg` so they survive an app restart and
#' can be consumed by `mod_monitoring` (zone registration) without
#' regenerating. Updates `samples_count` and `samples_generated_at`
#' in `metadata.json`.
#'
#' Failures emit a warning and return FALSE - saving samples must
#' never crash the sampling pipeline.
#'
#' @param project_id Character. Project ID.
#' @param plots sf POINT with at least a `plot_id` column.
#' @param layer Character. GPKG layer name. Default `"plots"` for
#'   Base/Over calibration plots from `mod_sampling`. Pass
#'   `"observations"` when sending action-plan observation points
#'   from `mod_action_plan`. The two layers coexist in the same
#'   `samples.gpkg` file and never overwrite each other.
#'
#' @return Logical. TRUE on success.
#' @noRd
save_samples <- function(project_id, plots, layer = "plots") {
  if (!inherits(plots, "sf")) {
    cli::cli_warn("save_samples: plots must be an sf object")
    return(FALSE)
  }
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_warn("save_samples: project not found: {project_id}")
    return(FALSE)
  }
  data_dir <- file.path(project_path, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  gpkg_path <- file.path(data_dir, "samples.gpkg")

  tryCatch({
    # Write only the named layer; keep any sibling layer intact.
    # `delete_layer = TRUE` replaces just this layer if it already
    # exists, so calibration "plots" survive an observations write
    # and vice-versa. On an empty file GDAL creates it.
    sf::st_write(plots, gpkg_path, layer = layer, driver = "GPKG",
                 append = FALSE, delete_layer = TRUE, quiet = TRUE)

    # `samples_count` documents the calibration plan size; only the
    # default `"plots"` layer drives this metadata. Observations
    # written by mod_action_plan have their own provenance and
    # should not perturb the sampling plan's bookkeeping.
    if (identical(layer, "plots")) {
      update_project_metadata(project_id, list(
        samples_count        = nrow(plots),
        samples_generated_at = Sys.time()
      ))
    }
    cli::cli_alert_success(
      "Saved {nrow(plots)} sample plots to layer '{layer}'"
    )
    TRUE
  }, error = function(e) {
    cli::cli_warn("Failed to save samples: {e$message}")
    FALSE
  })
}


#' Load sampling plots from project
#'
#' @description
#' Reads a layer from `<project>/data/samples.gpkg` written by
#' [save_samples()]. Returns NULL if the file or the requested
#' layer is missing - callers must treat NULL as "no plan yet".
#'
#' @param project_id Character. Project ID.
#' @param layer Character. GPKG layer name. Default `"plots"` for
#'   Base/Over calibration plots. Pass `"observations"` to read
#'   action-plan observation points.
#'
#' @return sf POINT, or NULL.
#' @noRd
load_samples <- function(project_id, layer = "plots") {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) return(NULL)

  gpkg_path <- file.path(project_path, "data", "samples.gpkg")
  if (!file.exists(gpkg_path)) return(NULL)

  available <- tryCatch(sf::st_layers(gpkg_path)$name,
                        error = function(e) character(0))
  if (!(layer %in% available)) return(NULL)

  tryCatch({
    sf::st_read(gpkg_path, layer = layer, quiet = TRUE)
  }, error = function(e) {
    cli::cli_warn("Failed to load samples layer '{layer}': {e$message}")
    NULL
  })
}


#' Invalidate cached indicator results
#'
#' @description
#' Sets \code{indicators.parquet} aside and flips the \code{indicators_computed}
#' metadata flag back to \code{FALSE}. Call this whenever the UGF layout
#' changes in a way that invalidates previously computed indicator values
#' (e.g. after a decoupage import that renumbers \code{ug_id}s - the
#' cached values are keyed on ug_id and silently tell
#' \code{compute_all_indicators()} that "everything is already done",
#' leaving the new UGFs unpopulated).
#'
#' The file is RENAMED, never deleted: \code{data/indicators.perime-<timestamp>.parquet}.
#' Only the \code{INDICATORS_STALE_KEEP} most recent generations are kept, and
#' the kept ones are listed in \code{metadata$indicateurs_perimes} (file,
#' reason, date). An accidental invalidation is therefore no longer a loss.
#' Only the exact name \code{indicators.parquet} is ever read back, so a
#' set-aside file cannot be mistaken for current results.
#'
#' The parcels cache and the UGF layout files (\code{tenements.gpkg},
#' \code{ugs.json}) are untouched - only the indicator results.
#'
#' @param project_id Character. Project ID.
#' @param motif Character. Why the indicators are invalidated (\code{"ugf"},
#'   \code{"parcelles"}, ...), recorded in the metadata.
#'
#' @return Invisible TRUE if the indicators file was present and set aside,
#'   FALSE if there was nothing to invalidate.
#' @noRd
invalidate_indicators <- function(project_id, motif = "invalidation") {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(invisible(FALSE))
  }

  indicators_path <- file.path(project_path, "data", "indicators.parquet")
  removed <- FALSE
  archive <- NULL
  if (file.exists(indicators_path)) {
    archive <- .set_aside_indicators(project_id, project_path, indicators_path, motif)
    removed <- !is.null(archive)
  }

  # Reset the computed flag + status so the UI knows the project is
  # back to a "needs compute" state.
  updates <- list(
    indicators_computed = FALSE,
    status = "draft",
    updated_at = Sys.time()
  )
  if (!is.null(archive)) updates$indicateurs_perimes <- archive
  tryCatch({
    update_project_metadata(project_id, updates)
  }, error = function(e) {
    cli::cli_warn("Failed to reset indicators flag: {e$message}")
  })

  if (removed) {
    cli::cli_alert_info("Invalidated cached indicators for project {project_id}")
  }
  invisible(removed)
}

#' Number of set-aside indicator generations kept per project
#' @noRd
INDICATORS_STALE_KEEP <- 2L

#' Rename indicators.parquet to a dated stale copy and prune old copies
#'
#' @param project_id,project_path Project identifier and directory.
#' @param indicators_path Path of the current `indicators.parquet`.
#' @param motif Reason recorded with the copy.
#' @return The list of kept stale copies (newest first) to store in
#'   `metadata$indicateurs_perimes`, or `NULL` when the rename failed (the
#'   current file is then left in place: invalidating would otherwise mean
#'   deleting it).
#' @noRd
.set_aside_indicators <- function(project_id, project_path, indicators_path,
                                  motif = "invalidation") {
  data_dir <- dirname(indicators_path)
  meta <- tryCatch(load_project_metadata(project_id), error = function(e) NULL)

  now <- Sys.time()
  base <- sprintf("indicators.perime-%s", format(now, "%Y%m%d-%H%M%S"))
  dest <- file.path(data_dir, paste0(base, ".parquet"))
  i <- 1L
  while (file.exists(dest)) {
    i <- i + 1L
    dest <- file.path(data_dir, sprintf("%s-%d.parquet", base, i))
  }
  if (!isTRUE(file.rename(indicators_path, dest))) {
    cli::cli_warn(c(
      "Projet {project_id} : indicateurs impossibles a mettre de cote ({.path {indicators_path}}).",
      i = "Ils sont conserves en place ; le projet reste marque a recalculer."))
    return(NULL)
  }
  cli::cli_alert_info("Indicateurs mis de cote : {.path {basename(dest)}}")

  entree <- list(fichier = basename(dest), motif = motif,
                 date = format(now, "%Y-%m-%dT%H:%M:%S"))
  anciennes <- meta$indicateurs_perimes %||% list()
  anciennes <- Filter(function(e) {
    is.list(e) && !is.null(e$fichier) &&
      file.exists(file.path(data_dir, e$fichier))
  }, anciennes)
  gardees <- c(list(entree), anciennes)
  if (length(gardees) > INDICATORS_STALE_KEEP) {
    trop <- gardees[-seq_len(INDICATORS_STALE_KEEP)]
    gardees <- gardees[seq_len(INDICATORS_STALE_KEEP)]
    for (e in trop) unlink(file.path(data_dir, e$fichier))
  }
  gardees
}


# PERF - chronometre leger, ACTIVE uniquement si NEMETON_PERF_TRACE est
# vrai ("1"/"true"/"yes"). En prod la variable est absente : aucun cout,
# aucune sortie console. Sert a repondre " ou ca coince au chargement
# d'un projet recent ? " - chaque appel logge le temps ecoule en ms via
# cli (regle 9 : pas de print/cat/message). `expr` est evalue dans le
# frame appelant (lazy), donc le wrapping ne change pas la semantique.
.perf_trace_on <- function() {
  v <- tolower(Sys.getenv("NEMETON_PERF_TRACE", ""))
  v %in% c("1", "true", "yes", "on")
}

.perf_time <- function(label, expr) {
  if (!.perf_trace_on()) {
    return(eval.parent(substitute(expr)))
  }
  t0 <- Sys.time()
  res <- eval.parent(substitute(expr))
  dt_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
  cli::cli_inform("\u23f1 [perf] {label}: {sprintf('%.0f', dt_ms)} ms")
  res
}

# PERF - pre-chauffage de la pile geo (arrow + geoarrow + sf). Le tout
# 1er `arrow::read_parquet()` + `sf::st_as_sf()` d'une session R paie
# ~1,5-2 s de chargement paresseux de namespaces / generation de code S4.
# Ce cout frappait jusqu'ici le PREMIER clic " projet recent " (mesure :
# load_project a froid ~= 2 s, dont ~1,6 s de warm-up, vs ~130 ms a chaud).
# On le deplace hors du chemin critique en l'executant une fois, peu apres
# le demarrage de l'app (via later, cf. app_server) : un mini round-trip
# parquet en memoire exerce exactement les chemins qui seront re-empruntes
# par load_parcels(). Idempotent et best-effort : toute erreur est avalee.
.geo_stack_warmed <- new.env(parent = emptyenv())
.geo_stack_warmed$done <- FALSE

warmup_geo_stack <- function() {
  if (isTRUE(.geo_stack_warmed$done)) {
    return(invisible(FALSE))
  }
  .geo_stack_warmed$done <- TRUE
  ok <- requireNamespace("arrow", quietly = TRUE) &&
        requireNamespace("geoarrow", quietly = TRUE) &&
        requireNamespace("sf", quietly = TRUE)
  if (!ok) {
    return(invisible(FALSE))
  }
  tryCatch({
    t0 <- Sys.time()
    # Deux petits polygones adjacents : exerce st_sf/st_sfc + les
    # operations geometriques (st_union/st_area) ET les deux backends IO
    # reellement empruntes par load_parcels/load_commune_geometry :
    #   - arrow/geoarrow (parquet) - chemin rapide par defaut,
    #   - GDAL (st_read GPKG) - dont la 1ere init coute le plus cher.
    poly <- function(x) sf::st_polygon(list(rbind(
      c(x, 0), c(x + 1, 0), c(x + 1, 1), c(x, 1), c(x, 0))))
    sfobj <- sf::st_sf(
      id = 1:2,
      geometry = sf::st_sfc(poly(0), poly(1), crs = 4326)
    )
    invisible(sf::st_area(sfobj))
    invisible(sf::st_union(sfobj))
    p_pq <- tempfile(fileext = ".parquet")
    p_gp <- tempfile(fileext = ".gpkg")
    on.exit(unlink(c(p_pq, p_gp)), add = TRUE)
    arrow::write_parquet(sfobj, p_pq)
    invisible(sf::st_as_sf(arrow::read_parquet(p_pq, as_data_frame = FALSE)))
    suppressWarnings(sf::st_write(sfobj, p_gp, quiet = TRUE,
                                  delete_dsn = TRUE))
    invisible(sf::st_read(p_gp, quiet = TRUE))
    if (.perf_trace_on()) {
      dt_ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
      cli::cli_inform("\u23f1 [perf] warmup_geo_stack: {sprintf('%.0f', dt_ms)} ms")
    }
  }, error = function(e) {
    cli::cli_warn("Geo-stack warm-up skipped (non-blocking): {conditionMessage(e)}")
  })
  invisible(TRUE)
}

# PERF - pre-chauffage des WORKERS future (pas seulement du thread principal).
# Le tout 1er `future_promise` d'une session charge le namespace nemetonshiny +
# ses deps (sf/terra/leaflet...) DANS le process worker : ~5-6 s mesurees. Ce cout
# frappait la 1re tache async - typiquement le `db_sync_project_async()` declenche
# a l'ouverture du 1er projet, ou le 1er calcul / moteur. On le deplace hors du
# chemin critique en chargeant le namespace dans les workers en arriere-plan, peu
# apres le demarrage (fire-and-forget, cf. app_server). Best-effort, idempotent,
# et NO-OP si le plan est sequentiel (sinon future_promise tournerait sur le
# thread principal et bloquerait - exactement ce qu'on veut eviter).
.async_workers_warmed <- new.env(parent = emptyenv())
.async_workers_warmed$done <- FALSE

warmup_async_workers <- function() {
  if (isTRUE(.async_workers_warmed$done)) {
    return(invisible(FALSE))
  }
  .async_workers_warmed$done <- TRUE
  ok <- requireNamespace("future", quietly = TRUE) &&
        requireNamespace("promises", quietly = TRUE)
  if (!ok) {
    return(invisible(FALSE))
  }
  plan_classes <- class(tryCatch(future::plan(), error = function(e) NULL))
  if (!any(c("multisession", "multicore", "cluster") %in% plan_classes)) {
    return(invisible(FALSE))   # plan sequentiel : aucun worker a chauffer.
  }
  dev_path <- tryCatch(
    if (isTRUE(pkgload::is_dev_package("nemetonshiny")))
      find.package("nemetonshiny") else NULL,
    error = function(e) NULL)
  # Chauffer jusqu'a 4 workers concurremment : couvre les taches courantes
  # (sync, calcul, moteur) sans saturer la machine au demarrage.
  #
  # TOUJOURS laisser un worker LIBRE (`n - 1`). Le pool est borne depuis
  # `.resolve_parallel_workers()` (4 par defaut, 2 au plancher) : warmer les
  # `min(nbrOfWorkers, 4)` occuperait la totalite du pool pendant les ~5-6 s de
  # chargement du namespace, et toute tache async declenchee pendant ce temps
  # (db_sync du 1er projet, ouverture d'une modale qui calcule) attendrait la fin
  # du warmup - boucle d'evenements Shiny figee pour l'utilisateur. Avant le
  # bornage du pool, 4 workers sur 8 restaient libres et masquaient le probleme.
  n <- tryCatch(as.integer(future::nbrOfWorkers()), error = function(e) 1L)
  n <- max(1L, min(n - 1L, 4L))
  for (i in seq_len(n)) {
    tryCatch(
      promises::future_promise({
        if (!is.null(dev_path) && requireNamespace("pkgload", quietly = TRUE)) {
          pkgload::load_all(dev_path, quiet = TRUE)
        } else {
          loadNamespace("nemetonshiny")
        }
        TRUE
      }, seed = TRUE, globals = list(dev_path = dev_path)),
      error = function(e) NULL)
  }
  invisible(TRUE)
}

#' Build and attach `indicators_sf` to a (migrated) project
#'
#' @description
#' Builds `project$indicators_sf` - one row per UGF with dissolved
#' geometry + indicator columns joined via `ug_id` - and refreshes the
#' geometry-free `project$indicators` with the FRESH UGF metadata. Used
#' by family/synthesis/sampling/monitoring so they can map, score and
#' aggregate at the UGF level without rejoining parcels.
#'
#' IMPORTANT: indicators.parquet captures UGF metadata (label, groupe,
#' surfaces, cadastral_refs) AT COMPUTE TIME. If the user later renames
#' / re-groups / splits UGFs, ugs.json gets updated but the parquet
#' still carries stale metadata. We therefore rebuild BOTH
#' `project$indicators` and `project$indicators_sf` from the fresh
#' `ug_sf`, keeping only the indicator VALUES from the parquet. This way
#' every consumer (mod_family table, mod_synthesis, export, ...) sees
#' the current UGF labels/groupes without having to re-join manually.
#'
#' Split out of [load_project()] so the interactive load path can DEFER
#' it off the critical rendering path (see the `mod_home` load
#' observer): `ug_build_sf()` does one `sf::st_union()` per UGF and can
#' cost 0.5-3 s on projects with many UGFs, none of which is needed to
#' render parcels on the map. The `tenements`/`ugs` data must already be
#' present (i.e. call [ensure_project_ug()] first).
#'
#' Non-blocking: any failure is warned and the project is returned
#' unchanged (without `indicators_sf`).
#'
#' @param project A migrated project list.
#' @return The project, possibly with `indicators_sf` attached and
#'   `indicators` refreshed.
#' @noRd
attach_indicators_sf <- function(project) {
  tryCatch({
    if (!is.null(project$indicators) && has_ug_data(project) &&
        "ug_id" %in% names(project$indicators)) {
      ug_sf <- .perf_time("ug_build_sf", ug_build_sf(project))
      if (!is.null(ug_sf) && nrow(ug_sf) > 0) {
        # Drop stale UGF metadata columns from indicators before merge
        dup_cols <- intersect(
          c("label", "groupe", "surface_m2", "surface_sig_m2",
            "n_tenements", "cadastral_refs", UG_ONF_COLS),
          names(project$indicators)
        )
        ind <- project$indicators[, setdiff(names(project$indicators), dup_cols),
                                  drop = FALSE]
        project$indicators_sf <- merge(ug_sf, ind, by = "ug_id", all.x = TRUE)
        # Refresh the geometry-free indicators with fresh UGF metadata.
        # Single source of truth so mod_family, mod_synthesis and
        # service_export stay in sync with the UGF tab.
        project$indicators <- sf::st_drop_geometry(project$indicators_sf)
      }
    }
  }, error = function(e) {
    cli::cli_warn("indicators_sf build failed (non-blocking): {e$message}")
  })
  project
}

#' Read a project's data files, without any migration or write
#'
#' Shared by [load_project()] and the read-only [projet_lire()] so both see the
#' same project: parcels, commune boundary, indicators, comments. Every reader
#' called here is read-only.
#'
#' @param project_id Character.
#' @return A list with `parcels`, `commune_geometry`, `indicators`, `comments`.
#' @noRd
.read_project_files <- function(project_id) {
  list(
    parcels = .perf_time("load_parcels", load_parcels(project_id)),
    commune_geometry = .perf_time("load_commune_geometry", load_commune_geometry(project_id)),
    indicators = .perf_time("load_indicators", load_indicators(project_id)),
    comments = .perf_time("load_comments", load_comments(project_id))
  )
}

#' Load project
#'
#' @description
#' Loads a complete project including metadata, parcels, and indicators.
#' @param build_indicators_sf Logical. Build `indicators_sf` now (`FALSE`
#'   lets the interactive path defer it, see [attach_indicators_sf()]).
#'
#' @param project_id Character. Project ID.
#'
#' @return List with project data, or NULL if not found.
#'
#' @noRd
load_project <- function(project_id, build_indicators_sf = TRUE) {
  .t_load0 <- Sys.time()
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_warn("Project not found: {project_id}")
    return(NULL)
  }

  metadata <- .perf_time("load_project_metadata", load_project_metadata(project_id))
  if (is.null(metadata)) {
    return(NULL)
  }

  # Projet anterieur a la 1.0.0 : pas repris (pas de migration). La liste des
  # projets le signale et ne propose que sa suppression.
  if (!.projet_format_ok(metadata)) {
    cli::cli_warn(c(
      "Projet {project_id} : cr\u00e9\u00e9 avant la version 1.0.0, il n'est pas repris.",
      i = "Le recr\u00e9er (m\u00eames parcelles) ; l'ancien dossier peut \u00eatre supprim\u00e9."))
    return(NULL)
  }

  project <- list(
    id = project_id,
    path = project_path,
    metadata = metadata
  )
  project <- c(project, .read_project_files(project_id))

  # UGF : chargees, ou decoupage par defaut a la premiere ouverture.
  project <- tryCatch(
    .perf_time("ensure_project_ug", ensure_project_ug(project_id, project)),
    error = function(e) {
      cli::cli_warn("UGF initialisation failed (non-blocking): {e$message}")
      project
    }
  )

  # Build indicators_sf inline by default (keeps every existing caller
  # and test unchanged). The interactive recent-project load path opts
  # out (`build_indicators_sf = FALSE`) and attaches it via later() so
  # the heavy per-UGF st_union() doesn't block the first map render.
  if (isTRUE(build_indicators_sf)) {
    project <- attach_indicators_sf(project)
  }

  # Sync vers PostGIS si configure et si le projet a des indicateurs.
  # DEFERE via later::later() pour ne PAS bloquer le retour de
  # load_project() - et donc le rendu des parcelles sur la carte. La
  # connexion + l'upload PostGIS peuvent prendre plusieurs secondes
  # (jusqu'a ~20 s sur un hote injoignable, faute de connect_timeout
  # cote libpq), ce qui donnait l'impression d'un gel entre " Connected
  # to PostgreSQL ... " et " Affichage des parcelles ". Le sync est un
  # effet de bord best-effort : aucun consommateur aval n'en depend, on
  # peut donc le repousser apres le premier flush de la carte.
  # v0.84.0 - le sync tourne desormais dans un worker `future`
  # (db_sync_project_async) au lieu d'un callback `later()` qui
  # s'executait sur le thread principal et gelait l'event loop / le
  # rendu carte apres le 1er flush (ressenti " connexion BD -> carte
  # longue "). Best-effort : aucun consommateur aval n'attend le resultat.
  if (is_db_configured() && isTRUE(metadata$indicators_computed)) {
    tryCatch(
      db_sync_project_async(project_id),
      error = function(e) cli::cli_warn(
        "Background DB sync dispatch failed (non-blocking): {conditionMessage(e)}")
    )
  }

  if (.perf_trace_on()) {
    .dt_ms <- as.numeric(difftime(Sys.time(), .t_load0, units = "secs")) * 1000
    cli::cli_inform(c("v" = "\u23f1 [perf] load_project TOTAL ({project_id}): {sprintf('%.0f', .dt_ms)} ms (build_indicators_sf={build_indicators_sf})"))
  }

  project
}


#' Does a project already carry a usable `monitoring_zone_id`?
#'
#' Single source of truth for the "zone id already known" predicate,
#' shared by [hydrate_monitoring_zone_id()] (early no-op return) and by
#' the `mod_home` load observer, which uses it to SKIP opening a
#' monitoring-DB connection entirely when hydration would be a no-op.
#'
#' Opening that connection (a synchronous `nemeton::db_connect()` +
#' schema-migration SELECT) was paid on EVERY project load - including
#' the common case where `metadata.json` already carries the id - and
#' could freeze the UI for seconds on a slow/unreachable Postgres host
#' (libpq has no `connect_timeout` here). Gating on this predicate
#' removes the round-trip from the critical load path for any project
#' saved after spec 011.
#'
#' @param project A project list (or NULL).
#' @return `TRUE` when `metadata$monitoring_zone_id` is a single value
#'   coercible to a non-NA integer; `FALSE` otherwise.
#' @noRd
.has_monitoring_zone_id <- function(project) {
  if (is.null(project)) return(FALSE)
  existing <- project$metadata$monitoring_zone_id
  !is.null(existing) && length(existing) == 1L &&
    !is.na(suppressWarnings(as.integer(existing)))
}

#' Hydrate `monitoring_zone_id` from a monitoring-DB lookup
#'
#' @description
#' Spec 011 (v0.41.0). When a project is loaded and its `metadata.json`
#' has no `monitoring_zone_id` (typical for projects registered as
#' zones before spec 011, or projects whose metadata was wiped), look
#' up the bound zone via `nemeton::find_zone_by_project(con, project$id)`
#' and persist the result back to `metadata.json` so subsequent loads
#' (and the monitoring tab's pre-select observer) find it immediately.
#'
#' This makes the project <-> zone binding canonically driven by the
#' DB column `monitoring_zone.project_uuid` (added in nemeton 0.44.0)
#' rather than by the freely-editable `metadata.json` - surviving
#' metadata loss, project copies, and out-of-band DB restores.
#'
#' No-ops (returns the project untouched) when:
#'   * `project` is NULL or has no `id`,
#'   * `metadata$monitoring_zone_id` is already set (truthy),
#'   * `con` is NULL (DB not configured / connection failed),
#'   * `nemeton` package is not loadable,
#'   * `find_zone_by_project()` returns `integer(0)` or errors.
#'
#' On hit, mutates `project$metadata$monitoring_zone_id` AND persists
#' the same value to disk via `update_project_metadata()`. Persistence
#' failure is non-fatal - the in-memory project is still returned with
#' the hydrated id so the current session benefits.
#'
#' @param project A project list (typically the return of
#'   [load_project()]).
#' @param con A monitoring-DB connection or NULL.
#'
#' @return The project, possibly with `metadata$monitoring_zone_id`
#'   populated.
#' @noRd
hydrate_monitoring_zone_id <- function(project, con) {
  if (is.null(project) || is.null(project$id)) return(project)
  if (.has_monitoring_zone_id(project)) {
    return(project)
  }
  if (is.null(con)) return(project)
  if (!requireNamespace("nemeton", quietly = TRUE)) return(project)

  zid <- tryCatch(
    nemeton::find_zone_by_project(con, project$id),
    error = function(e) integer(0)
  )
  if (length(zid) != 1L) return(project)
  zid <- as.integer(zid)
  if (is.na(zid)) return(project)

  project$metadata$monitoring_zone_id <- zid
  tryCatch(
    update_project_metadata(project$id, list(monitoring_zone_id = zid)),
    error = function(e) {
      cli::cli_warn("Failed to persist hydrated monitoring_zone_id \\
                    for {.val {project$id}}: {conditionMessage(e)}")
    }
  )
  project
}


#' Save comments to project
#'
#' @description
#' Persists synthesis and family comments as a JSON file in the project directory.
#'
#' @param project_id Character. Project ID.
#' @param synthesis Character or NULL. Synthesis comment.
#' @param families Named list. Family comments keyed by family code.
#'
#' @return Logical. TRUE if successful, FALSE otherwise.
#'
#' @noRd
save_comments <- function(project_id, synthesis = NULL, families = NULL,
                          synthesis_sources = NULL) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_warn("save_comments: project_path is NULL for id={project_id}")
    return(FALSE)
  }

  data_dir <- file.path(project_path, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  comments_path <- file.path(data_dir, "comments.json")

  # Merge with existing data to avoid overwriting fields not provided
  existing <- NULL
  if (file.exists(comments_path)) {
    existing <- tryCatch(jsonlite::read_json(comments_path), error = function(e) NULL)
  }

  # Build data: use provided values, fall back to existing
  syn <- if (!is.null(synthesis)) synthesis else existing$synthesis
  fam <- if (!is.null(families) && length(families) > 0) families else existing$families
  # v0.85.0 - RAG context du commentaire de synthese (sources_md +
  # n_sources). Persiste pour reafficher le bloc " Sources documentaires "
  # au rechargement (sinon NULL en session -> seul le commentaire revenait)
  # et fournir les notes de bas de page a l'export Quarto. NULL = conserver
  # l'existant (edition manuelle du commentaire, qui ne change pas les
  # sources).
  src <- if (!is.null(synthesis_sources)) synthesis_sources else existing$synthesis_sources

  data <- list(synthesis = syn, families = fam, synthesis_sources = src)

  tryCatch({
    .write_json_atomic(data, comments_path, auto_unbox = TRUE, pretty = TRUE)
    n_fam <- if (is.list(fam)) length(fam) else 0L
    cli::cli_inform("Comments saved: synthesis={!is.null(syn)}, families={n_fam}")
    TRUE
  }, error = function(e) {
    cli::cli_warn("Failed to save comments: {e$message}")
    FALSE
  })
}


#' Load comments from project
#'
#' @description
#' Loads saved comments from the project directory.
#'
#' @param project_id Character. Project ID.
#'
#' @return List with synthesis and families, or NULL if not found.
#'
#' @noRd
load_comments <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) return(NULL)

  comments_path <- file.path(project_path, "data", "comments.json")
  if (!file.exists(comments_path)) return(NULL)

  tryCatch({
    jsonlite::read_json(comments_path)
  }, error = function(e) {
    cli::cli_warn("Failed to load comments: {e$message}")
    NULL
  })
}


# ---------------------------------------------------------------------------
# Recent-projects listing cache
#
# Scanning metadata.json for every project on each render is costly (one read +
# JSON parse per project, plus the health-check stats). This caches the fully
# scanned + sorted listing per root. Cache validity is checked against a cheap
# filesystem signature - one list.dirs() + one vectorised file.info() over the
# metadata.json files (stat only, no read/parse) - so the heavy reads are
# skipped on a re-render whenever nothing changed, while any add / remove /
# in-place edit invalidates the cache automatically. A short TTL is an extra
# backstop, and mutations clear the cache explicitly as a fast path. The cache
# holds the unlimited sorted listing; per-call `limit` is applied on the cached
# result so callers with different limits (mod_home uses 50, others 10) share it.
# ---------------------------------------------------------------------------

.recent_projects_cache <- new.env(parent = emptyenv())
.RECENT_PROJECTS_TTL_SECS <- 10

#' Empty recent-projects data.frame (canonical schema)
#' @noRd
.empty_recent_projects_df <- function() {
  data.frame(
    id = character(0),
    name = character(0),
    description = character(0),
    owner = character(0),
    status = character(0),
    parcels_count = integer(0),
    created_at = as.POSIXct(character(0)),
    updated_at = as.POSIXct(character(0)),
    is_corrupted = logical(0),
    is_ancien = logical(0)
  )
}

#' Cheap filesystem signature for a projects root
#'
#' @description
#' Captures the set of project directories and the mtime + size of each
#' metadata.json. Stat-only (no read/parse); used to detect whether the
#' cached listing is still valid. Returns NULL when the root is absent.
#'
#' @param root Character. Projects root directory.
#'
#' @return List signature, or NULL.
#'
#' @noRd
.recent_projects_signature <- function(root) {
  if (!dir.exists(root)) {
    return(NULL)
  }
  dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  if (length(dirs) == 0) {
    return(list(dirs = character(0), mtime = numeric(0), size = numeric(0)))
  }
  info <- file.info(file.path(dirs, "metadata.json"))
  list(
    dirs = basename(dirs),
    mtime = as.numeric(info$mtime),
    size = as.numeric(info$size)
  )
}

#' Invalidate the recent-projects listing cache
#'
#' @description
#' Drops the cached listing so the next call to [list_recent_projects()]
#' rescans the disk. Called by every project mutation (create / update /
#' delete) so changes appear immediately.
#'
#' @noRd
.invalidate_recent_projects_cache <- function() {
  if (exists("entry", envir = .recent_projects_cache, inherits = FALSE)) {
    rm("entry", envir = .recent_projects_cache)
  }
  invisible(NULL)
}

#' Scan and sort all projects under a root (cached)
#'
#' @description
#' Returns the full, unlimited listing sorted by updated_at (most recent
#' first). Serves a cached result when the root's filesystem signature is
#' unchanged and the TTL has not elapsed; otherwise rescans and recaches.
#'
#' @param root Character. Projects root directory.
#'
#' @return data.frame with project info (no limit applied).
#'
#' @noRd
.scan_recent_projects <- function(root) {
  sig <- .recent_projects_signature(root)

  # Serve from cache when same root, unchanged signature and within TTL.
  if (exists("entry", envir = .recent_projects_cache, inherits = FALSE)) {
    entry <- get("entry", envir = .recent_projects_cache, inherits = FALSE)
    fresh <- identical(entry$root, root) &&
      identical(entry$sig, sig) &&
      as.numeric(Sys.time() - entry$cached_at, units = "secs") < .RECENT_PROJECTS_TTL_SECS
    if (fresh) {
      return(entry$data)
    }
  }

  cache_result <- function(data) {
    assign("entry", list(root = root, sig = sig, data = data, cached_at = Sys.time()),
           envir = .recent_projects_cache)
    data
  }

  if (is.null(sig)) {
    # Root absent - return empty without caching (signature is NULL/unstable).
    return(.empty_recent_projects_df())
  }

  # List project directories
  dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
  if (length(dirs) == 0) {
    return(cache_result(.empty_recent_projects_df()))
  }

  # Load metadata for each project.
  # metadata.json is read once per project and reused for the health check
  # (avoids re-reading the same file two more times - see check_project_health).
  projects <- lapply(dirs, function(dir) {
    project_id <- basename(dir)

    metadata_path <- file.path(dir, "metadata.json")
    if (!file.exists(metadata_path)) {
      return(NULL)
    }

    tryCatch({
      metadata <- jsonlite::read_json(metadata_path)
      health <- check_project_health(project_id, metadata = metadata)
      data.frame(
        # Le DOSSIER est l'identifiant : c'est lui que tout acces disque
        # utilise. Un dossier copie garde le `metadata$id` de l'original, et la
        # carte chargeait (ou supprimait) l'original.
        id = project_id,
        name = metadata$name %||% "Untitled",
        description = metadata$description %||% "",
        owner = metadata$owner %||% "",
        status = metadata$status %||% "unknown",
        parcels_count = metadata$parcels_count %||% 0L,
        created_at = as.POSIXct(metadata$created_at %||% NA),
        updated_at = as.POSIXct(metadata$updated_at %||% NA),
        is_corrupted = !health$valid,
        # Anterieur a la 1.0.0 : non repris, seule la suppression est proposee.
        is_ancien = isTRUE(health$ancien),
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      # Corrupted metadata
      data.frame(
        id = project_id,
        name = "Corrupted Project",
        description = "",
        owner = "",
        status = "error",
        parcels_count = 0L,
        created_at = as.POSIXct(NA),
        updated_at = as.POSIXct(NA),
        is_corrupted = TRUE,
        is_ancien = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })

  # Combine and sort
  projects <- do.call(rbind, Filter(Negate(is.null), projects))

  if (is.null(projects) || nrow(projects) == 0) {
    return(cache_result(.empty_recent_projects_df()))
  }

  # Sort by updated_at (most recent first)
  projects <- projects[order(projects$updated_at, decreasing = TRUE), ]
  rownames(projects) <- NULL
  cache_result(projects)
}

#' List recent projects
#'
#' @description
#' Lists recent projects sorted by last update date. Backed by a short-lived
#' in-memory cache (see [.scan_recent_projects()]) so repeated renders don't
#' rescan the disk; the cache is invalidated on any project mutation.
#'
#' @param limit Integer. Maximum number of projects to return (default: 10).
#'
#' @return data.frame with project info.
#'
#' @noRd
list_recent_projects <- function(limit = 10L) {
  root <- get_projects_root()
  projects <- .scan_recent_projects(root)

  if (nrow(projects) > limit) {
    projects <- projects[seq_len(limit), ]
    rownames(projects) <- NULL
  }

  projects
}


#' Check project health
#'
#' @description
#' Checks if a project is valid and not corrupted.
#'
#' @param project_id Character. Project ID.
#' @param metadata List. Already-parsed metadata.json content. When supplied,
#'   the file is not re-read from disk (used by list_recent_projects to avoid
#'   redundant IO). Default NULL: read the file when present.
#'
#' @return List with valid (logical) and issues (character vector).
#'
#' @noRd
check_project_health <- function(project_id, metadata = NULL) {
  project_path <- get_project_path(project_id)

  issues <- character(0)

  # Check project exists
  if (is.null(project_path) || !dir.exists(project_path)) {
    return(list(valid = FALSE, issues = "Project directory not found"))
  }

  # Check metadata - read the file once (unless caller already provided it)
  metadata_path <- file.path(project_path, "metadata.json")
  metadata_present <- file.exists(metadata_path)
  if (is.null(metadata) && metadata_present) {
    metadata <- tryCatch(
      jsonlite::read_json(metadata_path),
      error = function(e) {
        issues <<- c(issues, paste("Corrupted metadata:", e$message))
        NULL
      }
    )
  }

  if (!metadata_present) {
    issues <- c(issues, "Missing metadata.json")
  } else if (!is.null(metadata) && is.null(metadata$name)) {
    issues <- c(issues, "Metadata missing 'name' field")
  }
  ancien <- !is.null(metadata) && !.projet_format_ok(metadata)
  if (ancien) issues <- c(issues, "Project predates version 1.0.0")

  # Check data directory
  data_path <- file.path(project_path, "data")
  if (!dir.exists(data_path)) {
    issues <- c(issues, "Missing data directory")
  }

  # Check parcels file if metadata says parcels exist
  if (!is.null(metadata) && !is.null(metadata$parcels_count) && metadata$parcels_count > 0) {
    parcels_path <- file.path(project_path, "data", "parcels.parquet")
    if (!file.exists(parcels_path)) {
      issues <- c(issues, "Missing parcels.parquet but metadata indicates parcels exist")
    }
  }

  list(
    valid = length(issues) == 0,
    issues = issues,
    ancien = ancien
  )
}


#' Delete project
#'
#' @description
#' Permanently deletes a project and all its data.
#'
#' @param project_id Character. Project ID.
#'
#' @return Logical. TRUE if deleted successfully.
#'
#' @noRd
delete_project <- function(project_id) {
  project_path <- get_project_path(project_id)

  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_warn("Project not found: {project_id}")
    return(FALSE)
  }

  tryCatch({
    unlink(project_path, recursive = TRUE)
    cli::cli_alert_success("Project deleted: {project_id}")
    .invalidate_recent_projects_cache()
    TRUE
  }, error = function(e) {
    cli::cli_abort("Failed to delete project: {e$message}")
  })
}


#' Update project status
#'
#' @description
#' Updates the status of a project.
#'
#' @param project_id Character. Project ID.
#' @param status Character. New status (draft, downloading, computing, completed, error).
#'
#' @return Logical. TRUE if successful.
#'
#' @noRd
update_project_status <- function(project_id, status, project_path = NULL) {
  valid_statuses <- c("draft", "downloading", "computing", "completed", "error")

  if (!status %in% valid_statuses) {
    cli::cli_abort("Invalid status: {status}. Must be one of: {paste(valid_statuses, collapse = ', ')}")
  }

  update_project_metadata(project_id, list(
    status = status,
    updated_at = Sys.time()
  ))
}


#' Update project metadata
#'
#' @description
#' Updates specific fields in project metadata.
#'
#' @param project_id Character. Project ID.
#' @param updates List. Fields to update.
#' @param project_path Character. Optional project path (for async mode).
#'
#' @return Logical. TRUE if successful.
#'
#' @noRd
update_project_metadata <- function(project_id, updates, project_path = NULL) {
  if (is.null(project_path)) {
    project_path <- get_project_path(project_id)
  }
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  metadata_path <- file.path(project_path, "metadata.json")

  if (!file.exists(metadata_path)) {
    cli::cli_abort("Metadata file not found")
  }

  tryCatch({
    metadata <- jsonlite::read_json(metadata_path)

    # Update fields
    for (key in names(updates)) {
      metadata[[key]] <- updates[[key]]
    }

    # Always update updated_at
    metadata$updated_at <- Sys.time()

    .write_json_atomic(metadata, metadata_path, auto_unbox = TRUE, pretty = TRUE)
    .invalidate_recent_projects_cache()
    TRUE

  }, error = function(e) {
    cli::cli_abort("Failed to update metadata: {e$message}")
  })
}


#' Persist a foret ancienne (ancient-forest) historical source on a project
#'
#' @description
#' Spec 031. Copies the user-uploaded historical source (classified raster or
#' digitised vector) into the project's \code{data/} directory and records its
#' path + conversion parameters under \code{metadata$foret_ancienne}. Consumed
#' at compute time by \code{build_foret_ancienne_layer()} ->
#' \code{nemeton::build_foret_ancienne_mask()} -> \code{indicateur_n2_continuite()}.
#'
#' @param project_id Character.
#' @param source_path Character. Path to the uploaded (temp) file.
#' @param source_name Character. Original file name (used for the extension /
#'   raster-vs-vector detection).
#' @param forest_class Optional integer vector - raster class value(s) = forest.
#' @param threshold Optional numeric - raster: value >= threshold = forest
#'   (alternative to \code{forest_class}).
#' @param min_area_m2 Numeric. Drop specks smaller than this (default 0).
#'
#' @return TRUE on success.
#' @noRd
set_project_foret_ancienne <- function(project_id, source_path, source_name,
                                       forest_class = NULL, threshold = NULL,
                                       min_area_m2 = 0) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }
  if (is.null(source_path) || !file.exists(source_path)) {
    cli::cli_abort("For\u00eat ancienne source file not found")
  }

  data_dir <- file.path(project_path, "data")
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

  ext  <- tolower(tools::file_ext(source_name %||% source_path))
  kind <- if (ext %in% c("tif", "tiff")) "raster" else "vector"

  # Replace any prior source (possibly a different extension) so no stale file
  # lingers, then copy the upload in under a stable name.
  old <- list.files(data_dir, pattern = "^foret_ancienne_source\\.",
                    full.names = TRUE)
  if (length(old)) file.remove(old[file.exists(old)])
  dest_rel <- file.path("data", paste0("foret_ancienne_source.", ext))
  file.copy(source_path, file.path(project_path, dest_rel), overwrite = TRUE)

  cfg <- list(
    path         = dest_rel,
    kind         = kind,
    forest_class = if (length(forest_class)) as.integer(forest_class) else NULL,
    threshold    = if (!is.null(threshold) && nzchar(as.character(threshold))) as.numeric(threshold) else NULL,
    min_area_m2  = as.numeric(min_area_m2 %||% 0),
    source_name  = source_name,
    set_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )
  update_project_metadata(project_id, list(foret_ancienne = cfg),
                          project_path = project_path)
  cli::cli_alert_success(
    "For\u00eat ancienne : source enregistr\u00e9e ({kind}, {source_name})")
  TRUE
}


#' Remove the foret ancienne source (and its cache) from a project
#'
#' @param project_id Character.
#' @return TRUE if a project directory was found.
#' @noRd
clear_project_foret_ancienne <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) return(FALSE)

  old <- list.files(file.path(project_path, "data"),
                    pattern = "^foret_ancienne_source\\.", full.names = TRUE)
  if (length(old)) file.remove(old[file.exists(old)])

  cache_dir <- file.path(project_path, "cache", "layers", "foret_ancienne")
  if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE)

  # Assigning NULL removes the key from the metadata list.
  update_project_metadata(project_id, list(foret_ancienne = NULL),
                          project_path = project_path)
  TRUE
}


#' Is the SUFOSAT (clear-cut -> T3) source active for a project?
#'
#' @description
#' Single source of truth for the SUFOSAT opt-in state, shared by the settings
#' UI (`mod_sources_config`) and the compute service. **Absent metadata means
#' ENABLED**: the source ships on by default, so a project whose owner never
#' opened the settings tab still gets T3. Only an explicit `enabled = FALSE`
#' (saved from the UI) turns it off.
#'
#' @param metadata List. A project's `metadata` (or `NULL`).
#' @return Logical scalar.
#' @noRd
project_sufosat_enabled <- function(metadata) {
  isTRUE(metadata$sufosat$enabled %||% TRUE)
}


#' Is the urban-cooling (LST -> A5) source active for a project?
#'
#' @description
#' Mirror of [project_sufosat_enabled()] for the Theia LST source - absent
#' metadata means ENABLED. Outside urban / Thermocity coverage the indicator
#' still resolves to `NA` per unit, so defaulting to on costs a rural project
#' nothing beyond one skipped fetch.
#'
#' @param metadata List. A project's `metadata` (or `NULL`).
#' @return Logical scalar.
#' @noRd
project_lst_enabled <- function(metadata) {
  isTRUE(metadata$lst_urbain$enabled %||% TRUE)
}


#' Persist the SUFOSAT (clear-cut -> T3) source config on a project
#'
#' @description
#' Spec 030. Records the opt-in national SUFOSAT source and its parameters
#' under \code{metadata$sufosat}. There is no file to upload (the rasters are
#' fetched from Theia at compute time by \code{build_sufosat_layer()}); this
#' only stores the toggle + \code{window_years} / \code{min_proba}. When
#' disabling, the cached SUFOSAT rasters are dropped so a later re-enable
#' re-fetches a fresh coverage.
#'
#' @param project_id Character.
#' @param enabled Logical. Whether T3 (SUFOSAT) is active for this project.
#' @param window_years Integer. Recency window (default 5).
#' @param min_proba Numeric. Detection probability threshold (default 0.9).
#'
#' @return TRUE on success.
#' @noRd
set_project_sufosat <- function(project_id, enabled,
                                window_years = 5, min_proba = 0.9) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  enabled <- isTRUE(enabled)
  cfg <- list(
    enabled      = enabled,
    window_years = as.integer(window_years %||% 5),
    min_proba    = as.numeric(min_proba %||% 0.9),
    set_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )

  # Drop the cached rasters when disabling (fresh fetch on re-enable).
  if (!enabled) {
    cache_dir <- file.path(project_path, "cache", "layers", "sufosat")
    if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE)
  }

  update_project_metadata(project_id, list(sufosat = cfg),
                          project_path = project_path)
  cli::cli_alert_success(
    "Coupes rases (SUFOSAT) : {if (enabled) 'activ\u00e9' else 'd\u00e9sactiv\u00e9'} \\
     (window={cfg$window_years}, min_proba={cfg$min_proba})")
  TRUE
}


#' Default IFN production parameters (spec 054)
#'
#' @description
#' The core offers two **opt-in** modes fed by the IFN production by
#' sylvoecoregion (SER), estimated by Fay-Herriot:
#'
#'   * `p2_source = "ifn_fh"` - P2 becomes the volume production of the SER
#'     (m3/ha/yr) instead of the CHM site index;
#'   * `e1_mode = "flux"` - E1 follows that production (`production_field =
#'     "P2"`) instead of 2 % of the standing stock.
#'
#' Both default to the historical behaviour (`"chm"` / `"stock"`): nothing moves
#' until the owner opts in. `e1_taux` has **no default share on purpose** - the
#' core refuses to invent one -, so the flux mode stores either a share chosen by
#' the user (`e1_taux_type = "fixe"`) or the IFN observed ratio of the SER
#' (`"ifn_ser"`). The `0.6` below only seeds the slider; it is never sent to the
#' core unless the user saves it.
#'
#' @noRd
PRODUCTION_IFN_DEFAULT <- list(
  p2_source    = "chm",
  e1_mode      = "stock",
  e1_taux_type = "fixe",
  e1_taux      = 0.6
)


#' Read the IFN production parameters of a project
#'
#' @description
#' Single source of truth shared by the settings tab and the compute service.
#' E1's flux mode reads P2 as a **production** (m3/ha/yr): fed with the CHM site
#' index (a height in metres) it would compute nonsense. The flux mode is
#' therefore coerced back to `"stock"` whenever P2 is not in IFN mode.
#'
#' @param metadata List. A project's `metadata` (or `NULL`).
#'
#' @return List with `p2_source`, `e1_mode`, `e1_taux_type`, `e1_taux`, plus
#'   `taux_mobilisation` - the value to hand to the core (`NULL` in stock mode,
#'   a share or `"ifn_ser"` in flux mode).
#'
#' @noRd
project_production_ifn_params <- function(metadata) {
  src <- metadata$production_ifn %||% list()
  d <- PRODUCTION_IFN_DEFAULT

  p2 <- as.character(src$p2_source %||% d$p2_source)[1]
  if (!p2 %in% c("chm", "ifn_fh")) p2 <- d$p2_source

  e1 <- as.character(src$e1_mode %||% d$e1_mode)[1]
  if (!e1 %in% c("stock", "flux") || p2 != "ifn_fh") e1 <- "stock"

  type <- as.character(src$e1_taux_type %||% d$e1_taux_type)[1]
  if (!type %in% c("fixe", "ifn_ser")) type <- d$e1_taux_type

  taux <- suppressWarnings(as.numeric(src$e1_taux %||% d$e1_taux))[1]
  if (is.na(taux)) taux <- d$e1_taux
  taux <- min(1, max(0, taux))

  list(
    p2_source    = p2,
    e1_mode      = e1,
    e1_taux_type = type,
    e1_taux      = taux,
    taux_mobilisation = if (e1 == "flux") {
      if (type == "ifn_ser") "ifn_ser" else taux
    }
  )
}


#' Persist the IFN production parameters on a project
#'
#' @description
#' Mirrors [set_project_accessibility_params()]. No cache to drop: the compute
#' service recognises a P2 / E1 computed under another mode and recomputes it
#' (see `.production_mode_stale()`).
#'
#' @param project_id Character.
#' @param p2_source `"chm"` or `"ifn_fh"`.
#' @param e1_mode `"stock"` or `"flux"`.
#' @param e1_taux_type `"fixe"` or `"ifn_ser"`.
#' @param e1_taux Numeric share in `[0, 1]`, used when `e1_taux_type = "fixe"`.
#'
#' @return Invisible `TRUE`.
#'
#' @noRd
set_project_production_ifn <- function(project_id, p2_source = "chm",
                                       e1_mode = "stock",
                                       e1_taux_type = "fixe",
                                       e1_taux = NULL) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  cfg <- project_production_ifn_params(list(production_ifn = list(
    p2_source = p2_source, e1_mode = e1_mode,
    e1_taux_type = e1_taux_type, e1_taux = e1_taux)))
  cfg$taux_mobilisation <- NULL
  cfg$set_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  update_project_metadata(project_id, list(production_ifn = cfg),
                          project_path = project_path)
  cli::cli_alert_success(
    "Production IFN : P2 = {cfg$p2_source}, E1 = {cfg$e1_mode}")
  invisible(TRUE)
}


#' Default FAST detection parameters
#'
#' @description
#' Absolute thresholds and rolling window of the FAST health screening
#' (spec 013). Kept in one place so the sidebar that used to hold them, the
#' settings tab that holds them now, and the readers agree on the same values.
#'
#' The defaults are the core's: a healthy forest NDVI is typically 0.6-0.8 and a
#' healthy NBR 0.4-0.6, so a pixel alerts below 0.40 / 0.30. NDMI runs lower
#' under canopy, hence 0.20.
#'
#' @noRd
FAST_PARAMS_DEFAULT <- list(
  threshold_ndvi = 0.40,
  threshold_nbr  = 0.30,
  threshold_ndmi = 0.20,
  window_days    = 30L
)


#' Read the FAST detection parameters of a project
#'
#' @description
#' These four values were sliders in the Suivi sanitaire sidebar. They are
#' **calibrations**, set once per massif rather than adjusted at each run, so
#' they now live in the settings modal and persist per project - while the
#' observation period, which *is* the recurring gesture of a diagnosis, stayed
#' in the sidebar.
#'
#' Absent metadata yields the defaults: a project that never opened the settings
#' behaves exactly as before.
#'
#' @param metadata List. A project's `metadata` (or `NULL`).
#'
#' @return List with `threshold_ndvi`, `threshold_nbr`, `threshold_ndmi`,
#'   `window_days`.
#'
#' @noRd
project_fast_params <- function(metadata) {
  cfg <- metadata$fast_params
  num <- function(x, default) {
    v <- suppressWarnings(as.numeric(x %||% default))
    if (length(v) != 1L || is.na(v)) default else v
  }

  list(
    threshold_ndvi = num(cfg$threshold_ndvi, FAST_PARAMS_DEFAULT$threshold_ndvi),
    threshold_nbr  = num(cfg$threshold_nbr,  FAST_PARAMS_DEFAULT$threshold_nbr),
    threshold_ndmi = num(cfg$threshold_ndmi, FAST_PARAMS_DEFAULT$threshold_ndmi),
    window_days    = as.integer(round(
      num(cfg$window_days, FAST_PARAMS_DEFAULT$window_days)))
  )
}


#' Persist the FAST detection parameters on a project
#'
#' @description
#' Mirrors [set_project_sufosat()]. Unlike the two optional-source toggles, no
#' cache is dropped here: the thresholds are applied when *reading* the alert
#' raster, not when downloading it, so a re-calibration costs nothing to redo.
#'
#' @param project_id Character.
#' @param threshold_ndvi,threshold_nbr,threshold_ndmi Numeric. Absolute
#'   thresholds below which a pixel alerts.
#' @param window_days Integer. Rolling window length in days.
#'
#' @return Invisible `TRUE`.
#'
#' @noRd
set_project_fast_params <- function(project_id,
                                    threshold_ndvi = NULL,
                                    threshold_nbr = NULL,
                                    threshold_ndmi = NULL,
                                    window_days = NULL) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  # On passe par le lecteur pour que toute valeur absente ou aberrante retombe
  # sur le defaut plutot que d'ecrire un NA dans les metadonnees.
  cfg <- project_fast_params(list(fast_params = list(
    threshold_ndvi = threshold_ndvi,
    threshold_nbr  = threshold_nbr,
    threshold_ndmi = threshold_ndmi,
    window_days    = window_days
  )))
  cfg$set_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  update_project_metadata(project_id, list(fast_params = cfg),
                          project_path = project_path)
  cli::cli_alert_success(
    "Suivi sanitaire : seuils NDVI={cfg$threshold_ndvi} / NBR={cfg$threshold_nbr} \\
     / NDMI={cfg$threshold_ndmi}, fen\u00eatre {cfg$window_days} j")
  invisible(TRUE)
}


#' Default FORDEAD detection parameter
#'
#' @description
#' Anomaly threshold of the FORDEAD diagnosis (CRSWIR), spec 008. Default 0.16
#' is the ONF/DSF 2024 calibration. Kept next to [FAST_PARAMS_DEFAULT] so the
#' settings tab that now holds the slider and the run that consumes it agree on
#' one value.
#'
#' @noRd
FORDEAD_PARAMS_DEFAULT <- list(threshold_anomaly = 0.16)


#' Read the FORDEAD detection parameter of a project
#'
#' @description
#' Like the FAST thresholds, the anomaly threshold is a **calibration** - set
#' once per massif, not adjusted at each run - so it moved from the Suivi
#' sanitaire sidebar to the settings modal. The storage key is unchanged
#' (`metadata$monitoring_threshold_anomaly`), so projects calibrated before the
#' move keep their value.
#'
#' @param metadata List. A project's `metadata` (or `NULL`).
#'
#' @return List with `threshold_anomaly`.
#'
#' @noRd
project_fordead_params <- function(metadata) {
  v <- suppressWarnings(as.numeric(
    metadata$monitoring_threshold_anomaly %||%
      FORDEAD_PARAMS_DEFAULT$threshold_anomaly))
  if (length(v) != 1L || is.na(v)) {
    v <- FORDEAD_PARAMS_DEFAULT$threshold_anomaly
  }
  list(threshold_anomaly = v)
}


#' Persist the FORDEAD detection parameter on a project
#'
#' @description
#' Mirrors [set_project_fast_params()]. Nothing is invalidated: the threshold is
#' applied by the *next* FORDEAD run, so a re-calibration costs only that run.
#'
#' @param project_id Character.
#' @param threshold_anomaly Numeric. CRSWIR deviation above the modelled value
#'   from which a pixel is flagged as an anomaly.
#'
#' @return Invisible `TRUE`.
#'
#' @noRd
set_project_fordead_params <- function(project_id, threshold_anomaly = NULL) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  # Passage par le lecteur : une valeur absente ou aberrante retombe sur le
  # defaut plutot que d'ecrire un NA dans les metadonnees.
  cfg <- project_fordead_params(
    list(monitoring_threshold_anomaly = threshold_anomaly))

  update_project_metadata(
    project_id,
    list(monitoring_threshold_anomaly = cfg$threshold_anomaly),
    project_path = project_path)
  cli::cli_alert_success(
    "Suivi sanitaire : seuil d'anomalie CRSWIR={cfg$threshold_anomaly}")
  invisible(TRUE)
}


#' Default accessibility extent parameter
#'
#' @description
#' Buffer (metres) added around the forest AOI before acquiring the DEM and the
#' road network: access comes from the roads *outside* the stand. Default 250 m,
#' the value the sidebar numeric input carried before the move.
#'
#' @noRd
ACCESSIBILITY_PARAMS_DEFAULT <- list(buffer_m = 250)


#' Read the accessibility extent parameter of a project
#'
#' @description
#' The buffer sizes the acquired extent, so it is a per-massif calibration, not
#' a per-run gesture: it lives in the settings modal and persists with the
#' project. Absent metadata yields the default - a project that never opened the
#' settings behaves exactly as before.
#'
#' @param metadata List. A project's `metadata` (or `NULL`).
#'
#' @return List with `buffer_m`.
#'
#' @noRd
project_accessibility_params <- function(metadata) {
  v <- suppressWarnings(as.numeric(
    metadata$accessibility_params$buffer_m %||%
      ACCESSIBILITY_PARAMS_DEFAULT$buffer_m))
  if (length(v) != 1L || is.na(v)) {
    v <- ACCESSIBILITY_PARAMS_DEFAULT$buffer_m
  }
  list(buffer_m = max(0, v))
}


#' Persist the accessibility extent parameter on a project
#'
#' @description
#' Mirrors [set_project_fast_params()]. No cache is dropped: the accessibility
#' service keys its cache on the buffered extent, so a widened buffer simply
#' produces a new entry at the next run.
#'
#' @param project_id Character.
#' @param buffer_m Numeric. Buffer around the forest, in metres.
#'
#' @return Invisible `TRUE`.
#'
#' @noRd
set_project_accessibility_params <- function(project_id, buffer_m = NULL) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  cfg <- project_accessibility_params(
    list(accessibility_params = list(buffer_m = buffer_m)))
  cfg$set_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  update_project_metadata(project_id, list(accessibility_params = cfg),
                          project_path = project_path)
  cli::cli_alert_success(
    "Accessibilit\u00e9 : zone tampon {cfg$buffer_m} m")
  invisible(TRUE)
}


#' Default ONF crossing parameters
#'
#' @description
#' The settings that shape the ONF crossing. They are calibrations rather
#' than gestures - set once per massif, not at each attempt - hence their place
#' in *Sources & parametres* and their persistence per project
#' (`metadata$onf_params`).
#'
#' Since the 2026-10-08 brief (`onf-nouveau-chemin-seul`) the crossing goes
#' through `nemeton::construire_ugf_onf()` only, and the settings are its own:
#'
#' * `domanialite`: which ONF parcels are fetched (filter of the WFS call);
#' * `purger`: `TRUE` keeps only the parcels under the *regime forestier*
#'   (`selection = "foret"`: public owner in the DGFiP file AND covered at
#'   `seuil_couverture` by the warped ONF), `FALSE` keeps the whole selection
#'   (`selection = "toutes"`);
#' * `seuil_couverture` (share, 0.1-1): minimum ONF cover of a parcel;
#' * `clip_cadastre`: display only - cuts the orange preview and the raw
#'   parcellaire export to the cadastral parcels. The core always receives the
#'   raw layer, whose overflow the warping needs;
#' * advanced: `tol` (m), `larg_hors` (m), `seuil` (ha), `seuil_hors` (ha).
#'
#' The forest-share threshold of the former chain is gone: an old
#' `metadata.json` that still holds it is read without error and the key is
#' ignored.
#'
#' @noRd
ONF_PARAMS_DEFAULT <- list(
  domanialite      = c("domaniale", "autre"),
  purger           = TRUE,
  seuil_couverture = 0.5,
  clip_cadastre    = TRUE,
  tol              = 15,
  larg_hors        = 50,
  seuil            = 0.5,
  seuil_hors       = 1
)

#' Bounds of the numeric ONF parameters
#'
#' A value outside them, or `NA`, falls back on the default - on reading as on
#' writing.
#' @noRd
ONF_PARAMS_BORNES <- list(
  seuil_couverture = c(0.1, 1),
  tol              = c(0, 50),
  larg_hors        = c(10, 200),
  seuil            = c(0, 5),
  seuil_hors       = c(0, 10)
)

#' Advanced ONF parameters (folded in the settings panel)
#' @noRd
ONF_PARAMS_AVANCES <- c("tol", "larg_hors", "seuil", "seuil_hors")

#' Read one bounded numeric ONF parameter
#' @noRd
.onf_param_num <- function(src, cle) {
  v <- suppressWarnings(as.numeric(unlist(src[[cle]] %||% NA)))
  b <- ONF_PARAMS_BORNES[[cle]]
  if (length(v) != 1L || is.na(v) || v < b[1] || v > b[2]) {
    return(ONF_PARAMS_DEFAULT[[cle]])
  }
  v
}

#' Read the ONF crossing parameters of a project
#'
#' @param metadata Project metadata list.
#' @return List with `domanialite`, `purger`, `seuil_couverture`,
#'   `clip_cadastre`, `tol`, `larg_hors`, `seuil`, `seuil_hors`.
#' @noRd
project_onf_params <- function(metadata) {
  src <- metadata$onf_params %||% list()

  dom <- src$domanialite %||% ONF_PARAMS_DEFAULT$domanialite
  dom <- as.character(unlist(dom, use.names = FALSE))
  dom <- dom[dom %in% c("domaniale", "autre")]
  # Aucune domanialite cochee n'est un etat que le croisement refuse : plutot
  # que de le persister, on retombe sur le defaut.
  if (length(dom) == 0L) dom <- ONF_PARAMS_DEFAULT$domanialite

  list(
    domanialite      = dom,
    purger           = isTRUE(src$purger %||% ONF_PARAMS_DEFAULT$purger),
    seuil_couverture = .onf_param_num(src, "seuil_couverture"),
    clip_cadastre    = isTRUE(src$clip_cadastre %||% ONF_PARAMS_DEFAULT$clip_cadastre),
    tol              = .onf_param_num(src, "tol"),
    larg_hors        = .onf_param_num(src, "larg_hors"),
    seuil            = .onf_param_num(src, "seuil"),
    seuil_hors       = .onf_param_num(src, "seuil_hors")
  )
}

#' Do the advanced ONF parameters differ from their defaults?
#'
#' @param cfg Output of [project_onf_params()].
#' @return Logical scalar.
#' @noRd
onf_params_avances_modifies <- function(cfg) {
  any(vapply(ONF_PARAMS_AVANCES, function(k) {
    !isTRUE(all.equal(as.numeric(cfg[[k]]), ONF_PARAMS_DEFAULT[[k]]))
  }, logical(1)))
}

#' Persist the ONF crossing parameters on a project
#'
#' @param project_id Character.
#' @param domanialite Character vector, `"domaniale"` and/or `"autre"`.
#' @param purger Logical. Keep only the parcels under the regime forestier.
#' @param seuil_couverture Numeric 0.1..1. Minimum ONF cover of a parcel.
#' @param clip_cadastre Logical. Cut the ONF preview to the cadastre.
#' @param tol,larg_hors,seuil,seuil_hors Advanced parameters of
#'   `nemeton::construire_ugf_onf()`.
#' @return Invisible `TRUE`.
#' @noRd
set_project_onf_params <- function(project_id, domanialite = NULL,
                                   purger = NULL, seuil_couverture = NULL,
                                   clip_cadastre = NULL, tol = NULL,
                                   larg_hors = NULL, seuil = NULL,
                                   seuil_hors = NULL) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }
  cfg <- project_onf_params(list(onf_params = list(
    domanialite = domanialite, purger = purger,
    seuil_couverture = seuil_couverture, clip_cadastre = clip_cadastre,
    tol = tol, larg_hors = larg_hors, seuil = seuil, seuil_hors = seuil_hors)))
  cfg$set_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  update_project_metadata(project_id, list(onf_params = cfg),
                          project_path = project_path)
  cli::cli_alert_success(
    "ONF : domanialite {paste(cfg$domanialite, collapse = '+')}, couverture {round(cfg$seuil_couverture * 100)} %")
  invisible(TRUE)
}


#' Default road-network (desserte) parameters
#'
#' @description
#' The five values that size a desserte computation and price it: the buffer
#' widening the acquired extent (km), the skidding distance beyond which a
#' parcel is deemed unserved (m), the maximum buildable slope (%), the slope
#' pricing method and the platform width it uses. Defaults mirror the sidebar
#' values they replace - see `DESSERTE_SKIDDING_DEFAULT_M`,
#' `DESSERTE_PENTE_MAX_DEFAULT_PCT` and `DESSERTE_LARGEUR_DEFAULT_M` in
#' `service_desserte.R` - except `methode_pente`, now `"terrassement"`.
#'
#' @noRd
DESSERTE_PARAMS_DEFAULT <- list(
  buffer_km     = 1,
  skidding_m    = DESSERTE_SKIDDING_DEFAULT_M,
  pente_max_pct = DESSERTE_PENTE_MAX_DEFAULT_PCT,
  # Tarification de la pente : le TERRASSEMENT est desormais le defaut. Il
  # chiffre un volume de deblai/remblai plutot qu'un bareme de classes, donc il
  # rend compte de la largeur de plateforme - que le bareme ignore.
  methode_pente = "terrassement",
  largeur_m     = DESSERTE_LARGEUR_DEFAULT_M
)


#' Read the desserte parameters of a project
#'
#' @description
#' Like the FAST thresholds and the accessibility buffer, these are
#' **calibrations** of a massif - the acquired extent, the machine reach, the
#' slope one accepts to build on, how a slope is priced - not per-run gestures,
#' so they live in the settings modal and persist with the project. Only the
#' engine (glouton / Steiner) stayed in the sidebar: it is the choice one varies
#' from run to run.
#'
#' @param metadata List. A project's `metadata` (or `NULL`).
#'
#' @return List with `buffer_km`, `skidding_m`, `pente_max_pct`,
#'   `methode_pente`, `largeur_m`.
#'
#' @noRd
project_desserte_params <- function(metadata) {
  cfg <- metadata$desserte_params
  num <- function(x, default) {
    v <- suppressWarnings(as.numeric(x %||% default))
    if (length(v) != 1L || is.na(v) || v < 0) default else v
  }
  mp <- as.character(cfg$methode_pente %||%
                       DESSERTE_PARAMS_DEFAULT$methode_pente)[1]
  if (!mp %in% c("bareme", "terrassement")) {
    mp <- DESSERTE_PARAMS_DEFAULT$methode_pente
  }

  list(
    buffer_km     = num(cfg$buffer_km, DESSERTE_PARAMS_DEFAULT$buffer_km),
    skidding_m    = num(cfg$skidding_m, DESSERTE_PARAMS_DEFAULT$skidding_m),
    pente_max_pct = num(cfg$pente_max_pct,
                        DESSERTE_PARAMS_DEFAULT$pente_max_pct),
    methode_pente = mp,
    largeur_m     = num(cfg$largeur_m, DESSERTE_PARAMS_DEFAULT$largeur_m)
  )
}


#' Persist the desserte parameters on a project
#'
#' @description
#' Mirrors [set_project_fast_params()]. No cache is dropped: a desserte result
#' is cached under a key that already carries these values, so changing them
#' simply misses the cache and recomputes.
#'
#' @param project_id Character.
#' @param buffer_km Numeric. Buffer around the parcels, in kilometres.
#' @param skidding_m Numeric. Skidding distance, in metres.
#' @param pente_max_pct Numeric. Maximum buildable slope, in percent.
#' @param methode_pente Character. `"bareme"` or `"terrassement"`.
#' @param largeur_m Numeric. Platform width, in metres (terrassement only).
#'
#' @return Invisible `TRUE`.
#'
#' @noRd
set_project_desserte_params <- function(project_id,
                                        buffer_km = NULL,
                                        skidding_m = NULL,
                                        pente_max_pct = NULL,
                                        methode_pente = NULL,
                                        largeur_m = NULL) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  cfg <- project_desserte_params(list(desserte_params = list(
    buffer_km     = buffer_km,
    skidding_m    = skidding_m,
    pente_max_pct = pente_max_pct,
    methode_pente = methode_pente,
    largeur_m     = largeur_m
  )))
  cfg$set_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  update_project_metadata(project_id, list(desserte_params = cfg),
                          project_path = project_path)
  cli::cli_alert_success(
    "Desserte : tampon {cfg$buffer_km} km / d\u00e9bardage {cfg$skidding_m} m \\
     / pente max {cfg$pente_max_pct} %")
  invisible(TRUE)
}


#' Default reGeneration engine parameters
#'
#' @description
#' Phenology (budburst / leaf fall), the two expert overrides (`lai_max`,
#' `ewm`), the SoilGrids integration depth, the meteorological forcing and the
#' microclimate resolution. `lai_max` and `ewm` default to `NA`, which is not a
#' missing value here but a **meaning**: derive it from the data (LiDAR PAI /
#' SoilGrids) rather than force it.
#'
#' @noRd
REGEN_PARAMS_DEFAULT <- list(
  budburst         = 105,
  leaf_fall        = 300,
  lai_max          = NA_real_,
  ewm              = NA_real_,
  rooting_depth_cm = 100,
  forcing          = "safran",
  resolution       = "2"
)


#' Read the reGeneration engine parameters of a project
#'
#' @description
#' Same reasoning as the FAST thresholds: phenology, expert overrides, forcing
#' and resolution are set once for a massif and then left alone, so they live in
#' the settings modal rather than in the sidebar where they invited reflex
#' filling - typing a `lai_max` silently cancels a PAI computed in 57 minutes.
#'
#' The two overrides keep their `NA` = "derive it" semantics: an absent entry
#' and an explicitly emptied field are the same thing.
#'
#' @param metadata List. A project's `metadata` (or `NULL`).
#'
#' @return List with `budburst`, `leaf_fall`, `lai_max`, `ewm`,
#'   `rooting_depth_cm`, `forcing`, `resolution`.
#'
#' @noRd
project_regen_params <- function(metadata) {
  cfg <- metadata$regen_params
  num <- function(x, default) {
    v <- suppressWarnings(as.numeric(x %||% default))
    if (length(v) != 1L) default else v
  }
  # Un override absent OU vide vaut " derive " : on ne retombe pas sur un
  # scalaire par defaut, on retourne NA.
  ovr <- function(x) {
    v <- suppressWarnings(as.numeric(x %||% NA_real_))
    if (length(v) != 1L || is.na(v)) NA_real_ else v
  }
  enum <- function(x, allowed, default) {
    v <- as.character(x %||% default)[1]
    if (is.na(v) || !v %in% allowed) default else v
  }

  list(
    budburst         = num(cfg$budburst, REGEN_PARAMS_DEFAULT$budburst),
    leaf_fall        = num(cfg$leaf_fall, REGEN_PARAMS_DEFAULT$leaf_fall),
    lai_max          = ovr(cfg$lai_max),
    ewm              = ovr(cfg$ewm),
    rooting_depth_cm = num(cfg$rooting_depth_cm,
                           REGEN_PARAMS_DEFAULT$rooting_depth_cm),
    forcing          = enum(cfg$forcing, c("safran", "era5"),
                            REGEN_PARAMS_DEFAULT$forcing),
    resolution       = enum(cfg$resolution, c("2", "5"),
                            REGEN_PARAMS_DEFAULT$resolution)
  )
}


#' Persist the reGeneration engine parameters on a project
#'
#' @description
#' Mirrors [set_project_fast_params()]. The cached engine outputs are NOT
#' dropped: they are keyed by their own run, and a re-calibration is applied by
#' the next run.
#'
#' @param project_id Character.
#' @param budburst,leaf_fall Numeric. Day of year.
#' @param lai_max,ewm Numeric or `NA`. `NA` means "derive from the data".
#' @param rooting_depth_cm Numeric. SoilGrids integration depth, in cm.
#' @param forcing Character. `"safran"` or `"era5"`.
#' @param resolution Character. `"2"` or `"5"` (metres).
#'
#' @return Invisible `TRUE`.
#'
#' @noRd
set_project_regen_params <- function(project_id,
                                     budburst = NULL,
                                     leaf_fall = NULL,
                                     lai_max = NULL,
                                     ewm = NULL,
                                     rooting_depth_cm = NULL,
                                     forcing = NULL,
                                     resolution = NULL) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  cfg <- project_regen_params(list(regen_params = list(
    budburst         = budburst,
    leaf_fall        = leaf_fall,
    lai_max          = lai_max,
    ewm              = ewm,
    rooting_depth_cm = rooting_depth_cm,
    forcing          = forcing,
    resolution       = resolution
  )))
  cfg$set_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  update_project_metadata(project_id, list(regen_params = cfg),
                          project_path = project_path)
  cli::cli_alert_success(
    "reG\u00e9n\u00e9ration : d\u00e9bourrement {cfg$budburst} / chute \\
     {cfg$leaf_fall}, for\u00e7age {cfg$forcing}, r\u00e9solution \\
     {cfg$resolution} m")
  invisible(TRUE)
}


#' Persist the urban-cooling (LST -> A5) source config on a project
#'
#' @description
#' Spec 032. Records the opt-in Theia LST (Thermocity) source under
#' \code{metadata$lst_urbain}, mirroring \code{set_project_sufosat()}. No file to
#' upload - the LST raster is fetched from Theia at compute time by
#' \code{build_lst_layer()}; this only stores the toggle + \code{buffer_m} (the
#' reference-ring radius). When disabling, the cached LST raster is dropped so a
#' later re-enable re-fetches a fresh coverage.
#'
#' @param project_id Character.
#' @param enabled Logical. Whether A5 (urban cooling / LST) is active.
#' @param buffer_m Numeric. Reference-ring radius in metres (default 500).
#'
#' @return TRUE on success.
#' @noRd
set_project_lst_urbain <- function(project_id, enabled, buffer_m = 500) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path) || !dir.exists(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  enabled <- isTRUE(enabled)
  cfg <- list(
    enabled  = enabled,
    buffer_m = as.numeric(buffer_m %||% 500),
    set_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  )

  # Drop the cached LST raster when disabling (fresh fetch on re-enable).
  if (!enabled) {
    cache_dir <- file.path(project_path, "cache", "layers", "lst")
    if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE)
  }

  update_project_metadata(project_id, list(lst_urbain = cfg),
                          project_path = project_path)
  cli::cli_alert_success(
    "Rafra\u00eechissement urbain (LST) : {if (enabled) 'activ\u00e9' else 'd\u00e9sactiv\u00e9'} \\
     (buffer={cfg$buffer_m}m)")
  TRUE
}


#' Load project metadata
#'
#' @description
#' Loads metadata for a project.
#'
#' @param project_id Character. Project ID.
#'
#' @return List with metadata, or NULL if not found.
#'
#' @noRd
load_project_metadata <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  metadata_path <- file.path(project_path, "metadata.json")

  if (!file.exists(metadata_path)) {
    return(NULL)
  }

  tryCatch({
    meta <- jsonlite::read_json(metadata_path)

    meta
  }, error = function(e) {
    cli::cli_warn("Failed to load metadata: {e$message}")
    NULL
  })
}


#' Get project path
#'
#' @description
#' Returns the full path to a project directory.
#'
#' @param project_id Character. Project ID.
#'
#' @return Character path, or NULL if not found.
#'
#' @noRd
get_project_path <- function(project_id) {
  if (!.is_safe_project_id(project_id)) {
    return(NULL)
  }

  root <- get_projects_root()
  project_path <- file.path(root, project_id)

  if (!dir.exists(project_path)) {
    return(NULL)
  }
  # Le dossier resolu doit rester SOUS la racine : un lien symbolique place
  # dans la racine ne doit pas faire sortir une suppression recursive.
  reel <- normalizePath(project_path, winslash = "/", mustWork = FALSE)
  racine <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (!startsWith(reel, paste0(racine, "/"))) {
    return(NULL)
  }

  project_path
}


#' Is this a valid project identifier?
#'
#' @description
#' A project id names ONE directory under the projects root. It arrives from
#' the browser (`input$load_project`, `input$delete_corrupted`), so it is
#' untrusted: an id made of relative path segments would designate a directory
#' outside the root, which `delete_project()` then removes recursively. Only a
#' single plain path segment is accepted - no separator, no `.`/`..`, no
#' control character.
#'
#' @param project_id Candidate id.
#' @return Logical scalar.
#' @noRd
.is_safe_project_id <- function(project_id) {
  is.character(project_id) && length(project_id) == 1L &&
    !is.na(project_id) && nzchar(project_id) &&
    nchar(project_id) <= 200L &&
    !project_id %in% c(".", "..") &&
    !grepl("[/\\[:cntrl:]]", project_id) &&
    identical(basename(project_id), project_id)
}


#' Save external data cache
#'
#' @description
#' Saves downloaded external data (BD Foret, etc.) to project cache.
#' Part of preventive cache strategy.
#'
#' @param project_id Character. Project ID.
#' @param data_name Character. Name of the data (e.g., "bdforet", "corine").
#' @param data sf or data.frame. The data to cache.
#'
#' @return Logical. TRUE if successful.
#'
#' @noRd
save_cache_data <- function(project_id, data_name, data) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  cache_path <- file.path(project_path, "cache")
  if (!dir.exists(cache_path)) {
    dir.create(cache_path, recursive = TRUE)
  }

  file_path <- file.path(cache_path, paste0(data_name, ".parquet"))

  tryCatch({
    if (!requireNamespace("arrow", quietly = TRUE)) {
      cli::cli_abort("Package 'arrow' is required")
    }

    if (inherits(data, "sf")) {
      # Convert sf to data.frame with WKT
      data_df <- data
      data_df$geometry_wkt <- sf::st_as_text(sf::st_geometry(data))
      data_df <- sf::st_drop_geometry(data_df)

      # Save CRS
      crs_path <- file.path(cache_path, paste0(data_name, "_crs.json"))
      jsonlite::write_json(
        list(epsg = sf::st_crs(data)$epsg),
        crs_path,
        auto_unbox = TRUE
      )

      arrow::write_parquet(data_df, file_path)
    } else {
      arrow::write_parquet(data, file_path)
    }

    TRUE

  }, error = function(e) {
    cli::cli_warn("Failed to cache {data_name}: {e$message}")
    FALSE
  })
}


#' Load cached external data
#'
#' @description
#' Loads cached external data from project.
#'
#' @param project_id Character. Project ID.
#' @param data_name Character. Name of the data.
#'
#' @return Data (sf or data.frame), or NULL if not found.
#'
#' @noRd
load_cache_data <- function(project_id, data_name) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  file_path <- file.path(project_path, "cache", paste0(data_name, ".parquet"))

  if (!file.exists(file_path)) {
    return(NULL)
  }

  tryCatch({
    data_df <- arrow::read_parquet(file_path)

    # Check if it's spatial data
    if ("geometry_wkt" %in% names(data_df)) {
      crs_path <- file.path(project_path, "cache", paste0(data_name, "_crs.json"))
      crs <- 4326
      if (file.exists(crs_path)) {
        crs_info <- jsonlite::read_json(crs_path)
        if (!is.null(crs_info$epsg)) {
          crs <- crs_info$epsg
        }
      }

      data_sf <- sf::st_as_sf(data_df, wkt = "geometry_wkt", crs = crs)
      data_sf$geometry_wkt <- NULL
      return(data_sf)
    }

    data_df

  }, error = function(e) {
    cli::cli_warn("Failed to load cached {data_name}: {e$message}")
    NULL
  })
}


# ==============================================================================
# UG Data Persistence
# ==============================================================================

#' Save UG data (tenements + UG definitions) to project
#'
#' @description
#' Saves tenement geometries as GeoPackage and UG definitions as JSON.
#'
#' @param project_id Character. Project ID.
#' @param projet List. Project with $tenements (sf) and $ugs (data.frame).
#'
#' @return Logical. TRUE if successful.
#' @noRd
save_ug_data <- function(project_id, projet) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    cli::cli_abort("Project not found: {project_id}")
  }

  tenements <- projet$tenements
  ugs <- projet$ugs

  if (is.null(tenements) || is.null(ugs)) {
    cli::cli_warn("No UG data to save")
    return(FALSE)
  }

  data_dir <- file.path(project_path, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  tryCatch({
    # Save tenements as GeoPackage
    tenements_path <- file.path(data_dir, "tenements.gpkg")
    # Affectation tenement -> UGF AVANT ecriture : si elle change (creation,
    # fusion, decoupe, deplacement), les indicateurs calcules portent sur des
    # `ug_id` qui n'existent plus, et `compute_all_indicators()` les croirait
    # faits. Un renommage ou un changement de groupe ne la change pas.
    avant <- .affectation_ug(tenements_path)
    .st_write_atomic(tenements, tenements_path)

    # Save UG definitions as JSON.
    # NB: We use `dataframe = "columns"` (column-oriented object) instead of
    # the default row-oriented array. When a column is entirely NA (e.g.
    # `groupe` for fresh UGFs), the row-oriented serialisation emits
    # `"groupe": null` on every row, which `read_json(simplifyVector = TRUE)`
    # elides when reconstructing the data.frame - dropping the column
    # entirely. `load_ug_data()` would then see "UGs file missing required
    # columns" and `ensure_project_ug()` would silently wipe the
    # imported UGF layout with the default 1-UGF-per-parcel migration.
    # Column-oriented output keeps every key present regardless of NA
    # density.
    #
    # Colonnes ONF (brief 2026-10-08) : ecrites seulement quand elles portent
    # une valeur. Un projet sans parcellaire ONF garde un `ugs.json` sans ces
    # champs plutot que cinq tableaux de `null` ; la lecture les rajoute a NA.
    ugs_path <- file.path(data_dir, "ugs.json")
    ugs <- .ug_onf_normaliser(ugs)
    vides <- UG_ONF_COLS[vapply(UG_ONF_COLS, function(col) all(is.na(ugs[[col]])),
                                logical(1))]
    ugs <- ugs[, setdiff(names(ugs), vides), drop = FALSE]
    .write_json_atomic(ugs, ugs_path, auto_unbox = TRUE, pretty = TRUE,
                       dataframe = "columns")

    # Update metadata
    update_project_metadata(project_id, list(
      ug_count = nrow(ugs),
      updated_at = Sys.time()
    ))

    cli::cli_alert_success("Saved {nrow(ugs)} UGs with {nrow(tenements)} tenements")
    apres <- .affectation_ug_df(tenements)
    change <- !is.null(avant) && !identical(avant, apres)
    if (change) invalidate_indicators(project_id, motif = "ugf")
    structure(TRUE, indicateurs_invalides = change)

  }, error = function(e) {
    cli::cli_abort("Failed to save UG data: {e$message}")
  })
}


#' Load UG data from project
#'
#' @description
#' Loads tenement geometries and UG definitions from project files.
#'
#' @param project_id Character. Project ID.
#'
#' @return List with $tenements (sf) and $ugs (data.frame), or NULL if not found.
#' @noRd
load_ug_data <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  data_dir <- file.path(project_path, "data")
  gpkg_path <- file.path(data_dir, "tenements.gpkg")
  ugs_path <- file.path(data_dir, "ugs.json")
  if (!file.exists(gpkg_path)) {
    return(NULL)
  }

  if (!file.exists(ugs_path)) {
    return(NULL)
  }

  tryCatch({
    tenements <- sf::st_read(gpkg_path, quiet = TRUE)
    ugs <- jsonlite::read_json(ugs_path, simplifyVector = TRUE)

    # Ensure ugs is a proper data.frame
    if (!is.data.frame(ugs)) {
      ugs <- as.data.frame(ugs, stringsAsFactors = FALSE)
    }

    # Ensure required columns exist
    required_tenement_cols <- c("tenement_id", "parent_parcelle_id", "ug_id", "surface_m2")
    required_ug_cols <- c("ug_id", "label", "groupe")

    if (!all(required_tenement_cols %in% names(tenements))) {
      cli::cli_warn("Tenements file missing required columns")
      return(NULL)
    }

    # Surface SIG absente (decoupage importe d'un SIG tiers) : recalculee
    # depuis la geometrie.
    if (!"surface_sig_m2" %in% names(tenements)) {
      tenements$surface_sig_m2 <- as.numeric(sf::st_area(tenements))
    }

    # Normalise list-columns produced by jsonlite when a field was all
    # `null` in a row-oriented JSON (legacy save format): convert them
    # back to proper character vectors with NA_character_ for empty
    # entries so downstream column checks work.
    for (col in intersect(required_ug_cols, names(ugs))) {
      if (is.list(ugs[[col]])) {
        ugs[[col]] <- vapply(ugs[[col]], function(x) {
          if (is.null(x) || length(x) == 0L) NA_character_ else as.character(x[[1L]])
        }, character(1))
      }
    }

    # Without ug_id there is no way to link tenements to UGs -> genuinely
    # broken file, skip.
    if (!"ug_id" %in% names(ugs)) {
      cli::cli_warn("UGs file missing ug_id column")
      return(NULL)
    }

    # `label` and `groupe` can legitimately be all-NA (fresh UGFs have
    # no management group). Older jsonlite round-trips (row-oriented +
    # `null`) dropped these columns entirely - rather than re-triggering
    # the default migration (which silently wipes the imported layout),
    # backfill them with NA so the UGF layout on disk is preserved.
    for (col in setdiff(required_ug_cols, c("ug_id", names(ugs)))) {
      cli::cli_alert_info("Backfilling missing {.val {col}} column in ugs.json")
      ugs[[col]] <- NA_character_
    }

    # Colonnes ONF facultatives : absentes d'un fichier anterieur ou d'un projet
    # sans parcellaire ONF, elles reviennent a NA.
    ugs <- .ug_onf_normaliser(ugs)

    list(tenements = tenements, ugs = ugs)

  }, error = function(e) {
    cli::cli_warn("Failed to load UG data: {e$message}")
    NULL
  })
}


# NB: save_indicators_ug() / load_indicators_ug() were removed along
# with the obsolete "Recalculer les indicateurs" button. Indicators
# are now computed directly on the UGF geometries in one pass and
# stored in indicators.parquet - there is no separate UGF-level file
# to persist or reload.


#' Tenement -> UGF assignment, as a canonical sorted vector
#'
#' @param x A tenements data.frame / sf (`tenement_id`, `ug_id`), or NULL.
#' @return Sorted character vector `"<tenement>\t<ug>"`, or NULL.
#' @noRd
.affectation_ug_df <- function(x) {
  if (is.null(x) || !all(c("tenement_id", "ug_id") %in% names(x))) return(NULL)
  sort(paste(as.character(x$tenement_id), as.character(x$ug_id), sep = "\t"))
}

#' Tenement -> UGF assignment stored on disk
#'
#' @param path Path of `tenements.gpkg`.
#' @return See [.affectation_ug_df()]; NULL when absent or unreadable.
#' @noRd
.affectation_ug <- function(path) {
  if (!file.exists(path)) return(NULL)
  x <- tryCatch(sf::st_drop_geometry(sf::st_read(path, quiet = TRUE)),
                error = function(e) NULL)
  .affectation_ug_df(x)
}


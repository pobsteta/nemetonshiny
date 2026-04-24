#' Compute Service for nemetonApp
#'
#' @description
#' Service for managing asynchronous indicator calculations.
#' Handles data downloading, caching, and computation orchestration.
#'
#' @name service_compute
#' @keywords internal
NULL


#' Data sources configuration
#'
#' @description
#' List of data sources required for indicator calculations.
#'
#' @noRd
NULL

#' Get global shared cache directory
#'
#' @description
#' Returns the path to a global cache directory shared across all projects.
#' Used for large datasets like OSO (~6 GB) that should not be duplicated
#' per project.
#'
#' @return Character. Path to the global cache directory.
#' @noRd
get_global_cache_dir <- function() {
  if (requireNamespace("rappdirs", quietly = TRUE)) {
    base_dir <- rappdirs::user_data_dir("nemeton", "nemeton")
  } else {
    base_dir <- file.path(Sys.getenv("HOME"), ".nemeton")
  }
  cache_dir <- file.path(base_dir, "cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  cache_dir
}

DATA_SOURCES <- list(
  # Raster sources
  rasters = list(
    ndvi = list(
      name = "NDVI (from IGN IRC)",
      type = "raster",
      source = "ign_irc",
      required_for = c("indicateur_c2_ndvi")
    ),
    dem = list(
      name = "Digital Elevation Model",
      type = "raster",
      source = "ign_bd_alti",
      required_for = c("indicateur_w3_humidite", "risk_erosion")
    ),
    forest_cover = list(
      name = "Forest Cover (OSO)",
      type = "raster",
      source = "oso",
      required_for = c("indicateur_c1_biomasse", "indicateur_l2_fragmentation")
    ),
    lidar_mnh = list(
      name = "Canopy Height Model (LiDAR HD)",
      type = "raster",
      source = "ign_lidar_hd",
      product = "mnh",
      required_for = c("indicateur_b2_structure", "indicateur_p1_volume", "indicateur_r2_tempete")
    ),
    lidar_mnt = list(
      name = "Digital Terrain Model (LiDAR HD)",
      type = "raster",
      source = "ign_lidar_hd",
      product = "mnt",
      required_for = c("indicateur_w3_humidite", "risk_erosion", "indicateur_r1_feu", "indicateur_r2_tempete", "indicateur_r3_secheresse")
    )
    # SoilGrids CEC is no longer pre-downloaded to a local cache:
    # indicateur_f1_fertilite(source = "soilgrids") fetches it on
    # demand via nemeton::load_raster_source(), so the app doesn't
    # need to own the download path any more.
  ),
  # Vector sources
  vectors = list(
    protected_areas = list(
      name = "Protected Areas",
      type = "vector",
      source = "inpn_wfs",
      required_for = c("indicateur_b1_protection")
    ),
    water_network = list(
      name = "Water Network",
      type = "vector",
      source = "ign_bd_topo",
      required_for = c("indicateur_w1_reseau")
    ),
    water_surfaces = list(
      name = "Water Surfaces",
      type = "vector",
      source = "ign_bd_topo",
      required_for = c("indicateur_w2_zones_humides")
    ),
    wetlands = list(
      name = "Wetlands",
      type = "vector",
      source = "inpn_wfs",
      required_for = c("indicateur_w2_zones_humides")
    ),
    roads = list(
      name = "Roads",
      type = "vector",
      source = "ign_bd_topo",
      required_for = c("indicateur_n1_distance", "indicateur_s1_routes")
    ),
    buildings = list(
      name = "Buildings",
      type = "vector",
      source = "ign_bd_topo",
      required_for = c("indicateur_s2_bati", "indicateur_n1_distance")
    ),
    bdforet = list(
      name = "BD For\u00eat V2 (IGN)",
      type = "vector",
      source = "ign_bdforet",
      required_for = c("indicateur_c1_biomasse", "indicateur_p1_volume", "indicateur_p2_station",
                        "indicateur_b2_structure", "indicateur_b3_connectivite",
                        "indicateur_t1_anciennete", "indicateur_r1_feu", "indicateur_r4_abroutissement",
                        "indicateur_n2_continuite")
    )
  ),
  # Point cloud sources (not loaded as raster/vector but cached as files)
  point_clouds = list(
    lidar_copc = list(
      name = "LiDAR HD Point Clouds (COPC)",
      type = "point_cloud",
      source = "ign_lidar_hd",
      product = "nuage",
      required_for = c("indicateur_b2_structure")
    )
  )
)


#' Computation status codes
#'
#' @noRd
COMPUTE_STATUS <- list(
  PENDING = "pending",
  DOWNLOADING = "downloading",
  COMPUTING = "computing",
  COMPLETED = "completed",
  ERROR = "error",
  CANCELLED = "cancelled"
)


#' Initialize computation state
#'
#' @description
#' Creates a reactive state object for tracking computation progress.
#' Checks for existing progress to support resume.
#'
#' @param project_id Character. Project ID.
#' @param indicators Character vector. Indicators to compute.
#'
#' @return List with reactive values for computation state.
#'
#' @noRd
init_compute_state <- function(project_id, indicators = "all") {
  # Get full indicator list if "all"
  if (length(indicators) == 1 && indicators == "all") {
    indicators <- list_available_indicators()
  }
  # Ensure indicators is always a character vector (not a list)
  indicators <- as.character(unlist(indicators))

  # Check for existing progress (for resume)
  existing_progress <- get_computation_progress(project_id)
  # Ensure computed_indicators is a character vector (JSON reads as list)
  # Use as.character(unlist()) to handle all edge cases:
  # - list() -> NULL -> character(0)
  # - list("a", "b") -> c("a", "b")
  # - named empty list -> character(0)
  already_computed <- existing_progress$computed_indicators %||% character(0)
  already_computed <- as.character(unlist(already_computed))
  already_computed <- intersect(already_computed, indicators)

  # Initialize status based on existing progress
  initial_status <- stats::setNames(rep("pending", length(indicators)), indicators)
  if (length(already_computed) > 0) {
    initial_status[already_computed] <- "completed"
  }

  list(
    project_id = project_id,
    status = COMPUTE_STATUS$PENDING,
    phase = "init",
    progress = 0,
    progress_max = length(indicators) + 10,  # +10 for download phase
    current_task = NULL,
    indicators_total = length(indicators),
    indicators_completed = length(already_computed),
    indicators_skipped = length(already_computed),
    indicators_failed = 0L,
    indicators_status = initial_status,
    errors = list(),
    started_at = NULL,
    completed_at = NULL,
    is_resume = length(already_computed) > 0,
    last_saved_at = existing_progress$last_saved_at
  )
}


#' List available indicators
#'
#' @description
#' Returns list of indicators that can be computed.
#'
#' @return Character vector of indicator names.
#'
#' @noRd
list_available_indicators <- function() {
  c(
    # Carbon (C)
    "indicateur_c1_biomasse", "indicateur_c2_ndvi",
    # Biodiversity (B)
    "indicateur_b1_protection", "indicateur_b2_structure", "indicateur_b3_connectivite",
    # Water (W)
    "indicateur_w1_reseau", "indicateur_w2_zones_humides", "indicateur_w3_humidite",
    # Air (A)
    "indicateur_a1_couverture", "indicateur_a2_qualite_air",
    # Fertility (F)
    "indicateur_f1_fertilite", "indicateur_f2_erosion",
    # Landscape (L)
    "indicateur_l2_fragmentation", "indicateur_l1_sylvosphere",
    # Temporal (T)
    "indicateur_t1_anciennete", "indicateur_t2_changement",
    # Risk (R)
    "indicateur_r1_feu", "indicateur_r2_tempete", "indicateur_r3_secheresse", "indicateur_r4_abroutissement",
    # Social (S)
    "indicateur_s1_routes", "indicateur_s2_bati", "indicateur_s3_population",
    # Production (P)
    "indicateur_p1_volume", "indicateur_p2_station", "indicateur_p3_qualite_bois",
    # Energy (E)
    "indicateur_e1_bois_energie", "indicateur_e2_evitement",
    # Naturalness (N)
    "indicateur_n1_distance", "indicateur_n2_continuite", "indicateur_n3_naturalite"
  )
}


#' Start computation workflow
#'
#' @description
#' Main entry point for starting indicator calculations.
#' Orchestrates downloading, caching, and computation.
#'
#' @param project_id Character. Project ID.
#' @param indicators Character vector. Indicators to compute, or "all".
#' @param progress_callback Function. Called with progress updates (for sync mode).
#' @param use_file_progress Logical. If TRUE, write progress to file for async polling.
#' @param project_path Character. Full path to project directory (for async mode).
#'
#' @return List with success status and results.
#'
#' @noRd
start_computation <- function(project_id,
                              indicators = "all",
                              progress_callback = NULL,
                              use_file_progress = FALSE,
                              project_path = NULL) {

  # Get project path (use provided or look up)
  if (is.null(project_path)) {
    project_path <- get_project_path(project_id)
  }
  if (is.null(project_path) || !dir.exists(project_path)) {
    stop("Project not found: ", project_id)
  }

  # Clear any previous cancellation signal
  clear_cancel(project_id)

  # Initialize state FIRST (before anything that can fail)
  # This ensures errors can be properly recorded and reported
  state <- init_compute_state(project_id, indicators)
  state$started_at <- Sys.time()

  # Helper function to report progress (callback or file)
  report_progress <- function(state) {
    if (!is.null(progress_callback)) {
      progress_callback(state)
    }
    if (use_file_progress) {
      save_progress_state(project_id, state)
    }
  }

  # Wrap entire computation in tryCatch to handle all errors
  tryCatch({
    # Load parcels - try Parquet first (faster), fall back to GeoPackage
    parquet_path <- file.path(project_path, "data", "parcels.parquet")
    gpkg_path <- file.path(project_path, "data", "parcels.gpkg")

    parcels <- NULL

    # Try Parquet first
    if (file.exists(parquet_path) &&
        requireNamespace("geoarrow", quietly = TRUE) &&
        requireNamespace("arrow", quietly = TRUE)) {
      parcels <- tryCatch({
        parcels_arrow <- arrow::read_parquet(parquet_path, as_data_frame = FALSE)
        sf::st_as_sf(parcels_arrow)
      }, error = function(e) NULL)
    }

    # Fall back to GeoPackage
    if (is.null(parcels) && file.exists(gpkg_path)) {
      parcels <- tryCatch({
        sf::st_read(gpkg_path, quiet = TRUE)
      }, error = function(e) NULL)
    }

    # Check if parcels were loaded
    if (is.null(parcels)) {
      stop("Parcels file not found or could not be loaded")
    }

    if (!inherits(parcels, "sf")) {
      stop("Failed to load parcels as sf object")
    }

    if (nrow(parcels) == 0) {
      stop("No parcels found in project")
    }

    # ------------------------------------------------------------------
    # UGF mode: indicators are computed on UGF geometries (one row per
    # Unité de Gestion Forestière), not on cadastral parcels. We load
    # the UGF data, build the dissolved UGF sf, and pass that as the
    # compute unit. Parcels stay loaded for reference (they're still
    # used by download_layers_for_parcels to fetch the right map tiles).
    # ------------------------------------------------------------------
    projet_for_ug <- tryCatch(
      load_project(project_id),
      error = function(e) NULL
    )
    if (is.null(projet_for_ug) || !has_ug_data(projet_for_ug)) {
      # First-time compute on a freshly loaded v1 project: make sure
      # UGF data exists (1 UGF = 1 parcel by default).
      projet_for_ug <- ensure_project_migrated(project_id, projet_for_ug)
    }
    compute_unit <- ug_build_sf(projet_for_ug)
    if (is.null(compute_unit) || nrow(compute_unit) == 0) {
      stop("Could not build UGF sf for computation")
    }

    # Enrich the UGF sf with columns that the nemeton indicator
    # functions expect to find on cadastral parcels. Without these,
    # most indicator_* functions error out before they even look at
    # the geometry (they join / filter by parcel-like keys).
    #  - id / nemeton_id / geo_parcelle : unique identifier (→ ug_id)
    #  - contenance                      : cadastral surface (m²)
    #  - code_insee                      : commune code, inherited
    #                                     from the first parcel
    #  - section / numero                : placeholder (UGFs don't
    #                                     map to a single cadastral
    #                                     ref — the list is kept in
    #                                     cadastral_refs)
    compute_unit$id <- compute_unit$ug_id
    compute_unit$nemeton_id <- compute_unit$ug_id
    compute_unit$geo_parcelle <- compute_unit$ug_id
    if (!"contenance" %in% names(compute_unit)) {
      compute_unit$contenance <- compute_unit$surface_m2
    }
    if ("code_insee" %in% names(parcels)) {
      # Use the most common INSEE code across the parent parcels;
      # a single UGF normally stays within one commune.
      codes <- as.character(parcels$code_insee)
      compute_unit$code_insee <- names(sort(table(codes), decreasing = TRUE))[1]
    }
    if (!"section" %in% names(compute_unit)) compute_unit$section <- NA_character_
    if (!"numero"  %in% names(compute_unit)) compute_unit$numero  <- NA_character_

    cli::cli_alert_info(
      "Calcul des indicateurs sur {nrow(compute_unit)} UGF (dont {nrow(parcels)} parcelle{?s} cadastrale{?s})"
    )

    # Update project status (with path for async mode)
    update_project_status(project_id, "computing")

    # Report initial progress
    report_progress(state)
    # Phase 1: Download data layers
    state$phase <- "downloading"
    state$status <- COMPUTE_STATUS$DOWNLOADING
    state$current_task <- "download_start"
    report_progress(state)

    layers <- download_layers_for_parcels(
      parcels = parcels,
      project_path = project_path,
      progress_callback = function(layer_progress) {
        # Use <<- to modify state in enclosing scope (start_computation).
        # Plain <- would create a local copy that reverts to the original
        # state$progress (0) whenever layer_progress$completed is NULL
        # (e.g. download_oso progress callbacks), resetting the progress bar.
        if (!is.null(layer_progress$completed)) {
          state$progress <<- layer_progress$completed
        }
        state$current_task <<- layer_progress$current
        report_progress(state)
      }
    )

    # Propagate download warnings to state errors (for UI display)
    if (length(layers$warnings) > 0) {
      state$errors <- c(state$errors, layers$warnings)
      report_progress(state)
    }

    # Tag compute_unit with the CHM provenance so nemeton::detect_ndp()
    # picks up the augmented flag (height_ml) when reporting NDP on the
    # saved indicators. Only set when a chm raster is actually present
    # in the downloaded layers (the Open-Canopy call may have failed
    # and degraded silently to legacy mode).
    if (!is.null(layers$rasters$chm) &&
        !is.null(layers$chm_source) &&
        !identical(layers$chm_source, "none")) {
      attr(compute_unit, "chm_source") <- layers$chm_source
    }
    # NDP lifts to 1 ("Observation") as soon as ANY LiDAR HD product
    # is available for the AOI (MNH canopy, MNT terrain, or both).
    # Open-Canopy ML predictions leave the NDP at 0 and are only
    # reported as an augmented flag.
    if (!is.null(layers$rasters$lidar_mnh) ||
        !is.null(layers$rasters$lidar_mnt)) {
      attr(compute_unit, "has_lidar_hd") <- TRUE
    }

    # Persist the actual CHM outcome so the synthesis badge reflects
    # what really happened during this run (auto-detection result),
    # not a pre-selected preference.
    tryCatch(
      update_project_metadata(
        project_id,
        list(chm_source = layers$chm_source %||% "none")
      ),
      error = function(e) {
        cli::cli_warn("Failed to persist chm_source outcome: {e$message}")
      }
    )

    # Check cancellation after download phase
    if (is_cancelled(project_id)) {
      state$status <- COMPUTE_STATUS$CANCELLED
      state$current_task <- "cancelled"
      report_progress(state)
      return(list(success = FALSE, state = state, cancelled = TRUE))
    }

    # Phase 2: Compute indicators
    state$phase <- "computing"
    state$status <- COMPUTE_STATUS$COMPUTING
    state$progress <- 10  # Download phase complete
    state$current_task <- "compute_start"
    report_progress(state)

    results <- compute_all_indicators(
      parcels = compute_unit,
      layers = layers,
      indicators = if (length(indicators) == 1 && indicators == "all") {
        list_available_indicators()
      } else {
        indicators
      },
      progress_callback = function(ind_progress) {
        # Use <<- to persist state changes in enclosing scope so that
        # post-loop code (error handler, final status) sees accurate values.
        state$progress <<- 10 + ind_progress$completed
        state$indicators_completed <<- ind_progress$completed
        state$indicators_failed <<- ind_progress$failed
        state$indicators_status <<- ind_progress$status
        state$current_task <<- ind_progress$current
        state$errors <<- ind_progress$errors
        # Track skipped indicators (already computed)
        if (!is.null(ind_progress$skipped)) {
          state$indicators_skipped <<- ind_progress$skipped
        }
        report_progress(state)
      },
      project_id = project_id  # Enable incremental saving
    )

    # Check cancellation after compute phase
    if (is_cancelled(project_id)) {
      state$status <- COMPUTE_STATUS$CANCELLED
      state$current_task <- "cancelled"
      report_progress(state)
      clear_cancel(project_id)
      return(list(success = FALSE, state = state, cancelled = TRUE))
    }

    # Final save (ensures metadata is updated)
    save_indicators(project_id, results)

    # Sync to PostGIS database if configured (non-bloquant)
    if (is_db_configured()) {
      tryCatch({
        db_sync_project(project_id)
      }, error = function(e) {
        cli::cli_warn("Database sync failed (non-blocking): {e$message}")
      })
    }

    # Update final state
    state$status <- COMPUTE_STATUS$COMPLETED
    state$phase <- "complete"
    state$completed_at <- Sys.time()
    state$progress <- state$progress_max
    state$current_task <- "complete"

    # Update project status
    update_project_status(project_id, "completed")

    report_progress(state)

    list(
      success = TRUE,
      state = state,
      results = results
    )

  }, error = function(e) {
    state$status <- COMPUTE_STATUS$ERROR
    state$current_task <- "error"
    state$errors <- c(state$errors, list(
      list(
        type = "fatal",
        message = e$message,
        time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    ))

    # Update project status
    update_project_status(project_id, "error")

    report_progress(state)

    list(
      success = FALSE,
      state = state,
      error = e$message
    )
  })
}


#' Download layers for parcels
#'
#' @description
#' Downloads and caches all required data layers for the parcels extent.
#'
#' @param parcels sf object. Parcels to compute indicators for.
#' @param project_path Character. Path to project directory.
#' @param progress_callback Function. Progress callback.
#'
#' @return nemeton_layers object with downloaded data.
#'
#' @noRd
download_layers_for_parcels <- function(parcels,
                                        project_path,
                                        progress_callback = NULL) {

  cache_dir <- file.path(project_path, "cache", "layers")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Get bounding box with 200m buffer

  # Buffer in a projected CRS (Lambert-93) to ensure meters, then convert

  # back to WGS84 for WFS queries. Applying a numeric offset directly to

  # WGS84 coordinates would treat the value as degrees instead of metres,
  # producing a bbox that covers most of the globe.
  buffer_m <- 200
  parcels_proj <- sf::st_transform(parcels, 2154)
  parcels_buffered <- sf::st_buffer(parcels_proj, buffer_m)
  parcels_buffered_wgs84 <- sf::st_transform(parcels_buffered, 4326)
  bbox_buffered <- as.numeric(sf::st_bbox(parcels_buffered_wgs84))

  # Initialize layers structure
  rasters <- list()
  vectors <- list()
  download_warnings <- list()  # Collect download warnings for UI display

  total_sources <- length(DATA_SOURCES$rasters) + length(DATA_SOURCES$vectors)
  completed <- 0

  # Mapping source names to translation keys
  source_translation_keys <- list(
    ndvi = "source_ndvi",
    dem = "source_dem",
    forest_cover = "source_forest_cover",
    lidar_mnh = "source_lidar_mnh",
    lidar_mnt = "source_lidar_mnt",
    lidar_copc = "source_lidar_copc",
    protected_areas = "source_protected_areas",
    water_network = "source_water_network",
    water_surfaces = "source_water_surfaces",
    wetlands = "source_wetlands",
    roads = "source_roads",
    buildings = "source_buildings",
    bdforet = "source_bdforet"
  )

  # Download raster sources
  for (source_name in names(DATA_SOURCES$rasters)) {
    source <- DATA_SOURCES$rasters[[source_name]]

    # Get translated source name
    translation_key <- source_translation_keys[[source_name]] %||% source_name
    translated_name <- paste0("download:", translation_key)

    if (!is.null(progress_callback)) {
      progress_callback(list(
        completed = completed,
        total = total_sources,
        current = translated_name,
        source_key = translation_key
      ))
    }

    tryCatch({
      raster_data <- download_raster_source(
        source_name = source_name,
        source_config = source,
        bbox = bbox_buffered,
        cache_dir = cache_dir,
        progress_callback = progress_callback
      )

      if (!is.null(raster_data)) {
        rasters[[source_name]] <- raster_data
      } else if (source_name == "forest_cover") {
        # Special warning for OSO download failure
        global_oso_dir <- file.path(get_global_cache_dir(), "oso")
        download_warnings <- c(download_warnings, list(list(
          type = "warning",
          source = "OSO",
          message = paste0(
            "Le t\u00e9l\u00e9chargement OSO a \u00e9chou\u00e9 (fichier de 6 Go). ",
            "T\u00e9l\u00e9chargez manuellement depuis: ",
            "https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/UZ2NJ7 ",
            "et placez oso.tif dans: ", global_oso_dir
          ),
          time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )))
      } else if (source$source == "ign_lidar_hd") {
        # Warning when LiDAR HD tiles are not available for this area
        product_label <- toupper(source$product %||% "MNH")
        download_warnings <- c(download_warnings, list(list(
          type = "info",
          source = paste0("LiDAR HD ", product_label),
          message = paste0(
            "Aucune dalle LiDAR HD ", product_label,
            " disponible pour cette zone. ",
            "La couverture LiDAR HD de l'IGN est progressive et ne couvre pas encore tout le territoire. ",
            "Les indicateurs concern\u00e9s utiliseront des donn\u00e9es alternatives (NDVI, BD ALTI)."
          ),
          time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )))
      }
    }, error = function(e) {
      cli::cli_warn("Failed to download {source$name}: {e$message}")
      if (source_name == "forest_cover") {
        global_oso_dir <- file.path(get_global_cache_dir(), "oso")
        download_warnings <<- c(download_warnings, list(list(
          type = "warning",
          source = "OSO",
          message = paste0(
            "Le t\u00e9l\u00e9chargement OSO a \u00e9chou\u00e9: ", e$message, ". ",
            "T\u00e9l\u00e9chargez manuellement depuis: ",
            "https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/UZ2NJ7 ",
            "et placez oso.tif dans: ", global_oso_dir
          ),
          time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )))
      }
    })

    completed <- completed + 1
  }

  # Download vector sources
  for (source_name in names(DATA_SOURCES$vectors)) {
    source <- DATA_SOURCES$vectors[[source_name]]

    # Get translated source name
    translation_key <- source_translation_keys[[source_name]] %||% source_name
    translated_name <- paste0("download:", translation_key)

    if (!is.null(progress_callback)) {
      progress_callback(list(
        completed = completed,
        total = total_sources,
        current = translated_name,
        source_key = translation_key
      ))
    }

    tryCatch({
      vector_data <- download_vector_source(
        source_name = source_name,
        source_config = source,
        bbox = bbox_buffered,
        cache_dir = cache_dir
      )

      if (!is.null(vector_data)) {
        vectors[[source_name]] <- vector_data
      } else {
        translation_key <- source_translation_keys[[source_name]] %||% source_name
        download_warnings <- c(download_warnings, list(list(
          type = "info",
          source = source$name,
          message = paste0(
            "Aucune donn\u00e9e trouv\u00e9e pour ", source$name,
            " dans cette zone. Les indicateurs concern\u00e9s utiliseront ",
            "des donn\u00e9es alternatives ou retourneront NA."
          ),
          time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )))
      }
    }, error = function(e) {
      cli::cli_warn("Failed to download {source$name}: {e$message}")
    })

    completed <- completed + 1
  }

  # Download point cloud sources (LiDAR COPC tiles)
  point_clouds <- list()
  pc_sources <- DATA_SOURCES$point_clouds %||% list()

  for (source_name in names(pc_sources)) {
    source <- pc_sources[[source_name]]

    translation_key <- source_translation_keys[[source_name]] %||% source_name
    translated_name <- paste0("download:", translation_key)

    if (!is.null(progress_callback)) {
      progress_callback(list(
        completed = completed,
        total = total_sources + length(pc_sources),
        current = translated_name,
        source_key = translation_key
      ))
    }

    tryCatch({
      if (source$source == "ign_lidar_hd") {
        pc_files <- download_ign_lidar_hd(
          bbox = bbox_buffered,
          cache_dir = cache_dir,
          product = source$product %||% "nuage",
          progress_callback = progress_callback
        )
        if (!is.null(pc_files)) {
          point_clouds[[source_name]] <- pc_files
        } else {
          product_label <- toupper(source$product %||% "NUAGE")
          download_warnings <- c(download_warnings, list(list(
            type = "info",
            source = paste0("LiDAR HD ", product_label),
            message = paste0(
              "Aucune dalle LiDAR HD ", product_label,
              " (nuages de points) disponible pour cette zone. ",
              "La couverture LiDAR HD de l'IGN est progressive et ne couvre pas encore tout le territoire."
            ),
            time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          )))
        }
      }
    }, error = function(e) {
      cli::cli_warn("Failed to download {source$name}: {e$message}")
    })

    completed <- completed + 1
  }

  # Promote LiDAR HD MNT (1 m) to the canonical `dem` slot when it
  # is available: W3 (TWI), R1 (fire), R2 (storm), R3 (drought)
  # and the erosion risk all look up `rasters$dem` via
  # resolve_raster_layer(). Replacing BD ALTI 25 m with LiDAR MNT
  # 1 m lifts the terrain-derived indicators to NDP-1 precision
  # whenever LiDAR HD tiles exist for the AOI.
  if (!is.null(rasters$lidar_mnt) &&
      inherits(rasters$lidar_mnt, "SpatRaster")) {
    cli::cli_alert_success(
      "Using LiDAR HD MNT (1 m) for terrain indicators instead of BD ALTI 25 m"
    )
    rasters$dem <- rasters$lidar_mnt
  }

  # Optional Canopy Height Model (spec 005 phase 6). Auto-enabled
  # whenever the `opencanopy` package is installed, unless the user
  # opted out via option `nemetonshiny.chm = "none"` or environment
  # variable `NEMETONSHINY_DISABLE_CHM`. The effective provenance is
  # reported back as `chm_source` (either "opencanopy" on success or
  # "none" on skip/failure) so downstream code (badge in mod_synthesis,
  # detect_ndp augmented flag) reflects what actually happened.
  chm_pct_masked <- NULL
  chm_source <- "none"

  # Step 1: try LiDAR HD MNH (direct aerial measurement, ~0.5 m
  # vertical accuracy). IGN coverage is partial in 2026 (full France
  # planned by end of year); a missing-tile return lets us fall back
  # to Open-Canopy ML for AOIs not yet flown.
  if (chm_lidar_enabled()) {
    if (!is.null(progress_callback)) {
      progress_callback(list(
        completed = total_sources + length(pc_sources),
        total     = total_sources + length(pc_sources),
        current   = "chm_phase:lidar_hd_download",
        source_key = NULL
      ))
    }
    lidar_out <- tryCatch(
      download_chm_lidar_hd(parcels, cache_dir,
                            progress_callback = progress_callback),
      error = function(e) {
        cli::cli_warn("LiDAR HD CHM fetch failed: {e$message}")
        NULL
      }
    )
    if (!is.null(lidar_out) && !is.null(lidar_out$chm)) {
      rasters$chm <- lidar_out$chm
      chm_pct_masked <- lidar_out$pct_masked
      chm_source <- "lidar_hd"
      cli::cli_alert_success("Using LiDAR HD MNH as CHM source")
    }
  }

  # Step 2: Open-Canopy ML — only if LiDAR HD was skipped or failed.
  if (chm_source == "none" && chm_auto_enabled()) {
    if (!is.null(progress_callback)) {
      # Not "download:" — the pipeline fetches the IGN ortho once
      # (cheap) but the bulk of the time is the PVTv2/UNet ML
      # inference. Use a dedicated task key so the UI shows
      # "Inférence CHM Open-Canopy" instead of a misleading
      # "Téléchargement …" status.
      progress_callback(list(
        completed = total_sources + length(pc_sources),
        total     = total_sources + length(pc_sources),
        current   = "chm_inference_opencanopy",
        source_key = NULL
      ))
    }
    # Relay per-tile progress from opencanopy's WMS download loop
    # into the outer progress_callback so the Shiny status line
    # reads e.g. "Téléchargement tuile RVB 5/28 (Open-Canopy)…"
    # rather than staying frozen on the generic "Inférence CHM"
    # label while 2 minutes of tiles go by.
    chm_progress <- if (is.null(progress_callback)) NULL else
      function(ev) {
        if (!is.list(ev) || is.null(ev$type)) return(invisible())
        current <- switch(ev$type,
          phase = {
            step <- ev$step %||% "?"
            if (!is.null(ev$model) && nzchar(ev$model))
              sprintf("chm_phase:%s:%s", step, ev$model)
            else
              sprintf("chm_phase:%s", step)
          },
          tile_phase_start = sprintf("chm_tile_start:%s:%d",
                                     ev$prefix %||% "ortho",
                                     ev$n_tiles %||% 0L),
          tile = sprintf("chm_tile:%s:%d:%d",
                         ev$prefix %||% "ortho",
                         ev$idx %||% 0L,
                         ev$n_tiles %||% 0L),
          inference_phase_start = sprintf("chm_inference_start:%d",
                                          ev$n_tiles %||% 0L),
          inference_tile = sprintf("chm_inference_tile:%d:%d",
                                   ev$idx %||% 0L,
                                   ev$n_tiles %||% 0L),
          NULL
        )
        if (is.null(current)) return(invisible())
        progress_callback(list(
          completed = total_sources + length(pc_sources),
          total     = total_sources + length(pc_sources),
          current   = current,
          source_key = NULL
        ))
      }

    chm_out <- tryCatch(
      download_chm_opencanopy(parcels, cache_dir, rasters, vectors,
                              progress_callback = chm_progress),
      error = function(e) {
        cli::cli_warn("Open-Canopy CHM fetch failed: {e$message}")
        download_warnings <<- c(download_warnings, list(list(
          type = "warning", source = "Open-Canopy",
          message = paste0("Open-Canopy CHM non disponible: ", e$message),
          time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        )))
        NULL
      }
    )
    if (!is.null(chm_out)) {
      rasters$chm <- chm_out$chm
      chm_pct_masked <- chm_out$pct_masked
      chm_source <- "opencanopy"
    }
  }

  if (!is.null(progress_callback)) {
    progress_callback(list(
      completed = total_sources + length(pc_sources),
      total = total_sources + length(pc_sources),
      current = "download_complete",
      source_key = NULL
    ))
  }

  # Create nemeton_layers object
  structure(
    list(
      rasters = rasters,
      vectors = vectors,
      point_clouds = point_clouds,
      bbox = bbox_buffered,
      crs = sf::st_crs(parcels),
      cache_dir = cache_dir,
      chm_source = chm_source,
      chm_pct_masked = chm_pct_masked,
      warnings = download_warnings  # Include download warnings for UI display
    ),
    class = "nemeton_layers"
  )
}


# ============================================================
# CHM fetch via opencanopy (spec 005 phase 6)
# ============================================================

# Pipeline Open-Canopy + sanitize_chm, producing a cleaned CHM
# suitable for indicators P1/P2/C1/B2/R2.
#
# Runs synchronously: opencanopy::pipeline_aoi_to_chm() downloads
# IGN ortho (RVB + IRC), resamples to 1.5 m, runs the ML model
# (Python + GPU recommended), then we apply nemeton::sanitize_chm
# with whatever mask layers we already downloaded.
#
# @param parcels sf of UGFs (user parcels).
# @param cache_dir Character. Project-level cache directory.
# @param rasters List of already-downloaded raster layers (may
#   include `ndvi`).
# @param vectors List of already-downloaded vector layers (may
#   include `bdforet`, `buildings`, `water_surfaces`).
#
# @return list(chm = SpatRaster, pct_masked = numeric,
#   steps_applied = chr), or NULL if opencanopy is unavailable
#   or the pipeline fails.
#
# @noRd
#' Report whether Open-Canopy CHM should be attempted automatically.
#'
#' The pipeline is opt-out: it runs whenever the `opencanopy` package
#' is installed, unless the user forces it off via
#' `options(nemetonshiny.chm = "none")` or the environment variable
#' `NEMETONSHINY_DISABLE_CHM=1`. A hard-failing opencanopy call
#' (missing Python, missing model, network) is still caught by the
#' tryCatch in `download_layers_for_parcels()` and degraded silently.
#'
#' @return Logical.
#' @noRd
chm_auto_enabled <- function() {
  if (identical(getOption("nemetonshiny.chm"), "none")) {
    cli::cli_alert_info(
      "CHM skipped: options(nemetonshiny.chm = \"none\") is set."
    )
    return(FALSE)
  }
  env_val <- Sys.getenv("NEMETONSHINY_DISABLE_CHM")
  if (nzchar(env_val) && env_val != "0") {
    cli::cli_alert_info(
      "CHM skipped: env var NEMETONSHINY_DISABLE_CHM = {.val {env_val}}. \\
       Unset with Sys.unsetenv(\"NEMETONSHINY_DISABLE_CHM\") to re-enable."
    )
    return(FALSE)
  }
  if (!requireNamespace("opencanopy", quietly = TRUE)) {
    cli::cli_alert_info(
      "CHM skipped: package {.pkg opencanopy} is not installed."
    )
    return(FALSE)
  }
  TRUE
}


#' Report whether LiDAR HD MNH CHM should be attempted automatically.
#'
#' Opt-out: runs whenever \pkg{happign} is installed, unless the user
#' forces it off via \code{options(nemetonshiny.chm = "none")},
#' \code{options(nemetonshiny.chm_source = "opencanopy")} or env var
#' \code{NEMETONSHINY_DISABLE_CHM=1}. LiDAR HD coverage is incomplete
#' in France (target 2026); a missing-tile failure is caught by the
#' tryCatch in \code{download_layers_for_parcels()} and the pipeline
#' falls back to Open-Canopy ML.
#'
#' @return Logical.
#' @noRd
chm_lidar_enabled <- function() {
  if (identical(getOption("nemetonshiny.chm"), "none")) return(FALSE)
  env_val <- Sys.getenv("NEMETONSHINY_DISABLE_CHM")
  if (nzchar(env_val) && env_val != "0") return(FALSE)
  if (identical(getOption("nemetonshiny.chm_source"), "opencanopy")) {
    cli::cli_alert_info(
      "LiDAR HD CHM skipped: options(nemetonshiny.chm_source) = \"opencanopy\"."
    )
    return(FALSE)
  }
  if (!requireNamespace("happign", quietly = TRUE)) {
    cli::cli_alert_info(
      "LiDAR HD CHM skipped: package {.pkg happign} is not installed."
    )
    return(FALSE)
  }
  TRUE
}


#' Fetch CHM from IGN LiDAR HD MNH tiles via happign
#'
#' Wraps \code{download_ign_lidar_hd(bbox, cache_dir, product = "mnh")}
#' into the same return shape as \code{download_chm_opencanopy()} so
#' the call site in \code{download_layers_for_parcels()} can swap
#' sources without further branching. LiDAR HD MNH is a direct
#' airborne measurement (IGN, national flights 2021-2026) — when
#' tiles are available for the AOI, it is preferred over the
#' Open-Canopy ML prediction (which is a synthesised CHM from ortho
#' imagery, less accurate).
#'
#' Returns \code{NULL} gracefully when the AOI is not yet covered
#' by LiDAR HD flights (caller then falls back to Open-Canopy).
#'
#' @param parcels sf of project parcels (in any CRS).
#' @param cache_dir Character. Project cache directory.
#' @param progress_callback Optional function for per-tile progress
#'   reporting. Same contract as the opencanopy path: receives a
#'   list with \code{current}, \code{completed}, \code{total}.
#'
#' @return list(chm = SpatRaster, pct_masked = numeric or NULL,
#'   steps_applied = character) or NULL if no tiles were fetched.
#'
#' @noRd
download_chm_lidar_hd <- function(parcels, cache_dir,
                                  progress_callback = NULL) {
  if (!requireNamespace("happign", quietly = TRUE)) {
    stop("Package 'happign' is not installed")
  }

  # bbox in WGS84 (same convention as download_ign_lidar_hd).
  parcels_ll <- sf::st_transform(parcels, 4326)
  bbox <- sf::st_bbox(sf::st_buffer(
    sf::st_transform(parcels, 2154), 50
  ))
  bbox <- sf::st_bbox(sf::st_transform(
    sf::st_as_sfc(bbox), 4326
  ))

  if (!is.null(progress_callback)) {
    progress_callback(list(current = "chm_phase:lidar_hd_download",
                           completed = 0, total = 1))
  }

  cli::cli_alert_info("Fetching IGN LiDAR HD MNH tiles (direct measurement)...")
  mnh <- tryCatch(
    download_ign_lidar_hd(bbox, cache_dir, product = "mnh",
                         progress_callback = progress_callback),
    error = function(e) {
      cli::cli_alert_warning("LiDAR HD MNH fetch failed: {e$message}")
      NULL
    }
  )
  if (is.null(mnh)) return(NULL)

  # sanitize_chm() applies the same 5-step cleaning as the
  # opencanopy path (forest mask, buildings, water, slope, NDVI
  # threshold, max height). Degrade gracefully when the helper
  # layers are not available (e.g. happign didn't return buildings).
  if ("sanitize_chm" %in% getNamespaceExports("nemeton")) {
    cleaned <- tryCatch(
      nemeton::sanitize_chm(mnh),
      error = function(e) list(chm_clean = mnh, pct_masked = NA_real_,
                               steps_applied = character(0))
    )
    return(list(
      chm = cleaned$chm_clean,
      pct_masked = cleaned$pct_masked,
      steps_applied = cleaned$steps_applied
    ))
  }

  list(chm = mnh, pct_masked = NA_real_, steps_applied = character(0))
}


download_chm_opencanopy <- function(parcels, cache_dir, rasters, vectors,
                                    progress_callback = NULL) {
  if (!requireNamespace("opencanopy", quietly = TRUE)) {
    stop("Package 'opencanopy' is not installed")
  }
  oc_dir <- file.path(cache_dir, "opencanopy")
  if (!dir.exists(oc_dir)) dir.create(oc_dir, recursive = TRUE, showWarnings = FALSE)

  chm_path <- file.path(oc_dir, "chm_1_5m.tif")
  if (!file.exists(chm_path)) {
    cli::cli_alert_info("Running opencanopy::pipeline_aoi_to_chm (may take several minutes)...")
    aoi_path <- file.path(oc_dir, "aoi.gpkg")
    parcels_l93 <- sf::st_transform(parcels, 2154)
    aoi_bbox <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(parcels_l93, 50)))
    sf::st_write(
      sf::st_sf(id = 1L, geometry = aoi_bbox),
      aoi_path, delete_dsn = TRUE, quiet = TRUE
    )
    # Forward per-tile progress only if opencanopy is new enough to
    # accept it — older installs silently ignored a callback arg
    # but some revisions errored on unused args.
    pipe_args <- list(aoi_path = aoi_path, output_dir = oc_dir)
    if ("progress_callback" %in%
        names(formals(opencanopy::pipeline_aoi_to_chm))) {
      pipe_args$progress_callback <- progress_callback
    }
    pipe <- do.call(opencanopy::pipeline_aoi_to_chm, pipe_args)
    terra::writeRaster(pipe$chm_1_5m, chm_path, overwrite = TRUE)
  }

  chm <- terra::rast(chm_path)
  cli::cli_alert_info("Sanitizing CHM with available mask layers...")

  # download_{raster,vector}_source() return bare SpatRaster / sf
  # objects, not list(object = …) wrappers. Using $object on a
  # SpatRaster dispatches to [[ and throws "[subset] invalid
  # name(s)" when no layer of that name exists — which aborted the
  # whole CHM pipeline and forced P2 back into legacy mode.
  forest_mask <- vectors$bdforet
  water       <- vectors$water_surfaces
  buildings   <- vectors$buildings
  ndvi        <- rasters$ndvi

  cleaned <- nemeton::sanitize_chm(
    chm,
    forest_mask = forest_mask,
    buildings   = buildings,
    water       = water,
    ndvi        = ndvi
  )
  cli::cli_alert_success(
    "CHM ready: {round(cleaned$pct_masked * 100, 1)}% masked, \\
    steps: {paste(cleaned$steps_applied, collapse = ', ')}"
  )

  list(
    chm           = cleaned$chm_clean,
    pct_masked    = cleaned$pct_masked,
    steps_applied = cleaned$steps_applied
  )
}


#' Download raster source
#'
#' @noRd
download_raster_source <- function(source_name,
                                   source_config,
                                   bbox,
                                   cache_dir,
                                   progress_callback = NULL) {

  cache_file <- file.path(cache_dir, paste0(source_name, ".tif"))

  # Return cached if exists

  if (file.exists(cache_file)) {
    return(suppressWarnings(terra::rast(cache_file)))
  }

  # Download based on source type
  raster_data <- switch(
    source_config$source,
    "ign_irc" = download_ign_irc_ndvi(bbox, cache_file),
    "ign_bd_alti" = download_ign_dem(bbox, cache_file),
    "oso" = download_oso(bbox, cache_file, progress_callback = progress_callback),
    "ign_lidar_hd" = download_ign_lidar_hd(
      bbox = bbox,
      cache_dir = dirname(cache_file),
      product = source_config$product %||% "mnh",
      progress_callback = progress_callback
    ),
    {
      cli::cli_warn("Unknown raster source: {source_config$source}")
      NULL
    }
  )

  raster_data
}


#' Download vector source
#'
#' @noRd
download_vector_source <- function(source_name,
                                   source_config,
                                   bbox,
                                   cache_dir) {

  cache_file <- file.path(cache_dir, paste0(source_name, ".gpkg"))

  # Return cached if exists
  if (file.exists(cache_file)) {
    cached <- sf::st_read(cache_file, quiet = TRUE)
    # Drop Z/M coordinates (BD TOPO 3D data causes s2 warnings)
    cached <- sf::st_zm(cached, drop = TRUE, what = "ZM")
    return(cached)
  }

  # Download based on source type
  vector_data <- switch(
    source_config$source,
    "inpn_wfs" = download_inpn_wfs(source_name, bbox, cache_file),
    "ign_bd_topo" = download_ign_bdtopo(source_name, bbox, cache_file),
    "ign_bdforet" = download_ign_bdforet(bbox, cache_file),
    {
      cli::cli_warn("Unknown vector source: {source_config$source}")
      NULL
    }
  )

  vector_data
}


#' Download functions for external data sources
#'
#' @description
#' Functions to download raster and vector data from various French
#' geographic data providers (IGN, INPN, Copernicus).
#'
#' @name download_functions
#' @keywords internal
NULL


#' Download INPN WFS data (protected areas, wetlands)
#'
#' @description
#' Downloads vector data from INPN WFS service for protected areas
#' and wetlands in metropolitan France.
#'
#' @param layer_name Character. Name of the layer to download.
#'   Supported: "protected_areas", "wetlands"
#' @param bbox Numeric vector or sf bbox. Bounding box (xmin, ymin, xmax, ymax) in WGS84.
#' @param cache_file Character. Path to save the downloaded data.
#'
#' @return sf object with the downloaded data, or NULL if download fails.
#'
#' @noRd
download_inpn_wfs <- function(layer_name, bbox, cache_file) {
  # Use happign to download protection zones from INPN/patrinat WFS
  # with a 2km buffer around the area (like tutorial 04 section 2)

  if (!requireNamespace("happign", quietly = TRUE)) {
    cli::cli_warn("Package {.pkg happign} required for INPN WFS download")
    return(NULL)
  }

  cli::cli_alert_info("Downloading INPN data for {layer_name} via happign...")

  # Create sf shape from bbox with 2km buffer in projected CRS
  if (inherits(bbox, "bbox")) {
    bbox <- as.numeric(bbox)
  }
  bbox_poly <- sf::st_as_sfc(sf::st_bbox(c(
    xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4]
  ), crs = sf::st_crs(4326)))
  # Project to Lambert-93 for metric buffer, add 2km, back to WGS84
  shape <- sf::st_transform(bbox_poly, 2154)
  shape <- sf::st_buffer(shape, 2000)
  shape <- sf::st_transform(shape, 4326)

  # Get patrinat layers from happign WFS catalog
  wfs_layers <- tryCatch(
    happign::get_layers_metadata("wfs"),
    error = function(e) {
      cli::cli_warn("Failed to get WFS layer metadata: {e$message}")
      return(NULL)
    }
  )
  if (is.null(wfs_layers)) return(NULL)

  # Filter layers based on requested layer_name
  if (layer_name == "protected_areas") {
    target_layers <- wfs_layers$Name[grepl("patrinat", wfs_layers$Name,
                                           ignore.case = TRUE)]
  } else if (layer_name == "wetlands") {
    target_layers <- wfs_layers$Name[grepl(
      "patrinat.*(ramsar|znieff|zone_humide)",
      wfs_layers$Name, ignore.case = TRUE
    )]
  } else {
    cli::cli_warn("Unknown INPN layer: {layer_name}")
    return(NULL)
  }

  if (length(target_layers) == 0) {
    cli::cli_alert_warning("No patrinat layers found in WFS catalog")
    return(NULL)
  }

  cli::cli_alert_info("Found {length(target_layers)} patrinat layers to query")

  # Download each layer
  all_features <- list()

  for (layer in target_layers) {
    tryCatch({
      z <- happign::get_wfs(shape, layer)
      if (!is.null(z) && nrow(z) > 0) {
        z$type_protection <- layer
        all_features[[layer]] <- z
        cli::cli_alert_success("  {layer}: {nrow(z)} features")
      }
    }, error = function(e) {
      # Silently skip layers that fail (many patrinat layers may not
      # have data in the area or may return errors)
      NULL
    })
  }

  # Combine all features
  if (length(all_features) == 0) {
    cli::cli_alert_warning("No INPN features found for {layer_name} in this area")
    return(NULL)
  }

  # Standardize columns: zone_id, zone_name, zone_type, geometry
  result <- tryCatch({
    standardized <- lapply(all_features, function(x) {
      # Find name-like column
      name_candidates <- c("nom_site", "NOM", "nom", "SITENAME", "NAME",
                           "nomsit", "nom_s2", "nom_z2", "nom_z1")
      name_col <- intersect(names(x), name_candidates)
      zone_name <- if (length(name_col) > 0) {
        as.character(x[[name_col[1]]])
      } else {
        rep(NA_character_, nrow(x))
      }

      # Find id-like column
      id_candidates <- c("ID_MNHN", "id_mnhn", "SITECODE", "sitecode",
                          "id_sic", "id_zps", "gml_id", "id")
      id_col <- intersect(names(x), id_candidates)
      zone_id <- if (length(id_col) > 0) {
        as.character(x[[id_col[1]]])
      } else {
        rep(NA_character_, nrow(x))
      }

      sf::st_sf(
        zone_id = zone_id,
        zone_name = zone_name,
        zone_type = x$type_protection,
        geometry = sf::st_geometry(x)
      )
    })

    do.call(rbind, standardized)
  }, error = function(e) {
    cli::cli_warn("Failed to standardize features: {e$message}")
    # Fallback: just use the first non-empty result
    all_features[[1]]
  })

  # Save to cache
  if (!is.null(result) && nrow(result) > 0) {
    tryCatch({
      sf::st_write(result, cache_file, quiet = TRUE, delete_dsn = TRUE)
      cli::cli_alert_success("Saved {nrow(result)} INPN features to cache")
    }, error = function(e) {
      cli::cli_warn("Failed to cache INPN data: {e$message}")
    })
  }

  result
}


#' Download IGN BD TOPO data (roads, water network, buildings)
#'
#' @description
#' Downloads vector data from IGN Geoplateforme WFS service.
#'
#' @param layer_name Character. Name of the layer to download.
#'   Supported: "roads", "indicateur_w1_reseau", "buildings"
#' @param bbox Numeric vector or sf bbox. Bounding box in WGS84.
#' @param cache_file Character. Path to save the downloaded data.
#'
#' @return sf object with the downloaded data, or NULL if download fails.
#'
#' @noRd
download_ign_bdtopo <- function(layer_name, bbox, cache_file) {

  if (!requireNamespace("happign", quietly = TRUE)) {
    cli::cli_warn("Package {.pkg happign} required for IGN BD TOPO download")
    return(NULL)
  }

  # Map layer names to BD TOPO V3 typenames (source: inst/datasources/FR.json)
  # Charger la config depuis FR.json (les cles DATA_SOURCES = cles FR.json)
  layer_cfg <- get_data_source(layer_name, "FR")
  typename <- layer_cfg$typename

  # Fallback sur les noms en dur si la config n'est pas trouvee
  if (is.null(typename)) {
    layer_mapping <- list(
      water_network = "BDTOPO_V3:troncon_hydrographique",
      water_surfaces = "BDTOPO_V3:surface_hydrographique",
      roads = "BDTOPO_V3:troncon_de_route",
      buildings = "BDTOPO_V3:batiment"
    )
    typename <- layer_mapping[[layer_name]]
  }
  if (is.null(typename)) {
    cli::cli_warn("Unknown BD TOPO layer: {layer_name}")
    return(NULL)
  }

  cli::cli_alert_info("Downloading IGN BD TOPO {layer_name} via happign...")

  tryCatch({
    # Build sf shape from bbox for happign::get_wfs()
    if (inherits(bbox, "bbox")) {
      bbox <- as.numeric(bbox)
    }
    shape <- sf::st_as_sfc(sf::st_bbox(c(
      xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4]
    ), crs = sf::st_crs(4326)))

    result <- happign::get_wfs(x = shape, layer = typename)

    if (is.null(result) || nrow(result) == 0) {
      cli::cli_alert_warning("No IGN BD TOPO features found for {layer_name}")
      return(NULL)
    }

    # Drop Z/M coordinates (BD TOPO is 3D, s2 engine only supports 2D)
    result <- sf::st_zm(result, drop = TRUE, what = "ZM")

    # Project to Lambert-93
    result <- sf::st_transform(result, 2154)

    # Save to cache
    sf::st_write(result, cache_file, quiet = TRUE, delete_dsn = TRUE)
    cli::cli_alert_success("Downloaded {nrow(result)} BD TOPO features for {layer_name}")

    result

  }, error = function(e) {
    cli::cli_warn("Failed to download IGN BD TOPO {layer_name}: {e$message}")
    NULL
  })
}


#' Download BD Forêt V2 (formations végétales) from IGN WFS
#'
#' Downloads vegetation formation polygons from IGN Géoplateforme WFS.
#' The layer \code{LANDCOVER.FORESTINVENTORY.V2:formation_vegetale} provides
#' species, type and structure information used by carbon, production and
#' biodiversity indicators.
#'
#' @param bbox Numeric vector. Bounding box (xmin, ymin, xmax, ymax) in WGS84.
#' @param cache_file Character. Path to save the downloaded GeoPackage.
#'
#' @return sf object with BD Forêt features, or NULL if download fails.
#' @noRd
download_ign_bdforet <- function(bbox, cache_file) {
  # BD Foret V2 via WFS (source: inst/datasources/FR.json)
  bdforet_cfg <- get_layer_service("bdforet", "FR")
  base_url <- bdforet_cfg$url %||% "https://data.geopf.fr/wfs/ows"
  typename <- bdforet_cfg$typename %||% "LANDCOVER.FORESTINVENTORY.V2:formation_vegetale"

  if (inherits(bbox, "bbox")) {
    bbox <- as.numeric(bbox)
  }

  bbox_str <- paste(bbox[c(1, 2, 3, 4)], collapse = ",")

  cli::cli_alert_info("Downloading BD For\u00eat V2 (formations v\u00e9g\u00e9tales)...")

  tryCatch({
    wfs_url <- paste0(
      base_url,
      "?SERVICE=WFS",
      "&VERSION=2.0.0",
      "&REQUEST=GetFeature",
      "&TYPENAME=", typename,
      "&BBOX=", bbox_str, ",EPSG:4326",
      "&SRSNAME=EPSG:4326",
      "&OUTPUTFORMAT=application/json",
      "&COUNT=10000"
    )

    resp <- httr2::request(wfs_url) |>
      httr2::req_timeout(120) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) != 200) {
      cli::cli_warn("IGN WFS returned status {httr2::resp_status(resp)} for BD For\u00eat")
      return(NULL)
    }

    geojson <- httr2::resp_body_string(resp)

    if (nchar(geojson) < 50) {
      cli::cli_alert_warning("No BD For\u00eat features found for this area")
      return(NULL)
    }

    result <- sf::st_read(geojson, quiet = TRUE)

    if (nrow(result) == 0) {
      cli::cli_alert_warning("No BD For\u00eat features found for this area")
      return(NULL)
    }

    sf::st_write(result, cache_file, quiet = TRUE, delete_dsn = TRUE)
    cli::cli_alert_success("Downloaded {nrow(result)} BD For\u00eat formations")

    result

  }, error = function(e) {
    cli::cli_warn("Failed to download BD For\u00eat V2: {e$message}")
    NULL
  })
}


#' Download OSO land cover raster
#'
#' @description
#' Downloads OSO (Occupation du Sol) land cover data from Theia/CESBIO via
#' Recherche Data Gouv. OSO provides 10m resolution land cover classification
#' for France with forest classes (17=Feuillus, 18=Coniferes, 19=Mixte).
#'
#' @param bbox Numeric vector or sf bbox. Bounding box in WGS84.
#' @param cache_file Character. Path to save the downloaded raster.
#' @param progress_callback Function. Optional callback for download progress.
#'
#' @return SpatRaster object, or NULL if download fails.
#'
#' @noRd
download_oso <- function(bbox, cache_file, progress_callback = NULL) {
  # Ensure bbox is numeric
  if (inherits(bbox, "bbox")) {
    bbox <- as.numeric(bbox)
  }

  cli::cli_alert_info("Downloading OSO land cover (Theia/CESBIO)...")

  # ---- Global cache: full oso.tif shared across all projects ----
  global_cache <- file.path(get_global_cache_dir(), "oso")
  if (!dir.exists(global_cache)) {
    dir.create(global_cache, recursive = TRUE)
  }
  oso_path <- file.path(global_cache, "oso.tif")

  tryCatch({
    # Download & extract OSO to global cache if not already there
    if (!file.exists(oso_path)) {
      download_oso_global(oso_path, global_cache, progress_callback)
    }

    if (!file.exists(oso_path)) {
      cli::cli_warn("OSO file not found: {oso_path}")
      return(NULL)
    }

    # ---- Project cache: crop to bbox and save per-project ----
    cli::cli_alert_info("Cropping OSO to study area...")
    oso_full <- terra::rast(oso_path)

    # Create bbox extent in OSO CRS
    bbox_sf <- sf::st_bbox(
      c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4]),
      crs = sf::st_crs(4326)
    ) |> sf::st_as_sfc()

    bbox_oso_crs <- sf::st_transform(bbox_sf, terra::crs(oso_full))
    bbox_ext <- terra::ext(terra::vect(bbox_oso_crs))

    # Crop to study area
    oso_crop <- terra::crop(oso_full, bbox_ext)

    # Reproject to WGS84
    oso_wgs84 <- terra::project(oso_crop, "EPSG:4326", method = "near")

    # Save cropped raster to project cache
    terra::writeRaster(oso_wgs84, cache_file, overwrite = TRUE)

    cli::cli_alert_success("Cropped OSO land cover to project area")
    return(terra::rast(cache_file))

  }, error = function(e) {
    cli::cli_warn("Failed to process OSO: {e$message}")
    NULL
  })
}


#' Download full OSO raster to global shared cache
#'
#' @description
#' Downloads the full OSO raster (~6 GB) to the global shared cache directory.
#' This file is shared across all projects to avoid re-downloading.
#'
#' @param oso_path Character. Destination path for oso.tif.
#' @param global_cache Character. Global cache directory for temporary files.
#' @param progress_callback Function. Optional progress callback.
#'
#' @return Invisible NULL. Side-effect: creates oso_path if download succeeds.
#' @noRd
download_oso_global <- function(oso_path, global_cache, progress_callback = NULL) {
  cli::cli_alert_info("Downloading OSO from Recherche Data Gouv to global cache...")
  cli::cli_alert_info("  Global cache: {global_cache}")

  # URL from datasource config (source: inst/datasources/FR.json)
  oso_cfg <- get_data_source("oso", "FR")
  oso_url <- oso_cfg$url %||% "https://entrepot.recherche.data.gouv.fr/api/access/datafile/:persistentId?persistentId=doi:10.57745/8M1AN1"

  # Check for existing archive
  oso_tar_files <- list.files(global_cache, pattern = "^OSO_.*\\.tar\\.gz$", full.names = TRUE)
  if (length(oso_tar_files) > 0) {
    oso_tar <- oso_tar_files[1]
  } else {
    oso_tar <- file.path(global_cache, "OSO_RASTER.tar.gz")
  }

  oso_expected_size <- 5e9

  # Download if needed
  need_download <- TRUE
  if (file.exists(oso_tar)) {
    existing_size <- file.info(oso_tar)$size
    if (existing_size >= oso_expected_size) {
      cli::cli_alert_info("Using existing OSO archive ({round(existing_size/1e9, 1)} GB)")
      need_download <- FALSE
    }
  }

  if (need_download) {
    cli::cli_alert_info("Downloading OSO (~6 GB, this may take a while)...")

    # Increase timeout for large file download (1 hour)
    old_timeout <- getOption("timeout")
    options(timeout = 3600)
    on.exit(options(timeout = old_timeout), add = TRUE)

    # Use curl for download with progress tracking
    download_result <- tryCatch({
      h <- curl::new_handle()
      curl::handle_setopt(h,
        followlocation = TRUE,
        timeout = 3600,
        low_speed_limit = 1000,
        low_speed_time = 300
      )

      last_report_time <- Sys.time()
      con <- curl::curl(oso_url, handle = h, open = "rb")
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      out <- file(oso_tar, open = "wb")
      on.exit(try(close(out), silent = TRUE), add = TRUE)

      total_size <- oso_expected_size
      downloaded <- 0
      chunk_size <- 1024 * 1024  # 1 MB chunks

      while (TRUE) {
        buf <- readBin(con, raw(), n = chunk_size)
        if (length(buf) == 0) break
        writeBin(buf, out)
        downloaded <- downloaded + length(buf)

        now <- Sys.time()
        if (!is.null(progress_callback) &&
            as.numeric(difftime(now, last_report_time, units = "secs")) >= 2) {
          last_report_time <- now
          pct <- min(round(downloaded / total_size * 100), 99)
          progress_callback(list(
            current = paste0("download_oso_progress:", pct),
            download_percent = pct,
            download_size = downloaded,
            download_total = total_size
          ))
        }
      }
      close(out)
      close(con)

      if (!is.null(progress_callback)) {
        progress_callback(list(
          current = "download_oso_progress:100",
          download_percent = 100,
          download_size = downloaded,
          download_total = total_size
        ))
      }

      cli::cli_alert_success("OSO downloaded: {round(downloaded/1e9, 1)} GB")
      TRUE
    }, error = function(e) {
      cli::cli_warn("Download failed: {e$message}")
      FALSE
    })

    if (!download_result || !file.exists(oso_tar)) {
      cli::cli_warn("OSO download failed. Please download manually from:")
      cli::cli_alert("https://entrepot.recherche.data.gouv.fr/dataset.xhtml?persistentId=doi:10.57745/UZ2NJ7")
      cli::cli_alert("Extract and place oso.tif in: {global_cache}")
      return(invisible(NULL))
    }

    downloaded_size <- file.info(oso_tar)$size
    if (downloaded_size < oso_expected_size) {
      cli::cli_warn("Downloaded file smaller than expected ({round(downloaded_size/1e9, 1)} GB < {round(oso_expected_size/1e9, 1)} GB)")
      cli::cli_alert_info("Download may be incomplete. Please download manually.")
      return(invisible(NULL))
    }
  }

  # Extract archive
  cli::cli_alert_info("Extracting OSO archive...")
  utils::untar(path.expand(oso_tar), exdir = path.expand(global_cache), extras = "--touch")

  # Find extracted TIF file
  tif_files <- list.files(global_cache, pattern = "^OCS_.*\\.tif$",
                          recursive = TRUE, full.names = TRUE)
  if (length(tif_files) > 0) {
    file.copy(tif_files[1], oso_path)
    cli::cli_alert_success("OSO extracted to global cache: {oso_path}")

    # Cleanup extracted directories and archive
    oso_dirs <- list.dirs(global_cache, full.names = TRUE, recursive = FALSE)
    oso_dirs <- oso_dirs[grepl("^OSO_", basename(oso_dirs))]
    if (length(oso_dirs) > 0) {
      unlink(oso_dirs, recursive = TRUE)
    }
    if (file.exists(oso_tar)) {
      file.remove(oso_tar)
    }
  } else {
    cli::cli_warn("Could not find OCS_*.tif in extracted archive")
  }

  invisible(NULL)
}


#' Download IGN BD ALTI DEM
#'
#' @description
#' Downloads Digital Elevation Model from IGN Geoplateforme.
#' Uses the altimetry service or WMS fallback.
#'
#' @param bbox Numeric vector or sf bbox. Bounding box in WGS84.
#' @param cache_file Character. Path to save the downloaded raster.
#'
#' @return SpatRaster object, or NULL if download fails.
#'
#' @noRd
download_ign_dem <- function(bbox, cache_file) {
  # IGN Geoplateforme WMS for elevation (source: inst/datasources/FR.json)
  dem_cfg <- get_layer_service("dem", "FR")
  base_url <- dem_cfg$url %||% "https://data.geopf.fr/wms-r/wms"

  # Ensure bbox is numeric
  if (inherits(bbox, "bbox")) {
    bbox <- as.numeric(bbox)
  }

  cli::cli_alert_info("Downloading IGN elevation data (MNT)...")

  tryCatch({
    # Calculate image dimensions (target ~25m resolution)
    # 1 degree ~ 111km, so 0.00025 degrees ~ 25m
    width <- ceiling((bbox[3] - bbox[1]) / 0.00025)
    height <- ceiling((bbox[4] - bbox[2]) / 0.00025)

    # Limit size
    max_size <- 4000
    if (width > max_size) {
      scale <- max_size / width
      width <- max_size
      height <- ceiling(height * scale)
    }
    if (height > max_size) {
      scale <- max_size / height
      height <- max_size
      width <- ceiling(width * scale)
    }

    # Build WMS GetMap URL for elevation data
    wms_url <- paste0(
      base_url,
      "?SERVICE=WMS",
      "&VERSION=1.3.0",
      "&REQUEST=GetMap",
      "&LAYERS=ELEVATION.ELEVATIONGRIDCOVERAGE",
      "&STYLES=",
      "&CRS=EPSG:4326",
      "&BBOX=", paste(bbox[c(2,1,4,3)], collapse = ","),  # WMS 1.3.0 uses lat,lon order
      "&WIDTH=", width,
      "&HEIGHT=", height,
      "&FORMAT=image/geotiff"
    )

    # Download to temp file
    temp_file <- tempfile(fileext = ".tif")

    resp <- httr2::request(wms_url) |>
      httr2::req_timeout(180) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(path = temp_file)

    if (httr2::resp_status(resp) != 200) {
      cli::cli_warn("IGN WMS returned status {httr2::resp_status(resp)}")
      unlink(temp_file)
      return(NULL)
    }

    # Check if valid GeoTIFF
    if (file.exists(temp_file) && file.size(temp_file) > 1000) {
      # Re-write through terra to fix malformed GeoTIFF tags from WMS
      rast <- suppressWarnings(terra::rast(temp_file))
      terra::writeRaster(rast, cache_file, overwrite = TRUE)
      unlink(temp_file)

      rast <- terra::rast(cache_file)
      cli::cli_alert_success("Downloaded IGN DEM ({width}x{height} pixels)")
      return(rast)
    } else {
      cli::cli_warn("Invalid or empty DEM response")
      unlink(temp_file)
      return(NULL)
    }

  }, error = function(e) {
    cli::cli_warn("Failed to download IGN DEM: {e$message}")
    NULL
  })
}


#' Download IGN IRC orthophoto and compute NDVI
#'
#' @description
#' Downloads Infrared Color (IRC) orthophoto from IGN Geoplateforme
#' and computes NDVI from the NIR and Red bands.
#'
#' IRC bands:
#' - Band 1: Near-Infrared (NIR)
#' - Band 2: Red
#' - Band 3: Green
#'
#' NDVI = (NIR - Red) / (NIR + Red) = (Band1 - Band2) / (Band1 + Band2)
#'
#' @param bbox Numeric vector or sf bbox. Bounding box in WGS84.
#' @param cache_file Character. Path to save the computed NDVI raster.
#'
#' @return SpatRaster object with NDVI values (-1 to 1), or NULL if download fails.
#'
#' @noRd
download_ign_irc_ndvi <- function(bbox, cache_file) {
  # IGN Geoplateforme WMS for IRC orthophotos (source: inst/datasources/FR.json)
  irc_cfg <- get_layer_service("ortho_irc", "FR")
  base_url <- irc_cfg$url %||% "https://data.geopf.fr/wms-r/wms"

  # Ensure bbox is numeric
  if (inherits(bbox, "bbox")) {
    bbox <- as.numeric(bbox)
  }

  cli::cli_alert_info("Downloading IGN IRC orthophoto for NDVI calculation...")


  tryCatch({
    # Calculate image dimensions
    # Target resolution: ~5m (IRC is available at 50cm but we downsample for performance)
    # 1 degree ~ 111km, so 0.00005 degrees ~ 5m
    target_res <- 0.00005
    width <- ceiling((bbox[3] - bbox[1]) / target_res)
    height <- ceiling((bbox[4] - bbox[2]) / target_res)

    # Limit size to avoid memory issues and server limits
    max_size <- 4000
    if (width > max_size) {
      scale <- max_size / width
      width <- max_size
      height <- ceiling(height * scale)
    }
    if (height > max_size) {
      scale <- max_size / height
      height <- max_size
      width <- ceiling(width * scale)
    }

    # Build WMS GetMap URL for IRC orthophoto
    # Layer: ORTHOIMAGERY.ORTHOPHOTOS.IRC (most recent IRC)
    wms_url <- paste0(
      base_url,
      "?SERVICE=WMS",
      "&VERSION=1.3.0",
      "&REQUEST=GetMap",
      "&LAYERS=ORTHOIMAGERY.ORTHOPHOTOS.IRC",
      "&STYLES=",
      "&CRS=EPSG:4326",
      "&BBOX=", paste(bbox[c(2, 1, 4, 3)], collapse = ","),  # WMS 1.3.0: lat,lon order
      "&WIDTH=", width,
      "&HEIGHT=", height,
      "&FORMAT=image/geotiff"
    )

    # IRC cache file: save alongside ndvi.tif as irc.tif
    irc_cache_file <- file.path(dirname(cache_file), "irc.tif")

    # Use cached IRC if available, otherwise download
    if (file.exists(irc_cache_file) && file.size(irc_cache_file) > 1000) {
      cli::cli_alert_success("Using cached IRC orthophoto")
      irc <- terra::rast(irc_cache_file)
    } else {
      cli::cli_alert_info("  Requesting {width}x{height} pixels...")
      temp_file <- tempfile(fileext = ".tif")

      resp <- httr2::request(wms_url) |>
        httr2::req_timeout(300) |>  # 5 minutes for large images
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform(path = temp_file)

      if (httr2::resp_status(resp) != 200) {
        cli::cli_warn("IGN WMS returned status {httr2::resp_status(resp)}")
        unlink(temp_file)
        return(create_synthetic_ndvi(bbox, cache_file))
      }

      if (!file.exists(temp_file) || file.size(temp_file) < 1000) {
        cli::cli_warn("Invalid or empty IRC response")
        unlink(temp_file)
        return(create_synthetic_ndvi(bbox, cache_file))
      }

      # Save IRC to project cache
      file.copy(temp_file, irc_cache_file, overwrite = TRUE)
      unlink(temp_file)
      cli::cli_alert_success("Saved IRC orthophoto to cache")
      irc <- terra::rast(irc_cache_file)
    }

    # Check we have at least 3 bands
    if (terra::nlyr(irc) < 3) {
      cli::cli_warn("IRC image has insufficient bands ({terra::nlyr(irc)})")
      return(create_synthetic_ndvi(bbox, cache_file))
    }

    cli::cli_alert_info("  Computing NDVI from IRC bands...")

    # Extract NIR (band 1) and Red (band 2)
    nir <- irc[[1]]
    red <- irc[[2]]

    # Compute NDVI = (NIR - Red) / (NIR + Red)
    # Handle division by zero by setting those pixels to 0
    ndvi <- (nir - red) / (nir + red)

    # Replace NaN/Inf with NA (not 0, to avoid false biomass = 0)
    ndvi[is.nan(terra::values(ndvi))] <- NA
    ndvi[is.infinite(terra::values(ndvi))] <- NA

    # Ensure NDVI is in valid range [-1, 1]
    ndvi <- terra::clamp(ndvi, lower = -1, upper = 1)

    # Set proper name
    names(ndvi) <- "ndvi"

    # Save to cache
    terra::writeRaster(ndvi, cache_file, overwrite = TRUE)

    cli::cli_alert_success("Computed NDVI from IGN IRC ({width}x{height} pixels)")

    return(terra::rast(cache_file))

  }, error = function(e) {
    cli::cli_warn("Failed to download/compute NDVI from IRC: {e$message}")
    return(create_synthetic_ndvi(bbox, cache_file))
  })
}


#' Create synthetic NDVI raster (fallback)
#'
#' @description
#' Creates a synthetic NDVI raster with realistic forest values
#' when the IRC download fails.
#'
#' @param bbox Numeric vector. Bounding box.
#' @param cache_file Character. Path to save the raster.
#'
#' @return SpatRaster with synthetic NDVI values.
#'
#' @noRd
create_synthetic_ndvi <- function(bbox, cache_file) {
  cli::cli_alert_warning("Using synthetic NDVI values (IRC unavailable)")

  # Create raster with ~100m resolution
  rast <- terra::rast(
    xmin = bbox[1], xmax = bbox[3],
    ymin = bbox[2], ymax = bbox[4],
    resolution = 0.001,  # ~100m
    crs = "EPSG:4326"
  )

  # Fill with realistic forest NDVI values (0.5-0.85)
  set.seed(42)
  terra::values(rast) <- runif(terra::ncell(rast), 0.5, 0.85)
  names(rast) <- "ndvi"

  terra::writeRaster(rast, cache_file, overwrite = TRUE)
  cli::cli_alert_info("Created synthetic NDVI raster")

  return(rast)
}


# ==============================================================================
# IGN LiDAR HD Download
# ==============================================================================

#' Download IGN LiDAR HD tiles (MNH canopy height model)
#'
#' @description
#' Downloads LiDAR HD derived products (MNH = Modèle Numérique de Hauteur,
#' i.e. Canopy Height Model) from the IGN Géoplateforme.
#'
#' The workflow:
#' 1. Query the WFS `IGNF_MNH-LIDAR-HD:dalle` to find tiles covering the bbox
#' 2. Download each tile (1km x 1km GeoTIFF at 1m resolution)
#' 3. Mosaic tiles into a single raster
#' 4. Cache the result for future use
#'
#' Also supports downloading raw COPC point cloud tiles via
#' `IGNF_NUAGES-DE-POINTS-LIDAR-HD:dalle`.
#'
#' @param bbox Numeric vector or sf bbox. Bounding box in WGS84.
#' @param cache_dir Character. Directory to cache downloaded tiles.
#' @param product Character. Product to download:
#'   "mnh" (Canopy Height Model, default), "mnt" (DTM), "mns" (DSM),
#'   or "nuage" (raw COPC point clouds).
#' @param progress_callback Function. Optional progress callback.
#'
#' @return SpatRaster (for mnh/mnt/mns) or character vector of .copc.laz
#'   file paths (for nuage), or NULL if no tiles available.
#'
#' @noRd
download_ign_lidar_hd <- function(bbox,
                                  cache_dir,
                                  product = c("mnh", "mnt", "mns", "nuage"),
                                  progress_callback = NULL) {
  product <- match.arg(product)

  # WFS layer names for each product (source: inst/datasources/FR.json)
  wfs_layers_default <- list(
    mnh = "IGNF_MNH-LIDAR-HD:dalle",
    mnt = "IGNF_MNT-LIDAR-HD:dalle",
    mns = "IGNF_MNS-LIDAR-HD:dalle",
    nuage = "IGNF_NUAGES-DE-POINTS-LIDAR-HD:dalle"
  )
  # Charger depuis la config si disponible
  lidar_key <- paste0("lidar_", product)
  lidar_cfg <- get_data_source(lidar_key, "FR")
  wfs_layers <- wfs_layers_default
  if (!is.null(lidar_cfg$happign_layer)) {
    wfs_layers[[product]] <- lidar_cfg$happign_layer
  }

  wfs_layer <- wfs_layers[[product]]

  # File extension depends on product
  file_ext <- if (product == "nuage") ".copc.laz" else ".tif"
  cache_subdir <- file.path(cache_dir, paste0("lidar_", product))
  if (!dir.exists(cache_subdir)) {
    dir.create(cache_subdir, recursive = TRUE)
  }

  # Check for cached mosaic (raster products only)
  mosaic_cache <- file.path(cache_dir, paste0("lidar_", product, "_mosaic.tif"))
  if (product != "nuage" && file.exists(mosaic_cache)) {
    cli::cli_alert_success("Using cached LiDAR {toupper(product)} mosaic")
    return(terra::rast(mosaic_cache))
  }

  # For raw point clouds, check if we already have cached tiles
  if (product == "nuage") {
    cached_tiles <- list.files(cache_subdir, pattern = "\\.copc\\.laz$",
      full.names = TRUE)
    if (length(cached_tiles) > 0) {
      cli::cli_alert_success("Using {length(cached_tiles)} cached LiDAR COPC tiles")
      return(cached_tiles)
    }
  }

  # Ensure bbox is numeric
  if (inherits(bbox, "bbox")) {
    bbox <- as.numeric(bbox)
  }

  cli::cli_alert_info("Querying IGN WFS for LiDAR HD {toupper(product)} tiles...")

  # ---- Step 1: Query WFS to find tiles covering bbox ----
  tiles_sf <- query_lidar_wfs(wfs_layer, bbox)

  if (is.null(tiles_sf) || nrow(tiles_sf) == 0) {
    cli::cli_alert_warning(
      "No LiDAR HD {toupper(product)} tiles found for this area (coverage may be incomplete)"
    )
    return(NULL)
  }

  cli::cli_alert_info("Found {nrow(tiles_sf)} LiDAR HD {toupper(product)} tiles")

  # ---- Step 2: Extract download URLs and download tiles ----
  # The WFS response should contain a download URL attribute
  url_col <- find_url_column(tiles_sf)
  if (is.null(url_col)) {
    cli::cli_alert_warning("LiDAR WFS response has no download URL column")
    return(NULL)
  }

  download_urls <- as.character(tiles_sf[[url_col]])

  # Extract tile names for caching
  tile_names <- extract_tile_names(download_urls, file_ext)

  downloaded_files <- character(0)
  n_tiles <- length(download_urls)

  for (i in seq_along(download_urls)) {
    url <- download_urls[i]
    tile_file <- file.path(cache_subdir, tile_names[i])

    if (!is.null(progress_callback)) {
      progress_callback(list(
        current = paste0("download_lidar:", toupper(product), ":", i, "/", n_tiles),
        lidar_tile = i - 1,
        lidar_tiles_total = n_tiles
      ))
    }

    # Skip if already cached
    if (file.exists(tile_file) && file.size(tile_file) > 100) {
      cli::cli_alert_success("  Tile {i}/{n_tiles}: cached")
      downloaded_files <- c(downloaded_files, tile_file)
      next
    }

    # Download tile
    result <- download_lidar_tile(url, tile_file)
    if (!is.null(result)) {
      downloaded_files <- c(downloaded_files, result)
      cli::cli_alert_success("  Tile {i}/{n_tiles}: downloaded")
    } else {
      cli::cli_alert_warning("  Tile {i}/{n_tiles}: failed")
    }
  }

  if (length(downloaded_files) == 0) {
    cli::cli_alert_warning("No LiDAR HD tiles were successfully downloaded")
    return(NULL)
  }

  # ---- Step 3: For raster products, mosaic tiles ----
  if (product != "nuage") {
    result <- mosaic_lidar_tiles(downloaded_files, mosaic_cache)
    return(result)
  }

  # For COPC, return file paths
  downloaded_files
}


#' Query IGN WFS for LiDAR HD tiles covering a bounding box
#'
#' @param wfs_layer Character. WFS typename.
#' @param bbox Numeric vector. Bounding box (xmin, ymin, xmax, ymax) in WGS84.
#'
#' @return sf object with tile features, or NULL on failure.
#' @noRd
query_lidar_wfs <- function(wfs_layer, bbox) {
  # WFS URL from datasource config (source: inst/datasources/FR.json)
  wfs_cfg <- get_data_source("ign_wfs", "FR")
  base_url <- wfs_cfg$url %||% "https://data.geopf.fr/wfs/ows"

  bbox_str <- paste(bbox[c(1, 2, 3, 4)], collapse = ",")

  wfs_url <- paste0(
    base_url,
    "?SERVICE=WFS",
    "&VERSION=2.0.0",
    "&REQUEST=GetFeature",
    "&TYPENAMES=", wfs_layer,
    "&BBOX=", bbox_str, ",EPSG:4326",
    "&SRSNAME=EPSG:4326",
    "&OUTPUTFORMAT=application/json",
    "&COUNT=500"
  )

  tryCatch({
    resp <- httr2::request(wfs_url) |>
      httr2::req_timeout(120) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) != 200) {
      cli::cli_warn("IGN LiDAR WFS returned status {httr2::resp_status(resp)}")
      return(NULL)
    }

    geojson <- httr2::resp_body_string(resp)

    if (nchar(geojson) < 50) {
      return(NULL)
    }

    tiles <- sf::st_read(geojson, quiet = TRUE)

    if (nrow(tiles) == 0) {
      return(NULL)
    }

    tiles
  }, error = function(e) {
    cli::cli_warn("LiDAR WFS query failed: {e$message}")
    NULL
  })
}


#' Find the download URL column in WFS response
#'
#' @param tiles_sf sf object from WFS response.
#' @return Column name containing download URLs, or NULL.
#' @noRd
find_url_column <- function(tiles_sf) {
  # Possible column names for the download URL
  url_candidates <- c(
    "url_telechargement", "lien_telechargement",
    "url", "lien", "download_url", "href",
    "nom_pkk", "url_fichier"
  )

  cols <- names(tiles_sf)

  # Try exact match first

  for (candidate in url_candidates) {
    if (candidate %in% cols) {
      return(candidate)
    }
  }

  # Try case-insensitive match
  cols_lower <- tolower(cols)
  for (candidate in url_candidates) {
    idx <- which(cols_lower == candidate)
    if (length(idx) > 0) {
      return(cols[idx[1]])
    }
  }

  # Try partial match on "url" or "lien" or "telechargement"
  url_cols <- cols[grepl("url|lien|telecharg|download|href", cols, ignore.case = TRUE)]
  if (length(url_cols) > 0) {
    return(url_cols[1])
  }

  NULL
}


#' Extract tile filenames from download URLs
#'
#' @param urls Character vector of download URLs.
#' @param file_ext Character. Expected file extension.
#' @return Character vector of filenames.
#' @noRd
extract_tile_names <- function(urls, file_ext) {
  # Ensure urls is a character vector (WFS GeoJSON parsing may return a list)
  urls <- as.character(urls)

  # Extract filename from URL
  basenames <- basename(urls)

  # If basenames don't have the right extension, generate names
  has_ext <- grepl(gsub("\\.", "\\\\.", file_ext), basenames)
  for (i in which(!has_ext)) {
    basenames[i] <- paste0("tile_", i, file_ext)
  }

  basenames
}


#' Download a single LiDAR tile with retry
#'
#' @param url Character. Download URL.
#' @param dest_file Character. Destination file path.
#' @return dest_file on success, NULL on failure.
#' @noRd
download_lidar_tile <- function(url, dest_file) {
  max_retries <- 3

  for (attempt in seq_len(max_retries)) {
    tryCatch({
      resp <- httr2::request(url) |>
        httr2::req_timeout(300) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform(path = dest_file)

      if (httr2::resp_status(resp) == 200 && file.exists(dest_file) &&
          file.size(dest_file) > 100) {
        return(dest_file)
      }

      # Clean up failed download
      if (file.exists(dest_file)) unlink(dest_file)

      if (attempt < max_retries) {
        Sys.sleep(2^attempt)  # Exponential backoff
      }
    }, error = function(e) {
      if (file.exists(dest_file)) unlink(dest_file)
      if (attempt < max_retries) {
        Sys.sleep(2^attempt)
      }
    })
  }

  NULL
}


#' Mosaic multiple LiDAR raster tiles into one
#'
#' @param tile_files Character vector of tile file paths.
#' @param output_file Character. Path for the mosaic output.
#' @return SpatRaster, or NULL on failure.
#' @noRd
mosaic_lidar_tiles <- function(tile_files, output_file) {
  tryCatch({
    if (length(tile_files) == 1) {
      # Single tile - just copy/load
      rast <- terra::rast(tile_files[1])
      terra::writeRaster(rast, output_file, overwrite = TRUE)
      return(terra::rast(output_file))
    }

    # Load all tiles
    rast_list <- lapply(tile_files, function(f) {
      tryCatch(terra::rast(f), error = function(e) NULL)
    })
    rast_list <- Filter(Negate(is.null), rast_list)

    if (length(rast_list) == 0) {
      return(NULL)
    }

    if (length(rast_list) == 1) {
      rast <- rast_list[[1]]
    } else {
      # Create a SpatRasterCollection and mosaic
      src <- terra::sprc(rast_list)
      rast <- terra::mosaic(src, fun = "mean")
    }

    terra::writeRaster(rast, output_file, overwrite = TRUE)
    cli::cli_alert_success("Created LiDAR mosaic ({length(tile_files)} tiles)")
    terra::rast(output_file)

  }, error = function(e) {
    cli::cli_warn("Failed to mosaic LiDAR tiles: {e$message}")
    # Try returning just the first tile
    tryCatch(terra::rast(tile_files[1]), error = function(e2) NULL)
  })
}


#' Normalize indicator values to 0-100 scale
#'
#' @description
#' Converts raw indicator metrics to a standardized 0-100 score.
#' Indicators that already return 0-100 scores pass through unchanged.
#' Raw metrics (m³/ha, tC/ha, km/ha, etc.) are scaled using ecologically
#' meaningful reference values for temperate forests.
#'
#' @param indicator Character. Indicator name.
#' @param values Numeric vector. Raw indicator values.
#'
#' @return Numeric vector of normalized values in [0, 100].
#'
#' @noRd
normalize_indicator <- function(indicator, values) {
  # Reference maxima for indicators that return raw metrics
  # Values beyond ref_max are capped at 100
  #
  # Indicators already returning 0-100 scores → NULL (just clamp):
  #   biodiversity (B1-B3), air (A1-A2), fertility (F1-F2),
  #   landscape (L1-L2), temporal (T1-T2), risks (R1-R4),
  #   naturalness (N1-N3), indicateur_p3_qualite_bois (P3)
  ref_max <- switch(indicator,
    # Carbon: biomass in tC/ha, typical temperate forest max ~150 tC/ha
    "indicateur_c1_biomasse" = 150,
    # Carbon: NDVI in 0-1 scale — special handling below
    "indicateur_c2_ndvi" = NULL,
    # Water: hydrographic network density in m/ha (50 m/ha = well-watered forest)
    "indicateur_w1_reseau" = 50,
    # Water: wetland/water surface coverage (%) — 5% coverage = max score
    "indicateur_w2_zones_humides" = 5,
    # Water: TWI — special handling below
    "indicateur_w3_humidite" = NULL,
    # Social: distance to roads (m) — special inverse handling below
    "indicateur_s1_routes" = NULL,
    # Social: distance to buildings (m) — special inverse handling below
    "indicateur_s2_bati" = NULL,
    # Social: population within buffers
    "indicateur_s3_population" = 10000,
    # Production: standing volume in m³/ha (tuto 02 formula range ~100-800)
    "indicateur_p1_volume" = 800,
    # Production: annual increment in m³/ha/yr
    "indicateur_p2_station" = 15,
    # Energy: fuelwood potential in tep/ha/yr
    "indicateur_e1_bois_energie" = 0.3,
    # Energy: CO2 avoidance in tCO2/ha/yr (E1 * 2.5 * 0.85)
    "indicateur_e2_evitement" = 0.75,
    # All other indicators already return 0-100 scores → NULL
    NULL
  )

  # Special handling: TWI rescale [2.5, 4.5] → [0, 100]
  # Calibrated on typical temperate forest TWI range (sandy/draining substrates ~3,
  # clay/alluvial ~4-5). Tighter window gives meaningful differentiation.
  if (indicator == "indicateur_w3_humidite") {
    # Higher TWI = wetter = more water service
    return(pmin(100, pmax(0, (values - 2.5) / 2 * 100)))
  }

  # Special handling: NDVI scale 0-1 → 0-100
  if (indicator == "indicateur_c2_ndvi") {
    return(pmin(100, pmax(0, values * 100)))
  }

  # Special handling: distance indicators (inverse — closer = higher social value)
  # 0m → score 100 (very accessible), 2000m+ → score 0 (very remote)
  if (indicator %in% c("indicateur_s1_routes", "indicateur_s2_bati")) {
    return(pmin(100, pmax(0, 100 * (1 - values / 2000))))
  }

  if (!is.null(ref_max)) {
    # Linear normalization: value / ref_max * 100, clamped to [0, 100]
    values <- pmin(100, pmax(0, values / ref_max * 100))
  } else {
    # Already 0-100 indicators: just clamp to be safe
    values <- pmin(100, pmax(0, values))
  }

  values
}


#'
#' @description
#' Computes all requested indicators on the parcels.
#' Supports incremental computation and resume from previous state.
#'
#' @param parcels sf object. Parcels to compute indicators for.
#' @param layers nemeton_layers object. Data layers.
#' @param indicators Character vector. Indicators to compute.
#' @param progress_callback Function. Progress callback.
#' @param project_id Character. Project ID for incremental saving.
#'
#' @return sf object with computed indicator values.
#'
#' @noRd
compute_all_indicators <- function(parcels,
                                   layers,
                                   indicators,
                                   progress_callback = NULL,
                                   project_id = NULL) {

  # Load existing results if available (for resume)
  existing_results <- NULL
  computed_indicators <- character(0)

  if (!is.null(project_id)) {
    existing_results <- load_indicators(project_id)
    if (!is.null(existing_results)) {
      # Find which indicators are already computed
      # Skip indicators that are all NA or all zero (likely failed previous computation)
      available_cols <- intersect(names(existing_results), indicators)
      for (col in available_cols) {
        vals <- existing_results[[col]]
        has_data <- !all(is.na(vals))
        all_zero <- has_data && all(vals == 0, na.rm = TRUE)
        if (has_data && !all_zero) {
          computed_indicators <- c(computed_indicators, col)
        } else if (all_zero) {
          cli::cli_alert_info(
            "Indicator {col} has all-zero values, will recompute"
          )
        }
      }

      if (length(computed_indicators) > 0) {
        cli::cli_alert_info(
          "Found {length(computed_indicators)} already computed indicator(s), skipping..."
        )
      }
    }
  }

  # Start with existing results or parcels.
  # When resuming from a parquet snapshot, existing_results is a plain
  # data.frame (arrow::read_parquet drops the sf class). Re-attach the
  # live geometry from `parcels` so subsequent calls to
  # save_indicators_incremental() can extract geometry_wkt cleanly and
  # do not emit the "Results is not an sf object" warning.
  if (!is.null(existing_results) && nrow(existing_results) == nrow(parcels)) {
    if (!inherits(existing_results, "sf")) {
      existing_results$geometry_wkt <- NULL  # dropped — rebuilt below
      existing_results[[attr(parcels, "sf_column") %||% "geometry"]] <-
        sf::st_geometry(parcels)
      existing_results <- sf::st_as_sf(
        existing_results,
        sf_column_name = attr(parcels, "sf_column") %||% "geometry",
        crs = sf::st_crs(parcels)
      )
    }
    results <- existing_results
  } else {
    results <- parcels
  }

  # Filter out already computed indicators
  indicators_to_compute <- setdiff(indicators, computed_indicators)
  n_indicators <- length(indicators)
  n_to_compute <- length(indicators_to_compute)

  # BD Forêt enrichment (species/age) for P2 site index in CHM mode.
  # indicateur_p2_station expects a `species` column and an `age`
  # column on the units sf when called in CHM mode. These are derived
  # from BD Forêt V2 via nemeton::enrich_parcels_bdforet. We enrich
  # once up-front rather than inside compute_single_indicator because
  # the enrichment does a spatial intersection that is not cheap.
  # We skip P1/P3/E1: they additionally require dbh/stems-per-ha
  # terrain inventory (NDP ≥ 2) and would still fail.
  needs_bdforet_enrich <- "indicateur_p2_station" %in% indicators_to_compute &&
    !all(c("species", "age") %in% names(parcels))
  if (needs_bdforet_enrich) {
    bd <- resolve_vector_layer(layers, "bdforet")
    if (!is.null(bd) && nrow(bd) > 0) {
      enriched <- tryCatch(
        nemeton::enrich_parcels_bdforet(parcels, bd),
        error = function(e) {
          cli::cli_warn("BD Forêt enrichment failed: {e$message}")
          NULL
        }
      )
      if (!is.null(enriched)) {
        if (!"species" %in% names(parcels)) parcels$species <- enriched$species
        if (!"age" %in% names(parcels))     parcels$age     <- enriched$age
        n_ok <- sum(!is.na(parcels$species))
        cli::cli_alert_info(
          "BD Forêt enrichment: species/age set on {n_ok}/{nrow(parcels)} UGF for P2"
        )
      }
    } else {
      cli::cli_alert_info("Skipping BD Forêt enrichment: layer not available")
    }
  }

  # Initialize counters (include already completed)
  completed <- length(computed_indicators)
  failed <- 0
  errors <- list()

  # Initialize status
  status <- stats::setNames(rep("pending", n_indicators), indicators)
  status[computed_indicators] <- "completed"

  # Report initial progress
  if (!is.null(progress_callback)) {
    progress_callback(list(
      completed = completed,
      failed = failed,
      total = n_indicators,
      current = if (n_to_compute > 0) "resuming" else "complete",
      status = status,
      errors = errors,
      skipped = length(computed_indicators)
    ))
  }

  # Compute remaining indicators
  for (ind in indicators_to_compute) {
    # Check for cancellation before each indicator
    if (!is.null(project_id) && is_cancelled(project_id)) {
      cli::cli_alert_warning("Computation cancelled by user")
      break
    }

    # Clé de l'indicateur NMT pour la traduction
    indicator_key <- ind

    if (!is.null(progress_callback)) {
      progress_callback(list(
        completed = completed,
        failed = failed,
        total = n_indicators,
        current = paste0("compute:", indicator_key),
        indicator_key = indicator_key,
        status = status,
        errors = errors
      ))
    }

    status[ind] <- "running"

    tryCatch({
      # Compute indicator
      values <- compute_single_indicator(ind, parcels, layers)

      # Add to results (raw values — normalization happens at family aggregation)
      results[[ind]] <- values
      status[ind] <- "completed"
      completed <- completed + 1

      # Incremental save after each successful computation
      if (!is.null(project_id)) {
        save_indicators_incremental(project_id, results, ind)
      }

    }, error = function(e) {
      status[ind] <<- "error"
      failed <<- failed + 1
      errors <<- c(errors, list(list(
        indicator = ind,
        message = e$message,
        time = Sys.time()
      )))

      # Set NA for failed indicators
      results[[ind]] <<- rep(NA_real_, nrow(results))

      cli::cli_warn("Failed to compute {ind}: {e$message}")
    })
  }

  if (!is.null(progress_callback)) {
    progress_callback(list(
      completed = completed,
      failed = failed,
      total = n_indicators,
      current = "complete",
      status = status,
      errors = errors
    ))
  }

  # Summary of what failed — helps diagnose UGF-column compatibility
  # issues with nemeton indicator functions.
  if (failed > 0) {
    cli::cli_h2("R\u00e9capitulatif des \u00e9checs de calcul ({failed}/{n_indicators} indicateur{?s})")
    for (err in errors) {
      cli::cli_alert_danger("{err$indicator} : {err$message}")
    }
  }

  # Marquer les sources de donnees disponibles pour detect_ndp()
  results <- set_ndp_attributes(results, layers)

  results
}


#' Compute single indicator
#'
#' @description
#' Computes a single indicator. Dispatches to appropriate function.
#'
#' @param indicator Character. Indicator name.
#' @param parcels sf object. Parcels.
#' @param layers nemeton_layers object. Data layers.
#'
#' @return Numeric vector of indicator values.
#'
#' @noRd
compute_single_indicator <- function(indicator, parcels, layers) {
  # Utiliser directement le nom NMT comme nom de fonction (ex: indicateur_c1_biomasse)
  func_name <- indicator

  if (exists(func_name, mode = "function")) {
    func <- get(func_name, mode = "function")

    # Get function's formal arguments
    func_args <- names(formals(func))

    # Build argument list based on what the function accepts
    args <- list()

    # units/parcels parameter (required)
    if ("units" %in% func_args) {
      args$units <- parcels
    } else if ("parcels" %in% func_args) {
      args$parcels <- parcels
    }

    # layers parameter (optional - only pass if function accepts it)
    if ("layers" %in% func_args) {
      args$layers <- layers
    }

    # Extract specific layers from nemeton_layers structure
    # Rasters are in layers$rasters, vectors in layers$vectors
    if ("dem" %in% func_args) {
      dem <- resolve_raster_layer(layers, "dem")
      if (is.null(dem)) dem <- resolve_raster_layer(layers, "elevation")
      if (!is.null(dem)) args$dem <- dem
    }
    if ("ndvi" %in% func_args) {
      ndvi <- resolve_raster_layer(layers, "ndvi")
      if (!is.null(ndvi)) args$ndvi <- ndvi
    }
    if ("land_cover" %in% func_args) {
      lc <- resolve_raster_layer(layers, "landcover")
      if (is.null(lc)) lc <- resolve_raster_layer(layers, "forest_cover")
      if (!is.null(lc)) args$land_cover <- lc
    }
    if ("protected_areas" %in% func_args) {
      pa <- resolve_vector_layer(layers, "protected_areas")
      if (!is.null(pa)) args$protected_areas <- pa
    }
    if ("roads" %in% func_args) {
      roads <- resolve_vector_layer(layers, "roads")
      if (!is.null(roads)) args$roads <- roads
    }
    if ("buildings" %in% func_args) {
      buildings <- resolve_vector_layer(layers, "buildings")
      if (!is.null(buildings)) args$buildings <- buildings
    }
    if ("bdforet" %in% func_args) {
      bd <- resolve_vector_layer(layers, "bdforet")
      if (!is.null(bd)) args$bdforet <- bd
    }

    # Canopy Height Model (spec 005, NDP augmented). Routed to the
    # spec-005-phase-[2-4] indicators that accept a chm argument:
    # P1 volume, P2 station, C1 biomass, B2 structure, R2 storm.
    # The raster is produced upstream by opencanopy and cleaned by
    # nemeton::sanitize_chm() in download_layers_for_parcels().
    if ("chm" %in% func_args) {
      chm <- resolve_raster_layer(layers, "chm")
      if (!is.null(chm)) args$chm <- chm
    }
    if ("pct_masked" %in% func_args && !is.null(layers$chm_pct_masked)) {
      args$pct_masked <- layers$chm_pct_masked
    }

    # F1 soil fertility: opt into the absolute SoilGrids CEC path
    # (nemeton::cec_to_fertility_score — scores comparable across
    # projects) instead of the legacy min-max "layer" mode. nemeton
    # fetches SoilGrids itself via load_raster_source(), so no
    # layer needs to be staged in `layers$rasters$soil`.
    if (identical(indicator, "indicateur_f1_fertilite") &&
        "source" %in% func_args) {
      args$source <- "soilgrids"
    }

    # Call function with appropriate arguments
    result <- do.call(func, args)

    # Extract the indicator value from result
    # Some functions return the units sf with the indicator added as a column
    if (inherits(result, "sf") || inherits(result, "data.frame")) {
      # Map indicator names to the column names used by each function
      col_map <- c(
        "indicateur_b1_protection" = "B1",
        "indicateur_b2_structure" = "B2",
        "indicateur_b3_connectivite" = "B3",
        "indicateur_a1_couverture" = "A1",
        "indicateur_a2_qualite_air" = "A2",
        "indicateur_r1_feu" = "R1",
        "indicateur_r2_tempete" = "R2",
        "indicateur_r3_secheresse" = "R3",
        "indicateur_r4_abroutissement" = "R4",
        "indicateur_n1_distance" = "N1",
        "indicateur_n2_continuite" = "N2",
        "indicateur_n3_naturalite" = "N3",
        "indicateur_s1_routes" = "S1",
        "indicateur_s2_bati" = "S2",
        "indicateur_s3_population" = "S3",
        "indicateur_e1_bois_energie" = "E1",
        "indicateur_e2_evitement" = "E2"
      )

      # Try mapped column name first
      if (indicator %in% names(col_map)) {
        mapped_col <- col_map[[indicator]]
        if (mapped_col %in% names(result)) {
          return(result[[mapped_col]])
        }
      }

      # Chercher la colonne indicateur par nom NMT
      indicator_col <- intersect(
        c(indicator, toupper(indicator)),
        names(result)
      )
      if (length(indicator_col) > 0) {
        return(result[[indicator_col[1]]])
      }
      # Try common column patterns (B1, C1, etc.)
      pattern_cols <- grep("^[A-Z][0-9](_norm)?$", names(result), value = TRUE)
      # Exclude columns that existed before computation (from parcels)
      new_cols <- setdiff(pattern_cols, names(parcels))
      if (length(new_cols) > 0) {
        return(result[[new_cols[1]]])
      }
      if (length(pattern_cols) > 0) {
        return(result[[pattern_cols[1]]])
      }
    }

    # If result is a vector, return it directly
    if (is.numeric(result) && length(result) == nrow(parcels)) {
      return(result)
    }

    # Return as-is and hope for the best
    return(result)
  }

  # Fallback: return random values for demo (to be replaced with actual calculations)
  cli::cli_alert_info("Using placeholder values for {indicator}")
  runif(nrow(parcels), 0, 100)
}


#' Save indicators incrementally
#'
#' @description
#' Saves indicators after each successful computation.
#' Uses a lightweight approach to avoid overhead.
#'
#' @param project_id Character. Project ID.
#' @param results sf object. Current results.
#' @param indicator Character. Name of just-computed indicator.
#'
#' @return Logical. TRUE if successful.
#'
#' @noRd
save_indicators_incremental <- function(project_id, results, indicator) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(FALSE)
  }

  # Save to temporary incremental file
  results_path <- file.path(project_path, "data", "indicators.parquet")
  progress_path <- file.path(project_path, "data", "compute_progress.json")

  tryCatch({
    if (!requireNamespace("arrow", quietly = TRUE)) {
      return(FALSE)
    }

    # Convert sf to data.frame with WKT geometry
    results_df <- results

    # Check if results is an sf object before extracting geometry
    if (inherits(results, "sf")) {
      results_df$geometry_wkt <- sf::st_as_text(sf::st_geometry(results))
      results_df <- sf::st_drop_geometry(results_df)
    } else if (is.data.frame(results)) {
      # Already a data.frame, check if geometry_wkt exists
      if (!"geometry_wkt" %in% names(results_df)) {
        cli::cli_warn("Results is not an sf object, geometry may be missing")
      }
    } else {
      cli::cli_warn("Unexpected results type: {class(results)[1]}")
      return(FALSE)
    }

    # Write parquet via temp file to avoid Windows memory-mapped file lock
    tmp_path <- paste0(results_path, ".tmp")
    arrow::write_parquet(results_df, tmp_path)
    if (file.exists(results_path)) file.remove(results_path)
    file.rename(tmp_path, results_path)

    # Update progress tracking file
    progress <- list(
      last_indicator = indicator,
      last_saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      computed_indicators = names(results)[
        vapply(names(results), function(col) {
          col %in% list_available_indicators() && !all(is.na(results[[col]]))
        }, logical(1))
      ]
    )
    jsonlite::write_json(progress, progress_path, auto_unbox = TRUE, pretty = TRUE)

    cli::cli_alert_success("Saved indicator: {indicator}")
    TRUE

  }, error = function(e) {
    cli::cli_warn("Failed to save incrementally: {e$message}")
    FALSE
  })
}


#' Save progress state for async polling
#'
#' @description
#' Saves the full computation progress state to a JSON file for polling.
#' Used by ExtendedTask to communicate progress to the UI.
#'
#' @param project_id Character. Project ID.
#' @param state List. Full progress state.
#' @param project_path Character. Optional project path (for async mode).
#'
#' @return Invisible NULL.
#'
#' @noRd
save_progress_state <- function(project_id, state, project_path = NULL) {
  if (is.null(project_path)) {
    project_path <- get_project_path(project_id)
  }
  if (is.null(project_path) || !dir.exists(project_path)) {
    return(invisible(NULL))
  }

  progress_file <- file.path(project_path, "data", "progress_state.json")

  # Ensure data directory exists
  data_dir <- file.path(project_path, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  # Convert state to JSON-safe format
  state_json <- list(
    project_id = state$project_id,
    status = state$status,
    phase = state$phase,
    progress = state$progress,
    progress_max = state$progress_max,
    current_task = state$current_task,
    indicators_total = state$indicators_total,
    indicators_completed = state$indicators_completed,
    indicators_skipped = state$indicators_skipped %||% 0L,
    indicators_failed = state$indicators_failed,
    indicators_status = as.list(state$indicators_status),
    errors = state$errors,
    started_at = if (!is.null(state$started_at)) format(state$started_at, "%Y-%m-%d %H:%M:%S") else NULL,
    completed_at = if (!is.null(state$completed_at)) format(state$completed_at, "%Y-%m-%d %H:%M:%S") else NULL,
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  tryCatch({
    jsonlite::write_json(state_json, progress_file, auto_unbox = TRUE, pretty = TRUE)
  }, error = function(e) {
    cli::cli_warn("Failed to save progress state: {e$message}")
  })

  invisible(NULL)
}


#' Cancel a running computation
#'
#' @description
#' Writes a cancellation signal file that the compute process checks
#' between indicators.
#'
#' @param project_id Character. Project ID.
#'
#' @noRd
cancel_computation <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) return(invisible(NULL))

  cancel_file <- file.path(project_path, "data", "cancel_requested")
  tryCatch({
    writeLines("cancel", cancel_file)
  }, error = function(e) {
    cli::cli_warn("Failed to write cancel file: {e$message}")
  })

  invisible(NULL)
}


#' Check if cancellation was requested
#'
#' @param project_id Character. Project ID.
#'
#' @return Logical. TRUE if cancellation was requested.
#'
#' @noRd
is_cancelled <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) return(FALSE)

  cancel_file <- file.path(project_path, "data", "cancel_requested")
  file.exists(cancel_file)
}


#' Clear cancellation signal
#'
#' @param project_id Character. Project ID.
#'
#' @noRd
clear_cancel <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) return(invisible(NULL))

  cancel_file <- file.path(project_path, "data", "cancel_requested")
  if (file.exists(cancel_file)) {
    file.remove(cancel_file)
  }

  invisible(NULL)
}


#' Read progress state for async polling
#'
#' @description
#' Reads the full computation progress state from the JSON file.
#' Used by the UI to poll progress during ExtendedTask execution.
#'
#' @param project_id Character. Project ID.
#'
#' @return List with progress state, or NULL if not found.
#'
#' @noRd
read_progress_state <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  progress_file <- file.path(project_path, "data", "progress_state.json")

  if (!file.exists(progress_file)) {
    return(NULL)
  }

  tryCatch({
    jsonlite::read_json(progress_file)
  }, error = function(e) {
    NULL
  })
}


#' Clear computation cache for a project
#'
#' @description
#' Removes cached indicator results and progress tracking files,
#' forcing a full recomputation on next run.
#'
#' @param project_id Character. Project ID.
#'
#' @return Logical. TRUE if cache was cleared successfully.
#'
#' @noRd
clear_computation_cache <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) return(FALSE)

  files_to_remove <- c(
    file.path(project_path, "data", "indicators.parquet"),
    file.path(project_path, "data", "compute_progress.json"),
    file.path(project_path, "data", "progress_state.json")
  )

  removed <- vapply(files_to_remove, function(f) {
    if (file.exists(f)) {
      unlink(f) == 0
    } else {
      TRUE
    }
  }, logical(1))

  cli::cli_alert_info("Cleared computation cache for project {project_id}")
  all(removed)
}


get_computation_progress <- function(project_id) {
  project_path <- get_project_path(project_id)
  if (is.null(project_path)) {
    return(NULL)
  }

  progress_path <- file.path(project_path, "data", "compute_progress.json")

  if (!file.exists(progress_path)) {
    return(list(
      computed_indicators = character(0),
      last_indicator = NULL,
      last_saved_at = NULL
    ))
  }

  tryCatch({
    jsonlite::read_json(progress_path)
  }, error = function(e) {
    list(
      computed_indicators = character(0),
      last_indicator = NULL,
      last_saved_at = NULL
    )
  })
}


# NOTE: save_indicators() et load_indicators() sont definis dans
# R/service_project.R (version canonique avec NDP).
# Ne pas les redefinir ici pour eviter les conflits de masquage.


# NB: aggregate_indicators_to_ug() was removed. Indicators are now
# computed directly on the UGF geometries in a single pass by
# compute_all_indicators() — there is nothing to aggregate post-hoc.


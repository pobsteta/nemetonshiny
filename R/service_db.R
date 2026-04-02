#' Database Service for PostgreSQL/PostGIS
#'
#' @description
#' Connection and operations for the PostgreSQL/PostGIS database (ADR-002).
#' The database stores projects, parcels, indicators, and users
#' for multi-user access and spatial queries.
#'
#' Connection is optional: if no database credentials are configured,
#' the app falls back to local parquet storage.
#'
#' @name service_db
#' @keywords internal
NULL


# ============================================================
# Connection Management
# ============================================================

#' Check if database connection is configured
#'
#' Returns TRUE if PostgreSQL environment variables are set.
#'
#' @return Logical.
#' @noRd
is_db_configured <- function() {
  host <- Sys.getenv("POSTGRESQL_ADDON_HOST",
                     Sys.getenv("NEMETON_DB_HOST", ""))
  nchar(host) > 0
}


#' Get database connection
#'
#' Creates or returns a cached connection to the PostgreSQL database.
#' Supports both Clever Cloud variable names (POSTGRESQL_ADDON_*)
#' and generic names (NEMETON_DB_*).
#'
#' @param check_postgis Logical. Verify PostGIS extension on first connect. Default TRUE.
#'
#' @return A DBI connection object, or NULL if not configured.
#'
#' @noRd
get_db_connection <- function(check_postgis = TRUE) {
  if (!is_db_configured()) return(NULL)

  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    cli::cli_warn("Package {.pkg RPostgres} is required for database access.")
    return(NULL)
  }

  # Supporter les deux conventions de nommage
  host     <- Sys.getenv("POSTGRESQL_ADDON_HOST", Sys.getenv("NEMETON_DB_HOST"))
  port     <- Sys.getenv("POSTGRESQL_ADDON_PORT", Sys.getenv("NEMETON_DB_PORT", "5432"))
  dbname   <- Sys.getenv("POSTGRESQL_ADDON_DB",   Sys.getenv("NEMETON_DB_NAME"))
  user     <- Sys.getenv("POSTGRESQL_ADDON_USER",  Sys.getenv("NEMETON_DB_USER"))
  password <- Sys.getenv("POSTGRESQL_ADDON_PASSWORD", Sys.getenv("NEMETON_DB_PASSWORD"))

  tryCatch({
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      host     = host,
      port     = as.integer(port),
      dbname   = dbname,
      user     = user,
      password = password,
      sslmode  = "require"
    )

    if (check_postgis) {
      # Verifier que PostGIS est disponible
      has_postgis <- tryCatch({
        res <- DBI::dbGetQuery(con, "SELECT PostGIS_Version()")
        TRUE
      }, error = function(e) FALSE)

      if (!has_postgis) {
        cli::cli_warn("PostGIS extension not found. Spatial queries will not work.")
      }
    }

    cli::cli_alert_success("Connected to PostgreSQL: {dbname}@{host}:{port}")
    con
  }, error = function(e) {
    cli::cli_warn("Failed to connect to database: {e$message}")
    NULL
  })
}


#' Disconnect from database
#'
#' @param con DBI connection object.
#' @noRd
close_db_connection <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
  }
}


# ============================================================
# Schema Initialization
# ============================================================

#' Initialize database schema
#'
#' Creates the nemeton schema, tables, and indexes if they don't exist.
#' Reads from inst/sql/schema.sql.
#'
#' @param con DBI connection object. If NULL, creates a new connection.
#'
#' @return Logical. TRUE if successful.
#'
#' @export
db_init_schema <- function(con = NULL) {
  own_con <- is.null(con)
  if (own_con) {
    con <- get_db_connection()
    if (is.null(con)) return(FALSE)
  }
  on.exit(if (own_con) close_db_connection(con))

  schema_file <- system.file("sql", "schema.sql", package = "nemeton")
  if (!nzchar(schema_file)) {
    cli::cli_abort("Schema file not found: inst/sql/schema.sql")
  }

  sql <- readLines(schema_file, encoding = "UTF-8")
  sql_text <- paste(sql, collapse = "\n")

  # Executer chaque statement separement
  statements <- strsplit(sql_text, ";")[[1]]
  statements <- trimws(statements)
  statements <- statements[nchar(statements) > 0]

  tryCatch({
    for (stmt in statements) {
      # Ignorer les commentaires seuls
      if (grepl("^\\s*--", stmt) && !grepl("CREATE|ALTER|INSERT", stmt)) next
      DBI::dbExecute(con, paste0(stmt, ";"))
    }
    cli::cli_alert_success("Database schema initialized")
    TRUE
  }, error = function(e) {
    cli::cli_warn("Schema initialization error: {e$message}")
    FALSE
  })
}


# ============================================================
# Project Operations
# ============================================================

#' Save project to database
#'
#' @param con DBI connection.
#' @param project_id Character. Local project ID.
#' @param metadata List. Project metadata.
#' @param parcels sf object. Parcel geometries.
#' @param indicators data.frame. Computed indicators.
#'
#' @return Logical. TRUE if successful.
#' @noRd
db_save_project <- function(con, project_id, metadata, parcels = NULL,
                            indicators = NULL) {
  tryCatch({
    # Upsert project
    DBI::dbExecute(con, "
      INSERT INTO nemeton.projects (project_id, name, description, status, ndp_level,
                                     commune_code, commune_name, department_code, metadata)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
      ON CONFLICT (project_id) DO UPDATE SET
        name = EXCLUDED.name,
        description = EXCLUDED.description,
        status = EXCLUDED.status,
        ndp_level = EXCLUDED.ndp_level,
        updated_at = NOW(),
        metadata = EXCLUDED.metadata
    ", params = list(
      project_id,
      metadata$name %||% "Sans nom",
      metadata$description %||% "",
      metadata$status %||% "draft",
      as.integer(metadata$ndp_level %||% 0L),
      metadata$commune_code %||% NA_character_,
      metadata$commune_name %||% NA_character_,
      metadata$department_code %||% NA_character_,
      jsonlite::toJSON(metadata, auto_unbox = TRUE)
    ))

    # Sauvegarder les parcelles si fournies
    if (!is.null(parcels) && inherits(parcels, "sf") && nrow(parcels) > 0) {
      db_save_parcels(con, project_id, parcels)
    }

    # Sauvegarder les indicateurs si fournis
    if (!is.null(indicators) && nrow(indicators) > 0) {
      db_save_indicators(con, project_id, indicators)
    }

    cli::cli_alert_success("Project {.val {project_id}} saved to database")
    TRUE
  }, error = function(e) {
    cli::cli_warn("Failed to save project to database: {e$message}")
    FALSE
  })
}


#' Save parcels to database
#'
#' @param con DBI connection.
#' @param project_id Character.
#' @param parcels sf object.
#' @noRd
db_save_parcels <- function(con, project_id, parcels) {
  # Recuperer l'UUID du projet
  proj_uuid <- DBI::dbGetQuery(con,
    "SELECT id FROM nemeton.projects WHERE project_id = $1",
    params = list(project_id)
  )$id

  if (length(proj_uuid) == 0) return(invisible(NULL))

  # Supprimer les anciennes parcelles
  DBI::dbExecute(con,
    "DELETE FROM nemeton.parcels WHERE project_id = $1",
    params = list(proj_uuid)
  )

  # Reprojeter en CRS de stockage paneuropeen (ADR-008)
  storage_crs <- get_storage_crs()
  if (sf::st_crs(parcels)$epsg != storage_crs) {
    parcels <- sf::st_transform(parcels, storage_crs)
  }

  # Convertir en MultiPolygon
  parcels <- sf::st_cast(parcels, "MULTIPOLYGON")

  # Inserer via sf::st_write — ne garder que les colonnes du schema
  parcels_db <- parcels
  parcels_db$project_id <- proj_uuid
  if (is.null(parcels_db$nemeton_id)) parcels_db$nemeton_id <- parcels_db$id
  if (is.null(parcels_db$geo_parcelle)) parcels_db$geo_parcelle <- NA_character_

  # Colonnes du schema nemeton.parcels (sans id et created_at, generes par la DB)
  schema_cols <- c("project_id", "nemeton_id", "geo_parcelle", "section",
                   "numero", "contenance")
  keep_cols <- intersect(names(parcels_db), schema_cols)
  # Garder aussi la colonne geometrie
  geom_col <- attr(parcels_db, "sf_column") %||% "geometry"
  parcels_db <- parcels_db[, c(keep_cols, geom_col)]

  sf::st_write(parcels_db, con,
    DBI::Id(schema = "nemeton", table = "parcels"),
    append = TRUE, quiet = TRUE
  )
}


#' Save indicators to database
#'
#' @param con DBI connection.
#' @param project_id Character.
#' @param indicators data.frame.
#' @noRd
db_save_indicators <- function(con, project_id, indicators) {
  # Recuperer l'UUID du projet
  proj_uuid <- DBI::dbGetQuery(con,
    "SELECT id FROM nemeton.projects WHERE project_id = $1",
    params = list(project_id)
  )$id

  if (length(proj_uuid) == 0) return(invisible(NULL))

  # Supprimer les anciens indicateurs
  DBI::dbExecute(con,
    "DELETE FROM nemeton.indicators WHERE project_id = $1",
    params = list(proj_uuid)
  )

  # Preparer le data.frame pour l'insertion
  ind_df <- if (inherits(indicators, "sf")) {
    sf::st_drop_geometry(indicators)
  } else {
    as.data.frame(indicators)
  }

  # Mapping noms locaux (service_compute.R) → cles NMT du glossaire BMAD (schema DB)
  rename_map <- c(
    # B - Biodiversite
    biodiversity_protection   = "indicateur_b1_protection",
    biodiversity_structure    = "indicateur_b2_structure",
    biodiversity_connectivity = "indicateur_b3_connectivite",
    # C - Carbone
    carbon_biomass            = "indicateur_c1_biomasse",
    carbon_ndvi               = "indicateur_c2_ndvi",
    # W - Eau
    water_network             = "indicateur_w1_reseau",
    water_wetlands            = "indicateur_w2_zones_humides",
    water_twi                 = "indicateur_w3_humidite",
    # A - Air
    air_forest_buffer         = "indicateur_a1_couverture",
    air_quality               = "indicateur_a2_qualite_air",
    # F - Fertilite
    fertility_soil            = "indicateur_f1_fertilite",
    fertility_erosion         = "indicateur_f2_erosion",
    # L - Paysage
    landscape_edge_ratio      = "indicateur_l1_sylvosphere",
    landscape_fragmentation   = "indicateur_l2_fragmentation",
    # T - Temporel
    temporal_age              = "indicateur_t1_anciennete",
    temporal_change           = "indicateur_t2_changement",
    # R - Risques
    risk_fire                 = "indicateur_r1_feu",
    risk_storm                = "indicateur_r2_tempete",
    risk_drought              = "indicateur_r3_secheresse",
    risk_browsing             = "indicateur_r4_abroutissement",
    # S - Social
    social_trails             = "indicateur_s1_routes",
    social_accessibility      = "indicateur_s2_bati",
    social_population         = "indicateur_s3_population",
    # P - Production
    production_volume         = "indicateur_p1_volume",
    production_productivity   = "indicateur_p2_station",
    production_quality        = "indicateur_p3_qualite_bois",
    # E - Energie
    energy_wood               = "indicateur_e1_bois_energie",
    energy_co2                = "indicateur_e2_evitement",
    # N - Naturalite
    naturalness_distance      = "indicateur_n1_distance",
    naturalness_continuity    = "indicateur_n2_continuite",
    naturalness_score         = "indicateur_n3_naturalite",
    # Familles
    family_B                  = "famille_biodiversite",
    family_C                  = "famille_carbone",
    family_W                  = "famille_eau",
    family_A                  = "famille_air",
    family_F                  = "famille_sol",
    family_L                  = "famille_paysage",
    family_T                  = "famille_temporel",
    family_R                  = "famille_risque",
    family_S                  = "famille_social",
    family_P                  = "famille_production",
    family_E                  = "famille_energie",
    family_N                  = "famille_naturalite"
  )

  for (old_name in names(rename_map)) {
    if (old_name %in% names(ind_df)) {
      names(ind_df)[names(ind_df) == old_name] <- rename_map[[old_name]]
    }
  }

  ind_df$project_id <- proj_uuid

  # Colonnes du schema DB (cles NMT glossaire BMAD)
  db_cols <- c(
    # 31 indicateurs
    "indicateur_b1_protection", "indicateur_b2_structure", "indicateur_b3_connectivite",
    "indicateur_c1_biomasse", "indicateur_c2_ndvi",
    "indicateur_w1_reseau", "indicateur_w2_zones_humides", "indicateur_w3_humidite",
    "indicateur_a1_couverture", "indicateur_a2_qualite_air",
    "indicateur_f1_fertilite", "indicateur_f2_erosion",
    "indicateur_l1_sylvosphere", "indicateur_l2_fragmentation",
    "indicateur_t1_anciennete", "indicateur_t2_changement",
    "indicateur_r1_feu", "indicateur_r2_tempete", "indicateur_r3_secheresse", "indicateur_r4_abroutissement",
    "indicateur_s1_routes", "indicateur_s2_bati", "indicateur_s3_population",
    "indicateur_p1_volume", "indicateur_p2_station", "indicateur_p3_qualite_bois",
    "indicateur_e1_bois_energie", "indicateur_e2_evitement",
    "indicateur_n1_distance", "indicateur_n2_continuite", "indicateur_n3_naturalite",
    # 12 familles
    "famille_biodiversite", "famille_carbone", "famille_eau", "famille_air",
    "famille_sol", "famille_paysage", "famille_temporel", "famille_risque",
    "famille_social", "famille_production", "famille_energie", "famille_naturalite"
  )

  # Ne garder que les colonnes presentes dans les donnees ET dans le schema
  available_cols <- intersect(names(ind_df), db_cols)
  insert_cols <- c("project_id", available_cols)
  insert_df <- ind_df[, intersect(names(ind_df), insert_cols), drop = FALSE]

  DBI::dbWriteTable(con, DBI::Id(schema = "nemeton", table = "indicators"),
                    insert_df, append = TRUE, row.names = FALSE)
}


# ============================================================
# Load Operations
# ============================================================

#' Load project from database
#'
#' @param con DBI connection.
#' @param project_id Character. Local project ID.
#'
#' @return List with project data, or NULL if not found.
#' @noRd
db_load_project <- function(con, project_id) {
  proj <- DBI::dbGetQuery(con,
    "SELECT * FROM nemeton.projects WHERE project_id = $1",
    params = list(project_id)
  )

  if (nrow(proj) == 0) return(NULL)

  list(
    project_id = proj$project_id,
    metadata = jsonlite::fromJSON(proj$metadata),
    status = proj$status,
    ndp_level = proj$ndp_level
  )
}


#' Load parcels from database
#'
#' @param con DBI connection.
#' @param project_id Character. Local project ID.
#'
#' @return sf object, or NULL.
#' @noRd
db_load_parcels <- function(con, project_id) {
  sf::st_read(con,
    query = sprintf(
      "SELECT p.* FROM nemeton.parcels p
       JOIN nemeton.projects pr ON p.project_id = pr.id
       WHERE pr.project_id = '%s'",
      DBI::dbQuoteString(con, project_id)
    ),
    quiet = TRUE
  )
}


#' Load indicators from database
#'
#' @param con DBI connection.
#' @param project_id Character. Local project ID.
#'
#' @return data.frame, or NULL.
#' @noRd
db_load_indicators <- function(con, project_id) {
  DBI::dbGetQuery(con,
    "SELECT i.* FROM nemeton.indicators i
     JOIN nemeton.projects pr ON i.project_id = pr.id
     WHERE pr.project_id = $1",
    params = list(project_id)
  )
}


# ============================================================
# User Operations
# ============================================================

#' Create or update user from OAuth token
#'
#' @param con DBI connection.
#' @param external_id Character. Provider's user ID (sub claim).
#' @param provider Character. OAuth provider name.
#' @param email Character. User email.
#' @param name Character. Display name.
#' @param profile_key Character. Actor profile key (NMT convention).
#'
#' @return UUID of the user.
#' @noRd
db_upsert_user <- function(con, external_id, provider = "keycloak",
                            email = NULL, name = "Anonyme",
                            profile_key = "profil_citoyen") {
  result <- DBI::dbGetQuery(con, "
    INSERT INTO nemeton.users (external_id, provider, email, name,
                                profile_key, last_login_at)
    VALUES ($1, $2, $3, $4, $5, NOW())
    ON CONFLICT (external_id) DO UPDATE SET
      email = COALESCE(EXCLUDED.email, nemeton.users.email),
      name = COALESCE(EXCLUDED.name, nemeton.users.name),
      last_login_at = NOW()
    RETURNING id
  ", params = list(external_id, provider, email, name, profile_key))

  result$id
}


#' Get user by external ID
#'
#' @param con DBI connection.
#' @param external_id Character.
#'
#' @return List with user info, or NULL.
#' @noRd
db_get_user <- function(con, external_id) {
  user <- DBI::dbGetQuery(con,
    "SELECT u.*, array_agg(r.role) as roles
     FROM nemeton.users u
     LEFT JOIN nemeton.user_roles r ON u.id = r.user_id
     WHERE u.external_id = $1
     GROUP BY u.id",
    params = list(external_id)
  )

  if (nrow(user) == 0) return(NULL)
  as.list(user[1, ])
}


# ============================================================
# Sync: Local <-> Database
# ============================================================

#' Sync local project to database
#'
#' Uploads a local project (parquet files) to the PostGIS database.
#'
#' @param project_id Character. Local project ID.
#' @param con DBI connection. If NULL, creates a new one.
#'
#' @return Logical. TRUE if successful.
#'
#' @export
db_sync_project <- function(project_id, con = NULL) {
  own_con <- is.null(con)
  if (own_con) {
    con <- get_db_connection()
    if (is.null(con)) {
      cli::cli_warn("Database not configured. Sync skipped.")
      return(FALSE)
    }
  }
  on.exit(if (own_con) close_db_connection(con))

  # Charger les donnees locales
  meta <- load_project_metadata(project_id)
  if (is.null(meta)) {
    cli::cli_warn("Project {.val {project_id}} not found locally.")
    return(FALSE)
  }

  parcels <- tryCatch(load_parcels(project_id), error = function(e) NULL)
  indicators <- tryCatch(load_indicators(project_id), error = function(e) NULL)

  db_save_project(con, project_id, meta, parcels, indicators)
}

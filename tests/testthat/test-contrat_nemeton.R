# Contrat nemeton -> nemetonshiny (mission 015).
#
# nemetonshiny ne calcule rien : il appelle nemeton (Imports: nemeton, dernier
# tag de release). Ce test liste chaque objet de nemeton que le code de
# nemetonshiny utilise ; un objet renomme, retire, ou dont un argument disparait
# fait echouer la CI ici, pas chez l'utilisateur. Changer ces tables = changer
# le contrat : mettre a jour CONTRAT.md (section "API lue dans nemeton").
#
# Releve fait dans le code (R/), pas suppose :
#   - appels `nemeton::f` (129 objets, releves par analyse syntaxique) ;
#   - appels nus `f()` permis par `import(nemeton)` du NAMESPACE ;
#   - enveloppes `.nemeton_fn("f")` de R/imports.R (resolues par
#     getFromNamespace : objets INTERNES de nemeton, non exportes) ;
#   - noms resolus a l'execution : getExportedValue("nemeton", ...)
#     (applicabilite, inventaire video, theia, foret ancienne), `fun = "..."`
#     passe a run_memory_capped, et les indicateur_* de
#     list_available_indicators(), appeles par do.call().
#
# Compatibilite : tout argument liste doit rester dans formals() ; un argument
# AJOUTE cote nemeton ne casse rien et ne fait pas echouer le test.

# Fonctions exportees par nemeton : arguments (formals()) a la date du contrat.
EXPORTS <- list(
  a5_applicabilite                 = c("units", "lst", "buffer_m", "country"),
  aggregate_family_scores          = c("units", "weights", "surface_col", "family_pattern"),
  aggregate_plot_metrics           = c("placettes", "arbres", "plot_radius", "statuts_vivants"),
  attach_field_data_to_units       = c("units", "field_agg"),
  bdforet_v2_mapping               = c("file"),
  build_biljou_soil                = c("units", "ewm", "source", "rooting_depth_cm", "country", "roots", "macro", "micro", "init", "progress_callback", "..."),
  build_foret_ancienne_mask        = c("source", "forest_class", "threshold", "min_area_m2", "crs"),
  build_index_stack                = c("cache_dir", "scenes_df", "index", "mask_polygon", "parallel", "cache_result", "result_cache_dir"),
  build_knowledge_corpus           = c("con", "manifest", "provider", "include_to_confirm", "fresh", "dry_run", "pdf_dir", "api_key", "progress"),
  build_ndvi_season_composite      = c("cache_dir", "scenes", "mask_polygon", "season_doy", "max_scenes", "clamp"),
  build_project_monitoring_zones   = c("con", "project_name", "project_uuid", "ugf", "bdforet", "strata", "work_crs", "replace"),
  canopy_provenance                = c("augmented"),
  charger_forms_t_hauteurs         = c("aoi", "annees", "dossier", "country"),
  check_fordead_validity           = c("aoi", "units", "bdforet", "layers", "threshold_geo", "threshold_species", "min_resineux"),
  check_reconfort_validity         = c("aoi", "units", "bdforet", "layers", "threshold_geo", "threshold_species", "min_target"),
  climat_ete_series                = c("aoi", "source", "rayon_km", "mois", "annees", "cache_dir", "eobs_cache_dir", "version", "progress_callback", "..."),
  climat_nuages_annuels            = c("series", "mode", "reference", "recul_min", "n_points", "seed"),
  climat_tendance_bivariee         = c("series", "periode", "bornes", "palette", "n", "cache_path", "eobs_cache_dir", "version"),
  climate_ombrothermic_indices     = c("clim_rr", "clim_t"),
  completer_volume_ifn             = c("units", "volume_col", "species_field", "ser", "min_plac", "mesure", "source_col", "methode"),
  compute_dtm_chm_from_laz         = c("laz_dir", "dtm_dir", "chm_dir", "res", "aoi", "ncores", "overwrite", "verbose"),
  compute_fast_alert_mask          = c("con", "zone_id", "index", "threshold", "date_from", "date_to", "mode", "window_days", "months", "min_years", "min_obs_per_year", "alpha", "cache_dir", "mask_cache_dir", "breaks", "apply_zone_mask", "mask_polygon", "cache_result", "result_cache_dir", "parallel"),
  compute_general_index            = c("family_scores", "ndp"),
  compute_sample_size              = c("cv", "target_error", "alpha", "N", "max_iter", "tol"),
  compute_spectral_diversity       = c("reflectance", "mask", "window_size", "nb_cpu", "output_dir", "options", "reuse_existing"),
  construire_ugf_onf               = c("aoi", "insee", "parcelles_onf", "cadastre", "proprietaires", "pas", "dmax", "rayon", "k", "seuil_couverture", "tol", "larg_hors", "ha_hors", "seuil", "seuil_hors", "min_part_m2", "crs", "selection"),
  create_family_index              = c("data", "method", "weights", "na.rm", "family_codes"),
  create_qgis_project              = c("placettes", "zone_etude", "parcours_tsp", "output_dir", "project_name", "crs", "region", "lang", "overwrite"),
  create_sampling_plan             = c("zone", "n_base", "n_over", "target_error", "cv", "alpha", "over_ratio", "chm", "slope", "forest_mask", "mnt", "grid_step", "plot_radius", "min_forest_cover", "max_slope", "min_per_stratum", "seed"),
  create_trend_sanitary_plan       = c("con", "zone_id", "date_from", "date_to", "cache_dir", "index", "n_plots", "n_control", "months", "min_years", "min_obs_per_year", "alpha", "min_slope", "apply_zone_mask", "mask_polygon", "seed"),
  create_validation_sampling_plan  = c("zone", "alert_raster", "n_validation", "n_control", "classes", "control_classes", "buffer_m", "source", "weighting", "weight_raster", "seed"),
  creer_projet_inventaire_video    = c("placettes", "plot_ids", "rayon_m", "seuil_diam_cm", "unite_azimut", "declinaison_deg", "avec_lidar", "dem", "chm", "project_path", "marge_m", "output_dir", "project_name", "region", "lang", "overwrite"),
  cv_from_bdforet                  = c("bdforet_sf", "position", "aoi", "tfv_col", "mapping", "typology"),
  db_connect                       = c("url", "read_only", "connect_timeout"),
  db_disconnect                    = c("con"),
  db_migrate                       = c("con", "migrations_dir"),
  delete_knowledge_document        = c("con", "document_id"),
  detect_ndp    = c("data"),
  enrich_parcels_bdforet           = c("parcels", "bdforet_sf"),
  eobs_monthly_climatology         = c("daily", "point", "var", "years"),
  eobs_summer_series               = c("stack", "point"),
  eobs_trend_fit                   = c("series"),
  extract_indicator_value          = c("result", "indicator", "exclude"),
  extract_pixel_timeseries         = c("cache_dir", "scenes_df", "xy", "crs", "indices", "zone_polygon", "warn_outside_zone"),
  extract_pixel_trend              = c("cache_dir", "scenes_df", "xy", "crs", "index", "months", "min_years", "min_obs_per_year", "alpha", "min_slope", "zone_polygon", "warn_outside_zone"),
  find_zone_by_project             = c("con", "project_uuid"),
  find_zones_by_project            = c("con", "project_uuid"),
  format_citations                 = c("retrieved_chunks", "format", "lang"),
  format_duration                  = c("sec", "with_seconds"),
  get_data_source                  = c("source_key", "country", "section"),
  get_famille_code                 = c("col_name"),
  get_famille_col                  = c("code"),
  get_global_cache_dir             = character(),
  get_layer_service                = c("layer_key", "country"),
  get_ndp_level                    = c("ndp"),
  get_storage_crs                  = character(),
  groma_envoyer_projet             = c("qgz", "url", "jeton", "projet_nom"),
  groma_etat_placettes             = c("url", "jeton"),
  groma_telecharger_resultats      = c("url", "jeton", "plot_ids", "dossier", "fichiers", "etat"),
  ifn_covariables_domaines         = c("domaines", "hauteur", "altitude", "id_col", "unite_hauteur", "seuil_m"),
  ifn_production_domaines          = c("domaines", "attribut", "id_col", "n_campagnes", "campagnes", "ser_layer", "covariables"),
  ifn_tarif_schaeffer_ser          = c("espar", "code", "niveau", "type"),
  ifn_taux_prelevement_production  = c("ser", "groupe", "definition", "n_campagnes", "campagnes", "min_plac"),
  import_qgis_gpkg                 = c("path"),
  importer_inventaire_video        = c("gpkg", "resultats", "field_data_gpkg", "plan", "priorite", "region", "terrain", "dossier_photos"),
  indicateur_a1_couverture         = c("units", "land_cover", "forest_classes", "buffer_radius", "fvc"),
  indicateur_a2_qualite_air        = c("units", "layers", "atmo_data", "roads", "urban_areas", "method"),
  indicateur_a3_microclimat        = c("units", "micro", "chm", "bounds", "..."),
  indicateur_a4_tamponnement       = c("units", "micro", "chm", "bounds", "..."),
  indicateur_a5_rafraichissement   = c("units", "lst", "reference", "buffer_m", "delta_scale", "..."),
  indicateur_b1_protection         = c("units", "protected_areas", "source", "protection_types", "preprocess"),
  indicateur_b2_structure          = c("units", "layers", "strata_field", "age_class_field", "species_field", "method", "weights", "use_height_cv", "chm", "cv_chm_weight"),
  indicateur_b3_connectivite       = c("units", "bdforet", "dem", "max_distance"),
  indicateur_b4_div_spectrale      = c("units", "spectral", "reflectance", "embedding", "layers", "..."),
  indicateur_c1_biomasse           = c("units", "layers", "species_col", "age_col", "density_col", "chm", "dbh_col", "stems_col", "h_dom_percentile", "bef"),
  indicateur_c2_ndvi               = c("units", "layers", "ndvi_layer", "trend", "fapar"),
  indicateur_e1_bois_energie       = c("units", "volume_field", "species_field", "harvest_rate", "residue_fraction", "coppice_area_field", "chm", "production_field", "taux_mobilisation", "ser_field"),
  indicateur_e2_evitement          = c("units", "fuelwood_field", "volume_field", "energy_scenario", "material_scenario", "taux_recolte_materiau"),
  indicateur_f1_fertilite          = c("units", "layers", "soil_layer", "fertility_col", "source", "country", "rpf_code_col", "texture", "fertility_range"),
  indicateur_f2_erosion            = c("units", "layers", "dem_layer", "texture", "dem_target_res"),
  indicateur_l1_effet_lisiere      = c("units", "layers", "landcover_layer", "forest_values", "buffer"),
  indicateur_l2_morcellement       = c("units", "layers", "landcover_layer", "forest_values", "buffer"),
  indicateur_l3_het_spectrale      = c("units", "spectral", "reflectance", "min_windows", "embedding", "layers", "..."),
  indicateur_n1_distance           = c("units", "roads", "buildings", "layers"),
  indicateur_n2_continuite         = c("units", "bdforet", "foret_ancienne", "layers", "weight_anciennete"),
  indicateur_n3_naturalite         = c("units"),
  indicateur_p1_volume             = c("units", "species_field", "dbh_field", "height_field", "density_field", "method", "chm", "h_dom_percentile", "pct_masked", "use_climate_drift"),
  indicateur_p2_station            = c("units", "species_field", "fertility_field", "climate_field", "productivity_table", "chm", "age_field", "reference_age", "h_dom_percentile", "source", "ser_field"),
  indicateur_p3_qualite_bois       = c("units", "dbh_field", "form_score_field", "defects_field", "species_field", "weights", "chm"),
  indicateur_r1_feu                = c("units", "dem", "layers", "bdforet", "species_field", "climate", "weights", "dem_target_res", "fire_exp_res", "fire_exp_weights"),
  indicateur_r2_tempete            = c("units", "dem", "layers", "chm", "species_field", "h_dom_percentile", "h_reference", "dem_target_res"),
  indicateur_r3_secheresse         = c("units", "layers", "dem", "climate_data", "snow", "snow_relief_strength", "soil_moisture", "sm_relief_strength", "biljou", "biljou_weight", "dem_target_res"),
  indicateur_r4_abroutissement     = c("units", "layers", "bdforet", "game_density", "edge_buffer"),
  indicateur_r5_deperissement      = c("units", "fordead_results", "reconfort_results", "weights", "weights_reconfort", "min_resineux", "min_feuillus", "include_low_classes", "resineux_col", "feuillus_col"),
  indicateur_r6_sensibilite        = c("units", "micro_moyenne", "micro_canicule", "bounds", "..."),
  indicateur_r7_gel                = c("units", "tmin", "budburst_doy", "window_end_doy", "frost_threshold_c", "max_frost_days", "..."),
  indicateur_s1_routes             = c("units", "roads", "dem", "layers", "dem_target_res", "max_dist"),
  indicateur_s2_bati               = c("units", "buildings", "dem", "layers", "dem_target_res", "max_dist"),
  indicateur_s3_population         = c("units", "population_grid", "population_field", "method", "buffer_radii"),
  indicateur_t1_anciennete         = c("units", "layers", "bdforet", "age_field", "establishment_year_field", "current_year", "age_structurel"),
  indicateur_t2_changement         = c("units", "layers", "t1_values", "forms_t", "seuil_perte", "unite_hauteur"),
  indicateur_t3_coupes_rases       = c("units", "sufosat_dates", "sufosat_proba", "window_years", "min_proba", "reference_year"),
  indicateur_t4_vitalite           = c("units", "forms_t", "essence", "ser", "coupes", "attendu", "tolerance_h", "min_analogues", "min_pixels", "unite_hauteur"),
  indicateur_w1_reseau             = c("units", "layers", "watercourse_layer", "buffer", "proximity_m", "proximity_ref", "spring_layer"),
  indicateur_w2_zones_humides      = c("units", "layers", "wetland_layer", "wetland_values", "water_occurrence", "occurrence_threshold", "dem_target_res"),
  indicateur_w3_humidite           = c("units", "layers", "dem_layer", "method", "dem_target_res"),
  indicateur_w4_vpd                = c("units", "micro", "chm", "bounds", "..."),
  indicator_families               = c("codes", "lang"),
  indicator_labels                 = c("codes", "lang"),
  indice_priorite_regen            = c("units", "species", "weights", "tolerances", "flag_breaks", "..."),
  ingest_health_validation         = c("con", "gpkg_path", "zone_id", "snap_distance_m", "validated_by", "layer"),
  ingest_sentinel2_timeseries      = c("con", "zone_id", "start", "end", "bands", "max_cloud", "skip_cached", "cache_dir", "progress_callback", "cancel_path", "prewarm_alerts", "prewarm_mask_cache_dir"),
  knowledge_manifest_path          = c("writable"),
  knowledge_manifest_vocab         = character(),
  lai_max_depuis_pai               = c("units", "pai", "probs", "min_pai"),
  lai_sentinel2                    = c("aoi", "refl", "start", "end", "reducer", "source", "sensor", "selected_bands", "geom_acq", "mask", "cache_dir", "precomputed", "..."),
  list_alerts                      = c("con", "zone_id", "classes", "validation_status", "period"),
  list_indicators                  = c("category", "return_type", "conditionnels"),
  list_knowledge_documents         = c("con", "lang", "doc_type", "family"),
  list_species_regions             = character(),
  load_biljou_forcing              = c("aoi", "years", "source", "cache_dir", "era5_cache_dir", "raw", "points", "latitude", "altitude", "compute_pet", "progress_callback", "..."),
  load_eobs_source                 = c("aoi", "var", "years", "months", "source", "reducer", "nc", "cache_dir", "version", "resolution", "period", "progress_callback", "..."),
  load_foret_ancienne_source       = c("aoi", "crs", "layer", "..."),
  load_insee_population_source     = c("aoi", "buffer_m", "maille", "millesime", "crs", "territoire", "cache_dir"),
  load_onf_parcelles_source        = c("aoi", "crs", "domanialite", "territoire", "max_parcelles", "clip"),
  load_theia_source                = c("source_key", "aoi", "asset", "year", "datetime", "country", "stac_api", "limit"),
  localiser_ser                    = c("units", "ser_layer", "colonne", "timeout"),
  meteoland_daily_grid             = c("aoi", "dem", "years", "variable", "dates", "doy_range", "buffer_m", "max_cells", "min_stations", "calibrate", "..."),
  microclimate_detect_years        = c("eobs", "aoi", "years", "year_window", "lidar_year"),
  migrer_colonnes_l                = c("data", "quiet"),
  nemeton_radar                    = c("data", "unit_id", "indicators", "mode", "normalize", "title", "fill_color", "fill_alpha"),
  normalize_indicator              = c("indicator", "values", "statut"),
  pai_depuis_nuage                 = c("dossier_las", "grille", "res", "k", "parcelle", "fenetre", "cl_sol", "cl_veg", "epsg", "pai_max", "ncores", "precomputed", "..."),
  prepare_pixel_dieback_series     = c("df", "grid_step", "smooth", "summer", "gap_flag_days"),
  probe_ign_lidar_tiles            = c("urls", "timeout"),
  project_lock_acquire             = c("con", "project_id", "holder_id", "holder_label", "ttl_seconds"),
  project_lock_heartbeat           = c("con", "project_id", "holder_id"),
  project_lock_release             = c("con", "project_id", "holder_id"),
  project_lock_status              = c("con", "project_id", "ttl_seconds"),
  prune_orphan_zone_caches         = c("con", "cache_root", "subdirs", "dry_run", "project_uuid", "force"),
  r5_applicabilite                 = c("units", "bdforet", "layers", "resineux_col", "feuillus_col", "min_resineux", "min_feuillus", "threshold_geo"),
  read_fast_alert_mask             = c("con", "zone_id", "run_id", "cache_dir", "apply_zone_mask", "mask_polygon"),
  read_fordead_dieback_mask        = c("con", "zone_id", "run_id", "cache_dir", "apply_zone_mask", "mask_polygon"),
  read_fordead_layer               = c("con", "zone_id", "layer", "run_id", "cache_dir", "apply_zone_mask", "mask_polygon"),
  read_fordead_pixel_series        = c("con", "zone_id", "xy", "crs", "run_id", "cache_dir"),
  read_knowledge_manifest          = c("path"),
  read_reconfort_alert_mask        = c("con", "zone_id", "run_id", "cache_dir", "apply_zone_mask", "mask_polygon"),
  read_reconfort_layer             = c("layer", "con", "zone_id", "apply_zone_mask", "mask_polygon"),
  read_reconfort_pixel_series      = c("con", "zone_id", "xy", "crs", "run_id", "cache_dir"),
  read_s2_band_raster              = c("cache_dir", "scene_id", "band", "harmonize"),
  reconfort_cache_manifest         = c("cache_dir", "zone_id", "run_id", "include_range"),
  reconfort_layer_manifest         = c("result", "include_range"),
  reconfort_year_bounds            = c("v_model", "today", "lag_days"),
  regen_bilan_hydrique             = c("units", "meteo", "sol", "lai_max", "forest_type", "years", "precomputed", "progress_callback", "..."),
  regen_rank_species               = c("units", "species_pool", "top_n", "weights", "exclude_invasive", "region", "cover_col", "lai_col", "extinction_k", "id_col", "include_atlas", "..."),
  regen_sensibilite                = c("units", "mnt", "mnh", "las", "annees_moy", "annees_canic", "mois_ete", "res", "tampon", "reqhgt", "k", "pai", "pai_cache", "pai_ncores", "micro_max_cells", "type_foret", "cache_dir", "era5_cache_dir", "progress_callback", "precomputed", "..."),
  regen_species_choices            = c("units", "species_col", "tfv_col", "level", "include_atlas", "region", "lang"),
  regen_tmin_decennale             = c("units", "tn", "years", "prob", "source", "cache_dir", "version", "progress_callback"),
  regeneration_tolerances          = character(),
  register_monitoring_zone         = c("con", "zone_name", "zone_polygon", "placettes", "radius_m", "project_uuid"),
  renvoyer_inventaire_video        = c("import", "gpkg", "rasters", "etats", "filtrer", "output_dir", "project_name", "overwrite"),
  reset_knowledge_manifest         = c("confirm"),
  resolve_project_chm              = c("project_path", "load", "verbose", "try_compute_from_laz", "validate"),
  resolve_project_dem              = c("project_path", "load", "verbose", "try_compute_from_laz", "validate"),
  resoudre_espar                   = c("x"),
  resoudre_espar_bdforet           = c("libelle", "ser"),
  retrieve_knowledge               = c("con", "query", "top_k", "family_codes", "profile_codes", "min_similarity", "lang", "embed_provider", "api_key"),
  run_fordead_dieback              = c("con", "zone_id", "cache_dir", "dates_training", "dates_monitoring", "vegetation_index", "threshold_anomaly", "max_cloud", "output_dir", "mask_cache_dir", "keep_output", "replace", "python_env", "min_pixels", "connectivity", "verbose", "progress_callback", "cancel_path"),
  run_memory_capped                = c("fun", "args", "package", "db_url", "options", "progress_path", "progress_callback", "memory_max", "poll_ms", "quiet", "log_path"),
  run_reconfort_dieback            = c("con", "zone_id", "cache_dir", "s2_year", "date_from", "date_to", "v_model", "binary_mask", "aoi_crop", "oso_national", "number_of_chunks", "scheduler_type", "nb_parallel_tasks", "output_dir", "geodes_config", "model_cache_dir", "mask_cache_dir", "tiles", "skip_ingest", "keep_workdir", "quiet", "progress_callback", "cancel_path"),
  sanitize_chm                     = c("chm", "forest_mask", "buildings", "water", "ndvi", "max_height", "slope", "ndvi_threshold", "forest_coverage_threshold", "verbose"),
  segment_houppiers                = c("chm", "aoi", "ws", "hmin", "algorithme", "emprise", "marge_m", "resolution", "max_cells", "h_range", "image", "usage", "lsms", "resolution_image", "budget_s"),
  smooth_pixel_series              = c("ts", "window_days", "method", "min_obs", "n_harmonics"),
  tag_field_data_sources           = c("data", "placettes", "arbres", "source"),
  tarif_schaeffer_local            = c("units", "especes", "ifn", "campagnes", "min_arbres", "min_placettes", "ser", "cible_ha"),
  theia_source_status              = c("source_key", "aoi", "country", "datetime", "stac_api", "limit"),
  traiter_nuage_points             = c("nuage", "type", "res", "classifier", "methode_sol", "csf", "mnt_externe", "mnh_reference", "recalage_vertical", "aoi", "dossier", "ncores", "overwrite", "verbose"),
  validate_field_data              = c("placettes", "arbres", "region", "lang", "dbh_max_cm", "dbh_alerte_cm"),
  validate_knowledge_manifest      = c("manifest"),
  volume_mobilisable               = c("units", "volume_col", "unite", "taux_prelevement", "horizon_ans", "espar_field", "ser", "min_plac", "na_policy", "p1_max_plausible", "column_name"),
  write_knowledge_manifest         = c("manifest", "path", "validate")
)

# Fonctions INTERNES de nemeton (R/imports.R, getFromNamespace).
INTERNES <- list(
  as_pure_sf                   = c("x", "raster"),
  clean_indicator_name         = c("names"),
  detect_ndp_from_cache        = c("project_path"),
  get_allometric_coefficients  = c("species"),
  get_dem_raster               = c("layers"),
  get_language                 = character(),
  map_essence_to_species       = c("essence"),
  msg                          = c("key", "..."),
  msg_error                    = c("key", "..."),
  msg_info                     = c("key", "..."),
  msg_success                  = c("key", "..."),
  msg_warn                     = c("key", "..."),
  resolve_raster_layer         = c("layers", "name"),
  resolve_vector_layer         = c("layers", "name"),
  restore_ndp_attributes       = c("data", "ndp_level"),
  safe_extract                 = c("raster", "polygons", "..."),
  set_ndp_attributes           = c("data", "layers")
)

# Colonnes des tables rendues par nemeton et lues par l'app
# (R/app_config.R, .build_indicator_families()).
COLONNES_FAMILLES <- c("code", "family_column", "name_fr", "name_en", "icon",
                       "color", "indicators", "column_names")
COLONNES_LIBELLES <- c("family", "code", "column_name", "label_fr", "label_en",
                       "tooltip_fr", "tooltip_en", "doc_url", "doc_lang")

.contrat_noms <- function() c(names(EXPORTS), names(INTERNES))

.verifie_signatures <- function(table, interne) {
  ns <- asNamespace("nemeton")
  for (nom in names(table)) {
    expect_true(exists(nom, envir = ns, inherits = FALSE),
                info = paste("objet absent de nemeton :", nom))
    if (!exists(nom, envir = ns, inherits = FALSE)) next
    if (!interne) {
      expect_true(nom %in% getNamespaceExports("nemeton"),
                  info = paste("objet plus exporte par nemeton :", nom))
    }
    obj <- get(nom, envir = ns, inherits = FALSE)
    expect_true(is.function(obj), info = paste("pas une fonction :", nom))
    expect_identical(setdiff(table[[nom]], names(formals(obj))), character(0),
                     info = paste("argument(s) disparu(s) :", nom))
  }
}

# nemeton est dans Imports (DESCRIPTION) : la CI l'installe, aucun skip ici.
test_that("nemeton est installe a la version minimale du contrat", {
  expect_true(requireNamespace("nemeton", quietly = TRUE))
  expect_gte(utils::packageVersion("nemeton"), "2026.11.35")
})

test_that("le contrat est sans doublon", {
  expect_false(anyDuplicated(.contrat_noms()) > 0L)
  expect_length(EXPORTS, 181L)
  expect_length(INTERNES, 17L)
})

test_that("chaque fonction exportee utilisee existe avec ses arguments", {
  .verifie_signatures(EXPORTS, interne = FALSE)
})

test_that("chaque fonction interne utilisee existe avec ses arguments", {
  .verifie_signatures(INTERNES, interne = TRUE)
})

test_that("indicator_families() rend les colonnes lues par l'app", {
  fam <- nemeton::indicator_families()
  expect_s3_class(fam, "data.frame")
  expect_identical(setdiff(COLONNES_FAMILLES, names(fam)), character(0))
  expect_gte(nrow(fam), 12L)
})

test_that("indicator_labels() rend les colonnes lues par l'app, en fr et en en", {
  for (lang in c("fr", "en")) {
    lab <- nemeton::indicator_labels(lang = lang)
    expect_s3_class(lab, "data.frame")
    expect_identical(setdiff(COLONNES_LIBELLES, names(lab)), character(0),
                     info = lang)
    expect_gt(nrow(lab), 0L)
  }
})

test_that("les indicateur_* de list_available_indicators() sont tous au contrat", {
  utilises <- grep("^indicateur_", list_available_indicators(), value = TRUE)
  expect_gt(length(utilises), 30L)
  expect_identical(setdiff(utilises, names(EXPORTS)), character(0))
})

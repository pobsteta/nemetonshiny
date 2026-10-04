# BRIEF `nemeton` — `PLAN.md` : app 0.153.0 + 0.154.0, retour sur le brief 0.212.0, exports consommés (vague 6)

> **Statut** : ouvert, 2026-10-04.
> **Dépôt concerné** : `nemeton` — `PLAN.md` racine (journal + écarts) ;
> **aucun code cœur, aucune release cœur demandés.**
> **Émetteur** : session `nemetonshiny`, branche `claude/pilotage-mcp`.
> **Contexte** : `nemetonshiny` **v0.153.0** publiée (merge `c284bad2`,
> PR #216) ; **v0.154.0** en PR #217, CI verte, **à merger par Pascal**.
> Relever le SHA du merge de la #217 (`gh pr view 217 -R pobsteta/nemetonshiny --json mergeCommit`)
> avant de coller l'entrée 0.154.0 ; si elle n'est pas mergée, ne coller que
> la 0.153.0.

---

## 1. Ligne d'en-tête (ligne 5)

Remplacer la version publiée par **v0.154.0** une fois la release posée par
`release.yml` (`gh release list -R pobsteta/nemetonshiny`), sinon laisser
**v0.153.0**.

## 2. Journal du chantier « Pré-version 1.0 (audit du 2026-10-02) »

À coller **sous** l'entrée `*2026-10-03* (**v0.212.0**)` (et son ajout du
2026-10-04), **au-dessus** de « **Prochaine étape** : vague 6 ».

```markdown
**Journal** — *2026-10-04* (**app v0.153.0**, `nemetonshiny@c284bad2`, PR #216) :
phase 3 de l'audit côté app (contrat public et packaging). `run_app(options =)`
réellement utilisable (port, hôte), `CONTRAT.md` (contrat public de la 1.0),
licence GPL-3+ alignée partout, image Docker reconstruite et vérifiée,
`R CMD check` sans WARNING ni NOTE, `main` protégée. `opencanopy` résolu à
l'exécution (retiré des `Suggests`).

**Journal** — *2026-10-04* (**app v0.154.0**, `nemetonshiny@<SHA merge #217>`,
PR #217) : **API hors interface** (brief aigora-nemeton du 2026-10-04) et
**suivi du brief cœur 0.212.0**.
- Neuf fonctions exportées par l'app (`?api_hors_interface`) : `projets_lister`,
  `projet_etat`, `projet_lire`, `projet_migrer`, `parcelles_commune`,
  `projet_creer`, `projet_calculer`, `projet_rapport`, `projet_gpkg`.
  `projet_lire()` n'écrit rien (test d'empreinte) ; une migration nécessaire
  lève `nemetonshiny_projet_perime`, seule `projet_migrer()` l'applique.
- **Sûreté** : l'invalidation des indicateurs **renomme**
  (`indicators.perime-v<n>-<date>.parquet`, deux générations, listées dans
  `metadata$indicateurs_perimes`) au lieu de supprimer — un simple
  `load_project()` d'un projet au sens v2 détruisait ses indicateurs
  (Couchey). Un projet neuf porte le marqueur de sens courant : créé puis
  calculé hors interface, il n'est plus invalidé à sa première ouverture.
- Brief 0.212.0 : plancher **`Imports: nemeton (>= 0.212.0)`** (§1.1) ; tests
  alignés sur la borne E1/E2 = 2,64. **Non traités** : câblage T2 et libellés
  `r1_status` (écarts n° 18 et 19).
Suite app : 14 667 expectations, 0 échec, 7 SKIP (contre le cœur 0.212.0).
```

## 3. Écarts ouverts — deux lignes à ajouter

Dans le tableau « Écarts ouverts dans la chaîne cœur → app → foretaccess »,
après le n° 17, et mettre à jour le décompte du paragraphe qui suit
(« Trois écarts… » → cinq).

```markdown
| 18 | T2 sort NA sur tous les projets : l'app ne lui passe ni `T1` ni `N2` (N2 calculé après T2) | cœur **v0.212.0** (T2 = NA sans source) | `nemetonshiny` | Brief 0.212.0 §4 lu ; câblage à faire dans `service_compute.R` (`.units_for_indicator()` : injecter `T1`, calculer N2 avant T2). Prévu au cycle app 0.154.0.9xxx |
| 19 | `r1_status` (méthode de R1) non traduit ni affiché ; `r1_fallback_reason` non transporté | cœur **v0.212.0** (ajout du 2026-10-04) | `nemetonshiny` | `.r1_status` déjà transporté dans le parquet par `.capture_status_attr()` ; reste i18n FR/EN + explication des `skipped_*` dans `mod_family.R` |
```

## 4. Pour la vague 6 (contrat d'API, « 313 exports à trier »)

L'app consomme aujourd'hui **135 des 313 exports** du cœur (inventaire du
2026-10-04 : appels `nemeton::` et symboles importés via `import(nemeton)`).
**Ne pas les retirer ni changer leur signature sans brief app** — ce serait une
rupture pour `nemetonshiny` *et*, depuis 0.154.0, pour son API publique.

```
a5_applicabilite aggregate_plot_metrics attach_field_data_to_units
bdforet_v2_mapping build_biljou_soil build_foret_ancienne_mask
build_index_stack build_knowledge_corpus build_project_monitoring_zones
canopy_provenance cec_to_fertility_score check_fordead_validity
check_reconfort_validity compute_dtm_chm_from_laz compute_fast_alert_mask
compute_general_index compute_sample_size compute_spectral_diversity
create_family_index create_qfield_project create_qgis_project
create_sampling_plan create_trend_sanitary_plan
create_validation_sampling_plan croiser_parcelles_onf cv_from_bdforet
db_connect db_disconnect db_migrate delete_knowledge_document detect_ndp
enrich_parcels_bdforet ensure_inventory_fields eobs_downscale
eobs_downscale_bivariate eobs_monthly_climatology eobs_summer_series
eobs_trend_fit extract_indicator_value extract_pixel_timeseries
extract_pixel_trend find_zone_by_project find_zones_by_project
format_citations format_duration get_data_source get_famille_code
get_famille_col get_global_cache_dir get_layer_service get_ndp_level
get_storage_crs ifn_covariables_domaines ifn_production_domaines
ifn_taux_prelevement_production import_qfield_gpkg import_qgis_gpkg
indicateur_a3_microclimat indicateur_a4_tamponnement
indicateur_a5_rafraichissement indicateur_r3_secheresse
indicateur_r5_deperissement indicateur_r6_sensibilite indicateur_r7_gel
indicateur_t3_coupes_rases indicateur_w4_vpd indicator_families
indicator_labels indice_priorite_regen ingest_health_validation
ingest_sentinel2_timeseries knowledge_manifest_path knowledge_manifest_vocab
lai_max_depuis_pai lai_sentinel2 list_alerts list_indicators
list_knowledge_documents list_species_regions load_biljou_forcing
load_eobs_source load_foret_ancienne_source load_insee_population_source
load_onf_parcelles_source load_raster_source load_theia_source localiser_ser
meteoland_daily_grid microclimate_detect_years migrer_colonnes_l
nemeton_radar normalize_indicator prepare_pixel_dieback_series
probe_ign_lidar_tiles project_lock_acquire project_lock_heartbeat
project_lock_release project_lock_status prune_orphan_zone_caches
r5_applicabilite read_fast_alert_mask read_fast_alert_raster
read_fordead_dieback_mask read_fordead_layer read_fordead_pixel_series
read_knowledge_manifest read_reconfort_alert_mask read_reconfort_layer
read_reconfort_pixel_series read_s2_band_raster reconfort_cache_manifest
reconfort_layer_manifest reconfort_year_bounds regen_bilan_hydrique
regen_rank_species regen_sensibilite regen_species_choices
regeneration_tolerances register_monitoring_zone reset_knowledge_manifest
resolve_project_chm resolve_project_dem retrieve_knowledge
run_fordead_dieback run_memory_capped run_reconfort_dieback sanitize_chm
segment_houppiers smooth_pixel_series tag_field_data_sources
theia_source_status validate_field_data validate_knowledge_manifest
volume_mobilisable write_knowledge_manifest
```

S'y ajoutent **21 symboles internes** lus par `utils::getFromNamespace()`
(`R/imports.R` et quelques services). Ils ne sont pas exportés : le tri de la
vague 6 est l'occasion de les **exporter** (ou de dire lesquels l'app doit
cesser d'utiliser) plutôt que de les renommer en silence :

```
FAMILLE_NMT_MAP as_pure_sf clean_indicator_name detect_ndp_from_cache
enrich_parcels_bdforet get_allometric_coefficients get_dem_raster
get_famille_code get_famille_col get_language map_essence_to_species
msg msg_error msg_info msg_success msg_warn resolve_raster_layer
resolve_vector_layer restore_ndp_attributes safe_extract set_ndp_attributes
```

(`get_famille_code`, `get_famille_col` et `enrich_parcels_bdforet` figurent
dans les deux listes : exportés aujourd'hui, l'app passe encore par
`getFromNamespace()` par héritage — sans conséquence, à nettoyer côté app.)

Les exports **non** listés ci-dessus (≈ 178) ne sont pas appelés par l'app :
le cœur peut les trier librement de son point de vue.

## 5. Ce qui n'est **pas** demandé

- Aucun changement de code cœur. T2 et `r1_status` relèvent de l'app.
- Pas de nouvelle release cœur.
- Le pilotage VICTOR / AIGORA (`nemetonshiny/specs/BRIEF-pilotage-victor-aigora.md`)
  n'impacte pas le cœur : il s'appuie sur l'API app ci-dessus et sur des
  fonctions cœur déjà publiées.

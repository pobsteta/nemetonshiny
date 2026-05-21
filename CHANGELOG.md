# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

For a narrative, per-feature description of each release, see
[NEWS.md](NEWS.md). This file is the concise, categorised trail.

## [Unreleased]

## [0.40.0] - 2026-05-21

### Added

- **Verrou croisé FAST ↔ FORDEAD** : les deux diagnostics partagent le
  cache de bandes Sentinel-2 du projet ; ils sont désormais mutuellement
  exclusifs. Le bouton de lancement de l'un est grisé tant que l'autre
  tourne, un clic forcé affiche une notification explicite, et le
  verrou respecte le *force-unlock* (run abandonné via « Annuler »).

### Changed

- **`ingest_task` renommé `fast_task`** (variable interne de
  `mod_monitoring`, helper de test `make_fake_fast_task`, clé du retour
  de `mod_monitoring_server()`), par symétrie avec `fordead_task`. La
  fonction service `run_ingestion_async()` conserve son nom.

## [0.39.1] - 2026-05-21

### Fixed

- **`db_status` plantait sans projet chargé** : `bsicons::bs_icon()`
  était appelé avec l'identifiant inexistant `folder-open` →
  `folder2-open`.
- **`.build_progress_writer` laissait fuir un avertissement** sur
  écriture en répertoire absent → `suppressWarnings()`.
- **`audit_to_dataframe` ne renvoyait pas un data.frame propre** : la
  classe `json` de `jsonlite::toJSON()` se propageait à toute la
  colonne via `rbind()` → dé-classage `as.character()`.

### Changed

- **Réparation des suites de tests `monitoring` et `sampling`** :
  20 échecs préexistants corrigés (dérive tests↔code après
  évolutions). Mocks à signature trop étroite élargis, isolation des
  variables d'environnement DB, assertions de comptage de placettes
  recentrées sur le contrat de l'app plutôt que sur l'arithmétique de
  stratification GRTS du cœur. Deux tests `db_status` probe-gated
  marqués `skip()` (sonde DB asynchrone non pilotable par testServer).

## [0.39.0] - 2026-05-21

### Added

- **Notifications ntfy pour les runs FORDEAD longs** : canal de push
  `ntfy` émis côté worker `future` (donc indépendant de la survie de
  la session Shiny) — message au démarrage, un message par étape
  FORDEAD (dédupliqué), message de fin (nb d'alertes + durée lisible)
  et message d'échec. Opt-in via `NEMETON_NTFY_TOPIC` ; serveur et
  jeton optionnels (`NEMETON_NTFY_URL`, `NEMETON_NTFY_TOKEN`).
  No-op silencieux si non configuré.

### Fixed

- **Onglets FORDEAD figés après un run hors-session** : « Alertes
  FORDEAD » et « Carte FORDEAD » ne se rafraîchissaient pas quand un
  run survivait à sa session Shiny (run long + déconnexion du
  navigateur). Deux correctifs : (1) ouvrir un sous-onglet FORDEAD
  force la re-lecture base + masque disque ; (2)
  `.reconcile_fordead_state()` reconstruit le résultat « succès »
  depuis le masque persisté au chargement du projet, affichant la
  carte « Zone saine » datée au lieu du placeholder générique.

### Changed

- Libellé du placeholder « pas de cache » de la Carte FAST :
  « Lance une ingestion FAST… » → « Lance le diagnostic FAST… ».

## [0.38.8] - 2026-05-20

### Changed

- **`Remotes:` suit la dernière release `nemeton`** : passage de
  `pobsteta/nemeton@v0.41.0` (tag figé) à `pobsteta/nemeton@*release`.
  La référence `@*release` résout à chaque install le tag de
  release le plus élevé du cœur — l'app tire toujours la plus haute
  version `nemeton` publiée, sans bump manuel du pin. Le tag figé
  forçait l'install de `nemeton 0.41.0` alors que `v0.41.2` était
  publié. Plancher `Imports: nemeton (>= 0.41.0)` inchangé (minimum
  strict, pas un suivi). `CLAUDE.md` mis à jour (`DESCRIPTION`,
  `CLAUDE.md`).

## [0.38.7] - 2026-05-20

### Fixed

- **Warnings leaflet « Some values were outside the color scale »**.
  Deux causes : (1) Carte FORDEAD — `addRasterImage()` rééchantillonnait
  le masque catégoriel 0-4 en `bilinear` (défaut), créant des valeurs
  fractionnaires hors des niveaux `colorFactor` ; fix `method = "ngb"`
  + `colorFactor(levels = 0:4)`. (2) Carte FAST — `colorNumeric`
  ancré sur `[-1, 1]` recevait des NDVI/NBR/CRSWIR de bord hors
  domaine ; fix `terra::clamp(r, -1, 1, values = TRUE)` avant
  `addRasterImage()` (`R/mod_monitoring_fordead_map.R`,
  `R/mod_monitoring_pixel_map.R`).

## [0.38.6] - 2026-05-20

### Fixed

- **Carte FORDEAD ne se rafraîchit pas après un run** : le masque
  0-4 persisté par `nemeton@v0.41.0` était bien écrit sur disque
  mais le sous-onglet restait sur son empty-state. Le reactive
  `mask_r()` de `mod_monitoring_fordead_map` ne dépendait que de
  `input$zone_id` / `current_project` — rien ne l'invalidait à la
  fin d'un run. Nouveau paramètre `refresh_r` câblé sur le
  compteur `alerts_refresh` du parent (bumpé par le handler de
  résultat FORDEAD) ; `mask_r()` le lit → un run terminé relit le
  cache et affiche le masque (`R/mod_monitoring.R`,
  `R/mod_monitoring_fordead_map.R`).

### Tests

- Nouveau `test-mod_monitoring_fordead_map.R` (3 tests : UI,
  empty-state, refresh).

## [0.38.5] - 2026-05-20

### Changed

- **Bump `nemeton` v0.40.0 → v0.41.0** (`DESCRIPTION` : `Imports`
  floor + `Remotes` tag pin). v0.41.0 ship le writer du masque de
  dépérissement FORDEAD : `run_fordead_dieback()` persiste le
  raster catégoriel 0-4 dans
  `<project>/cache/layers/fordead/zone_<id>/dieback_mask_<ts>.tif`,
  le chemin lu par `read_fordead_dieback_mask()`. Le sous-onglet
  « Carte FORDEAD » (`mod_monitoring_fordead_map`, câblé depuis
  v0.36.0) cesse donc d'être un empty-state permanent et affiche
  le masque après un run FORDEAD. Aucun changement de code app —
  pur bump de dépendance ; signatures vérifiées rétrocompatibles.

## [0.38.4] - 2026-05-20

### Changed

- **Suivi sanitaire / `obs_pixel_data` debounced** : au chargement
  de projet, les 5 entrées dont dépend `obs_pixel_data` sont
  restaurées une à une → 4-5 ré-exécutions avec autant de requêtes
  SQL `read_obs_pixel` redondantes. Nouveau reactive
  `obs_pixel_inputs` (assemblage des 5 entrées) debouncé 300 ms ;
  `obs_pixel_data` ne dépend plus que de ce paquet → la requête
  tourne une fois par rafale. `shiny::debounce()` évaluant sa
  source de façon eager, c'est bien le *déclencheur* peu coûteux
  qui est debouncé, pas le reactive coûteux (`R/mod_monitoring.R`).
- **Logs de debug de la carte pixel gatés** : les 9
  `cli::cli_alert_info()` « UGF source / overlay / Placettes
  overlay » de `R/mod_monitoring_pixel_map.R` passent derrière
  `NEMETON_PIXEL_MAP_DEBUG` (helper `.pixel_map_debug_enabled()`).
  Console silencieuse par défaut.

### Tests

- `test-mod_monitoring.R` : test `testServer()` du debounce de
  `obs_pixel_data` (3 changements rapides de `zone_id` → 1 requête).

## [0.38.3] - 2026-05-20

### Fixed

- **Cache LiDAR HD non extent-aware** (`R/service_compute.R`). Deux
  bugs corrigés dans `download_ign_lidar_hd()` :
  - **Nuages de points** : le court-circuit global qui renvoyait
    toutes les dalles `.copc.laz` du cache dès qu'une seule existait
    (sans comparaison de bbox) est supprimé. La fonction interroge
    toujours le WFS et s'appuie sur le cache par-dalle de la boucle
    de téléchargement → recompute même zone = zéro réseau, zone
    différente = seules les dalles manquantes, jeu incomplet
    auto-réparé.
  - **Mosaïques raster (MNH/MNT/MNS)** : `lidar_<product>_mosaic.tif`
    n'est plus réutilisée sur un simple `file.exists()`. Nouveau
    helper `.lidar_mosaic_covers_bbox()` qui vérifie que l'emprise
    du raster en cache couvre la bbox demandée (comparaison en CRS
    commun) ; sinon la mosaïque est régénérée.

### Tests

- `test-service_compute.R` : test COPC obsolète réécrit + 3 tests
  ajoutés (recompute même zone, zone différente, régénération
  mosaïque) + test unitaire de `.lidar_mosaic_covers_bbox()`.

## [0.38.2] - 2026-05-20

### Fixed

- **Suivi sanitaire / sous-onglets blancs** : « Carte FORDEAD » (et
  « Alertes FAST ») s'affichaient totalement vides — pas même
  l'empty-state. Les `uiOutput`/`renderUI` des modules
  `mod_monitoring_fordead_map` et `mod_monitoring_fast_alerts`
  restaient suspendus (`suspendWhenHidden = TRUE` par défaut)
  parce que le mécanisme `bslib::nav_show()` / `nav_hide()` du
  navset casse la détection de visibilité par-output de Shiny.
  Fix : `outputOptions(..., suspendWhenHidden = FALSE)` sur les
  outputs `panel` / `counters` des deux modules, + `nav_select()`
  dans l'observer mode-driven pour ré-ancrer l'onglet actif sur
  un onglet visible au changement de mode (`R/mod_monitoring.R`,
  `R/mod_monitoring_fordead_map.R`,
  `R/mod_monitoring_fast_alerts.R`).

## [0.38.1] - 2026-05-20

### Fixed

- **Câblage du CHM Theia vers P1/P2/P3/E1** : `compute_single_indicator()`
  transmet désormais `age_field = "age"` à `indicateur_p2_station()`
  (mode CHM hauteur/âge), en plus de `chm` et `species_field` déjà
  câblés. Sans cela, P2 échouait avec `Missing required fields:
  fertility, climate`.
- **Échec explicite sans CHM** : nouvelle constante
  `CHM_REQUIRED_INDICATORS` (P1/P2/P3/E1). En l'absence de modèle de
  hauteur de canopée, ces indicateurs échouent avec un message i18n
  clair (`compute_chm_required`) au lieu de l'erreur cryptique du
  cœur `nemeton`, sans interrompre le reste du calcul.

## [0.38.0] - 2026-05-20

### Added

- **Intégration Theia / DATA TERRA (nemeton v0.40.0)** : nouveau
  service `R/service_theia.R` (détection du pré-requis Python /
  reticulate et de la clé API Theia, persistance de la clé,
  chargement du CHM FORMSpoT via `nemeton::load_theia_source()`
  avec conversion décimètres → mètres, chargement des rasters
  secondaires FAPAR / neige / humidité du sol, provenance des
  sources). Débloque la famille Production (P1/P2/P3) et E1 en
  NDP 0 à partir de données publiques.
- **Module de configuration Theia** (`R/mod_theia_config.R`) :
  entrée navbar (engrenage) ouvrant une modale de saisie de la
  clé API, statut du pré-requis Python et provenance / licence
  des sources Theia.

### Changed

- `R/service_compute.R` : nouvelle étape CHM Theia FORMSpoT dans
  `download_layers_for_parcels()` (utilisée quand le LiDAR HD est
  absent, avant Open-Canopy) ; `compute_single_indicator()`
  transmet `species_field`, `fapar`, `snow` et `soil_moisture`
  aux fonctions `nemeton` qui les acceptent ; enrichissement
  BD Forêt V2 (`species`/`age`) étendu à P1, P3 et E1.
- `DESCRIPTION` : `Imports: nemeton (>= 0.40.0)`,
  `Remotes: pobsteta/nemeton@v0.40.0`, `reticulate` en Suggests.

## [0.37.0] - 2026-05-19

### Added

- **Suivi sanitaire / G3 espèces — fallback BD Forêt V2** : le
  reactive `validity` charge désormais
  `<project>/cache/layers/bdforet.gpkg` via le nouveau helper
  `.load_project_bdforet()` et le passe à
  `validity_check_for_zone()`. Quand `units` n'a pas de colonne
  d'essence (cas par défaut des UGFs de l'app), le cœur
  (`nemeton@v0.26.0+`) dérive l'essence dominante via
  `enrich_parcels_bdforet()` et exécute le check espèces — le
  garde-fou G3 cesse d'être silencieusement désactivé
  (`R/mod_monitoring.R`, `R/service_monitoring_db.R`).
- `validity_check_for_zone()` accepte désormais un paramètre
  `bdforet = NULL` qu'il transmet directement à
  `nemeton::check_fordead_validity()`.
- 3 tests testthat couvrant le helper et le forwarding cœur
  (`tests/testthat/test-mod_monitoring.R`).

### Changed

- `DESCRIPTION` : plancher `Imports: nemeton (>= 0.26.0)` (au
  lieu de 0.25.4) — ancre la version qui expose les nouveaux
  arguments `bdforet` / `layers` de `check_fordead_validity()`.

## [0.36.8] - 2026-05-19

### Fixed

- **Suivi sanitaire / résolution FORDEAD** : trois fixes UX à la
  fin d'un run FORDEAD réussi. (1) Le bouton « Lancer le diagnostic
  FORDEAD » ne se ré-enable pas systématiquement quand
  `fordead_task$status()` transite de « running » à « success » ;
  ajout d'un `updateActionButton(disabled = FALSE)` + reset
  `force_unlock_health(FALSE)` explicite dans le handler de résultat
  (success ET error). (2) L'onglet « Alertes FORDEAD » restait
  muet quand `n_alerts_inserted == 0L` ; nouvelle card « Zone
  saine » avec durée du run quand `fordead_last_result()$status ==
  "success"` et que `alerts()` est vide. (3) Le snapshot de
  résultat est désormais conservé en `reactiveVal`
  (`fordead_last_result`) pour distinguer « pas encore lancé » /
  « run terminé sans anomalie » / « run terminé en erreur ».
  (`R/mod_monitoring.R`)

### Added

- 3 nouvelles clés i18n FR/EN pour la card « Zone saine » :
  `monitoring_fordead_no_alerts_title`, `_body`, `_meta`.
- `make_fake_fordead_task()` widened pour accepter `result =` /
  `status =` (préparation des futurs tests, harness actuel ne
  permet pas un test testServer du nouveau branch d'affichage).

## [0.36.7] - 2026-05-18

### Fixed

- **Sampling / câblage MNT-CHM sur `create_sampling_plan()`** : les
  rasters résolus via `nemeton::resolve_project_*` n'étaient pas
  passés à `create_sampling_plan()` (manque de `mnt =` / `chm =` au
  call site), si bien que `<project>/dtm.tif` opencanopy n'était
  jamais consommé. Pré-check ajouté : DEM absent → toast bloquant
  `sampling_no_dem_found_fmt` (i18n, avec chemin projet)
  `id = session$ns("dem_missing")` et arrêt avant l'appel cœur ;
  CHM absent → `cli::cli_alert_info` sans toast bloquant
  (`R/mod_sampling.R`).

### Changed

- Toast informatif `sampling_dem_resolved_fmt` (« MNT : %s », 5 s)
  qui surface `attr(dem, "nemeton_dem_layer")`
  (« opencanopy DTM », « LiDAR HD MNT », « IGN BD ALTI »…).
- 3 clés i18n FR/EN remplaçant les 4 ajoutées en v0.36.6 :
  `sampling_no_dem_found_fmt`, `sampling_dem_resolved_fmt`,
  `sampling_chm_missing` (`R/utils_i18n.R`).
- `DESCRIPTION` : `Imports: nemeton (>= 0.25.4)` (au lieu de
  `0.21.10`).

### Added

- 2 tests testthat ciblés (`tests/testthat/test-mod_sampling.R`) :
  câblage `mnt = <SpatRaster> / chm = NULL` vérifié via mock de
  `nemeton::create_sampling_plan` ; toast `dem_missing` + non-appel
  cœur vérifié quand `resolve_project_dem` renvoie NULL.
- Helper `make_fake_dem()` + 4 tests existants enveloppés dans
  `testthat::local_mocked_bindings(resolve_project_dem = ...,
  resolve_project_chm = ..., .package = "nemeton")` pour préserver
  le contrat « generate produit des plots ».

## [0.36.6] - 2026-05-18

### Changed

- **Sampling / résolution MNT/CHM déléguée à `nemeton`** : les
  réactives `chm_raster()` / `mnt_raster()` de `mod_sampling`
  appellent désormais `nemeton::resolve_project_chm()` et
  `nemeton::resolve_project_dem()` (nemeton >= 0.21.10) au lieu de
  faire leur propre lookup dans `<project>/cache/layers/`. Couvre les
  noms canoniques `dtm.tif`, `mnh.tif`, `lidar_mnh.tif` en plus des
  mosaics historiques (`R/mod_sampling.R`).

### Added

- **Pré-check DEM/CHM avant `create_sampling_plan()`** : toast erreur
  `mnt_missing` quand le DEM est absent (arrête l'appel pour éviter
  l'abort « Stratification-valid candidate pool (0) is below
  n_base ») ; warning soft `chm_missing` quand le CHM est absent ;
  toasts informatifs `mnt_found_fmt` / `chm_found_fmt` exposant la
  couche résolue via `attr(., "nemeton_dem_layer")` /
  `nemeton_chm_layer`.
- 4 clés i18n bilingues FR/EN (`R/utils_i18n.R`).

## [0.36.5] - 2026-05-18

### Fixed

- **Sampling / notification d'erreur `create_sampling_plan()`** : les
  séquences ANSI `cli` (`[38;5;250m`, `[31m`, `[36m`, `[39m`) issues de
  `cli::cli_abort()` côté `nemeton` apparaissaient brutes dans le toast
  Shiny. Le `conditionMessage(e)` est désormais nettoyé via
  `cli::ansi_strip()` avant `showNotification()` (`R/mod_sampling.R`).

## [0.36.4] - 2026-05-17

### Fixed

- **Suivi sanitaire / toast d'avertissement backend** : les warnings
  Sentinel-2 contenant une URL pré-signée Azure (~400 chars de SAS
  token) transformaient le toast en mur de texte. Nouveau helper
  interne `.summarize_backend_warnings()` qui remplace les URLs par
  `<URL>`, normalise les espaces et cap chaque warning à 200 chars
  (`R/mod_monitoring.R`).

### Added

- 2 tests testthat pour `.summarize_backend_warnings()` (cas réel
  SAS-token Azure + edge cases NULL / NA / multi-line)
  (`tests/testthat/test-mod_monitoring.R`).

## [0.36.3] - 2026-05-17

### Fixed

- **Suivi sanitaire / Carte FAST** : markers placettes (cercles
  bleus) invisibles depuis v0.34.0 sur certains navigateurs. Les
  CircleMarkers vivaient dans `overlayPane` à côté des polygones
  UGF ; selon l'ordre de re-draw les polygones finissaient en fin
  de `<g>` SVG et masquaient les markers. Pinned explicitement
  dans `markerPane` (z=600) via `pathOptions(pane = "markerPane")`,
  z-stack désormais strictement séparé
  (`R/mod_monitoring_pixel_map.R`).

## [0.36.2] - 2026-05-17

### Fixed

- **Suivi sanitaire / zone monitoring** : la liste des zones et la
  zone sélectionnée ne se mettaient pas à jour au changement de
  projet. Deux causes corrigées dans `R/mod_monitoring.R` :
  - Le reactive `zones` n'avait pas de dépendance effective sur
    `app_state$current_project` en mode Postgres (le resolver
    d'URL retournait tôt sans forcer le promise lazy). Lecture
    explicite via `proj <-` ajoutée.
  - L'observer qui pousse les zones dans le `selectInput` retombait
    sur la première zone alphabétique quand le projet n'avait pas
    de `monitoring_zone_id` — affichant la zone d'un autre projet.
    Sélection vidée (`character(0)`) à la place ; tous les
    downstream bailent déjà proprement sur zone vide.

## [0.36.1] - 2026-05-17

### Fixed

- **Suivi sanitaire / sidebar FAST** : sliders `threshold_ndvi` et
  `threshold_nbr` réalignés sur la sémantique « seuil absolu »
  consommée par `nemeton::list_fast_alerts_for_zone()` depuis
  v0.36.0. Défauts `0.40 / 0.30` (cœur defaults), range
  `[0.10, 0.80]` (au lieu de `0.15 / 0.25`, range `[0.05, 0.50]`
  hérités de la sémantique drop E6.a). Labels i18n recyclés
  (« Seuil minimum NDVI/NBR »). Empty-state des Alertes FAST :
  « relever le seuil » au lieu de « baisser le seuil »
  (`R/mod_monitoring.R`, `R/utils_i18n.R`).

## [0.36.0] - 2026-05-17

### Added

- **Suivi sanitaire / Alertes FAST** : module `mod_monitoring_fast_alerts`
  câblé sur `nemeton::list_fast_alerts_for_zone()`. Carte Leaflet des
  placettes par sévérité (critical/warning/info), compteurs au-dessus,
  popups par marker avec valeurs NDVI/NBR + drop. Remplace le
  placeholder de v0.35.0 (`R/mod_monitoring_fast_alerts.R`).
- **Suivi sanitaire / Carte FORDEAD** : module `mod_monitoring_fordead_map`
  câblé sur `nemeton::read_fordead_dieback_mask()`. Raster catégoriel
  0..4 affiché dans le pane `nemetonRaster` (z-index 250). Empty state
  cohérent tant que le writer cœur (persist du mask) n'a pas shippé
  (`R/mod_monitoring_fordead_map.R`).
- 17 nouvelles clés i18n FR/EN : sévérités FAST (`critical`, `warning`,
  `info`), compteur total, empty states + popups FAST,
  titre + classes 0..4 FORDEAD, empty state FORDEAD.

### Changed

- `DESCRIPTION` : plancher `Imports: nemeton (>= 0.25.0)` (depuis
  0.24.1) pour ancrer les deux nouveaux exporteurs consommés.
- `R/mod_monitoring.R` : les nav_panels `alerts_fast` et `pixel_map_fordead`
  consomment les UI modules au lieu d'inline placeholders. Server
  instancie les deux nouveaux modules + retourne leurs reactives.

## [0.35.1] - 2026-05-17

### Fixed

- **Terrain / Plan d'échantillonnage** : erreur
  `le tableau de remplacement a N lignes, le tableau remplacé en a M`
  remontée en toast quand un CHM et/ou un MNT étaient fournis avec
  une AOI bordurale. Fix dans `nemeton@v0.24.1` (filtrage des
  candidats GRTS avant `spsurvey::grts()`), consommé automatiquement
  via `Remotes: pobsteta/nemeton@main`. Aucun changement de code
  côté app.

### Changed

- `DESCRIPTION` : plancher `Imports: nemeton (>= 0.24.1)` pour
  bloquer un downgrade qui réintroduirait le bug sampling.

## [0.35.0] - 2026-05-17

### Added

- **Suivi sanitaire** : 4 sous-onglets symétriques FAST / FORDEAD —
  `Alertes FAST` + `Carte FAST` visibles en mode quick, `Alertes
  FORDEAD` + `Carte FORDEAD` visibles en mode health. Visibilité
  pilotée côté server via `bslib::nav_show()` / `nav_hide()` étendus
  aux 4 valeurs. Les deux placeholders (Alertes FAST, Carte FORDEAD)
  attendent les exporteurs cœur `list_fast_alerts_for_zone()` et
  `read_fordead_dieback_mask()` (`R/mod_monitoring.R`,
  `R/utils_i18n.R`).
- 4 nouvelles clés i18n FR/EN : `monitoring_subtab_alerts_fast`,
  `monitoring_subtab_alerts_fordead`,
  `monitoring_fast_alerts_placeholder_title`,
  `monitoring_fast_alerts_placeholder_body`.

### Changed

- Sous-onglet `alerts` renommé `alerts_fordead` (même contenu,
  label « Alertes FORDEAD »). Les `conditionalPanel` internes
  filtrant sur `input$mode == 'health'` sont supprimés — l'onglet
  entier est masqué en mode FAST par l'observer mode-driven.
- Texte du placeholder Carte FORDEAD : référence pointée vers
  « Alertes FORDEAD » au lieu de « Alertes ».

## [0.34.0] - 2026-05-16

### Fixed

- **Suivi sanitaire / Carte FAST** : cascade de redraws (raster +
  UGF + placettes) à chaque tick du slider date. Le raster est
  désormais épinglé dans un pane Leaflet custom `nemetonRaster`
  (z-index 250), entre `tilePane` (200) et `overlayPane` (400),
  via `addMapPane()` + `gridOptions(pane = "nemetonRaster")`. Le
  raster reste **visible** sur Satellite (un essai initial dans
  `tilePane` le faisait disparaître quand le `LayersControl`
  ré-ajoutait le tile satellite), tout en restant **sous les
  polygones et CircleMarkers** (qui restent cliquables sans
  ré-empilement). Les dépendances fictives `current_layer_r()`
  des observers UGF / Placettes sont supprimées — ils ne re-firent
  que quand leur source change vraiment
  (`R/mod_monitoring_pixel_map.R`).

### Added

- **Suivi sanitaire** : sous-onglets « Carte pixel (FAST) » et
  « Carte FORDEAD » séparés, visibilité pilotée par `input$mode`
  via `bslib::nav_show()` / `nav_hide()`. La Carte FORDEAD est un
  placeholder en attendant que le cœur expose le raster classifié
  des classes de dépérissement (`R/mod_monitoring.R`,
  `R/utils_i18n.R`).
- 4 nouvelles clés i18n FR/EN pour les libellés et le placeholder
  Carte FORDEAD.

### Changed

- `R/mod_monitoring_pixel_map.R` : valeur du nav_panel renommée
  `pixel_map` → `pixel_map_fast` (l'observe d'auto-zoom est aligné).

## [0.33.0] - 2026-05-16

### Changed

- **BREAKING (dep) — Migration vers `nemeton@v0.24.0`** : la
  signature de `nemeton::run_fordead_dieback()` a changé au cœur
  (`aoi` / `scenes_df` / `forest_mask` retirés, `con` / `zone_id` /
  `cache_dir` requis). Le pipeline passe de 5 à 6 phases avec une
  nouvelle phase 0 `ingest` qui télécharge les bandes Sentinel-2
  manquantes (B02/B05/B8A/B11) par-dessus celles déjà cachées par
  FAST (B04/B12).
- `R/service_monitoring.R` : worker `run_fordead_async()` adapté —
  perd `aoi`, gagne `cache_dir`, ouvre lui-même la connexion DBI.
- `R/mod_monitoring.R` : helper `.invoke_fordead()` simplifié — plus
  de `get_monitoring_zone_aoi()` ni de DBI éphémère côté app ;
  passage direct de `zone_id` et `cache_dir`.
- `DESCRIPTION` : plancher `Imports: nemeton (>= 0.24.0)`.

### Added

- Clé i18n `monitoring_fordead_phase_ingest` (FR « Téléchargement des
  bandes manquantes… » / EN « Downloading missing bands… »),
  consommée automatiquement par le dispatcher générique de phases
  livré en v0.32.0.

### Removed

- Mocks `get_monitoring_zone_aoi` (3×) et assertion
  `calls[[1]]$aoi` dans `tests/testthat/test-mod_monitoring.R` —
  l'AOI n'est plus matérialisée côté app.

## [0.32.0] - 2026-05-16

### Added

- **Suivi sanitaire** : toasts de progression FORDEAD en bas à
  droite. Branche le stream d'événements `fordead:start` /
  `fordead:phase` / `fordead:phase_done` / `fordead:complete` /
  `fordead:error` émis par `nemeton@v0.22.5+` sur des
  `shiny::showNotification` positionnées en bottom-right via
  override CSS `#shiny-notification-panel`. Affichage générique
  (i18n + humanized fallback) : un nouveau nom de phase shippé en
  `nemeton@v0.23.0` apparaît tel quel sans patch app
  (`R/mod_monitoring.R`, `R/utils_i18n.R`,
  `inst/app/www/css/custom.css`, `inst/app/www/css/custom.min.css`).
- 11 nouvelles clés i18n FR/EN : 4 templates + 7 labels per-phase
  1.x + 3 labels per-phase 2.x anticipés.

### Changed

- **DESCRIPTION** : `Imports: nemeton (>= 0.22.0)` → `(>= 0.22.5)`
  pour aligner sur l'API du `progress_callback` consommée par les
  toasts.

### Tests

- 3 nouveaux tests verrouillent le contrat du dispatcher
  `.fordead_handle_progress_event` (fordead:phase avec libellé i18n,
  fordead:start silencieux, fallback humanisé sur phase inconnue)
  dans `tests/testthat/test-mod_monitoring.R`.

## [0.31.5] - 2026-05-16

### Fixed

- **Suivi sanitaire / Carte pixel** : raster NDVI/NBR invisible
  sur fond Satellite (palette conventionnelle confondue avec
  l'imagerie naturelle, même à 0.85 d'opacité). Opacité bumpée
  0.85 → 1.0. Le contexte satellite reste visible autour du bbox
  du raster ; l'utilisateur peut toggle OSM s'il veut voir les
  parcelles à l'intérieur de la zone d'analyse
  (`R/mod_monitoring_pixel_map.R`).

## [0.31.4] - 2026-05-16

### Fixed

- **Suivi sanitaire / Carte pixel** : les marqueurs placettes
  n'étaient plus cliquables quand l'observe placettes firait avant
  l'observe UGF dans le même flush — les polygones interceptaient
  les clics. Échelle stricte de priorités : raster 100 (fond) →
  UGF 50 (milieu) → placettes 0 (haut, cliquables). Ajout aussi
  du dummy `current_layer_r()` dependency sur placettes pour
  qu'ils restent en haut après chaque update du raster
  (`R/mod_monitoring_pixel_map.R`).

## [0.31.3] - 2026-05-16

### Fixed

- **Suivi sanitaire / Carte pixel** : auto-zoom au chargement
  projet n'a vraiment jamais marché parce que la reactive firait
  AVANT que le widget Leaflet ne soit dans le DOM ; les commandes
  `leafletProxy` étaient queue puis rejouées sur une carte de
  taille 0×0, où `fitBounds` est un no-op silencieux. Refactor du
  pattern d'auto-zoom calqué sur `mod_ug.R:744-794` : observer la
  navigation `main_nav` + `monitoring-subtab` via `root_session`,
  délai 300 ms via `later::later`, `leafletInvalidateSize` puis
  `fitBounds` (`R/mod_monitoring_pixel_map.R`).

## [0.31.2] - 2026-05-16

### Fixed

- **Suivi sanitaire / Carte pixel** : le contour UGF orange était
  bien produit mais peint par-dessus par le raster NDVI/NBR (DOM
  order de `overlayPane` : dernière couche ajoutée = au-dessus,
  et le raster fire plus tard que l'UGF parce que `build_index_stack`
  est lourd). Fix : observe UGF dépend de `current_layer_r()` pour
  re-fire après chaque raster, et observe raster reçoit
  `priority = 100L` pour passer en premier dans un flush où les
  deux sont dirty (`R/mod_monitoring_pixel_map.R`).

## [0.31.1] - 2026-05-16

### Fixed

- **Suivi sanitaire / Carte pixel** : le contour orange de zone
  d'analyse n'apparaissait pas pour les projets sans
  `indicators_sf` ni `ugs.json` (placettes-only). Chaîne de
  fallback étendue à 4 sources : indicators_sf → ug_build_sf →
  raster bbox → placettes bbox. cli logs ajoutés pour identifier
  la source utilisée (`R/mod_monitoring_pixel_map.R`).
- **Suivi sanitaire / Carte pixel** : le raster NDVI/NBR était
  invisible sur fond Satellite (palette confondue avec l'imagerie
  naturelle). Opacité bumpée 0.75 → 0.85
  (`R/mod_monitoring_pixel_map.R`).

## [0.31.0] - 2026-05-16

### Removed (BREAKING)

- **Suivi sanitaire** : sous-onglet « Séries par placette » retiré.
  La vue multi-traces NDVI/NBR par placette (mode rapide) est
  remplacée par le clic sur marqueur placette de la Carte pixel.
  Le graphique de distribution d'alertes (mode sanitaire) qui
  partageait le même output disparaît également — à ré-ajouter à
  l'onglet Alertes si besoin (`R/mod_monitoring.R`,
  `R/utils_i18n.R`).

### Fixed

- **Suivi sanitaire / Carte pixel** : le contour UGF n'apparaissait
  pas et l'auto-zoom au chargement projet ne marchait pas pour les
  projets sans indicateurs calculés. `ugf_sf_r` tombe désormais
  sur `ug_build_sf(project)` quand `indicators_sf` est NULL — la
  géométrie UGF est disponible dès que l'utilisateur a défini ses
  UGFs (`R/mod_monitoring_pixel_map.R`).
- **Suivi sanitaire / Carte pixel** : cliquer un marqueur placette
  empilait le modal pixel sur le modal placette à cause de la
  propagation des clics `CircleMarker` (Leaflet Path) vers
  `map_click`. Flag horodaté `marker_just_clicked` posé par le
  handler marker, vérifié par le handler pixel avec un seuil de
  500 ms (`R/mod_monitoring_pixel_map.R`).

## [0.30.2] - 2026-05-16

### Fixed

- **Suivi sanitaire / Carte pixel** : les trois couches d'overlay
  (UGF, NDVI / NBR, Placettes) n'apparaissaient pas malgré leurs
  cases cochées dans le contrôle Leaflet. Cause : `overlayGroups`
  dans `addLayersControl` posait des cases pré-renderLeaflet
  alors que les couches arrivaient via `leafletProxy` async ; les
  références de couches restaient indéfinies côté JS. Drop de
  `overlayGroups`, overlays toujours visibles (`R/mod_monitoring_pixel_map.R`).

### Changed

- Contour UGF : épaisseur 2 → 3, opacité 0.9 → 1.0.
- Marqueurs placettes : rayon 5 → 7.
- Logs `cli::cli_alert_info()` ajoutés sur les reactives UGF,
  placettes et auto-zoom pour diagnostic terminal.

## [0.30.1] - 2026-05-16

### Changed

- **Suivi sanitaire / Mode rapide** : sémantique de la checkbox
  « Cache COG » inversée. Décoché (défaut) = nemeton vérifie le
  cache disque et télécharge uniquement les bandes manquantes
  (DB idempotente via `ON CONFLICT DO NOTHING`). Coché = wipe
  `<cache_dir>/*` puis re-télécharge intégralement scène par
  scène (pour récupérer d'un cache corrompu).
- L'ancien défaut court-circuitait sur la DB et laissait le cache
  disque vide, ce qui faisait re-télécharger intégralement
  FORDEAD au premier diagnostic. Le nouveau défaut prépare
  effectivement le terrain pour FORDEAD (`R/mod_monitoring.R`,
  `R/utils_i18n.R`).

### Tests

- Nouveau test de régression dans
  `tests/testthat/test-mod_monitoring.R` qui verrouille
  l'invariant `skip_cached = FALSE` dans l'appel à
  `ingest_task$invoke()`, quelle que soit la valeur de
  `input$reprime_cache`.

## [0.30.0] - 2026-05-16

### Added

- **Suivi sanitaire / Carte pixel** : couche UGF (périmètre du
  projet) affichée en contour orange au-dessus du raster NDVI/NBR.
  Troisième case à cocher « UGF » dans le contrôle des couches
  Leaflet, à côté de « NDVI / NBR » et « Placettes ». Visible sur
  les deux fonds OSM et Satellite
  (`R/mod_monitoring_pixel_map.R`).

### Fixed

- **Suivi sanitaire / Carte pixel** : auto-zoom au chargement projet
  retravaillé. Le correctif v0.29.1 (`observeEvent` sur `project$id`)
  ratait quand `indicators_sf` arrivait après `id` (chargement async)
  — la carte restait alors sur la vue Leaflet par défaut, donnant
  l'impression que le raster et les marqueurs étaient invisibles.
  Passage à `observe()` + `reactiveVal .last_fitted_id` qui couvre
  tous les ordres de population (`R/mod_monitoring_pixel_map.R`).

## [0.29.1] - 2026-05-16

### Fixed

- **Suivi sanitaire / Carte pixel** : la carte ne se centrait pas sur
  les UGF du projet au chargement — elle restait sur la vue Leaflet
  par défaut depuis le passage en `renderLeaflet` statique de v0.28.1.
  Ajout d'un `observeEvent` sur `project$id` qui appelle `fitBounds()`
  via `leafletProxy()`. Préserve le pan/zoom manuel après le centrage
  initial (`R/mod_monitoring_pixel_map.R`).

## [0.29.0] - 2026-05-16

### Added

- **Suivi sanitaire / Carte pixel** : overlay placettes cliquable.
  Les placettes du plan d'échantillonnage présentes dans la fenêtre
  courante apparaissent comme marqueurs cercles sur la carte ; un
  clic ouvre un modal plotly avec la série NDVI / NBR agrégée
  placette pour ce `plot_id`. Cohabite avec le clic pixel existant
  (`R/mod_monitoring_pixel_map.R`, `R/utils_i18n.R`).
- Contrôle des couches Leaflet enrichi : cases à cocher « NDVI / NBR »
  et « Placettes » permettent de masquer indépendamment chaque overlay.

## [0.28.5] - 2026-05-16

### Changed

- **Deps** : bascule de l'épingle `Remotes` de `pobsteta/nemeton@v0.22.1`
  vers `pobsteta/nemeton@main`. Les installs GitHub de `nemetonshiny`
  tirent désormais en continu le dernier commit `main` de `nemeton`
  — plus besoin de bumper l'épingle après chaque release cœur.
  Reproductibilité d'install dans le temps perdue (cf. NEWS.md pour
  les trade-offs) (`DESCRIPTION`).
- **Docs** : section *Stack technique* de `CLAUDE.md` mise à jour ;
  ajout d'une nouvelle section *Suivi de `nemeton@main` — implications
  pour les releases* ; suppression de la section *Épingle Remotes vers
  nemeton* devenue caduque (`CLAUDE.md`).

## [0.28.4] - 2026-05-15

### Fixed

- **Suivi sanitaire / Carte pixel** : la couche NDVI/NBR disparaissait
  visuellement au basculement OSM↔Satellite parce que le `group =`
  de `addRasterImage()` n'était pas déclaré dans
  `addLayersControl(overlayGroups=)`. Déclaration explicite de la
  couche overlay avec un libellé fixe « NDVI / NBR »
  (`R/mod_monitoring_pixel_map.R`).

### Removed

- Clé i18n orpheline `monitoring_pixel_map_layer` (FR/EN)
  supprimée — plus référencée depuis le fix ci-dessus
  (`R/utils_i18n.R`).

## [0.28.3] - 2026-05-15

### Changed

- **Deps** : bump de l'épingle `Remotes: pobsteta/nemeton` de
  `v0.22.0` vers `v0.22.1`. Sans ce bump, l'installation de
  `nemetonshiny` faisait redescendre `nemeton` à `0.22.0` même
  si une version plus récente était installée localement
  (`DESCRIPTION`).

## [0.28.2] - 2026-05-15

### Fixed

- **Suivi sanitaire** : après un téléchargement Sentinel-2 réussi,
  le graphique plotly des placettes et la sous-onglet *Carte pixel*
  ne se mettaient pas à jour automatiquement — l'utilisateur devait
  toucher à un contrôle (bandes, dates, zone) pour rafraîchir. Ajout
  d'un `reactiveVal` `obs_refresh` lu par `obs_pixel_data()` et
  bumpé en fin d'ingestion. Pattern symétrique à `alerts_refresh`
  côté FORDEAD (`R/mod_monitoring.R`).

## [0.28.1] - 2026-05-15

### Fixed

- **Suivi sanitaire / Carte pixel** : le fond satellite ne tenait pas
  quand l'utilisateur faisait défiler le slider de date ou changeait
  d'indice — Leaflet repassait sur OSM à chaque rendu. Le squelette
  de carte est désormais rendu une seule fois, et le raster + la
  légende sont mis à jour via `leafletProxy()`. Le choix de fond
  reste actif (`R/mod_monitoring_pixel_map.R`).

## [0.26.6] - 2026-05-13

### Fixed

- `fix(monitoring)`: worker `cli::cli_alert_*` output now actually
  reaches the parent R console in real time. The v0.26.5 `sink()`-
  based approach silently failed for cli messages because cli writes
  to `stderr()` directly via `cat(file = stderr())` in non-interactive
  mode, bypassing `sink(type = "message")` entirely. Replaced by
  `withCallingHandlers(message =, warning =)` wrapping
  `nemeton::ingest_sentinel2_timeseries()` — every condition (cli +
  plain `message()` + `warning()`) is rewritten to the log file with
  `writeLines()` + `flush()` and the original stderr write is muffled
  via `invokeRestart`. Includes `[s2_cache HH:MM:SS] …` traces when
  `NEMETON_S2_CACHE_DEBUG=TRUE`.
- `fix(db)`: `db_init_schema()` now suppresses the noisy
  `NOTICE: ... already exists, skipping` rafale that RPostgres
  surfaces via `message()` on each `CREATE ... IF NOT EXISTS`. The
  schema init loop is wrapped in `suppressMessages({...})`. Warnings
  and errors continue to propagate.

## [0.26.5] - 2026-05-13

### Added

- `feat(monitoring)`: when the **"Re-prime COG cache"** checkbox is
  ticked, `<project>/cache/layers/sentinel2/` is now wiped via
  `unlink(recursive = TRUE, force = TRUE)` right before
  `ingest_task$invoke()`. Without this, even with
  `skip_cached = FALSE`, nemeton's `.get_s2_band_raster()` served
  the `B0X.tif` files already present on disk (CACHE-HIT branch),
  silently defeating the toggle. The on-disk cache and the DB
  cache are now both forced. A `cli::cli_alert_info` reports how
  many entries were purged.
- `feat(monitoring)`: worker stdout + message stream `sink()`ed to
  `<project>/data/ingest_console.log`. The parent process tails
  the file every 500 ms via `reactivePoll`, reads newly-written
  bytes from a persistent offset and `cat()`s them to its own
  `stderr()`. Effect: every `cli::cli_*`, `message()`, `cat()` and
  `[s2_cache …]` trace from `nemeton::ingest_sentinel2_timeseries()`
  (including the verbose `NEMETON_S2_CACHE_DEBUG=TRUE` ones) lands
  in the developer's R console in real time, bypassing `future`'s
  built-in stdout capture. Cleanup mirrors the existing
  `progress.json` channel on success / error paths.

### Changed

- `run_ingestion_async()` (R/service_monitoring.R) gains an optional
  `log_path` parameter on its `$invoke()` signature. NULL = no
  console mirror (the legacy silent behaviour).

## [0.26.4] - 2026-05-13

### Added

- `feat(monitoring)`: worker instrumentation to diagnose async
  ingestion hangs. Two heartbeats emitted via `progress_callback`
  before the `nemeton::ingest_sentinel2_timeseries()` call:
  - `s2:worker_started` (post load_all + db_connect)
  - `s2:nemeton_call_starting` (about to enter nemeton)
- `feat(monitoring)`: wrap the nemeton call in `tryCatch` and emit
  `s2:fatal_error` (with `error_message` + `error_class`) before
  re-throwing. Replaces the opaque "MultisessionFuture was
  interrupted" with the real R error message.
- `feat(monitoring)`: observer routes the new events. Heartbeats
  update the persistent progress toast + `cli::cli_alert_info`
  mirror; fatal errors trigger `cli::cli_alert_danger` +
  `showModal()` with the full message.
- i18n: `monitoring_ingest_worker_event_fmt`,
  `monitoring_ingest_fatal_title`.

## [0.26.3] - 2026-05-13

### Fixed

- `fix(monitoring)`: propagate `NEMETON_*` environment variables
  from the Shiny main session to the `future::multisession` worker.
  Windows workers are separate `Rscript.exe` processes that don't
  inherit env vars set after their spawn — so
  `Sys.setenv(NEMETON_S2_CACHE_DEBUG = "TRUE")` was silently lost.
  `run_ingestion_async()` and `run_fordead_async()` now snapshot
  the relevant `NEMETON_*` vars at invoke time (parent side) and
  replay them at the top of the `future_promise()` body (worker
  side).

### Added

- `.capture_worker_envvars()` / `.apply_worker_envvars()` helpers in
  `R/service_monitoring.R` + 3 tests in
  `tests/testthat/test-service_monitoring_wiring.R`.

## [0.26.2] - 2026-05-13

### Fixed

- `fix(deps)`: bump nemeton pin to `>= 0.21.9`. v0.21.9 fixes a
  `terra::writeRaster()` call in the S2 cache write path that
  targeted a `.tif.tmp` path (atomic-write pattern) without an
  explicit `filetype` argument — terra refused with *"cannot guess
  file type from filename"*, so every band was fetched + cropped
  successfully then lost at the write step. UI symptom in v0.26.1:
  ingestion consumed 4-5 min per scene, reached N/N, but
  `<project>/cache/layers/sentinel2/` stayed empty.

## [0.26.1] - 2026-05-13

### Fixed

- `fix(deps)`: bump nemeton pin to `>= 0.21.8`. v0.21.8 fixes a
  per-scene S4→double coercion bug introduced in v0.21.4 (cache_dir
  wiring) that made every Sentinel-2 scene skipped with
  *"cannot coerce type 'S4' to vector of type 'double'"* on
  `skip_cached = FALSE` runs. UI symptom in v0.26.0: ticking the
  "Re-prime COG cache" checkbox triggered the run but neither the
  DB nor the disk cache filled up because every scene errored out.

## [0.26.0] - 2026-05-13

### Added

- `feat(monitoring)`: "Re-prime COG cache" checkbox under the
  ingestion button (mode quick). When ticked, plumbs
  `skip_cached = FALSE` through `run_ingestion_async()` to
  `nemeton::ingest_sentinel2_timeseries()`, which forces re-extraction
  and therefore re-fetches every band, finally populating
  `<project>/cache/layers/sentinel2/`. Default unchecked — preserves
  v0.25.0 behavior. INSERTs are `ON CONFLICT DO NOTHING` core-side,
  the DB is preserved.
- i18n: `monitoring_reprime_cache_label`,
  `monitoring_reprime_cache_help`.

### Changed

- `service_monitoring.R::run_ingestion_async()`: new `skip_cached`
  parameter on the `ExtendedTask` lambda (default `TRUE`),
  forwarded to `nemeton::ingest_sentinel2_timeseries()`.

## [0.25.0] - 2026-05-13

### Added

- `feat(monitoring)`: explicit routing for `progress_callback` events
  emitted by `nemeton@v0.21.4+`:
  - `s2:cache_lookup` → persistent toast "DB cache: N cached, M to
    process"
  - `s2:band_fetch_failed` → 6 s warning toast with `band` +
    `error_message`
  - `s2:pc_token_refreshed` → 3 s info toast
- `feat(monitoring)`: "COG cache active" hint under the ingestion
  button, showing the absolute path of
  `<project>/cache/layers/sentinel2/`.
- `tests/testthat/test-service_monitoring_wiring.R`: smoke test
  asserting `run_ingestion_async()` forwards `cache_dir` and a
  non-NULL `progress_callback` to
  `nemeton::ingest_sentinel2_timeseries()`.
- i18n: `monitoring_ingest_cache_lookup_fmt`,
  `monitoring_ingest_band_failed_fmt`,
  `monitoring_ingest_token_refreshed`,
  `monitoring_cache_active_fmt`.

### Changed

- `chore(deps)`: bump nemeton pin to `>= 0.21.7` to align on the
  versions that expose a stable `cache_dir` + `progress_callback`
  signature.

## [0.24.14] - 2026-05-13

### Changed

- `chore(deps)`: re-sync nemeton pin to `>= 0.21.5` (was `>= 0.21.3`)
  to match the version installed locally. No functional impact —
  removes `pak::pkg_install` resolution warnings on fresh machines.

## [0.24.13] - 2026-05-13

### Fixed

- `fix(monitoring)`: terminal toasts (`ingest_zero`, `ingest_success`,
  `ingest_warns`, `ingest_error`, `fordead_success`, `fordead_error`)
  now use explicit `id = session$ns(...)` so repeated clicks replace
  the previous toast instead of stacking duplicates.
- `fix(monitoring)`: Sentinel-2 band cache moved from
  `<project>/data/s2_cache/` to `<project>/cache/layers/sentinel2/`
  to comply with the NMT cache convention shared with
  `lidar_mnh/`, `lidar_mnt/`, `lidar_nuage/`, `opencanopy/`, etc.

## [0.24.12] - 2026-05-12

### Fixed

- Toast d'erreur **"Échec du téléchargement : argument inutilisé
  (cache_dir = cache_dir)"** au clic FAST après installation de
  v0.24.11. La v0.24.11 a livré le code applicatif qui appelle
  `nemeton::ingest_sentinel2_timeseries(..., cache_dir = ...)`
  mais le pin nemeton dans `DESCRIPTION` était resté à
  `v0.21.2` qui ignore cet argument. Correctif : `Imports:
  nemeton (>= 0.21.3)` + `Remotes: pobsteta/nemeton@v0.21.3`.

## [0.24.11] - 2026-05-12

### Added

- Cache local des bandes Sentinel-2 : branche le `cache_dir`
  introduit par `nemeton@v0.21.3+` sur
  `ingest_sentinel2_timeseries()`. Les bandes téléchargées sont
  posées sous `<project>/data/s2_cache/` et réutilisées au prochain
  run — gain massif sur un re-run après échec STAC ou extension
  de fenêtre. Helper `.resolve_s2_cache_dir(project)` (NULL si pas
  de projet → fallback in-memory legacy de nemeton).
- Abonnement aux events progress `s2:band_cached` /
  `s2:band_fetched` : chaque bande génère une ligne
  `cli_alert_info` dédiée dans la console R
  (`⤷ Bande B04 (cache) — scène S2A_MSIL2A_...`). Pas d'update du
  toast UI (2-4 bandes par scène à sub-second feraient flickerer
  l'UI). Helper `.log_band_event()`.

## [0.24.10] - 2026-05-12

### Added

- Capture des warnings STAC du worker async via
  `withCallingHandlers(warning = ...)`. Quand l'ingestion retourne
  0 scènes, le toast surface maintenant la cause réelle (ex. `STAC
  backend "pc" failed: HTTP 504 Gateway Timeout`) au lieu d'un
  `Téléchargement terminé : 0 scène(s)` muet.
- Phase "Recherche STAC" distincte de la phase "Téléchargement
  tuile" : avant la première tuile reçue, le toast affiche
  "Recherche des scènes Sentinel-2 disponibles…" (ou "Préparation
  du téléchargement : N scène(s) trouvée(s)…" si nemeton a déjà
  pré-rempli le `total`).
- 5 nouvelles clés i18n FR/EN : `monitoring_stac_search`,
  `monitoring_stac_search_with_count_fmt`,
  `monitoring_ingest_zero_fmt`, `monitoring_ingest_zero_default`,
  `monitoring_ingest_warns_fmt`.

### Fixed

- Console R inondée de `Database schema up to date (N migrations
  applied).` à chaque interaction (30-50 lignes par clic).
  Cause : `nemeton::db_migrate()` émet ce message à chaque
  connexion ré-ouverte (validity, zones, alerts, probe...).
  Correctif : `withCallingHandlers(message = ...)` autour de
  `db_migrate()` qui muffle uniquement les messages contenant
  "up to date" / "already migrated". Les "Applied migration X"
  du premier run et les warnings/erreurs restent visibles.
- Toast et console affichaient `(scene_id missing) (0/159)`
  entre la recherche STAC et la première tuile reçue.

### Changed

- Le terme "ingestion" est remplacé par "téléchargement" (FR) /
  "download" (EN) sur tous les textes utilisateur du contexte
  Sentinel-2 (`monitoring_*`). `field_ingest_*` et
  `health_validation_*` sont volontairement préservés (uploads
  de données utilisateur, pas des downloads distants).

## [0.24.9] - 2026-05-12

### Added

- Mirroring console des events de progression : chaque tuile
  Sentinel-2 et chaque phase FORDEAD produit une ligne
  `cli::cli_alert_info` (ou `cli_alert_warning` sur erreur) côté
  console R, exactement une fois par event grâce au `reactivePoll`.
  Format console plus riche que le toast (exploite `obs_date`,
  `cloud_pct`, `source` du payload nemeton).
- Roue dentée animée (`bsicons::bs_icon("gear-fill")` + classe
  `.nmt-spin`) devant chaque message du toast persistant —
  l'utilisateur voit que la tâche tourne toujours.

### Fixed

- Toast d'ingestion affichait `Tuile Sentinel-2 0/0` pendant tout
  le run alors que le `scene_id` arrivait. Cause : nemeton@v0.21.2
  émet `{current, completed, total, scene_id, obs_date, cloud_pct,
  source}` et non `{i, n}`. Lecture défensive des champs avec
  fallback `i` / `n` au cas où le schéma évoluerait.
- Reformatage i18n : compteur **entre parenthèses** en fin de
  message — `Tuile Sentinel-2 <scene_id> (X/N)` et
  `FORDEAD — phase <nom> (X/N)`.

## [0.24.8] - 2026-05-12

### Added

- Progression "X/N tuiles Sentinel-2 téléchargées" pendant
  l'ingestion FAST, et "FORDEAD — phase : %s (X/N)" pendant le
  diagnostic santé. Couplé à `nemeton@v0.21.2` qui introduit
  `progress_callback` sur `ingest_sentinel2_timeseries()` et
  `run_fordead_dieback()`. Le worker async écrit un
  `<project>/data/{ingest,fordead}_progress.json` atomique, le
  main process polle via `reactivePoll(500ms)` et rend un toast
  persistant remplacé à chaque tick.
- 4 nouvelles clés i18n FR/EN :
  `monitoring_ingest_progress_fmt`,
  `monitoring_ingest_progress_named_fmt`,
  `monitoring_health_phase_fmt`,
  `monitoring_health_phase_simple_fmt`.

### Fixed

- Boutons **"Lancer le diagnostic FAST"** et **"Lancer le
  diagnostic FORDEAD"** muets au clic. Cause :
  `tagAppendAttributes(disabled = NA)` HTML-disablait les boutons
  au premier rendu, et la classe `btn-primary` masque visuellement
  l'état `disabled` Bootstrap — le navigateur refuse le clic alors
  que l'utilisateur croit le bouton actif. Correctif : suppression
  du wrapper `disabled = NA`, simplification du
  `updateActionButton(disabled = is_running)` (greying uniquement
  pendant la tâche async), garde `is_running` en tête des
  `observeEvent` pour avaler les double-clics.

### Changed

- `DESCRIPTION`: `Imports: nemeton (>= 0.21.2)`,
  `Remotes: pobsteta/nemeton@v0.21.2`.

## [0.24.7] - 2026-05-12

### Fixed

- Migration de la base DuckDB du Suivi sanitaire :
  bump de `nemeton` vers `v0.21.1` (DDL portable
  Postgres/DuckDB via `CREATE SEQUENCE` + `DEFAULT nextval(...)`,
  remplace `INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY` qui
  cassait DuckDB avec *"syntax error at or near GENERATED"*).
- `DESCRIPTION`: `Imports: nemeton (>= 0.21.1)`,
  `Remotes: pobsteta/nemeton@v0.21.1`.

## [0.23.5] - 2026-05-09

### Added

- Plan d'actions chat: two new controls just below the
  history — **scope radio** (all UGFs / current selection)
  and **overwrite checkbox** (replace existing actions).
  Same semantics as the "Generate actions (AI)" modal.
  When overwrite is on, the apply modal surfaces a
  warning banner listing the number of targeted UGFs.
- New i18n keys `action_plan_chat_scope_sel` and
  `action_plan_chat_apply_overwrite_warn_fmt`.

### Fixed

- Language toggle FR ↔ EN in the navbar selector now
  actually applies. Two combined bugs:
  - The handler wrote to `nemeton.app_language` but
    `app_ui` reads `getOption("nemeton.app_options")$language`,
    so the choice did not survive a page reload. Now
    persists into the right option key.
  - The handler showed a toast saying "Reload the page"
    without actually reloading. Replaced by
    `session$reload()` so the UI rebuilds automatically.
    Anti-init guard: if the new value equals
    `app_state$language`, the observer returns early to
    avoid an unwanted reload at session start.

### Removed

- Orphaned i18n key `language_changed` (was only used by
  the dropped manual-reload toast).

## [0.23.4] - 2026-05-09

### Changed

- Plan d'actions chat: role labels in the conversation
  history now go through i18n. The raw LLM keys ("user" /
  "assistant") no longer surface in the UI; they render as
  **"Vous"** / **"Assistant"** (FR) or **"You"** /
  **"Assistant"** (EN), switching live with the language
  toggle. The underlying data model still uses the English
  keys so the prompt builder is unchanged.

### Added

- New i18n keys `action_plan_chat_role_user` and
  `action_plan_chat_role_assistant`.

## [0.23.3] - 2026-05-09

### Added

- Plan d'actions chat: clicking **Send** now displays a
  persistent **bottom-right toast** with a **spinning gear
  icon** and the label *"L'IA réfléchit…"* / *"AI is
  thinking…"*. The toast stays visible until the LLM
  response arrives or the call fails. Implemented via
  `shiny::showNotification(duration = NULL, closeButton =
  FALSE)` paired with an `on.exit(removeNotification(...))`
  hook so every return path (success, LLM error, parse
  error) clears the toast.
- New i18n key `action_plan_chat_thinking`.

## [0.23.2] - 2026-05-09

### Added

- Plan d'actions: chat history **auto-scrolls to the bottom**
  on every update so the latest message is always visible.
  Implemented via an inline `setTimeout(0)` script appended
  to each `chat_history_ui` render that sets
  `el.scrollTop = el.scrollHeight` on the `.chat-history`
  div (now carrying a stable id).

### Changed

- Plan d'actions: chat panel **moves from a left sidebar to
  the right sidebar**, sitting below the "Tableau des
  actions" panel. The nested `bslib::layout_sidebar`
  introduced in 0.23.1 is replaced by a single right
  sidebar containing both cards stacked top-to-bottom.
- Plan d'actions: button label "Générer (IA)" renamed to
  "Générer les actions (IA)" (FR) / "Generate actions
  (AI)" (EN) for clarity.

## [0.23.1] - 2026-05-09

### Added

- Plan d'actions: **AI chat now lives in a persistent left
  sidebar** (350 px, collapsible) instead of a modal. The
  conversation stays visible while the user navigates map /
  table / Kanban. Layout switches to a nested `layout_sidebar`
  (left chat / right action panel / main content).

### Changed

- Plan d'actions: "Ouvrir le chat" button removed from the
  right action panel (made redundant by the persistent
  sidebar). `input$open_chat` observer (~30 LOC of `showModal`)
  dropped.

### Fixed

- Plan d'actions map ↔ table sync: clicking a parcelle on the
  **map** now selects every corresponding row in the table.
  The `input$map_shape_click` handler now also calls
  `DT::selectRows()`. The reverse direction (table → map) was
  already working. No reactive loop: `reactiveVal` dedupes by
  `identical()` so the round-trip stops after one pass.

### Removed

- i18n keys `action_plan_open_chat` and
  `action_plan_chat_input_label` (orphaned by the chat
  refactor).

## [0.23.0] - 2026-05-09

### Added

- Kanban: double-click on a card opens an **edit modal**
  pre-filled with statut / priorité / année / commentaire.
  Primary use-case is editing long commentaires (DT inline
  cell-edit is single-line). Delegated dblclick listener at
  the board level with cleanup between renders.
- Kanban cards: each card now displays the **commentaire**
  under the type/year/UGF block when non-empty.
- Kanban columns: cards are **sorted by `annee_realisation`**
  ascending (NAs last) so each column reads chronologically.

### Changed

- Kanban: **free movement between any columns**. The
  proposée → validée → planifiée → réalisée → abandonnée DAG
  no longer gates drag-drop. `update_action_in_plan()` accepts
  any known status, rejects only unknown strings.
  `is_valid_status_transition()` and `ACTION_PLAN_TRANSITIONS`
  stay as informational documentation of the natural workflow.
- Kanban: per-card **"Déplacer"** dropdown removed (made
  redundant by free drag-drop). The `kanban_move_*` dispatcher
  observer (~50 LOC) and the unused `KANBAN_STATUSES`
  constant are gone too.
- Action plan table: **action count** moved from bottom-left to
  bottom-right. DT `dom` switched to a custom flex layout
  (`<"top"f>rt<"… dt-bottom-row"<"… "lp>i>`) with scoped CSS
  rules to override the default DT floats.
- Add action modal: the **UGF dropdown** now shows
  `ug_label` (sorted) instead of the raw `ug_id`; **Année
  cible** is now a real calendar year (default `base_year +
  1`, range `base_year + 1` … `base_year + horizon`),
  converted to the internal offset on save.

### Fixed

- Add action modal: previously surfaced the internal offset
  (1, 2, …) for "Année cible" and the raw `ug_id` for the
  UGF dropdown, both confusing for end-users.

### Removed

- i18n keys `action_plan_kanban_move` and
  `action_plan_kanban_drop_invalid_fmt` (orphaned by the
  Kanban refactor).

## [0.22.4] - 2026-05-09

### Changed

- Action plan table: the page-size selector ("Afficher
  5/10/25/50/All") moved **below the table**, next to the info
  count and pagination. Top of the table now only carries the
  global search box. DT `dom` switched from `"lfrtip"` to
  `"frtilp"`.
- Action plan table: only **UGF + Année** are frozen during
  horizontal scroll. `DISPLAY_COLS` reordered so hidden
  columns (`id`, `ug_id`, `annee_cible`) sit at the tail;
  `fixedColumns.leftColumns` reduced from 5 to 2 to match the
  count of visible frozen columns (DT's FixedColumns counts
  every DOM column, hidden included).
- Action plan map: leaflet legend titles now translated.
  `legend_title` literals (`"annee"`, `"type"`, `"priority"`)
  swapped for `i18n$t("action_plan_col_*")` so the map shows
  "Année / Type / Priorité" in FR and "Year / Type /
  Priority" in EN, switching with the language toggle.

### Fixed

- `mod_auth_server()` no longer crashes on startup in anonymous
  mode when `NEMETON_AUTH_DEV_ROLES` is set. The dev-roles
  branch interpolated `{auth_state$user_roles}` through
  `cli::cli_alert_info()` / glue outside any reactive consumer,
  which `reactiveValues` rejects. The parsed roles are now
  captured in a local `parsed_roles` before being assigned to
  `auth_state$user_roles`; the log message reads the local
  instead of the reactiveValues. Regression introduced by #41
  in v0.22.3.

## [0.20.0] - 2026-04-24

### Added

- LiDAR HD MNH fetched via `happign` as the preferred CHM source
  (Open-Canopy ML retained as fallback). E5.d phase 1.
- LiDAR HD MNT promoted to the canonical `dem` slot (1 m vs 25 m
  BD ALTI) so W3 / R1 / R2 / R3 / erosion use LiDAR resolution.
- `has_lidar_hd` attribute auto-lifts NDP to 1 when any LiDAR HD
  product is cached.
- New "Hauteur LiDAR HD" badge on the Synthesis tab, distinct
  from "Hauteur ML".
- Reactive loaders for cached CHM / MNT in mod_sampling; passed
  to `create_sampling_plan()` so stratified GRTS kicks in.
- `forest_mask` passed to the sampling plan (BD Forêt v2
  filtered) — points stop falling in water / roads.
- Immediate spinning-gear toast when clicking *Générer les
  placettes*.
- Tooltip on *Source du CV* radio clarifying that it only picks
  the CV value, not the draw method.

### Changed

- Sampling-method note rewritten to describe candidates on a
  regular 50 m grid, forest mask filter, then GRTS → LPM2 →
  random selection.
- Map auto-zoom fixed to the UGF extent instead of BD Forêt's
  (which is fetched with a buffer).
- `chm_phase:lidar_hd_download` progress key translated.
- Bumped `nemeton` minimum to `>= 0.19.5`.

### Fixed

- Duplicate PostGIS-sync toast at compute completion (kept the
  `mod_home` one, dropped the `mod_progress` one).
- Retry button now emits an immediate toast on the root session.

## [0.19.0] - 2026-04-24

### Added

- Tooltips on six sidebar inputs of the Export terrain sub-tab
  (target error, alpha risk, over-sample ratio, CV position, seed,
  region).
- Custom TSP legend on the leaflet map (inline-SVG glyphs for the
  route, start and finish).
- Immediate toast notification when clicking *Réessayer* on the
  compute-error card, dispatched on the root session.
- `URL` and `BugReports` fields in `DESCRIPTION` so the RStudio
  Packages pane shows the documentation icon next to the package.

### Changed

- README counters synced to the real state (31 indicators, 13
  expert profiles, 504 i18n keys).
- `sampling_tt_region` tooltip wording says QGIS, not QField.

### Fixed

- Duplicate PostGIS-sync toast at compute completion — removed the
  second occurrence in `mod_progress`; the `mod_home` one remains.

## [0.18.0] - 2026-04-24

### Added

- **Terrain top-level tab** with two sub-tabs via
  `bslib::navset_card_underline()`:
  - *Export terrain* — design a sampling plan, render a leaflet map
    with the BD Forêt v2 overlay (coloured by sylvicultural
    context) + the UGF polygons + the placettes, export a QField
    `.qgz` project.
  - *Import terrain* — ingest a GeoPackage returned by QField,
    validate it, attach aggregates to the project and bump the
    NDP.
- **Sampling sizing modes** in the Export terrain sidebar:
  fixed-size (legacy path) or *target error + CV* (new).
  The CV source can be manual, or derived automatically from the
  project's cached BD Forêt v2 layer via
  `nemeton::cv_from_bdforet()`. The computed sample size, Student
  quantile and ambiguous / unmapped TFV codes are displayed live.
- **TSP route on the map** — dashed magenta polyline connecting
  Base plots in `visit_order`, with inline-SVG orienteering
  symbols (open triangle for Départ, double concentric circle for
  Arrivée).
- **BD Forêt v2 overlay** coloured by resolved forest context
  (futaie régulière résineuse / feuillue, futaie irrégulière, TSF,
  taillis simple) with a toggleable layer control.
- **Field ingest module** (`R/mod_field_ingest.R`, E5.b) — closes
  the QField return loop: validate, aggregate, attach, persist to
  `<project>/data/field_data.gpkg`, update metadata, bump the NDP,
  reload the project.
- **Sampling export module** (`R/mod_sampling.R`, E5.a) — UI +
  `downloadHandler` producing a QField-ready `.qgz`.
- **Package-level help** (`R/nemetonshiny-package.R`) so
  `?nemetonshiny` works and RStudio shows the documentation icon
  in the Packages pane.
- `CITATION.cff` and `CHANGELOG.md` — release-metadata files.

### Changed

- `mod_sampling` now uses `nemeton::create_sampling_plan()`
  (GRTS / LPM2 / random) instead of a plain `sf::st_sample()`.
  The generation notification reports the selected method.
- The Export terrain map now draws per-UGF polygons (matching the
  Import terrain style) instead of a single unioned zone; the
  unioned zone is still used internally by
  `create_sampling_plan()`.
- Sidebar forms in mod_sampling and mod_field_ingest are wrapped
  in Bootstrap collapsible cards (same pattern as the
  "Informations projet" accordion in the Selection tab).
- `default_project_name` reactive — the QField project name input
  pre-fills with the sanitised current-project name, falling back
  to its id or `"echantillon"`.
- Renamed the "Inventaire estimé ML" badge in the Synthesis tab
  to "Inventaire ML"; both augmented-NDP tooltips now prefix
  "ML = Machine Learning" for discoverability.
- Renamed the QField download button from
  "Télécharger le projet QField (.qgz)" to
  "Télécharger le projet QGIS".
- Shortened the CV-compute button label from
  "Calculer le CV depuis BD Forêt v2" to "Calculer le CV".
- Bumped the `nemeton` dependency to `>= 0.19.0`.

### Fixed

- BD Forêt v2 mapping diagnostics: the sizing report now lists the
  actual ambiguous and unmapped TFV codes (with libellé, resolved
  context and alternative) instead of a bare count.
- TFV column auto-detection in `mod_sampling` widened to
  `TFV / tfv / CODE_TFV / code_tfv / essence / ESSENCE / LIB_FV /
  LIBELLE`.

## Prior versions

See [NEWS.md](NEWS.md) for the complete narrative history
(0.1.0 onwards).

[Unreleased]: https://github.com/pobsteta/nemetonshiny/compare/v0.20.0...HEAD
[0.20.0]: https://github.com/pobsteta/nemetonshiny/compare/v0.19.0...v0.20.0
[0.19.0]: https://github.com/pobsteta/nemetonshiny/compare/v0.18.0...v0.19.0
[0.18.0]: https://github.com/pobsteta/nemetonshiny/compare/v0.16.0...v0.18.0

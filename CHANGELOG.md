# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

For a narrative, per-feature description of each release, see
[NEWS.md](NEWS.md). This file is the concise, categorised trail.

## [Unreleased]

## [0.102.1] - 2026-07-11

### Fixed

- Le verrou d'édition v0.102.0 forçait la lecture seule dès que l'email OAuth
  était absent, bloquant à tort le mode admin/dev local (sans fournisseur
  d'identité, `user_email` est NULL alors que les rôles viennent de
  `NEMETON_AUTH_DEV_ROLES`). La lecture seule dépend désormais du **rôle**
  (`can_edit_action_plan()`), jamais de l'email ; le verrou multi-utilisateurs
  ne s'engage qu'avec une identité stable (email OAuth). Bandeau dédié pour le
  rôle lecteur, distinct du message « Connectez-vous » réservé au non
  authentifié.

### Changed

- reGénération : le choix « Essence cible » met à jour la choroplèthe **en
  direct** (`regen_reprioritize()` → `nemeton::indice_priorite_regen()`), sans
  relancer l'analyse complète. Le sélecteur « Essence cible » est déplacé dans
  la carte (sous « Couche affichée ») et le « Buffer contexte régional » dans
  l'onglet carte « Contexte régional (E-OBS) ».

## [0.102.0] - 2026-07-11

### Added

- Verrou d'édition de projet pour le déploiement serveur multi-utilisateurs :
  le premier utilisateur qui ouvre un projet l'édite, les suivants l'ouvrent en
  lecture seule. Enveloppe app `R/service_lock.R` sur l'API cœur
  `nemeton (>= 0.148.0)` (`project_lock_acquire`/`heartbeat`/`release`/`status`,
  table `nemeton.project_lock`, TTL 120 s). Cycle de vie piloté par `app_server`
  sur `app_state$project_id` (acquisition, heartbeat 45 s, relâche à la fermeture
  et en fin de session). Identité = email OAuth ; anonyme = lecture seule.
- Bandeau lecture seule global et garde partagé `deny_if_readonly(app_state)` sur
  toutes les actions mutantes : reGénération, projet (SUFOSAT/LST, suppression),
  calcul des indicateurs, ingestion terrain + validation sanitaire,
  échantillonnage, édition des UGF (16 actions), plan d'action (fusionné avec la
  permission de rôle existante). 6 clés i18n FR/EN `lock_*`.

### Changed

- Plancher `Imports: nemeton (>= 0.148.0)`.
- `nemeton::db_migrate()` (idempotent) appelé à l'initialisation du schéma pour
  créer la table de verrou sur la base plateforme.

## [0.101.4] - 2026-07-10

### Fixed

- reGénération : la carte « Contexte régional (E-OBS) » était toujours vide.
  `load_regeneration_precomputed()` lisait `eobs_{tx,rr}.tif` (jamais écrits) au
  lieu du NetCDF caché sous `cache/regeneration/eobs/`, et la carte bivariée exige
  `tx` **et** `rr` alors que seule `tx` est rapatriée. Lecture des `.nc` cachés via
  `load_eobs_source(nc = )` (aucun CDS au rendu), bandeau explicatif et bouton
  opt-in pour télécharger la série de précipitations (~800 Mo) en tâche de fond.

## [0.101.3] - 2026-07-10

### Fixed

- reGénération : l'ouverture d'un projet gelait toute l'application (~190 s sur un
  projet de 30 UGF). Régression de v0.101.0 (spec 035 B2) : l'observateur de
  restauration appelait `run_regeneration()` de façon synchrone, dont l'étape
  `indicateur_r3_secheresse(dem = )` re-dérive la topographie depuis la mosaïque
  MNT LiDAR (132 s). Nouvelle fonction `restore_regeneration()` qui rattache les
  colonnes déjà cachées et recalcule l'indice de priorité : 190 s → 0,41 s.

## [0.101.2] - 2026-07-10

### Added

- reGénération (spec 035 B4) : le moteur remonte le `lai_max` et la réserve utile
  qu'il a réellement utilisés ; la sidebar affiche médiane et étendue par UGF.
- Le repli silencieux de SoilGrids vers un sol uniforme devient un avertissement,
  une entrée du journal du moteur et une mention explicite dans l'UI.
- Section « Paramètres experts » repliée (`lai_max`, `ewm`, `rooting_depth_cm`),
  avec badge « forcé » à la place de la statistique dérivée.

### Fixed

- `run_regeneration_engine()` : `<<-` dans le bloc d'un `tryCatch()` (évalué dans
  le frame de l'appelant) aurait écrit dans l'environnement du package.
- `.regen_soil_ewm()` : la lecture de la provenance du sol ne peut plus faire
  échouer le bilan hydrique sur une forme inattendue.

### Removed

- Clés i18n orphelines `regen_soil_section`, `regen_lai_auto`.

## [0.101.1] - 2026-07-10

### Added

- reGénération (spec 035 B3) : observabilité du moteur. Journal disque en ajout
  seul (`cache/regeneration/engine.log`, JSONL) qui traverse la frontière du
  worker `future` et survit à sa mort (OOM) ; relais des entrées `error` /
  `warning` vers la console du processus principal ; cumul des avertissements du
  moteur au lieu de leur écrasement par le re-run fast-path ; relecture du
  journal dans la branche `error`, seul moyen quand `engine_task$result()` est
  inaccessible ; échec de `.ntfy_send()` journalisé au lieu d'être avalé.
- UI : bloc repliable « Journal du moteur » ; le panneau d'avertissements
  s'affiche désormais même sans résultat.

### Fixed

- `.regen_log()` : `cat(file =)` sur un répertoire absent émet un warning avant
  de lever, non capturé par `tryCatch(error =)`.

## [0.101.0] - 2026-07-10

### Added

- reGénération (spec 035 B1) : bilan hydrique spatialisé — le PAI LiDAR caché
  alimente le `lai_max` par UGF de BILJOU, agrégé en plateau (P90) et non en
  moyenne ; réserve utile dérivée par UGF depuis SoilGrids 250 m (Saxton &
  Rawls). UI : « Eau extractible » devient un override optionnel, nouveau champ
  « Profondeur d'enracinement », phase moteur « Réserve utile (SoilGrids) ».
- reGénération (spec 035 B2) : restauration du choroplèthe, de l'indice de
  priorité et de la table à l'ouverture d'un projet déjà analysé, par relecture
  du cache disque en fast-path (aucun moteur ne démarre). Alimente
  `app_state$regeneration_result` (perspective IA).

### Fixed

- reGénération : `run_regeneration()` passait `precomputed$eobs` (toujours `NULL`)
  à `microclimate_detect_years()` au lieu de la série cachée `eobs_tx` — la
  détection automatique des années échouait systématiquement.

### Changed

- Plancher cœur relevé à `nemeton (>= 0.147.0)`.

## [0.100.19] - 2026-07-09

### Added

- reGénération : infobulle « i » sur chacune des 4 couches du radio « Couche
  affichée » (définition, unité, sens de lecture, moteur d'origine) + rappel de
  lecture de la légende. 5 clés i18n FR/EN.

### Fixed

- reGénération : palette de la carte inversée (`reverse = TRUE`) — rouge =
  valeur élevée / situation critique, vert = situation favorable. Auparavant les
  UG les plus vulnérables ressortaient en vert.

## [0.100.18] - 2026-07-09

### Changed

- Sidebar reGénération : bloc « Années de référence » (années +
  bouton Auto E-OBS + indice) déplacé au-dessus de « Moteur microclimf réel ».

## [0.100.17] - 2026-07-09

### Fixed

- Push ntfy monitoring (FAST/FORDEAD/RECONFORT) : l'entête et le corps du
  message utilisent le **vrai nom de projet à jour** (résolu côté session)
  au lieu du nom de zone en base, qui pouvait être périmé (`file4ec…` au lieu
  de « Reconfort » après renommage). Nouveau paramètre worker `project_name`
  (repli DB si NULL).

## [0.100.16] - 2026-07-09

### Changed

- Notif bas-droite « en cours » unifiée entre FAST/FORDEAD/RECONFORT et
  reGénération : cadre, police et picto communs (engrenage animé `nmt-spin`) +
  chronomètre monospace « — MM:SS » qui défile (tick 1 s) désormais sur les
  quatre moteurs. Rendu centralisé dans `R/utils_notif.R`
  (`.running_notif_content` / `.fmt_elapsed`) ; `.monitoring_spinning_msg`
  devient un wrapper. Handlers FORDEAD/RECONFORT dotés de `start` + `on_msg`.
- Messages ntfy : nom du projet ajouté à l'entête (`Title`) — `Nemeton
  <MOTEUR> - <projet>` via `.ntfy_title()` (translittération ASCII stricte).

## [0.100.15] - 2026-07-09

### Added

- Indicateur radar **A5 « Rafraîchissement urbain »**
  (`indicateur_a5_rafraichissement`, spec 032) sur la famille A, alimenté par
  la température de surface (LST) Theia/Thermocity. Opt-in strict via le toggle
  projet (`set_project_lst_urbain()`), gaté sur les identifiants Theia ;
  `build_lst_layer()` fetch + cache `cache/layers/lst/`. Famille A inchangée
  (A1-A4) sans activation ; A5 = NA proprement hors couverture urbaine.

## [0.100.14] - 2026-07-08

### Added

- reGénération : cache disque persistant du PAI LiDAR — `pai_cache =
  cache/regeneration/pai.tif` passé à `nemeton::regen_sensibilite()` (branche
  LiDAR). Relecture si géométrie alignée (phase « PAI (cache) » éclair), sinon
  recalcul + réécriture (auto-invalidation AOI/res). ~38 min économisées/run.
- Lien « Recalculer le PAI » sous le badge canopée LiDAR HD (invalidation manuelle
  du cache, cas nuage remplacé à emprise constante).

### Changed

- reGénération : section « Moteur microclimf réel » remontée en tête du sidebar
  gauche (sous l'intro, avant « Années de référence »).
- Retrait de la mention « (option B) » du libellé moteur (jargon interne
  spec 027, sans sens utilisateur).
- Plancher cœur relevé à `nemeton (>= 0.146.2)` (moteur d'exposition de bout en
  bout sous microclimf 2.0.0 ; `pai_cache` disponible depuis 0.145.0).

## [0.100.13] - 2026-07-08

### Fixed

- Test SUFOSAT (`test-sufosat-t3.R`) rendu hermétique : fournit des clés Theia
  factices (`withr::local_envvar`) pour franchir le garde
  `theia_api_key_configured()` de `build_sufosat_layer()`, qui retournait `NULL`
  en CI (aucune clé `TLD_*`) avant d'atteindre l'I/O mockée. Aucun réseau. Fait
  passer R-CMD-check de FAIL 7 à **FAIL 0** — suite entièrement verte en CI.

## [0.100.12] - 2026-07-08

### Fixed

- Tests moteur reGénération (`test-regeneration_engine.R`) rendus robustes à la
  locale : chaque test force la langue app en FR (`withr::local_options`) pour ne
  plus dépendre de l'ordre d'exécution (une suite antérieure pouvait laisser
  `nemeton.app_options` en `"en"`), supprimant 6 faux négatifs en CI. Aucun
  changement de code produit.

## [0.100.11] - 2026-07-08

### Added

- reGénération — message de **phase en cours** du moteur réel affiché en bas à
  droite (grille → PAI → microclimat étés moyens `year (i/n)` → canicule →
  exposition → BILJOU), avec chrono, au lieu d'une notif indéterminée figée.
- Canal fichier `cache/regeneration/engine_status.json` (écriture atomique
  tmp+rename côté worker `future`) + poll `invalidateLater(1000)` côté module
  rafraîchissant la notif persistante `engine_notif` (spec 027, brief
  engine-phase-status ; lève la réserve §5 du brief engine-feedback).
- Phase sautée en premier plan : `microclimf_skipped` + raison (clé CDS absente
  ou structure de végétation manquante) pour le cas RECONFORT, sans blocage.
- 11 clés i18n FR/EN `regen_phase_*` ; nouveau `test-regeneration_phase_status.R`.

### Changed

- `on_prog` (callback de progression cœur) désormais toujours passé à
  `regen_sensibilite()` / `regen_bilan_hydrique()` — il alimente le canal in-app
  indépendamment de ntfy (le push ntfy reste opt-in strict).
- Plancher cœur relevé à `nemeton (>= 0.144.0)` : active la phase PAI
  (`regen_expo:pai`, `source = lidar|satellite`), 6ᵉ et dernière phase rendue.

## [0.100.10] - 2026-07-07

### Changed

- Plancher cœur relevé à `nemeton (>= 0.143.0)` : l'app bénéficie, sans changement
  de code, des optimisations du moteur d'exposition reGénération — lecture LiDAR
  clippée à l'AOI (`pai_depuis_nuage()` / `lasR -keep_xy`) et forçage ERA5 dégroupé
  (1 requête/an + retry/back-off, moins de throttle CDS).

## [0.100.9] - 2026-07-07

### Added

- reGénération : le moteur « Lancer le moteur réel » pousse des notifications
  ntfy au fur et à mesure (jalons start/microclimf/BILJOU/résumé + granularité
  fine par année ERA5 via `progress_callback`, cœur ≥ 0.142.0). Opt-in strict
  (`NEMETON_NTFY_TOPIC`) : sans configuration, aucun envoi ni surcoût. Plancher
  cœur relevé à `nemeton (>= 0.142.0)`.

## [0.100.8] - 2026-07-07

### Fixed

- reGénération : le moteur microclimf (« Lancer le moteur réel ») produit enfin
  `sensibilite.gpkg`. L'app passe désormais la structure de végétation exigée par
  `nemeton::regen_sensibilite()` : `las` (nuage LiDAR HD `cache/layers/lidar_nuage`,
  provenance « lidar ») en priorité, sinon repli `pai` LAI Sentinel-2/PROSAIL
  (provenance « satellite »), sinon warning dédié. `microclimf/` n'est créé que si
  le moteur tourne (era5_*.nc persistés → reprise) ; throttle CDS ERA5 signalé par
  un warning distinct.

## [0.100.7] - 2026-07-06

### Changed

- reGénération : les boutons « Lancer le moteur réel » et « Auto (E-OBS) »
  deviennent des `bslib::input_task_button` (désactivation + spinner pendant la
  tâche, anti-run-concurrent), avec notification persistante bas-droite et chrono
  MM:SS sous chaque bouton.
- Hygiène CRS à la source : LiDAR HD stampé EPSG:2154 (mosaïque / cache /
  couverture, conditionnel si autorité absente), WMS NDP 0 (DEM BD ALTI + IRC)
  réassigné EPSG:4326 — supprime « No DEM available » et « CRS do not match ».
- Theia : retrait de l'appel déprécié `theia_configure_s3()` (signature interne
  via gateway STAC, R pur) ; commentaires reticulate périmés mis à jour.

## [0.100.6] - 2026-07-06

### Fixed

- reGénération : R3 sécheresse réutilise le DEM terrain déjà acquis par le
  pipeline principal (LiDAR HD MNT 1 m `lidar_mnt_mosaic.tif` prioritaire, BD
  ALTI 25 m en repli, cherché à la racine du projet et sous `cache/layers/`) au
  lieu d'un `cache/regeneration/dem.tif` jamais produit — R3 dégradait toujours
  en « BILJOU seul ». Le MNH (hauteur de canopée) n'est pas confondu avec le MNT.

## [0.100.5] - 2026-07-06

### Fixed

- reGénération : le bouton « Auto (E-OBS) » ne téléchargeait rien et affichait
  « indisponible » — `run_regeneration_detect_years()` appelait
  `nemeton::load_eobs_source()` sans plage d'années, or le cœur sort en `NULL`
  avant toute requête CDS quand `years`/`period` sont absents. Une fenêtre
  d'années explicite est désormais passée (fin = année-2, début borné à 2011) ;
  les échecs d'acquisition sont tracés au lieu d'être avalés.

## [0.100.4] - 2026-07-06

### Added

- Validation : les onglets FORDEAD et RECONFORT du plan de validation offrent le
  tirage pondéré **continu** (parité FAST) — probabilité d'inclusion ∝ sévérité
  continue par pixel (FORDEAD `anomaly_index`, RECONFORT `score`), sélecteur
  « Pondération du tirage » (continu par défaut), colonne `alert_weight` en table
  et infobulle. Repli propre en uniforme si la couche de sévérité est absente.
  Plancher cœur relevé à `nemeton (>= 0.140.0)`.

## [0.100.3] - 2026-07-05

### Added

- reGénération : le sélecteur « Essence cible » est pré-rempli via
  `nemeton::regen_species_choices()` — les essences présentes sur l'AOI
  (mapping TFV BD Forêt v2 → classe d'essence, cœur) sont listées en tête,
  suivies des essences d'adaptation. Réactif à l'AOI ; dégradation propre sur
  la liste FRM complète sans colonne TFV.

## [0.100.2] - 2026-07-05

### Fixed

- reGénération : le bouton « Auto (E-OBS) » est fonctionnel — l'app acquiert la
  série E-OBS via `nemeton::load_eobs_source()` (async, cache disque) avant
  d'appeler `microclimate_detect_years()`, au lieu d'afficher systématiquement
  « Détection E-OBS indisponible ». Plancher cœur relevé à `nemeton (>= 0.134.0)`.

## [0.100.1] - 2026-07-05

### Added

- reGénération : **provenance canopée** (spec 033 D5) — badge « LiDAR HD » /
  « satellite (repli) » lu depuis `nemeton::detect_ndp()$augmented`, et **repli
  NDP 0** LAI Sentinel-2/PROSAIL (`nemeton::lai_sentinel2`) agrégé par UGF pour
  BILJOU en l'absence de LiDAR HD (cache `lai_prosail.tif`).

### Changed

- `regen_cds_credentials_ready()` reconnaît le motif `ecmwfr_<user>` (clé CDS
  posée dans `.Renviron`), en plus de `CDSAPI_KEY`/`ECMWFR_CDS_KEY` et du keyring.

## [0.100.0] - 2026-07-05

### Added

- reGénération : **bilan hydrique BILJOU réel** (option B, spec 027 L2) — le
  moteur réel opt-in lance aussi `nemeton::load_biljou_forcing` +
  `build_biljou_soil` + `regen_bilan_hydrique` (async, même worker que
  microclimf), sortie mise en cache (`biljou.gpkg`) consommée en fast-path.
  Forçage **SAFRAN** (défaut) sans clé Copernicus ; **ERA5** requiert les
  identifiants CDS. Statut par moteur en sidebar, garde `regen_guard_biljou`.
  Plancher cœur relevé à `nemeton (>= 0.132.0)`.

## [0.99.0] - 2026-07-05

### Added

- reGénération : **moteur microclimf réel** (option B, spec 027 L1) — run
  asynchrone du chemin moteur `nemeton::regen_sensibilite` (structure LiDAR HD +
  forçage ERA5-Land), opt-in depuis la sidebar, avec garde-fous de prérequis
  (grille LiDAR HD du projet + identifiants Copernicus CDS) et mise en cache de
  la sortie (`sensibilite.gpkg`) consommée en fast-path par le run normal.

### Changed

- BILJOU (bilan hydrique) reste sur la garde option A tant que l'acquisition
  météo/sol n'est pas exposée par le cœur (`load_biljou_forcing` /
  `build_biljou_soil` — brief cœur fourni).

## [0.98.6] - 2026-07-04

### Fixed

- reGénération, onglet Carte : le contour des UGF est désormais visible dès
  l'ouverture (avant toute analyse), et non plus seulement après le calcul du
  choroplèthe.

## [0.98.5] - 2026-07-04

### Changed

- Plancher cœur `Imports: nemeton (>= 0.129.2)` (microclimf validé sur LiDAR
  réel, séquences d'échappement corrigées côté cœur).
- reGénération : garde des moteurs (option A) — `run_regeneration()` n'appelle
  plus microclimf ni BILJOU sans sortie précalculée, et émet un message i18n
  propre au lieu de l'erreur cœur brute. Run réel des moteurs = opt-in (option B).
- reGénération : sidebar « Couche affichée » de l'onglet Carte non rétractable.

## [0.98.4] - 2026-07-03

### Changed

- Forêt ancienne (N2) : suppression du bloc d'upload manuel ; récupération
  automatique de la couche IGN « BD Forêts anciennes » (Etalab 2.0, nationale)
  au calcul, via `nemeton::load_foret_ancienne_source()` (résolu dynamiquement,
  dégradation propre → N2 sur couverture actuelle tant que le cœur ne l'exporte
  pas). Spec 031.
- reGénération : radio « Couche affichée » déplacé dans une sidebar droite du
  sous-onglet Carte.

## [0.98.3] - 2026-07-03

### Fixed

- reGénération : `units_sf()` retombe sur la géométrie UGF puis parcelles quand
  `indicators_sf` est absent (projets Reconfort/monitoring sans calcul des 31
  indicateurs), corrigeant le message trompeur « besoin d'un projet » et le
  bouton « Auto (E-OBS) ».

### Changed

- reGénération : carte au style FORDEAD/FAST — contrôle de couches natif Leaflet
  (OSM/Satellite + overlay UGF) dans la carte, radio « Couche affichée » dans la
  sidebar, mise à jour par `leafletProxy` ; suppression de l'overlay `<details>`.

## [0.98.2] - 2026-07-03

### Added

- reGénération : le rapport PDF Quarto inclut une section « reGénération —
  vulnérabilité climatique » (tableau des UG les plus sensibles), rendue quand
  une analyse est disponible. `generate_report_pdf()` / `prepare_report_data()`
  acceptent un `regen_units` optionnel (additif) ; `mod_regeneration` publie son
  résultat sur `app_state$regeneration_result`, transmis par `mod_synthesis`.

## [0.98.1] - 2026-07-03

### Fixed

- reGénération : warnings moteurs nettoyés des codes ANSI `cli` avant rendu HTML.
- reGénération : sélecteur « Essence cible » désormais peuplé (colonnes
  `code`/`label` de la table de tolérances, auparavant lues comme `essence`).

### Changed

- reGénération : retour au lancement (`withProgress` + toast final), feedback
  et tooltip du bouton Auto (E-OBS), années de référence pré-remplies (2018/2022),
  tooltip LAI max, boutons Export/Enregistrer déplacés dans la sidebar, sélecteur
  de couche en menu rétractable superposé dans la carte, retrait de l'onglet
  « Radar des 12 familles » (réservé à Synthèse).

## [0.98.0] - 2026-07-03

### Added

- **reGénération** : nouvel onglet de lecture de vulnérabilité climatique
  (exposition microclimatique × stress hydrique du sol) pour prioriser les
  interventions de régénération (spec 027, lots L4/L5/L6).
  - Radar : sous-indicateurs microclimat A3/A4/W4/R6 câblés dans les familles A/W/R.
  - Module `mod_regeneration` : panneau de configuration, run avec dégradation
    propre, cartes Leaflet commutables, carte de contexte E-OBS, tableau trié,
    fiche parcelle, radar A/W/R.
  - Service `run_regeneration()` (orchestration cœur via chemin `precomputed`)
    et loader `load_regeneration_precomputed()`.
  - Profil LLM « Adaptation climatique » (`inst/experts/adaptation_climat.yml`).
  - Persistance versionnée en base (`db_save_regeneration`,
    `nemeton.regeneration_states`, migration 004), export GeoPackage
    (`export_regeneration_geopackage`) et data-prep de section Quarto
    (`regeneration_report_summary`).
  - ~60 clés i18n FR/EN.

### Changed

- Plancher `Imports: nemeton (>= 0.118.0)`.

## [0.97.0] - 2026-07-01

### Added
- Diversité spectrale **B4** (α / Shannon, famille Biodiversité) et **L3**
  (β / turnover Bray-Curtis, famille Paysage) via biodivMapR sur Sentinel-2
  (spec 028) : affichage (config famille, radar, i18n FR/EN, progression) **et**
  calcul — `build_spectral_diversity()` assemble le cube réflectance S2
  (6 bandes, resample 10 m, masque UGF) et appelle
  `nemeton::compute_spectral_diversity()` **une seule fois**, partagé par B4/L3
  via `layers$spectral`. Dégradation propre en NA sans cache Sentinel-2.
  Plancher `nemeton (>= 0.110.0)`.

### Changed
- **Relicence EUPL v1.2 → GPL-3** : l'app importe désormais du GPL-3
  (nemeton → biodivMapR) ; EUPL Art. 5 autorise cette relicence ; amende
  ADR-006. `LICENSE`, `DESCRIPTION` et `CITATION.cff` alignés.
- CI : `github::cran/dissUtils` + `jbferet/spinR` ajoutés aux `extra-packages`
  (dépendances transitives non-CRAN de biodivMapR).

## [0.94.0] - 2026-06-30

### Added
- R5 dépérissement branché dans le radar de synthèse (`app_config.R` famille R
  → 5 indicateurs ; `R/service_r5.R::add_r5_to_indicators()` injecté dans
  `mod_synthesis` avant `create_family_index`). Alertes chargées via
  `nemeton::list_alerts`, routage par essence via intersection d'alertes
  (RECONFORT→feuillus / FORDEAD→résineux). Sens et score dans le cœur
  (`nemeton ≥ 0.99.1`). Best-effort (sans zone/alerte → famille R = R1-R4).
  8 tests (`test-service_r5.R`).

## [0.91.16] - 2026-06-27

### Changed

- Suivi sanitaire — indicateur unique « Calcul en cours… » (notification
  bas-droite) affiché dès l'arrivée sur l'onglet et maintenu tant qu'un
  calcul lourd tourne (raster d'alerte FAST + `build_index_stack` du pixel
  map). `build_index_stack` rendu différé (`onFlushed`) pour rendre son
  drapeau `loading` observable ; agrégation centralisée dans
  `mod_monitoring`. Nouvelle clé i18n `monitoring_computing`.
- Carte FAST — texte d'aide pixel corrigé en `(NDMI + NDVI + NBR)` et phrase
  « placette » obsolète retirée ; bandeau bleu in-panel « calcul en cours »
  supprimé (doublon avec la notification unique).

### Fixed

- Carte FORDEAD — `Error: impossible de trouver la fonction "ns"` du slider
  temporel (`renderUI` `fordead_date_slider` sans `ns <- session$ns` local).

## [0.89.1] - 2026-06-18

### Added

- Ingestion FAST — **sentinelle de run** disque écrite côté worker
  (`<projet>/data/ingest_run.json`, statut running/done/error/cancelled),
  indépendante de la session. Au relancement, détection
  (`.detect_ingest_state()`) → bandeau « ingestion en cours » (worker
  vivant) ou « ingestion interrompue » + bouton **Reprendre** (worker mort,
  reprise via `skip_cached`). Helpers `.write_ingest_sentinel()` /
  `.read_ingest_sentinel()`, paramètre `sentinel_path` de l'ExtendedTask,
  séquence d'invocation factorisée (`start_fast_ingest()`), 4 clés i18n.

## [0.88.2] - 2026-06-17

### Removed

- Alertes FORDEAD : panneau « Générer placettes QGIS (vérification terrain) »
  (générateur legacy E6.c.5) retiré — doublon du sous-onglet dédié « Plan de
  validation FORDEAD » (spec 014). `output$qgis_panel` + `output$qgis_download`
  + clés i18n `monitoring_qgis_*` orphelines supprimés.

## [0.88.1] - 2026-06-17

### Added

- Alertes FAST : notification « Calcul du raster d'alerte en cours… » (bas-droite)
  au changement d'indice / mode / zone / dates / seuils / params trend. Le calcul
  du mask est déféré (`session$onFlushed`) via un `reactiveVal` pour que la notif
  parte avant le calcul lourd ; consommateurs (bandeau, carte, clic) inchangés.

## [0.88.0] - 2026-06-17

### Added

- Graphe « série pixel » (Carte FAST) lissé (spec 026) : points bruts estompés
  (marqueurs) + courbe lissée par indice via `nemeton::smooth_pixel_series()`
  (≥ 0.90.0), au lieu des segments bruts en dents de scie. Contrôles dans la
  modale : fenêtre de lissage (15–90 j, défaut 45) + méthode (médiane glissante
  / LOESS). Seuils + plein écran conservés. Lissage 100 % cœur.

### Changed

- Plancher `nemeton (>= 0.90.0)`.

## [0.87.3] - 2026-06-17

### Added

- Plan de validation : notification « Génération du plan en cours… » (bas-droite)
  immédiate au clic « Générer », + garde anti-clics pendant le calcul (déclenché
  via `session$onFlushed` pour que la notif parte avant le calcul lourd).

## [0.87.2] - 2026-06-17

### Fixed

- Suivi sanitaire : `metadata$monitoring_zone_id` du projet courant ré-aligné
  automatiquement sur la zone `_tot` (union UGF, spec 020) à l'ouverture de
  l'onglet — mémoire + disque, une seule fois. Corrige les projets pointant sur
  une zone obsolète / d'un autre projet. No-op si pas de zone `_tot`.

## [0.87.1] - 2026-06-17

### Fixed

- Plan de validation : construit sur la zone du sélecteur « Zone de suivi »
  (`zone_id_r`) au lieu de `metadata$monitoring_zone_id` (qui pouvait pointer sur
  une zone obsolète / d'un autre projet → placettes hors union UGF). S'applique
  au trend FAST et au catégoriel FORDEAD/RECONFORT (repli metadata si rien
  sélectionné).

### Changed

- Plan de validation FAST : paramètres de tendance (mois, années min., obs.
  min./an, α) retirés de la sidebar et réutilisés depuis Alertes FAST mode
  Tendance (`trend_params_r`) — une seule source de vérité.

## [0.87.0] - 2026-06-16

### Changed

- Plan de validation FAST branché sur le trend (spec 025) :
  `nemeton::create_trend_sanitary_plan()` au lieu du masque catégoriel
  count/rolling. Placettes sanitaires pondérées par |pente| continue + témoins
  sur zones stables. Sidebar FAST refondue (indice, fenêtre pluriannuelle,
  placettes sanitaires/témoins, graine, avancés) ; retrait des classes d'alerte,
  classes témoins et tampon. Carte colorée par sévérité continue + popups
  alert_value. Message dédié « aucun déclin significatif ». FORDEAD/RECONFORT
  inchangés. Nouveau wrapper service `generate_trend_sanitary_plan()`.
- Plancher `nemeton (>= 0.88.0)`.

## [0.86.2] - 2026-06-16

### Changed

- Plan de validation : cases « Classes d'alerte » affichées en ordre décroissant
  de sévérité (4, 3, 2, 1) au lieu de 3, 4, 1, 2 (UI + refresh serveur). Sélection
  par défaut inchangée (3, 4).

## [0.86.1] - 2026-06-16

### Fixed

- Plein écran des graphes plotly (graphe trend par pixel Alertes FAST, graphe
  Pixel Carte FAST) : le graphe restait à sa taille initiale au lieu de remplir
  l'écran. Le bouton plein écran émet désormais un `window.resize` après le
  toggle `.modal-fullscreen` → plotly (`responsive`) se redimensionne.

## [0.86.0] - 2026-06-16

### Added

- Alertes FAST (mode Tendance) : clic carte → modale « graphe de tendance par
  pixel » : composites saisonniers annuels (NDRE/NDMI) + droite Theil-Sen
  (rouge si déclin significatif) + annotations (pente, p Mann-Kendall, déclin
  significatif, années valides, classe sévérité 0-4 lue au raster) + bouton
  plein écran + notification « calcul en cours » (via `session$onFlushed`).
  Statistique 100 % cœur (`nemeton::extract_pixel_trend`, cohérent avec le
  raster) ; l'app ne recalcule rien.

### Changed

- Plancher `nemeton (>= 0.87.0)` (consommation de `extract_pixel_trend()`).

## [0.85.16] - 2026-06-16

### Added

- Carte FAST : notification « Calcul du graphique pixel en cours… » (bas-droite)
  affichée immédiatement au clic + garde anti-multi-clics pendant le calcul
  (déclenché via `session$onFlushed` pour que la notif parte avant le calcul).

### Changed

- Suivi sanitaire : le mode par défaut est toujours « Diagnostic FAST » (`quick`)
  à l'ouverture ; le `monitoring_mode` persisté n'est plus restauré (les autres
  réglages le restent).
- Carte FAST : bouton plein écran du graphique Pixel restylé en bouton d'en-tête
  haut-droite (toggle `.modal-fullscreen`, pattern modale clés API/RAG) au lieu
  de l'icône `bslib::card` au survol.

## [0.85.15] - 2026-06-16

### Added

- Carte FAST : bouton « plein écran » sur le graphique de série temporelle par
  pixel (plot enrobé dans `bslib::card(full_screen = TRUE)`, plotly `responsive`).

## [0.85.14] - 2026-06-15

### Fixed

- Alertes FAST : le bandeau d'erreur affichait toujours le message générique
  « aucune scène ne porte les bandes » même quand le calcul du raster échouait
  pour une autre raison (le message générique écrasait la vraie erreur). Le vrai
  message cœur (ex. `[mosaic] resolution does not match`) est désormais conservé
  et affiché ; le générique n'apparaît que sur un résultat vide sans erreur.

## [0.85.13] - 2026-06-15

### Changed

- Alertes FAST, mode Tendance : pré-calcul des deux rasters NDMI et NDRE. Le
  cache disque de l'indice non affiché est réchauffé en arrière-plan (`later`,
  trigger débouncé 800 ms) → bascule NDMI ↔ NDRE instantanée. Idempotent ; un
  indice dont les bandes S2 manquent est ignoré. Nouvelle helper interne
  `.compute_fast_mask()` (plomberie partagée raster affiché / pré-calcul).

## [0.85.12] - 2026-06-15

### Removed

- Slider « Seuil minimum NDRE » du panneau de surveillance (orphelin depuis
  0.85.11 : NDRE est trend-only, et le mode Tendance ignore les seuils). Clé
  `ndre` retirée des trois `thresholds_r` ; clé i18n `monitoring_threshold_ndre`
  supprimée. Aucun impact fonctionnel.

## [0.85.11] - 2026-06-15

### Changed

- Alertes FAST : NDRE réservé au mode Tendance. Les modes Fréquence et Intensité
  n'exposent plus que NDMI / NDVI / NBR (annule l'ajout de NDRE à ces modes en
  v0.85.2 ; NDRE reste en Tendance pour le déclin chronique).

## [0.85.10] - 2026-06-15

### Changed

- Onglet Synthèse : barre de progression de confiance descendue (`mt-2`) pour
  s'aligner avec « Taille image Max 5 Mo, PNG/JPG ». `ndp_progress_bar()` gagne
  un paramètre `bar_class` (défaut inchangé, aucun impact sur mod_field_ingest).

## [0.85.9] - 2026-06-15

### Changed

- Onglet Synthèse : « Confiance φ » rendu à la même taille que « Score global »
  (`text-muted` sans `small`), abaissant la barre de confiance pour l'aligner
  avec « Taille image Max 5 Mo, PNG/JPG ». `ndp_progress_bar()` gagne un
  paramètre `label_class` (défaut inchangé, aucun impact sur mod_field_ingest).

## [0.85.8] - 2026-06-15

### Changed

- Onglet Synthèse : « / 100 (12 familles) » accolé à la valeur du score sur la
  même ligne que « Score global » + icône d'info (suite de 0.85.7) ; gagne une
  ligne, remontant d'autant la barre de confiance φ.

## [0.85.7] - 2026-06-15

### Changed

- Onglet Synthèse, bandeau supérieur : « Score global » + icône d'info + valeur
  sur une seule ligne ; le bloc remonte d'une ligne et aligne la barre de
  confiance φ avec le texte « Taille image Max 5 Mo, PNG/JPG ».

## [0.85.6] - 2026-06-15

### Added

- Bloc « Sources documentaires » sous le commentaire de chaque page Famille
  d'indicateurs (UI), comme la page Synthèse : sources citées par le commentaire,
  extraites de ses marqueurs `[^n]` et résolues via le contexte RAG partagé,
  dédupliquées par contenu. Nouvelle helper `.family_sources_md()`.

### Changed

- Rapport PDF, pages familles : suppression du bloc « Sources documentaires » du
  corps (ajouté en 0.85.5) ; seules les notes de bas de page dédupliquées et
  namespacées (`[^C-1]`…) sont conservées. `.prepare_family_footnotes()` perd son
  paramètre `language`.

## [0.85.5] - 2026-06-15

### Fixed

- Pages familles du rapport : une même source documentaire se répétait sous
  plusieurs numéros de notes (la dédup « une note par source » n'était appliquée
  qu'à la synthèse, pas aux commentaires de famille passés bruts au template).

### Added

- `.prepare_family_footnotes()` : dédup par contenu des notes de chaque famille
  (une note par source unique), labels namespacés par famille (`[^C-1]`…) pour
  éviter les collisions avec la synthèse dans le même PDF, et bloc visible
  « Sources documentaires » sous chaque commentaire listant les sources
  distinctes citées (bilingue FR/EN).

## [0.85.4] - 2026-06-15

### Fixed

- Overlay « Affichage des parcelles… » lent (page blanche) après la synchro
  PostGIS sur les communes à nombreuses parcelles. Labels de survol construits
  par sous-ensemble sf ligne par ligne (`sapply` + `parcel_data[i, ]`) et
  géométries envoyées non simplifiées à Leaflet.

### Changed

- Labels de survol des parcelles construits via `create_parcel_labels()`
  (vectorisé, géométrie retirée) au lieu d'un sous-ensemble par ligne ; robuste
  aux `lieu-dit` NA.
- Simplification géométrique pour l'affichage seulement (`sf::st_simplify`,
  ~1 m, `preserveTopology`), géométrie exacte conservée pour sélection / zoom /
  export. Tolérance réglable via l'option `parcel_simplify_tolerance_m`.

## [0.85.3] - 2026-06-15

### Fixed

- Chargement lent de la liste des projets récents à l'accueil. `metadata.json`
  était lu/parsé 3× par projet (deux fois dans `check_project_health()`, dont
  une relecture redondante, + une fois dans `list_recent_projects()`), de façon
  bloquante au rendu de `mod_home` (jusqu'à 50 projets) → jusqu'à 3N lectures
  JSON synchrones.

### Changed

- `check_project_health()` lit `metadata.json` une seule fois et accepte un
  paramètre optionnel `metadata =` (rétro-compatible) ; `list_recent_projects()`
  lit le fichier une seule fois par projet → 3 lectures/parsings ramenés à 1.
- Cache mémoire du listing trié validé par une signature filesystem bon marché
  (`list.dirs()` + `file.info()` vectorisé, stat seul) avec TTL de secours,
  invalidé sur toute création / mise à jour / suppression de projet.

## [0.85.2] - 2026-06-15

### Added

- Suivi sanitaire — « Alertes FAST » : indice **NDRE** (red-edge B05+B8A)
  ajouté aux modes `count` (Fréquence) et `rolling` (Intensité), à côté de
  NDMI/NDVI/NBR. Nouveau slider de seuil `threshold_ndre` (défaut 0.20),
  câblé dans les `thresholds_r` d'Alertes FAST et de la prévisualisation du
  plan de validation. Nouvelle clé i18n `monitoring_threshold_ndre` (FR/EN).
  Aucun changement cœur (bandes red-edge déjà cachées depuis 0.85.0).

## [0.85.1] - 2026-06-15

### Fixed

- Tests : `test-mod_monitoring.R` alignait encore `bands` sur
  `c("NDVI", "NBR", "NDMI")` (échec CI depuis l'ajout de NDRE en 0.85.0).
  Assertions mises à jour vers `c("NDVI", "NBR", "NDMI", "NDRE")`.

## [0.85.0] - 2026-06-15

### Added

- Suivi sanitaire — mode FAST `trend` (Theil-Sen + Mann-Kendall) dans
  « Alertes FAST » : déclin chronique pluriannuel des feuillus. Indices
  mode-dépendants (NDMI/NDRE en trend), paramètres `months`/`min_years`/
  `alpha` en sidebar conditionnelle, ingestion des bandes red-edge
  B05+B8A (NDRE), mapping toast `fast_prewarm:*_trend`.
- Synthèse — note explicite à l'emplacement des sources quand la
  perspective IA a été générée sans corpus documentaire (RAG indisponible).

### Changed

- Suivi sanitaire — libellés des trois modes de diagnostic harmonisés :
  « Diagnostic FAST (spot/trend) », « Diagnostic FORDEAD (résineux) »,
  « Diagnostic RECONFORT (feuillus) ».

## [0.84.10] - 2026-06-14

### Fixed

- Rapport PDF : une même source pouvait apparaître plusieurs fois en note
  de bas de page sous des numéros différents (ex. ONF = notes 10/11/12/14/17).
  `.prepare_footnotes()` déduplique désormais par contenu (id canonique par
  texte de citation) → une seule note par source unique, projets anciens
  inclus.

## [0.84.9] - 2026-06-14

### Fixed

- Rapport PDF : les refs `[^n]` du commentaire de synthèse restaient
  littérales malgré un bloc « Sources documentaires » correct. L'export
  lisait les sources depuis la copie in-memory
  `current_project$comments$synthesis_sources` (non rafraîchie après une
  génération) → `.prepare_footnotes` recevait un `sources_md` périmé/vide.
  L'export utilise désormais `rag_ctx_synthesis()` (même source que le bloc
  affiché), avec repli sur la copie persistée.

## [0.84.8] - 2026-06-14

### Fixed

- RAG : le profil « Propriétaire » (clé app `owner`) ne récupérait aucune
  référence — le corpus tague ses 4 documents sous `proprietaire_prive`
  mais `rag_profile_code("owner")` renvoyait `"owner"` (mismatch, docs
  orphelins). Alias `owner → proprietaire_prive` ajouté ; plus aucun code
  corpus orphelin.

## [0.84.7] - 2026-06-14

### Fixed

- Perspective Synthèse : le profil interne JSON-only « Planificateur
  sylvicole » (`planificateur.yml`, utilisé en dur par le Plan d'action)
  fuitait dans le sélecteur de « Générer par IA » → la génération
  renvoyait du JSON brut au lieu d'une prose. Il est désormais exclu de
  `get_expert_choices()` (sélecteur) tout en restant dans
  `get_expert_profiles()` (le Plan d'action en a besoin).

## [0.84.6] - 2026-06-14

### Fixed

- RAG (`rag_context`) : le bloc « Sources documentaires » ne listait pas
  toutes les sources citées (cause racine des `[^n]` orphelins). Le prompt
  numérotait par chunk (`[^1]..[^K]`, K≤8) tandis que les sources
  dédupliquaient par document (`[^1]..[^N]`, N≤K) → le LLM citait des
  numéros de chunk absents des sources, parfois sur le mauvais document.
  Le prompt est désormais numéroté **par document unique**, à l'identique
  du bloc sources (chunks d'un même doc regroupés sous un `[^d]`). À
  re-générer pour bénéficier de la correction.

## [0.84.5] - 2026-06-14

### Fixed

- Rapport Quarto : des refs `[^n]` restaient imprimées en littéral au lieu
  de devenir des notes de bas de page — (a) le LLM cite parfois un numéro
  au-delà des sources existantes (orphelin, sans définition) ; (b) une même
  note référencée plusieurs fois (Pandoc ne sait pas réutiliser une note).
  `.prepare_footnotes()` garde la 1re occurrence de chaque ref valide comme
  vraie note, retire orphelines + doublons, nettoie les virgules orphelines,
  et appende les définitions `[^n]:` des seuls ids utilisés.

## [0.84.4] - 2026-06-14

### Fixed

- Onglet Synthèse : l'observer de restauration des commentaires (v0.84.3)
  remettait `rag_ctx_synthesis` à NULL à chaque réassignation de
  `current_project` (dont l'attache différée de `indicators_sf`, même id),
  pouvant effacer le bloc « Sources documentaires » d'une perspective
  fraîchement générée. L'observer ne réagit plus qu'au vrai changement
  d'id de projet (garde `last_loaded_pid`). Rappel : la persistance des
  sources est forward-looking — re-générer la perspective sur un projet
  antérieur à v0.84.3 pour obtenir sources + notes Quarto.

## [0.84.3] - 2026-06-14

### Changed

- Onglet Synthèse, bloc « Sources documentaires » : (1) le contexte RAG
  (`sources_md` + `n_sources`) est désormais **persisté** dans
  `comments.json` et restauré au rechargement d'un projet (le bloc sources
  réapparaît, plus seulement le commentaire) ; (2) **réordonné** — titre
  « Sources documentaires » en premier, puis « Perspective appuyée sur N
  source(s) » dans la même police, puis la liste ; (3) export Quarto :
  les **définitions de notes** `[^n]: …` (dérivées des sources) sont
  appendées au commentaire → Pandoc rend de **vraies notes de bas de
  page** au lieu de `[^2]` littéral.

## [0.84.2] - 2026-06-14

### Fixed

- Tour guidé : ne se lançait plus depuis v0.84.1 (ni au démarrage ni via
  « relancer »). Le couple `tab`/`tab_id` natif de cicerone bascule
  l'onglet via le binding `shiny.bootstrapTabInput`, incompatible avec le
  `page_navbar` bslib (Bootstrap 5) — l'exception avortait tout le tour.
  Bascule d'onglet désormais côté client (clic sur
  `#main_nav a[data-value=...]`). Couverture multi-onglets conservée.
- Doc : politique semver de CLAUDE.md inversée — **PATCH par défaut**,
  MINOR réservé aux gros lots / nouveau module-onglet-mode / épaississement.

## [0.84.1] - 2026-06-14

### Fixed

- Tour guidé : couverture étendue à tous les onglets (socle, phase 0+1).
  Le tour ne couvrait que l'onglet Accueil (6 steps) et était devenu
  incohérent avec l'interface fortement modifiée. Nouveau
  `R/service_tour.R` (définition déclarative + builder cicerone) : un seul
  guide traverse les 6 onglets principaux (Accueil détaillé + 1 step clé
  par onglet, dont le Suivi sanitaire FAST/FORDEAD/RECONFORT) via le
  support natif `tab`/`tab_id` de cicerone. Chaque step porte un `tab`
  explicite (cadrage correct quel que soit l'onglet de relance) ; ancre
  conditionnelle fragile `start_compute` retirée. 5 paires de clés i18n
  `tour_*` ajoutées. Tests de cohérence des ancres (garde-fou
  anti-renommage). NB : cadrage visuel cicerone + tab-switching bslib à
  vérifier en navigateur.

## [0.84.0] - 2026-06-14

### Changed

- Chargement d'un projet récent : la synchronisation PostGIS best-effort
  (`db_sync_project`) tourne désormais dans un worker `future`
  (`db_sync_project_async`) au lieu d'un callback `later()`. Le `later()`
  s'exécutait sur le thread principal R et gelait l'event loop Shiny / le
  rendu de la carte pendant l'upload (connexion + `st_write` parcelles +
  `dbWriteTable` indicateurs), d'où le délai ressenti entre « Connected to
  PostgreSQL » et l'affichage des parcelles. Le sync est maintenant
  totalement hors du thread principal (dispatch non bloquant ~0 ms),
  best-effort, avec fallback `later()` si `future`/`promises` absents.
  Capture des `POSTGRESQL_ADDON_*` côté worker (Clever Cloud).

## [0.83.0] - 2026-06-14

### Added

- Mode RECONFORT (spec 021, L6 G4) : sous-onglet **« Plan de validation
  RECONFORT »** (3ᵉ couple à côté de FAST/FORDEAD). Réutilisation 1:1 de
  `mod_validation_sampling` : `generate_validation_plan(source="RECONFORT")`
  lit le masque catégoriel via `nemeton::read_reconfort_alert_mask`
  (cache `layers/reconfort`, pas de compute à la volée → `validation_no_mask`
  si aucun run persistant) ; UI source-aware (classes 2/3, témoin 1, libellés
  feuillus `reconfort_class_label_*`) ; câblage nav_panel + montage
  `source_fixed="RECONFORT"` + observer de visibilité. Persistance terrain
  inchangée (routage par `alert.alert_type="reconfort_dieback"` à l'ingest).
  Plancher `Imports: nemeton (>= 0.83.0)`. Clôt L6 RECONFORT côté app.

## [0.82.0] - 2026-06-13

### Added

- Mode RECONFORT (spec 021, L6) : **lancement d'un run** désormais câblé
  (complète la consultation livrée en 0.81.0). `run_reconfort_async()`
  (ExtendedTask + future_promise autour de
  `nemeton::run_reconfort_dieback`), `.invoke_reconfort()`, reactivePoll
  de progression + dispatcher `.reconfort_handle_progress_event` (events
  `reconfort:start|phase|complete|error`, 10 phases avec libellés i18n +
  fallback Title-Case), observer de résultat (toast + `reconfort_refresh`),
  grisage du bouton (cross-lock FAST/FORDEAD) et force-unlock. Sans conda
  IOTA²/GEODES/OTB le run échoue proprement (toast d'erreur) ; carte +
  diagnostic restent disponibles sur les runs existants. La validation
  QField des stades feuillus DSF reste un ajout cœur à demander.

## [0.81.0] - 2026-06-13

### Added

- Suivi sanitaire : 3ᵉ mode **RECONFORT** (dépérissement feuillus, spec 021
  L6) à côté de FAST/FORDEAD. Nouveau module `mod_monitoring_reconfort_map`
  (carte des alertes via `nemeton::list_alerts(classes =
  RECONFORT_ALERT_CLASSES)`, popup confidence_class + stress_index ;
  bannière de validité G3 advisory non bloquante via
  `check_reconfort_validity` ; clic → diagnostic pixel
  `read_reconfort_pixel_series` en modal plotly 2 traces CRSWIR/CRre).
  Sélecteur de mode à 3 valeurs, sous-onglet « Carte RECONFORT » lazy,
  i18n FR/EN complète. Plancher `Imports: nemeton (>= 0.80.0)`.

### Notes

- Le lancement d'un run RECONFORT (`run_reconfort_dieback`, lourd/opt-in
  conda) n'est pas encore câblé : le bouton signale l'indisponibilité ; la
  carte et le diagnostic restent fonctionnels sur les runs existants. La
  validation QField des stades feuillus DSF requiert une extension cœur de
  `get_health_validation_schema()`.

## [0.80.0] - 2026-06-13

### Added

- Onglet RAG (`mod_rag_admin`) : bouton « Réinitialiser depuis le corpus
  du package » (près de l'import/export manifeste). Resynchronise la copie
  éditable du manifeste — créée une fois puis figée, donc dérivant de la
  seed du package à chaque release cœur — via
  `nemeton::reset_knowledge_manifest(confirm = TRUE)` (cœur ≥ 0.79.0),
  après une modale de confirmation, puis recharge l'éditeur depuis la
  copie rafraîchie. Nouvelles clés i18n FR/EN `rag_reset_corpus{,_title,_warn,_done}`
  + clé générique `confirm`. Plancher `Imports: nemeton (>= 0.79.0)`.

## [0.79.1] - 2026-06-13

### Fixed

- Régression v0.78.0 : le callback `later()` qui ré-attache `indicators_sf`
  (build `ug_build_sf` différé) lisait/écrivait `app_state` hors de tout
  contexte réactif → `Can't access reactive value outside of reactive
  consumer`, plantant avant l'attache et privant Synthèse/Famille/
  Échantillonnage de leur géométrie UGF. Le corps du callback s'exécute
  désormais dans `shiny::withReactiveDomain(session, shiny::isolate(...))`.
- Toast « Aucun pixel sain — témoins tirés en classe N » qui fuyait
  par-dessus la carte de l'Accueil au chargement d'un projet :
  `alert_mask_r` (`mod_validation_sampling`) est désormais gaté sur
  l'onglet Santé actif (`active_main_tab == "monitoring"`), supprimant le
  toast et retirant une connexion DB + une lecture raster du chemin de
  chargement.

## [0.79.0] - 2026-06-13

### Added

- `get_monitoring_db_connection()` gagne un paramètre `connect_timeout`
  (défaut `2L`, secondes) forwardé à `nemeton::db_connect()` via le
  wrapper rétro-compatible `.nemeton_db_connect()` (introspection des
  `formals` : transmis seulement si le cœur l'expose). Borne la phase de
  connexion Postgres pour que le chemin d'hydratation `monitoring_zone_id`
  ne gèle pas l'UI sur un hôte injoignable. S'appuie sur
  `nemeton::db_connect(connect_timeout=)` exposé depuis le cœur v0.76.0 ;
  plancher `Imports` inchangé (consommation opportuniste).

## [0.78.0] - 2026-06-13

### Changed

- Chargement d'un projet récent : `load_project()` gagne un paramètre
  `build_indicators_sf` (défaut `TRUE`, comportement inchangé pour les
  appelants existants). Le build géométrique des UGF (`ug_build_sf()`,
  `st_union()` par UGF) est extrait dans `attach_indicators_sf()` et
  **différé** via `later()` dans le chemin de chargement interactif —
  `indicators_sf` (consommé seulement par Synthèse / Famille /
  Échantillonnage / Suivi) n'est plus construit avant le rendu de la carte.

### Fixed

- Chargement d'un projet récent : la connexion à la base de monitoring
  n'est plus ouverte à chaque chargement. L'hydratation de
  `monitoring_zone_id` (`mod_home`) est désormais gardée par le nouveau
  prédicat `.has_monitoring_zone_id()` — quand l'id est déjà présent dans
  `metadata.json` (cas commun post-spec 011), le round-trip DB synchrone
  (connexion TCP + migration de schéma), qui pouvait geler l'UI plusieurs
  secondes sur un hôte Postgres lent/injoignable, est entièrement évité.

## [0.77.1] - 2026-06-12

### Changed

- Suivi sanitaire (mode FAST) : le bandeau « Surfaces des zones de suivi »
  adopte le style carte (bordure info bleue, icône + titre en gras, corps)
  des bandeaux de validité FORDEAD, au lieu de la barre `alert` compacte.

## [0.77.0] - 2026-06-12

### Added

- Suivi sanitaire (mode FAST) : bandeau « Surfaces des zones de suivi »
  au-dessus des sous-onglets, rappelant la surface (ha) et la part (%)
  des 4 strates projet `_tot` / `_feu` / `_res` / `_mix` (pourcentage
  relatif à `_tot`). Surfaces calculées via `sf::st_area` sur le polygone
  de chaque zone (`get_monitoring_zone_aoi`, EPSG:2154). Helper interne
  `.compute_zone_surfaces()` + tests. Clés i18n
  `monitoring_fast_surfaces_title`, `monitoring_fast_surf_item`,
  `monitoring_fast_surf_item_tot`.

## [0.76.0] - 2026-06-12

### Changed

- Légende de la carte « Alertes FAST » (Suivi sanitaire) : le titre
  rappelle désormais l'indice actif (NDMI / NDVI / NBR), ex. « Sévérité
  de l'alerte (NDVI) ». Mise à jour réactive sur changement du radio
  « Indice FAST ». Clé i18n `fast_alert_legend_title` paramétrée `%s`.

### Added

- `BRIEF-nemeton-zones-fk-sqlite.md` : hand-off cœur documentant l'échec
  `FOREIGN KEY constraint failed` au re-build des zones de suivi sur
  backend SQLite (`build_project_monitoring_zones` upsert, fix attendu
  côté `nemeton`).

## [0.75.2] - 2026-06-11

### Fixed

- Chargement projet lent (~17 s à froid) : `nemeton::build_index_stack`
  (scan de scènes Sentinel-2 de la carte pixel du Suivi) se recalculait à
  chaque ouverture de projet, même depuis l'Accueil, à cause de
  `suspendWhenHidden = FALSE`. La reactive `pixel_stack_r` est désormais
  gatée sur l'onglet Suivi actif (`app_state$active_main_tab`). Chargement
  ramené à ~2-3 s ; le scan ne tourne plus qu'à l'ouverture du Suivi.

## [0.75.1] - 2026-06-11

### Fixed

- Projets *legacy* (sans `data/commune.gpkg`) : le contour communal était
  re-téléchargé à chaque ouverture via le chemin async lent. Backfill
  paresseux dans `mod_search` — le contour récupéré est persisté, le
  prochain chargement est instantané.

### Added

- `backfill_all_commune_geometries()` : migration one-shot qui réchauffe
  le cache de géométrie commune de tous les projets legacy en une passe.

## [0.75.0] - 2026-06-11

### Changed

- Notification de sync PostGIS persistante jusqu'à l'apparition de
  l'overlay carte « Affichage des parcelles… » : passe de `duration = 5`
  à `duration = NULL` (id `db_sync_notif`), retirée par `mod_map` quand
  l'overlay de chargement prend le relais. Filets de sécurité `later()`
  à 12 s et sur le chemin commune invalide. Évite le trou de feedback où
  la notif disparaissait avant le rendu de la carte.

## [0.74.1] - 2026-06-10

### Fixed

- CI rouge depuis v0.73.0 : `lasR` (Suggests, hébergé r-universe) non
  résolu par `pak` faisait échouer tous les jobs à l'install des
  dépendances. Ajout de `r-lidar/lasR` à `Remotes:`.
- 6 tests pré-existants révélés une fois `lasR` résolu (masqués jusque-là
  par l'échec d'install), tous côté test — code applicatif correct :
  `mod_rag_admin` testServer (×3 : `ignoreInit` + promesse non forcée),
  `mod_monitoring`/`mod_monitoring_pixel_map` (×3 : attente NDMI).

### Changed

- Smoke E2E shinytest2 `mod_rag_admin-e2e` quarantiné (`skip()` + FIXME) :
  jamais exécuté en CI auparavant, cassé (modale/tab-lazy sous headless),
  à ré-armer avec un environnement navigateur stable.

## [0.74.0] - 2026-06-10

### Added

- Persistance de la géométrie de la commune (`data/commune.gpkg`) au
  save du projet : `save_commune_geometry()` / `load_commune_geometry()`
  dans `service_project.R`, champ `project$commune_geometry` exposé par
  `load_project()`, paramètre `commune_geometry` sur `create_project()`,
  `update_project()` et `mod_project_server()`.

### Changed

- Restore projet instantané : la géométrie commune en cache est
  réinjectée synchroniquement au chargement, la carte se rend sans
  attendre la `restore_task` asynchrone (worker `future` + reload
  `nemeton` + 2 appels `geo.api.gouv.fr`). La tâche async ne sert plus
  qu'à peupler la liste déroulante des communes. Garde-fou anti
  double-render (flash blanc) dans son result handler. Les projets
  legacy sans cache retombent sur l'ancien chemin async.

## [0.73.1] - 2026-06-09

### Fixed

- **Génération des zones de suivi** : le bouton « Générer les zones
  de suivi » échouait avec `project_name must be a non-empty
  character scalar`. Le handler passait `project$name` (NULL) au lieu
  de `project$metadata$name` à
  `nemeton::build_project_monitoring_zones()`. Fallback `project$id`.
  Régression v0.73.0.

## [0.73.0] - 2026-06-04

### Changed

- **Plancher cœur bumpé** : `Imports: nemeton (>= 0.67.0)`
  (spec 020). Active les 4 nouvelles fonctions
  `build_project_monitoring_zones`, `create_monitoring_zone`,
  `find_zones_by_project`, `prune_orphan_zone_caches`.
- **Bouton « Enregistrer ce projet comme zone de suivi »**
  → **« Générer les zones de suivi »**. Crée jusqu'à 4 strates
  (`_tot/_feu/_res/_mix`) par croisement union UGFs × BD Forêt v2
  (au lieu d'1 zone à partir des placettes).
- **Selecteur « Zone de suivi »** : ne liste plus toutes les zones
  de la DB (`list_monitoring_zones`) mais uniquement celles du
  projet courant (`nemeton::find_zones_by_project`). Corrige le
  bug villards/Mouthe (zone d'un autre projet affichée par
  défaut).
- **Cleanup post-upsert** automatique : appel à
  `nemeton::prune_orphan_zone_caches()` après chaque
  `build_project_monitoring_zones()`.

### Added

- Clés i18n `zones_build_success_fmt`, `zones_bdforet_missing`,
  `zone_tot`, `zone_feu`, `zone_res`, `zone_mix`.

### Pre-conditions

- Bouton « Générer les zones de suivi » requiert :
  - BD Forêt produite (`cache/layers/bdforet.gpkg`) — message
    actionnable si absente.
  - UGFs définies dans le projet — message actionnable si vide.

## [0.72.0] - 2026-06-04

### Added

- **Modal pixel CRSWIR FORDEAD enrichi** : 3 nouvelles traces /
  annotations exploitant les colonnes `seuil_haut`, `anomalie` du
  dataframe + l'attribut `dans_zone_validite` (déjà retournés par
  `nemeton::read_fordead_pixel_series()` mais non affichés
  précédemment). Bande seuil orange pointillée, points anomalie
  rouges taille 8, annotation hors-validité. Axe Y dynamique
  selon `vegetation_index`.
- 3 nouvelles clés i18n
  (`monitoring_fordead_pixel_threshold`,
  `monitoring_fordead_pixel_anomaly`,
  `monitoring_fordead_pixel_outside_validity`).

### Fixed

- **Zone de suivi reste figée au changement de projet récent** :
  `mod_monitoring.R::~l.942` : `selected = ""` au lieu de
  `character(0)` (interprété par `updateSelectInput` comme « ne
  pas changer » dans certaines combos Shiny/navigateur).
- **Toast `no_data` clic FORDEAD hors zone** : `duration = 8` (au
  lieu de 4) + wording explicite avec instruction actionnable
  (« Cliquez DANS la zone d'alerte colorée »).

## [0.71.1] - 2026-06-03

### Fixed

- **Toast `fordead_success` qui clignote + bouton FORDEAD perçu grisé** :
  garde d'idempotence `fordead_result_consumed` reactiveVal
  (symétrique FAST v0.70.4). Reset à `FALSE` dans
  `observeEvent(input$run_health)`.
- **Push ntfy « Ingestion FAST démarrée »** : wording aligné avec
  le toast UI et le push complete → « Diagnostic FAST démarré ».
  Cohérence end-to-end.

### Changed

- **FORDEAD output_dir : fin de la pollution `/tmp/`** : le worker
  passe désormais `output_dir = <projet>/cache/layers/fordead/output_zone_<id>`
  + `keep_output = TRUE` à `nemeton::run_fordead_dieback()`.
  Per-zone, écrasé à chaque relance, taille bornée. Outputs
  préservés (training, masks bruts) → inspection admin possible.
- Nouveau helper `.resolve_fordead_output_dir(project, zone_id)`.
- `run_fordead_async()` : signature ExtendedTask étendue avec
  `output_dir = NULL` + `keep_output = TRUE` (NULL = retombe sur
  le défaut cœur, back-compat).

## [0.71.0] - 2026-06-03

### Added

- **Modal pixel Carte FAST : 3e indice NDMI complet** (couleur
  bleu `#1F77B4` + ligne de seuil horizontale). La courbe NDMI
  était déjà tracée depuis v0.66.0 (extraction cœur `indices =
  c("NDVI", "NBR", "NDMI")`) mais tombait sur le gris fallback
  faute d'entrée dans `.pixel_band_colors`, et son seuil n'avait
  pas de ligne. Désormais palette complète avec NDVI (vert) /
  NBR (rouge) / NDMI (bleu).

## [0.70.5] - 2026-06-03

### Removed

- **Avertissement NDMI / bande B11 dans les sidebars FAST**
  (Alertes FAST + Carte FAST). Obsolète depuis le plancher cœur
  `nemeton (>= 0.65.1)` (v0.69.1 app) qui garantit la mise en
  cache best-effort de B11 (spec 019 D3). Suppression du helper
  `.fast_ndmi_note()`, des 2 `uiOutput` côté UI, des 2 `renderUI`
  côté server, et des 2 clés i18n `monitoring_fast_ndmi_hint` +
  `monitoring_fast_ndmi_b11_note`. Test
  `.fast_ndmi_note renders...` retiré.

## [0.70.4] - 2026-06-03

### Fixed

- **Toast `ingest_success` qui clignote** : nouveau
  `fast_result_consumed` reactiveVal qui garde contre les re-fires
  multiples de `fast_task$result()`. Reset à `FALSE` dans
  `observeEvent(input$run)` pour le prochain Diagnostic FAST.
  S'applique aussi au branchement erreur (toast `ingest_error`).

### Changed

- **Cohérence ntfy ↔ toast UI** : push `monitoring_ntfy_ingest_complete`
  retire le `%d observations` (toujours 0 depuis nemeton@v0.58.0,
  déjà retiré du toast UI en v0.53.1) et aligne le wording
  « Ingestion FAST terminée » → « Diagnostic FAST terminé ».
  Test `sprintf` ajusté pour 2 args (était 3).

## [0.70.3] - 2026-06-03

### Fixed

- **Toast d'ingestion FAST initialisé à `(1/N)`** : ajout d'un
  handler `s2:search_done` qui affiche `Tuile (1/N) — démarrage
  du téléchargement…` avant que le polling 500 ms du JSON
  dernier-event ne capture sa 1ʳᵉ scène (souvent `(2/N)` à cause
  de l'écrasement multi-events). L'utilisateur voit désormais
  `(1/N)` au moins une fois.

### Added

- Clé i18n `monitoring_ingest_search_done_fmt` (FR/EN).

## [0.70.2] - 2026-06-03

### Fixed

- **Compteur de tuile 1-based** dans le mirror console et le toast
  Shiny de l'ingestion S2 (`Tuile (1/120) → (120/120)` au lieu de
  `(0/120) → (119/120)`). Le cœur émet `completed = i - 1`
  (fraction de progression) ; l'app applique désormais `+1`
  uniquement dans le libellé de la tuile en cours. Les gardes STAC
  (`!nzchar(scene) && i_val == 0L`) restent sur la valeur brute.

### Notes

- Partie A du brief `BRIEF-nemetonshiny-console-FAST.md` (drain
  NDJSON) confirmée déjà en place depuis v0.70.0. Aucune action
  additionnelle.

## [0.70.1] - 2026-06-03

### Fixed

- **Toast prewarm FAST persistant** : à `fast_prewarm:complete`,
  l'observer ne retirait pas le toast `fast_prewarm_progress`
  (id stable, `duration = NULL`). Conséquence : « Pré-calcul carte
  NDMI Intensité en cours… » restait collé en bas à droite alors
  que le worker était terminé. Fix : `removeNotification` explicite
  + nouveau toast court « Diagnostic FAST terminé — application
  disponible. » (4 s).
- **Filet de sécurité status()** : nouvel observer qui retire le
  toast running dès que `fast_task$status()` quitte `"running"`,
  couvrant le cas pathologique où le cœur n'émet pas `complete`.

### Added

- Clé i18n `monitoring_fast_diagnostic_complete` (FR/EN).

## [0.70.0] - 2026-06-03

### Fixed

- **Logs FAST propres** : suppression des sauts (`1/120 → 3 → 23 → 51`)
  et de la désynchro Tuile/Bande dans le mirror console pendant un
  Diagnostic FAST. Hand-off du brief
  `BRIEF-nemetonshiny-logs-FAST-propres.md`. Aucune modif cœur.

### Changed

- **Double transport de progression** : le worker écrit désormais
  en parallèle un `.json` (dernier event, atomic rename) pour le
  toast Shiny coalescé ET un `.ndjson` append-only pour le mirror
  console. Le mirror est désormais drainé par offset d'octets
  (pattern identique à `ingest_log_tick`), garantissant complétude
  et ordre.
- `R/service_monitoring.R::.build_progress_writer` : append NDJSON
  ajouté au writer JSON existant.
- `R/mod_monitoring.R` : nouveau drain `ingest_ndjson_lines` +
  observer dédié. `.log_band_event` et `.log_ingest_event`
  déplacés du chemin JSON dernier-event vers le chemin NDJSON
  drain.
- `R/mod_monitoring.R::.cleanup_progress_file` : étend la
  suppression au `.ndjson` au reset de chaque ingest.

### Pas de breaking change

L'API publique est inchangée. Le toast Shiny continue à fonctionner.
Si un worker plus ancien ne livre pas de `.ndjson`, le mirror console
reste silencieux (fallback transparent).

## [0.69.1] - 2026-06-03

### Changed

- **Plancher cœur bumpé** : `Imports: nemeton (>= 0.65.0)` →
  `(>= 0.65.1)`. nemeton v0.65.1 corrige l'oubli NDMI dans
  `.prewarm_fast_alerts()` (combos passent de 4 à 6 = NDVI +
  NBR + NDMI × count + rolling). Effet : 1re sélection NDMI dans
  Alertes/Carte FAST devient instantanée (hit cache D6) au lieu
  d'un calcul à froid.
- Commentaire `mod_monitoring.R:~l.1471` mis à jour : « les 4
  `_done` » → « les 6 `_done` » avec annotation cœur v0.65.0/v0.65.1.

### Audit cache FAST (RAS)

Brief retour cœur confirme la cohérence prewarm ↔ affichage et
auto-cohérence sampling. Aucun correctif côté app requis.

## [0.69.0] - 2026-06-03

### Changed

- **Renommage du cache `cache/layers/fast/` → `cache/layers/fast_sampling/`**
  (validation_sampling). Clarifie le contexte vs `fast_alert/` et
  `fast_alert_mask/` (monitoring). Pas de migration automatique :
  l'ancien `fast/` reste orphelin sur projets existants (suppression
  manuelle recommandée).

### Migration

- Projets existants : `rm -rf <projet>/cache/layers/fast/` pour
  récupérer l'espace disque. Le nouveau cache `fast_sampling/` sera
  créé à la prochaine demande de validation_sampling.

## [0.68.0] - 2026-06-03

### Changed

- **Plancher cœur bumpé** : `Imports: nemeton (>= 0.65.0)` (était
  `>= 0.64.0`). Débloque NDMI côté cœur (fix spec 019 D3 :
  `.enumerate_cache_scenes()` n'avait pas de branche NDMI →
  retour NULL systématique). Active aussi le nouvel orchestrateur
  exporté `read_fast_alert_rasters()` (3 indices × 2 modes = 6
  rasters en un appel — pas encore consommé par l'app, pipeline
  mono-index inchangé).
- Message i18n pour le cas « raster non calculable » : littéral FR
  inline remplacé par `sprintf(i18n_r()$t("monitoring_fast_alerts_no_scene"),
  idx)`. Wording explicite (« aucune scène cachée ne porte les
  bandes de cet indice »). Respecte règle stricte CLAUDE.md §4.

### Added

- Clé i18n `monitoring_fast_alerts_no_scene` (FR/EN).

## [0.67.1] - 2026-06-03

### Fixed

- **Oscillation infinie des radios Alertes FAST** (NDMI/NDVI/NBR et
  Fréquence/Intensité). L'observer i18n lisait `input$index` et
  `input$mode` sans `isolate()`, créant une dépendance réactive
  cyclique au clic. `shiny::isolate()` autour des lectures casse
  la boucle sans perdre la préservation de sélection sur switch
  de langue.

## [0.67.0] - 2026-06-03

### Added

- Slider **« Seuil minimum NDMI »** dans la sidebar Suivi sanitaire
  (range 0.10–0.80, défaut 0.20). Propagé via `thresholds_r$ndmi` aux
  consommateurs FAST ; les onglets Alertes FAST / validation lisent
  `th$ndmi` quand NDMI est l'indice sélectionné (repli NDVI sinon). Clé
  i18n `monitoring_threshold_ndmi`.

## [0.66.0] - 2026-06-03

### Added

- **NDMI dans l'UI FAST** : indice d'humidité sélectionnable dans Carte
  FAST et Alertes FAST (listé en premier, défaut NDVI). Propagation à
  `build_index_stack()` / `compute_fast_alert_mask()` /
  `extract_pixel_timeseries()` ; `bands = c("NDVI","NBR","NDMI")` à
  l'ingestion (cache B11 + prewarm masques NDMI). Note B11 affichée
  quand NDMI est sélectionné. Clés i18n `index_ndmi`,
  `monitoring_fast_ndmi_hint`, `monitoring_fast_ndmi_b11_note`.

### Changed

- `Imports: nemeton (>= 0.64.0)` (API NDMI).

## [0.65.1] - 2026-06-03

### Fixed

- Clé i18n manquante `db_not_configured` (consommée par `app_server.R`
  au démarrage quand aucune base n'est configurée) → warning console et
  affichage de la clé brute. Clé ajoutée FR/EN.

## [0.65.0] - 2026-06-03

### Added

- **Corpus RAG** : import d'un manifeste CSV depuis le disque
  (`fileInput`, parsé par `read_knowledge_manifest()`, chargé dans la
  table éditable sans écraser le CSV tant que non enregistré) et export
  du manifeste courant (`downloadButton`, `write_knowledge_manifest(
  validate = FALSE)` avec repli `utils::write.csv`). Clés i18n
  `rag_btn_import_csv`, `rag_btn_export_csv`, `rag_import_csv_*`.

## [0.64.1] - 2026-06-03

### Fixed

- **Modal Paramètres** : l'onglet « Fournisseur LLM » ne réaffichait
  plus statut + boutons clé après l'ajout de l'onglet RAG (0.63.0).
  Cause : init DataTables dans un onglet caché. Fix : montage à la
  demande de l'UI de l'onglet RAG (`output$rag_tab_content`).

### Changed

- Bouton plein écran déplacé en haut-droite du modal (positionnement
  absolu sur `.modal-content`).
- Titre/intro du modal mis à jour pour refléter clés API + LLM + corpus
  RAG (« Paramètres : clés API & corpus RAG »).

## [0.64.0] - 2026-06-03

### Changed

- **Carte FAST** : le slider de dates avance par pas de **5 jours**
  (`step = 5`, cadence Sentinel-2) au lieu de jour-par-jour ; le
  snapping sur la scène réelle la plus proche reste en place.

### Added

- **Alertes FAST** : bandeau `alert-info` bleu en haut de la carte
  (symétrique de Carte FAST) rappelant la résolution Sentinel-2 (10 m)
  et décrivant le rendu selon le mode (fréquence / intensité) et
  l'indice. Clés i18n `monitoring_fast_alerts_badge_count` /
  `monitoring_fast_alerts_badge_rolling`.

## [0.63.0] - 2026-06-03

### Changed

- **L'admin RAG passe dans le modal Paramètres** (roue dentée,
  `mod_theia_config`) en troisième onglet « Corpus RAG », au lieu d'un
  onglet de premier niveau de la navbar. Namespace imbriqué
  `theia_config-rag_admin-…`. Retrait de l'onglet navbar « Paramètres »
  (`app_ui.R`/`app_server.R`).
- Table manifeste rendue via un déclencheur `redraw` explicite (au lieu
  d'un proxy `DT`) pour rester cohérente à la réouverture du modal.

### Added

- **Modal Paramètres extensible en plein écran** : bouton bascule
  (`arrows-fullscreen`) appliquant `.modal-fullscreen` (Bootstrap 5) ;
  taille par défaut `xl`. Clés i18n `api_keys_tab_rag`,
  `api_keys_fullscreen`.

## [0.62.0] - 2026-06-03

### Added

- **Onglet « RAG / Corpus de connaissances »** (menu Paramètres, spec
  009.2, E7). Module `R/mod_rag_admin.R` : édition du manifeste corpus
  (table `DT` éditable, ajout/suppression de lignes, vocabulaire
  contrôlé), validation en direct (`validate_knowledge_manifest`),
  enregistrement (`write_knowledge_manifest`), prévisualisation
  dry-run, import asynchrone (`ExtendedTask` + `future_promise`,
  connexion ouverte dans le worker, `api_key` explicite, heartbeat de
  progression), inventaire base (`list_knowledge_documents`) et
  suppression (`delete_knowledge_document`). Accès réservé aux
  administrateurs (`can_admin_rag`).
- ~30 clés i18n FR/EN (`rag_*`, `tab_settings`).

### Changed

- `Imports: nemeton (>= 0.63.0)` — le code consomme l'API
  manifeste/corpus publiée par la spec 009.2 du cœur.

## [0.61.2] - 2026-06-02

### Changed

- **Le RAG s'applique désormais aussi aux 12 commentaires famille.**
  La boucle `fill_all_comments` de `mod_synthesis.R` (lignes 616-681)
  ne passait pas le `ctx$prompt_block` aux prompts famille — seule
  la synthèse globale en bénéficiait. Désormais les 13 perspectives
  (1 synthèse + 12 familles) sont enrichies avec le **même contexte**
  (1 seul retrieve total, cohérence des marqueurs `[^n]`). Conséquence
  observable : le `cli_inform("RAG: ...")` reste émis 1 seule fois
  par session, mais TOUS les commentaires (synthèse + familles)
  peuvent désormais citer les documents.
- Si le ctx RAG est vide (corpus muet, opt-out, échec retrieve),
  les prompts famille retombent sur leur comportement v0.61.1 sans
  RAG (`Filter(nzchar)` neutralise proprement le bloc vide).

## [0.61.1] - 2026-06-02

### Added

- **Observabilité RAG** : `cli::cli_inform("RAG: {n} chunk(s)
  récupéré(s) au-dessus de {min_similarity}")` dans
  `R/service_rag.R::rag_context()` juste après le retrieve cœur
  réussi. Item résiduel du brief RAG 2026-06-02 — le reste du
  câblage (`service_rag.R`, `mod_synthesis.R`, i18n, tests) était
  livré en v0.56.0.

## [0.61.0] - 2026-06-02

### Removed

- **3 contrôles UI redondants retirés en bundle** :
  - `checkboxInput("raster_visible")` Alertes FAST (sidebar droit) →
    visibilité pilotée par LayersControl (entrée « Alertes »).
  - `checkboxInput("raster_visible")` Carte FAST (sidebar droit) →
    visibilité pilotée par LayersControl (entrée « NDVI/NBR »).
  - `checkboxGroupInput("bands")` sidebar parent gauche → NDVI + NBR
    systématiquement téléchargés (`bands = c("NDVI", "NBR")` câblé
    en dur dans `fast_task$invoke()`).
- 4 clés i18n retirées (`monitoring_bands`, `monitoring_validate_bands`,
  `monitoring_fast_alerts_raster_visible`,
  `monitoring_pixel_map_raster_visible`).

### Changed

- `addLayersControl` Alertes FAST : `overlayGroups` enrichi de
  `"Alertes"` (= `.alert_raster_group`) pour que Leaflet pilote la
  visibilité du raster d'alerte au même titre que « UGF ».

### Tests

- `test-mod_monitoring.R` : test `"input$run with no band selected"`
  → réécrit en `"input$run invokes the task with NDVI+NBR
  hard-wired"`. Assertion HTML inversée sur la sidebar.

## [0.60.0] - 2026-06-02

### Removed

- **Checkbox « Mode rapide (multi-cœur) » Alertes FAST** (introduit
  en v0.58.0 / TODO #4). Désormais `parallel = TRUE` est passé en
  dur dans `nemeton::compute_fast_alert_mask()`. Le fallback
  séquentiel silencieux du cœur (si `furrr` absent) reste actif :
  aucun risque de cassure. L'opt-in faisait peser un choix
  technique sans bénéfice opérationnel sur l'utilisateur.
- Clé i18n `fast_alerts_parallel_label` (FR + EN) supprimée.

### Tests

- Retrait des 2 tests v0.58.0 devenus obsolètes (i18n du label +
  propagation `input$fast_mode → parallel`).
- Ajout d'un test de non-régression sur l'absence de la clé i18n.

## [0.59.1] - 2026-06-02

### Fixed

- **Test `register click` cassé par `bindEvent(ignoreInit = TRUE)`**
  (régression test introduite par commit 3f1059d, bouton inline).
  Matérialiser une transition `0L → 1L` sur `input$register` pour
  émuler un vrai clic d'`actionButton` et déclencher l'observer.
  Aucun changement de code de prod. Résultat : `[ FAIL 0 | PASS 6875 ]`
  sur la suite complète.

## [0.59.0] - 2026-06-02

### Added

- **Modal diagnostic pixel CRSWIR FORDEAD** (TODO #3, `nemeton@v0.43.0+`).
  Clic gauche sur la carte FORDEAD → modal plotly affichant la série
  CRSWIR observée (points bleus) + prédiction harmonique (ligne rouge)
  + marqueur vertical sur la date de 1re anomalie. Parité fonctionnelle
  avec la Carte pixel FAST existante. Wiring via
  `nemeton::read_fordead_pixel_series()`.
- 6 nouvelles clés i18n FR/EN
  (`monitoring_fordead_pixel_modal_title_fmt`,
  `monitoring_fordead_pixel_observed`,
  `monitoring_fordead_pixel_predicted`,
  `monitoring_fordead_pixel_first_anomaly`,
  `monitoring_fordead_pixel_yaxis`,
  `monitoring_fordead_pixel_no_data`).

### Tests

- 2 nouveaux : cohérence i18n des 6 clés (FR + EN) + signature cœur
  `read_fordead_pixel_series` compatible avec l'appel app.

## [0.58.0] - 2026-06-02

### Added

- **Toggle « Mode rapide » multi-cœur Alertes FAST** (TODO #4, spec
  017 D4 `nemeton@v0.57.0+`). Nouvelle case à cocher dans le sidebar
  droit de l'onglet Alertes FAST. Quand activée, propage `parallel =
  TRUE` à `nemeton::compute_fast_alert_mask()` qui distribue le
  calcul par scène sur plusieurs cœurs via `furrr`. **Opt-in**
  (décoché par défaut) ; résultats identiques au mode séquentiel ;
  fallback silencieux si `furrr` absent côté cœur.
- 1 nouvelle clé i18n FR/EN : `fast_alerts_parallel_label`
  (« Mode rapide (multi-cœur) » / « Fast mode (multi-core) »).

### Tests

- 2 nouveaux : cohérence i18n FR/EN + logique de propagation
  `input$fast_mode → parallel`.

## [0.57.0] - 2026-06-02

### Changed

- **Alertes FAST : affichage en quartiles 0-4** (TODO #5, spec 017
  D2 `nemeton@v0.55.0+`). Délégation de la discrétisation au cœur
  via `nemeton::compute_fast_alert_mask()`. Le raster passe de
  continu (gradient) à catégoriel 0-4 (transparent, jaune, orange,
  rouge-orangé, rouge foncé). Unification des modes count/rolling
  sur la même palette. Helper `.fast_alert_mask_cache_dir()` ajouté.
- 5 nouvelles clés i18n FR/EN (`fast_alert_legend_title`,
  `fast_alert_class_1` à `_4`).

### Tests

- 2 nouveaux : helper chemin mask + cohérence i18n classes.

## [0.56.0] - 2026-06-02

### Added

- **Perspectives IA sourcées via RAG (`nemeton@v0.62.0`).** Avant
  chaque appel `chat$chat(prompt)` dans `mod_synthesis`, l'app
  récupère via `nemeton::retrieve_knowledge()` les ~8 passages les
  plus pertinents (cosinus ≥ 0.55 sur embeddings Mistral) dans le
  corpus pgvector co-localisé avec la DB monitoring. Les chunks sont
  injectés en tête du prompt avec une consigne de citation `[^n]`.
  Sous la perspective générée, bloc « Sources documentaires »
  formaté par `nemeton::format_citations()` (titre i18n cœur).
- **Nouveau fichier `R/service_rag.R`** : orchestration mince
  (`rag_knowledge_con`, `rag_profile_code`, `build_situation_summary`,
  `rag_context`). Toute la logique métier reste au cœur (règle
  CLAUDE.md §1, §3).
- **Dégradation gracieuse** (impératif brief §5.7) : 7 chemins
  d'erreur testés renvoient un payload vide → perspective générée
  sans bloc Sources, aucune exception UI. Opt-out manuel possible
  via `options(nemeton.rag_enabled = FALSE)`.
- **2 clés i18n FR/EN** (`rag_sourced_badge`, `rag_toggle_label`).
- **11 nouveaux tests** dans `tests/testthat/test-service_rag.R`
  (mapping profil, situation summary FR/EN, nominal, dédup
  document_id, opt-out, erreur retrieve, 0 ligne, situation vide,
  app_con NULL).

### Changed

- **Plancher `Imports: nemeton (>= 0.62.0)`** (depuis 0.61.0).
  Garantit la présence de `retrieve_knowledge` + `format_citations`.

## [0.55.0] - 2026-06-02

### Changed

- **Pré-calcul FAST déplacé du helper app vers l'API native cœur**
  (`nemeton@v0.61.0`, spec 018). v0.54.0 livrait un helper local
  `.prewarm_fast_alerts()` qui faisait 4 `read_fast_alert_raster()`
  après l'ingest. Le cœur intègre désormais nativement cette logique
  via `prewarm_alerts = TRUE` + `prewarm_mask_cache_dir`. Le helper
  app est SUPPRIMÉ ; les 2 params sont forwardés au cœur depuis le
  worker.
- **Helper unique `.fast_alert_cache_dir()`** dans `mod_monitoring.R`.
  Factorise le chemin canonique `<projet>/cache/layers/fast_alert`
  utilisé par les 3 call sites (invoke worker + lecture Alertes FAST
  + prévisualisation validation_sampling). Cohérence cruciale du
  hash D6.
- **`Imports: nemeton (>= 0.61.0)`** — garantit la présence de
  `prewarm_alerts` + `prewarm_mask_cache_dir`.

### Added

- **Toasts localisés pour les events `fast_prewarm:*` du cœur.**
  L'observer `ingest_progress` reconnaît désormais le préfixe et
  produit des toasts à partir des clés machine du payload
  (`ev$index`, `ev$mode`) — jamais en parsant du FR. Mapping :
  `count` → Fréquence/Frequency, `rolling` → Intensité/Intensity.
  Événements supportés : `fast_prewarm:<idx>_<mode>` (running),
  `_done`, `_failed`, `:complete` (silencieux), `:cancelled`.
- **6 clés i18n FR/EN** : `fast_mode_frequence`, `fast_mode_intensite`,
  `fast_prewarm_running`, `fast_prewarm_done`, `fast_prewarm_failed`,
  `fast_prewarm_cancelled`.

### Removed

- Helper `R/service_monitoring.R::.prewarm_fast_alerts()` (redondant
  avec spec 018 cœur).
- 4 tests qui mockaient ce helper.

### Tests

- 3 nouveaux dans `test-service_monitoring.R` : helper chemin, sprintf
  placeholders, mapping mode → i18n.

## [0.54.0] - 2026-06-02

### Added

- **Pré-calcul inconditionnel des 4 cartes FAST en fin de
  Diagnostic FAST.** Nouveau helper `.prewarm_fast_alerts()` qui
  enchaîne 4 appels `nemeton::read_fast_alert_raster()` (NDVI×count,
  NDVI×rolling, NBR×count, NBR×rolling) après l'ingestion COG. Cache
  D6 content-addressed → revisite UI sub-seconde. Découple calcul ↔
  affichage : les coches/radios Alertes FAST pilotent désormais QUE
  l'affichage Leaflet, jamais le calcul.
- 4 nouveaux tests dans `test-service_monitoring.R` couvrent : les 4
  combos calculées, l'échec partiel toléré, le cancel coopératif,
  le no-op si `result_cache_dir` est NULL/vide.

### Changed

- **Signature `run_ingestion_async()` ExtendedTask** : nouveau
  paramètre `result_cache_dir = NULL`. Forwardé par
  `mod_monitoring.R` à `file.path(project$path, "cache", "layers",
  "fast_alert")` à chaque `fast_task$invoke()`.

## [0.53.1] - 2026-06-02

### Fixed

- **`db_scenes_df_r` introuvable dans `output$date_slider_ui`** (résidu
  refactor v0.52.16). L'exception non gérée fragilisait la session
  Shiny → bouton « Diagnostic FAST » pouvait rester grisé après la
  fin du worker + toast persistant. Case 2 du fallback supprimée
  (dead code depuis le retrait `obs_pixel`).
- **Toast `monitoring_ingest_success` simplifié** : depuis
  `nemeton@v0.58.0`, `n_obs_inserted` est toujours 0. Message
  reformulé en « Diagnostic FAST terminé : N scène(s) en cache. »
  au lieu du trompeur « N scène(s), 0 observation(s) insérée(s). »

## [0.53.0] - 2026-06-02

> Première release sous la convention semver stricte (CLAUDE.md
> §Consignes de release étape 1 révisée 2026-06-02). MINOR bump car
> refactor structurel + nouvelle feature UI.

### Fixed

- **`NEMETON_DB_LOCAL=1` ignoré au chargement projet.** La variable
  était lue uniquement par `service_monitoring_db.R` (monitoring DB),
  pas par `service_db.R` (project DB). `.resolve_db_config()`
  court-circuite désormais en tête si truthy → projects/parcels/
  comments/users restent sur disque (mode single-user local).
- **Carte Alertes FAST : raster invisible avant bump opacité.**
  Refactor structurel : `output$panel` éclaté en `output$banner`
  (uiOutput, re-render selon raster_r) + `leafletOutput("map")`
  direct dans l'UI (rendu UNE FOIS au montage). La map ne se
  recréait plus à chaque changement d'index/seuil, donc l'observer
  `leafletProxy::addRasterImage` peint correctement au premier coup.

### Added

- **Bandeau d'erreur diagnostique distinct de « zone saine ».**
  `output$banner` distingue désormais : VERT « Aucune alerte FAST
  sur la fenêtre » (raster calculé, 0 alerte) vs JAUNE warning
  « Raster d'alerte non calculable » + cause (cache S2 incomplet,
  exception cœur). Cas typique : NBR avec bande B12 partiellement
  absente du cache. Nouveau reactiveVal `last_raster_error`,
  nouvelle clé i18n `monitoring_fast_alerts_error_title`.

### Changed

- **Documentation : table de décision semver stricte** ajoutée à
  `CLAUDE.md` (§Consignes de release étape 1). Toute nouvelle
  feature UI / refactor structurel / retrait UX bumpe désormais
  en MINOR ; PATCH réservé aux fix régression purs + alignement
  plancher cœur + doc.

## [0.52.17] - 2026-06-02

### Changed

- **Plancher `Imports: nemeton (>= 0.60.0)`** — alignement avec la
  finalisation cœur de la spec 017. `nemeton@v0.58.0` (Phase A,
  drop `obs_pixel` insertion) puis `v0.60.0` (Phase B, retrait de
  `read_obs_pixel` + migration `0004_drop_obs_pixel`) publiés. App
  fonctionnellement inchangée — v0.52.16 fonctionne déjà contre
  ce nouveau cœur sans aucun warning.
- `tests/testthat/test-monitoring-smoke-e2e.R` : précondition
  `read_obs_pixel exported` retirée du skip (la fonction n'existe
  plus en `nemeton@v0.60.0`).

## [0.52.16] - 2026-06-02

### Changed

- **FAST 100 % pure raster per-pixel — suppression du couplage `obs_pixel`/placettes.**
  Suite à la spec 017 cœur (`nemeton@v0.55.0+`), le module Suivi
  sanitaire ne lit plus la table `obs_pixel` ni n'affiche les
  placettes de l'onglet Terrain. La modale « clic marqueur placette »
  est supprimée ; seule subsiste la modale « clic pixel pur » qui
  utilise `extract_pixel_timeseries()` (COG cache).

### Removed

- `obs_pixel_data` reactive + `obs_refresh` reactiveVal (mod_monitoring.R)
- `placettes_sf_r` reactive + observer addCircleMarkers placettes
  (mod_monitoring_pixel_map.R)
- `output$placette_ts_plot` + observer `input$map_marker_click`
- Toggle « Placettes » du LayersControl Leaflet
- Clés i18n obsolètes : `monitoring_pixel_map_placette_modal_title_fmt`,
  `monitoring_pixel_map_no_placette_data`
- 4 tests obs_pixel + helper `.skip_if_no_read_obs_pixel`

### Fixed

- `test-service_monitoring_db.R:170` : test obsolète depuis v0.52.1
  (Postgres RO migre aussi de manière idempotente) corrigé.

## [0.52.15] - 2026-06-02

### Fixed

- **Call site oublié `compute_fast_alert_mask()` (régression v0.52.13).**
  v0.52.13 avait migré `read_fast_alert_raster()` vers l'API
  mono-index `nemeton@v0.55.0` mais avait laissé
  `compute_fast_alert_mask()` (dans `service_validation_sampling.R`)
  sur l'ancienne API → crash « arguments inutilisés » sur
  « Générer le plan de validation FAST ». Fix : appel avec
  `index` + `threshold`, et nouveau param `index` propagé dans
  `.resolve_alert_raster()` / `generate_validation_plan()`.

### Added

- **Cache D6 du raster d'alerte (`nemeton@v0.57.0`).**
  Les 2 call sites de `read_fast_alert_raster()` et le call site de
  `compute_fast_alert_mask()` passent désormais
  `cache_result = TRUE` + `result_cache_dir = <project>/cache/layers/fast_alert`.
  Le COG résultat est persisté avec un hash content-addressed
  (zone × index × threshold × dates × mode × window_days). Revisite
  à paramètres identiques → sub-seconde.

### Changed

- **Plancher `Imports: nemeton (>= 0.57.0)`** — pour garantir la
  présence des params `cache_result` / `result_cache_dir`.

## [0.52.14] - 2026-06-01

### Changed

- **Radio « Indice FAST » déplacé du sidebar parent vers le sidebar droit d'Alertes FAST (symétrie avec Carte FAST).**
  Chaque onglet (Alertes FAST + Carte FAST) pilote désormais son
  indice indépendamment. `validation_sampling` FAST consomme l'index
  exporté par Alertes FAST via le retour `fast_alerts_ret$index_r`.
  Le radio parent posé en v0.52.13 est retiré ; les 4 `thresholds_r`
  purgés du champ `index = ...` (transmission désormais via le
  reactive export du sous-module).

## [0.52.13] - 2026-06-01

### Fixed

- **FAST API mono-index (suite à `nemeton@v0.55.0` spec 017).**
  Le cœur a simplifié `read_fast_alert_raster()` en mono-index
  (`threshold_ndvi` + `threshold_nbr` → `index` + `threshold`).
  L'app continuait à passer les anciens paramètres → `arguments
  inutilisés` → carte d'alertes vide.

### Added

- **Radio sidebar « Indice FAST » (NDVI / NBR, défaut NDVI).**
  Pilote l'indice utilisé par `read_fast_alert_raster()`. Les 2
  sliders thresholds restent en place ; seul celui correspondant à
  l'indice sélectionné est forwardé au cœur. Pour comparer les 2
  vues, basculer le radio (recalcul sub-seconde depuis le cache S2).

### Changed

- **`Imports: nemeton (>= 0.55.0)`** — l'app exige maintenant l'API
  mono-index. Sans ce plancher, un install contre un cœur antérieur
  casserait au premier `Diagnostic FAST`.

## [0.52.12] - 2026-06-01

### Fixed

- **Plan d'actions — tableau rendu VIDE (régression v0.52.10).**
  Le JS callback ajouté en v0.52.10 pour le dblclick sur la cellule
  commentaire passait à `DT::datatable(callback = …)` une fonction
  COMPLÈTE (`function(table) { … }`), alors que DT wrappe lui-même
  le callback dans `function(table) { … }`. Le double-wrapping
  créait une fonction interne JAMAIS invoquée (handler dblclick
  perdu) ET pas de `return table;` → DataTables cassait
  silencieusement son init → tableau rendu sans aucune ligne malgré
  un data.frame source de N lignes. Fix : le callback est désormais
  juste le CORPS de fonction (pas de wrapper), avec `return table;`
  à la fin. DT applique son propre wrapper et le pipeline init
  reprend normalement.

## [0.52.11] - 2026-06-01

### Changed

- **Carte FAST — `card_header` titre remplacé par un bandeau inline.**
  Le `bslib::card_header` qui portait le titre « Carte pixel — NDVI /
  NBR à la résolution Sentinel-2 (10 m) » mangeait une rangée
  entière et créait une dissymétrie avec Alertes FAST voisin (qui
  n'a pas de header). Le titre passe désormais en bandeau
  `alert-info` inline au-dessus de la carte, padding minimal,
  symétrique stylistiquement avec le bandeau vert « Aucune alerte
  FAST » d'Alertes FAST. Gain ~30-40 px verticaux + cohérence
  visuelle entre les 2 sous-onglets.

## [0.52.10] - 2026-06-01

### Added

- **Plan d'actions — dblclick sur cellule commentaire ouvre le modal d'édition.**
  La colonne commentaire est étroite + ellipsisée dans le tableau →
  long texte illisible. L'édition inline DT (single-line input dans
  cellule étroite) était même contre-productive. Le commentaire
  passe désormais EXCLUSIVEMENT par le modal multi-ligne (textarea
  6 rangs, déjà utilisé par dblclick kanban). Un dblclick sur la
  cellule commentaire du tableau ouvre maintenant ce même modal,
  qui expose en plus statut / priorité / année. Affordance visuel
  via CSS : curseur main + soulignement pointillé sur la cellule.

### Changed

- `EDITABLE_COLS` ne contient plus `commentaire` — l'édition inline
  DT est désactivée pour cette colonne uniquement. Les autres
  colonnes restent inline-éditables comme avant.

## [0.52.9] - 2026-06-01

### Fixed

- **Plan d'actions — contexte IA non rafraîchi après création des commentaires Synthèse.**
  Le reactive `plan_llm_context()` dans `mod_action_plan.R` ne
  dépendait d'aucun signal lié à `save_comments()` → il lisait
  `load_comments()` une seule fois au montage et restait figé sur le
  snapshot vide. L'utilisateur qui générait les commentaires côté
  Synthèse APRÈS avoir ouvert Plan d'actions voyait toujours
  `action_plan_generate_no_comments`. Fix : ajout d'un slot
  `app_state$comments_refresh = 0L` bumpé par les 3 call sites de
  `save_comments()` (mod_synthesis IA + manuel, mod_family manuel),
  et lu en tête de `plan_llm_context()` pour créer la dépendance
  Shiny. Pattern symétrique avec `samples_refresh` existant
  (mod_sampling → mod_monitoring).

## [0.52.8] - 2026-05-31

### Changed

- **Onglet Alertes FAST — contrôles déplacés à droite de la carte (sidebar).**
  Avant : Mode du raster (Fréquence/Intensité), Afficher le raster
  et Opacité occupaient une ligne `flex-wrap` horizontale au-dessus
  de la carte, mangeant de la hauteur utile et différant
  visuellement de l'onglet voisin Carte FAST (sidebar droite depuis
  v0.47.0). Après : `bslib::card` + `bslib::layout_sidebar(position
  = "right", width = 250L)`, exactement comme Carte FAST. La carte
  gagne la zone rectangulaire principale, les 3 contrôles vivent
  dans la sidebar à droite. L'observer de refresh i18n gère
  désormais aussi le label radio « Mode du raster » (NULL avant),
  le checkbox et le slider — plus de label figé en FR après un
  switch en EN.

## [0.52.7] - 2026-05-31

### Added

- **Bouton « Enregistrer ce projet comme zone de suivi » INLINE dans le bandeau Suivi sanitaire.**
  Le bouton sidebar historique tombait systématiquement sous le pli
  sur les écrans 1080p — l'utilisateur voyait le message « Aucune
  zone enregistrée » (ou le bandeau orphelin v0.52.5) sans voir
  l'action. Ce bouton est désormais rendu directement dans le
  bandeau dans les deux branches concernées : `n == 0` (DB vide)
  avec un style `btn-primary` bleu, et « zone orpheline » (zones
  présentes mais aucune pour ce projet, après wipe par les tests
  cœur) avec un style `btn-warning` jaune cohérent avec le card
  warning. Le bouton sidebar reste en place.

### Changed

- **Observer `input$register` refactoré en `observe() + bindEvent`.**
  L'observer historique de la registration écoute désormais à la
  fois `input$register` (sidebar) ET `input$register_inline`
  (bandeau) via un même `shiny::bindEvent(..., ignoreInit = TRUE)`
  — pas de duplication de logique entre les deux call sites.

## [0.52.6] - 2026-05-31

### Fixed

- **Synthèse — alignement fin de la légende « Taille image Max 5 Mo, PNG/JPG » sur les badges.**
  `v0.52.3` calait le centre vertical de la légende sur le centre du
  bouton « Image de couverture » (`padding-top: 0.55rem` ≈ ½ bouton
  38px). Mais la ligne des badges de la colonne droite (`NDP /
  Hauteur LiDAR / Inventaire ML`) tombe ~8 px sous le centre du
  bouton, parce que le flux `Score global → 54.8 → /100 (12 familles)`
  n'a pas exactement la même hauteur cumulée que les 2 boutons PDF +
  GeoPackage à gauche. `padding-top` passe à `1rem` (≈ 16 px) pour
  descendre le texte au niveau du centre des badges.

## [0.52.5] - 2026-05-31

### Added

- **Bandeau « zone orpheline » dans Suivi sanitaire.**
  Détection app-side de l'état où la DB monitoring contient des zones
  mais aucune n'est rattachée au projet chargé — symptôme typique
  d'un wipe par les tests cœur `helper-monitoring.R` qui DROP CASCADE
  les 7 tables monitoring sans garde-fou (incident villards
  2026-05-31). Avant ce fix, l'utilisateur voyait un bandeau vert
  trompeur « N zone(s) connectée(s) » alors qu'aucune ne lui
  appartenait. Désormais, si `nrow(zones) > 0` mais le `project$id`
  courant n'est dans aucune `monitoring_zone.project_uuid`, on
  bascule sur un bandeau jaune `warning` qui guide vers le bouton
  « Enregistrer ce projet comme zone de suivi » de la barre latérale.
  Le fix définitif est côté cœur (brief à passer en session
  `/home/pascal/dev/nemeton` → `nemeton@v0.54.0`).
- `R/utils_i18n.R` : nouvelles clés `monitoring_zone_orphan_title`
  et `monitoring_zone_orphan_body` (FR/EN).

## [0.52.4] - 2026-05-31

### Fixed

- **Carte FAST — courbes pixel/placette hachées sur les zones de recouvrement partiel MGRS.**
  La zone villards est couverte par deux tuiles Sentinel-2 MGRS qui
  se chevauchent partiellement (T31TGM large couvre toute la zone,
  T31TFM étroite ne couvre que l'OUEST). Pour un pixel à l'EST, les
  ~62 scènes T31TFM retournent `value = NA` (pixel hors couverture)
  et plotly cassait la ligne à chaque NA → les ~60 mesures T31TGM
  valides apparaissaient comme des points isolés sans lignes. Fix :
  filtre les NA après tri par date et avant `add_trace` dans les deux
  modaux (pixel-click et marker-click placette). La courbe redevient
  continue à partir des seules observations réellement disponibles.

## [0.52.3] - 2026-05-31

### Fixed

- **Onglet Synthèse — légende « Taille image Max 5 Mo, PNG/JPG » repositionnée à droite du fileInput.**
  `v0.52.2` avait centré la légende sous le fileInput ; la demande
  UX était de l'avoir à droite du sélecteur « Image de couverture »,
  au niveau de la ligne des badges. Solution :
  `align-items: flex-start` (ancre la légende en haut du flex = haut
  du bouton) + `padding-top: 0.55rem` (≈ moitié de la hauteur du
  bouton 38px) pour la descendre pile au centre du bouton.
  L'alignement reste stable que la barre « Upload complete » du
  fileInput soit affichée ou non.

## [0.52.2] - 2026-05-31

### Fixed

- **Onglet Synthèse — alignement de la légende « Taille image Max 5 Mo, PNG/JPG ».**
  Sortie du flex inline (où elle vivait à droite du bouton « Image de
  couverture », donc plus haut que la ligne des badges) et placée sur
  une ligne dédiée centrée sous le fileInput, ce qui l'aligne
  visuellement avec la ligne `NDP 1 – Observation | Hauteur LiDAR HD
  | Inventaire ML` de la colonne de droite.

## [0.52.1] - 2026-05-31

### Fixed

- **Warning « relation `monitoring_zone` does not exist » au boot Postgres.**
  Le chemin RO de `get_monitoring_db_connection()` sautait
  volontairement les migrations (optimisation correcte pour SQLite :
  fichier = déjà migré, mais fausse pour Postgres : base toujours là,
  schéma possiblement vide). Le premier reactive tick au démarrage
  émettait alors un warning, qui disparaissait dès que le premier RW
  path migrait la base. Fix : pour Postgres on appelle aussi
  `.ensure_monitoring_schema()` sur le RO path (idempotent,
  sub-milliseconde après la 1re fois). SQLite garde son fast-path.

## [0.52.0] - 2026-05-31

### Changed

- **Vrai cancel coopératif FAST/FORDEAD (s'appuie sur `nemeton@v0.53.0`).**
  Le clic « Annuler le diagnostic » écrit désormais
  `<projet>/data/{fast,fordead}_cancel.flag`, que le worker poll entre
  tuiles (FAST) / entre phases reticulate (FORDEAD) et qui le fait
  sortir proprement au prochain checkpoint avec commit partiel. Les
  INSERT déjà commités sont conservés (`ON CONFLICT DO NOTHING` — relance
  sans risque).
- **i18n — « Libérer l'interface » → « Annuler le diagnostic » /
  « Cancel the diagnostic ».** Le libellé reflète maintenant le vrai
  cancel coopératif ; le toast `monitoring_run_cancel_done` reformulé
  pour expliquer le mécanisme (tuile/phase courante finit, puis stop).
- **`Imports: nemeton (>= 0.53.0)`.** Bump du plancher : l'app exige
  maintenant `cancel_path` côté cœur (`ingest_sentinel2_timeseries` et
  `run_fordead_dieback`).

### Added

- `service_monitoring.R` : `run_ingestion_async()` et
  `run_fordead_async()` exposent un paramètre `cancel_path = NULL`,
  forwardé au cœur.
- `mod_monitoring.R` : `input$run` et `.invoke_fordead` purgent un flag
  résiduel avant chaque lancement (sinon le worker abandonnerait
  d'emblée) ; `fast_task$invoke()` et `fordead_task$invoke()` passent
  le chemin du flag ; observers `input$run_cancel` /
  `input$run_health_cancel` écrivent le flag **avant**
  `force_unlock_*(TRUE)` (UI libérée immédiatement, worker sort au
  prochain checkpoint).

## [0.51.11] - 2026-05-31

### Changed

- **i18n — « Annuler / Réinitialiser » → « Libérer l'interface ».** Le
  bouton qui apparaît pendant un diagnostic FAST/FORDEAD ne tue pas le
  worker (Shiny `ExtendedTask` n'a pas d'API d'annulation) — il
  force-unlock l'UI. Nouveau libellé qui reflète exactement ce que le
  bouton fait, sans suggérer que le diagnostic est arrêté en base. Toast
  de confirmation aligné. Les deux boutons (FAST + FORDEAD) partagent
  les mêmes clés i18n.

## [0.51.10] - 2026-05-31

### Added

- **Heartbeat de fin pour les workers d'ingestion.** Le worker FAST
  (resp. FORDEAD) émet désormais un événement `s2:ingest_done` (resp.
  `fordead:dieback_done`) via `progress_callback` juste après le retour
  du cœur. Permet de diagnostiquer un bouton resté grisé : si
  l'événement apparaît, le cœur a rendu la main et le bug est dans le
  hand-off Shiny ExtendedTask ; sinon nemeton finalise encore.

### Fixed

- **Carte FAST pixel map — silence des warnings `colors(.)`.**
  `terra::clamp(r, -1, 1, values = TRUE)` avant `addRasterImage()`
  ramène les ε-overshoots numériques de NDVI / NBR dans le domaine
  `[-1, 1]` de la palette plasma. Plus aucun warning « Some values
  were outside the color scale » à chaque re-render.

## [0.51.9] - 2026-05-30

### Fixed

- **Alertes FAST — raster d'alerte invisible.** Le masque
  `terra::ifel(r == 0, NA, r)` ne couvrait pas les valeurs négatives
  résiduelles, qui sortaient du domaine de `pal()` → 4 warnings
  `Some values were outside the color scale` et raster majoritairement
  transparent. Masque ≤ 0 (positif strict) + clamp à `upper` (p95) en
  mode rolling avant `pal()`.
- **Graphique pixel timeseries — lignes manquantes / sauts dans le
  temps.** Boucle `for (b in unique(ts$index))` ne triait pas par
  `obs_date` avant `plotly::add_trace` → segments reliés dans l'ordre
  des lignes du data.frame. Sort par date ajouté.

## [0.51.8] - 2026-05-30

### Fixed

- **Onglet Fournisseur LLM — status panel réactif au provider.** Le
  bloc statut + clé est désormais un `uiOutput` réactif à
  `input$llm_provider` (avant il restait figé sur le précédent provider
  quand on changeait dans la liste).

### Added

- **Onglet Fournisseur LLM — vue d'ensemble multi-providers.** Badge ✓
  dans la liste déroulante pour chaque provider configuré + ligne
  résumé au-dessus du sélecteur (« N / 3 fournisseurs configurés : …»).

## [0.51.7] - 2026-05-30

### Added

- **Modal de configuration à 2 onglets : Theia + Fournisseur LLM.**
  L'icône engrenage ouvre maintenant une boîte « Clés API externes »
  avec deux onglets. Theia (inchangé) ; LLM avec selectInput
  Mistral/Anthropic/OpenAI, status alert avec source (env ou fichier),
  bouton Save / Modifier / Supprimer. Persistance dans
  `~/.config/nemetonshiny/llm.json` (chmod 0600) + Sys.setenv pour
  effet immédiat. Résolution env > fichier (`.Renviron` continue de
  fonctionner). Nouveau service `R/service_llm.R` + tests dédiés.

## [0.51.6] - 2026-05-30

### Security

- **`~/.config/teledetection/.apikey` désormais en `0600`.** La clé
  Theia / DATA TERRA enregistrée via `theia_save_api_key()` est
  immédiatement verrouillée à l'écriture (`Sys.chmod`). Auparavant le
  fichier héritait du `umask` du process (souvent `0644`). No-op sous
  Windows.

### Changed

- **Modal Theia — section clé contextuelle.** Quand la clé est déjà
  configurée, le modal affiche un bandeau « configurée » + boutons
  « Modifier » / « Supprimer » au lieu d'un formulaire vide qui invitait
  à l'écrasement. Helper `theia_clear_api_key()` ajouté.

### Fixed

- **Modal Theia — table « Provenance et licence » apparaît.** Le
  `DT::datatable()` inline dans `modalDialog()` n'initialisait pas son
  JS htmlwidget → table invisible. Remplacée par une table Bootstrap
  statique (`htmltools::tags$table`).

## [0.51.5] - 2026-05-30

### Fixed

- **Alertes FAST — préserve le zoom et le fond OSM/Satellite.** Le
  `renderLeaflet` dépendait du raster et des contrôles (mode / opacité /
  visibilité / seuils) → chaque mouvement de slider ré-initialisait le
  zoom utilisateur et le fond sélectionné. Pattern aligné sur Carte
  FAST : la base (tuiles + UGF + fitBounds) est rendue une seule fois,
  le raster d'alerte et sa légende sont mis à jour via `leafletProxy` +
  `clearGroup` + `removeControl` (légende `layerId`-bée). Zoom et fond
  conservés à travers les sliders.

## [0.51.4] - 2026-05-29

### Fixed

- **Réamorçage du cache COG restreint à la fenêtre FAST.** Le cache S2
  est partagé FAST/FORDEAD ; cocher « Réamorcer le cache COG » faisait un
  `unlink` de tout le dossier, effaçant aussi les bandes/dates FORDEAD
  (dont l'apprentissage). Le wipe ne supprime désormais que les scènes
  dont la date d'acquisition tombe dans la fenêtre de dates FAST ; les
  scènes hors fenêtre (apprentissage FORDEAD) et non datables sont
  préservées. Libellé + aide i18n mis à jour.

## [0.51.3] - 2026-05-29

### Changed

- **Alertes FAST — alignement des contrôles d'en-tête.** Case « Afficher
  le raster » légèrement abaissée (`top: 2px`) pour s'aligner sur les
  radios Fréquence/Intensité ; label « Opacité du raster » déplacé à
  gauche du slider (inline) au lieu d'au-dessus.

## [0.51.2] - 2026-05-29

### Fixed

- **Régression v0.50.1 `objet '.pkg_path' introuvable`.** Le renommage
  `.pkg_path` → `.dev_pkg_path` (fix worker v0.50.1) n'avait été propagé
  qu'à `compute_task` ; `parcels_task` et les workers `mod_search` /
  `service_monitoring` référençaient encore `.pkg_path` → échec au
  chargement des parcelles cadastrales. Bootstrap worker `is_dev_package`
  désormais unifié sur toutes les ExtendedTasks (namespace installé ou
  source en vrai mode dev, plus jamais un clone git périmé).
- **Chargement de projet : plus de gel avant l'affichage des parcelles.**
  Le sync PostGIS (`db_sync_project`) au chargement est déféré
  (`later::later`) hors du chemin critique, et `get_db_connection` gagne
  un `connect_timeout` (défaut 8 s, `NEMETON_DB_CONNECT_TIMEOUT`) pour
  fail-fast sur un hôte injoignable au lieu du timeout OS (~20 s).

## [0.51.1] - 2026-05-29

### Fixed

- **Carte FAST pixel : rendu de l'AOI complète (toutes tuiles MGRS).**
  Le `scenes_df` du stack NDVI/NBR est construit depuis l'inventaire
  disque du cache Sentinel-2 (toutes scènes peuplées) au lieu de
  `obs_pixel` (pixels aux placettes seulement) : une AOI à cheval sur
  deux tuiles MGRS (villards) dont une tuile sans placette s'affiche
  désormais en entier. Date résolue depuis la base sinon parsée de
  l'identifiant de scène S2. Limite : si une seule tuile a été ingérée
  pour une date, l'autre moitié reste absente (sujet d'ingestion).

### Added

- Smoke E2E shinytest2 du sélecteur `control_classes`
  (`test-validation-control-classes-e2e.R`), skip propre sans chromote.

## [0.51.0] - 2026-05-29

### Added

- **Plan de validation : sélecteur `control_classes` pour les placettes
  témoins.** Le sous-onglet expose l'argument `control_classes` du cœur
  `nemeton::create_validation_sampling_plan()` : cases 0–4 (défaut 0)
  distinctes des classes d'alerte, affichage de la distribution du raster
  d'alerte (aide au choix), auto-relax vers la classe la plus saine
  présente quand aucune cellule classe 0 n'existe (cas villards), et
  garde-fou (toast clair) quand 0 témoin est produit. Nouvelles clés
  i18n FR/EN. Plancher `nemeton (>= 0.51.0)` inchangé.

## [0.50.1] - 2026-05-28

### Fixed

- **Le worker de calcul async chargeait un mauvais code.** Le worker
  `future::multisession` résolvait le package via `pkgload::pkg_path()`
  sans argument (qui remonte depuis `getwd()`), si bien qu'un
  utilisateur de la version installée lancée depuis un clone git local
  faisait `load_all()` du clone (souvent périmé) dans le worker →
  CHM/MNH/MNT échouaient silencieusement via l'UI alors que le calcul
  synchrone réussissait. Le mode dev n'est désormais retenu que si
  `is_dev_package("nemetonshiny")` est vrai (via `find.package()`) ;
  sinon le worker charge le namespace installé
  (`loadNamespace("nemetonshiny")`). La branche prod chargeait par
  erreur `nemeton` seul au lieu de `nemetonshiny`.

## [0.50.0] - 2026-05-28

### Changed

- **Monitoring local : SQLite/WAL uniquement.** Le backend DuckDB,
  déprécié en 0.49.0, est retiré définitivement (cœur `nemeton` v0.51.0).
  `.resolve_monitoring_db_url()` émet toujours
  `sqlite://<projet>/data/monitoring.sqlite` en local ; branche
  back-compat DuckDB et `.nemeton_supports_duckdb()` supprimés ;
  helpers `.is_file_db_url` / `.file_db_path_from_url` restreints à
  SQLite. PostgreSQL inchangé. Clé i18n `monitoring_db_duckdb_missing`
  → `monitoring_db_local_pkg_missing`.

### Removed

- `duckdb` retiré des `Suggests` ; plancher `Imports: nemeton (>= 0.51.0)`.

### Migration

- Un ancien `monitoring.duckdb` local n'est plus lu ni migré : le suivi
  local repart sur un `monitoring.sqlite` neuf. Ré-ingérer les séries
  (régénérables depuis le cache Sentinel-2 + la DB).

## [0.49.1] - 2026-05-28

### Fixed

- **Téléchargement des dalles MNH LiDAR HD (IGN) cassé sous Windows.**
  `extract_tile_names()` faisait `basename(url)` sur l'URL WMS GetMap de
  la Géoplateforme (`…/wms-r?…&FILENAME=LHD_…tif`), produisant un nom de
  cache truffé de `:` (`CRS=EPSG:2154`) et `,` (`BBOX=…`), illégaux sous
  Windows → 0 dalle écrite → CHM indisponible alors que la dalle existe.
  Nom canonique lu depuis `FILENAME=`, repli basename propre puis nom
  généré, nettoyage des caractères illégaux. + 5 tests de non-régression.
- **Lisibilité du bandeau vide « Aucune alerte FAST »** : corps passé de
  `text-muted` à `text-white` (gris illisible sur le vert saturé du thème).

## [0.45.0] - 2026-05-26

### Added

- **Fallback `lasR` pour le CHM depuis les nuages LiDAR HD locaux**.
  Quand les dalles MNH/MNT pré-rasterisées de l'IGN échouent au
  téléchargement (régulier en 2026 : la couche `NUAGE` COPC reste
  servie mais `IGNF_MNH-LIDAR-HD:dalle` et `IGNF_MNT-LIDAR-HD:dalle`
  retombent en 404 par dalle), `nemetonshiny` bascule sur
  `nemeton::compute_dtm_chm_from_laz()` pour dériver localement le
  CHM (et le MNT) depuis les `.copc.laz` déjà en cache. Mesure
  réelle (vs prédiction ML d'Open-Canopy), purement locale (pas de
  modèle à télécharger, pas de GPU), chaîne d'install légère.
  Intercalé dans la chaîne d'acquisition CHM entre LiDAR HD MNH
  (Step 1) et Theia FORMSpoT (Step 1.5). Opt-out via
  `options(nemetonshiny.chm_lasr_fallback = "off")` ou
  `NEMETONSHINY_DISABLE_CHM_LASR=1`. Plancher
  `nemeton (>= 0.48.0)`. `lasR` ajouté en `Suggests:`.
- **Diagnostic catégorisé des échecs de download IGN LiDAR HD**.
  `download_ign_lidar_hd()` appelle
  `nemeton::probe_ign_lidar_tiles()` quand 0 tuile a été téléchargée
  et affiche un résumé par catégorie (`not_found` / `forbidden` /
  `timeout` / `dns` / `connection` / `server_error`) au lieu du
  laconique `failed`.
- 5 nouvelles clés i18n bilingues NMT-compliant
  (`chm_phase_lasr_fallback`, `chm_fallback_lasr_start`,
  `chm_fallback_lasr_success`, `chm_fallback_lasr_skip_no_tiles`,
  `chm_fallback_lasr_skip_no_pkg`).
- 5 tests unitaires dans `tests/testthat/test-service_compute.R`
  couvrant les branches opt-out env, opt-out option, lasR manquant,
  dossier vide, et l'appel mocké à
  `nemeton::compute_dtm_chm_from_laz()`.

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

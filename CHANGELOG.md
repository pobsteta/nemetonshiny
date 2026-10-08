# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.1.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

For a narrative, per-feature description of each release, see
[NEWS.md](https://pobsteta.github.io/nemetonshiny/NEWS.md). This file is
the concise, categorised trail.

## \[Unreleased\]

## \[1.2.1\] - 2026-10-08

### Changed

- Indicator labels are read from the core only
  ([`nemeton::indicator_labels()`](https://pobsteta.github.io/nemeton/reference/indicator_labels.html)),
  short codes included; the 40 local `indicator_<CODE>` keys are removed
  (brief indicator-families, step 4).
- The family-view cause banner names the units left empty when an
  indicator is only partly empty (brief trois-derniers-points, 1.3).
- RECONFORT (broadleaves) tabs use a broadleaf tree icon instead of the
  conifer shared with FORDEAD.
- Application guide: Atlas tab (Sélection / Synthèse sub-tabs) and Suivi
  sanitaire sub-tabs.

### Fixed

- A bivariate E-OBS context cache built on another N × N scheme (legacy
  3 × 3) is recomputed instead of served (brief 034 bivariate-cache, bug
  A).

### Removed

- Unused i18n keys: `r5_label`, `r5_tooltip`, 15 `foret_ancienne_*`.

## \[1.2.0\] - 2026-10-08

### Added

- Network typing (Desserte): IFN reference route (rate per species, SER
  from `ensure_ugf_ser()`, species from the project’s BD Forêt); in NDP
  0, empty P1 is completed by
  [`nemeton::completer_volume_ifn()`](https://pobsteta.github.io/nemeton/reference/completer_volume_ifn.html).
  The result shows the rate level, the IFN share of the volume and
  unresolved species (brief 040).
- reGénération map: « ΔT°max × ΔVPD » bivariate layer and « Meilleure
  essence » layer (briefs 027 §4.3, 039 §7).
- E-OBS acquisition steps under the « Auto (E-OBS) » button; BILJOU
  forcing progress (SAFRAN unit, ERA5 unit × year) in the engine
  notification.
- E-OBS attribution and non-commercial licence under the context map.
- Canopy badge third state, Open-Canopy ML CHM; A5
  `skipped_no_reference` cause.

### Changed

- Canopy provenance goes through
  [`nemeton::canopy_provenance()`](https://pobsteta.github.io/nemeton/reference/canopy_provenance.html)
  (keys `lidar_hd`, `prosail_s2`, `opencanopy`).
- The OSM layer of the Desserte map shows the tracks outside the BD TOPO
  corridor (`osm_hors_corridor`, foretaccess ≥ 2.4.0), not the raw
  acquisition.
- Family menu, family order (PDF export, action-plan objectives) and
  family code lookup read from the core; `FAMILLE_NMT_MAP` removed.

## \[1.1.0\] - 2026-10-07

### Changed

- « Sélection » and « Synthèse » are now sub-tabs of the main « Atlas »
  tab (new `R/service_navigation.R` maps logical tabs to their parent
  tab).
- « Suivi sanitaire »: the « Mode de suivi » radio is replaced by three
  sub-tabs, FAST, FORDEAD and RECONFORT (same `mode` input and values).

### Fixed

- LiDAR HD tiles are listed through the IGN
  `IGNF_LIDAR-HD_METADONNEE:metadata` WFS layer; the per-product layers
  now answer 404.

## \[1.0.1\] - 2026-10-07

### Changed

- The « Sélection » tab is renamed **« Atlas »** (FR and EN), along with
  the messages pointing to it. The `tab_selection` i18n key is
  unchanged.

## \[1.0.0\] - 2026-10-06

First stable release. **Breaking**: projects and databases created with
0.x versions are not taken over (no migration); recreate them.

### Changed

- Requires `nemeton (>= 1.0.0)`.
- Projects carry `format_projet = 1`; older projects are flagged
  “pre-1.0” (delete only) and refused by the API
  (`nemetonshiny_projet_ancien`).
- [`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md)
  exposes `format_projet` / `format_ok`;
  [`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md)
  exposes `format_ok`.
- `inst/sql/schema.sql` holds the full 1.0 schema (`_norm` columns,
  `*_states` archives).
- Core 1.0.0 statuses translated (P2 without real age, C1 from NDVI, P3
  components, P2 outside site curves, conditional indicators).

### Removed

- `projet_migrer()` and the `nemetonshiny_projet_perime` error class.
- Indicator-sense check (`indicator_sense_version`) and legacy project
  file formats (`atomes.gpkg`, `atome_id`).
- `inst/sql/migration_00N_*.sql` (folded into `schema.sql`).
- INPN “wetlands” layer (mostly ZNIEFF, unused by the core).

## \[0.157.1\] - 2026-10-06

### Added

- Guide de l’application
  ([`vignette("guide-application_fr")`](https://pobsteta.github.io/nemetonshiny/articles/guide-application_fr.md),
  article pkgdown), repris du coeur et reecrit pour l’app actuelle.

## \[0.157.0\] - 2026-10-05

### Changed

- Scores de famille et score global ponderes par la surface des UGF
  ([`nemeton::aggregate_family_scores()`](https://pobsteta.github.io/nemeton/reference/aggregate_family_scores.html))
  : Synthese, radar, tableau, rapport PDF, prompt IA,
  [`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md),
  serveur MCP. Les scores affiches changent.
- Composite NDVI S2 de C2 par
  [`nemeton::build_ndvi_season_composite()`](https://pobsteta.github.io/nemeton/reference/build_ndvi_season_composite.html)
  (offset radiometrique retire) ; cache `ndvi_s2_v2.tif`.
- Indices ombrothermiques par
  [`nemeton::climate_ombrothermic_indices()`](https://pobsteta.github.io/nemeton/reference/climate_ombrothermic_indices.html).
- Plancher `Imports: nemeton (>= 0.216.0)`.

## \[0.156.1\] - 2026-10-05

### Added

- Signal ” page prete ” : `postMessage` a la page qui a ouvert l’app
  (VICTOR), `ready` apres application du lien profond et repos stable,
  `invalid` pour un lien refuse ; origine `NEMETON_VICTOR_ORIGIN`.

### Fixed

- Fermer un onglet pendant la restauration d’un projet arretait tout le
  serveur (shiny 1.14) : rappels `later` via `.later_sur()`.
- Mode dev `load_all()` : les patches de test (`future_promise` rendant
  NULL, `testServer`) ne s’appliquent plus que sous testthat.

## \[0.156.0\] - 2026-10-05

### Fixed

- Audit 1.0, lots 1 a 8 : fichiers de cles owner-only, TLS impose et
  identifiants masques pour la base de suivi, messages ntfy assainis.
- Lecture seule respectee partout (commentaires, desserte, plan
  d’actions) et recalculee au changement d’authentification ; audit
  signe par l’utilisateur.
- Langue par session ; analyse reGeneration, appels LLM et battement du
  verrou hors de la boucle Shiny ; delais Python et chien de garde de la
  chaine.
- Suivi sanitaire : etape zones, G3, annulations, zone RECONFORT `_tot`.
- Caches cles sur l’emprise (couches, desserte, contexte reGeneration).
- Ecritures atomiques (OSO, LiDAR) ; calcul sans indicateur = echec.
- Import de tenements (CRS, elements hors parcelles), contenance NA,
  score global NA, configuration des sources, carte, cartes de fin de
  calcul.

### Changed

- Badges NDP lisibles et traduits ; un seul bouton principal en Synthese
  ; textes restants passes par l’i18n ; JS sans ecouteurs empiles.
- Keycloak de dev : `keycloak/realm-nemeton-dev.json`, secrets
  obligatoires via `.env` (`.env.example`).
- CI : job `R-CMD-check-oldrel` non bloquant.
- NEWS et CHANGELOG archives avant 0.130.0 ; logo allege.

### Removed

- Code mort : radar/palettes de `utils_theme.R`,
  `tenement_split_by_line()`, `db_load_parcels()`, `needs_migration()`,
  sept helpers cadastre/communes.

## \[0.155.0\] - 2026-10-05

### Added

- Serveur MCP (`inst/mcp/server.R`, `mcptools` en Suggests) :
  `lister_projets`, `resume_projet`, `lancer_calcul`, `etat_calcul`,
  `annuler_calcul`, `generer_rapport`, `exporter_gpkg`, `url_app`.
- Calcul detache (`setsid`) suivi par `data/compute_job.json`.
- Liens profonds `?project=<id>&tab=<onglet>` ; variable
  `NEMETON_APP_PORT`.
- Libelles FR/EN de `r1_status` et des motifs d’import des validations.

### Fixed

- T2 n’est plus NA : N2 calcule avant T2 et transmis, T1 en repli.
- Bandeau d’indicateur : premier statut traduit retenu (repli R1
  visible).
- `prune_orphan_zone_caches(project_uuid =)` : pas de purge sur une
  autre base.
- Racine du corpus RAG transmise au worker d’import.
- Sources documentaires et reponses IA rendues sans HTML brut
  (`markdown_safe()`).

## \[0.154.0\] - 2026-10-04

### Added

- API hors interface exportee
  ([`?api_hors_interface`](https://pobsteta.github.io/nemetonshiny/reference/api_hors_interface.md))
  :
  [`projets_lister()`](https://pobsteta.github.io/nemetonshiny/reference/projets_lister.md),
  [`projet_etat()`](https://pobsteta.github.io/nemetonshiny/reference/projet_etat.md),
  [`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md),
  `projet_migrer()`,
  [`parcelles_commune()`](https://pobsteta.github.io/nemetonshiny/reference/parcelles_commune.md),
  [`projet_creer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_creer.md),
  [`projet_calculer()`](https://pobsteta.github.io/nemetonshiny/reference/projet_calculer.md),
  [`projet_rapport()`](https://pobsteta.github.io/nemetonshiny/reference/projet_rapport.md),
  [`projet_gpkg()`](https://pobsteta.github.io/nemetonshiny/reference/projet_gpkg.md)
  ; erreurs classees (`nemetonshiny_erreur`) ; `CONTRAT.md` section 1
  bis.
- [`projet_lire()`](https://pobsteta.github.io/nemetonshiny/reference/projet_lire.md)
  sans aucune ecriture : erreur `nemetonshiny_projet_perime` si une
  migration serait necessaire, appliquee seulement par
  `projet_migrer()`.
- Variable `NEMETON_PROJECT_DIR` : dossier des projets par defaut.

### Changed

- Scores de famille et score global de la Synthese extraits dans
  `R/service_synthesis.R` (memes chiffres pour l’onglet et l’API).
- Plancher `Imports: nemeton (>= 0.212.0)` (borne E1/E2 a 2,64).

### Fixed

- Les indicateurs invalides sont renommes
  (`indicators.perime-v<n>-<date>.parquet`, deux generations), plus
  jamais supprimes : un simple chargement d’un projet au sens perime les
  detruisait.
- Un projet neuf porte le marqueur de sens courant : cree puis calcule
  hors interface, il n’est plus invalide a sa premiere ouverture.

## \[0.153.0\] - 2026-10-03

### Added

- `run_app(options = list(...))` : port, hote et options Shiny ; le
  navigateur ne s’ouvre qu’en session interactive.
- `CONTRAT.md` : contrat public de la 1.0 (point d’entree, variables
  d’environnement, format des projets, schema PostGIS, compatibilite).

### Changed

- Licence : GPL-3 ou ulterieure, alignee dans tous les fichiers.
- `future`, `arrow` et `geoarrow` en `Imports`.
- Image Docker : R 4.6.1, utilisateur non root, volume `/data` ;
  verifiee (construction et service HTTP).
- README reecrit ; R CMD check sans WARNING ni NOTE ; CI qui echoue sur
  un WARNING ; branche `main` protegee.

### Removed

- Fichier `.s2.out` commite par erreur.

## \[0.152.5\] - 2026-10-03

### Fixed

- Plus de NDVI aleatoire en cache quand le WMS IGN echoue (un ancien
  NDVI synthetique est detecte et jete) ; plus de valeurs aleatoires
  pour un indicateur inconnu du coeur.
- Plans de validation : union des colonnes dans `samples.gpkg` (plus de
  colonnes perdues) ; plantage `.format_m3()` sur tige non cubee ;
  lecture seule a tort sans base de donnees.
- Ecritures atomiques des fichiers de projet ; UGF illisibles et plan
  d’actions illisible sauvegardes avant remplacement ; synchronisation
  PostGIS transactionnelle.
- Resultats asynchrones (calcul, reGeneration, « Tout calculer »,
  desserte, echantillonnage, ingestion terrain) lies a leur projet.
- Calendrier du plan d’actions ancre (`plan$annee_base`) ; GPKG sans
  decalage d’un an ; la chaine n’efface plus les actions ; edition d’UGF
  et changement de parcelles invalident les indicateurs.

## \[0.152.4\] - 2026-10-02

### Security

- Identifiant de projet valide cote serveur (un seul segment de chemin,
  sous la racine) avant tout acces disque ; suppression d’un projet
  corrompu reverifiee et refusee en lecture seule.
- Authentification qui echoue ferme quand OAuth est configure mais
  indisponible ; « sans role = editeur » reserve au mode anonyme ; roles
  techniques Keycloak ignores, `gestionnaire` reconnu.
- Cles Theia/LLM du serveur et reinitialisation du corpus RAG reservees
  a l’administrateur.
- Echappement des libelles d’UGF, du nom de projet et des essences
  Marculus (XSS stocke) ; echappement LaTeX dans les PDF.
- Mise a jour de projet refusee en lecture seule ; `.dockerignore` sans
  secrets ; realm Keycloak de dev avec les roles dans `userinfo`.

### Changed

- Sur un Keycloak existant, publier les roles du realm dans `userinfo` :
  sans ce mapper, les utilisateurs connectes passent en lecture seule.

## \[0.152.3\] - 2026-10-02

### Fixed

- Bandeau d’invalidation des indicateurs : un projet venu de la version
  de sens 1 est prevenu que la famille Risques change aussi (inversion
  v2, spec 048), et pas seulement Paysage, Dynamique temporelle et
  Energie.

## \[0.152.2\] - 2026-10-02

### Fixed

- Houppiers : chaque calcul consigne `metadata$houppiers` (statut,
  nombre, date, detail) ; une erreur de segmentation n’est plus
  confondue avec un resultat vide ; l’export Marculus signale en
  avertissement une couche houppiers absente (avec la raison) ou une
  desserte absente.

### Changed

- Export Marculus : le message de fin compte les contextes dates au 1er
  janvier de leur annee cible (annee de programme, a corriger dans
  Marculus au martelage).

## \[0.152.1\] - 2026-10-02

### Added

- Production du massif (spec 054 lot 5-bis, `nemeton >= 0.206.0`) :
  covariables FORMS-T (Theia, 10 m) et MNT du projet passees a
  `ifn_production_domaines()` pour la prevision hybride ; le panneau
  nomme la prevision (SER ou SER corrigee par FORMS-T) et signale
  `hors_calibrage` et le massif sans placette (`nature = "prediction"`).

### Fixed

- P2 en mode CHM ne sature plus a 100/100 (ecart n. 17,
  `nemeton >= 0.207.0`) : le statut `.p2_status` est passe a
  `normalize_indicator()` (plafond 40 m) ; le statut d’un calcul
  precedent est retire a chaque recalcul.

### Changed

- Plancher `Imports: nemeton (>= 0.207.0)`.

## \[0.152.0\] - 2026-10-02

### Added

- Production IFN par sylvoecoregion (spec 054, `nemeton >= 0.205.0`) :
  bloc « Production IFN » dans Sources & parametres pour basculer P2 sur
  la production de la SER (`source = "ifn_fh"`) et E1 en mode flux (part
  choisie ou taux IFN de la SER) ; localisation SER des UGF en cache ;
  P2 IFN et E1 flux sans CHM ; bandeau RSE / echelon / nature sous P2,
  avertissement « recolte observee » pour E1 ; panneau « Production du
  massif (IFN) » avec poids direct, part en bordure et ratios
  prelevement/production.

### Changed

- Plancher `Imports: nemeton (>= 0.205.0)`.

## \[0.151.4\] - 2026-09-25

### Added

- Plan d’actions : sous le graphique du bilan, un bloc « Fiches des UGF
  selectionnees » (actions triees par annee, resume du martelage,
  commentaire libre par UGF enregistre dans
  `data/action_plan_ug_comments.json`) et un bouton IA qui recopie le
  dernier conseil « Affiner le plan » dans les commentaires des UGF
  selectionnees.

## \[0.151.3\] - 2026-09-25

### Added

- Plan d’actions : un « i » explicatif sur Annee, Type et Priorite («
  Couche affichee »), qui dit comment la couleur d’une UGF a plusieurs
  actions est choisie.

### Changed

- Plan d’actions : la courbe du bilan cumule et les totaux passent sous
  le tableau des actions.
- reGeneration : le tableau des UGF adopte la presentation de celui du
  Plan d’actions (libelle UGF en premiere colonne, recherche en regex,
  nombre de lignes et pagination sous le tableau, 60 % de la hauteur),
  et s’intitule « Tableau des actions ».

### Removed

- reGeneration : la case « Masquer les UG mal couvertes » (toutes les UG
  sont affichees, la colonne Couverture reste) et la case « Bilan
  hydrique seul (rapide) » (l’analyse est toujours complete).

## \[0.151.2\] - 2026-09-25

### Changed

- Plan d’actions, « Carte + Tableau » : le choix de coloration (Annee,
  Type, Priorite) quitte l’en-tete de la carte pour une barre laterale
  droite « Couche affichee », toujours ouverte, comme la carte de
  reGeneration.

## \[0.151.1\] - 2026-09-25

### Fixed

- Import Marculus : une tige reimportee avec ses volumes (CSV au format
  3 apres un format 2, meme `uuid` et meme `modifie`) remplace desormais
  la version stockee sans volume. Il suffit de reimporter les fichiers
  pour que les tiges recuperent leurs volumes.

## \[0.151.0\] - 2026-09-25

### Added

- Kanban : un double-clic sur une fiche martelee ajoute, sous le
  formulaire d’edition, une section « Martelage ». Elle comprend :
  - un plan de situation de l’UGF et des tiges georeferencees ;
  - un diagramme debout, une barre par essence empilee par classe, aux
    couleurs PB/BM/GB/TGB de Marculus ;
  - le tableau des tiges designees, annulations deduites. Les categories
    suivent les seuils par defaut de Marculus, et le mode de mesure est
    recopie sur les tiges a l’import.

### Fixed

- Les widgets DT et leaflet places dans une fenetre restaient vides :
  `custom.js` relaie maintenant `shown.bs.modal` vers
  `shown.htmlwidgets`.

## \[0.150.0\] - 2026-09-25

Jalon mineur : les volumes du martelage reviennent de Marculus, qui
reste la seule source du calcul.

### Added

- Import Marculus : volumes unitaires par tige et totaux nets par
  contexte (`.marsync` et CSV `FormatCsv;3`). Les totaux font foi pour
  l’action : `volume_m3` (bois fort, qui alimente le bilan),
  `volume_martele_m3`, `volume_total_m3`, `surface_terriere_m2` et
  `nb_tiges_non_cubees`. Le volume net par case suit la regle
  d’annulation de Marculus. Les volumes s’affichent sur la fiche Kanban,
  dans la synthese (colonne Volume, tiges non cubees) et dans
  l’infobulle des tiges sur la carte. Le format 2, sans volumes, reste
  accepte.

## \[0.149.0\] - 2026-09-25

Jalon mineur : le retour Marculus est lisible dans le Plan d’actions.

### Changed

- Un contexte revenu avec des tiges designees fait passer son action a «
  Realisee » dans le Kanban.
- La fiche Kanban affiche, sous le commentaire, la date du martelage, le
  nombre de tiges designees et, parmi elles, les tiges « Biodiversite »
  (`quantite$nb_tiges_biodiversite`, net des annulations).
- La synthese d’import prend la forme d’une feuille (une ligne par
  essence, une colonne par classe, des totaux) dans une fenetre
  defilante. Le bilan de l’import s’y affiche, au lieu d’un toast qui
  masquait « Fermer ».

### Fixed

- Import Marculus : un fichier vide (0 octet) est signale comme vide, et
  non comme « illisible ». Le message d’erreur cite aussi le CSV au
  format 2, et les messages donnent le nom d’origine des fichiers.

## \[0.148.3\] - 2026-09-25

### Fixed

- Import CSV Marculus (`FormatCsv;2`, Marculus v0.48.0) : la colonne
  `QualiteFix` porte le libelle (« RTK fixe »), alors que le `.marsync`
  porte le nom de l’enum (`RTK_FIXE`). Le libelle est desormais ramene
  au nom, pour qu’une meme tige garde une seule forme quel que soit le
  fichier.

## \[0.148.2\] - 2026-09-25

### Changed

- Export Marculus : les contextes partent au diametre, avec des classes
  de 20 a 90 cm par pas de 5, les valeurs par defaut d’un contexte cree
  dans Marculus. Jusqu’ici, ils partaient en circonference, de 20 a 200
  cm.

## \[0.148.1\] - 2026-09-25

### Changed

- Export Marculus : chaque essence de la feuille de martelage porte sa
  couleur BD Foret V2, comme dans Marculus (`Referentiels.kt`). Les
  essences d’une meme famille recoivent des nuances de la couleur de
  famille, et le texte est blanc ou noir selon le meilleur contraste
  WCAG (au moins 4,5:1). Jusqu’ici, toutes les colonnes arrivaient en
  blanc sur noir.

## \[0.148.0\] - 2026-09-25

### Added

- « Importer de Marculus » accepte les CSV de contexte en `FormatCsv;2`
  (Marculus apres v0.47.0). L’action est retrouvee par `ContexteId` et
  les tiges sont unies par `Uuid` : un CSV et un `.marsync` du meme
  contexte ne se doublent pas. Le terrain fait foi. Un CSV de l’ancien
  format est refuse, avec un message dedie. La modale accepte
  `.marsync`, `.json` et `.csv`.

## \[0.147.1\] - 2026-09-25

### Changed

- Plan d’actions : le bouton d’import s’appelle « Importer de Marculus »
  (en : « Import from Marculus »), pendant de « Telecharger vers
  Marculus ».

## \[0.147.0\] - 2026-09-25

Jalon mineur : l’aller-retour avec Marculus est complet. Les chantiers
partent vers le telephone, avec leur fond ortho 20 cm (v0.146.x), et
leur martelage revient dans le Plan d’actions.

### Added

- Plan d’actions : bouton « Importer les donnees Marculus ». Il accepte
  les `.marsync` et la sauvegarde JSON, unit les tiges par `uuid` et
  apparie chaque contexte a son action par `id`. Le terrain fait foi :
  statut, `date_martelage`, `annee_cible` quand elle reste valide, et
  nombre de tiges net (PLUS - ANNULATION), le tout audite. S’y ajoutent
  une couche « Tiges martelees » sur la carte et une synthese par
  essence et par classe. L’export suivant reutilise la date revenue du
  terrain.

## \[0.146.2\] - 2026-09-24

### Fixed

- Marculus ne proposait jamais le fond « Ortho ». GDAL
  (`GoogleMapsCompatible`) declare des matrices de tuiles vides pour les
  zooms 0 a 12 ; la reprojection NGA de Marculus leve une
  `NullPointerException` sur une matrice vide. Ces matrices sont
  maintenant retirees a la construction du fond, et les fonds deja en
  cache sont repares a l’export. Reproduit puis verifie avec la
  bibliotheque NGA de bureau (`geopackage-core` 6.6.7).

## \[0.146.1\] - 2026-09-24

### Fixed

- « Telecharger vers Marculus » ne telechargeait rien en 0.146.0 : le
  `downloadButton` masque, que le serveur clique apres la preparation
  des fonds ortho, n’etait pas rendu par Shiny (sortie invisible
  suspendue : `href` vide). Il est desormais declare
  `suspendWhenHidden = FALSE` ; le correctif est verifie sous Chrome
  headless.
- Export Marculus : deux actions du meme type sur la meme UGF
  partageaient leur nom de contexte et leur fichier (20 contextes pour
  17 GeoPackages sur « Reconfort »). Les doublons prennent leur annee,
  puis un rang.

## \[0.146.0\] - 2026-09-24

### Added

- Export Marculus : chaque GeoPackage de chantier emporte le fond
  orthophoto IGN 20 cm (`HR.ORTHOIMAGERY.ORTHOPHOTOS`, zoom 19 Web
  Mercator avec sa pyramide), sur les parcelles de l’UGF + 50 m.
  Marculus propose alors OSM -\> Satellite -\> Ortho. Les fonds
  manquants sont prepares en tache de fond (`ExtendedTask`) au clic, mis
  en cache par UGF (`cache/layers/ortho_marculus/`), puis le
  telechargement se declenche seul. « Reconfort » : 16 fonds en 239 s la
  premiere fois, export 13 s, zip 111 Mo.

### Fixed

- Export Marculus : `dateMartelage` etait au 1er janvier de l’an 1 a 13,
  car `annee_cible` (un decalage) etait lu comme une annee civile. Les
  contextes portent maintenant l’annee reelle.

## \[0.145.0\] - 2026-09-24

Jalon mineur qui clot le lot « Plan d’actions + Marculus » des 0.144.1 a
0.144.2 : suppression des actions selectionnees, houppiers de nouveau
produits et exportes (cœur 0.199.2), couche RECONFORT « Probabilite
d’atteinte », et export Marculus 14 fois plus rapide.

### Fixed

- « Telecharger vers Marculus » : 133 s -\> 9,4 s sur « Reconfort »,
  pour un contenu identique. Repartition des houppiers par chantier en
  une passe GEOS plane (81 s -\> 2,3 s), GeoPackages temporaires ecrits
  sans synchronisation SQLite (31,2 s -\> 1,4 s), `zip -6` au lieu de
  `-9` (6,5 s -\> 1,7 s).

## \[0.144.2\] - 2026-09-23

### Changed

- Plancher `Imports: nemeton (>= 0.199.2)`.
- Houppiers : l’app appelle de nouveau
  `segment_houppiers(chm, aoi = emprise)` dans le processus, puisque le
  cœur 0.199.2 passe une copie `stars` a lidR. Le contournement de la
  0.144.1 est retire (processus `callr` neuf et filtrage d’emprise cote
  app). « Reconfort » : 80 982 houppiers en 54 s (185 s avant).
- RECONFORT : la couche de probabilite devient « Probabilite d’atteinte
  » (P(deperissant) + P(tres deperissant), 0-1000), avec son infobulle.
  Les avis `[minmax]` disparaissent (cœur 0.199.0).

## \[0.144.1\] - 2026-09-23

### Added

- Plan d’actions : bouton « Supprimer la selection » sous « Ajouter une
  action » (`btn-outline-danger`), avec une modale de confirmation
  (`btn-danger`) qui liste les actions visees. Chaque suppression est
  tracee dans l’audit, via `delete_actions_from_plan()`, et la garde de
  lecture seule s’applique a l’ouverture ET a la confirmation.

### Fixed

- Export Marculus : la couche `houppier` manquait depuis fin aout, car
  la segmentation echouait a chaque calcul. L’emprise est maintenant
  reparee (`st_make_valid()` en 2154 avant l’union) et
  `segment_houppiers()` est appele sans emprise dans un processus R neuf
  (`callr`). L’emprise est appliquee ensuite, cote app, par selection. «
  Reconfort » : 85 300 houppiers, et les 11 GeoPackages portent
  `houppier`. Il faut recalculer les indicateurs pour regenerer le
  cache.

## \[0.144.0\] - 2026-09-23

Jalon mineur qui regroupe le lot « Suivi sanitaire et CI » des 0.143.29
a 0.143.31 : plus de recalcul au retour sur l’onglet, stack d’indice de
la Carte FAST en cache disque (cœur 0.198.0), CI entierement verte
(`tests`, `R-CMD-check` et `coverage`) pour la premiere fois depuis le
2026-09-14.

### Fixed

- Sauvegarde en base : la note S4 « methode avec la signature
  ‘DBIObject#sf’ choisie pour dbDataType » ne s’affiche plus en console.
  `sf` et `RPostgres` definissent tous deux cette methode ; le choix est
  correct. Le
  [`sf::st_write()`](https://r-spatial.github.io/sf/reference/st_write.html)
  de `db_save_parcels()` est entoure de
  [`suppressMessages()`](https://rdrr.io/r/base/message.html).

## \[0.143.31\] - 2026-09-23

### Changed

- Carte FAST : `build_index_stack(..., cache_result = TRUE)` (cœur
  0.198.0), cache sous `<projet>/cache/layers/index_stack`. Mesure
  `armn` (327 scenes) : 39,5 s au premier appel, 0,22 s ensuite,
  resultat identique. `parallel` reste a `FALSE`.
- Commentaire de `.compute_fast_mask()` aligne sur les masques FAST
  nommes par contenu (cœur 0.198.0).
- Plancher `Imports: nemeton (>= 0.198.0)`.

### Fixed

- CI `coverage` : sous covr, `../../R` est le `R/` du paquet installe
  (`.rdb` seul) ; `test-app_ui` et `test-service_python` sautent
  desormais quand aucune source `.R` n’est trouvee, au lieu de tester
  l’existence du dossier.

## \[0.143.30\] - 2026-09-23

### Fixed

- CI `R-CMD-check` rouge depuis `0fc7a5e1` (2026-09-14) : cinq tests
  lisaient les sources `R/*.R`, absentes du paquet installe sous
  `R CMD check` (`test-mod_ug`, `test-service_pipeline`,
  `test-service_tour`, `test-app_ui`, `test-service_python`). Garde
  `skip_if_not(file.exists(...))` du repo.
- NEWS 0.143.29 : le rouge CI datait du 14 septembre, pas du plancher
  cœur 0.197.0.

### Changed

- Deux caracteres non-ASCII hors commentaire (`R/mod_home.R`,
  `R/mod_pipeline.R`) passent en `\uXXXX`.

## \[0.143.29\] - 2026-09-23

### Fixed

- Suivi sanitaire : revenir sur l’onglet relancait les deux calculs
  (Carte FAST ~9 s bloquantes via `build_index_stack()`, alertes FAST +
  toast + repeint). `mod_monitoring_fast_alerts` et
  `mod_monitoring_pixel_map` memorisent la signature des entrees du
  dernier calcul reussi ; meme signature = aucun recalcul, un echec est
  retente.
- Plan d’actions : la Carte des actions montrait le monde entier apres
  un changement de projet fait depuis un autre onglet (carte masquee,
  dimensions nulles). Le cadrage est differe puis applique apres
  `invalidateSize` a l’arrivee sur l’onglet.
- CI `R-CMD-check` : sept tests de `normalize_indicator` verifiaient
  encore les anciennes bornes E1/E2, alignees sur 1,32 par le cœur
  v0.197.0.

### Changed

- Commentaire de `.compute_fast_mask()` corrige : le masque 0-4 est
  reclasse et reecrit par le cœur a chaque appel, seul le raster continu
  est en cache.

## \[0.143.28\] - 2026-09-19

### Fixed

- Tour guide : l’etape « Plan d’action » tremblait sans fin. La sidebar
  ancree fait 793 px de haut dans une fenetre de 900 : driver.js, faute
  de place, poussait son popover hors de l’ecran, la page oscillait
  entre avec et sans barre de defilement, et chaque bascule reveillait
  le `ResizeObserver` de bslib qui redispatchait un `resize` que
  driver.js ecoutait pour se recadrer (326 evenements en 6,4 s). L’etape
  est desormais ancree sur la carte « Tableau des actions » avec
  `position = "left"` : 0 evenement, geometrie stable, popover
  entierement visible.

### Changed

- `action_table_card()` accepte un `card_id` optionnel, pour ancrer la
  carte entiere (en-tete compris) plutot que son seul corps repliable.

## \[0.143.27\] - 2026-09-19

### Fixed

- Tour guide : les etapes Synthese et Famille etaient proposees alors
  que l’app renvoie sur l’Accueil toute navigation vers ces onglets tant
  que le projet n’est pas `completed`. Le tour declenchait donc son
  propre renvoi - l’onglet s’affichait puis sautait. Le predicat de
  restriction (`.tab_requires_completed_project`) est desormais partage
  entre la garde de navigation d’`app_server` et le filtre des etapes.
- Tour guide : une ancre rendue cote serveur mesure 0 de haut a
  l’instant du cadrage (un `uiOutput` porte par un onglet masque est
  suspendu par Shiny), `canHighlight()` est faux et driver.js saute
  l’etape en silence sans toucher au popover. Ancres desormais statiques
  : `synthesis-summary_card` et `famille_carbone-family_header`.

## \[0.143.26\] - 2026-09-19

### Fixed

- Tour guide : le clic synthetique de bascule d’onglet remontait jusqu’a
  `window`, ou driver.js l’interpretait comme un clic hors popover et
  fermait le tour (`reset()`). L’etape suivante s’affichait encore, mais
  `isActivated` etait FALSE : ni « Suivant », ni « Fermer », ni le
  clavier ne repondaient, et la re-mesure `resize` etait inoperante. Le
  clic est etouffe au niveau de `document`, apres le handler delegue de
  Bootstrap.
- Tour guide : l’etape « Recherche » etait ancree sur
  `home-search_collapse` (le corps repliable seul), laissant le titre «
  Rechercher une commune… » sous le voile sombre. Nouvelle ancre
  `home-search_card`, la carte entiere.

## \[0.143.25\] - 2026-09-18

### Changed

- Plancher `Imports: nemeton (>= 0.197.0)`. `INDICATOR_SENSE_VERSION`
  passe a 3 : L1 s’inverse, T1 recoit une borne de 200 ans, E1/E2
  s’alignent sur P1 (spec 048 §9-§11). Les indicateurs calcules avant
  sont invalides a la premiere ouverture — un parquet perime reste
  LISIBLE, donc `compute_all_indicators()` le relirait et sauterait le
  recalcul.
- `load_project()` porte `indicators_invalidated` et un bandeau nomme
  les trois familles qui changent : l’utilisateur voyait son projet
  repasser en brouillon sans explication.

### Fixed

- Le tour guide cadrait a cote. Cliquer ACTIVE un onglet mais ne le
  MESURE pas : un `.tab-pane` masque est en `display: none`, dimensions
  nulles, plus une transition `.fade`. On attend `shown.bs.tab` puis on
  force une re-mesure via `resize`, que driver.js ecoute deja. Le
  demarrage, lui, reposait sur un `setTimeout` aveugle apres un
  `collapse('show')` anime : il suit desormais `shown.bs.collapse`.
- La carte cadastrale ne se recadrait pas au retour de l’onglet UGF :
  `input$main_tabs` n’etait observe nulle part. `invalidateSize()` seul
  restaure la taille, pas la vue — il faut recadrer derriere.
- `cli_alert_warning()` concatenait les puces d’un vecteur ; seul
  `cli_warn()` les rend sur des lignes distinctes.

## \[0.143.24\] - 2026-09-18

### Added

- `R/service_python.R` : registre `engine_python()` moteur -\>
  interpreteur et runner isole `run_with_python()`. `reticulate` lie un
  interpreteur une fois par processus et les quatre stacks Python de
  l’app ont des exigences contradictoires (opencanopy veut
  `RETICULATE_PYTHON` epinglee, FORDEAD la veut absente) : la regle « un
  moteur = un processus » est desormais ecrite, outillee et gelee par un
  test qui nomme tout fichier ajoutant une liaison en processus.
- Le verdict `chm_suspect` du cœur (\>= 0.191.1) est lu, persiste dans
  les metadonnees et affiche en bandeau dans la Synthese. Il ne mord que
  sans couverture LiDAR : avec du LiDAR,
  `resolve_project_chm(validate=)` ecarte deja l’ortho plate en amont.

## \[0.143.23\] - 2026-09-17

### Changed

- Plancher `Imports: nemeton (>= 0.196.0)`. La garde
  [`formals()`](https://rdrr.io/r/base/formals.html) posee en 0.143.21 —
  qui n’envoyait `cancel_path` que si le coeur installe l’acceptait, la
  v0.196.0 n’etant alors pas releasee — est retiree avec son
  commentaire. L’argument est passe directement, comme pour FAST et
  FORDEAD.
- L’arret cooperatif RECONFORT devient **effectif** : jusqu’ici
  l’argument etait omis en silence sur un poste en 0.195.0, donc le
  bouton « Arreter » ecrivait son flag sans que personne ne le lise.

## \[0.143.22\] - 2026-09-17

### Fixed

- Un moteur Sante **annule** n’est plus enregistre « ok » dans la
  chaine. Le coeur rend `status = "cancelled"` sans lever, donc
  l’`ExtendedTask` est en `"success"` : le rapport final annoncait une
  reussite la ou rien n’avait ete produit. Defaut preexistant sur FAST
  et FORDEAD, etendu a RECONFORT par le cablage de `cancel_path` en
  0.143.21 ; corrige pour les trois.

### Added

- Les trois rejets de `pipeline_record()` avertissent au lieu d’etre
  muets, en nommant l’etape fautive ET le curseur attendu. Le plus
  traitre — « repond hors de son tour » — enregistre le resultat sans
  avancer le curseur : la chaine parait progresser alors qu’elle est
  bloquee. Le chemin nominal reste silencieux.
- `data/pipeline_state.json` : l’etat de la chaine est ecrit a chaque
  transition (etape courante, index, statut et horodatages de chaque
  etape). L’etat ne vivait qu’en memoire, et un run bloque ne laissait
  aucune trace. Ecriture best-effort, lecture tolerante a un fichier
  corrompu. Ce n’est pas un format de reprise.

## \[0.143.21\] - 2026-09-14

### Added

- Arret cooperatif RECONFORT : le bouton « Arreter » ecrit
  `reconfort_cancel.flag`, scrute par
  [`nemeton::run_reconfort_dieback()`](https://pobsteta.github.io/nemeton/reference/run_reconfort_dieback.html)
  AUX FRONTIERES DE PHASE (IOTA2 decoupe cote Python, pas de point
  d’arret plus fin). Le run sort avec `status = "cancelled"`, workdir
  conserve. Les trois moteurs Sante s’arretent desormais de la meme
  facon.
- Deux cles i18n : « arret demande » au clic (le worker termine son
  etape) et « arrete » a l’arrivee du resultat — deux moments distincts.

### Fixed

- Un run RECONFORT annule serait passe pour un succes : le handler de
  resultat ne testait pas `result$status`, donc `n_alerts = NA` dans un
  `sprintf` et un `$rasters` NULL passe au sous-module carte.
- Le chronometre du toast de succes RECONFORT affichait `0` depuis
  toujours : le handler lisait `duration_sec` (nom FORDEAD) la ou le
  coeur rend `elapsed_sec`.

### Note

`nemeton` 0.196.0 n’etant pas encore releasee, `cancel_path` n’est
transmis que si le coeur installe l’accepte (garde
[`formals()`](https://rdrr.io/r/base/formals.html)), et le plancher
reste `(>= 0.195.0)`. Garde et plancher a reprendre des la release cœur.

## \[0.143.20\] - 2026-09-14

### Removed

- `app_ui.R` portait une copie « Placeholder » de `mod_home_ui`, masquee
  par celle de `mod_home.R` (collation alphabetique : le dernier charge
  gagne). Elle n’atteignait jamais l’ecran tout en restant lisible comme
  la vraie — avec ses propres boutons « OSM » / « Satellite », qui
  avaient survecu au passage au LayersControl de la 0.143.19 faute
  d’etre visibles. 114 lignes.

### Added

- Test verrouillant l’unicite de definition des trois `mod_*_ui`
  d’`app_ui.R` et leur construction. `mod_synthesis_ui` et
  `mod_family_ui` portent le meme titre « Placeholder » mais sont les
  implementations VIVANTES : le titre roxygen ne dit rien de l’etat reel
  d’une fonction, seule la collation le dit.

## \[0.143.19\] - 2026-09-14

### Fixed

- Le bandeau « Tuile Sentinel-2 (n/N) » de `mod_monitoring` survivait a
  « Arreter les calculs » et restait inclosable (`duration = NULL` +
  `closeButton = FALSE`). Il n’etait pas oublie mais RECREE chaque
  seconde par l’observer du chrono : seul `fast_run_start(NULL)` en
  coupe la source.

### Changed

- `app_state$cancel_computation` devient LE signal d’arret de l’app. Les
  trois handlers d’annulation de `mod_monitoring` (FAST, FORDEAD,
  RECONFORT) sont extraits en helpers, appeles par leur bouton ET par ce
  signal ; les trois boutons le posent desormais, donc arreter
  l’ingestion arrete aussi la chaine.
- Carte cadastrale (`mod_map`) et Carte UGF (`mod_ug`) : le choix du
  fond passe des deux boutons d’entete au `LayersControl` natif de
  Leaflet, comme dans toutes les autres cartes de l’app.
- Etapes Sante de la chaine « Tout calculer » : « Sante — surveillance
  rapide » -\> « Sante — FAST », « Sante — diagnostic FORDEAD » -\> «
  Sante — FORDEAD ».

### Removed

- `initBasemapToggle()` et le handler `toggleBasemapButtons`
  (`custom.js`), les regles `.basemap-btn` / `.basemap-btn-active`
  (`custom.css`), et `rv$basemap` dans les deux modules carte :
  orphelins apres le passage au LayersControl. `rv$basemap` n’etait de
  toute facon jamais lu, seulement ecrit.

## \[0.143.18\] - 2026-09-14

### Fixed

- Modele Mistral par defaut hors palier : `mistral-large-latest` -\>
  `mistral-medium-latest`. Le modele existe toujours cote API mais est
  ferme aux paliers d’entree, ce qui rendait l’analyse IA inutilisable
  (`HTTP 403 ... not available in your subscription tier`).
- Modele Anthropic par defaut : `claude-sonnet-4-5-20250929` -\>
  `claude-opus-5` (generation precedente, suffixe de date obsolete).

### Added

- Repli automatique de modele sur refus de palier ou de quota (403/429)
  : bascule sur `ministral-14b-latest`, puis 8b, puis 3b, et retour au
  modele configure des que le palier le redonne. Toute autre erreur
  remonte inchangee ; en cas d’echec total c’est l’erreur d’origine qui
  remonte ; un repli reussi notifie l’utilisateur (`ia_modele_repli`).

## \[0.143.17\] - 2026-09-04

### Changed

- **Les actions de projet passent dans le bloc « Tableau des actions »**
  (onglet Selection) : « Voir les resultats », « Reessayer » et « Lancer
  le calcul » flottaient au-dessus du bloc, elles sont maintenant
  dedans, au-dessus de « Tout calculer », et suivent son repli.
  `mod_pipeline_ui()` gagne un argument `actions_ui = NULL`
  retro-compatible.
- **« Tout calculer » passe de `btn-primary` a `btn-outline-primary`** :
  le regroupement mettait deux verts cote a cote, et ce bouton n’est pas
  le CTA du lancement — il ouvre la modale, dont le « Lancer la chaine »
  reste vert plein.

### Fixed

- **Deux tests du chronometre tombaient sous charge** : `.fmt_elapsed()`
  tronque, et les tests attendaient la seconde exacte dans une fenetre
  d’une seconde. L’horloge devient injectable (`now = Sys.time()`), le
  formatage exact est teste sur horloge figee, le rendu ne verifie plus
  que le cablage.

## \[0.143.16\] - 2026-09-03

### Added

- **Log de l’enfant plafonne sur les quatre chemins concernes**
  (FORDEAD, RECONFORT, calcul des indicateurs, moteur de reGeneration) :
  `log_path` passe a
  [`nemeton::run_memory_capped()`](https://pobsteta.github.io/nemeton/reference/run_memory_capped.html)
  (cœur \>= 0.195.0), fichier `data/<pipeline>_child.log` a nom stable,
  rotation au demarrage (`.prev-<horodatage>`, cinq conservees). Sans
  lui, la sortie de l’enfant partait dans le `/dev/null` du worker
  `future`.

### Changed

- Plancher `Imports: nemeton (>= 0.193.0)` -\> `(>= 0.195.0)`, apres
  publication de la release cœur `v0.195.0` et verification par
  [`pak::pkg_deps()`](https://pak.r-lib.org/reference/pkg_deps.html). Un
  premier bump anticipe, alors que `0.195.0` n’existait que sur `main`,
  avait rendu l’app non-installable (`@*release` ne resout que les
  tags).

- `.prune_failed_traces()` -\> `.prune_run_traces()`, avec un argument
  `motif` pour borner les deux familles de traces (`.failed-*` et
  `.prev-*`).

- `specs/REPONSE-nemeton-053-trace-enfant-plafonnee.md` : retour au cœur
  sur sa réponse au brief (deux corrections factuelles, la métrique du
  pic, et la fragilité `tiles_envelopes` vue des deux côtés).

## \[0.143.15\] - 2026-09-03

### Fixed

- **La trace d’un run en échec n’est plus effacée.** Les chemins
  d’erreur de FAST, FORDEAD et RECONFORT archivent le JSON et le NDJSON
  de progression en `<fichier>.failed-<horodatage>` au lieu de les
  supprimer (cinq archives gardées par fichier de base). Succès et
  annulation inchangés.

### Added

- `specs/BRIEF-nemeton-trace-enfant-plafonne.md` — brief cœur :
  rediriger `stdout`/`stderr` de l’enfant plafonné vers un fichier (ils
  partent dans le `/dev/null` du worker `future`), et investiguer
  l’arrêt d’IOTA² après `classification` sans production de `final/`.

## \[0.143.14\] - 2026-09-02

### Changed

- **Section « Tout calculer » (sidebar Sélection)** : l’entête du bloc
  s’appelle désormais **« Tableau des actions »** et ne répète plus le
  libellé du bouton qu’il contient (nouvelle clé i18n
  `pipeline_section_title`, FR/EN).
- **Le bouton « Tout calculer » se grise pendant toute la chaîne** et
  redevient cliquable à la clôture (fin naturelle *et* arrêt manuel).
- **Toast « Tous les calculs en cours… » en bas à droite** pendant la
  chaîne, retiré à la clôture (nouvelle clé i18n
  `pipeline_running_toast`, FR/EN).

### Fixed

- **Double lancement de la chaîne impossible** : garde serveur sur
  `input$open` en plus du grisage. Un second `pipeline_new_run()`
  écrasait le run en cours, dont les réponses étaient ensuite rejetées
  sur le `run_id`.

## \[0.143.13\] - 2026-09-02

### Fixed

- **Le moteur de reGénération et le gel R7 tournaient sur les années par
  défaut (2018 / 2022) sans le signaler** quand l’étape E-OBS était
  sautée — cas du projet Lajoux. `annees_pipeline()` n’est rempli qu’en
  cas de succès E-OBS ; le repli lisait les champs du formulaire, et le
  rapport affichait les deux étapes en vert. Elles nomment désormais les
  années utilisées et signalent le repli.

## \[0.143.12\] - 2026-09-02

### Fixed

- **La garde anti-recréation des zones se sabotait à son premier
  usage.** `.zones_a_jour()` exigeait le fichier de clé, écrit seulement
  *après* un enregistrement : au premier run il n’existait pas et la
  garde déclarait périmées des zones valides. Les zones existantes sans
  clé sont désormais adoptées (la clé est amorcée, rien n’est recréé).

## \[0.143.11\] - 2026-09-01

### Fixed

- **Les zones de suivi étaient recréées à chaque lancement de la
  chaîne.** `build_project_monitoring_zones(replace = TRUE)` supprime
  puis réinsère : les identifiants changent, et tout ce qui est indexé
  dessus devient orphelin (`output_zone_<id>/`). Mesuré sur Couchey :
  6,3 Go sous des zones qui n’existent plus, et les marqueurs de reprise
  de RECONFORT laissés derrière, ce qui faisait tout re-télécharger.
- **Le contrôle d’intégrité de la desserte était rejoué en entier**,
  réseau inchangé — 51 min sur Couchey. Le résultat était sur le disque,
  mais son lecteur ne prenait aucune clé de fraîcheur.

## \[0.143.10\] - 2026-09-01

### Fixed

- **RECONFORT emportait la session entière.** `systemd-oomd` a tué le
  scope RStudio (9 processus) pendant l’item 82/203 de l’ingestion —
  pression à 56,87 % sur `user@1000.service`, scope à 14,5 Go, aucun
  événement d’erreur. Le cœur ne plafonnait que le sous-processus Python
  ; la boucle d’ingestion des 203 scènes est du R pur et le run mourait
  avant d’atteindre Python. RECONFORT passe en enfant plafonné
  ([`nemeton::run_memory_capped()`](https://pobsteta.github.io/nemeton/reference/run_memory_capped.html)),
  comme FORDEAD.

### Changed

- `.build_reconfort_ntfy_callback()` isole la moitié « push » du
  callback composite : sous isolation, l’enfant écrit déjà les fichiers
  de progression, et rejouer le composite dupliquerait chaque ligne
  NDJSON.

## \[0.143.9\] - 2026-09-01

### Fixed

- **« objet ‘con’ introuvable » sur les moteurs Santé.** Les handlers
  `on.exit` s’exécutent dans l’ordre d’enregistrement ;
  `.release_worker_memory()`, enregistré en tête de corps, efface `con`
  de la frame du worker avant que la fermeture de connexion ne
  l’utilise. Les trois fermetures passent devant (`after = FALSE`).
  L’échec survenait après le travail utile, d’où sa discrétion.
- Même défaut, silencieux, sur `close(.ws_log_conn)` : enveloppé d’un
  `tryCatch`, il échouait sans rien dire et la connexion de log n’était
  jamais fermée.

## \[0.143.8\] - 2026-09-01

### Fixed

- **Le rapport de la chaîne nommait l’erreur, pas l’endroit.**
  `pipeline_task_error()` ajoute
  [`conditionCall()`](https://rdrr.io/r/base/conditions.html), qui nomme
  l’appel fautif et traverse la frontière `future` (vérifié de bout en
  bout sur un vrai worker). Sur le run Couchey du 2026-08-31, « objet
  ‘con’ introuvable » n’était pas localisable : une recherche statique
  dans les deux paquets n’a rien donné.

## \[0.143.7\] - 2026-08-31

### Changed

- **Le repli CHM en dur rendu au cœur.** `.project_chm()` ne sonde plus
  `cache/layers/opencanopy/` : il passe `.chm_exploitable()` à
  `nemeton::resolve_project_chm(validate =)` (cœur \>= 0.193.0), qui
  saute un candidat refusé et continue à la source suivante. Le repli
  inter-sources balaie désormais les six candidats du cœur au lieu de
  trois noms écrits à la main, dont un inexistant.
- Plancher `Imports: nemeton (>= 0.192.0)` -\> `(>= 0.193.0)`.

### Fixed

- `.chm_exploitable()` pouvait lever : le `tryCatch` ne couvrait que
  `spatSample()`, qui peut aussi *rendre* un `data.frame` sans colonne,
  sur lequel `v[[1]]` levait. Passé en `validate`, une erreur y arrêtait
  la résolution du cœur au lieu de passer au candidat suivant. Le
  prédicat est total.

## \[0.143.6\] - 2026-08-31

### Fixed

- **Le rapport de la chaîne affichait « Erreur » sans la cause.**
  `task$result()` re-lève l’erreur du worker — seul endroit où son
  message subsiste — et il était jeté au profit d’un libellé générique.
  Sur des moteurs tournant 13 h 40 (ingest FAST) et 4 h 10 (FORDEAD),
  c’était la seule information exploitable sans tout relancer.
  `pipeline_task_error()` l’extrait sur les sept réponses concernées,
  gère les échecs rendus par valeur
  (`list(status = "error", reason =, detail =)`) et tronque les
  tracebacks Python à 300 caractères.

## \[0.143.5\] - 2026-08-30

### Fixed

- **Le typage de la desserte s’affichait en rouge sur le meilleur
  résultat possible.** Sur Couchey, le réseau existant (17 056 tronçons)
  desservait déjà les 76 UGF : zéro route nouvelle à créer, coût 0.
  [`foretaccess::vectoriser_reseau()`](https://pobsteta.github.io/foretaccess/reference/vectoriser_reseau.html)
  travaille sur les routes nouvelles et abandonne quand il n’y en a
  aucune. Ce cas est distingué avant l’appel, par un statut propre :
  information, et étape *Sautée* plutôt qu’*Échec*.
- **microclimf annonçait « structure de végétation manquante » alors que
  la grille LiDAR HD était absente.** Trois causes, trois messages :
  grille absente, clé CDS absente, ou les deux. Le repli LAI Sentinel-2
  ne vit qu’à l’intérieur du bloc grille et n’était jamais atteint.
- Deux fixtures de test construisaient un `foretaccess_reseau` sans
  champ `lignes`, décrivant sans le vouloir ce cas limite.

## \[0.143.4\] - 2026-08-30

### Changed

- La section « Tout calculer » de la sidebar *Sélection* est repliable,
  avec le même mécanisme Bootstrap que les blocs voisins (en-tête
  cliquable, chevron, `data-bs-toggle="collapse"`), dépliée par défaut.
  Le panneau de progression listant dix-sept étapes repoussait sinon le
  reste de la sidebar hors écran.

## \[0.143.3\] - 2026-08-30

### Fixed

- **Les trois moteurs Santé se sautaient alors que les zones existaient
  en base.** La garde interrogeait `input$zone_id`, alimenté par
  `updateSelectInput()` — pas encore remonté du client juste après
  l’étape de création. Résolution désormais en base via
  `fordead_zone_id()`, zone transmise aux moteurs. Troisième occurrence
  de ce piège dans la chaîne.
- **Le rapport masquait la cause réelle des échecs IA** :
  `raison <<- ...` était placé dans le bloc d’un `tryCatch`, qui
  s’évalue dans le frame appelant — le `<<-` fuyait vers le namespace et
  la variable restait `NULL`. L’échec de l’appel Mistral était ainsi
  rapporté « prérequis manquant ». Le message d’erreur du LLM remonte
  maintenant au rapport.

## \[0.143.2\] - 2026-08-29

### Added

- Étape **« Santé — création des zones de suivi »** dans la chaîne,
  avant les trois moteurs Santé qui exigent un `zone_id`. Même chemin
  que le bouton d’enregistrement de l’onglet ; upsert, donc relançable.
  La chaîne compte 17 étapes.

### Fixed

- **La perspective IA était rapportée « Réussie » sans rien avoir
  généré** : l’appel LLM échouait, `tryCatch` rendait `NULL`, et la
  fonction continuait jusqu’à `invisible(TRUE)`. Constaté sur Couchey —
  étape verte en 1 s pour 13 appels LLM. Une synthèse vide fait
  désormais échouer l’étape. Expliquait aussi le plan d’actions sauté
  juste après.
- Les 12 commentaires de famille n’étaient pas générés par la chaîne :
  le switch « toutes les familles » est décoché par défaut et la chaîne
  le suivait, malgré un libellé d’étape annonçant « synthèse + 12
  familles ».
- Les messages de saut nommaient un « prérequis manquant » sans le
  désigner. Les moteurs Santé citent la zone de suivi absente ; l’IA
  remonte sa raison réelle.

## \[0.143.1\] - 2026-08-29

### Fixed

- **La chaîne « Tout calculer » restait bloquée sur « Indicateurs / En
  cours »** alors que le calcul s’était terminé. La réponse au pipeline
  était posée depuis `poll_fn`, un callback
  [`later::later()`](https://later.r-lib.org/reference/later.html) qui
  s’exécute hors contexte réactif : la lecture du `reactiveVal` y lève
  `Operation not allowed without an active reactive context`, l’erreur
  remonte et la réponse n’est jamais posée.
- Dans les six autres modules, les lectures de la mémoire de requête
  abonnaient l’observer de statut à celle-ci : poser la requête le
  redéclenchait et, sur un statut `success` hérité d’un run précédent,
  il répondait avant que le moteur n’ait redémarré — l’étape aurait été
  rapportée réussie sans avoir tourné. Toutes les lectures sont isolées.

## \[0.143.0\] - 2026-08-28

### Added

- **« Tout calculer »** : un bouton dans la sidebar *Sélection* enchaîne
  les seize calculs de l’application puis les générations IA. Modale de
  lancement (périmètre cochable + profil de l’analyste parmi les quinze
  profils experts, appliqué à toutes les générations IA), panneau de
  progression avec arrêt, et rapport final par étape avec durées. Une
  étape en échec n’interrompt pas la chaîne ; le rapport distingue
  réussie / échec / sautée / annulée.
- `R/service_pipeline.R` : registre ordonné des étapes et machine à
  états, sans Shiny (74 tests). `R/mod_pipeline.R` : bouton, modale,
  progression, rapport.
- Protocole orchestrateur ↔︎ modules : `app_state$pipeline_request` /
  `app_state$pipeline_answer`. L’orchestrateur ne lance aucun moteur
  lui-même — chaque module exécute le sien avec ses propres gardes et
  les inputs de son onglet.
- ~50 clés i18n FR/EN.

### Changed

- Le corps de chaque observer de bouton de lancement est extrait en
  fonction locale, appelée par le bouton **et** par la chaîne : aucun
  bloc dupliqué entre les deux chemins.
- `.lancer_moteur_regen()`, `.lancer_gel()` acceptent des années
  explicites ; `.lancer_accessibilite()` un `use_corrected_force` ;
  `.generer_ia_synthese()` un profil. Ces valeurs transitent d’ordinaire
  par `updateNumericInput()` / `renderUI()`, qui ne remontent au serveur
  qu’après un aller-retour client : l’étape suivante de la chaîne y
  lirait la valeur précédente. `NULL` conserve le comportement des
  boutons manuels.

### Fixed

- Les gardes internes des modules (pas de zone monitoring, pas de clé
  API, lecture seule) faisaient sortir la fonction de lancement sans
  démarrer de tâche : personne ne répondait et la chaîne se serait
  bloquée en silence sur cette étape. Les branchements testent désormais
  la valeur de retour du lancement et répondent « sautée ». Les deux
  générations IA rendaient `TRUE` en dur — une perspective jamais
  générée aurait été rapportée « réussie ».

## \[0.142.3\] - 2026-08-28

### Fixed

- **La carte UGF restait vide au premier passage sur son sous-onglet** :
  passer de « Carte cadastrale » à « Carte UGF » n’affichait ni
  tènements ni UGF, il fallait aller sur « Tableau UGF » et revenir.
  `output$ug_map` était la seule des six cartes leaflet de l’app à
  rester suspendue quand son onglet est caché ; « Carte UGF » étant un
  sous-onglet non-défaut, la carte n’existait pas encore côté client
  quand l’observer de dessin émettait ses `leafletProxy()`, et leaflet
  jette silencieusement les messages adressés à une carte absente du
  DOM. `mod_ug` posait déjà l’option sur son tableau et ses compteurs,
  mais avait oublié sa carte.

### Changed

- Brief cœur `specs/BRIEF-nemeton-resolve-chm-opencanopy.md` :
  `resolve_project_chm()` sonde `cache/layers/chm/` alors que
  `download_chm_opencanopy()` écrit dans `cache/layers/opencanopy/`. Le
  brief signale deux pièges (un candidat sans `file` mosaïquerait
  orthophotos et indices spectraux en VRT ; les entrées doivent venir
  après `"LiDAR HD MNH"`) et porte l’entrée `PLAN.md` de la v0.142.2,
  que la session app ne peut pas écrire côté cœur (règle 12).

## \[0.142.2\] - 2026-08-28

### Fixed

- **Chargement d’un projet récent : 13,2 s → 5,1 s** (médiane de 3
  mesures Chrome piloté, Couchey / 75 UGF / 223 tènements). Trois causes
  cumulées :
  - `ug_build_sf()` était appelée par **sept reactives** dans le même
    flush (`ug_sf_4326`, `units_sf` ×4, `ugf_sf_r`, rendu carte
    `mod_ug`), leurs sorties portant `suspendWhenHidden = FALSE`. Elle
    est mémoïsée, avec pour clé le hash du couple `(ugs, tenements)` —
    toute mutation du domaine invalide l’entrée d’elle-même. Hash : 0,4
    ms contre 2950 ms de reconstruction.
  - La dissolution faisait un `st_make_valid()` **par UGF** (75 appels,
    695 ms) au lieu d’un seul sur les 223 tènements (97 ms). Nouveau
    `.ug_geometries()` ; `ug_build_sf()` passe de 2988 ms à ~950 ms, à
    résultat géométriquement identique (75/75 `st_equals`, différence
    symétrique nulle).
  - `mod_ug` dessinait sa carte via `leafletProxy()` **onglet fermé**,
    travail que leaflet jette et que le module redessinait déjà à
    l’ouverture : 2 × 370 ms retirés du chemin critique.
- Le fallback planaire s2 n’écrit plus dans la console R. Sur Couchey,
  une seule UGF sur 75 a des sommets auto-tangents que s2 refuse de
  dissoudre ; la bascule vers GEOS est délibérée et sans effet sur le
  résultat, mais `sf_use_s2()` émettait un
  [`message()`](https://rdrr.io/r/base/message.html) à chaque changement
  d’état (règle stricte 9).
- **« Générer les placettes » échouait** avec « Stratification-valid
  candidate pool (0) is below `n_base` — 2108 of 2108 candidates fell on
  NA pixels ». Le message accusait la couverture des rasters ; la cause
  était une unité. `prep_sampling_raster()` comparait la résolution d’un
  MNT en EPSG:4326 (0,00025 **degré**) à `target_res_m = 5` **mètres**,
  d’où un facteur d’agrégation de 20 003 : le MNT sortait en **une seule
  cellule**. Le raster est désormais aligné sur le CRS métrique de la
  zone avant tout raisonnement en mètres — ce dont le cœur a besoin,
  calculant le TPI avec `focalMat(mnt, d = 100)` en unités du CRS.
- Le CHM Open-Canopy du projet n’était plus ignoré par le plan
  d’échantillonnage.
  [`nemeton::resolve_project_chm()`](https://pobsteta.github.io/nemeton/reference/resolve_project_layers.html)
  sonde `cache/layers/chm/` quand `download_chm_opencanopy()` écrit dans
  `cache/layers/opencanopy/` : sur Couchey, un CHM exploitable
  (EPSG:2154, 0,2 m, hauteurs jusqu’à 32 m) était invisible et le plan
  tirait sans strate de hauteur, en silence. Le plan passe d’une erreur
  bloquante à 112 placettes stratifiées hauteur × topographie. *Cause
  racine côté cœur, non corrigée ici (règle 12).*

### Changed

- `prep_sampling_raster()` sort de `mod_sampling_server()` en
  `.prep_sampling_raster()` : aucun état Shiny, et son contrat d’unités
  est précisément ce qui devait être testé.
- `.marculus_chm()` / `.marculus_chm_exploitable()` deviennent
  `.project_chm()` / `.chm_exploitable()` : ces helpers servent
  désormais la segmentation des houppiers **et** le plan
  d’échantillonnage, ce ne sont plus des helpers Marculus. \##
  \[0.142.1\] - 2026-08-27

### Changed

- `Imports: nemeton (>= 0.189.0)` → `(>= 0.192.0)` : `doc_url` /
  `doc_lang` sont désormais dans une release stable du cœur, ce qui rend
  l’icône « fiche » de la v0.142.0 réellement atteignable au lieu de
  silencieusement absente.
- `test-mod_family-doc-icon.R` : les trois `skip_if_not()` de version
  deviennent des `expect_true()` — le plancher rend le skip impossible à
  justifier.
- `test-mod_family-doc-icon.R` : le cas négatif passe de `C2` à des
  codes inconnus du cœur (`"unknown_indicator"`, `"ZZ"`).
  `nemeton 0.192.0` documente les 41 indicateurs, pas seulement C1 :
  figer un vrai code comme « non documenté » faisait rougir la suite à
  la première fiche ajoutée.
- Commentaires de `doc_icon()` et `.build_indicator_families()` : la
  raison des gardes défensives est corrigée (forme de `row` et cohérence
  avec `bilingual()`, non plus compatibilité de version). Les gardes
  elles-mêmes sont conservées.

## \[0.142.0\] - 2026-08-27

### Added

- Onglet *Familles d’indicateurs* : une icône « fiche » (`journal-text`)
  à côté du « i » pour les indicateurs que le cœur déclare documentés,
  ouvrant la vignette pkgdown correspondante dans un nouvel onglet (spec
  cœur 052). Lue depuis
  [`nemeton::indicator_labels()`](https://pobsteta.github.io/nemeton/reference/indicator_labels.html)
  (`doc_url` / `doc_lang`, cœur ≥ 0.192.0), jamais câblée sur un
  indicateur : C1 aujourd’hui, les suivants sans modification de l’app.
  Quand la fiche n’existe pas dans la langue courante, le cœur sert
  l’autre et l’infobulle le signale.
- `INDICATOR_FAMILIES$<F>$indicator_docs`, `get_indicator_doc()`,
  `doc_icon()`.
- i18n : `indicateur_fiche_ouvrir`, `langue_fr`, `langue_en`.
- CSS : `.nmt-doc-link`.

## \[0.141.1\] - 2026-08-27

### Fixed

- `test-indicator-families-defork.R` lisait `../../R/app_config.R` en
  direct. `R/` ne survit pas à l’installation : sous `R CMD check`,
  [`readLines()`](https://rdrr.io/r/base/readLines.html) échoue sur «
  cannot open the connection ». Le passage en `helper-sources.R`
  (v0.140.1.9001) avait traité onze fichiers et manqué celui-là, qui
  suffisait à faire échouer `R-CMD-check` sur `main`. Remède identique :
  `chemin_source()` + `skip_sans_sources()`.

### Changed

- Le brief ERA5 est déposé dans `briefs/vers-nemeton/` ; les deux copies
  pointent sur la release v0.141.0 plutôt que sur le cycle dev.

## \[0.141.0\] - 2026-08-27

### Added

- `download_insee_population()` + couche `population` : carroyage INSEE
  Filosofi 2021 câblé dans le résolveur, avec injection nommée
  `population_grid` — sans laquelle le dispatcher du cœur ne la transmet
  jamais.
- `helper-sources.R` : `chemin_source()`, `skip_sans_sources()`,
  `chemin_inst()`.
- Mapping de `regen_expo:era5_mois` + clé `regen_phase_micro_mois` :
  compteur de mois ERA5 dans la notification du moteur reGénération.
  Branche morte tant que le cœur n’émet pas l’événement — cf.
  `specs/BRIEF-nemeton-era5-progression-mensuelle.md`.

### Changed

- `Imports: nemeton (>= 0.189.0)`.
- Le rattachement du reliquat passe par
  `croiser_parcelles_onf(rattacher_reste = TRUE)` au lieu d’une
  implémentation locale.
- `.onf_part_foret()` mesure sur les deux couches brutes, plus sur la
  table de croisement.
- `onf_projet_croise()` rend `surface_rattachee_ha`.
- Le message « X ha hors forêt publique » devient « X ha … ont rejoint
  les parcelles forestières voisines ».
- Les houppiers retrouvent l’emprise du projet et la résolution native.
- `.regen_read_phase()` ne jette plus un `engine_status.json` périmé :
  elle rend son âge (`stale_s`) et la notification date le silence (« —
  dernier signe de vie il y a N min ») au lieu de retomber sur le
  libellé générique.
- `.regen_micro_lbl()` rend chaque morceau du libellé indépendamment :
  l’année s’affiche même sans compteur `(i/n)`.

### Fixed

- R-CMD-check : onze tests d’arbre source échouaient sous le paquet
  installé.
- L’export Marculus partait sans table `desserte` quand le réseau venait
  de l’onglet Accessibilité : `.marculus_desserte()` ne lisait que
  `cache/desserte/`. L’Accessibilité sert désormais de repli (et non
  d’union : les deux caches redisent la même BD TOPO).

### Removed

- `.onf_rattacher_reste()`, `.onf_singleparts()`,
  `MARCULUS_HOUPPIER_MAX_CELLS` : le cœur fait le travail.

## \[0.140.1\] - 2026-08-26

### Fixed

- `test-parcelles-csv.R` : `grepl("^\s*#", ...)` au lieu de `"^\\s*#"`
  rendait le fichier entier non analysable
  (`'\s' is an unrecognized escape`), et R-CMD-check échouait. Un
  fichier qui ne parse pas n’apparaît ni en `[failure]` ni en `[error]`
  : il disparaît du compte, d’où un faux vert en local.

## \[0.140.0\] - 2026-08-26

### Added

- `.onf_rattacher_reste()` : chaque bout de parcelle cadastrale sans
  numéro forestier rejoint la parcelle forestière avec laquelle il
  partage la **plus longue frontière**. Le reliquat est éclaté en
  parties simples au préalable.
- `.onf_part_foret()` : part forestière par parcelle, relevée sur la
  table de croisement **avant** rattachement.
- `.marculus_chm_exploitable()` : un modèle de hauteur dont rien
  n’atteint `hmin` n’est pas retenu.
- `MARCULUS_HOUPPIER_MAX_CELLS` : budget de cellules sous lequel lidR
  segmente.
- Repli accordéon sur le bloc « Calculs terminés » du sidebar de
  Sélection.

### Changed

- L’UGF « Hors forêt publique » n’est plus produite : rien d’une
  parcelle cadastrale n’est écarté.
- Une parcelle qu’aucune parcelle forestière ne touche garde sa propre
  UGF, nommée par sa référence cadastrale.
- L’import CSV ne purge plus aucune parcelle ; le réglage reste au
  bouton ONF.
- `onf_purger_hors_foret()` prend `part_foret` au lieu d’un libellé
  d’UGF.
- `onf_projet_croise()` prend `i18n` au lieu de `label_hors`, et rend
  `part_foret`.
- La résolution du modèle de hauteur passe par
  [`nemeton::resolve_project_chm()`](https://pobsteta.github.io/nemeton/reference/resolve_project_layers.html),
  qui préfère LiDAR HD à Open-Canopy.

### Fixed

- Les houppiers ne se calculaient sur aucun projet : mauvaise source de
  MNH, et refus de lidR de segmenter un raster stocké sur disque.
- Les messages de purge annonçaient « sous 10 % » en dur alors que le
  seuil est paramétrable et vaut 0 par défaut.

### Removed

- `.onf_label_hors_ugf()` et `.marculus_aoi()` : sans appelant.
- `n_partielles` : sans objet, l’UGF résiduelle n’existant plus.

## \[0.139.0\] - 2026-08-25

### Added

- `.add_normalized_indicators()` : chaque indicateur est persisté en
  **brut** et en **normalisé 0–100**
  ([`nemeton::normalize_indicator()`](https://pobsteta.github.io/nemeton/reference/normalize_indicator.html),
  bornes absolues). Seuls les indicateurs que le cœur déclare reçoivent
  un jumeau.
- `inst/sql/migration_006_indicateurs_norm.sql` : 31 colonnes `_norm`,
  idempotent. **À appliquer à la main** (l’app ne joue que
  `schema.sql`).

### Changed

- L’écriture en base ignore les colonnes absentes de la table plutôt que
  d’échouer, pour ne pas perdre les valeurs brutes avec les normalisées.
- Un indicateur entièrement vide ne prend plus de colonne dans les
  familles ; son statut est conservé pour que la raison reste dite.
- L’onglet Familles d’indicateurs affiche désormais les valeurs
  normalisées (conséquence de la persistance des `_norm`, que
  `create_family_index()` préfère).

### Fixed

- L’import CSV purge désormais comme le bouton ONF, avec la persistance
  des parcelles (`with_parcels`) et un compte rendu de ce qui est
  retiré.
- L’import CSV rafraîchit les sous-onglets de Sélection (relais de
  `restore_project` vers la réactive locale de `mod_home`).
- Cadre manquant du bloc ONF dans les paramètres ; `<strong>` affichés
  en clair dans deux infobulles ; libellé de la découpe cadastre
  clarifié.

## \[0.138.1\] - 2026-08-23

### Changed

- Les houppiers sont segmentés à la **fin du calcul des indicateurs**
  (`precompute_houppiers()`) et mis en cache dans
  `cache/layers/houppiers/houppiers.gpkg` ; l’export Marculus lit ce
  cache au lieu de segmenter. 173 s dans un `downloadHandler` gelaient
  la session.
- Best-effort : un échec de segmentation ne fait pas échouer un calcul
  d’indicateurs abouti ; un projet sans cache produit un GeoPackage
  valide, sans la couche.
- Repli sur la dalle entière quand l’appel avec emprise échoue (chaque
  contexte étant de toute façon découpé sur ses parcelles).

### Known issues

- La couche `houppier` reste absente en pratique : `nemeton v0.184.0`
  n’est ni taguée ni publiée, et son arbre de développement
  (`0.184.0.9000`) a régressé — `segment_houppiers()` échoue en
  `st_crs(x) == st_crs(y) is not TRUE`, avec ou sans emprise, sur une
  donnée qui passait le matin même. Brief déposé :
  `briefs/vers-nemeton/2026-08-23-houppiers-regression-crs.md`.

## \[0.138.0\] - 2026-08-23

### Added

- Couche `houppier` dans le lot Marculus via
  [`nemeton::segment_houppiers()`](https://pobsteta.github.io/nemeton/reference/segment_houppiers.html)
  (cœur v0.184.0, plancher **non** relevé : la version n’est pas taguée,
  le câblage dégrade en silence). Calculée une fois par projet,
  intersectée par contexte, CRS retamponné. **Réserve** : 173 s dans un
  `downloadHandler`.
- `project_onf_params()` / `set_project_onf_params()` : domanialité,
  purge, seuil et découpe cadastre persistés par projet.

### Changed

- Les trois calibrages du croisement ONF passent de la barre de Carte
  UGF à **Paramètres › Sources & paramètres** ; la sidebar garde un
  rappel.
- Seuil de purge paramétrable, **0 % par défaut**, purge et découpe
  cochées. La comparaison passe de `<` à `<=`, sans quoi 0 % ne
  supprimerait rien.
- Les débordements du parcellaire ONF hors cadastre sont écartés par une
  vraie intersection (une parcelle à cheval est coupée, pas rejetée).
- Bouton d’export renommé « Télécharger vers Marculus » (FR).
- Les contextes Marculus sont nommés par leur **parcelle forestière** («
  Couchey - parcelle 1 - coupe_rase ») et non par leur `ug_id` ; le nom
  de la forêt est élidé quand il répète celui du projet. Repli sur
  l’identifiant quand aucun libellé n’existe.

### Fixed

- Le cache Open-Canopy adopte `chm_predicted_1_5m.tif` : un pipeline
  interrompu ne fait plus recommencer 11 Go de travail valide.
- Le bouton « Générer les actions (IA) » et celui qui lance la
  génération prennent l’accent ambre `btn-ia` + trois étoiles. Test de
  source ajouté sur toutes les surfaces génératrices.
- Clé i18n `onf_params_save` manquante (le bouton affichait sa clé
  brute).

## \[0.137.0\] - 2026-08-23

### Changed

- Le bouton devient **« Télécharger pour Marculus »** : il produit un
  ZIP remis au navigateur, il n’envoie rien. Aucun canal de push
  n’existe des deux côtés.

### Added

- Champ `gpkgNom` dans chaque contexte du `.marsync` : nom de fichier du
  GeoPackage associé, nu (jamais un chemin) et ASCII. Clé inconnue des
  versions publiées de Marculus, donc inerte et sans ordre de livraison
  à respecter.
- `specs/BRIEF-marculus-import-zip.md` : entrée « Importer un lot (.zip)
  » côté Marculus — décompression, fusion des contextes, rattachement
  automatique par `gpkgNom`, avec les garde-fous (jamais
  `importerJson()`, refus du zip-slip, lot partiel accepté mais
  signalé).
- Tests : chaque contexte du lot désigne un fichier présent ; deux
  actions sur la même UGF ne partagent pas un nom de fichier.

## \[0.136.0\] - 2026-08-23

### Added

- `R/service_marculus.R` : export des chantiers de martelage vers
  **Marculus** (application Android). Un ZIP contenant un GeoPackage par
  action qui désigne des tiges (`eclaircie`, `coupe_rase`, `depressage`,
  `observation`) et un fichier `.marsync` portant tous les contextes.
  Bouton « Envoyer vers Marculus » dans le bloc *Exports* du Plan
  d’actions.
- Couches produites : `parcelle` (périmètre de l’UGF, colonnes
  `proprietaire`, `foret`, `commune`, `section`, `numero`) et `desserte`
  (repli des quatre couches de l’onglet Desserte en une seule,
  provenance conservée dans `type`). Aucune table de tuiles : vectoriel
  seulement.
- Le `.marsync` vise `fusionnerJson()` (union par UUID, non destructif)
  et non `importerJson()` (qui efface tout) : pas de section
  `referentiels`, un test l’interdit.
- Feuille de martelage **pré-remplie par profil de groupe** : les
  profils de `inst/config/groupes_amenagement.yaml` portent une liste
  `essences` (ONF, CRPF, OFB, générique), lue par
  `get_groupes_essences()`.
- `specs/BRIEF-nemeton-houppiers-mnh.md` : brief pour la couche
  `houppier` (segmentation de couronnes sur MNH), qui est de la logique
  métier et appartient au cœur.
- `specs/BRIEF-coeur-rattrapage-2026-08-23.md` : regroupe les cinq
  briefs cœur ouverts, et désigne la seule urgence.
- Nouvelles clés i18n `action_plan_download_marculus`,
  `action_plan_export_running_marculus`, `marculus_export_ok_fmt`,
  `marculus_export_empty`.

## \[0.134.1\] - 2026-08-23

### Fixed

- `.compute_error_message()` re-atténuait un message que le cœur
  **affirme**. Depuis `nemeton 0.183.1`, « ran out of memory » vient
  d’un `Result=oom-kill` constaté auprès de systemd : l’app l’affiche
  désormais à l’affirmatif, et réserve la formulation prudente au cas «
  tué, verdict indisponible » (mode dégradé). Implémente
  `briefs/vers-nemetonshiny/2026-08-23-reponse-oom-sigterm-scope.md`.
- Quand systemd répond `signal`, le cœur précise que ce n’est **pas** le
  plafond mémoire : l’app ne le contredit plus et passe le message tel
  quel.
- Le plafond en vigueur est extrait des deux formulations du cœur et
  affiché (`compute_error_ceiling_fmt`) : c’est lui qu’il faut relever.
- Nouvelles clés i18n `compute_error_killed`,
  `compute_error_ceiling_fmt` ; `compute_error_oom` reformulée à
  l’affirmatif.

## \[0.134.0\] - 2026-08-23

### Removed

- `.compute_memory_max()` et `.total_memory_bytes()`
  (`service_compute.R`), `.capped_memory_max()` (`service_monitoring.R`)
  : le plafond mémoire est une politique du cœur depuis
  `nemeton 0.183.0` (50 % de `MemTotal`, plancher 4 Go). Aucun site
  d’appel ne passe plus `memory_max`. L’app en portait une copie, d’où
  **trois plafonds** dans la même session (indicateurs 50 %, FORDEAD et
  reGénération 70 %). Implémente
  `briefs/vers-nemetonshiny/2026-08-22-plafond-memoire.md`.
- Un test interdit toute fraction de RAM (`MemTotal`, `/proc/meminfo`)
  dans `service_compute.R`, `service_monitoring.R` et
  `mod_regeneration.R`.

### Fixed

- Un calcul tué par le plafond mémoire s’affichait « failed in its
  capped child process (exit -15) ». `processx` surveille le client
  `systemd-run`, pas le R tué dans le scope : l’OOM (SIGKILL) devient un
  SIGTERM de démontage, que le cœur ne reconnaît pas comme mémoire.
  `.compute_error_message()` traduit −9, −15, 137 et 143 en un message
  qui nomme le plafond et le remède.
- Le message d’erreur de calcul était un `paste("Erreur de calcul:", …)`
  en dur en français (règle i18n) et recrachait le message brut du
  moteur sans échappement. Nouvelles clés `compute_error_fmt` et
  `compute_error_oom`.

### Changed

- Plancher relevé à `nemeton (>= 0.183.0)`.

## \[0.133.0\] - 2026-08-22

### Fixed

- L’import CSV ne posait pas `app_state$project_id` : les commentaires
  du nouveau projet s’écrivaient dans le répertoire du précédent
  (`save_comments()` le lit dans `mod_synthesis` et `mod_family`), le
  verrou restait sur l’ancien projet (cycle de vie branché sur
  `project_id` dans `app_server.R`), et les gardes qui comparent
  `project_id` raisonnaient sur le projet précédent. Les deux valeurs
  bougent désormais ensemble.

### Changed

- **Un import CSV remplace le projet courant** : l’ancien est supprimé
  avec toutes ses composantes (parcelles, UGF, indicateurs,
  commentaires, exports). La destruction n’intervient qu’une fois le
  nouveau projet créé, chargé et croisé ; tous les chemins d’échec
  repartent avant, projet intact. Garde `.remplacer_projet_courant()` :
  rien n’est détruit sans remplaçant valide.
- La modale d’import nomme le projet qui va disparaître et passe son
  bouton de confirmation en `btn-danger`. Sans projet ouvert : ni
  bandeau ni rouge.
- Nouvelle clé i18n `csv_import_replace_warn` (FR/EN).
- `reset_project_state()` (`mod_home`) est scindé :
  `reset_computation_state()` remet à zéro calcul, minuteur et cartes de
  progression sans toucher au projet courant. Nouveau signal
  `app_state$project_replaced`.
- Le bouton « Importer un CSV de parcelles » passe de l’en-tête du
  tableau (`mod_ug_table_panel()`) au **bloc « Tableau UGF » du sidebar
  gauche** (`mod_ug_table_actions_bar()`), en tête et séparé par un
  `hr()` des trois actions de sélection. Deux surfaces portent le même
  nom ; c’est dans le sidebar qu’on cherchait le bouton.
- Nouvelle clé i18n `csv_import_scope_hint` (FR/EN) : la portée du geste
  — crée un projet entier, ne suit pas la sélection du tableau — est
  affichée sous le bouton, à l’endroit où il se déclenche.
- `tests/testthat/test-parcelles-csv.R` : le test de présence vise
  désormais le sidebar, et un second test **interdit la duplication** du
  bouton dans l’en-tête du tableau.

## \[0.132.1\] - 2026-08-22

### Changed

- Suivi de `nemeton 0.182.0` (spec 049) : la famille F est décroisée
  côté cœur (`F1` = fertilité, `F2` = érosion). **Aucun changement de
  code applicatif requis** — l’app lit l’appariement code ↔︎ colonne ↔︎
  libellé depuis
  [`nemeton::indicator_families()`](https://pobsteta.github.io/nemeton/reference/indicator_families.html)
  / `indicator_labels()` depuis le dé-fork, et n’écrit aucune lettre
  d’axe en dur. Les quatre contrôles du brief sont vérifiés (libellés
  F1/F2, infobulle F2 topographique, `famille_fertilite` inchangée).
- Commentaires remis d’aplomb dans `R/app_config.R` (×2),
  `R/mod_family.R`, `R/utils_i18n.R`,
  `tests/testthat/test-indicator-families-defork.R` et
  `tests/testthat/test-libelles-famille-L.R` : ils présentaient le
  croisement de F comme un fait présent. Plus aucune famille n’est
  croisée (L en v0.176.0, F en v0.182.0), ce qui change ce que ces tests
  gardent — la concordance, non plus la distinction colonne/slug.
- Plancher relevé à `nemeton (>= 0.182.0)`.
- `tests/testthat/test-03mod_synthesis.R` : le test du bouton « Générer
  par IA » cherchait encore l’icône `robot`, remplacée par les trois
  étoiles et `btn-ia` en v0.130.9. Il était rouge sur `main` — sans
  rapport avec la famille F, corrigé au passage.
- `tests/testthat/test-info_popover.R` : le compte de « i » de la vue
  reGénération (14) datait d’avant le déplacement de quatre calibrages
  vers *Sources & paramètres* (v0.128.0). Compte corrigé à 10, **et**
  nouveau test qui retrouve les quatre « i » déplacés dans
  `output$regen_block` — rendu côté serveur, ils n’étaient plus couverts
  par aucune assertion.

### Added

- `specs/BRIEF-nemeton-plan-md-0.125-0.132.md` : brief de mise à jour du
  `PLAN.md` partagé du cœur, dont le journal s’arrête à v0.124.0 alors
  que 25 releases app se sont accumulées depuis.

## \[0.132.0\] - 2026-08-21

### Added

- Bouton « Importer un CSV de parcelles » dans le bloc *Tableau UGF* de
  l’onglet Sélection : crée un projet depuis un fichier
  `commune-code_insee.csv` listant les références cadastrales
  (`A1;A2;AO212`), croise avec le parcellaire ONF, et rafraîchit tous
  les sous-onglets. La commune vient du **nom** du fichier ; un nom hors
  convention est refusé plutôt que deviné. L’appariement porte sur le
  couple (section, numéro entier), pour que `A1` retrouve `A0001`.
- `R/service_parcelles_csv.R` : `parse_parcelles_csv()`,
  `resolve_parcelles_refs()`, `importer_parcelles_csv()`.

## \[0.131.1\] - 2026-08-21

### Added

- Une note sous le tableau de synthèse indique le sens de lecture de la
  colonne `Score` : haut = favorable, pour les douze familles, R
  comprise. L’inversion de R1–R4 a rendu visible un manque plus ancien —
  aucune colonne `Score` ne disait sa direction. Le nom de la famille R
  n’est pas renommé : il reste juste dans l’onglet Famille, où l’on voit
  les grandeurs brutes.

## \[0.131.0\] - 2026-08-21

### Fixed

- Suivi de `nemeton 0.181.0` (spec 048) : R1 à R4 sont désormais
  inversés à la normalisation comme R5, une UGF très exposée n’obtient
  plus un `famille_risque` flatteur. L’app n’inverse rien elle-même — un
  test le vérifie sur les sources.
- Les indicateurs calculés sous l’ancien sens sont invalidés une fois,
  via un marqueur `indicator_sense_version` : le parquet restait
  lisible, donc `compute_all_indicators()` aurait sauté le recalcul.
- `famille_risque` sort de la palette YlOrRd : orienté « haut = bon »,
  il aurait coloré en rouge les UGF les moins à risque.

### Changed

- Plancher relevé à `nemeton (>= 0.181.0)`.

## \[0.130.10\] - 2026-08-20

### Changed

- Le bouton IA des vues Famille rejoint l’accent ambre : les quatre
  surfaces de génération partagent désormais la même signalétique.
- Une ligne « Ambre » entre dans le tableau des couleurs de bouton du
  `CLAUDE.md`, avec la raison pour laquelle elle échappe à la hiérarchie
  sémantique (elle encode une provenance, pas un niveau d’action) et
  l’interdiction de l’employer pour une action de l’utilisateur.

### Fixed

- `updateActionButton()` restaurait l’icône robot après chaque
  génération (Synthèse et Famille) : l’accent se défaisait au premier
  clic. Un test lit désormais les sources et exige qu’aucune icône
  `robot` ne subsiste — ces appels ne s’exécutant qu’en session, aucun
  test d’UI ne pouvait les voir.
- Le bouton « insérer le conseil IA » de reGénération avait échappé au
  lot précédent.

## \[0.130.9\] - 2026-08-20

### Changed

- Le bouton « Générer par IA » de la Synthèse prend l’accent ambre
  `#E8A33D` et l’icône trois étoiles. Nouveaux jetons `--nemeton-ia` /
  `--nemeton-ia-dark` et classe `.btn-ia`. Cette couleur sort de la
  palette sémantique des boutons à dessein : elle marque une provenance
  (contenu généré), pas un niveau d’action. Texte sombre conservé —
  5,09:1 de contraste, contre 2,16:1 en blanc.
- Les panneaux IA du Plan d’actions et de reGénération prennent le même
  accent (classe `.bg-ia`), à la place du bleu « information » et du
  vert « succès ». Le vert du bloc « Tableau des actions » n’est pas
  touché.

## \[0.130.8\] - 2026-08-20

### Fixed

- La couche « Parcellaire ONF » de la Carte UGF n’était déclarée dans
  aucun des deux `overlayGroups` : sans case dans le contrôle de
  couches, elle restait affichée et ne pouvait pas être masquée.
- La prévisualisation ONF (`rv$onf_preview`) n’était jamais remise à
  `NULL` après le croisement : la surcouche restait superposée au
  résultat, affichant un parcellaire que le projet ne contenait plus
  après une purge.

### Added

- Un second message après la purge indique combien de parcelles restent
  partiellement forestières, et pourquoi cela maintient l’UGF « Hors
  forêt publique » — sans quoi sa survie se lit comme un échec de la
  suppression.

## \[0.130.7\] - 2026-08-20

### Fixed

- L’onglet Sélection continuait d’afficher les parcelles retirées par la
  purge « hors forêt publique » : il ne lit pas `current_project`, sa
  carte tient son propre état. Nouveau signal étroit
  `app_state$parcels_changed`, posé par les modules qui modifient
  `projet$parcels` et écouté par la carte seule — elle redessine et
  restreint la sélection, sans jamais l’élargir. `restore_project` a été
  écarté : il réveille `mod_search` (appel réseau) et exige des données
  que les autres modules n’ont pas.

### Added

- `specs/001-rafraichir-selection-parcelles.md` — première spec
  applicative du dépôt.

## \[0.130.6\] - 2026-08-20

### Added

- Coche « Supprimer les parcelles hors forêt publique (\< 10 %) » dans
  Carte UGF, décochée par défaut : retire du projet les parcelles que la
  forêt publique ne couvre pas ou couvre à moins de 10 %. Le
  raisonnement porte sur la parcelle, jamais sur le tènement — une
  parcelle forestière à 10 % ou plus est conservée entière, part hors
  forêt comprise. Les parcelles retirées quittent `$parcels` et
  `$tenements`, et `parcels.gpkg` est réécrit.

## \[0.130.5\] - 2026-08-20

### Fixed

- L’indicateur T3 (coupes rases) ne recevait pas `reference_year` : la
  fenêtre de récence s’ancrait sur la coupe la plus récente des UGF
  analysées, et non sur l’année courante. « Les 5 dernières années »
  pouvait donc désigner 2017-2021, et deux projets n’étaient pas
  comparables si leur dernière coupe différait. La fenêtre s’ancre
  désormais sur l’année courante. Les scores T3 déjà calculés peuvent
  changer sur les projets sans coupe récente.

## \[0.130.4\] - 2026-08-20

### Removed

- `.onf_parcelles_concernees()` et la réinjection des parcelles écartées
  : `nemeton 0.180.0` fait ce tri lui-même et expose le compteur via
  l’attribut `parcelles_concernees`. `onf_projet_croise()` redevient un
  appel direct. Plancher relevé à `nemeton (>= 0.180.0)`. Résultat
  identique.

### Fixed

- Le bloc roxygen d’`onf_projet_croise()` était détaché de sa fonction
  depuis v0.130.3, un helper s’étant inséré entre les deux. Sans effet
  au build (`@noRd`), corrigé par le retrait.

## \[0.130.3\] - 2026-08-20

### Changed

- Le bouton « Créer les UGF avec le parcellaire ONF » ne demande plus de
  sélection préalable : il retient lui-même les parcelles cadastrales
  qui rencontrent le parcellaire forestier, et l’annonce. Mesuré sur
  La-Vieille-Loye : 181 parcelles sur 1 271, calcul de 31,5 s à 11,1 s,
  résultat identique au croisement complet (le pavage passe toutefois de
  0,000000 % à 0,001231 % d’écart, 40× sous la tolérance).
- La domanialité passe de trois choix exclusifs à deux cases cochables
  ensemble ; « Toutes » n’était que leur conjonction. Le filtre agit en
  amont du croisement, donc sur l’auto-sélection. Ne rien cocher rend un
  message dédié sans lancer de requête.
- La note de calage passe du paragraphe permanent à un « i » à côté du
  bouton.

## \[0.130.2\] - 2026-08-19

### Changed

- Le calage des UGF sur les limites cadastrales devient **systématique**
  ; la coche correspondante est retirée. Les limites forestières ONF
  sont approximatives au bord : une UGF dont le tracé ne suit pas la
  parcelle qu’elle recouvre à 90 % ou plus est un artefact de
  numérisation. Mesuré sur La-Vieille-Loye : 170 → 124 tènements, 13 →
  41 bords exactement cadastraux. Une note permanente l’annonce ; les
  garde-fous du cœur restent entiers ; `caler_sur_cadastre` passe à
  `TRUE` par défaut mais subsiste comme paramètre.
- Le bouton « Croiser avec le parcellaire ONF » devient « Créer les UGF
  avec le parcellaire ONF » : le croisement est le moyen, créer les UGF
  est le but.

## \[0.130.1\] - 2026-08-19

### Removed

- Le bouton « Importer le parcellaire ONF », qui remplaçait les
  parcelles du projet par les parcelles forestières. Il partait de la
  même emprise et produisait les mêmes UGF que « Croiser », en jetant la
  composition cadastrale (donc `part_ugf`) — un cas dégradé du
  croisement, destructif de surcroît. Retirés avec lui :
  `onf_projet_from_parcelles()`, ses tests, et six clés i18n orphelines.

## \[0.130.0\] - 2026-08-19

### Fixed

- `onf_projet_from_parcelles()` échouait dès que le parcellaire
  forestier n’avait pas le même nombre de lignes que les parcelles du
  projet (« replacement has 427 rows, data has 1 »). L’idiome
  `utils::modifyList(projet, list(parcels = ...))`, repris de l’esquisse
  du brief, récurse dans les listes — et un `data.frame` en est une : il
  fusionnait les colonnes au lieu de remplacer l’objet. Remplacé par une
  affectation directe, avec un test de régression à tailles différentes.

### Changed

- Recette §6 de la spec 046 exécutée contre le **vrai** WFS ONF (forêt
  domaniale de Chaux) : les quatre cas passent. 213 parcelles / 2 114 ha
  en 1,1 s ; filtre de domanialité exact ; « aucune forêt publique » et
  « service indisponible » distingués. Bout-en-bout : 586 tènements /
  423 UGF, identifiants uniques, invariants verts, pavage cadastral
  exact à 0,000000 %.
- Calage cadastral **validé** sur le vrai cadastre de La-Vieille-Loye :
  170 → 124 tènements et 13 → 41 bords cadastraux, reproduisant
  exactement les mesures du cœur. Il n’était pas vérifiable sur cadastre
  synthétique, une grille régulière étant par construction désalignée du
  parcellaire forestier.
- `tenement_import_replace()` accélérée **95×** (628,9 s → 6,6 s sur 1
  422 fragments × 1 271 parcelles), à résultat strictement identique :
  index spatial calculé une fois au lieu d’être refait par fragment,
  suppression d’une intersection dont le résultat n’était jamais
  utilisé, et comparaisons d’aires sur géométries sans CRS (`st_area()`
  relisait les paramètres du CRS à chaque appel — 76,8 % du temps).
  Bénéficie aussi à l’import de découpage QGIS.

Versions before 0.130.0: see
[CHANGELOG-archive.md](https://pobsteta.github.io/nemetonshiny/CHANGELOG-archive.md).

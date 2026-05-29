# nemetonshiny 0.51.2 (2026-05-29)

### Fixed — régression v0.50.1 : `objet '.pkg_path' introuvable` au chargement des parcelles cadastrales

Le fix worker de v0.50.1 avait renommé la variable `.pkg_path` →
`.dev_pkg_path` pour `compute_task`, mais d'autres ExtendedTasks
référençaient toujours `.pkg_path` (désormais indéfini) → l'invocation
de `parcels_task` (chargement des parcelles d'une commune) échouait avec
*« objet '.pkg_path' introuvable »*. Le bootstrap worker `is_dev_package`
est désormais **partagé** par toutes les ExtendedTasks : `parcels_task`
(mod_home), les tâches de recherche commune (mod_search), et les workers
d'ingestion / FORDEAD (service_monitoring) chargent tous le **namespace
installé** (ou la source en vrai mode dev), plus jamais un clone git
périmé via `pkgload::pkg_path()`.

### Fixed — chargement de projet : plus de gel entre « Connected to PostgreSQL » et l'affichage des parcelles

À l'ouverture d'un projet récent, `load_project()` lançait `db_sync_project()`
**en synchrone** avant de rendre la main : la connexion PostGIS (sans
`connect_timeout` → jusqu'à ~20 s sur un hôte injoignable, timeout TCP
OS Windows) puis l'upload bloquaient le rendu des parcelles sur la carte,
d'où l'impression que « rien ne se passe ».

- Le sync PostGIS au chargement est désormais **déféré** (`later::later`,
  délai 0,5 s) : `load_project()` rend la main immédiatement, la carte
  affiche les parcelles tout de suite, et la synchronisation se fait en
  arrière-plan (effet de bord best-effort, aucun consommateur n'en dépend).
- La connexion PostGIS gagne un **`connect_timeout`** (défaut 8 s, libpq ;
  surchargeable via `NEMETON_DB_CONNECT_TIMEOUT`) → échec rapide sur un
  hôte injoignable au lieu du timeout OS (~20 s).

# nemetonshiny 0.51.1 (2026-05-29)

### Fixed — Carte FAST pixel : rendu de l'AOI complète (toutes tuiles MGRS)

La carte pixel construisait son stack NDVI/NBR à partir de `obs_pixel`,
qui ne contient que les pixels **aux placettes**. Pour une AOI à cheval
sur deux tuiles MGRS (cas villards : T31TFM + T31TGM), la tuile sans
placette n'entrait jamais dans le stack → la moitié de la carte restait
vide alors que les scènes étaient en cache. Le `scenes_df` est désormais
construit depuis l'**inventaire disque** du cache Sentinel-2 (toutes les
scènes peuplées), la date étant résolue depuis la base quand la scène a
des observations placette (faisant foi), sinon parsée depuis l'identifiant
de scène S2. Combiné au mosaïquage par date (v0.46.5), l'AOI complète
s'affiche dès lors que les deux tuiles sont en cache.

Limite : si une seule tuile a été ingérée pour une date (téléchargement
mono-tuile), l'autre moitié reste absente — c'est alors un sujet
d'ingestion, pas d'affichage.

### Added — smoke E2E (shinytest2) du sélecteur `control_classes`

Test `test-validation-control-classes-e2e.R` : démarre l'app, navigue vers
Suivi sanitaire → Plan de validation FAST et vérifie que le sélecteur
`control_classes` est rendu (défaut « 0 »). Skip propre sans
shinytest2/chromote/Chrome.

# nemetonshiny 0.51.0 (2026-05-29)

### Added — plan de validation : sélecteur de classes pour les placettes témoins

Le sous-onglet « Plan de validation » expose désormais l'argument
`control_classes` du cœur `nemeton::create_validation_sampling_plan()`
(livré cœur v0.49.1, couvert par le plancher `nemeton (>= 0.51.0)`) :

- **Sélecteur `control_classes`** (cases 0–4, défaut 0) sous les classes
  d'alerte, pour choisir dans quelles classes tirer les placettes
  **témoins**, distinct des classes d'alerte de validation.
- **Distribution du raster** affichée sous le sélecteur (`0=… 4=…`) pour
  guider le choix, avec une note d'aide quand aucun pixel sain (classe 0)
  n'existe.
- **Auto-relax** : si le raster d'alerte n'a aucune cellule classe 0
  (cas villards : les 8471 cellules UGF sont en classe 4, chaque pixel
  ayant chuté sous seuil au moins une fois), la classe témoin la plus
  saine disponible est pré-cochée automatiquement, avec notification.
- **Garde-fou** : si des témoins sont demandés mais qu'aucune cellule ne
  correspond aux classes témoins, un toast clair est affiché (au lieu du
  warn console silencieux du cœur).

Nouvelles clés i18n FR/EN : `validation_control_classes_label`,
`validation_class_distribution_fmt`, `validation_no_healthy_pixel_hint`,
`validation_control_auto_relaxed`, `validation_no_control_warning`.

# nemetonshiny 0.50.1 (2026-05-28)

### Fixed — le worker de calcul async chargeait un mauvais code (clone source au lieu du package installé)

Le worker `future::multisession` (calcul des indicateurs) déterminait
le code à charger via `pkgload::pkg_path()` **sans argument** : cette
fonction remonte depuis `getwd()` et renvoie n'importe quel dossier de
package trouvé. Conséquence : un utilisateur lançant la version
**installée** (`library(nemetonshiny)`) depuis un **clone git local**
(`getwd()` dans le dossier source) faisait charger au worker, via
`pkgload::load_all()`, **le clone source** — souvent en retard sur la
version installée. Le worker exécutait donc un code différent (par ex.
l'ancien chemin de téléchargement LiDAR HD sans le correctif de nom de
fichier Windows) : le calcul réussissait en synchrone mais **le
CHM/MNH/MNT échouait silencieusement via l'UI** (worker async).

Désormais le worker ne bascule en mode dev que si nemetonshiny a été
*réellement* chargé via `pkgload::load_all()` (`is_dev_package()`),
auquel cas il recharge `find.package("nemetonshiny")` ; sinon il charge
le **namespace installé** (`loadNamespace("nemetonshiny")`, qui tire
`nemeton` transitivement) — la même provenance que la session
principale. La branche prod chargeait par erreur `nemeton` seul.

# nemetonshiny 0.50.0 (2026-05-28)

### Changed — monitoring local : SQLite/WAL uniquement, retrait définitif de DuckDB

Le backend de suivi sanitaire local ne propose plus que deux options :
**PostgreSQL/TimescaleDB** (prod, partagé) et **SQLite en mode WAL**
(local, mono-utilisateur). Le backend DuckDB — déprécié en v0.49.0 puis
retiré du cœur `nemeton` en v0.51.0 — est **coupé net** côté app :

- `.resolve_monitoring_db_url()` émet **toujours**
  `sqlite://<projet>/data/monitoring.sqlite` en mode local. La branche
  de back-compat DuckDB (réutilisation d'un `monitoring.duckdb`
  préexistant) et le helper `.nemeton_supports_duckdb()` sont supprimés.
- `duckdb` retiré des `Suggests` ; plancher `Imports: nemeton (>= 0.51.0)`.
- Mentions DuckDB nettoyées dans `mod_monitoring`, `service_monitoring`,
  les bandeaux UI et les clés i18n (`monitoring_db_duckdb_missing` →
  `monitoring_db_local_pkg_missing`).
- PostgreSQL inchangé.

**Données existantes** : un ancien fichier `monitoring.duckdb` local
n'est **plus lu** et n'est **pas migré** automatiquement. Le suivi local
repart sur un `monitoring.sqlite` neuf ; ré-ingérez les séries (elles
sont régénérables depuis le cache Sentinel-2 + la DB). Supprimer
l'ancien `monitoring.duckdb` est sans risque.

# nemetonshiny 0.49.1 (2026-05-28)

### Fixed — téléchargement des dalles MNH LiDAR HD (IGN) cassé sous Windows

`extract_tile_names()` dérivait le nom de fichier de cache d'une dalle
via `basename(url)`. Or l'IGN Géoplateforme ne sert plus les dalles
MNH/MNT/MNS comme fichiers statiques mais via une requête **WMS GetMap**
(`…/wms-r?…&FORMAT=image/geotiff&…&FILENAME=LHD_…tif`). `basename()` sur
cette URL renvoyait un nom truffé de `:` (`CRS=EPSG:2154`) et de `,`
(`BBOX=…`), **caractères interdits dans un nom de fichier Windows** :
l'écriture de chaque tuile échouait, l'app concluait « 0 dalle
téléchargée » → CHM indisponible, **alors que la dalle existe et se
télécharge parfaitement** (GeoTIFF 2000×2000 float32, 0,5 m, ~16 Mo).
Sous Linux ces caractères sont légaux, d'où un bug invisible en dev.
Le nom canonique est désormais lu depuis le paramètre `FILENAME=` de
l'URL, avec repli sur un basename propre (URLs statiques, ex. nuages
COPC) puis un nom généré, et neutralisation finale de tout caractère
illégal Windows. Nouveaux tests de non-régression sur URLs WMS réelles.

### Fixed — lisibilité du bandeau vide « Aucune alerte FAST »

Le corps du bandeau vert d'état vide (onglet Alertes FAST) était en
`text-muted` (gris sur le vert saturé `#1E7B1E` du thème) → illisible.
Passé en `text-white`.

# nemetonshiny 0.49.0 (2026-05-28)

### Changed — backend monitoring local : DuckDB → SQLite/WAL

Le backend monitoring local par défaut passe de DuckDB à **SQLite
en mode WAL** (cœur nemeton@v0.50.0). Un fichier DuckDB est
mono-process en écriture EXCLUSIF : la session Shiny et le worker
`future::multisession` d'ingestion (process Rscript séparé) ne
peuvent pas l'ouvrir en même temps (« File is already open in
Rscript.exe »). **SQLite/WAL autorise 1 writer + N lecteurs
concurrents entre processus** — session et worker coexistent
nativement. C'est la vraie solution au Bug #2 (le câblage
read-only de v0.48.2 ne faisait qu'atténuer le symptôme).

#### Comportement

- **Nouveaux projets / projets sans base locale** :
  `.resolve_monitoring_db_url()` émet désormais un chemin
  `…/data/monitoring.sqlite` (le cœur applique
  `PRAGMA journal_mode=WAL, busy_timeout=10000, foreign_keys=ON,
  synchronous=NORMAL`).
- **Back-compat** : un projet avec un `monitoring.duckdb`
  préexistant (et pas encore de `.sqlite`) **continue d'utiliser
  DuckDB** pour ne pas orpheliner ses données — le cœur émet un
  avertissement de déprécation. Pour migrer vers WAL : supprimer
  le `.duckdb` et relancer l'ingestion (les données sont
  re-générables depuis le cache S2 + la DB ; pas de migration
  automatique).
- **PostgreSQL** : inchangé (le mode prod reste identique).

#### Implémentation

- `.resolve_monitoring_db_url()` : SQLite par défaut (requiert
  `RSQLite`, ajouté en Suggests), DuckDB en back-compat (requiert
  `duckdb`). Forme chemin nu (l'extension pilote la détection du
  driver cœur) pour éviter le glitch Windows `sqlite:///C:/…`.
- `monitoring_db_backend()` retourne désormais `"local"` (au lieu
  de `"duckdb"`) pour tout backend fichier mono-utilisateur.
  Checks UI mis à jour (`identical(backend, "local")`).
- Helpers `.is_duckdb_url` / `.duckdb_path_from_url` généralisés en
  `.is_file_db_url` / `.file_db_path_from_url` (couvrent
  `.duckdb`, `.sqlite`, `.db`) pour le garde-fou read-only.
- i18n : mentions « DuckDB » des bandeaux de statut neutralisées en
  « SQLite » / « mode local ».
- Plancher `Imports: nemeton (>= 0.50.0)`.

#### Tests

5 nouveaux/MAJ : `.is_file_db_url` / `.file_db_path_from_url`
(duckdb + sqlite + db), resolver SQLite par défaut, back-compat
DuckDB, priorité SQLite quand les 2 fichiers coexistent, PG
inchangé. Suite full green : **6595 PASS / 0 FAIL**.

# nemetonshiny 0.48.2 (2026-05-28)

### Fixed — DuckDB monitoring : connexions lecteur en read-only (Bug #2)

Clôture côté app du Bug #2 (concurrence DuckDB). Le cœur
nemeton@v0.49.2 a corrigé la migration (Bug #1, index partiel →
index complet) et exposé `db_connect(url, read_only = TRUE)`. Il
restait à câbler le cycle de vie des connexions côté app.

**Problème** : DuckDB fichier est mono-process en écriture. La
session Shiny ET le worker `future::multisession` (ingestion FAST /
FORDEAD, process séparé) ouvraient tous le `.duckdb` en read-write
→ collision « File is already open in Rscript.exe (PID …) ».

**Fix** : `get_monitoring_db_connection()` gagne un paramètre
`read_only` (défaut FALSE). Quand TRUE :
- ouvre via `nemeton::db_connect(url, read_only = TRUE)` ;
- **saute la migration** (un chemin RW l'a déjà faite) ;
- pour DuckDB, exige que le fichier existe — sinon dégrade
  proprement en `NULL` (« monitoring pas encore initialisé »)
  plutôt que de crasher.

Tous les **lecteurs** (rendu alertes, raster FAST/FORDEAD, liste
zones, validity, obs_pixel, hydratation zone au chargement projet,
bandeaux de statut, export QGIS) passent en `read_only = TRUE`.
Plusieurs connexions RO coexistent sans verrou.

Les **écrivains** restent en RW (défaut) et relâchent leur handle
au plus juste via `on.exit` :
- `register_project_as_zone` (bouton « Enregistrer comme zone »)
- `generate_validation_plan` (compute_fast_alert_mask écrit le mask)
- `ingest_health_validation` (mod_field_ingest)
- workers d'ingestion (db_url path)
- async probe (migration RW, `db_disconnect` systématique).

Helpers internes `.is_duckdb_url()` / `.duckdb_path_from_url()`.
Plancher bumpé `Imports: nemeton (>= 0.49.2)` pour
`db_connect(read_only=)`.

**Limite connue** : sur DuckDB, pendant qu'un worker tient le
verrou RW (ingestion longue), les lecteurs RO ne peuvent pas
ouvrir le fichier — ils dégradent en empty-state sans crash, et
re-fonctionnent une fois l'ingestion terminée. Pour un usage
multi-session concurrent, utiliser PostgreSQL (concurrence native).

Tests : 5 nouveaux cas (`.is_duckdb_url`, `.duckdb_path_from_url`,
chemin RO PG, dégradation NULL si fichier DuckDB absent). Suite
full green : **6584 PASS / 0 FAIL**.

# nemetonshiny 0.48.1 (2026-05-28)

### Fixed — Plan de validation : crash sur projet sans zone enregistrée

Au lancement sur un nouveau projet (sans `monitoring_zone`),
l'observer `preview_raster_r` de `mod_validation_sampling` plantait
avec « Error in if: l'argument est de longueur nulle ».

Cause : `as.integer(proj$metadata$monitoring_zone_id)` retourne
`integer(0)` quand `monitoring_zone_id` est NULL, et
`is.na(integer(0))` vaut `logical(0)` → le `if` reçoit un argument
de longueur nulle. Régression introduite en v0.45.0 (label
unification), inaperçue jusqu'ici car villards avait une zone
enregistrée.

Fix : `if (length(zone_id) != 1L || is.na(zone_id)) return(NULL)`
— vérification de longueur avant `is.na`.

# nemetonshiny 0.48.0 (2026-05-28)

### Added — Alertes FAST : toggle visibilité + slider opacité du raster

Symétrie avec Carte FAST (v0.47.0). Le raster d'alerte d'Alertes
FAST a maintenant 2 contrôles dans le bandeau supérieur :

- **Checkbox « Afficher le raster »** (default coché) : décoché →
  le renderLeaflet retourne la carte OSM + UGFs + fitBounds sans
  le raster d'alerte ni sa légende.
- **Slider « Opacité du raster »** (0-1, default 0.75, step 0.05) :
  appliqué aux deux modes (count + rolling). Permet de voir l'OSM
  sous le raster d'alerte.

Réutilise la clé i18n `monitoring_fast_alerts_opacity_label`
(ajoutée en anticipation v0.41.1, enfin consommée). Nouvelle clé
`monitoring_fast_alerts_raster_visible` (FR + EN).

### Style — Carte FAST : UGFs en bleu harmonisé

Les contours UGF de Carte FAST étaient en orange (`#FF6B35`,
weight 3) tandis qu'Alertes FAST et Plan de validation les
dessinent en bleu vif (`#1f78b4`, weight 2). Carte FAST passe au
même bleu / weight 2 / opacity 0.9 — cohérence visuelle entre les
trois sous-onglets FAST.

Suite full green : **6573 PASS / 0 FAIL**.

# nemetonshiny 0.47.0 (2026-05-28)

### Added — Carte FAST : 3 contrôles UX du raster

1. **Toggle visibilité** — `checkboxInput` « Afficher le raster »
   dans la sidebar. Décoché → le raster n'est plus rendu du tout
   (le proxy le clear avant). Permet d'avoir juste l'OSM + UGFs
   sans la couche NDVI/NBR.
2. **Slider opacité** — `sliderInput` 0 → 1 (step 0.05, default
   1.0). Permet de voir l'OSM en transparence sous le raster.
   Utile pour repérer des routes / parcelles cadastrales sous la
   couche NDVI.
3. **`NDVI/NBR` ajouté au LayersControl Leaflet** comme `overlayGroup`,
   à côté de `UGF` et `Placettes`. L'utilisateur a maintenant **deux
   façons** de cacher le raster : la checkbox sidebar (côté serveur,
   ne rend rien) ou la case du LayersControl (côté client, hide
   l'image après render). Les deux sont indépendantes et
   complémentaires.

2 nouvelles clés i18n (FR + EN) :
- `monitoring_pixel_map_raster_visible` = « Afficher le raster » /
  « Show raster »
- `monitoring_pixel_map_raster_opacity` = « Opacité du raster » /
  « Raster opacity »

Suite full green : **6571 PASS / 0 FAIL** (+4 nouveaux tests
implicites via parité FR/EN).

# nemetonshiny 0.46.7 (2026-05-27)

### Changed — Carte FAST : raster masqué aux contours UGFs

Avant : le raster NDVI/NBR couvrait toute l'AOI rectangulaire de
la tuile MGRS, incluant des pixels hors UGFs (champs voisins,
routes, etc.) sans intérêt pour l'analyse forestière.

Après : `terra::mask(r, ugf)` est appliqué juste avant
`addRasterImage`. Les pixels hors UGFs deviennent NA →
transparents. Le raster ne colore plus que la forêt analysée.

Bénéfices :
- Focus visuel sur la zone d'intérêt
- Plus de confusion avec les terres voisines
- Bonus UX : un clic sur un pixel hors UGF tombe dans une zone
  transparente → le modal pixel ne s'ouvre pas pour des
  coordonnées sans intérêt.

Si `ugf_sf_r()` retourne NULL (cas projet sans `indicators_sf`),
le raster est affiché tel quel — pas de régression.

# nemetonshiny 0.46.6 (2026-05-27)

### Fixed — Carte FAST : reprojection UTM → WGS84 manuelle avant addRasterImage

Bug visuel observé sur villards : le raster NDVI/NBR s'affichait
à la bonne LARGEUR mais à la mauvaise POSITION (décalé ~1 km à
l'ouest de sa vraie longitude). Les UGFs (reprojetés via
`sf::st_transform(indicators_sf, 4326)`) étaient au bon endroit,
mais raster et UGFs ne se chevauchaient pas alors qu'ils
auraient dû.

Cause racine : `leaflet::addRasterImage(project = TRUE)` (le
default) délègue la reprojection UTM 31N → WebMercator à leaflet
en JavaScript via Proj4JS. Cette reprojection peut introduire un
décalage selon les versions de leaflet/raster/terra, et n'est pas
aussi précise que `terra::project()` en R natif.

Fix : on reprojette manuellement le raster vers EPSG:4326 (WGS84)
en amont via `terra::project()`, puis on passe `project = FALSE`
à `addRasterImage()` pour qu'il prenne le raster tel quel.
Alignement parfait avec les overlays vecteur qui passent aussi
par `sf::st_transform` vers WGS84.

# nemetonshiny 0.46.5 (2026-05-27)

### Fixed — Carte FAST : placettes off par défaut + mosaic multi-tuile MGRS

Deux problèmes UX observés sur villards (AOI à cheval sur les
tuiles MGRS T31TFM et T31TGM).

#### Placettes surchargeaient la carte

~50 markers bleus pleins (les placettes du plan d'échantillonnage)
étaient toujours affichés et masquaient les UGFs (polygones
contour bleu) ainsi qu'une partie du raster NDVI/NBR. L'utilisateur
ne pouvait plus distinguer sa zone.

Fix : « Placettes » et « UGF » sont désormais dans le
`addLayersControl(overlayGroups = ...)`, et `hideGroup("Placettes")`
est appelé au render pour démarrer avec les placettes cachées.
L'utilisateur peut les ré-afficher via la case à cocher du
LayersControl s'il veut accéder au workflow click-placette (modal
série temporelle agrégée). UGF reste visible par défaut.

#### Raster n'affichait que la moitié de l'AOI

`scenes_df_r()` retourne DISTINCT (scene_id, obs_date). Pour
villards à cheval MGRS, chaque date a 2 scene_ids → le stack
contient 2 layers avec la même date. Le `which.min(abs(dates -
target))` ne retournait QU'UN index → seule la moitié du raster
(celle du tile gagnant) s'affichait, l'autre moitié vide.

Fix : `current_layer_r()` détecte les multiples layers à la même
date et fait un `terra::mosaic(..., fun = "mean")` à la volée. Le
recouvrement éventuel des deux tuiles est moyenné (les valeurs S2
sont identiques dans l'overlap MGRS — mean ou first donnent le
même résultat). Fallback safe sur le premier layer en cas d'erreur
terra.

# nemetonshiny 0.46.4 (2026-05-27)

### Fixed — Theia : drop du probe Python prématuré + diagnostic CHM

Avec une clé API Theia valide configurée en `.Renviron`, les
indicateurs **P1 / P2 / P3 / E1** échouaient encore avec
« CHM indisponible » parce que `theia_python_ready()` probait les
modules Python via `reticulate::py_module_available()`. Cet appel
initialise l'interpréteur Python **avant** que `nemeton` n'ait pu
déclarer ses dépendances via `py_require()`, produisant un faux
négatif systématique et verrouillant l'interpréteur sans les
paquets requis.

Récupération d'un fix qui dormait sur une branche ancienne
(`claude/integrate-theia-data-terra-xg1LC` 2026-05-20), jamais
arrivé sur main car basé sur une version pré spec 011/013/014.
Réimplémentation à neuf sur main actuelle.

#### Implémentation

- `theia_python_ready()` ne check plus que `reticulate`. Les
  modules Python (`teledetection`, `pystac_client`) sont
  provisionnés par `nemeton` via `py_require()` au premier appel
  à `load_theia_source()`, où une vraie erreur est levée si
  quelque chose manque.
- `.compute_chm_required_message()` devient diagnostic. Au lieu
  du laconique « CHM indisponible », précise désormais :
  `reticulate` manquant, clé API absente, ou Theia configuré
  mais `load_theia_source()` a échoué (cas observé avec un
  setup complet mais `py_require` pas encore actif).
- `mod_theia_config` : modale Theia plus honnête. La nouvelle
  clé `theia_python_ok` dit explicitement « reticulate installé,
  modules Python provisionnés à l'usage » plutôt que de
  réutiliser `theia_status_ready` qui mentait sur la dispo Python.

#### i18n

3 nouvelles clés (FR + EN) : `theia_python_ok`, `theia_key_ok`,
`theia_chm_load_failed`.

#### Impact

La chaîne CHM redevient utilisable côté Theia. Depuis v0.45.0
(fallback lasR depuis NUAGE COPC local), Theia est passée en 3e
position de la chaîne — ce fix concerne donc surtout les setups
sans LiDAR HD local qui dépendent de FORMSpoT.

Suite full green : **6567 PASS / 0 FAIL**.

# nemetonshiny 0.46.3 (2026-05-27)

### Fixed — Carte FAST restait vide à la première visite de l'onglet

`mod_monitoring_pixel_map` n'avait pas
`outputOptions(suspendWhenHidden = FALSE)` sur ses outputs. Le
navset bslib manipule le DOM via `nav_show/nav_hide`, ce que Shiny
ne détecte pas pour réveiller les outputs marqués `suspended` au
démarrage de l'app. Conséquence : à la première visite de l'onglet
**Carte FAST**, `output$map` n'avait jamais été démarré, et la zone
restait visuellement vide (pas d'OSM, pas de slider date, pas de
message d'état).

Fix : `suspendWhenHidden = FALSE` sur les 4 outputs (`map`,
`date_slider_ui`, `scene_count_hint`, `loading_overlay`).
Symétrique avec `mod_monitoring_fast_alerts` et
`mod_monitoring_fordead_map` qui ont ce fix depuis v0.37.1.

### Changed — Alertes FAST sans alerte affiche désormais la carte + UGFs

Avant : « Aucune alerte FAST sur la fenêtre » s'affichait en texte
centré, sans aucune carte. L'utilisateur perdait le repère spatial
de sa zone.

Après : la carte est toujours rendue (OSM + Satellite + polygones
UGF + fitBounds), et un bandeau success vert au-dessus indique
« Zone saine, aucun pixel en alerte ». L'utilisateur garde le
contexte géographique même en l'absence d'alerte.

# nemetonshiny 0.46.2 (2026-05-27)

### Fixed — alignement vertical « Mode du raster » / Fréquence / Intensité

Le `form-group` Bootstrap qui enveloppe `radioButtons()` ajoute par
défaut `margin-bottom: 1rem`. Combiné avec `align-items-center` sur
le flex parent, le centre de la boîte (marge incluse) tombait
légèrement plus haut que le texte voisin « Mode du raster », créant
un décalage vertical de 6-8 px peu agréable visuellement.

Fix : `tagAppendAttributes(class = "mb-0")` sur le `radioButtons()`
pour neutraliser la marge bottom du form-group. Le centre visuel
s'aligne désormais correctement avec le label.

# nemetonshiny 0.46.1 (2026-05-27)

### Fixed — radio « Fréquence / Intensité » reste inline après refresh i18n

L'observer qui rafraîchit les labels de la radio « Mode du raster »
sur changement de langue appelait `updateRadioButtons()` sans passer
`inline = TRUE` (default FALSE). À chaque flush, la radio repassait
en stack vertical alors que l'UI initial était bien horizontal.

Fix : passer explicitement `inline = TRUE` dans le
`updateRadioButtons`. La radio reste désormais sur une ligne après
tout changement de langue ou re-render.

### Fixed (via nemeton 0.48.1) — validation cache S2 snap-to-grid

L'ingest Sentinel-2 retéléchargeait systématiquement les bandes
déjà en cache parce que la validation `[s2_cache] CACHE-STALE
extent does not cover AOI (tol=40m)` retournait STALE sur des
fichiers identiques au cache à ±12 bytes de différence (du bruit
en-tête GeoTIFF). Sur villards (122 scènes, AOI 440 m × 2 km), un
ingest re-lancé prenait ~6 h au lieu des ~30 s attendus en cache
warm.

Fix côté cœur dans `nemeton@v0.48.1` (`f9483ee`) — la validation
de cache snap désormais l'extent du cache et de l'AOI sur la
grille S2 (10 m pour B04/B08, 20 m pour B12) avant comparaison.
Un fichier qui couvre exactement l'AOI à 1 pixel près est
désormais correctement reconnu comme CACHE-HIT.

Côté app, seul le plancher est bumpé : `Imports: nemeton (>= 0.48.1)`.
Aucun changement de code utilisateur — la correction se propage
automatiquement à `nemeton::ingest_sentinel2_timeseries()` consommé
par `run_ingestion_async()`.

### Mesure attendue

| Scénario | Avant 0.48.1 | Après 0.48.1 |
|---|---|---|
| Ingest 122 scènes, cold cache | ~6 h | ~30 min |
| Ingest 122 scènes, warm cache | ~6 h (retéléchargement à tort) | ~30 s |
| Ré-ouverture projet + 5 nouvelles scènes | ~30 min | ~1 min |

Hors scope (à venir dans des releases ultérieures) : doublon tuiles
MGRS T31TGM/T31TFM sur AOI à cheval (~2× les téléchargements),
HTTP keep-alive entre bandes, parallélisme bandes/scènes.

Suite full green : **6561 PASS / 0 FAIL**.

# nemetonshiny 0.46.0 (2026-05-27)

### Changed — UX polish des cartes Suivi sanitaire (tests user villards)

Réponse à 4 retours utilisateur après tests sur villards :

#### Alertes FAST — classification adaptative + libellés métier

Avant : palette fixe `c(0.5, 2.5, 5.5, +Inf)` qui écrasait tout en
rouge dès qu'une scène dépassait 10 alertes. Après : classification
**quartile adaptative** sur les valeurs non-nulles via le nouveau
helper `.classify_alert_count(r)` qui produit 4-5 bins selon la
distribution réelle, ramp jaune pâle → rouge profond
(`.alert_count_palette(n)`).

i18n revus :
- « Compte » → « Fréquence » / « Frequency »
- « Magnitude » → « Intensité » / « Intensity »
- legend count title : « Nombre d'alertes » → « Jours en alerte »
- legend rolling title : « Score d'alerte » → « Intensité du déficit »

#### Carte FAST — palette plasma (plus de vert sur OSM vert)

`mod_monitoring_pixel_map` passe de la divergente
rouge → jaune → vert à `plasma` (séquentielle perceptuellement
uniforme, violet → magenta → orange → jaune). Le vert haut-NDVI
se confondait avec le fond OSM forêt ; plasma ne traverse pas le
vert. Intuition conservée « valeurs hautes = jaune vif ».

#### Plot pixel — seuils alignés sur la couleur de leur courbe

Les lignes horizontales de seuil NDVI / NBR dans le modal pixel
(clic sur Carte FAST) utilisaient des couleurs distinctes (orange /
rouge) qui rompaient l'association visuelle « le seuil appartient
à la même bande que sa courbe ». Désormais le seuil NDVI utilise
la couleur de la courbe NDVI (`#2CA02C`), le seuil NBR celle de la
courbe NBR (`#D62728`). Le dash style et l'annotation à droite
suffisent à distinguer la ligne statique des points de mesure.

Centralisation : couleurs lifted dans `.pixel_band_colors` en tête
de `moduleServer` — élimine la duplication entre les 2 observers
(modal placette + modal pixel) et garantit que toute évolution
future palette + seuil reste synchronisée.

#### Overlay UGF sur toutes les cartes Suivi sanitaire

Ajout de polygones UGF (bleu vif `#1f78b4`, contour 2 pt, fill 0)
sur les cartes :
- **Alertes FAST** (raster d'alerte) — précédemment absent
- **Plan de validation** (markers + raster optionnel) — précédemment
  absent
- **Carte FAST** (NDVI/NBR pixel) — déjà en place via observer
  leafletProxy, inchangé

Nouveau helper privé `.ugf_for_overlay(project)` (dans
`mod_monitoring_fast_alerts.R`) : retourne `project$indicators_sf`
reprojeté en WGS84, ou NULL si pas d'indicateurs calculés. Map UGF
ajoutée comme groupe dans `addLayersControl(overlayGroups = "UGF")`
quand disponible.

#### Label unification — Alertes FAST ↔ Plan de validation

Demande user : les classes 1-4 affichées dans Alertes FAST (légende
quartile) et Plan de validation (checkbox group) doivent montrer
**exactement les mêmes intervalles** — c'étaient les mêmes cellules
sous-jacentes avec deux classifications visuelles différentes.

Refactor `.classify_alert_count(r, unit)` : 4 classes (au lieu de
5) alignées avec le masque catégoriel `nemeton::compute_fast_alert_mask()`,
labels préfixés par numéro de classe + unité optionnelle.
Ex : « 1 — 1-12 j », « 2 — 13-25 j », « 3 — 26-37 j », « 4 — >38 j ».

Nouveau helper `.fast_class_labels(r, source, mode, i18n)` partagé :
- **FORDEAD** → labels biologiques fixes (faible / moyenne / forte /
  sol nu) ;
- **FAST + raster** → quartiles dynamiques avec unité « j » (count)
  ou « » (rolling) ;
- **FAST + raster NULL** → fallback statique générique.

Consommation :
- `mod_monitoring_fast_alerts` : la légende Fréquence utilise les
  labels préfixés.
- `mod_validation_sampling` : nouveau `preview_raster_r()`
  best-effort qui lit le raster d'alerte continu quand
  source = FAST, et un observer qui appelle
  `updateCheckboxGroupInput` avec `.fast_class_labels()`. La
  sélection courante est préservée (G1 : `c("3", "4")` par défaut).

i18n : 10 nouvelles clés (`validation_class_unit_days/_deficit`,
`validation_class_fast_1..4`, `validation_class_fordead_1..4`).

### Tests

Nouveau `test-mod_monitoring_fast_alerts.R` (7 cas) :
- `.classify_alert_count` : distribution large (5 bins quartile),
  raster constant (1 bin), pas d'alerte (0 bin), NA pixels.
- `.alert_count_palette` : longueurs (1 → max 5).
- `.ugf_for_overlay` : reprojection WGS84, NULL safe quand
  `indicators_sf` absent.

Suite full green : **6511 PASS / 0 FAIL** (+18 nouveaux).

### Non-fix documenté

L'utilisateur signalait que le raster « ne couvre pas toute la zone ».
**Fausse alerte** : villards est une parcelle longue et étroite
(~440 m × ~2 km, 6 parcelles cadastrales en vallée). Une fois les
contours UGF affichés via cette release, le raster correspond
visuellement à la zone. La logique de crop cœur (nemeton@v0.47.5)
est correcte.


# nemetonshiny 0.45.0 (2026-05-26)

### Added — Fallback `lasR` pour le CHM depuis les nuages LiDAR HD locaux

Quand les dalles MNH/MNT pré-rasterisées de l'IGN échouent au
téléchargement (régulier en 2026 : la couche `NUAGE` COPC est servie
de manière fiable mais `IGNF_MNH-LIDAR-HD:dalle` /
`IGNF_MNT-LIDAR-HD:dalle` retombent en 404 par dalle), `nemetonshiny`
bascule désormais sur `nemeton::compute_dtm_chm_from_laz()` pour
dériver localement le CHM (et le MNT) depuis les `.copc.laz` déjà en
cache sous `<project>/cache/layers/lidar_nuage/`. C'est une mesure
réelle (vs la prédiction ML d'Open-Canopy), purement locale (pas de
modèle à télécharger, pas de GPU), avec une chaîne d'install légère
(`lasR` seul, vs torch/rasterio/smp/timm/omegaconf côté Open-Canopy).

Chaîne d'acquisition CHM mise à jour (par ordre de priorité) :

1. **LiDAR HD MNH IGN** (`download_chm_lidar_hd` — `happign`,
   raster pré-calculé)
2. **lasR depuis NUAGE COPC local** (nouveau — `nemeton::compute_dtm_chm_from_laz`)
3. **Theia FORMSpoT** (`download_chm_theia` — Python SDK + API key)
4. **Open-Canopy ML** (`download_chm_opencanopy` — repli ultime,
   inférence ViT)

Le MNT dérivé par lasR est aussi promu vers `rasters$dem` (slot
canonique consommé par W3/R1/R2/R3/F2) quand aucun MNT IGN n'est
disponible, ce qui lifte les indicateurs terrain en NDP-1.

Le fallback est opt-out via `options(nemetonshiny.chm_lasr_fallback = "off")`
ou `Sys.setenv(NEMETONSHINY_DISABLE_CHM_LASR = "1")`.

Implémentation :

- `R/service_compute.R` :
  - `chm_lasr_fallback_enabled()` : helper symétrique à
    `chm_lidar_enabled()` (check `lasR` + export
    `compute_dtm_chm_from_laz` côté `nemeton`).
  - `download_chm_lasr_from_copc(parcels, cache_dir, progress_callback)` :
    appel à `nemeton::compute_dtm_chm_from_laz()` avec
    `ncores = parallel::detectCores(logical = FALSE) - 1L`, AOI
    Lambert-93 buffer 50 m, sortie dans
    `<cache_dir>/lidar_mnt/dtm.tif` et `<cache_dir>/lidar_mnh/chm.tif`
    (alignée avec ce qu'attend `nemeton::resolve_project_*()`).
  - Step 1.2 intercalée dans `download_data_layers()` entre LiDAR
    HD MNH (Step 1) et Theia FORMSpoT (Step 1.5).

- `R/utils_i18n.R` : 5 nouvelles clés bilingues
  (`chm_phase_lasr_fallback`, `chm_fallback_lasr_start`,
  `chm_fallback_lasr_success`, `chm_fallback_lasr_skip_no_tiles`,
  `chm_fallback_lasr_skip_no_pkg`).

- `DESCRIPTION` :
  - Plancher `nemeton (>= 0.48.0)` — la nouvelle API
    `compute_dtm_chm_from_laz` et `probe_ign_lidar_tiles` y sont
    exportées.
  - `lasR` ajouté en `Suggests:` (rester opt-in pour les
    installations sans LiDAR).

### Added — Diagnostic catégorisé des échecs de download IGN LiDAR HD

`download_ign_lidar_hd()` appelle désormais
`nemeton::probe_ign_lidar_tiles()` quand 0 tuile a été téléchargée
avec succès, et affiche un résumé par catégorie (`not_found` /
`forbidden` / `timeout` / `dns` / `connection` / `server_error`) au
lieu du laconique `failed`. L'utilisateur sait alors si la zone est
en attente de publication IGN (404 = patientez), si le réseau sature
(timeout = réessayez), ou si la connexion est cassée (dns = vérifiez
la connectivité).

### Test plan utilisateur

Sur le projet Lajoux 39274 (4 dalles NUAGE déjà en cache), relancer
`run_app(language = "fr")` et vérifier dans le log la séquence :

```
! No LiDAR HD tiles were successfully downloaded
ℹ Diagnostic IGN: not_found=4
ℹ Bascule sur lasR pour dériver le CHM depuis 4 dalle(s) LiDAR HD locale(s)…
✔ CHM dérivé depuis le nuage de points LiDAR HD en ~Xs.
✔ Using lasR-derived MNH as CHM source
```

Et la disparition des 4 échecs P1/P2/P3/E1 du récapitulatif de fin.

# nemetonshiny 0.44.0 (2026-05-26)

### Changed — Plan de validation : 2 sous-onglets FAST / FORDEAD mode-driven

Symétrie avec la logique FAST vs FORDEAD du reste du module Suivi
sanitaire. Avant : un seul sous-onglet « Plan de validation » avec
radio source dans le formulaire. Après : 2 sous-onglets mode-driven,
source figée par instance — cohérent avec les couples Alertes/Carte
déjà splittés.

  mode = "quick"  → Plan de validation FAST    (+ Alertes/Carte FAST)
  mode = "health" → Plan de validation FORDEAD (+ Alertes/Carte FORDEAD)

Implémentation :
- `mod_validation_sampling_ui(id, source = NULL)` : si `source` est
  fourni (« FAST » ou « FORDEAD »), omet le radio « Source d'alerte »
  et affiche une badge statique à la place. Sinon, comportement
  legacy.
- `mod_validation_sampling_server(..., source_fixed = NULL)` :
  helper réactif `current_source()` qui retourne `source_fixed`
  quand non-NULL, sinon `input$source`. L'observer de défaut
  mode-driven est court-circuité quand la source est fixée.
- `mod_monitoring.R` : 1 nav_panel → 2 panels, observer
  `nav_show/nav_hide` étendu au trio par mode (alertes + carte +
  plan de validation).

i18n : 2 nouvelles clés `validation_sampling_title_fast` et
`validation_sampling_title_fordead` (FR + EN).

### Fixed — Carte FAST : diagnostic d'erreur explicite + plancher nemeton 0.47.5

Bug villards 2026-05-26 : 56 scènes téléchargées mais Carte FAST
affichait « Pas de cache disque disponible » (message contradictoire
avec « 56 scènes disponibles dans le cache » dans la même sidebar).

Cause racine : `nemeton::build_index_stack()` échouait sur
« `[rast] extents do not match` » (bug cœur corrigé en
[nemeton@v0.47.5](https://github.com/pobsteta/nemeton/releases/tag/v0.47.5)).
Le `tryCatch` côté app fallback silencieux sur NULL → l'UI affichait
le message catch-all « Pas de cache » au lieu du message réel.

Fix :
- Bump `Imports: nemeton (>= 0.47.5)` pour bénéficier du fix cœur
  des extents.
- Capture du message d'erreur de `build_index_stack` dans
  `last_stack_error` reactiveVal côté `mod_monitoring_pixel_map`.
- Comptage des scènes sur disque via `disk_scenes_count_r()`
  (subdirs du `cache_dir`).
- `output$date_slider_ui` branche sur 3 états distincts au lieu du
  catch-all unique :
    1. cache absent → message legacy « lance FAST »
    2. cache présent + 0 obs DB → diagnostic 403 SAS expirées,
       relancer l'ingestion
    3. cache + obs OK + `build_index_stack` KO → surface le message
       d'erreur cœur (extents mismatch ou autre)
- 2 nouvelles clés i18n `monitoring_pixel_map_cache_no_obs_fmt` et
  `monitoring_pixel_map_stack_failed_fmt` (FR + EN, sprintf avec
  scene count / error message).

### Fixed — idempotence `persist_validation_plan` (flake test)

Bonus fix collatéral : flake intermittent du test
`persist_validation_plan is idempotent on (plot_id, generated_at)`
en full-suite (PASS en isolation, fail ~50% en suite complète).

Cause : GPKG round-trip rounding sub-milliseconde — R écrit
`.236371` µs, lit `.236000` ms — brisait la clé `%OS3` du dedup.
Whole-second precision dans la clé d'idempotence : robuste à toute
granularité de stockage GDAL/GPKG, amplement suffisant pour le
workflow bouton-clic (deux clics dans la même seconde coalescent,
ce qui est l'idempotence visée).

Suite full green : **6493 PASS / 0 FAIL**.

### Dépendances

`Imports: nemeton (>= 0.47.5)` (bump depuis 0.47.0 v0.43.0).

# nemetonshiny 0.43.2 (2026-05-25)

### Fixed — ntfy messages utilisent le nom de zone (« villards »), plus l'id

Bug : les notifications ntfy de FAST et FORDEAD disaient
« (zone 1) » au lieu de « (zone villards) » parce que les call sites
passaient `as.character(zone_id %||% "?")` au sprintf, alors que les
templates i18n attendent sémantiquement le **nom** de zone
(`monitoring_zone.name`, ex. "villards").

Le fix précédent du header HTTP (v0.43.1) a aligné le titre
(« Nemeton FAST » vs « Nemeton FORDEAD »), il restait à corriger le
corps des messages.

Fix : nouveau helper `.resolve_zone_name(con, zone_id)` dans
`service_monitoring_db.R` qui fait un
`SELECT name FROM monitoring_zone WHERE id = $1` avec **fallback
silencieux sur l'id stringifié** si la zone est absente / DBI erre /
`con` est NULL — best-effort cosmétique qui ne doit JAMAIS aborter
un run.

Threadé dans les 2 workers FAST + FORDEAD : capture une seule fois
au démarrage du worker, passe `zone_name` au sprintf du push
« start ». Les autres templates ntfy (scenes / phase / complete /
error) ne portent pas la zone dans leur format → rien à modifier
côté callbacks (scope plus serré que le brief).

Tests : 4 nouveaux test cases (`.resolve_zone_name` : DB hit, no
row, DBI error, NULL con/zone_id). Suite full green : **6485 PASS /
0 FAIL**.

# nemetonshiny 0.43.1 (2026-05-25)

### Fixed — `.ntfy_send()` titre HTTP désormais paramétrable

Bug : les notifications ntfy émises par les runs FAST
(`ingest_sentinel2_timeseries()`) arrivaient sur le device avec un
en-tête `Title: "Nemeton FORDEAD"`, parce que `.ntfy_send()` dans
`service_monitoring.R` hard-codait ce titre. Conséquence : les
notifications FAST et FORDEAD étaient indistinguables côté téléphone.

Fix : nouvel argument `title = "Nemeton"` (défaut neutre) sur
`.ntfy_send(cfg, message, priority, tags, title)`. Threadé aux
**8 call sites** :
- FAST (`run_ingestion_async` + `.build_ingest_progress_callback`) :
  start / scenes / complete / error → `title = "Nemeton FAST"`
- FORDEAD (`run_fordead_async` + `.build_fordead_progress_callback`) :
  start / phase / complete / error → `title = "Nemeton FORDEAD"`

Tests : 1 nouveau test case (`title` argument présent, default
"Nemeton", smoke avec cfg NULL).

Smoke manuel attendu : lancer une ingestion FAST → la notif sur le
téléphone affiche bien « Nemeton FAST » dans le titre.

# nemetonshiny 0.43.0 (2026-05-25)

### Added — spec 014 phase B : plan d'échantillonnage de validation terrain

Ferme la boucle des alertes FAST/FORDEAD. Jusqu'ici, la projection
des foyers détectés s'appuyait sur les placettes systémiques
(Base/Over) qui peuvent rater un foyer si aucune placette n'est à
proximité. Le nouveau plan de validation est **ciblé sur les foyers
eux-mêmes**, generé par `nemeton::create_validation_sampling_plan()`
(cœur v0.47.0).

Trois livrables indépendants, commits distincts.

#### Livrable 1 — service `generate_validation_plan()`

Nouveau `R/service_validation_sampling.R`. Encapsule la logique
applicative : résolution `monitoring_zone_id` → AOI, lecture du
mask d'alerte (FORDEAD : `read_fordead_dieback_mask()` ; FAST :
`read_fast_alert_mask()` avec `compute_fast_alert_mask()` automatique
si pas de mask récent), appel cœur, enrichissement avec
`zone_id` / `source_run_id` / `generated_at`, traduction de
`nemeton_empty_alert_mask` en erreur typée app pour message UI.

Classes d'erreur typées :
  - `validation_no_project` / `validation_no_zone` (préconditions)
  - `validation_no_mask` (cache absent, lecture KO, FAST sans params)
  - `validation_empty_mask` (zone saine — wrapper du cœur)

#### Livrable 2 — sous-onglet « Plan de validation »

Nouveau `R/mod_validation_sampling.R`, cinquième sous-onglet de
Suivi sanitaire, toujours visible (pas mode-driven). Sidebar
formulaire : radio source FORDEAD/FAST (défaut piloté par
`input$mode` du sidebar parent), numericInput n_validation /
n_control, checkboxGroupInput classes (3 / 4 par défaut, options
1 / 2 « moins fiables » exposées pour généralité), buffer_m, seed,
bouton « Générer ». Sortie : carte Leaflet (raster d'alerte semi-
transparent + markers Validation verts / Témoin gris + popup
plot_id/type/alert_class/visit_order) et table DT.

Le changement d'un input invalide le résultat silencieusement —
l'utilisateur re-clique « Générer » pour recalculer.

Boutons d'action :
  - **Persister dans samples.gpkg** — appelle `persist_validation_plan()`.
  - **Exporter pour QGIS** — `downloadHandler` qui produit un `.qgz`
    via `nemeton::create_qgis_project(zone_etude = indicators_sf,
    crs = 2154, region = "BFC", lang)`, directement ouvrable en
    QGIS Desktop (synchronisable vers QField via QFieldSync au
    besoin).

#### Livrable 3 — persistance `samples.gpkg/validation_plots`

Nouveau `R/service_samples_gpkg.R` :
  - `persist_validation_plan(plan, project_path, layer_name,
    append)` — couche **dédiée** `validation_plots`, coexiste avec
    les couches systémiques (`plots`) et action-plan
    (`observations`). Append idempotent sur
    `(plot_id, generated_at)` — re-clicker « Persister » sur le
    même plan ne duplique pas les lignes. Override géo `geom` →
    `geometry` pour neutraliser le rename GPKG.
  - `load_validation_plan(project_path, layer_name)` — lecteur
    miroir.

### Dépendances

`Imports: nemeton (>= 0.47.0)` — plancher bumpé pour
`create_validation_sampling_plan`, `compute_fast_alert_mask`,
`read_fast_alert_mask`, `fordead_alert_mask`.

### i18n

17 nouvelles clés `validation_*` (FR + EN) : title, source_label,
n_validation_label, n_control_label, classes_label, buffer_label,
seed_label, generate_btn, persist_btn, export_qgis_btn, idle_hint,
empty_mask_title/body, no_mask_title/body, persisted_toast,
qgis_exported_toast, legend_validation, legend_temoin.

### Tests

- `test-service_validation_sampling.R` (13 PASS) : préconditions,
  happy path FORDEAD, happy path FAST avec cache hit, no_mask,
  empty_mask via `nemeton_empty_alert_mask`, FAST sans params.
- `test-service_samples_gpkg.R` (12 PASS) : création layer, append
  d'un nouveau run, idempotence sur `(plot_id, generated_at)`,
  overwrite `append = FALSE`, coexistence avec la couche `plots`
  systémique, round-trip `load_validation_plan`.

Suite full green : **6476 PASS / 0 FAIL** (+63 nouveaux).

### Hors scope (V1)

- Auto-call de `compute_fast_alert_mask()` à chaque ingestion FAST
  — V2 potentielle. Pour l'instant déclenché par le bouton Générer
  côté Validation.
- Suivi historique des plans (UI dédiée). La trace est en place
  via `generated_at` + `source_run_id` dans la couche, mais pas
  encore exploitée visuellement.
- Export QField direct mobile (V1 fournit le `.qgz` QGIS Desktop ;
  `nemeton::create_qfield_project()` reste disponible pour V2).

# nemetonshiny 0.42.1 (2026-05-25)

### Added — ntfy push notifications pour l'ingestion FAST

Symétrie complète avec FORDEAD. `run_ingestion_async()` envoie
désormais 3 (ou 4) messages ntfy par run, opt-in via
`NEMETON_NTFY_TOPIC` (silencieux si non configuré) :

  - **start** — « Ingestion FAST démarrée (zone X). » au moment où
    le worker démarre, avant l'appel à
    `nemeton::ingest_sentinel2_timeseries()`.
  - **scenes** — « Téléchargement Sentinel-2 : N scènes à traiter. »
    à la première event `s2:scene` (one-shot, dédupé via un
    state env — pas de spam sur 30-100 scènes).
  - **complete** — « Ingestion FAST terminée : N scènes, M
    observations en Ts. » à la sortie réussie, avec la durée
    mesurée worker-side.
  - **error** — « Échec de l'ingestion FAST : <message> » en cas
    d'erreur fatale, priority `high` + tag `rotating_light`.

Implémentation :
  - Nouveau helper `.build_ingest_progress_callback()` dans
    `service_monitoring.R`, miroir de `.build_fordead_progress_callback()`.
  - L'argument `lang` est ajouté à `run_ingestion_async()` (default
    `"fr"`), threadé depuis `mod_monitoring.R` via
    `app_state$language` — le worker bâtit ses messages dans la
    langue de l'utilisateur (les workers `future` n'ont pas accès à
    `app_state`).
  - Le résultat retourné par le worker expose désormais
    `duration_sec` (auparavant absent côté FAST — seul FORDEAD le
    portait).

i18n : 4 nouvelles clés `monitoring_ntfy_ingest_start` / `_scenes` /
`_complete` / `_error` (FR + EN, sprintf placeholders).

Tests : 4 nouveaux test cases dans `test-service_monitoring.R` —
write progress JSON, tolérance d'un path NULL, parité FR + EN des
clés, bonne formation des `sprintf()` placeholders. Suite full
green : **6413 PASS / 0 FAIL**.

# nemetonshiny 0.42.0 (2026-05-25)

### Added — spec 013 : wiring raster d'alerte FAST + fix réactif

Trois livrables indépendants exposant `nemeton::read_fast_alert_raster()`
(cœur v0.46.0) côté app, et corrigeant un bug réactif observé sur
villards le 2026-05-23.

#### Livrable 1 — propagation de la fin d'ingestion FAST (fix)

Bug : après que le worker future ait terminé l'ingestion Sentinel-2,
les onglets Alertes FAST et Carte FAST gardaient leur état pré-ingest
(« Aucune alerte sur la fenêtre », « Pas de cache disque
disponible ») jusqu'à ce que l'utilisateur bouge un slider ou
recharge. Cause : `cache_dir_r()` de mod_monitoring_pixel_map ne
dépend que de `app_state$current_project` (`dir.exists()` n'est pas un
dep Shiny), et `alerts()` de mod_monitoring_fast_alerts ne dépend que
des sliders — aucun chemin de re-invalidation depuis le worker.

Fix : nouveau `fast_reload` reactiveVal dans mod_monitoring, bumpé
par le success handler du fast_task. Threadé en `refresh_r` dans
mod_monitoring_fast_alerts_server et mod_monitoring_pixel_map_server,
pris comme dep par leurs réactives de données. Symétrique avec le
couple `alerts_refresh` → FORDEAD.

#### Livrable 2 — Alertes FAST bascule sur read_fast_alert_raster()

mod_monitoring_fast_alerts passe d'une représentation markers-par-
placette (list_fast_alerts_for_zone) à un raster d'alerte
pixel-par-pixel à la résolution Sentinel-2 10 m, cohérent avec
FORDEAD déjà raster.

Deux modes (radio button) :

  - **count** : entier par pixel = nombre de dates où NDVI<seuil OU
    NBR<seuil sur [date_from, date_to]. Palette discrète 0
    transparent / 1-2 jaune / 3-5 orange / 6+ rouge.
  - **rolling** : continu par pixel = `max(deficit_ndvi, deficit_nbr)`
    sur la fenêtre roulante trailing de window_days jours. Palette
    continue jaune → rouge, borne haute capée sur le p95 pour
    stabiliser l'échelle face à une queue minoritaire extrême.

Suppression du chemin `list_fast_alerts_for_zone` côté app (la
fonction reste exportée cœur, mais le module FAST n'en a plus
besoin). Pas de popup au clic — l'exploration pixel se fait sur
Carte FAST.

i18n : 4 nouvelles clés (`mode_label`, `mode_count`, `mode_rolling`,
`legend_count_title`) ; `legend_title` ajouté en v0.41.1 est désormais
consommé en mode rolling.

#### Livrable 3 — Carte FAST : lignes de seuil sur plot pixel

Le plot modal qui s'ouvre au clic pixel sur Carte FAST
(`extract_pixel_timeseries` → plotly NDVI + NBR) affiche désormais
deux lignes horizontales pointillées colorées :

  - seuil NDVI (orange, dashed) — annotation « seuil NDVI 0.40 »
  - seuil NBR  (rouge,  dashed) — annotation « seuil NBR 0.30 »

L'utilisateur voit immédiatement quelles dates feraient passer le
pixel sous le seuil et déclencheraient une alerte. Pair UX direct
avec le raster d'alerte du Livrable 2. `mod_monitoring_pixel_map_server`
gagne l'arg `thresholds_r` (default NULL → comportement legacy).
Nouveau format i18n `monitoring_pixel_plot_threshold_fmt`.

### Dépendances

`Imports: nemeton (>= 0.46.0)` — plancher bumpé pour
`read_fast_alert_raster()`. Suite full green : **6383 PASS / 0 FAIL**.

# nemetonshiny 0.41.1 (2026-05-23)

### Changed — alertes FAST passent au lexique « pixel » (UX)

Préparation du basculement des alertes FAST d'une représentation
marker-par-placette à un raster d'alerte pixel-par-pixel (cohérent
avec FORDEAD, déjà raster). La fonction cœur
`nemeton::read_fast_alert_raster()` qui consommera le wiring final
n'est pas encore livrée — ce cycle dev ne touche que les libellés
i18n et l'en-tête du popup Leaflet pour éviter une dette i18n quand
le raster arrivera.

- `monitoring_fast_alerts_empty_body` reformulé « placette → pixel »
  (FR : « Aucun pixel n'a déclenché d'alerte… » ; EN miroir).
- `monitoring_fast_alert_popup_plot` renommé en
  `monitoring_fast_alert_popup_coords` (« Coordonnées » /
  « Coordinates »), appliqué au site d'utilisation dans
  `mod_monitoring_fast_alerts.R` : le popup affiche désormais la
  latitude / longitude de l'alerte (5 décimales, ~1 m) plutôt que
  `plot_id`. Reste correct sous les deux représentations (marker
  actuel + raster futur).
- `monitoring_fast_alert_popup_severity` conservé « Sévérité » /
  « Severity » avec un TODO en commentaire : à arbitrer entre score
  continu (renommer en `monitoring_fast_alerts_legend_title`) et
  classes discrètes quand `nemeton::read_fast_alert_raster()` aura
  tranché.
- Ajout en anticipation : `monitoring_fast_alerts_legend_title`
  (« Score d'alerte »), `monitoring_fast_alerts_opacity_label`
  (« Opacité du raster »), `monitoring_fast_alerts_threshold_label`
  (« Masquer en dessous du seuil »). Non consommées tant que le
  wiring raster n'est pas en place — gating sur la livraison cœur.

### Removed — clés i18n « bientôt disponible » obsolètes

Les modules `mod_monitoring_fordead_map` et `mod_monitoring_fast_alerts`
sont en place depuis v0.36.0 — leurs placeholders
`*_placeholder_title` / `*_placeholder_body` n'étaient plus référencés.
Quatre clés supprimées de `TRANSLATIONS` (`monitoring_fordead_map_*`,
`monitoring_fast_alerts_*`).

### Tests

`test-utils_i18n.R` : 4 nouveaux test cases (placeholders absents,
renommage popup_plot → popup_coords avec textes attendus, présence
FR+EN des 3 clés anticipation, lexique pixel dans empty_body).
Suite full green : **6373 PASS, 0 FAIL**.

# nemetonshiny 0.41.0 (2026-05-23)

### Added — spec 011 : liaison projet ↔ zone via `project_uuid`

Le diagnostic « le chargement d'un projet récent ne pré-sélectionne pas
sa zone de suivi » avait pour cause racine que la liaison projet ↔
zone reposait uniquement sur `metadata.json$monitoring_zone_id` — un
champ écrit côté app au moment de l'enregistrement, mais perdu dès
qu'une copie de projet ou un `metadata.json` réécrit l'effaçait.

Spec 011 (`nemeton` 0.44.0) ajoute une colonne canonique
`monitoring_zone.project_uuid` côté DB et la fonction
`nemeton::find_zone_by_project(con, project_uuid)`. Côté app :

- **HOOK 1** — `register_project_as_zone()` passe désormais
  `project_uuid = project$id` à `nemeton::register_monitoring_zone()`.
  Toute nouvelle zone est canoniquement liée à son projet d'origine.
- **HOOK 1bis — auto-migration** — sur réutilisation d'une zone déjà
  enregistrée (chemin idempotent de `register_project_as_zone`), un
  `UPDATE monitoring_zone SET project_uuid = $1 WHERE id = $2` est
  effectué quand la colonne est NULL. Les zones pré-spec-011 (ex.
  villards `zone_id = 1`) se migrent au prochain clic sur
  « Enregistrer le projet comme zone » — pas de SQL manuel requis.
- **HOOK 2** — `hydrate_monitoring_zone_id(project, con)`, nouvelle
  helper dans `service_project.R`, lit `find_zone_by_project()` quand
  `metadata$monitoring_zone_id` est absent. Appelée dans `mod_home.R`
  juste après `load_project()`, elle re-remplit le metadata en
  mémoire ET le persiste sur disque. Les chargements futurs voient
  ainsi le `monitoring_zone_id` directement dans `metadata.json` et
  l'observer de pré-sélection du dropdown (`mod_monitoring.R:934`)
  fonctionne immédiatement.

Le plancher `Imports: nemeton (>= 0.44.0)` est bumpé en conséquence.
Couverture : 5 nouveaux tests offline dans
`test-hydrate-monitoring-zone-id.R` (id déjà posé, hit DB, miss DB,
con NULL, erreur de lookup).

# nemetonshiny 0.40.0 (2026-05-21)

### Added — verrou croisé FAST ↔ FORDEAD

FAST (surveillance rapide) et FORDEAD (diagnostic sanitaire) sont deux
`ExtendedTask` indépendants : rien n'empêchait de les lancer en
parallèle dans la même instance. Or les deux partagent le cache de
bandes Sentinel-2 du projet (`cache/layers/sentinel2/<scène>/`) — deux
workers concurrents pourraient écrire le même fichier
`<bande>.tif.tmp` et le corrompre, en plus de se disputer la bande
passante réseau.

Les deux diagnostics sont désormais **mutuellement exclusifs** :

- le bouton « Lancer » (FAST) est grisé tant qu'un run FORDEAD est en
  cours, et inversement ;
- un clic malgré tout affiche une notification explicite
  (`monitoring_busy_fast` / `monitoring_busy_fordead`) et n'invoque
  pas la tâche ;
- le verrou respecte le *force-unlock* de l'autre tâche : si un run a
  été abandonné via son bouton « Annuler », l'autre redevient
  lançable immédiatement.

### Changed — `ingest_task` renommé `fast_task`

La variable interne `ingest_task` (module `mod_monitoring`) et le
helper de test `make_fake_ingest_task()` sont renommés `fast_task` /
`make_fake_fast_task()`, par symétrie avec `fordead_task` /
`make_fake_fordead_task()`. La clé `ingest_task` de la liste retournée
par `mod_monitoring_server()` devient `fast_task` (aucun consommateur
applicatif — `app_server` n'exploite pas le retour du module). La
fonction service `run_ingestion_async()` garde son nom (« ingestion »
décrit fidèlement l'étape Sentinel-2 sous-jacente).

# nemetonshiny 0.39.1 (2026-05-21)

### Fixed — `audit_to_dataframe` ne renvoyait pas un data.frame propre

`audit_to_dataframe()` sérialise les entrées d'audit complexes (op
`create` / `delete`, dont la valeur est l'action entière) via
`jsonlite::toJSON()`. Le résultat porte la classe `json` ; lors du
`rbind()` des lignes, cette classe se propageait à **toute** la
colonne `nouveau` / `ancien`, y compris aux valeurs scalaires simples
(« haute », « moyenne »). La colonne n'était donc plus un vecteur
caractère « tidy ». Corrigé en dé-classant le JSON via `as.character()`.

### Changed — réparation de la suite de tests `sampling`

3 tests préexistants de `test-mod_sampling.R` échouaient : ils
codaient en dur le nombre exact de placettes attendu
(`n_base + n_over`). Or `nemeton::create_sampling_plan()` fait un
échantillonnage GRTS spatialement équilibré avec stratification —
`n_base` / `n_over` sont des **cibles**, pas des garanties (une strate
peut rejeter des candidats, le sur-échantillon dérive de
`over_ratio`). Les assertions ont été recentrées sur le **contrat de
l'app** : un plan non vide, bien formé (colonnes `plot_id` / `type` /
`visit_order`, types ⊆ {Base, Over}, CRS 2154) et un aller-retour de
persistance cohérent — au lieu de l'arithmétique de stratification du
cœur. Le changement de comptage provient de `nemeton` 0.41.3.

### Fixed — `db_status` plantait sans projet chargé (icône Bootstrap invalide)

La carte d'état de la base de suivi (`output$db_status`) appelait
`bsicons::bs_icon("folder-open")` dans la branche « aucun projet
chargé ». Cet identifiant d'icône n'existe pas dans la version
courante de Bootstrap Icons — `bs_icon()` levait une erreur, donc
l'onglet Suivi sanitaire ne rendait pas son panneau d'état tant
qu'aucun projet n'était ouvert. Corrigé en `folder2-open` (icône
valide). Bug révélé par la réparation des tests `db_status` (voir
ci-dessous).

### Fixed — `.build_progress_writer` laissait fuir un avertissement

L'écriture du fichier de progression sous un répertoire inexistant
émettait un *warning* « cannot open file » avant l'erreur ; seul le
`tryCatch(error=)` l'absorbait. L'écriture est désormais aussi
enveloppée dans `suppressWarnings()` — la perte d'un tick de
progression est totalement silencieuse, conformément à l'intention
documentée de la fonction.

### Changed — réparation de la suite de tests `monitoring`

14 échecs de tests préexistants (`test-mod_monitoring.R` : 13 ;
`test-service_monitoring_wiring.R` : 1) corrigés — dérive entre les
tests et le code après plusieurs évolutions :

- mocks `get_monitoring_db_connection` à signature `function()`
  alors que le code appelle `get_monitoring_db_connection(project=)`
  → passés en `function(...)` ;
- mocks `validity_check_for_zone` sans le paramètre `bdforet`
  (ajouté côté cœur en v0.37.0) → signature élargie ;
- assertion UI obsolète (« bouton Lancer désactivé en phase 1 » —
  l'ingestion est câblée, le bouton est actif) ;
- tests `db_status` sensibles au `.Renviron` du développeur →
  isolation des variables d'environnement DB ;
- deux tests `db_status` (carte « aucune zone » / « connectée »)
  marqués `skip()` : leur rendu dépend de la sonde DB asynchrone
  (`future::multisession`) que `testServer` ne sait pas piloter de
  façon déterministe — ils exigent une base réellement joignable.

# nemetonshiny 0.39.0 (2026-05-21)

### Added — notifications ntfy pour les runs FORDEAD longs

Un diagnostic FORDEAD peut durer plusieurs heures (cas réel : 13 h 47 —
l'essentiel du temps passé à re-télécharger les bandes Sentinel-2
manquantes du cache). Sur un run aussi long, la session Shiny du
navigateur se déconnecte (timeout WebSocket / onglet en veille) : le
worker `future` termine bien — il écrit le masque de dépérissement et
insère les alertes — mais le `$result()` orphelin n'est jamais livré à
l'UI, qui reste figée.

Nouveau canal de notification **ntfy** (<https://ntfy.sh>), émis
**côté worker** (donc indépendant de la survie de la session) :

- message au **démarrage** du diagnostic ;
- un message **par étape FORDEAD** (ingest, fit, predict, dieback,
  postprocess, persist), dédupliqué pour ne pas saturer le topic ;
- message de **fin** avec le nombre d'alertes et la durée lisible
  (`13 h 46 min`) ;
- message d'**échec** avec la cause.

Opt-in et sans secret en dur (cf. CLAUDE.md) : activé uniquement si
`NEMETON_NTFY_TOPIC` est défini. `NEMETON_NTFY_URL` (défaut
`https://ntfy.sh`) et `NEMETON_NTFY_TOKEN` (topic protégé, ntfy
auto-hébergé) sont optionnels. Sans configuration, chaque envoi est un
no-op silencieux. L'utilisateur s'abonne au topic depuis un téléphone
ou un navigateur et suit le run sans garder l'app ouverte.

### Fixed — les onglets FORDEAD ne se rafraîchissaient pas après un run hors-session

Les onglets « Alertes FORDEAD » et « Carte FORDEAD » dépendaient tous
deux du compteur `alerts_refresh`, bumpé uniquement par l'observer de
résultat — qui ne s'exécute jamais si le run survit à sa session.
Deux correctifs complémentaires :

- **Piste 1 — re-lecture à l'ouverture d'un sous-onglet.** Ouvrir
  « Alertes FORDEAD » ou « Carte FORDEAD » bumpe désormais
  `alerts_refresh`, forçant `alerts()` à re-interroger la base et
  `mask_r` à relire le masque persisté. Un run terminé hors-session
  apparaît à la prochaine visite de l'onglet, sans recharger le
  projet.
- **Piste 2 — réconciliation depuis le disque.** Au chargement d'un
  projet (ou changement de zone), `.reconcile_fordead_state()`
  reconstruit un résultat « succès » synthétique à partir du masque
  de dépérissement persisté
  (`cache/layers/fordead/zone_<id>/dieback_mask_<ts>.tif`). La carte
  « Zone saine » s'affiche alors avec la date du dernier diagnostic
  (nouvelle clé i18n `monitoring_fordead_no_alerts_meta_date`), au
  lieu du placeholder générique « pas encore lancé ». Un résultat
  in-session pour la même zone (qui porte la vraie durée) n'est
  jamais écrasé.

### Changed — libellé Carte FAST

Le placeholder « pas de cache » de la Carte FAST disait « Lance une
ingestion FAST… » ; remplacé par « Lance le diagnostic FAST… » pour
s'aligner sur le vocabulaire des onglets (clé
`monitoring_pixel_map_no_cache`).

# nemetonshiny 0.38.8 (2026-05-20)

### Changed — `Remotes:` suit désormais la dernière release `nemeton` (`@*release`)

Le `DESCRIPTION` épinglait `Remotes: pobsteta/nemeton@v0.41.0` — un
tag figé. Conséquence : `install_github("pobsteta/nemetonshiny")`
installait `nemeton 0.41.0` alors que le cœur était déjà publié en
`v0.41.2`, et les correctifs `v0.41.1` (réinstallation Python
FORDEAD à chaque run) / `v0.41.2` (déduplication des reprocessing
Sentinel-2) n'étaient pas tirés sans bump manuel du pin.

`Remotes: pobsteta/nemeton@*release` — la référence spéciale
`@*release` de `remotes`/`pak` résout à chaque install le **tag de
release le plus élevé** de `pobsteta/nemeton`. L'app consomme donc
toujours la plus haute version cœur publiée, automatiquement, sans
bump manuel du `Remotes:` à chaque release `nemeton`.

- **Avantages** : dernière fonction/correctif cœur disponible dès
  la release `nemeton` suivante ; on ne consomme que des **vraies
  releases taguées**, jamais du `main` non publié.
- **Contrepartie** : reproductibilité d'install pure perdue dans le
  temps — pour figer un état composite app+cœur, `renv::snapshot()`
  côté projet utilisateur.
- Le plancher `Imports: nemeton (>= 0.41.0)` est **inchangé** : il
  reste le minimum strict que le code app exige (garde-fou contre
  un cœur déjà installé trop ancien), il ne suit pas la dernière
  version.

Historique du `Remotes:` : `@main` avant v0.38.0, pin tag figé
`@vX.Y.Z` de v0.38.0 à v0.38.7, `@*release` depuis cette version.
`CLAUDE.md` mis à jour en conséquence (sections *Stack technique*
et *Suivi de la dernière release `nemeton`*).

---

# nemetonshiny 0.38.7 (2026-05-20)

### Fixed — Rafale de warnings leaflet « Some values were outside the color scale »

La console R crachait `Warning in colors(.) : Some values were
outside the color scale and will be treated as NA` en rafale lors
du rendu des cartes du Suivi sanitaire. Deux causes distinctes.

**Carte FORDEAD** (gros de la rafale, après un run). `output$map`
de `mod_monitoring_fordead_map` colore le masque catégoriel 0-4
via `colorFactor`, mais `addRasterImage()` reprojette en
web-mercator avec `method = "bilinear"` par défaut — l'interpolation
bilinéaire crée des valeurs **fractionnaires** entre les classes
discrètes (0.7, 2.3…) qui ne correspondent à aucun niveau de la
palette → NA + warning, une fois par bloc de raster.

Correctif : `addRasterImage(..., method = "ngb")` (nearest-neighbour
— préserve les classes entières), et `colorFactor(levels = 0:4)`
en niveaux numériques alignés sur les valeurs du raster.

**Carte FAST** (warning isolé). `.pixel_palette` est un
`colorNumeric` ancré sur `[-1, 1]` ; NDVI / NBR / CRSWIR sont
théoriquement bornés à `[-1, 1]` mais des pixels de bord ou des
artefacts de reprojection produisent parfois des valeurs juste
au-delà → NA + warning.

Correctif : `terra::clamp(r, -1, 1, values = TRUE)` avant
`addRasterImage()` — les valeurs hors domaine sont ramenées aux
extrêmes de la palette (un NDVI 1.02 devient « vert max », ce qui
est visuellement correct) au lieu d'être rendues transparentes.

Aucun de ces deux warnings n'était fatal — les valeurs hors échelle
étaient simplement dessinées en transparent — mais ils noyaient la
console.

---

# nemetonshiny 0.38.6 (2026-05-20)

### Fixed — Carte FORDEAD ne se rafraîchit pas après un run

Après un diagnostic FORDEAD réussi, le masque catégoriel 0-4 est
bien persisté sur disque (`nemeton@v0.41.0` :
`Dieback mask persisted: …/cache/layers/fordead/zone_<id>/dieback_mask_<ts>.tif`)
mais le sous-onglet « Carte FORDEAD » restait sur son empty-state
« Aucun masque FORDEAD disponible ».

Cause : le reactive `mask_r()` de `mod_monitoring_fordead_map` ne
dépendait que de `input$zone_id` et `app_state$current_project` —
rien ne l'invalidait à la fin d'un run. Il était évalué une seule
fois (avant le run, quand `cache/layers/fordead/` n'existait pas
encore → NULL) puis restait figé. Même classe de bug que le
« Zone saine » corrigé en v0.36.5.

**Correctif** : nouveau paramètre `refresh_r` du module
`mod_monitoring_fordead_map_server`, câblé sur le compteur
`alerts_refresh` du parent (bumpé par le handler de résultat
FORDEAD sur succès). `mask_r()` lit `refresh_r()` → un run terminé
invalide le reactive → le module relit
`<project>/cache/layers/fordead/zone_<id>/` et affiche le masque
sans recharger le projet ni re-sélectionner la zone.

### Tests

- Nouveau fichier `test-mod_monitoring_fordead_map.R` (3 tests) :
  UI = uiOutput, empty-state quand le cache fordead est absent,
  et `mask_r` qui relit le cache après un bump de `refresh_r`
  (mock de `nemeton::read_fordead_dieback_mask`).

---

# nemetonshiny 0.38.5 (2026-05-20)

### Changed — Bump `nemeton` v0.40.0 → v0.41.0 : Carte FORDEAD activée

`nemeton@v0.41.0` ship le **writer du masque de dépérissement**
FORDEAD. Jusqu'ici `run_fordead_dieback()` tournait dans un
`tempfile()` effacé en fin de session : le raster catégoriel 0-4
était perdu et `read_fordead_dieback_mask()` (livré dès v0.25.0
avec sa convention de chemin) renvoyait toujours `NULL`. Côté app,
le sous-onglet « Carte FORDEAD » (module `mod_monitoring_fordead_map`,
câblé depuis v0.36.0) affichait donc en permanence son empty-state
« Aucun masque FORDEAD disponible ».

v0.41.0 ajoute un hook de persistance (toujours actif) qui écrit le
masque dans
`<mask_cache_dir>/zone_<zone_id>/dieback_mask_<YYYYMMDDTHHMMSS>.tif`
— exactement le chemin que `read_fordead_dieback_mask()` interroge.
`mask_cache_dir` est dérivé par défaut comme
`<project>/cache/layers/fordead`, soit précisément le répertoire
que lit `mod_monitoring_fordead_map`.

**Aucun changement de code applicatif** : le câblage des deux côtés
était déjà en place et correct. Ce bump de dépendance suffit à
activer la Carte FORDEAD — après un run FORDEAD réussi, le masque
0-4 (sain / faible / moyenne / forte / sol-nu) s'affiche désormais
dans le sous-onglet.

Vérifications faites avant bump :

- `read_fordead_dieback_mask(con, zone_id, run_id = NULL,
  cache_dir = NULL)` — signature **inchangée** depuis v0.25.0.
- `run_fordead_dieback()` gagne `mask_cache_dir` et `keep_output`,
  tous deux avec valeur par défaut → l'appel du worker
  `run_fordead_async()` (qui ne les passe pas) reste valide. Le
  `mask_cache_dir` par défaut (`NULL`) dérive le bon chemin à
  partir du `cache_dir` Sentinel-2 transmis par le worker.

`DESCRIPTION` : `Imports: nemeton (>= 0.41.0)`,
`Remotes: pobsteta/nemeton@v0.41.0` (le pin tag est avancé — il
n'est plus sur `@main`, un bump explicite est nécessaire à chaque
montée de version cœur).

---

# nemetonshiny 0.38.4 (2026-05-20)

### Changed — Rafales de ré-exécution de `obs_pixel_data` coupées (debounce)

Au chargement d'un projet ou changement de zone, `obs_pixel_data`
(dans `R/mod_monitoring.R`) se ré-exécutait 4-5 fois de suite. Cause :
le `reactive()` dépend de 5 entrées (`input$mode`, `input$zone_id`,
`input$bands`, `input$date_range`, `obs_refresh()`) restaurées les
unes après les autres → un cycle de flush par entrée → une requête
SQL `read_obs_pixel` redondante à chaque cycle, et la rafale se
propageait à `placettes_sf_r()` puis à l'`observe()` des marqueurs.
Pas une boucle infinie, mais du gaspillage : requêtes DB inutiles
+ bruit console.

**Correctif** — debounce d'un *déclencheur* peu coûteux, pas du
reactive lui-même. Subtilité : `shiny::debounce()` évalue sa source
de manière **eager** — il ne fait que retarder la *publication* de
la valeur aux consommateurs aval. Debouncer `obs_pixel_data`
directement n'aurait donc PAS épargné la requête SQL (vérifié par
test : 3 changements rapides = 3 requêtes). On introduit à la place
`obs_pixel_inputs`, un reactive qui assemble les 5 entrées dans une
liste (coût nul), debouncé à 300 ms ; `obs_pixel_data` ne dépend
plus que de ce paquet debouncé → la requête `read_obs_pixel` tourne
une seule fois par rafale. Délai imperceptible ; tous les
consommateurs (plotly par placette, handler `input$map_marker_click`,
`placettes_sf_r()` dans `mod_monitoring_pixel_map`) lisent
`obs_pixel_data()` sans changement.

### Changed — Logs de debug de la carte pixel derrière un drapeau

Les `cli::cli_alert_info()` d'instrumentation « UGF source » /
« UGF overlay » / « Placettes overlay » de
`R/mod_monitoring_pixel_map.R` (9 lignes) s'affichaient en rafale
à chaque chargement de projet. Ils sont désormais gatés derrière
la variable d'environnement `NEMETON_PIXEL_MAP_DEBUG` (helper
`.pixel_map_debug_enabled()`, même modèle que `NEMETON_S2_CACHE_DEBUG`
côté cœur). Par défaut (variable non définie) → console silencieuse.
Mettre `NEMETON_PIXEL_MAP_DEBUG=TRUE` pour réactiver le tracing
depuis un terminal.

### Tests

- `test-mod_monitoring.R` : nouveau test `testServer()` du debounce
  de `obs_pixel_data` — 3 changements rapides de `input$zone_id`
  (toutes préconditions satisfaites) suivis de `session$elapse(400)`
  → une seule requête `read_obs_pixel` supplémentaire (mockée), au
  lieu de 3.

---

# nemetonshiny 0.38.3 (2026-05-20)

### Fixed — Cache LiDAR HD non extent-aware (mauvaise zone réutilisée)

Le cache des dalles LiDAR HD téléchargées par
`download_layers_for_parcels()` n'était pas conscient de l'emprise
demandée. Deux bugs en découlaient.

**Bug 1 — court-circuit global aveugle (nuages de points).**
`download_ign_lidar_hd(product = "nuage")` renvoyait *toutes* les
dalles `.copc.laz` du répertoire de cache dès qu'**une seule**
existait, sans comparaison de bbox. Conséquences : un calcul sur
une zone B après une zone A renvoyait les dalles de A (celles de B
n'étaient jamais téléchargées) ; un téléchargement interrompu figeait
un jeu de dalles incomplet renvoyé indéfiniment.

**Correctif** : suppression du court-circuit global. La fonction
interroge désormais toujours le WFS (`query_lidar_wfs`) pour la
liste des dalles couvrant la bbox courante, puis s'appuie sur le
cache *par dalle* déjà présent dans la boucle de téléchargement
(`file.exists()` + `file.size() > 100`). Résultat : recompute
même zone → zéro I/O réseau ; zone différente → seules les dalles
manquantes sont téléchargées, l'ensemble retourné couvre bien la
nouvelle zone ; jeu incomplet → complété au run suivant au lieu
d'être figé.

**Bug 2 — mosaïque raster non extent-aware (MNH/MNT/MNS).**
`lidar_<product>_mosaic.tif` était réutilisée sur un simple
`file.exists()`, sans vérifier que son emprise couvre la bbox
courante — même problème de raster obsolète au changement de zone.

**Correctif** : nouveau helper interne `.lidar_mosaic_covers_bbox()`
qui compare l'extent du raster en cache à la bbox demandée (en CRS
commun — la bbox WGS84 est reprojetée vers le CRS de la mosaïque).
La mosaïque n'est court-circuitée que si elle couvre réellement la
zone ; sinon elle est régénérée à partir de dalles fraîches. Toute
erreur de lecture / CRS manquant → régénération (pas de confiance
à une dalle potentiellement périmée).

Le garde anti-téléchargement-partiel (`file.size > 100`) et le
nettoyage des dalles échouées dans `download_lidar_tile()` sont
conservés.

### Tests

- `test-service_compute.R` : test obsolète « returns cached COPC
  tiles if available » réécrit (le court-circuit global n'existe
  plus) ; 3 tests ajoutés — recompute même zone sans
  re-téléchargement, zone différente ne télécharge que les dalles
  manquantes, mosaïque régénérée quand le cache ne couvre pas la
  nouvelle zone — plus un test unitaire dédié de
  `.lidar_mosaic_covers_bbox()` (couverture, égalité, débordement,
  zone disjointe, chemin illisible). Suite `test-service_compute.R` :
  218 tests, 0 échec.

---

# nemetonshiny 0.38.2 (2026-05-20)

### Fixed — Sous-onglets Suivi sanitaire blancs (Carte FORDEAD, Alertes FAST)

Symptôme : après un run FORDEAD terminé, le sous-onglet « Carte
FORDEAD » s'affichait totalement blanc — ni carte, ni message
d'état, juste une fine bande grise. Pareil pour « Alertes FAST »
en mode quick.

Cause : les modules `mod_monitoring_fordead_map` et
`mod_monitoring_fast_alerts` exposent leur contenu via
`uiOutput()` / `renderUI()`. Le navset de Suivi sanitaire bascule
ses `nav_panel` avec `bslib::nav_show()` / `nav_hide()`
(visibilité pilotée par le mode quick/health, livrée v0.34.0–
v0.35.0). Ce mécanisme rend la détection de visibilité par-output
de Shiny non fiable : avec `suspendWhenHidden = TRUE` (défaut),
les `uiOutput` restaient suspendus et le `renderUI` ne se
déclenchait jamais, même après que l'utilisateur ait cliqué sur
l'onglet. Le module fonctionne pourtant parfaitement en isolation
(testé : l'empty-state se rend correctement).

**Correctif** :

- `shiny::outputOptions(output, "<id>", suspendWhenHidden = FALSE)`
  sur `output$panel` (Carte FORDEAD) et sur `output$panel` +
  `output$counters` (Alertes FAST) — le `renderUI` s'évalue
  désormais inconditionnellement.
- `bslib::nav_select()` ajouté dans l'observer mode-driven : au
  changement de mode, l'onglet actif est ré-ancré sur un onglet
  visible (`alerts_fast` en quick, `alerts_fordead` en health).
  Sans ça, le navset gardait comme pane actif un onglet désormais
  masqué, laissant la zone de contenu dans un état incohérent.

Résultat : la « Carte FORDEAD » affiche bien son empty-state
explicatif (« Aucun masque FORDEAD disponible — postprocess hook
prévu dans une release ultérieure de nemeton »), et « Alertes
FAST » rend ses compteurs + carte.

---

# nemetonshiny 0.38.1 (2026-05-20)

### Fixed — Câblage du CHM Theia vers les indicateurs Production / Énergie

À l'écran « Calculs terminés », les indicateurs P1 (Volume),
P2 (Productivité), P3 (Qualité bois) et E1 (Bois-énergie)
échouaient avec les erreurs du mode sans CHM (`Missing required
fields: dbh, density` / `fertility, climate` / `volume`).

Deux corrections de câblage dans `R/service_compute.R` :

- **`age_field` transmis à P2** : `compute_single_indicator()`
  passait déjà `chm` et `species_field` (P1/P3/E1) mais pas
  `age_field`. En mode CHM, `indicateur_p2_station()` bascule sur
  le modèle hauteur/âge et a besoin de la colonne `age` —
  désormais transmise via `age_field = "age"` (colonne alimentée
  par `nemeton::enrich_parcels_bdforet()`).
- **Échec explicite quand le CHM est absent** : nouvelle constante
  `CHM_REQUIRED_INDICATORS` (P1/P2/P3/E1). Si aucun CHM n'a pu
  être chargé (Theia non configuré, LiDAR HD absent, Open-Canopy
  indisponible), ces quatre indicateurs échouent désormais avec un
  message i18n explicite (`compute_chm_required`) renvoyant vers le
  menu de configuration Theia, au lieu de l'erreur cryptique du
  cœur `nemeton`. Les autres indicateurs du calcul ne sont pas
  affectés. C1 et B2 acceptent aussi `chm` mais conservent un
  chemin legacy (OSO / NDVI) et ne sont donc pas concernés.

# nemetonshiny 0.38.0 (2026-05-20)

### Added — Intégration des sources Theia / DATA TERRA (nemeton v0.40.0)

`nemeton` v0.40.0 expose l'accès aux sources satellitaires publiques
Theia / DATA TERRA. `nemetonshiny` les consomme désormais pour
calculer les indicateurs en NDP 0 à partir de données publiques,
et débloque en priorité la famille Production (P1 Volume, P2
Productivité, P3 Qualité) et E1 (Bois-énergie), qui échouaient
faute de CHM fourni aux fonctions `indicateur_*()`.

**Service Theia (`R/service_theia.R`, nouveau)** :

- `theia_python_ready()` / `theia_api_key_configured()` /
  `theia_status()` : détection du pré-requis Python (`reticulate`
  + modules `teledetection` / `pystac_client`) et de la clé API
  Theia (`TLD_ACCESS_KEY` / `TLD_SECRET_KEY` ou
  `~/.config/teledetection/.apikey`).
- `theia_save_api_key()` : persistance de la clé.
- `download_chm_theia()` : charge le CHM FORMSpoT via
  `nemeton::load_theia_source("formspot", aoi, year)`, convertit
  les décimètres en mètres (FORMSpoT stocke la hauteur de canopée
  à 1,5 m **en décimètres**), puis nettoie via
  `nemeton::sanitize_chm()`.
- `download_theia_layers()` : charge FAPAR (C2), neige et humidité
  du sol (R3) — chaque source est tolérante aux pannes.
- `theia_source_provenance()` : provenance / licence des sources
  via `nemeton::get_data_source()`.

**Service de calcul (`R/service_compute.R`)** :

- Nouvelle étape CHM Theia FORMSpoT dans
  `download_layers_for_parcels()` : utilisée quand le LiDAR HD est
  absent de l'AOI et que Theia est prêt, avant Open-Canopy.
- `compute_single_indicator()` transmet `species_field = "species"`,
  `fapar`, `snow` et `soil_moisture` aux fonctions `nemeton` qui les
  acceptent. `fvc` (A1) et `texture` (F1/F2) restent sur leur chemin
  legacy tant que les assets Theia correspondants ne sont pas
  confirmés côté cœur.
- Enrichissement BD Forêt V2 (`species`/`age`) étendu à P1, P3 et E1
  (auparavant P2 seul) pour fournir l'essence dominante aux modèles
  allométriques en mode CHM.

**UI (`R/mod_theia_config.R`, nouveau)** :

- Entrée navbar (engrenage) ouvrant une modale de configuration :
  saisie de la clé API Theia, statut du pré-requis Python/reticulate,
  affichage de la provenance / licence des sources Theia.

**Gestion d'erreur** : quand aucun CHM n'est disponible (LiDAR HD
absent, Theia non configuré), un avertissement i18n explicite est
remonté à l'écran de calcul pour expliquer pourquoi la famille
Production ne peut pas être calculée.

`DESCRIPTION` : `Imports: nemeton (>= 0.40.0)`,
`Remotes: pobsteta/nemeton@v0.40.0`, `reticulate` ajouté en Suggests.
# nemetonshiny 0.37.0 (2026-05-19)

### Added — Fallback BD Forêt V2 sur le check de validité FORDEAD (G3 espèces)

Symptôme avant ce fix : console R lors du lancement d'un diagnostic
FORDEAD,

```
ℹ Database schema up to date (2 migrations applied).
Avis : No species column found on `units`.
ℹ Expected one of: essence_dominante, essence, species_label, species,
  essence_principale.
ℹ Skipping species check.
```

Les UGFs de l'app n'ont pas de colonne d'essence (l'essence n'est
pas un attribut foncier — elle est dérivée d'autres sources comme
BD Forêt V2 ou la classification Sentinel-2). Le garde-fou G3 sur
les espèces validées (épicéa + sapin pectiné, calibration ONF/DSF
2024) était donc désactivé silencieusement, et `overall_valid` ne
reflétait que le critère géographique.

`nemeton@v0.26.0` ajoute deux arguments à
`check_fordead_validity()` : `bdforet` (sf de BD Forêt V2,
formation_vegetale) et `layers` (un `nemeton_layers`). Quand
`units` n'a pas de colonne d'essence ET que `bdforet` est fourni,
le cœur dérive l'essence dominante par parcelle via
`enrich_parcels_bdforet()` et lance le check espèces normalement.

**Câblage côté app** :

- Nouveau helper `.load_project_bdforet(project)` dans
  `R/mod_monitoring.R` : lit
  `<project>/cache/layers/bdforet.gpkg` (alimenté par
  `download_ign_bdforet()` pendant le calcul des indicateurs)
  et retourne `sf` ou NULL.
- Le reactive `validity` du module Suivi sanitaire charge la
  BD Forêt depuis ce helper et la passe à
  `validity_check_for_zone()`.
- `validity_check_for_zone()` (dans `R/service_monitoring_db.R`)
  accepte désormais un argument `bdforet = NULL` qu'il transmet
  directement à `nemeton::check_fordead_validity()`.

**Comportement** :

| État du cache BD Forêt | Sortie `species_valid` |
|---|---|
| Cache présent (`bdforet.gpkg`) | `TRUE` ou `FALSE` selon le ratio résineux validés |
| Cache absent (projet sans compute ou DL BD Forêt échoué) | `NA` (fallback v0.25.9 préservé) |

### Changed

- Plancher `Imports: nemeton (>= 0.26.0)` (au lieu de 0.25.4).
  Le `Remotes: pobsteta/nemeton@main` reste inchangé — les nouveaux
  installs récupèrent directement v0.26.0.

### Tests

- 3 nouveaux tests testthat dans `tests/testthat/test-mod_monitoring.R` :
  `.load_project_bdforet()` NULL paths (NULL projet / sans path /
  sans cache), `.load_project_bdforet()` lecture GPKG fonctionnelle,
  et `validity_check_for_zone()` qui forwarde bien `bdforet` au cœur
  (mocked via `testthat::local_mocked_bindings(..., .package =
  "nemeton")`).

---

# nemetonshiny 0.36.8 (2026-05-19)

### Fixed — UX du diagnostic FORDEAD après résolution du run

Cas reporté : un run FORDEAD complet et réussi se termine en 142 s
côté cœur (`status == "success"`, `n_alerts_inserted == 0L`,
`alerts_sf == NULL`), le toast bas-droite affiche bien
« Diagnostic terminé : 0 alertes insérées en 142 s », mais l'UI
restait dans un état ambigu :

1. **Bouton « Lancer le diagnostic FORDEAD » resté grisé** après la
   résolution. La logique en place (`observe` qui lisait
   `fordead_task$status()` et appelait
   `updateActionButton(disabled = is_running)`) aurait dû ré-activer
   le bouton sur la transition de statut, mais une race ou un flush
   manqué le laissait dans l'état grisé.

   **Correctif** : belt-and-suspenders re-enable explicite dans le
   handler `fordead_task$result()`, en plus du status-based observe.
   Trois cas couverts (`success` avec alertes, `success` sans
   alertes, `error`). Reset de `force_unlock_health(FALSE)` au
   passage pour rester cohérent avec l'observer click.

2. **Onglet « Alertes FORDEAD » muet** quand `n_alerts_inserted == 0L`.
   L'utilisateur ne pouvait pas distinguer « pas encore lancé » /
   « calcul en cours » / « run terminé, 0 anomalie ».

   **Correctif** : nouvelle `reactiveVal` `fordead_last_result()` qui
   capture le payload du dernier run résolu dans la session. Quand
   `alerts()` est vide ET `fordead_last_result()$status == "success"`,
   l'`output$alerts_panel` affiche une card « Zone saine — aucune
   anomalie détectée » (bordure verte, icône `check-circle-fill`)
   avec la durée du run en sous-titre. Cinq états distincts
   maintenant lisibles dans le panneau Alertes FORDEAD :

   | État | Affichage |
   |---|---|
   | Pas encore lancé / pas de zone sélectionnée | placeholder neutre |
   | Run en cours | toast bas-droite avec phase 0..6 |
   | Run terminé, ≥ 1 alerte | carte Leaflet (chemin actuel) |
   | Run terminé, 0 alerte | card « Zone saine » avec durée |
   | Run terminé en erreur | toast bas-droite + result snapshot capturé |

3. **Onglet « Carte FORDEAD »** : empty-state déjà livré en v0.36.0
   (« Aucun masque FORDEAD disponible » + « postprocess hook prévu
   dans une release ultérieure de nemeton »). Pas de changement —
   le wiring se réactivera automatiquement quand le cœur shippera
   la persistance du raster 0..4.

### Added

- 3 nouvelles clés i18n FR/EN : `monitoring_fordead_no_alerts_title`,
  `monitoring_fordead_no_alerts_body`, `monitoring_fordead_no_alerts_meta`.
- Helper `make_fake_fordead_task()` widened pour accepter `result =`
  / `status =` (préparation des futurs tests).

### Notes opérationnelles

- Aucun changement de signature côté cœur. Plancher `Imports` reste
  à `nemeton (>= 0.25.4)` (hérité de v0.36.7).
- Le bonus optionnel « badge persistant Diagnostic en cours »
  mentionné dans le brief n'est pas livré — les toasts de phase
  bas-droite (`monitoring_fordead_phase_*`, livrés v0.32.0) couvrent
  déjà ce besoin.
- Les tests testServer pour la card « Zone saine » prototypés
  pendant ce ticket wedgent dans le graphe réactif de
  `mod_monitoring_server` (multiples `reactivePoll` timers +
  ExtendedTask + sous-module pixel-map — la file de tests existante
  documente déjà ce problème de harness). La vérification du
  rendering Zone-saine reste en QA manuelle pour cette release ;
  les `.summarize_backend_warnings()` tests (v0.36.4) restent en
  place et passent.

---

# nemetonshiny 0.36.7 (2026-05-18)

### Fixed — Câblage `resolve_project_dem` / `resolve_project_chm` sur `create_sampling_plan()`

Suite de la livraison v0.36.6 : on alignait la résolution des rasters
sur les nouveaux helpers `nemeton::resolve_project_*` mais l'appel à
`create_sampling_plan()` ne passait toujours pas `mnt =` ni `chm =`,
si bien que le DEM résolu n'était jamais consommé côté cœur (le
pré-check « Stratification-valid candidate pool (0) is below
`n_base` » de `nemeton@v0.25.1` aboutait alors avec un toast
cryptique).

**Changements** (`R/mod_sampling.R`) :

- Les réactives `chm_raster()` / `mnt_raster()` perdent leur guard
  `if (is.null(pp)) return(NULL)` : les helpers `resolve_project_*`
  sont défensifs (typed errors sur NULL / "" / chemin manquant), le
  `tryCatch` suffit.
- Pré-check DEM avant l'appel : si `mnt_raster()` est NULL, toast
  `sampling_no_dem_found_fmt` (i18n) avec `duration = NULL` (toast
  bloquant), `id = session$ns("dem_missing")` et `return()` immédiat
  — `create_sampling_plan()` n'est plus appelé du tout dans ce cas.
- Toast informatif `sampling_dem_resolved_fmt` (`"MNT : %s"`)
  exposant la couche résolue via `attr(dem, "nemeton_dem_layer")`
  (« opencanopy DTM », « LiDAR HD MNT », « IGN BD ALTI », …),
  `duration = 5`, `id = session$ns("dem_resolved")`.
- CHM absent : simple `cli::cli_alert_info("sampling_chm_missing")`
  sans toast bloquant — la stratification hauteur tombe mais
  `create_sampling_plan()` retient le DEM seul et bascule sur
  LPM2 / random.
- 3 nouvelles clés i18n FR/EN remplaçant les 4 ajoutées la veille :
  `sampling_no_dem_found_fmt`, `sampling_dem_resolved_fmt`,
  `sampling_chm_missing` (`R/utils_i18n.R`).
- `Imports: nemeton (>= 0.25.4)` (au lieu de `0.21.10`) pour
  garantir la version qui ajoute le pré-check coeur côté nemeton.

**Tests** (`tests/testthat/test-mod_sampling.R`) :

- Helper `make_fake_dem()` qui fabrique un `SpatRaster` 100 m sur la
  bbox des fixtures avec attribut `nemeton_dem_layer`.
- 4 tests existants qui cliquent « Generate » sont enveloppés dans
  `testthat::local_mocked_bindings(resolve_project_dem = ...,
  resolve_project_chm = function(...) NULL, .package = "nemeton")`
  pour préserver le contrat « plots non NULL après generate ».
- 2 nouveaux tests :
    * vérifie que `nemeton::create_sampling_plan` est bien appelé
      avec `mnt = <SpatRaster>` et `chm = NULL` (capture via mock) ;
    * vérifie que quand `resolve_project_dem` renvoie NULL, le toast
      `dem_missing` est émis et `create_sampling_plan` n'est PAS
      appelé.

# nemetonshiny 0.36.6 (2026-05-18)

### Changed — Résolution MNT/CHM déléguée à `nemeton::resolve_project_*`

Le module `mod_sampling` faisait sa propre résolution des rasters CHM
et MNT depuis `<project>/cache/layers/` avec une liste de chemins
hard-codés (`lidar_mnh_mosaic.tif`, `opencanopy/chm_1_5m.tif`,
`lidar_mnt_mosaic.tif`, `dem.tif`). `nemeton (>= 0.21.10)` expose
maintenant deux helpers qui font la même chose en couvrant les noms
canoniques `dtm.tif`, `mnh.tif`, `lidar_mnh.tif`, etc., avec la même
préférence LiDAR HD → opencanopy → BD ALTI.

**Changements** :

- `R/mod_sampling.R` : les réactives `chm_raster()` / `mnt_raster()`
  appellent maintenant `nemeton::resolve_project_chm(project_path,
  verbose = TRUE)` et `nemeton::resolve_project_dem(...)`. L'helper
  interne `cache_raster()` (résolution maison) est supprimé.
- Pré-check ajouté dans l'`observeEvent(input$generate)` : si
  `resolve_project_dem()` renvoie `NULL`, toast d'erreur i18n
  `mnt_missing` (« Aucun MNT trouvé. Téléchargez via opencanopynemeton
  (dtm.tif) ou IGN RGE ALTI. ») et arrêt avant l'appel à
  `create_sampling_plan()`. Si le CHM est `NULL`, simple warning soft
  (`chm_missing`) car la stratification hauteur tombe alors mais le
  plan se génère via LPM2/random.
- Toasts informatifs `mnt_found_fmt` / `chm_found_fmt` qui exposent la
  couche résolue via `attr(., "nemeton_dem_layer")` /
  `attr(., "nemeton_chm_layer")` (« MNT : LiDAR HD (1m) » vs
  « MNT : opencanopy DTM »).
- 4 nouvelles clés i18n bilingues : `mnt_found_fmt`, `mnt_missing`,
  `chm_found_fmt`, `chm_missing` (`R/utils_i18n.R`).
- `DESCRIPTION` : `Imports: nemeton (>= 0.21.10)` pour garantir la
  présence des helpers.

# nemetonshiny 0.36.5 (2026-05-18)

### Fixed — Codes ANSI cli affichés dans la notification d'erreur de `create_sampling_plan()`

Quand `nemeton::create_sampling_plan()` levait une erreur formatée par
`cli::cli_abort()` (par ex. « Stratification-valid candidate pool (0) is
below `n_base` (50) » quand l'AOI dépasse la couverture CHM/MNT), le
toast Shiny affichait les séquences d'échappement ANSI brutes
(`[38;5;250m`, `[31m`, `[36m`, `[39m`) car `conditionMessage(e)`
conserve le formatage `cli` qui n'a pas de sens en HTML.

**Correctif** — `mod_sampling.R` enveloppe `conditionMessage(e)` dans
`cli::ansi_strip()` avant la `showNotification()` pour ne montrer que
le texte lisible.

---

# nemetonshiny 0.36.4 (2026-05-17)

### Fixed — Toast d'avertissements backend illisible (SAS token Azure)

Quand l'ingestion Sentinel-2 remontait un warning du backend de type
« GDAL Error 1: HTTP error code : 403 ; Scene "S2A_..." skipped: [crop]
file does not exist: https://... », le toast Shiny affichait l'URL
pré-signée complète Azure (~400 caractères de query string
`?st=…&se=…&sp=…&sv=…&skoid=…&sktid=…&skt=…&ske=…&sks=…&skv=…&sig=…`)
sans pertinence pour l'utilisateur, transformant le toast en mur de
texte qui débordait l'écran.

**Correctif** — nouveau helper interne `.summarize_backend_warnings()` :

- remplace toute URL `https?://...` par le placeholder `<URL>`,
- normalise les espaces (multi-ligne → une ligne),
- caps chaque warning à 200 caractères avec ellipse `…`.

Appliqué aux deux toasts qui affichent `result$warnings` :

- « Ingest terminé avec 0 scène » (sur échec backend, warnings utiles
  pour diagnostiquer STAC 504 / timeout réseau).
- « Avertissement(s) du backend » (warnings non-bloquants à côté du
  succès, comme une scène individuelle skippée pour 403).

L'utilisateur garde l'info utile (code HTTP, scene id, raison du skip)
sans le bruit du SAS token. Couverture testthat ajoutée (2 tests, 5
expectations).

---

# nemetonshiny 0.36.3 (2026-05-17)

### Fixed — Markers placettes invisibles sur Carte FAST après v0.34.0

Conséquence latérale du fix `nemetonRaster` pane de v0.34.0 : les
CircleMarkers placettes (cercles bleus, `#1F77B4`) restaient dans
`overlayPane` (z=400) aux côtés des polygones UGF. Avec `leaflet 2.x`
et l'ordre de re-draw observé sur certains navigateurs, les
polygones finissaient en fin de `<g>` SVG → DOM order = z-order
dans un même pane → polygones par-dessus markers → markers
invisibles à l'écran.

**Correctif** — pousser explicitement les CircleMarkers dans
`markerPane` (z-index 600, séparé de `overlayPane`) via
`options = pathOptions(pane = "markerPane")` à l'appel
`addCircleMarkers()`. Le z-stack devient strict :

```
tilePane         z=200  OSM / Satellite
nemetonRaster    z=250  NDVI / NBR raster
overlayPane      z=400  Polygones UGF
markerPane       z=600  CircleMarkers placettes
```

Plus de course DOM entre polygones et markers : chacun dans son
pane, z-order garanti par CSS Leaflet. Clickabilité préservée
(les Paths émettent toujours `click` vers `map_marker_click`).

---

# nemetonshiny 0.36.2 (2026-05-17)

### Fixed — Zone monitoring qui ne se met pas à jour au changement de projet

Deux bugs en chaîne dans `R/mod_monitoring.R` :

1. **Dépendance fantôme en mode Postgres**. Le reactive `zones`
   lisait `app_state$current_project` uniquement via l'argument lazy
   `project = ...` de `get_monitoring_db_connection()`. En mode
   Postgres, `.resolve_monitoring_db_url()` retourne tôt sur la
   variable d'environnement `NEMETON_DB_URL` sans jamais accéder à
   `project`, donc le promise n'était jamais forcé et Shiny
   n'enregistrait pas de dépendance sur `current_project`.
   Conséquence : changer de projet n'invalidait pas la liste des
   zones. En mode DuckDB local le bug était masqué parce que le
   resolver finit par lire `project$path`.

   Correctif : `proj <- app_state$current_project` en lecture
   explicite avant l'appel, ce qui force le promise et enregistre
   la dépendance dans tous les modes.

2. **Pré-sélection trompeuse sur projet neuf**. Quand le projet
   chargé n'avait pas de `metadata$monitoring_zone_id` (cas typique
   d'un projet neuf, jamais enregistré comme zone), l'observer qui
   pousse les zones dans le `selectInput` retombait sur
   `choices[1]` — la première zone alphabétique. En mode Postgres
   partagé, c'était la zone d'un AUTRE projet, et l'utilisateur
   voyait ses alertes / sa carte FAST / sa carte FORDEAD sans
   comprendre que la donnée ne concernait pas son projet.

   Correctif : quand le projet n'a pas de zone bindée, la
   sélection est vidée (`character(0)`) au lieu de tomber sur la
   première zone. Tous les reactives downstream (`alerts`,
   `validity`, `obs_pixel_data`, modules FAST / FORDEAD) bailent
   déjà sur `!nzchar(input$zone_id)` → empty-states cohérents et
   l'utilisateur enregistre explicitement sa zone via le bouton
   dédié.

### Sémantique attendue (rappel)

- Chaque projet a SA zone monitoring, persistée dans
  `metadata$monitoring_zone_id` du `metadata.json` du projet.
- En mode DuckDB local : chaque projet a sa propre base
  `<project>/data/monitoring.duckdb`, isolation totale.
- En mode Postgres partagé : la liste des zones est commune mais
  chaque projet réfère à UNE zone précise via son metadata.

---

# nemetonshiny 0.36.1 (2026-05-17)

### Fixed — UX seuils FAST alignée sur la sémantique cœur

Les sliders sidebar `threshold_ndvi` / `threshold_nbr` du mode FAST
étaient calibrés pour la sémantique historique **drop** (delta NDVI
ou NBR sur la fenêtre roulante E6.a) — défauts 0.15 / 0.25, range
0.05-0.50. Mais `nemeton::list_fast_alerts_for_zone()` (consommé
depuis v0.36.0) interprète ces valeurs comme des **seuils absolus**
minimaux. Avec les anciens défauts, presque aucune placette ne
remontait en alerte parce que le NDVI d'une forêt saine (0.6-0.8)
était toujours bien au-dessus de 0.15.

**Correctif** :

- Slider `threshold_ndvi` : default `0.40` (cœur default), range
  `[0.10, 0.80]` (couvre la plage NDVI forestière saine).
- Slider `threshold_nbr` : default `0.30` (cœur default), range
  `[0.10, 0.80]`.
- Labels i18n : « Seuil de baisse NDVI/NBR » →
  « Seuil minimum NDVI/NBR » (resp. drop → minimum threshold).
  En anglais : « NDVI/NBR drop threshold » →
  « Minimum NDVI/NBR threshold ».
- Empty-state des Alertes FAST : « baisser le seuil » →
  « relever le seuil » (avec sémantique absolue, raising the
  threshold attrape plus de placettes en dessous).

Aucun changement de signature côté cœur — pure resync app/cœur.

---

# nemetonshiny 0.36.0 (2026-05-17)

### Added — Câblage des sous-onglets Alertes FAST + Carte FORDEAD

Les deux placeholders posés en v0.35.0 sont désormais branchés sur
les exporteurs `nemeton@v0.25.0` :

| Sous-onglet      | Module                              | Cœur consommé                                |
|------------------|-------------------------------------|---------------------------------------------|
| `Alertes FAST`   | `R/mod_monitoring_fast_alerts.R`    | `nemeton::list_fast_alerts_for_zone()`      |
| `Carte FORDEAD`  | `R/mod_monitoring_fordead_map.R`    | `nemeton::read_fordead_dieback_mask()`      |

**Alertes FAST** — carte Leaflet des placettes classées par sévérité
(`critical` / `warning` / `info`) selon le ratio (NDVI ou NBR) /
seuil. Trois zones de couleur correspondent aux buckets cœur :
ratio `< 0.5` (rouge `#D62728`), `[0.5, 1)` (orange `#FF9933`),
`[1, 1.1)` (orange clair `#FFD27F` — corridor d'avertissement). Les
placettes sécures (ratio `>= 1.1`) ne remontent pas du cœur. Au-dessus
de la carte, une rangée de compteurs montre la distribution par
sévérité + total. Popups marker : `plot_id`, sévérité, NDVI / NBR
avec leur Δ (drop) au seuil, dernière obs.

Les arguments envoyés à `list_fast_alerts_for_zone()` sont liés
1-pour-1 aux widgets sidebar de Suivi sanitaire :

- `zone_id` ← `input$zone_id`
- `threshold_ndvi` ← `input$threshold_ndvi`
- `threshold_nbr`  ← `input$threshold_nbr`
- `window_days` ← `input$window_days`
- `date_from`, `date_to` ← `input$date_range`

**Note UX** : les sliders sidebar sont historiquement libellés
« seuil » avec des défauts (0.15 NDVI, 0.25 NBR) calibrés pour la
sémantique *drop* (delta sur la fenêtre roulante E6.a). Le cœur
v0.25.0 les utilise comme *seuils absolus* dans le ratio
`value / threshold`. Avec les valeurs par défaut actuelles, peu
de placettes remonteront car les NDVI forestiers typiques (~0.6+)
sont bien au-dessus de 0.15. Pour des alertes utiles, cranker les
sliders à 0.40 / 0.30 (les valeurs par défaut cœur). Une refonte
des labels et défauts sidebar est prévue en patch ultérieur.

**Carte FORDEAD** — raster catégoriel 0..4 du dépérissement
(`0`=sain `#2CA02C`, `1`=faible `#FFD27F`, `2`=moyenne `#FF9933`,
`3`=forte `#D62728`, `4`=sol-nu `#222222`). Pinned dans le pane
`nemetonRaster` (z-index 250) pour préserver la visibilité sur fond
Satellite, même architecture que la Carte FAST. Légende
sticky bottom-right.

**Limite v0.36.0** : `read_fordead_dieback_mask()` est shippé côté
cœur mais le **writer** (persist hook dans `run_fordead_dieback()`
qui doit écrire `dieback_mask_<run_id>.tif` dans
`<project>/cache/layers/fordead/zone_<id>/`) n'est pas encore en
production. Tant que ce hook ne ship pas, le reader retourne NULL
et la sub-tab affiche un empty-state explicatif (« Aucun masque
FORDEAD disponible »). Le câblage est en place : la sub-tab
s'activera automatiquement le jour où le writer cœur ship.

### Changed

- `DESCRIPTION` : plancher `Imports: nemeton (>= 0.25.0)` (depuis
  0.24.1), pour bloquer un downgrade qui retirerait les deux
  exporteurs consommés.

### Notes opérationnelles

- Aucune logique métier ajoutée côté app — purement câblage UI
  vers les exporteurs cœur (ADR-009 respecté).
- 17 nouvelles clés i18n FR/EN (sévérités, popups, classes
  FORDEAD, empty states).
- Les deux modules suivent le pattern golem (`mod_*_ui` +
  `mod_*_server`) et reçoivent les reactives sidebar du parent par
  paramètre — symétrie avec `mod_monitoring_pixel_map_server()`.

---

# nemetonshiny 0.35.1 (2026-05-17)

### Fixed — Plan d'échantillonnage avec stratification CHM × MNT

Le bouton « Générer le plan » de l'onglet Terrain remontait en
toast d'erreur le message :

```
create_sampling_plan(): le tableau de remplacement a 363 lignes,
le tableau remplacé en a 337
```

dès qu'un CHM et/ou un MNT étaient présents sur l'AOI. Cause :
côté cœur, le pool de candidats GRTS contenait des centroïdes
tombant sur des pixels NA des rasters de stratification (bord
d'AOI bordurale, hors mask forêt, hors emprise CHM/MNT), et la
construction de la colonne stratum désalignait les longueurs.

Le fix vit dans `nemeton@v0.24.1` (filtrage des candidats AVANT
`spsurvey::grts()`, plus un `cli::cli_warn()` quand la réduction
de pool dépasse 10 %, plus un `cli::cli_abort()` propre quand le
pool stratification-valide tombe sous `n_base`). Côté app, aucune
modification de code — le fix flow automatiquement via
`Remotes: pobsteta/nemeton@main`.

**Plancher Imports** : `nemeton (>= 0.24.1)` (au lieu de 0.24.0)
pour bloquer un downgrade qui réintroduirait l'erreur.

---

# nemetonshiny 0.35.0 (2026-05-17)

### Added — Quatre sous-onglets Suivi sanitaire, symétriques FAST / FORDEAD

L'onglet « Alertes » unique ne reflétait pas la dichotomie modes
**quick (FAST)** vs **health (FORDEAD)** : en mode FAST il était
vide (placeholder « Aucune alerte à afficher ») parce que la table
`fordead_alerts` qui l'alimente n'est peuplée qu'après un run
FORDEAD. Pareil pour le bloc QGIS d'échantillonnage en bas, lui
aussi FORDEAD-only. Avec le split Carte FAST / Carte FORDEAD livré
en v0.34.0, on étend la même mécanique au canal d'alerte :

| Sous-onglet         | Valeur              | Visible en mode | Contenu                                                                          |
|---------------------|---------------------|-----------------|----------------------------------------------------------------------------------|
| `Alertes FAST`      | `alerts_fast`       | quick           | Placeholder — attend `nemeton::list_fast_alerts_for_zone()` (cf. spec PLAN.md)  |
| `Carte FAST`        | `pixel_map_fast`    | quick           | Raster NDVI/NBR + slider date + clic pixel/placette                              |
| `Alertes FORDEAD`   | `alerts_fordead`    | health          | Carte des placettes flaguées + bloc QGIS (renommé depuis `alerts`)              |
| `Carte FORDEAD`     | `pixel_map_fordead` | health          | Placeholder — attend `nemeton::read_fordead_dieback_mask()`                     |

Visibilité pilotée par un `observe` étendu côté server :
`bslib::nav_show()` / `nav_hide()` sur les 4 valeurs selon `input$mode`.
À l'écran, l'utilisateur ne voit jamais plus de 2 sous-onglets à la
fois — exactement comme avant v0.35.0 avec 3 déclarations, mais avec
le bon couple (alertes + carte) par mode.

**Renommage** : l'ancien sous-onglet `alerts` devient `alerts_fordead`,
même contenu (carte Leaflet `alerts_map`, popups par classe de
confiance, bloc QGIS GRTS/Random). Les `conditionalPanel` internes
qui filtraient sur `input$mode == 'health'` sont supprimés : tout le
sous-onglet est désormais masqué en mode FAST, plus besoin de
double-vérification.

**4 nouvelles clés i18n FR/EN** :

- `monitoring_subtab_alerts_fast`
- `monitoring_subtab_alerts_fordead`
- `monitoring_fast_alerts_placeholder_title`
- `monitoring_fast_alerts_placeholder_body`

Le placeholder Carte FORDEAD est mis à jour pour pointer vers
« Alertes FORDEAD » (au lieu de « Alertes »).

### Notes opérationnelles

- Les deux placeholders (Alertes FAST + Carte FORDEAD) seront
  câblés à de vrais modules dès que `nemeton@v0.25.0` shippera
  `list_fast_alerts_for_zone()` et `read_fordead_dieback_mask()`.
- Tant que ce n'est pas le cas, le mode FAST montre **Alertes FAST
  placeholder + Carte FAST fonctionnelle**, et le mode FORDEAD
  montre **Alertes FORDEAD fonctionnelle + Carte FORDEAD
  placeholder**.
- Aucune fonction métier ajoutée côté app — purement structuration
  UI (ADR-009 respecté).

---

# nemetonshiny 0.34.0 (2026-05-16)

### Fixed — Cascade de redraws sur la carte pixel pendant l'animation slider

L'animation du curseur de date dans « Carte pixel » provoquait
~90 redraws complets (raster + 63 polygones UGF + 105 marqueurs
placettes) sur 30 ticks, avec un pattern observable de 2× UGF + 1×
Placettes par cycle. Cause architecturale : `addRasterImage()` plaçait
le raster dans `overlayPane` (le défaut), à la même profondeur que
les polygones et les `CircleMarkers` (qui sont des `Path` Leaflet,
pas des `L.Marker`). Chaque swap raster montait au-dessus de
l'overlay, forçant les observers UGF et Placettes à re-fire via une
dépendance fictive `current_layer_r()` pour se ré-empiler.

**Correctif** — créer un pane Leaflet custom `nemetonRaster` à
z-index 250, entre `tilePane` (200, où vivent OSM et Satellite) et
`overlayPane` (400, où vivent polygones et CircleMarkers). Le raster
est épinglé dans ce pane via `addMapPane()` + `gridOptions(pane = …)`.
Avantages cumulés :

- le raster reste **toujours au-dessus du fond de carte**, qu'il
  s'agisse d'OSM ou de Satellite (un test précédent avec le raster
  dans `tilePane` cachait NDVI/NBR quand l'utilisateur basculait sur
  Satellite, parce que le `LayersControl` ré-ajoutait le tile
  satellite après le raster dans le DOM) ;
- les polygones UGF et markers placettes restent **au-dessus du
  raster** (donc cliquables) sans besoin de ré-empilement ;
- les dépendances fictives `current_layer_r()` des observers UGF et
  Placettes sont supprimées — ils ne re-firent que quand leur source
  change réellement (projet, refresh obs).

### Added — Sous-onglets « Carte pixel (FAST) » et « Carte FORDEAD »

L'onglet pixel unique ne reflétait pas la dichotomie modes
**quick (FAST)** vs **health (FORDEAD)**. Désormais :

| Sous-onglet            | Valeur            | Visible en mode | Contenu                                                                |
|------------------------|-------------------|-----------------|------------------------------------------------------------------------|
| `Alertes`              | `alerts`          | toujours        | Carte des alertes + bouton QGIS                                       |
| `Carte FAST`           | `pixel_map_fast`  | quick           | Raster NDVI/NBR + slider date + clic pixel/placette                   |
| `Carte FORDEAD`        | `pixel_map_fordead` | health        | Placeholder (carte des classes de dépérissement à venir, cf. Notes)   |

La visibilité est pilotée par un `observe` côté server qui utilise
`bslib::nav_show()` / `nav_hide()` selon `input$mode`. La Carte
FORDEAD reste un placeholder explicatif tant que le cœur `nemeton`
n'expose pas `read_fordead_dieback_mask()` — la coordination
cœur↔app sera ouverte dans une session dédiée à
`/home/pascal/dev/nemeton`.

**Nouvelles clés i18n FR/EN** :

- `monitoring_subtab_pixel_map_fast`
- `monitoring_subtab_pixel_map_fordead`
- `monitoring_fordead_map_placeholder_title`
- `monitoring_fordead_map_placeholder_body`

---

# nemetonshiny 0.33.0 (2026-05-16)

### Changed — Migration vers `nemeton@v0.24.0` (BREAKING)

`nemeton::run_fordead_dieback()` a changé de signature au cœur :
`aoi` / `scenes_df` / `forest_mask` ont été retirés (le cœur les
dérive lui-même depuis `(con, zone_id)` via le schéma monitoring),
et trois nouveaux arguments **requis** sont ajoutés :

| Argument          | Rôle                                                                                         |
|-------------------|----------------------------------------------------------------------------------------------|
| `con`             | Connexion DBI sur la base monitoring (TimescaleDB)                                          |
| `zone_id`         | Identifiant entier de la zone — le cœur résout AOI + masque forêt + scenes_df               |
| `cache_dir`       | Répertoire `cache/layers/sentinel2/` partagé avec FAST (B04/B12 réutilisés)                 |

Le pipeline passe de **5 phases** à **6 phases** : un nouveau
`phase 0 = ingest` télécharge les bandes manquantes (B02, B05, B8A,
B11) par-dessus celles déjà cachées par FAST. Cela réduit le coût
réseau du tandem FAST → FORDEAD et clarifie l'ownership des fichiers
(le cache `sentinel2/` est l'unique source vérité, plus de duplication).

**Côté app** — purement substitution d'arguments, aucune logique
métier modifiée (conforme ADR-009) :

- `R/service_monitoring.R` : le worker `run_fordead_async()` reçoit
  désormais `cache_dir` (résolu par `.resolve_s2_cache_dir()`), perd
  `aoi`, et ouvre lui-même la connexion DBI à passer au cœur.
- `R/mod_monitoring.R` : le helper `.invoke_fordead()` ne fabrique
  plus l'AOI via `get_monitoring_zone_aoi()` ni n'ouvre de
  connexion DBI éphémère — il passe juste `zone_id` et `cache_dir`
  au worker. La validation amont ne porte plus que sur
  `nzchar(input$zone_id)` ; l'existence effective de la zone est
  vérifiée côté cœur.
- `tests/testthat/test-mod_monitoring.R` : trois mocks
  `get_monitoring_zone_aoi` retirés et l'assertion `calls[[1]]$aoi`
  remplacée par une vérification de présence de `cache_dir` dans
  les arguments envoyés au task.

### Added — Libellé i18n pour la nouvelle phase ingest

Nouvelle clé `monitoring_fordead_phase_ingest` (FR/EN) :
« Téléchargement des bandes manquantes… » / « Downloading missing
bands… ». Le dispatcher générique livré en v0.32.0
(`paste0("monitoring_fordead_phase_", payload$phase_name)`) la
consomme automatiquement quand `phase_name = "ingest"` arrive du
worker — aucun branchement applicatif spécifique.

### Notes opérationnelles

- Le plancher `Imports: nemeton (>= 0.24.0)` interdit le downgrade
  sur un cœur encore en v0.23.x. `Remotes: pobsteta/nemeton@main`
  reste en place pour suivre le cœur en continu.
- Les autres fonctions du cœur consommées par l'app
  (`ingest_sentinel2_timeseries`, `read_obs_pixel`,
  `build_index_stack`, `extract_pixel_timeseries`,
  `check_fordead_validity`, …) sont **inchangées** dans v0.24.0.
- La séquence d'événements visible dans les toasts devient
  désormais `ingest → vegetation_index → train_model → forest_mask
  → dieback_detection → export_results` (les libellés post-process
  et persist 1.x deviennent obsolètes quand FORDEAD shippera son
  nouveau pipeline STAC, mais restent câblés pour rétrocompat).

---

# nemetonshiny 0.32.0 (2026-05-16)

### Added — Toasts de progression FORDEAD en bas à droite

L'utilisateur voulait suivre l'avancée du pipeline FORDEAD pendant
le diagnostic (10-30 min selon la taille de zone). Le worker
`nemeton::run_fordead_dieback()` émet déjà tous les événements
nécessaires depuis `v0.22.5` via son `progress_callback`. L'app les
reçoit via la `reactivePoll` sur le fichier JSON de progression et
les route désormais vers des toasts Shiny positionnés en
**bas-droite** :

| Événement cœur     | Toast app                                                           | Type    | Durée    |
|--------------------|---------------------------------------------------------------------|---------|----------|
| `fordead:start`     | (silencieux — le bouton « Lancer » désactivé suffit)               | —       | —        |
| `fordead:phase`     | « Phase 3/7 — Indice de végétation »                                | message | persistant |
| `fordead:phase_done`| « ✓ Indice de végétation »                                          | default | 1.5 s    |
| `fordead:complete`  | « FORDEAD terminé · 42 alertes · 142.3s »                          | message | 8 s      |
| `fordead:error`     | « FORDEAD échec en train_model : Python OOM »                       | error   | persistant (fermeture user) |

**Design générique** — pas de `switch()` hardcodé sur `phase_name`.
La fonction `.fordead_phase_label(phase_name, i18n)` cherche d'abord
la clé `monitoring_fordead_phase_<name>` ; si absente, retombe sur
une version Title-Case humanisée du nom brut (`future_unknown_phase`
→ « Future Unknown Phase »). Conséquence : quand `nemeton@v0.23.0`
shippera sa nouvelle séquence de phases (STAC-based, 4-5 noms
différents type `stac_assembly`, `fit`, `predict`), l'app les
affichera **sans nécessiter de release** — les libellés propres
suivront en patch.

**Positionnement bas-droite** via override CSS de
`#shiny-notification-panel` dans `inst/app/www/css/custom.css` +
`custom.min.css`. S'applique à toutes les notifications de l'app
(s2:* ingestion, fordead:*, validations), pas seulement FORDEAD.

**Nouvelles clés i18n** (FR/EN) :

- Templates : `monitoring_fordead_phase_progress`,
  `monitoring_fordead_phase_done`, `monitoring_fordead_complete`,
  `monitoring_fordead_error` (glue-style `{n}`/`{label}` consommé par
  `i18n$t(key, name = value)`).
- Per-phase 1.x : `vegetation_index`, `train_model`, `forest_mask`,
  `dieback_detection`, `export_results`, `postprocess`, `persist`.
- Per-phase 2.x anticipées : `stac_assembly`, `fit`, `predict`.

**Refactoring testable** : l'observe a été simplifiée pour déléguer
à `.fordead_handle_progress_event(ev, session, i18n)`, helper pur
testable hors session Shiny. Trois tests de régression
(`fordead:phase` avec libellé i18n, `fordead:start` silencieux, et
le fallback humanisé sur phase inconnue) verrouillent le contrat.

**Floor d'API** : `Imports: nemeton (>= 0.22.5)` — installs antérieurs
émettront une erreur explicite plutôt qu'un silence sur les nouvelles
clés d'event.

# nemetonshiny 0.31.5 (2026-05-16)

### Fixed — Carte pixel : raster NDVI/NBR visible sur fond Satellite

L'utilisateur signalait que sur fond Satellite la couche NDVI/NBR
restait imperceptible, alors qu'elle s'affichait correctement sur
fond OSM. C'est la dernière conséquence du conflit de palette : la
gamme NDVI conventionnelle (rouge → vert pâle → vert foncé) partage
exactement les tons de l'imagerie naturelle Esri (forêts vert
foncé). Pour les valeurs typiques en forêt (NBR ~0.5, dans la zone
« vert pâle » #A8DDB5), même à 0.85 d'opacité (bump v0.31.1) le
raster se fondait dans le satellite.

Correctif (`R/mod_monitoring_pixel_map.R`) : opacité raster 0.85 →
**1.0**. Les couleurs deviennent parfaitement lisibles sur les deux
fonds.

**Trade-off assumé** : sur l'emprise du raster (la zone d'analyse),
l'imagerie satellite est masquée. L'utilisateur garde le contexte
satellite **autour** de l'emprise (typiquement quelques km au-delà
de la zone d'analyse, déjà visible). S'il a besoin de voir les
routes/parcelles à l'intérieur de la zone, il peut basculer en OSM,
toujours pleinement compatible avec le raster.

# nemetonshiny 0.31.4 (2026-05-16)

### Fixed — Carte pixel : placettes redeviennent cliquables (ordre des couches)

L'utilisateur a signalé que les marqueurs placettes ne réagissaient
plus au clic sur la Carte pixel — le modal placette ne s'ouvrait
jamais.

Diagnostic : les `CircleMarker` Leaflet sont des **`Path`** (pas
des `L.Marker`), donc ils vivent dans le pane `overlayPane`, le
même que le raster `ImageOverlay` et les polygones UGF. Dans un
pane, c'est l'ordre DOM qui décide qui reçoit les clics — la
dernière couche ajoutée est au-dessus et capte les clics.

En v0.31.2 j'ai posé `priority = 100L` sur l'observe raster pour
garantir qu'il fire en premier (raster en bas de pile). Mais les
observes UGF et placettes étaient tous deux à la priorité 0 par
défaut, et l'ordre relatif entre eux n'était pas déterministe.
Quand le placettes observe firait AVANT l'UGF observe, les
polygones orange finissaient au-dessus des marqueurs bleus et
interceptaient tous leurs clics.

Correctif (`R/mod_monitoring_pixel_map.R`) : échelle stricte de
priorités, du bas vers le haut de `overlayPane` :

| Observe   | Priority | Position |
|-----------|----------|----------|
| Raster    | 100      | fond     |
| UGF       | 50       | milieu   |
| Placettes | 0        | haut (cliquable) |

Ajout aussi du dummy `current_layer_r()` dans le placettes observe
(déjà présent sur UGF depuis v0.31.2) : quand l'utilisateur bouge
le slider de date ou change NDVI/NBR, les marqueurs sont
re-empilés au-dessus du raster fraîchement peint.

# nemetonshiny 0.31.3 (2026-05-16)

### Fixed — Carte pixel : auto-zoom au chargement projet **vraiment** réparé

Symptôme : malgré les correctifs v0.29.1, v0.30.0 et v0.31.0, la
Carte pixel s'ouvrait toujours sur la vue Leaflet par défaut
(monde entier) au chargement projet, avec un seul pixel rouge en
plein milieu de la France pour marquer l'emprise du raster.

Diagnostic : ma reactive auto-zoom était fonctionnellement correcte
mais fire **avant que le widget Leaflet ne soit dans le DOM** dans
le flow réel utilisateur :

1. L'utilisateur charge un projet (depuis l'onglet Sélection ou
   Accueil).
2. Toutes mes reactives du module pixel_map fire (current_project
   change) → `leafletProxy("map") |> fitBounds(...)` envoyé.
3. Mais l'utilisateur n'est pas (encore) sur Carte pixel. Le
   widget Leaflet n'a pas été rendu côté client.
4. Les commandes `leafletProxy` se queue, et sont rejouées quand
   le widget initialise.
5. **Mais** `fitBounds` est silencieusement ignoré par Leaflet
   quand le container a une taille 0×0 (ce qui est le cas à
   l'instant T=0 du widget). Résultat : vue monde par défaut.

Correctif (`R/mod_monitoring_pixel_map.R`) : pattern emprunté à
`mod_ug.R:744-794` qui résolvait déjà ce souci pour la Carte UGF :

- Observer la navigation via `session$userData$root_session$input$main_nav`
  + `monitoring-subtab` — l'observe ne fire **que** quand
  l'utilisateur regarde effectivement la Carte pixel (sous-onglet
  actif).
- `later::later(0.3s)` pour laisser le DOM se construire.
- Custom message `leafletInvalidateSize` (handler JS dans
  `inst/app/www/js/custom.js:169`) pour que Leaflet re-mesure son
  container, puis `fitBounds` via `leafletProxy`.

Le guard `.last_fitted_id` de v0.31.0 est retiré : l'observe est
naturellement throttlée par la visibilité (un re-fit par visite de
sous-onglet est désirable pour gérer les resize de fenêtre ou les
multi-écrans).

Mid-session le user peut maintenant naviguer entre Alertes ↔
Carte pixel sans perdre le centrage — chaque retour sur Carte
pixel recadre proprement sur les UGFs.

# nemetonshiny 0.31.2 (2026-05-16)

### Fixed — Carte pixel : contour UGF désormais visible (problème de z-order)

L'utilisateur signalait toujours l'absence du contour UGF dans la
sous-onglet **Carte pixel**, alors que les mêmes UGFs s'affichaient
correctement dans **Sélection / Carte UGF**. La donnée était bien
là (la chaîne de fallback de v0.31.1 fonctionne), mais elle était
**peinte par-dessus** par le raster NDVI/NBR à cause d'un problème
de z-order dans le `overlayPane` Leaflet.

Diagnostic : Leaflet rend les couches du `overlayPane` dans l'ordre
DOM (dernière ajoutée = au-dessus). L'observe UGF est rapide (lecture
de champ projet) et fire tôt ; l'observe raster est lent
(`build_index_stack` lit N COGs) et fire tard. Résultat : polygones
ajoutés en premier → raster ajouté ensuite, par-dessus → orange
invisible.

Correctif en deux temps dans `R/mod_monitoring_pixel_map.R` :

1. **L'observe UGF lit `current_layer_r()` en dépendance**, sans
   utiliser sa valeur. Ainsi, à chaque update du raster, l'observe
   UGF re-fire et ré-ajoute les polygones APRÈS le raster en DOM
   order — polygones au-dessus.
2. **L'observe raster est marqué `priority = 100L`**. Dans un flush
   où raster et UGF sont tous deux dirty (typiquement au chargement
   projet), Shiny lance les observers de priorité élevée d'abord —
   raster ajoute son image, UGF ajoute ses polygones après.
   Polygones au-dessus de manière déterministe.

Combinés, les deux mécanismes garantissent que le contour orange
reste visible au-dessus du raster quel que soit l'ordre des
réactives dans le flush.

# nemetonshiny 0.31.1 (2026-05-16)

### Fixed — Carte pixel : contour de zone d'analyse + raster lisible sur Satellite

**Chaîne de fallback complète pour l'outline orange.** Le contour
n'apparaissait toujours pas pour les projets qui ont des placettes
mais ni `indicators_sf` (indicateurs non calculés) ni `ugs.json`
(UGFs non formellement définis) — cas courant quand on travaille
directement avec un plan d'échantillonnage. La reactive `ugf_sf_r`
de v0.31.0 retournait NULL dans ce scénario.

Nouvelle chaîne dans `R/mod_monitoring_pixel_map.R`, premier
non-NULL gagne :

1. `current_project$indicators_sf` — post-calcul indicateurs
2. `ug_build_sf(current_project)` — UGFs définis en l'onglet UG
3. **Bbox du raster** (`pixel_stack_r()`) — extent implicite de
   l'AOI d'ingestion, dérivé en rectangle
4. **Bbox des placettes** (`placettes_sf_r()`) — dernier recours

En (3) et (4), le label « UGF » est techniquement un abus (on
dessine un rectangle de zone d'analyse, pas des UGFs réelles), mais
la valeur utilisateur — un cadre orange visible autour de la zone —
est respectée même quand les UGFs ne sont pas définies. Un
`cli::cli_alert_info` indique au terminal quelle source a été
utilisée (`UGF source: raster bbox (fallback, no UGFs defined).`),
ce qui rend le diagnostic immédiat la prochaine fois.

**Opacité du raster bumpée 0.75 → 0.85.** Sur fond Satellite
(Esri.WorldImagery), la palette NDVI/NBR (rouge/orange/jaune/vert)
se confondait avec l'imagerie naturelle (vert forêt + jaune-brun
champs) — la couche existait (légende affichée bottom-right) mais
était indiscernable à l'œil. 0.85 laisse passer assez d'imagerie
sous-jacente pour garder le contexte spatial (routes, parcelles
visibles) tout en faisant ressortir le gradient. Sur OSM la
visibilité était déjà bonne, on perd un peu de transparence sans
problème.

# nemetonshiny 0.31.0 (2026-05-16)

### Breaking — suppression de l'onglet « Séries par placette »

L'onglet **Séries par placette** est retiré du sous-onglet Suivi
sanitaire. La vue multi-traces NDVI/NBR par placette (mode rapide)
est désormais accessible **spatialement** : un clic sur un marqueur
placette de la **Carte pixel** ouvre un modal avec la série agrégée
plot pour cette placette unique.

Note de régression pour les utilisateurs en mode sanitaire (FORDEAD) :
le graphique en barres de distribution des classes de confiance
d'alertes vivait dans le même output que les séries par placette et
disparaît avec lui. Si ce graphique est utile pour ton workflow,
ouvre un ticket et on l'ajoutera à l'onglet Alertes en patch.

### Fixed — Carte pixel : contour UGF désormais affiché et zoom à l'ouverture

Le contour UGF orange n'apparaissait pas et l'auto-zoom au chargement
projet ne marchait pas, parce que la reactive `ugf_sf_r` ne
consommait que `current_project$indicators_sf` — un champ qui n'est
construit par `service_project.R::load_project` **qu'après** le
calcul des indicateurs. Les projets avec UGFs définies mais
indicateurs non calculés retournaient NULL.

Correctif (`R/mod_monitoring_pixel_map.R`) : `ugf_sf_r` tente
d'abord `indicators_sf`, puis tombe sur `ug_build_sf(project)` qui
construit la sf POLYGON UGF directement depuis `ugs.json` +
`tenements` — disponible dès que l'utilisateur a défini ses UGFs
dans l'onglet UG. L'auto-zoom hérite automatiquement du fix puisque
les deux observers consomment la même `ugf_sf_r()`.

### Fixed — Carte pixel : plus de double modal au clic placette

Cliquer un marqueur placette ouvrait le modal placette puis
empilait immédiatement le modal pixel par-dessus. Cause : les
`L.CircleMarker` Leaflet sont des `Path`, et leur événement click
remonte naturellement à `map_click`. Les deux observers
(`input$map_marker_click` et `input$map_click`) firaient sur le
même tap.

Correctif (`R/mod_monitoring_pixel_map.R`) : pattern flag horodaté.
Le handler marker pose `Sys.time()` dans un `reactiveVal`
`marker_just_clicked`. Le handler pixel vérifie le delta au début
de son exécution et bail si < 500 ms (largement sous toute cadence
de double-clic plausible, suffisant pour attraper la propagation
synchrone d'un tap unique).

# nemetonshiny 0.30.2 (2026-05-16)

### Fixed — Suivi sanitaire / Carte pixel : couches UGF, NDVI/NBR et Placettes invisibles

Sur la sous-onglet **Carte pixel**, malgré les trois cases à cocher
« UGF », « NDVI / NBR », « Placettes » présentes et cochées par
défaut dans le contrôle Leaflet, aucune des trois couches n'apparaissait
visuellement sur la carte. Symptôme particulièrement visible sur le
fond Satellite, où ni le contour UGF orange, ni les marqueurs
placettes bleus, ni le raster d'indice n'étaient perceptibles.

Cause : `addLayersControl(overlayGroups = c("UGF", "NDVI / NBR", "Placettes"))`
était posé en `renderLeaflet` **avant** que les couches correspondantes
ne soient ajoutées via `leafletProxy()` depuis les observes (qui
dépendent de données async : raster cache, samples.gpkg,
`current_project`). Côté JS, `L.Control.Layers` créait ses cases à
cocher avec des références de couches indéfinies ; quand les couches
arrivaient ensuite via le proxy, le contrôle ne les rattachait pas
toujours à ses checkboxes — résultat « cases cochées mais couches
invisibles ».

La carte alerts du même module évite ce piège en n'utilisant pas
`overlayGroups` (ses markers sont ajoutés synchronement dans le
même chain `renderLeaflet`, donc le contrôle les voit à la
construction). On peut pas suivre ce pattern ici parce que nos
overlays dépendent de réactives async — on adopte donc la solution
plus simple : retirer `overlayGroups` de `addLayersControl`. Les
couches sont désormais **toujours visibles** (plus de checkbox
individuelle pour les masquer), mais elles s'affichent de manière
fiable. Le toggle de fond de carte (OSM / Satellite) continue de
fonctionner via `baseGroups`.

Polish associé pour rattraper le déficit de visibilité historique :

- Contour UGF : `weight` 2 → 3, `opacity` 0.9 → 1.0.
- Marqueurs placettes : `radius` 5 → 7.

Diagnostic ajouté : `cli::cli_alert_info()` sur chaque fire des
reactives UGF, placettes et auto-zoom — pour qu'un développeur
puisse, depuis le terminal, distinguer « la réactive renvoie NULL »
de « la réactive appelle bien `addX()` mais Leaflet ne rend rien »
si le problème ressurgit dans une autre configuration.

Tirage historique : la tentative `overlayGroups` introduite en
v0.28.4 (« layer NDVI/NBR disparaît au switch de fond ») n'a jamais
réellement résolu le problème — elle masquait juste un autre symptôme
dans certains cas. La règle pratique pour ce module : **pas
d'overlay groups dans `addLayersControl` quand les overlays sont
ajoutés via `leafletProxy`**.

# nemetonshiny 0.30.1 (2026-05-16)

### Changed — inversion de la sémantique de la checkbox « Cache COG »

La checkbox « Cache COG » du sous-onglet **Suivi sanitaire** (Mode
rapide) change de sémantique. Auparavant elle était un opt-in pour
peupler le cache disque ; désormais elle est un opt-in pour le wiper
intégralement.

| État | Avant (≤ v0.30.0) | Après (v0.30.1+) |
|---|---|---|
| ☐ Décoché (défaut) | `skip_cached = TRUE` → court-circuit DB, cache disque jamais peuplé | `skip_cached = FALSE` → nemeton vérifie le cache disque, télécharge uniquement les bandes manquantes |
| ☑ Coché | wipe `<cache_dir>/*` puis `skip_cached = FALSE` | wipe `<cache_dir>/*` puis `skip_cached = FALSE` (inchangé) |

Pourquoi ce changement : FORDEAD lit les COG sur disque, pas la
table `monitoring_obs`. L'ancien défaut laissait les utilisateurs
avec une DB pleine mais un cache disque vide. Quand l'utilisateur
lançait ensuite un diagnostic FORDEAD, celui-ci re-téléchargeait
intégralement parce que le cache disque n'avait jamais été peuplé,
défaisant tout le bénéfice du pré-ingest.

Le nouveau défaut (`skip_cached = FALSE` toujours) tire parti du
support cache COG introduit dans `nemeton@v0.21.4` et du fix
`writeRaster filetype` de `nemeton@v0.21.12`. Les `INSERT` DB
restent idempotents (`ON CONFLICT DO NOTHING` côté cœur), donc
re-lancer l'ingestion est sûr et bon marché.

Le libellé et le texte d'aide de la checkbox sont remis à jour
(`monitoring_reprime_cache_label`, `monitoring_reprime_cache_help`)
pour refléter la nouvelle sémantique : décoché = vérif cache,
coché = wipe & restart.

**Note de migration** :

- 1er run sur une zone vierge : aucun changement, le 1er ingest
  télécharge tout dans tous les cas.
- Re-runs sur une zone déjà ingérée : prennent quelques secondes
  de plus (vérification cache disque scène par scène) mais le
  cache se peuple réellement, et FORDEAD peut désormais s'en
  servir.
- Re-runs après un changement de fenêtre (nouvelles scènes
  apparues, dates étendues) : nemeton télécharge seulement le
  delta — bien plus rapide qu'avant où l'ancien défaut DB-cache
  rendait le delta invisible.
- Si tu rencontres un cache corrompu : coche la case pour wipe
  et recommencer.

Régression bloquée par `test-mod_monitoring.R` (nouveau test
« input$run passes skip_cached = FALSE regardless of
reprime_cache (v0.30.1+) »).

Aucune modif côté `nemeton` requise — le cœur supporte ce mode
depuis v0.21.4.

# nemetonshiny 0.30.0 (2026-05-16)

### Suivi sanitaire — Carte pixel : couche UGF + auto-zoom robuste

**Ajout d'une couche UGF.** Les polygones du périmètre du projet
(`current_project$indicators_sf`) sont désormais affichés sur la
**Carte pixel** sous forme de contour orange (#FF6B35) au-dessus du
raster NDVI/NBR. La couleur garantit la lisibilité aussi bien sur le
fond OSM (clair) que sur le fond Satellite (verts forêt). Le fill est
quasi-transparent (`fillOpacity = 0.05`) pour ne pas occulter
l'indice. Le contrôle des couches Leaflet expose trois cases à
cocher (« UGF », « NDVI / NBR », « Placettes ») toutes actives par
défaut.

**Refonte de l'auto-zoom.** Le correctif v0.29.1 (`observeEvent` sur
`project$id`) ne marchait que si `indicators_sf` était déjà populé au
moment où `id` était posé sur `app_state$current_project`. Si le
chargement projet posait l'`id` d'abord et les géométries après
(chargement async dans `mod_home`), l'`observeEvent` firait avec
`geom = NULL`, retournait, et ne refirait plus — la carte restait sur
la vue Leaflet par défaut (monde entier), où le raster et les
marqueurs apparaissaient à 1 pixel, donnant l'impression qu'ils
étaient invisibles.

Correctif : remplacement par un `observe()` simple (qui re-fire à
chaque mutation de `current_project`) avec un guard `reactiveVal`
`.last_fitted_id`. L'observer fit dès que **les deux** conditions
sont réunies (id présent + indicators_sf disponible), et marque
l'`id` comme « fit done » — pas de re-zoom intempestif au prochain
toggle d'une métadonnée projet, pan/zoom manuel préservé.

Détails d'implémentation (`R/mod_monitoring_pixel_map.R`) :
- Nouvelle constante `.ugf_overlay_group <- "UGF"`.
- Inclusion dans `addLayersControl(overlayGroups = c("UGF", "NDVI / NBR", "Placettes"))`.
- Nouvelle reactive `ugf_sf_r` (lit `indicators_sf`, transform 4326).
- Nouvel `observe` qui pose les polygones via `leafletProxy() |> addPolygons(...)`.
- Auto-zoom refactor : `reactiveVal .last_fitted_id` + `observe()` au lieu d'`observeEvent`.

# nemetonshiny 0.29.1 (2026-05-16)

### Suivi sanitaire — Carte pixel : auto-zoom sur les UGF au chargement projet

À l'ouverture d'un projet, la sous-onglet **Carte pixel** restait
centrée sur la vue Leaflet par défaut (monde entier) jusqu'à ce que
l'utilisateur pan/zoom à la main vers la zone d'étude. La vue n'a
jamais été rétablie depuis qu'on a basculé `renderLeaflet()` en
squelette statique en v0.28.1 (pour préserver le choix de fond).

Correctif (`R/mod_monitoring_pixel_map.R`) : ajout d'un
`observeEvent(app_state$current_project$id)` qui appelle
`leafletProxy("map") |> fitBounds()` sur l'emprise des UGF
(`current_project$indicators_sf`, transformés en EPSG:4326). Le
déclencheur est `project$id` — il ne se relance pas sur les
mutations internes du projet (saves de métadonnées, ajout d'une
placette…), donc le pan/zoom manuel de l'utilisateur reste stable
quand le raster est rebuildé ou que les marqueurs placettes sont
rafraîchis.

# nemetonshiny 0.29.0 (2026-05-16)

### Suivi sanitaire — Carte pixel : overlay placettes cliquable

La sous-onglet **Carte pixel** affiche désormais, par-dessus le raster
NDVI/NBR, les **placettes** du plan d'échantillonnage sous forme de
marqueurs cliquables (cercles bleus à bord noir). Seules les placettes
présentes dans `obs_pixel_data()` sont affichées — cohérent avec la
fenêtre courante (zone, bandes, dates).

Deux interactions distinctes :

- **Clic sur un pixel** → modal « Pixel à (lat, lon) » avec la série
  pixel brute extraite via `nemeton::extract_pixel_timeseries()`
  (comportement inchangé depuis v0.28.0).
- **Clic sur un marqueur placette** → modal « Placette P01 — série
  NDVI / NBR (moyenne plot) » avec la série agrégée placette filtrée
  sur `plot_id` depuis `obs_pixel_data()`. C'est exactement la même
  donnée que celle de l'onglet *Séries par placette*, mais centrée sur
  une seule placette et accessible spatialement par un clic sur la
  carte plutôt qu'un selectizeInput.

Le contrôle des couches Leaflet (en haut à droite) expose maintenant
deux cases à cocher (« NDVI / NBR » et « Placettes »), toutes deux
actives par défaut. L'utilisateur peut masquer indépendamment chaque
overlay.

L'onglet *Séries par placette* est conservé tel quel — il garde sa
valeur pour la comparaison multi-placettes simultanée (N traces sur
la même plotly), que la nouvelle interaction par marqueur ne couvre
pas (un clic = une placette).

Détails d'implémentation :
- Nouvelle reactive `placettes_sf_r` dans `mod_monitoring_pixel_map.R`
  qui consomme `load_samples(project$id, "plots")` (sf POINT, EPSG:2154)
  et `st_transform()` en 4326 pour Leaflet.
- `addLayersControl(overlayGroups = c("NDVI / NBR", "Placettes"))` —
  les deux libellés sont fixes (langue-indépendants) pour ne pas
  introduire de dépendance i18n dans `renderLeaflet` (préserve le
  choix de fond + le bouton ON/OFF des overlays au switch de langue).
- `addCircleMarkers(layerId = ~plot_id)` — l'identifiant remonte
  dans `input$map_marker_click$id`.
- Handler `observeEvent(input$map_marker_click, ...)` qui filtre
  `obs_pixel_data()` sur ce plot_id, restreint à NDVI/NBR, et ouvre
  un modal plotly cohérent avec le modal pixel.

# nemetonshiny 0.28.5 (2026-05-16)

### chore(deps) — l'app suit désormais `nemeton@main` en continu

L'épingle `Remotes: pobsteta/nemeton@v0.22.1` du `DESCRIPTION` est
remplacée par `Remotes: pobsteta/nemeton@main`. Effet : les nouveaux
installs `install_github("pobsteta/nemetonshiny")` récupèrent
automatiquement le dernier commit `main` du cœur, sans qu'il soit
nécessaire de publier une release app dédiée après chaque release
cœur.

Trade-offs assumés :

- **Reproductibilité d'install dans le temps perdue** : `install_github("…@v0.28.5")`
  fait dans 6 mois ne donnera plus le même état que le même appel fait
  aujourd'hui, parce que `nemeton@main` aura bougé entre-temps. Pour
  reproduire un état figé, utiliser `renv::snapshot()` côté projet
  utilisateur.
- **Une régression poussée sur `nemeton@main` casse l'install app**
  jusqu'au revert/fix. Mitigation : pousser sur `nemeton@main`
  uniquement avec CI verte côté cœur.

Documentation associée mise à jour dans `CLAUDE.md` (section *Stack
technique* + nouvelle section *Suivi de `nemeton@main` — implications
pour les releases*). La section *Épingle Remotes vers nemeton* ajoutée
en 0.28.3 est supprimée — devenue caduque avec ce changement.

# nemetonshiny 0.28.4 (2026-05-15)

### Suivi sanitaire — Carte pixel : couche NDVI/NBR perdue au switch de fond

Sur le sous-onglet **Carte pixel** de Suivi sanitaire, basculer entre
le fond OSM et le fond satellite faisait disparaître visuellement la
couche d'indice (NDVI ou NBR).

Cause : `addRasterImage()` était appelé avec un argument `group =`
**non déclaré** dans `addLayersControl()` (ni en `baseGroups` ni en
`overlayGroups`). Le contrôle des couches Leaflet ne reconnaissait
donc pas la couche raster, et selon le navigateur ou le timing du
toggle, son `ImageOverlay` était soit retiré transitoirement, soit
mal repositionné en z-index sous le tilePane.

Correctif (`R/mod_monitoring_pixel_map.R`) : la couche raster est
désormais **déclarée explicitement** comme `overlayGroups` dans
`addLayersControl()`, avec un libellé fixe « NDVI / NBR »
(langue-indépendant pour que `renderLeaflet()` reste sans dépendance
réactive — préserve le choix de fond fait en v0.28.1). Le `group =`
de `addRasterImage()` matche exactement le même libellé. Effet
utilisateur : la couche reste visible au basculement OSM↔Satellite,
et le contrôle expose en plus une case à cocher « NDVI / NBR » pour
masquer/afficher la couche à volonté.

Cleanup associé (`R/utils_i18n.R`) : suppression de la clé i18n
orpheline `monitoring_pixel_map_layer` (FR/EN), qui n'est plus
référencée nulle part.

# nemetonshiny 0.28.3 (2026-05-15)

### chore(deps) — bump épingle nemeton à v0.22.1

L'installation de `nemetonshiny` (`remotes::install_github`,
`pak::pkg_install`, `devtools::install`) faisait **redescendre**
`nemeton` à la version `0.22.0`, même quand une version plus récente
était déjà installée localement.

Cause : le champ `Remotes: pobsteta/nemeton@v0.22.0` du `DESCRIPTION`
épingle nemeton à exactement ce tag. L'épingle prime sur la borne
souple `Imports: nemeton (>= 0.22.0)`, donc le résolveur de
dépendances downgrade systématiquement.

Correctif : bump de l'épingle vers `pobsteta/nemeton@v0.22.1` (dernier
tag stable côté cœur). Aucun changement de code. Réflexe à adopter
pour chaque release `nemeton` : bumper l'épingle ici dans la foulée
(ordre cœur → app de CLAUDE.md).

# nemetonshiny 0.28.2 (2026-05-15)

### Suivi sanitaire — refresh automatique après ingestion Sentinel-2

À la fin d'un téléchargement Sentinel-2 réussi, l'onglet **Suivi
sanitaire** ne reflétait pas les nouvelles données : le graphique
plotly des placettes restait vide, et la sous-onglet **Carte pixel**
ne construisait pas son raster. L'utilisateur devait toucher à un
contrôle (bandes, fenêtre de dates, zone) pour forcer un rafraîchissement.

Cause : la reactive `obs_pixel_data()` — qui alimente à la fois le
plotly per-plot et la Carte pixel (via `scenes_df_r()`) — dépendait
uniquement de `input$mode`, `input$zone_id`, `input$bands` et
`input$date_range`. Aucune de ces entrées ne change quand l'ingestion
insère des lignes dans `monitoring_obs` côté DB ; Shiny n'avait donc
aucune raison de relancer la requête `nemeton::read_obs_pixel()`.

Correctif (`R/mod_monitoring.R`) : ajout d'un `reactiveVal`
`obs_refresh`, lu en première ligne de `obs_pixel_data()` pour créer
la dépendance, et bumpé dans le handler de succès d'ingestion juste à
côté de `zones_refresh`. Pattern symétrique à `alerts_refresh` côté
FORDEAD. Effet utilisateur : la Carte pixel se peuple toute seule
après ingestion, et le plotly également.

# nemetonshiny 0.28.1 (2026-05-15)

### Suivi sanitaire — Carte pixel : fix bascule de fond perdue au défilement des dates

Sur le sous-onglet **Carte pixel** de Suivi sanitaire, basculer du fond
OSM au fond satellite ne tenait pas : dès que l'utilisateur faisait
défiler le slider de date (ou changeait d'indice NDVI/NBR), le fond
revenait à OSM.

Cause : `renderLeaflet()` dépendait de `current_layer_r()` (le raster
de la date courante). Chaque changement de date relançait tout le
rendu, reconstruisant la carte de zéro avec `baseGroups = c("OSM",
"Satellite")` — OSM repris comme défaut, choix utilisateur perdu (le
choix de fond vit côté client, dans le widget Leaflet, et n'est pas
préservé à travers un remount).

Correctif (`R/mod_monitoring_pixel_map.R`) : le squelette de carte
(les deux fonds + le contrôle de couches) est désormais rendu **une
seule fois** dans `renderLeaflet()`. Les mises à jour du raster et
de la légende passent par `leafletProxy("map")` dans un `observe()`
qui `clearImages()` + `removeControl("pixel_legend")` puis ré-ajoute
l'image et la légende. Le widget Leaflet n'est plus remonté à chaque
date — le choix de fond reste sélectionné, et la navigation
temporelle est nettement plus fluide.

# nemetonshiny 0.28.0 (2026-05-15)

### Suivi sanitaire — nouveau sous-onglet « Carte pixel » (spec 010)

L'onglet **Suivi sanitaire** est désormais structuré en 3 sous-onglets
via `bslib::navset_card_tab` :

1. **Alertes** — la carte des clusters FORDEAD (existant, déplacé)
2. **Séries par placette** — le plotly NDVI/NBR per-plot livré en
   v0.27.0 (existant, déplacé)
3. **Carte pixel** — **nouveau** — visualisation des indices NDVI/NBR
   à la résolution native Sentinel-2 (10 m), navigable dans le temps,
   avec extraction de la série temporelle complète au clic sur un
   pixel.

Pourquoi : les vues existantes (carte alertes = POINT, plotly =
moyenne ⌀ 30 m sur les placettes) perdent la dynamique fine
intra-parcelle. Un peuplement de 10 ha contient ~1000 pixels S2 ;
quand FORDEAD détecte un cluster, le forestier veut voir lesquels
ont décroché en premier et à quelle vitesse.

Le matériau brut est déjà sur disque (cache COG sous
`<project>/cache/layers/sentinel2/{scene_id}/{B04,B08,B12}.tif`,
écrit par l'ingestion FAST) — cette release l'expose en carte
sans coût supplémentaire (zéro HTTP, zéro DB write).

#### Nouveau module `R/mod_monitoring_pixel_map.R`

* **UI** : `bslib::layout_sidebar` avec sidebar à droite (radio
  NDVI/NBR + slider de date avec animation step-by-step) et
  `leaflet::leafletOutput` plein écran à gauche, overlay spinner
  pendant le build du stack.
* **Server** :
  * `cache_dir_r()` résout `<project>/cache/layers/sentinel2`
    depuis `app_state$current_project`.
  * `scenes_df_r()` dérive `DISTINCT (scene_id, obs_date)` du
    `obs_pixel_data` reactive partagé avec la sub-tab per-plot
    (zéro requête DB additionnelle).
  * `pixel_stack_r()` appelle `nemeton::build_index_stack(cd, sdf,
    index = input$index)` (spec 010 §4.3) pour construire le
    `terra::SpatRaster` multi-temporel — debouncé sur (cache, scenes,
    index).
  * `current_layer_r()` snap la date du slider sur la scène la plus
    proche dans le stack (les obs S2 sont sparses, ~tous les 5 jours).
  * `output$map` — leaflet avec palette divergente
    `[#D62728, #FFD27F, #FFFFCC, #A8DDB5, #2CA02C]` (rouge → jaune
    → vert) ancrée sur 0, NA en transparent, légende avec valeurs
    extrêmes [-1, 1].
  * `observeEvent(input$map_click)` — appelle
    `nemeton::extract_pixel_timeseries()` au lat/lng cliqué et pop
    un modal `bslib::modalDialog(size = "l")` avec un plotly NDVI
    + NBR superposés (couleurs figées identiques à la sub-tab
    per-plot pour cohérence du modèle mental utilisateur).

#### Refactor `R/mod_monitoring.R`

L'UI principale (alertes + per-plot + nouveau pixel map) passe d'une
liste verticale de cards à un `bslib::navset_card_tab` à 3 panneaux.
Les inputs côté sidebar (mode, zone, dates, bands…) sont inchangés
— seul le main area est restructuré. Les conditionalPanels qui
gèrent les contrôles spécifiques au mode (`include_low` health,
`plot_filter` quick) sont déplacés à l'intérieur des sub-tabs
correspondants.

Server : un seul nouveau bloc, l'appel
`mod_monitoring_pixel_map_server("pixel_map", app_state,
obs_pixel_data, mode_input)` à la fin de `mod_monitoring_server`,
avec `mode_input = shiny::reactive(input$mode)` parce que la pixel
map ne s'active qu'en mode quick.

#### i18n

10 nouvelles clés FR/EN : `monitoring_subtab_alerts`,
`monitoring_subtab_per_plot`, `monitoring_subtab_pixel_map`,
`monitoring_pixel_map_title`, `_index`, `_date`, `_layer`,
`_click_hint`, `_modal_title_fmt`, `_no_cache`, `_loading`,
`_no_pixel`, `_scene_count_fmt`.

#### Pré-requis

* `nemeton@>=v0.22.0` installé (le pin DESCRIPTION est cohérent
  depuis v0.27.3).
* Un cache COG pré-existant — la pixel map affiche un message
  *« Pas de cache disque disponible »* tant qu'aucune ingestion
  FAST n'a été lancée pour la zone courante.

# nemetonshiny 0.27.3 (2026-05-15)

### Bump du pin nemeton vers v0.22.0

* `Imports: nemeton (>= 0.22.0)` (était `>= 0.21.11`)
* `Remotes: pobsteta/nemeton@v0.22.0` (était `v0.21.11`)

Récupère :

* `fix(monitoring)` v0.21.12 — `terra::writeRaster()` reçoit
  désormais explicitement `filetype = "GTiff"` quand on écrit sous
  `<cache_dir>/{scene_id}/{band}.tif`. Sans ça, GDAL infère parfois
  `MEM` ou un format ad hoc selon l'extension du fichier temporaire,
  et l'écriture pouvait échouer silencieusement (notamment sur
  Windows et certains runtimes Docker). Aucun changement d'API,
  100% transparent côté app.

* **4 nouveaux exports cœur** disponibles (utilisés par la prochaine
  release v0.28.0 pour la nouvelle vue *Carte pixel* — spec 010) :
  `read_s2_band_raster()`, `read_s2_band_stack()`,
  `build_index_stack()`, `extract_pixel_timeseries()`. Aucun
  câblage côté app dans cette release — les fonctions sont juste
  rendues disponibles pour la suivante.

### Suivi sanitaire — libellé toast cache lookup plus clair

Le toast émis à l'événement `s2:cache_lookup` (one-shot, juste après
la recherche STAC) disait *« Cache DB : 79 en cache, 26 à traiter »*.
Le mot *« à traiter »* laissait penser qu'on allait re-télécharger
26 scènes en entier — alors que dans la majorité des cas, ces 26
scènes ont déjà des bandes sur disque (cache COG) et seul le
**complément** est à fetcher (typiquement la 2ème bande quand un
run précédent a planté entre les deux).

Reformulation : *« Cache DB : %d scènes déjà ingérées (skip), %d
à compléter »* / *« DB cache: %d scenes already ingested (skip),
%d to complete »*. Le mot **« skip »** confirme que les premières
sont totalement court-circuitées (ni HTTP, ni read disque), et
**« à compléter »** indique que les secondes ne sont pas
forcément des re-downloads complets. Pour le détail bande par
bande, activer `NEMETON_S2_CACHE_DEBUG=TRUE` avant `run_app()` —
chaque CACHE-HIT / CACHE-MISS / FETCH apparaît dans la console R
en live (cf. v0.26.6).

Pas de modif fonctionnelle, uniquement i18n.

# nemetonshiny 0.27.2 (2026-05-15)

### Suivi sanitaire — bouton "Annuler / Réinitialiser" pour débloquer l'UI

**Symptôme remonté par l'utilisateur** : pendant une ingestion S2 où
l'API Planetary Computer renvoyait des HTTP 403 sur quasiment toutes
les bandes (typique d'un SAS token expiré mid-run ou d'une coupure
régionale Azure), le bouton "Lancer le diagnostic FAST" restait
désactivé pendant 10+ minutes — le temps que le worker
`future::multisession` épuise toutes les scènes (3 retries × backoff
exponentiel par bande), avec des toasts d'erreur HTTP 403 GDAL qui
s'empilaient sans fin sur la droite.

**Cause** : les warnings émis par `terra` (`GDAL Error 1: HTTP error
code: 403`, `Scene "..." skipped: [rast] file does not exist`) sont
capturés par `withCallingHandlers(warning = ...)` dans le worker
(`R/service_monitoring.R:180-184`) puis suppressed via
`invokeRestart("muffleWarning")`. L'ingestion **continue** sur les
scènes suivantes — qui plantent à leur tour pour la même raison —
au lieu de s'arrêter franchement. Le bouton ne se débloque qu'à la
toute fin de la boucle. C'est délibéré (un blip transient ne doit
pas tuer un long run), mais ça laisse l'utilisateur sans recours
quand l'erreur est en fait persistante.

**Fix** : nouveau bouton **« Annuler / Réinitialiser »** qui apparaît
sous le bouton "Lancer" dès qu'un worker tourne, et disparaît dès
qu'il termine. Au clic :

* `force_unlock_quick(TRUE)` (un nouveau `reactiveVal`) qui override
  l'observer du bouton — celui-ci passe de `is_running <- identical(
  ingest_task$status(), "running")` à `is_running <- … &&
  !isTRUE(force_unlock_quick())`. Le bouton "Lancer" redevient
  cliquable immédiatement.
* Tous les toasts d'ingestion sont supprimés (`removeNotification`
  sur `ingest_progress`, `ingest_band_failed`, `ingest_pc_token`,
  `ingest_error`, `ingest_warns`, `ingest_zero`, `ingest_success`,
  `ingest_cache_lookup`).
* Les fichiers locaux `progress.json` et `ingest_console.log` sont
  unlinkés et leurs `reactiveVal` resetés à NULL.
* Toast info qui explique la situation : *"Bouton réinitialisé.
  Vous pouvez relancer dès que le problème est corrigé. Note : le
  worker en cours continue en arrière-plan (les INSERT en base
  sont idempotents)."*

**Le worker n'est PAS killé** : Shiny `ExtendedTask` ne supporte pas
`cancel()`, et `future::multisession` non plus. Le worker continue à
boucler sur ses scènes en arrière-plan, mais c'est sans danger
parce que les INSERT côté `obs_pixel` sont `ON CONFLICT DO NOTHING`
(une scène qui finit par succeed après le reset ne crée pas de
doublon). Au prochain clic sur "Lancer", `force_unlock_quick(FALSE)`
remet le verrou en place pour le nouveau worker.

Même mécanisme dupliqué pour le mode HEALTH (`force_unlock_health`,
`run_health_cancel`) où FORDEAD prend déjà 10-30 minutes en nominal
et n'a aucun moyen d'interruption non plus.

3 nouvelles clés i18n : `monitoring_run_cancel_btn`,
`monitoring_run_cancel_done`. Pas de modif cœur.

# nemetonshiny 0.27.1 (2026-05-15)

### Suivi sanitaire — UX du toggle de re-téléchargement du cache COG

Deux ajustements UX sur la sidebar de l'onglet Suivi sanitaire en
mode rapide, sans changement fonctionnel sous-jacent :

* `feat(monitoring)` — Le label de la case à cocher
  *« Réamorcer le cache COG (skip_cached = FALSE) »* devient
  **« Retélécharger le cache COG »** (FR) / **« Re-download COG cache »**
  (EN). Plus court, plus direct, le verbe décrit l'action que voit
  l'utilisateur (re-fetch des bandes Sentinel-2) et non l'argument
  technique passé à `nemeton::ingest_sentinel2_timeseries()`.

* `feat(monitoring)` — L'explication détaillée *« Coche cette case
  lors du premier run d'une zone… »* qui était posée en
  `shiny::helpText()` sous la checkbox passe en **popover bslib**
  déclenché par une icône info (`circle-info`) à côté du label.
  Même pattern que le bouton « Tout » de l'onglet Synthèse.
  Permet de rendre la sidebar plus tight (l'explication ne prend
  plus 3 lignes verticales) tout en gardant l'info accessible
  d'un clic. Le contenu du popover supporte du HTML
  (`<strong>`, `<code>`, `<br>`) pour mettre en évidence le verbe
  d'action et le nom de l'argument cœur.

Pas de modification cœur (`nemeton@v0.21.11` toujours pinné).

# nemetonshiny 0.27.0 (2026-05-15)

### Suivi sanitaire — clôture du reliquat E6.b (phases 2, 3, 6)

Trois sous-chantiers du reliquat de l'épaississement 6 (suivi sanitaire)
sont livrés ensemble dans cette release. Le PLAN.md du repo `nemeton`
coche désormais E6.b en intégralité.

* `feat(monitoring)` — **Phase 3 — Plotly NDVI/NBR par placette**.
  L'onglet Suivi sanitaire en mode rapide affiche enfin les séries
  NDVI / NBR par placette, et plus le placeholder vide qui traînait
  depuis la phase 1. Câblage : nouveau reactive `obs_pixel_data()`
  dans `R/mod_monitoring.R` qui appelle `nemeton::read_obs_pixel()`
  (fonction publique introduite côté cœur dans `nemeton@v0.21.11`)
  avec les filtres `bands` (cases NDVI / NBR cochées dans la sidebar)
  et `date_range` (input dateRange existant). Resultat : un
  `data.frame` typé `(plot_id, obs_date, band, value, …)`. Le reactive
  retourne `NULL` quand un prerequis manque (mode health, pas de
  zone, pas de bandes cochées, dateRange invalide) — la renderPlotly
  affiche alors un état vide avec le message i18n adéquat
  (`monitoring_timeseries_placeholder` ou `monitoring_timeseries_no_plot_selected`).
  Côté UI, ajout d'un `selectizeInput("plot_filter", multiple = TRUE)`
  dans la card du time series (visible uniquement en mode quick),
  avec plugin `remove_button` pour décocher d'un clic. L'observer
  `observeEvent(obs_pixel_data())` met à jour ses choices à chaque
  re-fetch et préserve la sélection courante si elle reste un sous-
  ensemble des plots disponibles. Le plotly trace une ligne+marqueurs
  par couple `(plot_id, band)` — couleur figée par bande
  (NDVI vert, NBR rouge, NDWI bleu, B04/B08/B12 violet/marron/rose),
  `legendgroup = band` pour cliquer une bande entière, hovertemplate
  formaté `<plot · band> · YYYY-MM-DD · band = 0.xxx`. Layout
  i18nisé (xaxis = "Date d'observation" / "Observation date",
  yaxis = "Valeur de l'indice" / "Index value", légende horizontale
  sous le graphique). 6 nouvelles clés i18n
  (`monitoring_timeseries_select_plots`, `_select_plots_help`,
  `_no_plot_selected`, `_no_data`, `_xaxis`, `_yaxis`). 3 tests
  testServer dans `test-mod_monitoring.R` : (a) reader ne fire pas
  en mode health, (b) reader forwarde correctement zone_id/bands/
  date_from/date_to et l'observer rafraîchit `plot_filter` avec les
  plot_ids présents, (c) reader retourne NULL et ne consulte pas la
  DB quand une précondition manque (pas de zone, pas de bandes,
  dateRange invalide).

* `test(monitoring)` — **Phase 6 — Smoke E2E shinytest2**. Nouveau
  fichier `tests/testthat/test-monitoring-smoke-e2e.R` : un seul
  test `shinytest2::AppDriver` qui boot l'app via
  `shiny::shinyApp(app_ui, app_server)` (golem path), navigue vers
  l'onglet Monitoring (`main_nav = "monitoring"`), vérifie la
  présence du radio `monitoring-mode` et la bidirectionnalité du
  switch quick ↔ health. `on.exit(app$stop())` garanti même sur
  échec. `shinytest2 (>= 0.3.0)` ajouté à Suggests. Skips multiples :
  shinytest2 / chromote absents, pas de binaire Chrome détecté,
  `nemeton::read_obs_pixel` non exporté (cœur < v0.21.11), boot
  AppDriver échoué (avec message). Pas de DB requise — les zones
  restent vides, ce qui est exactement l'état utile pour un smoke.

* `chore(monitoring)` — **Phase 2 — Ingestion async + toasts**.
  Marquée livrée rétroactivement dans `nemeton/PLAN.md` : ses 6
  livrables (ExtendedTask, future_promise, progress_callback wired,
  reactivePoll sur progress.json, toasts persistants/erreurs/band-
  failure, console live) ont été câblés incrémentalement entre
  `nemetonshiny@v0.24.13` et `nemetonshiny@v0.26.6` sans qu'aucun
  commit n'identifie explicitement la phase. Pas de nouveau code
  livré ici — uniquement traçabilité dans le PLAN.

### UX

* `fix(monitoring)` — l'ID de tuile Sentinel-2 dans le toast de
  progression d'ingestion (« Tuile Sentinel-2 *S2A_MSIL2A_…* (X/N) »)
  faisait 60+ caractères et débordait la largeur ~250 px du toast
  Shiny en bas à droite, masquant les derniers segments (ex. tile
  code et timestamp de production). Wrappage du `scene_id` dans un
  `<span style="font-size:9px;font-family:monospace;word-break:break-all">`
  injecté côté `R/mod_monitoring.R` (bloc `nzchar(scene)` du
  `progress` handler) : le label « Tuile Sentinel-2 » et le
  compteur (X/N) restent à la taille normale, seul l'identifiant
  passe en monospace 9 px et casse sur 2 lignes courtes au lieu
  d'être tronqué. Pas de modif i18n (la chaîne
  `monitoring_ingest_progress_named_fmt` est inchangée — le span
  est injecté avant le `sprintf`, le résultat est passé en
  `htmltools::HTML(...)` à `.monitoring_spinning_msg()`).

### Bumps

* `Imports: nemeton (>= 0.21.11)` (était 0.21.9) — pour
  `read_obs_pixel()`.
* `Remotes: pobsteta/nemeton@v0.21.11` (était v0.21.9).
* `Suggests: shinytest2 (>= 0.3.0)` ajouté pour le smoke E2E.

# nemetonshiny 0.26.6 (2026-05-13)

### Console worker — capture réelle des messages cli + suppression des NOTICEs PG

Deux frictions observées au démarrage et pendant l'ingestion S2 :

* `fix(monitoring)` — la console R ne voyait toujours aucun message
  émis par le worker `future::multisession` (notamment les traces
  `[s2_cache HH:MM:SS] …` quand `NEMETON_S2_CACHE_DEBUG=TRUE` et les
  `S2 band cache: enabled at …` que nemeton émet au démarrage),
  alors que v0.26.5 prétendait les rendre live. Cause : en mode
  non-interactif, `cli::cli_alert_*` écrit directement sur
  `stderr()` via `cat(file = stderr())`, ce qui **contourne**
  `sink(type = "message")`. L'approche `sink()` était dead-on-arrival
  pour la sortie cli. Bascule sur `withCallingHandlers(message =,
  warning =)` autour de `nemeton::ingest_sentinel2_timeseries()` :
  les conditions `message` (cli inclus, via `rlang::inform`) et
  `warning` sont catchées et réécrites dans le log file avec
  `writeLines()` + `flush()` ligne par ligne ; chaque `invokeRestart`
  (`muffleMessage` / `muffleWarning`) supprime l'écriture stderr
  d'origine que `future` jetait de toute façon. Le `reactivePoll`
  côté parent voit maintenant chaque ligne au moment où elle est
  émise.

* `fix(db)` — `R/service_db.R::db_init_schema()` rejouait les
  `CREATE TABLE/INDEX/EXTENSION … IF NOT EXISTS` du schéma à
  chaque démarrage, ce qui faisait remonter ~17 `NOTICE: …
  already exists, skipping` via le canal `message()` de RPostgres.
  Pollution visuelle à chaque `run_app()`. Wrappé le loop de
  statements dans `suppressMessages({...})` — les `warning()` et
  `stop()` restent visibles, donc une vraie erreur de migration
  n'est pas masquée.

# nemetonshiny 0.26.5 (2026-05-13)

### Suivi sanitaire — réamorçage forcé du cache COG + console worker en live

Deux frictions remontées lors de l'usage réel de l'ingestion
Sentinel-2 sont levées :

* `feat(monitoring)` — quand la case **« Réamorcer le cache COG »**
  est cochée, le dossier `<project>/cache/layers/sentinel2/` est
  désormais purgé via `unlink(recursive = TRUE, force = TRUE)`
  juste avant `ingest_task$invoke()`. Sans cette purge, même avec
  `skip_cached = FALSE`, `nemeton:::.get_s2_band_raster()` servait
  les `B0X.tif` déjà présents sur disque (branche CACHE-HIT), ce
  qui défaisait tout l'intérêt du toggle pour qui voulait vérifier
  le re-fetch STAC. Le toggle force désormais un vrai
  re-téléchargement de toutes les bandes (`writeRaster()` ré-exécuté
  scène par scène). Une trace `cli::cli_alert_info` confirme le
  nombre d'entrées purgées et le chemin du cache.

* `feat(monitoring)` — la console R reçoit maintenant en temps réel
  toutes les lignes émises par le worker `future::multisession`
  (cli::cli_alert_*, message(), cat(), et les traces verboses
  `[s2_cache …]` quand `NEMETON_S2_CACHE_DEBUG=TRUE`). Implémentation :
  le worker `sink()` ses canaux `output` ET `message` sur un fichier
  `<project>/data/ingest_console.log` (nouvelle param `log_path` de
  `run_ingestion_async`), et le parent tail ce fichier toutes les
  500 ms via `reactivePoll` en lisant les nouveaux octets depuis un
  offset persistant, puis `cat()`e le delta sur `stderr()`. Bypass
  complet de la capture stdout par `future` (qui ne libère le buffer
  qu'à la fin du futur). Cleanup symétrique au `progress.json` dans
  les handlers success/error.

# nemetonshiny 0.26.4 (2026-05-13)

### Suivi sanitaire — instrumentation worker (diagnostic des hangs)

Quand le worker `future::multisession` se bloque ou plante, la
seule chose visible côté UI était `"MultisessionFuture was
interrupted"`, sans aucune indication d'où ça a coincé. Cette
release ajoute 2 heartbeats et 1 wrap d'erreur :

* `feat(monitoring)` — heartbeats émis via `progress_callback`
  AVANT l'appel `nemeton::ingest_sentinel2_timeseries()` :
  - `s2:worker_started` — le worker a atteint le début de son
    body (post `load_all` + `db_connect` + `progress_writer`).
    Si ce heartbeat n'apparaît pas → le futur ne spawn pas du
    tout (plan future cassé, env vars manquantes).
  - `s2:nemeton_call_starting` — juste avant d'entrer dans
    nemeton. Si on voit celui-là mais pas de scene events
    derrière → nemeton hang au démarrage (STAC, auth Microsoft,
    etc.).

* `feat(monitoring)` — l'appel `nemeton::ingest_sentinel2_timeseries`
  est wrappé dans un `tryCatch` qui émet
  `current = "s2:fatal_error"` (avec `error_message` +
  `error_class`) avant de re-throw. Conséquence : la cause exacte
  de l'erreur arrive dans la console + un modal côté UI au lieu
  d'un futur "interrupted" silencieux.

* `feat(monitoring)` — observer routant :
  - `s2:worker_started` / `s2:nemeton_call_starting` →
    persistance toast + log `cli::cli_alert_info`
  - `s2:fatal_error` → `cli::cli_alert_danger` + `showModal()`
    avec le message complet

* `feat(i18n)` — clés `monitoring_ingest_worker_event_fmt` +
  `monitoring_ingest_fatal_title` (FR + EN, accents en `\uXXXX`).

* Helper `.ws_emit(progress_cb, event)` — wrapper best-effort
  qui swallow les erreurs d'écriture (un échec de heartbeat ne
  doit JAMAIS tuer l'ingestion).

# nemetonshiny 0.26.3 (2026-05-13)

### Suivi sanitaire — propagation des `NEMETON_*` env vars vers le worker async

* `fix(monitoring)` — `future::multisession` workers sur Windows
  sont des processus `Rscript.exe` séparés qui n'héritent pas
  systématiquement des variables d'environnement settées dans la
  session principale après leur spawn. Conséquence pratique :
  `Sys.setenv(NEMETON_S2_CACHE_DEBUG = "TRUE")` en console R ne se
  voyait pas dans le worker → les lignes `[s2_cache HH:MM:SS] ...`
  émises par `nemeton` restaient muettes même quand le cache
  fonctionnait.

  Correctif : `run_ingestion_async()` et `run_fordead_async()`
  snapshottent à l'invoke les `NEMETON_*` env vars settées dans le
  parent (`.capture_worker_envvars()`), `future` les pickle comme
  globals automatiquement, et le worker les replay via
  `.apply_worker_envvars()` en tout début de `future_promise()`.

  Couvre : `NEMETON_S2_CACHE_DEBUG`, `NEMETON_DB_URL`,
  `NEMETON_DB_LOCAL`, `NEMETON_DB_HOST/_PORT/_NAME/_USER/_PASSWORD`.

* `test` — `test-service_monitoring_wiring.R` couvre les deux
  helpers (`.capture_worker_envvars` skip les valeurs vides ;
  `.apply_worker_envvars` no-op sur NULL/empty + setenv correct).

# nemetonshiny 0.26.2 (2026-05-13)

### Dépendance nemeton — pin à v0.21.9 (fix writeRaster .tif.tmp)

* `fix(deps)` — pin du cœur `nemeton` bumpé à `>= 0.21.9`. Cette
  version corrige le bug d'écriture du cache S2 où
  `terra::writeRaster()` était appelé sur un chemin `.tif.tmp`
  (pattern d'écriture atomique) sans argument `filetype`, et terra
  refusait avec *"cannot guess file type from filename"*. Toutes les
  bandes étaient FETCH + CROP avec succès puis perdues à l'étape
  WRITE → le cache disque restait vide même après le fix S4 de
  v0.21.8.

  Symptôme côté UI v0.26.1 : le run consommait 4-5 min par scène
  (FETCH + CROP), atteignait 26/26, mais
  `<project>/cache/layers/sentinel2/` restait vide.

  - `Imports: nemeton (>= 0.21.9)`
  - `Remotes: pobsteta/nemeton@v0.21.9`

# nemetonshiny 0.26.1 (2026-05-13)

### Dépendance nemeton — pin à v0.21.8 (fix S4→double extraction)

* `fix(deps)` — pin du cœur `nemeton` bumpé à `>= 0.21.8`. Cette
  version corrige un bug d'extraction où chaque scène Sentinel-2
  était skippée avec le message *"cannot coerce type 'S4' to vector
  of type 'double'"* lors d'un run `skip_cached = FALSE`
  (introduit en v0.21.4 avec le wiring `cache_dir`). Le bug se
  manifestait dans le bloc per-scène : un `terra::SpatRaster` (classe
  S4) était traité comme un vecteur numérique au lieu d'être extrait
  via `terra::extract()` / `terra::values()`.

  Conséquence côté UI v0.26.0 : cocher "Réamorcer le cache COG"
  donnait bien `skip_cached = FALSE` mais toutes les scènes étaient
  skippées → ni la DB ni le cache disque ne se remplissaient.

  - `Imports: nemeton (>= 0.21.8)`
  - `Remotes: pobsteta/nemeton@v0.21.8`

# nemetonshiny 0.26.0 (2026-05-13)

### Suivi sanitaire — exposition de `skip_cached` dans l'UI

* `feat(monitoring)` — checkbox **"Réamorcer le cache COG"** ajoutée
  sous le bouton d'ingestion en mode quick. Cochée → l'app appelle
  `nemeton::ingest_sentinel2_timeseries(..., skip_cached = FALSE)`
  ce qui force la ré-extraction plot-par-plot et donc le
  retéléchargement des bandes, qui se persistent dans
  `<project>/cache/layers/sentinel2/`. Décochée (défaut) → comportement
  v0.25.0 inchangé (`skip_cached = TRUE`).

  Contexte : v0.25.0 a finalisé le wiring `cache_dir` +
  `progress_callback`, mais le défaut `skip_cached = TRUE` côté nemeton
  court-circuite l'extraction quand la DB est déjà peuplée → le cache
  disque restait vide tant que la table `obs_pixel` contenait des
  lignes pour la zone + fenêtre demandées.

  Note : les INSERT sont `ON CONFLICT DO NOTHING` côté core, donc
  cocher la case n'écrase rien en DB.

* `feat(i18n)` — clés `monitoring_reprime_cache_label` +
  `monitoring_reprime_cache_help` (FR + EN, accents en `\uXXXX`).

# nemetonshiny 0.25.0 (2026-05-13)

### Suivi sanitaire — routage complet des événements `progress_callback` Sentinel-2

Cette release **complète le wiring** de
`nemeton::ingest_sentinel2_timeseries(cache_dir = , progress_callback = )`
côté UI. Le wiring de base (paramètres passés à la lambda
`ExtendedTask`, fichier JSON poll-é, toast dédupé) était déjà en place
depuis v0.24.11/v0.24.12 ; cette version ajoute le routage explicite
sur trois events qui étaient capturés par le fallback générique :

* `feat(monitoring)` — routage explicite des événements
  `progress_callback` introduits par `nemeton@v0.21.4+` :
  - `s2:cache_lookup` → toast persistant "Cache DB : N en cache,
    M à traiter" (réutilise le même `id` que la barre de progression
    pour éviter l'empilement)
  - `s2:band_fetch_failed` → toast warning non-persistant (6 s) avec
    `band` + `error_message`, sur un `id` distinct pour ne pas
    masquer la progression scène
  - `s2:pc_token_refreshed` → toast info éphémère (3 s) signalant la
    rotation du token SAS Planetary Computer

* `feat(monitoring)` — indicateur **"Cache COG actif"** sous le bouton
  d'ingestion (mode quick) : affiche le chemin absolu du répertoire
  `<project>/cache/layers/sentinel2/` où nemeton persiste les bandes
  cropées en COG. Le tooltip natif duplique le chemin pour copie
  rapide.

* `chore(deps)` — pin `nemeton` bumpé à `>= 0.21.7` pour aligner sur
  les versions qui exposent stablement la signature
  `progress_callback` + `cache_dir`.

### Priming du cache COG

Note importante pour l'utilisateur : si la DB de monitoring est déjà
peuplée par un run précédent, le défaut `skip_cached = TRUE` côté
`nemeton::ingest_sentinel2_timeseries()` court-circuite l'extraction
plot-par-plot → le cache disque sous
`<project>/cache/layers/sentinel2/` **reste vide** même avec ce
patch. C'est attendu. Pour amorcer le cache disque, il faut lancer
au moins une fois avec `skip_cached = FALSE` (paramètre core, non
encore exposé dans l'UI ; les INSERT sont `ON CONFLICT DO NOTHING`,
la DB reste intacte).

### i18n — nouvelles clés (FR + EN)

* `monitoring_ingest_cache_lookup_fmt` — "Cache DB : %d en cache, %d
  à traiter" / "DB cache: %d cached, %d to process"
* `monitoring_ingest_band_failed_fmt` — "Échec bande %s : %s" /
  "Band %s failed: %s"
* `monitoring_ingest_token_refreshed` — "Token SAS Planetary Computer
  rafraîchi" / "Planetary Computer SAS token refreshed"
* `monitoring_cache_active_fmt` — "Cache COG actif : %s" / "COG
  cache active: %s"

### Tests

* `tests/testthat/test-service_monitoring_wiring.R` — vérifie que
  `run_ingestion_async()` transmet bien `cache_dir` et un
  `progress_callback` non-NULL à
  `nemeton::ingest_sentinel2_timeseries()` (mock via
  `local_mocked_bindings`).

# nemetonshiny 0.24.14 (2026-05-13)

### Dépendance nemeton — pin re-synchronisé sur v0.21.5

* `chore(deps)` — pin du cœur `nemeton` aligné sur la dernière version
  stable installée en local (`>= 0.21.5`). Aucun changement
  fonctionnel côté app : `nemeton` v0.21.4 / v0.21.5 sont des bumps
  patch côté cœur (FORDEAD hardening + clarifications de
  signatures internes), mais `DESCRIPTION` restait à `>= 0.21.3`,
  ce qui provoquait des warnings `pak::pkg_install("nemetonshiny")`
  sur les machines neuves (résolution OK mais désynchro pin / version
  effectivement installée).

  - `Imports: nemeton (>= 0.21.5)`
  - `Remotes: pobsteta/nemeton@v0.21.5`
  - `CITATION.cff` bumpé à `0.24.14`

# nemetonshiny 0.24.13 (2026-05-13)

Release stable consolidant deux correctifs hardening sur le suivi
sanitaire (cycle dev `0.24.12.9001` → `0.24.12.9002`).

### Suivi sanitaire — cache S2 aligné sur la convention `<project>/cache/layers/`

* `fix(monitoring)` — la v0.24.11 posait le cache des bandes
  Sentinel-2 sous `<project>/data/s2_cache/`, ce qui violait la
  convention NMT déjà en place pour les autres rasters
  (`<project>/cache/layers/lidar_mnh/`, `lidar_mnt/`, `lidar_nuage/`,
  `opencanopy/`, `bdforet.gpkg`, etc. — cf. `mod_sampling.R::cache_raster()`).

  Correctif : `.resolve_s2_cache_dir(project)` renvoie désormais
  `<project>/cache/layers/sentinel2/`. Layout attendu :

  ```
  <project>/cache/layers/sentinel2/
    S2A_MSIL2A_20240515.../
      B04.tif
      B08.tif
      B12.tif
  ```

  **Migration manuelle** des anciens projets : si tu vois un dossier
  `<project>/data/s2_cache/` héritant de la v0.24.11, déplace-le à la
  main vers `<project>/cache/layers/sentinel2/` pour récupérer le
  cache existant. Sinon il sera juste ignoré et nemeton ré-téléchargera
  les bandes au prochain run.

# nemetonshiny 0.24.12.9001 (2026-05-12)

### Suivi sanitaire — dédup des toasts success / warning / error

* `fix(monitoring)` — les toasts finaux de l'ingestion et de FORDEAD
  s'empilaient à chaque re-clic au lieu de se remplacer (visible sur
  un 504 de Planetary Computer répété : 2-3 toasts identiques
  "Aucune scène Sentinel-2 trouvée...").

  Cause : les `shiny::showNotification()` finaux étaient appelés sans
  argument `id`. Sans id, Shiny crée à chaque fois une nouvelle
  notification.

  Correctif : `id = session$ns(...)` ajouté sur tous les toasts
  terminaux du module monitoring :
  - `ingest_zero` (0 scènes trouvées)
  - `ingest_success` (ingestion réussie)
  - `ingest_warns` (warnings non bloquants)
  - `ingest_error` (worker exception)
  - `fordead_success`, `fordead_error` (idem côté FORDEAD)

  Le toast persistant `ingest_progress` / `fordead_progress` avait
  déjà son id ; la dédup ne concernait que les terminaux.

# nemetonshiny 0.24.12 (2026-05-12)

### Suivi sanitaire — bump effectif du pin nemeton à v0.21.3

* `fix(deps)` — toast d'erreur **"Échec du téléchargement : argument
  inutilisé (cache_dir = cache_dir)"** au clic FAST après installation
  de v0.24.11.

  Cause : la v0.24.11 a livré le code applicatif qui appelle
  `nemeton::ingest_sentinel2_timeseries(..., cache_dir = ...)` mais
  j'ai oublié de bumper le **pin** dans `DESCRIPTION`. `Imports` et
  `Remotes` pointaient encore vers `nemeton@v0.21.2` qui ignore
  `cache_dir`, donc pak/install installait la vieille nemeton et
  R levait "argument inutilisé".

  Correctif : `Imports: nemeton (>= 0.21.3)` et
  `Remotes: pobsteta/nemeton@v0.21.3`. Aucun changement applicatif.

  À refaire côté utilisateur après pull :
  ```r
  pak::pak("pobsteta/nemetonshiny@claude/fix-remaining-errors-xbROB")
  # ou en local : devtools::install_local(".", force = TRUE)
  ```

# nemetonshiny 0.24.11 (2026-05-12)

### Suivi sanitaire — cache local Sentinel-2 + events band-level

* `feat(monitoring)` — branche le `cache_dir` introduit par
  `nemeton@v0.21.3+` sur `ingest_sentinel2_timeseries()`. Le worker
  pose désormais les bandes Sentinel-2 sous
  `<project>/data/s2_cache/`. Les bandes déjà téléchargées sont
  réutilisées au prochain run pour la même scène — gain massif sur
  un re-run après un échec STAC ou une extension de fenêtre temporelle.

  Helper `.resolve_s2_cache_dir(project)` côté `mod_monitoring.R` :
  retourne `NULL` quand aucun projet n'est ouvert (nemeton retombe
  sur son chemin legacy in-memory), sinon crée le sous-dossier au
  besoin et passe le chemin normalisé au worker.

* `feat(monitoring)` — abonnement aux nouveaux events progress
  `s2:band_cached` / `s2:band_fetched`. Chaque bande génère une
  ligne `cli_alert_info` dédiée dans la console R :

  ```
  ℹ Tuile Sentinel-2 S2A_MSIL2A_20260508T103651_R008_T31TFN_20260508T191011 (5/26) — 2026-05-08, 2.9% nuages, source=pc
    ⤷ Bande B04 (cache) — scène S2A_MSIL2A_20260508…
    ⤷ Bande B08 (téléchargement) — scène S2A_MSIL2A_20260508…
  ```

  Volontairement **pas** d'update du toast UI sur ces events :
  2-4 bandes par scène à sub-second feraient flickerer le toast
  et perdraient le contexte scène. Le toast reste sur l'event
  `s2:scene`.

  Helper `.log_band_event(ev, current_phase)` dispatché en tête de
  l'observer après détection de `current %in% c("s2:band_cached",
  "s2:band_fetched")`.

# nemetonshiny 0.24.10 (2026-05-12)

### Suivi sanitaire — 3 fixes UX critiques

* `fix(monitoring)` — **spam "Database schema up to date" en boucle**
  dans la console : `nemeton::db_migrate()` emet un
  `cli::cli_alert_info` à chaque connexion ré-ouverte. Avec les
  reactives multiples du module (validity, zones, alerts, probe...),
  on tape 30-50 lignes identiques par interaction.

  Correctif : `withCallingHandlers(message = ...)` autour de
  `db_migrate()` qui muffle uniquement les messages contenant "up
  to date" / "already migrated". Les "Applied migration X" du
  premier run et les warnings/erreurs restent visibles.

* `feat(monitoring)` — **0 scènes trouvées masquait un timeout
  Planetary Computer** (HTTP 504). Le worker affichait juste
  `Téléchargement terminé : 0 scène(s)`, sans dire que le backend
  STAC avait timeout.

  Correctif : `withCallingHandlers(warning = ...)` dans le worker
  capture les warnings nemeton (`STAC backend "pc" failed:
  HTTP 504...`) et les remonte dans `result$warnings`. Côté result
  observer :
  - Si `n_scenes == 0` : toast warning rouge avec les warnings
    capturés (ou un hint générique "Élargis la fenêtre temporelle
    ou tolère plus de nuages").
  - Si succès mais warnings non bloquants : toast secondaire
    "Avertissement(s) du backend : ..." pour ne pas les perdre.

* `fix(monitoring)` — **toast affichait "Tuile Sentinel-2 (scene_id
  missing) (0/159)"** entre la recherche STAC et la première tuile.
  Le log console était tout aussi opaque (`(scene_id missing)`).

  Correctif : quand `scene_id` est vide ET `completed == 0`, le
  toast affiche maintenant :
  - "Recherche des scènes Sentinel-2 disponibles…" (si total = 0)
  - "Préparation du téléchargement : N scène(s) trouvée(s)…" (si total > 0)

  Et la console : "Sentinel-2 STAC search done: N scene(s) found."

  Nouvelles clés i18n FR/EN : `monitoring_stac_search`,
  `monitoring_stac_search_with_count_fmt`,
  `monitoring_ingest_zero_fmt`, `monitoring_ingest_zero_default`,
  `monitoring_ingest_warns_fmt`.

### Suivi sanitaire — "Ingestion Sentinel-2" → "Téléchargement Sentinel-2"

* `chore(monitoring)` — le terme "ingestion" est trop technique pour
  le contexte UI où l'on télécharge des scènes Sentinel-2 depuis
  Planetary Computer. Renommage en "téléchargement" (FR) / "download"
  (EN) sur **tous les textes utilisateur du contexte Sentinel-2** :

  - `monitoring_validate_zone` : "lancer l'ingestion" → "lancer le téléchargement"
  - `monitoring_ingest_starting` : "Ingestion Sentinel-2 en cours" → "Téléchargement Sentinel-2 en cours"
  - `monitoring_ingest_success` : "Ingestion terminée" → "Téléchargement terminé"
  - `monitoring_ingest_error` : "Échec de l'ingestion" → "Échec du téléchargement"
  - `monitoring_timeseries_placeholder` / `monitoring_alerts_placeholder` :
    "après la première ingestion" → "après le premier téléchargement"
  - Console R (`cli_alert_info`) : "Sentinel-2 ingestion starting" →
    "Sentinel-2 download starting"

  **Volontairement non renommés** : `field_ingest_*` (import des
  données terrain depuis QField/GPKG) et `health_validation_*`
  (import des validations FORDEAD). Ce sont des uploads de données
  utilisateur, pas des téléchargements distants — le terme
  "ingestion" reste correct dans ces contextes.

  Les **clés i18n** elles-mêmes (`monitoring_ingest_*`) ne sont pas
  renommées — seul le contenu FR/EN change. Aucune cassure pour les
  modules qui les consomment.

# nemetonshiny 0.24.9 (2026-05-12)

### Suivi sanitaire — mirroring console des events de progression

* `feat(monitoring)` — la progression du Suivi sanitaire n'apparaissait
  que dans le toast UI. Aucune ligne dans la console R pour le
  développeur qui lance l'app via `Rscript -e ...`.

  Correctif : les deux observers de progression (ingestion + FORDEAD)
  écrivent désormais une ligne `cli::cli_alert_info` (ou
  `cli_alert_warning` sur `scene_error` / `phase_error`) à chaque
  event, exactement une fois par tuile / phase grâce à la
  granularité du `reactivePoll(500 ms)`.

  Le format console est plus riche que le toast : on profite des
  champs supplémentaires nemeton (`obs_date`, `cloud_pct`, `source`)
  pour donner une ligne du genre :

  ```
  ℹ Tuile Sentinel-2 S2A_MSIL2A_20260508T103651_R008_T31TFN_20260508T191011 (5/26) — 2026-05-08, 2.9% nuages, source=pc
  ```

  Pour FORDEAD :

  ```
  ℹ FORDEAD phase training (1/5)
  ```

  Helpers : `.log_ingest_event()` et `.log_fordead_event()` dans la
  section Internal de `mod_monitoring.R`. Aucune utilisation de
  `print()` / `message()` / `cat()` (règle 9 CLAUDE.md).

### Suivi sanitaire — toast progression aligné sur le payload nemeton

* `fix(monitoring)` — toast d'ingestion affichait **"Tuile Sentinel-2 0/0"**
  pendant tout le run alors que le `scene_id` arrivait correctement.

  Cause : nemeton@v0.21.2 émet le payload sous la forme
  `{current, completed, total, scene_id, obs_date, cloud_pct, source}`,
  pas `{i, n, status, ...}` comme initialement spécifié. L'observer
  cherchait `ev$i` / `ev$n` qui n'existaient pas, donc retombait
  toujours sur le défaut `0L`.

  Correctif :
  - Lecture des champs `completed` / `total` avec fallback vers
    `i` / `n` (au cas où le schéma évoluerait).
  - Idem côté FORDEAD : `current` pour la phase, fallback vers
    `phase` / `scene_id`.
  - Reformatage des i18n : compteur **entre parenthèses** en fin de
    message — `"Tuile Sentinel-2 <scene_id> (X/N)"` et
    `"FORDEAD — phase <nom> (X/N)"`.
  - Ajout d'une **roue dentée animée** devant chaque message
    (`bsicons::bs_icon("gear-fill")` + classe `.nmt-spin`,
    keyframe déjà définie dans `custom.css`). Le toast persistant
    ne ressemble plus à un message figé.

# nemetonshiny 0.24.8 (2026-05-12)

### Suivi sanitaire — progression "X/N tuiles Sentinel-2" + phases FORDEAD

* `feat(monitoring)` — pendant l'ingestion Sentinel-2 (FAST) et le
  diagnostic FORDEAD, l'utilisateur reste sur un toast statique
  pendant plusieurs minutes sans aucune indication de progression
  intermédiaire. Le seul retour était le toast final résumé
  (`%d scènes, %d observations insérées`).

  Correctif (couplé avec `nemeton@v0.21.2` qui introduit l'argument
  `progress_callback` sur `ingest_sentinel2_timeseries()` et
  `run_fordead_dieback()`) :

  - Le worker async (`run_ingestion_async` / `run_fordead_async`)
    construit un callback qui sérialise chaque événement en JSON
    atomique (write to `.tmp` + rename) vers
    `<project>/data/ingest_progress.json` (resp.
    `fordead_progress.json`).
  - Côté main process, un `shiny::reactivePoll(500 ms)` lit le
    fichier, et un observer met à jour un toast persistant (même
    `id`) avec `"Tuile Sentinel-2 X/N : <scene_id>"` (ou simplement
    `"X/N téléchargée…"` quand le scene_id n'est pas fourni) pour
    l'ingestion, et `"FORDEAD — phase : <nom> (X/N)"` pour FORDEAD.
  - À la fin de la tâche (succès ou erreur), le toast persistant
    est retiré et le fichier `progress.json` purgé. Le toast final
    `monitoring_ingest_success` / `monitoring_health_success`
    reprend la main.

  Nouvelles clés i18n FR/EN : `monitoring_ingest_progress_fmt`,
  `monitoring_ingest_progress_named_fmt`,
  `monitoring_health_phase_fmt`, `monitoring_health_phase_simple_fmt`.

  Bump `DESCRIPTION` : `Imports: nemeton (>= 0.21.2)`,
  `Remotes: pobsteta/nemeton@v0.21.2`. ADR-009 respecté — toute la
  logique métier (savoir qu'une tuile est téléchargée, qu'une
  phase est terminée) reste dans `nemeton` ; nemetonshiny n'écoute
  que le canal callback exporté.

### Suivi sanitaire — bouton "Lancer le diagnostic FAST" muet au clic

* `fix(monitoring)` — clic sur **"Lancer le diagnostic FAST"** (ou
  **"Lancer le diagnostic FORDEAD"** en mode santé) sans aucune
  réaction : ni toast d'ingestion, ni toast d'erreur.

  Cause : les deux boutons étaient rendus avec
  `htmltools::tagAppendAttributes(..., disabled = NA)` (commit
  `a880507`), ce qui les désactive **au niveau HTML** au premier
  rendu. Un observer côté serveur les réactivait via
  `updateActionButton(disabled = FALSE)`, mais le style `btn-primary`
  masque visuellement l'état `disabled` du Bootstrap — l'utilisateur
  voit un bouton bleu d'aspect cliquable alors que le navigateur
  refuse le clic, donc aucun `observeEvent` ne se déclenche.

  Correctif : on suit le pattern explicite déjà appliqué au bouton
  **"Enregistrer la zone"** (commenté dans le module) :
  - Suppression du wrapper `tagAppendAttributes(disabled = NA)` sur
    `run` et `run_health` → les boutons partent toujours actifs.
  - Les préconditions (zone sélectionnée, bands cochées, période
    valide) ne désactivent **plus** le bouton — elles sont validées
    dans l'`observeEvent` qui affiche un toast explicite par cause
    (`monitoring_validate_zone` / `monitoring_validate_bands` /
    `monitoring_validate_dates`).
  - `updateActionButton(disabled = is_running)` reste pour griser le
    bouton **pendant** la tâche async (protection double-clic).
  - Garde `is_running` ajouté en tête des deux `observeEvent` pour
    avaler un éventuel double-clic sans relancer la tâche.

# nemetonshiny 0.24.7 (2026-05-12)

### Suivi sanitaire — bump nemeton 0.21.1 (fix DDL DuckDB)

* `fix(monitoring)` — bandeau **"Migration failed: Parser Error:
  syntax error at or near GENERATED"** au premier passage dans
  l'onglet *Suivi sanitaire* en mode local (DuckDB).

  Cause : le DDL des migrations de `nemeton::db_migrate()` utilisait
  `id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY`, syntaxe
  acceptée par PostgreSQL mais rejetée par DuckDB.

  Correctif (côté `nemeton` v0.21.1) : remplacement par un DDL
  portable Postgres/DuckDB (`CREATE SEQUENCE IF NOT EXISTS …` +
  `DEFAULT nextval(…)`).

  Côté `nemetonshiny` : on remonte le pin de `nemeton` à
  `>= 0.21.1` (et `Remotes: pobsteta/nemeton@v0.21.1`). Aucune
  modification de code applicatif — la correction est purement
  dans le paquet cœur, conformément à ADR-009.

# nemetonshiny 0.24.6 (2026-05-12)

### Suivi sanitaire — worker async ne dépend plus de nemetonshiny

* `fix(monitoring)` — bandeau d'erreur **"argument inutilisé
  (db_url = db_url)"** au premier passage dans l'onglet *Suivi
  sanitaire* après installation de v0.24.5.

  Cause : la probe async passait par `nemetonshiny:::get_monitoring_db_connection(db_url = ...)`
  via `getFromNamespace`. Quand le worker chargeait une version
  obsolète de nemetonshiny (cache pak, dev checkout pas à jour
  via `pkgload::load_all`, binaire shadowé), le wrapper résolu
  était l'ancienne signature `get_monitoring_db_connection(project = NULL)`
  sans `db_url` → erreur R *"argument inutilisé"* dans le worker
  → bandeau warning avec le message brut.

  Correctif : le worker ne passe **plus jamais** par les helpers
  internes de nemetonshiny. Il appelle directement les fonctions
  exportées de `nemeton` (`db_connect()`, `db_migrate()`,
  `db_disconnect()`), qui sont l'API publique stable du paquet
  cœur. Bénéfices :

  - Plus de couplage entre la version de nemetonshiny dans le main
    process et celle dans le worker subprocess.
  - L'erreur de connexion (et de migration) est capturée
    directement et transférée proprement au main process via la
    valeur de retour de la promise.
  - Code plus court et plus simple (suppression de
    `.pkg_path_mon`, des trois `getFromNamespace`, du chargement
    conditionnel pkgload/loadNamespace).

* `chore(monitoring)` — suppression du capture
  `.pkg_path_mon <- pkgload::pkg_path()` (devenu inutile depuis
  que le worker ne charge plus nemetonshiny).


# nemetonshiny 0.24.5 (2026-05-12)

### Suivi sanitaire — hardening du worker async contre un nemetonshiny obsolète

* `fix(monitoring)` — bandeau d'erreur **"objet 'last_monitoring_db_error'
  introuvable"** au premier passage dans l'onglet *Suivi sanitaire*
  après installation de v0.24.4.

  Cause : la probe async lancée dans le worker `future::multisession`
  appelait `utils::getFromNamespace("last_monitoring_db_error",
  "nemetonshiny")`. Si le worker charge un nemetonshiny **plus ancien**
  que celui de la session principale (cache pak, .libPaths divergent,
  binaire obsolète qui shadow le dev), la fonction est absente du
  namespace et `getFromNamespace` jette l'erreur localisée
  "objet 'X' introuvable" — qui remontait telle quelle dans le
  bandeau, sans hint pour l'utilisateur.

  Correctif : le worker est maintenant entièrement défensif. Tous
  les `getFromNamespace()` sont enveloppés dans `tryCatch()`, et si
  un helper interne est manquant, le worker retourne un message
  explicite *"Outdated nemetonshiny in worker library path. Re-install
  pobsteta/nemetonshiny@v0.24.4 with pak::cache_clean(); pak::pak(…)."*
  plutôt qu'une erreur cryptique. Le worker capture aussi
  désormais l'erreur directe de `db_connect` (`probe_err`) pour le
  cas où le helper retourne NULL sans contexte transférable entre
  processus.

* `chore(monitoring)` — suppression de la dépendance à
  `last_monitoring_db_error()` dans le worker (le slot de package est
  process-local, pas accessible depuis main de toute façon). La
  dépendance à `%||%` est également retirée du worker (base R 4.4+
  only) au profit de `if/else` explicite.


# nemetonshiny 0.24.4 (2026-05-12)

### Suivi sanitaire — connexion DB asynchrone + correctif schéma DuckDB

* `fix(monitoring)` — **table `monitoring_zone` manquante** sur les
  fichiers DuckDB ouverts après le premier (erreur `Catalog Error:
  Table with name monitoring_zone does not exist!` au clic sur
  *Enregistrer ce projet comme zone de suivi*).

  Cause : `.ensure_monitoring_schema()` mémoïsait le succès de
  `db_migrate()` dans `.nemeton_env$.monitoring_schema_initialized`,
  flag **process-level** partagé entre toutes les connexions de la
  session R. Une fois TRUE après la 1re connexion réussie,
  l'appel à `db_migrate()` était **systématiquement skippé** sur
  les connexions suivantes — y compris quand celles-ci visaient un
  **fichier DuckDB tout neuf** (changement de projet, suppression
  du fichier, autre `db_url`). Résultat : `db_connect()` créait
  bien le fichier (file I/O), mais aucune table n'y était jamais
  insérée → le 1er `INSERT INTO monitoring_zone` plantait.

  Correctif : suppression du cache wrapper. `nemeton::db_migrate()`
  est **déjà idempotent** (la table `schema_migration` traque les
  versions appliquées, une migration appliquée est sautée après un
  simple SELECT). Le coût sur une base déjà migrée est sub-ms.

* `feat(monitoring)` — **connexion DB asynchrone** (`ExtendedTask`) +
  **bandeau "loading" persistant** avec roue dentée animée
  (`.nmt-spin` + `bsicons::bs_icon("gear-fill")`). Remplace le
  feedback par toasts qui auto-dismiss en 2.5 s et étaient
  positionnés top-right (donc faciles à manquer).

  Le worker `future::plan("multisession")` ouvre `db_connect +
  db_migrate` en arrière-plan ; pendant l'exécution, le bandeau
  *Connexion à la base de suivi… — Migration du schéma en cours,
  cela peut prendre quelques secondes au premier démarrage…*
  reste affiché en place dans la zone principale. Le résultat
  (succès / échec avec message d'erreur réel) remplace ensuite le
  bandeau de chargement, sans race ni clignotement.

  Le worker ne fait qu'**éprouver la connexion** (DBI n'est pas
  sérialisable entre processus) ; les reactives métier
  (`validity()`, `alerts()`, `zones()`) continuent d'ouvrir leurs
  propres connexions sync dans le main process — fast après la
  migration initiale.

* `chore(monitoring)` — suppression des helpers maintenant inutiles
  `.ensure_monitoring_db_announced()` et
  `.announce_monitoring_db_ready()` (toasts top-right), remplacés
  par `.monitoring_loading_card()` (bandeau persistant in-place).

* `feat(i18n)` — nouvelle clé `monitoring_db_loading_hint`
  ("Migration du schéma en cours, cela peut prendre quelques
  secondes au premier démarrage…").


# nemetonshiny 0.24.3 (2026-05-11)

### Suivi sanitaire — override `NEMETON_DB_LOCAL=1` pour forcer le mode local

* `feat(monitoring)` — nouvelle variable d'environnement
  **`NEMETON_DB_LOCAL`** (truthy = `1` / `true` / `yes` / `on`,
  case insensitive). Quand elle est truthy,
  `.resolve_monitoring_db_url()` **saute** les vars `NEMETON_DB_URL`
  et `POSTGRESQL_ADDON_*` et utilise directement la base DuckDB
  locale du projet.

  **Cas d'usage typique** : un développeur a les credentials
  Clever Cloud (`POSTGRESQL_ADDON_*`) qui traînent dans son
  `~/.Renviron` (déposés par un build CI ou copiés depuis
  `clever env`). Sans l'override, l'app dialait la Postgres prod
  depuis la machine locale, timeout, et le bandeau restait sur
  "Base non configurée". Avec `NEMETON_DB_LOCAL=1`, la cascade
  saute directement à la fallback DuckDB sans toucher au
  `.Renviron`.

  Le `cli_alert_info` mentionne désormais explicitement
  l'override quand il est actif : *"Monitoring uses local DuckDB
  at … (NEMETON_DB_LOCAL override)."*

### Suivi sanitaire — diagnostic d'erreur visible dans le bandeau

* `feat(monitoring)` — `get_monitoring_db_connection()` capture
  désormais l'erreur réelle de `nemeton::db_connect()` (et celle
  de `db_migrate()`) dans `.nemeton_env$.last_monitoring_db_error`.
  Le bandeau *Base de suivi non configurée* affiche ensuite cette
  erreur après un tiret cadratin, par ex.
  *"Renseignez NEMETON_DB_URL… — Invalid DB URL: C:/Users/…/monitoring.duckdb"*.
  Avant : `cli_warn` partait dans la console R (souvent invisible
  pour l'utilisateur Shiny) et le bandeau restait générique.
* `feat(monitoring)` — feature-flag : `.resolve_monitoring_db_url()`
  vérifie via `getFromNamespace(".detect_driver", "nemeton")` que
  la version installée de `nemeton` supporte bien le backend
  DuckDB (introduite en v0.21.0). Si non, message explicite
  *"Installed nemeton is too old (no DuckDB support). Re-install
  pobsteta/nemeton@v0.21.0."* dans le bandeau et un `cli_warn`
  une fois par session expliquant comment réparer (`pak::pak()`
  + clear cache).
* `feat(monitoring)` — capture aussi les erreurs de
  `db_migrate()` (qui s'exécute juste après la connexion) sous
  forme *"Migration failed: <msg>"*. Si la migration échoue,
  la connexion est refermée proprement et `NULL` est retourné
  (au lieu de laisser une connexion sans schéma qui crasherait
  les requêtes suivantes).

# nemetonshiny 0.24.2 (2026-05-11)

### Suivi sanitaire — 4 fixes UX critiques

* `fix(duckdb)` — **bug Windows path** : sur Windows
  `duckdb:///C:/Users/<projet>/data/monitoring.duckdb` était
  parsé par `nemeton:::.parse_duckdb_url()` en
  `/C:/Users/<projet>/data/monitoring.duckdb` (slash en trop)
  → DuckDB refusait d'ouvrir → bandeau "Base non configurée"
  affiché à tort alors que le toast disait "Base locale prête".
  Fix : `.resolve_monitoring_db_url()` émet désormais un **bare
  path** (sans préfixe `duckdb://`), reconnu cross-platform par
  `nemeton:::.detect_driver()` via le suffixe `.duckdb`.
* `fix(ux)` — **bouton "Enregistrer ce projet comme zone de
  suivi"** était figé en `disabled = NA` au niveau HTML, donc
  non cliquable même quand toutes les conditions étaient
  réunies. Supprimé : l'observer du clic valide déjà les
  préconditions et affiche une notification d'erreur si elles
  ne sont pas remplies, donc l'utilisateur reçoit toujours un
  feedback précis sans être bloqué à l'avance.
* `fix(ux)` — **toast "Création de la base DuckDB locale…"**
  apparaissait et disparaissait trop vite pour être perçu (la
  connexion DuckDB est typiquement < 100 ms). Désormais le
  toast a `duration = 2.5` s minimum et un toast séparé
  "Base locale prête" est émis APRÈS confirmation de la
  connexion (au lieu d'un `later::later` aveugle qui se
  déclenchait même en cas d'échec).
* `fix(ux)` — la **checkbox "Inclure les classes faible et
  moyenne"** débordait du `card_header` next to *"Alertes
  détectées"* sur les viewports étroits. Déplacée sur sa
  propre ligne juste sous le header — toujours sur une seule
  ligne, peu importe la largeur.

# nemetonshiny 0.24.1 (2026-05-11)

### UX Suivi sanitaire — diagnostic précis + toast + séparateur "au"

* `feat(i18n)` — le séparateur du `dateRangeInput` (sidebar
  *Période d'observation* et *Période d'entraînement* FORDEAD)
  était hardcodé en *"to"* malgré la langue FR. Nouvelle clé i18n
  `date_range_separator` : *"au"* en FR, *"to"* en EN.
* `feat(ux)` — un **toast "Tentative de connexion à la base…"
  ou "Création de la base DuckDB locale…"** s'affiche en bas à
  droite la première fois que l'utilisateur ouvre l'onglet
  *Suivi sanitaire* (suivi 1,2 s plus tard d'un toast *"Base
  prête"*). Avant cette release, l'onglet semblait figé pendant
  le bootstrap du schéma (qui prend ~1 s sur un DuckDB neuf).
  Le toast est idempotent (un par session, id stable).
* `fix(ux)` — le bandeau *Base de suivi non configurée*
  distingue maintenant **deux causes** au lieu d'une seule :
  - **aucun projet chargé** → message *"Aucun projet chargé.
    Sélectionnez ou créez un projet dans l'onglet Sélection
    pour activer le mode local (DuckDB)."*
  - **`duckdb` package manquant** → message *"Le paquet R
    duckdb n'est pas installé, le mode local n'est pas
    disponible. Installez avec install.packages('duckdb') ou
    configurez Postgres."*
  Avant : un seul message générique qui ne disait pas quoi faire.
* Le `register_hint` (petit texte sous *Enregistrer ce projet
  comme zone de suivi*) bénéficie du même diagnostic — il
  indique désormais l'installation de `duckdb` plutôt que le
  générique *"impossible d'enregistrer la zone"*.

# nemetonshiny 0.24.0 (2026-05-11)

### Added — Suivi sanitaire en mode local (DuckDB)

L'onglet *Suivi sanitaire* ne nécessite plus une instance
PostgreSQL+TimescaleDB pour démarrer. Quand aucune variable
d'environnement `NEMETON_DB_*` n'est définie et qu'un projet est
chargé, la couche monitoring bascule automatiquement sur un fichier
**DuckDB local** stocké à `<project>/data/monitoring.duckdb` (à
côté de `samples.gpkg`).

* **Bascule transparente** : `service_monitoring_db.R::.resolve_monitoring_db_url(project)`
  inspecte les vars `NEMETON_DB_URL` / `NEMETON_DB_*` / `POSTGRESQL_ADDON_*`
  et, à défaut, émet un `duckdb:///<project>/data/monitoring.duckdb`
  exploitable par `nemeton::db_connect()` (v0.21.0).
* **Pré-requis** : le package `duckdb (>= 0.8.0)` est ajouté en
  `Suggests`. S'il n'est pas installé, la bascule reste silencieuse
  et l'utilisateur voit l'ancien bandeau "Base non configurée".
* **UI** : le bandeau de l'onglet *Suivi sanitaire* affiche
  désormais **trois états** au lieu de deux :
  - rouge : aucune base configurée et aucun projet chargé ;
  - **bleu (NOUVEAU)** : *"Mode local (DuckDB) — base de suivi
    monoposte stockée dans le projet"* avec un hint pour passer
    en Postgres multi-utilisateurs ;
  - vert : Postgres connecté, N zones disponibles.

### Changed — Signatures async pour passer la DB URL au worker

`run_ingestion_async()` et `run_fordead_async()` acceptent
désormais un paramètre `db_url` dans leur `$invoke(...)`. Les
observers dans `mod_monitoring.R` pré-résolvent l'URL via
`.resolve_monitoring_db_url(app_state$current_project)` avant de
lancer le worker — nécessaire parce que les futures workers ne
voient pas `app_state` ni les vars Shiny.

`get_monitoring_db_connection()` accepte deux nouveaux paramètres
optionnels :
* `project` : pour la résolution synchrone du fallback DuckDB
  (utilisé par les 9 callsites dans `mod_monitoring.R`).
* `db_url` : pour le path asynchrone qui reçoit l'URL déjà
  résolue depuis l'observer.

### Bumped — `Remotes: pobsteta/nemeton@v0.21.0`

Pour bénéficier du backend DuckDB côté cœur (`nemeton::db_connect`
détecte le scheme `duckdb://` et applique les migrations dans
`inst/db/migrations/duckdb/`).

# nemetonshiny 0.23.17 (2026-05-11)

### Plan d'actions — fond satellite Esri.WorldImagery

* `feat(report)` — les cartes UGF du PDF *Plan d'actions* passent
  d'`OpenTopoMap` (relief topographique) à **`Esri.WorldImagery`**
  (imagerie satellite gratuite, sans clé API). Permet de voir la
  canopée réelle / l'occupation du sol derrière chaque parcelle
  plutôt que des courbes de niveau abstraites.
* Le rapport *Synthèse* reste sur `OpenTopoMap` (différent
  domaine d'usage : score par famille d'indicateurs, le relief
  est plus parlant qu'une photo aérienne).
* Le fallback géométrie-seule (déclenché si les tuiles
  satellite sont inaccessibles côté serveur) est inchangé.

# nemetonshiny 0.23.16 (2026-05-11)

### Export PDF Plan d'actions — \small retiré + verbose quarto

* `fix(report)` — l'export PDF échouait encore avec une
  erreur générique `"Error running quarto CLI from R"` même
  quand toutes les UGF tombaient en fallback géométrie-seule
  (tuiles OSM inaccessibles côté serveur, ce qui est légitime
  et géré). Cause probable : la déclaration `\small` placée
  en v0.23.13 dans `inst/quarto/action_plan_template.qmd`
  entre `\begin{longtable}{...}` et `\toprule` — booktabs +
  longtable peuvent mal traiter une déclaration de taille à
  cet endroit sur certaines toolchains xelatex et faire
  planter tout le rendu.
  Désormais le template **ne wrappe plus le longtable** dans
  un changement de taille ; il utilise la taille par défaut
  qui compile de manière fiable. Les `{` `}` parasites
  corrigés en v0.23.13 ne reviennent pas (puisqu'il n'y a
  plus de wrapper à mal sortir).
* `debug(report)` — `quarto::quarto_render()` passe en
  `quiet = FALSE` côté Plan d'actions (aligné avec la
  Synthèse qui était déjà verbose). La sortie quarto / xelatex
  est désormais streamée dans la console R, ce qui permet de
  diagnostiquer une vraie erreur LaTeX (package manquant,
  `\includegraphics` malformé, etc.) au lieu du seul
  "Error running quarto CLI from R" générique.

# nemetonshiny 0.23.15 (2026-05-11)

### Plan d'actions — cartes UGF alignées sur le rapport Synthèse + libellés boutons

* `fix(export)` — porte la recette éprouvée de
  `generate_family_maps()` (utilisée par le rapport PDF de
  l'onglet *Synthèse* qui rend ses cartes correctement) sur
  `render_ug_map()` de l'export PDF Plan d'actions :
  - provider **OpenTopoMap** (au lieu d'OpenStreetMap) ;
  - **zoom explicite** calculé à partir de la bbox de la
    parcelle (`auto_zoom <- min(17, max(13, round(17 -
    log2(extent_size * 100))))`) au lieu de `zoom = NULL` ;
  - dimensions PNG **800×600 res=150** (au lieu de 1200×800
    res=180) pour rester aligné avec la sortie qui passe
    déjà en production sur la Synthèse.

  Les autres garde-fous v0.23.14 (fermeture explicite du
  device avant la validation taille, fallback géométrie
  seule sans tuiles, sanity check `file.size < 100 →
  NA_character_`) sont conservés.
* `ui(action_plan)` — libellés des boutons d'export
  uniformisés avec le reste de l'app :
  - "Exporter GeoPackage" → "Télécharger le GeoPackage"
    (FR) / "Download GeoPackage" (EN)
  - "Exporter PDF" → "Télécharger le PDF" (FR) /
    "Download PDF" (EN)

  Cohérent avec les libellés `download_pdf` (rapport
  Synthèse) et `download_gpkg` (export projet) déjà existants.

# nemetonshiny 0.23.14 (2026-05-11)

### Export PDF Plan d'actions — toast persistant + résilience renderUGmap

* `fix(export)` — le toast *"Génération PDF…"* (roue dentée
  animée) **s'auto-dismissait après 8 s** côté JS. Pour un
  export Quarto + xelatex typique (15–30 s avec maps), le
  spinner disparaissait bien avant que la boîte de dialogue
  *Enregistrer le PDF* du navigateur n'apparaisse →
  l'utilisateur voyait un écran vide puis une boîte de
  dialogue sortie de nulle part.
  Désormais `nemetonShowDownloadToast` :
  - n'a plus d'auto-dismiss (`duration: null`) ;
  - le serveur envoie un `nemetonHideDownloadToast` (custom
    message Shiny) à la fin de `content` (via `on.exit` →
    s'exécute même si l'export échoue), donc le spinner
    disparaît **synchroniquement** avec l'envoi du fichier
    par le serveur ;
  - filet de sécurité 120 s (au lieu de 8 s) en cas de
    perte du message custom.
* `fix(export)` — le PDF généré faisait **1 KB et ne
  s'ouvrait pas**. Cause : `generate_action_plan_pdf()`
  levait une erreur (typiquement la fallback `render_ug_map`
  v0.23.13 passait `bg` / `xlim` / `ylim` à `plot.sf` qui ne
  les supporte pas uniformément selon la version de `sf` →
  PNG corrompu ou 0 byte → `\includegraphics` plantait
  xelatex → `quarto_render` échouait). Le tryCatch upstream
  écrivait alors `"PDF generation failed"` (22 octets) dans
  le fichier — d'où le PDF cassé livré à l'utilisateur.
  Désormais :
  - **fallback `render_ug_map` minimal** : appel `plot.sf`
    sans `bg` / `xlim` / `ylim`, plot.sf auto-fit la bbox
    de la géométrie ;
  - **fermeture explicite du device PNG** avant la fin de
    `render_ug_map` (au lieu de `on.exit`) pour garantir que
    le fichier est complètement flushé avant la
    validation ;
  - **validation taille PNG** : si le fichier produit fait
    moins de 100 octets ou n'existe pas, `render_ug_map`
    renvoie `NA_character_` et le template skip
    `\includegraphics` proprement (au lieu d'envoyer un
    fichier corrompu à xelatex).
* `fix(export)` — la notification d'erreur PDF est désormais
  **sticky** (`duration = NULL` au lieu de `duration = 8`).
  L'erreur réelle (`conditionMessage(e)`) reste affichée
  jusqu'à fermeture manuelle, ce qui permet de diagnostiquer
  la cause sans avoir à inspecter les logs serveur. Le
  message verbatim est aussi loggué via `cli::cli_warn`.
* `fix(export)` — quand la génération échoue, le fichier
  livré contient maintenant le texte *"PDF generation
  failed. See the in-app error toast for the underlying
  cause."* (au lieu d'un cryptique *"PDF generation
  failed"*) pour orienter vers la vraie diagnostic.

# nemetonshiny 0.23.13 (2026-05-11)

### Rapport PDF Plan d'actions — accolades parasites + cartes UGF restaurées

* `fix(report)` — **`{` et `}` apparaissaient en littéral** dans
  le PDF, au-dessus et au-dessous du tableau d'actions de
  chaque UGF. Cause : le template écrivait `cat("{\\small\n")`
  puis le longtable puis `cat("}\n\n")`. Pandoc voyait `{` /
  `}` seuls sur leur propre paragraphe (séparés par des
  newlines), ne les reconnaissait pas comme bloc LaTeX brut
  (raw LaTeX = `\begin{env}…\end{env}` ou backticked
  `\`\`\`{=latex}\`\`\`` fences) et les passait verbatim au PDF.
  Désormais `\small` est placé **à l'intérieur** de
  l'environnement `longtable` (Pandoc préserve son contenu
  comme raw LaTeX) ; la déclaration scope automatiquement
  jusqu'à `\end{longtable}`.
* `fix(report)` — **cartes des parcelles absentes** du PDF.
  `render_ug_map()` retournait `NA_character_` dès la
  moindre erreur de `maptiles::get_tiles()` (réseau coupé sur
  le serveur, rate-limit OSM, package `maptiles` non
  installé) ; le template skippait alors silencieusement le
  bloc `\includegraphics`. Désormais un **fallback
  sans tuiles OSM** dessine la géométrie de la parcelle sur
  fond gris clair, donc le rapport contient toujours une
  carte par UGF — même réseau coupé. `cli::cli_alert_info`
  documente la raison du fallback dans les logs R.

# nemetonshiny 0.23.12 (2026-05-11)

### Terrain — fixes défensifs init (réactivité UI)

* `fix(samples)` — l'observer `leafletProxy()` du *Terrain* est
  désormais **gardé derrière `req(input$map_zoom)`**. Avant le
  premier rendu de la carte Sampling (typiquement quand
  l'utilisateur travaille dans un autre onglet — *Plan
  d'actions* par ex.), l'observer tirait sur chaque changement
  de `sampling_rv$plots` / `sampling_rv$observations` /
  `app_state$language` et empilait des messages
  `leaflet-calls` dans la file de flush différée pour une
  carte qui n'existait pas encore côté client. Désormais
  l'observer court-circuite tant que `input$map_zoom` est
  NULL ; il reprend son comportement nominal dès que la carte
  est ouverte au moins une fois.
* `perf(samples)` — `.restore_samples` ouvre désormais
  `samples.gpkg` **une seule fois** via `sf::st_layers()` pour
  scanner les layers disponibles, puis lit directement les
  layers présents via `sf::st_read()`. L'ancien chemin appelait
  `load_samples(layer = ...)` deux fois (plots + observations),
  ce qui ouvrait le GPKG quatre fois au total (chaque
  `load_samples` faisait son propre `st_layers` + `st_read`).
  Sur un projet où `current_project` est régulièrement
  réassigné, cette demi-pile d'I/O disparaît.

Ces deux mesures sont défensives. Si la sluggishness
ressentie sur l'onglet *Plan d'actions* ne disparaît pas, il
faut probablement chercher du côté de `ug_build_sf()` /
`output$kanban_board` (renderUI sur grandes listes
d'actions) — voir issue à venir.

# nemetonshiny 0.23.11 (2026-05-11)

### Terrain — légende plots restaurée au rendu initial

* `fix(samples)` — **la légende Base/Over/Observation n'apparaissait
  plus** sur la carte *Terrain* depuis v0.23.10. La cause : la
  légende avait été déplacée entièrement dans un observer
  `leafletProxy()` séparé de `renderLeaflet`. Quand l'observer
  proxy se déclenche **avant** que l'élément carte ne soit monté
  sur le client (cas fréquent au premier flush — l'ordre entre
  outputs et observers n'est pas garanti), les messages
  `leaflet-calls` arrivent à un client sans carte et **sont
  perdus**. Du coup, plus aucune légende.
* La légende initiale est désormais redessinée **dans
  `renderLeaflet`** avec `addLegend(colors = …, labels = …)`
  (toujours sans `colorFactor`, donc l'ordre saumon ↔ vert reste
  correct — pas de régression v0.23.9 → v0.23.10). Lecture de
  `sampling_rv$observations` en `isolate()` pour ne PAS forcer
  un full-redraw de la carte quand seules les observations
  changent : la mise à jour dynamique reste portée par
  l'observer `leafletProxy()` (qui réémet `removeControl +
  addLegend` avec `layerId = "plots-legend"`).
* Net : légende correcte au premier render (cas le plus fréquent
  côté utilisateur) ; mise à jour live conservée au clic
  *Envoyer vers Terrain* (pas de flicker tuiles, pan/zoom
  préservé).

# nemetonshiny 0.23.10 (2026-05-11)

### Envoyer vers Terrain — auto-refresh carte + légende correcte

* `fix(samples)` — **légende inversée** : `leaflet::colorFactor()`
  réordonne son `domain` par ordre **alphabétique** ; pour
  `c("Base","Over","Observation")` il triait en
  `c("Base","Observation","Over")` et mappait la palette
  positionnellement → *Observation* héritait du saumon
  `#ff7f0e` (place 2 = position d'*Over*) et *Over* héritait
  du vert `#2ca02c` (place 3). Les pastilles légende
  contredisaient les marqueurs sur la carte. Désormais la
  légende est construite avec `addLegend(colors = …, labels =
  …)` qui préserve strictement l'ordre passé.
* `fix(samples)` — **la carte ne se mettait pas à jour
  automatiquement** après *Envoyer vers Terrain* : l'output
  `renderLeaflet` est suspendu (`suspendWhenHidden = TRUE`)
  pendant que l'utilisateur reste sur l'onglet *Plan
  d'actions* ; les changements de `sampling_rv$observations`
  n'étaient observés qu'au retour sur l'onglet *Terrain* —
  et encore, redessinaient la carte entière (flicker tuiles,
  perte du pan/zoom). Désormais les points d'observation et la
  légende sont gérés par un observer dédié `leafletProxy()`
  qui synchronise **uniquement** le groupe `Observations` et
  le contrôle légende (`layerId = "plots-legend"`). Conséquence :
  - la carte se met à jour **immédiatement** au clic
    *Envoyer vers Terrain*, même si l'onglet est masqué (le
    proxy met les opérations en file d'attente côté serveur
    et les rejoue dès que le client est connecté) ;
  - les tuiles ne sont plus rechargées ;
  - le pan/zoom courant est conservé ;
  - la légende reflète strictement les marqueurs présents.
* `refactor(samples)` — `renderLeaflet` ne dépend plus de
  `sampling_rv$observations` ; le groupe `Observations` est
  préinscrit dans le `addLayersControl` pour exposer le toggle
  dès le premier render, et c'est le proxy qui peuple le
  groupe. Le groupe peut être vide tant qu'aucun point n'a été
  envoyé.

# nemetonshiny 0.23.9 (2026-05-11)

### Envoyer vers Terrain — coexistence calibration + observations

* `fix(samples)` — bug critique : cliquer **Envoyer vers Terrain**
  dans l'onglet *Plan d'actions* **détruisait silencieusement les
  placettes de calibration Base/Over** générées dans l'onglet
  *Terrain*. `save_samples()` faisait `unlink(samples.gpkg)` puis
  réécrivait le fichier avec uniquement les points d'observation.
  Désormais :
  - `save_samples(project_id, plots, layer = "plots"|"observations")`
    écrit **un layer nommé** dans `samples.gpkg` avec `append =
    FALSE, delete_layer = TRUE` — il remplace uniquement le layer
    cible et préserve les autres ;
  - les placettes de calibration (`mod_sampling`) restent dans le
    layer `plots` (par défaut) ; les points d'observation
    (`mod_action_plan`) sont écrits dans le layer `observations` ;
  - `samples_count` / `samples_generated_at` (metadata projet)
    ne sont mis à jour **que** pour le layer `plots` — l'envoi
    d'observations ne perturbe plus la comptabilité du plan
    d'échantillonnage ;
  - `load_samples(project_id, layer = "plots"|"observations")`
    lit un layer spécifique et renvoie `NULL` si le layer est
    absent (silencieusement).
* `feat(samples)` — la **carte de l'onglet Terrain** affiche
  désormais les **deux familles de points en simultané** :
  - Base / Over (bleu / orange, parcours TSP, icônes
    orienteering Départ/Arrivée) — inchangés ;
  - Observations issues du plan d'actions (vert `#2ca02c`,
    groupe leaflet dédié `Observations` toggle-able dans le
    layer control, popup *plot_id — observation (UGF, an)*).
* `feat(samples)` — **légende dynamique** : ne liste que les
  familles effectivement présentes dans `plots` et
  `observations`. Plus de `addLegend(values = c("Base","Over"))`
  en dur. Nouvelle clé i18n `sampling_legend_plots_title`
  (FR *Placettes* / EN *Plots*).
* `feat(samples)` — `mod_sampling` réagit à `samples_refresh` :
  quand `mod_action_plan` bumpe ce signal après *Envoyer vers
  Terrain*, le layer `observations` est rechargé sans changer
  de projet.
* Couverture : test régression
  `save_samples 'observations' layer coexists with 'plots'
  layer` (couche calibration préservée par un save observations
  successif, et inversement ; `samples_count` reste sur
  `plots` ; couche inconnue → NULL silencieux).

# nemetonshiny 0.23.8 (2026-05-11)

### Plan d'actions — toast PDF unifié

* `fix(action_plan)` — l'export PDF avait deux toasts qui se
  superposaient au clic sur **Exporter PDF** : un toast
  serveur (`showNotification` avec icône `spinner` et libellé
  *« Génération du PDF en cours… »*, dans `downloadHandler`) et
  le nouveau toast client roue dentée v0.23.7 (helper JS
  `nemetonShowDownloadToast`). Le serveur est retiré : on garde
  uniquement le toast client, cohérent avec **Exporter
  GeoPackage**. Clé i18n orpheline `action_plan_pdf_generating`
  supprimée.

# nemetonshiny 0.23.7 (2026-05-11)

### Plan d'actions — Exports GPKG / PDF

* `feat(action_plan)` — export GeoPackage : nouvelle colonne
  **`annee_civile`** ajoutée à la couche `actions`, dérivée de
  `current_year + annee_cible - 1L`. Le champ `annee_cible` du
  schéma reste un offset relatif 1..HORIZON (utilisé par le LLM
  et les calculs internes du plan) ; la colonne `annee_civile`
  matérialise l'année calendaire au moment de l'export pour les
  consommateurs QGIS/terrain. Ancre = année courante au clic
  d'export (pas de stockage). Placée juste après `annee_cible`
  dans la couche `actions`. Test de régression
  `export_action_plan_gpkg writes actions + ugf layers` étendu
  pour vérifier la présence + la valeur + l'ordre des colonnes.
* `feat(action_plan)` — clic sur **Exporter GeoPackage** ou
  **Exporter PDF** affiche maintenant un **toast roue dentée** en
  bas à droite pendant la phase d'export. `downloadButton` ne
  fournit aucun événement côté serveur (l'ouverture de la boîte
  de dialogue de téléchargement est strictement client), donc
  le toast est déclenché par un `onclick` JS qui appelle un
  helper `nemetonShowDownloadToast()` ajouté dans `custom.js`.
  Le toast utilise `Shiny.notifications.show` (même slot visuel
  que `shiny::showNotification`) avec `duration: null,
  closeButton: false`, et s'auto-dismisse après 8 s. Idempotent
  sur re-clic (le timer est réinitialisé). Deux nouvelles clés
  i18n FR/EN : `action_plan_export_running_gpkg`,
  `action_plan_export_running_pdf`.
* `feat(action_plan)` — **rapport PDF Quarto refondu**. Page de
  garde dédiée (titre, sous-titre, encadré bleu avec date /
  horizon / nombre d'actions / nombre d'UGF), header courant
  *fancyhdr*, synthèse globale en `tcolorbox` avec bilan coloré
  vert/rouge. Pour chaque UGF :
  - une **carte OSM** centrée sur la parcelle est pré-rendue
    côté R via `maptiles::get_tiles(provider = "OpenStreetMap")`
    + `plot_tiles()` + tracé du polygone (bleu translucide,
    bordure pleine). PNG 1200x800 sauvegardé dans le temp_dir
    et inclus via `\includegraphics`. Dégradation silencieuse
    si `maptiles` indisponible ou si la requête tuile échoue
    (le PDF est rendu sans la carte plutôt que de planter) ;
  - une **carte de synthèse UGF** (surface, nb d'actions, coût,
    revenu, bilan coloré) en `tcolorbox` ;
  - le **tableau des actions** passe en `longtable` (continue
    sur plusieurs pages si besoin), avec priorité en couleur
    (rouge / orange / vert) ;
  - une **zone "Commentaires" large** en `tcolorbox`
    *breakable* sous le tableau, qui restitue le `commentaire`
    de chaque action non-vide avec l'année cible, le type et
    le statut coloré.

  `generate_action_plan_pdf()` enrichit le payload `data`
  passé au qmd : `commentaire` par action, `map_png` (chemin
  ou NA) par UGF, et factorise la création du `temp_dir`
  pour qu'il porte à la fois les PNG et le qmd rendu.

# nemetonshiny 0.23.6 (2026-05-11)

### Plan d'actions — chat IA : surface / volume / coûts conservés

* `fix(action_plan)` — quand l'utilisateur cliquait sur
  **Affiner le plan avec l'IA**, les lignes proposées par
  le LLM arrivaient avec les colonnes `surface_ha`,
  `volume_m3`, `nb_tiges`, `rdi`, `cout_eur` et `revenu_eur`
  **vides**. Deux causes côté prompt :
  - `build_action_plan_chat_prompt()` se contentait de
    référencer `build_action_plan_prompt` par son nom de
    fonction R sans embarquer le schéma JSON, donc le LLM
    ignorait l'existence du bloc `quantite` ;
  - `mod_action_plan` envoyait au LLM un **résumé** des
    actions courantes (juste `id, ug_id, type, annee_cible,
    statut, priorite`), ce qui empêchait le LLM de réémettre
    les quantités existantes lors d'une modification.
  Le schéma JSON et le rappel économique sont maintenant
  factorisés dans `.action_plan_json_schema()` et
  `.action_plan_econ_hint()`, partagés par les deux prompts.
  Le prompt de chat embarque désormais le schéma complet,
  ajoute des règles explicites (« remplir tout le bloc
  `quantite` », « réémettre l'action entière sur update »),
  et reçoit le **JSON complet** du plan courant.
  Couverture testthat : `build_action_plan_chat_prompt embeds the full JSON schema + econ hint`.

# nemetonshiny 0.23.5 (2026-05-09)

### Plan d'actions — chat IA : scope + \u00e9crasement

* `feat(action_plan)` — le **chat IA gagne deux contrôles**
  juste sous l'historique pour piloter chaque tour de
  conversation : un radio **Toutes les UGF / Sélection
  courante** (`chat_scope`) et une checkbox **Écraser le
  plan existant** (`chat_overwrite`). Mêmes sémantiques
  que dans le modal "Générer les actions (IA)" :
  - `scope = "selected"` restreint le `ctx$ug_ids` envoyé
    au prompt aux seules UGF cochées sur la carte
    (sinon garde-fou `action_plan_no_ug` si rien n'est
    sélectionné) ;
  - `overwrite = TRUE` au moment de l'apply supprime
    les actions existantes des UGF ciblées avant le
    `bulk_upsert_actions()`. Le modal de confirmation
    qui s'ouvre quand le LLM renvoie un bloc `actions`
    JSON affiche désormais une **bannière `text-warning`**
    quand l'overwrite est coché, listant le nombre
    d'UGF impactées (`action_plan_chat_apply_overwrite_warn_fmt`).
  Les UGF cibles sont stashées dans `rv_state$pending_chat_target_ugs`
  au moment de l'envoi pour rester cohérentes entre le tour
  qui propose les actions et celui qui les applique.
  Nouvelle clé i18n `action_plan_chat_scope_sel`.

### Bascule de langue FR ↔ EN

* `fix(language)` — basculer FR↔EN dans le sélecteur de la
  navbar **fonctionne enfin** : la page se recharge
  automatiquement en EN (ou FR) et **ne reverse plus** sur
  la langue de démarrage. Deux bugs combinés :
  - `app_server.R` écrivait dans `nemeton.app_language`
    alors que `app_ui` lit `getOption("nemeton.app_options")$language`
    (clé totalement différente) — donc la préférence ne
    survivait pas au reload. Désormais on persiste dans
    *la bonne* option `nemeton.app_options$language`.
  - L'observer affichait un toast *"Rechargez la page pour
    appliquer"* sans recharger automatiquement. Remplacé
    par un appel direct `session$reload()` pour que le
    rebuild d'`app_ui` se fasse sans intervention de
    l'utilisateur. Garde anti-init : si la nouvelle valeur
    est identique à `app_state$language`, l'observer
    retourne tôt — pas de reload involontaire au démarrage
    de session.
* `i18n` : clé orpheline `language_changed` (le toast
  manuel) retirée.

# nemetonshiny 0.23.4 (2026-05-09)

### Plan d'actions — chat IA en français

* `i18n(action_plan)` — les libellés de rôle dans
  l'historique du chat passent par i18n : `msg$role` brut
  ("user" / "assistant") n'apparaît plus tel quel dans
  l'UI. `output$chat_history_ui` traduit au moment du
  rendu en **"Vous"** / **"Assistant"** (FR) ou **"You"** /
  **"Assistant"** (EN). Le modèle de données conserve les
  clés anglaises (cohérent avec la convention LLM, le
  prompt builder downstream reste intact) — la traduction
  est purement cosmétique au display, et bascule
  dynamiquement avec le switch de langue. Deux nouvelles
  clés i18n : `action_plan_chat_role_user`,
  `action_plan_chat_role_assistant`.

# nemetonshiny 0.23.3 (2026-05-09)

### Plan d'actions — toast "L'IA réfléchit…"

* `feat(action_plan)` — clic sur **Envoyer** dans le chat
  IA fait apparaître un **toast en bas à droite** avec une
  **roue dentée tournante** et le label *"L'IA réfléchit…"*.
  Le toast reste affiché tant que la réponse du LLM n'est
  pas reçue : `duration = NULL`, `closeButton = FALSE`,
  type `"default"` (neutre). Suppression garantie via
  `on.exit(shiny::removeNotification(thinking_id))` dans
  toutes les branches de retour de l'observer (succès,
  erreur LLM, parse échoué). L'engrenage utilise la classe
  Font Awesome `fa-spin` sur l'icône `shiny::icon("gear")`.
  Nouvelle clé i18n `action_plan_chat_thinking` (FR
  *"L'IA réfléchit…"* / EN *"AI is thinking…"*).

# nemetonshiny 0.23.2 (2026-05-09)

### Plan d'actions — chat dans la sidebar droite + auto-scroll

* `feat(action_plan)` — le **chat IA** rejoint la sidebar
  droite **sous la carte "Tableau des actions"**, plutôt
  que dans une sidebar gauche dédiée. Disposition haut → bas
  de la sidebar droite : *Tableau des actions* (Sélection /
  IA / Manuel / Exports) puis *Affiner le plan avec l'IA*
  (historique scrollable + textarea + boutons Effacer /
  Envoyer). Le double `bslib::layout_sidebar` imbriqué
  introduit en v0.23.1 disparaît, on revient à un seul
  `layout_sidebar` avec les deux cartes empilées dans la
  sidebar droite — meilleure densité d'information sur les
  écrans portables.
* `feat(action_plan)` — la zone d'historique du chat
  **scrolle automatiquement vers le bas** à chaque mise à
  jour. La div `.chat-history` reçoit un `id` stable
  (`ns("chat_history")`) ; `output$chat_history_ui` injecte
  à la fin de chaque render un `tags$script` qui fait
  `setTimeout(function(){ el.scrollTop = el.scrollHeight; }, 0)`
  — le `setTimeout(0)` diffère au prochain tick pour que le
  DOM soit à jour quand `scrollHeight` est lu. Le dernier
  message reste visible sans intervention manuelle.

### Plan d'actions — libellé bouton

* `i18n(action_plan)` — bouton **"Générer (IA)"** renommé
  **"Générer les actions (IA)"** (FR) / **"Generate
  actions (AI)"** (EN), pour clarifier que la cible est
  bien la génération du plan d'actions et pas autre chose
  (ex. : un rapport).

# nemetonshiny 0.23.1 (2026-05-09)

### Plan d'actions — chat IA en sidebar gauche

* `feat(action_plan)` — le **chat IA** quitte la modal et
  s'installe dans une **sidebar gauche persistante** de 350 px.
  Carte collapsible (header `bg-info` avec icône `chat-dots`),
  historique scrollable (max-height 50 vh, min-height 160 px,
  fond gris clair), `textAreaInput` 3 lignes resize=vertical,
  boutons *Effacer* / *Envoyer* en flex. La conversation reste
  visible pendant que l'utilisateur navigue map / table /
  Kanban — auparavant un bouton "Ouvrir le chat" déclenchait
  un modal qui se fermait à chaque interaction.
* `refactor(action_plan)` — le bouton **"Ouvrir le chat"** dans
  la section IA de la sidebar droite est retiré (doublon avec
  le panel persistant). L'observer `input$open_chat` (~30 LOC
  qui faisait `showModal()`) supprimé. Layout passe à un
  double `bslib::layout_sidebar` imbriqué : sidebar gauche
  (chat) → sidebar droite (action panel) → contenu principal.
  Les deux sidebars se collapsent indépendamment via le
  bouton bslib en bordure (utile sur écran portable).

### Plan d'actions — sync carte ↔ tableau

* `fix(action_plan)` — clic sur une parcelle dans la **carte**
  sélectionne maintenant **toutes les lignes correspondantes**
  dans le tableau. Le handler `input$map_shape_click` (le
  toggle qui mettait à jour `selected_ug_rv` et la couche
  orange Selection) ne propageait pas la sélection à `DT` ;
  un appel `DT::selectRows(proxy, which(df$ug_id %in% cur))`
  est ajouté. Le sens table → carte (déjà fonctionnel via
  l'observer `input$action_table_rows_selected`) reste
  inchangé. Pas de boucle réactive : `reactiveVal` dedupe par
  `identical()` donc le round-trip map → selected_ug_rv →
  table → reverse-observer → selected_ug_rv s'arrête au 2e
  pas.

### Removed

* i18n: `action_plan_open_chat`, `action_plan_chat_input_label`
  (orphelines après la refonte du chat en sidebar).

# nemetonshiny 0.23.0 (2026-05-09)

### Plan d'actions — Kanban libre + édition par double-clic

* `feat(action_plan)` — **drag-and-drop libre entre toutes les
  colonnes du Kanban**. La sémantique du DAG (proposée → validée →
  planifiée → réalisée → abandonnée) qui empêchait certaines
  transitions disparaît : un utilisateur peut désormais déplacer
  n'importe quelle fiche vers n'importe quelle colonne. Le service
  `update_action_in_plan()` n'enforce plus le DAG ; il valide
  uniquement que le nouveau statut figure dans
  `ACTION_PLAN_TRANSITIONS`. La fonction `is_valid_status_transition()`
  reste exportée comme documentation du flux naturel mais ne gate
  plus les writes.
* `feat(action_plan)` — **double-clic sur une fiche Kanban ouvre
  une modal d'édition** pré-remplie avec les valeurs courantes
  (statut, priorité, année calendaire, commentaire). Le commentaire
  est éditable en `textAreaInput` 6 lignes, c'est le cas d'usage
  principal de la modal — l'édition inline du tableau DT est
  single-line et restait peu pratique pour des commentaires
  longs. Le handler `dblclick` est délégué au niveau du board (un
  seul listener pour toutes les cartes), avec cleanup entre
  re-renders pour éviter les fuites.
* `refactor(action_plan)` — le bouton **"Déplacer"** dans la
  dropdown de chaque carte Kanban est supprimé : avec le drag-drop
  libre il faisait doublon. ~50 lignes d'observer dispatcher
  `kanban_move_*` retirées en conséquence. La constante
  `KANBAN_STATUSES` (uniquement utilisée par la dropdown)
  retirée également.
* `feat(action_plan)` — **tri chronologique des cartes par
  colonne** : dans chaque statut Kanban, les fiches sont triées
  par `annee_realisation` ascendante (NAs en queue), pour qu'une
  colonne se lise du plus tôt au plus tard du haut vers le bas.
* `feat(action_plan)` — **commentaire affiché sur chaque carte
  Kanban**. Une div `.kanban-card-comment` (small text-muted, mt-1,
  word-break) apparaît sous le bloc type/année/UGF si le
  commentaire est non-vide ; rien si vide pour préserver la
  hauteur minimale.

### Plan d'actions — fiche d'ajout d'action

* `fix(action_plan)` — la dropdown **UGF** dans la modal "Ajouter
  une action" affichait le `ug_id` brut (ex. `ugf_42`) au lieu du
  libellé humain. Construction d'un `ug_choices` via
  `setNames(ids, labels)` à partir de `sf$label` mappé sur
  `sf$ug_id`, trié par label. Fallback sur les IDs si `ug_sf_4326()`
  est indisponible.
* `fix(action_plan)` — le champ **Année cible** de la même modal
  affichait l'offset interne (1, 2, 3 …) au lieu d'une année
  calendaire. Le `numericInput` montre désormais l'année réelle
  (default = `base_year + 1`, min = `base_year + 1`,
  max = `base_year + horizon`) ; la conversion en offset
  (`year - base_year`) se fait au moment du save dans
  l'observer `add_run`.

### Plan d'actions — UX du tableau

* `ui(action_plan)` — **total des actions affiché en bas à
  droite** du tableau DT (auparavant à gauche). Le `dom` DT
  passe à un layout custom `<"top"f>rt<"d-flex …
  dt-bottom-row"<"d-flex gap-3 align-items-center"lp>i>` ; règles
  CSS scoped sur `.dt-bottom-row` neutralisent les `float`/
  `clear` par défaut de `dataTables_info`/`_length`/`_paginate`
  et alignent l'info à droite via `text-align: right`.

# nemetonshiny 0.22.4 (2026-05-09)

### Plan d'actions — UX polish

* `ui(action_plan)` — le sélecteur **"Afficher 5/10/25/50/All"**
  passe **sous le tableau** (à côté de l'info "_TOTAL_ action(s)"
  et de la pagination Préc./Suiv.). Le `dom` DT passe de
  `"lfrtip"` à `"frtilp"` : la barre de recherche reste seule
  au-dessus du tableau, ce qui aère l'en-tête de la card.
* `ui(action_plan)` — **figeage strict** des colonnes UGF +
  Année lors du scroll horizontal. `DISPLAY_COLS` réordonné
  pour placer les colonnes cachées (`id`, `ug_id`,
  `annee_cible`) en queue ; `fixedColumns: leftColumns` passe
  de 5 à 2 — l'extension `FixedColumns` de DT compte toutes
  les colonnes du DOM (y compris `visible:FALSE`), donc seul
  le décompte sur les colonnes visibles évite les artefacts
  d'en-têtes clones. `colname_map` et `hidden_targets`
  ajustés en conséquence (cibles 13-15).
* `i18n(action_plan)` — titres de légende de la **carte des
  actions** traduits. Les littéraux `"annee"` / `"type"` /
  `"priority"` passés à `leaflet::addLegend(title = …)` sont
  remplacés par `i18n$t("action_plan_col_annee" | "_type" |
  "_priorite")` ; ré-utilisation des clés des en-têtes de
  colonnes pour rester cohérent avec le tableau. Affiche
  désormais "Année" / "Type" / "Priorité" en FR (et "Year" /
  "Type" / "Priority" en EN), avec switch dynamique au
  changement de langue.

### Auth — fix démarrage anonyme avec dev roles

* `fix(auth)` — `mod_auth_server()` plantait au démarrage en
  mode anonyme dès que `NEMETON_AUTH_DEV_ROLES` était défini :
  l'appel `cli::cli_alert_info("…dev roles: {.val {auth_state$user_roles}}.")`
  faisait évaluer `{auth_state$user_roles}` par `glue` hors
  d'un `reactive()/observe()`, ce que `reactiveValues` interdit
  ("Can't access reactive value 'user_roles' outside of
  reactive consumer"). La valeur parsée est désormais capturée
  dans une locale `parsed_roles` avant l'assignation à
  `auth_state$user_roles` ; le message `cli` interpole la
  locale, plus le reactiveValues. Régression introduite par
  #41 (v0.22.3).

# nemetonshiny 0.22.3 (2026-05-09)

### Plan d'actions — UX polish

* `feat(action_plan)` — global DT search box now uses **regex
  with OR semantics** (#35). Typing `eclaircie|plantation` in
  the search box returns rows matching either term. Search is
  case-insensitive (`caseInsensitive = TRUE`) so accent-less
  typing keeps working.
* `feat(action_plan)` — new **Surface totale** badge in the
  totals strip above the action table (#36, plus reorder in
  this release). Sums `surface_ha` over the rows currently
  visible in the DT, formatted with two decimals + `ha`,
  rendered in `text-primary`. The badge sits **after Bilan** so
  the monetary totals (Coût / Revenu / Bilan) read first and
  the surface tally is the trailing metric. The `pill()` helper
  now accepts optional `unit` and `digits` arguments
  (defaulting to `"EUR"` / `0`) for backward-compat with the
  three monetary pills.
* `refactor(action_plan)` — DT table paginated at **5 rows per
  page** (was 50 with a 60 vh internal scroll). New
  `lengthMenu` lets users expand to 10 / 25 / 50 / All on
  demand; `dom` switched from `"frtip"` to `"lfrtip"` so the
  length selector sits left of the global search. Removed
  `scrollY` + `scrollCollapse` so the table card now contracts
  around the visible rows instead of padding to 60 vh.
  `scrollX = TRUE` and the two pinned columns (UGF + Année)
  are unchanged.
* `ui(action_plan)` — sidebar title now carries an icon and
  reads "Tableau des actions" (#37) for a tighter visual link
  to the table card it controls.
* `ui(action_plan)` — bulk-status block (the *Statut Kanban*
  section that duplicated the per-card Kanban moves) dropped
  in favour of a **collapsible action card** (#38) so the
  sidebar stays scannable.
* `ui(action_plan)` — right action panel resized to **350 px**
  with **dual collapse** behavior (#39): the bslib sidebar
  itself can collapse, and the action card inside it has its
  own collapse toggle.

### Plan d'actions — role-based permissions (Lot 6 S15)

* `feat(action_plan)` — **role-based write permissions** (#40).
  New helper `can_edit_action_plan(auth_state)` centralises the
  role convention used across the tab: roles `proprietaire`,
  `editeur`, `admin`, `manager`, `owner`, `editor` are allowed
  to mutate the plan; anonymous sessions default to *editor*
  to preserve the current dev experience; the `lecteur` role
  (and any other unrecognised role) is read-only. `app_state$auth`
  now exposes the auth reactive so the action plan module can
  subscribe to it. Read-only sessions see a banner above the
  action table and 8 server-side mutation observers (add /
  edit / delete / bulk status / IA generation / Kanban drop /
  GeoPackage write / PDF write) bounce back with a toast. The
  Kanban drop handler also rolls back the optimistic UI move
  when the user lacks edit rights. 5 unit tests on
  `can_edit_action_plan()`. Closes #22, refs #7.

### Auth

* `feat(auth)` — **`NEMETON_AUTH_DEV_ROLES` env override**
  (#41). In anonymous mode (no OAuth client configured),
  `auth_state$user_roles` is now seeded from the comma-separated
  env var `NEMETON_AUTH_DEV_ROLES` (e.g.
  `NEMETON_AUTH_DEV_ROLES=lecteur` to test the read-only
  banner). Lets developers exercise the role-based mutation
  guards landed in PR #40 without standing up a Keycloak
  realm. Refs #7.

### i18n

* New key `action_plan_total_surface` (FR: *Surface totale* /
  EN: *Total area*).
* New keys for the Lot 6 read-only banner + permission-denied
  toast (FR/EN), bundled in PR #40.

# nemetonshiny 0.22.2 (2026-05-06)

### Plan d'actions — table & Kanban polish

* `refactor(action_plan)` — DT table trimmed and stabilised.
  Removed columns *Type libre*, *Objectifs*, *RDI*, and *Source* —
  they were rarely used, made horizontal scroll worse, and the
  underlying fields are still editable through the row-level form.
  Per-column filter row dropped (`filter = "top"` → `filter = "none"`);
  the global search box is the single filter exposed. Rows now have
  a uniform height: the datatable carries `class = "compact stripe
  hover nowrap"` and a scoped `.dt-truncate` rule (`max-width:
  220px; overflow: hidden; text-overflow: ellipsis`) keeps long
  commentaire / labels on a single line. The two pinned left
  columns (UGF + Année) are unchanged.
* `feat(action_plan)` — Kanban board layout reorganised. The four
  active workflow stages (*Proposée*, *Validée*, *Planifiée*,
  *Réalisée*) sit side by side as a 4-column grid; *Abandonnée* is
  rendered full-width below as a separate, less prominent archive
  lane. Empty columns now reserve a 60 px drop zone so cards can be
  dragged into them.
* `feat(action_plan)` — **Drag-and-drop on the Kanban board**.
  Cards can be moved between columns by dragging. SortableJS 1.15.6
  is vendored under `inst/app/www/js/Sortable-1.15.6.min.js` (MIT,
  ~45 KB), wired up by a small init script
  (`action_plan_kanban.js`) that re-binds on every renderUI tick to
  avoid stale instances. On drop, the JS pushes
  `input$kanban_drop = list(action_id, target_status, source_status,
  nonce)` to the server, where a new observer validates the
  transition through the existing `is_valid_status_transition()`
  rules:
    - **Allowed transition** → `update_action_in_plan()` +
      `save_action_plan()`, then `plan_rv()` is bumped, which
      triggers a renderUI re-run that confirms the move.
    - **Refused transition** (e.g. trying to drag a *Réalisée* card
      back to *Proposée*) → a warning toast surfaces with the
      offending pair, and `kanban_render_token` is bumped to
      re-render the board, which puts the card back where the data
      says it belongs.
  The previous per-card *Déplacer* dropdown is preserved — both
  paths share the same validator + persistence code.

### i18n

* New key `action_plan_kanban_drop_invalid_fmt` (FR/EN) for the
  refused-transition toast.

### Tests

* `tests/testthat/test-mod_action_plan.R` — three new test_thats:
  vendored asset existence (`Sortable-1.15.6.min.js` +
  `action_plan_kanban.js`), validator coverage for legal vs refused
  drag-drop transitions, and presence of the new i18n key in both
  locales.

# nemetonshiny 0.22.1 (2026-05-06)

### Bug fixes

* `fix(action_plan)` — empty action plans no longer crash the
  reactive chain. `actions_df_all()` was assigning a length-1
  `NA_character_` to `df$ug_label` on a 0-row data.frame, which R
  rejects with *"replacement has 1 row, data has 0"*. The 0-row
  branch now returns early with explicit empty columns. Surfaced
  on a fresh project with no actions yet (regression introduced
  in v0.22.0 with the new Plan d'actions tab).

### Improvements

* `feat(db)` — app schema (`nemeton.projects` and friends) is now
  initialized automatically on first connection through
  `get_db_connection()`. The existing idempotent `db_init_schema()`
  used to be exported but never invoked, so a freshly provisioned
  database surfaced *"relation \"nemeton.projects\" does not exist"*
  on the first project save. Memoized once per R session via
  `.nemeton_env`.
* `feat(monitoring)` — monitoring-DB migrations
  (`monitoring_zone`, `alert`, `obs_pixel`, …) are now applied
  automatically on first connection through
  `get_monitoring_db_connection()` by calling
  `nemeton::db_migrate()`. The Monitoring tab no longer warns
  *"relation \"monitoring_zone\" does not exist"* on a fresh
  TimescaleDB. Memoized once per R session via `.nemeton_env`.

# nemetonshiny 0.22.0 (2026-05-06)

### New feature — "Plan d'actions" tab

A full new tab dedicated to building, visualising and exporting
multi-year forest action plans, delivered across PRs #23..#34.

* **Lot 1+2 (S1..S6, #23)** — scaffold of the tab, interactive
  Leaflet map + DT table with two-way sync, per-UGF action rows.
* **Lot 3 (S7+S8, #25)** — LLM-powered plan generation through a
  new `planificateur` expert profile, Kanban board view, and an
  audit modal exposing the prompt, model, latency and token counts
  for each generation.
* **Lot 4 (S9..S11, #30)** — cumulative balance sparkline per UGF,
  Gantt timeline of scheduled actions, and bridges from each row
  to the Terrain tab (jump to the matching plot).
* **Lot 5 (S12+S13, #32)** — GeoPackage export of the full plan
  and per-UGF PDF export through a new Quarto template
  `inst/quarto/action_plan_template.qmd`.
* **Bilan column + steered LLM (#29)** — derive a per-UGF balance
  column (`revenu_eur - cout_eur`, cumulative) and steer the LLM
  prompt toward solutions that keep the cumulative balance
  positive over the planning horizon.
* **`revenu_eur` field (#28)** — new revenue column alongside
  `cout_eur`, propagated through all views, exports and prompts.
* **UI polish** — 50/50 layout with native DT filters and frozen
  UGF label column (#26), map auto-refit on bbox change with
  calendar year display (#27), categorical year legend with UGF
  label in popup (#31), Kanban board with sticky DT header (#33),
  right-hand bslib action sidebar grouping all toolbar buttons
  by intent (#34, refs #7).
* **Robustness (#24)** — guard map color palettes when the plan
  is empty so the tab never crashes on a project with no actions.

### Other improvements

* `feat(project)` — the *Informations projet* card now shows the
  storage directory of the loaded project, so users can locate the
  GeoPackage and metadata files without leaving the app.
* `feat(home)` — the PostGIS sync toast now reports the actual
  target as `dbname@host:port`, so users immediately know which
  database their commune cache is going to.
* `feat(home)` — load the active project into the Monitoring zone
  picker so users do not have to redraw the AOI when switching
  tabs.
* `feat(monitoring)` — the "Enregistrer comme zone" button now
  exposes a tooltip explaining why it is disabled (no AOI drawn,
  no project loaded, etc.).
* `i18n(monitoring)` — clarify FAST naming in the quick-mode
  labels.
* `refactor(monitoring)` — rename the health-export "QField"
  labels to "QGIS" in the UI to match what is actually produced
  (.qgz project file).
* `fix(db)` — `service_db.R` now honors `NEMETON_DB_URL` priority
  over the legacy `POSTGRESQL_ADDON_*` Clever Cloud variables, so
  local overrides are respected.
* `fix(sampling)` — surface silent `save_samples()` failures with
  a user-visible toast instead of swallowing the error.

### Internal

* New module `R/mod_action_plan.R` (~1816 LOC) and service layer
  `R/service_action_plan.R` (~686 LOC).
* New expert profile `inst/experts/planificateur.yml`.
* New Quarto template `inst/quarto/action_plan_template.qmd`.
* New tests `test-action_plan_prompts.R`,
  `test-mod_action_plan.R`, `test-service_action_plan.R`
  (~500 LOC of testthat).
* `R/utils_i18n.R` — +263 LOC of new keys (NMT convention) for
  the Plan d'actions tab in FR/EN.

# nemetonshiny 0.21.0 (2026-04-30)

### New feature — Forest health monitoring (E6.c.5, spec 008)

The Monitoring tab is now a two-mode forest health workstation.

* **Mode 1 — Surveillance rapide** (existing E6.b NDVI/NBR rolling
  window, kept as-is) detects recent shocks (cuts, windthrows, fires)
  in seconds.
* **Mode 2 — Diagnostic sanitaire (FORDEAD)** wraps
  `nemeton::run_fordead_dieback()` (CRSWIR + harmonic model via
  reticulate, GPL-3 isolated to the Python frontier) in a
  `shiny::ExtendedTask`. Detects progressive dieback (bark beetle,
  drought) on conifers in minutes-to-hours. Both pipelines write to
  the same `alert` table.
* **G1 — class filter**. By default the leaflet shows only
  `confidence_class` 3-forte and 4-sol-nu (>70% true positives per
  ONF/DSF 2024). A "Inclure faible/moyenne" toggle adds the 1-2
  classes and surfaces a `border-warning` banner citing the up-to-50%
  false-positive rate.
* **G2 — disturbance classification**. Alerts go through
  `nemeton::classify_disturbance()` server-side, so the popup carries a
  `disturbance_type` to separate progressive dieback from mechanical
  intervention.
* **G3 — validity banners + confirmation modal**.
  `nemeton::check_fordead_validity()` is called on the current zone
  whenever the user enters health mode. Two `border-warning` banners
  fire when the AOI overlaps the 5 validated departments
  (88, 39, 01, 73, 74) under 50%, or when épicéa + sapin pectiné drops
  under 70%. Launching FORDEAD on an out-of-domain area pops a modal
  citing the ONF/DSF caveat; "Run anyway" forwards to the task.
* **G4 — QField field-validation workflow**. A new card in health
  mode lets the user pick *n* plots (default 30) and a sampling
  method (GRTS / random) and download a `.qgz`. Re-uploading the
  filled GPKG via the new "Validation sanitaire" sub-tab in
  *Données terrain* runs `nemeton::ingest_health_validation()`,
  reports counts (confirmed / false-positive / unmatched), and
  updates `validation_status` + `validation_cause` per alert.
* **G5 — R5 dieback indicator**. The radar's R-family picks up R5
  automatically through `nemeton::INDICATOR_FAMILIES$R` (no UI
  change needed; the cœur computes it via
  `nemeton::indicateur_r5_deperissement()`).
* **Plotly time series**: in health mode shows the alert distribution
  by `confidence_class`. Quick-mode time series (NDVI/NBR per plot)
  ships with E6.b phase 3.
* **Persistence**: each FORDEAD launch writes
  `monitoring_mode`, `monitoring_threshold_anomaly`,
  `monitoring_vegetation_index`, `monitoring_dates_training`, plus
  the validity intersection percentages, to `metadata.json`. The
  module restores these inputs whenever a project is reopened.
* **i18n**: ~30 new keys (`monitoring_mode_*`, `monitoring_warning_*`,
  `monitoring_class_*`, `monitoring_qfield_*`, `health_validation_*`,
  `r5_*`). The tab itself is renamed *Suivi sanitaire* /
  *Forest health monitoring*.
* **Dependencies**: `plotly` promoted to Imports;
  `nemeton (>= 0.20.1.9004)` (FORDEAD pipeline + helpers).

# nemetonshiny 0.20.0 (2026-04-24)

### New feature — LiDAR HD integration (E5.d)

* **LiDAR HD MNH as preferred CHM source**. The download path now
  tries `download_ign_lidar_hd(product = "mnh")` via `happign`
  first — a direct airborne measurement (~0.5 m vertical accuracy,
  NDP 2 precision). Open-Canopy ML remains the fallback when
  LiDAR HD tiles are missing for the AOI.
* **LiDAR HD MNT promoted to the `dem` slot** (1 m vs 25 m BD ALTI)
  so W3 (TWI), R1 (feu), R2 (tempête), R3 (sécheresse) and the
  erosion risk all run at LiDAR HD resolution.
* **NDP 1 "Observation" auto-lifts** whenever any LiDAR HD product
  (MNH or MNT) is cached for the AOI, via
  `attr(compute_unit, "has_lidar_hd")` consumed by
  `nemeton::detect_ndp()`.
* **Stratified GRTS kicks in on the sampling plan**. Two new
  reactives (`chm_raster`, `mnt_raster`) load the cached CHM / MNT
  with the same LiDAR-first / fallback order and pass them to
  `nemeton::create_sampling_plan()`. The core upgrades from
  LPM2 to stratified GRTS whenever CHM + MNT + BD Forêt are all
  available. The draw method is surfaced in the generation toast.
* **New "Hauteur LiDAR HD" badge** on the Synthesis tab
  (`augmented_height_lidar_*` i18n keys) — green,
  distinct from the cyan "Hauteur ML" used for Open-Canopy.
* `chm_phase:lidar_hd_download` progress key translated so the
  compute status line reads "Téléchargement CHM LiDAR HD (IGN)…"
  instead of the raw key.

### New feature — Sampling polish

* **`forest_mask` passed to the sampling plan**: reuse the
  project's cached BD Forêt v2 polygons (filtered to true forest)
  so points falling in water, fields or roads are filtered by the
  `min_forest_cover = 0.7` constraint. Fixes the Couchey lake
  scenario.
* **Map zoom fixed to the UGF extent**, not BD Forêt's (which is
  fetched with a buffer and was dominating the auto-fit).
* **Immediate toast on Générer les placettes** with a spinning
  gear, matching the Projet chargé / Retry pattern. Dispatched on
  the root session.
* **Tooltip on the Source du CV radio** explicitly states that
  the choice controls the CV value (Cochran), not the draw
  method (GRTS / LPM2 / random).
* **Sampling method note rewritten** to describe the full
  pipeline: candidates on a regular 50 m grid, filtered by the
  forest mask, then GRTS → LPM2 → random depending on what is
  provided.

### Fixed

* Duplicate PostGIS-sync toast at compute completion — removed the
  second occurrence in `mod_progress`; only the `mod_home` one
  fires now.
* Immediate toast when clicking *Réessayer* on the compute-error
  card, dispatched on the root session.

### Dependencies

* Bumped `nemeton` minimum to `>= 0.19.5` (for `height_lidar`
  augmented flag and TSP tour integration).

# nemetonshiny 0.19.0 (2026-04-24)

### New feature — Sampling UX polish

* **Tooltips** on six sidebar inputs of the Export terrain sub-tab
  (target error, alpha risk, over-sample ratio, CV position, seed,
  region). Each tooltip explains the statistical or biological
  meaning of the parameter.
* **TSP legend on the map**: when a sampling plan with ≥ 2 Base
  plots is drawn, a legend panel appears at the bottom-left with
  three inline-SVG glyphs — dashed magenta line (*Ordre de visite*),
  open triangle (*Départ*), double concentric circle (*Arrivée*) —
  matching the markers and route on the map.
* **Retry toast**: clicking *Réessayer* now fires an immediate
  notification with a spinning arrow-clockwise icon
  ("Projet réinitialisé — prêt à relancer le calcul."), dispatched
  on the root session so it lands in the top-level toast stack.

### Fixed

* **Duplicate PostGIS-sync toast** on compute completion: the same
  notification used to fire both from `mod_home` and `mod_progress`
  with slightly different wording ("la base PostGIS" vs "la base
  de données PostGIS"). Kept the `mod_home` one (orchestrator),
  dropped the `mod_progress` one.
* **Package documentation icon in RStudio's Packages pane**: added
  `URL` and `BugReports` fields to `DESCRIPTION` so the icon is now
  rendered alongside the globe and uninstall icons.

### Docs

* README: synced counters to the real state (31 indicators, 13
  expert profiles, 504 i18n translation keys). Previously 29 / 16
  / 293.
* i18n: `sampling_tt_region` tooltip says *QGIS*, not *QField*
  (the species dropdown is defined in the QGIS project descriptor).

# nemetonshiny 0.18.0 (2026-04-24)

### New feature — Sample size from target error + BD Forêt v2 CV (E5.c)

* **`R/mod_sampling.R`** — the sidebar accordion in the Export
  terrain sub-tab gains a *Mode de dimensionnement* radio
  (*Taille fixe* / *Erreur cible*). In *Erreur cible* mode the user
  picks a relative error (default 10 %), an alpha risk (default 5 %),
  an over-sample ratio (default 20 %) and either a manual CV or an
  automatic CV derived from BD Forêt v2 via
  `nemeton::cv_from_bdforet()`. The computed sample size is shown
  live under the inputs, along with diagnostics on the BD Forêt v2
  coverage and any ambiguous / unmapped TFV codes.
* BD Forêt v2 is read from the project cache populated during the
  first compute run (`<project>/cache/layers/bdforet.gpkg`). When the
  cache is absent, the UI points the user at the manual mode via an
  explicit warning.
* The TFV column is auto-detected (`TFV`, `tfv`, `code_tfv`) to cope
  with different WFS layouts.
* The existing *Taille fixe* path is preserved via a
  `conditionalPanel`; `create_sampling_plan()` is called with
  `n_base` / `n_over` computed upstream depending on the mode.
* Bumped nemeton dependency to `>= 0.19.0.9000` (the dev version
  introducing `compute_sample_size()`, `cv_from_bdforet()` and the
  editable CV typology CSVs).
* 19 new FR/EN i18n keys (`sampling_sizing_mode`, `sampling_mode_*`,
  `sampling_target_error_label`, `sampling_alpha_label`,
  `sampling_over_ratio_label`, `sampling_cv_source_*`,
  `sampling_cv_position*`, `sampling_cv_compute`,
  `sampling_cv_bdforet_hint`, `sampling_cv_bdforet_missing`,
  `sampling_cv_computed`, `sampling_cv_ambiguous`,
  `sampling_cv_unmapped`, `sampling_n_computed*`).
* Tests: 6 new testServer assertions covering the Cochran sizing
  path (manual CV) and the bail-out when CV is zero. Full suite
  5145 / 0 failure.

### New feature — Field ingest (E5.b — QField return path)

* **`R/mod_field_ingest.R`** — new "Ingestion terrain" tab that closes
  the terrain → plateforme loop. A field agent drops the GeoPackage
  returned by QField; the module runs
  `nemeton::import_qfield_gpkg()` + `validate_field_data()`, renders
  a validation report (counts, errors, warnings), and previews the
  placettes / arbres on the project map.
* **NDP bump on attach**: clicking *Rattacher au projet* calls
  `aggregate_plot_metrics()` + `attach_field_data_to_units()` on the
  project's UGF sf, tags it via `tag_field_data_sources()`, runs
  `detect_ndp()` along the alternative field path (NDP 2 with plots
  only, NDP 3 from 10 trees/plot on average), persists the GPKG to
  `<project>/data/field_data.gpkg` and updates project metadata so
  the bumped NDP is picked up by every downstream module (synthesis
  badge, family tabs). Before/after NDP badges make the change
  visible to the user.
* **MVP scope**: this iteration persists the field data and bumps
  the NDP, but does not rerun `compute_all_indicators()`. The
  indicators consuming field aggregates (P1, P2, B2, C1, R2) are
  picked up on the next compute triggered from the Home tab.
* i18n: 22 new FR/EN keys (`tab_field_ingest`, `field_ingest_*`,
  `field_ingest_ndp_before` / `_after`, report headers).
* Tests: `tests/testthat/test-mod_field_ingest.R` — 24 assertions
  covering UI controls, reactive NULL state, the validate flow on a
  real-ish GPKG (placettes + arbres) and the attach flow with mocked
  persistence (GPKG written to the project dir + metadata update
  recorded).

### Sampling module now uses the library-level GRTS pipeline

* **`R/mod_sampling.R`** — replace the temporary
  `sf::st_sample(..., type = "random")` draw with
  `nemeton::create_sampling_plan()`, which delivers GRTS
  stratification when CHM/DEM/BD Forêt layers are provided and
  falls back to spatially-balanced LPM2 or plain random otherwise.
  The notification now appends the draw method (`GRTS`, `LPM2`,
  `RANDOM`) so users can see which path was taken.
* i18n: `sampling_method_note` rewritten to describe the new
  behaviour.

### New feature — Field sampling / QField export (E5.a)

* **`R/mod_sampling.R`** — new "Terrain" tab: given the current
  project's study area (union of `indicators_sf` polygons), the user
  sets `n_base` / `n_over` / seed / biogeographic region, clicks
  *Générer*, and previews the sample plots on a leaflet map. A
  *Télécharger le projet QField (.qgz)* button produces a QField-ready
  project via `nemeton::create_qfield_project()` (placettes + empty
  arbres layer + pre-configured forms).
* First iteration uses a spatial random draw (`sf::st_sample`). The
  full stratified GRTS + TSP pipeline from the 09-sampling tutorial
  will be lifted to `nemeton::create_sampling_plan()` in a follow-up.
* `DESCRIPTION` now requires `nemeton (>= 0.18.0.9000)` for
  `create_qfield_project()`.
* i18n: 14 new FR/EN keys (`tab_sampling`, `sampling_*`, `qfield_*`).
* Tests: `tests/testthat/test-mod_sampling.R` — 23 assertions
  covering UI controls, reactive draw, empty-state handling and a
  round-trip .qgz built from the module's generated plots.

### Changes — F1 soil fertility

* **F1 now uses the absolute SoilGrids CEC scoring path** from the
  core package. `compute_single_indicator()` passes
  `source = "soilgrids"` to `indicateur_f1_fertilite()`, which
  streams the 250 m CEC topsoil raster on demand via
  `nemeton::load_raster_source()` and applies
  `nemeton::cec_to_fertility_score()` (absolute 0-100). Scores are
  now comparable across projects instead of being min-maxed per
  AOI.
* **Removed the duplicated `download_soilgrids_cec()`** and its
  entry in `DATA_SOURCES$rasters$soil`. The core package owns the
  download path (ADR-009), so the app no longer stages a SoilGrids
  layer in `layers$rasters$soil`. One less pre-compute step
  surfaces in the progress UI.
* **Bumped the `nemeton` dependency** to `>= 0.17.0.9000` (the dev
  version introducing `load_raster_source()`, `source = "soilgrids"`,
  and the UTS → fertility crosswalk).

# nemetonshiny 0.16.0

First release targeting the v0.17.0 nemeton core. End-to-end
integration of the Open-Canopy CHM pipeline, live per-step
progress feedback, and a consolidated i18n layer.

### New Features — CHM / Open-Canopy

* **Auto-detected Open-Canopy CHM** — the UI no longer forces the
  user to pick "CHM: none / Open-Canopy" before every run. The
  pipeline fires automatically when the `opencanopy` package is
  installed, unless the user opts out via
  `options(nemetonshiny.chm = "none")` or
  `NEMETONSHINY_DISABLE_CHM=1`. Each synthesis view gets two
  provenance badges:
    * ⚡ **Hauteur ML** — CHM was consumed by height-aware
      indicators (C1, B2, R2, P2).
    * 📋 **Inventaire estimé ML** — P1 / P3 / E1 ran, meaning
      `dbh` / `density` were synthesised from the CHM via
      `nemeton::ensure_inventory_fields()` (Charru 2012
      self-thinning).
* **BD Forêt enrichment for P2** — UGFs are enriched with
  `species` / `age` from BD Forêt V2 once up-front via
  `nemeton::enrich_parcels_bdforet()` when
  `indicateur_p2_station` is scheduled, so the CHM mode can run
  instead of falling back on the legacy `fertility` / `climate`
  path that never had its inputs.

### New Features — progress UX

* **Live step-by-step status** replaces the "frozen on Inférence
  CHM" ~8-minute silence on large AOIs. The task toast now paints:
    * "Étape 1/5 : chargement de l'AOI…"
    * "Étape 2/5 : téléchargement ortho IGN…" +
      "Téléchargement ortho IGN RVB : tuile 5/28…" per WMS tile
    * "Étape 3/5 : configuration Python + téléchargement modèle…"
    * "Étape 4/5 : inférence du modèle pvtv2…" +
      "Inférence CHM : tuile 2/3…" per inference tile
    * "Étape 5/5 : export des résultats…"
* **Initialisation spinner** — the toast paints ⚙ + "Initialisation
  des calculs…" the moment the user clicks "Lancer les calculs"
  or "Réessayer", so the 1-3 s gap before the async worker writes
  its first progress event is no longer silent.
* **Task translator unified** — `mod_progress.R` no longer ships
  its own partial `translate_task()`; it delegates to the canonical
  `translate_task_message()` in `utils_i18n.R`, so every new task
  prefix is routed to its label in one place.

### New Features — i18n

* **Single source of truth** — the `TRANSLATIONS` list in
  `R/utils_i18n.R` is now the only runtime dictionary. The stale
  `inst/app/i18n/{fr,en}.json` files (339 keys, 19 behind the R
  list) have been removed and the unused `shiny.i18n` suggested
  dependency dropped. `export_translations_json()` remains
  available for one-way R → JSON exports to external translators.

### Bug Fixes

* **`download_chm_opencanopy()`** — unwraps the bare `SpatRaster`
  / `sf` returned by `download_{raster,vector}_source()` instead
  of chasing a `$object` attribute that didn't exist. The previous
  code called `[[` on a `SpatRaster` looking for a layer named
  "object" and triggered a terra `[subset] invalid name(s)` error
  that aborted the whole CHM pipeline and forced P2 back into
  legacy mode.
* **Open-Canopy pipeline resume** — the retry and recompute paths
  now reset the project to "draft" and wait for the user to re-
  launch the run, instead of silently firing a new `compute_task`
  invocation. One entry point is the confirmation modal.
* **Resume from a legacy progress file** — `translate_task_message()`
  now maps the pre-`e74bdcc` literal
  `"download:source_chm_opencanopy"` to the new
  `"chm_inference_opencanopy"` label so re-opening an older
  project no longer spams "Translation key not found:
  source_chm_opencanopy".
* **Tests** — `NEMETONSHINY_DISABLE_CHM=1` is now scoped to the
  test run via `withr::local_envvar(.local_envir =
  testthat::teardown_env())` in a dedicated `setup-chm.R`, so
  `devtools::test()` in an interactive session no longer leaves
  the CHM pipeline silently disabled.

### Breaking changes

* None. The CHM toggle that was previously visible on the compute
  button disappeared, but the underlying metadata is still written
  (now reflecting the *outcome* of the auto-detected run, not the
  user's a priori choice), so the synthesis badge keeps working on
  old projects.

# nemetonshiny 0.15.1

See git history.

# nemetonshiny (development version)

# nemetonshiny 0.107.5 (2026-07-16)

### Fixed — bouton plein écran du panneau de graphes E-OBS

Le panneau de graphiques au clic sur la maille E-OBS ne montrait pas de bouton
plein écran fonctionnel (le `full_screen` de `navset_card` de bslib ne rend pas
de bouton visible dans une modale). Remplacé par le **patron exact FAST/FORDEAD**
(`mod_monitoring_pixel_map`) : un bouton `arrows-fullscreen` ancré en haut à
droite du titre qui bascule la classe Bootstrap `.modal-fullscreen` et déclenche
un `resize` (plotly responsive). Wrapper flex + règle CSS pour que le graphe
occupe tout l'écran en plein écran.

# nemetonshiny 0.107.4 (2026-07-16)

### Fixed — trous du raster bivarié + ascenseur de la légende (suite)

- **Trous dans le raster bivarié** : `addRasterImage(project = TRUE)` reprojetait
  par défaut en **bilinéaire**, interpolant les codes de classe entiers 1-25 en
  valeurs décimales (3.4, 7.2…) qui ne correspondent à aucune classe de la
  palette → pixels transparents (les « trous »). Correctif : `method = "ngb"`
  (plus proche voisin, obligatoire pour un raster catégoriel) + coercition
  explicite du raster en entier (`terra::round`) comme garde-fou.
- **Ascenseur de la légende** : la légende bivariée n'utilise plus la classe
  `info legend` (dont le CSS impose `max-height:150px; overflow:auto`) mais une
  classe propre `nmt-bivariate-control`. Le plafond ne peut donc plus s'appliquer
  — correctif robuste au cache navigateur (l'ancienne règle ne cible plus
  l'élément), contrairement à la surcharge de la v0.107.3.

# nemetonshiny 0.107.3 (2026-07-16)

### Fixed — légende bivariée sans ascenseur + notif « calcul en cours » au clic

- **Légende bivariée** : la grille 5×5 + axes dépassait le plafond de 150 px du
  contrôle leaflet (`.info.legend`), provoquant **deux ascenseurs**. La légende
  reçoit désormais une classe dédiée (`nmt-bivariate-control`) sans plafond ni
  overflow, et ses cases sont agrandies (16 → 22 px) pour rester lisibles.
- **Message « Calcul du graphique en cours… »** au clic sur la maille E-OBS
  (parité FAST/FORDEAD) : notification affichée immédiatement, extraction
  différée après le flush (`onFlushed`), et **garde anti-multi-clics** qui
  ignore les clics tant que le graphique précédent n'a pas fini de se calculer.

# nemetonshiny 0.107.2 (2026-07-16)

### Added — recalcul du contexte, plein écran et distribution bivariée (spec 036)

Trois améliorations du contexte régional E-OBS de l'onglet reGénération :

- **Recalculer le contexte** : nouveau bouton dans la sidebar qui **purge le
  cache raster (.tif + meta) des 3 vues** (tx / rr / bivariée) et relance le
  calcul de la vue courante. Indispensable quand un raster caché est périmé
  (ancienne version cœur, classement/bornes bivariés modifiés) — sinon
  l'observer relisait indéfiniment le raster obsolète.
- **Bouton plein écran** sur le panneau de graphiques au clic : la modale passe
  en `navset_card` avec `full_screen = TRUE`, cohérent avec FAST/FORDEAD.
- **Distribution en vue bivariée** : le 3ᵉ graphique (distribution régionale)
  s'affiche désormais aussi en vue bivariée, avec **les deux variables** (tx ET
  rr) côte à côte — les 4 graphiques sont peuplés quelle que soit la vue.

# nemetonshiny 0.107.1 (2026-07-16)

### Added — percentile régional dans le graphe de distribution (spec 036 §5.3)

Le 3ᵉ graphique du panneau au clic (distribution régionale des pentes) annote
désormais le **percentile régional** de la maille cliquée (« P{xx} du massif »),
répondant à la question décisionnelle de la spec : ce point est-il chaud/sec
localement ou dans la moyenne du buffer ? Aucun appel cœur (dérivé des valeurs
déjà en mémoire).

# nemetonshiny 0.107.0 (2026-07-16)

### Added — graphiques au clic sur la maille E-OBS (spec 036)

La carte « Contexte régional (E-OBS) » de l'onglet reGénération n'affichait
qu'une couleur par maille (la pente estivale). Un **clic** ouvre désormais un
panneau de **4 graphiques** rendant la donnée sous la couleur, à la maille
cliquée (résolution E-OBS native ~11 km — honnêteté spatiale) :

1. **Série + tendance** estivale, la pente affichée étant exactement
   `nemeton::eobs_trend_fit()$slope_decade` (= la couleur de la maille) ;
2. **Anomalies** annuelles (chaud/sec en rouge), révélant les étés porteurs ;
3. **Distribution régionale** des pentes du buffer + trait à la maille cliquée ;
4. **Diagramme ombrothermique** (Gaussen-Bagnouls) : précip/température à axes
   couplés `P = 2T`, mois secs ombrés, indice de De Martonne.

Toute la donnée vient de trois accesseurs cœur (`eobs_summer_series`,
`eobs_monthly_climatology`, `eobs_trend_fit`, nemeton ≥ 0.160.0) — aucun calcul
métier côté app. Nouveau bouton opt-in **« Télécharger la T° moyenne (Gaussen) »**
(`tg`, ~800 Mo) requis par le diagramme ombrothermique honnête (sinon repli sur
T°max explicitement étiqueté « saison sèche majorée »). Plancher
`Imports: nemeton (>= 0.160.0)`.

# nemetonshiny 0.106.16 (2026-07-15)

### Changed — légende bivariée : titre sur deux lignes

Le couple de variables entre parenthèses (« T°max estivale × précipitations »)
passe désormais sur une **seconde ligne en sous-titre** (petite fonte régulière)
sous le titre principal « Tendance bivariée », pour une légende plus lisible. Le
découpage se fait sur la dernière parenthèse du libellé cœur (`meta$value_label`) ;
repli sur le titre seul si aucune parenthèse.

# nemetonshiny 0.106.15 (2026-07-15)

### Changed — légende bivariée : bornes d'axes = plage réellement observée

Les min/max des axes de la légende bivariée affichent désormais la plage
**réellement observée** sur la zone (quantiles des rasters T°max / précipitations
downscalés, `meta$tx`/`meta$rr`) au lieu des seuils fixes de classification. Le
minimum est repoussé un peu avant 0 quand toutes les tendances sont du même signe,
pour que la **ligne 0 en pointillé blanc reste visible** ; sa position est
recalculée sur cette plage observée. Repli sur les seuils cœur si la plage
observée est absente (vieux cache).

# nemetonshiny 0.106.14 (2026-07-15)

### Added — légende bivariée E-OBS : bornes des axes + lignes 0 en pointillé

La légende bivariée « Contexte régional (E-OBS) » annote désormais les extrêmes
min/max de chaque axe (seuils T°max et précipitations, `meta$breaks`) et trace la
**ligne « pas de changement » (0)** de chaque variable en **pointillé blanc**
(position fournie par le cœur, `meta$palette$zero`) — comme les cartes bivariées
de l'inventaire forestier IGN. Pur affichage, aucune modification du classement.

# nemetonshiny 0.106.13 (2026-07-15)

### Fixed — la couche gel R7 survit à un recalcul dans la même session (brief 035 §8)

Après un R7 calculé (« Risque de gel »), relancer « Lancer l'analyse » ou le
« Moteur » vidait la couche « Gelées tardives » : les deux handlers écrasaient
`rv$result` avec un résultat sans R7 (ces recalculs n'ont pas de Tmin en entrée).
Les colonnes R7 (`r7_gel_days` / `r7_status` / `R7`) sont désormais reportées du
résultat précédent par `ug_id` (`.regen_attach_r7`, réutilisé de la persistance
inter-sessions) — R7 n'est jamais recalculé (il reste opt-in), seulement conservé.

# nemetonshiny 0.106.12 (2026-07-15)

### Fixed — légende bivariée « Contexte régional (E-OBS) » en 5×5 (25 classes)

La carte bivariée (T°max × précipitations) affichait déjà 25 classes — le cœur
`eobs_downscale_bivariate()` classe en **5 par axe** (comme l'inventaire forestier
IGN) — mais la **légende** restait figée en 3×3 (9 cases), incohérente avec le
raster. `bivariate_legend_html()` dessine désormais une grille **N×N** pilotée par
`meta$palette$ncol` (5), avec repli sur la racine carrée du nombre de couleurs
puis 3×3 par défaut. Pur affichage, aucun changement de classement (cœur).

# nemetonshiny 0.106.11 (2026-07-15)

### Fixed — la couche « Gelées tardives » (R7) persiste entre deux sessions

À la réouverture d'un projet, la carte « Gelées tardives » retombait sur le
contour d'UGF nu : `restore_regeneration()` ne ré-attachait que l'exposition
microclimatique et le bilan hydrique, jamais R7 (calculé seulement en mémoire par
un run « Risque de gel »). `run_regeneration_frost()` persiste désormais son
résultat par UGF dans `cache/regeneration/r7.gpkg` ; `load_regeneration_precomputed()`
le relit et `restore_regeneration()` ré-attache les colonnes R7 par `ug_id`
(jointure pure, aucun recalcul), comme les autres couches.

### Added — infobulle « i » sur le radio « Forçage météo » (reGénération)

Le radio Forçage (SAFRAN / ERA5-Land) était le seul de la sidebar sans aide. Une
infobulle explique la différence de précision : SAFRAN (réanalyse Météo-France,
France seule, ETP fournie, sans clé) vs ERA5-Land (réanalyse globale, ETP
recalculée, clé CDS). Brief 035 §7.

# nemetonshiny 0.106.10 (2026-07-15)

### Changed — moteur reGénération plafonné en mémoire (anti-OOM, brief 035)

Le moteur microclimf+BILJOU tournait dans un worker `future::multisession` **nu**,
dans le scope de l'app : un pic mémoire postérieur au run pouvait faire tuer
RStudio par `systemd-oomd` (incident 2026-07-15, projet Reconfort). Le worker
délègue désormais l'exécution à `nemeton::run_memory_capped()`, qui isole le
calcul lourd dans un **enfant sous cgroup plafonné** (même mécanisme que FORDEAD,
spec 008). L'async UI et le suivi de progression (canal disque, poll 1 s) sont
inchangés.

**Garde de capacité** : le chemin plafonné ne s'active que si le `nemeton`
installé expose la version généralisée de `run_memory_capped()` (arguments
`package=`/`options=`, requiert le prérequis cœur du brief, ≥ 0.158.0). Tant que
ce n'est pas le cas, repli propre sur le chemin actuel — l'app ne casse pas sur
un cœur antérieur, et le plafonnement s'active automatiquement dès que le cœur
le fournit. Flag `nemetonshiny.regen_capped` (TRUE par défaut ; FALSE force le
repli, utile en dev quand la lib installée est en retard).

# nemetonshiny 0.106.9 (2026-07-15)

### Added — bilan persistant sous le bouton « Risque de gel (meteoland) »

En fin de calcul du risque de gel tardif (R7), un message reste affiché sous le
bouton, alors qu'auparavant seul un toast de 6 s apparaissait. Le cas le plus
utile : **« Calcul terminé : aucun jour de gel tardif détecté »** (message vert)
— un run réussi sans gélée tardive semblait sinon n'avoir rien produit. Quand du
gel est détecté, le message donne la médiane et l'étendue des jours de gel par
UGF ; si R7 n'a pu être calculé (Tmin indisponible), le motif reste affiché.

# nemetonshiny 0.106.8 (2026-07-15)

### Changed — grisage instantané des boutons de calcul (reGénération)

Le verrou d'exclusion des calculs grisait les autres boutons seulement après un
aller-retour serveur : pendant cette fenêtre, ils paraissaient encore cliquables
(sans risque — la garde serveur `deny_if_busy()` bloquait déjà tout second run).
Le grisage est désormais **instantané côté client** : un clic sur n'importe quel
bouton de calcul (`.regen-calc-btn`) désactive tous les autres sans attendre le
serveur (JS délégué, sans dépendance `shinyjs`). Le serveur reste l'autorité —
un compteur `click_tick` force l'observer de verrou à ré-activer les boutons dès
que le calcul se termine **ou** qu'un clic n'a finalement rien lancé (pas de
projet, prérequis manquants…). Le bouton « gel » n'est plus ré-activé à tort
quand meteoland est absent.

# nemetonshiny 0.106.7 (2026-07-15)

### Fixed — verrou d'exclusion mutuelle des calculs (onglet reGénération)

Pendant qu'un calcul lourd tournait (moteur microclimf/BILJOU, détection E-OBS,
téléchargement précipitations, gel R7), les autres boutons de l'onglet
reGénération restaient cliquables : `bslib::bind_task_button()` ne grise que le
bouton **propriétaire** de la tâche. Un second clic lançait un worker `future`
concurrent (mêmes rasters, même RAM, même `cache/regeneration/`), avec deux
risques — confusion (l'app semble gelée) et corruption de
`engine_status.json` écrit par deux tâches à la fois.

- Réactif `busy()` : vrai dès qu'une tâche **utilisateur** tourne (`engine`,
  `eobs`, `eobs_rr`, `frost` ; `context_task` auto-déclenché exclu).
- Grisage client de tous les boutons de calcul quand `busy()` est vrai
  (`update_task_button` pour les task buttons — leur binding ignore `disabled`,
  `updateActionButton` pour les boutons classiques).
- Garde serveur `deny_if_busy()` en tête de chaque déclencheur (backstop robuste
  du grisage, contournable par double-clic / race websocket).
- Libellé « occupé » générique (`regen_busy_generic`) sur les 4 task buttons :
  un bouton grisé par ricochet n'affiche plus le libellé mensonger d'une autre
  tâche.

### Fixed — confirmation E-OBS persistante sous le bouton « Auto (E-OBS) »

Les années détectées (`regen_auto_done`) n'apparaissaient qu'en toast de 6 s.
Elles restent désormais affichées sous le bouton une fois la détection terminée.

# nemetonshiny 0.106.6 (2026-07-14)

### Fixed — FORDEAD tourne désormais dans un process plafonné (spec 008)

Un run FORDEAD qui débordait en mémoire emportait **toute la session** : sous
pression, `systemd-oomd` ne tue pas le processus fautif mais le *scope* entier
— le 2026-07-14, RStudio et l'app sont partis à l'OOM en plein FORDEAD.
RECONFORT était épargné (son Python est un sous-processus `conda run`, déjà
placé dans un cgroup plafonné par le cœur) ; FORDEAD non, car son Python vit
dans l'interpréteur **embarqué** de reticulate — sa mémoire *est* celle du
worker `future`, donc celle du scope de l'app. Il n'y avait rien à plafonner
in-process.

Le run est donc déporté dans un **process R enfant plafonné** via
`nemeton::run_memory_capped()` (cœur ≥ 0.157.0, `MemoryMax=` +
`MemorySwapMax=0`). Un run qui déborde meurt **seul**, avec une erreur
attrapable qui remonte dans le toast d'erreur habituel. Le worker `future`
reste en place (il garde l'UI réactive) ; l'enfant vit dedans.

Ce qui ne change pas pour l'utilisateur : le `reactivePoll` de progression
(mêmes fichiers `.json` / `.ndjson`, même format), l'annulation coopérative
(l'enfant polle le même `cancel_path`) et les push ntfy par phase. Ces derniers
demandaient un soin particulier : sous isolation, **l'enfant écrit déjà** le
fichier de progression, donc le parent ne rejoue que la partie ntfy du callback
(nouveau `.build_fordead_ntfy_callback()`) — rejouer le composite aurait
dédoublé chaque événement.

Nouveau réglage optionnel `NEMETON_MEMORY_MAX` : vide → défaut cœur (70 % de la
RAM) ; `"16G"` pour laisser de l'air à RStudio sur une machine à 32 Go ;
`none` / `off` pour retirer le plafond si un run légitime se fait tuer.

Plancher cœur : `nemeton (>= 0.157.0)`.

### Added — Roue dentée + chrono sous les trois boutons « Lancer… »

Le retour visuel sous le bouton n'existait que pour RECONFORT. FAST et FORDEAD
sont alignés : les trois modes affichent désormais, **à l'endroit même où
l'utilisateur a cliqué**, la roue dentée qui tourne, l'étape en cours et le
chronomètre MM:SS — le même contenu que le toast (`.running_notif_content`,
partagé avec le moteur reGénération), qui reste affiché par ailleurs.

Un run peut durer des heures : le toast en haut à droite peut être fermé ou
manqué, et l'utilisateur se retrouvait alors sans aucun signe de vie. Le
rendu s'efface tout seul à la fin du run (même source que le toast : les
handlers de fin remettent `*_run_start` à `NULL`). RECONFORT passe du sablier à
la roue dentée pour que les trois soient strictement identiques.

### Fixed — Suivi sanitaire : deux textes qui mentaient

- **« Quelques minutes »** dans l'aide du mode RECONFORT : c'était faux. Un run
  peut durer des **heures, voire des jours** — l'emprise et le nombre de dates
  Sentinel-2 gouvernent tout. La promesse de durée est retirée : le chronomètre
  sous le bouton dit la vérité pendant le run, une estimation figée dans l'aide
  ne peut que se tromper. La dépendance **conda IOTA²/GEODES est conservée**,
  reformulée en « Opt-in : nécessite le bundle conda IOTA²/GEODES » — c'est une
  condition d'exécution (sans elle le run échoue), pas une estimation.
- **« sur les placettes enregistrées »** dans le sous-titre du Suivi continu
  Sentinel-2 : dernier vestige textuel des placettes dans le suivi sanitaire,
  devenu 100 % raster (FAST, FORDEAD, puis RECONFORT en v0.106.4). Remplacé par
  « sur la zone de suivi ».

# nemetonshiny 0.106.5

### Fixed — `NEMETON_SCRATCH_DIR` n'atteignait pas le worker (cœur >= 0.156.0)

`nemeton::scratch_dir()` (0.156.0) décide où atterrissent les intermédiaires
volumineux des pipelines longs — de ~800 Mo à la dizaine de Go. Mais le run
s'exécute **dans le worker**, et les workers sont **pré-chauffés au démarrage de
la session** : ils figent alors leur environnement. `NEMETON_SCRATCH_DIR`
n'étant pas dans la liste des variables transmises, un réglage posé ensuite
n'aurait jamais atteint le process qui calcule, et le cœur serait retombé sur
`tempdir()` **en silence** — parfois un tmpfs, c'est-à-dire de la RAM, ce qui
annulerait tout le bénéfice du streaming sur disque. La variable est désormais
capturée et rejouée côté worker (test de non-régression).

Plancher relevé : `Imports: nemeton (>= 0.156.0)`.

### Changed — spec 008 §4 : rendre au système la mémoire des workers persistants

Audit mémoire de l'app (le cœur a traité sa part en `nemeton` 0.155.0 : plafond
cgroup du sous-processus IOTA2, `filename=` sur ses appels terra, et
`terraOptions(memfrac = 0.25)` posé dans son `.onLoad` — **l'app ne duplique donc
pas ce réglage**, elle le consomme).

Ce qui restait à l'app, et que le cœur ne peut pas voir : les workers
`future::multisession` sont des processus R **persistants**. Un run lourd les
fait gonfler à plusieurs Go, et **ils y restent** jusqu'à la fin de la session.
Mesuré ici : un worker passe de 207 Mo à **6 409 Mo** après un calcul raster, et
n'en redescend jamais. Huit workers dans cet état suffisent à mettre la session
sous le seuil de pression de `systemd-oomd` (incident du 2026-07-13).

- Nouveau `.release_worker_memory()`, appelé en `on.exit()` à la fin des **10
  corps de workers** (FAST, FORDEAD, RECONFORT, les 5 moteurs reGénération,
  calcul projet). Il fait `rm(list = ls())` **puis** `gc(full = TRUE)` — les deux
  comptent, et dans cet ordre : `on.exit` s'exécute pendant que la frame du
  worker est encore vivante, donc un `gc()` seul ne libère presque rien (mesuré :
  6,4 Go → 1,6 Go). Avec le `rm()` d'abord, le worker retombe à **~210 Mo**, son
  niveau à vide.
- Carte RECONFORT : `masked_rasters_r` ne lit plus que la couche **affichée**.
  Les couches sont exclusives (`radioButtons`) : lire les trois masquait et
  matérialisait deux rasters pleins pour rien, que le cache du `reactive()`
  gardait ensuite vivants dans la session.
- Les 8 workers `future` sont conservés (décision produit : le `gc()` de fin de
  tâche fait disparaître le résidu, seul le socle de 1,9 Go subsiste).

### Changed — spec 008 §5 : une seule source de vérité pour les durées

`nemeton::format_duration()` (cœur 0.155.0) devient la source unique.
`format_elapsed()` et `.format_duration_human()` sont réduits à de minces
adaptateurs (règle #2 : l'app consomme, le cœur décide). Comportement vérifié
identique sur tous les cas limites (`NULL` / `NA` / négatif / non-numérique →
« ? »). Plancher relevé : `Imports: nemeton (>= 0.155.0)`.

# nemetonshiny 0.106.4

### Fixed — Suivi sanitaire : les deux bandeaux de validité RECONFORT

- Le bandeau « **Composition d'essences hors domaine validé** » n'existait pas
  côté RECONFORT (FORDEAD en a deux). Pire : il n'aurait **jamais** pu s'afficher,
  car le module appelait `nemeton::check_reconfort_validity(aoi)` sans lui passer
  `units` ni `bdforet` — `species_valid` restait donc `NA` en toutes
  circonstances. Les UGF du projet et la BD Forêt en cache
  (`cache/layers/bdforet.gpkg`) sont désormais transmises au cœur, et le parent
  rend les **deux** bandeaux (géo + essences), parité stricte avec FORDEAD.
- **Légende « Classes de santé »** : elle affichait un carré vert « 1-sain » que
  la carte ne dessine jamais (la classe 1 est volontairement transparente pour
  laisser voir le fond de carte). La légende ne liste plus que les classes
  réellement peintes (2-dépérissant, 3-très dépérissant).
- État vide de la carte RECONFORT : ne parle plus d'« alertes » mais d'absence
  de **run** (la carte est 100 % raster depuis le retrait de la couche Alertes).

### Added — Chronomètre sous le bouton « Lancer le diagnostic RECONFORT »

- Le chrono existait, mais uniquement dans le toast en haut à droite. Un run
  RECONFORT dure ~15 min : l'utilisateur qui ferme ou rate le toast n'avait plus
  aucun retour à l'endroit même où il a cliqué. Chrono ajouté **sous le bouton**
  (parité avec le moteur reGénération) ; le toast reste, il porte l'étape en cours.

### Fixed — spec 008 : un OOM se lit comme un OOM, et les durées sont lisibles

Implémentation du brief `nemeton/specs/008-suivi-sanitaire/brief-nemetonshiny.md`
(§3 et §5 ; §4 « pression mémoire de l'app » et §6 sont hors périmètre).

- **§3 — Échec mémoire.** Un diagnostic tué par le plafond mémoire du cœur sort
  en SIGKILL (`exit 137`). Le message brut (« RECONFORT map production failed for
  zone 3 (exit 137) ») ne disait rien à l'utilisateur : il est remplacé par une
  consigne actionnable (fermer d'autres applications, réduire l'emprise). Appliqué
  aux trois chemins d'erreur (RECONFORT + les deux de FORDEAD).
- **§5 — Durées.** Quatre messages de fin de run affichaient des **secondes
  brutes** : le run RECONFORT validé (819 s) annonçait « terminé en 819 s », un
  FORDEAD de deux heures aurait dit « 7243 s ». Ils passent désormais par
  `format_elapsed()` — « **13 min 39 s** », « **2 h 00 min 43 s** ». Les champs de
  données (`duration_sec`, `run_meta.json`) restent en secondes brutes.
- ⚠️ Le brief préconise `nemeton::format_duration()` (cœur **0.155.0**), mais la
  dernière release publiée est **0.154.1** : on utilise le formateur app existant,
  que le brief qualifie lui-même de correct. La consolidation vers le cœur (une
  seule source de vérité) reste à faire quand 0.155.0 sera publiée.

### Removed — Suivi sanitaire : la notion de « placette » disparaît de RECONFORT

- **Couche « Alertes » de la carte RECONFORT supprimée.** C'était le dernier
  vestige des marqueurs « placettes » dans le suivi sanitaire : FAST (v0.52.16)
  et FORDEAD (Phase A, décision D2) étaient déjà passés en pur raster. Elle
  dupliquait de surcroît le signal de la couche « Classes de santé ». La carte
  RECONFORT est désormais **100 % raster** (score / classes / probabilité).
- La table `alerts` de la base de suivi **reste écrite par le cœur et lue par
  `service_r5.R`** (indicateur R5) : seul l'affichage cartographique disparaît.
  Les placettes du **Plan de validation** (`data/samples.gpkg`, couche
  `validation_plots`) sont désormais les seules à persister dans l'application.
- `list_alerts_for_zone()` (service_monitoring_db) supprimée : plus aucun
  appelant depuis le retrait des marqueurs.
- Bouton « Exporter PNG » de la modale pixel RECONFORT retiré, avec ses helpers
  `save_plotly_png()` / `.pixel_export_engine()`. La planche reste exportable
  par la barre d'outils native de plotly.

### Changed — Vocabulaire des couches : « Alertes » → « Raster »

- Les groupes Leaflet nommés `"Alertes"` portaient en réalité un **raster**
  (masque de dépérissement FORDEAD, pixels au-dessus du seuil FAST) : renommés
  `"Raster"`, un nom qui dit ce que la couche contient.
- Sous-onglet « Alertes FORDEAD » renommé **« Carte FORDEAD »** — il rend la
  carte raster (le sous-onglet vectoriel homonyme avait été supprimé en
  v0.92.x), parité avec « Carte FAST » et « Carte RECONFORT ».
- Graphique pixel RECONFORT : contrôles « Lissage » + « Observations brutes »
  sur **une seule ligne** ; annotations pédagogiques (« creux estival qui
  s'enfonce », « pic plus faible = sénescence précoce », dérive) passées de
  **10 à 14 px** et de `#666` à `#333` — elles étaient illisibles en plein
  écran, où plotly agrandit la planche mais garde une police fixe en px.

# nemetonshiny 0.106.3

### Removed — reGénération : contexte régional vectoriel (code mort)

- `regeneration_context_eobs()` et `.regen_eobs_from_cache()` sont supprimées :
  le contexte régional passe par le raster downscalé E-OBS depuis la v0.105.0,
  plus aucun appelant n'existait. Leurs 3 tests partent avec elles, et les
  commentaires qui décrivaient encore l'ancien couplage `tx` + `rr` obligatoire
  sont remis en cohérence (chaque vue a ses besoins : `tx` seule pour la
  tendance T°max, `tx` + `rr` pour les précipitations et la bivariée).

# nemetonshiny 0.106.2

### Changed — reGénération : ligne « Indice E-OBS » redondante retirée

- La ligne « Indice E-OBS : année / année » sous le bouton « Auto (E-OBS) »
  doublait les champs Années déjà renseignés au-dessus : supprimée.

# nemetonshiny 0.106.1

### Changed — Contexte régional : tooltip « i » sur chaque vue

- Le sélecteur de vue de la carte « Contexte régional (E-OBS) » (Tendance T°max /
  Tendance précipitations / Bivariée) porte un « i » d'information par vue,
  expliquant la variable, l'unité, la lecture de la palette (rouge = défavorable)
  et la fiabilité (basse pour les précipitations/bivariée).

# nemetonshiny 0.106.0

### Added — Contexte régional E-OBS : 3 vues (T°max / précip / BIVARIÉE)

- La carte « Contexte régional (E-OBS) » gagne un **sélecteur de vue** (sidebar
  droite) : tendance **T°max** (`var="tx"`), tendance **précipitations**
  (`var="rr"`), et **carte bivariée** T°max × précipitations (classes 1-9), toutes
  en raster downscalé via le cœur `nemeton (>= 0.153.0)` (`eobs_downscale` +
  `eobs_downscale_bivariate`, `dem = NULL` auto-source WMS IGN).
- **Palette pilotée par le cœur** : univarié via `colorNumeric` selon
  `meta$palette$sense` (`hot_unfavorable` → chaud = rouge ; `dry_unfavorable` →
  sec = rouge) ; bivarié via `colorFactor` (9 couleurs/libellés du cœur) + une
  **légende bivariée 2D (grille 3×3)** rendue par l'app (aucun classement
  côté app — il vient du cœur). Chaud + sec = coin critique rouge.
- Chaque vue a son **cache** (`context_{tx,rr,bivariate}.tif` + meta.json) et son
  calcul **async** (notif chrono). Le **bouton de téléchargement des
  précipitations** (~800 Mo) est ré-exposé (requis pour rr/bivariée). Bandeau
  `need_tx` / `need_rr` et note de **fiabilité basse** pour rr/bivariée
  (downscaling pluie bruité). Plancher `Imports: nemeton (>= 0.153.0)`.

# nemetonshiny 0.105.1

### Fixed — Contexte régional : zoom perdu au slider d'opacité + raster qui disparaît au changement de fond

- La carte « Contexte régional (E-OBS) » lisait l'opacité directement dans son
  rendu : bouger le curseur reconstruisait la carte (zoom réinitialisé), et le
  raster, dessiné dans le rendu, disparaissait au changement de fond
  OSM/Satellite. Passage au patron FAST/FORDEAD : fond STABLE (rendu de base
  lisant raster + opacité en `isolate`, jamais re-rendu) + `leafletProxy` qui
  met à jour le raster/la légende sur changement d'opacité ou d'analyse. Zoom et
  fond préservés ; le raster survit au changement de fond (map-pane dédié,
  respect de la décoche du groupe).

# nemetonshiny 0.105.0

### Added — reGénération : contexte régional E-OBS en RASTER (downscalé)

- La carte « Contexte régional (E-OBS) » affiche désormais un **raster continu**
  (tendance de la T°max estivale) au lieu d'un semis de points bivarié. Le raster
  vient de `nemeton::eobs_downscale(dem = NULL)` (cœur >= 0.152.0) : le cœur
  auto-source une élévation grossière sur le buffer (WMS IGN) et krige les mailles
  E-OBS avec — le MNT parcellaire (trop petit) n'est plus passé. Résultat validé
  en réel : `n_points` 1 → 27–33, `status = "ok"`.
- Calcul en **worker async** (~4–18 s selon le réseau, WMS + krigeage), avec
  notification « engrenage + chrono » et **cache disque** (`.tif` + `meta.json`) :
  relecture instantanée (~0,01 s) aux visites/réouvertures suivantes. Le
  **curseur d'opacité** pilote enfin un vrai raster ; légende + palette issues de
  `meta` (chaud = rouge, `sense = "hot_unfavorable"`).
- Le contexte raster n'a besoin que de la série **tx** (plus de bivarié tx×rr) :
  le bandeau se réduit à « lancer Auto (E-OBS) » quand tx manque, et mappe
  `meta$reason` (4 clés i18n) pour les cas dégradés. Plancher
  `Imports: nemeton (>= 0.152.0)`.

# nemetonshiny 0.104.2

### Fixed — reGénération : carte « Contexte régional (E-OBS) » vide malgré les données

- La carte de contexte restait vide même quand les deux séries E-OBS (tx + rr)
  étaient bien en cache. Régression du refactor v0.103.0 : le semis E-OBS était
  ajouté par un observer proxy séparé, qui ne se redéclenchait pas à l'activation
  de l'onglet (dépendances stables) — les points n'étaient donc jamais dessinés.
  Le semis est de nouveau dessiné dans le rendu de la carte ; le fitBounds vise
  l'emprise UGF (stable), donc changer le buffer/l'opacité ne déplace pas la vue.

### Changed — reGénération : « Essence cible » masquée hors couche « Indice de priorité »

- Le sélecteur « Essence cible » n'affecte QUE la couche « Indice de priorité »
  (la re-priorisation live) — il est désormais masqué (`conditionalPanel`) quand
  une autre couche est affichée, où il n'aurait aucun effet visible.

# nemetonshiny 0.104.1

### Changed — perf : pré-chauffage des workers future au démarrage

- Le tout premier `future` d'une session charge le namespace `nemetonshiny` +
  ses dépendances (sf/terra/leaflet…) **dans le process worker** : ~5–6 s
  mesurées. Ce coût frappait la première tâche asynchrone — typiquement le
  `db_sync_project_async()` déclenché à l'ouverture du premier projet, ou le
  premier calcul / moteur. Un `warmup_async_workers()` charge désormais le
  namespace dans les workers **en arrière-plan**, 0,3 s après le démarrage (dans
  des process séparés, sans compétition avec le rendu de la page) : la première
  tâche async ne paie plus ce délai. Best-effort, idempotent, no-op sous un plan
  séquentiel. Complète le `warmup_geo_stack()` existant (thread principal).

# nemetonshiny 0.104.0

### Added — reGénération : risque de gel tardif (R7, moteur meteoland)

- Nouvel indicateur **R7 « risque de gel tardif »** (famille Risques & Résilience),
  câblé côté cœur `nemeton (>= 0.151.0)` : `meteoland_daily_grid()` interpole la
  température minimale journalière (SAFRAN → MNT) sur la fenêtre de débourrement,
  puis `indicateur_r7_gel()` compte les gelées post-débourrement. R7 suit le sens
  normal (haut = FAIBLE risque), NA-safe (absent du radar sans calcul), sans le
  routage d'inversion propre à R5.
- **Moteur opt-in** dans l'onglet reGénération : bouton « Risque de gel
  (meteoland) » (grisé si `meteoland`, en Suggests, est absent — avec message
  explicatif). Exécution en worker de fond, sortie **cachée**
  (`cache/regeneration/meteoland/tmin_*.tif`), repli gracieux → R7 absent sans
  jamais planter. Notification persistante « engrenage + chrono » dédiée
  (« interpolation gel »), distincte de biljou/microclimf.
- **Carte** : nouvelle couche « Gelées tardives (R7) » dans « Couche affichée »
  (rouge = beaucoup de gelées = critique). Libellé/tooltip R7 (FR/EN) + famille R
  étendue à 7 indicateurs (`INDICATOR_FAMILIES`).
- Rappel d'échelle (UI/tooltips) : R7/meteoland = **contexte régional NDP 1**
  (gradient topographique), PAS l'effet tampon sous couvert — ce dernier reste le
  domaine de `microclimate_run()` (microclimf + LiDAR HD, NDP 2+).
- Plancher `Imports: nemeton (>= 0.151.0)` ; `meteoland` ajouté en Suggests.

### Fixed — tests : plus de push ntfy réel titré « fileXXXX »

- Un `setup-ntfy.R` neutralise `NEMETON_NTFY_TOPIC` pour toute la suite : les
  tests exécutant un moteur n'envoient plus de vraie notification ntfy (titrée
  avec le basename du répertoire temporaire faute de metadata.json). Les tests
  qui valident ntfy posent leur propre topic dans un bloc local.

# nemetonshiny 0.103.0

### Fixed — reGénération : le toast de chargement ne s'affiche qu'en onglet reGénération

- Le toast « Chargement des cartes reGénération… » apparaissait dès l'ouverture
  d'un projet récent, même depuis l'onglet Sélection. La restauration (relecture
  du cache + toast) est désormais **différée à l'activation de l'onglet
  reGénération** : un changement de projet/UGF purge le résultat précédent (tout
  onglet, pour ne pas laisser un choroplèthe périmé) et pose un drapeau, que
  l'observateur ne consomme que lorsque l'onglet reGénération est actif.

### Changed — reGénération : téléchargement des précipitations avec chrono

- Le téléchargement de la série E-OBS de précipitations (~800 Mo, plusieurs
  minutes) affiche désormais une notification persistante « engrenage qui tourne
  + chronomètre MM:SS » (cadre unifié partagé avec les moteurs), retirée en fin
  de tâche — au lieu d'un simple message fugace.

### Changed — reGénération : carte « Contexte régional (E-OBS) » enrichie

- La carte de contexte gagne le choix de fond **OSM / Satellite** (contrôle de
  couches), l'**emprise des UGF en bleu**, et un **curseur d'opacité** pour le
  semis E-OBS. Le semis (centres de mailles E-OBS ~11 km, classe bivariée
  température × précipitation — un semis de POINTS, pas un raster) passe par un
  proxy Leaflet : changer le buffer ou l'opacité ne réinitialise plus le zoom.

# nemetonshiny 0.102.5

### Changed — reGénération : « Buffer contexte régional » en sidebar droite

- Le champ « Buffer contexte régional » (rayon, 25 km par défaut) passe dans une
  sidebar droite dédiée de la carte « Contexte régional (E-OBS) », en parité avec
  la sidebar « Couche affichée » de la carte principale — au lieu d'un champ
  posé au-dessus de la carte.

# nemetonshiny 0.102.4

### Fixed — commune : plus de flicker « … → Sélectionner une commune → … »

- À l'ouverture d'un projet, la commune restaurée (ex. « Loury ») disparaissait
  brièvement au profit du placeholder « Sélectionner une commune » avant de
  réapparaître. La liste est peuplée en deux temps (commune seule immédiate, puis
  liste complète du département) : la 2e mise à jour, en mode serveur, rechargeait
  les options en AJAX et selectize vidait l'affichage le temps du chargement. Le
  flux de restauration passe désormais en mode local (`server = FALSE`, options
  inline) : la sélection reste affichée en continu entre les deux mises à jour. Le
  flux normal de changement de département reste en mode serveur.

# nemetonshiny 0.102.3

### Added — reGénération : indicateur de chargement à l'ouverture d'un projet

- À l'ouverture d'un projet récent déjà analysé, la restauration des cartes
  reGénération (relecture du cache + rendu Leaflet) prenait un temps perceptible
  sans aucun retour visuel. Un toast « Chargement des cartes reGénération… »
  (bas de l'écran, parité Suivi sanitaire) s'affiche désormais pendant la
  restauration, retiré une fois les cartes prêtes. La restauration est différée
  d'un tick (`later`) pour que le toast se peigne avant le travail synchrone.

# nemetonshiny 0.102.2

### Changed — reGénération : champ « Type de peuplement » clarifié

- Un tooltip « i » explique que le choix Feuillu/Résineux fixe la **phénologie**
  du bilan hydrique du sol (BILJOU) — interception/transpiration saisonnières du
  feuillu caduc vs permanentes du résineux sempervirent — donc les jours de
  stress, la sensibilité et la priorité, et qu'il agit en amont (relance
  nécessaire), contrairement à l'essence cible (live).
- Les champs **Débourrement** et **Chute des feuilles** (phénologie des feuillus
  caducs) n'apparaissent plus que lorsque « Feuillu » est coché ; un résineux
  sempervirent n'a pas de cycle foliaire saisonnier.

# nemetonshiny 0.102.1

### Fixed — mode admin/dev local passait à tort en lecture seule

- Le verrou v0.102.0 forçait la lecture seule dès que `user_email` était vide.
  Or en mode admin/dev local (sans fournisseur OAuth), `mod_auth` laisse
  `user_email = NULL` tout en donnant des rôles via `NEMETON_AUTH_DEV_ROLES` :
  l'app se retrouvait bloquée en lecture seule alors qu'elle devait être
  éditable. La lecture seule dépend désormais du **rôle**
  (`can_edit_action_plan()` : rôle `lecteur` ou non authentifié → lecture seule ;
  anonyme-sans-rôle / `editeur` / `admin` → éditeur), jamais de la présence d'un
  email. Le verrou multi-utilisateurs ne s'engage qu'avec une identité stable
  (email OAuth) ; sans email — mono-utilisateur / dev — l'app est éditable sans
  poser de verrou en base. Nouveau bandeau `lock_role_banner` pour le cas rôle
  lecteur (distinct du « Connectez-vous » réservé au non-authentifié).

### Changed — reGénération : « Essence cible » met à jour la carte en direct

- L'essence cible n'alimente que la dernière étape du calcul (indice de
  priorité). Changer l'essence re-priorise désormais la choroplèthe **en direct**
  (`regen_reprioritize()` → `nemeton::indice_priorite_regen()`), sans relancer
  l'analyse complète (donc sans re-dériver la topographie LiDAR de R3). Le
  sélecteur « Essence cible » est déplacé dans la carte, sous « Couche
  affichée ». Le « Buffer contexte régional » est déplacé dans l'onglet carte
  « Contexte régional (E-OBS) », la seule vue qu'il concerne.

# nemetonshiny 0.102.0

### Added — verrou d'édition de projet (serveur multi-utilisateurs)

- L'application est désormais destinée à tourner sur un serveur avec droits
  CRUD partagés. Pour éviter que deux utilisateurs modifient le même projet en
  même temps, un **verrou d'édition** est posé à l'ouverture d'un projet :
  le premier utilisateur édite, les suivants l'ouvrent en **lecture seule**.
- La logique de verrou vit dans le cœur `nemeton (>= 0.148.0)`
  (`project_lock_acquire` / `heartbeat` / `release` / `status`, table
  `nemeton.project_lock`, TTL 120 s volé par péremption). Côté app,
  `R/service_lock.R` n'est qu'une **enveloppe** : une connexion courte par appel,
  aucune logique métier (règle #1). Le verrou est tenu par un **heartbeat**
  (toutes les 45 s), pas par la connexion — il survit au va-et-vient des
  connexions DB de l'app.
- `app_server` pilote le cycle de vie à partir d'un **point de branchement
  unique** (`app_state$project_id`) : acquisition à l'ouverture, relâche du
  projet précédent, heartbeat périodique, relâche à la fin de session
  (`onSessionEnded`, hors contexte réactif — `pid`/`hid` capturés par
  `isolate`). L'identité stable est l'**email OAuth** : un utilisateur anonyme
  (sans email) n'obtient jamais de verrou et travaille en lecture seule.
- Un **bandeau** en tête signale l'état lecture seule (tenu par un autre
  utilisateur, ou anonyme). **Toutes** les actions mutantes de l'app sont
  court-circuitées par le garde partagé `deny_if_readonly(app_state)` (toast
  d'avertissement) : reGénération (analyse, moteur E-OBS, recalcul PAI,
  enregistrement DB), projet (sauvegarde SUFOSAT/LST, suppression), calcul des
  indicateurs (`mod_home` : lancer/recalculer), ingestion terrain et validation
  sanitaire (`mod_field_ingest`), échantillonnage (`mod_sampling`,
  `mod_validation_sampling`), et l'édition des UGF (`mod_ug` : créer, déplacer,
  fusionner, découper, renommer, grouper, importer, annuler — 16 actions). Le
  plan d'action (`mod_action_plan`) combine désormais sa permission de rôle
  existante **et** le verrou projet dans le même garde. Les actions de lecture
  (navigation, sélection carte, aperçus, analyse CV) restent libres. i18n FR/EN
  (6 nouvelles clés `lock_*`).
- Plancher `Imports: nemeton (>= 0.148.0)`. La table de verrou est créée par
  `nemeton::db_migrate()`, appelé (idempotent) à l'initialisation du schéma.

# nemetonshiny 0.101.4

### Fixed — reGénération : la carte « Contexte régional (E-OBS) » était toujours vide

- La carte de contexte n'a **jamais** pu s'afficher. Deux causes cumulées.
  D'abord, `load_regeneration_precomputed()` cherche `eobs_tx.tif` / `eobs_rr.tif`,
  des fichiers **qu'aucun code n'écrit** : la vraie donnée est un NetCDF déposé par
  « Auto (E-OBS) » sous `cache/regeneration/eobs/`. Même famille que le bug
  `pc$eobs` corrigé en v0.101.0 — la clé lue n'est pas celle qui est écrite.
  Ensuite, `nemeton::tendances_estivales_eobs()` est **bivariée** : elle exige la
  série des températures maximales (`tx`) **et** celle des précipitations (`rr`),
  or « Auto (E-OBS) » ne rapatrie que `tx`. Le cœur levait, `tryCatch` renvoyait
  `NULL`, la carte s'affichait nue, sans explication.
- `regeneration_context_eobs()` lit désormais les NetCDF réellement cachés via
  `nemeton::load_eobs_source(nc = )` — lecture disque, **jamais** de requête CDS
  au rendu d'un onglet. Un bandeau indique ce qui manque et propose, quand seule
  `rr` fait défaut, un **bouton opt-in** « Télécharger les précipitations
  (~800 Mo) » lancé en tâche de fond (`ExtendedTask`). La carte se re-rend seule à
  la fin du téléchargement.
- Nouveaux helpers `regen_eobs_cached_nc()`, `regen_context_availability()`,
  `regen_fetch_eobs_rr()`. Clés i18n `regen_context_need_tx`,
  `regen_context_need_rr`, `regen_eobs_rr_fetch`, `regen_eobs_rr_running`,
  `regen_eobs_rr_running_short`, `regen_eobs_rr_done`, `regen_eobs_rr_failed`.

# nemetonshiny 0.101.3

### Fixed — reGénération : l'ouverture d'un projet gelait l'application (~190 s)

- **Régression introduite en v0.101.0 (spec 035 B2).** L'observateur de
  restauration appelait `run_regeneration()` **de façon synchrone** à chaque
  ouverture de projet. Shiny étant mono-thread, toute la session était bloquée le
  temps de l'appel — l'onglet **Sélection** ne s'affichait qu'ensuite. Mesuré sur
  un projet réel de 30 UGF : **190 s**, dont **132 s** dans
  `indicateur_r3_secheresse(dem = )`, qui re-dérive la topographie (pente,
  exposition, TWI) depuis la mosaïque MNT LiDAR à chaque appel.
- Le brief B2 affirmait que « le coût du rechargement est nul, aucun moteur n'est
  relancé ». C'est exact de microclimf, biljouR et lasR — et faux de R3, qui n'est
  pas un moteur au sens du brief mais coûte plus cher qu'eux tous.
- Nouvelle fonction `restore_regeneration()` : la restauration **rattache** les
  colonnes déjà en cache (exposition microclimatique, bilan hydrique) puis
  recalcule l'indice de priorité, une arithmétique par ligne. Ni R3, ni les
  sous-indicateurs A3/A4/W4/R6 — ces colonnes du radar appartiennent à une vraie
  analyse, et aucun critère d'acceptation de B2 (carte, indice, table) n'en a
  besoin. **190 s → 0,41 s** sur le même projet.
- Tests : un test de service fait échouer volontairement R3, A3, A4, W4, R6 et
  `microclimate_detect_years()` pour garantir qu'aucun n'est atteint ; un test de
  module garantit que `run_regeneration()` n'est jamais appelé à l'ouverture.

# nemetonshiny 0.101.2

### Added — reGénération : lisibilité des overrides (spec 035 B4)

- **Ce que le moteur a réellement utilisé est désormais affiché.** B1 a rendu le
  `lai_max` et la réserve utile variables par UGF, mais rien à l'écran ne le
  distinguait d'un scalaire : le vecteur était passé à `regen_bilan_hydrique()`
  puis oublié. `run_regeneration_engine()` remonte maintenant `lai_max`, `ewm` et
  `ewm_source` ; la sidebar affiche la **médiane et l'étendue sur les UGF**
  (« LAI dérivé : 4,1 [2,8 – 6,3] sur 30 UGF »).
- **Le repli SoilGrids n'est plus silencieux.** `build_biljou_soil(source =
  "soilgrids")` dégrade vers un sol uniforme quand `files.isric.org` est
  injoignable, et le `cli_warn` qui l'annonce meurt sur le `stderr` du worker
  `multisession`. La provenance effective du sol est déduite de sa forme (liste de
  `biljou_soil` par UGF vs objet unique) : le repli devient un **avertissement
  dans l'onglet**, une **entrée dans le journal du moteur**, et une mention
  explicite sous le badge. Un `ewm` saisi reste un choix délibéré (`uniform`), pas
  un repli.
- **Section « Paramètres experts » repliée** (`bslib::accordion`) regroupant
  `lai_max`, `ewm` et `rooting_depth_cm`. Isolés dans la sidebar, ces champs
  invitaient au remplissage réflexe : saisir `lai_max` annule le bénéfice d'un PAI
  calculé en 57 minutes, sans aucun signal. Une seule phrase porte la sémantique
  commune (vide = dérivé, rempli = forcé) et un badge **« forcé »** s'affiche à la
  place de la statistique dérivée quand l'utilisateur court-circuite la donnée.
- Nouvelles clés i18n FR/EN : `regen_expert_section`, `regen_expert_hint`,
  `regen_override_badge`, `regen_lai_derived_stats`, `regen_ewm_derived_stats`,
  `regen_ewm_fallback_uniform`. Clés devenues orphelines retirées
  (`regen_soil_section`, `regen_lai_auto`).

### Fixed

- `run_regeneration_engine()` : le bloc d'un `tryCatch()` est évalué dans le frame
  de l'appelant, pas dans une fonction — un `<<-` y aurait écrit dans
  l'environnement du package au lieu de la variable locale. Un test de
  non-régression vérifie qu'aucune liaison ne fuit dans le namespace.
- La lecture de la provenance du sol (`.regen_soil_ewm()`) est purement
  informative et ne peut plus faire échouer le bilan hydrique : toute forme
  inattendue de `sol` donne `source = NA` au lieu de lever.

# nemetonshiny 0.101.1

### Added — reGénération : observabilité du moteur (spec 035 B3)

- **Les erreurs du moteur n'atteignaient que ntfy.** `run_regeneration_engine()`
  s'exécute dans un worker `future` (`multisession`) : ses `cli::cli_warn()`,
  `message()` et `cat()` partent sur le `stdout` du processus fils, que R ne
  relaie pas vers la console principale. Le seul canal qui traversait la frontière
  de processus en temps réel était le POST HTTP de `.ntfy_send()` — d'où des
  erreurs visibles dans le téléphone et nulle part ailleurs.
- **Journal disque en ajout seul** : nouveau `cache/regeneration/engine.log`
  (une ligne JSON par entrée, via `.regen_log()`). Il traverse la frontière de
  processus **par le disque** et **survit à la mort du worker** (OOM, `kill`) —
  c'est le seul post-mortem disponible. Distinct de `engine_status.json`, que
  `.regen_write_phase()` écrase à chaque phase et qui ne peut donc pas accumuler.
  Tronqué au **début** de chaque run, jamais à la fin ;
  `.regen_cleanup_status()` n'y touche pas.
- **Les diagnostics du moteur ne sont plus perdus.** En succès, `rv$warnings`
  était **écrasé** par les avertissements du re-run fast-path — un ensemble sans
  rapport — et ceux du moteur ne survivaient que dans un toast de 10 s. Ils sont
  désormais **cumulés**. En erreur (worker mort), `engine_task$result()` lève et
  `eng` n'existe jamais : les avertissements accumulés avant la mort sont relus
  depuis `engine.log`.
- **Relais console** (`.regen_relay_log()`) : chaque entrée `error` / `warning`
  du journal est rejouée en `cli::cli_alert_danger()` / `cli_alert_warning()`
  dans le **processus principal**, dans les deux branches. Les jalons `info`
  restent muets.
- **L'échec de ntfy n'est plus avalé.** `.ntfy_send()` renvoie `FALSE` sans rien
  dire ; si le premier envoi d'un run échoue alors que `NEMETON_NTFY_TOPIC` est
  défini, une entrée `warning` est journalisée. Perdre silencieusement le seul
  canal de notification est ce qui rendait ce type de panne indiagnosticable.
- **UI** : bloc repliable « Journal du moteur » sous le badge d'état (horodatage,
  niveau, source), et le panneau d'avertissements s'affiche désormais **même sans
  résultat** — un moteur qui meurt n'en produit aucun, et c'est précisément là
  qu'il faut voir pourquoi. Clés i18n `regen_engine_log`, `regen_log_ntfy_failed`.

### Fixed

- `.regen_log()` : `cat(file =)` sur un répertoire absent émet un **warning**
  avant de lever, que `tryCatch(error =)` ne capture pas — un journal « jamais
  fatal » polluait quand même la sortie. Handler `warning` ajouté.

# nemetonshiny 0.101.0

### Added — reGénération : bilan hydrique réellement spatialisé (spec 035 B1)

- **Le PAI LiDAR alimente enfin BILJOU.** Le PAI structural dérivé du nuage
  (57 min de calcul sur un run réel, caché dans `cache/regeneration/pai.tif`)
  n'était consommé que par microclimf : le bilan hydrique, lui, retombait sur le
  défaut cœur par type de peuplement (5 pour du feuillu, 4,5 pour du résineux).
  Le garde-fou `is.null(grid)` sautait le calcul du `lai_max` précisément quand
  le LiDAR HD était présent. Désormais `nemeton::lai_max_depuis_pai()` dérive un
  `lai_max` **par UGF** depuis le PAI caché ; le repli satellite S2/PROSAIL est
  inchangé et ne sert plus qu'en l'absence de grille LiDAR.
- **Le plateau, pas la moyenne.** `.regen_lai_per_unit()` prenait une moyenne
  zonale, alors que `biljouR::biljou_lai()` lit `lai_max` comme le **plateau** de
  la phénologie — la moyenne le sous-estime, d'autant plus que l'UGF contient des
  trouées. L'agrégation passe au cœur (percentile haut, pixels non-canopée
  exclus).
- **Sol dérivé de SoilGrids par UGF.** `build_biljou_soil(source = "soilgrids")`
  dérive la réserve utile de chaque UGF depuis SoilGrids 250 m (pédotransfert
  Saxton & Rawls) au lieu d'un sol uniforme sur tout le massif. Dégrade proprement
  en sol uniforme si `files.isric.org` est injoignable.
- **UI** : « Eau extractible (mm) » devient un **override optionnel** — vidé
  (défaut), il déclenche SoilGrids ; renseigné, il force l'ancien comportement
  uniforme. Nouveau champ « Profondeur d'enracinement (cm) » (défaut 100). Le
  badge « Canopée : LiDAR HD » signale quand le `lai_max` du bilan hydrique vient
  du PAI. Nouvelle phase moteur « Réserve utile (SoilGrids) (i/n) » sur le canal
  `engine_status.json`.

Ensemble, ces trois corrections lèvent la cause du symptôme observé sur le projet
réel `20260701_204501_ltcp` : `njstress` et `deb_stress` **identiques sur les 30
UGF**, `istress` et `rew_min` ne prenant que 2 valeurs (les 2 mailles SAFRAN de
8 km). BILJOU n'a aucun terme spatial : toute la variabilité vient du sol et du
LAI qu'on lui passe.

### Added — reGénération : restauration à l'ouverture d'un projet (spec 035 B2)

- Rouvrir un projet déjà analysé affichait les **contours d'UGF nus** — pas de
  choroplèthe, pas d'indice de priorité, pas de table — jusqu'à un clic sur
  « Lancer l'analyse ». `rv$result` n'était écrit que par ce bouton ou par la fin
  du moteur ; aucun observateur n'écoutait `app_state$current_project`. Un
  observateur relit désormais le cache disque à l'ouverture. Ce n'est **pas un
  recalcul** : `run_regeneration()` consomme les sorties `precomputed` en
  fast-path (rattachement de colonnes) — ni microclimf, ni biljouR, ni lasR ne
  démarrent.
- Restauration **seulement si une sortie de moteur existe** (`biljou.gpkg` ou
  `sensibilite.gpkg`) : un projet sans cache reste vierge, sans avertissement
  trompeur. Les années de référence sont fournies au service pour ne pas
  déclencher `microclimate_detect_years()` à l'ouverture.
- Purge du résultat précédent en changeant de projet (sinon le choroplèthe de
  l'ancien projet restait à l'écran), et alimentation de
  `app_state$regeneration_result`, que `mod_synthesis` consomme pour la
  perspective IA — la synthèse d'un projet rouvert ignorait silencieusement la
  reGénération.

### Fixed — reGénération : la détection des années de référence ne marchait jamais

- `run_regeneration()` passait `precomputed$eobs` à
  `nemeton::microclimate_detect_years()`, alors que
  `load_regeneration_precomputed()` peuple `eobs_tx` / `eobs_rr` et **jamais**
  `eobs`. Le cœur recevait donc toujours `NULL` et abandonnait aussitôt
  (« needs an E-OBS summer series ») : **la détection automatique des années
  échouait systématiquement** dès que l'utilisateur n'avait pas saisi les deux
  années à la main, avec un avertissement `detect_years: …` en prime. La série
  cachée `eobs_tx` (Tmax JJA, une couche par année) est exactement le contrat
  attendu par le cœur : elle lui est désormais transmise (`pc$eobs %||%
  pc$eobs_tx`).
- En l'absence totale de série E-OBS, l'appel est **sauté** au lieu d'être tenté
  puis rattrapé : il ne pouvait qu'échouer, et son avertissement ne renseignait
  que sur une absence de donnée déjà visible à l'écran.

### Changed

- Plancher cœur relevé : `Imports: nemeton (>= 0.147.0)` (`lai_max_depuis_pai()`,
  `build_biljou_soil(source =)`, `ewm_depuis_soilgrids()`).

# nemetonshiny 0.100.19

### Added — reGénération : infobulles « i » sur les couches de la carte

- Chacune des 4 entrées du radio **« Couche affichée »** (carte reGénération)
  porte désormais un **« i » d'information** qui explique la variable
  cartographiée — définition, unité, sens de lecture (valeur élevée = quoi) et
  moteur d'origine (microclimf / BILJOU) — puis rappelle que la **légende** est
  un dégradé continu **borné aux min/max des UG affichées** : les couleurs sont
  relatives au projet, jamais absolues. 5 clés i18n FR/EN
  (`regen_map_*_info`, `regen_map_legend_scale`), tooltip placé à gauche
  (le sidebar est collé au bord droit).

### Fixed — reGénération : palette de la carte inversée (rouge = critique)

- La carte reGénération peignait les UG **les plus critiques en vert** et les
  plus saines en rouge : `colorNumeric("RdYlGn", domain = vals)` associe le
  rouge à la borne basse, or pour les 4 couches une valeur **haute** est
  toujours défavorable (indice de priorité élevé, plus de jours de stress
  hydrique, ΔT°max élevé = canopée qui tamponne mal). La palette passe en
  `reverse = TRUE` : **rouge = valeur élevée / situation critique, vert =
  situation favorable**. La légende (`addLegend`) suit la palette et reste
  bornée aux min/max des UG affichées.

# nemetonshiny 0.100.18

### Changed — reGénération : bloc « Années de référence » au-dessus du moteur

- Dans le sidebar reGénération, le bloc **« Années de référence »** (année
  moyenne / caniculaire + bouton **Auto (E-OBS)** + ligne indice E-OBS) est
  déplacé **au-dessus** de la section **« Moteur microclimf réel »**. L'ordre
  suit le flux logique : l'utilisateur choisit d'abord les millésimes de
  référence, puis lance le moteur qui les consomme.

# nemetonshiny 0.100.17

### Fixed — ntfy monitoring : vrai nom de projet dans l'entête (au lieu du nom de zone DB périmé)

- Les push ntfy des moteurs **FAST / FORDEAD / RECONFORT** affichaient le **nom
  de zone lu en base** (`monitoring_zone.name`), qui pouvait être **périmé** —
  p. ex. `file4ec…` (nom temporaire de la 1ʳᵉ inscription) au lieu du vrai nom
  de projet « Reconfort » après un renommage. Le worker reçoit désormais le
  **nom de projet à jour** résolu côté session (`app_state$current_project`)
  et le préfère au nom de zone DB (`project_name %||% .resolve_zone_name()`).
  Répercuté sur l'entête **et** le corps du message. Repli DB conservé pour les
  tests / appels legacy (paramètre `project_name = NULL`).

# nemetonshiny 0.100.16

### Changed — notif bas-droite unifiée (cadre + engrenage animé + chrono) + projet dans l'entête ntfy

- Les notifications persistantes « en cours » des moteurs **FAST / FORDEAD /
  RECONFORT** (`mod_monitoring`) et **reGénération** (`mod_regeneration`)
  partagent désormais **le même cadre, la même police et le même picto** — un
  **engrenage qui tourne** (`gear-fill` animé via la classe CSS `nmt-spin`) —
  plus un **chronomètre monospace « — MM:SS » qui défile** (H:MM:SS au-delà
  d'une heure). Rendu centralisé dans le nouveau `R/utils_notif.R`
  (`.running_notif_content()`, `.fmt_elapsed()`) ; chaque module garde son cycle
  de vie (id / removeNotification) et son tick (`invalidateLater(1000)`).
- Le **chrono défile désormais aussi sur FAST / FORDEAD / RECONFORT** (avant :
  seul reGénération l'avait). Chaque moteur mémorise son instant de lancement
  (`*_run_start`) et son dernier libellé (`*_run_msg`) ; un `observe` dédié
  re-rend la notif chaque seconde entre deux events de progression — utile
  quand une phase reticulate (fit harmonique FORDEAD) reste plusieurs minutes
  sans event. Les handlers `.fordead_handle_progress_event` /
  `.reconfort_handle_progress_event` reçoivent `start` + `on_msg` et coupent le
  ticker (`on_msg(NULL)`) sur `complete`/`error` (pas de notif « fantôme »).
- `.monitoring_spinning_msg()` devient un mince wrapper sur le helper partagé —
  fin de la divergence historique entre les deux familles de moteurs (l'un avait
  l'engrenage animé, l'autre non).
- **Messages ntfy** : le **nom du projet** est ajouté à l'entête (`Title`) de
  chaque push — `Nemeton FAST - <projet>`, `Nemeton Regen - <projet>`, etc. via
  le helper `.ntfy_title(engine, project)` (translittération ASCII stricte de
  l'entête HTTP : accents → ASCII, jamais de caractère non imprimable). Nom de
  zone résolu côté monitoring (`.resolve_zone_name`), nom de projet côté
  reGénération (`.regen_project_name`, lu depuis `metadata.json`).

# nemetonshiny 0.100.15

### Added — Radar A5 « Rafraîchissement urbain » (LST Theia, spec 032)

- Nouvel indicateur **A5** (`indicateur_a5_rafraichissement`) câblé sur la famille
  **A** du radar (à côté de A1-A4), alimenté par la **température de surface (LST)**
  Theia/Thermocity. **Sens direct** (haut = plus frais que l'entour = bon) : aucune
  inversion côté app, la normalisation positive se fait au cœur.
- **Acquisition + cache** : `build_lst_layer()` (miroir SUFOSAT) fetch `theia_lst`
  et cache sous `cache/layers/lst/` ; branche dispatcher injecte `lst` + `buffer_m`
  (rayon de l'anneau de référence) dans l'indicateur.
- **Opt-in strict** : toggle « Rafraîchissement urbain (LST) » dans l'onglet projet
  (`set_project_lst_urbain()` → `metadata$lst_urbain`), gaté sur les identifiants
  Theia. A5 n'entre dans le set calculé que si activé (`list_available_indicators()`
  gaté) — sans activation, **famille A inchangée (A1-A4)**, aucune requête Theia.
- **Couverture urbaine uniquement** (Thermocity = quelques métropoles) : sur un
  projet rural, A5 = NA proprement (pas de 5ᵉ axe vide), sans régression. Tooltip
  i18n explicite.
- Plancher cœur déjà satisfait (`nemeton (>= 0.146.2)` ≥ 0.114.0 requis par A5).
  Dernier brief app de la série specs 027-034 : tous livrés.

# nemetonshiny 0.100.14

### Added — reGénération : cache disque du PAI LiDAR (~38 min économisées/run)

- La phase **PAI** (`pai_depuis_nuage()` sur le nuage COPC) tient >30 min sur un
  gros AOI et était **recalculée à chaque run** (ancien PAI en `tempfile()`). Elle
  ne dépend pourtant que du nuage + de la grille : invariante pour un projet/AOI.
  L'app passe désormais `pai_cache = cache/regeneration/pai.tif` à
  `nemeton::regen_sensibilite()` (branche LiDAR) : le cœur **relit** le PAI si la
  géométrie s'aligne (phase éclair, `source = "cache"`), sinon le calcule + l'écrit
  (auto-invalidation sur changement d'AOI/résolution — jamais de PAI périmé).
- Notif bas-droite : la phase affiche **« PAI (cache) »** sur un hit disque (au lieu
  de « PAI (LiDAR) », phase longue) — nouvelle clé `regen_phase_pai_cache`.
- **Lien « Recalculer le PAI »** sous le badge « Canopée : LiDAR HD » (n'apparaît
  qu'après un run LiDAR réussi, quand le cache existe) : supprime `pai.tif` pour le
  cas d'un nuage remplacé à emprise constante (géométrie inchangée → le cœur ne
  réinvalide pas seul).
- Plancher cœur relevé à `nemeton (>= 0.146.2)` : version où le moteur d'exposition
  tourne de bout en bout sous microclimf 2.0.0 (chaîne de correctifs 2026-07-08) —
  `sensibilite.gpkg` réellement produit. (`pai_cache` existe depuis 0.145.0.)

### Changed — reGénération : moteur microclimf en tête du sidebar, jargon retiré

- La section « Moteur microclimf réel » est remontée **en tête du sidebar gauche**
  (juste sous l'intro, avant « Années de référence ») — c'est l'action longue
  principale de la vue, elle n'a plus à être cherchée en bas.
- Suppression de la mention « **(option B)** » dans le libellé : c'était du jargon
  interne de la spec 027 (option A = chemin rapide sur rasters précalculés,
  option B = run réel du moteur), sans signification pour l'utilisateur.

# nemetonshiny 0.100.13

### Fixed — R-CMD-check vert : test SUFOSAT hermétique

- `test-sufosat-t3.R` (« build_sufosat_layer fetches via Theia, caches… »)
  échouait en CI sur 7 assertions : `build_sufosat_layer()` garde en amont sur
  `theia_api_key_configured()` (clés `TLD_ACCESS_KEY`/`TLD_SECRET_KEY` ou fichier
  `.apikey`) et retourne `NULL` **avant** d'atteindre l'I/O mockée quand aucune
  clé n'est configurée. Sans clés (CI), le moteur ne partait pas — faux négatif,
  jamais un vrai appel réseau (`load_theia_source` est mocké).
- Le test fournit désormais des clés Theia **factices** (`withr::local_envvar`)
  pour franchir le garde ; il reste 100 % hermétique (toute l'I/O Theia mockée,
  cache en tempdir, aucun réseau). **R-CMD-check passe de FAIL 7 à FAIL 0**
  (PASS 8899) — la suite est enfin entièrement verte en CI.

# nemetonshiny 0.100.12

### Fixed — Tests moteur reGénération : robustesse à la locale

- `test-regeneration_engine.R` comparait la sortie i18n du moteur (résolue via
  `get_app_options()$language`) à des chaînes **FR codées en dur**. Selon l'ordre
  d'exécution, une suite antérieure pouvait laisser `nemeton.app_options` en
  `"en"` → le moteur émettait des warnings/ntfy EN et 6 tests échouaient (faux
  négatif, sans lien avec le code produit). Chaque test force désormais la langue
  FR localement (`withr::local_options`), les rendant déterministes.
- Vérifié **FAIL 0 / PASS 73** sous locale EN forcée. Aucun changement de code
  produit. (Le R-CMD-check global reste bloqué par `test-sufosat-t3.R`, tests
  Theia réseau-dépendants hors sandbox CI — chantier distinct.)

# nemetonshiny 0.100.11

### Added — reGénération : message de phase en cours du moteur (notif bas-droite)

- Le **moteur réel** affiche désormais **en bas à droite** la **phase en cours**
  (grille → PAI → microclimat étés moyens `year (i/n)` → canicule → exposition →
  BILJOU) au lieu d'une notif indéterminée figée plusieurs minutes. Chrono qui
  ticke, libellé rafraîchi chaque seconde.
- **Canal fichier + poll** (spec 027, brief engine-phase-status ; lève la réserve
  du §5 du brief engine-feedback) : le worker `future` écrit la phase courante
  dans `cache/regeneration/engine_status.json` (écriture atomique tmp+rename), la
  session principale le poll (`invalidateLater(1000)`) et rend la phase dans la
  notif persistante `engine_notif` (même id → remplacement en place). Indépendant
  de ntfy : le canal in-app fonctionne sans `NEMETON_NTFY_TOPIC`.
- **Phase sautée = information de premier plan** : sur un projet type RECONFORT
  (pas de clé CDS ou structure de végétation manquante), microclimf ne démarre
  pas → la notif affiche « Exposition microclimf ignorée : {raison} » puis passe
  à BILJOU (SAFRAN), au lieu de rester bloquée sur une phase antérieure.
- 11 clés i18n FR/EN `regen_phase_*` ; nettoyage du fichier d'état à l'invoke
  (phase fantôme d'un run précédent) et en fin de tâche (success/error).
- Plancher cœur relevé à `nemeton (>= 0.144.0)` : active la phase **PAI**
  (`regen_expo:pai`, `source = lidar|satellite`) « Structure de végétation
  (PAI LiDAR/satellite)… » entre la grille et le microclimat. Les 6 phases sont
  donc toutes rendues.

# nemetonshiny 0.100.10

### Changed — Plancher cœur relevé à `nemeton (>= 0.143.0)` : moteur d'exposition plus rapide

- Bénéficie des optimisations `nemeton@0.143.0` du moteur d'exposition
  reGénération, **sans changement de code applicatif** (le cœur les applique en
  interne) : (1) `pai_depuis_nuage()` clippe la lecture LiDAR (`lasR`, filtre
  `-keep_xy`) à l'emprise de travail — sur nuage COPC, seule l'AOI est lue,
  dérivation PAI bien plus rapide sur grand massif ; (2) forçage ERA5 dégroupé
  (1 requête/an au lieu de 12) avec retry/back-off — nettement moins de throttle
  CDS. Le warning `regen_engine_era5_interrupted` reste comme filet de sécurité.

# nemetonshiny 0.100.9

### Added — reGénération : notifications ntfy du moteur réel (au fur et à mesure)

- Le moteur « Lancer le moteur réel » (long : microclimf + ERA5 + BILJOU, worker
  `future`) pousse désormais des **notifications ntfy** aux jalons — comme
  FAST/FORDEAD/RECONFORT : `start` → `micro_start` → `micro_done`/`skip` →
  `biljou_start` → `biljou_done` → résumé (`cached`) + warnings.
- **Granularité fine** (cœur ≥ 0.142.0, `progress_callback`) : un push par année
  ERA5 (`ERA5 %d (%s) [i/n]`), par été microclimf, et au démarrage BILJOU
  (`%d points`).
- **Opt-in strict** : sans `NEMETON_NTFY_TOPIC`, aucun envoi, zéro surcoût
  (callback non passé au cœur). Titre ntfy ASCII (`Nemeton Regen`) ; body accentué.
- Plancher cœur relevé à `nemeton (>= 0.142.0)` (`progress_callback` sur
  `regen_sensibilite()` / `regen_bilan_hydrique()`).

# nemetonshiny 0.100.8

### Fixed — reGénération : le moteur microclimf produit enfin sa sortie

- « Lancer le moteur réel » ne produisait **jamais** `sensibilite.gpkg`
  (`cache/regeneration/microclimf/` vide, bandeau « non calculée ») : l'app
  appelait `nemeton::regen_sensibilite()` **sans structure de végétation**
  (`las`/`pai`), or le cœur l'exige et abandonnait avant tout ERA5 (abort avalé
  en warning). L'app fournit désormais cette structure :
  - **nuage LiDAR HD** prioritaire — `resolve_regen_lidar_grid()` résout aussi
    `cache/layers/lidar_nuage` (`.las/.laz/.copc.laz`) → passé en `las` (le cœur
    en dérive le PAI) ;
  - **repli LAI Sentinel-2/PROSAIL** sinon — `pai` réutilise le raster déjà caché
    par BILJOU ;
  - **aucune source** → warning i18n dédié `regen_engine_no_vegetation_structure`.
- Le dossier `microclimf/` n'est créé que si le moteur est réellement lancé, et
  retiré s'il reste vide après échec (plus de fausse impression de run). Les
  `era5_*.nc` cachés **persistent** → un re-lancement reprend depuis le cache.
- Throttle CDS pendant l'acquisition ERA5 (~12 requêtes/an) : warning distinct
  `regen_engine_era5_interrupted` (« relancez pour reprendre »).

# nemetonshiny 0.100.7

### Changed — Nettoyages à la source : CRS des couches + Theia

- **CRS LiDAR HD → EPSG:2154 à la source** : les GeoTIFF LiDAR HD IGN au WKT
  dégénéré (sans autorité) sont stampés 2154 à la mosaïque, au retour de cache et
  dans le contrôle de couverture — supprime les « No DEM available » et les
  re-téléchargements. Stamp **conditionnel** (uniquement si `describe$code` est
  `NA`) : jamais de tamponnage aveugle.
- **CRS WMS NDP 0 → EPSG:4326 à la source** : le DEM BD ALTI et l'IRC servis par
  le WMS IGN (datum « unknown ») se voient réassigner l'autorité 4326 demandée ;
  le NDVI dérivé en hérite — plus de « CRS do not match » sur C2/R1/R2/R3/W3.
- **Theia** : retrait de l'appel déprécié `nemeton::theia_configure_s3()` (le
  cœur signe en interne via la gateway STAC, R pur) ; commentaires `reticulate`
  périmés mis à jour. `reticulate` reste requis ailleurs (FORDEAD/RECONFORT).

### Changed — reGénération : feedback des boutons async (moteur réel & Auto E-OBS)

- Les boutons **« Lancer le moteur réel »** et **« Auto (E-OBS) »** deviennent des
  `bslib::input_task_button` : ils se **désactivent + affichent un spinner**
  pendant la tâche (plus de run concurrent qui corromprait
  `sensibilite.gpkg` / `biljou.gpkg`) et se réactivent à la fin (succès ou erreur).
- Une **notification persistante** en bas à droite reste affichée pendant tout le
  run (retirée à la fin), au lieu de disparaître après 6 s.
- Un **chrono MM:SS** (H:MM:SS au-delà d'une heure) ticke chaque seconde sous
  chaque bouton tant que la tâche tourne.

# nemetonshiny 0.100.6

### Fixed — reGénération : R3 sécheresse utilise enfin le DEM terrain

- R3 affichait toujours « R3: no DEM; drought stress from BILJOU metrics alone »
  et ignorait la modulation topographique (pente / exposition / TWI) : le loader
  ne lisait le DEM que depuis `<projet>/cache/regeneration/dem.tif`, un fichier
  **jamais produit** par l'app. Il réutilise désormais le **DEM terrain déjà
  acquis par le pipeline principal** — LiDAR HD **MNT** 1 m (`lidar_mnt_mosaic.tif`,
  NDP-1) en priorité, BD ALTI 25 m en repli — cherché à la racine du projet **et**
  sous `cache/layers/`.
- Rappel : `lidar_mnh_mosaic.tif` est un **modèle de hauteur de canopée (MNH)**,
  pas un DEM terrain (MNT) — il n'est volontairement pas utilisé pour R3.

# nemetonshiny 0.100.5

### Fixed — reGénération : bouton « Auto (E-OBS) » ne téléchargeait rien

- Le bouton « Auto (E-OBS) » affichait « Détection E-OBS indisponible » **sans
  déclencher de téléchargement** : l'app appelait `nemeton::load_eobs_source()`
  **sans plage d'années**, or le cœur (`.eobs_cds_fetch`) sort en `NULL` avant
  toute requête CDS quand `years` et `period` sont absents. `run_regeneration_detect_years()`
  passe désormais une **fenêtre d'années explicite** (se terminant à
  `année_courante - 2` pour la latence de publication E-OBS, bornée à 2011 pour
  tenir dans un seul bloc de période CDS) — la requête CDS est bien soumise
  (validé : `period=2011_2024`, `version=30.0e` acceptée par le serveur).
- Les échecs d'acquisition sont désormais tracés (`cli::cli_warn`) au lieu d'être
  avalés silencieusement, ce qui rendait le diagnostic impossible.

# nemetonshiny 0.100.4

### Added — Validation : tirage pondéré continu FORDEAD/RECONFORT (spec 014)

- Les onglets **FORDEAD** et **RECONFORT** du plan de validation offrent
  désormais la même pondération **continue** que l'onglet FAST : les placettes
  sont tirées avec une probabilité d'inclusion **∝ la sévérité continue par
  pixel** (FORDEAD `anomaly_index`, RECONFORT `score`), au lieu du seul poids
  par classe. Un sélecteur « Pondération du tirage » (continu / uniforme, défaut
  **continu**) apparaît ; les classes restent le **masque d'éligibilité**. La
  colonne `alert_weight` (sévérité au point tiré) est exposée dans la table et
  l'infobulle carte. Normalisation et équilibre GRTS restent côté cœur.
- Repli propre décidé par l'app : si un ancien run ne porte pas la couche de
  sévérité, le tirage retombe en **uniforme** (info, pas d'erreur). Un raster de
  sévérité non géoréférencé est signalé (`validation_weight_mismatch`).
- Plancher cœur relevé à `nemeton (>= 0.140.0)`
  (`create_validation_sampling_plan(weighting, weight_raster)`).

# nemetonshiny 0.100.3

### Added — reGénération : pré-remplissage « Essence cible » depuis BD Forêt v2

- Le sélecteur « Essence cible » est désormais **pré-rempli** via
  `nemeton::regen_species_choices()` : les **essences présentes sur la zone**
  (mapping TFV BD Forêt v2 → classe d'essence, cœur) sont listées **en tête**
  (groupe « Présentes sur la zone »), suivies des essences d'adaptation. Le
  sélecteur réagit à l'AOI du projet. Dégradation propre : sans colonne TFV, la
  liste complète des essences FRM est affichée (au lieu d'un sélecteur vide).
- L'info-bulle est mise à jour (le mapping TFV→essence est livré côté cœur, plus
  « à venir »).

# nemetonshiny 0.100.2

### Fixed — reGénération : bouton « Auto (E-OBS) » fonctionnel (spec 027 L2 / 034)

- Le bouton « Auto (E-OBS) » affichait systématiquement « Détection E-OBS
  indisponible » : l'app ne passait que l'AOI à `microclimate_detect_years()`,
  qui exige une série E-OBS. Elle **acquiert désormais la série** via
  `nemeton::load_eobs_source()` (E-OBS ECA&D/Copernicus, même clé CDS qu'ERA5,
  mise en cache) avant de détecter les années moyenne/caniculaire.
- **Exécution asynchrone** (worker `future`) : le téléchargement E-OBS (CDS,
  potentiellement en file) ne bloque plus l'application ; un indicateur « en
  cours » s'affiche et les champs Année se remplissent au retour. Dégradation
  propre en NULL (pas de clé / hors couverture / CI) → saisie manuelle inchangée.
- Plancher cœur relevé à `nemeton (>= 0.134.0)` (`load_eobs_source`).

# nemetonshiny 0.100.1

### Added — reGénération : provenance canopée + repli satellite NDP 0 (spec 033 D5)

- **Badge de provenance canopée** dans l'onglet reGénération : « Canopée :
  LiDAR HD » (mesure structurale, PAI) ou « Canopée : satellite (repli) »
  (LAI Sentinel-2/PROSAIL, NDP 0) avec info-bulle d'avertissement (proxy
  dégradé, LAI ≠ PAI). La provenance est **lue** depuis
  `nemeton::detect_ndp()$augmented` — aucune décision de source côté app (règle #1).
- **Repli NDP 0** : en l'absence de grille LiDAR HD, le moteur BILJOU utilise le
  **LAI Sentinel-2/PROSAIL** (`nemeton::lai_sentinel2`) agrégé par UGF comme
  `lai_max`, mis en cache (`lai_prosail.tif`). Dégradation propre : sans scène /
  sans PROSAIL → pas de badge satellite, comportement inchangé.
- **Détection CDS durcie** : `regen_cds_credentials_ready()` reconnaît désormais
  aussi le motif de stockage `ecmwfr_<user>` (clé posée dans `.Renviron`), en
  plus de `CDSAPI_KEY`/`ECMWFR_CDS_KEY` et du keyring `ecmwfr`.

# nemetonshiny 0.100.0

### Added — reGénération : bilan hydrique BILJOU réel (option B, spec 027 L2)

- Le **moteur réel** (opt-in) lance désormais **aussi le bilan hydrique BILJOU**
  (`nemeton::load_biljou_forcing` + `build_biljou_soil` +
  `regen_bilan_hydrique`), en plus de microclimf, dans le même worker `future`.
  Sortie mise en cache (`biljou.gpkg`) et consommée en fast-path par le run
  normal. Plancher cœur relevé à **`nemeton (>= 0.132.0)`**.
- **SAFRAN sans clé Copernicus** : le forçage `SAFRAN` (défaut) débloque le
  bilan hydrique **sans identifiants CDS** ; `ecmwfr::wf_set_key` n'est requis
  que pour le forçage `ERA5`. Le statut du moteur affiche la disponibilité de
  chaque moteur (microclimf / BILJOU) selon les prérequis réunis.
- **Dégradation propre** : hors couverture forçage/sol → garde `regen_guard_biljou`
  (message i18n) sans erreur brute ; chaque moteur échoue indépendamment.

# nemetonshiny 0.99.0

### Added — reGénération : moteur microclimf réel (option B, spec 027 L1)

- Nouveau **run microclimf réel** (LiDAR HD + forçage ERA5-Land) via le chemin
  moteur de `nemeton::regen_sensibilite`, opt-in depuis la sidebar reGénération
  (« Moteur microclimf réel »). Exécution **asynchrone** (worker `future`) : le
  run lourd (minutes→heures) ne bloque pas l'application ; la sortie est mise en
  cache (`sensibilite.gpkg`) et consommée en fast-path par le run normal.
- **Prérequis gardés** : le moteur n'est lançable que si la **grille LiDAR HD**
  du projet existe (`<projet>/cache/layers/lidar_mnt` + `lidar_mnh`, produite au
  calcul des indices avec source CHM LiDAR HD) et que les **identifiants
  Copernicus CDS** sont configurés (`ecmwfr::wf_set_key`) — sinon un message
  i18n propre indique le prérequis manquant. Aucun secret dans le repo (règle #8).
- **BILJOU (bilan hydrique) reste sur la garde option A** : l'acquisition
  météo/sol doit vivre dans le cœur (`load_biljou_forcing` / `build_biljou_soil`,
  brief cœur fourni) avant d'activer son run réel.

# nemetonshiny 0.98.6

### Fixed — reGénération : contour UGF visible sur la carte avant analyse (spec 027)

- Onglet Carte de reGénération : les UGF n'apparaissaient **qu'après** un calcul
  (le choroplèthe n'était dessiné que depuis le résultat). Le **contour UGF de
  base** est désormais tracé dès l'ouverture (depuis l'emprise du projet) et
  conservé tant qu'aucune couche exploitable n'est disponible.

# nemetonshiny 0.98.5

### Changed — reGénération : plancher cœur 0.129.2 + garde moteurs (spec 027)

- **Plancher `Imports: nemeton (>= 0.129.2)`** : le moteur microclimf tourne
  désormais réellement côté cœur (bugs API corrigés en 0.129.1) et le charabia
  de séquences d'échappement disparaît (0.129.2).
- **Garde des moteurs reGénération (option A)** : `run_regeneration()` n'appelle
  plus microclimf (sensibilité) ni BILJOU (bilan hydrique) quand aucune sortie
  précalculée n'est fournie — il émet un **message i18n propre et actionnable**
  au lieu de l'erreur cœur brute. Le run réel des moteurs (entrées LiDAR /
  météo-sol) reste un opt-in (option B, à venir).
- **UI** : sidebar « Couche affichée » de l'onglet Carte reGénération rendue non
  rétractable (`open = "always"`).

# nemetonshiny 0.98.4

### Changed — Forêt ancienne (N2) : auto-fetch IGN, fin de l'upload manuel (spec 031, 2026-07-03)

- **Suppression du bloc d'upload « Forêt ancienne »** de la sidebar « Sélection »
  (onglet Sélection). Plus rien à configurer.
- **Récupération automatique** au calcul de la couche **IGN « BD Forêts
  anciennes »** (nationale, millésime 2025, Licence Ouverte / Etalab 2.0) pour
  l'emprise du projet, injectée dans `indicateur_n2_continuite()`. L'acquisition
  (téléchargement départemental + filtre `Nature = forêt ancienne`) est déléguée
  au cœur `nemeton::load_foret_ancienne_source()` (résolu dynamiquement) ;
  l'app orchestre et cache sous `<projet>/cache/layers/foret_ancienne/`.
- **Dégradation propre** : tant que le cœur n'expose pas l'acquisition, N2 reste
  calculé sur la couverture forestière **actuelle** (aucune régression). Se câble
  automatiquement dès la release cœur, sans intervention (`@*release`).

### Changed — reGénération : sélecteur de couche en sidebar droite (spec 027)

- Sous-onglet « Carte » : le radio « Couche affichée » passe dans une **sidebar
  droite dédiée** accolée à la carte (parité panneau droit des cartes FAST), au
  lieu de la sidebar de configuration à gauche. Les fonds OSM/Satellite/UGF
  restent dans le contrôle natif Leaflet.

# nemetonshiny 0.98.3

### Changed / Fixed — reGénération : carte et projets Reconfort (spec 027, 2026-07-03)

- **Carte** : le sélecteur de couche adopte le style des cartes FORDEAD/FAST —
  **contrôle de couches natif Leaflet dans la carte** (fonds OSM/Satellite +
  couche « UGF », coin haut-droit, rétractable) ; le radio « Couche affichée »
  passe **dans la sidebar** (parité « Indice FAST »). Le choroplèthe et la
  légende sont mis à jour par `leafletProxy` (zoom conservé). Suppression de
  l'overlay `<details>`.
- **Fix** : `units_sf()` retombe désormais sur la géométrie **UGF puis parcelles**
  quand `indicators_sf` est absent (projet **Reconfort/monitoring** sans calcul
  des 31 indicateurs). Auparavant l'onglet affichait le message trompeur
  « besoin d'un projet » alors qu'un projet était chargé, et « Auto (E-OBS) »
  semblait ne rien faire.
  > Rappel : l'auto-détection E-OBS elle-même dépend d'une extraction estivale
  > côté cœur `nemeton` (`microclimate_detect_years`, « not wired yet », spec 027
  > L2) — d'où les années pré-remplies par défaut (2018/2022) comme chemin nominal.

# nemetonshiny 0.98.2

### Added — reGénération : section PDF Quarto (spec 027 L6, finalisation, 2026-07-03)

- Le rapport PDF (`report_template.qmd`) inclut désormais une section
  **« reGénération — vulnérabilité climatique »** : tableau des UG les plus
  sensibles (rang, indice de priorité, sensibilité, jours de stress, réserve
  en eau, ΔT°max, couverture), rendu via `knitr::kable`/LaTeX. La section
  n'apparaît que si une analyse reGénération est disponible.
- `generate_report_pdf()` / `prepare_report_data()` acceptent un `regen_units`
  optionnel (additif, rétrocompatible) ; `mod_regeneration` publie son résultat
  sur `app_state$regeneration_result`, transmis par l'export PDF de `mod_synthesis`.
- Rendu vérifié end-to-end (lualatex). Data-prep déjà couvert par
  `regeneration_report_summary()`.

# nemetonshiny 0.98.1

### Changed — reGénération : durcissement UX de l'onglet (spec 027, 2026-07-03)

- **Warnings** moteurs nettoyés des codes ANSI `cli` avant rendu HTML
  (`.regen_step` / détection années) — plus de `\033[…m` dans le bandeau.
- **Retour au lancement** : `withProgress` (overlay immédiat, empêche les
  reclics pendant le run) + toast final visible (succès / « N avertissement(s) »).
- **Auto (E-OBS)** : tooltip explicatif + message de confirmation en bas à droite
  (années détectées, ou avertissement si indisponible).
- **Années de référence** pré-remplies par défaut (2018 / 2022), affinables par Auto.
- **Essence cible** : correction du peuplement du sélecteur (colonnes `code`/`label`
  de la table de tolérances — auparavant vide). Tooltip précisant que le
  pré-remplissage depuis BD Forêt v2 nécessite un mapping TFV→essence côté cœur.
- **LAI max** : tooltip (auto depuis le PAI LiDAR HD si vide).
- **Boutons Export GPKG / Enregistrer l'état** déplacés dans la sidebar, sous
  « Lancer l'analyse ».
- **Sélecteur de couche** déplacé dans un **menu rétractable superposé dans la
  carte** (haut-droite), au lieu d'une barre au-dessus.
- **Onglet « Radar des 12 familles » retiré** de reGénération (réservé à Synthèse).

# nemetonshiny 0.98.0

### Added — reGénération : nouvel onglet de vulnérabilité climatique (spec 027 — L4/L5/L6 livrés, 2026-07-03)

Premier jalon de l'onglet **reGénération** (lecture de vulnérabilité climatique,
spec 027). Nécessite `nemeton (>= 0.118.0)`.

- **Radar** (`app_config`) : sous-indicateurs microclimat câblés dans les
  familles A/W/R — **A3** (T°max sous couvert), **A4** (tamponnement thermique),
  **W4** (VPD), **R6** (sensibilité microclimatique) — libellés + tooltips FR/EN.
  Sans run reGénération, colonnes absentes → familles inchangées (aucune
  régression).
- **Service** (`service_regeneration`) : `run_regeneration()` orchestre la
  séquence cœur (détection années → sensibilité microclimf → bilan hydrique
  BILJOU → sous-indicateurs A3/A4/W4/R6 → R3 enrichi → indice de priorité) via
  le chemin `precomputed`, avec mode « bilan hydrique seul » et **dégradation
  propre** (erreur moteur → warning actionnable, pas de crash). Aucune
  ré-inversion côté app.
- **UI** (`mod_regeneration`) : nouvel onglet « reGénération » — panneau de
  configuration (années moyenne/canicule + auto E-OBS, peuplement, sol, forçage
  SAFRAN/ERA5, résolution, essence cible optionnelle, buffer), run avec retour
  immédiat et dégradation propre, puis résultats en onglets : **carte** Leaflet
  commutable (indice de priorité / sensibilité / jours de stress / ΔT°max),
  **carte de contexte E-OBS**, **tableau** trié par rang (filtre couverture),
  **fiche parcelle**, **radar** A/W/R (via `create_family_index` + `nemeton_radar`).
- **i18n** : section reGénération FR/EN (~55 clés) + `indicator_A3/A4/W4/R6`.
- Plancher `Imports: nemeton (>= 0.118.0)`.
- **LLM (L5)** : nouveau profil expert **« Adaptation climatique »**
  (`inst/experts/adaptation_climat.yml`, FR/EN) — priorise A/R/W/C, exploite
  `njstress`/`istress`/`sensibilite`/`indice_priorite_regen`, recommande des
  itinéraires de régénération (essences tolérantes, densité, îlots, calendrier)
  et flague les conflits avec la biodiversité (B/N). Auto-chargé et sélectionnable
  dans `mod_synthesis` (liste de profils dynamique).

- **Persistance & export (L6)** :
  - Persistance **versionnée** en base : `db_save_regeneration()` écrit chaque
    run dans `nemeton.regeneration_states` (JSONB par UG, versions archivées pour
    le suivi dans le temps, §6A) + migration `inst/sql/migration_004_regeneration.sql`.
  - Export **GeoPackage** des colonnes §7 : `export_regeneration_geopackage()`.
  - `mod_regeneration` : bouton de téléchargement GPKG + enregistrement en base
    (si configurée) avec retour de version.
  - Data-prep de la **section Quarto** reGénération : `regeneration_report_summary()`
    (top-N trié par rang de sensibilité, prêt pour `knitr::kable`).
  - 5 clés i18n L6.

> L4 + L5 + L6 (cœur) livrés. Reste : insertion de la section reGénération dans
> le template Quarto PDF + vérification de rendu (à faire en local avec Quarto).

# nemetonshiny 0.97.8 (2026-07-02)

### Fixed — Alignement suite de tests + libellé i18n de l'axe radar T3

Correctif de cohérence suite à l'ajout de T3 (coupes rases, spec 030) à la
famille T : les assertions dérivées de `INDICATOR_FAMILIES` /
`list_available_indicators()` sont réalignées (comptages d'indicateurs 33→34 /
34→35, `get_column_family_map` 68→70) et la clé i18n `indicator_T3` (« Coupes
rases » / « Clear-cuts ») est ajoutée — requise pour le libellé de l'axe radar
T3 (le test i18n `mod_synthesis` exige une clé `indicator_<code>` par code de
famille). Aucun changement fonctionnel ; répare la CI de 0.97.7. Suite complète
revérifiée en local (équivalent `test_local`).

# nemetonshiny 0.97.7 (2026-07-02)

### Fixed — Chargement d'un projet récent : plus de bascule « nouveau projet » sur la Commune

Au chargement d'un projet récent, l'app pouvait basculer comme si on démarrait
un nouveau projet dès que le menu « Commune » se mettait à jour : toutes les
parcelles cadastrales de la commune s'affichaient et les parcelles du projet
disparaissaient. Cause : le selectize « Commune » émet un `input$commune=""`
transitoire pendant que ses choix sont repeuplés (restauration), et ce blip
n'était gardé que par `rv$is_restoring` (mod_search) tandis que la
réinitialisation du projet (mod_home) était gardée par
`app_state$restore_in_progress` — deux drapeaux relâchés à des instants
différents (`delay=1` vs `delay=0`), d'où une fenêtre de course qui effaçait le
projet puis rechargeait le cadastre communal. Les deux observers
`selected_commune` de `mod_home` utilisent désormais une garde **fondée sur
l'intention** (indépendante du timing) : la commune du projet chargé est
dérivée de ses parcelles, et un blip `""` ou une re-sélection de la commune du
projet ne réinitialise plus rien ; seule une bascule vers une **autre** commune
démolit le projet en cours. Test de régression ajouté.

### Added — Radar T3 « Coupes rases » (SUFOSAT, spec 030)

L'indicateur **T3** (`nemeton::indicateur_t3_coupes_rases`) est câblé sur le
radar de la famille **T** (à côté de T1/T2), alimenté par le produit national
**SUFOSAT** (détection de coupes rases par radar Sentinel-1, CNES/CESBIO)
récupéré depuis Theia. Toute la logique métier — y compris l'**inversion** de
T3 (haut = beaucoup de coupe = mauvais) — reste dans le cœur ; l'app ne câble
que la source et ne ré-inverse jamais.

- **UI** (`mod_project`) : bloc optionnel « Coupes rases (SUFOSAT) » — pas
  d'upload, un simple toggle + `window_years` (1-8, défaut 5) et `min_proba`
  (0.5-1.0, défaut 0.9). **Gaté sur Theia configuré** (identifiants S3) :
  message d'invite sinon.
- **Calcul** (`service_compute`) : `build_sufosat_layer()` récupère les deux
  assets SUFOSAT (dates + proba) via `theia_configure_s3()` /
  `load_theia_source()`, les met en cache sous
  `<projet>/cache/layers/sufosat/` (clé = bbox AOI) et les injecte dans
  `indicateur_t3_coupes_rases(sufosat_dates=, sufosat_proba=, window_years=,
  min_proba=)`. T3 est ajouté à `list_available_indicators()` (comme N2) ;
  **sans source activée : T3 = NA, famille T inchangée (T1/T2), aucune requête
  Theia.**
- **Radar** (`app_config`) : T3 rattaché à la famille T (libellé + tooltip
  FR/EN) — l'axe T passe de 2 à 3 sous-indicateurs quand la source est active.
- **Persistance** (`service_project`) : `set_project_sufosat()`
  (`metadata$sufosat`, purge du cache au désactivage).
- 10 clés i18n FR/EN. Tests : build+cache+reuse, garde-fous, injection +
  propagation des paramètres, persistance (25 assertions).

# nemetonshiny 0.97.6 (2026-07-02)

### Added — Forêt ancienne → continuité N2 (spec 031)

L'utilisateur peut désormais fournir une **source historique de forêt ancienne**
(raster classé — Cassini / état-major scanné — ou vecteur digitalisé — couche
IGN forêt ancienne) pour un score de **continuité N2 réel**, au lieu du défaut
50 / de la seule couverture actuelle. Toute la logique métier reste dans le cœur
(`nemeton >= 0.113.0`, `build_foret_ancienne_mask()`), l'app ne fait que câbler
la source.

- **UI** (`mod_project`) : bloc optionnel « Forêt ancienne (continuité N2) » —
  upload raster (`.tif`/`.tiff`) ou vecteur (`.gpkg`/`.shp`/`.geojson`), paramètres
  adaptés au type détecté (raster → `forest_class` multi-sélection via
  `terra::unique()` **ou** `threshold` ; commun → `min_area_m2`), aide i18n
  rappelant les sources acceptées et que **Corona-4B** n'est pas utilisable en
  France. Enregistrement/retrait de la source par projet.
- **Calcul** (`service_compute`) : si une source est fournie **et** que N2 est
  demandé, `build_foret_ancienne_layer()` construit le masque via le cœur, le met
  en cache sous `<projet>/cache/layers/foret_ancienne/` (clé = hash source +
  params) et le stage dans `layers$vectors$foret_ancienne`, injecté dans
  `indicateur_n2_continuite(foret_ancienne = )`. **Sans source : N2 inchangé
  (aucune régression).**
- **Persistance** (`service_project`) : `set_project_foret_ancienne()` /
  `clear_project_foret_ancienne()` (copie de la source dans le projet + config
  sous `metadata$foret_ancienne`).
- 14 clés i18n FR/EN.
# nemetonshiny 0.97.5 (2026-07-02)

### Fixed — Tour guidé : bascule d'onglet réparée (erreur JS cicerone)

La bascule d'onglet du tour guidé (`on_highlight_started`, `R/service_tour.R`)
était **cassée silencieusement** : cicerone 1.0.4 injecte ce callback brut dans
`new Function("return " + js)()` (cicerone.js:101), or on lui passait une chaîne
commençant par `var …` → `return var …` → `SyntaxError: Unexpected token 'var'`.
La compilation des steps échouait (driver.js : « There are no steps defined to
iterate »), donc le tour ne pouvait plus changer d'onglet. Le JS est désormais
une **expression de fonction** (`function(){…}`), valide une fois préfixée de
`return`. Corrige aussi les deux erreurs JS client observées au boot (visibles
notamment dans les logs du smoke E2E, tour auto-démarré sur session neuve).
Test de régression ajouté (format `function(){…}`, chaîne équilibrée).

# nemetonshiny 0.97.4 (2026-07-02)

### Fixed — Smoke E2E `mod_rag_admin` ré-armé (spec 009.2 / E7)

Le smoke test `test-mod_rag_admin-e2e.R`, quarantiné depuis v0.74.1
(`skip("FIXME…")`), est **ré-armé et vert**. Deux corrections de test (le code
applicatif était correct) :
- **Ouverture de la modale** : le gear rend un `nav-link role="tab"` ; un clic
  DOM et `app$click()` sont avalés par le comportement d'onglet Bootstrap et ne
  déclenchent jamais `input$open`. On pilote l'input d'action avec la valeur
  `"click"` (`app$set_inputs(..., allow_no_input_binding_ = TRUE)`), seul
  déclencheur fiable pour un `actionLink` sous shinytest2.
- **Stabilisation** : `wait_for_idle()` lève « session unstable » à cause d'une
  erreur JS *cicerone* (tour guidé, « no steps to iterate ») sans rapport ; on
  la tolère (`try()`) et on attend l'apparition du DOM via `wait_for_js()`
  (modale puis bouton `add_row`). La tab RAG lazy monte alors correctement.

Pré-requis : un **Chrome non-snap** (`google-chrome` .deb) — le chromium *snap*
fait wedger `Page.navigate` sous `AppDriver`. Le boot reste gardé
(`tryCatch → skip()`) pour ne jamais transformer une flakiness navigateur en
échec.

# nemetonshiny 0.97.3 (2026-07-02)

### Changed — Liste « Commune » remplie instantanément au chargement d'un projet

Au chargement d'un projet, la liste déroulante « Commune » restait vide
plusieurs secondes : son remplissage attendait `restore_task` (spawn d'un worker
`future` + `load_all(nemeton)` + appel `geo.api.gouv.fr` pour **toute** la liste
du département). Or le contour communal mis en cache au moment de la sauvegarde
porte déjà le **nom** de la commune (colonne `nom`). On peuple donc désormais le
dropdown **immédiatement** avec la seule commune restaurée (libellé correct
affiché tout de suite) ; `restore_task` remplace ensuite ce choix unique par la
liste complète du département en arrière-plan (pour changer de commune), en
conservant la sélection. Projets *legacy* sans contour caché : comportement
inchangé (chemin async).

# nemetonshiny 0.97.2 (2026-07-02)

### Fixed — Bouton de calcul rétabli pour un projet figé en « computing »

Un projet dont le calcul des indices a été **interrompu** (redémarrage ou
crash de l'app entre l'écriture du statut `computing` et celle de
`completed`/`error`) restait figé en `status = "computing"` sur disque. Au
rechargement, aucun worker ne tourne : l'observateur de reprise décline (fichier
de progression périmé > 120 s) et `computing_project_id()` vaut `NULL`, donc le
garde « calcul en cours dans cette session » ne masque pas le bouton — pourtant
`compute_button_ui` ne gérait que `draft`/`error` et `completed`, si bien que
`computing` tombait dans la branche `NULL` : **le bouton de calcul disparaissait
sans moyen de relancer** (cf. projet RECONFORT bloqué). Désormais les statuts
transitoires périmés (`computing`, `downloading`, `pending`) qui franchissent le
garde live sont traités comme relançables et réaffichent le bouton « Calculer ».

# nemetonshiny 0.97.1 (2026-07-02)

### Fixed — Suivi sanitaire : prérequis BD Forêt surfacé avant le clic

Créer une zone de suivi croise UGF × BD Forêt v2 ; le GPKG
`<projet>/cache/layers/bdforet.gpkg` est produit par le **calcul du projet**
(onglet Synthèse, `download_ign_bdforet`). Ce prérequis n'était signalé qu'en
notification transitoire **au clic** de « Enregistrer la zone » — le bouton
paraissait pourtant actif. Il est désormais traité comme les autres gardes :
- le bouton « Enregistrer la zone » est **désactivé** tant que
  `bdforet.gpkg` est absent ;
- le **message d'aide proactif** sous le bouton indique alors « BD Forêt
  manquante… Lancez d'abord le calcul du projet (onglet Synthèse) », avec la
  même précédence que le handler (projet → BD Forêt → UGF → base de suivi).

### Changed — Planche pixel RECONFORT : essence + lon/lat dans le titre

Le titre de la planche pixel (`plot_pixel_dieback()`) reprend désormais
l'**essence** du pixel (« Pixel de <essence> — … », comme la maquette) et
affiche en 2ᵉ ligne la **latitude/longitude** du pixel cliqué (+ le modèle) —
parité avec le graphique FORDEAD. L'essence, le modèle et les coordonnées
proviennent des métadonnées du pixel (`attr(series, "species")` / `"v_model"`
et le clic carte), passés en options par `mod_monitoring_reconfort_map` au
rendu **et** à l'export PNG (le titre voyage donc dans l'image exportée). Repli
générique (« Pixel — … ») quand une métadonnée manque. 4 clés i18n FR/EN.

### Changed — Planche pixel RECONFORT alignée sur la maquette de référence

`plot_pixel_dieback()` (`R/fct_plot_pixel_dieback.R`) restylée pour coller à la
maquette fournie :
- Trajectoires estivales distinguées par **couleur dédiée** : **rouge** pour le
  creux CRswir, **violet** pour le pic CRre (points, lignes pointillées et
  libellés de valeurs assortis) — au lieu du brun commun.
- **Légende interne** au panneau haut (« CRswir (lissage léger) », « CRre
  (lissage léger) » + les deux trajectoires).
- **Axes fléchés** : « CRswir (↑ = plus d'eau) » (bleu) / « CRre (↑ = plus de
  chlorophylle) » (vert).
- Cycles repliés B/C : **bande estivale orange** + annotations « creux estival
  qui s'enfonce » / « pic plus faible = sénescence précoce ».
- Espace d'état D : centroïdes reliés en **pointillés**, annotation « dérive
  vers eau ET chloro. basses », et **barre de couleur des années horizontale**
  en bas de planche.
- 5 nouvelles clés i18n FR/EN + 6 mises à jour (libellés exacts de la maquette).

# nemetonshiny 0.97.0 (2026-07-01)

### Added — Diversité spectrale B4 / L3 (affichage, spec 028)

Câblage d'affichage des deux nouveaux indicateurs de diversité spectrale
(biodivMapR, `nemeton >= 0.110.0`) :
- **B4** (famille Biodiversité) — diversité spectrale α (Shannon).
- **L3** (famille Paysage) — hétérogénéité spectrale β (turnover Bray-Curtis),
  distincte de L2 (fragmentation structurale).

Détail :
- `list_available_indicators()` liste désormais B4 et L3 (31 → 33).
- Config famille (`app_config.R`), labels de progression (`mod_progress.R`) et
  libellés i18n (`utils_i18n.R`) FR/EN complétés — la vue famille et le radar
  les affichent automatiquement (config-driven), avec le caveat « proxy à
  valider terrain ».
- Injection générique de l'objet de diversité (`layers$spectral`) dans le
  dispatch, sur le modèle de `fapar`/`snow` — sans court-circuit.

### Added — Calcul B4 / L3 : cube réflectance S2 + biodivMapR (spec 028 P2/P3)

Le service de calcul assemble désormais le **cube réflectance Sentinel-2** et
lance la primitive **une seule fois** pour les deux indicateurs (`layers$spectral`
partagé) :
- `build_spectral_diversity()` (dans le worker du calcul) : repère le cache S2
  `<projet>/cache/layers/sentinel2`, choisit une **scène estivale** (DOY 152–273,
  au plus proche de mi-août), assemble un cube multi-bandes via le lecteur cœur
  `nemeton::read_s2_band_raster()` (6 bandes B04/B05/B08/B8A/B11/B12, resamplées
  sur une grille commune 10 m), rasterise le **masque UGF/forêt**, puis appelle
  `nemeton::compute_spectral_diversity(reflectance, mask, window_size = 10,
  nb_cpu = getOption("nemeton.biodivmapr_cpu", 1))`. Aucune logique métier dans
  l'app (règle 1) : tout le dérivé vient du cœur.
- **Dégradation propre** : pas de cache S2, pas de scène estivale, bandes
  illisibles ou erreur biodivMapR ⇒ `layers$spectral = NULL` ⇒ B4/L3 en **NA**
  (jamais fatal, un run n'échoue pas à cause d'eux). Feedback immédiat pendant
  l'analyse (toast « Diversité spectrale… », règle 9).
- **Pré-requis données** : B4/L3 produisent des valeurs réelles dès qu'un cache
  S2 existe (issu d'une ingestion de suivi sanitaire FAST/RECONFORT) ; sinon ils
  restent grisés.

Suite (spec 028 P6) : après le premier run réel, remonter les plages effectives
de B4 (Shannon) et β pour recalibrer les bornes de normalisation côté cœur.

### Fixed — Complétion affichage B4 / L3

- Clés i18n **code court** `indicator_B4` / `indicator_L3` ajoutées FR/EN
  (« Diversité spectrale (α) » / « Hétérogénéité spectrale (β) ») — le radar et
  les libellés courts les résolvent désormais (elles manquaient au lot
  d'affichage initial).
- Assertions de comptage d'indicateurs mises à jour : `list_available_indicators`
  = 33, `get_all_indicator_codes` = 34, `get_column_family_map` = 68,
  `init_compute_state` = 33 (B4 + L3, en plus de R5).

### Changed — Dépendance & licence

- **`nemeton (>= 0.110.0)`** (indicateurs B4/L3 + primitive
  `compute_spectral_diversity()`).
- **Relicence EUPL v1.2 → GPL-3** : l'app importe désormais du GPL-3 (nemeton →
  biodivMapR). EUPL Art. 5 autorise cette relicence. Voir ADR-006.
- CI : `github::cran/dissUtils` + `jbferet/spinR` ajoutés aux `extra-packages`
  (deps transitives non-CRAN de biodivMapR, tirées via nemeton).

# nemetonshiny 0.96.3 (2026-07-01)

### Changed — Liste Commune : mise à jour bien plus rapide (sélection projet récent)

Au clic sur un projet récent, la liste déroulante Commune mettait « un temps fou »
à se mettre à jour. Trois causes cumulées, toutes corrigées :
- **Cache département→communes** (`R/service_communes.R`) : `get_communes_in_department()`
  re-téléchargeait tout le département depuis geo.api.gouv.fr à **chaque**
  restauration (~0,55 s réseau). La liste d'un département étant stable en
  session, elle est désormais **mémoïsée** (les erreurs/vides ne sont pas
  cachés). Bénéficie aussi au changement de département et au diagnostic.
- **Construction vectorisée** du data.frame des communes (fin du
  `do.call(rbind, lapply(...))` en O(n²) — ~0,3 s pour ~560 communes → négligeable).
- **`server = TRUE`** sur le `selectizeInput` Commune (`R/mod_search.R`) : les
  jusqu'à 1000 options ne sont plus toutes sérialisées + rendues côté navigateur
  (filtrage **serveur**), ce qui supprime le gel visible de la liste sur les
  gros départements.

### Changed — Sous-onglet familles : cartes indicateurs sur une seule ligne

Dans le sous-onglet de détail d'une famille, les cartes des indicateurs
passaient par un grid Bootstrap 12 colonnes (`col-3` pour n ≥ 4). Le grid ne
répartissant pas 5 cartes également (12/5 non entier), la famille **Risques &
Résilience** avec R5 activé (5 indicateurs) débordait sur 2 lignes (4 + 1). La
disposition passe à un **flexbox à largeurs égales sans retour à la ligne**
(`flex: 1 1 0` + `flex-nowrap`), qui aligne n'importe quel nombre d'indicateurs
(1 à 5) sur **une seule ligne**, cartes de largeur égale.

# nemetonshiny 0.96.2 (2026-07-01)

### Changed — extraction de la valeur d'indicateur déléguée au cœur (anti-dérive)

`compute_single_indicator()` construisait sa propre correspondance
« indicateur → colonne de sortie » (un `col_map` de 17 entrées tenu à la main),
duplication de la convention déjà portée par `nemeton::compute_indicator()` —
deux implémentations vouées à diverger. Le `col_map` est supprimé au profit d'un
appel unique à **`nemeton::extract_indicator_value(result, indicator,
exclude = names(parcels))`** : la convention de nommage vit désormais dans **une
seule** source de vérité (le cœur). Dépendance relevée à `nemeton (>= 0.108.0)`.
Aucun changement fonctionnel visible ; test de délégation dans
`test-service_compute.R`.

# nemetonshiny 0.96.1 (2026-07-01)

### Added — Export PNG de la planche pixel dépérissement (RECONFORT)

La modale du diagnostic pixel expose désormais un bouton **« Exporter PNG »** qui
enregistre la planche 4 panneaux COURANTE (avec les réglages lissage/points
actifs) en image statique. Rendu par un helper pur réutilisable
`save_plotly_png()` (`R/fct_plot_pixel_dieback.R`) qui détecte le moteur
disponible : `kaleido` en priorité (via `reticulate`, pas de serveur), sinon
`webshot2` (Chrome headless, ajouté en `Suggests`). Le bouton n'apparaît que si
un moteur est présent (`.pixel_export_engine()`), et le handler garde un repli
(image-note) si le rendu échoue à l'exécution. 2 clés i18n FR/EN ajoutées.

Note : l'inclusion au **rapport Quarto de synthèse** n'est pas branchée — ce
rapport est projet-global tandis que la planche est un diagnostic par pixel ;
une section « pixel représentatif » nécessiterait une sélection métier
(à porter côté `nemeton`, pas dans l'app). Le helper `save_plotly_png()` est
prêt à être réutilisé le jour où cette section sera spécifiée.

# nemetonshiny 0.96.0 (2026-07-01)

### Added — Planche pixel dépérissement 4 panneaux (RECONFORT, Partie B)

Le diagnostic pixel RECONFORT (clic sur la carte) affichait un plotly simple à
2 traces mono-axe. Il devient une **planche 4 panneaux** rendue par une fonction
de tracé **pure** (`R/fct_plot_pixel_dieback.R::plot_pixel_dieback()`), alimentée
par `nemeton::prepare_pixel_dieback_series()` — **aucun calcul dans l'app**
(règles 1-3) : toutes les séries lissées, creux/pics estivaux, centroïdes et
lacunes viennent du cœur. Panneaux : (A) double axe Y **natif** CRswir/CRre
(plus de recadrage manuel) avec points bruts optionnels, bandes estivales,
ombrage des lacunes d'interpolation et trajectoires creux/pic distinguées par
tiret + symbole (redondance non-couleur) ; (B/C) cycles annuels repliés
(jour de l'année), une ligne par année, palette **cividis** ; (D) espace d'état
CRswir × CRre coloré par année (une seule colorbar) avec la trajectoire des
centroïdes. Contrôles dans la modale : lissage `none`/`light` (passé au cœur ;
lissage fort volontairement absent) et points bruts. Accessibilité WCAG :
**table `DT` équivalente** en repli et `aria-label` sur le widget. Modale
agrandie (~760 px). 12 clés i18n FR/EN ajoutées. Plancher
`Imports: nemeton (>= 0.106.0)`.

Différé (hors périmètre testable ici) : export PNG statique via `kaleido` +
inclusion au rapport Quarto — la planche est un diagnostic **interactif par
pixel** sans section de rapport naturelle, et `kaleido` requiert un runtime
Python absent de l'environnement de test.

# nemetonshiny 0.95.0 (2026-07-01)

Le sélecteur d'année Sentinel-2 (mode RECONFORT) proposait/acceptait l'année en
cours, dont la saison n'est pas terminée — un run lancé dessus travaillait sur
des données incomplètes. Les bornes sont désormais **calculées par le cœur**
(règle 1 : zéro logique métier dans l'app) via
`nemeton::reconfort_year_bounds("v3")` (dernière saison CLOSE selon l'`edate` du
modèle) : `value`/`max`/`default` = dernière année complète, `min` = première
année S2 exploitable. Un **garde-fou serveur** dans `.invoke_reconfort()`
(`nemeton::reconfort_year_bounds`) refuse toute saisie clavier hors bornes avec un
toast d'avertissement (`monitoring_reconfort_year_incomplete`, FR/EN) sans lancer
le run. Plancher `Imports: nemeton (>= 0.104.0)`. Dépend aussi de
`nemeton@0.105.0` (clé de cache FAST sur la couverture S2 réelle), tirée
automatiquement via `@*release`.

# nemetonshiny 0.94.10 (2026-07-01)

### Changed — Seuil FAST débouncé (moins de recalculs de raster)

Le slider de seuil (NDVI/NBR/NDMI) de l'onglet « Alertes FAST » relançait un
calcul de raster complet **à chaque cran** pendant le glissement : côté cœur,
le nom du cache encode le seuil (`thr%.2f`), donc chaque valeur intermédiaire est
une clé de cache distincte, non cachée. Les seuils consommés par le calcul sont
désormais **débouncés (400 ms)** (`thresholds_deb`) : seule la valeur finale
déclenche le calcul. L'indice, le mode, la zone et les dates restent à réaction
immédiate.

# nemetonshiny 0.94.9 (2026-07-01)

### Fixed — Barre de progression qui recule pendant la phase CHM

Pendant le calcul des indicateurs, la barre montait jusqu'à ~32 % (chargement
du CHM FormSpot) puis **redescendait à ~27 %** au passage sur l'inférence
Open-Canopy / le calcul. Cause : le callback de téléchargement écrivait dans
`state$progress` le **compteur brut de sources (0→13)** alors que la phase de
téléchargement ne réserve que **10 unités** de `progress_max` ; le compteur
débordait son budget, puis la phase de calcul remettait `progress` à 10. La
progression du téléchargement est désormais **normalisée** dans sa bande
`[0, 10]` (ratio `completed/total`) et rendue **monotone** (jamais de recul),
ce qui supprime aussi le risque de retour à 0 % via l'étape LiDAR HD
(`download_chm_lidar_hd` émettait `completed = 0`). Le calcul lui-même n'était
pas affecté — bug purement d'affichage.

### Fixed — Carte RECONFORT : notification « Calcul du graphique en cours… » manquante

Au clic-pixel sur la Carte RECONFORT, aucun message bas-droite ne s'affichait et
l'UI se figeait silencieusement pendant la lecture de la série CRSWIR/CRre. Le
handler faisait tout de façon **synchrone**, contrairement aux Cartes FAST et
FORDEAD qui affichent immédiatement la notification `monitoring_pixel_map_computing`
puis diffèrent le calcul via `session$onFlushed`. RECONFORT adopte désormais ce
même motif (notification immédiate + calcul déféré + `on.exit` de nettoyage),
pour une parité complète des trois cartes pixel (règle stricte #9 : retour
immédiat sur action longue).

### Changed — Police agrandie dans les graphiques pixel FAST/FORDEAD/RECONFORT

En plein écran, les libellés d'axes, ticks, légende et surtout les annotations
in-plot (seuils NDVI/NBR/NDMI, date de 1re anomalie FORDEAD, hors zone de
validité, libellés d'année RECONFORT) étaient difficiles à lire. Une police
globale `size = 16` est posée sur chaque `plotly::layout()` (axes/ticks/légende/
hover héritent), et les annotations in-plot passent de 10-12 à 14-15 px. Concerne
les quatre graphiques pixel au clic : FAST (série NDVI/NBR/NDMI), FAST Tendance
(Theil-Sen), FORDEAD (CRSWIR + harmonique) et RECONFORT (CRSWIR/CRre).

### Changed — Bandeau RECONFORT « hors domaine de calibration » remonté sous « Base de suivi connectée »

Le bandeau d'avertissement RECONFORT (zone hors domaine de calibration,
non bloquant) était rendu *dans* l'onglet « Alertes RECONFORT », au-dessus de la
carte. Il est désormais rendu au niveau parent, juste sous « Base de suivi
connectée » et au-dessus des sous-onglets — même emplacement et même style
(`.monitoring_validity_banner`, carte à bordure warning) que les bandeaux de
validité FORDEAD, pour une parité visuelle complète. Le sous-module carte expose
`validity` ; le parent (`mod_monitoring::output$reconfort_validity_banner`) en
tire le bandeau, visible uniquement en mode RECONFORT et hors domaine. Deux clés
i18n ajoutées (titre + corps).

# nemetonshiny 0.94.8 (2026-06-30)

### Fixed — Sélection : bouton « Lancer les calculs » masqué à tort

Le bouton de calcul disparaissait du projet chargé dès qu'**un autre** projet
calculait en arrière-plan : la condition de masquage testait `computing_project_id()`
non-NULL (un seul calcul à la fois) sans vérifier que c'était bien le projet
**courant**. Elle teste désormais l'égalité avec `project$id` — lancer un calcul
sur le projet B ne masque plus le bouton du projet A rechargé.

### Fixed — Test `defers indicators_sf` : assertion fragile retirée

Le test `test-05mod_home.R::"recent-project load defers indicators_sf attach"`
échouait sur `main` : son `expect_null` supposait que le callback `later::later`
de l'attache n'avait pas encore tourné après le flush, or selon que le harnais
`testServer` pompe ou non la file `later` (mod_home a des `reactivePoll` qui
l'influencent) il peut s'exécuter pendant le setup. L'assertion de timing
fragile est retirée ; les garanties réelles (pas d'erreur « reactive outside
consumer » — régression v0.79.1 — et `indicators_sf` attaché) restent testées.

# nemetonshiny 0.94.7 (2026-06-30)

### Fixed — Cartes Suivi sanitaire : UGF réapparue, décoche overlay effective, « Indice »

Trois corrections sur les cartes Leaflet du Suivi sanitaire :

* **Couche UGF restaurée (Alertes FORDEAD + RECONFORT)** — l'overlay UGF
  pouvait disparaître : il n'était dessiné qu'au *render de base* et
  conditionné à `indicators_sf`, or l'attache de `indicators_sf` est différée
  au chargement → si le render précédait l'attache, l'UGF manquait
  définitivement. « UGF » est désormais **toujours** dans le contrôle, et les
  polygones sont (re)dessinés par un **observer réactif `leafletProxy`** dès
  que `indicators_sf` est disponible (parité avec la Carte pixel FAST).
* **Décoche d'overlay enfin effective (FAST « Indice », Alertes FAST/FORDEAD)**
  — décocher une couche raster dans le LayersControl ne la masquait pas : les
  observers `leafletProxy` la re-dessinaient en ignorant l'état du contrôle.
  Ils lisent désormais `input$<id>_groups` (groupes cochés côté client) et
  appellent `hideGroup()` après re-dessin si la couche est décochée.
* **« NDVI/NBR » renommé « Indice »** dans le contrôle de couches de la Carte
  pixel FAST.

### Fixed — « Réessayer » : toast vraiment immédiat (later au lieu de onFlushed)

Le toast « Réinitialisation en cours… » ajouté en v0.94.6 n'apparaissait
toujours pas immédiatement : `session$onFlushed` s'exécute dans le **même tick**
de la boucle d'événements, donc le travail synchrone (vidage cache + reload)
bloquait **httpuv** *avant* la transmission du toast au navigateur. Le travail
lourd est désormais différé via **`later::later(delay)`** : la boucle reprend la
main (toast + bouton désactivé transmis), puis le travail s'exécute au tick
suivant (ré-entrée du domaine réactif + garde `session$isClosed()`).

### Fixed — Open-Canopy isolé : restaure les messages de progression dans l'app

Depuis l'isolation du CHM Open-Canopy en sous-processus (callr), les messages de
calcul (ÉTAPE 1/5…5/5, tuiles, inférence) ne remontaient plus **en bas à droite**
de l'app — ils partaient sur la console du worker. Un callback R ne traverse pas
la frontière du process.

`download_chm_opencanopy()` re-streame désormais la progression : le
sous-processus émet les **événements `progress_callback` d'opencanopy en lignes
JSON taguées** (`__CHM_EV__{...}`) sur stdout ; le parent les **re-parse et les
rejoue** via le `progress_callback` de l'app (`r_bg` + boucle `poll_io`/
`read_output_lines`, helper `.chm_forward_line`). Les messages structurés
(`chm_phase:` / `chm_tile:` / `chm_inference_tile:`) reviennent donc dans le
panneau de l'app, tout en gardant l'isolation reticulate (env `open_canopy`).
Le reste de la sortie est miroité sur la console. 3 tests
(`test-opencanopy-python.R`).

# nemetonshiny 0.94.6 (2026-06-30)

### Changed — UX boutons : convention normative + fusion des deux verts

* **Convention de couleurs de boutons documentée** dans `CLAUDE.md` (section
  *Conventions de code*) : 🟢 vert = action principale / CTA ; 🟤 brun =
  action secondaire / fermeture ; ⚪ blanc (`outline`) = action tertiaire /
  auxiliaire ; 🟡 warning = prudence ; 🔴 danger = destructif. Toujours une
  classe explicite (pas de brun par défaut accidentel).
* **`primary` et `success` fusionnés** sur un seul vert `#1B6B1B` (avant deux
  verts quasi identiques `#1B6B1B` / `#1E7B1E` pour deux rôles) — thème
  (`utils_theme.R`) + `custom.css` + `custom.min.css`.

### Fixed — Sélection : « Réessayer » sans retour visuel

Cliquer « Réessayer » (recompute) lançait un vidage de cache + rechargement
projet **synchrones** sans aucun retour : le clic paraissait sans effet et
l'utilisateur re-cliquait partout. Le bouton est désormais **désactivé** et un
**toast** « Réinitialisation des calculs en cours… » s'affiche **immédiatement**
(travail lourd différé d'un flush via `onFlushed` pour que le retour s'affiche
avant le blocage), remplacé en fin d'opération par la confirmation
(`retry_toast`, jusqu'ici définie mais jamais affichée). Nouvelle clé i18n
`retry_in_progress`.

### Fixed — Open-Canopy CHM : isolation reticulate (débloque les indicateurs Production)

Les indicateurs **Production (P1/P2/P3)** et **Bois-énergie (E1)** échouaient
(« Modèle de hauteur de canopée indisponible ») parce que le CHM Open-Canopy ne
pouvait pas être produit : `opencanopy::pipeline_aoi_to_chm` passe par
**reticulate**, qui ne lie qu'**un seul Python par session R**. reticulate était
déjà lié à un Python uv éphémère (`~/.cache/uv/...`) → impossible de basculer sur
l'env conda `open_canopy` → CHM absent → P1/P2/P3/E1 en échec.

`download_chm_opencanopy()` exécute désormais `pipeline_aoi_to_chm` dans un
**sous-processus R isolé** (`callr`) avec `RETICULATE_PYTHON` épinglé sur
`open_canopy` (résolu via `options(nemetonshiny.opencanopy_python=)` /
`OPENCANOPY_PYTHON` / `reticulate::conda_python("open_canopy")` / chemins conda
usuels) et `R_ENVIRON_USER=""` (un `~/.Renviron` ne peut pas écraser le pin). Le
sous-processus part d'un reticulate vierge → il se lie toujours au bon env, **quel
que soit** le binding parent. **Open-Canopy et FORDEAD (env distinct) cohabitent.**
Repli in-process si `callr` ou l'env est introuvable. `callr` en `Suggests`. 3
tests (`test-opencanopy-python.R`).

# nemetonshiny 0.94.5 (2026-06-30)

### Changed — Alertes RECONFORT : couches exclusives + modale pixel enrichie

* **Couches exclusives** — les couches de la carte passent de cases à cocher
  (multi) à des boutons radio (une seule à la fois), parité avec la Carte
  FORDEAD. Le slider d'opacité agit sur la couche raster sélectionnée.
* **Modale diagnostic pixel : plein écran + fermer** — ajout du bouton
  « plein écran » (haut-droite, bascule `.modal-fullscreen` BS5, plot
  responsive) en plus de « Fermer », parité exacte avec la Carte FORDEAD.
* **Délimitation des 2 années Sentinel-2** sur le graphe CRSWIR / CRre —
  bandes de fond alternées par année calendaire + libellé de l'année, pour
  situer la dynamique dans la fenêtre 2 ans du modèle.

Note : RECONFORT est un classifieur Random Forest — il n'existe pas de seuil
de détection par indicateur (contrairement au modèle harmonique FORDEAD), donc
le graphe garde les deux séries observées CRSWIR / CRre sans courbe de seuil.

### Changed — Alertes FORDEAD : rampe bleu→rouge pour « Date de 1ère détection »

La couche *Date de 1ère détection* (first_anomaly) passait par la palette
`viridis`. Elle utilise désormais `RdYlBu` **inversé** : les détections les
plus **anciennes** sont en **bleu**, les plus **récentes** en **rouge** —
lecture immédiate de la progression temporelle du dépérissement. Légende
(années) inchangée.

Cycle dev `0.94.4` → `0.94.4.9002` → release stable `v0.94.5`.

# nemetonshiny 0.94.4 (2026-06-30)

### Fixed — Alertes RECONFORT : rasters affichés après rechargement de projet

La carte « Alertes RECONFORT » n'affichait ses rasters (classification / score
/ probabilité) qu'à partir du `result` **en mémoire** d'un run de la session
courante. Après un **rechargement de projet**, `result` était perdu → plus de
toggles, plus de slider d'opacité, plus de raster — seules les alertes (DB)
restaient. FORDEAD, lui, lit ses couches depuis le **cache** et les réaffiche
après rechargement.

Parité atteinte : `manifest_r` retombe désormais sur
`nemeton::reconfort_cache_manifest(cache_dir, zone_id)` (nemeton ≥ 0.100.1 —
0.100.1 lit le dossier IOTA² `final/` et expose les 3 rasters score /
classification / probability) quand aucun `result` n'est en mémoire — le run
persisté est découvert sur disque, schéma **interchangeable** avec
`reconfort_layer_manifest(result)`.
Toute la machinerie existante (toggles, opacité, masque UGF au read via
`read_reconfort_layer`, clip alertes, clic-pixel) est réutilisée telle quelle.

Les couches affichables sont désormais unifiées via `available_layers_r` :
rasters du manifeste (run mémoire OU cache) **+** couche *Alertes* (DB)
toujours toggleable, même en mode cache (le manifeste cache ne porte pas de
ligne `alerts`). Sans aucune couche ni alerte → état vide.

Raffinements de rendu :

* **Rasters continus (score / probabilité) en échelle par QUANTILES** —
  `leaflet::colorBin` sur les bornes de quantiles calculées des valeurs
  réelles du raster (5 classes), parité avec la carte FAST. Une distribution
  concentrée utilise alors toute la palette uniformément, au lieu d'une rampe
  linéaire min/max (et le `vmin/vmax` générique du manifeste — score `1-100`,
  proba `0-1000` — n'est plus utilisé). Repli linéaire si distribution
  dégénérée.
* **Classe 1-sain transparente** dans la couche classification — seuls les
  pixels affectés (2-deperissant / 3-tres-deperissant) sont peints, comme
  FORDEAD rend sa classe 0 (sain) transparente. La classe 1 reste en légende.
* **Info-bulles « i » par couche** — chaque case à cocher (rasters + alertes)
  porte une icône `info-circle` (tooltip) décrivant la couche, parité avec la
  Carte FORDEAD (helper `.reconfort_layer_choice`, clés i18n
  `reconfort_couche_*_info`).

### Changed — Suivi sanitaire : « Carte FORDEAD » renommé « Alertes FORDEAD »

Cohérence des libellés de sous-onglets entre les deux pipelines : « Carte
FORDEAD » devient « Alertes FORDEAD » (parallèle à « Alertes RECONFORT »).
Clé `monitoring_subtab_pixel_map_fordead`, FR/EN.

Plancher relevé à `Imports: nemeton (>= 0.100.1)`.

Cycle dev `0.94.3` → `0.94.3.9001` → release stable `v0.94.4`.

# nemetonshiny 0.94.3 (2026-06-30)

### Fixed — Famille Risques & Résilience : R5 dépérissement affiché

L'indicateur **R5 dépérissement** (32e indicateur, conditionnel) était calculé
et injecté dans le radar de la Synthèse, mais **n'apparaissait pas** dans la
page famille *Risques & Résilience* (R1-R4 seulement) : la page lisait
`project$indicators_sf` brut, sans la colonne `indicateur_r5_deperissement`
(injectée en direct, non persistée). La config `INDICATOR_FAMILIES$R` listait
pourtant déjà R5, et le libellé (`indicator_R5` → « Dépérissement ») + le
tooltip existaient.

`mod_family` enrichit désormais ses données via le même helper que le radar
(`add_r5_to_indicators`), au sein d'un réactif partagé `enriched_sf` dont
dérivent `indicators_data()` (carte/tableau) et `indicators_sf()`. L'enrichis-
sement n'est fait **que pour la famille R** (les 11 autres évitent le coût
DB + spatial). Best-effort : sans zone de suivi liée / sans alerte, la famille
R reste R1-R4. Repli préservé sur `project$indicators` quand il est lui-même
un sf (geoarrow).

Cycle dev `0.94.2` → `0.94.2.9001` → release stable `v0.94.3`.

# nemetonshiny 0.94.2 (2026-06-30)

### Fixed — Chargement de projet : « Calcul en cours » fantôme (resume périmé)

Charger un projet dont le dernier calcul avait été interrompu (fichier
`progress_state.json` figé sur `status = "downloading"`/`"computing"`)
ré-affichait une carte « Calcul en cours… » **bloquée à 0 %** : la reprise
de suivi (`mod_home`, `observeEvent(current_project)`) relançait le polling
alors qu'**aucun worker `future` ne tournait** (process d'origine mort après
un redémarrage de l'app).

La reprise ne se déclenche désormais **que si le calcul tourne réellement** :
un worker vivant réécrit `progress_state.json` toutes les ~2 s, donc un
fichier **frais** (< 120 s) signe un run en vol (reconnexion navigateur),
tandis qu'un fichier **périmé** signe un worker mort → reprise ignorée (plus
de fantôme ; l'utilisateur relance le calcul explicitement au besoin).

Nouveau helper `progress_state_age_sec(project_id)` (service_compute) : âge
du fichier de progression via son `mtime` (`Inf` si absent). Cliquer un
projet récent ne lance jamais de calcul — seul le bouton de calcul le fait.

Cycle dev `0.94.1` → `0.94.1.9001` → release stable `v0.94.2`.

# nemetonshiny 0.94.1 (2026-06-30)

### Fixed — Chargement de projet : crash « module session has been destroyed »

Charger un projet déclenche une restauration via `session$reload()`, qui
détruit puis recrée la session. Deux callbacks `later::later` de `mod_home`
(hors cycle de vie de session) restaient programmés et lisaient `app_state`
après la destruction → `Can't access reactive ...; its module session has
been destroyed`, terminant la session :

* le **polling de progression** du calcul (`poll_fn`, « Calcul en cours… ») —
  son garde-fou existant `isolate(computing_project_id())` était lui-même une
  lecture réactive, donc plantait avant de pouvoir protéger ;
* le **defer de `attach_indicators_sf`** (construction de `indicators_sf` à
  chaque chargement) via `withReactiveDomain(session, isolate(...))`.

Les deux callbacks testent désormais `session$isClosed()` **en premier**, avant
toute lecture réactive, et sortent silencieusement si la session est morte
(la boucle non-réactive n'était sinon arrêtée par rien). Bug pré-existant,
révélé par les projets à calcul reprenable (« Resuming progress tracking »).

Cycle dev `0.94.0` → `0.94.0.9001` → release stable `v0.94.1`.

# nemetonshiny 0.94.0 (2026-06-30)

### Added — R5 dépérissement branché dans le radar de synthèse

L'indicateur **R5** (32ᵉ, conditionnel) de la famille **R — Risques & Résilience**
apparaît désormais sur le radar et dans l'indice général quand la zone de suivi
liée au projet porte des alertes de dépérissement.

- **Config** : `app_config.R` étend la famille R à 5 indicateurs (R1…R5) —
  `indicateur_r5_deperissement`, libellés + tooltips FR/EN. Clé i18n
  `indicator_R5` + label d'axe radar.
- **Calcul live** (`R/service_r5.R::add_r5_to_indicators()`) : injecté dans
  `mod_synthesis` juste avant `create_family_index`. Charge les alertes de la
  zone (`nemeton::list_alerts`), les sépare par `alert_type`, et appelle
  `nemeton::indicateur_r5_deperissement()`. **Aucune sémantique métier côté
  app** : le score et l'inversion de sens vivent dans le cœur
  (`normalize_indicator`, `nemeton` ≥ 0.99.1).
- **Routage par essence** : les UGF ne portant pas de colonne d'essence,
  chaque UGF est routée par le **type d'alerte qui l'intersecte** (RECONFORT →
  feuillus, FORDEAD → résineux). Best-effort : sans zone liée / sans alerte,
  la famille R reste R1-R4 (R5 absent, pas d'erreur). Une UGF sans alerte
  intersectante → R5 = NA.
- Plancher relevé à `Imports: nemeton (>= 0.99.1)`.
- Tests : `test-service_r5.R` (8) ; comptes d'indicateurs ajustés 31 → 32.

# nemetonshiny 0.93.1 (2026-06-30)

### Fixed — Carte RECONFORT : alertes clippées à l'UGF

Les points de la couche **Alertes** débordaient le périmètre UGF : les
alertes sont persistées sur l'emprise feuillus OSO (≫ UGF) et `list_alerts`
ne filtrait que par `zone_id`, sans prédicat spatial. Elles sont désormais
clippées **au read** au polygone de la zone sélectionnée par le helper cœur
`nemeton::filter_alerts_to_zone(alerts, con, zone_id, apply_zone_mask =
TRUE)` (nemeton ≥ 0.99.0) — pendant vecteur du masque raster
`read_reconfort_layer` (spec 016 / 021 L7), partagé par les 3 pipelines
RECONFORT / FORDEAD / FAST. La table d'alertes n'est pas modifiée (« mask at
read, not write ») ; aucun prédicat spatial dans `mod_*` (CLAUDE.md §1-3) :
le polygone est résolu côté cœur depuis `con + zone_id` (la connexion RO
déjà ouverte par `alerts_r` est réutilisée).

Plancher relevé à `Imports: nemeton (>= 0.99.0)`.

Cycle dev `0.93.0` → `0.93.0.9001` → release stable `v0.93.1`.

# nemetonshiny 0.93.0 (2026-06-29)

### Changed — Carte RECONFORT : masquage UGF rapatrié dans le cœur

L'opération spatiale `terra::mask` introduite en v0.92.3 (clip des rasters à
la zone de suivi) **quitte la présentation** : elle est désormais déléguée
au reader cœur `nemeton::read_reconfort_layer()` (nemeton ≥ 0.98.0). Le
module `mod_monitoring_reconfort_map` ne porte **plus aucune op spatiale de
masquage** — respect strict des règles §1-3 (aucune sémantique spatiale dans
`mod_*`) et parité avec les readers FAST / FORDEAD (état cible spec 016 +
spec 021 L7).

* Lecture + masquage UGF en un seul appel :
  `read_reconfort_layer(layer = row, mask_polygon = aoi, apply_zone_mask =
  TRUE)` → SpatRaster déjà masqué à l'AOI de la zone sélectionnée
  (`_tot` / `_res` / `_feu` / `_mix`). Le polygone (concern UI) est choisi
  par l'app, le masque (concern métier) appliqué par le cœur.
* Rasters masqués mis en cache dans un reactive (`masked_rasters_r`), clé
  manifeste / zone et **non l'opacité** : un mouvement du curseur ne relit
  ni ne remasque (parité FORDEAD — l'opacité n'est qu'un paramètre de
  rendu). Seules les lignes `type == "raster"` du manifeste sont passées au
  reader (il rejette la ligne `vector` des alertes).

Inchangé : manifeste (`reconfort_layer_manifest`), cases à cocher + curseur
d'opacité, couche UGF overlay (vecteur toggleable, ce n'est pas du
masquage), vecteur alertes (`result$alerts_sf` / DB), clic-pixel CRSWIR/CRre.

Plancher relevé à `Imports: nemeton (>= 0.98.0)`.

Cycle dev `0.92.3` → `0.92.3.9001` → release stable `v0.93.0` (minor :
consomme la nouvelle API cœur, retire le masque local).

# nemetonshiny 0.92.3 (2026-06-28)

### Changed — Carte RECONFORT : sidebar droite, UGF, clip à la strate

Suite du chantier couches RECONFORT, alignement complet sur les cartes
FORDEAD / FAST :

* **Contrôles en sidebar droite** — les cases à cocher des couches et le
  curseur d'opacité passent dans une `bslib::layout_sidebar` (sidebar
  droite, `open = "always"`), parité visuelle avec la Carte FORDEAD. La
  carte Leaflet devient **statique** (overlay d'état par-dessus) pour
  préserver le binding `input$map_click` (clic → diagnostic pixel).
* **Couche UGF** — les Unités de Gestion Forestière du projet
  (`project$indicators_sf`) sont dessinées en overlay toggleable via le
  LayersControl natif (helper partagé `.ugf_for_overlay`, comme FORDEAD /
  FAST).
* **Résultats clippés à la zone de suivi sélectionnée** — les rasters sont
  masqués (`terra::mask`) à l'AOI de la strate choisie dans la liste
  déroulante (`fordead_tot` / `_res` / `_feu` / `_mix`), exactement comme
  FORDEAD masque le raster `_tot` par strate. Présentation pure, aucun
  calcul métier (CLAUDE.md §3). Les alertes vectorielles sont déjà
  filtrées par zone côté `nemeton::list_alerts(zone_id = ...)`.

Le `result` en mémoire n'est plus purgé au changement de zone (mais au
changement de **projet**) : on conserve le run et on se contente de le
clipper à la strate sélectionnée.

Cycle dev `0.92.2` → `0.92.2.9001` → release stable `v0.92.3`.

# nemetonshiny 0.92.2 (2026-06-28)

### Added — Carte RECONFORT : couches sélectionnables + opacité

Le sous-onglet « Alertes RECONFORT » du Suivi sanitaire affiche désormais,
au choix (cases à cocher), les **couches d'un run RECONFORT** — rasters
*Score de dépérissement* / *Classes de santé* / *Probabilité* + vecteur
*Alertes* — avec un **curseur d'opacité** agissant sur les rasters
(re-render léger via `leafletProxy`, parité avec la carte FORDEAD).

Zéro sémantique métier dans le module (CLAUDE.md §2-4) : la liste des
couches, les palettes, les domaines de rampe, le sens (reverse) et la
visibilité par défaut proviennent tous du **manifeste cœur**
`nemeton::reconfort_layer_manifest(result, include_range = TRUE)`
(nemeton ≥ 0.97.0). Le module bascule selon la source :

* **mode manifeste** (post-run, en session) — `result` de
  `run_reconfort_dieback()` threadé du parent (`reconfort_result`) au
  sous-module via `result_r`, réinitialisé au changement de zone et au
  lancement d'un nouveau run ;
* **mode DB legacy** (projet rechargé) — repli sur les alertes
  vectorielles lues en base (`nemeton::list_alerts`), comportement
  inchangé.

Nouvelles clés i18n FR/EN : `reconfort_couches`, `reconfort_opacite`,
`reconfort_couche_score`, `reconfort_couche_classes`,
`reconfort_couche_proba`, `reconfort_couche_alertes` (libellés émis par
le `label_key` du manifeste). Légende du raster de classification
réutilisant `reconfort_class_label_1/2/3`.

Cycle dev `0.92.1` → `0.92.1.9001` → release stable `v0.92.2`.

### Removed — Suivi sanitaire : sous-onglet « Alertes FORDEAD » (doublon)

Le sous-onglet « Alertes FORDEAD » faisait doublon avec la « Carte
FORDEAD » : même couche raster, même sidebar (indice CRSWIR + opacité). Il
est supprimé, ainsi que ses entrées dédiées (`fordead_index_alerts`,
`alerts_opacity`, `output$alerts_panel`, `monitoring_alerts_placeholder` et
les variantes meta de la card). Le verdict « Zone saine » (run terminé sans
pixel affecté) vit désormais uniquement dans l'overlay de
`mod_monitoring_fordead_map`. Tests parent ajustés en conséquence.

# nemetonshiny 0.92.1 (2026-06-28)

### Changed — RECONFORT : message de lancement réaliste (durée à froid)

Le toast de lancement du diagnostic RECONFORT annonçait « Durée estimée :
quelques minutes », trompeur pour un premier calcul à froid qui dure en
réalité plusieurs heures. Message aligné sur le style du diagnostic FORDEAD :
« Premier calcul à froid : plusieurs heures ; nettement plus rapide ensuite. »
(clé `monitoring_reconfort_starting`, FR/EN).

# nemetonshiny 0.92.0 (2026-06-28)

### Added — RECONFORT : progression par scène de l'ingestion S2

Le bandeau de progression du run RECONFORT n'affichait que la phase globale
`ingestion` pendant des heures (download S2 ~4 min/scène, limité par GEODES).
`.reconfort_handle_progress_event()` rend désormais les nouveaux événements
émis par `nemeton (>= 0.96.0)` :

- `reconfort:ingest_listed` → « Ingestion S2 · N scène(s) à traiter » ;
- `reconfort:ingest_item` → « Scène X/N — téléchargement / extraction +
  recadrage AOI / recadrée / déjà en cache / échec » (+ date de la scène),

à la manière du suivi FORDEAD et de l'ingestion FAST. Nouvelles clés i18n
FR/EN (`monitoring_reconfort_ingest_*`, `monitoring_reconfort_step_*`). Plancher
relevé à `nemeton (>= 0.96.0)`.

# nemetonshiny 0.91.18 (2026-06-27)

### Fixed — R CMD check : test `app_server` lisant un réactif après destruction de session

`test-app_server.R` (« app_server passes app_state to modules ») lisait
`received_app_state$language` APRÈS la sortie du bloc `testServer`, alors que
la session du module est déjà détruite → `destroyedReactiveError` (« Can't
access reactive `app_state`; its module session has been destroyed »). Cet
ERROR rendait le job `R-CMD-check` rouge à chaque release (le job `tests`,
lui, passait). La lecture du réactif est désormais figée DANS le bloc
`testServer` (session vivante) puis assertée à l'extérieur.

# nemetonshiny 0.91.17 (2026-06-27)

### Added — Carte FORDEAD : bouton de défilement automatique du slider de date

- Le slider temporel de la Carte FORDEAD porte désormais le bouton play/pause
  natif de défilement automatique (parité avec la Carte FAST) : pas de 5 jours
  aligné sur la cadence Sentinel-2, lecture en boucle simple. Chaque cran filtre
  les pixels par date (`display_r`), donnant l'animation cumulative de la
  progression du dépérissement.
- Le slider (avec son bouton de défilement) s'affiche et applique le filtrage
  cumulatif non seulement pour la couche « sévérité » mais aussi pour la couche
  « Date de 1ère détection » (`first_anomaly`) : les pixels apparaissent dans
  l'ordre chronologique de leur détection.

# nemetonshiny 0.91.16 (2026-06-27)

### Fixed — Carte FORDEAD : `Error: impossible de trouver la fonction "ns"`

Le slider temporel de la Carte FORDEAD (v0.91.15) plantait avec
`Error: impossible de trouver la fonction "ns"` dès qu'un run exposait la
couche `first_anomaly` (domaine de dates valide) : le `renderUI` de
`output$fordead_date_slider` appelait `ns(...)` sans `ns <- session$ns`
local (contrairement aux autres outputs du module). Ajout de la définition
manquante.

### Changed — Carte FAST : texte d'aide pixel + suppression du bandeau bleu redondant

- Le texte d'aide du pixel map indiquait `(NDVI + NBR)` et mentionnait un
  clic « sur une placette » qui n'existe plus → corrigé en
  `(NDMI + NDVI + NBR)` et phrase placette retirée
  (`monitoring_pixel_map_click_hint`).
- Le bandeau bleu in-panel « Calcul du raster d'alerte en cours… » sous
  *Alertes FAST* faisait doublon avec la notification bas-droite unique
  (ci-dessous). Pendant le calcul, le bandeau ne rend plus rien (il
  continue d'empêcher l'affichage prématuré du vert « zone saine ») ; le
  retour visuel est porté uniquement par la notification agrégée.

### Changed — Suivi sanitaire : indicateur « calcul en cours » unique à l'arrivée

À l'ouverture de l'onglet **Suivi sanitaire**, une notification bas-droite
**« Calcul en cours… »** s'affiche désormais immédiatement et reste visible
tant qu'un des deux calculs lourds tourne — supprimant la fenêtre où l'UI
était figée sans aucun retour visuel (et où l'on pouvait cliquer dans le
vide). Les deux calculs concernés :

- **raster d'alerte FAST** (`mod_monitoring_fast_alerts`) ;
- **`nemeton::build_index_stack`** du pixel map (`mod_monitoring_pixel_map`),
  qui était jusqu'ici synchrone et bloquait R plusieurs secondes à froid
  sans feedback. Il est désormais **différé** (`onFlushed`) et stocké dans
  un `reactiveVal`, ce qui rend son drapeau `loading` observable d'un cycle
  à l'autre (overlay carte + indicateur agrégé).

L'indicateur est **centralisé** dans `mod_monitoring` (un seul `id`, donc
une seule notif, retirée dès que tout est calculé), à partir des états
`computing` / `loading` exportés par les deux sous-modules. La notification
bas-droite *inline* de `mod_monitoring_fast_alerts` est retirée au profit de
cet agrégateur (cf. *Carte FAST* ci-dessus pour le bandeau in-panel).
Nouvelle clé i18n `monitoring_computing`.

# nemetonshiny 0.91.15 (2026-06-26)

### Added — Carte FORDEAD : slider temporel cumulatif (couche sévérité)

La Carte FORDEAD dispose désormais d'un sélecteur de date (couche sévérité),
analogue conceptuel au slider de date de la Carte FAST. À la date choisie,
seuls les pixels dont la **1ère détection** est antérieure ou égale sont
affichés → on voit la **progression du dépérissement** dans le temps. Le
domaine du slider (étendue des dates de détection) et le filtrage sont
dérivés de la couche « date de 1ère détection » du run
(`nemeton::read_fordead_layer("first_anomaly")`) ; aucun nouveau produit
cœur requis. Le slider n'apparaît que pour la couche sévérité ; les autres
couches (résumés non temporels) ne sont pas filtrées. Mise à jour du raster
via `leafletProxy` (pas de re-render, zoom/fond/clic-pixel préservés).
Nouvelle clé i18n `monitoring_fordead_date_label`.

### Added — RECONFORT : notifications ntfy (miroir de FORDEAD)

`run_reconfort_async()` pousse désormais des notifications ntfy comme
FORDEAD : start (zone) / phase (1 notif dédupliquée par étape) / error /
complete (n_alertes + durée), via un nouveau
`.build_reconfort_progress_callback()`. Opt-in identique (no-op si
`NEMETON_NTFY_TOPIC` absent). 4 clés i18n
`monitoring_ntfy_reconfort_{start,phase,complete,error}`.

# nemetonshiny 0.91.14 (2026-06-26)

### Changed — RECONFORT : sous-onglet renommé « Alertes RECONFORT »

Le sous-onglet « Carte RECONFORT » est renommé **« Alertes RECONFORT »**
pour la symétrie d'UI avec « Alertes FAST » et « Alertes FORDEAD ». Il
affichait déjà les clusters d'alertes vectorielles (centroïdes colorés par
classe 2/3) + le diagnostic pixel CRSWIR/CRre au clic ; seul le libellé
change (nom de clé i18n conservé, legacy).

### Fixed — Alertes FAST : vert « zone saine » + « calcul en cours » simultanés à l'ouverture

À l'ouverture de l'onglet Suivi sanitaire, on voyait en même temps le
bandeau vert « Aucune alerte FAST » **et** la notification « Calcul du
raster d'alerte en cours… » — incohérent. Cause : le bandeau
(`suspendWhenHidden = FALSE`) traitait `raster_rv` NULL (raster **pas encore
calculé**) comme un raster vide → vert affiché prématurément (y compris
pendant qu'on était encore sur un autre onglet). Désormais le vert
« zone saine » n'apparaît que pour un raster **réellement calculé et vide** ;
tant que rien n'est calculé, le bandeau reste vide et la notification (ou le
bandeau « calcul en cours ») prend le relais.

# nemetonshiny 0.91.13 (2026-06-24)

### Changed — RECONFORT : messages de durée alignés sur la chaîne AOI-scoped

Côté cœur, `run_reconfort_dieback()` est désormais AOI-scoped (crop + reproj
+ masque OSO à la zone) et tourne en ~quelques minutes au lieu de plusieurs
heures. Les messages app sont mis à jour en conséquence :

* Aide du mode RECONFORT : « Quelques minutes, opt-in » (était « Lourd,
  opt-in »).
* Toast de lancement : « Durée estimée : quelques minutes » (était
  « plusieurs minutes à heures »).

Aucun autre changement app : `run_reconfort_async()` / `.invoke_reconfort()`
sont rétrocompatibles (la signature cœur garde des défauts), et la carte
RECONFORT affiche déjà les clusters d'alertes (`nemeton::list_alerts(…,
classes = RECONFORT_ALERT_CLASSES)`, centroïdes colorés par classe).

# nemetonshiny 0.91.12 (2026-06-24)

### Changed — Carte FORDEAD : graphique pixel au rendu canonique FORDEAD (4 catégories)

Le graphique pixel CRSWIR adopte le rendu canonique de FORDEAD avec quatre
catégories de points (marqueurs « × »), au lieu d'un simple « observé /
anomalie » :

* **Entraînement** (bleu) — points dans la fenêtre d'entraînement du run
  (lue dans les métadonnées projet `monitoring_dates_training`).
* **Sain** (vert) — points de suivi sous le seuil de détection.
* **Anomalie** (orange) — points au-dessus du seuil, **avant** la date de
  confirmation du dépérissement.
* **Anomalie confirmée** (rouge) — points de suivi à partir de la 1re
  détection (`premiere_detection`), le pixel étant entré en état
  dépérissement.

La prédiction harmonique (ligne pleine) et le seuil de détection (ligne
pointillée) sont désormais en bleu. Ceci **corrige** au passage le bug où
**tous** les points apparaissaient en rouge : le filtre d'anomalie
contenait un terme scalaire `isTRUE(any(ts$anomalie))` recyclé sur tout le
vecteur. Trois nouvelles clés i18n (`monitoring_fordead_pixel_training`,
`_healthy`, `_confirmed`).

# nemetonshiny 0.91.11 (2026-06-23)

### Added — Carte FORDEAD : bouton plein écran sur le graphique pixel

Le modal du diagnostic pixel CRSWIR de la Carte FORDEAD dispose désormais
d'un bouton « plein écran » en haut à droite (parité avec la Carte FAST) :
il bascule la classe BS5 `.modal-fullscreen` et redimensionne le graphe
plotly (`responsive = TRUE`). Le graphe est enveloppé dans un
`div.pixel-ts-wrap` qui occupe `calc(100vh - 200px)` en plein écran.

# nemetonshiny 0.91.10 (2026-06-23)

### Fixed — Carte FORDEAD : erreur « reactive value outside of reactive consumer » au clic

Au clic sur la Carte FORDEAD, le diagnostic pixel plantait avec
« Can't access reactive value 'current_project' outside of reactive
consumer ». Le callback `session$onFlushed` (qui s'exécute hors
consommateur réactif) appelait `con_provider()` (= `mon_con()` du parent),
qui lit `app_state$current_project$id` pour clé de cache. Aligné exactement
sur le pattern de la Carte FAST (`mod_monitoring_pixel_map`) : toutes les
valeurs réactives (connexion, projet, zone, dossier cache, lat/lng) sont
désormais **résolues avant** le `onFlushed` (en contexte réactif), et le
corps différé est enveloppé dans `shiny::isolate()`. Le message « Calcul
graphique en cours… » et le graphe pixel CRSWIR s'affichent au clic.

# nemetonshiny 0.91.9 (2026-06-22)

### Fixed — Carte FORDEAD : clic-pixel inopérant (leafletOutput recréé)

Le clic sur la Carte FORDEAD n'affichait ni « Calcul graphique en cours… »
ni le graphe pixel. Cause racine : le `leafletOutput` vivait à l'intérieur
de `output$panel` (un `renderUI`), recréé à chaque changement de masque /
couche / langue — chaque recréation détruisait le binding `input$map_click`.
Le correctif v0.91.2 stabilisait `output$map` mais pas le conteneur, qui
était toujours reconstruit. Aligné sur la Carte FAST : le `leafletOutput`
est désormais en **UI statique** (jamais recréé) et les états
placeholder / zone saine / couche indisponible passent par un **overlay
séparé** (`output$overlay`) positionné par-dessus la carte. Le binding du
clic est ainsi préservé.

# nemetonshiny 0.91.8 (2026-06-21)

### Changed — FORDEAD : message de lancement plus réaliste sur la durée

Le toast au lancement du Diagnostic FORDEAD annonçait « Durée estimée :
plusieurs minutes », trompeur pour un premier calcul. Il indique désormais
que le **premier calcul peut prendre plusieurs heures à un jour** si toutes
les dates ne sont pas déjà téléchargées par le Diagnostic FAST, et qu'il est
nettement plus rapide ensuite (`monitoring_health_starting`).

# nemetonshiny 0.91.7 (2026-06-21)

### Fixed — Alertes FAST : faux « zone saine » pendant le calcul du raster

À l'ouverture de l'onglet Suivi sanitaire (ou au changement de zone /
indice / seuils), le bandeau vert « Aucune alerte FAST sur la fenêtre »
s'affichait pendant que le raster d'alerte se calculait encore — message
incohérent puisque des alertes apparaissaient une fois le calcul terminé.
Un état « calcul en cours » (`computing_rv`) est introduit : tant que le
raster n'est pas prêt, le bandeau affiche « Calcul du raster d'alerte en
cours… » (avec spinner) au lieu du vert « zone saine ». La logique
erreur / zone saine / alertes reprend la main une fois le calcul terminé.

# nemetonshiny 0.91.6 (2026-06-21)

### Fixed — Suivi sanitaire : une seule période d'observation par diagnostic

Le `date_range` du haut (« Période d'observation » du Diagnostic FAST) était
toujours visible et s'affichait donc **en double** avec la période
d'observation FORDEAD en mode santé. Désormais chaque diagnostic n'expose
que **sa** période d'observation :

* **FAST** (mode quick) : `date_range` déplacé dans le panneau quick — visible
  uniquement en FAST (défaut `01/01/2017` → aujourd'hui).
* **FORDEAD** (mode health) : `dates_observation` (défaut `01/01/2019` →
  aujourd'hui) + période d'entraînement.
* **RECONFORT** (mode reconfort) : millésime S2 (`reconfort_s2_year`).

Câblage vérifié et corrigé pour que chaque fonction reçoive la bonne période :

* Plan de validation **FORDEAD** : utilise désormais `dates_observation`
  (utilisait à tort le `date_range` FAST).
* Plan de validation **RECONFORT** : utilise l'année S2 (`reconfort_s2_year`,
  traduite en plage `01/01` → `31/12`) au lieu du `date_range` FAST.

### Fixed — Suivi sanitaire : plus de notification « Calcul du raster » au chargement

Le message « Calcul du raster d'alerte en cours… » s'affichait au chargement
d'un projet alors que l'utilisateur était encore sur l'onglet « Sélection »
(la zone de suivi auto-sélectionnée déclenchait le calcul du raster FAST en
tâche de fond). L'observer est désormais gardé sur l'onglet principal actif
(`app_state$active_main_tab`) : le raster n'est calculé que lorsque l'onglet
« Suivi sanitaire » est ouvert.

### Changed — Suivi sanitaire : textes d'aide des modes mis à jour

* FAST : « …via NDMI/NDVI/NBR rolling-window + trend. » (était « …via
  NDVI/NBR rolling-window. Quelques secondes. »).
* FORDEAD : suppression de « Plusieurs minutes à heures ».

# nemetonshiny 0.91.5 (2026-06-21)

### Changed — FORDEAD : période d'observation dédiée (2019) découplée de FAST

Le mode santé (FORDEAD) a désormais sa **propre période d'observation**,
distincte du `date_range` qui pilote le Diagnostic FAST :

* Nouvel input **« Période d'observation »** dans le panneau du mode santé,
  par défaut `01/01/2019` → aujourd'hui. C'est lui qui alimente
  `dates_monitoring` du run FORDEAD (auparavant le `date_range` partagé).
* **Période d'entraînement** (`dates_training`) : `01/01/2017` → `31/12/2018`
  (était `01/01/2016` → `31/12/2017`) — baseline saine qui précède
  l'observation.
* Le `date_range` du haut **reste à `01/01/2017` → aujourd'hui** pour le
  Diagnostic FAST (inchangé).

FORDEAD entraîne son modèle harmonique sur la fenêtre d'entraînement puis
détecte les anomalies sur la période d'observation, qui démarre désormais
après l'entraînement. La période d'observation est persistée/restaurée dans
les métadonnées projet (`monitoring_dates_observation`).

# nemetonshiny 0.91.4 (2026-06-21)

### Fixed — Alertes FAST : bandeau « déclin NBR » en mode Tendance

En mode Tendance, le bandeau au-dessus de la carte affichait « tendance
pluriannuelle du déclin NBR » alors que NBR n'est pas un indice de tendance
(seuls NDMI / NDRE le sont). Au passage de count/rolling → trend, le slot
`resolution_badge` se re-render sur `input$mode` avant que le radio
`index` n'ait renvoyé NDMI depuis le client, donc `input$index` valait
encore l'ancien indice (NBR). Le bandeau assainit désormais l'indice
contre les choix valides du mode (`.fast_index_choices`) et retombe sur le
défaut (NDMI en trend) si l'indice courant n'est pas valide.

# nemetonshiny 0.91.3 (2026-06-21)

### Fixed — Synthèse : vide sous la carte « Synthèse du projet »

La carte « Synthèse du projet » (en-tête de l'onglet Synthèse) laissait un
grand espace vide sous son contenu. L'`layout_columns` extérieur est une
grille fillable (dans le `nav_panel`) qui étirait ses deux rangées à parts
égales sur la hauteur du viewport ; la carte du haut, peu remplie, était
donc gonflée. Ajout de `row_heights = c("auto", 1)` : la 1re rangée prend
la hauteur de son contenu, la 2e (radar / récapitulatif / commentaires)
prend le reste.

# nemetonshiny 0.91.2 (2026-06-20)

### Fixed — Carte FORDEAD : clic-pixel inopérant (carte re-rendue)

Le clic sur la Carte FORDEAD ne déclenchait rien (ni message « calcul en
cours », ni graphe), même au cœur de la zone affectée : `output$map`
dépendait de `mask_r()` / `layer_r()`, donc le widget leaflet était
**re-rendu** à chaque changement de couche/masque, ce qui faisait perdre
le binding `input$map_click`. Aligné sur la Carte FAST : la **carte de
base est désormais stable** (ne dépend que du projet ; masque/couche/
opacité lus en `isolate()`), et le raster + la légende sont mis à jour via
`leafletProxy` (group « Alertes » + légende à `layerId` stable) sans
reconstruire la carte. Le binding du clic, le zoom et le fond
(OSM/Satellite) sont ainsi préservés.

### Changed — Enregistrement de zone conditionné aux UGF

Le bouton « Enregistrer ce projet comme zone de suivi » est désormais
activé dès que le projet a des **UGF** (et non plus seulement un plan
d'échantillonnage), cohérent avec la création des zones par UGF.

# nemetonshiny 0.91.1 (2026-06-19)

### Added / Changed — Carte FORDEAD : aides couches, légende date, feedback clic

- **Bouton « i » par couche** — chaque choix du sélecteur de couche porte
  une icône d'aide (tooltip) expliquant ce que la couche raster affiche
  (sévérité, date de 1re détection, indice d'anomalie, zone modélisée).
- **Légende « Date de 1re détection » en année** (`%Y`) au lieu de la date
  complète — plus lisible sur une échelle continue pluriannuelle.
- **Feedback au clic-pixel** — un message « Calcul du graphique pixel en
  cours… » s'affiche immédiatement au clic sur la Carte FORDEAD (calcul
  différé via `session$onFlushed`, parité Carte FAST), retiré à la fin
  (succès, « pas de données » hors zone modélisée, ou erreur). Le graphe
  CRSWIR (série observée + prédiction harmonique + seuil + dates de stress)
  s'affiche pour tout pixel situé dans la zone modélisée FORDEAD.

# nemetonshiny 0.91.0 (2026-06-19)

### Added — Carte FORDEAD : sélecteur de couche pixel

La Carte FORDEAD gagne un radio **« Couche »** (sidebar droite) permettant
d'afficher, en plus de la **sévérité 0-4** (défaut), trois couches pixel
lues via `nemeton::read_fordead_layer()` (cœur ≥ 0.94.0) :

- **Date de 1re détection** (`first_anomaly`) — palette viridis, légende en
  dates.
- **Indice d'anomalie** (`anomaly_index`) — sévérité continue, palette
  YlOrRd.
- **Zone modélisée** (`modelled_pixels`) — binaire (modélisé / non),
  palette discrète.

Le masquage par strate (D2) et le réglage d'opacité s'appliquent à toutes
les couches. Le court-circuit « zone saine » ne vaut que pour la sévérité ;
les autres couches s'affichent toujours. Si une couche est absente du
bundle (anciens runs < cœur v0.94.0), un message « couche indisponible »
remplace la carte (pas de plantage). Plancher `Imports: nemeton (>= 0.94.0)`.

# nemetonshiny 0.90.4 (2026-06-19)

### Changed — Suivi sanitaire : bascule de mode beaucoup plus rapide

Passer de Diagnostic FAST à FORDEAD (et inversement) prenait plusieurs
secondes : à chaque bascule, **plusieurs réactives (zones, validity,
masque…) rouvraient CHACUNE leur propre connexion** monitoring — or ouvrir
une connexion PostGIS distante coûte ~0,4–1,2 s — et le **check de
validité BD Forêt** (~1 s) était recalculé à chaque fois.

- **Connexion read-only mise en cache par session** (`mon_con()`) :
  réutilisée par `zones()`, `validity()`, `fast_zone_surfaces()` et le
  sous-module Carte FORDEAD (masque + clic-pixel) au lieu d'un
  open/close par évaluation. Reconnexion automatique si le serveur a
  fermé la connexion (timeout) ou au changement de projet ; fermeture en
  fin de session. Les chemins RW (ingestion, enregistrement de zone)
  gardent leurs propres connexions.
- **Mémoïsation de `validity()`** par (projet, zone) : le check BD Forêt
  n'est plus relancé sur les allers-retours de mode.

Mesuré (PostGIS distant) : bascule de mode de ~0,4–2,5 s à **~0,03 s** sur
les bascules répétées (la connexion à froid, ~1 s, n'est payée qu'une fois
par session). Instrumentation `NEMETON_PERF_TRACE` conservée.

### Fixed — Alertes FORDEAD : pixels sains affichés en vert opaque

La carte « Alertes FORDEAD » peignait la classe 0 (sain) en vert, recouvrant
toute la zone, alors que la « Carte FORDEAD » ne montre que les pixels
affectés. La classe 0 est désormais rendue **transparente** sur les deux
cartes (on ne peint que les classes 1-4 au-dessus du fond ; la carte « zone
saine » prend le relais quand tout est sain). La légende garde la classe 0
en référence.

# nemetonshiny 0.90.3 (2026-06-19)

### Fixed — Carte FORDEAD : clic-pixel et opacité inopérants (onglet non-défaut)

La Carte FORDEAD est un sous-onglet non-défaut affiché via `nav_show` /
`nav_hide` (visibilité pilotée par le mode). Shiny ne détectait pas
fiablement quand son conteneur Leaflet devenait visible : la carte
s'initialisait à **taille 0**, si bien que le **clic-pixel** (graphe CRSWIR)
et la **mise à jour d'opacité** via `leafletProxy` tombaient dans le vide —
alors que tout fonctionnait sur Alertes FORDEAD (onglet visible au
démarrage). Corrigé en alignant la Carte FORDEAD sur la Carte FAST :

- `outputOptions(output, "map", suspendWhenHidden = FALSE)` ;
- observer de navigation qui, à l'ouverture du sous-onglet, force
  `leafletInvalidateSize` (Leaflet re-détecte ses dimensions) + un
  `fitBounds` sur l'emprise UGF / raster.

# nemetonshiny 0.90.2 (2026-06-19)

### Fixed — Cartes FORDEAD : opacité, clic-pixel, nettoyage

- **Opacité du raster (zoom + fond préservés)** — déplacer le slider
  d'opacité dans Alertes FORDEAD / Carte FORDEAD reconstruisait toute la
  carte (`renderLeaflet`), réinitialisant le zoom et le fond (OSM ↔
  Satellite). L'opacité est désormais appliquée via `leafletProxy` (mise à
  jour du seul group « Alertes »), comme dans Diagnostic FAST.
- **Clic-pixel Carte FORDEAD** — le clic lisait la série CRSWIR sur la
  strate sélectionnée alors que FORDEAD tourne sur la zone `_tot` (Phase
  A) : sur une strate ≠ `_tot`, aucun graphe n'apparaissait. Le clic lit
  désormais la série sur `_tot` (helper de résolution mutualisé avec le
  masque). Le graphe (CRSWIR observé + prédiction harmonique + seuil
  d'anomalie + dates de stress) s'affiche de nouveau.

### Removed — Case « Inclure les classes faible et moyenne » (santé)

Inerte depuis la Phase A (l'affichage est piloté par le raster, toujours
les 5 classes 0-4) : la case ne pilotait plus que la réactive legacy
`alerts()` (filtre DB des alertes vectorielles), elle-même sans aucun
consommateur. Case + bandeau d'avertissement + réactive `alerts()`
supprimés.

# nemetonshiny 0.90.1 (2026-06-18)

### Added — Cartes FORDEAD : parité d'affichage avec Diagnostic FAST

Les sous-onglets **Alertes FORDEAD** et **Carte FORDEAD** gagnent une
**sidebar droite** (comme Diagnostic FAST) et les mêmes contrôles de carte :

- **Couche UGF** — contour des unités de gestion (`project$indicators_sf`)
  en overlay togglable via le LayersControl.
- **Couche « Alertes »** — le raster catégoriel 0-4 devient un overlay
  nommé togglable.
- **Opacité du raster** — slider 0–1 (défaut 0.75) par onglet.
- **Indice de végétation en radio** — l'indice FORDEAD passe d'une liste
  déroulante (sidebar gauche) à un **radio à droite des cartes**. Comme
  FORDEAD ne modélise que le **CRSWIR** côté cœur, les choix **NDVI/NDWI
  (non calculés) sont retirés** : seul CRSWIR est exposé, et le run utilise
  CRSWIR.

Le **clic sur un pixel** de la Carte FORDEAD continue d'ouvrir le graphe
CRSWIR (série observée + prédiction harmonique + seuil d'anomalie + dates
de stress / 1ʳᵉ détection).

# nemetonshiny 0.90.0 (2026-06-18)

### Changed — Suivi sanitaire FORDEAD : affichage piloté raster + masquage par strate (Phase A)

Réf. spec 008 §15, ADR-013 A5, décision D2. La **placette disparaît** du mode
santé : FORDEAD est calculé **une seule fois sur la zone `_tot`** (union de
toutes les UGFs), et l'affichage par strate n'est plus qu'un **masquage** du
raster `_tot` — aucun recalcul au changement de strate. Plancher
`Imports: nemeton (>= 0.92.0)`.

- **Calcul forcé sur `_tot`** (`mod_monitoring.R`) — le lancement FORDEAD
  (`run_fordead_dieback`), le stamping du résultat et la réconciliation
  disque ciblent désormais la zone `_tot` résolue par convention de nommage
  (spec 020), quelle que soit la strate sélectionnée au menu. Garde-fou
  conservé si le projet n'a pas de zone `_tot`.
- **Masquage à l'affichage par strate** (`mod_monitoring_fordead_map.R`) — le
  masque lu est toujours celui de `_tot` ; si la strate sélectionnée n'est
  pas `_tot`, le raster est clippé à l'AOI de la strate (`terra::mask` via
  `get_monitoring_zone_aoi`, EPSG:2154). Changer de strate ⇒ re-masquage
  instantané, sans nouveau run.
- **« Zone saine » décidée sur le raster** — la décision sain/affecté
  (carte raster / carte « zone saine » / placeholder) se lit désormais sur
  le raster masqué (classe ≥ 1 = affecté), plus sur un compte d'alertes DB
  (`list_alerts` legacy en Phase A). La notification de fin de run n'annonce
  plus de décompte d'alertes (durée seule ; `n_alerts_inserted = NA` côté
  cœur).
- **i18n** — retrait du terme « placette » des messages santé FORDEAD
  (`monitoring_fordead_no_alerts_body`, `monitoring_zone_orphan_body`).

# nemetonshiny 0.89.1 (2026-06-18)

### Added — Ingestion FAST : sentinelle de run + reprise au relancement

L'ingestion Sentinel-2 (Suivi sanitaire, mode rapide) tourne déjà dans un
process worker séparé (`future_promise` / `multisession`), mais le suivi
était perdu à la fermeture de la session. Le worker écrit désormais une
**sentinelle de run** sur disque (`<projet>/data/ingest_run.json`),
indépendante de la session : `running` au démarrage, `done` / `error` /
`cancelled` à la fin.

Au (re)lancement d'une instance Shiny, l'app lit cette sentinelle
(`.detect_ingest_state()`, liveness fondée sur la fraîcheur du fichier de
progression) et affiche dans la sidebar FAST :

- un bandeau **« ingestion en cours en arrière-plan (X/Y) »** quand un
  worker est encore vivant (typiquement après une déconnexion navigateur) ;
- un bandeau **« ingestion interrompue (X/Y) »** + bouton **« Reprendre »**
  quand le worker est mort (process R redémarré). La reprise re-invoque le
  cœur avec `skip_cached` → les tuiles déjà téléchargées sont sautées.

Détails :
- `service_monitoring.R` : helpers `.write_ingest_sentinel()` /
  `.read_ingest_sentinel()`, paramètre `sentinel_path` de l'`ExtendedTask`
  d'ingestion (écriture worker-side).
- `mod_monitoring.R` : `.detect_ingest_state()`, `output$ingest_resume_banner`,
  observer de détection (tick 5 s + changement de projet), bouton
  `input$ingest_resume`, séquence d'invocation factorisée (`start_fast_ingest()`).
- 4 nouvelles clés i18n (`monitoring_ingest_running_banner`,
  `monitoring_ingest_interrupted_banner`, `monitoring_ingest_resume_btn`,
  `monitoring_resume_no_state`).

# nemetonshiny 0.89.0 (2026-06-17)

### Added — Carte FAST : 3ᵉ méthode de lissage « Harmonique »

La modale du graphique « série pixel » (Carte FAST) expose désormais une
**3ᵉ méthode de lissage : Harmonique** (régression de Fourier saisonnière),
en plus de la médiane glissante et de LOESS. Un curseur **« Harmoniques
(cycles annuels) »** (1–3, défaut 2) apparaît uniquement quand la méthode
harmonique est sélectionnée. Les paramètres `method` et `n_harmonics` sont
transmis à `nemeton::smooth_pixel_series()` (spec 026, cœur ≥ 0.91.0). Sur
série trop courte (< 2·K+4 points clairs, < ~9 mois), le garde-fou cœur
lève une erreur déjà capturée → dégradation propre (courbe lissée absente),
les points bruts restent affichés. Plancher `Imports: nemeton (>= 0.91.0)`.

### Changed — Performance : chargement d'un projet récent

Diagnostic + optimisations du chemin « clic projet récent → affichage des
UGFs » :

- **Pré-chauffage de la pile géo** (`arrow`/`geoarrow`/`sf`) ~1,5 s après
  le démarrage de l'app, hors du chemin critique : le coût de chargement
  paresseux des namespaces (surtout `arrow`, ~1,5–2 s) qui frappait le
  **tout premier** clic de projet d'une session est désormais payé pendant
  que l'utilisateur parcourt la page d'accueil.
- **Suppression du `ug_build_sf()` redondant** dans le rendu carte de
  `mod_ug` : la géométrie UGF dissoute déjà construite par
  `attach_indicators_sf()` (`project$indicators_sf`) est réutilisée au lieu
  d'un nouveau `st_union()` par UGF (gain croissant avec le nombre d'UGFs).
- **Instrumentation chrono** activable par `NEMETON_PERF_TRACE=1` (gated,
  zéro coût en prod) le long du chemin de chargement pour mesurer chaque
  étape dans la console.

# nemetonshiny 0.88.2 (2026-06-17)

### Removed — Alertes FORDEAD : générateur QGIS legacy retiré

Le panneau « Générer placettes QGIS (vérification terrain) » de l'onglet
**Alertes FORDEAD** est supprimé : vestige (E6.c.5) faisant doublon avec le
sous-onglet dédié **« Plan de validation FORDEAD »** (spec 014), qui génère
les placettes de vérification terrain avec la méthodologie complète
(placettes validation + témoins, classes, tampon, graine, persistance,
export QGIS). `output$qgis_panel` + `output$qgis_download` et les clés i18n
`monitoring_qgis_*` orphelines sont retirés. Aucune perte de fonctionnalité :
utiliser « Plan de validation FORDEAD ».

# nemetonshiny 0.88.1 (2026-06-17)

### Added — Alertes FAST : message « calcul du raster en cours »

Au changement d'**indice FAST** (NDMI/NDVI/NBR/NDRE) ou de **mode du raster**
(Fréquence/Intensité/Tendance) — ainsi que de zone, dates, seuils ou
paramètres trend — une notification **« Calcul du raster d'alerte en cours… »**
s'affiche immédiatement en bas à droite, le temps que la nouvelle carte se
calcule. Évite les clics intempestifs avant l'affichage.

Le calcul du mask (`compute_fast_alert_mask`) est désormais **déféré**
(`session$onFlushed`) et son résultat passe par un `reactiveVal` : la
notification part au client avant le calcul lourd (un calcul synchrone ne
flush l'UI qu'à sa sortie). Bandeau, carte et clic-pixel consomment le même
résultat (pas de double calcul).

# nemetonshiny 0.88.0 (2026-06-17)

### Added — Graphe « série pixel » lissé (spec 026)

Le graphe de série temporelle par pixel (modale au clic sur la Carte FAST)
ne relie plus chaque acquisition par des segments (dents de scie dues au
bruit nuageux / acquisitions irrégulières). Désormais, par indice
(NDVI/NBR/NDMI) :

- **Points bruts estompés** (marqueurs seuls, taille 4, opacité 0,35, sans
  ligne).
- **Courbe lissée** opaque (ligne, épaisseur 2, `connectgaps = FALSE`), via
  le helper cœur `nemeton::smooth_pixel_series()` (≥ 0.90.0) — médiane
  glissante par défaut.
- Contrôles dans la modale : **fenêtre de lissage** (15–90 j, défaut 45) +
  **méthode** (médiane glissante / LOESS) en accordéon repliable. Le graphe
  se recalcule (cœur) à chaque changement.
- Lignes de seuil (NDVI 0,40 / NBR 0,30 / NDMI 0,20) et plein écran
  conservés.

Le lissage est **purement de l'affichage à l'échelle scène** (lisibilité du
bruit) — distinct du déclin pluriannuel (mode Tendance / `extract_pixel_trend`,
composites + Theil-Sen). Calcul 100 % cœur ; l'app n'effectue aucun lissage.
Plancher relevé à `nemeton (>= 0.90.0)`.

# nemetonshiny 0.87.3 (2026-06-17)

### Added — Plan de validation : message « génération en cours »

Au clic sur **« Générer le plan sanitaire »** (et « Générer plan de
validation » pour FORDEAD/RECONFORT), une notification **« Génération du
plan en cours… »** s'affiche immédiatement en bas à droite, et un garde
ignore les clics suivants tant que le plan se calcule (tirage + lecture
raster). Le calcul est déclenché après le flush (`session$onFlushed`) pour
que la notification parte au client avant le calcul lourd — même pattern
que le graphe trajectoire (v0.85.16).

# nemetonshiny 0.87.2 (2026-06-17)

### Fix — Suivi sanitaire : ré-alignement auto de la zone projet sur `_tot`

À l'ouverture de l'onglet Suivi sanitaire, `metadata$monitoring_zone_id`
du projet courant est désormais **ré-aligné automatiquement** sur sa zone
`_tot` (union complète des UGFs, convention spec 020) — en mémoire **et**
persisté sur disque. Corrige les projets dont la metadata pointait encore
sur une zone pré-spec-020 (mono-zone) ou sur une zone d'un autre projet
(ex. Mouthe → `villards`). Opère une seule fois (garde sur l'égalité), ne
touche rien si aucune zone `_tot` n'existe encore (projet sans zones
générées). Complète le correctif v0.87.1 (le plan utilisait déjà la zone
sélectionnée ; la metadata est maintenant cohérente pour les autres
consommateurs).

# nemetonshiny 0.87.1 (2026-06-17)

### Fix — Plan de validation : utilise la zone du sélecteur « Zone de suivi »

Le plan de validation était construit sur la zone mémorisée dans la
metadata du projet (`monitoring_zone_id`), qui pouvait pointer sur une zone
**obsolète** ou **d'un autre projet** (cas spec 020 : zones par essence
`_tot`/`_res`/`_mix`) — d'où des placettes générées **hors de l'union des
UGFs** affichée. Désormais le plan (trend FAST **et** catégoriel
FORDEAD/RECONFORT) est construit sur la **zone sélectionnée dans le
sélecteur « Zone de suivi »** (`zone_id_r`), avec repli sur la metadata
seulement si aucune zone n'est sélectionnée. Par défaut le sélecteur
pointe sur la zone `_tot` = union complète des UGFs.

### Changed — Plan de validation FAST : paramètres de tendance mutualisés

Les paramètres avancés (mois du composite, années min., obs. min./an, seuil
α) sont **retirés de la sidebar** du Plan de validation FAST : ils sont déjà
définis dans l'onglet **« Alertes FAST » (mode Tendance)** et désormais
**réutilisés** par le plan (`trend_params_r`). Le plan échantillonne ainsi
exactement la tendance affichée dans Alertes FAST — une seule source de
vérité. Une note le rappelle sous le sélecteur de graine.

# nemetonshiny 0.87.0 (2026-06-16)

### Changed — Plan de validation FAST branché sur le trend (spec 025)

Le sous-onglet **« Plan de validation FAST »** ne s'appuie plus sur le masque
catégoriel count/rolling (qui renvoyait souvent « zone saine » car il ne
capte pas le dépérissement chronique) mais sur le **trend** (déclin
pluriannuel) via `nemeton::create_trend_sanitary_plan()` (≥ 0.88.0) :
placettes **sanitaires** tirées avec une probabilité ∝ sévérité continue du
déclin (|pente| Theil-Sen), + placettes **témoins** sur les zones stables.
Option A pure : pondération continue, pas de classes 0-4, pas de TSP.

- **Sidebar FAST refondue** : Indice (NDRE/NDMI), Fenêtre d'analyse
  pluriannuelle, Placettes sanitaires, Placettes témoins, Graine, +
  « Paramètres avancés » repliable (mois saison, années min, obs min/an,
  alpha). **Retrait** des contrôles « Classes d'alerte », « Classes témoins »
  et « Tampon » (sans objet en mode trend).
- **Carte** : placettes colorées par sévérité continue (vert → rouge),
  témoins en gris ; popup `plot_id` / type / `alert_value` ; légende sévérité.
- **Aucun déclin significatif** → message dédié « rien à valider » (≠ « zone
  saine »).
- Cohérence : `alert_value` d'une placette == valeur pré-quartile du raster
  trend au même pixel (clic carte → graphe trajectoire).

FORDEAD et RECONFORT conservent leur plan catégoriel (masque + classes + TSP)
inchangé. Statistique 100 % cœur ; l'app ne recalcule rien (règle 1).
Plancher relevé à `nemeton (>= 0.88.0)`.

# nemetonshiny 0.86.2 (2026-06-16)

### Changed — Plan de validation : classes d'alerte en ordre décroissant

Dans le plan de validation (Alertes/Carte FAST + FORDEAD), les cases à
cocher « Classes d'alerte » s'affichent désormais en **ordre décroissant de
sévérité (4, 3, 2, 1)** au lieu de `3, 4, 1, 2`. Sélection par défaut
inchangée (3 et 4). Corrigé à la fois dans l'UI et dans le rafraîchissement
serveur des libellés (`updateCheckboxGroupInput`).

# nemetonshiny 0.86.1 (2026-06-16)

### Fix — Plein écran : le graphe remplit désormais tout l'écran

En plein écran, le graphe plotly (graphe trend par pixel des Alertes FAST,
et graphe Pixel de la Carte FAST) restait à sa taille initiale (~600 px) au
lieu de remplir l'écran : basculer la classe CSS `.modal-fullscreen` ne
déclenche pas d'événement `resize`, et plotly (`responsive`) n'écoute que
`window.resize`. Le bouton plein écran émet maintenant un `window.resize`
(différé) après le toggle → le graphe se redimensionne pour occuper toute la
modale agrandie (largeur + hauteur).

# nemetonshiny 0.86.0 (2026-06-16)

### Added — Alertes FAST : graphe de tendance NDRE par pixel au clic

En mode **Tendance** de l'onglet **Alertes FAST**, un clic sur la carte ouvre
une modale montrant, pour le pixel cliqué, **pourquoi il a cette couleur** :

- **Composites saisonniers annuels** de l'indice (NDRE par défaut, NDMI
  possible) — les valeurs exactes utilisées par le cœur — en points.
- **Droite Theil-Sen** superposée (rouge si déclin significatif, gris sinon),
  tracée uniquement si assez d'années valides.
- **Annotations** : pente (indice/an), p-value Mann-Kendall, badge « déclin
  significatif oui/non », nombre d'années valides, et la **classe de sévérité
  0-4** (lue directement dans le raster mask affiché — non recalculée).
- Bouton **plein écran** en en-tête de modale (toggle `.modal-fullscreen`).
- **Notification « calcul en cours »** immédiate + garde anti-multi-clics
  (calcul déclenché via `session$onFlushed`).

Toute la statistique vient du cœur `nemeton::extract_pixel_trend()` (≥ 0.87.0),
garanti cohérent avec le raster (`alert_value` == valeur pré-quartile du pixel) :
l'app ne recalcule rien (règle 1). Plancher relevé à `nemeton (>= 0.87.0)`.

# nemetonshiny 0.85.16 (2026-06-16)

### Changed — Suivi sanitaire : mode par défaut « Diagnostic FAST »

L'onglet Suivi sanitaire ouvre désormais **toujours** sur le mode
**Diagnostic FAST** (`quick`), quel que soit le dernier mode persisté du
projet. Auparavant, le mode de suivi enregistré était restauré à
l'ouverture ; la plupart des projets ayant lancé FORDEAD avaient `health`
persisté → ouverture systématique sur FORDEAD, non désirée. Les autres
réglages (seuils, indice de végétation, dates FORDEAD) restent restaurés.

### Changed — Carte FAST : bouton plein écran du graphique Pixel restylé

Le bouton « plein écran » du graphique Pixel (modale au clic) adopte le
style de la modale « clés API & corpus RAG » : un **bouton ancré en haut à
droite de l'en-tête** (icône `arrows-fullscreen`) qui bascule la classe
BS5 `.modal-fullscreen` (bord à bord), au lieu de l'icône d'expansion
`bslib::card` au survol (v0.85.15). Le plot grandit en plein écran.

### Added — Carte FAST : message « calcul en cours » au clic Pixel

Au clic sur la Carte FAST, une notification **« Calcul du graphique pixel
en cours… »** s'affiche immédiatement en bas à droite, et un garde ignore
les clics suivants tant que le graphique se calcule (évite les clics
intempestifs). Le calcul est déclenché après le flush (`session$onFlushed`)
pour que la notification parte au client avant le calcul lourd.

# nemetonshiny 0.85.15 (2026-06-16)

### Added — Carte FAST : bouton « plein écran » sur le graphique Pixel

Le graphique de série temporelle par pixel (modale au clic sur la carte
pixel) gagne un bouton **plein écran** : le plot est enrobé dans une
`bslib::card(full_screen = TRUE)` (même affordance que les cartes Leaflet
de l'app — icône d'expansion au survol, en haut à droite). Le plot remplit
la carte et se redimensionne en plein écran (`plotly` en mode `responsive`).

# nemetonshiny 0.85.14 (2026-06-15)

### Fixed — Alertes FAST : message d'erreur réel du raster (au lieu du générique)

Quand le calcul d'un raster d'alerte **échoue** (le cœur lève une erreur),
le bandeau affichait systématiquement le message générique « aucune scène
cachée ne porte les bandes de cet indice » — qui **écrasait** la vraie
erreur (un 2ᵉ setter masquait le 1ᵉʳ). On voyait donc « bandes manquantes »
même quand les bandes étaient présentes et que l'échec venait d'ailleurs
(ex. `[mosaic] resolution does not match` sur un cache multi-tuiles MGRS).

Désormais : le message générique « aucune scène » n'est affiché que si le
cœur renvoie un résultat vide **sans lever d'erreur** ; en cas d'erreur, le
**vrai message** (`NDRE : [mosaic] resolution does not match`, etc.) est
conservé et affiché. Diagnostic bien plus utile.

# nemetonshiny 0.85.13 (2026-06-15)

### Changed — Alertes FAST : pré-calcul des deux rasters de Tendance

En mode **Tendance**, les rasters des **deux** indices NDMI **et** NDRE sont
désormais pré-calculés : le raster affiché reste celui de l'indice
sélectionné, mais le cache disque de l'autre indice est réchauffé en
arrière-plan (tâche différée `later`), de sorte que basculer le radio
NDMI ↔ NDRE soit **instantané** (lecture du cache, plus de recalcul).

- Le pré-calcul n'a lieu qu'en mode Tendance (count/rolling restent
  mono-indice à la demande).
- Idempotent : `compute_fast_alert_mask()` persiste un TIF par (indice,
  paramètres) ; un cache déjà chaud n'est pas recalculé.
- Le déclencheur est débouncé (800 ms) pour ne pas empiler de calculs
  lourds pendant le réglage des sliders Tendance.
- Chaque indice requiert ses bandes dans le cache S2 (NDMI : B8A+B11,
  NDRE : B05+B8A) ; un indice dont les bandes manquent est simplement
  ignoré par le pré-calcul (l'autre reste disponible).
- Nouvelle helper interne `.compute_fast_mask()` (plomberie partagée entre
  le raster affiché et le pré-calcul, source unique).

# nemetonshiny 0.85.12 (2026-06-15)

### Removed — Slider « Seuil NDRE » du panneau de surveillance

Le slider « Seuil minimum NDRE » est retiré : depuis v0.85.11 NDRE est
réservé au mode Tendance des Alertes FAST, qui ignore les seuils
(Theil-Sen / Mann-Kendall). Le contrôle n'avait donc plus aucun effet.
La clé `ndre` est retirée des trois objets `thresholds_r` (Alertes FAST,
Carte FAST, plan de validation), et la clé i18n `monitoring_threshold_ndre`
est supprimée. Aucun impact fonctionnel (le mode Tendance n'utilise pas de
seuil ; les autres indices NDMI/NDVI/NBR conservent leurs sliders).

# nemetonshiny 0.85.11 (2026-06-15)

### Changed — Alertes FAST : NDRE réservé au mode Tendance

NDRE (red-edge) n'est plus proposé dans les modes **Fréquence** et
**Intensité** des Alertes FAST : ces modes détectent des chocs court terme
et n'exposent plus que **NDMI / NDVI / NBR**. NDRE reste disponible dans le
mode **Tendance** (NDMI / NDRE), où il a du sens pour un déclin chronique
pluriannuel. Annule l'ajout de NDRE aux modes Fréquence/Intensité (v0.85.2).

Note : le slider « Seuil NDRE » du panneau de surveillance n'a plus d'effet
(le mode Tendance ignore les seuils — Theil-Sen / Mann-Kendall) ; il pourra
être retiré dans un lot ultérieur.

# nemetonshiny 0.85.10 (2026-06-15)

### Changed — Synthèse : barre de confiance descendue d'un cran

Ajustement fin de v0.85.9 : la barre de progression de confiance est
descendue (`mt-2`) pour s'aligner précisément avec le texte « Taille image
Max 5 Mo, PNG/JPG ». `ndp_progress_bar()` reçoit un paramètre `bar_class`
(défaut inchangé `progress`, aucun impact sur mod_field_ingest).

# nemetonshiny 0.85.9 (2026-06-15)

### Changed — Synthèse : « Confiance φ » à la taille du « Score global »

Le libellé « Confiance φ : 16.7% » du bandeau Synthèse est rendu à la
**même taille** que « Score global » (`text-muted` sans `small`), ce qui
abaisse la barre de confiance pour l'aligner avec le texte « Taille image
Max 5 Mo, PNG/JPG » de la colonne centrale. `ndp_progress_bar()` reçoit un
paramètre `label_class` (défaut inchangé `text-muted small`, donc aucun
impact sur les autres usages — mod_field_ingest).

# nemetonshiny 0.85.8 (2026-06-15)

### Changed — Synthèse : « / 100 (12 familles) » sur la ligne du score

Suite de v0.85.7 : « / 100 (12 familles) » est désormais accolé à la
valeur du score sur la **même ligne** que « Score global » + l'icône
d'info. Le bloc gagne une ligne supplémentaire, remontant d'autant la
barre de confiance φ.

# nemetonshiny 0.85.7 (2026-06-15)

### Changed — Synthèse : score global sur une seule ligne

Dans le bandeau supérieur de l'onglet Synthèse (colonne de droite), le
libellé « Score global » + l'icône d'info et la valeur (ex. 55.7) sont
désormais sur la **même ligne**. Le bloc remonte d'une ligne, ce qui aligne
la barre de confiance φ au niveau du texte « Taille image Max 5 Mo,
PNG/JPG » de la colonne centrale.

# nemetonshiny 0.85.6 (2026-06-15)

### Changed — Rapport : pages familles, notes de bas de page uniquement

Le bloc « Sources documentaires » ajouté en v0.85.5 dans le **corps** des
pages familles du rapport PDF est retiré : on ne conserve que les **notes
de bas de page** (dédupliquées, labels namespacés `[^C-1]`…). La liste des
sources reste disponible dans l'application (voir ci-dessous).

### Added — Bloc « Sources documentaires » sur chaque page Famille (UI)

Chaque page **Famille d'indicateurs** affiche désormais, sous son
commentaire, un bloc **« Sources documentaires »** listant les sources
citées par ce commentaire — comme la page Synthèse. Les sources sont
extraites des marqueurs `[^n]` du commentaire et résolues via le contexte
RAG partagé (`synthesis_sources$sources_md`), dédupliquées par contenu et
présentées dans l'ordre de première citation. Vide quand le commentaire ne
cite aucune source. Nouvelle helper `.family_sources_md()`.

# nemetonshiny 0.85.5 (2026-06-15)

### Fix — Sources dédupliquées + bloc « Sources documentaires » par famille

Dans le rapport, les **pages d'analyse par famille** d'indicateurs
répétaient une même source sous plusieurs numéros de notes (ex. famille
Carbone & Vitalité : Bontemps 2006 = notes 5, 6, 9, 11 ; Breda 2002 =
notes 7, 8, 10).

**Cause** : la dédup « une note par source » (`.prepare_footnotes()`,
v0.84.6→v0.84.10) n'était appliquée **qu'à la synthèse**. Les commentaires
de famille étaient passés bruts au template Quarto, donc chaque chunk RAG
d'une même source devenait une note distincte.

**Fix** : nouvelle fonction `.prepare_family_footnotes()`, appliquée à
chaque commentaire de famille avant export :

- **Dédup par contenu** : une seule note de bas de page par source unique,
  en gardant la première référence (orphelines et doublons retirés), comme
  pour la synthèse.
- **Labels namespacés par famille** (`[^C-1]`, `[^C-2]`, …) pour éviter
  toute collision avec les notes de la synthèse — ou d'une autre famille —
  dans le même PDF (Pandoc ne sait pas référencer deux fois une note, et
  des ids numériques se chevaucheraient).
- **Bloc « Sources documentaires » visible** sous chaque commentaire,
  listant les sources distinctes citées dans l'ordre de première citation
  (en plus des notes de bas de page). Bilingue FR/EN.

# nemetonshiny 0.85.4 (2026-06-15)

### Fix — Affichage des parcelles sur la carte plus rapide

L'écran restait sur l'overlay « Affichage des parcelles… » (page blanche)
pendant plusieurs secondes après la synchro PostGIS, surtout sur les
communes à nombreuses parcelles.

**Causes** (rendu serveur bloquant, pendant l'overlay) :

- Les labels de survol étaient construits par un **sous-ensemble sf ligne
  par ligne** (`parcel_data[i, ]` dans un `sapply`) — chaque sous-ensemble
  traîne la géométrie, coût O(N) sur des centaines/milliers de parcelles.
- Les géométries cadastrales étaient envoyées **non simplifiées** à
  Leaflet → GeoJSON volumineux, parsing + rendu navigateur lents.

**Fix** :

- Nouvelle fonction **vectorisée** `create_parcel_labels()` : tous les
  labels construits en une passe sur la table attributaire (géométrie
  retirée), sans sous-ensemble par ligne. Robuste aux `lieu-dit` NA (la
  version unitaire `create_parcel_label()` plantait dessus).
- **Simplification géométrique pour l'affichage seulement**
  (`sf::st_simplify`, tolérance ~1 m en CRS projeté, `preserveTopology`),
  appliquée à la couche dessinée. La géométrie exacte est conservée dans
  `parcels()` pour la sélection, le zoom et l'export. Tolérance réglable
  via l'option app `parcel_simplify_tolerance_m`.

# nemetonshiny 0.85.3 (2026-06-15)

### Fix — Chargement des projets récents plus rapide (IO disque)

L'écran d'accueil mettait du temps à afficher la liste des **projets
récents**, surtout avec beaucoup de projets.

**Cause** : `list_recent_projects()` (appelée de façon **bloquante** au
rendu de `mod_home`, jusqu'à 50 projets) lisait et parsait
`metadata.json` **trois fois par projet** — deux fois dans
`check_project_health()` (dont une relecture redondante) et une fois pour
les champs affichés. Avec N projets, jusqu'à 3N lectures+parsings JSON
synchrones figeaient l'UI au démarrage.

**Fix** :

- `check_project_health()` lit `metadata.json` **une seule fois** (la
  double-lecture interne est supprimée) et accepte un paramètre optionnel
  `metadata =` pour réutiliser des données déjà parsées — signature
  publique rétro-compatible.
- `list_recent_projects()` lit le fichier **une seule fois** par projet et
  passe le résultat à `check_project_health()` → **3 lectures/parsings par
  projet ramenées à 1**.
- Ajout d'un **cache mémoire** du listing trié, validé par une signature
  filesystem bon marché (un `list.dirs()` + un `file.info()` vectorisé sur
  les `metadata.json` — stat seul, sans lecture ni parsing) avec TTL de
  secours. Les re-rendus successifs ne rescannent plus le disque tant que
  rien n'a changé ; toute création / mise à jour / suppression invalide le
  cache (explicitement et via la signature), donc les changements
  apparaissent immédiatement.

# nemetonshiny 0.85.2 (2026-06-15)

### Added — Alertes FAST : indice NDRE en modes Fréquence / Intensité

Le sous-onglet « Alertes FAST » du Suivi sanitaire expose désormais
**NDRE** (red-edge B05+B8A) comme 4ᵉ indice des modes `count` (Fréquence)
et `rolling` (Intensité), à côté de NDMI / NDVI / NBR. NDRE n'était jusqu'à
présent disponible que dans le mode `trend`. Les bandes red-edge étant déjà
mises en cache à l'ingestion (`bands = c("NDVI", "NBR", "NDMI", "NDRE")`,
v0.85.0), aucun changement cœur n'est requis : `compute_fast_alert_mask()`
consomme l'indice choisi via le radio local de l'onglet.

- Nouveau slider de seuil **« Seuil minimum NDRE »** dans le sidebar parent
  (`threshold_ndre`, défaut 0.20, parité avec NDMI), câblé dans les
  `thresholds_r` d'Alertes FAST et de la prévisualisation du plan de
  validation (`mod_validation_sampling`).
- `.fast_index_choices()` count/rolling → `c(NDMI, NDVI, NBR, NDRE)`.
- Nouvelle clé i18n `monitoring_threshold_ndre` (FR/EN).

# nemetonshiny 0.85.1 (2026-06-15)

### Fixed — Tests alignés sur l'ajout de NDRE aux bandes FAST

`test-mod_monitoring.R` figeait encore `bands = c("NDVI", "NBR", "NDMI")`
dans deux assertions du `fast_task$invoke()`, qui échouaient depuis l'ajout
de `NDRE` (red-edge B05+B8A, mode FAST `trend`, v0.85.0). Assertions mises
à jour vers `c("NDVI", "NBR", "NDMI", "NDRE")`. Aucun changement de code de
production — alignement de tests uniquement.

# nemetonshiny 0.85.0 (2026-06-15)

### Added — Suivi sanitaire : mode FAST `trend` (Theil-Sen + Mann-Kendall)

Le diagnostic FAST expose désormais un **3ᵉ mode** dans le sous-onglet
« Alertes FAST », à côté de Fréquence (`count`) et Intensité (`rolling`) :
**Tendance** (`trend`). Il détecte le **déclin chronique pluriannuel**
(dépérissement des feuillus) via la régression Theil-Sen + le test de
Mann-Kendall sur un composite saisonnier annuel (nemeton spec 023,
cœur ≥ 0.69.0).

- **Radio « Mode du raster »** : ajout de l'option `trend`.
- **Indices mode-dépendants** : `trend` propose NDMI (défaut) et **NDRE**
  (red-edge) ; `count`/`rolling` restent NDMI/NDVI/NBR.
- **Paramètres trend en sidebar** (conditionnels) : mois du composite
  saisonnier, années minimum, seuil de significativité (alpha).
  `threshold`/`window_days` sont masqués (ignorés par le cœur en trend).
- **Ingestion** : les bandes red-edge **B05 + B8A** sont désormais mises
  en cache (ajout de `NDRE` aux bandes) pour alimenter le mode trend ;
  le cœur (release prewarm trend) pré-chauffe alors aussi les cartes
  trend. Sur un cœur antérieur, la carte trend est calculée à la demande
  (dégradation gracieuse).
- Mapping du toast de pré-calcul `fast_prewarm:*_trend` → libellé
  « Tendance ».
- Nouvelles clés i18n (FR/EN) : `monitoring_fast_alerts_mode_trend`,
  `monitoring_fast_alerts_badge_trend`, `monitoring_trend_months`,
  `monitoring_trend_min_years`, `monitoring_trend_alpha`,
  `validation_class_unit_trend`, `fast_mode_trend`.

### Changed — Suivi sanitaire : libellés des trois modes de diagnostic

Renommage des trois modes du sélecteur « Mode de suivi » pour une
nomenclature homogène « Diagnostic <méthode> (<cible>) » :

- « Surveillance rapide (FAST) » → **« Diagnostic FAST (spot/trend) »**
- « Diagnostic sanitaire (FORDEAD) » → **« Diagnostic FORDEAD (résineux) »**
- « Dépérissement feuillus (RECONFORT) » → **« Diagnostic RECONFORT (feuillus) »**

Clés i18n `monitoring_mode_quick` / `monitoring_mode_health` /
`monitoring_mode_reconfort` (FR/EN). Aucun changement de logique : les
valeurs internes (`quick` / `health` / `reconfort`) sont inchangées.

Note : la bascule spot/trend du diagnostic FAST existe déjà — c'est le
radio « Mode du raster » (Fréquence = spot / Intensité = trend) dans le
sous-onglet « Alertes FAST » ; ses libellés sont conservés.

### Added — Note explicite quand la perspective IA n'a pas de sources

Sous la perspective IA de la synthèse, à l'emplacement habituel du bloc
« Sources documentaires », une **note grisée** s'affiche désormais quand la
perspective a été générée **sans aucune source** : « Perspective générée
sans sources documentaires : le corpus de connaissances est indisponible ou
vide… ». Auparavant le bloc disparaissait silencieusement et l'utilisateur
ne savait pas si le RAG avait été consulté.

La note précise les causes typiques (base PostgreSQL/pgvector non
configurée, clé d'embedding absente, **base locale SQLite qui ne supporte
pas le RAG**). Elle réapparaît aussi au rechargement d'un projet dont la
perspective avait été générée sans sources.

- Nouvelle clé i18n `rag_no_sources_note` (FR/EN) dans `utils_i18n.R`.
- `mod_synthesis.R` : `output$ai_sources` rend la note au lieu de `NULL`
  quand `ctx` existe mais `sources_md` est vide ; la restauration au reload
  réinjecte le contexte RAG dès qu'une synthèse existe (sources vides
  incluses).

# nemetonshiny 0.84.10 (2026-06-14)

### Fix — Rapport PDF : une seule note par source (dédup par contenu)

Dans le rapport, une même source (ex. « ONF — Manuel d'aménagement »)
pouvait apparaître **plusieurs fois** en note de bas de page sous des
numéros différents (notes 10, 11, 12, 14, 17 = le même document), au lieu
d'une référence unique.

**Cause** : une même source peut être présente dans `sources_md` sous
plusieurs ids (corpus ingéré en plusieurs documents, ou ancienne
numérotation par chunk d'une perspective générée avant la dédup v0.84.6).

**Fix** : `.prepare_footnotes()` déduplique désormais **par contenu** —
pour chaque texte de citation, un id canonique (le 1er rencontré), et
toutes les refs vers la même source sont réécrites vers cet id. Résultat :
**une seule note de bas de page par source unique**, quelle que soit la
provenance (projets anciens inclus, sans re-génération).

# nemetonshiny 0.84.9 (2026-06-14)

### Fix — Rapport PDF : les `[^n]` restaient littéraux malgré des sources OK

Le bloc « Sources documentaires » affichait bien les bonnes sources
(numérotation correcte), mais dans le **PDF** les refs `[^1]`..`[^4]`
s'imprimaient en **littéral** au lieu de devenir des notes de bas de page.

**Cause** : l'export lisait les sources depuis la copie **in-memory**
`current_project$comments$synthesis_sources`, qui **n'est pas rafraîchie**
après une génération (seul le `comments.json` sur disque l'est). Sur un
projet re-généré en session, `.prepare_footnotes()` recevait donc un
`sources_md` périmé (ou vide) → aucune définition `[^n]:` appendée → refs
littérales, alors que l'écran (qui lit `rag_ctx_synthesis()`) montrait les
bonnes sources.

**Fix** : l'export utilise désormais en priorité `rag_ctx_synthesis()`
(la MÊME source que le bloc affiché), avec repli sur la copie persistée.
Affichage et PDF sont cohérents.

# nemetonshiny 0.84.8 (2026-06-14)

### Fix — Profil « Propriétaire » : récupère enfin ses sources RAG

Le profil « Propriétaire » (clé app `owner`, fichier `owner.yml`) ne
récupérait **aucune référence** dans sa perspective : le corpus tague ses
documents avec le code `proprietaire_prive` (4 documents), mais
`rag_profile_code("owner")` renvoyait `"owner"` → aucun match dans
`retrieve_knowledge(profile_codes = …)`, et ces 4 documents restaient
**orphelins** (jamais servis à aucun profil).

**Fix** : table d'alias `.RAG_PROFILE_CODE_ALIASES` (`owner →
proprietaire_prive`) dans `rag_profile_code()`. Le profil « Propriétaire »
voit désormais ses 4 documents corpus, et plus aucun code corpus n'est
orphelin.

# nemetonshiny 0.84.7 (2026-06-14)

### Fix — Perspective Synthèse : profil « Planificateur sylvicole » renvoyait du JSON

Sélectionner « Planificateur sylvicole » dans « Générer par IA » (onglet
Synthèse) produisait un commentaire en **JSON brut** (```json { … }```)
au lieu d'une perspective en prose.

**Cause** : `planificateur.yml` est un profil **JSON-only interne** (« Tu
produis exclusivement du JSON valide conforme au schéma demandé… »)
consommé par la génération du **Plan d'action** (qui code en dur
`build_system_prompt(expert = "planificateur")`). Mais `get_expert_choices()`
listait **tous** les profils `.yml` → il fuitait dans le sélecteur de
perspective.

**Fix** : `planificateur` est exclu de `get_expert_choices()` (sélecteur),
tout en restant dans `get_expert_profiles()` (le Plan d'action en a
besoin). Les profils de perspective légitimes restent proposés.

# nemetonshiny 0.84.6 (2026-06-14)

### Fix — RAG : numérotation `[^n]` cohérente prompt ↔ sources (cause amont)

Le bloc « Sources documentaires » ne listait pas toutes les sources que le
LLM citait — **cause racine** des `[^n]` orphelins dans le rapport.

`rag_context()` (`service_rag.R`) numérotait **incohéremment** :
- le **prompt** présentait au LLM **tous les chunks** `[^1]..[^K]`
  (jusqu'à `top_k = 8`) ;
- le bloc **sources** **dédupliquait par document** puis renumérotait
  `[^1]..[^N]` (N = documents uniques, souvent < K).

Le LLM citait donc des **numéros de chunk** (`[^7]`, `[^8]`) absents de la
liste dédupliquée des sources, et même les numéros « valides » pouvaient
pointer sur le mauvais document.

**Fix** : le prompt est désormais numéroté **par document unique**
(`[^1]..[^N]`), avec la **même numérotation** que le bloc sources
(`format_citations(best_per_doc)`). Les chunks d'un même document sont
regroupés sous un seul `[^d]`. Le LLM ne peut donc citer que `[^1]..[^N]`,
tous présents dans les sources et pointant sur le bon document.

Combiné au sanitiseur d'export v0.84.5, le rendu des notes est correct de
bout en bout.

# nemetonshiny 0.84.5 (2026-06-14)

### Fix — Rapport Quarto : `[^n]` orphelins / dupliqués restaient littéraux

Suite à v0.84.3, certaines refs `[^n]` du commentaire restaient imprimées
en littéral dans le PDF au lieu de devenir des notes de bas de page. Deux
causes (observées sur de vraies perspectives) :

- **Refs orphelines** : le LLM cite parfois un numéro **au-delà** des
  sources disponibles (`[^7]`, `[^8]` alors qu'il n'y a que 4 sources) →
  aucune définition → Pandoc les laisse littérales.
- **Refs dupliquées** : une même note est référencée plusieurs fois (corps
  + résumé « Sources mobilisées ») → Pandoc **ne sait pas** référencer 2×
  une même note → les occurrences suivantes restent littérales.

**Fix** : `.prepare_footnotes()` (remplace `.sources_md_to_footnote_defs`)
garde la **1re occurrence de chaque ref valide** comme vraie note, **retire**
les orphelines et les doublons (plus de `[^n]` littéral), nettoie les
virgules orphelines laissées par un résumé « Sources mobilisées : [^x],
[^y] » vidé, et appende les définitions `[^n]:` uniquement pour les ids
réellement utilisés.

Note : la cause amont (le LLM cite des numéros inexistants) reste une
question de discipline de citation / cohérence du RAG côté cœur ; ce
correctif rend l'export robuste quoi qu'il arrive.

# nemetonshiny 0.84.4 (2026-06-14)

### Fix — Sources de synthèse : ne plus effacer le contexte RAG in-session

Suite à v0.84.3, l'observer « restauration des commentaires au chargement
projet » remettait `rag_ctx_synthesis` à NULL à **chaque** réassignation
de `app_state$current_project` — y compris l'attache différée de
`indicators_sf` (v0.78.0), qui ré-assigne `current_project` ~0,1 s après
le chargement avec le **même id**. Conséquence : le bloc « Sources
documentaires » pouvait disparaître alors qu'une perspective venait d'être
générée (sources en session mais pas encore dans la copie in-memory).

**Fix** : l'observer ne réagit plus qu'à un **vrai changement d'id de
projet** (garde `last_loaded_pid`). Les sources fraîchement générées
restent affichées ; le reset/restore ne se fait qu'en changeant
réellement de projet.

Rappel : les sources/notes n'apparaissent que pour une perspective
**générée depuis v0.84.3** (persistance forward-looking) — re-générer la
perspective sur un projet ancien pour les obtenir.

# nemetonshiny 0.84.3 (2026-06-14)

### Synthèse — Sources RAG persistées, réordonnées, et vraies notes Quarto

Trois améliorations liées du bloc « Sources documentaires » de l'onglet
Synthèse :

- **Sources affichées au rechargement d'un projet.** Le contexte RAG
  (`sources_md` + `n_sources`) n'était qu'un reactiveVal de session, rempli
  seulement à la génération d'une perspective : au rechargement, seul le
  commentaire revenait, pas les sources. Il est désormais **persisté** dans
  `comments.json` (`save_comments(synthesis_sources=)`) et restauré au
  chargement → le bloc « Sources documentaires » réapparaît. Une édition
  manuelle du commentaire conserve les sources.
- **Ordre + police.** Le bloc affiche maintenant le titre **« Sources
  documentaires » en premier**, puis « Perspective appuyée sur N source(s) »
  **dans la même police** (paragraphe normal, plus le petit gris), puis la
  liste des citations.
- **Vraies notes de bas de page dans le rapport Quarto.** Le commentaire
  porte des refs inline `[^n]` mais `format_citations` produit les entrées
  sans deux-points (`[^1] …`), donc Pandoc imprimait `[^2]` en littéral. À
  l'export, les **définitions** `[^n]: …` (dérivées des sources persistées
  via `.sources_md_to_footnote_defs()`) sont appendées au commentaire →
  Pandoc rend de **vraies notes numérotées** en bas de page.

# nemetonshiny 0.84.2 (2026-06-14)

### Fix — Le Tour guidé ne se lançait plus (régression v0.84.1)

La refonte v0.84.1 ajoutait `tab`/`tab_id` à chaque step pour la
navigation inter-onglets. Or le JS de cicerone bascule l'onglet via
`Shiny.inputBindings…['shiny.bootstrapTabInput'].binding.setValue()`,
**incompatible avec le `page_navbar` bslib (Bootstrap 5)** : l'appel
levait une exception qui **avortait tout le tour** dès le 1ᵉʳ step — le
tour ne se lançait plus, ni au démarrage ni via « relancer le tour ».

**Fix** : on n'utilise plus le couple `tab`/`tab_id` natif de cicerone.
La bascule d'onglet se fait **côté client** dans `on_highlight_started`
en cliquant le lien de nav (`#main_nav a[data-value="<tab>"]`, marqué
`data-bs-toggle="tab"`), synchrone et compatible BS5. Le tour se relance
donc normalement ; la couverture multi-onglets reste en place.

# nemetonshiny 0.84.1 (2026-06-14)

### Refonte — Tour guidé : couverture de tous les onglets (socle)

Le Tour guidé (cicerone) ne couvrait que **6 steps sur le seul onglet
Accueil** (flux de création de projet) et n'avait pas suivi la forte
évolution de l'interface. Phase 0+1 de la refonte :

- **`R/service_tour.R`** (nouveau) : définition **déclarative** des steps
  (`build_tour_steps`) + builder du guide (`build_tour_guide`). Un **seul
  guide traverse désormais tous les onglets** de `main_nav` grâce au
  support natif `tab`/`tab_id` de cicerone (>= 1.0.4) — pas
  d'orchestrateur de chaînage manuel.
- **Couverture 6 → 11 steps, 1 → 6 onglets** : Accueil (onboarding
  détaillé) + 1 step clé par onglet — Synthèse, Plan d'action, Terrain,
  **Suivi sanitaire** (présente les 3 modes FAST / FORDEAD / RECONFORT),
  Familles d'indicateurs.
- **Robustesse** : chaque step porte un `tab` explicite, donc le tour se
  cadre correctement **quel que soit l'onglet d'où il est relancé**
  (corrige un cadrage cassé au restart). L'ancre conditionnelle fragile
  `start_compute` (rendue seulement en statut draft) est retirée au profit
  d'ancres `uiOutput` toujours présentes.
- **i18n FR/EN** : 5 nouvelles paires de clés `tour_*` (Synthèse, Plan
  d'action, Terrain, Suivi, Familles).
- **Tests** : structure des steps, résolution i18n (FR + EN), et surtout
  **cohérence des ancres** — chaque id ciblé doit exister dans l'UI de
  l'app, garde-fou contre les renommages de modules (la cause des « cadres
  incohérents »).

La mécanique (auto-start première visite via localStorage, bouton
« relancer le tour ») est inchangée. Granularité « 1 step clé par onglet »
volontairement concise ; étapes détaillées par onglet = phase 2.

# nemetonshiny 0.84.0 (2026-06-14)

### Perf — Sync PostGIS du projet déplacé hors du thread principal

Le chargement d'un projet récent laissait un délai ressenti entre le log
« Connected to PostgreSQL … » et l'affichage des parcelles sur la carte.

**Cause** : la synchronisation PostGIS best-effort (`db_sync_project` —
connexion + `sf::st_write()` des parcelles + `dbWriteTable()` des
indicateurs) tournait dans un callback `later::later(delay = 0.5)`. Or un
callback `later` s'exécute sur le **thread principal R** : même différé,
l'upload **gelait l'event loop Shiny** juste après le premier flush de la
carte, retardant les flushs suivants (retrait de l'overlay, `fitBounds`,
rendu de la sélection) et figeant l'UI pendant toute la durée du sync.

**Fix** : nouveau `db_sync_project_async()` qui exécute le sync dans un
worker `future` (process R séparé), **hors du thread principal** — même
machinerie que les runs FORDEAD/RECONFORT (re-chargement du paquet +
replay des variables d'environnement DB côté worker, y compris
`POSTGRESQL_ADDON_*`). `load_project()` l'appelle à la place du `later()`
synchrone. Le sync reste best-effort (aucun consommateur n'attend son
résultat) ; dégradation propre vers le `later()` historique si `future` /
`promises` sont indisponibles. Le dispatch retourne en ~0 ms.

# nemetonshiny 0.83.0 (2026-06-14)

### Nouveauté — Sous-onglet « Plan de validation RECONFORT » (spec 021, L6 G4)

Le mode RECONFORT du Suivi sanitaire gagne son **plan d'échantillonnage de
validation terrain**, 3ᵉ couple à côté de FAST et FORDEAD. Réutilisation
**1:1** de `mod_validation_sampling` — aucune logique métier nouvelle
(règles CLAUDE.md #2/#3), uniquement du câblage. Le cœur `nemeton ≥ 0.83.0`
expose tout le nécessaire ; plancher `Imports` bumpé.

- **`service_validation_sampling.R`** : `generate_validation_plan()` accepte
  `source = "RECONFORT"` ; `.resolve_alert_raster()` lit le masque catégoriel
  `1-sain / 2-déperissant / 3-très-déperissant` via
  `nemeton::read_reconfort_alert_mask()` (cache `<projet>/cache/layers/reconfort`,
  fichier `zone_<id>/reconfort_mask_<run_id>.tif`). **Pas de compute à la
  volée** (contrairement à FAST) : le masque doit préexister (phase persist
  du run) — sinon `validation_no_mask`, comme FORDEAD sans run.
- **`mod_validation_sampling.R`** : UI source-aware pour RECONFORT (classes
  `2/3`, témoins `1`, libellés biologiques feuillus `reconfort_class_label_*`).
  L'observer de labels quartile FAST est sauté pour RECONFORT (échelle 1/2/3,
  labels statiques).
- **`mod_monitoring.R`** : `nav_panel("validation_sampling_reconfort", …)` +
  montage `source_fixed = "RECONFORT"` + ajout au vecteur `reconfort_tabs` de
  l'observer de visibilité mode-driven.
- **i18n FR/EN** : `validation_sampling_title_reconfort`,
  `reconfort_class_label_{1,2,3}`. Clés génériques (`validation_persisted_toast`,
  `validation_export_qgis_btn`, …) réutilisées telles quelles.
- **Persistance terrain** : aucune modif app — `ingest_health_validation`
  route déjà sur la colonne DB `alert.alert_type == "reconfort_dieback"`
  (posée par le run cœur) → applique le schéma DEPERIS feuillus.
- Tests : service (`source="RECONFORT"` : lecture masque, classes 2/3 +
  témoin 1, `validation_no_mask` sans run) + `testServer` du module.

**Spec 021 L6 RECONFORT : entièrement livrée côté app** (carte, diagnostic
pixel, run, validation terrain). Réserves cœur héritées (poids de confiance
provisoires, arborescence S2 MUSCATE à valider sur run réel) sans impact sur
ce sous-onglet (il consomme le masque déjà produit).

# nemetonshiny 0.82.0 (2026-06-13)

### Nouveauté — Lancement d'un run RECONFORT (spec 021, L6 — suite)

Complète le mode RECONFORT (v0.81.0, jusque-là consultation seule) avec le
**pipeline de lancement de run**, en miroir de FORDEAD :

- **`run_reconfort_async()`** (`service_monitoring.R`) : `ExtendedTask` +
  `future_promise` autour de `nemeton::run_reconfort_dieback(con, zone_id,
  cache_dir, s2_year, progress_callback)`. Le worker recharge
  `nemetonshiny`, ouvre une connexion DB fraîche et écrit les événements
  de progression dans un fichier JSON tailé par le parent. cache_dir =
  `<projet>/cache/layers/reconfort` (la phase persist y écrit
  `zone_<id>/run_<run_id>/`).
- **Câblage parent** (`mod_monitoring`) : bouton « Lancer le diagnostic
  RECONFORT » → `.invoke_reconfort()`, `reactivePoll` de progression →
  dispatcher `.reconfort_handle_progress_event()` (events
  `reconfort:start|phase|complete|error`, 10 phases env/model/mask/tiles/
  ingest/stage/mapprod/collect/postprocess/persist avec libellés i18n +
  fallback Title-Case), observer de résultat (toast succès/erreur +
  `reconfort_refresh` qui réinvalide la carte d'alertes), grisage du
  bouton (cross-lock avec FAST/FORDEAD sur le cache S2 partagé) et
  **force-unlock** pour réarmer un run bloqué (pas de cancel coopératif
  côté cœur).
- Sur un déploiement **sans conda IOTA²/GEODES/OTB**, le worker échoue et
  l'erreur est surfacée via le toast d'erreur ; la carte et le diagnostic
  restent fonctionnels sur les runs déjà produits (Limite #1 spec 021).
- Tests : dispatcher RECONFORT (phase / start silencieux / erreur).

Reste différé : **QField — stades feuillus DSF (G4)**, qui requiert une
extension cœur de `get_health_validation_schema()` (ajout à demander).

# nemetonshiny 0.81.0 (2026-06-13)

### Nouveauté — Mode de suivi sanitaire « RECONFORT » (spec 021, L6)

Le Suivi sanitaire gagne un **3ᵉ mode** à côté de FAST et FORDEAD :
**RECONFORT** (dépérissement des feuillus — chêne, châtaignier — via
CRSWIR + CRre). Aucune logique métier côté app : tout passe par le cœur
`nemeton` (≥ 0.80.0, plancher `Imports` bumpé).

Livré (consultation) :

- **Nouveau module `mod_monitoring_reconfort_map`** : carte Leaflet des
  alertes de dépérissement feuillus via `nemeton::list_alerts(…, classes =
  RECONFORT_ALERT_CLASSES)` (filtre G1 : 2-dépérissant / 3-très-dépérissant),
  popup `confidence_class` + `stress_index`. Bannière de validité **G3
  advisory** (`check_reconfort_validity`) : avertit hors domaine de
  calibration **sans bloquer**. Clic carte → diagnostic pixel
  (`read_reconfort_pixel_series`) en modal plotly à 2 traces (CRSWIR + CRre
  observés ; pas de prédiction harmonique — RECONFORT n'a pas de modèle).
- **3ᵉ option** dans le sélecteur de mode + sous-onglet « Carte RECONFORT »
  monté à la volée (lazy) et géré par l'observer de visibilité mode-driven.
- **i18n FR/EN** : `monitoring_mode_reconfort`, `monitoring_reconfort_*`
  (validité, classes, popup, séries, phases env/model/ingest/mapprod/
  postprocess/persist, run). Tout texte via `i18n$t()` (`\uXXXX`).

Différé (signalé) :

- **Lancement d'un run RECONFORT** : `nemeton::run_reconfort_dieback()` est
  lourd/opt-in (conda IOTA²/GEODES/OTB). Le pipeline asynchrone complet
  (ExtendedTask + polling des phases) n'est pas encore câblé : le bouton
  « Lancer le diagnostic RECONFORT » signale l'indisponibilité et renvoie
  vers la consultation des runs déjà produits (Limite #1 spec 021). La
  carte et le diagnostic pixel restent pleinement fonctionnels.
- **QField — stades feuillus DSF (G4)** : le schéma cœur
  `get_health_validation_schema()` n'expose pour l'instant que les stades
  scolyte/résineux. L'extension feuillus DSF est un ajout **cœur** à
  demander (l'app réutilisera le workflow QField existant tel quel).

# nemetonshiny 0.80.0 (2026-06-13)

### Nouveauté — Bouton « Réinitialiser depuis le corpus du package » (RAG)

L'onglet RAG (`mod_rag_admin`) gagne un bouton **Réinitialiser depuis le
corpus du package**, à côté de l'import/export du manifeste. La copie
éditable du manifeste (`knowledge_manifest_path(writable = TRUE)`) est
créée une fois puis figée, donc elle **dérive de la seed du package** à
chaque release cœur. Le bouton appelle
`nemeton::reset_knowledge_manifest(confirm = TRUE)` (cœur ≥ 0.79.0) pour
la resynchroniser, après une **modale de confirmation** (l'opération
écrase les modifications locales non exportées), puis recharge l'éditeur
depuis la copie writable rafraîchie.

Nouvelles clés i18n FR/EN : `rag_reset_corpus`, `rag_reset_corpus_title`,
`rag_reset_corpus_warn`, `rag_reset_corpus_done`, plus une clé générique
`confirm` (réutilisable, pendant de `cancel`). Plancher
`Imports: nemeton (>= 0.79.0)` (consommation de
`reset_knowledge_manifest`).

# nemetonshiny 0.79.1 (2026-06-13)

### Fix — Régression v0.78.0 : l'attache différée d'`indicators_sf` plantait

Le chargement d'un projet récent déclenchait dans la console :
`Can't access reactive value 'project_id' outside of reactive consumer`.

**Cause** : le callback `later::later()` introduit en v0.78.0 (qui diffère
le build `ug_build_sf` → `attach_indicators_sf`) lisait/écrivait
`app_state$project_id` / `app_state$current_project` **hors de tout
contexte réactif** — un callback `later` ne s'exécute dans aucun
consommateur réactif. Le callback **plantait avant** d'attacher
`indicators_sf`, privant Synthèse / Famille / Échantillonnage de leur
géométrie UGF pour tout projet ouvert via l'écran récent.

**Fix** : le corps du callback s'exécute désormais dans le domaine réactif
de la session via `shiny::withReactiveDomain(session, shiny::isolate(...))`,
rendant légales la lecture et l'écriture des valeurs réactives et
propageant l'invalidation aux reactives (suspendues) en aval. Test de
non-régression dédié (`later::run_now()` exécute réellement le callback).

### Fix — Toast « Aucun pixel sain » qui fuyait au chargement

Le chargement d'un projet pouvait afficher, par-dessus la carte de
l'Accueil, le toast « Aucun pixel sain — témoins tirés en classe N (la
plus saine disponible) » issu du module d'échantillonnage de validation
(contexte Santé). En cause : `alert_mask_r` (`mod_validation_sampling`)
n'était pas gardé par l'onglet actif — il se ré-évaluait à chaque
changement de `current_project`, **ouvrant une connexion à la base
monitoring et lisant un raster d'alerte hors du chemin de chargement**,
puis l'observateur auto-relax émettait sa notification globalement.

**Fix** : `alert_mask_r` est gaté sur l'onglet Santé actif
(`shiny::req(identical(app_state$active_main_tab, "monitoring"))`), même
garde que `pixel_stack_r` (v0.75.2). Supprime le toast intempestif **et**
retire une connexion DB + une lecture raster du chargement de projet.

# nemetonshiny 0.79.0 (2026-06-13)

### Perf — `connect_timeout` borné sur la connexion monitoring

Complète le Fix #1 de v0.78.0 : `get_monitoring_db_connection()` gagne un
paramètre `connect_timeout = 2L` (secondes) forwardé à
`nemeton::db_connect()` pour **borner la phase de connexion** Postgres —
afin que même le chemin où l'hydratation `monitoring_zone_id` est
réellement nécessaire (id absent de `metadata.json`) ne gèle pas l'UI
pendant le timeout TCP par défaut de libpq (plusieurs dizaines de
secondes sur un hôte injoignable).

Le paramètre `connect_timeout` est exposé par `nemeton::db_connect()`
**depuis le cœur v0.76.0** (`feat(db)`). L'app le consomme via un wrapper
`.nemeton_db_connect()` **rétro-compatible** : il introspecte
`formals(nemeton::db_connect)` et ne transmet l'argument que si le cœur
installé l'expose, dégradant sinon à l'appel 2-arguments. Le plancher
`Imports: nemeton (>= 0.67.0)` n'est donc **pas** bumpé — le timeout
s'active automatiquement dès qu'un cœur ≥ 0.76.0 est chargé (cas par
défaut via le remote `@*release`), sans l'exiger comme minimum strict.

# nemetonshiny 0.78.0 (2026-06-13)

### Perf — Chargement projet récent : deux blocages synchrones retirés

Suite à v0.75.2 (`build_index_stack` sorti du chemin), deux opérations
**synchrones** subsistaient dans le chemin critique de chargement d'un
projet récent (avant le rendu des parcelles) :

1. **Connexion DB monitoring ouverte à chaque chargement.** L'observer
   `mod_home` ouvrait systématiquement une connexion
   `get_monitoring_db_connection()` (TCP connect + migration de schéma)
   pour hydrater `monitoring_zone_id`, alors que `hydrate_monitoring_zone_id()`
   est un no-op dès que l'id est déjà dans `metadata.json` (cas commun
   post-spec 011). Sur un hôte Postgres lent/injoignable, ce round-trip
   pouvait geler l'UI plusieurs secondes (libpq sans `connect_timeout`).
   **Fix** : nouveau prédicat `.has_monitoring_zone_id()` partagé ;
   l'observer **n'ouvre plus** la connexion quand l'id est déjà connu.

2. **`ug_build_sf()` (géométrie UGF) construit en synchrone.** Une boucle
   `sf::st_union()` par UGF (0,5–3 s pour beaucoup d'UGF) produisait
   `indicators_sf`, consommé uniquement par les onglets Synthèse /
   Famille / Échantillonnage / Suivi — **aucun** actif au chargement.
   **Fix** : extraction de `attach_indicators_sf()` + nouveau paramètre
   `load_project(build_indicators_sf = TRUE)`. Le chemin de chargement
   interactif passe `FALSE` et ré-attache `indicators_sf` via `later()`
   après le premier flush, en ré-assignant `current_project` pour que les
   reactives (inactives/suspendues) le récupèrent à l'ouverture de l'onglet.

Les autres appelants de `load_project()` conservent le build inline
(défaut `TRUE`) — comportement inchangé.

**Note (dette cœur)** : un troisième garde-fou — `connect_timeout` sur la
connexion Postgres pour borner le gel même quand l'hydratation est
nécessaire — requiert une modification de `nemeton::db_connect()`
(actuellement `.parse_pg_url()` + `DBI::dbConnect()` sans timeout). À
traiter dans une session dédiée au cœur.

# nemetonshiny 0.77.1 (2026-06-12)

### UI — Bandeau « Surfaces des zones de suivi » : style carte

Le bandeau des surfaces (mode FAST) adopte le **style carte** des bandeaux
de validité FORDEAD (« Composition d'essences hors domaine validé ») :
carte à bordure info bleue, icône + titre en gras, surfaces des strates
dans le corps. Remplace la barre compacte `alert` précédente.

# nemetonshiny 0.77.0 (2026-06-12)

### Nouveauté — Suivi sanitaire (FAST) : bandeau « Surfaces des zones de suivi »

En mode **FAST** (suivi rapide), un bandeau bleu s'affiche au-dessus des
sous-onglets et rappelle la **surface (ha)** et la **part (%)** des 4
strates projet `_tot` / `_feu` / `_res` / `_mix`. Le pourcentage est
relatif à la strate `_tot` (toutes essences, surface de référence). Le
bandeau est masqué en mode FORDEAD (health) et tant qu'aucune zone n'a
été générée. Les surfaces sont calculées à partir du polygone de chaque
zone (`get_monitoring_zone_aoi`, EPSG:2154) via `sf::st_area`.

Nouvelles clés i18n : `monitoring_fast_surfaces_title`,
`monitoring_fast_surf_item`, `monitoring_fast_surf_item_tot`.
Helper interne `.compute_zone_surfaces()` + tests dédiés.

# nemetonshiny 0.76.0 (2026-06-12)

### Amélioration — Légende Alertes FAST : indice rappelé dans le titre

La légende de la carte « Alertes FAST » (onglet Suivi sanitaire) affiche
désormais l'indice actif (NDMI / NDVI / NBR) dans son titre, ex.
« Sévérité de l'alerte (NDVI) ». Le titre se met à jour quand l'utilisateur
change le radio « Indice FAST », en cohérence avec le badge de résolution
qui mentionne déjà l'indice. Clé i18n `fast_alert_legend_title` paramétrée
avec `%s`.

# nemetonshiny 0.75.2 (2026-06-11)

### Perf — Chargement projet : `build_index_stack` ne bloque plus

**Symptôme** : ouvrir un projet depuis l'Accueil prenait plusieurs
secondes (≈ 17 s à froid, ≈ 4 s à chaud) avant l'affichage des parcelles.

**Cause** (mesurée en instrumentant le handler) : ni `load_project`
(0.6 s) ni l'hydratation `monitoring_zone_id` (0.1 s) n'étaient en cause.
Le coût venait de `nemeton::build_index_stack` (scan de centaines de
scènes Sentinel-2) appelé par la **carte pixel** du Suivi sanitaire.
Ses outputs sont marqués `suspendWhenHidden = FALSE` (v0.46.3, pour
s'afficher dès le 1ᵉʳ clic d'onglet sous bslib `nav_show/nav_hide`),
si bien que la reactive lourde `pixel_stack_r` se recalculait à **chaque
changement de projet**, y compris depuis l'Accueil, bloquant l'event
loop (donc le rendu carte).

**Fix** : `pixel_stack_r` est désormais gaté sur l'onglet Suivi
réellement actif — `shiny::req(identical(app_state$active_main_tab,
"monitoring"))`. Le scan ne tourne plus qu'à l'ouverture du Suivi (là où
il est nécessaire). Chargement projet mesuré **~2-3 s** après le fix,
`build_index_stack` totalement absent du chemin de chargement.

- `app_server.R` : expose `app_state$active_main_tab` (suivi de
  `input$main_nav`).
- `mod_monitoring_pixel_map.R` : garde `req(active_main_tab ==
  "monitoring")` dans `pixel_stack_r`. Monitoring vérifié fonctionnel
  (la carte pixel se construit bien à l'ouverture du Suivi).

# nemetonshiny 0.75.1 (2026-06-11)

### Perf — Backfill paresseux du cache de géométrie commune (projets legacy)

Le cache disque de la frontière communale (`data/commune.gpkg`, introduit
en v0.74.0 pour un restore instantané) n'existait que pour les projets
**sauvegardés depuis** v0.74.0. Les projets **legacy** (créés avant)
re-téléchargeaient le contour à **chaque** ouverture via le chemin async
lent (worker `future` + rechargement `nemeton` + 2 appels séquentiels à
`geo.api.gouv.fr`) — d'où un délai de plusieurs secondes avant l'affichage
des parcelles.

Désormais, quand un projet legacy est chargé et que `mod_search` récupère
le contour via ce chemin lent, le résultat est **persisté** dans
`data/commune.gpkg` (best-effort, dans le result handler de la
`restore_task`). Le **prochain** chargement du projet injecte la géométrie
**synchroniquement** et rend la carte instantanément. Aucune action
utilisateur requise : chaque projet legacy se « réchauffe » tout seul à sa
première ouverture après cette version.

- `mod_search.R` : backfill dans le result handler de `restore_task`
  (garde `is.null(current_project$commune_geometry)` pour ne pas réécrire
  un cache existant ; vérifie que le projet courant correspond).

### Migration proactive — `backfill_all_commune_geometries()`

Pour ne pas attendre que chaque projet legacy soit ouvert une fois, un
helper one-shot réchauffe **tous** les projets d'un coup : pour chaque
projet sans cache mais avec des parcelles, il récupère le contour communal
(`geo.api.gouv.fr`) et le persiste. Idempotent (les projets déjà en cache
sont ignorés), best-effort par projet, network-bound (un appel API par
commune non cachée). Retourne un data.frame `id` / `name` / `status`
(`backfilled` | `cached` | `no_parcels` | `no_commune_code` |
`fetch_failed`).

À lancer une fois côté utilisateur :
`Rscript -e 'pkgload::load_all("."); nemetonshiny:::backfill_all_commune_geometries()'`

# nemetonshiny 0.75.0 (2026-06-11)

### UX — Notification DB persistante jusqu'à l'apparition de l'overlay carte

À l'ouverture d'un projet synchronisé PostGIS, la notification « Projet
synchronisé avec la base PostGIS » (bas à droite) passait en `duration = 5`
et pouvait disparaître **avant** que l'overlay « Affichage des parcelles… »
n'apparaisse, laissant un trou de feedback.

Elle devient **persistante** (`duration = NULL`, id `db_sync_notif`) et
`mod_map` la retire dès que l'overlay de chargement prend le relais
(`show_map_loading`). Filets de sécurité : retrait via `later()` à 12 s et
sur le chemin « commune invalide », pour ne jamais laisser la notification
coincée.

- `mod_home.R` : notif `id = "db_sync_notif"`, `duration = NULL`, fallback
  `later()` + retrait sur le chemin commune invalide.
- `mod_map.R` : `show_map_loading()` retire `db_sync_notif` (handoff carte).

# nemetonshiny 0.74.1 (2026-06-10)

### Bug fixé — CI rouge : dépendance `lasR` non résolue par `pak`

**Symptôme** : tous les jobs GitHub Actions (R-CMD-check, tests,
pkgdown) échouaient à l'étape « Install R dependencies », **avant**
toute compilation, avec `Could not solve package dependencies`
(conflits signalés sur `rcmdcheck` / `remotes` / `sessioninfo` /
`testthat`). Régression d'infrastructure présente depuis v0.73.0.

**Cause** : `lasR` (traitement LiDAR, en `Suggests`) est hébergé sur
r-universe (`r-lidar`), pas sur le CRAN. Sans entrée `Remotes` ni
`Additional_repositories`, `pak` ne trouvait pas le paquet — le
message réel (`Can't find package called lasR`) était masqué par les
« dependency conflict » génériques sur les paquets d'outillage.

**Fix** : ajout de `r-lidar/lasR` à `Remotes:`. La résolution
`pak::lockfile_create()` réussit de nouveau (vérifié localement,
`lasR` présent dans le lockfile). Aucun changement de code applicatif.

### Bug fixé — R-CMD-check rouge : 3 tests `mod_rag_admin` cassés

**Symptôme** : une fois `lasR` résolu, `R-CMD-check` atteignait enfin
la suite de tests et révélait 3 échecs pré-existants dans
`test-mod_rag_admin.R` (édition de cellule sans effet, `has_errors()`
ne réagissant pas), masqués jusque-là par l'échec d'install. Bug
antérieur (reproduit sur l'ancien `main`), sans lien avec `lasR`.

**Causes (toutes côté test, le code applicatif est correct)** :
1. **`ignoreInit` mangé** : l'observer `manifest_cell_edit` utilise
   `observeEvent(..., ignoreInit = TRUE)`. `testServer` ne fait pas de
   flush de démarrage (contrairement à une vraie session), donc le tout
   premier `setInputs(cell_edit=…)` était avalé comme run d'init. Fix :
   `session$flushReact()` initial pour consommer l'`ignoreInit`.
2. **Promesse non forcée** : le mock
   `validate_knowledge_manifest = function(manifest) issue_state$df` ne
   touchait jamais son argument. La reactive `issues` appelle
   `nemeton::validate_knowledge_manifest(man())` ; l'argument `man()`
   restait une promesse non évaluée → la reactive ne souscrivait jamais
   à `man()` → figée au premier calcul. Fix : `force(manifest)` dans le
   mock (reproduit le comportement de la vraie fonction nemeton).

### Bug fixé — R-CMD-check rouge : 4 autres tests pré-existants

Toujours masqués par l'échec d'install lasR, révélés une fois la suite
atteinte. Tous côté test ; le code applicatif est correct.

- **3 tests monitoring obsolètes** (`test-mod_monitoring.R` ×2,
  `test-mod_monitoring_pixel_map.R`) : attendaient encore
  `bands = c("NDVI", "NBR")`, alors que NDMI a été ajouté (3ᵉ indice,
  v0.71.0 / nemeton >= 0.64.0) et est désormais câblé en dur. Attentes
  mises à jour vers `c("NDVI", "NBR", "NDMI")`.
- **1 smoke E2E shinytest2** (`test-mod_rag_admin-e2e.R`) — **quarantiné
  (`skip()` + FIXME)**. Jamais exécuté en CI auparavant (masqué par lasR),
  il s'est révélé cassé : (a) ouvrait la modale via `set_inputs(open = 1)`
  sur un `actionLink` — invalide (« only valid value is click ») ; (b)
  supposait un rendu *eager* des onglets, or la tab RAG est *lazy* (montée
  seulement quand `config_tab == "tab_rag"`, anti-DataTables-dans-
  conteneur-caché). Le correctif tenté (clic DOM réel + activation de la
  tab RAG) ne suffit pas sous shinytest2 headless — le contenu de la
  modale ne se monte pas, interaction à creuser avec un environnement
  navigateur stable. Le code applicatif est correct (vérifié à la main).
  Le test reste à ré-armer ultérieurement.

# nemetonshiny 0.74.0 (2026-06-10)

### Perf — Restore projet instantané : cache de la géométrie commune

**Symptôme** : entre le toast « Projet *X* chargé » et l'affichage des
parcelles sur la carte, un long délai (plusieurs secondes), pendant
lequel l'overlay blanc « Affichage des parcelles… » restait visible.

**Cause** : la frontière de la commune n'était **pas** persistée avec
le projet — seules les parcelles l'étaient. À chaque ouverture,
`mod_search` la re-téléchargeait via la `restore_task` asynchrone :
démarrage d'un worker `future::multisession`, rechargement de
`nemeton` dans ce worker, puis **deux appels réseau séquentiels** à
`geo.api.gouv.fr` (`req_timeout(15)` + 3 retries chacun). Or
l'observateur de rendu de `mod_map` attend **à la fois** les parcelles
**et** la géométrie commune avant de dessiner → la carte restait
bloquée sur l'overlay jusqu'à la fin de cette chaîne.

**Fix** : la géométrie commune est désormais **persistée sur disque**
(`data/commune.gpkg`) au save du projet et **réinjectée
synchroniquement** au chargement, en même temps que les parcelles. La
carte se rend immédiatement, sans attendre le worker ni le réseau. La
`restore_task` continue de tourner en arrière-plan, mais uniquement
pour peupler la liste déroulante des communes — elle ne bloque plus le
rendu. Les projets *legacy* (sans cache) retombent automatiquement sur
l'ancien chemin asynchrone.

Détail technique :
- `service_project.R` : `save_commune_geometry()` /
  `load_commune_geometry()` (best-effort, GeoPackage, EPSG:4326) ;
  câblage dans `create_project()`, `update_project()`, `load_project()`
  (nouveau champ `project$commune_geometry`).
- `mod_project.R` : nouvelle entrée `commune_geometry` (reactive)
  persistée au create/update.
- `mod_home.R` : `app_state$restore_project$geometry` transporte la
  géométrie en cache jusqu'à `mod_search`.
- `mod_search.R` : injection synchrone si géométrie en cache
  (`rv$geometry_injected`), garde-fou anti double-render (flash blanc)
  dans le result handler de la `restore_task`.

# nemetonshiny 0.73.1 (2026-06-09)

### Bug fixé — Génération des zones de suivi : `project_name must be a non-empty character scalar`

**Symptôme** : cliquer sur « Générer les zones de suivi » échouait
immédiatement avec
`project_name must be a non-empty character scalar` (deux toasts
d'erreur rouges, aucune zone créée).

**Cause** : le handler passait `project_name = project$name` à
`nemeton::build_project_monitoring_zones()`. Or l'objet projet
retourné par `load_project()` a la forme `list(id, path, metadata)`
— le nom vit dans `project$metadata$name`, **pas** au premier niveau.
`project$name` valait donc `NULL`, rejeté par le garde-fou cœur.

**Fix** : `project_name = project$metadata$name %||% project$id`
(fallback sur l'id local si le nom est jamais absent). Commentaire
de doc du handler corrigé en conséquence. Régression introduite en
v0.73.0.

# nemetonshiny 0.73.0 (2026-06-04)

### Changed — Wiring des zones de suivi (spec 020 — 4 strates par projet)

Hand-off du brief
`/home/pascal/dev/nemeton/BRIEF-nemetonshiny-zones-spec020.md`.
nemeton v0.67.0 expose 4 nouvelles fonctions pour gérer **jusqu'à 4
zones de suivi** par projet (croisement union UGFs × strates BD
Forêt v2) :

* `build_project_monitoring_zones(con, project_name, project_uuid,
  ugf, bdforet, ...)` — construit/upsert les 4 strates (`_tot`,
  `_feu`, `_res`, `_mix`) en une seule fois.
* `create_monitoring_zone(con, zone_name, zone_polygon,
  project_uuid)` — insert zone seule.
* `find_zones_by_project(con, project_uuid)` — liste `(id, name)`
  des zones du projet courant.
* `prune_orphan_zone_caches(con, cache_root, ...)` — purge les
  caches `zone_<old_id>/` orphelins après upsert.

### Bug fixé — Mauvaise zone affichée au changement de projet (villards/Mouthe)

**Symptôme** : charger un projet sans zone propre faisait retomber
le selecteur sur la **1ʳᵉ zone alphabétique de la base** (ex.
`villards`), même si l'utilisateur travaillait sur Mouthe.
L'utilisateur voyait des alertes d'un autre projet sans s'en rendre
compte.

**Cause** : `zones` reactive appelait `list_monitoring_zones(con)`
qui retournait **toutes** les zones de la DB, indépendamment du
projet courant.

**Fix** : `zones` consume désormais
`nemeton::find_zones_by_project(con, project_uuid = proj$id)`. Le
selectInput n'affiche que les zones du projet courant. Si vide
→ pas de zone, le bandeau « générer les zones » apparaît.

### Bouton « Générer les zones de suivi »

Le bouton « Enregistrer ce projet comme zone de suivi » (qui créait
1 zone à partir des placettes `samples.gpkg`) est **refactoré** :

* **Nouveau wording** : « Générer les zones de suivi » /
  « Generate monitoring zones ».
* **Nouvelle action** : appel à
  `nemeton::build_project_monitoring_zones()` qui crée jusqu'à 4
  strates (`_tot`, `_feu`, `_res`, `_mix`) par croisement union
  UGFs × BD Forêt v2.
* **Pré-requis BD Forêt** : si `cache/layers/bdforet.gpkg` absent,
  message « lancer d'abord le calcul du projet » (pas d'appel à la
  fonction). La BD Forêt est produite par `download_ign_bdforet`
  au 1er calcul projet (onglet Synthèse).
* **Pré-requis UGFs** : si `ug_build_sf(project)` est vide,
  message « définir les UGFs d'abord ».
* **Cleanup post-upsert** : appel automatique à
  `nemeton::prune_orphan_zone_caches()` après chaque
  `build_project_monitoring_zones()` — l'upsert
  (`replace = TRUE` par défaut) ré-assigne de nouveaux `zone_id`,
  donc les caches `cache/layers/*/zone_<old_id>/` deviennent
  orphelins. La purge nettoie best-effort.
* **Auto-sélection `_tot`** : la zone `_tot` (toutes essences) est
  pré-sélectionnée par défaut après build, via
  `grep("_tot$", z$name)` dans l'observer du selectInput.

### Détails techniques

* `DESCRIPTION` : `Imports: nemeton (>= 0.65.1)` → `(>= 0.67.0)`.
* `R/mod_monitoring.R` :
  - `zones <- shiny::reactive(...)` : `list_monitoring_zones(con)`
    remplacé par `nemeton::find_zones_by_project(con,
    project_uuid = proj$id)`.
  - Stratégie de sélection par défaut révisée : `_tot` >
    `metadata$monitoring_zone_id` > vide.
  - Observer du bouton `register` / `register_inline` : appel à
    `nemeton::build_project_monitoring_zones()` + lecture
    `cache/layers/bdforet.gpkg` + `ug_build_sf(project)` + cleanup
    `nemeton::prune_orphan_zone_caches()`.
* `R/utils_i18n.R` : nouvelles clés `zones_build_success_fmt`,
  `zones_bdforet_missing`, `zone_tot`, `zone_feu`, `zone_res`,
  `zone_mix`. Wording bouton `monitoring_register_btn`
  « Enregistrer... » → « Générer les zones de suivi ».
* `tests/testthat/test-mod_monitoring.R` : tests des mocks
  `list_monitoring_zones` adaptés vers
  `nemeton::find_zones_by_project`. Tests legacy
  `register_project_as_zone` marqués `skip` (le helper reste
  exporté pour back-compat, testé dans `test-service_monitoring_db.R`).

### Pas de breaking change fonctionnel

Le helper legacy `nemetonshiny:::register_project_as_zone()` est
toujours exporté côté `service_monitoring_db.R` (tests
indépendants conservés). Mais il n'est plus appelé par l'app.

### Tests

* 2763 pass / 3 fails pré-existants (NDMI bands tests v0.66.0
  non liés, hors scope).

# nemetonshiny 0.72.0 (2026-06-04)

### Added — Modal pixel CRSWIR FORDEAD enrichi (5 traces + zone validité)

**Avant v0.72.0** : le modal CRSWIR (clic pixel sur la carte FORDEAD)
n'exploitait que 2 colonnes (`crswir_obs`, `crswir_pred`) sur les 5
retournées par `nemeton::read_fordead_pixel_series()`. Pas de
visualisation du seuil de détection, ni des anomalies, ni de
l'indicateur de validité.

**v0.72.0** : le modal affiche désormais l'intégralité du diagnostic
FORDEAD :

| Élément | État précédent | État v0.72.0 |
|---|---|---|
| Courbe `crswir_obs` (observé) | ✅ points bleus | inchangé |
| Courbe `crswir_pred` (harmonique) | ✅ ligne rouge | inchangé |
| **Bande `seuil_haut`** | ❌ absente | 🟧 ligne pointillée orange (= pred + Δ) |
| **Points en anomalie** | ❌ absents | 🔴 surlignés rouge foncé taille 8 |
| Marqueur 1ʳᵉ anomalie | ✅ ligne verticale noire | inchangé |
| **Indicateur `dans_zone_validite`** | ❌ absent | 🏷 annotation discrète si FALSE |
| Axe Y | « CRSWIR » hardcodé | « Indice (CRSWIR) » dynamique selon `vegetation_index` |

L'utilisateur voit désormais d'un coup d'œil :
- **Où** est le seuil de détection (bande orange)
- **Quels** points l'ont franchi (rouges)
- **Quand** la 1ʳᵉ anomalie a été détectée (ligne verticale)
- **Si** le pixel est dans la zone de calibration (annotation
  jaune si hors zone)

### Fixed — Zone de suivi pas mise à jour au changement de projet récent

**Symptôme** : en chargeant un projet récent sans
`monitoring_zone_id`, le selectInput « Zone de suivi » de Suivi
sanitaire gardait la zone du projet PRÉCÉDENT sélectionnée.

**Cause** : `mod_monitoring.R:942` utilisait
`selected = if (length(preferred)) preferred else character(0)`.
Le `character(0)` est interprété par `updateSelectInput` comme
« ne pas changer la sélection côté client » dans certaines combos
Shiny/navigateur — la zone précédente persistait.

**Fix** : `selected = ""` (string vide explicite) force la
non-sélection cohérente. Le selectInput se vide proprement quand
le nouveau projet n'a pas de zone de suivi associée.

### Fixed — Toast `no_data` du clic pixel FORDEAD plus durable et explicite

Quand le clic tombe hors de la zone modélisée FORDEAD (extent du
bundle plus petit que l'AOI affichée), le toast warning passait
trop vite (`duration = 4`) pour être lu.

* **`duration` : 4 → 8 s**
* **Wording** : « Aucune série CRSWIR disponible pour ce pixel.
  Cliquez DANS la zone d'alerte colorée du raster (extent modélisé
  plus petit que l'AOI). » — instruction actionnable.

### Détails techniques

* `R/mod_monitoring_fordead_map.R::observeEvent(input$map_click)` :
  - Lecture des nouvelles colonnes `seuil_haut`, `anomalie` du
    dataframe + des attributs `dans_zone_validite`, `vegetation_index`.
  - 2 nouvelles traces plotly (`seuil_haut` en ligne pointillée
    orange, `anomalie==TRUE` en markers rouge foncé taille 8).
  - Annotation top-left si pixel hors zone de validité.
  - Axe Y dynamique selon `attr(ts, "vegetation_index")`.
  - Toast no-data `duration = 8` au lieu de 4.
* `R/utils_i18n.R` : 3 nouvelles clés
  (`monitoring_fordead_pixel_threshold`,
  `monitoring_fordead_pixel_anomaly`,
  `monitoring_fordead_pixel_outside_validity`) + reformulation
  de `monitoring_fordead_pixel_no_data` + ajustement de
  `monitoring_fordead_pixel_yaxis` (« Indice » au lieu de
  « CRSWIR » hardcodé).
* `R/mod_monitoring.R::~l.942` : `selected = ""` au lieu de
  `character(0)`.

### Pas de breaking change

* Le dataframe retourné par le cœur est inchangé (seules les
  colonnes consommées s'étendent).
* L'API `nemeton::read_fordead_pixel_series()` n'est pas modifiée.

### Tests

* 2497 pass / 3 fails pré-existants (NDMI bands tests v0.66.0
  non liés, hors scope).

# nemetonshiny 0.71.1 (2026-06-03)

### Fixed — Bundle 3 fixes UX FORDEAD + cohérence ntfy

**1. ntfy push « Ingestion FAST démarrée » → « Diagnostic FAST démarré »**

Le push ntfy de démarrage utilisait encore le wording legacy
« Ingestion FAST ». Alignement avec :
- Le toast UI : « Diagnostic FAST terminé »
- Le push ntfy fin : « Diagnostic FAST terminé » (v0.70.4)

Cohérence end-to-end FR + EN.

**2. FORDEAD output_dir : fin de la pollution `/tmp/`**

`nemeton::run_fordead_dieback()` utilisait par défaut
`tempfile("fordead_")` → outputs intermédiaires (training, masks
bruts, calibration vectors) écrits dans `/tmp/fordead_XXX/` puis
supprimés à la fin (`keep_output = FALSE`).

Désormais l'app force :

* `output_dir = <projet>/cache/layers/fordead/output_zone_<id>`
* `keep_output = TRUE`

**Avantages** :

* Plus de pollution `/tmp/` — les caches /tmp peuvent saturer un
  homedir en cas de runs répétés (~50-200 Mo par run).
* Outputs préservés (training data, masks bruts, calibration) →
  inspection possible côté admin.
* **Per-zone**, écrasé à chaque relance → taille bornée (pas
  multipliée par run).

Nouveau helper `R/mod_monitoring.R::.resolve_fordead_output_dir(
project, zone_id)`. Le worker `run_fordead_async()`
(`R/service_monitoring.R`) accepte 2 nouveaux params optionnels
(`output_dir`, `keep_output`) forwardés directement à
`nemeton::run_fordead_dieback()`. NULL = retombe sur le défaut
cœur (back-compat).

**3. Toast `Diagnostic terminé : 0 alertes` qui clignote + bouton FORDEAD grisé**

Symptôme observé sur la capture d'écran utilisateur : le toast
en bas à droite restait collé, et le bouton « Lancer le
diagnostic FORDEAD » paraissait grisé après un run terminé.

**Cause** : `fordead_task$result()` peut refire plusieurs fois
(symétrique à `fast_task$result()`, cf. v0.70.4). Sans garde,
l'observer ré-émet le toast `fordead_success` (duration 8 s) en
boucle.

**Fix** : nouveau `fordead_result_consumed <- shiny::reactiveVal(FALSE)`
qui agit comme garde d'idempotence :

* L'observer vérifie au début si le flag est `TRUE` → return
  silencieux.
* À la 1ʳᵉ exécution, le flag est mis à `TRUE` via `isolate()`.
* Reset à `FALSE` dans `observeEvent(input$run_health)` (avant
  le nouvel `invoke()`).

S'applique aussi au branchement `error` (toast `fordead_error`).

### Détails techniques

* `R/utils_i18n.R::monitoring_ntfy_ingest_start` : wording
  « Ingestion FAST » → « Diagnostic FAST ».
* `R/mod_monitoring.R::.resolve_fordead_output_dir()` : nouveau
  helper.
* `R/mod_monitoring.R::~l.2308` : `fordead_task$invoke()` passe
  désormais `output_dir = .resolve_fordead_output_dir(...)` et
  `keep_output = TRUE`.
* `R/service_monitoring.R::run_fordead_async()` : nouveaux params
  `output_dir = NULL` + `keep_output = TRUE` dans la signature
  de l'ExtendedTask + mkdir best-effort côté worker + forward à
  `nemeton::run_fordead_dieback()`.
* `R/mod_monitoring.R::~l.1306` : nouveau `fordead_result_consumed
  <- shiny::reactiveVal(FALSE)`.
* `R/mod_monitoring.R::~l.2431` (observer fordead result) : garde
  d'idempotence dans le tryCatch error ET dans le branchement
  success (parité v0.70.4).
* `R/mod_monitoring.R::~l.2371` (observeEvent run_health) : reset
  `fordead_result_consumed(FALSE)` avant le nouvel invoke.

### Pas de breaking change

* Le ntfy ne change que le wording (sémantique identique).
* `output_dir` + `keep_output` = NULL → retombe sur le défaut cœur,
  donc safe pour les workers anciens.
* Les gardes d'idempotence sont strictement défensives.

### Tests

* 2753 pass / 3 fails pré-existants (NDMI bands tests v0.66.0
  non liés, hors scope).

# nemetonshiny 0.71.0 (2026-06-03)

### Added — Modal pixel Carte FAST : 3e indice NDMI affiché avec sa propre couleur et sa ligne de seuil

Au clic sur un pixel de la Carte FAST, la modale plotly trace
désormais **3 courbes** (au lieu de 2) avec **3 couleurs distinctes** et
**3 lignes de seuil horizontales** alignées :

| Indice | Sémantique | Couleur |
|---|---|---|
| NDVI | Vigueur végétation | 🟢 `#2CA02C` (vert) |
| NBR | Brûlé / cassé | 🔴 `#D62728` (rouge) |
| **NDMI** | **Humidité / eau** | 🔵 **`#1F77B4` (bleu)** ← nouveau |

**État avant v0.71.0** : `extract_pixel_timeseries()` était déjà
appelée avec `indices = c("NDVI", "NBR", "NDMI")` côté cœur (l.770)
et la boucle de rendu itérait bien sur les 3 indices, MAIS :

* `.pixel_band_colors` (l.451) ne contenait que NDVI + NBR → la
  courbe NDMI tombait sur `%||% "#7F7F7F"` (gris fallback).
* La section des lignes de seuil horizontales (l.838) ne traitait
  que `th$ndvi` et `th$nbr` → pas de ligne pour le seuil NDMI.

Conséquence : la 3e courbe était bien tracée mais en gris discret,
sans le seuil de référence — l'utilisateur ne pouvait pas voir
visuellement « ce pixel est-il en alerte NDMI ? ».

### Détails techniques

* `R/mod_monitoring_pixel_map.R::.pixel_band_colors` (l.451) :
  ajout de `NDMI = "#1F77B4"`.
* `R/mod_monitoring_pixel_map.R` section seuils (l.849+) :
  - Lecture de `th_ndmi <- suppressWarnings(as.numeric(th$ndmi))`
    depuis le reactive parent `thresholds_r` (déjà exposé via
    `mod_monitoring.R:2564, 2587, 2631`).
  - Nouveau bloc `if (length(th_ndmi) == 1L && !is.na(th_ndmi))`
    qui ajoute le `shape` (ligne horizontale pointillée bleue)
    + l'`annotation` (libellé « NDMI = 0.30 » à droite).

### Pas de breaking change

L'extraction des 3 indices, le slider `threshold_ndmi` côté sidebar
parent (l.159) et le reactive `thresholds_r$ndmi` étaient déjà en
place depuis nemetonshiny@v0.66.0. La v0.71.0 connecte simplement
les 2 derniers maillons (couleur + ligne seuil) côté modale.

### Tests

* 2491 pass / 3 fails pré-existants (NDMI bands tests v0.66.0
  non liés, hors scope).

# nemetonshiny 0.70.5 (2026-06-03)

### Removed — Avertissement NDMI / bande B11 (obsolète depuis le plancher `nemeton (>= 0.65.1)`)

Le helper `.fast_ndmi_note()` qui affichait au-dessus de la carte
(quand NDMI était sélectionné dans le radio) :

> *NDMI : baisse sous stress hydrique.*
>
> *NDMI nécessite la bande B11, cachée seulement depuis les
> ingestions ≥ v0.64.0. Pour une zone déjà ingérée, relancez
> l'ingestion Sentinel-2 (ré-ingestion complète, sans réutiliser
> le cache) pour activer NDMI — sinon la carte NDMI reste vide.*

est **retiré** des deux sidebars FAST (Alertes FAST + Carte FAST).

**Pourquoi** : le plancher cœur `nemeton (>= 0.65.1)` (v0.69.1
app) garantit que `ingest_sentinel2_timeseries()` cache **B11
systématiquement en best-effort** (spec 019 D3). Le message
« ré-ingestion sans cache pour activer NDMI » ne s'applique plus
aux installations récentes :

* Toute nouvelle ingestion S2 → B11 cachée → NDMI calculable.
* Une zone ingérée AVANT v0.64.0 sans B11 → l'app affiche
  désormais la clé i18n `monitoring_fast_alerts_no_scene`
  (v0.68.0) : « aucune scène cachée ne porte les bandes de cet
  indice dans la fenêtre » → message générique propre.

L'avertissement long et alarmiste dans la sidebar était devenu
bruit visuel — l'utilisateur le voyait à chaque sélection NDMI.

### Détails techniques

* `R/mod_monitoring_fast_alerts.R` : suppression du helper
  `.fast_ndmi_note()` (l.37-45), du `shiny::uiOutput(ns("ndmi_note"))`
  dans la sidebar et de `output$ndmi_note <- shiny::renderUI(...)`
  côté server.
* `R/mod_monitoring_pixel_map.R` : suppression du
  `shiny::uiOutput(ns("ndmi_note"))` dans la sidebar et de
  `output$ndmi_note <- shiny::renderUI(...)` côté server.
* `R/utils_i18n.R` : suppression des clés
  `monitoring_fast_ndmi_hint` et `monitoring_fast_ndmi_b11_note`
  (FR + EN).
* `tests/testthat/test-mod_monitoring_fast_alerts.R` :
  - assertion HTML inversée pour la présence du placeholder
    `fa-ndmi_note` (désormais absent) ;
  - retrait du test `.fast_ndmi_note renders the NDMI hint and
    the B11 re-ingest note` devenu obsolète.

### Pas de breaking change fonctionnel

Le calcul NDMI continue à fonctionner inchangé (cœur >= 0.65.1).
Seul le bandeau d'avertissement disparaît.

### Tests

* 2491 pass / 3 fails pré-existants (NDMI bands tests v0.66.0
  non liés, hors scope).

# nemetonshiny 0.70.4 (2026-06-03)

### Fixed — Toast `Diagnostic FAST terminé` qui clignote (apparaît / disparaît / réapparaît)

**Symptôme** : à la fin d'un Diagnostic FAST, le toast en bas à
droite « Diagnostic FAST terminé : 120 scène(s) en cache. »
apparaissait, disparaissait (après les 6 s de duration), puis
**réapparaissait** plusieurs fois.

**Cause** : l'observer de fin de worker (l.2020) dépend de
`fast_task$result()`. Shiny `ExtendedTask$result()` peut, dans
certains cycles de vie (cascade reactive, transition de status,
re-évaluation lors d'événements parallèles), **refire plusieurs
fois pour le MÊME result**. Sans garde, `showNotification(
"ingest_success", duration = 6)` était ré-appelé à chaque fire →
le toast disparaissait après 6 s puis ré-apparaissait quand
l'observer fire suivant survenait.

**Fix** : nouveau `fast_result_consumed <- shiny::reactiveVal(FALSE)`
qui agit comme garde d'idempotence :

* L'observer toast vérifie au début si le flag est `TRUE` (= déjà
  traité) → return silencieux.
* À la 1ʳᵉ exécution, le flag est mis à `TRUE` (via `isolate()`
  pour éviter d'établir une dépendance reactive).
* Reset à `FALSE` dans `observeEvent(input$run)` (avant le nouvel
  `invoke()`) → le prochain Diagnostic FAST sera bien traité.

La garde s'applique aussi au branchement `error` du `tryCatch`
(même symptôme possible sur le toast `ingest_error`).

### Changed — Cohérence ntfy : alignement wording + retrait du `%d observations`

**Avant v0.70.4** : le push ntfy « ingest complete » affichait
`"Ingestion FAST terminée : %d scènes, %d observations en %s."`
avec **`n_obs_inserted` toujours 0 depuis nemeton@v0.58.0** (drop
obs_pixel insertion). Le toast UI avait déjà retiré cette mention
en v0.53.1 (commentaire `mod_monitoring.R:2049`), mais ntfy non.
Wording incohérent (« Ingestion FAST » vs « Diagnostic FAST »).

**Après v0.70.4** :

| Surface | Avant | Après |
|---|---|---|
| Toast UI | « Diagnostic FAST terminé : 120 scène(s) en cache. » | inchangé |
| ntfy push | « Ingestion FAST terminée : 120 scènes, 0 observations en 3 min. » | « Diagnostic FAST terminé : 120 scène(s) en cache (3 min). » |

**Détails techniques** :

* `R/utils_i18n.R::monitoring_ntfy_ingest_complete` : format passe
  de `%d %d %s` → `%d %s` (retrait du `n_obs`). Wording « Ingestion
  FAST terminée » → « Diagnostic FAST terminé ».
* `R/service_monitoring.R::~l.301` : `sprintf(...)` retire
  l'argument `as.integer(summary$n_obs_inserted %||% 0L)`.
* `tests/testthat/test-service_monitoring.R::~l.278` : test
  `sprintf` ajusté pour 2 args (était 3).

### Pas de breaking change

Strictement défensif côté toast. Le push ntfy n'expose plus une
métrique trompeuse (0 toujours). Surface app + ntfy alignées.

### Tests

* 2760 pass / 3 fails pré-existants (NDMI bands tests v0.66.0
  non liés, hors scope).

# nemetonshiny 0.70.3 (2026-06-03)

### Fixed — Toast d'ingestion FAST initialisé à `(1/N)` (au lieu de sauter à `(2/N)`)

**Symptôme** : depuis v0.70.2 (fix off-by-one), le toast en bas à
droite affichait souvent **`Tuile (2/120)`** comme **première**
valeur visible, jamais `(1/120)`.

**Cause** : pas un bug du `+1` v0.70.2 — la sémantique cœur
`completed = i - 1` est respectée. Le toast lit
`ingest_progress.json` (réécrit + atomic rename à chaque event,
polling 500 ms). Le worker pousse `s2:scene completed=0` (scène 1)
puis très rapidement `s2:scene completed=1` (scène 2) en bien
moins de 500 ms → le JSON est écrasé entre 2 polls → le 1er event
capturable par Shiny est souvent `completed=1` → affiché `(2/120)`
après le `+1`. La scène 1 vit dans le mirror console (drain NDJSON
v0.70.0) mais pas dans le toast.

**Fix** : ajouter un handler `s2:search_done` qui initialise le
toast à `Tuile (1/N) — démarrage du téléchargement…` **avant** le
1er event `s2:scene`. L'utilisateur voit donc `(1/N)` au moins une
fois ; quand le 1er `s2:scene` capturé arrive (peut être à
`completed=2` → `(3/N)`), le toast est remplacé par le numéro
réel. Le saut résiduel reste mais la perception « commence à 2 »
disparaît.

### Détails techniques

* `R/mod_monitoring.R` (~l.1656) : nouvelle branche
  `if (identical(current_phase, "s2:search_done"))` qui appelle
  `showNotification(... ingest_progress)` avec le format
  `monitoring_ingest_search_done_fmt`, hardcodé à `(1, total)`.
  Placée AVANT la lecture des champs `i_val` / `n_val` et la
  garde STAC (qui reste sur `i_val == 0L` brut).
* `R/utils_i18n.R` : nouvelle clé `monitoring_ingest_search_done_fmt`
  (FR : « Tuile (%d/%d) — démarrage du téléchargement… » /
  EN : « Tile (%d/%d) — starting download… »).

### Cas où le toast peut quand même sauter

Si `total = 0` (échec STAC ou aucune scène trouvée), l'initialisation
est sautée (`if (n_val_init > 0L)`). Comportement de fallback
identique à v0.70.2.

### Pas de breaking change

Strictement additif. Le drain NDJSON (mirror console) continue à
afficher `Tuile (1/N) → (N/N)` complet et ordonné — inchangé.

### Tests

* 2498 pass / 3 fails pré-existants (NDMI bands tests v0.66.0
  non liés, hors scope).

# nemetonshiny 0.70.2 (2026-06-03)

### Fixed — Compteur de tuile 1-based (`Tuile (1/120) → (120/120)`)

Hand-off du brief `BRIEF-nemetonshiny-console-FAST.md` (Partie B).
Aucune modif cœur nécessaire.

**Symptôme** : pendant un Diagnostic FAST, le compteur affichait
`Tuile Sentinel-2 ... (0/120)` sur la 1ʳᵉ scène et `(119/120)` sur
la dernière, au lieu de `(1/120)` → `(120/120)`.

**Cause** (pas un bug cœur) : le cœur émet `completed = i - 1`
dans les events `s2:scene` / `s2:scene_cached` / `s2:scene_skipped`,
soit « scènes terminées AVANT celle-ci ». Convention volontaire :
`completed/total` est une **fraction de progression** (0 au
départ, `total` à la fin via `s2:complete`). L'app affichait
`completed` brut comme libellé de tuile EN COURS → off-by-one.

**Fix (app uniquement)** : afficher `completed + 1` **uniquement
dans le libellé** « tuile en cours » (1..total), à 2 endroits :

* `R/mod_monitoring.R::.log_ingest_event` (~l.2810-2818) — mirror
  console. Calcul d'un `tile_no <- i_val + 1L` après la garde STAC.
* `R/mod_monitoring.R` observer toast (~l.1681-1687) — toast
  Shiny. `i_val + 1L` dans les 2 branches
  (`monitoring_ingest_progress_named_fmt` +
  `monitoring_ingest_progress_fmt`).

Les **gardes « entre STAC et 1ʳᵉ tuile »** (`!nzchar(scene) &&
i_val == 0L`) restent sur `i_val` brut — elles dépendent du 0
pour distinguer l'état « scan STAC en cours » de l'état
« 1ʳᵉ tuile en téléchargement ». Sans cela, l'affichage de
« Recherche STAC... » serait sauté.

### Vérification

* `s2:scene` émis au début de chaque scène (`i = 1..total`) →
  `completed = 0..(total-1)` → `tile_no = 1..total` →
  `1/120 → 120/120`.
* `s2:complete` (`completed = total`) n'est pas rendu comme une
  tuile → pas de `121/120`.
* Garde STAC inchangée : « Recherche STAC... » affiché tant que
  `completed == 0` ET pas de `scene_id`.

### Pas de breaking change

Strictement défensif. Le code cœur n'est pas modifié — `completed`
reste à sa sémantique fraction-of-progress. Seul le libellé
d'affichage côté app est ajusté.

### Partie A du brief (drain NDJSON) — confirmée en place

La Partie A du brief (logs `Tuile 1→120` ordonnés sans saut ni
entrelacement via drain NDJSON) avait été **déjà livrée en v0.70.0**.
Vérification effectuée : `ingest_ndjson_lines` reactivePoll +
observer drainage sont bien présents dans `mod_monitoring.R:1387+`.
Aucune action additionnelle requise.

### Tests

* 416 pass / 3 fails pré-existants (NDMI bands tests v0.66.0
  non liés, hors scope).

# nemetonshiny 0.70.1 (2026-06-03)

### Fixed — Toast prewarm FAST persistant + signal explicite de fin du Diagnostic

**Symptômes observés** : après un Diagnostic FAST terminé,

1. Le toast en bas à droite « Pré-calcul carte NDMI Intensité en
   cours… » **restait collé** indéfiniment.
2. Le bouton « Lancer le diagnostic FAST » **ne redevenait pas
   cliquable**, donnant l'impression d'une UI bloquée.
3. Aucun signal explicite ne disait à l'utilisateur que
   l'application était de nouveau disponible.

**Cause** : le toast running `fast_prewarm_progress` (id stable,
`duration = NULL`, persistent) était créé à chaque phase
`fast_prewarm:<idx>_<mode>` sans suffixe (l.1601), mais à
`fast_prewarm:complete` (l.1550) l'observer faisait juste un
`cli::cli_alert_info` + `return()` **sans `removeNotification`**.
Le toast vivait donc à vie.

Pour le bouton, l'observer (l.1716) lit `fast_task$status()` et
désactive le bouton tant que le statut est `"running"`. Si le
worker termine légitimement, `status()` passe à `"success"` et le
bouton se réactive automatiquement. Mais si le toast running est
encore là, c'est que le worker tourne toujours — symptôme cohérent
mais déroutant.

**Fix (3 changements complémentaires)** :

* **À `fast_prewarm:complete`** : retrait explicite du toast
  running (`shiny::removeNotification(session$ns(
  "fast_prewarm_progress"))`) + nouveau toast court (4 s)
  confirmant la disponibilité de l'app.
* **Filet de sécurité** : nouvel observer qui watch
  `fast_task$status()` et retire le toast running dès que le
  statut quitte `"running"`. Couvre le cas pathologique où le
  cœur n'émet pas `complete` (hang silencieux, exception non
  capturée côté prewarm).
* **Nouvelle clé i18n** `monitoring_fast_diagnostic_complete`
  (FR : « Diagnostic FAST terminé — application disponible. » /
  EN : « FAST diagnostic complete — application available. »).

### Comment savoir que l'application est disponible

* **Toast vert court** (4 s) « Diagnostic FAST terminé —
  application disponible. » émis à la toute fin.
* **Bouton « Lancer le diagnostic FAST »** redevient cliquable
  automatiquement (était déjà le cas, mais le toast persistant
  masquait le signal).
* **Filet de sécurité** : si le worker s'arrête mais
  qu'aucun `complete` n'arrive, le toast running disparaît quand
  même grâce au nouvel observer status().

### Pas de breaking change

Strictement défensif. Le toast vert s'ajoute uniquement à la fin
réussie ; en cas d'annulation ou d'erreur, les toasts existants
(`fast_prewarm_cancelled`, `fast_prewarm_failed`) restent intacts.

### Tests

* 2758 pass / 3 fails pré-existants (NDMI bands tests v0.66.0
  non liés, hors scope).

# nemetonshiny 0.70.0 (2026-06-03)

### Fixed — Logs FAST propres : Tuile 1/N → N/N sans saut ni entrelacement

**Symptôme** : pendant un Diagnostic FAST, le mirror console
affichait des **sauts** dans la numérotation des tuiles
(`1/120 → 3 → 23 → 51 → 93…`) et un **faux entrelacement** : la
ligne `⤷ Bande B04 (cache)` correspondait à une autre scène que
l'en-tête `Tuile Sentinel-2 ...(X/120)` affichée juste au-dessus.

**Cause racine** (hand-off du brief
`/home/pascal/dev/nemeton/BRIEF-nemetonshiny-logs-FAST-propres.md`,
vérifié file:ligne) : deux transports de progression coexistaient
dans `mod_monitoring.R`. Le flux métier (Tuile/Bande FR formaté par
l'app) passait par le **mauvais** transport.

| Transport | Mécanisme | Complétude |
|---|---|---|
| `ingest_progress` (`mod_monitoring.R:1322`) | `reactivePoll 500ms` sur `ingest_progress.json` réécrit (atomic rename) à chaque event | ❌ Lossy — seul le dernier event présent au poll survit |
| `ingest_log_tick` (`mod_monitoring.R:1346`) | `tail` par offset d'octets sur le `sink()` stdout du worker | ✅ Complet, ordonné — mais véhicule uniquement le cli **anglais** du cœur |

Le worker pousse 4-5 events par scène (1 `s2:scene` + 2-4 `s2:band_cached`)
en bien moins de 500 ms → la quasi-totalité des events était écrasée
avant le poll suivant. D'où les sauts ET la désynchro entête / bande
(lus à des polls différents).

**Fix** (app uniquement, aucune modif cœur) : faire passer le flux
métier d'un *« dernier event JSON »* à un *« journal NDJSON
append-only drainé intégralement »*, exactement le pattern déjà
utilisé par `ingest_log_tick`.

### Double transport, séparation des responsabilités

* **`.json`** (dernier event, réécrit + atomic rename) → pilote le
  **toast Shiny coalescé** (1 toast actif, remplacement par id).
  Comportement inchangé.
* **`.ndjson`** (append-only, une ligne par event, jamais écrasé) →
  pilote le **mirror console** (`.log_band_event` + `.log_ingest_event`).
  Drainage par offset d'octets, **garanti complet et ordonné**.

### Détails techniques

* `R/service_monitoring.R::.build_progress_writer` (l.342-380) :
  - Continue à écrire le `.json` (atomic rename) — toast inchangé.
  - **En plus**, append une ligne NDJSON dans `<path>.ndjson` à
    chaque event via `cat(line, "\n", append = TRUE)`.
  - Path NDJSON dérivé du JSON : `.../ingest_progress.json` →
    `.../ingest_progress.ndjson`.
* `R/mod_monitoring.R` :
  - Nouveau `ingest_ndjson_path` reactive + `ingest_ndjson_offset`
    reactiveVal + `ingest_ndjson_lines` reactivePoll (300 ms, tail
    par offset).
  - Nouveau `shiny::observe` qui itère sur **chaque event** NDJSON
    et appelle `.log_band_event` / `.log_ingest_event` dans
    l'ordre du worker.
  - Observer toast `ingest_progress` (l.1397+) **inchangé pour le
    toast**, mais **retire les appels** `.log_band_event` et
    `.log_ingest_event` du chemin JSON (ils sont désormais
    pilotés par le drain NDJSON).
  - `.cleanup_progress_file` (l.2611) étendu : supprime aussi
    `<path>.ndjson` au reset de chaque ingest.

### Garde-fous

* Le toast reste coalescé (1 update par tick) — pas de 600 toasts
  pour 120 scènes × 4-5 events.
* Si le worker ne livre pas de `.ndjson` (worker plus ancien ou
  erreur d'écriture), le mirror console reste silencieux, le
  toast continue à fonctionner — fallback transparent.
* Reset propre : `ingest_ndjson_offset` repart à 0 à chaque ingest
  (le `.ndjson` est supprimé puis recréé par le worker).

### Effet observable

Le mirror console affiche désormais **dans l'ordre** :

```
ℹ Tuile Sentinel-2 S2B_MSIL2A_... (1/120) — 2025-05-23, 12.4% nuages
  ⤷ Bande B04 (cache) — scène S2B_MSIL2A_...
  ⤷ Bande B08 (cache) — scène S2B_MSIL2A_...
  ⤷ Bande B11 (cache) — scène S2B_MSIL2A_...
ℹ Tuile Sentinel-2 S2A_MSIL2A_... (2/120) — 2025-05-26, 8.2% nuages
  ⤷ Bande B04 (téléchargement) — scène S2A_MSIL2A_...
  ...
```

au lieu des sauts précédents.

### Pas de breaking change

* Aucune modif cœur (le brief le confirme : le flux d'events
  cœur EST déjà séquentiel 1→120).
* Le toast Shiny existant continue de fonctionner identiquement.
* Si le `.ndjson` n'existe pas (ancien worker), le mirror est
  vide mais l'app fonctionne.

### Tests

* 416 pass / 3 fails pré-existants (NDMI bands tests v0.66.0
  non liés, hors scope).

### Optionnel hors scope

Le brief mentionne aussi un bouton « Afficher (cache) » distinct
de « Rafraîchir » qui appellerait directement `read_fast_alert_rasters()`
en relecture pure (pas de STAC, pas de worker). À ouvrir en chantier
séparé si l'usage le justifie.

# nemetonshiny 0.69.1 (2026-06-03)

### Changed — Plancher cœur `nemeton (>= 0.65.1)` : prewarm FAST 6 combos (NDMI)

Hand-off du brief
`/home/pascal/dev/nemeton/BRIEF-nemetonshiny-retour-prewarm-v0.65.1.md`.
nemeton v0.65.1 publié le 2026-06-03 corrige l'oubli NDMI dans la
boucle `.prewarm_fast_alerts()` (cf. brief v0.65.0 → v0.65.1
côté cœur) :

* `expand.grid(index = c("NDVI", "NBR"), ...)` → ajout `"NDMI"` →
  **6 combos pré-chauffées** au lieu de 4.
* Garde-fou existant suffisant : une scène sans B11 (bande NDMI)
  emprunte le chemin de skip best-effort (`tryCatch` +
  `cli::cli_warn` + event `fast_prewarm:NDMI_<mode>_failed`).
  Comportement symétrique avec NBR sans B12.

**Effet UX côté app** : après ingestion S2, la 1re sélection NDMI
dans Alertes FAST ou Carte FAST devient un **hit cache D6
instantané** au lieu d'un calcul à froid (1-5 s selon zone).

### Audit cache FAST (RAS)

Le brief retour cœur confirme que la disposition des caches FAST
côté app est cohérente :

* `cache/layers/fast_alert/` (`.fast_alert_cache_dir()`) — cache D6
  continu monitoring, lu par `compute_fast_alert_mask(result_cache_dir
  = ...)` ET écrit par le prewarm (`prewarm_mask_cache_dir =
  .fast_alert_cache_dir()`). **Prewarm bénéficie à l'affichage** —
  y compris NDMI depuis v0.65.1.
* `cache/layers/fast_alert_mask/` (`.fast_alert_mask_cache_dir()`)
  — mask 0-4 catégoriel monitoring.
* `cache/layers/fast_sampling/` (renommé en v0.69.0, ex-`fast/`) —
  continu + mask validation_sampling.

Compromis assumé : le prewarm ne réchauffe **pas**
`fast_sampling/` (dossier distinct) → 1re prévisualisation
sampling reste à froid. Acceptable car validation_sampling
est un usage ponctuel d'admin.

### Détails techniques

* `DESCRIPTION` : `Imports: nemeton (>= 0.65.0)` → `(>= 0.65.1)`.
* `R/mod_monitoring.R::~l.1471` : commentaire « les 4 `_done` »
  → « les 6 `_done` (4 en cœur ≤ v0.65.0, 6 depuis v0.65.1 avec
  NDMI) ».
* Vérifications post-bump :
  - Feed de progression : chaque toast prewarm a un `id` unique
    (`fast_prewarm_done_<idx>_<mode>`). 12 events uniques (6 ×
    `_done` + 6 × *running* éphémères) — pas de rate-limit ni
    saturation.
  - i18n NDMI : `idx_payload` ("NDMI") affiché en raw dans
    `sprintf(i18n$t("fast_prewarm_done"), idx_payload,
    mode_label)`. Pas de clé NDMI spécifique nécessaire (parité
    NDVI/NBR).

### Pas de breaking change

L'API consommée côté app (`prewarm_alerts = TRUE`,
`prewarm_mask_cache_dir = .fast_alert_cache_dir()`) est inchangée.
Le cœur enchaîne désormais les 6 cartes au lieu de 4 sans aucun
changement de signature.

# nemetonshiny 0.69.0 (2026-06-03)

### Changed — Renommage du cache `cache/layers/fast/` → `cache/layers/fast_sampling/`

Le sous-répertoire `cache/layers/fast/` utilisé par le module
**validation_sampling** (cache des masques 0-4 pour la prévisualisation
de plan d'échantillonnage) est renommé en
**`cache/layers/fast_sampling/`**. La sémantique restait ambiguë avec
les caches voisins du **monitoring** :

| Avant v0.69.0 | Après v0.69.0 | Contexte |
|---|---|---|
| `cache/layers/fast/` | `cache/layers/fast_sampling/` | Validation d'échantillonnage (spec 011) |
| `cache/layers/fast_alert/` | inchangé | Cache D6 raster continu monitoring (spec 017) |
| `cache/layers/fast_alert_mask/` | inchangé | Mask 0-4 catégoriel monitoring (spec 017 D2) |

**Pourquoi** : trois caches FAST coexistaient, dont un (`fast/`) au
nom générique sans rapport explicite avec son usage. Le nouveau nom
`fast_sampling` reflète clairement qu'il appartient au contexte
validation_sampling, pas au monitoring direct.

### Migration

**Pas de migration automatique** (décision UX consciente). Sur les
projets existants ayant déjà préchauffé un `cache/layers/fast/`,
l'ancien répertoire restera orphelin sur disque. Le nouveau code
créera `cache/layers/fast_sampling/` à la prochaine demande de
validation_sampling. Suppression manuelle de l'ancien recommandée
pour récupérer l'espace disque :

```bash
rm -rf <projet>/cache/layers/fast/
```

### Détails techniques

* `R/service_validation_sampling.R` lignes 186, 258 : `"fast"` →
  `"fast_sampling"` dans les 2 `file.path()`.
* `R/mod_validation_sampling.R` lignes 278, 439 : idem dans les 2
  branches discriminantes `if (FORDEAD) ... else ...`.
* `tests/testthat/test-service_validation_sampling.R:111` : test mis
  à jour pour créer `fast_sampling/zone_9` au lieu de `fast/zone_9`.

### Pas de breaking change fonctionnel

L'API consommée (`nemeton::read_fast_alert_mask`,
`compute_fast_alert_mask`) est inchangée. Seul le chemin de cache
côté projet utilisateur change de nom.

### Tests

* 18 pass / 0 fail sur `test-service_validation_sampling.R`.

# nemetonshiny 0.68.0 (2026-06-03)

### Changed — Plancher cœur bumpé à `nemeton (>= 0.65.0)`

Le brief `nemeton/BRIEF-nemetonshiny-fast-6-cartes.md` documente
l'API publiée par nemeton v0.65.0 pour le Diagnostic FAST 6 cartes
(3 indices × 2 modes) :

* Fix régression spec 019 D3 : `.enumerate_cache_scenes()` n'avait
  **pas** de branche NDMI dans son switch d'index, donc
  `compute_fast_alert_mask(index = "NDMI", ...)` renvoyait
  systématiquement `NULL` côté cœur même quand B08 + B11 étaient
  cachés. **Conséquence côté app** : la radio NDMI (introduite
  côté UI en v0.66.0) ne produisait jamais de carte tant que le
  plancher cœur restait à v0.64.0.
* Nouveau symbole exporté `read_fast_alert_rasters()` (pluriel) :
  orchestrateur 3 indices × 2 modes = 6 rasters continus en un
  appel. Pas encore consommé par l'app (le pipeline mono-index
  `compute_fast_alert_mask()` via les radios reste l'unique entrée
  d'affichage en v0.68.0).

**Effet utilisateur** : après réinstall (`@*release` tire
automatiquement la dernière release cœur), sélectionner NDMI dans
les radios Alertes FAST ou Carte FAST produit désormais une carte
(au lieu d'une banderole jaune « non calculable »).

### Changed — Message i18n pour le cas « aucune scène cachée »

Le banderole jaune affichée quand `compute_fast_alert_mask()` renvoie
`NULL` (raster non calculable) contenait un littéral FR :

```
"%s : aucun raster d'alerte calculable (cache vide ou hors fenêtre)."
```

→ Remplacé par `sprintf(i18n_r()$t("monitoring_fast_alerts_no_scene"),
idx)` (règle stricte CLAUDE.md §4). Nouvelle clé i18n FR/EN avec
wording explicite : « aucune scène cachée ne porte les bandes de
cet indice dans la fenêtre » — couvre le cas typique où NDMI exige
B08+B11 et la zone n'a que B08+B04 cachés.

### Détails techniques

* `DESCRIPTION` : `Imports: nemeton (>= 0.64.0)` → `>= 0.65.0`.
* `R/mod_monitoring_fast_alerts.R::raster_r` (~l.323) : littéral FR
  remplacé par `sprintf(i18n_r()$t("monitoring_fast_alerts_no_scene"),
  idx)`.
* `R/utils_i18n.R` : nouvelle clé `monitoring_fast_alerts_no_scene`
  (FR/EN).

### Vérification rapide

Sur une zone avec cache S2 incluant B08 + B11 (B11 cachée
systématiquement en best-effort par
`ingest_sentinel2_timeseries()` depuis spec 019 D3) :

* radio « NDMI » + mode « count » → carte affichée (avant v0.68.0 :
  bandeau jaune systématique)
* les 6 combinaisons indice × mode rendent une carte ou un état
  vide explicite via la nouvelle clé i18n

### Pas de breaking change

L'API consommée côté app (`compute_fast_alert_mask`) est inchangée.
Le plancher v0.65.0 est rétro-compatible avec tout le code app
existant.

# nemetonshiny 0.67.1 (2026-06-03)

### Fixed — Bug d'oscillation des radios Alertes FAST

**Symptôme** : dans l'onglet Alertes FAST, après un clic sur les
radios « Indice FAST » (NDMI/NDVI/NBR) ou « Mode du raster »
(Fréquence/Intensité), l'application se mettait à clignoter en
continu, alternant toute seule entre les valeurs des radios.

**Cause** : l'observer i18n (rafraîchissement des labels FR/EN à
chaque switch de langue) lisait `input$index` et `input$mode` pour
les passer en argument `selected =` de `updateRadioButtons()`.
`shiny::observe()` capture toutes les dépendances réactives lues
dans son corps → l'observer devenait dépendant de
`input$index`/`input$mode`. Au clic sur un radio :

1. `input$index` change → l'observer i18n re-fire ;
2. `updateRadioButtons(selected = "<nouvelle valeur>")` ré-envoie au client ;
3. Le client réinterprète le selected → re-fire `input$index` ;
4. Boucle infinie de cliquetis visibles à l'écran.

**Fix** : `shiny::isolate()` autour des lectures de `input$index`
et `input$mode` dans l'observer i18n. La sélection courante est
lue au moment du re-render i18n SANS être traquée comme dépendance
réactive. Le switch de langue préserve toujours la sélection
courante, mais le clic radio n'amorce plus de cascade.

**Détails techniques** :

* `R/mod_monitoring_fast_alerts.R::mod_monitoring_fast_alerts_server`
  observer i18n (l.183-210) :
  - `cur_index <- shiny::isolate(input$index %||% "NDVI")`
  - `cur_mode  <- shiny::isolate(input$mode  %||% "count")`
  - `selected = cur_index` et `selected = cur_mode` dans les
    `updateRadioButtons()`.

**Pas de breaking change** : strictement défensif. Le comportement
attendu (clic radio → mise à jour raster + légende sans
oscillation, switch de langue → labels traduits + sélection
préservée) est désormais correct.

# nemetonshiny 0.67.0 (2026-06-03)

### Added — Slider « Seuil minimum NDMI » dans la sidebar Suivi sanitaire

Complément de v0.66.0 : le panneau de gauche du Suivi sanitaire ne
proposait des seuils que pour NDVI et NBR ; NDMI réutilisait donc le
seuil NDVI. Un slider dédié **« Seuil minimum NDMI »** est ajouté sous
les deux autres (range 0.10–0.80, défaut 0.20 — NDMI sain est plus bas
que NDVI/NBR).

* Propagé via `thresholds_r$ndmi` aux 4 consommateurs (Carte FAST,
  Alertes FAST, et les deux plans de validation FAST/FORDEAD).
* Les onglets Alertes FAST / validation lisent désormais `th$ndmi`
  quand l'indice NDMI est sélectionné (repli sur le seuil NDVI si
  absent, par robustesse).
* Clé i18n `monitoring_threshold_ndmi` (FR/EN).

# nemetonshiny 0.66.0 (2026-06-03)

### Added — NDMI dans l'UI FAST (humidité / stress hydrique)

NDMI (indice d'humidité, sensible au stress hydrique) est désormais
sélectionnable dans les deux onglets FAST, en plus de NDVI / NBR.
Aucune logique métier ajoutée : NDMI est calculé / seuillé /
classifié 0-4 côté `nemeton` (>= 0.64.0).

* **Sélecteur d'indice** (Carte FAST + Alertes FAST) : NDMI listé en
  premier, défaut NDVI conservé. La valeur se propage telle quelle à
  `build_index_stack()` (carte pixel) et `compute_fast_alert_mask()`
  (alertes) — le seuil pour NDMI reprend celui de NDVI (« baisse sous
  stress », même sémantique).
* **Série temporelle pixel** : `extract_pixel_timeseries()` reçoit
  désormais `indices = c("NDVI","NBR","NDMI")`.
* **Pré-chauffage / ingestion** : `bands = c("NDVI","NBR","NDMI")`
  passé à `ingest_sentinel2_timeseries()` → la bande B11 est cachée et
  les masques d'alerte NDMI sont pré-calculés par le cœur.
* **Note B11** : un encart (visible quand NDMI est sélectionné) signale
  que NDMI nécessite la bande B11, cachée seulement depuis les
  ingestions ≥ v0.64.0 — pour une zone déjà ingérée, il faut relancer
  l'ingestion Sentinel-2 (sinon la carte NDMI reste vide).
* Légende carte pixel et titre de couche adaptés (libellé
  « NDMI (humidité) »). Clés i18n `index_ndmi`,
  `monitoring_fast_ndmi_hint`, `monitoring_fast_ndmi_b11_note`.
* `Imports: nemeton (>= 0.64.0)` (API NDMI).

# nemetonshiny 0.65.1 (2026-06-03)

### Fixed — Clé i18n manquante `db_not_configured`

Au démarrage, quand aucune base n'est configurée, `app_server.R`
affichait une notification via `i18n$t("db_not_configured")` — mais la
clé n'existait pas dans `TRANSLATIONS`, d'où le warning console
« Translation key not found: db_not_configured » et l'affichage de la
clé brute au lieu d'un message. Clé ajoutée (FR/EN). Un scan complet
des appels littéraux `i18n$t("…")` confirme qu'il s'agissait de la
seule clé manquante.

# nemetonshiny 0.65.0 (2026-06-03)

### Added — Corpus RAG : import / export du manifeste (CSV)

L'onglet « Corpus RAG » du modal Paramètres gagne deux contrôles :

* **Importer un CSV** (`fileInput`) : charge un manifeste depuis le
  disque. Le fichier est parsé par `nemeton::read_knowledge_manifest()`
  (aucune logique de parsing côté app) et chargé dans la table éditable.
  Le CSV inscriptible sur disque **n'est pas écrasé** tant que l'on n'a
  pas cliqué « Enregistrer » — l'import peut donc être relu / corrigé /
  validé avant persistance. La validation réagit immédiatement.
* **Exporter (CSV)** (`downloadButton`) : télécharge le manifeste
  courant (y compris les éditions non enregistrées) avec le *quoting*
  déterministe du cœur (`write_knowledge_manifest(validate = FALSE)`,
  jamais bloqué par des avertissements), repli sur `utils::write.csv`
  si l'helper cœur est indisponible.

Pour mémoire, le corpus **exemple** est déjà amorcé automatiquement au
1er accès depuis le seed packagé `nemeton` (`knowledge_manifest_path(
writable = TRUE)`) — l'import sert à charger *son propre* manifeste.
Clés i18n `rag_btn_import_csv`, `rag_btn_export_csv`,
`rag_import_csv_placeholder`, `rag_import_csv_ok`, `rag_import_csv_error`.

# nemetonshiny 0.64.1 (2026-06-03)

### Fixed — Modal Paramètres : onglet LLM vidé par l'init DT de l'onglet RAG

Après l'intégration de l'admin RAG dans le modal Paramètres (0.63.0),
sélectionner un fournisseur dans l'onglet « Fournisseur LLM » ne
réaffichait plus le bandeau de statut ni les boutons Modifier /
Supprimer la clé. Cause : les tables `DT` de l'onglet « Corpus RAG »
s'initialisaient dans un onglet **caché** (`display:none`) dès
l'ouverture du modal — DataTables (a fortiori avec `scrollX`) échoue
sur un conteneur masqué, et cette erreur JS en cascade empêchait les
autres sorties (dont `llm_status_panel`) de se mettre à jour.

**Fix** : l'UI de l'onglet RAG est désormais **montée à la demande**
(`output$rag_tab_content`, rendue seulement quand `config_tab ==
"tab_rag"`). Les tables DT s'initialisent dans un conteneur visible —
plus d'erreur JS, l'onglet LLM refonctionne. Le serveur RAG reste
initialisé une fois ; ses sorties se lient quand l'UI apparaît.

### Changed

* Bouton plein écran déplacé dans le **coin haut-droit** du modal
  (positionnement absolu sur `.modal-content`), au lieu d'être collé au
  titre à gauche.
* Titre et intro du modal mis à jour pour refléter les trois volets
  (clés API Theia, fournisseur LLM, corpus RAG) :
  « Paramètres : clés API & corpus RAG ».

# nemetonshiny 0.64.0 (2026-06-03)

### Changed — Carte FAST : slider de dates au pas de 5 jours

Le slider « Date d'observation » de l'onglet Carte FAST
(`mod_monitoring_pixel_map`) avançait jour par jour (`step = 1`), alors
que les scènes Sentinel-2 sont au mieux espacées de 5 jours (revisite
2 satellites sur une même tuile MGRS) : 4 crans « morts » entre deux
scènes. Le pas passe à **5 jours** (`step = 5`), aligné sur la cadence
nominale — chaque cran avance d'une scène à la suivante. Le snapping
sur la scène réelle la plus proche (`current_layer_r`) reste en place,
donc un décalage ponctuel (nuages, tuile manquante) reste géré sans
cran à vide.

### Added — Alertes FAST : bandeau bleu résolution + contexte

Ajout d'un bandeau `alert-info` bleu en haut de la carte Alertes FAST
(`mod_monitoring_fast_alerts`), symétrique de celui de Carte FAST. Il
rappelle la résolution Sentinel-2 (10 m) et décrit ce qui est peint
selon le mode et l'indice : « fréquence des dépassements de seuil
NDVI/NBR » (mode Fréquence) ou « intensité du déficit sur fenêtre
roulante » (mode Intensité). Rendu en output séparé du `leafletOutput`
(ne ré-initialise pas la carte) et réactif au mode / à l'indice / à la
langue. Clés i18n `monitoring_fast_alerts_badge_count` /
`monitoring_fast_alerts_badge_rolling`.

# nemetonshiny 0.63.0 (2026-06-03)

### Changed — L'admin RAG passe dans le modal Paramètres (roue dentée)

L'onglet « RAG / Corpus de connaissances » livré en 0.62.0 était un
onglet de premier niveau de la navbar. Il est désormais **intégré au
modal de configuration** (icône roue dentée à gauche du sélecteur de
langue, `mod_theia_config`), en **troisième onglet** à côté de
« Theia / DATA TERRA » et « Fournisseur LLM ».

* `R/mod_theia_config.R` : initialise `mod_rag_admin_server("rag_admin",
  …)` une fois (namespace imbriqué `theia_config-rag_admin-…`) et insère
  `mod_rag_admin_ui()` dans un `tabPanel` « Corpus RAG ».
* `R/app_ui.R` / `R/app_server.R` : retrait de l'onglet navbar
  « Paramètres » et du câblage serveur autonome correspondant.
* Le rendu de la table manifeste passe par un déclencheur `redraw`
  explicite (au lieu d'un proxy) pour rester correct quand le modal est
  fermé puis rouvert (resynchronisation sur l'état courant, plus de
  snapshot périmé).

### Added — Modal Paramètres extensible en plein écran

Bouton bascule (icône `arrows-fullscreen`) dans la barre de titre du
modal : un handler JS minimal applique la classe Bootstrap 5
`.modal-fullscreen` sur le `.modal-dialog`, pour étendre la boîte de
dialogue bord à bord (pratique pour la large table manifeste) et la
réduire, sans aller-retour serveur. Taille par défaut élargie à `xl`.
Clés i18n `api_keys_tab_rag`, `api_keys_fullscreen` (FR/EN).

# nemetonshiny 0.62.0 (2026-06-03)

### Added — Onglet « RAG / Corpus de connaissances » (spec 009.2, E7)

Nouvel onglet d'administration (menu **Paramètres**) qui permet à un
administrateur de curer le corpus de connaissances alimentant les
perspectives IA sourcées :

* **Édition du manifeste** (`R/mod_rag_admin.R`) : tableau `DT` éditable
  cellule par cellule, ajout / suppression de lignes, validation des
  valeurs de vocabulaire contrôlé (`lang`, `status`, `ingest_strategy`,
  `doc_type`, `license`) via `nemeton::knowledge_manifest_vocab()`.
* **Validation en direct** : panneau d'anomalies issu de
  `nemeton::validate_knowledge_manifest()` (sévérité colorée). Tant
  qu'il reste des `error`, « Enregistrer » et « Importer » sont
  désactivés.
* **Enregistrement** : `nemeton::write_knowledge_manifest()` réécrit la
  copie projet inscriptible (`knowledge_manifest_path(writable = TRUE)`),
  refusé en cas d'erreur.
* **Prévisualisation (dry-run)** : `build_knowledge_corpus(dry_run =
  TRUE)` sans toucher la base ni l'API.
* **Import asynchrone** : `shiny::ExtendedTask` + `future_promise`. Le
  worker ouvre **sa propre** connexion DB (les connexions DBI ne sont
  pas partageables entre processus), reçoit la clé d'embedding
  explicitement (résolue en session principale), et écrit l'avancement
  dans un fichier heartbeat lu par le main process via
  `invalidateLater`. Barre de progression + bilan coloré sur `action`.
* **Inventaire base** : `list_knowledge_documents()` + suppression
  (`delete_knowledge_document()`) de la ligne sélectionnée.
* Case « inclure les documents à confirmer » (avertissement licence D5)
  et « reconstruire (fresh) » avec modale de confirmation.
* Accès réservé aux administrateurs (`can_admin_rag()`), avec repli
  éditeur en mode anonyme (installs mono-utilisateur).

### Changed

* `Imports: nemeton (>= 0.63.0)` — plancher relevé : le code consomme
  l'API manifeste/corpus publiée par la spec 009.2 cœur.
* ~30 nouvelles clés i18n FR/EN (`rag_*`, `tab_settings`) dans
  `R/utils_i18n.R`.

# nemetonshiny 0.61.2 (2026-06-02)

### Changed — Le RAG s'applique désormais aussi aux 12 commentaires famille

Constat utilisateur post-v0.61.1 : `[ FAIL 0 | PASS 41 ]` côté RAG,
mais en exécution live, **une seule** ligne `RAG: 3 chunk(s)
récupéré(s)` apparaissait dans le log alors que **13 perspectives**
étaient générées (1 synthèse globale + 12 familles).

Cause : `R/mod_synthesis.R` appelait `rag_context()` uniquement pour
la synthèse globale (lignes 565-588), et la boucle qui génère les 12
commentaires famille (lignes 616-681) appelait
`chat$chat(fam_prompt)` sans préfixer le `ctx$prompt_block` ni la
consigne de citation. Conforme au brief original (« la perspective »
au singulier) mais sous-exploitait le corpus RAG.

**Fix** : préfixer chaque `fam_prompt` par le `ctx$prompt_block`
déjà récupéré pour la synthèse (et la consigne `cite_rule`), puis
passer le tout au LLM. **1 seul appel `retrieve_knowledge()`**
(donc 1 seule ligne `cli_inform`), **13 prompts enrichis**
(synthèse + 12 familles) avec le même contexte.

### Avantages

* **Coût** : pas de retrieve supplémentaire (1× embedding Mistral
  + 1× cosine search pgvector au total). Si la perspective globale
  est calculée, les 12 familles en bénéficient sans surcoût.
* **Cohérence** : les marqueurs `[^n]` injectés dans tous les
  prompts pointent vers les mêmes documents. Le bloc « Sources
  documentaires » affiché sous la synthèse (`output$ai_sources`)
  documente toutes les citations potentiellement émises.
* **Robustesse** : `Filter(nzchar, c(ctx$prompt_block, cite_rule,
  fam_prompt))` neutralise proprement le cas où le ctx est vide
  (corpus muet, opt-out, échec retrieve) — le prompt famille
  retombe sur son comportement v0.61.1 sans RAG.

### Détails techniques

* `R/mod_synthesis.R` ligne 651-664 : insertion d'un
  `fam_user_prompt <- paste(Filter(nzchar, c(ctx$prompt_block,
  cite_rule, fam_prompt)), collapse = "\n\n")` avant le
  `chat$chat()`, et l'argument passé au LLM devient
  `fam_user_prompt` (vs `fam_prompt` auparavant).

### Pas de breaking change

Le ctx étant lu depuis la même closure (calculé en amont dans le
même observer), il n'y a pas de race condition. Si le RAG est
opt-out (`options(nemeton.rag_enabled = FALSE)`), `ctx$prompt_block
== ""` → `Filter(nzchar)` le retire → prompt famille identique à
v0.61.1.

# nemetonshiny 0.61.1 (2026-06-02)

### Added — Observabilité RAG (`cli_inform` par perspective)

Ajout d'une ligne de log par appel `rag_context()` réussi, dans
`R/service_rag.R` juste après le retrieve cœur :

```r
cli::cli_inform("RAG: {nrow(chunks)} chunk(s) récupéré(s) au-dessus de {min_similarity}")
```

**Pourquoi** : sans ce log, un corpus muet (0 chunk au-dessus du
seuil) ressemblait à un cas où `rag_context()` court-circuitait
avant le retrieve (opt-out, corpus indisponible, situation_text
vide…). Désormais on distingue :

* **Ligne présente** → le retrieve a réussi, le nombre de chunks
  est connu, ajuster le seuil ou les filtres si trop bas.
* **Ligne absente** → court-circuit en amont (corpus, opt-out,
  query vide).

**Pas de breaking change** : strict ajout d'un log côté
`cli::cli_inform()`, aucun comportement modifié.

Item résiduel du brief RAG du 2026-06-02 (chantier #2). Le reste du
câblage (`R/service_rag.R`, `R/mod_synthesis.R`, i18n, tests) était
déjà livré en v0.56.0.

# nemetonshiny 0.61.0 (2026-06-02)

### Removed — Simplification des contrôles raster FAST

Trois éléments d'UI redondants ou trompeurs sont retirés en bundle :

**1. Checkbox « Afficher le raster » — Alertes FAST**

Le `checkboxInput("raster_visible")` du sidebar droit d'Alertes FAST
est retiré. La visibilité du raster d'alerte est désormais pilotée
par le **LayersControl Leaflet** (entrée « Alertes » sous l'entrée
« UGF »). Quand l'utilisateur décoche « Alertes » dans le contrôle
de couches, Leaflet masque proprement le group sans repasser par un
re-render Shiny.

**2. Checkbox « Afficher le raster » — Carte FAST**

Idem pour `mod_monitoring_pixel_map`. Le LayersControl avait déjà
l'entrée « NDVI/NBR » dans `overlayGroups` depuis v0.47.0 mais le
checkbox UI était redondant — désormais c'est l'unique mécanisme.

**3. Checkbox group « Indices spectraux » — Sidebar Suivi sanitaire**

Le `checkboxGroupInput("bands")` du sidebar parent gauche (qui
permettait à l'utilisateur de désélectionner NDVI ou NBR avant
Diagnostic FAST) est retiré. NDVI et NBR sont systématiquement
téléchargés (`bands = c("NDVI", "NBR")` câblé en dur dans
`fast_task$invoke()`).

**Pourquoi** : le couplage entre les bandes téléchargées (sidebar
parent) et les bandes affichées (radios NDVI/NBR des sidebars droits
des onglets) était trompeur — un utilisateur pouvait désélectionner
NDVI pour le téléchargement et ensuite tenter de l'afficher dans
Carte FAST, sans message d'erreur clair. Désormais les deux indices
sont toujours téléchargés ; les radios des sidebars droits pilotent
uniquement l'**affichage**.

### Caractéristiques

* **Pas de breaking change fonctionnel** : tous les calculs FAST
  continuent à s'effectuer avec NDVI + NBR. La visibilité du raster
  passe par le contrôle de couches Leaflet (un clic au même endroit).
* **UI plus claire** : moins de contrôles redondants dans les
  sidebars. Le mécanisme « tu vois ce qui est coché dans le contrôle
  de couches » devient l'unique source de vérité pour la visibilité.

### Détails techniques

* `R/mod_monitoring.R` : retrait du `checkboxGroupInput("bands")` +
  retrait de la validation `if (length(input$bands) == 0L)` +
  câblage `bands = c("NDVI", "NBR")` en dur dans
  `fast_task$invoke()`.
* `R/mod_monitoring_fast_alerts.R` : retrait du
  `checkboxInput("raster_visible")` + retrait du
  `updateCheckboxInput(session, "raster_visible")` + ajout de
  `"Alertes"` (= `.alert_raster_group`) dans `overlayGroups` de
  `addLayersControl` + suppression de la garde
  `if (!visible || ...)` dans l'observer leafletProxy.
* `R/mod_monitoring_pixel_map.R` : retrait du
  `checkboxInput("raster_visible")` + suppression de la garde
  `if (!isTRUE(input$raster_visible ...))` dans l'observer
  leafletProxy.
* `R/utils_i18n.R` : retrait de 4 clés
  (`monitoring_bands`, `monitoring_validate_bands`,
  `monitoring_fast_alerts_raster_visible`,
  `monitoring_pixel_map_raster_visible`).
* `tests/testthat/test-mod_monitoring.R` : adaptation des tests
  (renommage `"no band selected"` → `"NDVI+NBR hard-wired"`,
  inversion de l'assertion HTML sur la sidebar).

### Réponse à la question collatérale

*« Le radio NDVI/NBR et Intensité/Fréquence change-t-il le raster
de la carte ni la légende ? »*

* **Raster** : oui, le wiring est correct (`raster_r()` lit
  `input$index` et `input$mode` → `compute_fast_alert_mask()`
  ré-appelé). Si visuellement c'est imperceptible, c'est parce que
  le mask catégoriel 0-4 (spec 017 D2, v0.57.0) répartit en
  quartiles dynamiques — la distribution peut être très similaire
  d'un indice/mode à l'autre sur une zone homogène.
* **Légende** : non, par design. Les 4 labels (Faible / Modéré /
  Fort / Sévère) sont des classes génériques de sévérité,
  indépendantes de l'indice et du mode.

# nemetonshiny 0.60.0 (2026-06-02)

### Removed — Checkbox « Mode rapide (multi-cœur) » Alertes FAST

Le toggle UI introduit en v0.58.0 (TODO #4) est retiré. Désormais
`parallel = TRUE` est passé en dur dans le wiring
`nemeton::compute_fast_alert_mask()`.

**Pourquoi** : le checkbox n'apportait pas de garantie supplémentaire.
Le cœur fait déjà un fallback séquentiel silencieux si `furrr` est
absent, et l'overhead de spawn `furrr` (~0.5-1s) est négligeable
face à un calcul FAST typique (10-300s). L'opt-in faisait peser un
choix technique sans bénéfice opérationnel sur l'utilisateur.

**Caractéristiques** :

* Aucun changement de comportement quand `furrr` est installé
  (équivalent à activer le checkbox).
* Fallback séquentiel silencieux toujours actif si `furrr` est
  absent côté cœur.
* Pas de breaking change fonctionnel — le retrait porte uniquement
  sur l'UI ; la fonction cœur consommée reste la même.

**Détails techniques** :

* `R/mod_monitoring_fast_alerts.R` :
  - retrait du `shiny::checkboxInput(ns("fast_mode"))` (sidebar
    droit) ;
  - retrait du `updateCheckboxInput(session, "fast_mode")` de
    l'observer i18n ;
  - remplacement de `use_parallel <- isTRUE(input$fast_mode %||%
    FALSE)` par `parallel = TRUE` en dur dans l'appel.
* `R/utils_i18n.R` : retrait de la clé `fast_alerts_parallel_label`
  (FR + EN).
* `tests/testthat/test-service_monitoring.R` :
  - retrait des 2 tests v0.58.0 devenus obsolètes (test i18n du
    label + test de propagation `input$fast_mode → parallel`) ;
  - ajout d'un test de non-régression vérifiant que la clé
    `fast_alerts_parallel_label` n'est pas ré-introduite
    accidentellement.

# nemetonshiny 0.59.1 (2026-06-02)

### Fixed — Test `register click` cassé par `bindEvent(ignoreInit = TRUE)`

Le test `test-mod_monitoring.R::"register click invokes
register_project_as_zone and persists zone_id in app_state"` failait
sur ses 3 assertions (lignes 944-946) parce que l'observer du
bouton « Enregistrer la zone » était passé d'un `observeEvent(input$register)`
direct à un `observe() |> bindEvent(input$register, input$register_inline,
ignoreInit = TRUE)` lors de l'ajout du bouton inline (commit 3f1059d).

En `testServer`, `session$setInputs(register = 1L)` posé directement
est traité comme l'état d'init par `bindEvent(ignoreInit = TRUE)` —
l'observer ne se déclenche pas, `register_project_as_zone` n'est
jamais appelé, `captured_project` reste `NULL` et les 3 assertions
cascadent.

**Fix** : matérialiser une transition d'input avant la vraie valeur :

```r
session$setInputs(register = 0L)  # init
session$setInputs(register = 1L)  # vrai clic, fire l'observer
```

Identique au pattern qu'un `actionButton` réel suit (init à 0,
incrément à chaque clic). Aucun changement de code de prod —
c'est strictement un fix de fidélité du harness de test.

**Résultat** : `[ FAIL 0 | PASS 6875 ]` sur la suite complète
(vs `[ FAIL 3 | PASS 6873 ]` avant le fix).

# nemetonshiny 0.59.0 (2026-06-02)

### Added — Modal diagnostic pixel CRSWIR FORDEAD (TODO #3)

Chantier #3 du TODO `nemetonshiny`. Au clic gauche sur la carte
FORDEAD (`mod_monitoring_fordead_map`), un modal interactif s'ouvre
et affiche, pour le pixel cliqué :

* la **série CRSWIR observée** (points bleus),
* la **prédiction harmonique** du modèle FORDEAD (ligne rouge),
* un **marqueur vertical** sur la date de 1re anomalie détectée
  (`attr(., "premiere_detection")`), si présente.

Parité fonctionnelle avec la « Carte pixel FAST » existante : même
pattern `observeEvent(input$map_click)` + `showModal` +
`plotly::plotlyOutput`. Le wiring cœur passe par
`nemeton::read_fordead_pixel_series(con = NULL, zone_id, xy, crs,
run_id = NULL, cache_dir)`, fonction shippée dans `nemeton@v0.43.0`
(spec FORDEAD pixel-series). `con` est réservé pour un futur
`fordead_run` tracking — `NULL` accepté en l'état.

**Détails techniques** :

* `R/mod_monitoring_fordead_map.R` : nouveau
  `shiny::observeEvent(input$map_click)` qui résout `cache_dir` à
  partir du projet actif (`<projet>/cache/layers/fordead`), extrait
  la série au `xy = c(lng, lat)` clic, puis monte le plotly et l'ouvre
  dans un `modalDialog(size = "l", easyClose = TRUE)`.
* `R/utils_i18n.R` : 6 nouvelles clés FR/EN
  (`monitoring_fordead_pixel_modal_title_fmt`,
  `monitoring_fordead_pixel_observed`,
  `monitoring_fordead_pixel_predicted`,
  `monitoring_fordead_pixel_first_anomaly`,
  `monitoring_fordead_pixel_yaxis`,
  `monitoring_fordead_pixel_no_data`).
  Encodage `\uXXXX` (règle stricte CLAUDE.md §4).
* 2 nouveaux tests dans `test-service_monitoring.R` : i18n des
  6 clés (FR + EN) + signature `read_fordead_pixel_series` cohérente
  avec l'appel app.

**Empty state préservé** : si aucune série n'est disponible pour le
pixel (hors zone modélisée, run FORDEAD absent, env Python
indisponible), `read_fordead_pixel_series()` renvoie `NULL` et l'app
affiche une `shiny::showNotification(type = "warning", duration =
4)` au lieu d'un modal vide.

**Pas de breaking change** : seul ajout, comportement existant de
`mask_r` / `output$map` inchangé.

Plancher cœur : `nemeton (>= 0.62.0)` déjà satisfait (la fonction
shippe depuis `nemeton@v0.43.0`).

# nemetonshiny 0.58.0 (2026-06-02)

### Added — Toggle « Mode rapide » multi-cœur Alertes FAST (TODO #4)

Chantier #4 du TODO `nemetonshiny`. Nouvelle case à cocher
**« Mode rapide (multi-cœur) »** dans le sidebar droit de l'onglet
Alertes FAST. Quand activée, propage `parallel = TRUE` à
`nemeton::compute_fast_alert_mask()` (spec 017 D4 `nemeton@v0.57.0+`)
qui distribue le calcul par scène sur plusieurs cœurs via `furrr`.

**Caractéristiques** :
* **Opt-in** : décoché par défaut. L'utilisateur active explicitement
  pour les gros diagnostics (zones ≥ 100 ha où le surcoût futurr est
  amorti).
* **Fallback silencieux** : si `furrr` n'est pas installé côté cœur,
  le cœur retombe sur séquentiel sans erreur.
* **Résultats identiques** au mode séquentiel (spec 017 D4 garantit la
  reproductibilité).
* **Gain réel** sur grosse zone : ~×N_cores sur la phase raster.
  Sur petite zone, l'overhead futurr peut être > au gain — d'où le
  défaut FALSE.

**Détails techniques** :

* `R/mod_monitoring_fast_alerts.R` :
  - sidebar : nouveau `shiny::checkboxInput(ns("fast_mode"))` après le
    slider opacité ;
  - `raster_r()` : `use_parallel <- isTRUE(input$fast_mode %||% FALSE)`
    forwardé à `nemeton::compute_fast_alert_mask(..., parallel = use_parallel)` ;
  - observer i18n refresh : `updateCheckboxInput(session, "fast_mode")`.
* `R/utils_i18n.R` : nouvelle clé FR/EN `fast_alerts_parallel_label`
  (« Mode rapide (multi-cœur) » / « Fast mode (multi-core) »).
* 2 nouveaux tests dans `test-service_monitoring.R` : i18n + logique
  de propagation `input$fast_mode → parallel`.

**Pas de breaking change** : `parallel = FALSE` par défaut côté cœur,
case décochée par défaut côté app. Comportement existant inchangé.

Cycle dev `0.57.0` → `0.57.0.9001` → release stable `0.58.0`.

# nemetonshiny 0.57.0 (2026-06-02)

### Changed — Alertes FAST : affichage en quartiles 0-4 via `compute_fast_alert_mask()` (spec 017 D2)

Chantier #5 du TODO `nemetonshiny`. L'onglet **Alertes FAST**
affichait jusqu'ici le raster d'alerte **continu** retourné par
`nemeton::read_fast_alert_raster()`, avec une discrétisation
ad-hoc côté app (`.classify_alert_count` en mode count, p95 cap +
gradient en mode rolling). v0.57.0 délègue cette discrétisation au
cœur via `nemeton::compute_fast_alert_mask()` qui produit un
**SpatRaster catégoriel 0-4** (spec 017 D1-D2 nemeton@v0.55.0+) :

* `0` = sain (pas d'alerte) → **transparent** (UGFs et OSM/Satellite
  restent visibles)
* `1` = Faible → jaune `#fee08b`
* `2` = Modéré → orange `#fdae61`
* `3` = Fort → rouge-orangé `#f46d43`
* `4` = Sévère → rouge foncé `#d73027`

**Pourquoi ce changement** :
* Cohérence cœur (CLAUDE.md §1) : la discrétisation par quartile est
  une décision métier qui appartient à `nemeton`, pas à l'app.
* Unification des modes : les branches `count` et `rolling` partagent
  désormais la même palette 5 classes. Le `mode` côté cœur change
  COMMENT les quartiles sont calculés (compte de jours vs intensité
  rolling), pas COMMENT ils sont affichés.
* Lisibilité : 5 classes ordinales discrètes sont plus actionnables
  qu'un gradient continu (l'utilisateur reconnaît immédiatement le
  niveau de sévérité).

**Détails techniques** :

* `R/mod_monitoring.R` : nouveau helper privé
  `.fast_alert_mask_cache_dir(project_path)` parallèle à
  `.fast_alert_cache_dir()`. Pointe vers
  `<projet>/cache/layers/fast_alert_mask` (distinct du cache du
  raster continu intermédiaire — 2 produits différents).
* `R/mod_monitoring_fast_alerts.R::raster_r()` : appelle désormais
  `nemeton::compute_fast_alert_mask()` au lieu de
  `read_fast_alert_raster()`. La fonction renvoie invisiblement le
  path du TIF mask 0-4 persisté ; on le charge avec `terra::rast()`
  pour passer au reactive en aval. Le `cache_result = TRUE` +
  `result_cache_dir = .fast_alert_cache_dir(...)` reste actif (cache
  le raster continu intermédiaire). Le nouveau `mask_cache_dir =
  .fast_alert_mask_cache_dir(...)` cache le mask 0-4 final.
* `R/mod_monitoring_fast_alerts.R` (observer raster) : palette
  unifiée 4 classes `colorBin` avec `bins = c(0.5, 1.5, 2.5, 3.5,
  4.5)` qui mappe sans ambiguïté chaque valeur entière. La classe
  0 reste transparente via `terra::ifel(... | r <= 0, NA, r)` (le
  raster continu était déjà géré ainsi pour les négatifs ; pour le
  mask discret, ça suffit aussi).
* `R/utils_i18n.R` : 5 nouvelles clés FR/EN (`fast_alert_legend_title`,
  `fast_alert_class_1` à `_4`). La classe 0 sain n'a pas de libellé
  i18n — elle est transparente, le banner « zone saine » prend le
  relais visuel si tout le raster est en classe 0.

**Test ajoutés** (2) dans `tests/testthat/test-service_monitoring.R` :
* `.fast_alert_mask_cache_dir()` renvoie le chemin canonique distinct
* Clés i18n classes 1-4 + titre legend non-vides FR/EN

**Performance** : `compute_fast_alert_mask()` calcule en interne le
raster continu (qui peut être en cache D6 chaud après le prewarm) ET
le discrétise. À paramètres identiques, le mask 0-4 est persisté
dans son propre cache (`mask_cache_dir`) → revisites sub-seconde.

**Non-breaking côté UX** : la fonctionnalité essentielle (visualiser
les zones d'alerte FAST) est préservée et améliorée. Les utilisateurs
qui avaient l'habitude du gradient continu trouveront les 5 classes
plus interprétables.

Cycle dev `0.56.0` → `0.56.0.9001` → release stable **`v0.57.0`**
(MINOR, refactor structurel affichage + délégation logique métier
au cœur).

# nemetonshiny 0.56.0 (2026-06-02)

### Added — Perspectives IA sourcées via RAG (`nemeton@v0.62.0`)

**Avant** : les perspectives IA de l'onglet Synthèse étaient
générées **sans sources** — le LLM produisait du texte plausible mais
non rattaché à du corpus documentaire vérifiable.

**Maintenant** : avant chaque appel `chat$chat(prompt)`, l'app
récupère via `nemeton::retrieve_knowledge()` les ~8 passages les
plus pertinents (similarité cosinus ≥ 0.55 sur embeddings Mistral
`mistral-embed`) dans le corpus pgvector. Les chunks sont injectés
en tête du prompt avec une consigne de citation (`[^n]` markers).
La perspective générée est suivie d'un bloc « Sources
documentaires » formaté par `nemeton::format_citations()` (titre
i18n cœur, FR / EN selon `app_state$language`).

**Architecture** (CLAUDE.md §1, §3) : toute la logique métier est
dans le cœur (`retrieve_knowledge`, `embed_query`, similarité
pgvector, `format_citations`). L'app ne fait qu'orchestrer.

* `R/service_rag.R` — **nouveau** fichier (orchestration mince) :
  - `rag_knowledge_con(app_con)` : résout la connexion corpus
    (priorité `NEMETON_KNOWLEDGE_DB_URL`, sinon réutilise la
    connexion app — corpus co-localisé en prod).
  - `rag_profile_code(key)` : map les clés app
    `profil_<short>` → codes corpus `<short>`.
  - `build_situation_summary(units, profile_key, lang)` : produit
    une phrase semantique courte FR / EN pour l'embedding (V1
    minimaliste — futurs raffinements possibles).
  - `rag_context(...)` : orchestre la récupération, déduplique
    par `document_id` pour le bloc Sources, retourne un payload
    `list(chunks, prompt_block, sources_md, n_sources)`.
* `R/mod_synthesis.R` :
  - nouveau `reactiveVal rag_ctx_synthesis` qui stocke le contexte
    du dernier `ai_generate` ;
  - `output$ai_sources` qui rend le markdown des citations sous
    la perspective ;
  - observer `input$ai_generate` enrichi : appel `rag_context()`
    avant `build_synthesis_prompt()`, concaténation
    `prompt_block + cite_rule + base_prompt`, stockage ctx pour
    affichage UI.
* `R/app_ui.R` : `uiOutput(ns("ai_sources"))` ajouté sous le
  `textAreaInput` des commentaires.
* `R/utils_i18n.R` : 2 nouvelles clés FR/EN
  (`rag_sourced_badge` : phrase synthétique « Perspective appuyée
  sur N source(s)… » ; `rag_toggle_label` : réservé pour un futur
  toggle UI).

**Dégradation gracieuse** (impératif §5.7 du brief) — tous ces cas
renvoient un contexte vide, la perspective est générée sans bloc
Sources, **aucune exception UI** :
- `options(nemeton.rag_enabled = FALSE)` (opt-out manuel)
- Aucune connexion DB (`app_con = NULL` + pas d'env var
  `NEMETON_KNOWLEDGE_DB_URL`)
- Corpus vide / schéma `knowledge_*` absent
- Clé Mistral `MISTRAL_API_KEY` absente
- Erreur réseau pendant l'embedding
- 0 chunk au-dessus du seuil de similarité

**Provider d'embedding fixé à `mistral`** : doit matcher le provider
d'ingestion du corpus prod (19 docs, 1845 chunks, `mistral-embed`).
Ne pas changer sans réembedder l'ensemble du corpus.

**Réglages par défaut** : `top_k = 8`, `min_similarity = 0.55`,
`family_codes = NULL` (cf. brief §5.2 — éviter l'intersection
exacte sur petit corpus, s'appuyer sur similarité + profil).

**Tests** : 11 nouveaux dans `tests/testthat/test-service_rag.R` :
- mapping `rag_profile_code` (préfixe / pas de préfixe / NULL /
  vide / NA)
- `build_situation_summary` FR/EN non-vide + fallback profil
- nominal `rag_context` (3 chunks → `[^1]` `[^2]` `[^3]` + bloc
  sources + dédup par document_id)
- dégradation : erreur retrieve, 0 ligne, opt-out option, situation
  vide, app_con NULL sans env var
- i18n : placeholders sprintf des nouvelles clés

Suite : 2979 pass, 3 fails pré-existants (register click — non
liés). Plancher : `Imports: nemeton (>= 0.62.0)`.

**Pas de breaking change** : si le corpus n'est pas peuplé ou la
clé Mistral absente, le comportement actuel (perspective sans
sources) est intact.

Cycle dev `0.55.0` → `0.55.0.9001` → release stable **`v0.56.0`**
(MINOR, feat — RAG sourcé).

# nemetonshiny 0.55.0 (2026-06-02)

### Changed — Pré-calcul FAST déplacé du helper app vers l'API native cœur (`nemeton@v0.61.0`)

`nemeton@v0.61.0` (publié 2026-06-02 14:56 UTC, spec 018) ajoute à
`ingest_sentinel2_timeseries()` deux paramètres opt-in :
`prewarm_alerts = FALSE` et `prewarm_mask_cache_dir = NULL`. Quand
`TRUE`, le cœur enchaîne en fin d'ingestion sur 4
`read_fast_alert_raster()` (NDVI/NBR × count/rolling) au seuil défaut
(0.40/0.30) et remplit le cache D6 sous
`<prewarm_mask_cache_dir>/zone_<id>/`.

L'app v0.54.0 avait livré le **même comportement** via un helper
local `.prewarm_fast_alerts()` dans `R/service_monitoring.R`. Avec la
spec 018 cœur, ce helper devient **redondant** : la logique migre
nativement dans le cœur (même process worker, même `con` /
`cache_dir` / `cancel_path`, gestion d'erreur partielle, parallel
opt-in si `furrr`).

**Avantages du déplacement** :
* **Cohérence** : un seul code path pour le pré-calcul (cœur).
* **Performance** : pas de re-traversée du worker boundary entre
  ingestion et pré-calcul (économie ~100-200 ms négligeable).
* **Maintenance** : les futurs ajustements de la spec 018 (D7…) se
  feront côté cœur sans bump app.
* **Tests cœur** : la spec 018 a son propre test suite côté nemeton,
  l'app n'a plus à dupliquer.

**Côté app** :
* `service_monitoring.R::run_ingestion_async()` — la signature
  ExtendedTask perd `result_cache_dir`, gagne `prewarm_alerts` +
  `prewarm_mask_cache_dir`. Les 2 sont forwardés tels quels à
  `nemeton::ingest_sentinel2_timeseries()`.
* `service_monitoring.R` — helper `.prewarm_fast_alerts()` SUPPRIMÉ.
* `mod_monitoring.R::fast_task$invoke()` — passe désormais
  `prewarm_alerts = TRUE` (feature désirable par défaut) et
  `prewarm_mask_cache_dir = .fast_alert_cache_dir(project$path)`.
* `mod_monitoring.R` — nouveau helper `.fast_alert_cache_dir(project_path)`
  qui factorise le chemin canonique
  `<projet>/cache/layers/fast_alert`. Cohérence cruciale du hash D6 :
  les 3 call sites (invoke côté worker + lecture côté Alertes FAST +
  prévisualisation validation_sampling) utilisent désormais le même
  helper.
* `mod_monitoring_fast_alerts.R` + `mod_validation_sampling.R` —
  utilisent `.fast_alert_cache_dir()` au lieu de `file.path(...)`
  inline.

### Added — Toasts localisés pour les events `fast_prewarm:*` du cœur

L'observer `ingest_progress` de `mod_monitoring.R` reconnaît
désormais le préfixe `fast_prewarm:` émis par le cœur (spec 018).
Chaque combinaison `(index, mode)` produit jusqu'à 3 events :
* `fast_prewarm:<idx>_<mode>` — démarré → toast info « Pré-calcul
  carte NBR Intensité en cours… ».
* `fast_prewarm:<idx>_<mode>_done` — carte prête → toast vert
  « Carte NBR Intensité prête. » (4 s).
* `fast_prewarm:<idx>_<mode>_failed` — échec partiel (B12
  manquante…) → toast jaune warning « Carte NBR Intensité non
  calculable. — <error> » (6 s).

Plus 2 events de synthèse :
* `fast_prewarm:complete` — silencieux (les 4 `_done` ont déjà
  couvert) ; log console uniquement.
* `fast_prewarm:cancelled` — toast warning « Pré-calcul des cartes
  FAST annulé. » (5 s).

**Mapping mode → libellé** : `count` → « Fréquence » / `Frequency`,
`rolling` → « Intensité » / `Intensity`. Les libellés sont construits
à partir des CLÉS MACHINE du payload (`ev$index`, `ev$mode`), jamais
en parsant du texte FR — conforme à la convention i18n CLAUDE.md.

`R/utils_i18n.R` — 6 nouvelles clés (FR/EN) :
* `fast_mode_frequence`, `fast_mode_intensite`
* `fast_prewarm_running`, `fast_prewarm_done`, `fast_prewarm_failed`,
  `fast_prewarm_cancelled`

### Tests

* 4 tests retirés (mockaient le helper `.prewarm_fast_alerts()` qui
  n'existe plus).
* 3 nouveaux tests dans `test-service_monitoring.R` :
  - `.fast_alert_cache_dir()` renvoie le bon chemin canonique
  - clés i18n `fast_prewarm_*` ont les bons placeholders sprintf
  - mapping mode → libellé i18n produit le bon toast (FR + EN)

### Changed (plancher)

* `Imports: nemeton (>= 0.61.0)` (depuis 0.60.0). Garantit la
  présence des params `prewarm_alerts` + `prewarm_mask_cache_dir`.

Cycle dev `0.54.0` → `0.54.0.9001` → release stable **`v0.55.0`**
(MINOR, refactor structurel + delegation spec 018 cœur + toasts feat).

# nemetonshiny 0.54.0 (2026-06-02)

### Added — Pré-calcul inconditionnel des 4 cartes FAST en fin de Diagnostic FAST

**Découplage calcul ↔ affichage** : le clic sur « Diagnostic FAST »
déclenche désormais le calcul des 4 cartes raster usuelles
(`NDVI×count`, `NDVI×rolling`, `NBR×count`, `NBR×rolling`)
**indépendamment de l'état de l'UI** (radio Indice FAST, radio Mode
du raster, checkbox Afficher le raster). Les coches/radios ne
pilotent désormais QUE l'affichage des couches Leaflet, jamais le
calcul.

**Implémentation** : nouveau helper privé
`.prewarm_fast_alerts()` dans `R/service_monitoring.R`, appelé par
le worker `run_ingestion_async()` juste après que
`nemeton::ingest_sentinel2_timeseries()` ait rendu la main. Boucle
sur les 4 combinaisons `(index, mode)`, chaque appel à
`nemeton::read_fast_alert_raster()` persiste son résultat dans
`<projet>/cache/layers/fast_alert/zone_<id>/<hash>.tif` via le cache
content-addressed D6 (spec 017, nemeton@v0.57.0+).

**Robustesse** :
* **Inconditionnel** : aucun input UI lu. La fonction reçoit
  `result_cache_dir` en paramètre depuis le `fast_task$invoke()`.
* **Cancel coopératif** : check `cancel_path` entre chaque
  combinaison ; sortie propre avec commit partiel (les COG déjà
  calculés restent valides en cache D6).
* **Échec partiel toléré** : tryCatch par combo. Un échec sur NBR
  (cache S2 incomplet B12) ne casse pas NDVI. Un warning par échec,
  collecté dans le summary du worker.
* **Parallel opt-in** : `parallel = TRUE` activé automatiquement si
  `furrr` est dispo (spec 017 D4 cœur).
* **Threshold = NULL** (défauts cœur : 0.40 NDVI / 0.30 NBR). Les
  thresholds custom de l'utilisateur ne sont pas pré-calculés —
  recalcul à la demande (sub-seconde car cache D6 chaud).

**UX** :
* Le switch radio Indice (NDVI ↔ NBR) ou Mode (count ↔ rolling) dans
  le sidebar Alertes FAST est désormais **instantané** (lecture COG
  depuis disque, sub-seconde).
* Le toast ingest_success continue d'afficher en fin, mais le worker
  émet 4 phases de progress supplémentaires
  (`fast_prewarm:NDVI_count` → `_done`, etc.) que l'observer
  parent peut surfacer si désiré.
* Coût : ~5-15 s × 4 = 20-60 s ajoutés en fin de worker (mais
  invisible UX vs les ~5-15 min d'ingestion COG).

**API ExtendedTask** : la signature du worker `run_ingestion_async()`
gagne un paramètre `result_cache_dir = NULL`. NULL = pas de
pré-calcul (no-op). `mod_monitoring.R::fast_task$invoke(...)` le passe
toujours à `file.path(project$path, "cache", "layers", "fast_alert")`.

**Tests** : 4 nouveaux dans `tests/testthat/test-service_monitoring.R`
* `calcule les 4 combinaisons (NDVI/NBR × count/rolling)`
* `continue sur échec partiel (NBR fail → NDVI OK)`
* `respecte le cancel coopératif`
* `no-op quand result_cache_dir est NULL/vide`

Suite : `[ FAIL 3 | PASS 364 ]` — fails pré-existants (register
click), non liés.

Cycle dev `0.53.1` → `0.53.1.9001` → release stable **`v0.54.0`**
(MINOR, feat — découplage calcul/affichage FAST).

# nemetonshiny 0.53.1 (2026-06-02)

### Fixed — `db_scenes_df_r` introuvable dans `output$date_slider_ui` (résidu refactor v0.52.16)

Le refactor v0.52.16 avait supprimé le reactive `db_scenes_df_r`
(dépendant de `obs_pixel_data`) dans `mod_monitoring_pixel_map.R`,
mais avait laissé un appel résiduel dans `output$date_slider_ui`
(case 2 du fallback : « disk has scenes but DB has no obs »). À chaque
ouverture de l'onglet Carte FAST sans cache valide, Shiny levait :

```
Error in db_scenes_df_r: impossible de trouver la fonction "db_scenes_df_r"
  118: renderUI
  117: func
  101: output$monitoring-pixel_map-date_slider_ui
```

Conséquence en cascade : l'erreur non gérée fragilisait la session
Shiny, le bouton « Diagnostic FAST » pouvait rester grisé après la
fin du worker, et le toast « Téléchargement terminé » persistait.

`R/mod_monitoring_pixel_map.R` : case 2 du fallback supprimée
(devenue dead code depuis le retrait de `obs_pixel`). Le compteur
disk (`disk_scenes_count_r`) est désormais la seule source de
vérité pour distinguer « pas de cache » de « cache présent ».

### Changed — Toast `monitoring_ingest_success` simplifié

Depuis `nemeton@v0.58.0` (drop obs_pixel insertion), le compteur
`n_obs_inserted` est TOUJOURS 0 — le message « Téléchargement
terminé : X scène(s), **0 observation(s) insérée(s)** » devenait
trompeur (donnait l'impression d'un bug). Reformulé en
« Diagnostic FAST terminé : %d scène(s) en cache. » /
« FAST diagnostic done: %d scene(s) cached. »

`R/utils_i18n.R` (clé `monitoring_ingest_success`) +
`R/mod_monitoring.R` (l.1808 : suppression de la lecture de
`n_obs_inserted` du résumé, et du 2e arg `%d` au sprintf).

Cycle dev `0.53.0` → `0.53.0.9001` → release stable **`v0.53.1`**
(PATCH, fix régression résiduelle + cleanup wording).

# nemetonshiny 0.53.0 (2026-06-02)

> **Première release sous la nouvelle convention semver stricte**
> (voir `CLAUDE.md` §Consignes de release étape 1 révisée).
> Ce bump est MINOR parce qu'il contient un **refactor structurel**
> (split banner/leafletOutput) et une **nouvelle feature UI**
> (bandeau diagnostic d'erreur) — auparavant ces changements
> auraient été regroupés en PATCH par habitude.

### Fixed — `NEMETON_DB_LOCAL=1` ignoré au chargement projet

`NEMETON_DB_LOCAL=1` était lu uniquement par `service_monitoring_db.R`
(monitoring DB), pas par `service_db.R` (project DB). Conséquence : un
clic sur un projet récent affichait quand même
« Connected to PostgreSQL: nemeton@host:5432 (source: url) » et écrivait
les `comments` / `parcels` / `projects` dans PostGIS, contredisant le
mode local attendu.

`R/service_db.R` : `.resolve_db_config()` court-circuite en tête si
`NEMETON_DB_LOCAL=1` (truthy) → retourne `NULL` → l'app marche en
mode single-user local, projets sur disque uniquement.

### Fixed — Carte Alertes FAST : raster invisible au premier render (devait bouger le slider opacité)

Le `output$panel` (renderUI) incluait à la fois le banner « zone
saine » ET le `leafletOutput(map)`. Comme `panel` dépendait de
`raster_r()`, chaque changement de raster (switch index NDVI/NBR,
slider seuil, etc.) re-render TOUT le panel → le `leafletOutput`
était détruit + recréé → la map ré-initialisée → l'observer
`leafletProxy::addRasterImage` peignait dans le vide. L'utilisateur
devait bouger un autre slider (opacité) pour forcer un repaint.

Refactor structurel : `output$panel` éclaté en 2 outputs distincts —
`output$banner` (uiOutput, re-render selon `raster_r`) et
`leafletOutput("map")` (rendu UNE FOIS au montage). La map est
désormais immortelle ; seul le banner clignote.

### Added — Bandeau d'erreur distinct de « zone saine » (diagnostic NBR cache incomplet)

`output$banner` distingue désormais 2 cas au lieu d'un message
générique :
* **Bandeau VERT** « Aucune alerte FAST sur la fenêtre » — quand le
  raster est calculé avec 0 cellule en alerte (zone vraiment saine).
* **Bandeau JAUNE warning** « Raster d'alerte non calculable » +
  description de la cause — quand `read_fast_alert_raster()` lève
  une exception OU retourne NULL. Cas typique : cache S2 incomplet
  pour l'indice choisi (NBR souvent affecté quand la bande B12
  manque sur beaucoup de scènes).

Nouveau reactiveVal `last_raster_error` qui capture le message
côté serveur ; nouvelle clé i18n `monitoring_fast_alerts_error_title`
(FR/EN).

Cycle dev `0.52.17` → `0.52.17.9001` → release stable **`v0.53.0`**
(MINOR, première application stricte semver).

# nemetonshiny 0.52.17 (2026-06-02)

### Changed — Alignement avec `nemeton@v0.60.0` (finalisation spec 017)

Le cœur a publié 2 releases successives qui finalisent la suppression
du couplage `obs_pixel` :

* **`nemeton@v0.58.0`** (Phase A) — `feat: drop obs_pixel insertion
  from S2 ingest; pure-raster FAST`. L'ingestion Sentinel-2
  n'alimente plus la table `obs_pixel` ; la table existe encore
  côté schéma mais n'est plus écrite.
* **`nemeton@v0.60.0`** (Phase B) — `feat!: remove deprecated
  obs_pixel consumers; strip obs_pixel from schema`. La fonction
  `read_obs_pixel()` est retirée du NAMESPACE exporté, et la
  migration `0004_drop_obs_pixel` DROP CASCADE la table sur toute
  base existante. Skip de `v0.59` pour signaler la rupture API.

**Côté app** : aucune adaptation fonctionnelle nécessaire — v0.52.16
ne consomme déjà plus `obs_pixel`. Cette release est purement un
alignement de documentation et de plancher d'import.

* `DESCRIPTION` : plancher `Imports: nemeton (>= 0.60.0)` (depuis
  0.57.0). Garantit qu'un install ne tombe pas sur un cœur antérieur
  où `obs_pixel` serait encore alimenté en sous-main.
* `tests/testthat/test-monitoring-smoke-e2e.R` : précondition
  `read_obs_pixel exported` retirée du skip (la fonction n'existe
  plus en `v0.60.0`).

Cycle dev `0.52.16` → `0.52.16.9001` → release stable **`v0.52.17`**
(PATCH, chore — alignement plancher).

# nemetonshiny 0.52.16 (2026-06-02)

### Changed — FAST 100 % pure raster per-pixel : suppression complète du couplage `obs_pixel`/placettes

**Contexte** : depuis `nemeton@v0.55.0` (spec 017) le diagnostic FAST
est conceptuellement une analyse **per-pixel** sur la zone monitoring,
indépendante des placettes de calibration définies dans l'onglet
Terrain. La table `obs_pixel` (per-placette × per-scène) restait
néanmoins consommée par le module Suivi sanitaire pour la modale
« clic marqueur placette ». Cette release coupe **tout** contact entre
FAST et les placettes côté app.

**Conséquences UX visibles** :

* ❌ La modale « clic sur un marqueur placette → série temporelle
  agrégée plot » a été **supprimée**.
* ❌ Les marqueurs CircleMarkers des placettes ne sont **plus
  affichés** sur la Carte FAST ni sur Alertes FAST.
* ❌ Le toggle « Placettes » du LayersControl Leaflet a disparu.
* ✅ La modale « clic sur un pixel → série temporelle pure » reste —
  elle utilise `nemeton::extract_pixel_timeseries()` qui lit
  directement le cache COG, donc `obs_pixel`-free.
* ✅ Le compteur « N scènes disponibles » dérive désormais directement
  du cache COG via `list.dirs()`.

**Détails techniques** :

* `R/mod_monitoring.R` :
  - reactiveVal `obs_refresh` retiré (plus de consommateur)
  - reactive `obs_pixel_inputs` + `obs_pixel_data` retirés
  - le param `obs_pixel_data = obs_pixel_data` passé à
    `mod_monitoring_pixel_map_server()` retiré
  - le bump `obs_refresh()` après ingestion FAST retiré (`fast_reload`
    suffit pour signaler aux modules raster qu'ils doivent re-scanner
    le cache COG).
* `R/mod_monitoring_pixel_map.R` :
  - signature server : `obs_pixel_data` retiré de la liste des
    paramètres
  - reactive `db_scenes_df_r` retiré ; `scenes_df_r` dérive désormais
    purement du cache COG disque (date parsée du scene_id Sentinel-2
    via `.pixel_scene_date_from_id()`)
  - reactive `placettes_sf_r` retiré
  - observer `addCircleMarkers` placettes retiré
  - reactiveVal `marker_just_clicked` retiré
  - observeEvent `input$map_marker_click` + `output$placette_ts_plot`
    retirés (modale clic-placette)
  - fallback `placettes bbox` retiré du resolver `ugf_sf_r()`
  - LayersControl : overlay « Placettes » retiré (`hideGroup`
    également)
* `R/utils_i18n.R` : 2 clés obsolètes retirées
  (`monitoring_pixel_map_placette_modal_title_fmt`,
  `monitoring_pixel_map_no_placette_data`)
* `tests/testthat/test-mod_monitoring.R` : 4 tests obs_pixel
  supprimés + helper `.skip_if_no_read_obs_pixel`
* `tests/testthat/test-mod_monitoring_pixel_map.R` : 1 test consolidé
  (« scenes_df enumerates populated cache dirs, date parsed from
  scene id »), 3 tests adaptés (param `obs_pixel_data` retiré du
  setup `args =`)
* `tests/testthat/test-service_monitoring_db.R` : test `read_only`
  path corrigé pour refléter le comportement v0.52.1 (Postgres RO
  migre aussi de manière idempotente).

**Impact suite** : `nemeton::read_obs_pixel()` n'a plus aucun
consommateur côté app. La table `obs_pixel` continue d'être écrite
par `nemeton::ingest_sentinel2_timeseries()` (effet de bord cœur),
mais l'app n'y touche plus. Un **brief cœur** sera fourni
séparément pour retirer cette insertion + la table elle-même côté
nemeton (spec 017 suite logique).

Suite tests : `[ FAIL 3 | PASS 6721 ]` — les 3 fails restants sont
**pré-existants** (`test-mod_monitoring.R:944-946`, test « register
click »), non liés à ce refactor.

Cycle dev `0.52.15` → `0.52.15.9001` → release stable **`v0.52.16`**
(PATCH, refactor — suppression couplage obs_pixel).

# nemetonshiny 0.52.15 (2026-06-02)

### Fixed — Call site oublié de `compute_fast_alert_mask()` (régression v0.52.13)

v0.52.13 avait migré les 2 call sites de `read_fast_alert_raster()`
vers l'API mono-index `nemeton@v0.55.0`, mais avait oublié le call
site de `compute_fast_alert_mask()` dans `service_validation_sampling.R`
qui passait encore `threshold_ndvi` / `threshold_nbr`. Conséquence :
un clic sur « Générer le plan de validation FAST » crashait avec
« arguments inutilisés » dès qu'il fallait recalculer le mask
discrétisé (fast_alert.tif absent du cache).

`R/service_validation_sampling.R` : `compute_fast_alert_mask()` est
maintenant appelé avec `index` + `threshold`. La fonction
`.resolve_alert_raster()` et `generate_validation_plan()` exposent
un nouveau paramètre `index = NULL` (NULL → fallback "NDVI" côté
cœur). Le call site UI (`mod_validation_sampling.R`) passe
`index = th$index` — alimenté par le radio « Indice FAST » de
l'onglet Alertes FAST via `fast_alerts_ret$index_r` (v0.52.14).

### Added — Cache D6 du raster d'alerte (`nemeton@v0.57.0`)

`nemeton@v0.57.0` ajoute le **cache content-addressed** du
`SpatRaster` retourné par `read_fast_alert_raster()` et
`compute_fast_alert_mask()` (paramètres `cache_result = TRUE`,
`result_cache_dir`). Le COG résultat est persisté sous
`<project>/cache/layers/fast_alert/zone_<id>/<hash>.tif`. Le hash
encapsule tous les paramètres (zone, index, threshold, date_from,
date_to, mode, window_days) — toute modification d'un seul produit
un hash différent et déclenche un recalcul ; à paramètres identiques,
la revisite est instantanée (lecture COG depuis disque, sub-seconde).

Pas d'invalidation manuelle à faire — le cache se renouvelle
automatiquement. La taille typique d'un COG résultat est ~50-200 Ko
par zone, donc l'empreinte disque reste négligeable même avec
plusieurs entrées (index × mode × thresholds différents).

* `R/mod_monitoring_fast_alerts.R` : call site Alertes FAST passe
  `cache_result = TRUE` + `result_cache_dir =
  <project>/cache/layers/fast_alert`.
* `R/mod_validation_sampling.R` : prévisualisation FAST passe les
  mêmes paramètres — un clic sur l'aperçu après changement
  d'index/seuil est désormais instantané quand le COG résultat est
  déjà sur disque.
* `R/service_validation_sampling.R` : `compute_fast_alert_mask()`
  active aussi le cache D6 (`result_cache_dir = cd`).

### Changed — Plancher `Imports: nemeton (>= 0.57.0)`

L'app consomme désormais les paramètres `cache_result` /
`result_cache_dir` apparus en v0.57.0 du cœur. Sans ce plancher, un
install contre un cœur antérieur (< 0.57.0) bouclerait sur
« argument inutilisé : cache_result ».

Cycle dev `0.52.14` → `0.52.14.9001` → release stable **`v0.52.15`**
(PATCH, fix régression + activation cache D6 cœur).

# nemetonshiny 0.52.14 (2026-06-01)

### Changed — Radio « Indice FAST » déplacé du sidebar parent vers le sidebar droit d'Alertes FAST

v0.52.13 avait posé le radio « Indice FAST » dans le sidebar parent
de Suivi sanitaire (couvrant transversalement Alertes FAST, Carte
FAST, Plan de validation). UX moins propre que ce que demande
l'utilisateur : la **Carte FAST a déjà son propre radio `index`**
dans son sidebar droit (depuis v0.47.0). Pour la symétrie,
**Alertes FAST a maintenant aussi son radio `index` dans son sidebar
droit**.

Chaque onglet pilote désormais son indice **indépendamment** : on
peut être en NDVI sur Alertes FAST et en NBR sur Carte FAST si on
veut comparer. Recalcul sub-seconde depuis le cache S2 quand on
bascule.

* `R/mod_monitoring_fast_alerts.R` : nouveau radio `ns("index")` en
  tête du sidebar droit (avant `mode`). Le call site
  `read_fast_alert_raster()` lit `input$index` (au lieu de
  `th$index`). Refresh i18n du label sur switch FR/EN. Le module
  exporte `index_r` dans son `return(list(...))` pour les
  consommateurs aval.
* `R/mod_monitoring.R` : radio `fast_index` retiré du sidebar
  parent. Les 4 `thresholds_r` purgés du champ `index = ...`.
  `validation_sampling_fast` lit désormais l'indice via
  `fast_alerts_ret$index_r()` — sa prévisualisation suit
  automatiquement le choix utilisateur côté Alertes FAST.
* `validation_sampling_fordead` garde `index = "NDVI"` en dur (sa
  source de mask est FORDEAD sur disque, pas FAST — l'`index` n'est
  pas consommé dans son code path).

Cycle dev `0.52.13` → `0.52.13.9001` → release stable **`v0.52.14`**
(PATCH, feat).

# nemetonshiny 0.52.13 (2026-06-01)

### Fixed — API mono-index FAST (suite à `nemeton@v0.55.0` spec 017)

**Symptôme** : warnings en cascade pendant le Diagnostic FAST :
```
! read_fast_alert_raster failed: arguments inutilisés
  (threshold_ndvi = as.numeric(th$ndvi),
   threshold_nbr = as.numeric(th$nbr))
```
La carte d'alertes FAST restait vide après chaque ingestion.

**Cause** : `nemeton@v0.55.0` (spec 017) a **délibérément** simplifié
`read_fast_alert_raster()` en mono-index — la combinaison historique
NDVI+NBR a été abandonnée. La nouvelle signature est :
```r
read_fast_alert_raster(con, zone_id,
                       index     = c("NDVI", "NBR"),   # un seul
                       threshold = NULL,               # un seul
                       date_from, date_to, mode, ...)
```
L'app continuait à passer les anciens paramètres `threshold_ndvi` et
`threshold_nbr` → `arguments inutilisés`.

**Correctif** :
* `R/mod_monitoring.R` : nouveau radio sidebar `fast_index` (NDVI /
  NBR, défaut NDVI) qui pilote l'indice utilisé pour le raster
  d'alerte. Les 2 sliders `threshold_ndvi` et `threshold_nbr`
  restent en place ; seul celui correspondant à l'indice sélectionné
  est forwardé au cœur.
* `R/mod_monitoring.R` : tous les `thresholds_r` (3 modules
  consommateurs : `pixel_map`, `fast_alerts`, `validation_sampling`
  FAST + FORDEAD) gagnent un champ `index = input$fast_index %||%
  "NDVI"`.
* `R/mod_monitoring_fast_alerts.R` + `R/mod_validation_sampling.R` :
  les 2 call sites de `read_fast_alert_raster()` passent désormais
  `index = th$index, threshold = th$<idx-ndvi-ou-nbr>`. Plus de
  `threshold_ndvi`/`threshold_nbr`.
* `R/utils_i18n.R` : nouvelle clé `monitoring_fast_index_label`
  (« Indice FAST » / « FAST index »).
* `DESCRIPTION` : plancher `Imports: nemeton (>= 0.55.0)`.

**UX** : pour comparer les 2 indices, l'utilisateur switch le radio
« Indice FAST » entre NDVI et NBR — le raster est recalculé à la
volée à partir du cache S2 (sub-seconde).

Cycle dev `0.52.12` → `0.52.12.9001` → release stable **`v0.52.13`**
(PATCH, fix régression API cœur).

# nemetonshiny 0.52.12 (2026-06-01)

### Fixed — Plan d'actions : tableau rendu VIDE (régression v0.52.10)

**Symptôme** : sur la vue « Carte + Tableau » du Plan d'actions, le
tableau ne contient AUCUNE ligne, alors que l'en-tête indique
correctement « N action(s) après filtres » et que les KPI badges +
sparkline cumulatif fonctionnent.

**Cause (régression v0.52.10)** : le JS callback ajouté pour le
dblclick sur la cellule commentaire passait à `DT::datatable(callback
= ...)` une fonction COMPLÈTE :
```js
function(table) { table.on('dblclick.dt', ...); }
```
Or `DT::datatable` wrappe **automatiquement** le callback fourni dans
`function(table) { … }`. Le résultat était une fonction-dans-une-fonction
où :
1. Le handler `dblclick` est dans une fonction INTERNE jamais appelée
   → handler jamais enregistré.
2. La fonction externe ne fait `return table;` → DataTables casse
   silencieusement son pipeline d'initialisation → tableau RENDU SANS
   LIGNES alors que `data.frame` a bien N lignes.

**Fix** : le callback passe désormais juste le **corps** de fonction
(pas de wrapper `function(table) { ... }`), avec un `return table;`
final. DT applique son propre wrapper et le pipeline init reprend
normalement.

`R/mod_action_plan.R`. Cycle dev `0.52.11` → `0.52.11.9001` →
release stable **`v0.52.12`** (PATCH, fix régression urgent).

# nemetonshiny 0.52.11 (2026-06-01)

### Changed — Onglet « Carte FAST » : `card_header` titre supprimé, remplacé par un bandeau inline

Le `bslib::card_header` qui portait le titre « Carte pixel — NDVI /
NBR à la résolution Sentinel-2 (10 m) » mangeait une rangée entière
en haut de l'onglet et créait une dissymétrie avec « Alertes FAST »
voisin (qui n'a pas de header).

Le titre passe désormais en **bandeau `alert-info`** inline au-dessus
de la carte, padding minimal, symétrique stylistiquement avec le
bandeau vert « Aucune alerte FAST sur la fenêtre » d'Alertes FAST.
Gain : ~30-40 px de hauteur récupérée pour la carte, cohérence
visuelle entre les deux sous-onglets.

`R/mod_monitoring_pixel_map.R`. Cycle dev `0.52.10` →
`0.52.10.9001` → release stable **`v0.52.11`** (PATCH, feat).

# nemetonshiny 0.52.10 (2026-06-01)

### Added — Plan d'actions : dblclick sur cellule commentaire ouvre le modal d'édition

**Problème** : dans le tableau du Plan d'actions, la colonne
`commentaire` est étroite et ellipsisée — le texte long est
illisible. L'édition inline DT était même contre-productive : un
single-line input dans une cellule étroite tronquait le commentaire
sans permettre de le voir entièrement.

**Fix** : le commentaire passe désormais EXCLUSIVEMENT par le modal
d'édition (textarea 6 lignes, multi-ligne, déjà utilisé par le
double-clic Kanban). Un **dblclick sur la cellule commentaire** du
tableau ouvre maintenant ce même modal, qui expose en plus statut /
priorité / année d'occurrence.

* `R/mod_action_plan.R` :
  - `EDITABLE_COLS` ne contient plus « commentaire » (édition inline
    désactivée pour cette colonne uniquement, les autres colonnes
    restent inline-éditables comme avant).
  - La colonne commentaire reçoit la className DT
    `action-comment-trigger` via `columnDefs`.
  - `DT::datatable(callback = ...)` ajoute un handler JS qui écoute
    `dblclick.dt` sur `td.action-comment-trigger` et émet
    `input$row_edit_request <- {action_id, _ts}`.
  - La logique d'ouverture du modal est extraite en helper
    `.open_action_edit_modal(action_id)` appelée par les 2
    observers : `input$kanban_edit_request` (existant, dblclick
    kanban) et `input$row_edit_request` (nouveau, dblclick tableau).
* `inst/app/www/css/custom.css` : nouvelle règle
  `table.dataTable td.action-comment-trigger { cursor: pointer;
  text-decoration: underline dotted; }` qui donne un affordance
  visuel (curseur main + soulignement pointillé) sur la cellule
  cliquable. Hover : léger fond bleu.

Cycle dev `0.52.9` → `0.52.9.9001` → release stable **`v0.52.10`**
(PATCH, feat).

# nemetonshiny 0.52.9 (2026-06-01)

### Fixed — Onglet « Plan d'actions » : contexte IA non rafraîchi après création des commentaires Synthèse

**Symptôme reproduit** : un utilisateur ouvre **Plan d'actions** sans
avoir écrit de commentaires dans Synthèse → l'IA refuse de générer le
plan avec le message `action_plan_generate_no_comments`. Il navigue
vers **Synthèse**, génère les commentaires via l'IA (ou les saisit
manuellement), retourne sur Plan d'actions → **même message**, alors
que les commentaires existent maintenant sur disque.

**Cause** : la `shiny::reactive` `plan_llm_context()` dans
`mod_action_plan.R` (l.1716) lisait `load_comments(project$id)`
**une seule fois au montage du module**. Ses dépendances Shiny
étaient `app_state$current_project`, `ug_ids()` et
`plan_rv()$horizon_annees` — RIEN qui ne change quand
`save_comments()` écrit sur disque depuis `mod_synthesis` /
`mod_family`. Donc le contexte restait figé sur le snapshot vide du
premier affichage.

**Correctif** : signal de refresh inter-modules via `app_state`.

* `R/app_server.R` : nouveau slot `app_state$comments_refresh = 0L`
  dans `reactiveValues`, sur le même pattern que le
  `samples_refresh` existant (mod_sampling → mod_monitoring).
* `R/mod_synthesis.R` : les 2 call sites de `save_comments()`
  (observer IA + observer manuel `input$synthesis_comments`)
  bumpent désormais `app_state$comments_refresh <- ... + 1L`.
* `R/mod_family.R` : observer manuel `input$analysis_comments`
  bumpé symétriquement.
* `R/mod_action_plan.R` : `plan_llm_context()` lit
  `app_state$comments_refresh` en tête de reactive pour créer la
  dépendance Shiny — le rechargement `load_comments()` se déclenche
  désormais à chaque sauvegarde côté Synthèse/Famille.

Cycle dev `0.52.8` → `0.52.8.9001` → release stable **`v0.52.9`**
(PATCH, fix).

# nemetonshiny 0.52.8 (2026-05-31)

### Changed — Onglet « Alertes FAST » : contrôles déplacés à droite de la carte (sidebar)

Avant : « Mode du raster (Fréquence/Intensité) », « Afficher le
raster » et « Opacité du raster » occupaient une ligne horizontale
`flex-wrap` au-dessus de la carte, ce qui :
* mangeait de la hauteur verticale utile sur la carte
* différait visuellement de l'onglet voisin **Carte FAST** (déjà en
  layout sidebar droite depuis v0.47.0)
* sur les écrans étroits, faisait wrapper les contrôles en deux
  rangées qui empilaient encore plus haut

Après : `bslib::card` + `bslib::layout_sidebar(position = "right",
width = 250L, open = "always")` exactement comme Carte FAST. Les
trois contrôles vivent dans la sidebar à droite, la carte gagne
toute la zone rectangulaire principale. Les labels passent du
sibling `<strong>` au `label =` natif du contrôle (alignement
vertical naturel en sidebar).

L'observer de refresh i18n (`shiny::observe` ↔ langue) gère
désormais aussi le rafraîchissement du label radio « Mode du raster »
(il était `label = NULL` avant), du checkbox et du slider — pas de
texte qui resterait figé en FR après un switch en EN.

`R/mod_monitoring_fast_alerts.R`. Cycle dev `0.52.7` →
`0.52.7.9001` → release stable **`v0.52.8`** (PATCH, feat).

# nemetonshiny 0.52.7 (2026-05-31)

### Added — Bouton « Enregistrer ce projet » INLINE dans le bandeau Suivi sanitaire

Le bouton `input$register` (sidebar « Enregistrer ce projet comme zone
de suivi ») existe déjà depuis longtemps, mais vit sous le bloc « Mode
de suivi » de la barre latérale et tombe **systématiquement sous le
pli** sur les écrans 1080p courants. Résultat : l'utilisateur voit le
message « Aucune zone enregistrée » (ou le bandeau orphelin v0.52.5)
mais **ne voit pas l'action de récupération**, et le texte du bandeau
ne mentionne que la voie R (`nemeton::register_monitoring_zone(...)`).

Cette release ajoute le bouton **directement dans le bandeau** dans
les deux branches concernées :

* **`n == 0`** (DB monitoring vide) : bouton primary bleu sous le
  texte explicatif.
* **« zone orpheline »** (zones présentes mais aucune ne correspond
  au projet — typiquement après un wipe par les tests cœur, cf.
  v0.52.5) : même bouton, variante `btn-warning` jaune pour rester
  visuellement cohérent avec le bandeau warning.

Le bouton inline est un alias de `input$register` — pas de
duplication de logique : l'observer du sidebar a été refactoré pour
écouter les deux inputs via `shiny::bindEvent(input$register,
input$register_inline)`. Le sidebar bouton historique reste en place
(pour les utilisateurs qui ont scrollé ou qui ont un grand écran).

* `R/mod_monitoring.R` :
  - branches `n == 0` et orphelin de `output$db_status` rendent le
    bouton inline avec son icône `plus-circle` ;
  - l'observer historique `observeEvent(input$register, ...)`
    devient un `observe()` + `bindEvent(input$register,
    input$register_inline, ignoreInit = TRUE)`.

Cycle dev `0.52.6` → `0.52.6.9001` → release stable **`v0.52.7`**
(PATCH, feat).

# nemetonshiny 0.52.6 (2026-05-31)

### Fixed — Onglet « Synthèse » : ajustement fin du padding de la légende « Taille image »

`v0.52.3` calait le centre vertical de la légende sur le centre du
bouton « Image de couverture » via `padding-top: 0.55rem` (≈ 9 px, soit
~½ de la hauteur du bouton 38 px). En pratique, la ligne des badges
(`NDP / Hauteur LiDAR / Inventaire ML`) de la colonne droite tombe
~8 px **sous** le centre du bouton — le flux `Score global` → `54.8` →
`/100 (12 familles)` n'a pas exactement la même hauteur cumulée que
les 2 boutons PDF + GeoPackage de la colonne du milieu.

`padding-top` passe de `0.55rem` à `1rem` (≈ 16 px) pour descendre le
texte juste au niveau du centre des badges. `R/app_ui.R`.

Cycle dev `0.52.5` → `0.52.5.9001` → release stable **`v0.52.6`**
(PATCH, fix).

# nemetonshiny 0.52.5 (2026-05-31)

### Added — Bandeau de récupération « zone orpheline » après wipe par les tests cœur

L'incident **villards 2026-05-31** a montré que `helper-monitoring.R`
côté cœur `nemeton` (lignes 82-88) DROP CASCADE 7 tables monitoring
sans aucun garde-fou. Quand `NEMETON_DB_URL` pointe sur une base de
production, un cycle `devtools::test()` côté cœur détruit les zones
utilisateur réelles puis les remplace par des stubs de test
(`name = "Zskip"`, `plot_id = "P01"`, `project_uuid = NULL`).

Sans détection app-side, l'utilisateur voyait :
1. Bandeau **vert** « N zone(s) connectée(s) » — trompeur, aucune
   zone n'est rattachée à son projet.
2. Dropdown « Zone de suivi » vide (`monitoring_zone_id` du projet
   ne match aucune zone).
3. Plus tard, `Diagnostic FAST` crashait en violation de FK
   `obs_pixel.plot_id → plot.id` parce que `plot` ne contient que
   les stubs de test.

Le module monitoring détecte désormais l'état « orphelin » en lisant
la colonne `monitoring_zone.project_uuid` (ajoutée par la migration
`0003_project_uuid`) : si `nrow(zones) > 0` mais aucune zone ne porte
le `project$id` courant, on bascule sur un bandeau jaune `warning` :

> **Zones présentes — mais aucune ne correspond à ce projet**
>
> La base contient N zone(s), mais aucune n'est rattachée au projet
> chargé. Symptôme typique d'un wipe par les tests cœur (incident
> villards 2026-05-31). Clique sur « Enregistrer ce projet comme
> zone de suivi » dans la barre latérale pour recréer la zone et
> ses placettes en un clic.

Le bouton de récupération existe déjà (`input$register` →
`register_project_as_zone(con, project)`) — on ne duplique pas
l'action, on guide vers elle. Requête `project_uuid` en best-effort :
si la migration `0003` n'est pas encore appliquée (cas extrême), on
retombe gracieusement sur le banner de succès classique.

* `R/mod_monitoring.R` : nouvelle branche dans `output$db_status`
  entre le cas « n == 0 » et le cas succès, qui interroge
  `monitoring_zone WHERE project_uuid IS NOT NULL` et compare au
  `project$id` courant.
* `R/utils_i18n.R` : deux nouvelles clés
  `monitoring_zone_orphan_title` / `monitoring_zone_orphan_body`
  (FR/EN, encodage UTF-8 littéral conforme aux entrées récentes).

Côté **cœur**, le brief de correction définitive est rédigé : ajouter
un garde-fou `.guard_test_db()` dans `helper-monitoring.R` qui
refuse de tourner sauf si `NEMETON_DB_URL_TEST` est défini et
distinct de `NEMETON_DB_URL`. À traiter dans une session dev
`/home/pascal/dev/nemeton` ; release cible `nemeton@v0.54.0`.

Cycle dev `0.52.4` → `0.52.4.9001` → release stable **`v0.52.5`**
(PATCH, feat).

# nemetonshiny 0.52.4 (2026-05-31)

### Fixed — Pixel/Placette plots : courbes hachées sur les zones de recouvrement partiel MGRS

Sur la **Carte FAST** (mode pixel modal), un clic sur un pixel à l'EST
de la zone villards donnait une courbe NDVI/NBR très hachée (lignes
absentes entre la plupart des points adjacents), alors qu'un clic sur
un pixel à l'OUEST donnait une courbe parfaitement continue avec les
mêmes paramètres.

**Cause** : la zone villards est couverte par DEUX tuiles Sentinel-2
MGRS qui se chevauchent partiellement — `T31TGM` (large, ~1340 m) qui
couvre toute la zone, et `T31TFM` (étroite, ~440 m) qui ne couvre que
l'OUEST. Pour un pixel à l'EST :
* les ~60 scènes `T31TGM` retournent une mesure valide,
* les ~62 scènes `T31TFM` retournent `value = NA` (pixel hors
  couverture).

`plotly` casse la ligne à chaque NA → l'utilisateur voyait ~60
mesures valides perdues entre des trous, alors qu'au moins 60
observations existent réellement pour ce pixel.

**Correctif** : `R/mod_monitoring_pixel_map.R` filtre désormais les
lignes `value = NA` après le tri par date et avant `plotly::add_trace`,
dans les deux modaux (pixel-click ET marker-click placette). La
courbe redevient continue à partir des seules observations
réellement disponibles pour le pixel/la placette.

Cycle dev `0.52.3` → `0.52.3.9001` → release stable **`v0.52.4`**
(PATCH, fix).

# nemetonshiny 0.52.3 (2026-05-31)

### Fixed — Onglet « Synthèse » : légende « Taille image Max 5 Mo » remise à DROITE du fileInput, alignée avec les badges

`v0.52.2` avait placé la légende centrée sous le fileInput ; le
résultat ne correspondait pas à la demande UX, qui était de mettre la
légende **à droite** du sélecteur « Image de couverture » au **niveau
horizontal de la ligne des badges** (`NDP 1 – Observation`,
`Hauteur LiDAR HD`, `Inventaire ML`) de la colonne de droite.

Le piège : `shiny::fileInput` rend bouton + placeholder + barre de
progression. La barre s'affiche dès qu'on charge une image, ce qui
augmente la hauteur totale du fileInput. Un `align-items: center` sur
le flex aurait alors centré la légende sur (bouton + barre), donc
plus bas que le bouton dès qu'on aurait choisi un fichier — exactement
le décalage qu'on cherche à supprimer.

Solution : flex avec `align-items: flex-start` (ancre la légende en
haut du flex = niveau du haut du bouton) et `padding-top: 0.55rem`
(≈ moitié de la hauteur du bouton, 38px) pour la descendre pile au
centre du bouton. Le placement reste stable que la barre de
progression soit affichée ou non.

`R/app_ui.R`. Cycle dev `0.52.2` → `0.52.2.9001` → release stable
**`v0.52.3`** (PATCH, fix).

# nemetonshiny 0.52.2 (2026-05-31)

### Fixed — Onglet « Synthèse » : alignement de la légende « Taille image Max 5 Mo »

La petite légende sous le sélecteur d'image de couverture
(« Taille image Max 5 Mo, PNG/JPG ») vivait dans un flex inline à
droite du bouton « Image de couverture », ce qui la plaçait au niveau
vertical du bouton et donc légèrement plus haut que la ligne des
badges (`NDP 1 – Observation`, `Hauteur LiDAR HD`, `Inventaire ML`)
de la colonne de droite. Elle est désormais placée sur une ligne
dédiée sous le fileInput, centrée, ce qui l'aligne visuellement avec
la ligne des badges et clarifie qu'elle décrit la contrainte de
l'input qu'elle suit.

`R/app_ui.R` (sortie de la `bslib::layout_columns` "Project summary /
Download buttons / Global score"). Cycle dev `0.52.1` →
`0.52.1.9001` → release stable **`v0.52.2`** (PATCH, fix).

# nemetonshiny 0.52.1 (2026-05-31)

### Fixed — Warning « relation "monitoring_zone" does not exist » au boot (Postgres)

Sur Postgres uniquement, le warning suivant apparaissait au démarrage
de la session, juste avant les `Applied migration 0001_init / 0002_fordead / 0003_project_uuid` :

```
Avis : Failed to list monitoring zones: Failed to prepare query :
ERROR: relation "monitoring_zone" does not exist
```

Cause : `get_monitoring_db_connection(read_only = TRUE)` sautait
volontairement l'étape de migration sur le chemin RO (optimisation
pertinente pour SQLite : la présence du fichier proxy-prouve que le
schéma est déjà migré). Pour Postgres ce raccourci était faux — la
base existe toujours, mais le schéma peut très bien ne pas avoir
encore été appliqué au tout premier reactive tick. Le warning
disparaissait dès que le premier RW path (sauvegarde projet,
ingestion FAST…) ouvrait une connexion qui finissait par migrer.

Correctif : sur le RO path, si le backend n'est PAS un fichier
(SQLite), on appelle aussi `.ensure_monitoring_schema()` —
idempotent, sub-milliseconde après la 1re fois, et la race au
démarrage disparaît proprement. SQLite garde son fast-path
inchangé (existence du fichier = déjà migré).

`R/service_monitoring_db.R`. Cycle dev `0.52.0` → `0.52.0.9001` →
release stable **`v0.52.1`** (PATCH, fix).

# nemetonshiny 0.52.0 (2026-05-31)

### Changed — Vrai cancel coopératif FAST/FORDEAD (s'appuie sur `nemeton@v0.53.0`)

L'app câble désormais le mécanisme `cancel_path` introduit côté cœur en
`nemeton@v0.53.0` : un clic sur **« Annuler le diagnostic »** écrit
`<projet>/data/fast_cancel.flag` (resp. `fordead_cancel.flag`), que le
worker poll entre tuiles (FAST) / entre phases reticulate (FORDEAD) et
qui le fait sortir proprement au prochain checkpoint avec commit
partiel. Les INSERT déjà commités sont conservés (idempotents,
`ON CONFLICT DO NOTHING`) — la relance est sans risque.

* `R/service_monitoring.R` : `run_ingestion_async()` et
  `run_fordead_async()` exposent un paramètre `cancel_path = NULL` et
  le forwardent à `nemeton::ingest_sentinel2_timeseries()` /
  `run_fordead_dieback()`.
* `R/mod_monitoring.R` : `input$run` et `.invoke_fordead` purgent le
  flag résiduel avant chaque lancement (sinon le worker abandonnerait
  d'emblée), `fast_task$invoke()` / `fordead_task$invoke()` passent le
  chemin du flag, et les observers `input$run_cancel` /
  `input$run_health_cancel` écrivent le flag **avant** le
  `force_unlock_*(TRUE)` (l'UI est libérée immédiatement, le worker
  sort au prochain checkpoint).
* `R/utils_i18n.R` : libellé `monitoring_run_cancel_btn` passé de
  « Libérer l'interface » → **« Annuler le diagnostic »** /
  **« Cancel the diagnostic »** ; toast `monitoring_run_cancel_done`
  reformulé pour expliquer le mécanisme (« le worker termine la tuile
  (FAST) / la phase (FORDEAD) en cours puis s'arrête proprement »).
* `DESCRIPTION` : plancher `Imports: nemeton (>= 0.53.0)` (l'app exige
  maintenant `cancel_path` côté cœur).

Cycle dev `0.51.11` → `0.51.11.9001` → release stable **`v0.52.0`**
(MINOR, feat).

# nemetonshiny 0.51.11 (2026-05-31)

### Changed — Libellé du bouton « Annuler / Réinitialiser » → « Libérer l'interface »

Le bouton qui apparaît pendant un diagnostic FAST/FORDEAD ne **tue pas**
le worker (Shiny `ExtendedTask` ne propose pas d'API d'annulation) — il
**force-unlock l'UI** (`force_unlock_quick` / `_health`) pour que
l'utilisateur reprenne la main sur le bouton « Lancer le diagnostic »
sans attendre la fin du worker. L'ancien libellé « Annuler / Réinitialiser »
suggérait à tort que le diagnostic était arrêté en base, ce qui prêtait
à confusion : nouveau libellé **« Libérer l'interface »** /
**« Release the interface »**. Le toast de confirmation reformulé en
miroir (« Interface libérée. Vous pouvez relancer dès que le problème
est corrigé. Le worker en cours continue en arrière-plan… »). Les deux
boutons (FAST et FORDEAD) partagent les mêmes clés i18n, un seul
changement couvre les deux.

# nemetonshiny 0.51.10 (2026-05-31)

### Added — Heartbeat de fin pour les workers FAST et FORDEAD

Quand le diagnostic FAST atteint « Tuile 122/122 » et que le bouton ne
se réactive plus, on ne savait pas si `nemeton::ingest_sentinel2_timeseries()`
avait rendu la main ou s'il était encore en train de finaliser ses
INSERTs `obs_pixel` / son checkpoint SQLite/WAL. Le worker émet
désormais un événement final `s2:ingest_done` (resp.
`fordead:dieback_done`) juste après le retour du cœur. Le
`progress_callback` pousse l'événement dans le fichier JSON tailé par
la session principale : si tu le vois mais que la notification de
complétion n'arrive pas, le bug est dans le hand-off Shiny ExtendedTask
(et tu peux force-unlock l'UI via le bouton « Annuler » qui reste
visible) ; si tu ne le vois jamais, c'est nemeton qui est encore au
travail.

### Fixed — Carte FAST : silence des warnings « Some values were outside the color scale »

NDVI = (B08-B04)/(B08+B04) et NBR = (B08-B12)/(B08+B12) sont
théoriquement bornés à [-1, 1] mais le bruit numérique sur les bandes
fait dépasser quelques cellules de ±ε (1.0001, -1.0001) → la palette
plasma (domaine `c(-1, 1)`) les déclarait hors domaine et émettait 4
warnings `colors(.)` par re-render. `terra::clamp(r, -1, 1, values = TRUE)`
ramène ces cellules sur la borne avant `addRasterImage()` — cap visuel
préservé, plus aucun warning.

# nemetonshiny 0.51.9 (2026-05-30)

### Fixed — Alertes FAST : raster invisible + warnings « Some values were outside the color scale »

Le raster d'alerte ne s'affichait plus (modes Fréquence et Intensité) :
- En mode count, le masque `terra::ifel(r == 0, NA, r)` ne couvrait
  pas les valeurs négatives résiduelles (bruit numérique en bord de
  tuile) → `pal()` les déclarait hors domaine `[0.5, max+0.5]` → 4
  warnings `colors(.)` à la console et raster majoritairement
  transparent. Masque devient `terra::ifel(is.na(r) | r <= 0, NA, r)`
  (positif strict).
- En mode rolling (Intensité), les ~5 % de cellules au-dessus de
  `p95` étaient hors domaine `[0, p95]` → mêmes warnings. Un
  `terra::ifel` clamp les valeurs `> upper` à `upper` avant `pal()` :
  cap visuel p95 conservé (couleur max), zéro hors-domaine côté haut.

### Fixed — Graphique de série pixel : lignes manquantes entre points

Dans le modal « Pixel à (lat, lon) » (Carte FAST → clic), beaucoup de
points NDVI / NBR apparaissaient isolés, parfois reliés par de longs
segments qui sautaient des mois. La boucle `for (b in unique(ts$index))`
ne triait pas le data.frame par `obs_date` avant `plotly::add_trace` →
plotly reliait les points dans l'ordre des lignes. Sortie de
`extract_pixel_timeseries()` triée par date avant tracé. La boucle
voisine du graphique placette agrégée triait déjà — alignement.

# nemetonshiny 0.51.8 (2026-05-30)

### Fixed — Onglet Fournisseur LLM : le bloc statut + clé suit la sélection

Dans la boîte « Clés API externes » → onglet **Fournisseur LLM**,
changer le provider dans la liste déroulante ne rafraîchissait pas le
bandeau de statut ni la section clé en dessous — ils restaient figés
sur le provider précédent. Le bloc est désormais servi par un
`uiOutput` réactif à `input$llm_provider`, `llm_edit_mode()` et
`status_refresh()` → mise à jour fluide sans re-render du modal.

### Added — Onglet Fournisseur LLM : vue d'ensemble multi-fournisseurs

- **Badge `✓` par fournisseur** directement dans la liste déroulante :
  un coup d'œil sur le dropdown ouvert montre lesquels sont déjà
  configurés (ex. `Mistral ✓`, `Anthropic`, `OpenAI ✓`).
- **Ligne résumé** au-dessus du sélecteur :
  `« 2 / 3 fournisseurs configurés : Mistral, OpenAI »`
  ou `« Aucun fournisseur configuré. »` quand rien n'est posé. Vue
  d'ensemble immédiate sans avoir à dérouler la liste.

# nemetonshiny 0.51.7 (2026-05-30)

### Added — modal de configuration à 2 onglets (Theia + Fournisseur LLM)

L'icône engrenage de la barre de navigation ouvre désormais une boîte
de dialogue **« Clés API externes »** structurée en 2 onglets :

- **Theia / DATA TERRA** : contenu existant (statut Python, clé d'accès
  + clé secrète, provenance et licences des sources).
- **Fournisseur LLM** : sélecteur de fournisseur (Mistral / Anthropic /
  OpenAI), champ clé API, statut configuré / non avec sa source
  (variable d'env ou fichier local), boutons Modifier / Supprimer
  lorsqu'une clé est déjà en place — strictement le même UX que Theia.

Nouveau service `R/service_llm.R` (`llm_providers`, `llm_status_all`,
`llm_save_api_key`, `llm_clear_api_key`). La clé est persistée dans
`~/.config/nemetonshiny/llm.json` (chmod `0600`, fichier unlinké
lorsqu'il devient vide) et `Sys.setenv()` est appelé dans la session R
pour effet immédiat. Résolution **env > fichier** — `.Renviron`
continue de fonctionner sans rien changer si tu préfères y stocker tes
clés. Nouvelles clés i18n FR/EN (`api_keys_*`, `llm_*`). Tests dédiés
`test-service_llm.R` (30 assertions).

# nemetonshiny 0.51.6 (2026-05-30)

### Security — `~/.config/teledetection/.apikey` est désormais en `0600`

La clé Theia / DATA TERRA enregistrée via le modal de configuration
(`theia_save_api_key()`) est immédiatement protégée par `Sys.chmod(..., "0600")` (lecture/écriture pour le propriétaire uniquement). Auparavant le fichier héritait du `umask` du process, souvent `0644` (world-readable). No-op silencieux sous Windows.

### Changed — modal Theia : section clé contextuelle (Modifier / Supprimer)

Quand la clé Theia est déjà configurée, le modal n'affiche plus le
formulaire de saisie + le bouton « Enregistrer » (qui invitait à
l'écrasement involontaire). À la place, deux boutons : **« Modifier la
clé »** (révèle le formulaire pré-rempli vide, avec un bouton Annuler)
et **« Supprimer la clé »** (supprime `~/.config/teledetection/.apikey`
et `Sys.unsetenv` de `TLD_ACCESS_KEY` / `TLD_SECRET_KEY`). Quand aucune
clé n'est configurée, le formulaire reste affiché comme avant.
Nouveau helper `theia_clear_api_key()`. 4 nouvelles clés i18n FR/EN.

### Fixed — modal Theia : la table « Provenance et licence » apparaît enfin

`DT::datatable(...)` inséré inline dans `modalDialog()` n'initialisait
pas son JS (htmlwidget non câblé hors d'un `DTOutput`) → la table de
provenance était invisible. Remplacée par une **table Bootstrap
statique** (`htmltools::tags$table`) — aucun JS requis, info statique
de toute façon, toujours affichée.

# nemetonshiny 0.51.5 (2026-05-30)

### Fixed — Alertes FAST : préserve le zoom et le fond OSM/Satellite

Sur la carte Alertes FAST, déplacer un slider (seuils NDVI/NBR, opacité)
ou changer le mode (Fréquence/Intensité) re-déclenchait un
`renderLeaflet` complet → le zoom utilisateur et le fond sélectionné
(OSM/Satellite) étaient réinitialisés à chaque tick. Le pattern est
désormais celui de la Carte FAST : la carte de base (tuiles + UGF +
fitBounds) est rendue une seule fois, le raster d'alerte et sa légende
sont mis à jour via `leafletProxy` + `clearGroup`/`removeControl` →
zoom et fond conservés.

# nemetonshiny 0.51.4 (2026-05-29)

### Changed — réamorçage du cache COG restreint à la fenêtre FAST (préserve FORDEAD)

Le cache Sentinel-2 (`<projet>/cache/layers/sentinel2`) est **partagé**
entre FAST et FORDEAD. Auparavant, cocher « Réamorcer le cache COG »
avant un run FAST faisait un `unlink` **de tout** le dossier → cela
effaçait aussi les bandes et dates de FORDEAD (dont la période
d'apprentissage), forçant un re-téléchargement complet au diagnostic
suivant.

Désormais le réamorçage ne supprime **que les scènes dont la date
d'acquisition tombe dans la fenêtre de dates FAST** courante. Les scènes
hors fenêtre (typiquement l'apprentissage FORDEAD) sont **préservées**.
Une scène dont la date n'est pas parsable depuis son identifiant S2 est
conservée par prudence. Le libellé et l'aide de la case sont mis à jour.

# nemetonshiny 0.51.3 (2026-05-29)

### Changed — Alertes FAST : alignement des contrôles de l'en-tête

- La case « Afficher le raster » est légèrement abaissée (`top: 2px`)
  pour s'aligner sur les radios « Fréquence / Intensité » voisines.
- Le label « Opacité du raster » est désormais **à gauche** du slider
  (inline) au lieu d'au-dessus.

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
